# Native Include / Preprocessor Input Specification v0.1

## Summary

This specification defines the first native AmigaOS `.include` behavior for the
`opforge_cli` front end. The immediate goal is a small, deterministic input
expansion layer that can feed the existing native tokenizer and later parser VM
work while leaving room for broader preprocessor functionality.

Rust already has a source preprocessor that handles `.include`, `.incbin`,
conditional assembly, and macro expansion. The native AmigaOS port should not
copy that whole system at once. It should first model `.include` as host-owned
input stream expansion with stable records, bounded storage, deterministic
diagnostics, and explicit extension points for later conditional, define,
include-once, macro, and binary-include features.

## Problem

Many Amiga assembly sources use include files for constants, structure layouts,
system definitions, and macro source before the first executable instruction.
The native `opforge_cli` path currently reads a single source file and can run
the package-backed tokenizer over its lines, but it does not yet expand include
files into that input stream.

Without early `.include` support, the first native VM pipeline can only prove
toy single-file sources. That would under-represent real Amiga projects and
would make later `PRVM`, `EXVM`, symbol, and emitter work iterate against the
wrong input shape.

The Rust behavior to preserve or intentionally subset is visible in:

- `crates/opforge-core/src/preprocess.rs`
- `crates/opforge-asm/src/tests.rs`
- `crates/opforge-lsp/src/session.rs`

## Goals

- [ ] Define native `.include` input expansion for the first AmigaOS CLI slice.
- [ ] Preserve Rust-compatible include target parsing for double-quoted,
      single-quoted, and unquoted non-empty operands.
- [ ] Preserve semicolons inside quoted include paths while still allowing
      trailing comments after the quoted target.
- [ ] Define fixed-capacity native include state: include roots, open include
      stack, expanded-line records, and logical source-location records.
- [ ] Define deterministic diagnostics for missing targets, malformed include
      directives, include cycle, nesting-depth overflow, path length overflow,
      include-root overflow, and expanded-line/source-map overflow.
- [ ] Define deterministic first-slice behavior for conditional preprocessor
  directives before native conditional state exists.
- [ ] Keep `.include` independent from `.use` module dependency loading while
      allowing future shared path-root helpers.
- [ ] Leave explicit extension points for conditional assembly, define tables,
      include-once guards, macro-source expansion, and `.incbin` without
      implementing those features in the first native slice.

## Non-Goals

- [ ] Do not implement full Rust preprocessor parity in the first native slice.
- [ ] Do not implement `.if` / `.ifdef` / `.ifndef` / `.else` / `.elseif` /
      `.endif` conditional filtering in this slice.
- [ ] Do not implement define substitution or macro expansion in this slice.
- [ ] Do not implement include-once or pragma-style file guards in this slice.
- [ ] Do not implement `.incbin` in this slice.
- [ ] Do not route `.include` through `TKVM`, `PRVM`, `EXVM`, or expression VM
      bytecode. `.include` remains host-owned input expansion.
- [ ] Do not resolve `.use` module graph dependencies through the include stack.
- [ ] Do not add native symbol resolution, instruction encoding, relocation, or
      Hunk emission behavior as part of include support.

## Invariants / Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding
  for work derived from this specification.
- `.include` expansion is host-owned. It feeds source lines into `TKVM`; it is
  not itself a tokenizer, parser, expression parser, or emitter VM contract.
- The first native implementation must use fixed capacities and deterministic
  failures rather than unbounded allocation.
- Native include records must preserve enough logical source context for later
  tokenizer, parser, diagnostic, listing, and emitter records to report the
  original file and line.
- Include expansion must happen before delegated opasm statement parsing for the
  expanded line stream.
- A native implementation may tokenize the `.include` directive line as a guard
  before expansion, but the expanded source stream must be what later parser and
  emitter stages consume.
- `.include` and `.use` are distinct concepts. `.include` expands source text in
  the current compilation stream; `.use` records module imports and later module
  graph work.
- Include path resolution must be deterministic and must not silently search
  outside the allowed source root or configured include roots.
- Fixture/reference regeneration is allowed only when the native CLI report or
  generated reference output intentionally changes.

## Behavioral Contract

The first native `.include` implementation must accept these target forms:

- `.include "path/to/file.inc"`
- `.include 'path/to/file.inc'`
- `.include path/to/file.inc`

Directive spelling is case-insensitive after the leading dot. The parser should
allow whitespace after the dot before the directive name when that is practical
for the native scanner, matching the Rust preprocessor's source-line route.

Quoted targets must strip the outer quotes. Semicolons inside quoted targets are
part of the path. Semicolons after the quoted target begin a trailing comment.
An empty target is malformed.

Path resolution must use this order:

1. the including file's directory;
2. configured include roots in command-line/configuration order.

The first matching file wins. Relative paths must stay inside the including
file's directory tree or one configured include root after canonicalization.
Parent-relative paths such as `../shared.inc` are allowed only when the resolved
target remains inside an allowed root. Absolute paths are allowed only when the
resolved file is inside an allowed root.

The native implementation must maintain at least these logical records:

- include root table: configured path roots in search order;
- include stack: currently open files with canonical identity, parent record,
  include directive line, and depth;
- include event records: parent file, child target text, resolved child file,
  source line, and status;
- expanded-line records: emitted source line text plus logical file id and
  logical one-based line number;
- diagnostic records: native diagnostic code, message, file id, line, column
  where available, and searched path list for missing files.

The first native CLI slice must expose these records through simple textual
report markers, before the later VM pipeline report spec assigns any richer
wire format:

- `STAGE include`
- `STATUS include-ok`
- `INCLUDE-FILE <file-id> <path>`
- `INCLUDE-ROOT <index> <path>`
- `INCLUDE-ENTER <depth> <parent-file-id> <line> <child-file-id> <target>`
- `INCLUDE-LINE <file-id> <line>`
- `INCLUDE-LEAVE <depth> <child-file-id>`
- `ERROR OPC-NCLI014: native include expansion failed`

The path or target text begins after the final fixed numeric field in each
record. Tests may lock marker presence and representative file/target text in
the first slice; Work item 4 of the roadmap may later replace or augment these
text markers with a compact handoff record contract.

Include cycle detection must use canonical file identity when available and must
report a deterministic chain. Repeating the same include after the previous
include has returned is allowed; only active-stack cycles are errors.

The native default maximum include depth should be fixed and documented by the
implementation. If the native value is lower than Rust's default depth of 64,
the native diagnostic must name the native limit.

Conditional filtering is deferred. Until native conditional state exists, the
first native implementation must reject `.if`, `.ifdef`, `.ifndef`, `.else`,
`.elseif`, and `.endif` preprocessor directives with `ERROR OPC-NCLI015: native
conditional preprocessing not implemented`. It must not process includes inside
conditional blocks and must not claim conditional parity.

Other deferred preprocessor-adjacent source lines must be preserved verbatim in
the expanded source stream for the first slice. This includes `.incbin`, macro
definition bodies, include-once or pragma-like lines, and define-like source
forms that are not implemented by the native include layer yet. They are not
expanded by this slice and may be rejected later by parser/emitter stages, but
the include layer must not silently drop or reinterpret them.

## Boundary Cases

- `.include` with no operand returns a missing-target diagnostic.
- `.include ""` and `.include ''` return a missing-target or empty-target
  diagnostic.
- `.include "dir;name.inc" ; comment` resolves `dir;name.inc`.
- `.include 'shared.inc'` resolves `shared.inc`.
- Missing include files report the requested target and all searched paths.
- If two include roots contain the same relative target, the first configured
  include root wins after the including file directory is checked.
- A self-include reports an include-cycle diagnostic.
- A multi-hop active include cycle reports a deterministic chain.
- Repeating the same include twice from the same parent after the first include
  returns is allowed.
- Include depth overflow reports the configured native maximum depth.
- Path length overflow reports a deterministic native diagnostic and does not
  truncate silently.
- Include root table overflow, include stack overflow, expanded-line overflow,
  and source-location table overflow stop before parser/emitter stages.
- Parent-relative traversal outside the including directory and configured
  include roots is rejected.
- `.if`, `.ifdef`, `.ifndef`, `.else`, `.elseif`, and `.endif` preprocessor
  directives are rejected with the native conditional-preprocessing diagnostic
  until conditional state lands.
- `.incbin`, macro-source lines, include-once or pragma-like lines, and
  define-like forms are preserved verbatim in the expanded source stream until
  their own native preprocessor slices land.

## Acceptance Criteria

- [ ] Native CLI expands one `.include "defs.inc"` file from the including
      file's directory and feeds the included lines into the tokenizer path.
- [ ] Native CLI accepts a single-quoted include target and preserves semicolons
      inside quoted include paths.
- [ ] Native CLI resolves include roots in deterministic order: including file
      directory first, then configured include roots in CLI/config order.
- [ ] Native CLI emits include report records with parent file, child file,
      include depth, and logical source-location mapping.
- [ ] Native CLI emits the first-slice textual include markers `STAGE include`,
  `STATUS include-ok`, `INCLUDE-FILE`, `INCLUDE-ENTER`, `INCLUDE-LINE`, and
  `INCLUDE-LEAVE` for successful expansion.
- [ ] Native CLI reports missing include files with searched paths.
- [ ] Native CLI reports include cycles and depth overflow deterministically.
- [ ] Native CLI rejects conditional preprocessor directives with `OPC-NCLI015`
  before conditional state exists.
- [ ] Native CLI preserves `.incbin`, macro-source lines, include-once or
  pragma-like lines, and define-like forms verbatim in the expanded source
  stream without expanding or dropping them.
- [ ] Native CLI leaves define substitution, macro-source expansion,
  include-once behavior, and `.incbin` explicitly unsupported or deferred
  without claiming parity.
- [ ] Focused host tests lock native labels, status strings, and report markers
      for include expansion.
- [ ] Opt-in FS-UAE smoke covers one successful include source and one malformed
      or missing include path when configured.

## Validation Expectations

Minimum validation for a spec-only slice:

- `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-native-include-preprocessor-input-spec-v0_1.md`
- `python3 scripts/workflow/check_plan_checkboxes.py documentation/plans/opforge-portable-vm-68020-amigaos-roadmap-v0_1.md`
- plan-quality review for this specification and roadmap linkage

Minimum validation for the first native implementation slice:

- `cargo fmt --all --check`
- `cargo test -p asm motorola68020_opforge_native_cli -- --nocapture`
- `cargo test -p asm examples_match_reference_outputs -- --nocapture`
- `cargo test --workspace`

Additional validation when native runtime behavior changes:

- opt-in FS-UAE native CLI smoke for include success and one failure path;
- reference refresh only when expected native CLI output changes;
- focused Rust parity tests if the Rust include contract is tightened while
  implementing the native slice.

## Open Questions

- What fixed capacities should the first native include root table, include
  stack, path buffers, expanded-line table, and source-location table use?
- Should native include roots reuse the Rust CLI `--include-path` spelling first,
  or should the initial AmigaOS slice use a smaller `Work:`-oriented implicit
  root model before exposing CLI flags?
- Should the later compact VM pipeline report keep these first-slice text
  markers, replace them with fixed-width binary-like records, or emit both for a
  transition period?
- Should `.incbin` share the include path resolver in the next preprocessor
  slice, or wait until native data emission exists?