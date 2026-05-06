# Review Report

## Scope

Native 68020/AmigaOS opForge implementation under `native/motorola68000/amigaos`,
with emphasis on target layout, production/readiness boundaries, code quality,
and alignment with the Rust VM architecture in `crates/opforge-vm`.

## Version Impact

- Affected component(s): native AmigaOS opForge CLI, native tkpkg service,
  native parser/tokenizer VM modules.
- Impact class: internal architecture and native deliverable readiness.
- Owned contract: native host/runtime layering for package-backed opForge targets.
- Rationale: the review does not identify an external CLI contract change, but
  it does identify structure and layering mismatches that should be resolved
  before treating the native tree as a stable deliverable surface.

## Findings

### RVW-2026-05-06-001

- Severity: High
- File: `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm:59`
- Issue: The tkpkg ABI advertises `ENTRY_ORD_PARSE_LINE`, but the dispatcher
  routes it to the generic deferred-runtime error path. The native CLI also has
  an unused `ENTRY_ORD_PARSE_LINE` envelope while the live path bypasses tkpkg
  and imports `prvm_route_line_68000` directly from `opforge_cli.asm`.
- Why it matters: Rust VM architecture exposes parse as a package/model-backed
  stage (`tokenize -> parse -> encode`) through the runtime model. Native
  currently splits that contract: tokenization and encoding are service calls,
  but parsing is a direct CLI-to-PRVM shortcut. That makes diagnostics, ABI
  validation, package resolution, and future host parity harder to keep aligned
  with the Rust VM implementation.
- Fix direction (one direction only; resolve competing options before finalizing):
  implement `ENTRY_ORD_PARSE_LINE` in `tkpkg_service_dispatch_v1` by routing
  through the PRVM line router, then change the CLI to use the service envelope
  for parse the same way it already does for tokenize and encode.

### RVW-2026-05-06-002

- Severity: High
- File: `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm:3106`
- Issue: The native CLI hard-codes instruction acceptance, addressing-mode
  selection, operand payload construction, and PC advancement for a tiny mnemonic
  set before it calls the package-backed encoder service.
- Why it matters: Rust VM encoding delegates mnemonic/mode selection to the
  runtime model and package-backed selector/encoder pipeline. The native
  implementation has a package-backed encoder available in tkpkg, but the CLI
  pre-filters the instruction surface (`lda`, `sta`, `jmp`, plus local PC-size
  rules) and rejects anything outside that hand-coded slice before the package
  can decide. This turns the CLI into a second selector implementation and risks
  immediate drift from Rust/package behavior.
- Fix direction (one direction only; resolve competing options before finalizing):
  move selector request construction out of the CLI and into a native opasm
  runtime module that consumes PRVM operand results and package metadata, leaving
  the CLI to pass parsed statement records into the native runtime stage.

### RVW-2026-05-06-003

- Severity: Medium
- File: `native/README.md:10`
- Issue: The target tree mixes deliverable runtime modules with debug/smoke
  harnesses and sample inputs as first-class sibling components.
- Why it matters: The new root-level `native/` structure is the right top-level
  home for target-hosted opForge deliverables, but the current layout makes
  `tkpkg_debug_cli.asm`, `prvm_debug_cli.asm`, `prvm_smoke.asm`,
  `prvm_line_iterator_smoke.asm`, and `tokvm_test_input.asm` look like production
  runtime pieces. Those are useful validation tools, but keeping them at the same
  level as runtime modules weakens the intended signal that `native/` contains
  deliverables rather than examples or experiments.
- Fix direction (one direction only; resolve competing options before finalizing):
  keep production modules under `native/motorola68000/amigaos/{opforge-cli,opcore,prvm,tkpkg,tokvm}`
  and move debug/smoke/sample entry points into a clearly named non-deliverable
  subtree such as `native/motorola68000/amigaos/test-harnesses/`.

### RVW-2026-05-06-004

- Severity: Medium
- File: `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm:3592`
- Issue: `--bin` and `--hunk` are parsed into the same `nativeCliHunkPath`, and
  the output path is later written by `opforge_native_cli_write_flat_output`
  regardless of which flag selected it.
- Why it matters: Rust CLI output selection distinguishes output formats, while
  native currently treats both flags as a flat byte stream sink. That may be fine
  for the first bin-focused smoke slice, but as a production target structure it
  makes `--hunk` misleading and couples CLI flag parsing to a temporary emitter
  shortcut.
- Fix direction (one direction only; resolve competing options before finalizing):
  split native output selection into explicit format state (`bin` versus `hunk`)
  and route only `--bin` to the flat writer until a native Hunk writer exists;
  have `--hunk` return a deterministic not-implemented diagnostic rather than
  writing flat bytes.

## Testing Gaps

The FS-UAE and Rust quality gates cover useful smoke/parity paths, but they do
not yet prove that the native parse stage is service-routed, that the native CLI
does not pre-filter selector behavior ahead of the package encoder, or that bin
and hunk output modes are separated.

## Residual Risks

The current implementation is good enough as an advancing native slice, but it
still contains transitional harnesses and CLI-owned runtime logic that should not
be treated as the final target-hosted architecture.

## Brief Summary

The `native/` root is the right structural direction. The production shape needs
one more tightening pass: make tkpkg the consistent service boundary for
tokenize/parse/encode, move test harnesses out of the production-looking module
set, and keep temporary output/selector shortcuts visibly outside the stable CLI
contract.
