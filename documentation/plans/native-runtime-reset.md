# Native assembler status and completion

## Current product state

The maintained AmigaOS assembler starts at
[`main.asm`](../../native/motorola68000/amigaos/main.asm), selects `.cpu 68020`
and declares `build/opforge` as its Hunk output. It is the promoted product;
complete Rust language, CPU-family and output parity remain unqualified.

Deploy `opforge` beside `packages/`. CPU packages load from disk by default;
embedding selected packages is an explicit build option. The Rust host generates
those packages through the shared hierarchy model and
[`native_runtime_package.rs`](../../crates/opforge-asm/src/native_runtime_package.rs).
Native self-hosting rebuilds the assembler executable using a prebuilt package;
it does not regenerate the package toolchain on Amiga.

`opforge src` reads `src/main.asm`, honoring source `.cpu` and `.output`
declarations. The root file's directory supplies default module/include roots;
additional `-M`/`-I` paths extend them. Ordinary CLI builds create bounded scratch
in working-directory `build/scratch` and reuse one persistent `build/ofm` object
set. `--capture-dir`, `--incremental-dir` and `--rebuild` override the corresponding
storage/rebuild choices. Product debug includes resolve relative to their files.
See the [native guide](../../native/README.md) for build and hardware commands.

The latest full bare-CLI proof, dated **2026-10-23**, uses source manifest
`fnv1a64:9e447609ba8deed3`: 270 inputs / 2,918,308 bytes. Under the established
8 MiB installed-Workbench/plain-background FS-UAE setup and 64 KiB stack,
`opforge src --rebuild` completes cold in **275 s** and `opforge src` completes
warm in **58 s**. Both exit zero and produce the complete **348,824-byte Hunk**
exactly matching live Rust and the bootstrap. Cold rebuilds all 190 dependency
modules; warm reuses all 190. The separate m68020 package is 478,042 bytes.

This proves completion in that 8 MiB setup, with unrestricted emulator CPU speed.
Peak heap was not remeasured; neither physical 68020 timing nor 2 MiB fit follows.
The engineering budget is **6.5 MiB process / 1.5 MiB OS**; the current pair does
not independently remeasure that process split. The preceding 348,752-byte
external-package snapshot passed A6000 cold/warm in **1,066/90 s**, with
unrestricted hardware RAM. The current bare-CLI bundle awaits hardware testing.
Exact identities, earlier qualified memory figures and retained performance
comparisons live in [runtime and memory status](native-performance-diagnosis.md).
Different source snapshots and machines are not an isolated speed comparison.

The last promotion-era ASM unit batch excluded the known
`examples_match_reference_outputs` listing mismatch: implicit module wrappers
appear in current listings but not the checked MOS listing. Older broad Rust
failure counts are not a current result. Full Rust quality-gate readiness remains
unqualified, and pre-existing native redundant-test findings need review.
The [audited-source inventory](../architecture/native-runtime-boundary-inventory-v0_1.md)
now covers relative and nested literal helper includes alongside the reviewed
module sources. Its checker and workflow tests pass; this structural review does
not resolve the broader qualification gaps above.

## Implemented responsibilities and covered subsets

These are implemented boundaries with focused proof, not whole-area parity claims.

| Area | Current responsibility and covered behavior |
| --- | --- |
| Source representation | TKVM tokenizes physical input; retained numeric captures and packed records drive replay. Original text is not re-tokenized on assembler passes. VM-selected lexical recipes preserve macro/statement substitutions and diagnostic provenance. |
| Expressions and values | Shared EXVM owns grammar. Numeric preparation and ExprVM handle supported scalar/typed values, lists/ranges, structs, nominal members and certified immutable forward dependencies. Layout aliases retain relocation provenance. |
| Controls | Shared PRVM plans drive conditionals, match arms and counted/iterable/while controls. Executed declaration ownership, iteration snapshots and named acquired collections have focused two-package controls. Preparation-known acquisition is distinct from assembly-state-dependent acquisition. |
| Templates and statements | Shared signature compilation, typed matching, head selection and existing packed template cursors support direct, imported and expanded invocations, caller labels, lexical substitutions and declaration-relative provenance. |
| Modules | Discovery/order, aliases, selected names, typed `with` snapshots and stored interfaces have focused coverage. Values evaluate at the importing site and must be known there. Selection controls visibility; whole referenced `.block` units retain internal labels and fall-through. |
| Includes and assets | Inactive/unexpanded statements do not open files. Active includes preserve parent cursors, declaration origins and definition-relative paths. Static child streams can contribute modules and `.use` edges before graph ordering. |
| Text encoding | The shared text registry, directive/emitter path and ExprVM string provider have connected proof: 24 exact positive artifacts and 28 diagnostic rejections across 6502/68020. This supersedes the earlier component-only integration gap; wider text/language parity remains unqualified. |
| Data and layout | Generic data/reservation directives remain shared core behavior. Numeric spans support sparse forward/backward `.org`, later-write overwrite, initialized payloads and independent BSS geometry. Placed/mapped sections preserve address provenance. |
| Metadata and output | Dotted fields, metadata blocks and target overlays share numeric plans and output owners. BIN/FILL vectors are preflighted before opening output files. Bin, Hex, S-record and streamed Hunk paths have focused exact controls. |
| Module storage | Eligible dependency modules publish pointer-free `.ofm` code/data, relocations and assembly interfaces; consumers demand stored entities. Producer storage retires after publication. The root stays in the shared final driver. |
| Incremental reuse | Timestamp/size freshness, tool/package/configuration identity and dependency generations guard reuse. Cold and warm linking use the same persistent objects; unchanged hits do not rewrite them. |

CPU/family/dialect semantics remain package-owned. Generic directives must not
fall through to instruction encoding. Full-family support requires the registered
pipeline and unchanged-source corpus, not a few representative encodings.

## Remaining parity and qualification gaps

### Whole-corpus evidence

The last complete frozen **86-root opcore audit** recorded **38 positive roots
with exact live Rust/reference artifacts**, **36 expected diagnostic rejections**
and **12 positive failures**. Rejection proof did not establish exact diagnostic
text parity. That audit predates later metadata repairs, promotion and storage
work; it is not a current-build coverage total.

Its unresolved fixture checklist was:

- `cli_json_outputs` and `expr_syntax` (the latter exposed mixed CPU phases).
- `linker_regions_full`, `linker_regions_minimal`, `linker_regions_no_dsection`
  and `linker_regions_pack_no_dsection`.
- `macro_syntax`.
- `module_metadata_block`, `module_metadata_output` and `module_metadata_outputs`.
- `statement_capture_types` and `statement_signatures`, localized after successful
  preparation to the missing 8085 structured no-operand candidate.

Later focused metadata and statement proofs qualify their named cases, not a new
whole-corpus result. Rerun unchanged roots against current live Rust before
updating this inventory; preserve original CPUs and examples. Historical failure
groups are not automatically established causes or current failing counts.

### Structural and language boundaries

- **Actual-state source acquisition:** guards depending on PC, instruction sizes
  or placement still need growing-source publication and a resumable assembly
  context. Frozen sessions reject changed record/context views. Preparation must
  not invent provisional assembly values. Runtime/expanded acquisition that
  introduces module or `.use` graph changes remains unsupported. Preparation-only
  effects inside retained match envelopes still need qualification.
- **Package phases:** sequential mixed `.cpu` contexts need a common numeric
  identity domain, atomic activation and retained lexical/execution contexts.
  Exchanging a package pointer would collide local IDs. Catalog/seeded-lowering
  experiments are host evidence only; they do not implement native activation.
  Keep grammars separate and package selection explicit. Initial target bootstrap
  scans the root preamble while skipping includes and conditional bodies; an
  encountered template definition inside a skipped conditional is still a
  conservative selection barrier.
- **Conditional/member ownership:** different-owner forward compound prefixes,
  mutable/contextual dependencies, additional guard-activation chains and later
  count-expanded declarations remain restricted. Preserve strict unavailable-value
  rejection, inactive alternatives and nominal schema identity.
- **Templates:** the residual decoded-string body fallback in
  `templates.rewriteCallText` still needs explicit VM-selected recipes. An operand
  such as `lea .block,a0` in an unexpanded macro can be misclassified as a labeled
  directive; the telemetry wrapper's renamed formal is a workaround, not a grammar
  repair. Named blocks in anonymous macro scopes and broader loop/macro behavior
  still require complete qualification. The migrated `imported-template-boundary`
  source control has a passing live Rust oracle but no completed native breadth
  result after bounded attempts; the cause is not localized. That gap does not
  invalidate the separately completed full self-host pair above.
- **Expressions and addresses:** supported one-base-plus-absolute-addend fixups do
  not cover multi-base/non-affine algebra or compound symbolic-address cells.
  Unsupported relocation forms must reject without losing provenance. Preparation
  expression-range/operand-wrapper policy and Rust's spelling-based expression
  path need a shared grammar/metadata audit.
- **CLI, metadata and outputs:** JSON/listing parity, LIST metadata, dynamic
  metadata values and source-context-dependent execution remain open. Connected
  origins at/above `$80000000` and terminal-PC overflow are not qualified merely
  because the standalone flat writer covers inclusive `$ffffffff`. Requalify the
  linker/output-policy fixture group and metadata-selected geometry.
- **Instruction-family cases:** the 8085 no-operand candidate, unchanged-source
  CAS2 colon-pair normalization, Motorola absolute-to-absolute byte/word MOVE,
  nested full-extension `4.W` members and bounded extra call projections remain
  reported gaps or incomplete coverage. Full FPU/MMU/AMMX/Apollo support cannot
  be inferred from focused FPU/full-extension/PFLUSH controls.
- **VM contracts:** consolidate each VM's current capabilities into its own v1,
  separating semantic kind from version. Migrate producers/codecs/executors
  together, remove superseded formats and native version fields/checks/dispatch,
  and retain package/container integrity and bounds checks. Current BS38 statement
  programs do not by themselves complete this migration.

The detailed [source-session design](native-source-execution.md),
[frontend boundary reference](compact-frontend-vm-boundary.md) and
[VM consolidation specification](native-vm-consolidation.md) describe these
boundaries. Older fixture/measurement paragraphs in those references retain their
own snapshot scope; this page owns the completion inventory.

## Completion order and next steps

Parity implementation remains paused pending the next assignment. This document
records the agreed completion order; it does not activate implementation work.

1. Finish cleanup of current contracts and supporting tooling. Keep the frontend
   refactoring separate: its combined acquisition, binding and preparation
   responsibilities need explicit ownership, not mechanical file splitting.
2. Before resuming parity, re-establish a current host/corpus baseline, triage Rust
   failures first, and select a coherent shared-opcore gap from the table above.
   Complete source/session, template, expression, output and VM-contract gaps
   without adding grammar or target semantics to generic native shortcuts.
3. Qualify shared opcore, then CPU families in the agreed order: **6502; 68000
   including FPU, MMU, AMMX, 68080/Apollo extensions; i8080; RISC-V; 6809**.
4. Qualify representative multi-module and macro-heavy projects, diagnostics,
   the supported CLI/output matrix and the intended AmigaOS/hardware floor.
   Keep resource-fit qualification separate from language/family completeness.

The latest baseline's hardware verification and a fresh separate peak-memory run
are still useful follow-ups. Optimization candidates require measured work and
unchanged-input comparisons; retained decisions and rejected approaches are in
[the performance reference](native-performance-diagnosis.md#enduring-earlier-decisions).

## Complete and qualify a slice

Use live Rust oracles, fresh case-bound completion, explicit exit status and exact
complete outputs, following the
[native parity contract](../../agents/rules/native-rust-parity-porting.md).
Timeouts, crashes, partial captures and launcher success are not completion.
Expected rejection is distinct from exact diagnostic parity. Component proof and
historical corpus counts do not establish whole-language or family parity.

Record reproducible release timing, executable/static storage and separate
instrumented heap evidence when qualifying behavior or performance changes.
Static reservation is not peak RAM. Keep benchmarks representative beyond
self-hosting, including small multi-module and macro-heavy workloads. Run focused
checks during development and broad host/corpus/full-self-host checks at relevant
completion boundaries. Preserve reference paths while evaluating replacements;
remove superseded experiments after decisions. Keep `target` only for active
batches, preserve needed deliverables outside it and clean after active work ends.

Claim complete opcore only after acquisition, templates/statements, expressions,
data/layout, modules/parameters, CLI/output, diagnostics and VM-contract gaps are
qualified against current Rust. Require full unchanged corpus and fresh complete
current-source self-host at that boundary. The 8 MiB desktop proof and original
2 MiB goal are separate; current peak accounting must not be inferred from older
snapshots. Commits are recovery points; remote pushes require explicit authority.

## Maintained documentation owners

- This page: current product status, parity evidence/limits and completion order.
- [Runtime and memory](native-performance-diagnosis.md): identified measurements,
  resource limits and retained performance decisions.
- [Module interfaces](native-module-interfaces.md): numeric assembly interfaces,
  incremental invalidation, bounded consumers and publication.
- [Per-module objects](native-fragment-preparation.md): linker payload lifecycle,
  relocation proofs, naming and rollback ownership.
- [Capture and preparation](native-lazy-preparation.md): storage policies,
  configuration/source suspension and owner retirement.
- [Source execution](native-source-execution.md),
  [frontend boundaries](compact-frontend-vm-boundary.md),
  [VM consolidation](native-vm-consolidation.md) and
  [package/shared-directive boundaries](package-execution-boundaries.md): their
  specialized contracts and open designs.
- [Native guide](../../native/README.md) and
  [reference manual](../opForge-reference-manual.md): invocation and user behavior.
- [Performance procedure](../performance/vm-efficiency.md) and
  [FS-UAE setup](../../agents/rules/fs-uae.md): measurement and execution safeguards.

Git history retains superseded experiment chronology; no archive tree is maintained.
