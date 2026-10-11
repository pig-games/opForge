# Native assembler completion plan

## Assignment and completion order

Complete current Rust assembler language, packages and assembly CLI/output
behavior through the compact native path in this order:

1. Shared opcore.
2. 6502 family.
3. Complete 68000 family, including FPU, MMU, AMMX and 68080/Apollo.
4. i8080.
5. RISC-V.
6. 6809.

Repair discovered Rust defects before qualifying corresponding native behavior.
Keep CPU/family/dialect semantics in package owners and generic directives in
shared processing. Developer tools remain separate. Continue autonomously through
coherent recovery points; consult Erik for dramatic performance or memory
regressions. Begin no optimization rounds until this completion sequence is done;
then report remaining limits and wait.

## Current evidence and limits

The fresh correctness-only opcore audit completed all 86 roots with explicit
guest completion and no timeout: 27 positive roots match exact live Rust outputs,
36 expected-negative roots reject, and 23 positive roots reject with exit 20.
Exact negative diagnostic parity is unqualified. This exposes current native gaps;
it does not qualify opcore completion or CPU-family breadth. Failing roots span
shared expression/conditional grammar, macros and typed statements, module/import
closure, section/linker/output behavior, metadata, text encoding and include/CLI
side products. These are triage categories, not established root causes. The Rust reference
audit matches 81 roots; five expected-error roots produce no partial artifacts
and therefore lack the stricter audit's artifact comparison evidence. This is
not a green whole-workspace qualification.

At `fd6e848f`, the inactive composed qualified-member case passes freshly on both
68020 and 6502; ordinary malformed `@0` controls reject. The active imported
typed-member case still fails during native assembly, after preparation. Proxy
ownership/liveness is a hypothesis pending fresh controls. Expanded repetition
with active acquisition has nine positive behaviors on each package (18 cases
total): 17 completed in the original matrix and the pre-START stall passed a
fresh retry. That result excludes the separate failing active composed-member
case. Named acquired collections and actual assembly-state/PC-dependent
acquisition remain pending. Current executable size is 646,920 bytes and linked
static reservation is 671,272 bytes; neither is peak RAM.

The latest full self-host proof is BS35, source manifest `54fb484dd0022ab5`:
121 mapped inputs (1,780,288 bytes), with exact bootstrap/native/live Rust equality
for the entire 601,196-byte Hunk. The uninstrumented release run took
1,522.60420225 seconds on FS-UAE 68020/74 MiB. This proves that identified source
checkpoint only. Later changes have focused evidence, with no new full self-host
proof. Full BS35 peak allocation, physical A6000 timing and 2 MiB fit remain
unmeasured. See the maintained [self-host proof](compact-frontend-vm-boundary.md#most-recent-full-self-host-qualification-bs35).

Source-session restructuring and VM-contract consolidation are separate pending
implementation designs. Consolidate each VM's current capabilities into its own
v1 contract, remove superseded formats and native VM version fields/checks/dispatch,
while preserving package/container integrity and bounds validation.

## Complete and qualify a slice

Use current live Rust oracles, fresh case-bound guest completion, explicit exit
status and exact complete outputs. Timeouts, crashes, partial captures and launcher
success are failures. Keep positive and negative controls; distinguish rejection
from exact diagnostic parity. Focused tests, package generation and historical
corpus snapshots do not establish whole-language or family parity.

Before each coherent production change, identify unchanged input/settings and
record release-time, executable and linked-storage impact. Use separate
instrumented runs for phase/heap evidence; static storage is not peak RAM. Keep
cache in `target` only during an active batch, preserve needed diagnostics outside
it, and coordinate `make clean` after active builds/native runs finish.

Claim opcore complete only after source acquisition, macros/statements,
expressions, data/layout, modules/parameters, CLI/output, diagnostics and VM
contract gaps are qualified against current Rust behavior. Rerun complete
unchanged corpus roots and fresh full current-source self-host at that boundary,
and review combined responsibility boundaries. Then qualify each CPU family in
the stated order, including registered pipelines and current examples. Promotion
from experimental needs the full language/target/CLI matrix, representative
multi-module projects, useful diagnostics and honest resource requirements.
Qualify the 68020/AmigaOS floor and 2 MiB product goal separately from the 74 MiB
emulator result. Commit coherent recovery points; pushes require explicit authorization.

## Maintained documentation owners

- [Compact frontend boundary](compact-frontend-vm-boundary.md): implemented subsets,
  explicit limits and identified self-host proof.
- [Native source execution](native-source-execution.md): pending source-session design.
- [Native VM consolidation](native-vm-consolidation.md): pending contract migration.
- [Package execution boundaries](package-execution-boundaries.md): shared directive
  and package responsibility boundaries.
- [Native guide](../../native/README.md) and [reference manual](../opForge-reference-manual.md):
  invocation and user behavior.
- [VM efficiency](../performance/vm-efficiency.md): measurement procedure and evidence.
- [Native parity contract](../../agents/rules/native-rust-parity-porting.md): fresh-run proof.

This page owns current status and completion order. Git history retains superseded
checkpoint chronology, experiments and measurements; no historical checkpoint
anchors or archive tree are maintained here.
