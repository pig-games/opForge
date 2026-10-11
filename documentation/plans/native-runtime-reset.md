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

The fresh frozen-image BS36/schema correctness-only opcore audit completed all 86
roots under the fresh-run protocol: 29 positive roots match all requested live
Rust artifacts exactly, 36 expected-negative roots reject with diagnostics (not
exact diagnostic parity), and 21 positive roots exit 20. Compared with the prior
full audit, exactly `text_encoding.asm` and `text_encoding_definitions.asm`
improved; no previously passing positive root regressed. The 370.69-second
correctness-batch duration includes harness overhead and is not a performance
measurement. This does not qualify complete opcore or CPU-family parity. The Rust
reference audit matches 81 roots; five expected-error roots lack comparable
partial artifacts for the stricter audit.

The 21 remaining positive failures group by fixture: five statement signature or
expansion roots (`statement_boundary_span`, `statement_capture_types`,
`statement_cross_module_ok`, `statement_expansion`, `statement_signatures`); four
linker/output-policy roots (`linker_regions_full`, `linker_regions_minimal`,
`linker_regions_no_dsection`, `linker_regions_pack_no_dsection`); three metadata
roots (`module_metadata_block`, `module_metadata_output`,
`module_metadata_outputs`); and nine roots across include/CLI, conditional and
expression syntax, macros, modules/imports and general syntax. These are observed
failure groups, not established causes. Source/session behavior and dynamic
binding ownership still require separate focused controls outside this corpus.

The numeric proxy-ownership repair qualifies direct and composed imported
typed-member references: 18 positive controls match live Rust and two malformed
`@0` controls reject freshly on 68020 and 6502. Unknown/mutable prefixes retain
possible owners; a common owner is indexed once, while differing owners reject
only when reached. Runtime block marking remains O(1), with no new heap allocation.
A subsequent five-scenario matrix matches forward fields, forward compound
aliases and mutable-kind changes on both targets (eight exact positive cases).
The longer unknown prefix declared in another physical block still rejects on
both targets; native dynamic selection between those owners remains pending.
Rust now resolves actual declaration ownership, carries typed aliases through
layout stabilization, and preserves used finalized schemas without retaining
pruned code. Its focused member/schema controls pass; the native owner matrix
continues to require the valid different-owner case to succeed.
The current acquired-include regression passes all ten positive behaviors on
each package (20 fresh exact cases), including inactive composed block references.
That matrix is separate from the composed-member controls. Named acquired
collections pass twelve fresh exact controls, including colonless
and explicit-colon labels. The final candidate checks bounded batch publication
and independently validates marker nesting and balance in inventory and runtime. Actual assembly-state/
PC-dependent acquisition also remains pending. The identified proxy-repair image
is 657,484 bytes with 681,816 bytes linked static reservation. The subsequent
collection image is 659,608 bytes with 684,524 bytes linked static reservation; neither is peak RAM.

The latest full self-host proof is BS35, source manifest `54fb484dd0022ab5`:
121 mapped inputs (1,780,288 bytes), with exact bootstrap/native/live Rust equality
for the entire 601,196-byte Hunk. The uninstrumented release run took
1,522.60420225 seconds on FS-UAE 68020/74 MiB. This proves that identified source
checkpoint only. Later changes have focused evidence, with no new full self-host
proof. Full BS35 peak allocation, physical A6000 timing and 2 MiB fit remain
unmeasured. See the maintained [self-host proof](compact-frontend-vm-boundary.md#most-recent-full-self-host-qualification-bs35).

The separate shared text-owner component matches its complete live Rust artifact
in a fresh native run. This covers builtin/custom maps, variable escapes, cloning
and lifecycle APIs. The shared packed text-grammar component separately matches
its complete 384-byte live Rust artifact, including planned/snapshot record flags
and malformed-input rejection. Source directive/emitter and EXVM encoded-string
provider integration remain pending; component proof does not qualify those paths.

Source-session restructuring is in progress; its frozen-input lifecycle has
focused native proof and is now the sole assembly driver. It does not yet support
growing records or actual-state acquisition. Its isolated unchanged release
control adds 0.76% on one sample and 952 bytes linked static storage.
Independent symbol buffers now stage allocation and commit their views/count
atomically. Fresh component and production matrices each pass twelve exact
positive cases and two malformed rejections. Stress controls preserve cells and
pointers across all five injected allocation failures, support prepared keys and
lazy kinds, and exercise the 65,536-symbol bound. The unchanged release control
changes from 27.064863 to 26.944213 seconds (-0.45%, one sample), with 568 more
linked static bytes. Frozen session pins remain; actual-state acquisition,
relocatable source views and peak heap are not yet qualified.

Record-relative loop positions and refreshed owner views now pass twelve fresh
exact collection cases and two malformed-input rejections through the production
CLI. The unchanged release control changes from 26.848584 to 27.116166 seconds
(+1.00%, one sample), with 504 more linked static bytes.

Rust conditional declaration ownership is repaired in `aaca8086`: each layout pass
rebuilds executed ownership, references, reachability and schema uses. Ninety-eight
focused tests pass. Possible longer `.const` declarations under unresolved guards
can defer initial evaluation without granting executed ownership or visibility.
The native schema replay repair qualifies unconditional longer declarations while
pruning their unused schema-owning code. Its fresh 26-case matrix has 14 positive
exact matches, six diagnostic rejections and six remaining forward-guarded positive
failures; executed conditional ownership remains pending. This repair alone changes
the unchanged release control from 33.11 to 32.80 seconds (-0.9%, one comparison),
with 564 more linked bytes. UTF-8 preservation in Rust
macro preprocessing is repaired in `a616140f`, with all 26 preprocessing tests and
all 211 opcore library tests passing. BS36 text emission/provider integration passes 52 fresh native controls: 24 exact
positive artifacts and 28 diagnostic rejections across 68020 and 6502. Its isolated
unchanged release control grows from 27.42 s to 33.11 s (+20.8%, one pair); linked
storage grows 13,464 bytes (+2.0%), and instrumented owned-memory peak is unchanged.
See [source execution](native-source-execution.md) for scope and measurement limits.
No new full self-host or complete opcore claim is made.

VM-contract consolidation remains a separate pending design.
Consolidate each VM's current capabilities into its own
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
