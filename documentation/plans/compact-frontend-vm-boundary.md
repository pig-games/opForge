# Compact frontend: VM boundary correction

Status: numeric normalization, composed-name recipes and the PRVM boundary/resume
foundation, macro descriptor services, compact descriptor storage integration,
ordinary macro/segment string fragment recipes, and generated-call argument
re-tokenization are implemented. The residual decoded-string fallback and
expression correction remain active. Counted packed `.for` replay now passes
focused real-native comparison; iterable `.for` and `.bfor` remain unsupported.
The remaining frontend boundary work and next self-host frontier are tracked
alongside the [native reset](native-runtime-reset.md#fixed-input-allocation-slice).
Speculative shortcuts are deferred until completed native self-host proof.
Measured preparation bottlenecks may be addressed to shorten convergence runs.

Current measured frontier: the frozen 61-file input rejects at native origin
`0x35`, line `0x240` (576), on 68020 / 10 MiB. The origin is a discovery/include
ID, not an index into the sorted Rust manifest; its path still needs mapping.
The entry-file BSS-to-struct transfers now pass focused exact Hunk comparison.
See [match facts after export rejection](#match-facts-after-native-export-rejection)
for the separate change measurement and remaining immediate-transfer probe.
No native self-host Hunk exists. Older 59-file measurements below use a different
input; the current frozen identity is recorded in
[the scalar-root slice](#complete-scalar-roots-and-target-predicates).

Earlier convergence and memory observations: the compact native CLI accepts concrete code and
data sections reopened by another module, with a focused 68020 / 2 MiB Hunk
matching Rust exactly. The earlier isolated TKVM graph's binding rejection was
not a valid frontier: that subset lacked a root `.output` declaration and Rust
also rejected it. In the valid 59-file self-host graph, a 2 MiB instrumented
run reached `tkvm_runtime.asm` and showed an AmigaOS allocation refusal while
growing a 64 KiB preparation block to 128 KiB. After exact sizing for fixed
preparation and CLI discovery buffers, a ten-minute-limit run with gated
progress capture exited at about 154 seconds. It read 168,728 of 667,179 staged
source bytes before AmigaOS refused a 262,144-byte growth request for a
131,072-byte preparation block with 131,067 bytes used. The last completed
source boundary was source ordinal 4; the active source had ordinal 40, and
the rejection was reported at include origin 60, line 328
(`.TOKEN_SCOPE_END #0`). The diagnostic source-path field was malformed, so the
line identifies the failure site but not a trustworthy path. Memory telemetry
recorded one allocation failure and a peak of 1,083,256 tracked bytes against
1,121,200 bytes free at entry. The small compact CLI search-roots case still
matches Rust under 2 MiB. After functional self-host parity, identify the owner
and lifetime of that growing block and reduce its peak without narrowing source.
There is no completed native self-host artifact or full-run timing claim.

For functional convergence, the runner now also supports a 68020 profile with
2 MiB chip and 8 MiB fast RAM. On that profile the instrumented 59-file case
passed source selection, binding and packed-record materialization, but timed
out at ten minutes between the post-materialization and post-block-selection
progress markers. The ordinary build also timed out at ten minutes without a
guest completion marker or output. A focused 128/513-block import chain, with
only the first block selected and an unreachable final block, matches Rust's
output exactly on native. With gated memory telemetry, native finalization took
1.331/13.737 seconds respectively, while tracked peak ownership was
820,056/832,344 bytes. About four times as many blocks took about ten times
as long. These are instrumented relative timings, not release self-host timing.
An experimental numeric-ID lookup did not improve this case and was discarded;
the repeated reachability sweeps were the next scaling cause to address. The
native selector now queues each newly reachable outer block, scans records
outside blocks once and scans each queued block once, reusing its old nesting
stack as the queue. Fresh exact native parity covers the import chain, a cycle,
preceding labels, entry roots and selected imports. The same gated finalization
measurement fell to 0.915/7.223 seconds for 128/513 blocks; queued counts were
128/513 and scanned packed records 393/1,548. A renewed uninstrumented
59-file self-host run timed out at ten minutes. With a two-hour safety bound,
the same exact-output run reached a real native exit after 2,816.57 seconds of
host test wall time. It rejected physical file 1, hexadecimal line `0000003A`
(decimal 58, `addq.l #1, IncludeCount` in the entry file), without producing a
Hunk. A focused fresh Rust Hunk oracle accepts `ADDQ.L #1,reserved` with a
CODE-to-BSS relocation, while compact native exits 20 at that instruction;
`ADDQ.L #1,payload` targeting DATA failed on native as well. The package
lowerer had treated a diagnostic suffix as part of the fixup operand. It now
separates that suffix as the Rust executor does; focused native BSS and DATA
cases match Rust exactly. The next full run reached a fresh guest exit after
2,816.79 seconds of host test wall time, at file 1, hexadecimal line
`00000061` (decimal 97, `move.l #UsageText,d1`). Rust emits a CODE-to-DATA
relocation for that immediate address. The compact package now has a distinct
atomic-target match projection, and native fixup identity binding strips the
immediate marker before reading the symbol. BSP8 is the sole current package
format; no earlier executor remains. Focused native exact-output cases pass for
the immediate DATA address and a relocation-free numeric immediate. A renewed
full self-host comparison is still required. Neither completed guest exit was
a successful self-host performance measurement. The remaining repeated
label-to-block search in `mark` is a deferred performance hypothesis, not a
claim that it explains this runtime.
The expanded profile is a parity aid, not a revision of the 2 MiB product goal.

## Assembly position capture

Optional preparation progress now captures the assembly pass and section sweep
at their owned boundary, then reports the final packed-record byte offset and
total on failure. The decoder distinguishes a Hunk section sweep from the later
outside-section scan and rejects impossible or uninitialized active-pass counts.
This is scan position, not a percentage of completed assembler work: section
filtering, two passes and loop replay change the work represented by each byte.
The binary MEMD schema is unchanged. The snapshot is 20 bytes and requires all
three instrumentation gates; ordinary builds emit no snapshot code or storage.

The first capture exposed a Rust host Hunk-relocation defect: an absolute
`Base+Struct.Field` operand can be emitted without its base relocation. The
probe's apparent pass/sweep and zero remaining fields are discarded. Capture
and reporting now load bare base labels with `lea` and use relative fields.
The existing record-count progress sites use the same reusable mechanism.
The subsequent Rust repair below fixes this address-addend classification;
the capture slice itself changes telemetry addressing only.

A fresh 68020 / 2 MiB negative Hunk probe, accepted by the live Rust assembler,
rejects a memory-to-memory `move.w` on native. Its corrected capture reports
pass 1, section sweep 2 of 4, section ID 1, and record byte 42 of 144 (29.17%).
An expected negative completion is not successful artifact parity. The source-
end count for the full probe's file `0x27` uniquely matches the frozen
`tkvm/tkvm_scanner.asm` among the staged files; its line 112 also contains a
memory-to-memory `move.w`. This is supporting localization, not a direct path
capture.

The fresh full frozen-source run completes preparation and explicitly exits 20
at the same file `0x27`, line `0x70`. Its snapshot reports pass 1, Hunk mode 5,
raw sweep 9, section ID 3, and section sweep 2 of 4 (`code` in the root output
order `entry,code,data,bss`). The record offset is 9,765 of 509,604 bytes,
or 1.916% through that scan. The entry-section sweep is complete; the rest of
the code sweep, data/BSS sweeps, outside-section controls and pass 2 remain.
This cannot be converted to an overall completion percentage.

Native-runner wall time with memory and preparation progress enabled is
208.090 seconds. The preceding ordinary run was 197.438 seconds; their
10.652-second difference includes all instrumentation and run variation, not
an isolated cost for this capture. No release speed change is claimed: the
ordinary executable remains byte-for-byte identical at 87,516 bytes. The
instrumented image is 91,544 bytes with 104,276 bytes linked reservation.
This full probe uses 2 MiB chip plus 8 MiB fast RAM and has no successful guest
completion time or native self-host output.

Qualification: the three decoder tests and all eight telemetry macro tests
pass. The fresh negative native capture and positive compact CLI control run
under 2 MiB; the positive control completes with exact Rust output (5 bytes).
Formatting, native instrumentation safety, fresh-proof contract, test ownership,
runtime-boundary contract and workflow links pass. This is capture qualification,
not full native parity or broad project qualification. The memory-to-memory
MOVE rejection is the next localized native instruction capability to investigate.

## Rust Hunk named address addends

Rust's absolute-address relocation classifier now recognizes the existing
absolute-constant model instead of accepting only literal syntax for an addend.
`base+offset`, `offset+base` and `base-offset` retain the base's relocation when
the offset is a constant equate or struct field, including a qualified field.
The section-relative value and CODE-to-BSS relocation are checked together for
loads, stores and `lea`; constant-only operands and register displacements remain
unrelocated. No CPU/package semantics or native execution code changes.

The address tests cover placed 68000 and unplaced 68020 sections. The source
audit confirmed one remaining debug failure-report call at `binary_app.prepare`
using `Records+memory.Block.Used` through `MEMORY_LAYOUT`. Its relocation is
covered by the repaired classifier. Other scanned candidates were constant-only
layout sums or register-relative fields; this audit is not an exhaustive proof
over every macro expansion.

### Follow-up: stable placement and complete relocation proof

The three broader probes now pass as active regressions:

- Placed 68020/030/040 symbols are rebased from the section origin recorded when
  each label is captured. This avoids adding a seeded section base twice and
  does not rebase symbols that were not redefined during the current pass.
  CODE/DATA/BSS cross-section references are checked after stabilization.
- `MOVE.L #address+offset,Dn` has a general expression package descriptor with
  the same absolute-long fixup as the atomic descriptor. The atomic matcher is
  retained because it is already supported by compact package projection.
  Named offsets relocate; same-section address differences are absolute. The
  generated all-family native package is synchronized (+35 bytes in `CMSE`).
  A signed-scalar regression also exposed a profile/pass mismatch in direct
  `AsmLine` use: pass2 now evaluates instead of inheriting initial-pass deferral;
  engine layout-stabilization deferral is preserved.
- The fixup VM reports absolute inputs that lack a representable relocation
  base. The selector combines this with the assembler's constant provenance and
  carries the result as internal instruction effects. Hunk output rejects unsafe
  arithmetic even alongside a valid fixup; flat binary retains numeric encoding.
  This is generic provenance metadata, not CPU-specific logic or a new serialized
  VM version. Both normal and VM-only builds use the same constant classifier.

This does not establish compact-native support for complex immediate expressions
or full Hunk expression completeness. The next native self-host frontier remains
subject to fresh execution proof.

The first named-addend repair's uninstrumented frozen 59-file self-host probe
completed with the existing expected rejection at file 39, line 112. Under the
same 68020/10 MiB profile it took 198.100 s, versus 197.438 s before that repair
(+0.662 s; one run each, no speed claim). Its ordinary CLI remained 87,516 bytes
with 98,580 bytes linked reservation; the package was 294,232 bytes. Source size
was 674,295 bytes and the Rust Hunk oracle was 86,396 bytes. This is time to a
reported failure, not successful native self-host assembly time.

The follow-up repair's fresh uninstrumented run reports the same expected
rejection at file 39, line 112 in 196.372 s, separately compared with 198.100 s
above (-1.728 s; one run each, no speed claim). The package is 294,360 bytes
(+128 bytes for the general expression descriptor). The ordinary executable is
byte-for-byte unchanged, with the same 98,580-byte linked reservation. Inputs,
profile and Rust oracle sizes are unchanged. Two fresh native bare-address and
numeric-immediate Hunk controls complete with exact Rust output.

Focused qualification passes: all 80 linker/Hunk tests, the placement and
address/arithmetic probes, all 464 VM tests and all 214 family tests. VM-only
instruction probes and same-section immediate differences pass; that feature's
existing shared `.long` relocation stub remains a separate limitation. Runtime
boundary, instrumentation safety, fresh-native proof, test ownership, benchmark
selector and workflow-link checks pass. Workspace Clippy remains blocked by two
existing `nonminimal_bool` findings in `packed_macro_vm.rs`; the full architecture
scan flags ten terms in unchanged native files. Neither is waived as a green
qualification result. The staged architecture check passes with no enforced
errors. The final `cargo test --workspace --lib` assembler result is 1,819 passed,
69 failed and 290 ignored; its 69 failure names match the earlier named-addend
checkpoint exactly. The follow-up introduces no additional failures in that run,
but the broad suite remains unqualified.

Run `cargo test -p asm --lib native_hunk_struct_constants` for address, immediate,
constant and negative arithmetic cases, `cargo test -p asm --lib
placed_section_symbols_rebase_once` for stabilization, and `cargo test -p asm
--lib linker_output_hunk_` for the affected output subsystem.

## Automatic branch package translation

The unsuffixed `beq` frontier was a compact package-producer gap. Canonical
semantic branch plans carry `auto` in their requested-candidate field, but the
compact lowerer treated it as an unsupported projection. Preparation now maps
that field to the existing numeric SEMV automatic-request sentinel, `-1`, while
preserving the package program, opcode and automatic class. The four-field
branch contract is validated before lowering; `auto` in other fields or a
non-branch input plan remains unsupported. Native execution and the package format were
unchanged in that slice, with no mnemonic-specific shortcut or legacy package path.

The package's automatic policy chooses word width for a nearby target too;
short width requires an explicit suffix. Fresh 68020 / 2 MiB native cases
match live Rust bytes for near and far automatic forward branches, including
the `bhs` alias, and an explicit short control. The far automatic output also
matches the explicit-word Rust output. Before the producer fix, a focused bare
`beq` case completed with exit 20 at that instruction. Afterward, the focused
cases complete with exit zero and exact output. This is capability repair,
not a measured speed improvement. The m68020 runtime package grows from
292,504 to 294,232 bytes (+1,728) because the automatic recipes are now retained.

Run `cargo test -p vm --lib binary_source_package` and
`cargo test -p asm --lib compact_branch_width_rust_oracles` for the producer and
Rust checks. With configured FS-UAE, run
`cargo test -p asm --lib compact_branch_ -- --ignored --nocapture --test-threads=1`
for the three native comparisons.

The renewed uninstrumented 59-file probe on the expanded 68020 profile passes
the original `beq` frontier and explicitly exits 20 at file `0x27`, line `0x70`
(decimal 39 and 112). The physical path still needs localization: discovery
ordinals must not be inferred from the sorted Rust dependency list. Native-runner
wall time is 197.438 seconds, against 196.699 seconds before this repair
(+0.739 seconds), with a different stopping point. No speed ratio or completed
self-host output is claimed. Source size remains 674,295 bytes, the Rust Hunk
oracle 86,396 bytes, native image 87,516 bytes and linked reservation 98,580
bytes. This run uses 2 MiB chip plus 8 MiB fast RAM, rather than the product's
2 MiB target.

Qualification: 14 package-producer tests and 149 affected assembler tests pass,
along with the three fresh focused native comparisons. Formatting, native proof,
runtime boundary, test ownership, benchmark-selector, supply-chain and workflow
link checks pass. The architecture guard retains ten findings in unchanged
native files; broad qualification is not claimed. Localize the new full-input
rejection before selecting the next structural parity slice.

## Buffered physical-line collection

Disk reads were already buffered in 4,096-byte blocks, but collection called the
preserving `readByte` helper for every character and updated global cursor,
line-length and source-byte counters each time. The hypothesis is that retaining
those values in registers throughout a physical line removes repeated call and
memory traffic without changing lowering or language semantics. The new
`binary_line_input` owner consumes the existing buffer, refills it only when
exhausted, and returns the copied length and consumed-byte count. LF is consumed;
CR remains for the existing lowering owner. The buffer size and 4,096-byte
physical-line bound are unchanged. The byte reader remains for discovery scans.

The app and include stack share the reader state. Include push/pop still saves
and restores the parent handle and unread buffer span; path search, source
selection, tokenization and packed lowering remain in their existing owners.
The runtime state uses real storage labels: numerical aliases to labels did not
retain the Hunk relocation information needed by the loaded Amiga executable.
These are transient I/O pointers, with no change to persisted binary-source or
package formats.

Use the same frozen 59-file input, package, Rust oracle and expanded 68020
profile as the template-index experiment. Compare the indexed byte-reader
checkpoint against the integrated line reader, separately from telemetry.
Acceptance requires focused exact Rust/native output, capacity rejection and the
same full-input stopping point. Stop if correctness changes or the measured
result does not justify the new responsibility. `OPFORGE_INPUT_DETAIL=1` with
memory telemetry isolates collection before lowering; its counters exclude
module-discovery reads. The sole current record is MEMD, 2,280 bytes; earlier
decoders are removed.

Fresh 68020 / 2 MiB native proof covers a full-capacity physical line, LF at the
last and first byte of refills, CRLF split between refills, an instruction split
between refills, blank lines, an include without final LF followed by buffered
parent input, and a main file whose final unterminated directive emits bytes.
A separate completed negative case rejects a 4,097-byte physical line at file 1,
line 1. Run `cargo test -p asm compact_physical_line_ -- --ignored --nocapture
--test-threads=1` with the configured FS-UAE environment;
`compact_physical_line_boundaries_rust_cli_oracle` checks the live Rust oracle.

The matched collection-only profile records 35,257 attempts, 760,743 consumed
bytes and 304 DOS reads on each revision. Collection falls from 15.068 to 6.341
seconds: 8.727 seconds saved, or 57.9%. These are instrumented E-clock scopes,
including probe cost, not release timing. The source-I/O-and-other stage falls
from 35.200 to 26.543 seconds; it contains collection and must not be added to it.
Tokenization remains 64.454/64.422 seconds and module discovery 12.764/12.775.
Both profiles reach the same `beq` rejection, retain 509,604 packed bytes and
peak at 3,247,768 tracked bytes, with balanced cleanup and zero profiling errors.
The matched ordinary builds take 205.223 and 196.699 seconds of native-runner
wall time: 8.523 seconds saved, a 4.15% reduction for this change alone. These
single observations include native executable construction and emulator startup,
exclude the Rust oracle build, and time the same explicit rejection rather than
completed self-host assembly. Both use the same 292,504-byte package and
86,396-byte Rust Hunk oracle. The comparison images are 87,292/87,516 bytes,
with linked reservations of 98,360/98,580. The instrumented reference snapshot
has a four-byte error-exit clock-cleanup stub even with probes disabled; that
stub is not executed by this workload. Relative to the unmodified index
checkpoint, the integrated release image grows by 228 bytes and linked
reservation by 224. There is no release input-probe code or storage.

Qualification: 144 affected Rust tests and seven telemetry gating/transparency
tests pass. Both focused native boundary cases pass with and without input
telemetry under 68020 / 2 MiB, as do
the expanded-profile before/after full-input probes. Instrumentation safety,
evidence classification, native proof contract, runtime boundary contract,
test-module ownership, benchmark-selector, supply-chain and workflow-link
checks pass. All changed native assembly formats cleanly. Broader qualification
remains limited by ten unchanged enforced architecture findings and a stale
inventory entry for unchanged `tkpkg_value_execution.asm`; the linked CLI
formatter still reports one unchanged file would change. This is a preparation
optimization with the existing self-host rejection unresolved.

## Template candidate index

The full-input preparation profile exposed repeated scans over every retained
template, including ordinary dot statements that are not template calls. On the
frozen 59-file, 674,295-byte input, the reference performs 443,406 failed role
candidate checks and examines 468,115 candidates in template-line selection.
The hypothesis is that preparation-owned indexing can remove this repeated work
without changing parsing, visibility, lexical selection or import semantics.

`binary_template_index` owns insertion-ordered leaf buckets and a separate exact
numeric-name index. Scopes supplies a folded final-component bucket; collisions
still undergo the existing candidate and lexical-distance checks. Definition
order preserves the first equal-distance match. A read-only named-import hint
covers aliases with a different leaf; `resolveTemplate` remains authoritative
for visibility, public declarations and selection. Only successful definitions
are published. The index contains numeric IDs and index-plus-one links, and is
released with the other preparation pools. No package or executable binary-source
contract changes. Fixed scratch is 1,552 bytes, with six bytes per definition
before geometric pool rounding.

The same Rust oracle, package and frozen source graph are used before and after.
The test-only `OPFORGE_COMPARE_NATIVE_ROOT` override builds a preserved native
revision without changing the guest input. Compare ordinary builds separately
from detailed telemetry. Acceptance requires exact focused outputs, retained
negative cases and the same full-input stopping point; a changed or incomplete
frontier cannot establish a comparative gain.

On the same 68020 / 2 MiB chip + 8 MiB fast FS-UAE profile, uninstrumented
native-runner wall time falls from 490.734 to 204.977 seconds: 285.757 seconds
saved, a 58.2% reduction or 2.39× observed gain for this index change alone.
These are single matched observations, include native executable construction
and emulator startup, and exclude the preceding Rust oracle build. They are
time to the same explicit rejection at file `0x2c`, line `0x18` (`beq`), not
completed self-host assembly or physical-machine timing. Both runs use a
292,504-byte runtime package and the same 86,396-byte Rust Hunk oracle.
The release executable grows from 86,408 to 87,288 bytes (+880), with linked
reservation from 97,544 to 98,356 (+812).

Fresh focused native proof covers nearest lexical templates and ASCII case
folding, exact qualified identity rejection, duplicate declarations, exported
macros, pool growth, nested expansion and colliding leaf names `M008`/`M080`.
The growth case emits all 2,835 Rust bytes. A renamed-import regression emits
the independently expected byte on both native revisions; Rust currently
rejects that selected renamed-macro invocation, so this is not Rust parity.
Reproduce focused checks with `cargo test -p asm compact_template_ -- --ignored
--nocapture --test-threads=1` and the configured FS-UAE environment. For the
full comparison, set `OPFORGE_SELF_HOST_SOURCE_ROOT` to the same frozen source
root and run `compact_cli_self_host_entry_readiness_fs_uae`; the default negative
probe does not claim successful self-hosting. Add `OPFORGE_COMPARE_MEMORY=1`,
`OPFORGE_PHASE_ONLY=1`, `OPFORGE_BINDING_DETAIL=1` and `OPFORGE_TEMPLATE_WORK=1`
only for the separate attribution run.

The matched detailed run reaches the same rejection in 258.138 seconds versus
543.689 before indexing. Instrumentation perturbs both runs; the ordinary
204.977-second observation above is the performance result. The principal
preparation boundaries are:

| Instrumented boundary | Before (s) | Indexed (s) |
|---|---:|---:|
| Binding and raw records (whole stage) | 355.175 | 87.916 |
| Initial line plan | 82.487 | 8.484 |
| String line plan | 73.910 | 3.789 |
| Template dispatch | 120.983 | 6.547 |
| Source-line writer including binding | 34.481 | 34.488 |
| Conditional/scope/import processing | 16.366 | 16.450 |
| Tokenization (whole stage) | 64.475 | 64.425 |
| Module discovery (whole stage) | 12.480 | 12.493 |

Failed role candidates fall from 443,406 to 791 (99.82% fewer); line candidates
from 468,115 to 1,816 (99.61% fewer). Outcomes remain 184 invocations, 2,994
body captures, 296 definitions, 1,672 plan VM runs and zero string-plan captures.
Binding calls remain 76,780, with 1,200 timed samples estimating 26.624 seconds
versus 26.566 before. Both retain 509,604 packed-source bytes and read 760,743
source bytes including includes/reloads. The sample estimate overlaps writer
time; boundary rows overlap whole stages and must not be added together.
Peak tracked ownership grows from 3,244,168 to 3,247,768 bytes (+3,600), with
balanced allocation/free totals, zero live ownership and zero profiling errors.
This remains expanded-memory convergence evidence, not 2 MiB self-host proof.

Qualification: 143 affected Rust tests and six telemetry gating/transparency tests
pass. Fresh wildcard, selected and qualified-alias template imports match Rust
under 68020 / 2 MiB. Changed native source formatting, instrumentation safety,
proof-contract, evidence classification, test-module ownership, benchmark-selector,
supply-chain and workflow-link checks pass. Broader gates remain blocked by ten
unchanged enforced architecture findings and nine missing ownership annotations
in unchanged files; the linked CLI formatter also reports one unchanged file
would change. This slice does not claim repository-wide qualification.

## Finding

The experimental compact CLI uses TKVM for initial tokenization and ExprVM for
expression evaluation, but does not make its entire parsing path VM-controlled.
Correct output on tested inputs does not prove the architectural boundary. This
was understated when preparation was described as just binary lowering.

The [canonical boundary](../vm-boundary-protocol-v1.md#3-canonical-boundary-matrix)
permits host-owned bootstrap and macro expansion. It assigns line tokenization,
statement/operand parsing and covered mathematical expression parsing to the VM.
[Package-controlled execution](package-execution-boundaries.md#outcome-and-scope)
allows substantial shared primitives: every character operation need not become a
small bytecode. The issue is who selects the grammar and owns its contract, not
whether a native routine contains branches.

## Inspected responsibilities

| Component | Current behavior | Assessment |
|---|---|---|
| `binary_frontend.line` | Runs TKVM, then calls the writer and preparation machinery. | Real VM tokenization, followed by additional native grammar. |
| TKVM `ComposeNames` / writer recipe branch | Package-selected placeholder policy produces bounded composed-name recipes. The writer validates extents and copies them. | VM-controlled lexical recognition; the three superseded writer parsers are removed. |
| TKVM `NormalizeNumbers` / writer numeric branch | Package-selected spelling rules produce an unsigned 64-bit value or deferred invalid/overflow metadata. The writer copies values that fit its existing u32 representation. | Literal normalization is VM-controlled; the superseded `binary_source.parseNumber` is removed. |
| `binary_source.literalString` | Copies bytes already decoded by TKVM. | Appropriate packing; no duplicated escape parser. |
| `binary_source.nameOperand` and binder | Classify the package-owned numeric-looking `.cpu` name and resolve identifiers to IDs. | Context and binding are necessary, but normalized-token changes must preserve this name/value distinction. |
| `binary_source.appendPlan` / `binary_macro_plans` | Copies VM-selected descriptor/spelling regions and appends an offset handle. | The raw call-region sidecar is removed; captured invocation plans support the retained spelling consumers; bound core directives do not receive invocation plans. |
| `binary_templates.rewriteCallText` | Still consumes decoded-string bytes in the residual body-token fallback. Captured generated calls now bypass it. | Explicitly unfinished until the remaining caller is replaced; scratch is not persisted as executable source. |
| `binary_templates.expandComposite` | Joins literal/argument spelling fragments and binds the generated name, or emits string bytes. | Generated names need spelling during preparation; this need not be source reparsing if recipes are explicit and already recognized. |
| `binary_expression.compile` | Implements precedence and associativity via `bitOr`, `product`, `power`, `unary`, `primary`; emits and folds expression bytecode. | A native mathematical parser outside the EXVM parser contract. |
| `binary_expression.evaluate` | Executes the compact expression through shared ExprVM. | VM evaluation; it does not establish VM-owned compilation. |
| `binary_prepare` | Recognizes statement and operand wrappers and chooses expression ranges. | Needs a separate PRVM/package-boundary audit when migrating expression compilation; not presumed correct from token IDs or package register IDs alone. |

Source anchors: [writer](../../native/motorola68000/amigaos/experimental/binary_source.asm),
[frontend](../../native/motorola68000/amigaos/experimental/binary_frontend.asm),
[templates](../../native/motorola68000/amigaos/experimental/binary_templates.asm),
[expressions](../../native/motorola68000/amigaos/experimental/binary_expression.asm),
[preparation](../../native/motorola68000/amigaos/experimental/binary_prepare.asm).
This is a bounded frontend audit, not an exhaustive encoding or language audit.

## Remaining lexical boundary

Rust `PortableTokenKind::Number` now carries optional normalized u64 metadata.
Native TKVM retains its 20-byte records, using the former reserved word for
numeric status and offsets into scratch for spelling followed by an eight-byte
value. Package opcode `0x13` selects normalization and its ordered radix rules.
The scanners remain deliberately permissive. Invalid and overflow metadata are
deferred until a value is required, preserving numeric-looking names and macro
fragments. Strings already carry decoded bytes. Composed-name and ordinary
macro/segment body string recipes are now VM metadata. Captured generated calls
also re-tokenize VM-bound fragments; the residual decoded-string path still
needs migration. The ordinary Rust expression path still uses core token
spelling; it does not yet consume the portable numeric metadata.

Default identifier continuation includes `@`, so `label@1` can be one identifier;
`@1suffix` can be `At` plus the permissive number spelling `1suffix`. Neither
represents an explicit substitution recipe. Normalization must distinguish these
forms before rejecting them as malformed ordinary numeric literals. It must also
preserve numeric-looking package names and ordinary numbers on the same line.

References: [portable tokens](../../crates/opforge-vm/src/portable_contract.rs),
[Rust scanners](../../crates/opforge-vm/src/tokenizer_runtime_utils.rs),
[native scanners](../../native/motorola68000/amigaos/tkvm/tkvm_scanner.asm),
[program generation](../../crates/opforge-vm/src/builder.rs),
[opcode contract](../../crates/opforge-package/src/package.rs).
The compact BSP4 preparation capsule embeds TKVM and macro descriptor programs; it does not embed an
EXVM expression-parser program. Existing canonical expression bytecode produced
by its native compiler must not be confused with that missing parser program.

## Proposed sequence

1. **Normalized lexical contract.** Define and implement VM-controlled literal
   normalization and explicit placeholder/composite lexical forms in Rust and
   native together, with package generation selecting the operation and policy.
   Preserve decoded strings, spans, numeric-looking names, macro syntax and the
   canonical scalar value domain. The current packed u32 field is a representation
   constraint, not permission to narrow the language globally. Begin with the
   numeric-value part as a bounded implementation checkpoint, accounting for
   placeholder/name contexts before conversion. The writer then packs values and
   binds names; remove its superseded literal parser when that seam is proven.
2. **Binary substitution recipes.** Finish explicit recipes for positional,
   named, full-list and embedded substitutions. Preserve observable spelling where
   textual substitution requires it, as bounded literal fragments owned by the
   binary format. Remove the raw call-region sidecar and its character-scanning
   substitution path. Retain generated-name interning, template storage and
   invocation state on the host. Resolve context-sensitive placeholders through
   the parser/macro contract rather than a context-blind tokenizer rule.
3. **VM-controlled expression compilation.** Select the covered grammar through
   EXVM/package contracts over packed tokens; retain compilation-once, folding and
   compact runtime evaluation. Audit the PRVM expression-range/operand wrapper
   seam with it. A new shared primitive is acceptable if its contract is explicit
   and package-selected; merely moving the recursive parser into a VM-named file
   or wrapping an uncontrolled callback in an opcode is not a correction.
4. Resume loop parity on explicit binary records with these boundaries recorded.
   Do not extend the current ad hoc text grammar to get past the self-host frontier.

For each implementation checkpoint compare Rust/native normalized records,
acceptance, diagnostics and final bytes on identical inputs; include malformed
literals, adjacency, quoted/embedded placeholders, name/value contexts and exact
expression precedence as relevant. Run bounded fresh native proofs under 68020 /
2 MiB and the unchanged release control. Record image, package, peak ownership and
unprofiled timing deltas. Stop and discuss a change that requires a larger grammar
or storage redesign rather than hiding it in a local helper.

Before 1.0 migrate the latest affected contracts and consumers together; do not
add legacy executors. Packed records contain offsets, not memory pointers.
The initial audit changed documentation only. The numeric checkpoint below
changes the latest TKVM contract and both consumers together; it introduces no
legacy executor. Explicit composite recipes and VM-controlled expression
compilation remain active work, rather than implied completion of the boundary.


## Numeric normalization checkpoint

TKVM opcode `0x13` selects an ordered package table of prefixes, suffixes,
radices and terminal-body flags. Both interpreters validate the table and produce
checked unsigned 64-bit values or deferred invalid/overflow metadata. Native
records retain their 20-byte layout; values occupy eight additional scratch bytes
following a copied spelling, reached through offsets. The writer packs normalized
values and enforces its existing u32 limit. It no longer interprets literal text.
The default fast Rust tokenizer uses the same operation and matches generic
execution including logical step accounting.

Fresh 68020 / 2 MiB native proof matches 24 live generic Rust numeric records,
including u64 limits, overflow, separators, alternate spellings and overlapping
prefix/suffix rejection. Mixed compact assembly matches all 44 output bytes.
A 14-byte scratch probe on `1 2` returns status 3, cursor 2, committed extent 11,
first value 1 and a still-raw second record. An independent Sol review found and
helped repair terminal-rule precedence and capacity-failure publication; final
review found no remaining actionable issue.

Reusable telemetry advances to MEM6: 20 opcode counters and 400 adjacent pairs
in a 1,916-byte record, 160 bytes more than MEM5 in instrumented builds only.
The positive mixed proof reconciles opcode/pair counts and reports zero profiling
errors and zero live tracked ownership after cleanup. Normalization contributes
to helper time; it does not inflate committed token or spelling-work counters.
Release builds retain no telemetry code or storage.

VM library tests pass 438/438 and package library tests 101/101. A broader
assembler library run reports 1,764 passed, 61 failed and 214 ignored; all 61
failures reproduce with the pre-change test executable. This is not a full
repository qualification claim. The generated default package also had existing
1,521-byte drift before this slice; refreshing it incorporates that drift plus
260 bytes for the four normalization programs. The smoke package grows by 65
bytes. No unrelated example or output goldens were regenerated.

Caller storage now reserves the previous spelling budget plus worst-case copied
number spellings and eight value bytes per token. Compact scratch grows from
1,024 to 2,560 bytes (+1,536 required scratch extent). This remains within
the existing geometric arena allocation on the measured alias cases. Each of
TKPKG's two shared scratch buffers grow from 256 to 1,024 bytes (+1,536 bytes combined for consumers
that link those buffers). The compact release linked reservation above does not
grow from this buffer change. Consumers use the same named bounds; the shared rejection buffer must grow with its bound.
This is provisional storage, not a claim of optimal numeric packing.

The unchanged unprofiled release control (84,687 source bytes, 121 templates)
produces all 1,701 Rust-identical bytes in 9.116 s, versus 8.862 s previously:
an observed 2.9% increase in single runs, not a statistical regression estimate.
The release Hunk grows from 71,712 to 72,344 bytes (+632), with linked reservation
from 82,936 to 83,560 (+624). Its m68020 capsule grows from 269,162 to 269,226
bytes (+64). Full-width literal native output also matches all 60 bytes.

Focused final host checks verify both generated package fixtures, numeric oracles,
numeric CPU aliases and release-transparent telemetry. The digest pin matches
the refreshed live package; its existing combined source-contract test then
fails at an obsolete compact-table version assertion. That source-contract
issue remains in the baseline failure set. Native proof and instrumentation
guards, their nine Python tests and the linked-source formatter pass. Workflow
links, selector and supply-chain checks pass; the architecture gate retains its
ten existing enforced findings, 25 enforced warnings and 565 outside warnings.

Final numeric-alias runs match Rust on both CPUs with zero live tracked ownership
and zero profiling errors. Peak ownership is 531,888 bytes for m68020 and
153,048 for m6502: 64 bytes above the identical allocation-checkpoint inputs,
matching capsule growth. The larger scratch extent stays inside existing arena
capacity on these cases. The fresh original 68020 / 2 MiB self-host probe still
rejects at `binary_source.asm` line 234, the same `.for 4` construct previously at
line 224. Peak tracked ownership is 1,011,120 bytes, with balanced cleanup and
unfinished-interval flag 16. No complete self-host artifact or duration is claimed.

A proposed 20-literal `.long` stress line rejected because `appendCallText`
already exports all leading dot-statement argument text and caps it at 251 bytes.
That source limit is distinct from numeric scratch capacity and remains part of
the binary-recipe correction. The scratch proof instead uses an assignment with
a long, zero-padded binary spelling and emits its value through a short `.long`.

That focused assignment proof passes freshly on native: the binary literal has
512 leading zero digits and 32 one digits, normalizes to u32 max, and emits four
`FF` bytes matching Rust. Its numeric spelling fits the original 1,024-byte
budget while its copied spelling and metadata require the expanded capacity.
Cleanup and telemetry reconciliation pass with zero profiling errors.

## Composed-name recipe checkpoint

This bounded part of the binary-substitution slice moves composed identifier
recognition into package-selected TKVM opcode `0x14`. It preserves lexical tokens
and adds explicit fragment recipes; the writer copies them and binds generated
names. Initial qualified name prefixes remain lexical content, while the package
selects positional markers/range and allowed suffix bytes. Bare positional tokens
and decoded strings retain their existing forms.

Hypothesis: this removes the writer's lexical grammar without losing observable
macro behavior or materially increasing native preparation cost. The comparison
baseline is `b36a3a7c`: unchanged unprofiled release control 9.116 s, Hunk 72,344
bytes, linked reservation 83,560 bytes, capsule 269,226 bytes. Correctness requires
live generic Rust/native recipe comparison, existing embedded/default/nested macro
output comparisons, and fast/generic Rust equivalence. Malformed policy, adjacency,
qualified prefixes and capacity publication need explicit checks.

Success means the writer's `leadingComposite`, `identifierComposite` and suffix
validator are gone and fresh native proofs pass. Stop for a consequential grammar
or storage redesign, or an unexplained output/performance regression. This
checkpoint deliberately retains `appendCallText`, `captureCallText` and
`rewriteCallText`; argument ranges and call/string recipes must replace those
together in the next checkpoint. Native expression compilation remains later work.


The package supplies opcode `0x14` marker/range/suffix operands; native and Rust
attach explicit recipes while preserving lexical records. The writer's three
composed-name recognition routines are removed. Initial qualified prefixes stay
verbatim. An independent Sol review found and resolved invalid-run traversal:
both executors now annotate only the attempted run's head and skip its extent.
The native number scanner also had a pre-existing continuation mismatch: `$`,
`%` and `@` were accepted inside number bodies. Bodies now match Rust's ASCII
alphanumeric/underscore rule; leading numeric-prefix handling remains separate.

Fresh native proof matches 24 live generic Rust recipe records, including custom
marker/range/suffix policy, qualified prefixes, multiple placeholders, whitespace
and malformed composition. Four additional token-count/kind comparisons cover
numeric boundaries. A 14-byte scratch probe rejects `name@1` with status 3,
cursor zero, committed extent six and no published recipe. The first capacity
comparison had its expected reporting blocks in the wrong order; the corrected
fresh comparison passes without changing runtime behavior.

Native policy validation and membership use a local 32-byte bitmap instead of
quadratic uniqueness scans. Caller scratch grows from 2,560 to 5,888 bytes in the
compact frontend, reserving copied spelling, literal-fragment headers and sidecar
headers conservatively. Each shared TKPKG scratch buffer grows from 1,024 to
2,048 bytes. These bounds are provisional; they do not claim optimal storage.
Reusable telemetry advances to MEM7: 21 opcode counters and 441 adjacent pairs
in 2,084 bytes, a 168-byte instrumented-only increase. Release telemetry remains
absent. The four default package programs grow by 272 bytes total; the smoke
fixture grows by 68 bytes. No example output goldens changed.

Host checks pass 440 VM tests, 101 package tests, 111 affected binary-source checks
and both live generated-package fixture comparisons. The final invalid-run fix
also passes its all-original-token regression. Native proof/instrumentation
checks and their nine tests pass. A broader 36-test native-guard selection has
34 passes and two failures on unchanged files: the existing value-execution
inventory hash and missing compact-CLI owner annotation. This is a checkpoint,
not full repository qualification.


The unchanged unprofiled release control (m6502, 84,687 source bytes, 121 templates)
passes all 1,701 output bytes in 9.887 s, versus 9.116 s at `b36a3a7c`: an observed
8.5% increase in single runs, not a statistical estimate. This checkpoint corrects
grammar ownership and adds a lexical pass; it is not a speedup claim. The release
Hunk is 72,620 bytes (+276), with linked reservation 83,828 bytes (+268).
Fresh native embedded/default/multiple-placeholder macro comparisons pass four
inputs, and all 24 numeric normalization records still match live generic Rust.

Fresh 68020 / 2 MiB telemetry comparisons on the unchanged CPU-alias inputs
report peak tracked ownership 531,952 bytes (m68020) and 153,112 bytes (m6502),
both +64 bytes. Cleanup is balanced, live ownership returns to zero and profiling
errors are zero. Opcode/pair counts reconcile, including seven ComposeNames
executions per input. Capsule sizes are 269,294 and 11,032 bytes respectively.
The expanded scratch fits the existing arena allocation on these inputs; this
is not a worst-case whole-program memory proof. Full self-host remains at the
previous `.for 4` frontier and was not rerun for this lexical checkpoint.

The linked native formatter checks 259 files with no changes or warnings.
Independent final review reports no remaining actionable finding. Remaining work
is explicit argument ranges and call/string substitution recipes, followed by
VM-controlled expression compilation and the preparation-boundary audit.

## Call and string recipe migration: design checkpoint

The next correction needs an explicit parser contract and a new latest compact
capsule/record layout. It cannot be an enlarged token-42 trailer: packed lines
must remain at most 256 bytes, and current trailers already duplicate spelling
and impose a 251-byte limit even on ordinary directives. Do not put argument or
header parsing into TKVM merely because BSP3 currently carries only that program.

### Proposed first implementation checkpoint

Embed a package-selected macro frontend PRVM program in the compact capsule.
Its input is the initial lexical records and source spans, plus binding identities
for core keywords and known templates. The VM recognizes the call/header shape,
optional outer parentheses, balanced argument boundaries, optional parameter
types and default boundaries. Host code resolves names, owns storage and manages
invocations; it does not choose delimiter or placeholder grammar.

Reuse the existing PRVM request/result contract with an explicit macro entry;
keep the OPASM statement-entry guard intact. The native opcode `0x41`
(`ScanTopLevelCommaBoundaries`) currently does no work, while `0x50` stops at
the first comma without nesting checks. Implement the shared boundary primitive
before using it for descriptors. Rust's boundary scan currently reaches the end
of the input: add a VM-owned active range, defaulting to the full token array,
which macro-envelope recognition narrows to the argument region. Preserve the
current Rust delimiter acceptance rather than accidentally tightening it.

Treat that prerequisite as a coherent PRVM state/consumer checkpoint, not an
isolated opcode patch. Scan activation, origin and end must survive cursor changes,
checkpoint/rollback and expression resumes. The existing 40-byte native resume
record has no spare fields: migrate the latest resume contract with its consumers
rather than hiding state in unrelated fields. Use one boundary walker with Rust's
three signed delimiter depths, including its current unmatched-close behavior.
Dynamic operand parsing without a scan produces no operands. The first empty
range supplies numeric zero; later empty ranges supply an expression error and
stop. Repair the Rust/native request bridges coherently where necessary.

Focused acceptance includes nested delimiters, leading/consecutive/trailing commas,
scan plus cursor movement, repeated scan, rollback, multiple resumes and error
stopping. Extend the existing real PRVM smoke harness for native proof. This
prerequisite does not yet introduce the macro entry or descriptor arena.

Emit descriptor events through the existing 32-byte result records. Define the
new event kinds coherently in Rust, native and documentation; existing record-kind
documentation already disagrees with native codes 6/7. Hosts validate events and
copy VM-selected spans; they do not rescan delimiters, whitespace or `=`.

Produce offset-only descriptors in an owned companion arena:

- Line plan: role, head token range, argument descriptor range and supplied-list
  spelling/formatting recipe handle.
- Argument: binary token range and spelling recipe handle.
- Formal: name/type identity, optional default token range and default spelling
  recipe handle.
- Spelling recipe: literal spans and explicit separator/whitespace spans. Individual
  arguments trim as Rust does; the full supplied list keeps spacing and excludes
  defaults.

Packed lines contain typed handles into that arena rather than copied call text.
All spans/handles are offsets within validated regions, never memory pointers.
The latest capsule replaces BSP3 when this descriptor contract is implemented;
there is no legacy executor. One spelling arena should replace duplicated trimmed
argument/full-list buffers. Template/default pools and nested frames refer to it
through validated offsets with explicit ownership and cleanup.

This checkpoint removes writer `appendCallText`, template `captureCallText` and
the header-default `=` scanner. The remaining substitution consumer is explicitly
unfinished until the next checkpoint; retaining it temporarily must not be described
as binary-only expansion or a complete boundary correction.

### Substitution ordering needs an explicit contract

The authoritative Rust processor recognizes substitutions on original spelling
before tokenization/string decoding. Native currently feeds already decoded
string bytes into `rewriteCallText`. These orders are observably different:

| Template / argument | Rust result | Fresh native result after module fix |
|---|---|---|
| `.byte "\x401"` / `A` | Literal bytes `@1` | `41` (`A`): decoding introduces a marker that is then substituted. |
| `.byte "\x2ename"` / `A` | Literal bytes `.name` | `41` (`A`): decoding introduces a named marker that is then substituted. |
| `.byte "@1"` / `A",7,"B` | Bytes `41 07 42` from three expressions | Bytes `41 22 2c 37 2c 22 42`: inserted spelling remains inside one string. |

Live full Rust CLI probes with an explicit `.module app` verify all three Rust
results. Probe filenames must not supply an invalid implicit module name; initial
hyphenated filenames caused unrelated module errors and were corrected rather
than interpreted as macro behavior. The committed test inputs use `input.asm`
and an explicit module. Initial fresh native runs completed with empty output on
all three inputs and on ordinary literal, substitution and numeric controls.
An implicit-module control passed, isolating an explicit-module selection defect:
block selection treated a source module binding as a dense graph-node index.
It now resolves the binding through the graph's existing hash lookup, preserving
the distinct binding used by import selection. This restores the ordinary controls
and exposes the actual substitution-order mismatches; it does not repair those
semantics. No missing `.org` / `.end` or forbidden macro-name explanation was
supported by the source or comparisons.

Escaped markers must stay literal whichever interpolation policy is chosen.
Preserving all current Rust behavior requires a package VM lexer/decoder over
binary fragment streams when substitution changes quote/escape/token structure.
There must be no rendered-source buffer handed back to host parsers. Ordinary
shape-stable substitutions should keep the token-splicing path; any bypass of
fragment lexing needs VM-owned eligibility and equivalence tests against forced
fragment execution on identical inputs. This avoids recreating an unproven fast
path. Erik chose to preserve the existing substitution semantics for now,
including substitutions that change token/quote structure. Implement the fragment
stream contract; do not reject those substitutions or narrow the Rust language.

### Subsequent implementation checkpoint

Compile positional, named and full-list references into explicit recipes while
original spans are available. Resolve named references to formal identities once;
unknown named references retain exact literal fallback. Host expansion copies or
splices binary ranges. It does not recognize sigils, resplit commas, scan `=` or
revisit original source. Strings and nested calls carry domain-tagged recipes;
introduced escape/quote boundaries follow the agreed package VM contract.
Remove `rewriteCallText` and the fake-sidecar path in `expandStringToken` only
when the complete nested/default/string comparisons pass.

Compare identical inputs against the live Rust CLI, including escaped markers,
quoted arguments, delimiter combinations, defaults, full-list spacing and nested
substitutions. Include exact native recipe/descriptor bounds and failed-publication
probes, fresh 68020 / 2 MiB output, the unchanged release control and peak ownership.
Baseline before the module-owner repair is `bfb4bc91`: release control 9.887 s, Hunk 72,620 bytes,
linked reservation 83,828 bytes; alias peaks 531,952 / 153,112 bytes.

### Design-checkpoint validation

The Rust regression test passes all six cases (three ordering cases and three
ordinary controls); the affected graph selection passes 12 host tests. Fresh
68020 / 2 MiB ordinary native controls pass after the module-owner fix. The final
graph lookup wrapper passes entry-root, reachability and transitive native cases,
the previous root-macro fixture and the minimal m68020 explicit-module regression.
The three ordering cases fail with the exact bytes shown above; their correction
belongs to the fragment-stream migration. Native formatting checks all 259 files
without changes/warnings, and the fresh-run proof-contract check passes. Focused
Rust formatting and diff whitespace checks pass. Workflow links, benchmark
selectors and the supply-chain check pass;
the architecture boundary check reports ten findings in unchanged native
encoding/mask files. Whole-workspace formatting also reports an existing
module-order difference in unchanged `crates/opforge-vm/src/lib.rs`.

The unchanged unprofiled m6502 release control (84,687 source bytes / 121 templates)
matches all 1,701 Rust output bytes in 9.780 s, compared with 9.887 s before this
repair. That is a 1.1% lower single-run observation, not a statistical speedup
claim. The compact Hunk is 72,676 bytes (+56), with linked reservation 83,880
bytes (+52). Allocation telemetry was not rerun for this lookup-only repair.


## PRVM boundary and resume foundation

The prerequisite now implements native `0x41` scan activation and nested dynamic
`0x50` range selection. It preserves Rust's three signed delimiter depths,
including unmatched-close behavior. Dynamic parsing uses the saved scan rather
than the current token cursor; repeated parsing restarts the scan. Checkpoints
and expression resumes preserve loaded/label metadata, predicates, scan state and
the cumulative step budget. A ready expression error stops that parsing invocation.

Expression requests advance to version 2 with an explicit static/dynamic range
mode. Only the first empty dynamic range becomes numeric zero; other empty ranges
remain expression errors. The scan ordinal is independent of accumulated result
slots. Native resume version 2 stores offsets and values only, with a 468-byte
record (+428) and a 428-byte runtime local frame (+244). All current callers use
the shared runtime size symbol; there is no legacy resume executor.

The host bridge tests compare ASTs against live Rust parsing. The native smoke
checks exact request ranges, ordinals, cursor and result counts, pause/resume,
rollback and malformed state rejection. These are primitive/service-contract
proofs. The existing full native CLI service still supplies opaque expression
slots for downstream parsing; this slice does not establish full AST parity or
remove its text parser. The compact macro frontend does not yet consume PRVM,
so no integrated assembler speedup is claimed and its unchanged release benchmark
was not rerun.

A blocking Rust Hunk proof bug is repaired: `.fill` counts control allocation,
while the repeated value supplies emitted bytes. Exported computed counts now
retain literal-data proof; symbolic address values still fail proof, and invalid
counts still fail without emitting bytes.

One language gap remains explicit: a harness operand such as
`move.l #5, runtime.PRVM_RESUME_LOCAL_STATE + runtime.LOCAL_CHECKPOINT_DEPTH(a4)`
causes lockstep AST span divergence (reference tuple starts at column 13; VM
starts at column 45). A named constant for that displacement keeps the harness
usable. This does not repair the general displacement-expression mismatch.

The next coherent slice remains package-selected macro descriptors and argument
ranges, followed by VM fragment streams that preserve the three recorded macro
substitution-order cases. This foundation does not change those cases.


Validation: the VM library passed 440 tests, and the added live cursor test passes.
All 13 request ABI tests and five bridge tests pass, as do the three `.fill`
regressions. The PRVM host selection has 23 passes and one ignored guest test;
its one failure is an unchanged ExprVM telemetry-include resolution issue.
Fresh 68020 / 2 MiB execution completes both PRVM guests with zero exits and
required markers, checking 17 boundary/state cases and eight malformed resume
probes. The 30.78-second two-guest test duration includes harness/launcher work
and is not an assembly benchmark.

Isolated runtime code grows from 3,156 to 4,240 bytes (+1,084). The expanded smoke
Hunk grows from 7,140 to 13,248 bytes, mostly its new test matrix; that is not the
compact executable size. The linked native formatter checks 259 files cleanly.
Fresh-run proof, canonical debug contracts, emulator invocation policy, evidence
classification, workflow links and benchmark-selector checks pass. Instrumentation
safety retains three existing `DiagnosticBuffer` label findings in the PRVM
harnesses, confirmed against the preceding commit. This is a focused checkpoint,
not a full qualification claim.


## Macro descriptor service checkpoint

This first implementation checkpoint introduces an explicit macro PRVM entry
(kind 2, version 2), independently of compact capsule/storage integration. The
statement entry remains kind 1 and cannot execute macro descriptor programs.
Package programs select call/header envelopes, optional labels/parentheses/leading
commas, quote-aware comments, saturated comma depths and first-raw-`=` defaults.
These are macro semantics, distinct from the statement parser's signed depths.

The source is traversed only by the VM during initial preparation. Hosts receive
selected original spelling spans and token identities; they copy literal fragments
and bind identities. This is necessary to preserve substitutions before string
escape decoding. It does not authorize the host to parse rendered source later.

The program operations are `0x80` envelope (mode, flags), `0x81` argument split
(depth policy, separator), `0x82` formals (default policy), `0x83` publication and
`0x00` end. Call mode is 1 and header mode is 2. Envelope flag bits select labels
(1), outer parentheses (2), optional leading comma (4, call only), and unquoted
semicolon comments (8). Default call/header policies are 15/11. Unknown policies,
invalid order, truncated programs and trailing operations fail explicitly.

Each descriptor occupies 32 big-endian bytes: kind/flags (two words), then token
start/end, source start/end and three auxiliary longs. Token/source spans are
half-open indices/offsets, never pointers. Kinds are line (8), argument (9), formal
(10) and default (11). Line flags distinguish call/macro/segment (1/2/3); its token
range selects the head name, source range preserves the supplied full list and
auxiliaries select first child, supplied child count and optional label token.
Formals select their name and carry optional type-token and default-descriptor
indices. Defaults are appended after the contiguous formal region. Empty defaults
remain distinct from absent defaults; empty arguments are errors.

Publication is atomic: at most 64 records, caller capacity checked independently,
no records returned or caller output bytes changed on failure. Native staging is
2,048 bytes plus local state on the call stack; the statement interpreter's
428-byte frame must not be allocated for this independent entry. Common native
request/status/result definitions move to one `prvm.amigaos.abi` owner, rather than
being duplicated in the two executors.

This checkpoint deliberately does not change BSP3 or compact template storage.
`appendCallText`, `captureCallText`, the header-default scanner and substitution
consumer remain until descriptors are integrated together. The three recorded
substitution-order discrepancies also remain. Initial descriptors diagnose forms
that cannot be represented precisely by the supplied lexical token spans; this is
not a claim of complete macro frontend parity.

The live generic TKVM/Rust oracle and fresh native release run cover 38 cases:
14 valid calls/headers (including the 64-record limit), plus 24 grammar, token,
capacity, budget and opcode-sequence failures. Every status, error offset, event
field and caller-buffer byte is compared. Failure records remain unpublished;
the native harness writes the actual caller buffer rather than a conditional
copy that could conceal writes. Six Rust service tests also compare spelling,
types and defaults with the existing core macro processor.

The release service contributes 3,438 code bytes. Combined PRVM code is 7,700
bytes, compared with the preceding statement-only 4,240 bytes (+3,460, including
22 dispatch bytes). The native macro entry uses 2,236 bytes of local state and
staging, plus 60 bytes of saved registers and call return addresses. It avoids
the statement entry's 428-byte local frame. The release harness Hunk is 9,444
bytes with 25,836 bytes of linked allocation; these are fixture costs, not the
compact CLI footprint. Enabled reusable telemetry adds 548 code bytes and 228
BSS bytes; the disabled release build emits neither.

Both the release and telemetry-enabled 38-case batches match the same live Rust
oracle, with fresh completion and zero guest exits. They complete in 0.758/0.775
host START-to-DONE seconds with the configured FS-UAE 68020/2 MiB profile.
This includes harness file I/O and protocol
overhead, and is not a speedup measurement or a hardware clock claim. Compact
assembly performance remains unchanged until storage/binding integration.

The existing fresh-native statement/resume and line-iterator smoke checks pass
after ABI extraction, as do 24 focused PRVM host checks. Canonical native
formatting checks 271 files without changes or warnings. Fresh-run proof,
instrumentation safety on the new modules/harness, debug evidence classification,
benchmark-selector and workflow-link checks pass. This is a focused checkpoint;
the existing three instrumentation-label findings in older PRVM harnesses and
the retained compact macro consumers have not been resolved here.


## Compact macro descriptor storage checkpoint

BSP4 replaces BSP3 for this experimental producer/consumer pair. Its 116-byte
header retains the earlier fields and appends offsets/lengths for initial call,
header, generated packed-call and generated spelling programs, plus macro contract
version 2. Preparation programs follow the runtime region. No old capsule executor
is retained. This is a host-to-native preparation capsule, not a persistent source
format or final runtime package design.

Initial TKVM spans remain live long enough for package-selected PRVM entry 2 to
produce plans. The writer supplies a lexical-to-packed offset map; the session
arena copies each selected supplied-list spelling once and stores the 32-byte
rows. Token ranges and optional label/type fields become packed offsets; spelling
ranges become arena offsets. A six-byte trailer carries an offset-plus-one handle.
The arena is released with its preparation session. Outside template capture,
ordinary generic directives do not receive call plans. Captured dot statements
can retain plans for placeholder spelling consumers. Inactive definitions
preserve their previous skip behavior.

Templates consume descriptor-selected formal/default/argument ranges. The writer's
`appendCallText`, template `captureCallText`, host comma-boundary walker and raw
header-default `=` scanner are removed. Generated calls retain their existing
executable packed tokens: entry 3 selects packed boundaries, while configured TKVM
and entry 2 select boundaries over transient generated spelling. Their counts and
kinds must agree before publication. Packed policy 2 preserves matched delimiters
and the 16-level bound; spelling policy remains separately selected. The VM owns
these boundaries, and the host owns copying, identity binding and expansion.
Each definition retains a four-byte header-plan handle. The remaining spelling
consumer reads formal names from the VM-selected spans rather than assuming
their executable IDs belong to the symbol table. This also covers formal names
that coincide with package names, such as `a` on 6502.

The existing placeholder/string consumers remain for the next recipe checkpoint.
This does not resolve the three recorded quote/substitution-order discrepancies,
and is not a claim of binary-only expansion or complete macro parity.

A first fresh native boundary comparison exposed stack exhaustion: the original
4,412-byte packed-service frame exceeded the default Amiga stack. Compact u16
offsets and contiguous token ranges remove the redundant end table; the frame is
now 2,884 bytes including a matched-delimiter stack. The separately supplied
spelling scratch does not live on the call stack. Atomic buffer checks compare
all caller bytes, so this corruption could not be hidden by copying only valid
records.

The unchanged release macro-repeat comparison uses 256 calls, 3,036 source bytes
and 512 output bytes. Both runs produce the same live Rust output under the same
configured 68020 / 2 MiB profile. START-to-DONE rises from 3.7871 to 5.5516 seconds
(+46.6%); linked allocation rises from 83,880 to 94,276 bytes (+10,396), including
code 67,588→77,928, data 568→588 and BSS 15,724→15,760. The preparation capsule
rises from 11,032 to 11,102 bytes (+70). These are single comparative observations,
not statistical estimates or physical Amiga clock claims. This slice corrects
ownership of grammar boundaries; it does not demonstrate a performance gain.
Reproduce with `scripts/performance/prepared_source_native.py --workload macro-repeat
--blocks 32 --compact-cli --compact-only --memory-profile 2m --cpus m6502`, supplying
the current native test executable and output directory. The preceding baseline
uses native sources from `f707b271`. Further work must account for this regression
before expanding the same mechanism broadly.

Focused qualification passes all 451 Rust VM library tests, capsule preparation,
the fresh 38-case initial and 19-case packed descriptor comparisons, scoped and
imported calls, exact full-list spacing, inactive headers, both conditional
branches, two-target omitted defaults, package-name formals with embedded string
substitution, and four quoted-argument/default/header cases. Existing native
statement/resume and line-iterator proofs also pass. A telemetry-enabled compact
run under the 2 MiB profile reports 532,024 peak owned bytes, balanced allocation
and free accounting, zero remaining owned bytes and zero profiling errors.

The compact formatter checks 48 files without changes or warnings. Fresh-run
proof, boundary contract, canonical native contracts, instrumentation checks on
the adapted production modules, debug classification and workflow links pass.
This is focused qualification, not a clean broad gate: the architecture checker
retains 10 baseline enforced findings, the runtime inventory retains its existing
`tkpkg.amigaos.value_execution` source mismatch, and three instrumentation-label
findings remain in older PRVM harnesses. These checks were not weakened.


## Macro descriptor cost reduction

The experiment uses `992fd712` as the working reference: remove unnecessary
publication and preparation work without moving grammar out of the package/VM,
changing output, or bypassing the retained spelling consumers. Stop on a fresh
native mismatch, invalid descriptor publication, or an unexplained regression.
The workload remains 256 macro calls, 3,036 source bytes and 512 output bytes.

Generated descriptors now publish directly into their session arena. This removes
an identity map of 257 offsets, a 2,590-byte staging frame and the second mapped
row pass. Count/kind/range validation remains; the arena's used extent is published
only after all rows validate. Spelling is copied once, and persistent plan fields
remain offsets. A shared private reservation helper serves initial and generated
publication.

Compact clients use the thin `prvm.amigaos.macro_runtime` entry, with the same
package-selected executors, request guards and optional telemetry. The general
PRVM entry delegates macro calls to it; compact linkage no longer brings in the
statement interpreter and its resume-state storage. Macro calls receive one VM
profiling enter/leave, rather than nested wrapper observations.

Bound core directive identities, including captured `.byte`, `.align` and `.res`,
no longer acquire macro invocation plans. Their packed substitutions still run;
embedded string substitution obtains formal spelling from the definition's
header plan. Actual and unresolved nested dot calls retain the generated packed
and spelling services. This changes the captured-core-plan policy recorded in the
preceding storage checkpoint; it does not remove the remaining spelling consumer
or complete the deferred fragment recipes.

The existing gated MEM7 record is now decoded by the compact macro comparison.
Use the same performance command above with `--compare-memory` to collect owned
memory, preparation stages and tokenizer work. These instrumented timings include
probe overhead and must be kept separate from release timings. No telemetry record
or bytecode contract changed.

On identical inputs, the instrumented comparison reduces tokenizer invocations
from 519 to 263: the 256 generated core-body scans disappear. Tokenizer instructions
fall from 20,678 to 12,742 and source reads from 19,124 to 12,396. Peak owned memory
falls from 307,552 to 262,496 bytes; prepared live memory remains 33,024 bytes.
Both runs balance allocation/free accounting, release all owned blocks and report
zero profiling errors. Instrumented preparation falls from 7.12 to 5.12 seconds;
assembly stays at 0.66 seconds. These stage observations precede the final cheap
scope-ID filter and describe the same eliminated work, not release speed.

The final release comparison under the configured 68020 / 2 MiB profile is:

| Metric | Reference `992fd712` | This checkpoint |
|---|---:|---:|
| START-to-DONE host seconds | 5.5516 | 4.3105 |
| Linked allocation bytes | 94,276 | 90,232 |
| Linked code bytes | 77,928 | 73,904 |
| Linked data bytes | 588 | 568 |
| Linked BSS bytes | 15,760 | 15,760 |
| Preparation capsule bytes | 11,102 | 11,102 |

This is a 22.4% elapsed-time reduction and 4,044 fewer linked bytes. It remains
13.8% slower than the preceding pre-descriptor observation of 3.7871 seconds;
the architectural migration's regression is reduced, not eliminated. Each run is
one observation, includes guest input/preparation/assembly/output and protocol
work, and is not a physical Amiga clock claim. Output matches the live Rust oracle
and independent workload bytes with fresh completion and zero guest exit.
Release runs contain no memory telemetry record.

Reproduction uses the unchanged command above. Input SHA-256 is
`053168a2f23a43bea9b22977381f4ce63e6629f751a834cf8dd1712ced6c5100`;
output SHA-256 is
`1b1fa0c425b18a5f3a3122ef7397480da69e69db9451689884a00aa2c284b6a5`.
Final native source SHA-256 is
`8ca9ff5dff15bb7b854cf01832e37c4e27175b2be02874f368341c17526e8188`;
release image digest is `fnv1a64:c10748bb7100628e`.

Focused qualification passes fresh two-target core-body string/default/positional
substitutions followed by nested calls, labeled segments, nested invocations,
exact full-list spacing, BSS `.res`/`.align` segment expansion with whole-Hunk
comparison, the telemetry-enabled 38-case descriptor batch, and shared statement/
resume and line-iterator smoke checks. Thirteen affected Rust oracle/descriptor
checks and three performance-tool tests pass. Native formatting checks 48 files
without changes or warnings. Proof, boundary, canonical contract, instrumentation
safety on adapted modules, debug classification, benchmark-selector and workflow
link checks pass. The runtime inventory still reports only the previously recorded
`tkpkg.amigaos.value_execution` mismatch; existing broad-gate limitations above
remain. This is a bounded checkpoint, not complete macro parity or broad
integration qualification.


## Generated-call fragment recipes checkpoint

Agreed scope: cache package-selected literal, positional, named and supplied-list
fragment descriptors for captured nested calls, then expand by copying those
records. Remove generated-call use of the native `rewriteCallText` scanner;
retain its decoded-string consumer for the following ordering correction. The
working reference is `c4449e11` (release macro-repeat 4.3105 seconds, linked
90,232 bytes). Keep the current native spelling policy in this bounded migration;
this does not claim every Rust named-marker form or complete macro parity.

PRVM entry 4 emits bounded offset-only fragment records from the original selected
call-list spelling. Host code binds named fragments against VM-selected formal
spans and copies invocation values; it does not recognize placeholder grammar.
The latest preparation capsule replaces BSP4 with BSP5 to carry this program.
Recipes are created during definition capture and reused across invocations.

Success requires Rust/native recipe-record agreement, live full Rust CLI output
for nested/default/named/positional/full-list cases, fresh zero guest exits and
unchanged release/memory control inputs. Include malformed program, capacity,
work-budget and unresolved-marker cases. Stop for an unexplained output mismatch,
unsafe publication, broader syntax/storage redesign or disproportionate cost.


PRVM entry 4 and its package-selected grammar are implemented in Rust and native.
During definition capture the frontend emits fragments once; the plan arena
stores a validated recipe region and copied spelling, referenced by offsets.
The shared plan header grows from eight to twelve bytes for an optional recipe
region offset. Normal and generated publication preserve atomic used-extent
updates. Generated definitions can also cache their captured call fragments.

A separate compact copier owns bounds checks, named-formal binding and copying.
It never scans placeholder grammar or scans inserted values again. Generated calls
now use this copier, so `rewriteCallText` is called only by the retained decoded
string path. Named-formal comparisons still occur during invocation; this slice
caches grammar recognition, not every binding result. The source spelling arena
and generated TKVM/PRVM boundary services remain; binary-only expansion and the
three recorded string-order discrepancies are not resolved here.

BSP5 has a 124-byte header and appends the fragment-program offset/length. All
native consumers use this latest capsule; no BSP4 reader remains. PRVM retains
frame ABI 1 and contract 2 with explicit entry 4. The cached records use no
persisted memory pointers. Recipe and copied-byte work use existing reusable
telemetry macros; release emission stays conditional at assembly time.

Fresh release and telemetry-enabled 29-case batches match Rust records, status,
error offsets and untouched failure buffers. Alternate marker/digit/brace programs
prove that operands select recognition. Integrated two-target nested macro/segment
calls, default/positional/named/braced/full-list substitutions, unresolved markers,
exact supplied-list spacing and four quoted-argument/header-state cases match the
live full Rust CLI. The mixed new fixture is 265/266 source bytes and produces
26 output bytes. These are functional proofs, not comparative timing claims.

The unchanged macro-repeat control gives:

| Metric | `c4449e11` | This checkpoint |
|---|---:|---:|
| Release START-to-DONE seconds | 4.3105 | 4.2688 |
| Linked allocation bytes | 90,232 | 92,720 |
| Linked code bytes | 73,904 | 76,384 |
| Linked data bytes | 568 | 568 |
| Linked BSS bytes | 15,760 | 15,768 |
| Preparation capsule bytes | 11,102 | 11,120 |
| Instrumented peak owned bytes | 262,496 | 262,512 |

The single observations show no material control timing change, not a demonstrated
speedup. This control has no nested captured calls; it measures carrying the new
service and plan layout, not the benefit of cached fragment execution. Instrumented
preparation/assembly remains 5.12/0.66 seconds, with 263 tokenizer invocations and
12,742 tokenizer instructions. All tracked allocations are released, capacities
balance and profiling errors are zero. Input/output hashes remain the preceding
control's hashes; release image digest is `fnv1a64:65900adfb2b94cee` and native
source SHA-256 is
`d32528e10bb9b8bd37904992ad46e9260f0437004e5a39ef6b61f5f55175e6dc`.
The same bounded performance command and separate `--compare-memory` run reproduce
these controls; instrumented times include probes.

Qualification includes 456 VM and 101 package library tests, the capsule bounds/
program test, the new mixed Rust CLI oracle and the 29-case host oracle. Native
formatting checks 50 files without changes or warnings. Focused proof, boundary,
canonical contracts, instrumentation safety, debug classification, benchmark
selectors and workflow links pass. The CPU architecture guard retains 10 baseline
findings, and the inventory retains only the existing `value_execution` mismatch.
This checkpoint does not claim clean broad qualification or complete macro parity.

The actual cached-copy fixture also passes with telemetry enabled on both target
packages under the 68020 / 2 MiB profile. Peak owned memory is 185,200 bytes for
the 6502 package and 532,296 bytes for the 68020 package, with zero profiling
errors and balanced cleanup. These are different package footprints, not a
before/after comparison. They verify the adapted copier's enabled instrumentation
rather than inferring preservation from a control that does not execute it.

### Checkpoint: whole-line string substitution ordering

Hypothesis: owned pre-decoding spelling recipes plus a TKVM fragment entry can
preserve canonical substitution order without introducing a host text parser.
The baseline is `c5a43659`, including the three recorded discrepancies above.

The first integrated checkpoint routes ordinary macro-body lines containing VM
string tokens through complete-line recipes. PRVM entry 4 selects literal and
parameter ranges while original spelling exists; the host binds borrowed fragment
views. TKVM privately materializes at most 1,024 logical bytes and executes the
package-selected tokenizer. Only lexical records and normalized lexemes return;
the packed writer receives no expanded source pointer. Persisted recipes remain
offsets into owned storage. Preparation-only line trailer 43 selects this route.
This is internal bounded materialization, not a direct streaming interpreter.

Success requires fresh native equality with the live full Rust CLI for escaped
markers, injected quote/comma structure, comments consuming later tokens and
escapes crossing fragment boundaries. Existing nested-call/default/segment checks
must stay passing. Repeat the unchanged macro control and tracked-memory run;
report size/runtime costs without claiming a speedup from individual observations.
Stop and revise if lexical state still depends on host decisions or native proof
fails. Known nested call spelling and segment-body consumers retain their prior
paths at this checkpoint; no eligibility shortcut or complete macro parity is
claimed. Removing the old decoded-string scanner depends on migrating those
consumers too.


The recipe source bound is now 1,024 bytes in Rust and native, matching TKVM's
logical input limit. It does not change the 256-byte packed-line limit. Escaped
spelling can therefore exceed the former 253-byte limit while its decoded packed
output stays small. Private materialization source reads use the existing gated
`TOKEN_WORK` macro; release builds emit no probes. The service uses about 1,080
bytes of stack, plus its caller and ordinary TKVM frames; tracked owned-memory
measurements do not include that stack usage.

The independent harness exposed a separate Rust Hunk relocation defect:
computed absolute destinations such as `FragmentFrame+fragments.Frame.Count`
were emitted without the required section relocation. One failed instruction
contained absolute destination `$22` and consequently wrote into low OS memory.
The harness now loads the owned frame/view base once and addresses symbolic
struct offsets through registers. This is not a tokenizer VM failure. Repairing
computed absolute expression relocation remains a separate follow-up; this
checkpoint does not claim to fix that host assembler defect. The debugger
captures were localization evidence only, followed by fresh normal proof runs.

Fresh normal proof passes the complete-line Rust/native batch: escaped positional
and named markers, quote/comma injection, an injected comment consuming a later
expression, an escape crossing fragments, full-list/braced/unknown references,
normalized arithmetic, long escaped spelling, twenty substitutions in one string,
and default/repeated invocation values. Enabled telemetry reports peak owned
memory of 194,672 bytes, balanced releases and zero profiling errors. Independent
native batches pass 19 tokenizer cases (including invalid length and untouched
failure buffers) and 31 recipe cases (including the 1,024/1,025 boundary). Existing
nested/default fragment consumers pass with telemetry on both target packages;
nested macro/segment expansion also passes on both targets.

The unchanged 3,036-byte / 263-line macro-repeat control produces the same 512
bytes under the configured 68020 / 2 MiB profile:

| Metric | `c5a43659` | This checkpoint |
|---|---:|---:|
| Release START-to-DONE seconds | 4.2688 | 4.3076 |
| Linked allocation bytes | 92,720 | 94,132 |
| Linked code bytes | 76,384 | 77,796 |
| Linked data bytes | 568 | 568 |
| Linked BSS bytes | 15,768 | 15,768 |
| Preparation capsule bytes | 11,120 | 11,120 |
| Instrumented peak owned bytes | 262,512 | 262,512 |

These individual observations show no demonstrated material timing change.
The control has no template strings, so it measures carrying the new path, not
its string-expansion throughput. Instrumented preparation/assembly takes
5.14/0.68 seconds; all owned allocations are released with zero profiling errors.
Tokenizer work remains 263 invocations, 12,742 instructions and 12,396 source
reads. The new string route's materialization reads are measured by gated probes
in its own functional comparison. Private stack usage remains separate from
owned-memory accounting. Input/output hashes match the preceding control.
Release image digest is `fnv1a64:b5453170b63ee590`; native source SHA-256 is
`d5f138103c8cc4bf2782d34c0553e8a9bce71d094ab42d4c304146f76d356924`.
Use the preceding bounded performance command and a separate `--compare-memory`
run to reproduce these controls.

Qualification passes 457 VM and 101 package library tests, focused live Rust
oracles, the fresh native comparisons above and the proof/boundary/23 canonical
contracts/instrumentation/debug-classification/selector/workflow-link guards.
Native formatting checks 51 files without changes or warnings. The architecture
checker retains its 10 baseline enforced findings; the inventory retains only
the existing `value_execution` source mismatch. No clean broad qualification,
complete macro parity or removal of the remaining decoded-string scanner is
claimed. At that checkpoint, the next migration consumers were nested call
spelling and segment strings; the newly localized Rust relocation defect warrants
a separate repair.

### Checkpoint: segment strings and macro-call classification

Ordinary segment-body string lines now use the same pre-decoding, VM-selected
fragment route as macro-body strings. The prior native path substituted after
escape decoding, so `"\x401"` with argument `A` incorrectly emitted `A` instead
of the literal `@1`. Fresh native comparisons with live Rust pass that case,
quote/comma injection, named substitution and a labeled segment call. The
labeled call needed the invocation label copied onto the fragment-produced
packed line; its `.word first` now resolves to the correct address. The shared
route was checked with m6502 and m68020 packages.

An independent bounded self-host probe exposed a regression introduced by the
macro descriptor integration at `992fd712`: an instruction operand such as
`move.l .value,d0` inside a macro body was mistaken for a nested macro call.
The call classifier now distinguishes package-bound instruction heads from
source labels before applying its unresolved-call fallback. Fresh native tests
pass the reduced inactive-conditional operand, the full telemetry macro, and
a forward nested call with a canonical source label. The source-form distinction
is still incomplete: an *unindented* instruction head inside a template can be
bound as a source name and rejected. That binder decision needs a separate fix;
the real native source here uses an indented instruction.

The unchanged 3,036-byte / 263-line macro-repeat control still emits 512 bytes
under the 68020 / 2 MiB profile. One release START-to-DONE observation was
4.2346 seconds versus 4.3076 seconds at the preceding checkpoint; this does
not establish a speed difference. Linked reservation is 94,324 versus 94,132
bytes (+192), while the preparation capsule remains 11,120 bytes. Enabled
telemetry still peaks at 262,512 owned bytes with balanced cleanup and zero
profiling errors. The control does not exercise segment strings.

The fresh bounded self-host entry probe now advances past the telemetry macro
and rejects at `experimental/binary_source.asm` line 281, `.for 4`. Its staged
graph has 58 files and 646,248 source bytes; peak tracked ownership is
1,044,040 bytes with balanced cleanup. The unfinished interval flag remains
set, so there is no completed native output or timing. The next planned language
frontier remains packed loop expansion after the remaining frontend VM-boundary
work. This is a diagnostic checkpoint, not self-host parity.

### Checkpoint: generated-call argument tokenization

The captured-call copier already substituted from VM-selected original-spelling
fragments, but expansion then reprocessed decoded packed string tokens through
`rewriteCallText`. With an outer call passing `A`, a nested call containing
`"\x401"` emitted `A` instead of Rust's literal `@1`. After bypassing that
second substitution, a nested `"@1"` whose argument introduces quotes and
commas still rejected: the packed argument boundaries described the old line.

Generated calls now bind the captured fragments, tokenize the resulting
argument list through TKVM, and combine those packed arguments with the already
bound call head before PRVM describes the generated call. A transient leading
space marks the argument-only token stream as indented; no expanded source
pointer enters the packed record. The retained decoded-string scanner is no
longer used for captured generated calls, but remains in the fallback for body
tokens without a whole-line recipe.

Fresh native output matches the live Rust CLI for the escaped marker, a
substitution that introduces three arguments, a segment forwarding that string,
a zero-argument nested call, and the existing default/positional/named/braced/
full-list fixture on both m6502 and m68020 packages. A labeled segment whose
first body line invokes another macro still rejects with no source position.
The same reduced labeled case rejects at the preceding `e565f21e` checkpoint
even without strings, so that is a separate existing parity gap.

The unchanged 68020 / 2 MiB macro-repeat control emits the same 512 bytes from
3,036 source bytes. One release START-to-DONE observation is 4.2854 seconds
versus 4.2346 seconds before this change; the control contains no generated
calls and cannot measure their execution cost. Linked reservation grows from
94,324 to 94,728 bytes (+404), with the 11,120-byte package unchanged. Enabled
telemetry still peaks at 262,512 owned bytes, with balanced cleanup and zero
profiling errors. On the same 26-byte-output generated-call fixture, observed
m6502-package times were 0.5133 seconds before and 0.7743/0.7667 seconds after;
m68020-package times were 1.0287 before and 1.0109/1.0322 after. These few
host-clock observations suggest that unconditional re-tokenization can matter
for short nested calls, but do not establish a stable throughput ratio. Defer
shape-stable shortcuts and other performance tuning until the compact native
CLI completes a fresh self-host assembly with Rust-identical output and native
timing. Then use that workload's profile to decide whether generated-call
re-tokenization warrants optimization. Any shortcut pursued must have VM-owned
eligibility and explicit equivalence checks against forced re-tokenization on
identical inputs. Correctness and language parity needed for self-host remain
the immediate work.

The bounded self-host entry probe still rejects at `experimental/binary_source.asm`
line 281, `.for 4`, after staging 58 files and 650,076 source bytes. The Rust
oracle Hunk is 83,668 bytes with 94,728 bytes linked reservation; the runtime
package is 269,382 bytes. This negative probe supplies no native output or
completed self-host timing. Expression compilation and the residual decoded-
string consumer remain frontend boundary work before packed loop parity.

The 12 focused Rust macro oracles pass; fresh native cases above complete with
exact output under 68020 / 2 MiB. Native formatting, proof-contract, workflow
links, benchmark-selector and supply-chain checks pass. The CPU architecture
guard still reports its 10 enforced findings in unchanged files and none in
the changed modules; broad integration qualification is not claimed.

## Counted packed-loop checkpoint

BSP6 binds shared `.for` and `.endfor` identities in the package, replacing two
unused header words without growing the 124-byte header. Preparation compiles
the count expression; the compact native assembler replays the same packed body
records in both passes, with bounded nesting and the Rust 65,536-iteration limit.
No source text or package spelling lookup enters replay. The loop state holds
transient pointers, while binary records retain only numeric IDs and expression
bytes. Labels inside an active unscoped loop reject. Iterable `.for`, `.bfor`,
and general loop-body parity are still future work.

Fresh FS-UAE runs under 68020 / 2 MiB exactly match live Rust output for zero,
one, nested, and named-constant counted loops on the m6502 package and for a
four-iteration 68020 instruction/data loop. A labeled-body negative case has a
fresh nonzero guest completion. The bounded self-host probe now passes the
original `.for 4` frontier and rejects at `tkvm/tkvm_runtime.asm` line 109,
`TK_CLASS_IDENTIFIER_START = 2`. That remains a negative probe: no completed
self-host output or native timing exists. The m68020 runtime package is 269,404
bytes and the Rust oracle Hunk is 84,208 bytes with 95,372 linked reserved
bytes; these differ from the prior source graph and are not a performance gain.
The unchanged five-byte compact CLI control also matches Rust on native, with
one 0.505-second START-to-DONE observation and 95,436 linked reserved bytes.
There is no matched pre-change timing for this control, so no speed ratio is
claimed.

## Package-word label checkpoint

The next self-host rejection was a package-word collision: the source declares
an unindented `end` label in `binary_scopes.asm`, while the BSP6 dictionary also
owns `end` for `.end`. The packed writer now distinguishes a dotted statement
head from an ordinary name. A column-one name binds as a source declaration;
an operand using the `.end` spelling binds as a source reference, including a
forward reference. A focused m68020 / 2 MiB FS-UAE run emitted exactly the
live Rust bytes for `bra.w end` followed by an `end` label and retained working
`.cpu`, `.byte` and `.end` directives. Other package-word collisions are not
claimed as supported: `.res long` shows why operand roles cannot all be
redirected to source symbols.

The expanded-memory bounded self-host probe passes the previous line-573
frontier and reads the whole 59-file source graph, but still rejects during
late preparation, before output or native self-host timing. The new provisional
failure diagnostic reports preparation step 2, the `frontend.complete` call
that resolves scoped identities and imports. Its tracked peak is 3,040,608
bytes with instrumentation enabled; that is not a 2 MiB feasibility result.
The ordinary 68020 / 2 MiB profile remains blocked by a separate memory
ceiling. The current bounded run rejects at file ordinal 40, line 67 with a
damaged source-path diagnostic after a 1,044,064-byte tracked owned peak;
it does not reach `frontend.complete`. The earlier checkpoint rejected at
`tkvm_runtime.asm` line 109. An
exact-size fixed preparation allocation was tried and reverted: it saved only
1,576 peak tracked bytes in the expanded-memory run and the 2 MiB self-host
probe timed out at its five-minute bound. No performance gain or completed
self-host parity is claimed. The next focused step is to identify the first
failing scoped identity in `frontend.complete`, then revisit the 2 MiB memory
ceiling with a measured allocation breakdown.

## Signed word data and full-input completion probe

On the expanded-memory FS-UAE profile, the current 59-file compact self-host
input (656,451 source bytes before this checkpoint) completes preparation but
exits 20 during native assembly. Its hexadecimal diagnostic identifies physical
file 1, line 0x10 (decimal 16, `lea DosName, a1`); a fresh Rust build succeeds,
but no native self-host output exists.
The guest rejection is explicit, rather than a timeout. The run takes roughly
five and a half minutes of host test wall time with memory telemetry enabled;
the harness does not provide a reliable guest START-to-DONE duration for this
negative case.

A separate focused comparison found that native `.word` rejected a negative
scalar even where Rust emitted the fitting signed 16-bit value. Shared data
emission now accepts signed -32768 through -1 and unsigned 0 through 65535 for
word units. Fresh 68020 / 2 MiB native cases exactly match Rust for a direct
negative symbol and for a referenced imported module followed by the self-host
constant pattern. A subsequent Hunk-section case with the same constants and
a `.word GET_ARG_STR` use also matches the fresh Rust Hunk bytes. This correction
did not move the full self-host rejection:
the next full exact-output attempt again exited 20 at file 1, line 0x10 after
preparation. That instruction crosses from the entry/code section to `DosName`
in data; the compact Hunk path currently rejects section-bearing instruction
operands before encoding, so an instruction-relocation probe is the next step.
Assembly startup now sets its record offset to an invalid sentinel so a failure
before any record cannot masquerade as the first source line. No speed claim or
full self-host parity follows from this checkpoint.

## Package instruction relocations and next self-host frontier

The first instruction rejection at source line 16 was the entry section's
`lea DosName,a1` referencing DATA. The diagnostic printed `00000010` in
hexadecimal; earlier notes treated it as decimal line 10. A focused Hunk
comparison established that Rust emits an absolute-long instruction extension
and a relocation at offset 2. The compact package now carries validated
`fixup` sequence stages and numeric target projections; native execution
passes the selected package fixup through a bounded numeric side channel to
the existing Hunk relocation collector. A higher-priority unsupported member
candidate also needed a precise packed-shape exclusion so a bare symbol can
reach the applicable package recipe. Fresh 68020/2 MiB native comparisons
exactly match Rust for a forward-symbol LEA in flat output, a literal LEA,
and a CODE-to-DATA LEA Hunk relocation. Member-form fixup targets remain
explicitly unsupported in the compact package until their numeric identity
can be bound; no spelling-specific relocation shortcut was added.

The next full 59-file attempt on the expanded FS-UAE profile still exited 20
after 338.7 seconds of host test time. It advanced to file 1, hexadecimal
line `00000034` (decimal 52), `move.l IncludeCount,d0`. This is another
instruction with a section-backed symbol operand; no native completion or
whole-file timing is claimed. The next focused case should establish the
Rust relocation form and whether package preparation or native execution
blocks this operand shape. The ordinary 68020/2 MiB full-input memory ceiling
remains unproven after this expanded-profile attempt.

A focused live Rust Hunk oracle for `move.l reserved,d0` confirms a
CODE-to-BSS relocation at the instruction extension's offset 2. The same
source explicitly exits 20 in compact native at that instruction. Flat
`move.l target,d0` and even `move.l 8,d0` also explicitly exit 20, so this
frontier precedes Hunk relocation and symbol resolution. Diagnostic-only
package variants that exclude unsupported candidates, the fixup stage, and
the match stage did not produce native output; those variants were removed.
The precise native selector/projection/encoding rejection point is not yet
established. The next investigation should observe the failing boundary in
the real guest before changing its semantics. The focused native parity tests
are retained as ignored known-failure cases, not as passing evidence.

## Tuple-class selection proof

Hypothesis: the scanner's `move.w LOCAL_PENDING_KIND(a2),(a1)` is already
covered by a canonical semantic sequence, but an earlier unsupported recipe
blocks it because compact selection cannot disprove that recipe's necessary
tuple-base register class. The replacement adds a generic package-directed
proof, not a new MOVE encoder. Scope ends at a fresh full-input frontier;
unrelated capability failures become the next slice.

BSP8 reuses row bytes 22 and 23 for necessary tuple-base classes for operands
0 and 1. Zero carries no proof; class + 1 represents classes 0 through 254.
Only canonical match conjuncts contribute this metadata. Native selection
compares a complete packed tuple's register against the package's first matching
register row. A proven mismatch skips the unsupported recipe; a matching class,
unknown register or incomplete wrapper retains the barrier. Malformed table
metadata fails. Producer and all current readers move together to BSP8; no
legacy executor remains. The row size and package size stay unchanged.

Baseline: the fresh minimal displacement-to-indirect case exits 20, while Rust
accepts it. The frozen 59-file input exits at file 39, line 112 in 196.372 s
on the expanded 68020 / 10 MiB profile. Success requires fresh exact-output
native comparisons for scanner transfer forms, preservation of the unsupported
PC-tuple barrier, and a full frozen-input run. Time to a changed failure site
will be reported separately and will not be called a speed improvement.

The first proof extension passes the original minimal case, byte/word/long
displacement/update transfer matrix, exact compact CLI Hunk output, and a
matching unsupported PC-tuple control under 2 MiB. Its ordinary frozen-input
run advances to scanner line 274 (`move.b 0(a4,d2.l),(a0)+`) in 196.803 s,
versus 196.372 s before the extension (+0.430 s; one run each to different
failures, not a speed result). The executable is 87,860 bytes (+344), reserving
98,912 linked bytes (+332); the package remains 294,360 bytes.

That indexed form exposes the same conservative proof boundary: the optional
second name may have a package qualifier. The matcher now checks its complete
four-byte token without interpreting that qualifier; only the exact unqualified
base name supplies the class proof. Focused comparisons include both word- and
long-qualified index names. This remains structural matching, with all encoding
semantics in the existing package sequence.

The qualified-index proof's fresh ordinary run reaches scanner line 382,
`move.l d1,-(sp)`, with an explicit guest exit 20 in 196.857 s. This compares
separately with 196.803 s before that extension (+0.054 s; again different
failure frontiers, no speed claim). The ordinary image is 87,864 bytes (+4 for
this extension, +348 overall), reserving 98,916 linked bytes (+336 overall).
The package remains 294,360 bytes. All three ordinary runs use the same frozen
59-file / 674,295-byte source graph, expanded 68020 / 10 MiB profile, disabled
telemetry and 86,396-byte live Rust Hunk oracle. None produces native self-host
output. The next register-to-stack frontier is separate from the now-proven
memory transfer cases.

Focused qualification: 151 Rust binary-source checks pass, plus four fresh
68020 / 2 MiB native cases (minimal displacement, seven-form transfer matrix,
exact CLI Hunk output, and matching unsupported PC-class rejection). Sol's
static review finds no bounds or preservation issue, including the qualified
second token. No telemetry code was added to this structural predicate; existing
selection service instrumentation remains its owner. Rust formatting, the
289-file native formatter gate, formatting of affected experimental dependencies,
staged architecture boundaries, instrumentation safety, fresh-native proof,
contract assertions, test ownership, runtime-boundary contract, benchmark-selector
and workflow-link checks pass. Broad qualification remains incomplete: the
no-growth guard reports nine pre-existing missing ownership annotations, the
inventory checker reports existing drift in unchanged `tkpkg.value_execution`,
and the full architecture scan retains one blocking term in an unchanged file.
This slice removes nine existing terminology findings in the adapted matcher
and wrapper helper. The earlier broad Rust failures above were not rerun or
waived.

The maintained gated failure-position control now uses Rust-valid compound
immediate relocation (`move.l #payload+1,d0`), since the old MOVE failure is
fixed. Fresh native completion reports the expected exit 20 at input line 7,
pass 2, section sweep 2 of 4, with record offset 42 / 146 bytes. The control
checks that precise diagnostic and capture; it is an expected rejection, not
positive assembly parity or a full-input position measurement.

## Ambiguous single-name transfer shapes

Hypothesis confirmed in source: the native coarse shape classifier overwrites
an ordinary pair with the structured-list shape when a single name is paired
with a head/tail update wrapper. The package already supplies the ordinary
register transfer recipe, but its row becomes ineligible before projection.
The minimal Rust-valid `move.l d1,-(sp)` also rejects in a fresh 2 MiB native
run before the correction.

Retain the primary shape and permit the structured shape as an alternative
only for this ambiguous single-name form. Explicit lists/ranges still use
the structured route. Candidate priority and all package match/projection
checks remain authoritative; an unsupported eligible recipe still blocks
selection. The optional shape is reset for every instruction, with a sentinel
that cannot match any wire shape. This changes native session state only;
BSP8 and canonical package semantics remain unchanged.

Success requires fresh exact-output comparisons for B/W/L register transfers
in both directions, ordinary controls, one-element and explicit masks, MOVEA
and arithmetic recipes, and compact CLI Hunk output. Invalid register classes
and an instruction following an ambiguous form must still reject. Use the
same frozen 59-file input for a separate ordinary self-host timing; the baseline
is 196.857 s to scanner line 382 under the expanded 68020 / 10 MiB profile.
Stop at the next unrelated structural capability failure.

The shape correction alone proves the original push, but the transfer matrix
then rejects `move.l (sp)+,d1`. Complete `(name)`, `-(name)` and `(name)+`
wrappers cannot satisfy a necessary bare named root (form 7), or a nested tuple
first-item root (forms 8/9). The earlier wire inventory used the wrong qualifier;
the actual long restore is blocked by a higher-priority unsupported row requiring
a bare named root. All three proofs now use the same bounded, pure packed-shape
helper. Other root forms, incomplete wrappers and matching unsupported candidates
retain the barrier. The selector uses this structural fact only where the
package's necessary root demands it; register classes remain package-owned.

Focused final qualification passes 157 Rust binary-source checks, the final
invalid-transfer Rust oracle, and seven fresh native executions under
68020 / 10 MiB: the minimal push, the 19-instruction transfer matrix, exact
124-byte compact CLI Hunk output, three invalid class/shape-reset controls, and
the matching unsupported PC-tuple barrier. Targeted formatting, changed-scope
CPU boundaries, instrumentation safety, fresh-run proof, test ownership, canonical
contracts, runtime boundaries, benchmark selectors and workflow links pass.
The broad architecture scan retains one existing enforced finding; broad Rust
qualification was not repeated for this slice.

A fresh 2 MiB rerun stalls in the OS Startup-sequence before guest START and
before opForge executes. A bounded host sample shows an active emulation thread;
boot markers localize the stall between the CPU gate and User-Startup. This is
not assembly evidence or proof that opForge exceeds 2 MiB. Product qualification
under 2 MiB remains open. The final comparisons use the agreed expanded profile
without changing the saved emulator template or weakening completion checks.

Separate release-build measurements use the same frozen 674,295-byte, 59-file
input, unchanged 294,360-byte runtime package and 68020 / 10 MiB profile, with
telemetry disabled:

| Revision | Runner time | Fresh rejection frontier |
| --- | ---: | --- |
| `c8257c41`, tuple-class proof | 196.857 s | Scanner line 382: `move.l d1,-(sp)` |
| `e826a380`, alternative shape and forms 8/9 | 196.700 s | Scanner line 386: `move.l (sp)+,d1` |
| Final form-7 wrapper proof | 200.804 s | File `0x28`, line 259 |

The final change adds 4.104 seconds of runner time while reaching further into
assembly; different failure frontiers prevent a speed-regression or speed-gain
claim. Runner time includes native CLI construction, launch and capture; an
isolated guest assembly duration was not captured. The final native image is
88,096 bytes with 99,128 linked reserved bytes, respectively 16 and 12 bytes
above `e826a380`. Rust produces an 86,396-byte Hunk with four segments and
97,532 linked reserved bytes from this input. Native still exits with a fresh
expected rejection, so no completed native self-host output comparison exists.

## Complete scalar roots and target predicates

The next module inventory identified qualified absolute state loads, comparisons,
clears and stores in `tkvm_runtime.asm`. The package already supplies their
semantics. A fresh minimal `move.w state.Start,d0` has a valid Rust Hunk but
native exits 20 before the correction. Whole compiled scalars cannot match
required member or nested-indirect roots (forms 5/8/9); scalar-required form 6
retains its barrier. A reusable bounded `binary_shapes.isScalar` proof checks
only the wrapper extent, without decoding payloads or looking up spellings.

The combined matrix exposed two further structural discrepancies. Canonical
`target:expr` requires an identifier and projects zero; native previously
accepted any scalar. That could select absolute-long output for a numeric
absolute-word operand. The new projection consumes ExprVM's existing symbol
flag through `expression.evaluateWithSymbols`. Its macro shares the evaluator
source while keeping the ordinary entry's ABI and instruction sequence intact;
the second entry duplicates a small emitted body to avoid adding a wrapper call
or second opcode scan to ordinary evaluation. No release telemetry is added.

A nine-byte compiled `Limit+1` also passed the old tuple-prefix length check.
BSP8's earlier unsupported `MOVE.W` tuple rows (form 4, priorities 56 onward)
therefore blocked the supported absolute-long row at priority 77. Whole scalars
now disprove that tuple root too. Actual tuples, unknown shapes and matching
unsupported recipes still retain the barrier. There are no new native opcode
rules, package versions or CPU-dependent special cases.

Fresh native exact Hunk comparison passes the thirteen-instruction matrix
(200 bytes), including qualified B/W/L loads, compare, clear, register and
immediate stores, MOVEA, literal/folded numeric addresses and absolute symbol
expressions. Existing section-relative compound fixups remain unsupported;
this slice does not claim parity for `state.Start+2`. The target predicate also
uses evaluation to obtain its symbol flag, whereas Rust matches structurally;
unstable expressions that fail evaluation remain a limitation. These boundaries
must be handled coherently when their capabilities enter a later slice.

The former `/tmp`-only 59-file, 674,295-byte snapshot disappeared during host
recovery and could not be reconstructed exactly. Measurements now use the
61 dependency files from committed `45e7d6c8`, totaling 696,570 bytes. Their
sorted relative-path/NUL/content/NUL SHA-256 is
`60b85ffd6c0286252b8ef268a35a85720c39449f49cc4b95fb38370b142ea197`.
All measurements below use that identical input, the unchanged 294,360-byte
runtime package, telemetry disabled, 68020 / 10 MiB and a thirty-minute safety
bound. Rust's input oracle is an 88,096-byte, four-segment Hunk with 99,128
linked reserved bytes. Host-startup stalls from the earlier recovery attempt
are discarded; only fresh guest completions appear here.

| Native change | Runner time | Fresh rejection | Image / linked reserved bytes |
| --- | ---: | --- | ---: |
| Unmodified `45e7d6c8` baseline | 225.516 s | File `0x2a`, line 259 | 88,096 / 99,128 |
| Whole scalar disproves forms 5/8/9 | 227.088 s | File `0x2a`, line 310 | 88,096 / 99,128 |
| Plus canonical target predicate | 225.071 s | File `0x2a`, line 310 | 88,328 / 99,344 |
| Plus shared scalar proof and tuple-root correction | 224.881 s | File `0x2a`, line 310 | 88,388 / 99,396 |

Runner time includes native executable construction, emulator launch and capture;
isolated guest timing is unavailable for these full-input captures. Single-run
variation and different failure frontiers prevent speed claims. The scalar-root
change adds 1.572 seconds while reaching further; the target correction's observed
delta is -2.017 seconds; the final shared proof and tuple correction's delta is
-0.190 seconds. These are separately recorded observations, not proven performance
gains or regressions. The staged runtime's line 310 is
`lea TkvmOpcodeDispatchTable(PC),a1`; the indexed MOVEA follows on line 311.
Fresh minimal comparisons confirm that indexed MOVEA already passes and the
PC-relative LEA rejects. The earlier indexed-MOVEA location hint was incorrect;
the captured line number and measurements are unchanged.

Focused Rust qualification passes 161 binary-source checks. Affected native
formatting (19 files), instrumentation safety, fresh-native proof contract,
test ownership, runtime boundary contract, canonical contracts and benchmark
selectors pass. Five final fresh native executions pass: the two exact Hunk
cases, numeric CLR rejection, nested-indirect barrier and existing PC-tuple
barrier. The matrix's final guest START-to-DONE time is 1.020 seconds; this tiny
case is correctness evidence, not a full-input speed comparison. Changed-scope
architecture checking passes. The architecture guard's macro-operand exception
now permits a comma after a declared parameter; seven
focused Python tests preserve definition/data checking and macro scope limits.
This corrects its false label classification of `movem.l .saved, -(sp)` without
adding instruction allowlists.
The existing broad architecture finding in unchanged `binary_source.asm`
and earlier broad Rust qualification gaps remain outside this slice.


## PC-relative tuple fixups

The canonical package already defines PC-relative displacement semantics. Bounded
sequence lowering now accepts tuple-value fixup inputs only when an earlier match
proves the same operand has two items and a register base. Native transports the
bounded scalar value and exact numeric target identity; package VM execution owns
position subtraction, range checks and emission. Labels remain targets in flat
output too; absolute constants and literal offsets retain displacement semantics.
No CPU opcode rule, contract version or compatibility executor is added.

Hunk output accepts a sole same-section reference only after a successful resolved,
target-aware positional VM step proves cancellation and emits no absolute fixup.
The VM supplies that proof; the caller does not decode its program again. Reference
counting now returns D1, so the instruction caller preserves its qualifier across
that scan. Fresh exact comparison covers the forward LEA and its dispatch table,
PC literal/absolute-constant offsets, existing absolute LEA/MOVE.L Hunk relocations,
and a PC-relative MOVE. Compound address targets and mixed positional/absolute Hunk
fixups remain fail-closed limits. The compound flat probe also exposes an existing
Rust parser AST span-lockstep discrepancy; it is not a positive Rust byte oracle.

Focused qualification passes 167 Rust binary-source tests plus the new mixed-fixup
Rust oracle, fourteen VM package tests including unsafe tuple-lowering controls, formatter checks and the relevant
engineering guards. A fresh OS boot stalled before guest START; resetting the
disposable guest recovered it. That startup delay supplies no product timing claim.
The corrected CLI image is 88,868 bytes with 99,836 linked reserved bytes, adding
480/440 bytes over the previous coherent slice. The runtime package is 295,656
bytes, an increase of 1,296 bytes.

The release comparison uses the identical 61-file, 696,570-byte input and hash
recorded above, telemetry disabled and the same 68020 / 10 MiB profile:

| Coherent change | Runner time | Fresh rejection |
| --- | ---: | --- |
| Scalar-root and canonical target correction | 224.881 s | File `0x2a`, line 310 |
| PC-relative tuple fixups and Hunk proof | 219.819 s | Entry file 1, line 90 |

This slice's separately observed delta is -5.062 seconds. Runner time includes
native construction, emulator launch and capture; isolated full guest timing is
unavailable. Different rejection frontiers and single-run variation prevent a
speed claim. The Rust oracle remains 88,096 bytes/four Hunk segments, and the
new native result is a fresh exit 20 with no self-host Hunk output.


## Match facts after native export rejection

The entry-file transfers of BSS values into imported struct displacements are
now accepted through the existing package sequence. A canonical PC-to-member
sequence was downgraded to an unsupported native recipe because a later fixup
projection has no native transport. Export previously lost its leading tuple
match facts during that downgrade, letting the unsupported row block scalar
source operands. Export now retains bounded tuple-root and base-class facts
from leading typed match-only stages; encoding and fixup stages provide no
selection proof. Unknown or genuinely matching forms retain their barrier.
No native code, storage, package layout or contract version changed.

Fresh telemetry-off comparisons on 68020 / 10 MiB:

| Case | Before | After | Native start-to-done host seconds |
|---|---|---|---:|
| Two BSS values to imported struct offsets | Native exit 20 at first transfer | Exact Rust 116-byte Hunk | 1.009166666 |
| BSS value to literal offset | Native exit 20 at transfer | Exact Rust 104-byte Hunk | 0.768713375 |
| PC tuple to absolute member | Unsupported Rust recipe | Fresh native exit 20 at the matching instruction | — |

The ordinary native image remains 88,868 bytes with 99,836 linked reserved bytes.
The temporary stage trace was removed. A macOS executable-assessment stall
increased the combined test runner duration; it is excluded from performance
claims. The independent immediate-BSS-address-to-struct-offset control still
rejects natively while Rust accepts it. It is a convergence probe, not completed
parity. The identical frozen 61-file / 696,570-byte full-input run now rejects at native
origin `0x35`, line `0x240` (576), after 236.051206042 runner seconds, against
219.819007583 before this change (+16.232198459 seconds). It reaches a different
failure frontier, so this single comparison is not a speed claim. Runtime
package size remains 295,656 bytes; the live Rust oracle for the frozen source
remains an 88,096-byte, four-segment Hunk. No native self-host output was produced.

Qualification: seven export-wire tests, affected packed-source Rust checks, fresh
exact native transfers and the matching PC-to-member native rejection control.
A formerly zero-count PC barrier assertion was corrected: the row was already
unsupported, and now exports its necessary match facts. The executable ordinary
PC transfer assertion remains explicit. Eight deterministic guards and seven CPU
boundary guard unit tests pass. All changes remain experimental.
