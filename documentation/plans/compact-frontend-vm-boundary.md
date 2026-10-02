# Compact frontend: VM boundary correction

Status: BS13 has full external-default (68-file) and m68020-embedded (69-file)
self-host proofs with exact live Rust output; see the
[current plan](native-runtime-reset.md#embedded-configuration-native-self-assembly).
The BS14 built-in `.emit` and CLI record-output checkpoints have focused native
qualification. The current BS15 inline metadata checkpoint has focused qualification
separately in the current plan. That is
not a fresh full self-host proof of its changed source tree. The current embedded
self-host originally failed at an imported constant in a struct reservation.
Dependency-first preparation now passes that focused regression and seven other
native cases, but the complete current run exhausts the 10 MiB investigation
profile during unbound capture and emits no output. With expanded diagnostic
memory and the shared quoted-scalar repair, the release retry reaches final
binding but still exits 20 before assembly, without output. The
[repair status](native-runtime-reset.md#r2-dependency-first-preparation--focused-proof-full-run-still-blocked)
records the measured preparation cost and the expanded-memory diagnostic.
The earlier 61-file
implementation also completed on the physical A6000. The 2 MiB product target
remains unqualified. Residual frontend ownership gaps and the deferred
ordinary-label instruction binding issue are listed below.

## Recorded P2 self-host proof

The 2026-10-01 external-only BS12 implementation fully self-assembles its current
65-file, 748,663-byte source tree (`fnv1a64:210d4e6e48e356c7`) in FS-UAE with
exit zero. The complete 94,356-byte Hunk equals fresh Rust output byte-for-byte,
including four segments, payload, relocations and 106,328 linked reserved bytes.
Uninstrumented host-observed guest START/DONE is 563.914554959 seconds; runner
time is 588.966862375 seconds and whole-test time is 592.09 seconds. Environment:
68020 / 10 MiB, unlimited emulator CPU speed. The source differs from the
recorded 61-file checkpoint below; no comparative speed claim follows.
[The current plan](native-runtime-reset.md#p2-configuration-and-current-boundary)
records configurable package storage, measurements and outstanding gaps.
This proof covers external-only self-hosting. Embedded `.incbin`, the 2 MiB
product target and fresh P2 execution on the physical A6000 remain unqualified.

## Recorded self-host proof and measurement

At native implementation checkpoint `a49bc945`, the current-tree input contains 61 files and 719,331 source bytes (diagnostic fingerprint `fnv1a64:01877bbc3c29d612`). On 68020 / 10 MiB with unlimited CPU speed, the fresh native guest exits 0 and emits a Hunk byte-for-byte equal to the fresh Rust Hunk: 89,880 bytes, four segments, 100,864 linked reserved bytes. Host-observed guest START/DONE is 545.460594291 s; native runner time is 569.87131525 s and whole-test wall time is 572.54 s. These are single-run measurements, not physical-Amiga clock timings.

The release CLI image is 89,880 bytes (+312); linked reservation is 100,864 bytes (+304), compared with the preceding implementation. Independent template/value ownership adds four bytes per preparation entry. The preceding implementation rejects this current input, so no full-run before/after speed claim is available for the namespace repair.

The separate instrumented run also exits 0 and matches the same Rust Hunk on the identical input fingerprint. Its host-observed START/DONE is 563.969686917 s, runner time 589.645619041 s and whole-test wall time 592.33 s. It is 18.509092626 s longer than the release observation (3.39%); this single pair includes instrumentation cost and run variation. The instrumented bootstrap image is 94,156 bytes with 106,784 linked reserved bytes; its self-assembled output remains the release Hunk above.

The fresh MEMD clocks record preparation at 217.62 s and assembly at 346.66 s. They use guest DateStamp resolution and are separate from host START/DONE. Preparation stage clocks report:

| Preparation work | Seconds |
| --- | ---: |
| Tokenization | 75.079 |
| Binding and raw records | 74.557 |
| Source I/O and other work | 28.958 |
| Runtime finalization | 15.347 |
| Module discovery | 13.182 |
| Expression preparation | 10.412 |
| Package setup | 0.095 |

Peak tracked allocation is 5,222,440 bytes (4.98 MiB). Tracked live allocation is 1,576,960 bytes after preparation and 1,986,560 during assembly. There are zero allocation failures and zero profiling errors; terminal tracked ownership is zero. These counters exclude untracked OS allocations and do not establish total RAM needs. Source reads total 904,839 bytes; packed records occupy 552,366 bytes. The successful current-source results do not qualify the 68020 / 2 MiB product target.

Reproduce the current-checkout proof with the [configured FS-UAE environment](../../agents/rules/fs-uae.md)
and a fresh Rust oracle. The test always requires successful exact self-hosting;
the historical expected-failure readiness mode has been removed.

```sh
export OPFORGE_FS_UAE_MEMORY_PROFILE=68020-10m
export OPFORGE_FS_UAE_TIMEOUT_MS=1800000
export OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=1800000
cargo test -p asm --lib compact_cli_self_host_entry_readiness_fs_uae -- --ignored --nocapture --test-threads=1
```

For a frozen comparative run, set `OPFORGE_SELF_HOST_SOURCE_ROOT=<frozen-root>`.
Label that result as a comparison
against the selected tree; it does not replace a current-checkout proof.

Require fresh case-bound START/DONE challenges, guest exit zero and exact complete Hunk equality, including segment allocation, payload and relocation data. Do not use a stored Hunk as the parity oracle. Report host-observed START/DONE separately from runner and whole-test durations. For the separate phase/memory run, add `OPFORGE_COMPARE_MEMORY=1 OPFORGE_PREPARATION_PROGRESS=1 OPFORGE_PHASE_ONLY=1` to the same command. Frozen-source results are comparative evidence, not completion of a changed current tree. Git retains the superseded convergence history.

### Physical A6000 run

The host-only ignored test `export_compact_self_host_bundle` builds a fresh release
bootstrap and Rust oracle, gathers the actual dependencies and generates the current BS15
package. Its preparation-only file plan has focused native qualification recorded
in the [current plan](native-runtime-reset.md#bs13-binary-inclusion-qualification);
host export alone does not prove self-hosting. Set `OPFORGE_COMPACT_EXPORT_DIR` to a new absolute directory when invoking
the test. It preserves source bytes and include filenames exactly.

Run [the hardware script](../../scripts/performance/run_a6000_selfhost.py) from
macOS Terminal where `ash` and `acp` can reach the A6000:

```sh
python3 scripts/performance/run_a6000_selfhost.py --bundle /tmp/opforge-a6000-compact-export
```

For the current export, `/tmp/opforge-a6000-current` points to the BS15 release
bundle `/tmp/opforge-a6000-cli-metadata-release`, with its m68020 package embedded in both
bootstrap and oracle. Rust verifies the relocated source; full native BS15
self-hosting has not yet been run. With that pointer present, the ordinary command needs no options:

```sh
python3 /Users/erik/Code/Retro/opForge/scripts/performance/run_a6000_selfhost.py
```

Use `--bundle` only to select another export, such as the instrumented bundle.
After creating a new release export, update the current pointer to its directory;
preserve the existing bundle rather than overwriting it. Without the pointer,
the historical `/tmp/opforge-a6000-compact-export` default remains available.

The provisional hardware export mode accepts
`OPFORGE_COMPACT_EXPORT_EMBED=68020` with
`OPFORGE_COMPACT_EXPORT_DIR=/tmp/opforge-a6000-m68020-embedded`. It embeds only
the default-dialect m68020 package in the bootstrap executable named `opforge`.
Run it with:

```sh
python3 /Users/erik/Code/Retro/opForge/scripts/performance/run_a6000_selfhost.py --bundle /tmp/opforge-a6000-m68020-embedded
```

The local `m68020--motorola68k.bin` remains available for package identity
verification but is not transferred. On the guest, the runner invokes
`opforge --cpu 68020` without
external packages. Input remains the unchanged default release/native CLI
source, and the 94,356-byte release Rust oracle remains an external-package
configuration. This recorded P2 bundle does not prove native self-assembly of the embedded
configuration. The subsequent BS13 embedded-output bundle is
`/tmp/opforge-a6000-bs13-embedded-selfhost-v2`, with a 395,676-byte embedded
Rust oracle and full emulator self-host qualification. It has no recorded new
hardware timing. Old BS12/BS13 exports require their matching historical runner;
current BS15 source and package bytes must be exported afresh.

Defaults are host `192.168.0.220`, volume `Development` and a one-hour assembly
timeout. Each invocation creates a fresh remote directory and local result tree.
The script verifies current sources and round-trips the transferred input bytes
before execution. Export rejects filename components longer than 30 bytes.
The first hardware transfer truncated `binary_source_discovery_index.i` to 30
characters; no assembly started. Its repository filename and include are now
`binary_source_discovery_idx.i`. Transfers preserve these current names exactly.
The fresh export has 61 files and 719,329 source bytes, fingerprint
`fnv1a64:465747a987753572`. The new Rust Hunk, bootstrap and runtime package are
byte-for-byte identical to the preceding export. This host result does not replace
fresh physical execution of the renamed source tree.

Only overall duration is measured: guest `Date` captures bracket assembly at
one-second resolution, excluding transfers and environment capture. Host command
duration is also recorded and explicitly includes connection and shell setup.
Neither adds telemetry to the native executable. Fresh case-bound markers, explicit
assembler exit zero and exact full-Hunk comparison against that export's Rust
oracle are required for success. Source/package/bootstrap identities and results
are recorded locally. A timeout can leave assembly running remotely; inspect that
run before retrying. The first completed invocation returned exit 20 with the CLI
usage message, before assembly. After switching to ordinary output redirection
immediately after the executable and making the `C:CPU` probe optional, the same
source/package/bootstrap case completes successfully. No assembler logic changed.

On 2026-10-01, the physical A6000 run at wrapper checkpoint `ac78348e` assembled
all 61 current source files (719,329 bytes) and returned exit zero. Its complete
89,880-byte Hunk matches the fresh export's Rust oracle byte-for-byte. Guest
`Date` reports 15 seconds at one-second resolution (01:08:39 to 01:08:54); the
host's monotonic command duration, including connection and Shell setup, is
16.019971292 seconds. An unchanged repeat in a fresh remote directory also
returns exit zero with the exact Hunk: guest time is 15 seconds (01:15:42 to
01:15:57), and host command time is 15.848975208 seconds. Both runs are
uninstrumented. They reproduce the physical-machine result but do not establish
a code-change speedup against the earlier 545-second FS-UAE run: the execution
environments differ, and the timing gap has not been explained.

Case SHA-256 is
`2919f26180f81f3f9db1e8529cbd9f4e4cbfedf1af3507104b6f356474675cfc`;
remote directories are `Development:opforge-decab6f88c53` and
`Development:opforge-3e4f5796d66c`. Each retained pre-run round-trip contains
every input and no output Hunk, Rust oracle or completion markers. Each post-run
capture has unchanged inputs, fresh case-bound START/DONE responses, exit zero
and the exact output. Environment capture reports Kickstart
51.51, Workbench 40.0 and a 65,536-byte stack; no CPU clock measurement or 2 MiB
memory qualification was performed.

For a separate fully instrumented compact build, set
`OPFORGE_COMPACT_EXPORT_INSTRUMENTED=1` alongside a new
`OPFORGE_COMPACT_EXPORT_DIR` when running `export_compact_self_host_bundle`.
The export builds a fresh release oracle first, then enables the existing memory,
phase/progress, token-detail, binding-detail, template-work and input probes in
the bootstrap. Its manifest records all build defines and separate bootstrap and
oracle identities. The current instrumented bootstrap is 98,780 bytes, with
110,736 linked reserved bytes; the 61-file source manifest, 299,048-byte runtime
package and 89,880-byte release oracle are identical to the physical release case.

The prepared local instrumented bundle can be run from macOS Terminal with:

```sh
python3 scripts/performance/run_a6000_selfhost.py --bundle /tmp/opforge-a6000-compact-instrumented-export
```

Instrumentation writes relative `memory.bin` inside the isolated `Development:`
run directory. The script requires no pre-existing outputs or telemetry, verifies
the full release Hunk as before, then decodes the current MEMD record into
`instrumented_memory` in `result.json`. It reports `assembly_success` separately
from `telemetry_completed`; overall success additionally requires balanced tracked
ownership, zero profiling/allocation errors and valid phase/stage clocks. Nested
token/binding observations must not be added to exclusive preparation stages.
These measurements include potentially substantial hot-probe overhead. The two
15-second release runs remain the release baseline. The instrumented export,
16 focused Python checks and both release/instrumented dry runs pass.

The fully instrumented physical A6000 run at checkpoint `2f4cf816` also completes
with exit zero and the exact release Hunk. Guest overall duration is 58 seconds;
host command duration including connection is 58.798227208 seconds. MEMD records
49.16 seconds preparation and 8.82 seconds assembly (57.98 seconds together).
Exclusive preparation stages sum to 49.150610604 seconds, within 9.4 ms of the
coarse preparation clock:

| Instrumented preparation stage | Seconds |
| --- | ---: |
| Tokenization | 22.987 |
| Binding and raw records | 14.952 |
| Source I/O and other preparation | 9.089 |
| Expression preparation | 1.260 |
| Runtime finalization | 0.465 |
| Module discovery | 0.390 |
| Package setup | 0.008 |

Input collection is a separate nested observation: 1.363 seconds, 904,837 bytes
and 351 reads. The broader source-I/O/other stage must not be reported as pure
I/O time. Token telemetry counts 1,878,569 VM opcode executions and 157,921
committed tokens. Expression work compiles 21,606 programs (81,168 bytes) and
performs 62,391 evaluations. Sampled numeric binding estimates 3.166 seconds
from 1,397 samples out of 89,370 calls; it is an estimate nested within binding
work, not another exclusive stage. Full probing adds approximately 43 seconds
relative to the observed release runs, including run variation. These phase ranks
do not establish the uninstrumented phase split.

Peak tracked ownership is 5,222,440 bytes, prepared live ownership 1,576,960 and
assembly live ownership 1,986,560. Terminal tracked live ownership is zero;
allocated and freed totals both equal 10,093,072 bytes. There are no profiling
errors or allocation failures. The 552,366 packed-source bytes and these peak/live
sizes match the recorded emulator measurements. This supports the full-workload
result but neither explains the emulator/hardware timing gap nor qualifies 2 MiB.
Case SHA-256 is
`f30bc4035524bafd27412c1e5f978e569f41f121fd50c66ee3d248c379dff57a`;
remote directory is `Development:opforge-0b31a5ac3225`. The retained pre/post-run
captures verify fresh outputs, unchanged case inputs, exact completion challenges
and a complete valid 2,280-byte MEMD record. A phase-only physical measurement
would be needed before interpreting these detailed-profile ranks as release costs.

## Ownership and representation contracts

The compact path is a package-directed execution path. Rust and the canonical package define semantic behavior; native code executes bounded shared primitives and package-selected operations. Update package producers and consumers together and retain only the latest supported format; version identifiers detect mismatches and do not require compatibility executors.

| Responsibility | Contract |
| --- | --- |
| TKVM | Own line tokenization and package-selected lexical policy, including numeric normalization and explicit composed-name/substitution recipes. |
| PRVM | Own statement, operand, macro and other preparation grammar. Native code may validate bounds or execute selected primitives, but must not invent source grammar from text or packed offsets. |
| EXVM | Own expression parsing and evaluation for covered expressions. Native code may write or execute the selected expression program; a native precedence parser is not proof of VM ownership. |
| Native writer / assembler | Consume numeric identities, structured values, bounded expression programs and offset-based records. Preserve source provenance for diagnostics. Keep immutable preparation separate from mutable pass/layout state. |

Bootstrap and macro expansion may have host-owned steps where the VM boundary protocol allows them. The relevant contract is which component selects the grammar and owns its behavior, not whether native routines contain branches.

Persist offsets and numeric IDs, never process pointers. Every stored offset has an explicit base and validated bounds; moving a block must not require patching stored records. Scratch preparation storage is released before assembly. Hunk fixups retain complete symbol and section identity through stabilization and output; unsafe arithmetic and unsupported provenance fail closed. Generic directives and CPU/family semantics remain in their established shared or package-owned layers.

## Portable token and package details

Rust `PortableTokenKind::Number` carries optional normalized `u64` metadata.
Native TKVM keeps its 20-byte token record: the former reserved word carries
numeric status, and scratch offsets refer to spelling followed by the normalized
eight-byte value. Package opcode `0x13` selects normalization and ordered radix
rules. Invalid and overflowing number spellings remain deferred until a caller
requires a value. This preserves numeric-looking package names, ordinary
numbers and substitution text on the same line.

Strings already carry decoded bytes. During preparation, package-selected
composed-name, number and string policies produce bounded recipes and offset
handles. The writer copies or binds the selected result; it must not parse the
same spelling a second time. Generated calls re-tokenize VM-bound argument
fragments. The remaining decoded-string body-token fallback is called out as an
unfinished boundary below.

The compact capsule embeds TKVM and macro descriptor programs; it does not
embed an EXVM expression-parser program. Existing canonical expression
bytecode and shared ExprVM execution do not supply that missing parser program.
The macro-only initial PRVM plan currently runs after binding, which is why it
cannot yet classify an ordinary statement head before the value binder acts.

BS15 is the current compact package format; the producer writes `BS15` and the
native package owner checks the matching magic. Its header is 160 bytes, with
big-endian block-relative fields. The canonical target identity remains at
offset 124 (length at 128); the preparation-only file plan offset and length are
at 132 and 136. Built-in `.emit` identity/CPU word width are at 140/142; its
shared data-plan offset/length at 144/148 remains in the runtime prefix. The
preparation-only inline metadata program is at 152/156; shared PRVM entry 7
selects output/descriptive roles and bounded decoded-string spans. Rust
preparation expands only active `.incbin` statements for
the quoted relative native subset, using definition-file-relative roots
and the explicit supported cases. [Focused native qualification](native-runtime-reset.md#bs13-binary-inclusion-qualification)
passes; BS13 embedded-config self-hosting is qualified in the current plan.
BS15 full self-hosting and per-record origins for macro bodies drawn from several
physical files remain unqualified. Unsupported candidate recipes stay explicit rows rather than
becoming silent omissions. Package rows select package-owned recipes, numeric
projections and literal constants, including instruction encodings. Generic
native executes them without inventing CPU/family semantics. Preparation-only
markers are consumed before runtime records are emitted. If a bytecode or
package contract changes, regenerate its producer and migrate Rust/native
consumers together. Keep one current contract rather than a compatibility
executor for a superseded format.

For Hunk output, section and symbol provenance survives stabilization. The
native output path must preserve absolute instruction fixups and supported
CODE/DATA/BSS references, while shared DATA emission preserves `.word` signed
and unsigned range behavior. A numeric value has no relocation; an address
requires its complete relocation identity. Unsafe address arithmetic and an
unrepresentable alias must reject without emitting partial relocation data.

## Module and macro subset

The focused subset covers explicitly ordered physical files, module-local
imports, dependency discovery/order, selected includes, common `.use` forms,
visibility, public qualified references, selected-file discovery, and the
tested direct per-item, wildcard and scalar configured parameters. Numeric
identities are bound before assembly; native assembly does not return to source
string lookup. Fresh exact Rust/native cases include missing and ambiguous
imports, cycles, private names, invalid include paths, aliases, and imported
macro calls.

Scalar `.use ... with (...)` evaluation is limited to earlier module-scope `=`
constants and incoming parameters in the compact signed-32-bit expression
grammar. Preparation filters nested `.if`/`.else`/`.endif` records only when
their module-scope scalar inputs are known. The unsupported forms are listed
explicitly below; they are not implicitly covered by the imported-module tests.

Macro descriptors preserve definition order, lexical distance, visibility,
source order and argument spelling. Duplicate definitions and import/value
visibility conflicts reject according to Rust precedence. Numeric value
remapping is independent from template lookup and template visibility. A
per-item import alias currently renames the value; the macro remains callable
under its declared name. Module aliases and qualified macro calls are separate
supported cases.

Anonymous macro invocation scopes retain lexical hygiene through a
preparation-only marker. They must not become named reachability spans, because
selected imported expansion may otherwise discard emitted bytes. This marker
does not enter runtime records or alter the package format. Named blocks nested inside
anonymous macro scopes remain a distinct Rust/native selection edge.

## Implemented behavior retained

- TKVM-selected number normalization, composed-name recipes, and ordinary macro/segment string fragment recipes are implemented. Generated-call argument fragments are re-tokenized under VM control. Macro descriptor services use the compact PRVM boundary and offset-only fragment records.
- Preparation binds scopes, module identities, selected imports, visibility and supported scalar parameters before assembly. Selected-file discovery and dependency order, common `.use` forms, selected includes and numeric import identities have fresh focused native/Rust coverage.
- Counted packed `.for` replay passes focused real-native comparison. Package-selected branch width, supported register masks, scalar roots, predicates, and supported instruction/data Hunk relocations are exercised by exact focused native/Rust comparisons.
- Imported anonymous macro invocation scopes carry a preparation-only marker so their expansions remain reachable without creating a named reachability span. The marker does not enter runtime records or change the package format.
- Template declarations and numeric value declarations have independent ownership and visibility. Per-item selection aliases currently rename values; selected macro calls retain their declared name. Module aliases and qualified macro calls are separate supported forms.

The compact path has no general capability claim from those examples. Each
instruction recipe, expression form, import form and layout behavior needs
focused fresh Rust/native proof at its owning boundary. The full self-host result
at the top qualifies the current source tree on its recorded profile; it does
not turn unsupported language forms into supported ones.

These statements describe the implemented subset. They do not imply complete assembler-language parity.

## Remaining boundary gaps and explicit limitations

- `binary_templates.rewriteCallText` still consumes decoded-string bytes in the residual body-token fallback. Replace this last caller with explicit VM-selected recipes before claiming complete string-boundary migration; keep decoded-string provenance for diagnostics and generated spelling.
- `binary_expression.compile` still implements precedence and associativity in native code before invoking shared ExprVM. Move covered expression compilation behind package/EXVM ownership. Ordinary Rust expression handling also still uses core token spelling instead of portable numeric metadata. The expression-range and operand-wrapper choices in preparation need a PRVM/package audit as this boundary moves.
- A separate ordinary-label plus instruction binding gap remains: `entry nop` and `entry: nop` fail in instruction encoding because the instruction head is bound as a value operand. Defer repair to a package-owned PRVM head-classification plan before binding. Existing PRVM parsing exposes head spans, but its current macro-only initial plan runs after binding; integrate producer and consumer contracts. Do not add native grammar heuristics based on packed offsets.
- Iterable `.for` and `.bfor` remain unsupported; labels inside an active unscoped loop reject. Counted packed-loop execution is supported and has focused native/Rust comparison. These limits do not imply that general loop execution is absent.
- Named blocks inside anonymous macro scopes remain a separate Rust/native selection edge. Preserve the focused controls when changing macro selection.
- Compact projections remain bounded: supported fixups retain one section base plus an absolute addend; multi-base, non-affine or section-dependent addend algebra and genuine unsupported member-form targets must reject. Non-absolute layout aliases retain relocation identity and reject when output cannot represent them. Do not broaden expression or member behavior through a generic native shortcut.
- Scalar `.use ... with (...)` support is limited to supported signed-32-bit expressions from earlier module-scope `=` constants and incoming parameters. Within this import-parameter evaluation model, `.const` values, compound values, loop-derived or other assembly-time-dependent conditions, and expressions outside the compact grammar remain unsupported. This limit does not describe general loop execution. Do not claim full module, parameter or Hunk-expression parity.

For instruction and address selection, required tuple/register classes and
package predicates remain barriers: native may skip a higher-priority recipe
only after a package-owned structural proof shows that recipe cannot match.
Unknown shapes and matching unsupported recipes still block. Register masks
cover the proved name/range/join forms; cross-class ranges, overflowing package
ordinals and a genuinely matching member target remain rejected. The proof is
about package-selected structure, never a new native opcode rule.

For expressions, named absolute addends and supported immediate-memory
relocations have focused exact cases. Do not infer that all algebra preserves
relocation: supported one-base absolute-addend fixups do not imply support for
multi-base or unsafe address arithmetic. A numeric or flat-label value with no section provenance can be
treated as scalar where Rust does so; a defined non-absolute layout alias must
not lose its identity merely because section metadata is absent.

Undefined expression identities are provisional only in pass 1 and must resolve
by pass 2. In relocatable output, a defined non-absolute alias without usable
section provenance rejects; a flat label with no section identity may remain
scalar. Preserve this distinction when adapting expression and fixup handling.

## Focused native comparison commands

Run native comparisons with the configured FS-UAE environment, serial test
execution, a fresh Rust oracle, and the same source bytes. The physical-line
boundary suite remains the reusable check for refill splits and the fixed
4,096-byte line limit:

```sh
cargo test -p asm compact_physical_line_ -- --ignored --nocapture --test-threads=1
```

The full current-source readiness command and its evidence requirements are at
the top of this note. Keep small exact-output boundary cases alongside it when
changing package selection, macro visibility, strings, expressions, loops or
Hunk relocation. Negative expected exits establish only the specific rejection
contract; they are not successful artifact parity.

## Stable correction anchors

The following anchors remain for active documentation links. They summarize current behavior rather than preserving the old checkpoint narrative.

### Numeric normalization checkpoint

<a id="numeric-normalization-checkpoint"></a>

TKVM selects numeric normalization policy and emits normalized unsigned 64-bit metadata or deferred invalid/overflow state. The writer consumes values that fit its existing representation. Preserve numeric-looking names and substitution syntax until context establishes that a value is required. The portable token record layout and package opcode contract are documented in the referenced VM sources.

References: [portable token contract](../../crates/opforge-vm/src/portable_contract.rs), [Rust tokenizer helpers](../../crates/opforge-vm/src/tokenizer_runtime_utils.rs), [native scanner](../../native/motorola68000/amigaos/tkvm/tkvm_scanner.asm), and [package generation](../../crates/opforge-vm/src/builder.rs).

### Macro descriptor service checkpoint

<a id="macro-descriptor-service-checkpoint"></a>

Macro definitions and calls use package-selected PRVM services and bounded offset-only descriptor/fragment records. Generated calls re-tokenize selected argument fragments; the residual decoded-string fallback above remains. Preserve lexical hygiene, source order, visibility and the separation between macro identity and numeric value identity.

## Current implementation owners

| Area | Current owner | Boundary to preserve |
| --- | --- | --- |
| Portable token contract | [portable contract](../../crates/opforge-vm/src/portable_contract.rs) and [Rust scanner helpers](../../crates/opforge-vm/src/tokenizer_runtime_utils.rs) | Token identity and optional number metadata. |
| Native tokenization | [TKVM scanner](../../native/motorola68000/amigaos/tkvm/tkvm_scanner.asm) and [program builder](../../crates/opforge-vm/src/builder.rs) | Package-owned lexical classification and normalization policy. |
| Native frontend and preparation | [frontend](../../native/motorola68000/amigaos/experimental/binary_frontend.asm), [preparation](../../native/motorola68000/amigaos/experimental/binary_prepare.asm), [writer](../../native/motorola68000/amigaos/experimental/binary_source.asm) | Token use, identity binding, recipe selection and packed-source output. |
| Macro fragments and descriptors | [templates](../../native/motorola68000/amigaos/experimental/binary_templates.asm), [macro plans](../../native/motorola68000/amigaos/experimental/binary_macro_plans.asm), and PRVM package producers | Offset-only descriptors, lexical hygiene and selected string/argument recipes. |
| Expression parsing and execution | [native expression compiler](../../native/motorola68000/amigaos/experimental/binary_expression.asm) and shared ExprVM | Current compiler is the unresolved native parser boundary; evaluation stays in ExprVM. |

These are responsibility references, not an exhaustive source or instruction
audit. CPU/family semantics stay in package definitions and their specialized
implementation layers. Shared `.byte`, `.word`, `.org`, and other generic
directives remain in shared processing; unresolved dot-prefixed names cannot
fall through to instruction handling.

### Native proof boundary

The maintained proof contract is [native Rust parity](../../agents/rules/native-rust-parity-porting.md). Fresh guest completion is mandatory: timeout, crash, partial capture, or launcher success is not completion. Compare against a live Rust build from identical source bytes. The run recorded at the top is the current-source baseline; any later source change requires a fresh exact proof.

## Related current documentation

- [Native assembler completion plan](native-runtime-reset.md) owns the 68020 / 2 MiB product goal, memory direction and broader integration boundaries.
- [Workflow guide](../workflow/README.md) describes validation and artifact lifecycle.
- [Canonical VM boundary](../vm-boundary-protocol-v1.md#3-canonical-boundary-matrix) defines which VM owns each grammar and operation.
- [Package execution boundaries](package-execution-boundaries.md#outcome-and-scope) defines permitted shared execution primitives and package ownership.
