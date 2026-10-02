# Native assembler completion plan

Status: active. The recorded 61-file compact native implementation fully
self-hosted with exact live Rust Hunk output on the physical A6000. It remains
experimental: that proof does not establish full language, CPU, CLI or output
parity. Current measurements, reproduction commands and remaining frontend
ownership gaps are maintained in the
[compact frontend note](compact-frontend-vm-boundary.md).

This plan replaces the completed migration and prepared-source experiment
journals. Git retains their slices, measurements and superseded contracts.
The [workflow](../workflow/README.md) and
[native parity contract](../../agents/rules/native-rust-parity-porting.md)
govern execution; a future plan item alone does not authorize its implementation.

## Product outcome and scope

Provide the current Rust assembler language, all registered source CPU/dialect
pipelines, and assembly CLI/output capabilities through the compact native path.
Formatting, fix-it generation and other developer tools are deferred.

The execution-platform floor remains a 68020 running AmigaOS 3.1 or newer.
The product goal is full self-assembly within 15 minutes, preferably much faster,
on a machine with 2 MiB installed RAM. The A6000 release measurement is 15 seconds,
but the separate instrumented peak tracked allocation is 5,222,440 bytes; neither
proves the 2 MiB goal. Preserve working full self-hosting while completing breadth,
then qualify memory and platform requirements before promotion.

Source targets and execution platforms are separate. Adding a CPU package must
not add that CPU's parsing or encoding rules to shared native code. Canonical
packages and live Rust define behavior. Derived runtime packages are generated
from them, never maintained as a second source of target semantics.

## Agreed runtime package model

Runtime package generation is independent of source projects. Its inputs are the
canonical runtime model and selected CPU/dialect pipeline. Generate distributable
packages at build time; an Amiga user must not need Rust or project-specific host
preparation to assemble a new project.

- Configure the executable to embed zero, some or all target packages. Distribute
  external packages for other supported targets; full CPU support does not require
  every package to be resident or embedded.
- Resolve the requested CPU and dialect through package/catalog metadata. Prefer
  the matching embedded package; otherwise load it from configured external roots.
  Report missing, invalid or incompatible packages explicitly.
- Use identical runtime-package bytes, validation and execution for embedded and
  external storage. Keep catalog lookup, storage acquisition, package validation
  and assembly execution as separate responsibilities.
- Validate the current format/VM contract, target identity, offsets, lengths and
  resource requirements before use. Before 1.0, migrate producer and consumers
  together and retain only the latest supported contract.
- Keep package data immutable and position independent: stored offsets have
  declared bases, with no persisted memory pointers. Bound loaded-package lifetime
  and memory; do not copy Rust's unbounded artifact cache into native code.
- Preserve package identity alongside prepared instruction IDs. Numeric IDs are
  local to a package; `.cpu` changes cannot be implemented by replacing one global
  package pointer. Every assembly pass must use the originating package and the
  correct mutable CPU state.

Current BS15 represents one CPU/dialect pipeline and carries its canonical
`CPU--dialect` identity in the retained runtime prefix. It is distinct from the
canonical `.opasm` container. P2 adds configurable embedding and catalog selection.
Its `.cpu` directive still checks that same pipeline rather than switching it;
multi-package source execution is P3. Aliases select canonical packages without
creating duplicate assets. BS11 executors are superseded, with no compatibility
path. The P1 size inventory below records the earlier BS11 checkpoint.

Start with self-contained packages. Shared family/core payload deduplication is a
later measured option, not a prerequisite that complicates the first resolver.
A future editable binary source format may preserve optional formatting bytes,
but this plan does not establish that disk format.

## Package work: inspectable slices

1. **P1 — implemented: all-target generation and coverage inventory.** Enumerate
   the production registry rather than a copied target list. Export fresh packages
   for every registered CPU/dialect, their sizes, aliases/defaults, recipe counts,
   generation failures and unsupported candidate forms. Distinguish intermediate
   unsupported plans from final wire-format barriers: lowering can introduce more
   barriers. A generated package is not proof of successful native assembly.
2. **P2 — common acquisition and validation (delivered 2026-10-01).** Specify the catalog and build-time
   embedded selection, then deliver an embedded/external pair for the same target
   through one native loader. Compare exact output with live Rust; prove missing,
   wrong-target, truncated and wrong-contract packages fail explicitly. Measure
   executable/package size, load cost and peak memory separately from assembly.
3. **P3 — package identity through preparation and replay.** Support source `.cpu`
   transitions and defaults with stable package-context identities. Exercise at
   least two families and repeated switching, checking emitted bytes, endianness,
   instruction legality, CPU state and both passes. Avoid retaining dictionaries
   solely to resolve already-prepared instruction IDs.
4. **P4 — close target coverage gaps.** Choose coherent package/VM capabilities
   from P1 findings and representative Rust reference sources. Cover legal forms,
   aliases, qualifiers, rejection precedence and stateful CPUs. Maintain a target
   matrix derived from the registry; capsule presence or a NOP per CPU is not full
   CPU qualification.

Keep P3 and P4 grounded in the inventory's actual gaps; package storage alone
does not establish instruction or language parity.

### Embedded-configuration native self-assembly

Use the completed shared `.incbin` capability to rebuild the whole current
compact CLI with its m68020 package embedded. Generated `catalog.i` and its
`packages/` assets must use relative paths and remain relocatable. Stage the
configured entry, generated catalog and exact package bytes alongside unchanged
native dependencies; reassemble that relocated tree with Rust before running it
on native. Distinguish binary build inputs from runtime package fallback files.

Success requires fresh native START/DONE, exit zero and exact equality with the
complete Rust embedded Hunk, including its package payload. Record release timing
and image/reservation sizes; this is a configuration qualification, not a speed
optimization or full CPU/language parity. Stop to investigate a real functional
or capacity blocker. Keep the external-output bundle path working. P3 remains
separate.

The recorded BS13 embedded build passes fresh FS-UAE START/DONE,
exit zero and exact comparison against the live Rust Hunk. The 69 input files
contain 768,109 text bytes and one 299,104-byte package asset (1,067,213 bytes
total), fingerprint `fnv1a64:81fdeed3c3f5b3c1`. Rust assembly before and after
relocation produces identical bytes. Native emits the entire 395,676-byte Hunk,
including the package payload, matching its bootstrap exactly; linked reservation
is 407,564 bytes. Release START/DONE time is 590.4703 seconds (9m 50.47s), under
68020/10 MiB FS-UAE with unlimited CPU speed. This qualifies the embedded
configuration's self-assembly, not the 2 MiB target, physical hardware timing or
full CPU/language parity. An initial run was interrupted by a mistakenly shorter
overall harness timeout at five minutes and is excluded from measurement.

A controlled comparison uses the exact same 395,676-byte embedded release
bootstrap, package bytes, roots and emulator profile for both configurations:

| Configuration assembled | Input bytes | Result Hunk / reserved bytes | Native seconds |
| --- | ---: | ---: | ---: |
| External-default catalog | 768,020 | 96,572 / 108,460 | 587.3049 |
| m68020 embedded catalog | 1,067,213 | 395,676 / 407,564 | 590.4703 |

Both runs have fresh completion, exit zero and exact full Rust Hunk matches.
The observed extra cost is 3.1654 seconds (0.54%) for the configured input and
299,104-byte package payload. There is one valid release sample per configuration;
this is a measured workload cost, not a statistically established speed change.
The portable path change leaves the bootstrap byte-for-byte unchanged.

Four package-builder checks, 50 proof-runner unit checks, 19 hardware-runner checks,
the public builder command, relocated Rust assemblies and the focused proof,
ownership, invocation, debug, instrumentation, formatting and workflow guards
pass. The aggregate native gate still fails on 13 pre-existing missing ownership
annotations in unchanged native modules; it is not reported as green.

The hardware bundle is `/tmp/opforge-a6000-bs13-embedded-selfhost-v2`, also selected
by `/tmp/opforge-a6000-current` at that checkpoint. The BS14 slice below updates
the pointer. The usual Mac Terminal command remains:

```sh
python3 /Users/erik/Code/Retro/opForge/scripts/performance/run_a6000_selfhost.py
```

It transfers the package under `src/experimental/packages/` as a binary build
input. No external runtime fallback package or oracle is transferred. The emitted
executable is embedded too. No new A6000 execution has occurred during this slice.

### Parked — ordinary list symbols for shared embedding configuration

Erik deferred this work after qualification of the embedded self-build. The
current host-configured source is sufficient for the coming days. Focus next on
shared opcore and assembler-language parity; this note does not activate further
embedding or list work.

Erik wants the same source build to choose embedded packages on Rust and native
through ordinary symbols and module conditions. Use a list of declared package-ID
symbols, not a family bitfield, strings requiring a new value kind, or a separate
native catalog generator. The intended CLI surface is normal expression-valued
definitions, for example `-D 'embed={m68020,m6502}'`; that compact-native command
is proposed, not implemented. The package IDs must be declared by source/catalog
metadata; CPU names do not become implicit language constants.

First qualify Rust list-valued `.use with` binding explicitly, then establish
VM-owned flat scalar lists in the compact native value model. The current native
symbol, assignment, expression and import ABIs carry scalar i64 values only; this
is broader than accepting braces in the import parser. A coherent first
checkpoint covers list literals, assignment, indexing, `.len`, module binding
and forwarding, with offset-addressed owned storage and intact scalar behavior.
Invalid indexes and scalar/list misuse need explicit diagnostics. Keep parsing
and evaluation in their existing VM/package owners.

Then use those ordinary bindings and conditions in the catalog source and wire
normal native CLI definitions. Qualify empty, single and multiple package sets
using identical configured source on Rust/native and exact complete self-build
outputs. Packages remain project-independent prepared assets. Iterable `.for`,
nested compound values and broader language parity are separately reviewable;
do not silently implement a catalog-only list evaluator or source-text fallback.

### CLI checkpoint C1 — ordinary single-input invocation (delivered)

Replace the provisional three-positional command with a separate bounded CLI
argument/configuration layer over the packed engine. Accept one positional input
or `-i`/`--infile`; `.` or omitted input selects the current directory, and a
folder resolves to `main.asm`. Its resolved root directory
is the first module/include search root, while nested includes first search their
own directory. Explicit `-M`/`-I` roots retain command order. Input is a discovery
root and must not impose execution order.

Support `--cpu`, help/version, quote-aware Amiga Shell argument tails, long
`--name=value`, attached short values and `--`. Deliver one explicit binary output
and preserve source-configured Hunk self-builds through `--hunk`. Multiple inputs,
multiple/ranged outputs, defines and broader CLI artifacts remain following
slices. CLI output policy and source-selected filenames are the separate C2
checkpoint agreed below; C1 still requires an explicit `--bin` or `--hunk`.
Initial package selection still requires `--cpu` or `--runtime-package`; source
CPU selection/defaults are missing CLI parity, tracked separately below;
mid-source package transitions remain P3. `--hunk` currently selects source-configured
sections, rather than general Rust Hunk CLI synthesis. Informational commands
need no input, package or output configuration.
Native runtime-package/dialect/search-root options remain separate from canonical
Rust `.opasm` loading; never mislabel BS15 as the canonical container.

Reuse isolated old CLI routines where coherent, without importing the old engine
state or preprocessor. Qualify actual accepted/rejected invocations against live
Rust and fresh native protocol/output/diagnostics. Recheck a root-dir module and
nested include from a different working directory, quoted paths and search order.
Compare the unchanged module benchmark with checkpoint `4009e4b5`, recording
release time, image/package size and limitations separately from earlier gains.

C1 has 19 distinct CLI invocation cases qualified across 22 fresh native runs:
file/directory/current-directory inputs, root and nested search precedence,
quoted/attached/equal/separator syntax, filename defaults, source-configured Hunk,
help/version and explicit rejections. Fresh named-6502 selection and wrong-target
payload rejection also pass after migrating existing package-loader callers.
Positive artifacts match live Rust exactly; negative/informational commands have
fresh completion, the required explicit exit and isolated diagnostic/stdout proof.
Nine live Rust input oracles, 59 CLI-policy tests, four package-builder checks,
19 hardware-runner tests and the informational-proof guard pass. Two warning-report
tests reproduce unchanged at `4009e4b5`; the ownership guard still reports ten
pre-existing missing annotations (the touched CLI entry now has an owner).
Workflow, proof-contract, test-ownership, formatting and affected CCR checks pass.

This slice's separate release comparison uses the unchanged 10,687-byte,
two-module/64-block input and identical 299,142-byte BS14 package. All samples
emit the exact live Rust 2,434-byte artifact on 68020/10 MiB FS-UAE, unlimited
speed. These are host-observed START/DONE intervals, with roughly 0.25-second
polling resolution and only two samples per version; they are not hardware times.

| CLI state | Native seconds | Median seconds | Image / linked reserved bytes |
| --- | --- | ---: | ---: |
| Before C1 (`4009e4b5`) | 7.8431, 7.6000 | 7.7216 | 99,400 / 111,244 |
| Final C1 | 8.1016, 8.1060 | 8.1038 | 103,388 / 115,856 |

The observed C1 cost is +0.3822 seconds (+4.95%), with input validation and default
discovery enabled. Image growth is 3,988 bytes; linked reservations grow 4,612
bytes. Package bytes are unchanged. This adds CLI capability; it is not another
performance gain or a precise statistical overhead estimate.

A fresh embedded-m68020 export has 74 inputs (809,376 text bytes plus the
299,142-byte package), fingerprint `fnv1a64:261b1c46ffcea3c6`, and a 402,532-byte
release Hunk. Rust assembly before/after relocation is identical and the hardware
runner validates the files and new command. **The entire current source has not
been reassembled by native in this checkpoint**; the export records
`native_validation: not_run`. C1 qualification is focused CLI execution, not a
new full self-host or physical A6000 completion claim.

#### Source-selected initial package — required CLI follow-up (not implemented)

Erik's `opforge src/experimental/opforge_compact_cli.asm` must accept the root
source's `.cpu 68020` without a redundant CLI CPU/package option. The current
Shell entry rejects before source I/O. Its frontend requires a loaded package
for tokenization/binding, and native `.cpu` only validates that selected package;
removing the rejection alone would not implement source selection.

Add package-independent bootstrap recognition through shared TKVM/PRVM programs,
then use the existing catalog/loader to resolve CPU aliases, dialect and
embedded/external storage. Preserve lexical spellings such as `68020` for catalog
lookup; do not treat the token's normalized numeric value as its target identity.
Keep CPU mappings and the initial default in generated data, not native family
branches. Rust's current no-option default is `8085`; explicit `--cpu` sets the
initial target while source `.cpu` may later change it. A single-pipeline initial
selection checkpoint must not claim mid-source switching parity.

The first bounded proof should cover an unconditional root-source preamble
declaration, including `.module`, whitespace/comments and canonical/alias names,
and the current entry-file example. Define the bootstrap stopping boundary before
implementation: inactive branches, includes and macro bodies must not be scanned
indiscriminately for a CPU. Either support those preprocessing contexts through
their shared owners or reject an unsupported discovery case explicitly. Document
that limitation; full source selection remains the parity target.

Qualify omitted/file/directory input, explicit CPU/package options, unknown or
malformed declarations, missing packages, embedded/external lookup and no-CPU
default behavior against fresh Rust/native results. Preserve the full-source
preparation-order failure as a separate regression: selecting its package does
not fix struct preparation. Coordinate this bootstrap contract with the proposed
unbound-token capture slice while keeping dependency-order redesign paused for
the requested plan review.

### CLI checkpoint C2 — requested outputs and source declarations (in progress)

`.lst`, `.hex` and `.srec` are equally optional: request them explicitly through
CLI flags or source metadata; never generate listing/Hex merely because no
output flag was supplied. Source `.output` declarations must retain their literal
filenames and work without CLI output arguments. With neither CLI nor source
outputs requested, the intended behavior is assembly/validation without files.
The first C2 checkpoint implements this policy in Rust: listing, Hex and S-record
are equally opt-in, including multi-input validation. `-o` and metadata base names
alone do not request an artifact. Shared-library defaults remain unchanged.

The first native C2 checkpoint retains numeric output descriptors with literal
paths and section selections. `binary_output_plan` owns bounded descriptor
validation and flat ranges; `binary_output_io` owns parent-directory creation,
short writes, prefix/payload transport and close failures. All source requests
share one completed assembly. Source paths are literal; only CLI names gain a
missing format extension. No output request still runs preparation and assembly.

The supported declaration is `.output "path",format=bin|prg|hunk,sections=name,...`:
format precedes the required section list; other options reject explicitly. Bin
and PRG select the existing one/two contiguous placed-section layout, including a
data-only selection declared before the sections themselves. Flat selections are
resolved after declarations without allocating slots or changing layout. Hunk
still establishes the existing section-relative layout: multiple Hunk artifacts
require the same ordered selection. Decoupling Hunk layout from artifact requests,
arbitrary selections/options and one-code-Hunk defaults remain later work. One
explicit CLI bin/Hunk artifact is additive to source requests; a Hunk buffer is
never emitted as `--bin`. Compact `-o` is still unsupported.

Packed descriptors carry IDs and bytes, never memory pointers. Their internal
record schema changes together in preparation/execution; BS14 packages are
unchanged. The touched section preparer also fixes its map scratch area overlapping
five live state words, adding ten reserved bytes rather than retaining corruption.

Focused live Rust oracles cover eleven positive output/validation cases. Fresh
native runs verify literal and nested paths, multiple Hunk files, relocations/BSS,
mapped sections, bin/PRG, additive CLI output, pre-declaration selection, inactive
requests and output-free validation; five negative cases verify source errors,
unsupported options, unplaced bin, differing Hunk selections and failed writes.
The first matrix had a wrong expected diagnostic for unplaced bin; the corrected
probe completes with explicit exit 20 at binding, not the output writer. The
harness absence proof requires fresh START/DONE, exit zero and missing named
artifacts, and rejects even an unexpectedly created empty file.

CLI-core reports 65 passed and two previously reproduced warning-report failures.
The reference check still fails first on `45gs02_absx_overrides`, reproduced with
unchanged CLI files at `892de310`. Workflow, proof-contract, test-ownership,
formatter, hardware-runner (19 tests), and affected CCR checks pass; the runtime
ownership guard retains ten pre-existing missing annotations. An explicit
`lea buffer.l,a0` probe exposes an existing binding gap; bare `lea buffer,a0`
passes. Operand-suffix parity is pending, not counted as output support.

C2's separate release comparison freezes the native tree from `892de310` and
uses `OPFORGE_COMPARE_NATIVE_ROOT` with the same `new_named_external` command on
both versions. Source (10,687 bytes), BS14 package (299,142 bytes), live Rust oracle
(2,434 bytes), 68020/10 MiB configuration and unlimited emulator speed are identical.
Both samples per version complete fresh and match exactly; no telemetry is enabled.

| CLI state | Native seconds | Median seconds | Image / linked reserved bytes |
| --- | --- | ---: | ---: |
| Before C2 (`892de310`) | 7.9379, 7.9406 | 7.9393 | 103,388 / 115,856 |
| First C2 output checkpoint | 8.1696, 8.1698 | 8.1697 | 105,060 / 117,712 |

The observed change is +0.2305 seconds (+2.90%), not a performance gain. With only
two host-observed START/DONE samples and approximately 0.25-second polling, it
is not a precise overhead estimate or an A6000 time. Image growth is 1,672 bytes;
linked reservations grow 1,856 bytes. Package bytes are unchanged. This workload
measures the integrated checkpoint using an explicit bin request; it does not
isolate the cost of each new format or directory creation.

#### Second C2 checkpoint — Hex/S-record streaming

The agreed scope is native `-x`/`--hex [FILE]`, `-s`/`--srec [FILE]` and
`-g`/`--go ADDRESS` over the existing flat assembly path. Start addresses use
Rust's 4–8 hexadecimal-digit syntax. One explicit CLI artifact remains the limit;
source bin/PRG/Hunk requests remain additive. Listing and source metadata stay
separate. Hunk-to-record conversion rejects explicitly, rather than serializing
the container as addressed data. CLI sparse `.org` execution remains a language
gap, independent of the renderer's sparse-view support.

`binary_output_spans` coalesces numeric address/byte-offset/count spans observed
during final-pass emission. `binary_record_output` validates that view and
renders complete lines through an 84-byte frame and one reusable 4 KiB buffer.
`binary_output_io` transports generated chunks and handles short writes/close
failures. No second full text image is allocated. Ordinary bin/Hunk output
collects no spans and allocates no text buffer. Assembly observation passes no
source strings, package grammar or CPU semantics to the output modules.

The live Rust fixture matrix covers thirteen positive CLI cases; fresh native
execution matches every complete artifact. It includes 6502 and 68020 target
data, record splitting, the 64 KiB boundary, 24/32-bit addresses, optional names,
start addresses, empty images, adjacent placed sections and additive source bin.
Seven negative cases complete with explicit exit 20: four malformed start
addresses, conflicting CLI outputs, unsupported Hunk conversion and failed
record writes. Long data lines are split in the fixtures to respect the existing
frontend token limit; that limit has not been raised by this output slice.

The independent renderer probe checks sparse ranges, disjoint backing offsets,
unused backing bytes, multiple buffered writes and invalid span/capacity states.
Its first native attempt hung because the test wrapper called between Hunk
sections with relative branches. Relocatable absolute calls repair the wrapper;
the release probe then matches both Rust artifacts exactly. Reusable telemetry
uses `MEMORY_COUNTER_ADD`; progress phase 31 reports emission events, rendered
records and ASCII bytes without changing the MEMD record layout. Enabled native
CLI telemetry preserves the additive artifacts; the enabled sparse probe also
verifies the rendered-byte counter against each Rust file's length. Both enabled
and disabled renderer executions complete fresh. Existing native bin, PRG,
Hunk relocation/BSS and output-free checks pass after the transport change.
Workflow, proof-contract, test-ownership, formatting and affected CCR checks pass;
the runtime ownership guard retains the same ten pre-existing missing annotations.

The new embedded-m68020 A6000 bundle contains 78 inputs: 837,419 text bytes and
the unchanged 299,142-byte package. Its fingerprint is
`fnv1a64:2b172c6914651b27`; the release executable is 406,316 bytes. It is freshly
host-assembled, relocation-checked and validated by the hardware runner's bundle
loader. `/tmp/opforge-a6000-current` selects `/tmp/opforge-a6000-cli-records-final`.
The full native self-host has **not** been rerun for this checkpoint;
`native_validation` remains `not_run`. The usual hardware command is unchanged.

This slice's release comparison freezes `e42cc114` at
`/tmp/opforge-cli-c2-record-baseline` and runs `new_named_external` twice on each
tree with `OPFORGE_PACKAGE_PERF_ROUNDS=2`. It uses the same 10,687-byte module/use
workload, 299,142-byte BS14 package, live 2,434-byte Rust oracle, 68020/10 MiB
profile and unlimited emulator speed as above. All four fresh completions match
exactly, with telemetry disabled.

| CLI state | Native seconds | Median seconds | Image / linked reserved bytes |
| --- | --- | ---: | ---: |
| Before record writers (`e42cc114`) | 8.1705, 8.1858 | 8.1781 | 105,060 / 117,712 |
| With record writers | 8.1758, 8.1870 | 8.1814 | 107,172 / 119,832 |

The observed difference is +0.0032 seconds (+0.039%); it is unresolved at the
roughly 0.25-second START/DONE observation resolution. This slice shows no
resolved timing regression on ordinary binary output. Executable growth is
2,112 bytes and linked reservations grow 2,120 bytes; package bytes are unchanged.
This measures the integrated binary path, not isolated Hex/S-record rendering
cost or physical A6000 time. It is separate from the previous C2 comparison.

Inline source output metadata is the next checkpoint below; listing remains a
later C2 capability. Reuse numeric
final-pass emission events and add optional display provenance for listings.
Retain display text only for requested reporting, outside binary execution.
Do not restore text-based parsing/execution, import the legacy engine's large
state, or fabricate simplified listings under a parity claim. Keep each writer
separate from output planning and VM-owned declaration preparation. Qualify
filenames and complete bytes against live Rust, including simultaneous outputs,
source-only declarations, no-output validation, reservations, origins, section
order and write failures. Split these capabilities into inspectable recovery
points as needed; completing C1 does not claim output parity.

#### Third C2 checkpoint — inline output metadata (focused qualification)

Implement quoted inline `.meta.output.name` and `.meta.output.hex ["name"]`,
plus descriptive `.meta.name`/`.meta.version`. A missing or empty Hex name uses
`<output-base>.hex`. Naming alone requests no artifact. CLI Hex overrides the
source Hex filename; other CLI formats remain additive, and an omitted CLI
filename follows the metadata base. Metadata belongs to the entry's root module
and obeys active conditionals. Existing literal `.output` artifacts stay additive.

Keep grammar in shared packed PRVM entry 7, contract 2: a package-selected table
maps numeric heads to output/descriptive roles and publishes bounded decoded
string spans. Native preparation validates scope and retains only output names;
assembly continues over numeric records. A separate output policy module resolves
paths before assembly; rendering and file transport retain their existing owners.
BS15 replaces BS14: its 160-byte header adds the preparation-only metadata program
at offset/length 152/156. Producer, native readers, exports and hardware checks
migrate together; regenerate packages instead of keeping old executors.

This checkpoint does **not** complete metadata parity: unquoted values, `.meta`/
`.output` configuration blocks, CPU overrides, source BIN/FILL and listing remain
unsupported. Unknown or unsupported active forms must fail explicitly; inactive
forms must not request outputs. The existing linker `.output "path",format=...`
subset is a distinct capability.

Qualification uses fresh Rust artifact oracles, complete output inventories,
case-bound native completion, precedence/default/conditional/scope cases, and
existing CLI/output regressions. Compare the unchanged two-module/64-block release
workload against frozen `4ed61b50` and its own BS14 package, reporting this slice's
time and image/reservation delta separately. Stop on mismatched artifacts,
unaccounted output requests, or a material runtime regression. A host export is
not a new full native self-host proof.

The complete fresh native matrix passes 21 positive cases against live Rust
artifacts and output inventories, plus 13 expected rejections with fresh exit 20.
Final regressions cover four CLI input/default/source-Hunk cases, two record-output
cases (including 6502), and two macro-hygiene cases. Two enabled metadata cases
also check zero terminal owned memory, balanced allocation/free totals, and zero
allocation/profiling errors. Private macro scope markers reach their owning scope
layer before the portable metadata envelope. An undefined `compactCopyBytes`
telemetry call was removed; copy-byte telemetry remains unavailable rather than
being mapped to an unrelated counter. Its removal changes no release code.

Affected host checks pass: 256 packed-source tests, 478 VM tests, 101 package
tests, CLI/output oracles and 19 hardware-runner unit tests. Proof, native boundary,
instrumentation, contract, ownership, formatting and workflow checks pass. The
no-growth guard still fails on ten existing missing owner annotations. The root
and nested multi-argument macro-call probes fail at `.args Frame.Value(a4),d1,SourceBytes`
on both this tree and frozen `4ed61b50` with its own BS14 package; this is a
pre-existing macro gap, not qualified by this checkpoint.

Release comparison uses the same 10,687-byte two-module/64-block source, fresh
2,434-byte Rust oracle, 68020 / 10 MiB FS-UAE with unlimited CPU speed, and two
uninstrumented rounds per implementation. START/DONE excludes emulator startup.

| Metric | Frozen `4ed61b50` / BS14 | C2 metadata / BS15 | This slice |
| --- | ---: | ---: | ---: |
| START/DONE rounds, seconds | 8.1464, 7.9133 | 8.3513, 8.1081 | — |
| Median, seconds | 8.0299 | 8.2297 | +0.1999 (+2.49%) |
| External-package CLI bytes | 107,172 | 110,308 | +3,136 |
| Linked static reservation bytes | 119,832 | 124,400 | +4,568 |
| Runtime package bytes | 299,142 | 299,306 | +164 |

The timing difference is unresolved at the roughly 0.25-second observation
resolution and the observed 0.23–0.24-second within-pair variation. These are
relative emulator measurements, not physical Amiga time or a 2 MiB qualification.
They are separate from earlier C2 improvements.

Fresh hardware bundle: `/tmp/opforge-a6000-cli-metadata-release`, selected by
`/tmp/opforge-a6000-current`. Both bootstrap and expected output embed only the
m68020 package. Rust assembles the relocated source exactly: 81 inputs,
1,161,681 bytes including the package, source digest `fnv1a64:7832dd5b24eee742`,
and a 409,616-byte Hunk. **This BS15 tree has not completed a full native
self-host run.** The ordinary hardware-runner command selects this export;
listing provenance/rendering and the unsupported metadata forms remain later work.

#### Self-host blocker — preparation precedes dependency ordering

The 2026-10-02 A6000 run of the BS15 export completes its guest protocol but
exits 20 after three seconds, with no output. Its capture identifies
`Request .res abi.PRVM_REQUEST_FRAME_SIZE` in `binary_data_prepare.asm`.
This is failed assembly, not full self-host completion.

The focused binding repair resolves already-declared imported scalar values in
an owned numeric-token copy through the existing import/visibility helpers and
ExprVM. It preserves original tokens and reads current canonical values rather
than caching proxy values. Wildcard imports no longer capture an already-declared
local value or template. The real ABI-first reservation case and alias, selected
and wildcard reservation cases match live Rust; private, missing and unknown
values still reject. The separate ABI-last positive regression remains failing
in native and passing in Rust; it is retained rather than changing its oracle.
Focused checks report six passing Rust/native tests (two exact native positive
cases and three fresh expected rejections). The packed-source host batch reports
259 passed; formatting, workflow links, proof, boundary, ownership, contract and
instrumentation checks pass. The ten pre-existing missing-owner findings in the
no-growth guard remain unresolved.

The complete embedded-package retry still exits 20 at the same reservation after
90.638832250 seconds of fresh guest START/DONE. It uses 81 inputs, 1,164,356 bytes
including the package, source digest `fnv1a64:fc30cdabb13c0a72`, and a fresh
409,944-byte Rust Hunk. No native output is produced. These failed durations
cannot establish a performance gain. The existing current-bundle pointer is
not changed to this unqualified retry.

This repair's release cost is measured separately on the unchanged 10,687-byte
two-module/64-block workload, with the same fresh 2,434-byte Rust oracle and
68020 / 10 MiB unlimited-speed FS-UAE profile. Both new rounds complete exactly.

| Metric | Before (`9c612a55`) | Binding repair | This repair |
| --- | ---: | ---: | ---: |
| START/DONE rounds, seconds | 8.351344875, 8.108089167 | 8.104508792, 8.361962833 | — |
| Median, seconds | 8.229717021 | 8.233235813 | +0.003518792 (+0.043%) |
| External-package CLI bytes | 110,308 | 110,636 | +328 |
| Linked static reservation bytes | 124,400 | 124,716 | +316 |
| Runtime package bytes | 299,306 | 299,306 | 0 |

The timing change is below the roughly 0.25-second observation resolution and
within-pair variation; no meaningful speed change is established. The repair
image digest is `fnv1a64:6c75b5cf00c83aeb`.

The cause is sequencing: `frontend.processRecord` evaluates struct extents and
captures constants while source files are read; `frontend.orderGraph` schedules
dependencies only after that semantic preparation. An imports-only repair cannot
resolve a declaration in an unread dependency. Rust accepts either physical
module order because it loads dependencies before preparing consumers.

Erik approved the preparation-order repair after reviewing the diagnosis. The
structural direction is to retain compact token records and
their provenance, scan module configuration/dependencies, then replay dependency
modules before consumers through the existing semantic frontend. Match
[Rust's configured-use scan](../../crates/opforge-engine/src/source_graph.rs):
only incoming parameters and preceding module-level constants are available to
configuration expressions; inactive imports create no edges. Keep grammar in its
owning shared/package boundaries, with no second source-text parser. Graph owns
scheduling; imports owns configuration/visibility; frontend owns numeric capture
and replay; app owns loading and memory lifetime. Preserve include/macro gating,
root metadata identity, lexical scopes, diagnostics and selective block inclusion.
Do not defer just `.res` to finalization: its result can affect later declarations
and conditionals, and final constant arrays lose source-position semantics.

The existing prepared records cannot be replayed unchanged: they already contain
lexically bound identifiers, compiled expressions, consumed directives and macro
expansions. Capture must precede `writer.writeLine`, retaining owned unbound token
rows and spelling identities, distinct from final symbol IDs. Specify retained
VM-produced macro/string plans and their spans explicitly; tokenizer rows alone
are insufficient. Generate expansions in the later semantic environment, not in
the configuration scan. Store file/line/include provenance as offsets/ordinals.

Reviewable sequence:

1. **Capture contract proof.** Keep production ordering unchanged. Define owned
   unbound tokens and required plans, then prove capture/replay through the existing
   writer/binder equals direct processing in source order. Cover scoped labels,
   struct reservations, package-name collisions, strings and macro arguments.
   Record transient peak memory and released ownership. This checkpoint alone
   does not repair dependency ordering or establish full self-host completion.
2. **Configuration and scheduling.** Separate configured dependency scanning from
   full semantic preparation, then replay modules dependency-first. Reuse existing
   graph traversal and semantic processors. Qualify both ABI orders, incoming
   parameters, preceding constants, active/inactive imports, conflict/cycle errors,
   scopes and include provenance. Root metadata ownership must remain tied to the
   physical entry module even when dependencies are prepared first. Do not add a
   duplicate native grammar or eagerly evaluate unrelated bodies/assets.
3. **Current-source proof.** Run the complete configured embedded-package self-host,
   requiring fresh zero exit and exact live Rust Hunk equality. Measure release
   duration separately from instrumentation and each change against the unchanged
   representative workload; inspect memory before removing the comparison path.

The main risks are raw spelling versus contextual symbol identity, macro/string
plan ownership, conditional/include configuration visibility and extra transient
storage. Favor one shared token payload plus offset descriptors over simultaneous
full raw/prepared copies; measure the actual peak instead of promising a memory
neutral change. The configuration scan adds some work but avoids repeating full
semantic preparation. Its total cost remains an experiment, not a speed claim.

Success requires both physical module orders on the actual ABI case, existing
parameter/conditional/cycle/visibility rejections, then a complete fresh native
self-host with exact Rust Hunk equality. Stop for discussion if the split would
duplicate directive grammar or change Rust's configuration visibility.

#### R1 capture/replay proof — ordering repair remains in progress

The native frontend can now capture owned, unbound TKVM rows, lexemes, physical
provenance and VM-produced macro plan candidates. Records contain offsets, not
process pointers. Replay binds names and maps plan token indices through the
current writer map; it does not read or tokenize the original source. Candidate
plans are selected only after contextual binding. The normal streaming path
remains available while dependency scheduling is integrated.

The focused fresh native comparison covers a struct reservation using an earlier
constant, canonical block labels, nested namespace lookup, macro header defaults,
both parameter-controlled conditional branches, embedded string substitutions and
an ordinary string containing an unsubstituted placeholder. Both paths exit zero
and match the complete live Rust output. The replay harness copies the capture
block into a distinct allocation, frees the original, overwrites the original
line buffer and clears the frontend source view before replay. The optional
lowering callback exists only to make this intermediate proof possible; remove it
when the deferred pipeline replaces this comparison setup.

On the same focused input, release START/DONE observations were 1.022985084 seconds
direct and 1.017211250 seconds capture/replay. Their difference is below the
approximately 0.25-second observation resolution, so this establishes no speed
change. The capture-enabled image was 105,916 bytes versus 104,320 bytes direct;
linked reservations were 111,568 versus 109,968 bytes. The separately instrumented
capture run matched Rust and released every tracked allocation, with 824,368 bytes
peak owned storage. That peak includes the whole assembler's owned storage, not
just capture records, and excludes OS/loader allocations. The separately
instrumented direct run had the same 824,368-byte peak; capture's temporary
allocations did not raise the whole-run maximum on this small input.

Configuration-only scanner, graph scheduling statuses and canonical parameter
remapping APIs were introduced with this capture checkpoint. Their integration
is described below.

A more complex `.if .value==7` macro probe failed at its first invocation. The
unchanged `52f5ff9f` reference also rejects the probe with the macro named `emit`.
The positive regression is retained; its precise cause is not yet established.
It is outside the passing capture proof and must not be described as fixed or as
a successful negative parity case.

This is an intermediate capture proof; it does not establish full self-host
completion for the ordering repair.

#### R2 dependency-first preparation — focused proof, full run still blocked

The compact CLI now captures owned unbound records, scans configured dependency
edges and replays reachable modules dependency-first through the existing
semantic frontend. Configuration sees incoming parameters and preceding
module-level constants; it does not prepare structs, instructions or assets.
Parameter spellings are rebound into fresh semantic scope state rather than
copying transient symbol IDs. Physical entry identity still controls root output
metadata. Tokenizer control is restored after releasing the configuration session.
The streaming harness remains a comparison path while this repair is qualified.

Five focused fresh native tests cover eight cases: both physical module orders,
both branches of a parameter-selected dependency chain, an imported entry module
receiving parameters before configuration, an empty file-derived dependency and
the real PRVM ABI declared after its importer. Every case exits zero and equals
live Rust output. The ABI reservation produces the expected 112-byte frame offset;
this repairs the original focused preparation-order regression.

The separate representative release comparison uses identical 10,687-byte input,
2,434-byte Rust output and runtime package on 68020 / 10 MiB. Two observations at
`52f5ff9f` are 8.112051 and 8.355077625 seconds (median 8.2335643125); two after
this repair are 11.051957791 and 11.035703542 seconds (median 11.0438306665).
The measured increase is 2.810266354 seconds, or 34.13%. Release image size rises
from 110,636 to 119,200 bytes; linked reservations rise from 124,716 to 132,700.
This is a correctness repair with a measured preparation cost, not an optimization.

The complete current embedded self-host has **not succeeded**. On 68020 / 10 MiB,
the release run exits 20 without output after 168.510260958 seconds. A separate
fully instrumented failure completes after 272.092272917 seconds and confirms an
allocation failure during capture: peak owned storage 8,233,864 bytes, failed
growth from 262,144 to 524,288 bytes, and 676,246 source bytes read. Terminal
ownership is zero and allocated/freed capacities balance. The different failing
source lines between builds are allocation frontiers, not evidence that those
branch instructions are unsupported.

The capture currently retains 20-byte tokenizer rows, a 68-byte header per
physical line and owned candidate plans. This is substantially larger than final
packed source. A separately labelled 68020 / 74 MiB release run passes the earlier
allocation frontier, then exits 20 without output after 408.918868167 seconds at
`addi.w #'0'-1,d0` in `binary_record_output.asm`. Native preparation previously
handled a standalone quoted scalar as a special case but rejected trailing
arithmetic; the shared expression repair below addresses that gap. This failed run qualifies
neither complete self-hosting nor the 10 MiB investigation or 2 MiB product budget.

The subsequent shared expression repair passes fresh direct, relocated
capture/replay and compact CLI comparisons. Decoded one-byte and two-byte quoted
leaves work in arithmetic, assignments and instruction operands; two bytes pack
big-endian regardless of target output endianness. Plain quoted data operands
retain string emission. Empty and longer scalar leaves reject. The frozen
pre-repair direct and capture paths both reject the original `'0'-1` probe,
confirming that this gap predates deferred preparation. The CLI image increases
by 56 bytes, to 119,256 bytes, with 132,756 linked reserved bytes.

Its separate representative timing observations are 10.885410958 and
10.895558333 seconds (median 10.8904846455), versus R2's 11.0438306665 median.
The 0.153346021-second difference is below the approximately 0.25-second
observation resolution; no speed improvement is established. Fresh captured-CLI
negative cases also reject dependency cycles, self-import and missing modules.
The complete current embedded release retry still exits 20 without output after
452.265375833 seconds on the 74 MiB diagnostic profile. It passes per-line
semantic preparation and fails at preparation step 2, final binding, before
assembly begins. The current source has 84 inputs and 1,230,675 bytes including
the embedded package, manifest `fnv1a64:944699b4f0067efa`; its fresh Rust release
Hunk is 418,564 bytes. The separate fully instrumented run also exits 20 at this
boundary after 723.947896417 seconds. It has zero allocation failures, peak owned
storage of 19,357,384 bytes (18.46 MiB), and 1,025,979 bytes of packed records.
Terminal ownership is zero; all 41,069,784 allocated bytes are released. Profiling
flag 16 records the unfinished preparation clock, not an allocation failure.
This establishes a logical finalization failure in the expanded-memory run;
the individual import, section and scope checks still need localization. These
instrumented durations include probe cost. This is localization progress, not
full self-host completion, and the current A6000 bundle pointer remains unchanged.

A subsequent phase-only failure capture localizes the unresolved import proxy
`state.tkvmlastfailurekind` at final binding. It exits 20 without output after
550.382683084 seconds on the same 74 MiB diagnostic profile; this instrumented
duration is not release timing. The diagnostic source has 85 inputs and 1,234,649
bytes, manifest `fnv1a64:eaa6068e266008ec`. Its release Hunk remains byte-identical
to the preceding 418,564-byte oracle: disabled diagnostics emit no release changes.
Peak owned storage is 19,488,456 bytes with zero allocation failures and zero
terminal ownership. Six focused native cases resolve public bare address labels
and constant controls across both physical orders and split files, so the general
import spelling alone does not explain the full failure. A further gated probe
records the canonical target returned by binding and its declaration flags.

That canonical-target capture exits 20 after 551.887688125 seconds, still before
assembly. It forms the correct `tkvm.amigaos.state.tkvmlastfailurekind` spelling,
but binding creates a new entry: index 20,632 of 20,633, with only the explicit-name
flag and no declaration flag. Its source manifest is `fnv1a64:2a8219283cb58828`
(85 inputs, 1,235,281 bytes); the release oracle is unchanged. This narrows the
failure to a missing declaration lookup rather than an incorrect import prefix.

The subsequent bounded linear declaration scan finds the declaration, but its
stored canonical name contains four zero bytes replacing `.sta` in
`tkvm.amigaos.state.tkvmlastfailurekind`. The declaration is index 354,
owner 338, with a 19-byte leaf offset and declared/address flags; the failed
correctly spelled target remains index 20,632. The fresh phase-only diagnostic
exits 20 before assembly after 551.118666458 seconds, manifest
`fnv1a64:a291f1e8b806845e` (85 inputs, 1,235,323 bytes). This establishes a corrupt
stored spelling, without yet establishing its writer. Release output is still
the unchanged 418,564-byte Rust oracle. A gated owner/neighbor capture is being
used to distinguish corrupted-prefix composition from a later isolated overwrite.

The owner/neighbor capture leaves the module name and adjacent declarations
intact (553.023165333 seconds, still exit 20 before assembly). The writer is
`frontend.templateIdentity`: `scopes.bind` can return A1 at the end of a matched
name, but the caller restored saved `FirstBound` flags through that clobbered
register. A repeated `.word` lookup therefore writes four zeros twelve bytes
past its stored spelling, exactly into the following declaration's prefix.
The caller now reacquires its scope pointer before restoring the flags. Other
binder callers were reviewed without finding the same mistake.

An ordinary unused macro activates the affected template-lookup path in the
four-module state fixture, while Rust's complete Hunk remains unchanged. Fresh
native execution fails at final binding before the repair and exits zero with
exact Rust Hunk equality after it. The no-macro control also passes. The repair
adds four release bytes (119,260-byte CLI, 132,760 linked reserved bytes); the
short observations do not establish a speed change. The full release retry still
exits 20 at final binding before assembly after 466.466222125 seconds. It has
85 inputs, 1,235,473 bytes, manifest `fnv1a64:99f7f92ca6485512`, and a fresh
418,568-byte Rust release Hunk. A further failure is being localized; this repair
does not establish complete self-hosting. The completed instrumented captures above are failure-localization
measurements, not completed self-host timings.

Further fresh native discriminators pass complete live Rust comparisons: a
four-module CODE/DATA/BSS Hunk with public bare labels and an imported address
initializer (1.547492542 seconds), that graph split across mismatched filenames
with an `other.state` decoy (1.518117333 seconds), and a 712,942-byte growth case
with 3,000 long constants crossing name/entry growth and capture-region boundaries
(134.587513 seconds). These are correctness/localization cases, not comparative
performance claims. The stronger reachable-consumer version of the original
six-case matrix has Rust proof but has not been rerun natively yet.

A bounded failure-only declaration scan now distinguishes an existing canonical
declaration from the first declaration with the same leaf under another scope.
Validated import proxies are excluded. It reuses the existing snapshot storage;
only gated view descriptors grow by two bytes each. Two fresh native rejected
assemblies verify the absent and alternate-scope results (0.752716417 and
0.761505791 seconds). They remain failed assemblies and diagnostic evidence.

Affected Rust binary-source checks pass (262 passed, 400 ignored); ignored native
tests are not native execution evidence. The wider Rust library run is not green:
1,955 passed, 69 failed and 410 were ignored. Its legacy source assertions,
token/AST span comparisons and reference/diagnostic failures remain outside this
repair; not every failure has been reproduced on the baseline. Workflow, native
instrumentation, canonical contracts and fresh-proof checks pass. The aggregate
native ownership gate still reports nine unchanged missing owner annotations.

Existing configuration limits also remain: native scalar configuration resolves
some explicitly qualified constant spellings that Rust's static source-spelling
environment treats as unknown, and textual includes inside an inactive
preprocessor `.ifdef` are still opened before capture. These are unqualified
parity gaps, not claims that the new pipeline covers the complete Rust language.

### Built-in `.emit` — implementation and focused qualification

The compact path implements `.emit unit,value[,value...]` over packed source.
The package supplies numeric byte/word/long identities and CPU word width; shared
PRVM entry 6 selects unit/value spans and ExprVM evaluates scalars. Data grammar
never enters a CPU operand parser. Preparation checks unit symbol availability
in declaration order; later value references may resolve during assembly.
Values follow Rust scalar-to-u32 conversion, strict width overflow below four
bytes and zero extension above four in target byte order. BSS rejects emission;
Hunk preserves supported 32-bit affine relocations and rejects unsupported
address arithmetic or non-literal relocation-bearing unit forms.

BS14 migrates producer, consumers, exports and the hardware runner together,
without a legacy executor. Its retained 16-byte data plan uses envelope,
directive and operand opcodes `0x93`–`0x95`, then publish/end `0x83`/`0x00`.
The atomic 32-byte result contains kind 18, unit offset/length at 4/8, value
span at 12/16, fixed width at 20 and optional label-prefix length at 24.
Offsets are relative to the packed record. The shared invocation wrapper counts
telemetry; disabled telemetry emits no instrumentation code. Execution uses
bounded stack scratch and existing output/relocation owners.

Integration also corrects lexical template precedence for dot-prefixed names
that occur in package dictionaries. A declared macro named `emit` keeps precedence;
`pack` is likewise a template call even when PACK is an instruction mnemonic.
Generated macro hygiene scopes no longer publish repeated address labels, and
counted replay accepts their empty packed scope records, including `.for 0`.
These are shared binding/scope repairs, with no target spelling special cases.

Fresh positive FS-UAE cases match complete live Rust artifacts for both byte
orders, canonical/colon labels, named/expression widths, earlier labels and `$`,
forward data values, zero/repeated macro expansion, template precedence and
CODE/DATA Hunk relocation payloads. Fourteen overflow/grammar/forward-unit/BSS
cases and four unsupported Hunk cases are covered by explicit negative tests.
All 18 negative cases complete freshly with exit 20 and the required native
rejection diagnostic. The five positive cases exit zero and match their complete
live Rust artifacts. These focused results are not full language parity or a
fresh full native self-host proof.
Host checks pass: 475 VM tests, 101 package tests, 243 packed-source tests,
four package-builder tests and 19 hardware-runner unit tests. All 16 registered
pipeline packages generate; six still lack instruction candidates, which is
not full CPU parity. Workflow, formatting, invocation, proof, test ownership and
instrumentation guards pass. The aggregate native gate retains 11 pre-existing
missing ownership annotations in untouched modules; it is not green.

The same 10,687-byte, two-module/64-block workload produces the exact live
2,434-byte Rust result in each uninstrumented comparison, using 68020 / 10 MiB
FS-UAE with unlimited CPU speed. The frozen preceding image is `d875ad3a`'s
BS13 external-default release with its own package; current execution uses BS14.

| Release | Native seconds (two samples) | Median seconds | Image / reserved bytes | Package bytes |
| --- | --- | ---: | ---: | ---: |
| Before this slice | 7.6096, 7.6052 | 7.6074 | 96,572 / 108,460 | 299,104 |
| Built-in `.emit` slice | 7.8588, 7.8451 | 7.8520 | 99,400 / 111,244 | 299,142 |

The observed overhead is 0.2446 seconds (3.22%), plus 2,828 image bytes,
2,784 linked reserved bytes and 38 package bytes. START/DONE is host-observed
command time; the small timing delta is close to polling resolution. Two samples
do not establish a statistically precise slowdown. This workload measures the
integrated capability's cost on existing source, not `.emit` versus `.byte` speed.
Earlier optimization gains are separate. Raw logs are retained locally at
`/tmp/opforge-emit-perf-before.log` and `/tmp/opforge-emit-perf-after.log`.

A fresh m68020-embedded bundle is `/tmp/opforge-a6000-bs14-emit`, selected by
`/tmp/opforge-a6000-current`. Rust assembles the original and relocated configured
source identically; the hardware runner validates the current sources and bundle.
It contains 72 inputs totaling 1,089,123 bytes, including its 299,142-byte package
asset, fingerprint `fnv1a64:243daf02bc9e77df`. Bootstrap and oracle are the same
398,544-byte embedded Hunk (410,388 linked reserved bytes). No full BS14 native
self-host run or physical A6000 timing has been performed in this slice.
The normal hardware command above uses this fresh bundle. The older BS13 full
self-host results remain their own evidence. List bindings, native arbitrary
package embedding and multi-package source switching remain deferred.

### Completed prerequisite — shared binary inclusion before P3

Close `.incbin` for quoted relative whole-file assets before multi-package replay,
so an embedded configuration can progress toward native self-assembly. PRVM
selects path and optional label spans from packed source; shared preparation owns
file search/authorization and buffered I/O. Stream asset bytes into existing
packed `.byte` string records; do not regenerate source text or retain paths and
handles for replay. Keep the first record's label and the physical source origin.

The current Rust behavior has no offset/length operands. Qualify empty files,
all byte values, read/record boundaries, labels, includes, allowed search roots,
inactive conditionals and explicit missing/forbidden-file failures against fresh
Rust. Measure unchanged release workload overhead separately from new asset work;
host generation and a focused asset case are not full embedded self-host proof.
BS13 extends the preparation contract with the package-selected file plan; migrate
its producer and consumers together without a BS12 compatibility executor.

### BS13 binary-inclusion qualification

The shared PRVM file plan selects a decoded path and optional label from packed
records. The application resolves authorized paths, streams 4 KiB reads and emits
ordinary numeric data records; filenames, handles and the origin-path registry
are released before assembly replay. An empty labeled file still defines its
label. The stream's reusable state uses an owned 4,368-byte heap allocation,
leaving only its small descriptor on the stack.

Rust now leaves file loading to active shared assembly statements, rather than
preprocessing every asset. Expanded macros, segments and statements retain their
physical definition origins. Session-owned provider capabilities and immutable
caches survive loops, assembly passes and reachable-module relayout. Only loaded
assets enter dependencies. Custom source providers must supply an owned binary
reader; missing authority fails explicitly without filesystem fallback.

Eleven fresh FS-UAE release cases pass: patterned data containing all 256 byte values,
4 KiB read/packed-record boundaries, canonical and colon labels, empty assets,
6502/68020 data byte order, nested include paths, configured search roots,
definition-relative nested macros with consecutive asset actions, macro filename
arguments, inactive
missing files, Hunk data/offset output and three missing/forbidden-file rejections.
Positive cases exit zero and match live Rust artifacts exactly; negatives have
fresh completion, exit 20 and the expected diagnostic. A separate instrumented
4,109-byte output case also matches Rust, frees all 1,063,328 allocated bytes,
has zero terminal ownership and profiling errors, and peaks at 831,840 tracked
owned bytes. That small case does not qualify the full product's memory target.

This slice's separate release comparison uses the unchanged P2 workload: 10,687
source bytes, two modules, 64 reachable blocks and exact 2,434-byte output. FS-UAE
is 68020/10 MiB, unlimited CPU speed, with telemetry disabled. Valid host-observed
START/DONE samples are:

| Build | Seconds (two valid runs) | Mean | Image / linked reserved bytes |
| --- | --- | ---: | ---: |
| Retained P2 BS12 | 7.5991, 7.6063 | 7.6027 | 94,356 / 106,328 |
| BS13 with binary inclusion | 7.5971, 7.8411 | 7.7191 | 96,572 / 108,460 |

The observed mean increases 0.1164 seconds (1.53%), less than the new build's
0.2441-second run spread; these samples do not establish a meaningful speed change.
Image cost is +2,216 bytes, linked reservation +2,132 bytes and the m68020 package
+28 bytes (299,104 total). One pre-start emulator timeout is excluded from timing;
its subsequent run and a fresh retry supply the two valid baseline samples.

Current-format inventory generates all 16 packages (six still have no instruction
candidates). Affected core, engine, resource and VM checks pass. The broad host
assembler selection has 1,457 passes, three ignored tests and 47 failures; all
47 failing names occur in the retained P2 baseline. This is focused feature
qualification, not an all-green broad suite or a fresh full self-host proof.

A fresh local m68020-only embedded-bootstrap bundle is
`/tmp/opforge-a6000-bs13-incbin`: 68 source files, 768,020 bytes, source fingerprint
`fnv1a64:1b92de0b3e833dfc`, 395,676-byte bootstrap and 96,572-byte external-default
Rust oracle. It remains host-built and has no new hardware/self-host result.
The separate embedded-output qualification above uses portable staged catalog
paths and a fresh current-source oracle; it does not reuse this earlier bundle's
external-output result.

P3 discovery found a second prerequisite beyond package pointers: source-symbol
IDs currently start at the active package's `NameCount`. P3 needs a session-wide
source-symbol boundary and record-bound package identities that survive graph
ordering, generated records and section sweeps. Reconfiguring one global package
pointer would misinterpret existing tokens. Stateful CPU transport is a separate
coverage requirement; 68020/6502 switching alone cannot qualify it.

### P2 configuration and current boundary

Build configuration is a list of CPU or `CPU:DIALECT` names. Registry metadata
resolves aliases and default dialects. External-only is the default; repeated
`--embed` options replace the optional JSON configuration's defaults, and
`--external-only` or `--embed-all` selects an explicit whole-build policy.
There is no family bitfield or fixed family-count limit.

```sh
cargo run -p asm --example build_native_packages -- \
  --out-dir /tmp/opforge-native-new --embed 6502 --embed 68020
```

The destination must be fresh and absolute. The builder generates all registered
external packages, `catalog.i`, configured CLI source, a manifest and the assembled
`opforge_compact` executable. An optional `--config CONFIG.json` accepts
`{"embed":["6502","68020"]}`. `--catalog-only` generates assets and source without
assembling the executable. The checked-in `package_catalog.i` is the generated
external-only catalog; a host test guards drift from the production registry.
The configured entry includes `catalog.i` beside it; embedded payloads are read
from `packages/` relative to that catalog. The generated build directory can be
moved without retaining the original host path.

The local self-host exporter accepts
`OPFORGE_COMPACT_EXPORT_OUTPUT_EMBED=68020` to make both the release oracle and
bootstrap the embedded configuration. It records generated entry/catalog/asset
origins separately from unchanged native dependencies. Using the known-good
FS-UAE environment from the [execution guide](../../agents/rules/fs-uae.md), add
native qualification explicitly:

```sh
OPFORGE_COMPACT_EXPORT_DIR=/tmp/opforge-embedded-selfhost-new \
OPFORGE_COMPACT_EXPORT_OUTPUT_EMBED=68020 \
OPFORGE_COMPACT_EXPORT_NATIVE=1 \
OPFORGE_FS_UAE_MEMORY_PROFILE=68020-10m \
OPFORGE_FS_UAE_TIMEOUT_MS=1830000 \
OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=1800000 \
cargo test -p asm --lib export_compact_self_host_bundle -- --ignored --nocapture --test-threads=1
```

The overall timeout still bounds the entire run after START; increase both limits
for long self-host cases. Omitting `OUTPUT_EMBED` preserves external-default
output. `OPFORGE_COMPACT_EXPORT_EMBED=68020` independently selects an embedded
bootstrap for that comparison. A host-only export is not native completion.

Copy `opforge_compact` and the needed `packages/` files together. The provisional
native syntax is:

```text
opforge_compact --cpu 6502 -i input.asm --bin output.bin
opforge_compact --cpu 68020 -i input.asm --bin output.bin -d motorola68k -P Development:packages
opforge_compact --runtime-package p.bin -i input.asm --bin output.bin
```

Named selection prefers the matching embedded payload, otherwise loads
`PROGDIR:packages/CPU--dialect.bin` (or the directory selected by `-P`). `-M` and
`-I` retain module/include search. The explicit-package form remains useful for
harnesses and self-hosting; `-d` and `-P` apply to named selection, while an
explicit package path already determines the target and file. It cannot be
combined with named-target selection options. Quoted paths are now supported;
full Rust CLI/output parity remains outside this provisional interface.

Catalog lookup, acquisition/ownership, structural validation and assembly are
separate modules. Catalog and package offsets are relative to their stated bases.
Both storage modes use identical current package bytes and the same validator. External
allocations are owned and released; embedded image bytes are borrowed and never
freed. A matching invalid embedded payload fails; it does not silently fall back.
After preparation, both modes still copy the execution prefix and discard lexical
storage. The whole embedded payload remains part of the executable image, so
tracked allocation savings alone do not establish lower total RAM use.

BS15 uses a 160-byte header. The canonical target offset remains at 124, its
length at 128 and the reserved word at 130; the preparation-only file plan offset
and byte length are at 132 and 136. Built-in `.emit` identity is at 140, CPU
word bytes at 142, and the retained data-plan offset/length at 144/148. Fields
are big-endian and block-relative. The preparation-only metadata plan is at
152/156. Regenerate superseded packages; only BS15 is supported.
Target identity lies inside `RuntimeBytes`, survives preparation, uses safe
filename characters and fits in 26 bytes (plus `.bin`, within the classic
30-byte component limit). The current slice loads assets only from active,
quoted relative `.incbin` statements expanded into packed `.byte` records. Search
roots are relative to the defining file and behavior is limited to the explicit
whole-file cases qualified below. Macro bodies assembled from multiple physical
files still need per-record asset origins; this slice tracks the definition header
file. Full m68020 embedded-config self-hosting is qualified above.
Unknown contracts, wrong target identities, invalid spans, truncation and missing
files must fail before execution. Program interpreters retain opcode/version and
execution bounds checks beyond the common structural validator.

Configured embedded builds use shared `.incbin` on Rust and native. Both the
external-only default and the m68020 embedded configuration can self-assemble;
the latter includes its generated catalog and package asset in the input tree.
This does not establish that every target's instruction forms are implemented.

The catalog's offset tables also require same-section address subtraction. Rust
DATA directives and compact Hunk provenance now recognize cancellation of equal
section bases; unrelated section bases still reject. Instruction fixups evaluate
their scalar before relocation proof, defer unresolved pass-one identities, and
recognize cancelled bases during pass-two reference accounting. Compact `.emit` preparation
was a language gap at P2; that slice used `.byte`, `.word` and `.long` for its native
offset proof. The Rust repair also covers `.emit long`.
The compact frontend rejects the operand form `#'a'-'A'`; the catalog uses a named
numeric ASCII case offset with identical emitted code.

### P2 full current-source self-host proof

The final external-only release CLI assembles all **65 current source files**
(748,663 source bytes, `fnv1a64:210d4e6e48e356c7`) on native FS-UAE, exits zero,
and emits the complete **94,356-byte Hunk exactly equal to fresh Rust output**.
Both have four segments and 106,328 linked reserved bytes. The run uses BS12
m68020 (299,076 bytes), 68020 / 10 MiB and unlimited emulator CPU speed.
Uninstrumented guest START/DONE observed on the host is **563.914554959 seconds**;
native runner duration including preparation/startup is 588.966862375 seconds,
and whole-test wall time is 592.09 seconds. This was a full current-checkout
self-host proof at P2; it does not qualify the subsequent BS13 source changes. The source differs from the earlier
61-file checkpoint, so these times do not establish a before/after speed change.
It does not qualify the 2 MiB target, physical A6000 timing or an embedded build's
self-assembly. The later BS13 file-inclusion cases do not replace that full proof.

### P2 measurements and qualification

The 2026-10-01 comparison uses the same 10,687-byte source, two compatible
modules, 64 referenced blocks, mixed arithmetic/branches/data and a fresh
2,434-byte Rust oracle. FS-UAE uses the 68020/10 MiB profile with the template's
unlimited CPU setting. Release timings are host observations between fresh guest
START/DONE markers, excluding emulator startup; polling resolution and scheduling
limit small differences. These initial P2 measurements precede the final
forward-expression repairs below; they are separate from earlier optimization
gains.

| Release invocation | Observed seconds | Executable bytes | Linked static bytes |
| --- | ---: | ---: | ---: |
| Previous explicit BS11 package | 7.60, 7.73 | 89,880 | 100,864 |
| P2 explicit BS12 package | 7.46, 7.58 | 94,300 | 106,280 |
| P2 named external package | 7.60, 7.61, 7.84 | 94,300 | 106,280 |
| P2 named embedded m68020 | 7.60, 7.60 | 393,376 | 405,356 |

Every listed observation completed with zero exit and exact output. One additional
named-external attempt stalled without a fresh completion and is excluded; two
fresh retries passed. These few observations show similar command costs and do
not establish a meaningful speedup. The m68020 package grew by 28 bytes to
299,076 bytes; all 16 BS12 packages total 1,902,524 bytes.

The final forward-expression changes were measured separately on exactly the
same source and Rust oracle, using `new_explicit` and two fresh runs per state:

| Repair state | Observed seconds | Executable bytes | Linked static bytes |
| --- | ---: | ---: | ---: |
| Before pass-one DATA deferral | 7.46, 7.58 | 94,300 | 106,280 |
| DATA deferral only | 7.69, 7.71 | 94,308 | 106,288 |
| Plus instruction deferral and cancellation proof | 7.67, 7.71 | 94,356 | 106,328 |

Polling resolution and these few observations do not establish a meaningful
gain or regression for either repair. Each state completes
with exit zero and exact output on this benchmark. The pre-repair implementation
fails the new forward-expression correctness probes and updated full self-host,
so failed self-host durations are not compared as speed measurements.
`OPFORGE_COMPARE_NATIVE_ROOT` selects the frozen native implementation for this
test; it does not change its source workload or live Rust oracle. The final
external-only executable is 4,476 bytes larger than P1; embedding 6502 and 68020
produces a 404,600-byte executable. The memory observations below precede these
final expression repairs.

Separate instrumented runs have balanced allocation/free totals, zero terminal
owned bytes and no profiling errors:

| Storage | Package setup seconds | Peak owned bytes | Linked static bytes | Accounted peak bytes |
| --- | ---: | ---: | ---: | ---: |
| External | 0.120 | 840,008 | 110,588 | 950,596 |
| Embedded m68020 | 0.071 | 577,792 | 409,664 | 987,456 |

Package setup includes acquisition, structural validation and frontend
initialization, not isolated file I/O. Accounted peak adds Hunk segment reservations
to tracked owned peak; it excludes OS, executable-loader overhead and untracked
allocations. Embedding saves heap allocation but increases accounted peak by
36,860 bytes here because the whole payload stays resident. Both paths still copy
the 290,512-byte execution prefix. Instrumented timings do not replace release
timings or qualify the 2 MiB self-host target.

With the environment in the [FS-UAE guide](../../agents/rules/fs-uae.md), run:

```sh
OPFORGE_PACKAGE_BASELINE_CLI=/path/to/previous/opforge \
OPFORGE_PACKAGE_BASELINE_PACKAGE=/path/to/previous/m68020--motorola68k.bin \
OPFORGE_PACKAGE_PERF_MEMORY=1 \
cargo test -p asm --lib native_package_loading_performance -- --ignored --nocapture --test-threads=1
```

The previous image/package must be the BS11 checkpoint; the test builds fresh P2
images and the shared Rust oracle. Omit both baseline variables for storage-mode
comparison only. `OPFORGE_PACKAGE_PERF_CASE=new_named_external` selects just that
case for a bounded retry; `OPFORGE_PACKAGE_PERF_ROUNDS` selects 1–8 rounds (default
two). These controls affect the test driver, never production behavior.

Focused package-generation/identity tests, ten fresh native loader positive and
negative cases, and exact native Hunk offset/rejection probes pass. A PC-relative
`LEA dispatchTable(PC),A1` probe rejects before candidate execution in both the
pre-repair P2 implementation and the repaired implementation. That existing
coverage gap remains pending; it is not counted as successful native validation.
The broad Rust assembler run reports 1,923 passed, 69 failed and 385 ignored. All 69 failing tests
also fail individually at clean P1 `9ee9e92a`; they remain existing qualification
debt and are not claimed fixed by P2. Architecture, proof-contract and formatting
guards pass. Full CPU/language/CLI parity and promotion remain pending.

### P1 findings and reproduction

The 2026-10-01 host inventory generates all 16 registered CPU/dialect pipelines
for 14 canonical CPUs without generation errors. Total self-contained package
size is 1,902,110 bytes (1.81 MiB), including duplicated family/core content.
The m68020 package is byte-for-byte equal to the 299,048-byte package used in the
physical self-host export. This is host generation evidence; no native instruction
coverage is inferred from successful export.

| CPU | Dialect | Package bytes | Compact instruction candidates |
| --- | --- | ---: | ---: |
| 45gs02 | transparent | 23,942 | 420 |
| 65816 | transparent | 22,956 | 490 |
| 65c02 | transparent | 14,958 | 260 |
| 8085 | intel8080 | 5,364 | 0 |
| 8085 | zilog | 5,490 | 0 |
| hd6309 | motorola680x | 2,768 | 0 |
| m6502 | transparent | 11,142 | 193 |
| m68000 | motorola68k | 267,648 | 2,882 |
| m68010 | motorola68k | 271,290 | 2,923 |
| m68020 | motorola68k | 299,048 | 3,386 |
| m68030 | motorola68k | 299,026 | 3,386 |
| m68040 | motorola68k | 300,482 | 3,401 |
| m68080 | motorola68k | 343,044 | 4,116 |
| m6809 | motorola680x | 2,600 | 0 |
| z80 | intel8080 | 16,122 | 0 |
| z80 | zilog | 16,230 | 0 |

The six zero-candidate pipelines cannot execute instructions in the compact
native route. They do carry instruction table programs, but no executable compact
candidate rows connect those programs to instruction selection. Their existing
mode keys are opaque Intel descriptors or Motorola names such as `Inherent`, while
the compact producer's synthesized candidate route handles only `implied`.
Close the selector/transport gap in the owning package definitions, using
authoritative tables and existing VM primitives; do not add CPU-specific encoders
or mode-name interpretation to the generic native path. A first bounded bridge
can connect operand-free instruction selectors to the existing table bodies,
including CPU-extension and prefixed instructions and illegal-operand controls.
It would establish that capability, not full instruction coverage.

Final recipe-6 rows are rejection barriers, including deliberate illegal-form
rejections. The inventory records every row and distinguishes matched declared
rejections from other/unclassified barriers. Neither a barrier count nor its
absence proves legal-form coverage; qualify actual inputs and rejection precedence.
The report also includes CPU aliases/defaults and intermediate unsupported plans,
which alone cannot account for barriers introduced during wire lowering.

Choose a fresh absolute directory outside `target` and run:

```sh
OPFORGE_COMPACT_PACKAGE_EXPORT_DIR=/tmp/opforge-package-inventory-new \
cargo test -p asm --lib compact_package_inventory_export -- --ignored --nocapture --test-threads=1
```

The [host-only exporter](../../crates/opforge-asm/src/tests/compact_package_inventory.rs)
writes provisional `CPU--dialect.bin` files and `inventory.json`, retaining any
generation failures in the report. Passing the inventory test means the report
was produced, not that every target succeeded. Inspect its summary and individual
statuses. Existing destinations are refused. Filenames are checked for classic
Amiga's 30-byte component limit; these names are not a final catalog specification.
After the build/test batch, run `make clean`; exports survive outside `target`.

This slice changes no native execution or package-generation behavior, so it
makes no new native speed claim. Its purpose is to make completion work measurable
and prevent missing instruction transports from being mistaken for CPU support.

## Language and product completion tracks

Choose the next structural capability from representative complete files and
known gaps, preserving separation of concerns. These tracks are priorities to
refine, not claims that each is a single small change:

- **Frontend ownership.** Move ordinary statement-head classification ahead of
  value binding under PRVM/package control, including `entry nop` and `entry: nop`.
  Move expression compilation behind EXVM/package ownership and remove the residual
  decoded-string macro fallback. Add grammar through the owning VM boundary rather
  than extending native precedence parsers or ad hoc packed-record heuristics.
- **Expansion and values.** Complete macros/segments, conditionals, loop forms,
  statement patterns and structured values against Rust. Preserve source provenance,
  lexical hygiene, declaration order and pass-dependent behavior. Reconcile stale
  expected-failure tests with fresh probes before counting them as gaps.
- **Data and layout.** Complete shared data/text/encoding/binary-inclusion behavior,
  section/region/mapping/output layout, relocation and stabilization. Keep address
  provenance through fixups; unsupported algebra must fail explicitly. General
  directives remain shared even when output endianness comes from a target package.
- **Modules and parameters.** Finish forms that depend on broader expansion or
  values. `.use ... with (...)` values follow the ordinary symbol value model and
  are evaluated in the importer from values known at the use site. Preserve
  reference-driven whole named-block retention: an internal label reference retains
  the entire block, including fall-through code; merely importing it does not.
  The entry file is a discovery root, not an ordering shortcut.
- **Assembly CLI and outputs.** Integrate target selection/package roots, defines,
  include/module roots and assembly controls; qualify the current Rust assembly
  artifact formats and diagnostics. Remove compact-only invocation requirements
  as normal CLI behavior becomes available.

The current frontend note owns precise supported subsets and residual limits.
Inventory each track from live Rust behavior and reference sources before claiming
completeness. In particular, recheck the old signed-32-bit parameter description
against the newer full-width scalar tests rather than treating stale prose as truth.

## Working and validation boundaries

Each slice delivers something Erik can inspect and test: a complete source,
artifact or reproducible inventory, with explicit remaining limits. Delegate bounded
work to the cheapest capable configured model when coordination saves total cost.
Pause after a few slices to review architecture direction, code structure, package
size and resource growth. New native code should use opForge's language features
where they improve clarity, and short names within qualified modules.

Tokenization produces binary source line by line. Later passes use numeric
identities, structured values and compact expression programs; original text is
for diagnostics. Keep immutable preparation separate from mutable pass/layout
state, free preparation scratch before assembly, and count transient growth/copy
overlap in memory measurements.

Preserve the full self-host case as a regression anchor, alongside focused positive
and negative Rust/native comparisons. A timeout, partial capture or launcher
success is never completion. Real-native proof requires fresh case-bound completion,
explicit exit and exact complete output against live Rust. Host-only inventory
results must be labelled accordingly.

Record each implementation change's time impact separately on unchanged inputs and
settings. Use release timing for user-facing performance and separately instrumented
runs for phase/work/memory attribution. Telemetry uses reusable conditionally
compiled macros with no release code/data overhead. Broaden validation at meaningful
capability boundaries; do not repeat expensive unchanged runs mechanically.
After build/test batches preserve required deliverables outside `target`, run
`make clean`, and leave the empty directory intact.

## Promotion out of experimental

Promote only after the target/language/assembly CLI matrix is qualified, representative
multi-module projects and full self-hosting match live Rust, native failure diagnostics
are useful, and resource requirements are stated honestly. Qualify the 68020/AmigaOS
floor and 2 MiB goal separately from the much larger A6000 environment.

Before integrating the replacement, review combined responsibilities, update current
user/technical documentation, and remove the superseded experimental or legacy code
that no longer serves a validated purpose. Local commits are recovery points;
remote pushes remain separately authorized.
