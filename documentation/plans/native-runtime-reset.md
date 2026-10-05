# Native assembler completion plan

Status: active. BS24 adds numeric FPU operand projections on top of package-owned
runtime state and selector guards. Its [FPU slice](#bs24-fpu-operand-projections--baseline-f7969d7f)
passes focused native checks, two complete FPU examples and full release self-host
Hunk equality on 68020/74 MiB in 1,092.771469042 seconds. No new physical A6000
run or complete peak-memory result is claimed. Packages and bundles must use BS24.
This remains experimental: full self-host equality does not establish full
language, CPU, CLI/output parity or the 2 MiB product goal. Remaining frontend
ownership gaps are tracked in the [compact frontend note](compact-frontend-vm-boundary.md).

Git retains superseded experiments and measurements. The
[workflow](../workflow/README.md) and
[native parity contract](../../agents/rules/native-rust-parity-porting.md)
govern execution; a future plan item alone does not authorize implementation.

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

Current BS24 represents one CPU/dialect pipeline and carries its canonical
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
At C1, initial package selection required `--cpu` or `--runtime-package`; the
source-selection checkpoint below supersedes that restriction;
mid-source package transitions remain P3. `--hunk` currently selects source-configured
sections, rather than general Rust Hunk CLI synthesis. Informational commands
need no input, package or output configuration.
Native runtime-package/dialect/search-root options remain separate from canonical
Rust `.opasm` loading; never mislabel the compact runtime package as the canonical container.

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

#### Source-selected initial package — initial preamble qualified

The compact CLI accepts an initial root-source `.cpu` without a redundant CLI
CPU/package option. Package-independent shared TKVM tokenizes each preamble line;
shared PRVM entry 8 executes generated declaration policy over unbound tokens and
decoded lexemes. The CLI owns input and catalog loading, not CPU identity rules.
Numeric aliases retain their lexical spelling, quoted names are decoded, and
catalog metadata selects embedded or external packages. The shared tokenizer,
bootstrap policy and engine default (`8085`) are generated into a checked asset.
Explicit `--cpu` or `--runtime-package` bypasses bootstrap discovery.

This bounded discovery skips comments, empty lines and `.module NAME`, and stops
at the first other statement or a `.cpu NAME`. Only an unconditional root preamble
is supported: it does not search includes, inactive conditionals or macro bodies,
or implement mid-source package switching. Absent a discovered declaration, it
uses the engine default. Limits are 1,024 bytes per line/decoded-lexeme buffer,
64 tokens per line and 4,096 preamble lines. These are experimental limits, not
full Rust source-selection parity.

Fresh 68020 / 10 MiB native runs qualify six successful cases against exact live
Rust artifacts: numeric source CPU, qualified module plus quoted alias,
directory input, omitted input, no declaration/default, and explicit CPU
precedence. Two further runs qualify nonzero rejection of malformed and unknown
CPU declarations. Both embedded m68020 and external m6502/default packages are
exercised. No-space semicolon comments are covered. The embedded CLI is 444,256
bytes; the prepared source-independent m6502 package is 11,406 bytes. Base-6502
addressing breadth is the next qualification, not implied by these small cases.
This initial checkpoint did not repeat the full self-host run. The subsequent BS17
self-host qualification is recorded below.

Separate unchanged-workload release observations are 11.191842167 and
11.143127709 seconds (median 11.167484938), versus the preceding
11.154376917 median: +0.013 seconds (+0.12%), too small to establish a change.
Explicit CPU selection bypasses bootstrap source I/O in this comparison.
The external CLI is 122,808 bytes (+2,128); linked static reservations are
140,536 bytes (+6,372). Dynamic peak was not measured. Source, output and
321,458-byte m68020 package are unchanged. Focused bootstrap VM tests, generated
asset equality, native formatting and workflow/instrumentation/ownership guards
pass. At this checkpoint, broader 6502 native proof rejected the all-modes
source at `lda ($20,x)` (diagnostic line `$94`, decimal 148). The earlier
`eor $BCDE` interpretation incorrectly treated the hexadecimal line as decimal.
Addressing breadth is qualified separately below.


### Base-6502 package parity checkpoint

The compact native CLI now qualifies all 151 instructions in
`examples/mos6502/6502_allmodes.asm` against the complete live Rust output
(321 bytes). The prepared `m6502--transparent.bin` is source-independent and
13,640 bytes. The CLI embeds m68020 and loads m6502 externally from `-P` roots.
Native code contains no 6502 operand parsing or encoding decisions: the family
package selects typed scalar, wrapped-value and numeric tuple-name projections.
The bounded wrapper helper owns structural preparation separately from selection.

BS17 replaces BS16 without a legacy executor. Header target flags at offset 130
request wrapper preservation based on canonical projections. Semantic encoding
can explicitly emit no bytes; sequence TABLE prefixes bind an empty payload slot
before later stages append their output. The producer accepts that prefix path
only for literal prefixes followed by one trailing payload slot. Flat current-PC
expressions have absolute fixup provenance; section-relative current-PC compound
provenance remains unsupported and fails closed.

Fresh 68020 / 10 MiB release qualification covers five exact-artifact cases:
structural forms, the complete matrix, forward plain/indexed widening, and a
fixed-width forward operand. Ten further fresh runs reject wrong indices,
malformed indirect forms, an invalid accumulator and overwide values. Final matrix
START/DONE time is 2.296862250 seconds (an earlier same-code observation was
2.558640417). These are completion times, not a gain against a working matrix
baseline. The CLI is 445,088 bytes (+832 from the CPU-selection checkpoint);
linked static reservations are 462,780 bytes. Dynamic peak was not measured.

Separate unchanged-workload release measurements are 11.158090041 and
10.890247958 seconds (median 11.0241689995), versus 11.167484938 at the
CPU-selection checkpoint: observed -0.143 seconds (-1.28%). The two-run spread
is larger than this difference; no performance gain is established. Source
(10,687 bytes), exact Rust output (2,434 bytes), 68020 / 10 MiB settings and
m68020 package size (321,458 bytes) are unchanged. The external CLI is 123,628
bytes (+820); linked static reservations are 141,320 bytes (+784). This comparison
isolates this checkpoint from earlier optimization gains.

261 packed-source Rust checks and all 102 package checks pass, alongside the
focused family/VM checks. Native formatting, architecture, instrumentation,
proof, runtime-boundary and test-ownership guards pass. The full BS17 self-host is
qualified separately below; this matrix does not imply full language,
output, 65C02/65816 or 2 MiB product qualification. Initial `.cpu` discovery retains
the root-preamble limits described above; source-dependent package switching is
still deferred.

To reproduce/export with a configured FS-UAE environment, choose a fresh,
nonexistent absolute directory:

```sh
OPFORGE_CPU_PARITY_EXPORT=/tmp/opforge-6502-new \
  cargo test -p asm --lib native_base6502_parity_fs_uae -- --ignored --nocapture --test-threads=1
```

On AmigaOS, run from the exported directory:

```text
opforge main.asm --bin output.bin -P packages
```

Compare the complete `output.bin` with `oracle.bin`. This is a 6502 test bundle,
not a replacement self-host bundle for `run_a6000_selfhost.py`.

### Full MOS-family and shared-language corpus audit

The audit covers all 39 roots in `examples/mos6502` (base 6502, 65C02,
65816, 45GS02/MEGA65 and a source-CPU-switching example), plus all 85 root
examples in the native opcore ownership inventory, with their owned support
files. Shared assembler features apply to every CPU; they are not excluded
from 6502 support. Passing an addressing matrix does not qualify those features.

The two explicit audit tests in
`crates/opforge-asm/src/tests/compact_mos_corpus.rs` separate agreement with
stored references from fresh compact-native agreement with the current Rust
CLI. Stored references are not refreshed. The reference audit uses
`ExecutionMode::Rust`; existing reference-test helpers retain their lockstep
VM defaults. The Rust reference audit records 54 matches, 65 failures and five
unqualified error-output fixtures on these 124 roots. The failures comprise
35 assembly failures, 29 listing-only differences and one Hex/listing difference.
The five fixtures have no comparable partial artifacts; the canonical suite
permits their omission, so they are not five additional Rust regressions.
These baseline problems and absent evidence must be resolved before claiming
canonical reference parity.

Numeric-leading MOS filenames currently generate implicit module declarations
that Rust rejects. To observe the instructions rather than stop there, the native
audit copies each MOS source byte-for-byte to `input.asm` for both engines.
The report identifies the original filename, staged entry and source digest.
This explicitly qualifies source semantics under the neutral filename; it does
not qualify the original CLI filename behavior. Generic roots retain their
filenames and support-file layout.

Native runs use one freshly built release CLI, external current CPU packages,
68020 / 10 MiB FS-UAE settings, and a single explicit `--hex` output. Each
positive case requires fresh guest completion, exit zero and exact equality
with the live Rust Hex artifact; source-declared additional outputs are also
compared. Each expected-error case records fresh nonzero rejection with a
diagnostic, **not diagnostic-text parity**. Error-output fixtures without a
diagnostic contract follow the live Rust exit status, with that basis recorded
explicitly. An unexpected Rust oracle error, timeout or
partial capture cannot establish positive native parity. The current CLI does
not support `--list` or mixed CLI output kinds, so those remain independent
interface gaps. Timings in the report are individual START/DONE observations;
the overall audit time includes emulator startup for each isolated case.

With the FS-UAE environment configured, reproduce the audits with:

```sh
OPFORGE_MOS_CORPUS_REPORT=/tmp/opforge-mos-rust-references.json \
  cargo test -p asm --lib compact_mos_corpus_rust_references -- --ignored --nocapture --test-threads=1
OPFORGE_MOS_CORPUS_REPORT=/tmp/opforge-mos-native-corpus.json \
  OPFORGE_FS_UAE_MEMORY_PROFILE=68020-10m \
  OPFORGE_FS_UAE_TIMEOUT_MS=180000 \
  OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=120000 \
  cargo test -p asm --lib compact_mos_corpus_fs_uae -- --ignored --nocapture --test-threads=1
```

Both audit tests deliberately fail when they find gaps, after recording the
whole selected corpus. `OPFORGE_MOS_CORPUS_CASES` optionally selects comma-separated
path substrings for a focused native retry; the report records that selection.
Do not present a selected retry as a new full-corpus qualification. Reports are
saved incrementally outside the build cache and should be preserved before
running `make clean` at the end of the batch.

#### Observed gaps — 2026-10-03 baseline (`3889198f`)

All 124 roots were attempted with the same 445,080-byte CLI
(`fnv1a64:2c747d77ba201f03`). A separate six-case retry preserves the original
attempts: one failed guest startup and five corrected error-output classifications.
Input, image and package digests match across retries. No production code or
stored reference was changed by this audit.

| Corpus | Roots | Exact live Rust Hex matches | Expected nonzero rejections observed | Other results requiring work or qualification |
| --- | ---: | ---: | ---: | ---: |
| MOS family | 39 | 16 | 0 | 23 |
| Shared opcore | 85 | 3 | 35 | 47 |

The MOS matches comprise two base-6502 and 14 45GS02 examples. The complete
151-instruction base-6502 matrix matches; its fresh retry START/DONE interval
is 2.279971916 seconds. No complete 65C02, 65816 or mixed-CPU example is qualified
by this corpus run. That does not mean their entire instruction sets are absent:
many first stops occur in shared language processing. The unchanged 15-case
base-6502 matrix/negative suite also passes independently.

The main groups are:

- **Shared labelled statements:** ordinary instruction heads after inline bare
  labels are misbound as source symbols. This blocks `6502_simple`,
  `6502_native_cli_smoke`, `65c02_simple` and a 45GS02 branch example. Existing
  bare/colon controls in `binary_source_members.rs` identify this as a shared
  context-binding gap. Labelled `.const` is a separate missing shared declaration
  path; giving a dotted head the right role does not implement that directive.
- **Includes at an Amiga volume root:** support files are present, but the native
  parent-path helper recognizes `/` and not `Work:`. It fails before searching
  configured roots. This blocks four include examples; it is not evidence that
  all includes are unsupported.
- **Other shared forms:** `.var` and collection/struct values, collection
  `.for`/`.bfor` and `.while`,
  statement declarations, module metadata and advanced region/reservation/output
  forms have failing examples. Counter `.for`, grouping and simple sections match.
  `align_simple` completes with zero exit but differs in Hex gap handling: native
  emits zero-filled contiguous data where Rust uses sparse records.
- **Package operand/state work:** first stops directly on indexed indirect JSR,
  PHW immediate, bracketed Z operands, 65816 indirect JMP/JML and 65C02 BBR.
  Inspect package export/projections and native recipe support before changing
  instruction semantics. `.assume` state and source-dependent CPU switching also
  remain unqualified. File/line-zero preparation failures are not localized
  instruction failures.
- **Rejection and completion:** native accepts `loop_pass_instability_error`,
  where Rust rejects it. `led1` reaches guest START but exceeds its 120-second
  execution bound; it remains unresolved, not completed. Five otherwise-positive
  live Rust oracles fail: the placed-section branch artifact example, two macro
  examples and two scope examples. Their native runs cannot establish positive
  artifact parity until the Rust baseline is repaired.

The following BS18 checkpoint addresses shared instruction-head binding after bare
and colon labels, with split-label controls and the affected MOS examples.
Fix the volume-root include boundary separately. Restore the canonical Rust
filename/reference baseline before claiming full example/reference parity;
then choose narrowly defined package operand/state slices from the remaining
first stops. These are audit findings and proposed work, not completed features.

### BS18 shared instruction heads — focused parity

Repair the shared binding defect identified by the corpus audit, without adding
instruction or label grammar to native preparation. The package now supplies a
four-opcode PRVM prefix policy using the existing optional-leading-label grammar.
Before binding, the frontend presents the first two logical tokens; composed-name
recipes preserve their physical extent and colon adjacency. The returned cursor
selects the physical head token. The writer gives ordinary inline heads package
identity, dotted heads shared directive/call identity, and value operands their
existing contextual identities. Register-kind labels follow the Rust PRVM rule.
Normal tokenization, macro fragments and generated-call relexing use this boundary.

BS18 replaces BS17; regenerate embedded and external packages together. The
180-byte header contains the retained four-byte policy and its PRVM version.
There is no BS17 executor or compatibility fallback.

Fresh release comparisons pass split, bare and adjacent-colon forms on both 6502
and 68020, including mnemonic-spelling constants, a mnemonic-spelling label,
forward labels, register operands and labelled shared data emission. Every output
equals its live Rust oracle. The selected real-example retry now matches
`6502_simple`, `6502_native_cli_smoke` and `65c02_simple` exactly as Hex.
This is a four-example retry, not a repeat of the whole corpus audit.

Seven further real-native regression tests pass: bare/colon member operands with
exact Hunk relocations, composed macro labels, mnemonic-spelling labels/constants,
register-spelling inline labels, stale BS17 rejection and rejection of a head-policy
span outside the retained prefix. Host checks pass 275 binary-source tests, two
inline-source oracle tests and the shared PRVM prefix grammar test. Native format,
architecture, fresh-run proof, instrumentation and test-ownership checks pass.
The host inventory validates all 16 BS18 packages; six still have no instruction
candidates. Its target-flag check now recognizes the existing wrapper-preservation
bit rather than treating that BS17 field as reserved. Inventory success is not
instruction-set parity.

`45gs02_rel_branch_overrides` now completes with exit zero but remains unqualified:
native branch displacements are zero while Rust emits 1 through 9. Every branch
targets the next instruction, so zero is consistent with the source. This suggests
a separate Rust/package branch sizing or symbol-resolution defect; its cause has
not been established. Do not refresh its reference or claim parity from completion.

The isolated timing comparison uses the unchanged 10,687-byte mixed workload,
2,434-byte exact Rust output, release image, embedded m68020 package and
68020 / 10 MiB profile with unlimited emulator CPU speed. It measures
host-observed fresh guest START/DONE, excluding guest startup and transfer.

| Measurement | BS17 before (`3889198f`) | BS18 after | Change |
| --- | ---: | ---: | ---: |
| Mixed workload run 1 | 11.280576375 s | 11.281888375 s | +0.001312 s |
| Mixed workload run 2 | 11.008896958 s | 11.593824875 s | +0.584927917 s |
| Two-run median | 11.144736667 s | 11.437856625 s | +0.293119958 s (+2.6%) |
| Embedded CLI image | 445,080 bytes | 449,940 bytes | +4,860 bytes |
| Linked reserved allocation | 462,776 bytes | 467,648 bytes | +4,872 bytes |
| m68020 package | 321,458 bytes | 321,474 bytes | +16 bytes |
| m6502 package | 13,640 bytes | 13,656 bytes | +16 bytes |

The observed time increase is small and close to the variation between these two
observations; two runs do not establish a precise regression estimate. This is a
parity repair, not a speed improvement. The adapter adds 864 bytes of bounded
stack workspace plus 52 bytes of saved registers while active; this is a static
frame calculation, not a measured peak-RAM claim. It makes no new heap allocation.
Before/after image identities are `fnv1a64:2c747d77ba201f03` and
`fnv1a64:6a0848d5a8eda1d5`; both reports retain matching source/oracle digests.

Reproduce the paired native controls and timing cases with the usual configured
FS-UAE environment and an absolute report path:

```sh
OPFORGE_INLINE_HEAD_REPORT=/tmp/opforge-inline-heads.json \
  OPFORGE_FS_UAE_MEMORY_PROFILE=68020-10m \
  cargo test -p asm --lib compact_inline_heads_fs_uae -- --ignored --nocapture --test-threads=1
```

Current reports are `/tmp/opforge-inline-before.json`,
`/tmp/opforge-inline-after.json` and `/tmp/opforge-mos-inline-after.json`.
This BS18 checkpoint did not repeat full self-hosting. Subsequent BS20 baseline
full qualification is recorded below. The following checkpoint addresses
volume-root include handling; the other corpus gaps remain open.

### BS18 volume-root includes — focused parity

The shared parent-path helper now recognizes the Amiga volume separator as well
as directory separators. `Work:main.asm` supplies `Work:` as its parent instead
of failing before sibling lookup or configured-root search. Textual `.include`
and active `.incbin` reuse this helper, including the existing authorization
boundary. Path normalization, volume floors, cycle checks and relative-path
restrictions remain in their existing owners. No grammar, package bytes, heap
allocation or stack-frame size changes were introduced; BS18 remained current
at this checkpoint.

A fresh before-run fails at `.include "part.inc"` in `Work:main.asm`, exit 20.
The focused controls cover sibling lookup at the actual volume root, configured
root fallback, binary assets, normalized cycles, traversal above a volume, and
parent-relative includes with explicit/default roots or outside all allowed roots.
The ordinary source-set helper stages under `Work:sources/`; the new volume
controls deliberately stage directly under `Work:` so a slash cannot hide the
regression. Rust file-resource and CLI-default oracles use the actual CLI.
Nine focused native checks pass, with exact live Rust binary output for positive
cases and fresh completed nonzero diagnostics for rejection cases. Fourteen host
discovery tests pass; native formatting, architecture, fresh-run proof,
instrumentation and test-ownership checks pass. The initial binary-asset test
could not construct a Rust oracle through the low-level graph helper; the corrected
test uses the real CLI resource context and passes fresh native comparison.

An older native negative fixture had expected rejection of a path inside the
entry directory tree, ignoring the CLI's later default `-I` root. The real Rust
CLI accepts that case. It is now a positive default-root comparison, with a
separate negative fixture whose existing include lies outside both the entry
tree and configured roots. The lower-level Rust API still has its explicit-root
rejection control; CLI defaults do not change that API contract.

The four selected corpus retries all reach their included files, but none is
qualified end to end: each next stop is a labelled `.const` declaration in
`cli_json_outputs.inc`, `module_use_lib.inc`, `preproc_syntax.inc` or
`section_module_use_lib.inc`. The report is
`/tmp/opforge-volume-include-corpus.json`. These are completed nonzero failures,
not successful assemblies or include-file lookup failures. Shared `.const`
declarations are the next proposed parity slice.

The isolated release comparison reuses the two recorded observations for the
unchanged `4fde8796` image and repeats the same 10,687-byte source, command,
321,474-byte embedded m68020 package and 2,434-byte live Rust output. Source,
oracle and package digests match. Profile remains 68020 / 10 MiB, unlimited
emulator CPU speed, without telemetry; intervals are host-observed guest START/DONE.

| Measurement | Before (`4fde8796`) | After | Change |
| --- | ---: | ---: | ---: |
| Mixed workload run 1 | 11.281888375 s | 11.390938250 s | +0.109049875 s |
| Mixed workload run 2 | 11.593824875 s | 11.391589375 s | -0.202235500 s |
| Two-run median | 11.437856625 s | 11.391263813 s | -0.046592813 s (-0.4%) |
| Embedded CLI image | 449,940 bytes | 449,948 bytes | +8 bytes |
| Linked reserved allocation | 467,648 bytes | 467,656 bytes | +8 bytes |
| m68020 / m6502 packages | 321,474 / 13,656 bytes | unchanged | 0 bytes |

These timings are effectively unchanged at the observed variation; no speedup
claim follows. The after-image is `fnv1a64:6dc2ee4e0a1197ae`. Baseline observations
are in `/tmp/opforge-inline-after.json`; the current report is
`/tmp/opforge-volume-include-timing.json`.

Focused release timing can select only the unchanged mixed workload using
`OPFORGE_INLINE_HEAD_CASES=mixed-release/0,mixed-release/1` with the existing
`compact_inline_heads_fs_uae` test. The selector validates exact test names and
does not select production behavior. Selected timing runs do not repeat the
full inline-head qualification. No full self-host is claimed for this checkpoint.

### BS19 shared scalar declarations — focused parity

Labelled scalar `.const` declarations now use shared package-owned PRVM policy
and the existing immutable assignment path. Bare and adjacent-colon labels retain
the same constant, signed-value, dependency, scope and import ownership as `=`.
Macro expansions lower after lexical recipes are consumed; configuration capture
lowers its private writer record before parameter evaluation. The policy consumes
numeric packed tokens and returns a bounded operand span; the generic adapter
copies that span into the canonical assignment record. No CPU-specific directive
parser or source-text replay is introduced.

The first importer-parameter probe exposed an earlier selection gap: discovery
skipped `.const` records before `bindCapture` could normalize them. Its classifier
now selects declaration heads through the existing package dictionary and shared
PRVM policy identity. Existing activity and depth checks still govern scheduling;
PRVM remains authoritative for the declaration grammar. The fresh repaired probe
uses an importing `.const` in a `.use with` expression, then an incoming parameter
in the dependency's colon-labelled `.const`; native output matches Rust exactly.

This is scalar support, not complete `.const` value-model parity. Lists, ranges
and structs remain unsupported by the compact scalar expression domain. `.var`
and `.set` are outside this slice. BS19 replaces BS18: regenerate embedded and
external packages together. Its 192-byte header adds a retained five-byte scalar
declaration plan, numeric `.const` identity and PRVM contract version; there is
no legacy executor. Normalization uses a 144-byte request/result frame and a
36-byte VM stage, plus register saves; configuration gains a four-byte callback
slot. No new heap or persistent source-state table is introduced. Full peak RAM
and the 2 MiB product target have not been measured here.

Focused proof covers scalar forward dependencies, signed division, current-PC
and label-difference values, both conditional branches, repeated macro-local
constants, and instruction operands on 6502 and 68020. Six positive native cases
compare `=`, bare `.const` and colon-labelled `.const` against live Rust bytes;
four rejection cases require fresh completed nonzero diagnostics for mixed-form
duplicates, cycles, missing labels and missing values. All ten pass on the final
implementation, as does the separate importer-parameter case. Host qualification
passes 279 packed-source tests, the additional import-parameter oracle, two PRVM
contract tests and the 16-package inventory. Native formatting, architecture,
fresh-run proof, instrumentation, test ownership and workflow checks pass. A
bounded Luna review found no concrete contract, span, register or callback issue.

The four original real examples are retried separately from those controls.
`module_use_include.asm`, `preproc_syntax.asm` and
`section_module_use_include.asm` complete with exact live Rust HEX output.
`cli_json_outputs.asm` gets past the included constant and fails at `START: nop`
on line 5: its selected 8085 pipeline has no compact instruction candidates.
That CPU-package gap remains open. Existing Rust listing-reference drift also
keeps the audit red; no reference output was refreshed or test weakened. These
selected retries do not qualify the complete MOS/opcore corpus.

The isolated release comparison uses the same 10,687-byte mixed source,
command, input digest and exact 2,434-byte Rust output as `f363f4cd`; it contains
ordinary assignments, so this measures the added policy/selection overhead on
existing work. Environment remains 68020 / 10 MiB, unlimited emulator CPU speed,
without telemetry. Package bytes change from BS18 to BS19 as required by the
contract migration. Intervals are host-observed guest START/DONE.

| Measurement | Before (`f363f4cd`) | BS19 after | Change |
| --- | ---: | ---: | ---: |
| Mixed workload run 1 | 11.390938250 s | 11.784530541 s | +0.393592291 s |
| Mixed workload run 2 | 11.391589375 s | 11.545062042 s | +0.153472667 s |
| Two-run median | 11.391263813 s | 11.664796292 s | +0.273532479 s (+2.4%) |
| Embedded CLI image | 449,948 bytes | 451,132 bytes | +1,184 bytes |
| Linked reserved allocation | 467,656 bytes | 468,832 bytes | +1,176 bytes |
| m68020 package | 321,474 bytes | 321,504 bytes | +30 bytes |
| m6502 package | 13,656 bytes | 13,686 bytes | +30 bytes |

This is an observed small cost for added functionality; two observations include
run variation and do not establish a precise population estimate. No speedup is
claimed. The after-image is `fnv1a64:a3e265fef177600e`; source is
`fnv1a64:eea73a7ec2cfca28`, oracle `fnv1a64:5149ec034f77e53c`.
Reports survive build-cache cleanup at `/tmp/opforge-const-parity-final.json`,
`/tmp/opforge-const-corpus-final.json` and `/tmp/opforge-const-timing.json`;
comparison input is `/tmp/opforge-volume-include-timing.json`.

With the configured FS-UAE environment, reproduce the focused proof using
`OPFORGE_CONST_REPORT=/tmp/opforge-const-parity.json` and
`cargo test -p asm --lib compact_const_fs_uae -- --ignored --nocapture --test-threads=1`.
Run `compact_const_import_parameter_fs_uae` separately. The existing timing
selector is `OPFORGE_INLINE_HEAD_CASES=mixed-release/0,mixed-release/1`, with an
absolute `OPFORGE_INLINE_HEAD_REPORT`, invoking `compact_inline_heads_fs_uae`.

This BS19 checkpoint did not repeat full self-hosting. Subsequent BS20 baseline
full qualification is recorded below.

### BS20 scalar mutable declarations — single-sweep checkpoint

Shared PRVM now classifies `.const`, `.var` and `.set` through a 13-byte
package-owned identity/role table. `.var` and `.set` both create or update mutable
scalars; neither overwrites a readonly constant. Preparation emits tag 43 and
runtime updates preserve signed64 values and source-order snapshots. Binding,
expression evaluation and execution remain separate owners. Mutable storage
reuses the existing symbol-state byte; no additional per-symbol heap table is
allocated. BS20 replaces BS19, retaining the same 192-byte header; regenerate
embedded and external packages together.

Dependency preparation distinguishes mutable ancestry from PC/label dependencies.
It owns flag 64 on the headers of immutable declarations that capture mutable
values, including transitive and forward-derived snapshots. Only these snapshots
accept unresolved pass-one placeholders and refresh in pass two; ordinary
immutable consistency checks and readonly ownership remain enforced. Record
bodies and compiled expressions stay unchanged. Macro-private scope records
bypass declaration normalization.

Scalar mutations now execute in source order for flat output, one/two concrete
sections/regions (modes 0/1/3), and Hunk output (mode 5). Hunk serialization order
is independent of statement execution; its focused migration is recorded below.
Mapped modes 2/4 still traverse concrete/logical sections in filtered sweeps and
explicitly reject active `.var`/`.set` records before those sweeps start. Inactive
declarations and unused macro templates do not trigger the guard. Mapped
source-order traversal remains a separate structural slice. Lists, ranges and
struct-valued mutable symbols remain outside
the scalar slice. Imported mutable updates are unqualified; this is **not general
mutable-variable parity or an integration-ready completion of the slice**.

The focused controls exercise bare/colon declarations on 6502 and 68020,
reassignment, location-counter values, signed division, wide-value arithmetic,
conditionals, macro-local ownership, forward/self references and direct/transitive
readonly snapshots. A reduced probe exposed the existing native `.byte` range
policy: `.byte wide>>32` with `wide .var $ffffffff+1` rejects natively, whereas
Rust truncates to zero (shift counts mask to 31). The mutable control uses
explicit narrowing and `.long wide/2`; this does not resolve the general narrow
data-emission mismatch. The existing 6502 package-register name restriction also
excludes a scalar declaration named `a`; probes use nonreserved names.

Fresh proof passes 17 mutable controls (12 positive exact-artifact cases and
five completed diagnostic rejections), plus three immutable regression controls
(two exact artifacts and one cycle rejection). Host qualification passes 283
packed-source tests, three current mutable Rust oracles, three shared declaration
VM tests and all 16 package generations. These prove the selected flat-output
cases; they do not qualify section traversal, the full corpus or compound values.

The proof reports identify the actual source, image and package digests:
`/tmp/opforge-mutable-parity-final.json`, `/tmp/opforge-mutable-operands.json`
and `/tmp/opforge-mutable-const-regressions.json`. Reproduce the mutable proof
with the configured FS-UAE environment and
`OPFORGE_MUTABLE_REPORT=/tmp/opforge-mutable-parity.json cargo test -p asm --lib compact_mutable_fs_uae -- --ignored --nocapture --test-threads=1`.
The optional `OPFORGE_DECLARATION_CASES` selects exact comma-separated case names;
selected retries are not a complete declaration qualification. The host section
oracle separately specifies CODE `[2]` / DATA `[1,3]` under reordered Hunk section
output; native now matches that mutable case after the Hunk traversal repair below.

The isolated release comparison uses the identical 10,687-byte mixed source
(`fnv1a64:eea73a7ec2cfca28`), command and live 2,434-byte Rust output
(`fnv1a64:5149ec034f77e53c`) as BS19. Environment remains 68020 / 10 MiB,
unlimited emulator CPU speed, without telemetry. This workload uses ordinary
immutable assignments, so it measures added policy/runtime overhead on existing
work rather than the benefit of using mutable variables.

| Measurement | BS19 (`ddc82248`) | BS20 checkpoint | Change |
| --- | ---: | ---: | ---: |
| Mixed workload run 1 | 11.784530541 s | 11.538642084 s | -0.245888457 s |
| Mixed workload run 2 | 11.545062042 s | 11.548778833 s | +0.003716791 s |
| Two-run median | 11.664796292 s | 11.543710459 s | -0.121085833 s (-1.0%) |
| Embedded CLI image | 451,132 bytes | 451,808 bytes | +676 bytes |
| Linked reserved allocation | 468,832 bytes | 469,500 bytes | +668 bytes |
| m68020 package | 321,504 bytes | 321,532 bytes | +28 bytes |
| m6502 package | 13,686 bytes | 13,714 bytes | +28 bytes |

Two observations do not establish a speedup: the faster median reflects a slower
first baseline observation, while the second pair is nearly identical. No
material runtime cost is demonstrated by this limited comparison. The checkpoint
image is `fnv1a64:54afadc7bd0fc8f8`; retained timing reports are
`/tmp/opforge-const-timing.json` and `/tmp/opforge-mutable-timing.json`. Total peak
RAM and the 2 MiB target are unqualified. Reproduce with
`OPFORGE_INLINE_HEAD_CASES=mixed-release/0,mixed-release/1`, an absolute
`OPFORGE_INLINE_HEAD_REPORT` and the `compact_inline_heads_fs_uae` ignored test.

Two real examples are retried separately. `65816_wide_const_var.asm` completes
with exact live Rust HEX; it has declarations but no instructions/emitted bytes,
so this does not establish 65816 instruction or native listing parity.
`6502_first_run_artifact_contract.asm` still rejects its `.region` at source line
7, before the mutable declaration; Rust reference comparison also remains red.
The selected audit therefore fails after both cases, retaining its report at
`/tmp/opforge-mutable-corpus-retry.json`. No goldens were refreshed. Reproduce
with `OPFORGE_MOS_CORPUS_CASES=6502_first_run_artifact_contract.asm,65816_wide_const_var.asm`,
an absolute `OPFORGE_MOS_CORPUS_REPORT` and `compact_mos_corpus_fs_uae`.

#### Mapped layout guard and retained guard baseline

The guard inspects bounded packed record headers once before reordered assembly
sweeps. It uses numeric layout modes and the shared mutable declaration marker;
it does not parse source strings or dispatch through family-specific semantics.
The existing frame's reserved word carries a dedicated failure reason, without
changing frame size or allocating another table. Diagnostics preserve source
provenance, and a rejected run creates no output artifact.

The guard checkpoint (`79838c60`) qualified nine controls: seven exact live Rust artifact
comparisons (flat mutable output, one/two concrete layouts, readonly Hunk,
marker-valued Hunk data, unused macro, and inactive declaration), plus two
Hunk diagnostic rejections (direct and macro-expanded mutation). The Hunk
traversal repair below replaces those rejections with exact-output comparisons.
The readonly comparisons use actual nonempty Hunk files. Four affected Rust
oracle tests, workflow checks, native formatting, proof-contract, test ownership and
instrumentation checks pass; a bounded Sol review found no actionable production
issues. Reports are retained at `/tmp/opforge-mutable-layout-hunk-qualified.json`
and `/tmp/opforge-mutable-layout-host-final.log`.

The one-map and two-map mutable controls now pass with the required layout
rejection, exit 20 and no output. The preparation repair below restores the
previously blocked readonly mapped-body comparison as well. Separate baseline
proof at `1020f7d0` established that the preparation failure predated this guard;
it was not introduced by mutable-layout validation.

Reproduce the qualified controls with the configured FS-UAE environment,
`OPFORGE_MUTABLE_LAYOUT_REPORT=/tmp/opforge-mutable-layout.json` and
`cargo test -p asm --lib compact_mutable_layout_fs_uae -- --ignored --nocapture --test-threads=1`.
For the qualified mapped controls, substitute `compact_mutable_mapped_layout_fs_uae`.
`OPFORGE_DECLARATION_NATIVE_ROOT` optionally points the proof builder at an isolated
native source snapshot for a before/after check; package generation still uses the
current host registry, so compare only snapshots sharing that package contract.

The separate guard comparison retains the identical mixed source, command,
packages, live Rust output and 68020 / 10 MiB release profile described above.

| Measurement | BS20 before guard (`1020f7d0`) | Guard checkpoint | Change |
| --- | ---: | ---: | ---: |
| Mixed workload run 1 | 11.538642084 s | 11.575919500 s | +0.037277416 s |
| Mixed workload run 2 | 11.548778833 s | 11.604494708 s | +0.055715875 s |
| Two-run median | 11.543710459 s | 11.590207104 s | +0.046496645 s (+0.4%) |
| Embedded CLI image | 451,808 bytes | 452,196 bytes | +388 bytes |
| Linked reserved allocation | 469,500 bytes | 469,868 bytes | +368 bytes |
| m68020 / m6502 packages | 321,532 / 13,714 bytes | unchanged | 0 bytes |

This small observed time increase includes run variation; two observations do
not establish a precise overhead estimate. The flat benchmark takes the guard's
constant-time allow path and did **not** measure the full record scan used by
Hunk/mapped layouts at that guard checkpoint. No new heap storage is added; total peak RAM remains
unqualified. The separate before/after reports are
`/tmp/opforge-mutable-timing.json` and `/tmp/opforge-mutable-layout-timing.json`.

### BS20 source-order Hunk traversal

Hunk assembly now executes packed records once per pass in source order. Each
section retains its local PC and initialized-byte cursor; reopening resumes them.
Pass one measures all declared sections, then plans selected payload offsets in
Hunk output order. Pass two routes bytes into those bounded ranges and requires
matching final PC/payload extents. BSS consumes reservation space without an
initialized payload. Outside-section PC remains separate and starts at zero.
Unselected sections still execute mutations and definitions, matching Rust, but
contribute neither output bytes nor relocation/emission callbacks.

Assembly owns record traversal and repetition; section state owns switching and
bounded layout. No per-record symbol snapshot table or speculative execution pass
is added. Mapped modes 2/4 retain the mutation guard because their concrete/logical
placement planning is separate from this Hunk migration.

Readonly snapshots with mutable ancestry carry runtime state 4 only when the
resolved definition expression proves absolute through the existing numeric
relocation classifier. They refresh value and proof at their declaration each
pass. Unresolved placeholders do not establish that proof. State 2 still denotes
precomputed absolutes and alone skips statement evaluation. Address-derived
snapshots remain conservatively unsupported rather than losing relocation
identity; their scalar value alone cannot prove absolute provenance.

Fresh native qualification passes all six exact live Rust Hunk controls: forward
readonly snapshots, outside-section PC, reopened/reordered sections, unselected
sections, combined scalar snapshots/relocations/BSS, and nested/zero loops.
Existing layout controls pass all nine cases, including macro-expanded mutations;
both mapped-mutation controls retain their dedicated rejection and no output.
Existing Hunk section, PC-dispatch and forward-immediate controls pass. An obsolete
negative test expected `payload+1` to reject: fresh comparisons at `b4382b27` and
this checkpoint both match Rust. It is now a positive addend test, with unsupported
`payload*2` as the fresh rejecting control. An address-derived snapshot also
rejects without output. Host checks pass 293 packed-source tests, 22 focused Hunk
oracles and 18 progress-decoder tests, plus formatting, workflow/boundary,
instrumentation, test-ownership and native-proof guards. Independent read-only
review found no actionable issues. The ownership/no-growth guard also passes
following comment-only repair of 12 pre-existing missing module-owner annotations;
a final fresh snapshot/relocation/BSS control matches Rust and retains the exact
release image digest (`/tmp/opforge-hunk-traversal-owner-final.json`).

The subsequent Rust Hunk repair preserves scalar assignment provenance through
aliases and source-order snapshots. Direct `.long $` now relocates to its current
section; mutable-derived readonly scalar instruction immediates are absolute;
address-derived snapshots retain their section and frozen addend. Mapped logical
Hunk references use the concrete segment identity and include its existing bytes
in their addends. Fixup inputs keep full addresses separate from section-relative
addends so package-owned positional projections also preserve placed/mapped
PC-relative aliases. At that checkpoint, native section-relative `.long $`,
address-derived snapshots and general layout aliases remained fail-closed gaps;
the following DATA-PC slice addresses only the first of these. The Rust repair does not
establish those native cases or complete Hunk parity.

Rust qualification passes all 46 existing Hunk instruction relocation controls,
the new provenance/mapped-layout regressions and all 521 VM tests. The final
broad assembler run reports 1,876 passing, 203 failing and 461 ignored tests;
201 failures also occur in the unmodified baseline. The other two were obsolete
expectations for a missing member projection and the old callback signature;
both were corrected and pass separately. Six baseline failures are resolved by
the full-address fixup repair. This is not broad product qualification.
Fresh FS-UAE scalar-snapshot assembly matches the live Rust Hunk oracle
(1.272 seconds START-to-DONE); the separate section-PC probe rejects with exit 20.
Host assembly of all 94 native source files succeeds, producing the unchanged
452,776-byte embedded executable and unchanged 321,532-byte package. The retained
bundle is `/tmp/opforge-hunk-repair-selfhost-final`; its manifest explicitly says
native validation was not run. No new full native self-host or performance gain
is claimed by this correctness repair.
The installed 0.9.7 CLI predates the committed mapped-label replay fix
(`7349ca53`); the exact split-file report now has a live CLI regression checking
`02 09 60`, header `$0900` and worker entry `$0902`. The current Presenter project
also assembles successfully with the repaired Rust CLI, removing its reported
branch-range blocker; its files were not changed. This host build does not qualify
the Presenter application on hardware.
Mapped mutations, compound mutable values, complete corpus qualification and a
fresh BS20 full self-host remain outside this checkpoint.

#### Native section-relative DATA PC

The compact native DATA path now retains the current Hunk source section as
runtime context. `$` therefore carries a section base without becoming a fake
symbol, serialized pointer or CPU-specific expression rule. The bounded affine
proof distinguishes symbol IDs, current-PC identity and absolute values; equal
nonzero section identities cancel under subtraction. Output selection/reordering
does not redefine that identity. Instruction transport still accepts symbol IDs
only and rejects the new PC identity explicitly.

DATA operands all evaluate at their statement's starting PC. Emission still
advances the live PC and records each field's actual relocation offset. This
also fixes `.long $,$` in flat output and applies to shared `.emit`; subsequent
lines see the advanced PC normally. No package or packed-source format changed.

Fresh FS-UAE comparisons cover DATA PC relocations, absolute addends,
same-section cancellation, same-line operands, narrow absolute cancellation,
unselected sections and reordered/reopened sections. Six invalid-expression
cases complete with exit 20 and no Hunk file, including equal numeric offsets
in distinct sections. The flat-PC control and the existing mixed scalar
snapshot/relocation/BSS control match their live Rust oracles.

The imported mapped logical-section Hunk fixture still rejects at its import,
before DATA execution. Replacing `$` with ordinary numbers reproduces this in
both baseline `0650076b` and the current implementation with identical source,
packages and Rust oracle. The desired positive Rust/native test remains separate
and explicitly marked as a known native gap. This slice does not establish
mapped Hunk parity, instruction `$`, address-derived snapshots, general aliases
or a new full native self-host.

The release image grows from 452,776 to 452,936 bytes (+160). Persistent native
state grows by six bytes (current-section identity and statement PC); affine
proof scratch grows by 16 bytes and the shared DATA call frame by four bytes.
PC preservation and helper calls also use bounded stack scratch. The m68020
package remains 321,532 bytes. These are implementation sizes, not a new measured
peak-memory claim.

The small mixed control measures 1.275279 seconds before and 1.262542 after.
This is too short to establish a performance gain. Reports are retained as
`/tmp/opforge-section-pc-control-{baseline,current}.json`; the source and exact
Rust Hunk digests match across the pair. The existing 128-fragment readonly Hunk workload (18,391 source bytes) measures:

| Fresh release run | Baseline `0650076b` | DATA-PC repair |
|---|---:|---:|
| 1 | 18.990015 s | 19.021078 s |
| 2 | 18.995526 s | 18.977380 s |
| Median | 18.992770 s | 18.999229 s |

The median difference is +0.006458 seconds (+0.034%). Two runs per
build do not establish a reliable change this small. All four produce the exact
same live Rust Hunk (`fnv1a64:0a02e9fc68860da3`), with the same source
(`fnv1a64:1d62eb38a482ac9a`), package bytes and command. This measures only this
correctness slice on that workload, not full self-host duration or a hardware
speedup. FS-UAE uses the existing 68020/10 MiB profile; timings are guest
START-to-DONE host wall time and exclude emulator startup. Reports are
`/tmp/opforge-section-pc-measurement-{baseline,current}.json`.

Final checks pass 25 focused Rust Hunk tests and the shared emission Rust matrix,
eight fresh native DATA-PC Hunk comparisons, the flat control, six required
native rejections, and the mixed regression before/after. Independent read-only
review, Rust/native formatting, CPU boundaries, runtime ownership/no-growth,
instrumentation safety, native test ownership, fresh-proof and benchmark-selector
guards pass. The separate mapped-Hunk positive probe still fails as documented;
this is not broad corpus or self-host qualification.

#### Native readonly address snapshots and aliases

Readonly statement execution now belongs to `binary_constants.asm`; the assembly
traversal delegates to it. The helper captures a numeric value and its section
proof together in the existing Values/Defined/SectionIds arrays. Address aliases
and `$` snapshots retain the declaration's section; absolute mutable-derived
snapshots remain numeric. Alias use does not reparse or reevaluate a definition.
Same-section differences cancel, while unsupported arithmetic retains no section
proof and cannot become absolute through another assignment or `alias-alias`.

An unresolved pass-one readonly value remains unavailable. Pass two must resolve
it before use; the supported forward-label case declares the alias before its
use. Known immutable layout values still have to remain stable, while snapshots
with mutable ancestry refresh at their declaration. This preserves the existing
two-pass boundary: unresolved alias chains/use before an unresolved declaration
and variable-size convergence are not established by this slice.

Instruction reference counting now includes nonabsolute aliases with missing
proof, for raw and compiled operands. Otherwise an unsupported alias could vanish
from the requirement for package-proven fixups. Reserved package names such as
registers are excluded; CPU semantics stay in package projections. The packed
readonly declaration marker has a shared symbolic name, with no format change.

Seven positive Rust/native cases cover captured address values, frozen mutable
addends, chained aliases, same-section cancellation, a later label, a `$` snapshot,
absolute instruction fixups and a same-section positional branch. The instruction
cases were rerun on the final reference scanner. Seven rejecting cases include
hidden unsupported assignments and an immediate instruction use; the latter also
was rerun after the scanner review. Four existing Hunk controls pass on the final
image: forward scalar snapshot, outside-section PC, mixed snapshots/relocations/BSS
and nested/zero loops. Three 6502 flat-output checks also pass on that exact image:
snapshot update, derived snapshot and unresolved snapshot. The symbolic-marker
cleanup retains the same release image fingerprint. Focused Rust checks pass
13 tests (11 native tests ignored).
Independent review found and resolved the wrapped-operand/package-name counting
issues; the final review found no further actionable issue.

The release image grows from 452,936 bytes at `cdcd9f01` to 453,004 (+68),
`fnv1a64:fed6d4afd1975c82`. Symbol slots, context and persistent storage are
unchanged. The extracted helper's register-preserving call adds 60 bytes of bounded
transient stack at that boundary. This is not a peak-memory measurement. The
m68020 package remains 321,532 bytes.

The 128-fragment scalar-snapshot workload exercises declarations, mutation,
reopened sections and data emission on both implementations. Fresh uninstrumented
FS-UAE 68020/10 MiB timings exclude emulator startup:

| Run | Baseline `cdcd9f01` | Readonly address repair |
|---|---:|---:|
| 1 | 13.564583 s | 13.357474 s |
| 2 | 13.559268 s | 13.629594 s |
| Median | 13.561925 s | 13.493534 s |

The median change is -0.068391 seconds (-0.504%). Variation in the two current
runs exceeds that difference, so no speedup is established. All four outputs
match the same live Rust Hunk (`fnv1a64:7eaac19333981023`), source
(`fnv1a64:a7120dfa242f6e68`, 15,748 bytes), packages and command. Reports are
`/tmp/opforge-hunk-alias-measurement-{baseline,current}.json`.

Rust currently accepts `bra.w alias` when `alias` hides `payload*2`; the compact
native positional proof requires a nonzero compatible section identity. This
specific positional Rust behavior needs separate review and was not added to
the paired rejection matrix.
Mapped logical-section Hunk, direct instruction `$` and broader unresolved alias
resolution remain gaps. This slice does not claim a new full native self-host or
complete Hunk/assembler parity.

Reproduce the new positive controls with the configured FS-UAE environment and
`cargo test -p asm --lib compact_hunk_traversal_fs_uae -- --ignored --nocapture
--test-threads=1`. Set `OPFORGE_HUNK_TRAVERSAL_REPORT` to capture case identities,
image/package digests and exact-match outcomes. Set
`OPFORGE_DECLARATION_TELEMETRY=1` and
`OPFORGE_DECLARATION_CASES=snapshots-relocations-bss` for the instrumented control;
phase 300 reports actual source sweeps and record visits independently of guest
completion and artifact proof. The fresh instrumented combined
snapshot/relocation/BSS control also completes with exact live Rust output: phase 300 records two scheduled source sweeps and 54
record visits. Its separate image is 459,752 bytes (`a6433b21a77efb0f`); report
`/tmp/opforge-hunk-traversal-telemetry-final.json` retains the capture and
`/tmp/opforge-hunk-traversal-progress-final.txt` is the decoder input. This capture
is diagnostic proof, excluded from release timing. Release builds omit counter
storage, updates and reporting through the existing macro gates.

Reports for this checkpoint are `/tmp/opforge-hunk-traversal-qualified.json`,
`/tmp/opforge-hunk-traversal-regressions.json` and
`/tmp/opforge-hunk-addend-comparison.json`. The regression report retains the
obsolete negative-test failure; the addend report records its baseline/current
positive comparison and the corrected negative control.

Isolated release comparison against the preceding `b4382b27` checkpoint uses the
same 68020 / 10 MiB FS-UAE profile, unlimited emulator speed and host START-to-DONE
time, excluding emulator startup. Each workload has two fresh native runs and
exact output comparisons; telemetry is disabled. Both inputs, commands, live Rust
oracles and package bytes/digests are identical before/after.

| Measurement | Before (`b4382b27`) | This checkpoint | Change |
|---|---:|---:|---:|
| Readonly Hunk run 1 | 19.016693 s | 18.970511 s | — |
| Readonly Hunk run 2 | 18.986377 s | 19.006571 s | — |
| Readonly Hunk median | 19.001535 s | 18.988541 s | -0.0130 s (-0.068%) |
| Flat mixed run 1 | 11.645048 s | 11.668905 s | — |
| Flat mixed run 2 | 11.636356 s | 11.423127 s | — |
| Flat mixed median | 11.640702 s | 11.546016 s | -0.0947 s (-0.81%) |
| Embedded release image | 452,564 bytes | 452,776 bytes | +212 bytes |
| Linked Hunk reservations | 470,236 bytes | 470,524 bytes | +288 bytes |
| Bounded section scratch | prior layout | prior layout +70 bytes | +70 bytes |

The readonly Hunk input reopens two sections across 128 fragments and uses mixed
instructions/data: 18,391 source bytes, 1,844 output bytes. Its source digest is
`1d62eb38a482ac9a` and output digest `0a02e9fc68860da3`. Flat mixed input is
10,687 bytes, output 2,434 bytes; digests remain `eea73a7ec2cfca28` and
`5149ec034f77e53c`. Image digests are `0b8861cfd55707dc` before and
`3c8722a0693935bb` after. Both use unchanged 321,532-byte m68020 and 13,714-byte
m6502 packages (`d313ed96a210a3c7` / `b4cff8b25e13282a`). These two-run observations
show no material runtime change, not a demonstrated speedup. Linked reservations
are static executable allocation, not peak working memory; no new dynamic snapshot
table is allocated. The section scratch increase is already included in linked
reservations and must not be added again.

Reproduce with `compact_hunk_traversal_measurement_fs_uae` and
`OPFORGE_HUNK_TRAVERSAL_REPORT`; for flat controls use
`compact_inline_heads_fs_uae`,
`OPFORGE_INLINE_HEAD_CASES=mixed-release/0,mixed-release/1` and
`OPFORGE_INLINE_HEAD_REPORT`. Reports are
`/tmp/opforge-hunk-traversal-baseline.json`,
`/tmp/opforge-hunk-traversal-timing.json`,
`/tmp/opforge-map-repair-timing.json` and
`/tmp/opforge-hunk-traversal-flat-timing.json`. This is focused functionality and
relative emulator measurement, not an A6000 timing, 2 MiB qualification, complete
6502/corpus parity or a fresh full self-host proof.

### BS20 mapped preparation — configuration transfer repair

Discovery already collected active section maps, but the transition into
dependency-ordered preparation transferred only scalar import parameters.
Dependency logical sections therefore ran without their mappings; replaying the
import later encountered the section preparer's late-map restriction.

Preparation now transfers the bounded map metadata before dependency bodies run.
It rebinds owner/module identities canonically and logical/concrete names in the
importing module's lexical scope. No configuration-scope IDs or pointers survive.
Each seeded map must match its ordinary import replay exactly once, and finish
rejects unconsumed maps. This preserves the native boundary that all maps owned
by an importer precede its concrete sections. The two-map limit and rejection
of late maps remain explicit native limitations; Rust accepts the late forms.
No CPU-specific processing or package-format change is added.

Fresh native checks restore exact live Rust output for one/two mapped layouts,
logical-section bodies, selected parameterized imports and inactive imports.
Inactive imports neither discover missing modules nor consume map capacity.
Overlap and both late-map controls complete with the expected native rejection.
The real ABI and configured dependency-chain preparation regressions also pass.
The one/two-map mutable controls now reach their dedicated layout diagnostic;
flat mutable and readonly Hunk exact comparisons remain passing. The subsequent
Hunk traversal repair also supports Hunk mutations; mapped mutable traversal
remains deferred.

The packed-source host suite passes 285 tests. Workflow, native formatting
(81 files), Rust formatting, native proof contract, test ownership and
instrumentation safeguards pass. A bounded independent Sol review found no
actionable production issues. Reports survive cache cleanup at
`/tmp/opforge-map-repair-guard-final.json`, `/tmp/opforge-map-config-native.log`,
`/tmp/opforge-map-regressions.json`,
`/tmp/opforge-map-repair-nonmap-regressions.json` and
`/tmp/opforge-map-repair-packed-host.log`.

Reproduce the map/parameter controls with `compact_map_configuration_` as the
ignored native test filter, and the mutable map guard with
`compact_mutable_mapped_layout_fs_uae`, using the configured FS-UAE environment
and `--ignored --nocapture --test-threads=1`. The regular
`map_configuration_rust_oracles` test demonstrates Rust acceptance of the late
forms; their native rejection is a retained gap, not a parity claim.

The isolated repair comparison retains the same mixed source, command, packages,
exact live Rust output and uninstrumented 68020 / 10 MiB profile as the guard
checkpoint (`79838c60`). Emulator startup is excluded; speed is unlimited, so
these are relative emulator measurements, not physical hardware predictions.

| Measurement | Before map repair | Map repair | Change |
| --- | ---: | ---: | ---: |
| Mixed workload run 1 | 11.575919500 s | 11.645047958 s | +0.069128458 s |
| Mixed workload run 2 | 11.604494708 s | 11.636356208 s | +0.031861500 s |
| Two-run median | 11.590207104 s | 11.640702083 s | +0.050494979 s (+0.4%) |
| Embedded CLI image | 452,196 bytes | 452,564 bytes | +368 bytes |
| Linked reserved allocation | 469,868 bytes | 470,236 bytes | +368 bytes |
| m68020 / m6502 packages | 321,532 / 13,714 bytes | unchanged | 0 bytes |

Two runs do not separate this small observed increase from run variation. This
flat workload measures the unaffected-path cost; it does not measure map-copy
work on a mapped project, and the failing mapped baseline permits no speedup
claim. Section state gains one 2-byte pending-map word per scope, without a new
heap table. Total dynamic peak RAM and the 2 MiB target remain unqualified.
Before/after reports are `/tmp/opforge-mutable-layout-timing.json` and
`/tmp/opforge-map-repair-timing.json`; their executable digests are
`fnv1a64:6bb22fa9e53fd622` and `fnv1a64:0b8861cfd55707dc` respectively.

No full corpus completion is claimed for the mapped-preparation repair.
Subsequent BS20 baseline full self-host qualification is recorded below.

### Current full self-host qualification and search roots

The complete BS20 baseline compact implementation (`7f8bebf2` native source)
assembles itself on native with a fresh case-bound START/DONE protocol, exit zero
and exact equality against the live Rust oracle: all 452,776 bytes of the embedded
Hunk. Bootstrap and output are the same release configuration, each containing
one identical 321,532-byte m68020 package. Both repository and relocated bundle
inputs were freshly assembled by Rust before native execution.

| Release self-host property | Current BS20 |
|---|---:|
| Complete source inputs | 94 |
| Input bytes, including generated package asset | 1,360,588 |
| Source manifest fingerprint | `fnv1a64:ddfdd62b45de2cc1` |
| Release/bootstrap Hunk bytes | 452,776 |
| Complete Hunk fingerprint | `fnv1a64:3c8722a0693935bb` |
| m68020 package fingerprint | `fnv1a64:d313ed96a210a3c7` |
| Linked static reservations | 470,524 bytes |
| Native START/DONE duration, telemetry disabled | 1,037.690232625 s (17m 17.69s) |

The FS-UAE profile is 68020 / 74 MiB, with unlimited emulator speed and host
START/DONE timing excluding emulator startup. This is complete native self-host
proof for this source/package state, not physical hardware timing, full assembler
parity or qualification of the 2 MiB product target. The earlier BS17 run took
950.411414625 seconds on the same profile but used 90 inputs / 1,291,116 bytes and
a 445,080-byte executable. Those different workloads do not isolate change cost;
individual feature comparisons remain in their focused checkpoint notes. Full
release-run dynamic peak is not measured.

The separate phase-only instrumented bootstrap also completes the **entire same
self-host case**, with fresh completion, exit zero and the exact same release
Hunk. Source mappings, command, package bytes and release oracle are identical;
bootstrap instrumentation is the only configuration change. It enables the
existing memory, phase/progress, sampled binding, template and input probes,
excluding detailed per-opcode tokenizer probes via `OPFORGE_PHASE_ONLY=1`.

| Instrumented full-run measurement | Value |
|---|---:|
| Host START/DONE | 1,147.941797667 s (19m 07.94s) |
| Guest preparation clock | 665.36 s (11m 05.36s) |
| Guest assembly clock | 481.54 s (8m 01.54s) |
| Scheduled source sweeps | 2 |
| Record visits, including controls and loop replay | 122,798 |
| Peak tracked owned allocation | 22,380,568 bytes (21.34 MiB) |
| Tracked live after preparation | 2,691,072 bytes |
| Tracked live during assembly | 3,510,272 bytes |
| Terminal tracked live allocation | 0 bytes |
| Total allocated / freed capacities | 50,449,312 / 50,449,312 bytes |
| Profiling errors / allocation failures | 0 / 0 |
| Packed source bytes | 1,120,052 |
| Compiled expressions / evaluations | 33,465 / 93,810 |
| Compiled expression program bytes | 126,200 |
| Instrumented bootstrap Hunk / static reservations | 462,068 / 482,064 bytes |
| Instrumented bootstrap fingerprint | `fnv1a64:7dc8ac8bf15a6688` |

Exclusive preparation stage timings use the guest E-clock:

| Preparation responsibility | Seconds |
|---|---:|
| Binding and raw records | 390.130 |
| Tokenization | 107.299 |
| Source I/O and other preparation | 79.270 |
| Module discovery | 43.042 |
| Runtime finalization | 26.782 |
| Expression preparation | 15.790 |
| Package setup | 3.044 |
| Total | 665.357 |

Stage totals agree with the coarse preparation clock within 3.2 ms. Input
collection is nested within preparation (10.674 seconds, 1,302,502 bytes,
512 reads), not an additional exclusive stage. Sampled binding/template details
are retained in the decoded report. Token opcode counters are disabled in this
phase-only build and must not be interpreted as zero tokenizer work.

The instrumented run is 110.251565042 seconds (+10.62%) longer than release.
This single pair includes probe overhead and run variation; measured phase ranks
are instrumented observations, not the release phase split. Allocation counts
exclude static executable reservations and untracked OS storage. They establish
balanced tracked ownership for this full case, not total required RAM. Peak is
not compared as an incremental cost: no matched BS17 full-run dynamic baseline
was measured. The current 2 MiB product target remains unqualified.

The portable bundle command exercises the root source `.cpu 68020`:

```text
opforge -i src/experimental/opforge_compact_cli.asm --hunk output.hunk -M src -I src/debug
```

The entry is below the project root, so one recursive `-M src` exposes sibling
modules. Bare debug include names need `-I src/debug`. A root entry layout can
remove those explicit paths later. The hardware runner checks source/package/image
identity and the source-selected preamble, at that checkpoint accepted only the BS20 package
header, and leaves VM opcode validation to native execution. Header/bounds,
source/output identity and fresh-completion tests pass (84 performance-tool tests).

The release bundle is `/tmp/opforge-selfhost-bs20-release`, now selected by
`/tmp/opforge-a6000-current`. Bootstrap and output both embed only
`m68020--motorola68k.bin`; the named verification package is retained locally,
not used as a runtime fallback. Its manifest records fresh FS-UAE exact-Hunk
validation. The transfer dry run succeeds. It has not been run on the physical
A6000; no new hardware time is claimed. Use the unchanged Mac Terminal command:

```sh
python3 /Users/erik/Code/Retro/opForge/scripts/performance/run_a6000_selfhost.py
```

The separate instrumented bundle is `/tmp/opforge-selfhost-bs20-instrumented`,
with its successful fresh protocol metadata, captured `native-stdout.txt` /
`native-stderr.txt`, raw 2,280-byte `memory.bin` and decoded `measurements.json`.
Its transfer dry run also passes. Select it explicitly in macOS Terminal:

```sh
python3 /Users/erik/Code/Retro/opForge/scripts/performance/run_a6000_selfhost.py --bundle /tmp/opforge-selfhost-bs20-instrumented
```

Neither new bundle has a physical A6000 result yet. Both target the same release
output; running the instrumented executable does not create an instrumented
self-build. Hardware duration and telemetry remain separate fresh measurements.

Full logs are `/tmp/opforge-selfhost-bs20-release.log` and
`/tmp/opforge-selfhost-bs20-instrumented.log`; the exported
`manifest.json`, `oracle.hunk`, executable, package and exact mapped input files
remain outside `target`. With the configured FS-UAE environment, reproduce the
release run using a fresh export directory:

```sh
OPFORGE_COMPACT_EXPORT_DIR=/tmp/opforge-selfhost-bs20-new \
OPFORGE_COMPACT_EXPORT_OUTPUT_EMBED=68020 \
OPFORGE_COMPACT_EXPORT_NATIVE=1 \
OPFORGE_FS_UAE_MEMORY_PROFILE=68020-74m \
OPFORGE_FS_UAE_TIMEOUT_MS=5430000 \
OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=5400000 \
cargo test -p asm --lib export_compact_self_host_bundle -- --ignored --nocapture --test-threads=1
```

For the instrumented counterpart, choose another new directory, add
`OPFORGE_COMPACT_EXPORT_INSTRUMENTED=1 OPFORGE_PHASE_ONLY=1`, and set the overall /
post-start bounds to 7,230,000 / 7,200,000 ms. These bounds allow 90 minutes after
START for release and 120 minutes for instrumentation to distinguish slow
completion from timeout; they are not measured durations or product targets.

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

#### R2 dependency-first preparation — focused proof and full-run recovery

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

The first complete embedded self-host attempt after R2 did **not succeed**. On 68020 / 10 MiB,
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
418,568-byte Rust release Hunk. The separate unchanged-workload pointer-fix
comparison is 10.915554500/11.187310083 seconds before and
10.895403833/11.119199625 after (medians 11.051432292 and 11.007301729).
The 0.044-second difference is below useful timing resolution; this is a
correctness repair with four added release bytes, not a measured speed gain.

The fresh phase-only diagnostic reaches the next unresolved proxy,
`SelectedOutputKind.l`, after 561.407299 seconds on the same source manifest.
This is a package-defined address member, incorrectly bound as a qualified
source name. The current repair carries contextual member forms from canonical
selector projections, normalizes complete instruction operands before binding,
and transports MemberShape and TargetMember fixup identities. Shared directives
and exact package registers retain their existing identities. The new binding
module consumes owned TKVM lexemes without changing unbound capture storage.
BS16 replaces BS15; regeneration is required and no legacy executor is retained.
Fresh native member comparisons now exit zero with exact live Rust Hunk equality,
including qualified bases, address addends, CODE/DATA/BSS relocation, macro bodies,
register-member precedence and numeric/quoted CPU directives. The final spelling
comparisons take 1.540888625 and 1.531500208 seconds and produce the complete
188-byte Hunk. Affected host checks pass: 273 assembler tests, 18 VM package
tests and 19 A6000 runner tests. The host inventory regenerates all 16 package
combinations without generation failures; six still have no compact instruction
candidates, so inventory generation is not CPU coverage proof.

The fresh BS16 embedded release self-host still exits 20 without output after
463.532497500 seconds, reporting physical origin 75, line 572. Its 86 inputs
contain 1,268,843 bytes, manifest `fnv1a64:5907162c81a85037`; the fresh Rust
release Hunk is 441,896 bytes (`fnv1a64:934bbeab1fab7076`). The current package
is 321,458 bytes (`fnv1a64:9361ef54cac5e022`), 22,152 more than BS15. This is
another unsuccessful full self-host run, not a speed comparison: both source and
package changed. The separate phase-only capture takes 559.383321500 seconds,
still exits 20 without output, and records one bounded allocation failure:
request 1,048,675 bytes, capacity 1,048,576, used 1,048,419. The packed-record
block reaches its 1 MiB owner limit while replaying the embedded package's
`.incbin`. This is not an unresolved-symbol failure. Peak tracked owned storage
is 19,553,992 bytes and all tracked allocations are freed. The next repair gives
only packed records a larger bounded allowance, preserving the symbol and other
buffer limits. The 74 MiB investigation profile does not qualify the 2 MiB target.

The separate member-only release comparison keeps the same 10,687-byte source,
2,434-byte exact Rust output and 68020/10 MiB settings. Final BS16 observations
are 11.054639833/11.305725042 seconds (median 11.180182438), compared with
10.895403833/11.119199625 (median 11.007301729) immediately before this repair.
The observed median difference is +0.173 seconds (+1.57%); with two observations
and overlapping ranges, it does not establish a performance regression or gain.
The external CLI grows by 1,176 bytes to 120,436; linked reservations grow by
1,160 bytes to 133,920. These numbers isolate the member repair, excluding the
subsequent diagnostic and record-budget changes.

The owner-budget repair keeps `memory.reserve` and `reserveExact` at their
existing 1 MiB limit. A separate `reserveBounded` entry accepts a caller-owned
unsigned limit, clamps non-power-of-two growth and preserves the old block on
rejection or allocation failure. Only Records and its dependency-ordering copy
receive a 2 MiB allowance; symbol, capture-region, origin and output limits are
unchanged. A fresh native allocator harness passes zero/unsigned caps, 300-byte
clamping, rejection without mutation, byte preservation across 1 MiB growth,
public register/stack/CCR preservation and release cleanup. Its host assembly
test and relevant formatting, instrumentation, ownership and proof guards pass.

The error-path repair uses bounded retained-origin paths. The former fallback
also had a confirmed local-name collision: case-insensitive lookup resolved
`SourcePath` to `reportFailure.sourcePath`, and the old Rust Hunk's LEA points to
that instruction itself. Extracting path resolution into its own scope removes
the collision. A fresh included-file rejection regression exits 20 and reports
`project/part.i`, proving the owned filename is retained through the error path.

A fresh complete embedded self-host now succeeds on 86 inputs, 1,270,932 bytes,
manifest `fnv1a64:44f083e11ac57356`, with a fresh 442,140-byte Rust release Hunk
(`fnv1a64:21b5603818ea2ca6`). Native exits zero and produces that entire Hunk
byte-for-byte, including the embedded package. Uninstrumented host-observed guest
START/DONE is 931.022937958 seconds (15 minutes 31 seconds); whole-test time is
969.78 seconds. Settings are 68020 / 74 MiB with unlimited emulator CPU speed.
This restores full current experimental CLI self-hosting after dependency-first
preparation; it does not establish full language/CPU/CLI parity or the 2 MiB
product target. The earlier failed runs cannot serve as comparative full-run
timings. The separate unchanged-workload comparison uses the same 10,687-byte
source, 2,434-byte exact output, BS16 package and 68020 / 10 MiB profile.
Observations are 11.179954000 and 11.128799833 seconds (median 11.154376917),
versus 11.180182438 immediately before the budget/diagnostic repair. The
0.026-second decrease is below useful timing resolution; no speed change is
established. The external CLI and its linked reservations each grow by 244 bytes,
to 120,680 and 134,164 bytes respectively.
An additional probe confirms that MOVE absolute-word `(8).w` remains an
unsupported packet form; it is outside this address-long binding repair.
The instrumented captures above remain failure-localization measurements,
separate from this completed release self-host timing.

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

BS22 uses a 192-byte header. The canonical target offset remains at 124, its
length at 128 and structural target flags at 130; the preparation-only file plan offset
and byte length are at 132 and 136. Built-in `.emit` identity is at 140, CPU
word bytes at 142, and the retained data-plan offset/length at 144/148. Fields
are big-endian and block-relative. The preparation-only metadata plan is at
152/156. Contextual member-binding offset/count are at 160/164; each eight-byte
row holds mnemonic ID, qualifier, operand index, field ID and a zero reserved
word. Rows derive from canonical package projections, including unsupported
candidate plans, and remain in the runtime prefix. Generic preparation does not
contain CPU suffix spellings. The retained shared instruction-head policy offset
and byte length are at 168/172, its PRVM version is at 176 and a zero reserved
word is at 178. The retained shared scalar declaration-plan offset/length are at
180/184, its PRVM version at 188 and a zero reserved word at 190. Regenerate
superseded packages; only BS22 is supported.
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


#### Explicit mapped logical-section Hunk layout

The compact native Hunk path now accepts ordinary concrete sections alongside
explicit import maps. Preparation emits numeric fragment/destination slots;
execution never looks up section strings. `binary_hunk_mapping.asm` owns map
validation, concrete-prefix measurement, fresh symbol/proof reset, stability
checks and final extent merging. `binary_sections.asm` keeps independent concrete
and logical cursors and routes their payloads; assembly only schedules the passes.

Mapped Hunk builds perform one disposable source-order layout traversal, freeze
concrete PC and initialized-byte prefixes, then run two fresh authoritative
source-order assembly passes. Only compile-time constants and incoming parameters
survive the reset. Labels, data `$` and aliases use the destination section identity
and prefix. Concrete reopenings still precede appended logical content; mutable
symbols and readonly snapshots retain statement order. Map-free builds keep two
passes. Flat mapped layouts retain their existing mutation rejection.

Nine fresh native positives match current Rust Hunk output exactly: mapped PC and
scalar blocks, imported aliases, reversed output selection, dependency-first source,
two destinations, reopened concrete prefixes, BSS and mutable snapshot order.
Three paired rejections require explicit nonzero completion and no Hunk artifact:
kind mismatch, logical/concrete name collision, and one logical name mapped to two
targets. The current Rust section table uses global names; the equal-name controls
are existing limitations, not newly supported module-qualified section identities.
The bounded native path retains two maps and eight total source slots, rejects
chained/many-to-one maps, and requires stable concrete prefixes. General layout
convergence, implicit mapping and broader Hunk parity are not established here.

The release image grows from 453,004 bytes at `a9e08952` to 453,912 (+908),
`fnv1a64:e7cd7408abff1373`. Section-control storage grows by 86 bytes (six state
bytes plus ten per slot); symbol slots and packages are unchanged. These are
static layout counts, not a measured peak or 2 MiB qualification. Mapped open
records retain one extra source-slot byte, plus a destination byte for logical
opens. No legacy executor or source-text assembly path was added.

On identical uninstrumented scalar-snapshot input, FS-UAE 68020/10 MiB timings
exclude emulator startup and measure START-to-DONE host wall time:

| Run | Baseline `a9e08952` | Mapped-layout repair |
|---|---:|---:|
| 1 | 13.427084 s | 13.362053 s |
| 2 | 13.693347 s | 13.609220 s |
| Median | 13.560215 s | 13.485636 s |

The median difference is -0.074579 seconds (-0.550%). Run-to-run spread exceeds
that difference, so no speedup is established. Both images match the same live
Rust output `fnv1a64:7eaac19333981023`, source `fnv1a64:a7120dfa242f6e68`
(15,748 bytes), packages and command. Reports are
`/tmp/opforge-hunk-map-measurement-baseline.json` and
`/tmp/opforge-hunk-map-measurement-current-final.json`.

A separate post-change mapped workload has a 128-longword concrete prefix and
128 mapped current-PC fields, plus an imported entry reference. Its two fresh
release runs take 4.116896 and 3.837860 seconds (median 3.977378). Both match live
Rust Hunk `fnv1a64:aa448e957e3fcca2`, source `fnv1a64:4482a308fb156dc2`
(3,416 bytes), on the final image and unchanged packages. Report:
`/tmp/opforge-hunk-map-mapped-measurement-final.json`. This is an absolute
post-change measurement; the old mapped Hunk path rejects, so no mapped speedup
comparison is possible.

Fresh final-image regression controls also pass for mixed snapshots/relocations/BSS,
nested/zero loops, flat logical reopening without an explicit map, and two mapped
6502 regions. The flat reader validates current metadata-bearing open records
while retaining the current unmapped reopening form; it is not a legacy executor.
Two flat mutation-rejection controls retain explicit nonzero/no-output behavior.
Focused Rust checks pass 30 Hunk tests, 23 reachable-block relayout tests and one
flat reopening test. Library Clippy, Rust/native formatting, CPU boundaries,
runtime ownership/no-growth, instrumentation safety, native test ownership,
fresh-proof, benchmark-selector and workflow-link checks pass. Independent review
resolved the current record-length edge. The broader inventory hash check fails
for five unchanged runtime modules identically at baseline; test-only Clippy
reports 22 existing findings in unchanged test sources. These are not broad
qualification claims.
This checkpoint does not claim a new full native self-host.

Batch cleanup reclaimed 3.1 GiB with `make clean`; `target` remains empty.
Required release deliverables and diagnostic summaries are preserved outside it.

#### Direct instruction current-address parity — qualified checkpoint

Direct `$` expressions now participate in instruction address proof and transport.
ExprVM evaluation and affine relocation identity are reused; native Hunk projection
binds opaque VM targets to canonical one-based section IDs, including current PC,
without fabricated symbols or CPU logic in shared execution. Optional branch scalar
identity now covers bounded affine addresses, while flat numeric behavior remains.
The Rust oracle was repaired first: direct `$` retains immediate Hunk relocation,
and unsupported positional address algebra rejects Hunk output.

Fresh native comparisons cover direct/addended current addresses, same-section
differences, mapped sections and invalid arithmetic. Rust placed instruction output
is covered separately. Native explicit Hunk `.place` stops during section preparation
before encoding (fresh exit 20, step 6); its separate known-gap reproducer remains,
with placed origins deferred to the next layout slice. Unchanged release workload
timing is compared against `d56ca7e5`; new workload timings are recorded separately.

Focused proof: seven supported-layout positives and nine invalid immediate
or branch cases pass fresh FS-UAE comparison on release image 454,092 bytes,
`fnv1a64:8bf2312a2ef1111a`. Invalid cases have explicit exit 20 and no Hunk output.
Flat PC/alias numeric arithmetic, including direct `#$` and `BRA.W $`, matches
Rust; existing `run+0` branch now also matches. Packages are unchanged.
Compared with `d56ca7e5`, image growth is 180 bytes with no added static storage.

The unchanged scalar-snapshot workload takes 13.587732 and 13.489737 seconds at
baseline, versus 13.514463 and 13.364657 after this change: median 13.538734 ->
13.439560, -0.099174 s (-0.733%). Spread exceeds the change; no speedup is
established. Input/output/package identities remain those recorded in the prior
mapped-layout checkpoint. Reports: `/tmp/opforge-instruction-pc-measurement-baseline.json` and `/tmp/opforge-instruction-pc-measurement-current.json`. The new
128-pair current-PC immediate/branch workload takes 6.386515 and 6.392916 seconds
(median 6.389716). This is post-change only: the old path rejects current-PC
instruction targets. Timings are fresh START-to-DONE host wall time under FS-UAE
68020/10 MiB with unlimited emulator CPU, excluding startup, not physical 68020
clock measurements.

The focused default Rust checks pass 34 Hunk tests, 14 value-provenance tests,
four branch tests and the flat-PC test; VM library 496 and registry library 23
tests pass. Library Clippy for asm/VM/registry, native formatting and affected
engineering guards pass. Broad asm library testing found 202 failures: 201 names
also fail on a fresh isolated `d56ca7e5` build. The one new failure was a source
shape assertion requiring the old Identifier-only target arm; it now also checks
Dollar and passes independently. The baseline itself has 210 failures, including
extra snapshot-path/environment failures. No broad-green qualification is claimed;
this existing test/reference debt needs its own maintained-contract review before
promotion. Raw logs and failure-name comparison are outside `target` in `/tmp`.

A fresh m68020-embedded release bundle contains 96 source files, 1,373,030 bytes,
source manifest `fnv1a64:6bfa19d06fe2dab0`. Host export and transport dry-run pass.
Direct `ash` cannot connect to the A6000 and computer use refuses Terminal, so
physical-device execution remains unavailable here. Full native FS-UAE self-host
qualification first stopped during source preparation after 179.031102 seconds
under 68020/10 MiB. A separate instrumented capture established allocation
failure, not instruction rejection: a 524,288-byte request while growing a
262,144-byte block (262,076 used) failed; tracked peak was 8,628,136 bytes and
all owned storage was freed at terminal cleanup. The failing source location
was an ordinary forward branch; a focused three-branch block regression passes
fresh native comparison. The release full self-host subsequently completed under
the separate 68020/74 MiB diagnostic profile. This does not qualify the 10 MiB
or 2 MiB memory target. The earlier full BS20 qualification also used 74 MiB, with a separate
instrumented peak of 22,380,568 bytes. Thus the 10 MiB failure does not establish
a new memory regression. The full-source memory target remains open; this slice
does not change its priority relative to language/layout parity. Telemetry: `/tmp/opforge-instruction-pc-memory-failure.json`.

Forced VM-only Rust checks also pass all four direct-PC Hunk tests and 14
value-provenance tests; they do not depend on Rust's specialized fallback.

The complete current native self-host now passes: all 96 source inputs,
1,373,030 bytes including the embedded package asset, fresh case-bound START/DONE,
explicit exit zero and exact complete 454,092-byte Rust Hunk. Bootstrap and output
both embed the unchanged m68020 package. Release START-to-DONE duration is
1,049.062893959 seconds (17m 29.06s), with telemetry disabled, under FS-UAE
68020/74 MiB and unlimited emulator CPU. This is full native self-host proof for
this exact source/package state, not physical hardware timing, full language
parity or proof of the product memory budget. The earlier 1,037.690233-second
full run used 94 inputs/1,360,588 bytes and a different image; that comparison
does not isolate this change's performance cost. Workspace compilation also
passes, including CLI, library, LSP and FFI.

The qualified A6000 bundle is
`/tmp/opforge-selfhost-instruction-pc-qualified-74m`; the local
`/tmp/opforge-a6000-current` link selects it for the maintained hardware runner.
Transport dry-run validates the bundle. Physical A6000 execution is still for
Erik to run; no current hardware timing is claimed. Explicit native Hunk placement
was the next layout-parity candidate, qualified in the checkpoint below. Broad
test/reference debt and the
full-source memory budget remain separately recorded limits.

Batch cleanup removed 7.2 GiB of Cargo-generated files with `make clean` and
retained the empty `target` directory. Deliverables, the exact-output bundle and
diagnostic summaries remain outside it. No remote push is part of this checkpoint.


#### Placed Hunk layout parity — qualified checkpoint

Implement shared `.region` / `.place` layout through packed numeric records.
Baseline is `06dab3d0`: placed Hunk input rejects before execution; unplaced and
mapped current-address proof plus full current self-host are qualified. Labels,
statement `$` and branch evaluation must use absolute placed addresses; Hunk
longword fields and relocation locations remain relative to canonical sections.
Region placement order and Hunk output order are independent. Keep preparation,
layout measurement, numeric encoding and serialization in their existing owners;
place geometry receives its own bounded runtime responsibility.

Qualify direct addresses/addends, branches, two sections sharing a region with
alignment and reverse placement, BSS, mapped blocks, and overlap/overflow/duplicate
failures against live Rust. Do not merely remove the rejection or discard placed
origins. Keep native's existing bounded section capacity explicit and reject
unsupported/nonconvergent layouts. No source strings or pointers enter packed
layout records. Use unchanged scalar-snapshot release timing before/after this
change, plus a separately identified placed workload. Require fresh full current
native self-host on the previously qualified 68020/74 MiB profile and export the
complete embedded-m68020 bundle. A6000 execution remains separate hardware proof.
Discuss any required VM/package semantic redesign; keep this focused on shared
layout parity and preserve the working reference until exact comparison passes.

Implementation now lowers region bounds, alignments and placement identities to
packed numeric records. Geometry has a dedicated runtime owner; canonical origins
feed label/PC evaluation, while package-proven instruction fields and shared DATA
fields are normalized before Hunk serialization. Layout retains maximum extents
for reservations, following placements and mapped prefixes, with zero-filled
initialized gaps. Pass-one replay is bounded. There are eight source section
slots, eight regions, eight placements and two explicit maps. Literal geometry
with power-of-two alignment is supported; `.pack`, expression-valued geometry,
general convergence and nondefault flat placement alignment remain outside this
checkpoint. Unsupported flat alignment rejects explicitly.

Fresh release native comparison covers 16 placement cases: exact live Rust Hunk
bytes for addresses/addends, branches, alignment/order, BSS, mapped blocks and
high-water gaps; overlap, overflow and duplicate placement reject with exit 20.
Host oracles also assert placed label values. Native label-file export remains
unqualified. Existing 6502 flat two-section and mapped-region release regressions
pass, as do unplaced/mapped instruction-PC probes. An earlier flat release timeout
did not recur in instrumented or fresh release retries; it is not a demonstrated
code regression.

Identical 15,748-byte scalar-snapshot input, identical package and exact 1,588-byte
Hunk: two-run mean START-to-DONE is 13.595977792 seconds at `06dab3d0`, versus
13.591572021 seconds now (-0.0324%, effectively unchanged). The separate placed
15,810-byte version averages 13.819500521 seconds (+1.68% against current
unplaced input); this is a capability comparison, not an isolated speedup. Both
use release builds without telemetry, FS-UAE 68020/10 MiB and unlimited emulator
CPU. The embedded-m68020 release image grows from 454,092 to 456,048 bytes
(+1,956 bytes, +0.43%); package bytes are unchanged. Raw comparative records are
retained in `/tmp/opforge-placement-before.json`, `-after.json` and
`-placed-times.json`; the 16 native case records are consolidated in
`/tmp/opforge-placement-validation.json`.

Focused Hunk host checks pass (29 passed, 24 ignored), reachable-block layout
checks pass (23), and format, affected-library Clippy, workflow and deterministic
native engineering guards pass. A broad Hunk run is not green: 13 unrelated
failures recur in the earlier recorded baseline; an intermediate new high-water
failure was repaired and the final host/native probes pass. No whole-suite
qualification claim is made.

Fresh full current native self-host completes with guest exit zero and exact
complete 456,048-byte Rust Hunk (`fnv1a64:a1e43b34ac8e71fb`), including unchanged
embedded m68020 package bytes. All 98 inputs (97 text/generated inputs plus one
321,532-byte package asset) total 1,388,392 bytes; source manifest is
`fnv1a64:bc4281fb706d43cf`. START-to-DONE is 1,066.644855709 seconds
(17m 46.64s), release telemetry disabled, FS-UAE 68020/74 MiB with unlimited
emulator CPU. Hunk linked reservation totals 474,144 bytes. This qualifies the
complete assembly of this current source state, not full language parity,
physical hardware timing or the 2 MiB product memory budget. Compared with the
preceding 1,049.062893959-second self-host, both source and image changed; only
the scalar-snapshot comparison above isolates this change's runtime impact.

The refreshed bundle is `/tmp/opforge-selfhost-placement-qualified-74m` and
`/tmp/opforge-a6000-current` now selects it. The maintained hardware runner's
`--dry-run` validates and stages it; no physical A6000 execution is claimed.
Run `python3 scripts/performance/run_a6000_selfhost.py` from macOS Terminal for
fresh transfer, overall hardware timing and exact-output comparison. Both
bootstrap and assembled output embed m68020. The comparative records and full
manifest summary are retained in `/tmp/opforge-placement-qualification-summary.json`;
the full native log is `/tmp/opforge-placement-full-selfhost.log`.

Batch cleanup completed after all native runs and builds: `make clean` reported
4.6 GiB of generated outputs removed and retained the empty `target` directory.
The qualified bundle and reports remain outside it. No remote push is authorized
for this checkpoint.

#### Hunk qualification recovery — host checks qualified; listing gap exposed

Baseline `85f4e81f`: broad host Hunk checks reproduce 212 passes, 13 failures and
54 ignored tests. Eleven failures occur before guest execution because the legacy
CLI's checked-in canonical package differs from the current registry; the other
two concern a missing telemetry include root and structural checks still pointing
at an extracted fixup owner. Refresh only the identified package with the canonical
generator, restore the intended test setup and ownership checks, and rerun the
complete host Hunk selection. Preserve exact package equality, relocation checks
and fresh-run native proof requirements. Do not interpret skipped emulator cases
as native qualification. The compact runtime/package and its qualified A6000
bundle should remain unchanged; if production behavior differs, investigate that
separately and measure/qualify the affected implementation.

The canonical generator refreshed only
`native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm`, from 370,900
to 372,302 bytes (+1,402). Only CSEM and CMSE differ; the other 22 chunks are
byte-identical, and a second independent generation produces identical bytes.
Regenerate this asset explicitly with:

```sh
cargo run -p cli --bin build_vm_package -- native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm
```

The graph test now receives its configured include roots and graph source-resource
context, as production assembly does for active `.incbin`. Structural checks
follow fixup execution into its shared owner and retain relocation, width, opaque
target and capacity checks. All 81 duplicate freshness assertions now use one
exact-byte comparison helper; mismatch diagnostics show sizes, the first differing
offset and the regeneration command instead of dumping the package. Expected
artifacts and fresh-run safeguards are unchanged. Independent review confirms
the mechanical substitutions and restored ownership/setup.

`cargo test -p asm --lib hunk -- --test-threads=1` now passes all 225 enabled host
tests; 54 optional tests are ignored. This is broader host qualification, not
proof that skipped native cases ran. Formatting, affected-library Clippy and
deterministic native guards pass. Test-target Clippy reports 22 warnings in
untouched test code; no whole-suite or all-target Clippy qualification is claimed.

Fresh FS-UAE legacy CLI checks with the current canonical package, on 68020/74 MiB,
exit zero and match complete live Rust Hunk artifacts for cross-section absolute
relocation and shared `.long`. A simultaneous Hunk/S-record/listing case completes
with guest exit zero and exact Hunk/S-record bytes, but its listing mismatches
(412 native bytes versus 515 Rust bytes). The comparison remains failing. Rust's
source graph inserts implicit `.module input` / `.endmodule` lines and lists them;
legacy native lists the original physical lines. Listing policy needs discussion
before changing either output contract; no filtered oracle or relaxed comparison
was introduced. That gap is distinct from the repaired host setup failures.

No compact native source or prepared package changed. All 95 native inputs in
the `85f4e81f` qualified compact self-host bundle still match the workspace, and
the refreshed legacy asset is outside that source closure. Its full self-host
proof and A6000 bundle remain current; no repeated compact timing or fresh
hardware run is claimed. Package-refresh details and logs are retained outside
the build cache under `/tmp/opforge-hunk-package-refresh.json` and
`/tmp/opforge-hunk-qualification-*`.

Workflow checks pass with the existing advisory architecture findings. After all
builds and native runs finished, `make clean` removed 3.4 GiB of generated cache;
the empty `target` directory remains. Reports and qualified bundles remain outside
that cache.

#### Current MOS-family audit — baseline `77210ddb`

Refresh all 39 roots in `examples/mos6502` against the current compact native CLI
and live Rust CLI before choosing more parity work. The selection includes every
MOS example, but excludes the separate 85-root opcore corpus. Build one fresh
release native executable and the current source-independent packages, then run
each case serially on FS-UAE 68020/10 MiB without telemetry. Preserve the sources,
fresh completion/exit/artifact checks and per-case timing; do not repair production
semantics, refresh stored references or relax comparisons during this inventory.
Completion means every selected root has a recorded result, including failures;
it does not mean family parity or full self-host qualification.

The corpus contains five base-6502 cases, two 65C02 cases, nine 65816 cases,
22 45GS02/MEGA65 cases and one 6502-to-65C02 switching case. Separate instruction
encoding gaps from shared-language/layout/output gaps. Six 65816 wide/layout
examples primarily exercise shared functionality, not the 65816 instruction set.
An independent Luna source inventory confirms these scope limits.

Reproduce with the maintained FS-UAE environment and:

```sh
OPFORGE_MOS_CORPUS_CASES=examples/mos6502/ \
  OPFORGE_MOS_CORPUS_REPORT=/tmp/opforge-mos-current-77210ddb.json \
  OPFORGE_FS_UAE_MEMORY_PROFILE=68020-10m \
  OPFORGE_FS_UAE_TIMEOUT_MS=180000 \
  OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=120000 \
  cargo test -p asm --lib compact_mos_corpus_fs_uae -- --ignored --nocapture --test-threads=1
```

The report's total inventory remains 124 MOS/opcore roots; its selection and
attempted count identify the 39-root scope. Numeric-leading source filenames
remain staged byte-for-byte as `input.asm` for both engines; this qualifies source
semantics under that name, not original filename handling. CLI listing and mixed
output kinds remain outside this invocation. Source-requested additional outputs
are compared and can fail independently of an exact Hex match. Stored-reference
checks are a separate result, and diagnostic text parity is not established by
nonzero rejection. Timings are absolute current release observations; this audit
does not provide an isolated before/after speed claim.

The full 39-case run completed in 1,220.75 host seconds, including package/image
preparation, emulator startup and one timeout. Every live Rust oracle succeeded.
Twenty native cases match every required artifact; 17 complete with exit 20,
one completes with exit zero but different Hex bytes, and one exceeds the
120-second post-START bound. The audit test deliberately fails after recording
all cases. Only seven stored-reference checks pass: 30 report source errors with
the original filenames and two report listing differences. No references changed.

| Corpus group | Roots | Exact live Rust artifact matches | Remaining cases |
| --- | ---: | ---: | ---: |
| Base 6502 | 5 | 4 | 1 |
| 65C02 | 2 | 1 | 1 |
| 65816 | 9 | 1 | 8 |
| 45GS02 / MEGA65 | 22 | 14 | 8 |
| 6502-to-65C02 switching | 1 | 0 | 1 |
| Total | 39 | 20 | 19 |

Compared with the earlier audit's 16 qualified MOS matches, the four additional
matches are `6502_simple`, `6502_native_cli_smoke`, `65c02_simple` and
`65816_wide_const_var`. The last is a scalar-declaration example with no emitted
instructions; it is not 65816 instruction qualification. The complete base-6502
151-instruction matrix still matches (321 emitted bytes). Matches take
0.7546–2.7849 START/DONE host seconds; the matrix takes 2.7849 seconds. These
single observations are not a comparative performance claim. The release image
is 456,048 bytes, `fnv1a64:a1e43b34ac8e71fb`, identical to the qualified
`85f4e81f` compact self-host image. Current m6502 is 13,714 bytes,
`fnv1a64:b4cff8b25e13282a`.

The only timeout, `45gs02_simple`, was retried independently with identical
source/input/image/package digests and command. It completed in 0.761342750
START/DONE host seconds with exit 20 at line 11, `lda [$21],z`. The timeout did
not recur; the original failure remains in its full-run report. This retry is
localization of the unsupported form, not successful assembly. The reports are
`/tmp/opforge-mos-current-77210ddb.json`,
`/tmp/opforge-mos-current-77210ddb-timeout-retry.json` and the compact derived
summary `/tmp/opforge-mos-current-77210ddb-summary.json`.

Remaining first stops, with preparation failures kept distinct from instruction
diagnostics:

- **Package operands:** 65C02 `bbr0 $20,bbr_target`; 45GS02
  `jsr ($2002,x)`, `phw #$1234`, `ldq [$23],z`, `lda [$21],z` and
  `bsr far_bsr`; 65816 `jmp [$3456]` and `jml [$2345]`.
- **Package state:** 65816 `.assume` rejects before later width/bank/direct-page
  forms execute. Those later forms are not qualified by this run.
- **Shared language/output:** the base-6502 artifact example now passes region
  placement and stops at `.text "OK"`; other stops are the second `.org` in
  `45gs02_relfar`, `.res long,20000`, wide `.output` metadata and `.mapfile`.
- **Unlocalized preparation:** stack-relative indirect Y and the CPU-switching
  example reject at preparation step 2; wide alignment and listing-aux examples
  reject at step 6. A first stop does not prove every later feature missing.
- **Completed mismatch:** `45gs02_rel_branch_overrides` emits zero displacements
  while Rust emits 1 through 9 for branches to the following instruction. Its
  native exit is zero, but exact comparison fails. Resolve the Rust/package
  sizing discrepancy before treating either output as corrected.

The proposed next slice is the **65C02 addressing matrix**, starting with
family-owned binary projections for BBR/BBS and word-sized indexed-indirect
operands. Source review identifies legacy `pair_u8_rel8` descriptors and an
omitted `AbsoluteIndexedIndirect` structural projection. BBR/BBS need a three-byte
instruction-relative fixup, not the existing two-byte branch adjustment. Reuse
neutral execution mechanisms; retain CPU policy in package producers. Qualify
the complete 65C02 matrix, branch limits/forward labels and base-6502 regressions;
also check the corresponding 45GS02 indexed-indirect JSR form. Immediate PHW's
word-width policy and 65816 state/bracketed projections remain following slices
unless the matrix exposes a necessary shared prerequisite. This is a proposed
implementation scope, not completed work.

No production code, packages, golden references or qualified bundle changed in
this audit. No new full self-host or A6000 run is claimed. The current experimental
CLI still lacks command-line listing and mixed output-kind support; the legacy
CLI's implicit-wrapper listing mismatch is a separate unresolved issue.

Documentation links and whitespace checks pass. Once both native batches ended,
`make clean` removed 1.5 GiB of generated cache and retained an empty `target`.
Reports and logs remain outside the cache. Unrelated workflow-notebook edits
remain untouched.

#### Cross-family parity comparison — baseline `b840b27f`

Audit the 43 top-level `examples/motorola68000` instruction fixtures, including
seven expected-error cases, against the current compact native CLI and live Rust.
Nested AmigaOS executable demos and support modules remain outside this Hex
instruction audit; they require separate Hunk/implicit-CPU setup. Reuse the MOS
audit's fresh executable/package preparation, byte-preserving neutral filenames,
per-case completion/exit/artifact checks and independent stored-reference checks.
Run serially on FS-UAE 68020/10 MiB without telemetry. Production code, packages,
sources and goldens remain unchanged. Record all selected results before comparing
them with the 39-case MOS audit to select the next implementation slice.

The test harness now shares its corpus runner rather than duplicating it.
Existing MOS/opcore commands and selection remain intact. Family reference paths
retain relative subdirectories; a host inventory/staging check verifies complete
top-level selection and byte-identical staging. Expected-error cases establish
fresh nonzero rejection with a diagnostic, not diagnostic text/identity parity
or successful assembly. Check whether native rejects before the intended operation.

```sh
OPFORGE_M68K_CORPUS_REPORT=/tmp/opforge-m68k-current-b840b27f.json \
  OPFORGE_FS_UAE_MEMORY_PROFILE=68020-10m \
  OPFORGE_FS_UAE_TIMEOUT_MS=180000 \
  OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=120000 \
  cargo test -p asm --lib compact_motorola68000_corpus_fs_uae -- --ignored --nocapture --test-threads=1
```

Optional `OPFORGE_M68K_CORPUS_CASES` selects path substrings for later focused
retries; unset it for the complete 43-root audit. Success for this inventory is
recording every case honestly, not forcing the deliberately gap-reporting test
green. Neither this run nor the MOS audit establishes whole-language parity,
diagnostic parity, original numeric-filename behavior, new full self-host proof
or physical A6000 timing.

The full run records all 43 cases in 1,380.04 host seconds, including preparation,
emulator startup and one startup timeout. All 36 positive live Rust oracles
succeed. Fifteen match complete native artifacts; 20 complete with exit 20 and
one stalls before guest START. All seven expected-error cases complete with
nonzero exit and a diagnostic. The audit test remains deliberately failing;
negative rejection is not positive assembly or diagnostic identity qualification.

| Target | Positive roots | Exact artifact matches | Expected-error roots / nonzero rejections observed |
| --- | ---: | ---: | ---: |
| 68000 | 20 | 14 | 1 / 1 |
| 68010 | 1 | 0 | 0 / 0 |
| 68020 | 5 | 0 | 0 / 0 |
| 68030 | 2 | 0 | 0 / 0 |
| 68040 | 3 | 1 | 4 / 4 |
| 68080 | 5 | 0 | 2 / 2 |
| Total | 36 | 15 | 7 / 7 |

This is example coverage, not a percentage of instruction-set support. A first
stop prevents qualification of later forms, and the existing current full
m68020 self-host proves its actual source workload rather than every 68020
instruction. The fresh release image remains 456,048 bytes,
`fnv1a64:a1e43b34ac8e71fb`, byte-identical to the MOS audit and qualified compact
self-host image. Positive match START/DONE observations range from 1.2618 to
1.5249 seconds. Inputs differ from MOS, so these times do not establish a
cross-family speed ratio or the impact of a code change.

The stalled `68020_fpu_registers` case was retried with identical source/input,
image, package digests and command. Fresh execution completes with exit 20 at
line 3, `.fpu 68881`, in 1.016601417 START/DONE host seconds. Startup timeout does
not recur; preserve both outcomes. The complete and retry reports are
`/tmp/opforge-m68k-current-b840b27f.json` and
`/tmp/opforge-m68k-current-b840b27f-startup-retry.json`; the derived summary is
`/tmp/opforge-cross-family-parity-summary.json`.

Four negative cases reject earlier than Rust's intended failing operation:
FSIN stops at `.fpu`; AMMX-shape and Apollo-gating cases stop at `.apollo`; MOVEC
CAAR fails during preparation. Three negatives reach the same reported source
operation as Rust, but their generic diagnostics still do not establish reason
or text parity. Do not present seven observed rejections as seven CPU-policy
contracts qualified.

#### Shared priorities identified by both family audits

1. **Implicit module naming:** only one of 43 M68K stored-reference checks passes;
   35 positive originals report source errors and all seven error references
   report different diagnostics. MOS has 30 original-filename source failures
   and two listing differences. A fresh Rust CLI discriminator confirms that
   original `6502_simple.asm` and `68000_basic_moves.asm` fail at the generated
   `.module` line, while byte-identical `input.asm` copies succeed. The engine
   derives implicit IDs directly from filename stems, which may begin with a
   digit although module identifiers cannot. This shared bug is independent of
   the valid neutral-filename native comparisons. Evidence:
   `/tmp/opforge-cross-family-implicit-name-report.json`.
2. **Packed package recipe coverage:** M68K first stops include wrapped
   absolute-long MOVE, zero-displacement/scaled indexed aliases, MOVEP, default
   LINK, BKPT, full-extension addressing and register-pair DIVS/CAS2. Existing
   CPU-owned indexed selectors use tuple register item 0, qualified item 1 and
   identity-scale projections that the packed exporter does not retain. BKPT's
   `semv.scalar.v1` recipe is unsupported, and MOVEP's match-after-encode ordering
   cannot lower. Wrapped absolute-W already passes; absolute-L and LINK have
   existing transport and need localization. MOS similarly lacks structural
   projections for some indirect/tuple forms, but BBR's three-byte relative
   fixup and PHW's word width also require their own family-owned producer work.
   Extend neutral transport/execution where needed; CPU policy remains in
   package definitions. Do not add CPU parsers to native preparation.
3. **Package configuration:** ten positive M68K examples first stop at `.fpu`
   or `.apollo`, after the retry, while 65816 `.assume` is a MOS first stop.
   These require package-owned state transitions through generic execution.
   FPU/AMMX instruction bodies remain unqualified; fixing configuration alone
   may reveal later recipe gaps.
4. **Shared layout/language/output:** second `.org` statements block the M68K
   qualified-module call and a MOS far-branch example. MOS also stops at `.text`,
   typed `.res`, wide output metadata and `.mapfile`; collection values and
   unlocalized preparation failures remain separate known gaps. Keep the
   completed MOS branch-displacement mismatch open; no matching M68K artifact
   discrepancy was observed among completed positive cases.

The recommended **first implementation slice is legal implicit module IDs**.
Derive stable valid IDs for implicit files, preserve valid and explicit names,
check module discovery/import consistency and avoid silently merging collisions.
Verify original numeric-leading filenames on both Rust and native, compare
emitted bytes with the neutral-name controls, and rerun relevant original-path
reference checks without refreshing goldens. This is a shared correctness repair
with a small coherent scope; it does not resolve native operand/configuration
gaps. Then take the packed tuple/index recipe-export slice, followed by package
configuration. This revises the MOS-only proposed 65C02-first ordering using the
cross-family evidence; these are recommendations, not implemented repairs.

The inventory/staging host test, Rust formatting, native fresh-proof guard and
native test-ownership guard pass. No production, package or golden change was
made, and the qualified A6000 bundle remains unchanged.

A Luna result review confirms the reported scope, counts and shared naming
diagnosis. After both native runs and host checks ended, `make clean` removed
2.9 GiB of generated cache; the final staging-test batch then passed and its
cleanup removed another 1.5 GiB, retaining an empty `target`. Reports and CLI
discriminator artifacts remain outside the cache. Unrelated notebook edits are
preserved.

#### Numeric-leading implicit module repair — baseline `262ed3bf`

The shared Rust filename helper now prefixes ASCII digit-leading stems with `_`:
`6502_driver.asm` defines `_6502_driver`, usable through `.use _6502_driver`.
Valid existing stems and explicit module declarations are preserved. Rust root
metadata, dependency indexing and generated module boundaries already share this
helper. Compact native `pathStem` applies the same rule in discovery, captured
configuration and dependency-ordered preparation. Its bounded temporary buffer
leaves the physical source path untouched. Distinct dependency files such as
`6502_dep.asm` and `_6502_DEP.asm` remain ambiguous through the existing folded
declaration lookup; no file silently wins.

The corpus harness now retains original family filenames and source bytes,
removing the neutral-name workaround. Neutral copies remain explicit controls
in focused regression tests. The shared Rust reference audit also has a separate
Motorola entry point, so original-path checks need no emulator rerun.

Validation:

- All 72 engine unit tests pass, including root metadata and explicit identity.
- Focused discovery/include host checks: 16 pass, 11 native checks ignored.
  Corpus inventory/staging passes with original names.
- Fresh FS-UAE 68020/10 MiB, release/no telemetry: six runs complete. The original
  `6502_simple.asm` (49 bytes) and `68000_basic_moves.asm` (16 bytes), and each
  byte-identical `input.asm` control, match live Rust exactly with exit 0. A
  numeric implicit root importing a numeric dependency matches `[7, 7]`; the
  normalized-name collision completes with exit 20.
- Observed START/DONE seconds: MOS original 0.763332459, control 0.753267958;
  M68K original 1.010124834, control 1.019982375; numeric import 0.763951667;
  collision 0.508423208. These compare filenames under the same new build,
  not before/after code performance. The focused executable is 134,564 bytes,
  with 152,916 linked reserved bytes; it uses the focused harness's package
  configuration, not the full audit/self-host image configuration.
- Original-path reference audits record all 124 MOS/opcore roots and 43 M68K
  roots. All 39 MOS payloads and all 36 positive M68K payloads match their stored
  references; all seven M68K negative diagnostics match. The audits still fail
  on 32 MOS and 35 M68K listings (7/39 and 8/43 complete reference matches).
  The source errors caused by numeric names are gone. A representative CLI
  listing exposes the generated `.module` row and shifted line numbers, a
  previously known listing-provenance issue. No golden is refreshed or listing
  comparison weakened; remaining row/symbol differences require their own slice.
- Rust formatting and engine-library Clippy with warnings denied pass. The
  native formatter checks 80 files through `binary_app.asm`: no changes/warnings.
  Workflow, CPU-boundary, fresh-proof and native test-ownership guards pass.
  The experimental-directory redundant-test scan still reports five autofixable
  findings in untouched encoding/imports/sections/templates code; none are in
  this repair. This is not a clean whole-native redundant-test qualification.

Logs/reports are outside `target`, under `/tmp/opforge-implicit-*`, including
`native-test.log`, `host-test.log`, `engine-full.log`, `mos-rust-references.json`
and `m68k-rust-references.json`. The previously qualified A6000 bundle is retained
at its earlier baseline; no new full self-host or hardware run is claimed here.
The next proposed slice remains packed tuple/index operand-recipe coverage,
with the listing-provenance repair tracked separately. Build-cache cleanup is
complete: `make clean` removed 3.1 GiB and retained the empty `target` directory.
Unrelated workflow-notebook edits are preserved.

#### BS21 bounded tuple projections — baseline `a30de087`

The canonical projection selects an actual tuple item, not a fixed native field.
Packed export now retains item indices 0–2 for register, scalar and qualified
register projections. Descriptor bytes 10/11 carry arity/item. Explicit arity
2/3 remains exact; absent arity uses 0 for bounded actual arity 2/3; conflicting
arity predicates reject. Native `binary_tuples` validates complete bounds and
exposes opaque leaves; package classes/qualifiers and the expression VM retain
semantic ownership. Existing projection telemetry boundaries remain in place;
no per-helper instrumentation or release-time telemetry was added.

Native preparation preserves `(a3,d4)` as a two-item tuple instead of inventing
`0(a3,d4)`. Package literals supply implicit displacement when required. The
package-owned `move.b/w/l (a3,d4),d5` recipes now execute and match live Rust's
12 bytes, also equal to explicit `0(a3,d4.w)` forms. A complete tuple can disprove
a bare register/named-range rejection; unknown unsupported candidates still fail
closed. Necessary scalar-first class facts require canonical scalar item-0
evidence, so an index register is not mislabeled as the base.

The producer, native consumer, inventory and hardware runner now use BS21.
BS20 is superseded, with no compatibility executor. Old packages and self-host
bundles require regeneration; no BS21 full self-host or A6000 qualification was
performed in this slice. Host export produced 16 packages without generation
errors; six pipelines still have no compact instruction candidates. Generation
is not target parity proof. Assets/report are outside `target` at
`/tmp/opforge-bs21-tuple-inventory`.

Focused qualification:

- Packed-source host subsystem: 320 tests pass; native-only tests remain ignored
  in this host batch. VM package projections: 19 tests pass. Invalid indices,
  exact versus absent arities, conflicting bounds and unsupported match facts
  have regression coverage. The two initially exposed host match-fact failures
  were repaired and the subsystem rerun passed. The final arity-coverage guard
  then passes all 11 wire tests; all 16 regenerated package files are byte-identical
  to those used for native qualification, so the measurements remain applicable.
- Fresh FS-UAE 68020/10 MiB release runs: indexed register pairs, displacement/
  qualifier forms, MOVEA, signed sequences, the repeated workload and 6502 indexed
  wrappers match live Rust bytes with explicit exit 0. Invalid base/arity/range
  cases complete with exit 20. The nine-test batch performs ten fresh runs.
- A separate fresh PC-relative LEA fixup regression passes with all ten bytes
  equal to Rust, exit 0 and a 1.013161-second START/DONE observation.
- Rust formatting, VM/assembler library Clippy with warnings denied, native
  formatting (81 files), CPU-boundary, fresh-proof, test-ownership and workflow
  link checks pass. Hardware-runner unit tests: 24 pass. Independent Sol review
  found no substantive correctness/architecture issue. The changed-file CCR
  scan still finds one pre-existing autofixable test in encoding (also present
  at `a30de087`) plus report-only call-return checks; this is not a clean whole-
  native redundant-test qualification.

Per-change comparison on identical source, FS-UAE 68020/10 MiB, telemetry off:

| Property | Baseline `a30de087` | BS21 tuple change |
| --- | ---: | ---: |
| Source / output bytes | 7,489 / 1,632 | 7,489 / 1,632 |
| Fresh START/DONE seconds | 11.814089333 | 12.006181667 |
| Focused executable bytes | 134,564 | 134,372 |
| Linked reserved bytes | 152,916 | 152,716 |
| m68020 package bytes | 321,532 | 342,736 |

Observed time increases 1.6% in this single comparison; no statistical speedup
or regression conclusion is claimed. The earlier intermediate run was
12.044083750 seconds and is not the final result. Package growth is 21,204 bytes
(6.6%) because additional canonical recipes now retain executable projections.
This focused executable uses external package staging, so its image size does
not include that package growth. Logs are retained as `/tmp/opforge-tuple-*.log`.

Remaining: identity-scale projections such as `(a0,d1.w*1)` stay explicit
unsupported forms. Do not flatten multiplication into a register or add CPU
logic to native. Their canonical expression/scale projection transport is a
logical follow-up; listing provenance, BBR/BBS fixup support and other family
inventory gaps remain separate. Full self-host and hardware qualification must
use freshly generated BS21 inputs before updating those status claims.

After the build/native batch ended, `make clean` reported 4.5 GiB removed and
retained an empty `target` directory. Deliverable packages and diagnostic logs
remain outside it. Unrelated workflow-notebook edits are preserved.

#### BS22 identity-product projections — baseline `f23ebf3a`

This slice transports canonical identity-scale tuple projections without flattening
multiplication, recreating source strings or selecting target scales in native.
The shared product owner retains one top-level numeric multiplication node with
bounded name/compiled-scalar children. Tuple bounds expose it as an opaque leaf;
package projections select qualifiers/classes and the expected identity. Scalar
factor evaluation uses shared ExprVM, retaining all 64 bits and unresolved state.
The encoder only adapts package wire fields to these shared primitives.
This does not add a PRVM/EXVM expression-parser program or close the existing
native expression-parser ownership gap.

BS22 migrates producer, native package readers, inventory and hardware runner
as one latest-only contract. Kind 23 carries the package's identity predicate;
register kinds 11/13 carry expected identity in Literal's high word and the
qualifier ID in the low word. Descriptor arity/item bounds remain unchanged.
The product is `$82,u8 payload bytes,u8 operator,u8 left bytes,left,right`, with
no pointers or nested product nodes. Mixed outer infixes and chained outer
products reject rather than being silently reassociated.

Two candidate-selection distinctions were necessary:

- Downgraded unsupported rows must retain canonical *match* arity. RequiredForms
  nibbles 10/11 mean exact tuple arity 2/3. Complete mismatching bounds or a
  proven complete non-tuple root can skip the row; unknown structure, conflicting
  predicates and later encode/fixup facts supply no new proof. This lets a
  two-item identity alias pass an earlier three-item PC-relative barrier without
  weakening that barrier.
- Unresolved factors differ from evaluation errors. Rust's pass-one unresolved
  scalar placeholder is a successful zero; the right-first identity predicate
  therefore cannot fall back to a left one. Native retains that strict barrier.
  Treating an unknown qualified name as an error had incorrectly accepted
  `1*d1.w`; the rejection regression covers the distinction.

The live Rust controls accept MOVE byte/word/long aliases with word/long data
indices, address-register indices, PC bases, displacement, earlier scalar
constants and computed identity `(1+0)`. Ordinary explicit-displacement
spellings compare byte-for-byte where Rust supports them. Rust's ordinary
address-register-index MOVE spelling has no portable recipe, so that alias is
checked against a known four-byte opcode fixture instead. Reversed qualified
products, bad qualifiers, nonidentity factors, a 64-bit factor whose low word is
one, and chained products remain rejected. These are bounded coverage claims,
not full CPU/family or expression parity.

The old member-export test accidentally identified an alias with the same
selection key as its canonical member recipe. It now preserves stable duplicate
order and checks the actual executable member/value-program descriptors. The
old native known-gap assertion also became obsolete: its complete 171-byte
source now produces all 30 live Rust bytes in a fresh packed-harness run.
That control covers mnemonic labels and the parenthesized member-value input;
it does not establish general member-value parity.

Focused qualification:

- Final packed-source host subsystem: 327 pass, 463 native-only/opt-in tests
  ignored. VM package projections: 20 pass. Wire transport: 15 pass, including
  exact/unknown/conflicting arity, item bounds, unsupported sources and later-stage
  nonproof controls. The two initially stale tuple-root assertions now check
  the stronger exact-two-item proof; the complete final subsystem rerun passes.
- FS-UAE 68020/10 MiB release indexed batch before the final non-tuple safeguard:
  11 tests, 17 fresh guest runs.
  Supported cases match live Rust bytes with explicit exit zero; invalid
  base/arity/range and all six identity rejection cases complete with exit 20.
  The seven identity aliases produce the exact 28 Rust bytes in 1.267023 seconds.
  The shared 6502 wrapper control also matches all six Rust bytes.
- Final review restored the existing complete non-tuple proof when an exact
  arity row cannot find a tuple. Fresh controls after that refinement match all
  30 member-fixture Rust bytes in the packed harness (1.007752 seconds), and all
  200 Hunk bytes in the full compact CLI absolute-state matrix (1.774168 seconds).
  The latter includes module/use, BSS and scalar/qualified absolute operands.
  The final identity rerun again matches the seven aliases' 28 live Rust bytes
  (1.278361 seconds), and all six rejection controls again complete with exit 20.
  Final PC-relative LEA dispatch also matches its ten Rust bytes (1.025831 seconds).
  Together with the final workload and breadth controls, the reviewed code has
  six passing native tests with eleven fresh guest runs. The earlier indexed
  batch supplies additional unchanged tuple/class/range coverage.
- VM/assembler library Clippy with warnings denied, Rust formatting, experimental
  native formatting (86 files), CPU-boundary, fresh-proof, ownership and workflow
  link guards pass. Hardware-runner unit tests: 24 pass, including rejection of
  the immediately superseded BS21 contract. Independent Sol reviews found no
  remaining product ABI/framing or projection-contract defect. The changed-file
  CCR scan still finds one pre-existing autofixable encoding test plus report-only
  checks; no whole-native clean CCR qualification is claimed.
- Host inventory regenerates all 16 packages without errors; six pipelines still
  have no compact instruction candidates. Assets/report are retained outside
  `target` at `/tmp/opforge-bs22-identity-final`. m68020 now exports 1,175 semantic
  sequence rows, up from 1,151; unsupported rows drop from 903 to 879.
  Generation and row counts are not family parity proof.

Per-change comparison on identical 7,489-byte source / 1,632-byte output,
FS-UAE 68020/10 MiB, unlimited emulator CPU speed, telemetry off:

| Property | Baseline `f23ebf3a` | Final BS22 identity change |
| --- | ---: | ---: |
| Fresh START/DONE seconds | 12.159126833 | 12.323494250 |
| Focused executable bytes | 134,372 | 135,744 |
| Linked reserved bytes | 152,716 | 154,060 |
| m68020 package bytes | 342,736 | 346,768 |

Observed duration increases 0.164367 seconds (1.4%) in this single comparison;
this does not establish a statistical regression. The intermediate run before
exact-arity and unresolved-factor repair was 12.159168417 seconds; the indexed
batch before the final non-tuple safeguard measured 12.421884834 seconds.
Neither is the final measurement. Executable growth is 1,372 bytes; linked static reservations
increase 1,344 bytes, and package growth is 4,032 bytes (1.2%). Static reservations
are not peak memory. The focused image uses an external package; its image size
therefore excludes package growth. Workload SHA-256 is
`80b128940a471c41bd2c304b65cfdc9f9cf4bf0fa6f3ee7c98a66f888bd78107`.
Logs remain as `/tmp/opforge-identity-*.log`.

With the configured [FS-UAE environment](../../agents/rules/fs-uae.md), reproduce
this batch with:

```sh
OPFORGE_FS_UAE_MEMORY_PROFILE=68020-10m cargo test -p asm --lib binary_indexed_ -- --ignored --nocapture --test-threads=1
```

At this focused checkpoint, full self-host and physical A6000 qualification had
not been repeated for BS22. The full self-host result follows below. Regenerate
all older packages/bundles before further runs. Nonidentity
scales, nested/path/full-extension projections, listing provenance, BBR/BBS
fixups and the other corpus gaps remain separate work. No Rust source-language
semantics were expanded to make these controls pass.

After all builds and native runs finished, `make clean` reported 5.6 GiB removed
and left an empty `target` directory. Package inventory and diagnostic logs remain
outside it; unrelated workflow-notebook edits are preserved.

#### BS22 full self-host checkpoint — source `97589afc`

The complete current compact native CLI assembles itself with a fresh case-bound
START/DONE protocol, explicit guest exit zero and exact equality against the
entire live Rust Hunk oracle. Both the repository inputs and relocated bundle
inputs were freshly assembled by Rust before this native run. The bootstrap and
native-produced output are the same uninstrumented release configuration, each
embedding the identical m68020 BS22 package. This is full self-host completion
for this source/package state, not complete assembler-language or family parity.

| Property | Previous full checkpoint (`85f4e81f`, BS20) | Current source `97589afc`, BS22 |
| --- | ---: | ---: |
| Source/generated/package inputs | 98 | 100 |
| Total input bytes, including package asset | 1,388,392 | 1,424,440 |
| Complete release/bootstrap Hunk bytes | 456,048 | 482,512 |
| Linked static reserved bytes | 474,144 | 500,828 |
| Embedded m68020 package bytes | 321,532 | 346,768 |
| Native START/DONE seconds | 1,066.644855709 | 1,079.204961625 |

The current 100 inputs comprise 99 text/generated files and one package asset.
Source manifest fingerprint is `fnv1a64:86a557be012263fa`; complete release/bootstrap
Hunk fingerprint is `fnv1a64:d4707aa5c06d794d`; package fingerprint is
`fnv1a64:f049c4fe0e8ebb86`. All input filename components fit the classic 30-byte
limit. The native case used the fresh in-memory Rust oracle rather than a stored
expected-output file.

Duration is 17m 59.20s, measured from fresh guest START to DONE on FS-UAE
68020/74 MiB with the same configuration template as the preceding full run.
Telemetry is disabled; emulator startup and host preparation are excluded.
The observed increase is 12.560106 seconds (1.2%). Source, package and executable
all changed, so this is an aggregate workload comparison, not the isolated cost
of the latest identity-product change. Its focused comparison remains above.
Static reservations are not peak RAM; this run does not qualify the 2 MiB product
goal, a fixed hardware clock speed or physical A6000 timing.

The qualified bundle is `/tmp/opforge-selfhost-bs22-release-74m`;
`/tmp/opforge-a6000-current` now selects it. Both bootstrap and output embed only
`m68020--motorola68k.bin`. Invoking the hardware runner with `--dry-run` and its
default bundle selection validates the current sources and prepares local transfer inputs. It does
not prove remote transfer or hardware execution. From macOS Terminal, use:

```sh
python3 /Users/erik/Code/Retro/opForge/scripts/performance/run_a6000_selfhost.py
```

That command transfers to the A6000's `Development:` volume, measures overall
guest assembly duration, and requires fresh completion, exit zero and exact Hunk
equality. No physical-device run was performed in this checkpoint.

Reproduce the FS-UAE proof with the configured environment in the
[execution guide](../../agents/rules/fs-uae.md), a fresh absolute destination and:

```sh
OPFORGE_COMPACT_EXPORT_DIR=/tmp/opforge-selfhost-bs22-new \
OPFORGE_COMPACT_EXPORT_OUTPUT_EMBED=68020 \
OPFORGE_COMPACT_EXPORT_NATIVE=1 \
OPFORGE_FS_UAE_MEMORY_PROFILE=68020-74m \
OPFORGE_FS_UAE_TIMEOUT_MS=3630000 \
OPFORGE_FS_UAE_POST_START_TIMEOUT_MS=3600000 \
cargo test -p asm --lib export_compact_self_host_bundle -- --ignored --nocapture --test-threads=1
```

The complete log is `/tmp/opforge-bs22-full-selfhost.log`; the bundle contains the
current manifest, exact release oracle and command. The comparative summary is
`/tmp/opforge-bs22-selfhost-qualification-summary.json`; the default hardware
dry-run log is `/tmp/opforge-bs22-a6000-qualified-dry-run.log`. These remain outside
`target` and do not substitute for a fresh native run.

After the build/native batch ended, `make clean` removed 1.5 GiB and retained the
empty `target` directory. No production code changed during this qualification;
unrelated workflow-notebook edits remain untouched. No remote push was performed.

#### Wider native memory operations — source `cfa0ee92`

Hypothesis: contiguous word/long accesses reduce native instruction and memory
traffic during capture, packed-record preparation, expression compilation and
package input projection. Retain BS22, exact byte order, buffer boundaries and
register/status contracts. The native floor is 68020; these accesses are ordinary
RAM, including unaligned packed payloads. Separate buffers and variable-width
fields remain byte-oriented where widening would change their meaning.

The change replaces contiguous clears, fixed BE/LE scalar serialization, macro
and struct metadata copies, and the disjoint owned-capture copy loop. That loop
uses longs, then a word/byte tail without reading or writing past its count.
Nonzero LE scalar stores still reverse bytes explicitly. Four legacy package
service/bridge files and their smoke wrapper also receive contiguous clear
changes; those files are outside the compact self-host closure and its timing.
No CPU package, source-language rule, VM contract or instrumentation changed.

The unchanged 7,489-byte indexed workload still emits the exact 1,632-byte live
Rust oracle with the identical 346,768-byte BS22 package. On the existing
68020/10 MiB profile, the prior source's 12.323494250-second sample compares with
12.067000542 and 12.082992084 seconds for this checkpoint: mean 12.074996313,
2.02% lower. This is a modest observation from one baseline and two new samples,
not a statistical performance qualification. Workload SHA-256 remains
`80b128940a471c41bd2c304b65cfdc9f9cf4bf0fa6f3ee7c98a66f888bd78107`.
The external-package image is 135,580 bytes, down 164; linked reservations are
153,900 bytes, down 160. Native clocks from this profile are not combined
with the full self-host profile below.

The complete embedded release self-host finishes with fresh case-bound START/DONE,
explicit guest exit zero and exact equality against the entire live Rust Hunk.
Both repository and relocated bundle inputs were freshly assembled by Rust. The
bootstrap and produced output are the same release configuration, each embedding
only `m68020--motorola68k.bin`.

| Property | Prior full checkpoint (`97589afc`) | Wider accesses (`cfa0ee92`) |
| --- | ---: | ---: |
| Source/generated/package inputs | 100 | 100 |
| Total input bytes, including package asset | 1,424,440 | 1,423,665 |
| Complete release/bootstrap Hunk bytes | 482,512 | 482,348 |
| Linked static reserved bytes | 500,828 | 500,668 |
| Embedded m68020 package bytes | 346,768 | 346,768 |
| Native START/DONE seconds | 1,079.204961625 | 1,072.190396500 |

On the same FS-UAE 68020/74 MiB profile and template, duration is 17m52.19s,
7.014565125 seconds (0.65%) lower than the immediately preceding full run. This
isolates the current cleanup from earlier development, but includes its slightly
shorter assembly input (775 bytes fewer). One sample per full source state cannot
separate a small gain from run variation. No large or statistical speedup claim
is made. Telemetry, emulator startup and host preparation are excluded; no new
phase or peak-memory capture was performed. Static reservations are not peak RAM.

Source fingerprint is `fnv1a64:8c39f768c3beb5f7`; complete output fingerprint is
`fnv1a64:101e8c4f49ddf5b7`; the unchanged package fingerprint is
`fnv1a64:f049c4fe0e8ebb86`. The qualified bundle is
`/tmp/opforge-selfhost-wide-access-release-74m`, selected by
`/tmp/opforge-a6000-current`. Its local hardware-runner dry run passes. No remote
transfer, physical A6000 execution, full language/family parity or 2 MiB fit is
claimed. Run the maintained hardware command from macOS Terminal when desired:

```sh
python3 /Users/erik/Code/Retro/opForge/scripts/performance/run_a6000_selfhost.py
```

Focused validation covers mixed copy lengths and asymmetric endian values,
full-width literal/parameter/symbol handling, scalar characters, capture after
relocation and source poisoning, binary assets on both families, indexed operand
projections and odd-origin Hunk alignment. Literal/capture/placement controls
were repeated after the final fixed-width additions. The new copy-tail source
also has a Rust oracle with explicit expected bytes. Both native format checks
(319 legacy and 86 compact files), Rust formatting, workflow/boundary checks and
the native proof guard pass.

The whole-tree CCR checker remains non-green: its ten autofixable and 510
advisory findings are identical to HEAD before this change. Legacy
`external_fs_uae_hunk_smoke` fails at `tkpkg_debug_cli` without guest completion
on 68020/10 MiB; the same failure reproduces with all five changed legacy sources
restored to HEAD. Those clear edits have bounds/ABI review and host assembly,
but no successful legacy native smoke qualification is claimed. Neither existing
gap is weakened or reclassified as a pass.

With the configured [FS-UAE environment](../../agents/rules/fs-uae.md), reproduce
the unchanged-input timing using:

```sh
OPFORGE_FS_UAE_MEMORY_PROFILE=68020-10m \
cargo test -p asm --lib binary_indexed_workload_fs_uae -- --ignored --nocapture --test-threads=1
```

The new regression is `compact_copy_tails_fs_uae`; the placement control also
requires an absolute `OPFORGE_HUNK_PLACEMENT_ALIGNMENT_REPORT` path. Full export
uses the command in the previous checkpoint with a fresh absolute destination
and the same embedded-package, release and 68020/74 MiB settings. Logs are
`/tmp/opforge-wide-*.log`; comparison identities and durations are retained in
`/tmp/opforge-wide-access-performance-summary.json`. These stored results cannot
replace fresh native execution.

All builds and native runs finished before `make clean`; it removed 2.2 GiB
and retained the empty `target` directory. The qualified bundle and diagnostic
summaries remain outside `target`. Unrelated workflow-notebook edits are preserved;
no remote push was performed.

## BS23 package state — baseline 8940af51

Scope: connect package-owned STVM state to compact execution, starting with the
M68K `.fpu` and `.apollo` directives. Focused native state checks and complete
release self-host equality pass. The prior `cfa0ee92` full self-host is the timing
baseline; no new hardware result is claimed.

The hypothesis is that resolving the selected profile's existing STVM matrix to
numeric data will unlock configuration and guarded encodings without adding
CPU-specific directive rules to native code. Defaults, arguments, per-profile
legality and selector guard values remain authoritative in the package. The
65816 `.assume` grammar/state model and general CPU switching are separate work.

BS23 adds a bounded state-plan offset/length to the 200-byte header. All plan
references are relative offsets; candidate rows retain their 32-byte layout and
use their final word for a one-based guard identity. State plans contain numeric
defaults, directive/argument rows and ordered guard clauses. Each guard preserves
whether failure records a diagnostic refusal or merely mismatches. Guard refusal
precedes nested-plan predicates/recipes, and a later candidate may still succeed;
invalid metadata remains fatal. Unknown recipes
remain explicit unsupported rows. Only BS23 is accepted by the new executables;
old bundles remain historical evidence, not compatible runtime packages.

State argument spellings have a separate preparation-only dictionary role. This
keeps `.cpu 68040` aliases independent of the exact `.fpu 68040` vocabulary; an
unlisted spelling must not become valid through CPU alias normalization. The
writer binds arguments once, including quoted and numeric-looking spellings.
Assembly uses IDs and scalar values after lexical storage is released. Native
state storage is private execution state; no addresses enter the serialized plan.

Defaults reset per source sweep and on `.cpu`, as in Rust. Active directives run
at statement positions. Mapped sweeps replay state in source order; counted-loop
traversal precedes output filtering so skipped bodies cannot alter state. Inactive
conditionals and unreachable records do not apply directives.

Success requires fresh Rust/native output equality for legal transitions and
representative guarded instructions, fresh completed rejection for invalid
states, host package generation across all supported CPU/dialect pipelines,
existing compact controls, and a recorded unchanged-input timing comparison.
Representative corpus retries identify subsequent encoding gaps rather than
asserting complete FPU/Apollo instruction parity. Native error wording remains a
separate diagnostic-parity gap. A complete self-host comparison is the final
contract-migration check, distinct from all reduced tests.

Focused qualification (2026-10-05): live Rust oracles and fresh native execution
agree for m68020 external-FPU changes, quoted targets, an inactive conditional,
same-CPU reset, m68040 integrated-FPU operations, m68080 default FPU/Apollo toggles,
a lexical `on` symbol, and state directives expanded from a no-argument macro.
The mapped regression uses a supported concrete section with an initialized
prefix followed by imported logical content; `.for 0` does not change replayed
state. It is not qualification of separate-header/two-concrete-section flat maps.

Twelve state rejection inputs complete with explicit guest exit 20, including
illegal profiles, disabled instructions, invalid arguments and CPU spellings that
are not state argument spellings. A separate invalid `.res` control also rejects;
that is not state-parity evidence. Generic native failure text is checked, but
exact Rust diagnostic wording/precedence is not qualified.

Three synthetic policy inputs give the same edited selector package to live Rust
and native: a false soft guard preceding an unsupported nested recipe accepts a
later executable candidate; a diagnostic refusal also permits that fallback; and
a diagnostic refusal without fallback rejects before its nested operand mismatch.
Wire assertions ensure the guarded row actually precedes the fallback. These are
contract regressions, not additional canonical instruction coverage.

Host validation: 25 numeric-package tests; 284 packed-source tests, including
all registered CPU/dialect package-generation paths; three focused state oracles;
24 hardware-runner Python tests; production-library Clippy with warnings denied;
compact formatting (87 files), workflow/architecture guards and the native proof
contract check pass. Test-target Clippy additionally encounters existing VM/ASM test warnings
(including `field_reassign_with_default` at `runtime_model_core.rs:2669` and
redundant fields in CLI tests); the two new `clone_on_copy` findings were fixed.
No full workspace quality-gate claim is made. Native tests continue across individual
case failures before reporting the aggregate result.

The external-package executable is 137,296 bytes and linked static reservation
156,592 bytes, compared with 135,580 and 153,900 in `cfa0ee92`. Private numeric state
reserves 1,020 bytes for the bounded key limit; linked reservation is not peak RAM.
Timing, corpus retries and complete self-host qualification follow below.

Unchanged-input comparison, telemetry disabled, same 68020/10 MiB template:

| Property | `cfa0ee92` wider-access baseline | BS23 state slice |
| --- | --- | --- |
| Indexed source / output | 7,489 / 1,632 bytes | identical |
| m68020 package | 346,768 bytes | 376,418 bytes (+29,650; 8.55%) |
| External-package executable | 135,580 bytes | 137,296 bytes (+1,716) |
| Linked static reservation | 153,900 bytes | 156,592 bytes (+2,692) |
| Fresh START/DONE samples | 12.067000542, 12.082992084 s | 12.264527458, 12.060403084 s |
| Mean | 12.074996313 s | 12.162465271 s (+0.087468958; 0.72%) |

Both new runs complete with explicit exit zero and exact live Rust equality. The
input/output sizes and representative indexed operations are unchanged; package
and executable identities necessarily change. Two samples, with noticeable new
sample spread, do not establish a reliable timing regression or improvement.
The cost buys package state and newly lowered guarded plans; it is not an
optimization claim. Keep this incremental comparison separate from cumulative
gains, full-source self-host timing, hardware timing and peak memory. Unchanged
counted loops (MOS and M68K) and the MOS mapped-prefix control also pass fresh
native comparison under BS23.

Representative M68K corpus retry (`OPFORGE_M68K_CORPUS_CASES` selects the four
cases below) remains partial, not corpus qualification:

| Case | Fresh native result after state integration |
| --- | --- |
| `68020_fpu_registers.asm` | Rejects line 26, `FSINCOS FP0,.pair(FP6,FP7)`; prior first stop was `.fpu` |
| `68030_pflush_external_fpu.asm` | Rejects line 7, `PFLUSH #0,#0`; prior first stop was `.fpu` |
| `68040_integrated_fpu.asm` | Rejects line 9, `FMOVEM FP0/FP2,(A0)`; prior first stop was `.fpu` |
| `68080_apollo_gate_error.asm` | Fresh completed negative check passes after `.apollo` processing |

All four run independently; the audit exits nonzero for the three remaining
positive failures. The first-stop lines identify investigation targets, not proof
that all preceding instructions or emitted prefixes match. Full byte equality is
claimed only for the successful focused artifact cases above.

The three positive cases' stored `.lst` comparisons also fail. Bounded listing
checks for the first two find identical instruction addresses/bytes (20 and five
rows respectively), with drift in the header suffix, generated implicit-module
boundaries, line numbers and qualified symbols. Current live Rust assembly
succeeds. Golden listing refresh/qualification is a separate follow-up; no
reference fixtures were modified or stale output substituted for a live oracle.
The diagnostic `preparation step` can reflect the last completed preparation stage
on an assembly preflight failure; zero file/line is not enough to locate that
failure in preparation. The state mapped fixture initially hit existing flat
layout limits and was narrowed to the supported single concrete mapped section.

Complete BS23 self-host attempt: the fresh release run on 68020/74 MiB completed
the guest protocol with exit 20 after 562.204570500 seconds. It reported binding
completion (`preparation step: 00000002`) with zero file/line and produced no
successful output comparison. The exported case contains 101 source inputs,
1,466,695 source bytes, a 513,716-byte live Rust oracle and a 376,418-byte embedded
m68020 package. This failed qualification was localized and repaired below.

That reduced probe completes with native exit zero and exact equality with a
fresh 1,248-byte Rust Hunk (13,885 source bytes; 12.348308834 seconds on
68020/10 MiB). It assembles the actual state/package modules and a consumer of
all state record fields and five public routines. This rules out those isolated
bindings; it does not qualify the full-source case.

The full diagnostic capture uses `capture_compact_self_host_failure` with
`OPFORGE_COMPACT_EXPORT_INSTRUMENTED=1` and
`OPFORGE_BINDING_FAILURE_ONLY=1`. This keeps the existing bounded completion
snapshot, phase progress and allocation accounting while omitting detailed
token/binding/template/input counters. Its manifest records the exact bootstrap
defines. Diagnostic timing includes instrumentation; only a subsequent release
comparison can qualify the replacement.

The completed diagnostic capture (586.906738375 seconds, explicit guest exit 20)
identifies `binary_templates.State.Origin` during imported-name resolution. The
new `state` import alias collides case-insensitively with the module's existing
`State` struct; native alias resolution precedes local struct lookup. The repair
renames only that import to `pkgstate` and its `find` call. General parity for an
import alias sharing a local struct name remains a separate gap. The alias repair
leaves the complete Rust-built Hunk byte-for-byte unchanged.

Final complete release self-host qualification (2026-10-05): fresh case-bound
START/DONE, explicit guest exit zero, and exact live Rust Hunk equality pass on
the same 68020/74 MiB configuration. Bootstrap and output embed only the m68020
package; telemetry is disabled. The 101 mapped inputs total 1,466,701 bytes.
The complete Hunk is 513,716 bytes; linked static reservation is 533,012 bytes,
not peak RAM. Source fingerprint is `fnv1a64:b117cd67d550c1e3`, package fingerprint
`fnv1a64:b7ee99560acc04d9`, and bootstrap/output fingerprint
`fnv1a64:866683b8e9bcad5b`.

Uninstrumented START/DONE duration is 1,089.649658834 seconds (18m09.65s), compared
with 1,072.190396500 seconds at `cfa0ee92`: +17.459262334 seconds (+1.63%). This is
the integrated slice impact with larger source/package inputs, one full sample
per state. The unchanged-input indexed comparison above separately measures the
executable change. Neither comparison establishes hardware speed or the 2 MiB
product goal. Full self-host equality does not remove the remaining corpus,
diagnostic, alias-shadowing, language or CLI/output parity gaps.

The qualified release bundle is
`/tmp/opforge-selfhost-bs23-state-qualified-release-74m`, selected by
`/tmp/opforge-a6000-current`. Local hardware-runner dry-run validation passes;
no remote transfer or physical A6000 execution was performed. Logs and bundle
deliverables remain outside `target`; the build cache is cleaned at handback.

## BS24 FPU operand projections — baseline f7969d7f

Scope: lower canonical call-argument register and single-class register-mask
projections, beginning with `FSINCOS` and `FMOVEM`. Classes, bit positions,
reversal, state guards and encoding programs remain package data. Native code
consumes bounded numeric operands; no FPU spelling or opcode logic enters it.
PFLUSH and broader FPU/CPU parity remain separate work.

BS24 retains the 200-byte header and 12-byte projection slots. Kind 24 carries
operand ordinal, register class, zero Literal and argument ordinal (0/1) in the
Reserved word. The standard value-program field remains available. Kind 18 can
omit its second register class with `$ffff`, requiring a zero second shift.
The first class cannot be the sentinel. Producer, native consumers, inventory,
CLI identification and hardware runner migrate together; BS23 is superseded.

Packed preparation preserves dotted call boundaries and prepares argument leaves
without a source-text fallback. Scope tracking excludes a dotted callee from
address dependencies while its arguments retain normal binding. Shared bounded
call views select the requested existing argument without imposing a particular
callee name or exact argument count. Ordinary tuple views keep their 2/3 arity
contract. The canonical parser/selector remains the reference; this structural
transport does not close the existing frontend/EXVM ownership gaps.

Focused live Rust/native comparisons pass for FSINCOS paired destinations,
FMOVEM indirect, predecrement and postincrement transfers, a register range and a
call with additional scalar/register arguments. A separate m68040 mask case
passes. The complete focused case also proves FMOVEM.L control-register lists
in both directions. Four fresh native rejections agree with Rust for a wrong destination
class, a missing argument, a mixed-class mask and a descending range. Rejection
text remains generic rather than exact diagnostic parity.

Both complete `68020_fpu_registers.asm` and `68040_integrated_fpu.asm` now complete
with exit zero and exact live Rust Hex equality. The combined corpus audit still
returns failure for their existing stored listing drift; native equality does not
qualify those stored references. No references were regenerated or substituted.

A bounded limitation remains: preparation validates extra argument leaves that
canonical call-argument projections do not inspect. Unused nested calls, long
strings and unresolved extra names can therefore reject. General call-argument
transport/parameter semantics are not completed by this slice.

Host qualification passes 27 numeric-package tests, 287 packed-source tests,
production vm/asm library Clippy, all registered target/dialect package generation,
24 hardware-runner tests and relevant workflow/proof/format guards. Independent
Sol implementation and read-only integration review covered the projection wire
contract, native bounds, register preservation and reference tracking.

Separate release timing comparison: the unchanged indexed input is 7,489 source
bytes and produces the same 1,632 output bytes. On 68020/10 MiB, the preceding
BS23 samples were 12.264527458 and 12.060403084 seconds (mean 12.162465271).
BS24 samples are 12.303503500 and 12.068884875 seconds (mean 12.1861941875).
The difference is +0.0237289165 seconds (+0.1951%), within the observed sample
variation; no meaningful regression or gain is established. These are separate
START/DONE observations with telemetry disabled, not the full self-host input.
The external-package CLI grows from 137,296 to 137,612 bytes (+316), and linked
static reservation from 156,592 to 156,904 (+312). The m68020 package grows from
376,418 to 376,994 bytes (+576). Candidate count remains 3,386; six previously
unsupported rows now have executable projections. Generation remains source
project independent. Linked reservation is not peak memory.

Complete current BS24 self-host qualification (2026-10-05): release bootstrap and
assembled output both embed only `m68020--motorola68k.bin`. Fresh case-bound
START/DONE completed, guest exit was explicitly zero and the entire 514,608-byte
Hunk exactly matches newly assembled Rust output. All 101 mapped inputs total
1,469,495 bytes. Linked static reservation is 533,900 bytes; this is not peak
memory. Input fingerprint is `fnv1a64:d24acf8b5a76a710`, package fingerprint
`fnv1a64:618b859db7f54c74` and bootstrap/output fingerprint
`fnv1a64:01643ab5d2cdff45`.

Uninstrumented START/DONE duration is 1,092.771469042 seconds (18m12.77s), excluding
host preparation and emulator startup. The preceding BS23 full run was
1,089.649658834 seconds: +3.121810208 seconds (+0.2865%). This compares successive
complete implementations, including their changed source/package inputs, on the
same 68020/74 MiB configuration. It is one full sample per checkpoint rather than
an isolated same-input microbenchmark. The full Hunk grows by 892 bytes and static
reservation by 888; the separate unchanged-input comparison above owns the
measurement of the indexed workload.

The qualified bundle is `/tmp/opforge-selfhost-bs24-fpu-qualified-release-74m`,
selected by `/tmp/opforge-a6000-current`. Default hardware-runner dry-run preparation
passes. No remote transfer or physical A6000 execution is claimed. The older BS23
bundle remains a baseline artifact outside the repository tree, not a supported
current runtime package.


## BS25 required immediate expressions — baseline 2946d7bd

Scope: make the canonical `immediateN` projection executable in packed native
selection, beginning with the deferred 68030 PFLUSH form. The 68040 indirect
form is checked alongside it. Both encoding programs and CPU restrictions remain
unchanged in the canonical package; no MMU/CPU encoding logic enters generic
native processing.

Kind 25 uses the existing 12-byte slot: operand ordinal 0/1, zero Class, Literal
and Reserved, and the standard optional value-program field. Selection requires
the prepared hash wrapper and then uses ordinary scalar evaluation. Producer,
native magic/identification, inventory and hardware runner migrate together to
BS25; BS24 packages must be regenerated, without a legacy executor.

Success requires fresh exact Rust/native output for legal constant/expression
operands, fresh rejection for invalid wrappers/ranges/arity/CPU forms, and the
complete 68030 example. Compare the unchanged indexed workload with BS24 on the
same 68020/10 MiB profile; retain release self-host timing separately on 74 MiB.

Focused correctness passes two positive CPU cases (68030 constants, arithmetic
and assigned values; 68040 indirect registers) and nine fresh native rejections
for wrapper, range, arity and CPU restrictions. The complete unchanged
`68030_pflush_external_fpu.asm` now completes with fresh START/DONE, guest exit
zero and exact live Rust Hex equality in 1.545340166 seconds. The combined corpus
audit still fails its existing stored `.lst` comparison; that reference drift is
not native failure and was neither regenerated nor substituted.

All registered target/dialect package generation passes (16 generated, zero
failed, six without instruction candidates). The 68030 package gains one legal
semantic-input row and 24 bytes (376,972 to 376,996); the 68020 and 68040 package
sizes are unchanged. The external CLI grows 72 bytes (137,612 to 137,684), with
linked static reservation likewise +72 (156,904 to 156,976); this is not peak RAM.
Host qualification passes 28 numeric-package and 289 packed-source tests,
production vm/asm Clippy, 24 hardware-runner tests, native formatting and the
relevant architecture/workflow/proof guards. Sol implemented and reviewed the
Rust projection/contract migration and reviewed native field bounds and calling
behavior; the coordinator integrated and ran the fresh native checks.

Separate release timing comparison: unchanged indexed input, 7,489 source bytes
and identical 1,632-byte output, on 68020/10 MiB. BS24 samples were 12.303503500
and 12.068884875 seconds (mean 12.1861941875); BS25 samples are 12.035299500 and
12.060032375 seconds (mean 12.0476659375). The mean difference is
-0.1385282500 seconds (-1.1368%). With only two samples and variation
in the preceding pair, this does not establish a meaningful performance gain.
This change adds operand coverage rather than an optimization.

Complete uninstrumented release self-host now passes fresh case-bound START/DONE,
guest exit zero and exact live Rust equality for the entire 514,680-byte Hunk.
Bootstrap and output embed only the 376,994-byte m68020 package; 101 mapped inputs
total 1,470,140 bytes. Linked static reservation is 533,972 bytes, not peak RAM.
Input fingerprint is `fnv1a64:a6332b613ab63e31`, package fingerprint
`fnv1a64:fe2c303018fffd3b`, and bootstrap/output fingerprint
`fnv1a64:b2082f2bf51cbcb1`.

On the unchanged 68020/74 MiB profile, START/DONE takes 1091.929879083 seconds
(18m11.93s), versus BS24's 1092.771469042 seconds (18m12.77s):
-0.841589959 seconds (-0.0770%). These are one complete sample per
implementation, with different current source/package bytes; the small decrease
does not demonstrate an optimization. Keep this comparison separate from the
unchanged indexed input on 10 MiB. No physical A6000 timing, full peak-memory
capture or 2 MiB fit is claimed.

The qualified release bundle is
`/tmp/opforge-selfhost-bs25-immediate-qualified-release-74m`, selected by
`/tmp/opforge-a6000-current`. Local hardware-runner dry-run validation passes.
No remote transfer or physical hardware execution was performed. Deliverables and
validation logs remain outside `target`; its cache is cleaned at handback.


## BS25 four-example reassessment — implementation 2332b7d0

The unchanged complete sources were rerun on the current release executable,
with current packages, live Rust Hex oracles and the 68020/10 MiB profile.
The audit attempted all four cases independently. No production code, package
contract or stored references changed.

| Complete example | Fresh native result | START/DONE seconds |
|---|---|---:|
| `68020_fpu_allmodes.asm` | Exit 0, exact live Rust Hex | 1.766111833 |
| `68020_fpu_instruction_catalog.asm` | Exit 0, exact live Rust Hex | 2.026752000 |
| `68020_full_extension_addressing.asm` | Exit 20, rejects line 6: `MOVE.W (4.W,A0,D1.L*4),D0` | 1.016026083 |
| `68030_carry_forward.asm` | Exit 20, rejects line 12: `CAS2.W D0:D1,D2:D3,(A0):(A1)` | 1.017956583 |

All four stored listing checks still differ from current Rust. Consequently the
combined audit exits nonzero: two native parity gaps plus four existing listing
reference gaps. The two FPU successes qualify their complete Hex output only,
not listing output or the entire FPU family. Rejected complete files have no
qualified full artifact. Their rejection times are not assembly-performance
measurements; the positive times are single observations, without a same-input
completed baseline. No optimization claim follows from this reassessment.

At the BS25 baseline, read-only Sol analysis and coordinator inspection identified
two independent structural boundaries. Full-extension selectors already described `xp1:` paths
through indirect/bracket wrappers, tuple children, qualified register products
and qualified displacement values. The numeric lowerer had no path projection;
shallow tuple/identity-product projections could not express them. Packed nested
preparation also needed qualification before such paths could execute. The line-6
diagnostic alone does not localize all missing preparation/selection behavior.

CAS2 requires three top-level operands and projections into paired call arguments,
including indirect registers. Current native storage and call-register projections
cover only two operand slots and direct register arguments. Colon-pair normalization
must follow the canonical frontend contract; do not add a CAS2-specific native
parser. This is a separate structural slice.

### Numeric expression paths (BS26)

BS26 lowers the canonical `xp1:` forms to bounded numeric traversal programs,
with indirect/bracket unwrap, tuple child 0–2 and register, qualified-register,
scale or member-value terminals. Programs have at most eight four-byte steps.
Kind 26 retains the standard 12-byte projection slot and value-program field;
path programs are deduplicated outside descriptor arrays. Package-base-relative
offsets carry storage references; no binary record stores a memory pointer.
The producer, package validator, runtime, export/runner and CLI contract checks
migrate together. BS25 is retained only as frozen baseline evidence.

`binary_nested_operands.asm` owns bounded packed preparation and node views;
`binary_operand_paths.asm` executes the numeric programs. Instruction selection
only dispatches the new projection. Preparation preserves numeric names,
qualifiers, opaque expression/product capsules, nested parentheses/brackets and
commas. Qualified displacement-prefix and tuple spellings normalize alike.
Ordinary lexical struct fields retain shared scalar processing. Container views
distinguish singleton wrapper interiors from actual tuples. The existing indexed
projection path remains the reference; neither CPU spellings nor opcode selection
are added to these generic helpers. Existing conditional selection-position
telemetry records the new projection kind without adding release-time telemetry.

The complete unchanged full-extension example now has fresh native completion
and exact live Rust raw-output equality. Additional qualification covers scale
1/2/4/8, ordinary struct-field operands, invalid classes/qualifiers/scales and
corrupted path opcodes.

Separate release timing uses the unchanged indexed input (7,489 source bytes,
1,632 output bytes) on 68020/10 MiB. BS25 samples were 12.035299500 and
12.060032375 seconds (mean 12.0476659375); BS26 samples are 12.410430875 and
12.338585000 (mean 12.3745079375). This slice adds 0.326842 seconds (+2.7129%).
Two samples per state give only a limited estimate; this is a measured cost of
added coverage, not an optimization. The new full-extension example has no
completed native baseline, so its 1.522867916-second current observation cannot
establish a speedup. The additional valid scale/struct-field set takes
1.271196334 seconds. Rejected-input times are not assembly-performance samples.
The external CLI grows from 137,684 to 140,020 bytes (+2,336); linked static
reservation grows from 156,976 to 159,264 (+2,288), not peak RAM. The size figures
include the new helper code and ordinary latest-contract migration. Stored
listing drift from the four-example audit remains separate; references were not
regenerated.

All 16 registered target/dialect packages generate, with zero failures and six
without instruction candidates. The m68020 package grows from 376,994 to 378,874
bytes (+1,880); m68030 and m68040 grow by the same amount. Their final unsupported
row counts each decrease by eight, while declared rejection barriers remain
unchanged. The m68080 package grows by 6,764 bytes and its unsupported count
decreases by 30. These are host inventory facts, not family-wide native proof;
other/unclassified rows are not automatically legal coverage gaps.

Complete current-code uninstrumented self-host passes fresh case-bound START/DONE,
native exit zero and exact live Rust equality for the entire 518,896-byte Hunk.
Bootstrap and output both embed only the 378,874-byte m68020 package. All 103
mapped inputs total 1,489,427 bytes (including the generated binary package);
linked static reservation is 538,140 bytes, not peak RAM. Source fingerprint is
`fnv1a64:251e179f71cb9d53`, package fingerprint `fnv1a64:ba0f35c20b1942a8`, and
bootstrap/output fingerprint `fnv1a64:b5bd5e0ff6349a48`.

On the same release 68020/74 MiB profile, START/DONE takes 1114.496267083 seconds
(18m34.50s), versus BS25's 1091.929879083 (18m11.93s): +22.566388000 seconds
(+2.0667%). There is one full sample per state, with changed source/package
bytes; this is distinct from the unchanged indexed workload comparison above.
The complete source adds two files and 19,287 mapped bytes. No physical A6000
execution, complete peak-memory capture or 2 MiB fit is claimed.

The qualified release bundle is
`/tmp/opforge-selfhost-bs26-nested-qualified-release-74m`, selected by
`/tmp/opforge-a6000-current`. Local hardware-runner dry-run validation passes;
no remote transfer was performed. Logs and inventory remain under
`/tmp/opforge-bs26-*`, outside the build cache.

Affected host qualification passes 30 numeric-package and 292 packed-source
tests, 24 hardware-runner tests, production vm/asm Clippy, native formatting and
the architecture/workflow/proof guards. Two Sol delegates implemented/reviewed
package lowering and bounded native preparation/traversal; the coordinator
integrated and ran final fresh native checks. The new helper modules are 554
and 327 lines, keeping their responsibilities separate from selection.

This slice does not cover the remaining path operations/terminals, CAS2's third
operand and indirect call-child traversal, or PC-indexed fixup grouping. It does
not claim all expression parsing has moved into VM programs: the existing
EXVM/frontend ownership gaps remain explicit. Exploratory Rust oracles also
exposed restrictions on symbol-qualified displacements, a negative qualified
outer displacement and a span disagreement for parenthesized arithmetic before
an address tuple. Those need separate canonical investigation; the native path
must not invent behavior to work around them.

The raw report and command log are retained outside the build cache at
`/tmp/opforge-bs25-four-example-audit.json` and
`/tmp/opforge-bs25-four-example-audit.log`. The qualified BS25 self-host bundle
and default A6000 selection remain unchanged.

### Three operands and indirect call children (BS27, checkpoint)

The current slice extends shared numeric instruction transport to three bounded
operands and preserves package-recognized indirect register children in canonical
call operands. Opcode, register-class and range semantics remain in the canonical
package selectors. Candidate rows grow from 32 to 36 bytes, retaining old field
offsets and appending third-operand predicate slots; projection 27 selects an
indirect call child. Production accepts only the new BS27 contract.

Correctness separates same-source `PACK`/`UNPK` parity from transformed-input
CAS2 execution probes, with class, indirectness, arity and range controls. Rust
cannot directly parse the indirect child structure from `.pair((a0),(a1))`;
its family parser constructs that structure from raw colon pairs. Therefore
transformed-input output equality cannot qualify unchanged-source CAS2 parity. Existing two-operand, FPU-call and nested
addressing cases remain regression controls. Release cost is measured separately
on the unchanged indexed workload against BS26 (mean 12.3745079375 seconds,
140,020-byte external image, 159,264-byte static reservation); a fresh complete
release self-host remains required before updating the hardware bundle.

Raw colon pairs are a separate exposed boundary: Rust's family compatibility
parser creates `.pair` nodes, while the packed native frontend currently retains
token 5. This slice must not implement mnemonic-specific normalization in generic
native code. Its qualification must identify whether that spelling is still
unsupported; canonical call equivalence is not unchanged-source example parity.

Current evidence, before full qualification:

- Fresh native same-source `PACK` register/memory and `UNPK` output matches Rust.
  Existing FPU call/mask controls also match Rust. Seven malformed call/range
  controls complete with the expected explicit error exit.
- The actual native package validator accepts a valid BS27 package and rejects
  a nonzero reserved row word and an out-of-range third-operand form. Migration
  exposed two hidden 32-byte candidate strides in `binary_state`; these now use
  the package owner's row-size constant.
- A standalone component feeds actual package IDs through `prepare.line` and
  proves exact preservation of three calls, including wrapped register children.
  This is component evidence, not full frontend or instruction parity.
- Both positive transformed CAS2 CLI probes still reject during captured-source
  preparation, before the ORDER/BIND progress boundaries. The actual frontend
  component probe is host-assembled and ready for native localization. No CAS2
  output or unchanged-source example completion is claimed.
- Host checks pass: 510 VM unit tests, 297 compact-source subsystem tests
  (448 native tests ignored), Clippy for VM/assembler libraries, package generation
  for all 16 executable pipelines, workflow/architecture and fresh-proof guards.

The one valid release sample on the unchanged 7,489-byte indexed workload takes
12.506508041 seconds and matches all 1,632 output bytes. BS26's two-sample mean
is 12.3745079375 seconds: this single sample is 0.132000104 seconds (1.067%)
higher, not a statistically established slowdown. The second run stalled before
guest START while the Mac was locked; it supplies no assembly timing. External
image size grows 140,020 → 140,500 bytes (+480); linked static reservation grows
159,264 → 159,736 (+472), not peak RAM. The m68020 package grows
378,874 → 394,246 bytes (+15,372); 13,544 bytes are the four additional bytes
for each of its 3,386 candidate rows.

No BS27 full self-host or qualified A6000 bundle exists yet. The previous BS26
bundle remains baseline evidence; the migrated BS27-only hardware helper requires
a newly qualified BS27 bundle before its default command can be used again.
Native localization, the second release timing and complete exact-Hunk self-host
remain required before integration readiness.
