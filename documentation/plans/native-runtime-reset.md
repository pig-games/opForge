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

Current BS12 represents one CPU/dialect pipeline and carries its canonical
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

Copy `opforge_compact` and the needed `packages/` files together. The provisional
native syntax is:

```text
opforge_compact --cpu 6502 input.asm output.bin
opforge_compact --cpu 68020 input.asm output.bin -d motorola68k -P Development:packages
opforge_compact p.bin input.asm output.bin
```

Named selection prefers the matching embedded payload, otherwise loads
`PROGDIR:packages/CPU--dialect.bin` (or the directory selected by `-P`). `-M` and
`-I` retain module/include search. The explicit-package form remains useful for
harnesses and self-hosting; `-d` and `-P` apply to named selection, while an
explicit package path already determines the target and file. Quoted paths and
full Rust CLI flag parity remain
outside this provisional interface.

Catalog lookup, acquisition/ownership, structural validation and assembly are
separate modules. Catalog and package offsets are relative to their stated bases.
Both storage modes use identical BS12 bytes and the same validator. External
allocations are owned and released; embedded image bytes are borrowed and never
freed. A matching invalid embedded payload fails; it does not silently fall back.
After preparation, both modes still copy the execution prefix and discard lexical
storage. The whole embedded payload remains part of the executable image, so
tracked allocation savings alone do not establish lower total RAM use.

BS12 extends the header from 124 to 132 bytes: canonical target offset at 124,
length at 128 and zero reserved word at 130, all big-endian. Target identity lies
inside `RuntimeBytes`, survives preparation, uses safe filename characters and
fits in 26 bytes (plus `.bin`, within the classic 30-byte component limit).
Unknown contracts, wrong target identities, invalid spans, truncation and missing
files must fail before execution. Program interpreters retain opcode/version and
execution bounds checks beyond the common structural validator.

Configured embedded builds currently use Rust `.incbin` during host assembly;
compact native `.incbin` parity is still outstanding. The external-only default
source contains no `.incbin` and remains the self-hosting configuration. Embedding
a package proves its storage and execution path, not that an embedded build can
self-assemble or that every target's instruction forms are implemented.

The catalog's offset tables also require same-section address subtraction. Rust
DATA directives and compact Hunk provenance now recognize cancellation of equal
section bases; unrelated section bases still reject. Instruction fixups evaluate
their scalar before relocation proof, defer unresolved pass-one identities, and
recognize cancelled bases during pass-two reference accounting. Compact `.emit` preparation
remains a language gap; this slice uses `.byte`, `.word` and `.long` for its native
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
and whole-test wall time is 592.09 seconds. This is a current-checkout full
self-host proof, not a frozen subset. The source differs from the earlier
61-file checkpoint, so these times do not establish a before/after speed change.
It does not qualify the 2 MiB target, physical A6000 timing or an embedded build's
self-assembly. Embedded `.incbin` remains outstanding.

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
