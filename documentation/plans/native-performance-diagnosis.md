# Native runtime and memory diagnosis

## Current assignment and decision points

Reduce full-build runtime and peak memory without sacrificing shared VM/package
semantics or native correctness. Parity remains paused. The confirmed machine
budget is **8 MiB total: 6.5 MiB for the complete assembler process and 1.5 MiB
for AmigaOS**. No full-build fit is established.

The first-reference module proposal remains the intended direction, but its first
CLI integration attempt failed the resource gate and was removed. The maintained
resident/disk engine is unchanged. The reusable frozen comparison now covers a
full assembler build, a small multi-module project and a macro-heavy project.

Next work must retire meaningful owners before their peak. A smaller final
representation or a successful component alone is insufficient. State expected
savings first; drop changes below about 1% of the target resource. Use exact work
and capacity counters first, then at least three release timings for a speed claim.
No new observer framework, macro-only investigations or selector stack is needed.

## Owned module exports: qualified component, not CLI integration

The private leaf producer now saves public scalar, list/range and struct-schema
values with owned names/member keys in its offset-only image. A consumer moves
that image, poisons the original source/image, releases the request, then reads
the exported values. The live Rust consumer is the exact byte oracle. Unsupported
public templates and concrete nominal values reject; complete template/schema
closure and address/layout recipes remain integration work.

The maintained CLI/reference is unchanged. This is a capability/ownership
checkpoint, **not** a measured optimization against an equivalent eager producer.
No new full-build runtime or memory saving is claimed. The eventual CLI change
still needs to meet the meaningful-owner-retirement gate.

| Scaled component, identical release/memory input | Result |
|---|---:|
| Source / prepared records | 11,116 / 14,507 B |
| Stored code / semantic metadata / total image | 2,575 / 352 / 3,000 B |
| Compiles / selected block requests | 1 / 5 |
| Exact Rust output, including semantic consumer | 5,151 B |
| Uninstrumented release observations | 0.609526 / 0.613320 / 0.606791 s |
| Release median | 0.609526 s |
| Separate MEMD peak tracked heap | 150,576 B |
| Retained after publication / terminal ownership | 3,000 / 0 B |
| Total allocation / free capacity | 269,864 / 269,864 B |

The new saved image costs 361 B more than the previous code-only format for the
same 2,575 code bytes: 352 semantic bytes, eight header bytes and one alignment
byte. That is format overhead, not a full-build memory comparison. The separate
instrumented process estimate is 919,192 B (703,080 linked bytes + tracked heap +
65,536 stack), excluding OS/untracked costs. Component fit cannot establish
full-build fit. The last full matched memory comparison above 19 MB of tracked
heap remains far outside the 6.5 MiB process budget; this checkpoint changes none
of those build-wide owners.

These are fresh FS-UAE START/DONE intervals under max-speed 68020 emulation and
the 8 MiB component profile; they are not hardware times. Release image identity:
`fnv1a64:e5bb692519e60f2f`; source: `fnv1a64:a61403ed321af043`; package:
`fnv1a64:415d2172932e243a`. Memory runs use a separately instrumented image.

The same release component also passes on the physical A6000 (2026-10-19).
Downloaded inputs, the pre-run transfer snapshot, fresh challenge markers, exit
zero, all 5,151 oracle bytes and seven metric words were independently rechecked.
It compiled once and retained a 3,000 B image containing 352 semantic bytes and
three code blocks; the request image pointer was zero after retirement. Case
SHA-256: `4dee472c00f407ae58924f59a2614a78ff60bd3607a8008eccfbc4a61a173fb0`.
Both guest timestamps are 14:18:41 at one-second resolution, so they provide no
resolved assembly duration. The 1.462699 s host command interval includes network
and connection overhead. This release run supplies no hardware heap-peak result
and does not qualify the normal CLI module path or a full hardware self-host.

A fresh maintained-CLI self-host also completes: 223 files / 2,784,288 B produce
728,804 B exactly equal to live Rust and the bootstrap, in 121.379 s. Bootstrap
identity is `fnv1a64:f5d910ef19616ded`; source manifest:
`fnv1a64:5973448cccfce8c4`. This uses the expanded 74 MiB diagnostic profile and
64 KiB stack. It qualifies the hardware bundle's full self-host case, **not** the
new module producer inside CLI scheduling or an 8 MiB full build.

## Configured-module CLI integration: not retained

Checkpoint `cf78c259` preserves the attempted converter and its focused tests;
Git is the archive. It staged one closed module, executed eligible instructions
through the shared engine, and replaced them with compact raw-byte records plus
ordinary symbolic relocation expressions. Final layout and reachability remained
global. Unsupported whole modules used the original records once. Staging was
bounded by 1,024 rows / 1 MiB, with streaming fallback for larger modules.

This preserved tested behavior but did not release lexical, typed, template or
other build-wide owners. Conservative exclusions include typed expressions,
nonlocal PC-relative references, unsupported relocation forms and open bindings.
Only **9 modules** converted on the full workload: **16,279 -> 6,459 prepared
bytes**, saving **9,820 B**, below 0.5% of the prepared stream. It missed the
1 MiB preparation-peak target by two orders of magnitude.

Both executables assembled identical frozen **228 manifest files / 2,833,390 B**
with the same embedded m68020 package. Rust regenerated each exact oracle before native
execution. Frozen source identity: `fnv1a64:87c7e594df0f4cd4`; package identity:
`fnv1a64:415d2172932e243a`. Release output: **736,104 B**,
`fnv1a64:953b89ad30caf39b`.

| Full resident build, identical input | Reference | Candidate |
|---|---:|---:|
| Release observations, seconds | 125.293 / 125.613 / 125.693 | 125.158 / 126.238 / 125.760 |
| Release median, seconds | 125.613 | 125.760 |
| Separate MEMD peak tracked heap | 19,431,520 B | 19,443,296 B |
| Allocated/freed capacity | 70,365,056 B | 83,780,448 B |
| Prepared stream | 2,186,104 B | 2,176,284 B |
| Terminal tracked ownership | 0 B | 0 B |

Peak grows **11,776 B** and allocation traffic grows **13,415,392 B / 19.1%**.
The median difference is **+0.147 s / +0.12%**, well within the roughly 2 s
timing-noise threshold: no runtime improvement or regression is demonstrated.
The converter is removed rather than kept behind another switch. The ten focused
native cases covered exact output, macro fallback, source-PC fallback,
shifted section content, conditional fallback and oversized resident/disk paths.
All three candidate release runs rebuild the full frozen executable exactly:
native output equals the live Rust oracle and the candidate bootstrap, with
matching source manifests, fresh completion and explicit exit zero. Both full
instrumented rebuilds also match Rust and have balanced cleanup. This qualifies
the rejected candidate configuration, not an unimplemented general module-object path.

| Control | Release median, reference / candidate | Peak heap, reference / candidate |
|---|---:|---:|
| small | 0.1443 / 0.1389 s | 598,784 / 597,760 B |
| macro | 0.8881 / 0.8949 s | 616,960 / 616,960 B |

The small heap saving is only 1,024 B / 0.17%; macro peak is unchanged. Both
runtime differences are below meaningful resolution. All control runs match their
live Rust oracles and all memory runs finish with balanced ownership.
These are FS-UAE host-observed START/DONE intervals under pinned maximum-speed
68020 emulation, 74 MiB diagnostic RAM and a 64 KiB stack; they are not physical
68020 or A6000 times. Runtime excludes the separately instrumented memory runs.

## Step 1: budget qualification

The process limit is **6,815,744 B** and the OS reserve **1,572,864 B**. Include
linked code/data/BSS, embedded packages, stack, heap and untracked process costs.
Count the embedded package once. The current reference's linked release reservation
is 764,632 B; after the 65,536 B stack its heap/other allowance is
5,985,576 B. Its measured tracked heap alone already exceeds the entire
process budget by **12,615,776 B / 12.03 MiB**, before static storage and stack.
This failed resource gate does not justify another full 8 MiB trial.

The maintained `68020-8m` FS-UAE profile exposes 2 MiB chip plus 4+2 MiB fast,
no other/RTG RAM, max CPU speed and no cycle-exact execution. FS-UAE registers
8,372,224 B; the physical/registered difference belongs in the OS reserve.
See the [native run and budget recipe](../../agents/rules/fs-uae.md).

| Qualified boot environment | Free before assembler | Largest free block |
|---|---:|---:|
| Minimal Shell, 64 KiB stack | 8,161,520 B | 6,082,048 B |
| Minimal Workbench | 7,800,856 B | 5,768,160 B |
| Installed desktop and startup utilities | 3,352,232 B | 2,014,568 B |

Earlier full resident/disk attempts under the real 8 MiB profile ended with fresh
exit 20 and allocation failures. Those partial peaks do not establish successful
fit. The older frozen disk heap result **6,909,720 B** excludes image, stack and
OS and uses different input/storage from the resident table above; do not compare
those values as an optimization result.

## Hardware storage tradeoff

The unchanged 214-file / 2,648,242 B hardware workload produces 708,216 B exactly.
Its frozen workload SHA-256 is
`07043b5fb7dfe72b476e13925249cc313b0ba60e92cbc35495a861d700cc2593`.
The completed A6000 observations supplied by Erik are:

| Storage | Two completed release observations |
|---|---:|
| Resident | 123 / 124 s |
| Disk | 1,292 / 1,254 s |

Both modes match Rust. These are two observations, not the planned three-run
median. Hardware RAM was unrestricted, so neither mode proves 8 MiB fit. The
remaining batch was not replaced, overlapped or declared complete. Current CLI
measurements use FS-UAE because the A6000 is unavailable. Large disk overhead
supports moving toward sequential near-output module images, not another blind
page-cache tweak. Emulator DOS-read stalls were observed separately; no speculative
production retry was retained as a fix.

## Measurement contract

Use `native_cli_frozen_comparison` in
`crates/opforge-asm/src/tests/native_cli_frozen_comparison.rs` with five export
paths: `OPFORGE_CLI_FROZEN_BUNDLE`, `OPFORGE_CLI_BASELINE_RELEASE`,
`OPFORGE_CLI_CANDIDATE_RELEASE`, `OPFORGE_CLI_BASELINE_MEMORY` and
`OPFORGE_CLI_CANDIDATE_MEMORY`. The candidate release can also be the frozen input.
The test checks source/package/image identities, regenerates Rust oracles, then
runs full/small/macro workloads serially: three releases and a separate MEMD run
for each executable. Use the [configured FS-UAE environment](../../agents/rules/fs-uae.md),
`--ignored --nocapture --test-threads=1`, and explicitly pin clocks for diagnostic
RAM profiles. The memory builds use the five existing light progress/capture/MEMD
defines, not full hotspot instrumentation.

A self-build requires the exact output to equal its bootstrap and the frozen/export
source manifests to match. Instrumented code producing a release image is a full
correctness rebuild, not instrumented self-replication. Fresh challenge-bound
START/DONE, explicit zero exit and exact complete live-Rust bytes are mandatory.
Timeouts, partial captures and launch success supply no completed timing or peak.
See the [native proof contract](../../agents/rules/native-rust-parity-porting.md).

## Enduring earlier decisions

Keep adaptive symbol buckets and shared masks, release-gated reusable telemetry,
opaque asset bytes, compact capture headers, removal of overwritten record clearing,
chunked capture, bounded prepared records and completed-configuration retirement.
Clearing elision had no measurable full A6000 speed gain. Header compaction
previously saved 1.08 MiB on its own input; that historical
result does not qualify today's full build. Buffer reuse removed 88.41% of allocation
traffic without changing its measured 6,909,720 B frozen disk peak.
See the [performance reference](../performance/vm-efficiency.md).

Macro-only audits/deferral, first-use plan/candidate switches, binary-token/fragment
prototypes, include-text reuse and the prepared leaf cache remain removed.
The failed CLI converter adds no new maintained switch. The existing standalone
module image/request/leaf producer remains a limited component, documented in
[the object proposal](native-fragment-preparation.md); it is not CLI integration.
Current capture/storage ownership is in [the storage note](native-lazy-preparation.md).

## Remaining work and evidence limits

The next implementation needs a module semantic-completion/export boundary,
including closed scalar/typed definitions, import identities and placement-dependent
recipes, before its source-oriented owners can retire. Increasing only the frozen
instruction subset cannot deliver the measured missing memory. Discuss that scope
before another implementation; no new optimization or parity work is activated here.

Parked budget alternatives remain: cache-budget ablation under real 8 MiB,
a resident chain/hash bridge (up to 393,210 B extra at 65,535 IDs), adaptive
storage/cache selection from available RAM, and a refreshed exclusive hotspot
profile. They are candidates for review, not implemented changes or automatic
follow-on work. Complete the pending hardware repeatability evidence when the
A6000 is available; do not overwrite its interrupted batch. Choose a bounded next
implementation and parity-resumption checkpoint before extending this phase.

Qualification also reproduced two existing support gaps: Rust flat output leaves
some placed mapped imported block addresses section-relative, while native emits
the placed address; native rejects `.org` inside a section. The former has an
ignored host regression beside the comparison fixtures. Neither was introduced
or repaired by this memory experiment. Broad workspace qualification remains known
red from earlier baseline failures; focused checks are not a clean workspace claim.

The frozen bundles, exact comparison log and machine-readable results are outside
`target` under `/tmp/opforge-cli-fragments-*20261019*` and
`/tmp/opforge-cli-fragments-full-comparison.log`. They are local evidence, not
repository dependencies. Checkpoint `cf78c259` preserves the rejected implementation;
current documentation records decisions rather than its debugging chronology.
