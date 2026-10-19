# Native runtime and memory diagnosis

## Current assignment and decision points

Reduce full-build runtime and peak memory without sacrificing shared VM/package
semantics or native correctness. Parity remains paused. The confirmed machine
budget is **8 MiB total: 6.5 MiB for the complete assembler process and 1.5 MiB
for AmigaOS**. No full-build fit is established.

The first-reference module proposal remains the intended direction, but its first
CLI integration attempt failed the resource gate and was removed. The ordinary
CLI retains the resident/disk engine; module images remain standalone.
The reusable frozen comparison covers a full assembler build, a small multi-module
project and a macro-heavy project.

Next work must retire meaningful owners before their peak. A smaller final
representation or a successful component alone is insufficient. State expected
savings first; drop changes below about 1% of the target resource. Use exact work
and capacity counters first, then at least three release timings for a speed claim.
No new observer framework, macro-only investigations or selector stack is needed.

## Module images: qualified component, not CLI integration

The private producer now saves independently relocatable blocks from shared code
sections, with owned scalar/list/schema exports. Exact label ownership distinguishes
an end label from the next block's entry even when their original addresses match.
Inter-block branches retain their package program and chosen candidate; linking
replays that VM program at the new source/target addresses. No instruction decoder
or new VM version selection is introduced. Absolute data recipes currently write
BE32; incompatible byte order rejects. PC dependence is separate from ordinary
Hunk cancellation proof, so unsupported numeric branches reject without changing
Hunk acceptance. The matching package must remain available;
persistent package fingerprints are not implemented.

The shared evaluator has a producer-only safety policy. It rejects positional
observations that cancel to an absolute/unsafe intermediate, including a later
expression that regains positive provenance. Positional conditions also reject.
This is conservative: even safe same-block differences remain unsupported until
there is a stronger proof or saved layout recipe. The shared assembly handler
likewise rejects alignment under this policy, since fixed padding cannot preserve
arbitrary block placement. Ordinary CLI policy is zero.

The real `binary_text_names` proof compiles once, moves its immutable image,
poisons source/original image storage, releases the request, then links routines
in order 3/1/2 at different addresses. All 528 bytes match fresh Rust. A second
fixture combines absolute data, relative branches and an end label sharing the
next block's original PC; all 30 bytes match. Eleven valid Rust inputs test
unsupported cancellation, immediate arithmetic, typed selection, aliases,
intermediate cancellation, comparisons, conditions, current-PC observations,
numeric branches without owned targets, and alignment with zero/nonzero original
padding.
They reject without publication and with balanced native cleanup. Existing native
module/export/image checks also pass.

| Real component, identical release/memory input | Result |
|---|---:|
| Source / prepared records | 3,677 / 2,985 B |
| Saved code / semantic metadata / complete image | 528 / 1,536 / 2,192 B |
| Blocks / retained inter-block recipes / compiles | 3 / 2 / 1 |
| Uninstrumented command interval, one observation | 0.145156 s |
| Separate MEMD peak tracked heap | 121,384 B |
| Retained after publication / terminal ownership | 2,192 / 0 B |
| Total allocation / free capacity | 214,944 / 214,944 B |

The component pair above predates the final eight-byte alignment guard; its
changed rejection branch has fresh eleven-case native coverage. The final CLI
series below includes that guard. This is a capability and ownership checkpoint,
not an optimization against an equivalent eager producer. It does not release
any additional ordinary CLI owner,
reduce the full-build peak, or establish 8 MiB full-build fit. The component input
identity is `fnv1a64:4f4b3ac7aa6e3722`; package identity is
`fnv1a64:415d2172932e243a`. Component intervals are max-speed FS-UAE host-observed
START/DONE times, not hardware measurements or statistically resolved speedups.

## Full resident reference and current self-host

**Storage modes must stay explicit.** The lower historical result was disk-backed:
6,909,720 B of tracked heap on an older frozen workload, excluding executable,
stack and OS. It is not a current resident result or a directly comparable input.
The disk policy remains available. Module images have not replaced either path.

On identical frozen 223-file / 2,789,855 B input, the unchanged resident reference
and the shared-section implementation both peak at **19,364,448 B** of tracked
heap, with **69,935,968 B** allocated/freed and zero terminal ownership. This is
not a rise from the older disk figure. The frozen source manifest is
`fnv1a64:d717c6af1a278e75`; the package is identical across roles.

| Isolated stage on the same frozen input | Release seconds | Observations | CLI bytes | Peak tracked heap |
|---|---:|---:|---:|---:|
| Unchanged resident reference | 124.860900 median | 3 | 728,804 | 19,364,448 B |
| Shared-section hooks | 123.548913 median | 3 | 729,140 | 19,364,448 B |
| Observation safety policy | 123.736524 median | 3 | 729,352 | 19,364,448 B |
| Separate PC-dependence metadata | 123.321652 | 1 | 729,456 | 19,364,448 B |
| Alignment safety guard | 123.884632 | 1 | 729,464 | 19,364,448 B |

Timing differences are within the approximately 2 s noise threshold. The final
stages have one observation each; no statistically resolved speed change is
claimed. Each added cost is reported separately rather than hidden in a total.
The final stage's separate MEMD run allocates/frees 69,935,968 B and ends
with zero ownership and no profiling errors. The final release reservation is
765,304 B; complete executable growth from the reference is 660 B.

The small multi-module workload's reference/shared-hook medians are
0.142664 / 0.141342 s (598,784 B peak for both); the macro workload's are
0.887214 / 0.897387 s (616,960 B for both). The observation policy's corresponding
medians are 0.141625 / 0.890455 s, with the same peaks. All 24 initial native
comparisons and 12 policy comparisons match their newly assembled Rust oracles.
The broad initial command also selected the intentionally ignored known Rust
mapped-address regression; that separate test remains failing and is not a new
defect. The final PC/alignment guards use focused full comparisons, not another
small/macro timing series.

The final **current-source resident self-host** completes in
**124.257629 s**: 223 files / 2,794,129 source bytes produce
all **729,464 B**, exactly equal to fresh Rust and the bootstrap. Its source
manifest is `fnv1a64:fd212346bcf3e1de`. The runner freshly assembles the staged
inputs with Rust before native launch; fresh challenge-bound completion, zero
exit and complete output equality are required in that same run.

The final image identity is `fnv1a64:4898407fbf4adb67`; the separately instrumented
image is `fnv1a64:f7116d6c21a00f49` (743,988 B). Input and output package storage is
embedded in all full comparisons. The current-source proof uses a different
input from the frozen performance series; its interval is not a per-change delta.

Full runs use the expanded 74 MiB diagnostic profile, pinned max-speed 68020 and
64 KiB stack. They qualify the maintained CLI, not module images inside CLI
scheduling, physical A6000 runtime, or an 8 MiB full build. Earlier A6000 proof of
the scalar/list/schema component belongs to checkpoint `3cdd5d99`; it is not
hardware qualification of the current code.

## Configured-module CLI integration: not retained

Checkpoint `cf78c259` preserves the rejected instruction-only converter. It
converted nine modules and saved 9,820 prepared bytes (below 0.5%), but retained
build-wide lexical/type/template owners. On its matched workload heap grew
11,776 B and allocation traffic grew 13,415,392 B; runtime was neutral. It missed
the 1 MiB owner-retirement gate and was removed rather than kept behind a switch.
Git contains its full measurements and implementation. The current component is
not a promotion or restoration of that converter.

## Step 1: budget qualification

The process limit is **6,815,744 B** and the OS reserve **1,572,864 B**. Include
linked code/data/BSS, embedded packages, stack, heap and untracked process costs.
Count the embedded package once. The final CLI's linked release reservation
is 765,304 B; after the 65,536 B stack its heap/other allowance is
5,984,904 B. The matched resident tracked heap alone exceeds the entire
process budget by **12,548,704 B / 11.97 MiB**, before static storage and stack.
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

The next integration boundary must map exported addresses to fragment IDs/offsets,
remap imported identities, and preserve template/nominal dependency closure. Local
section provenance alone is insufficient for independently placed fragments. Then
schedule a private module before its source-oriented owners enter the build-wide
peak; do not convert already-prepared records while retaining their old owners.
Keep captured binary source, declaration context, configuration, include suspension,
source order and whole-block reachability authoritative. Ordinary unsupported
modules must retain the complete reference path without repeating file effects.
The acceptance gate remains at least about 1 MiB of measured full-build peak saving;
no numerical gain is forecast before that lifetime boundary is sized.

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

Current frozen bundles, exact logs and machine-readable results live outside
`target` under `/tmp/opforge-shared-*20261019*` and `/tmp/opforge-shared-results-20261019`.
The final current-source full proof is `/tmp/opforge-module-shared-complete-selfhost-20261019`;
final frozen release/memory cases are `/tmp/opforge-module-shared-complete-frozen-*20261019`.
These are local evidence, not repository dependencies. Checkpoint `cf78c259`
preserves the rejected implementation; Git is the archive.
