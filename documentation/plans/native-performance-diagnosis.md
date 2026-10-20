# Native runtime and memory diagnosis

## Current assignment and decision points

Native parity remains paused. The confirmed budget is **6.5 MiB for the complete
assembler process and 1.5 MiB for AmigaOS**. The active direction is compiling eligible
modules to inspectable individual `.ofm` objects, retiring private state, then linking
objects at the end. The object lifecycle has focused native qualification, but full
per-module frontend coverage and full-build qualification remain incomplete. Do not
claim a full-build saving or 8 MiB fit.

The current focused native fix addresses `binary_memory.asm` and generated macro
hygiene. The actual-memory fixture reaches stage 14 after `MOVE` encoding; structural
failed candidates had poisoned the strict geometry proof. The generic guard repair
preserves conservative rejection of address-dependent and unknown geometry. Separately,
macro hygiene scopes no longer claim declared address values when their openers emit
no runtime label. Both the `binary_memory.asm` case and macro-hygiene regression
privately accept one module, write/read one `.ofm`, and match fresh Rust binary and Hunk
outputs. Forced-reference runs accept zero modules; both policies finish with zero
cleanup ownership. `binary_memory.asm` plus seven real includes totals 41,210 source
bytes, unchanged.

On the actual-memory case, finalization prepared-resident bytes were 424 B candidate / 8,652 B
reference. Peak tracked heap was 1,063,624 B / 1,034,392 B (+29,232 B candidate), and
instrumented intervals were 1.030 s / 1.010 s. These small-case figures are not evidence
of a full-build memory or speed gain. Current v20 host output was 766,268 B; no full
self-host has run on the current code.

A prior code state (`21c33d6b+cec`) passed full release self-host in 404.10960475 s:
248 files / 3,078,975 source bytes, manifest `fnv1a64:5c365008a574a159`, package
455,026 B (`fnv1a64:415d2172932e243a`), output 765,952 B
(`fnv1a64:69d248979eaaa1fa`) exactly equal to fresh Rust and bootstrap. It emitted 26
named `.ofm` files totaling 19,520 B. This was a mixed bulk/fallback path and is prior
correctness evidence, not current-code proof or an isolated speed claim.

Full comparisons are paused. The matching frozen workload has 246 files / 3,053,899 B,
manifest `fnv1a64:d0002edfe8a0d8f6`, and fresh oracle 763,288 B
(`fnv1a64:a53f1e2cc8d56bca`). The candidate release was aborted and the latest heap-only
run timed out after 30 minutes without a valid receipt. The old frozen reference heap
result is 7,439,536 B peak with zero live at completion, plus 809,472 B linked static
reservation and 65,536 B stack. It used an older image and is not a paired comparison or
fit claim. Fix module fallback coverage before resuming full comparisons.

## Object lifecycle and next steps

The resident layout directory contains Header, BlockRows and RelocRows metadata. `add`
retains the complete pending image and copies the linker prefix; pending state supplies
carrier extraction. Store writes one named `.ofm` per eligible module, then retires
pending ownership. Compact public semantic owners remain resident for later compilation.
Successful cleanup preserves `.ofm` deliverables separately from ephemeral captures.

`acquireEmit` is the only payload acquisition path. It runs during final Hunk emission
and caches one current module. Permit zero payload reads for unlinked or BSS-only modules
and at most one read per selected module. Check object writes, selected-module reads,
deliverable survival, exact outputs and zero private ownership. This is a lifecycle
contract, not a resource win.

The next code-bearing boundary is publishing closed public templates and their declaration
context: `binary_capture_store` exports three macros and the strict producer currently
rejects them. A separate interface-only gap rejects modules with no sections or blocks,
including ABI/layout modules. Static includes are supported by the memory regression;
active file effects and remaining closure still require complete ownership. Qualify a
current full self-host without quiet global fallback before interpreting resource results.
Retain the explicit reference route. Constants, types and exported
binary macros needed by later modules stay in compact shared public semantic owners;
compiled payload does not. Unresolved conditions, loops and relaxation stay in shared
numeric metadata/recipes and converge before payload streaming.

## Qualification and limits

Use maintained `native_cli_frozen_comparison` / `native_cli_frozen_candidate_comparison`
for any later frozen full comparison. Identify source, package, image and input manifests,
module/storage policy and fresh Rust oracle. Keep release timing separate from light
MEMD/capture instrumentation; three release repetitions are needed for a speed claim.
Native correctness requires fresh guest completion, explicit exit and exact live Rust
binary/Hunk output. Memory fit requires the documented 68020-8m profile; expanded memory
is diagnostic only. The process allowance includes linked code/data/BSS, embedded package
once, stack, heap and other process costs; AmigaOS has its separate 1.5 MiB allowance.
See the [native parity contract](../../agents/rules/native-rust-parity-porting.md)
and [FS-UAE environment guide](../../agents/rules/fs-uae.md).

## Enduring earlier decisions

Keep adaptive symbol buckets/shared masks, release-gated telemetry, opaque assets,
compact capture headers, owned clearing elision, chunked capture, bounded prepared
records and completed-configuration retirement where they remain in current code.
Keep one reference engine and explicit ownership; CPU/family semantics remain package-
owned. See the [performance reference](../performance/vm-efficiency.md). Generic capture
contracts remain in [preparation ownership and capture storage](native-lazy-preparation.md).

## Qualification limits

A previous broad workspace run was red (2,319 passed, 161 failed, 667 ignored); it is
not a clean workspace qualification. Known unrelated parity discrepancies remain: some
placed mapped imported addresses can stay section-relative in Rust flat output; native
rejects `.org` inside a section; and Rust retained an unreferenced named block in a module
imported only for top-level effects where native omitted it. These remain unfixed. Full
8 MiB fit and net memory gain remain unproven.
