# Native runtime and memory diagnosis

## Current assignment and decision points

Native parity remains paused. The confirmed budget is **6.5 MiB for the complete
assembler process and 1.5 MiB for AmigaOS**. The current implementation compiles eligible
modules into current-build `.ofm` link objects, retires private compile state and links
at the end. Full native build qualification and resource comparisons are paused; there is
no current full-build memory result, net-saving claim or 8 MiB fit claim.

The current producer can publish owned capsules for closed public macros, segments and
statements. Capsules retain binary bodies, defaults, plans and names while preserving
caller binding and privacy. They enter the normal shared pools before the private
frontend retires. Interface-only producers write zero-block `.ofm` objects; values and
types remain in shared semantic owners without synthetic PCs, sections or a CPU
requirement when the interface has no effects. Temporary template facets are owned
in-memory capsules, not a persistent cache format. `.ofm` remains the current-build link
object format.

## Focused evidence

All seven dependency cases pass under both native policies (14 results), with exact live
Rust binary and Hunk output and zero remaining owned bytes. Candidate runs accept two
modules; forced-reference runs accept zero. Additional typed-interface cases without a
CPU, odd-statement and text-side-effect cases that publish zero modules also pass under
both policies. Dependent value-only and combined macro/segment/default/alias imports
pass. A proxy-scope kind-4 identity collision was fixed by the flags-aware classifier;
its focused identity case passes natively. The latest host release image is 777,400 B; its source has 258 files / 3,169,351 bytes
and manifest `fnv1a64:412ba8e45ee91ead`. A v27 full-build audit started but was
intentionally stopped when fallback remained; it is incomplete and supplies no native
completion, timing or memory result. Its captured prefix
contains 89 private-compiler outcomes: 46 OK, 41 EXECUTION_INVALID and 2 binding INVALID.
This is not full-build coverage. Focused results establish capsule behavior and object
lifecycle only; they do not establish all-fallback coverage, resource savings or budget fit.

## Next steps

Continue with package-owned position/relocation proof coverage. At stage 14,
`markInstructionRelocs`, 38 stops are DBRA/DBF and three are MOVE. The selected semantic
sequence supplies a relative-fixup cancellation proof, but module layout accepts a
replayable position recipe only for the separate semantic-branch form. First carry the
exact selected target into a same-fragment cancellation proof, preserving invariant
prefix bytes. Cross-fragment replay needs a generic field-fixup contract. Do not add
mnemonic-specific handling to native code. The MOVE and two binding failures remain
separate, unresolved cases.
Then complete a current full-native admission/correctness proof with dependency coverage.
Keep the root driver on the normal engine where needed. Full comparisons remain paused
until coverage and correctness are established.

For `.ofm` objects, keep the metadata directory limited to Header, BlockRows and RelocRows.
`add` retains the pending full image and copies the linker prefix; pending state supplies
carrier extraction. Store writes named objects and retires pending ownership. Compact
public semantic owners remain available to later compiles. `acquireEmit` reads payload
only during final Hunk emission, with one current-module cache. Unlinked or BSS-only
objects may have zero payload reads; selected payloads may be read at most once. Keep
object deliverables through successful cleanup and clear ephemeral owners.

## Qualification and limits

For later frozen comparisons, identify source, package, image and input manifests,
module/storage policy and fresh Rust oracles. Keep release timing separate from light
MEMD/capture instrumentation; three release repetitions are needed for a speed claim.
Native correctness requires fresh guest completion, explicit exit and exact live Rust
binary/Hunk output. Preserve the focused `.org`-inside-section negative fixture as an
explicit known gap, ignored for native pass claims with its Rust oracle retained; both
current native policies reject that existing case. The 6.5 MiB process budget includes
linked code/data/BSS, embedded package once, stack, heap and other process costs; the
1.5 MiB AmigaOS reserve is separate.
Memory fit requires the documented 68020-8m profile. See the [native parity contract](../../agents/rules/native-rust-parity-porting.md)
and [FS-UAE environment guide](../../agents/rules/fs-uae.md).

Known unrelated parity discrepancies remain: some placed mapped imported addresses can
stay section-relative in Rust flat output; native rejects `.org` inside a section; and
Rust retained an unreferenced named block in a module imported only for top-level effects
where native omitted it. A previous broad workspace run was red (2,319 passed, 161 failed,
667 ignored), so these focused changes do not establish clean workspace qualification.
Full 8 MiB fit and net memory gain remain unproven.

## Enduring earlier decisions

Keep adaptive symbol buckets/shared masks, release-gated telemetry, opaque assets,
compact capture headers, owned clearing elision, chunked capture, bounded prepared
records and completed-configuration retirement where they remain in current code.
Keep one reference engine and explicit ownership; CPU/family semantics remain package-
owned. See the [performance reference](../performance/vm-efficiency.md). Generic capture
contracts remain in [preparation ownership and capture storage](native-lazy-preparation.md).
