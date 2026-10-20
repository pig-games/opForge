# Native runtime and memory diagnosis

## Current assignment and decision points

Broad language and CPU parity work remains paused. The confirmed budget is **6.5 MiB
for the complete assembler process and 1.5 MiB for AmigaOS**. The current implementation compiles eligible
modules into current-build `.ofm` link objects, retires private compile state and links
at the end. This slice has no full self-host or memory result. Full performance
comparisons remain user-paused; there is no net-saving or 8 MiB fit claim.

The current producer can publish owned capsules for closed public macros, segments and
statements. Capsules retain binary bodies, defaults, plans and names while preserving
caller binding and privacy. They enter the normal shared pools before the private
frontend retires. Interface-only producers write zero-block `.ofm` objects; values and
types remain in shared semantic owners without synthetic PCs, sections or a CPU
requirement when the interface has no effects. Temporary template facets are owned
in-memory capsules, not a persistent cache format. `.ofm` remains the current-build link
object format.

## Current host state and focused evidence

The current frozen source has 258 files / 3,174,298 bytes and manifest
`fnv1a64:63a12d57c2e5a9ba`. Its host release image is 777,888 B, linked static
reservation is 816,596 B, and the embedded package is 455,026 B with digest
`fnv1a64:415d2172932e243a`. These are image and linked-static sizes, not heap or total
memory measurements. This slice has no full self-host, runtime comparison or memory
comparison. Full performance comparisons remain user-paused; no resource or fit claim
follows.

All seven dependency cases pass both native policies (14 results), with exact live Rust
binary/Hunk output and zero remaining owned bytes. Candidate runs accept two modules;
forced-reference runs accept zero. Typed-interface/no-CPU, odd-statement and
text-side-effect zero-publication cases also pass both policies. The four-case CLI matrix
passes all eight native results under candidate and forced-reference policies, with exact live Rust binary/Hunk output, expected payload I/O,
and zero cleanup ownership. Candidate accepts two modules for same-fragment and one for
cross-align, cross-block and unanchored-PC; forced-reference accepts zero. Same-fragment
stores base + worker, while the other cases store only base and fall back for worker. The proxy-scope kind-4 identity collision was fixed by the flags-aware classifier.

The position-fixup proof is deliberately narrow. Production records transient
`Context.PositionFixupCount/Identity` with selected-encoder fields and counts every fixup
field. The only proven case is one resolved, untransformed target-relative field whose
input identities and target match exactly, under the geometry guard; it emits no Hunk
relocation. Any extra or nonpositional field is rejected and remains unproven by this path.
Layout retains the pending recipe, then freezes bytes only when exact source and target
labels belong to the same fragment. Cross-fragment, branch, mixed and transformed cases
remain unproven by this cancellation path; existing replayable branch recipes are
unchanged. No image VM contract change or CPU-specific hard-coding is involved.
Generic field assertions pass in both source orders against expected bytes; the
identity-bearing case matches the live Rust artifact.

The native component passes after moving the image and retiring the source: 32 Rust bytes
(20 payload + 12 endpoints), one fragment, zero serialized relocations, with forward and
backward four-counter-register cases. Targets across an alignment boundary, including
within one block, or in a separate block reject atomically. The full geometry case passes
its fresh protocol with exact 79 Rust bytes across 11 fragments and source retirement; it
covers code/data/BSS, alignment and replayable branch recipes. Its focused image digest is
`fnv1a64:16804ede7323d48c` (source `fnv1a64:58b578b49ab564f5`, package
`fnv1a64:415d2172932e243a`).

Earlier v27 audit counts (89 private-compiler outcomes, including 38 DBRA/DBF stops)
describe a prior source state and do not prove all 38 cases fixed now. Three MOVE and two
binding failures from that audit remain unresolved.

## Next steps

Localize the remaining three MOVE and two binding failures from the earlier audit without
assuming a repair recipe. Preserve fallback where neither a replayable recipe nor the
narrow cancellation proof applies. This focused slice
has not run a full self-host. Full performance comparisons remain user-paused.

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
