# Step 27 — CPU execution properties

Status: focused Rust, complete fresh native property proof and required full gates pass; independent plan-compliance review passes; focused local commit is next. Phase A is open.

CPEX v1 carries registry-owned word size and maximum program address through the existing package container. Rust and native validate canonical coverage and direct aliases, preserve legacy absence, and expose explicit property accessors. Native selection caches the values and publishes availability only with successful selection. This does not implement reservation arithmetic or listing retention.

The candidate main package is 368,579 bytes (SHA-256 `4d6a03d5718bc9380f02452294c404ec82e4e3716471ad5c95b349c8c6e25872`); debug package is 7,860 bytes (`14af49750c24709475c64c264b1aab8255c6cd809d17c92fbc5b059e223ec7d0`). The separately named candidate manifest retains all ten Step21 workload cases exactly. Historical packages and measurements remain distinct.

Step runtime gain and total Phase A runtime gain are unmeasured. This is a correctness prerequisite with lookup at package loading/CPU selection, no per-opcode property dispatch and no per-statement storage. Separately, the verified source-read mechanism reduced DOS calls from 1,608 to 8 (99.50% fewer); validation time fell 85.27% in its own experiment. Neither establishes a cumulative runtime percentage.

| ID | Hypothesis | Evidence for | Evidence against | Status | Next discriminator |
| --- | --- | --- | --- | --- | --- |
| H1 | New native source does not assemble | r2-r5 host diagnostics identify unsupported byte copy, location expressions and branch ranges | None | fixed | native-r9 complete proof PASS |
| H2 | Staged loading loses incoming package length | r6 both guests return 20 before required artifacts/diagnostic; clearLoadedState uses D0 as DBF counter before validation | r6 alone does not isolate the boundary | fixed | Staged-length contract and native-r9 PASS |
| H3 | Harness request fields use wrong byte order | Harness used MOVE.W; production parser reads the ABI fields little-endian | r6 exits do not separately prove this later boundary | fixed | Little-endian wire contract and native-r9 PASS |

The r6 failure is fresh guest-completion evidence, not parity: the positive guest exited 20 and produced no property artifact; the legacy guest did not produce its required diagnostic. Both cases ran. All case-specific guest trees are ephemeral. No ad-hoc instrumentation was added. Corrections remain awaiting complete native proof; movement of the failure boundary alone is not a fix claim.

Fresh r8 now clears the separate legacy-negative case, while the positive CPEX case still exits 20 without an artifact. The absent-dialect fixture used two NULs; the native parser correctly treated the second as an explicit one-byte dialect. Single-NUL requests correct that fixture. Static review found no remaining concrete CPEX load/selection cursor defect, so an opt-in approved debug-event probe now localizes the positive failure. It is Level E only and must be removed before Level D proof. The probe records successful boundaries in the existing bounded debug-event buffer, preserves all registers and CCR, never runs between a flag producer and branch, and only reports a nonzero harness diagnostic exit. It changes no production control flow or request/error buffers.

The final native-r9 filter passes all four tests without SKIP. Both fresh cases complete: the positive guest exits zero and its sixteen bytes exactly equal the live Rust accessor values from the same package, while the separate legacy guest completes with explicit nonzero exit and the required unavailable diagnostic. The final correction was in `cpexCountCanonicalCpusV1`: D0 held the nonzero count after D1 had been set to status zero, so CCR falsely told the caller to reject success. An explicit `tst.l d1` before return makes the branch reflect status. Independent review confirms the defect and correction; the focused source contract and complete guest proof verify it. The Level E probe was removed before this run.

Native-r9 terminal exit 0: 107.988065 seconds including host compilation, 864-byte log, SHA-256 `7dc1ebdc7d51d87ec20e11b87850e63209392710d38d7b862aa96c66e531764f`. This is validation duration, not a performance benchmark. All six product capacity tests pass after the corrected source budgets.

The final non-LSP Rust quality gate passes with terminal exit 0 in 288.540828 seconds, SHA-256 `57904ebe92858f2cc7206f20fef52e06d8ff00f3ef2e997d084cfeabf1da4060`, 338,112 log bytes. The staged workflow/native gate passes with exit 0 in 30.152138 seconds, SHA-256 `1a430f2f5ebf8ed0d98e6e840ce50857881219206650dc093e3a78b70a549a96`, 7,097 log bytes. The standalone harness formatter also passes; the standard native gate checks 238 files. LSP remains explicitly deferred. Earlier gate failures were retained and corrected in focused tests before the final successful gate.

Independent plan-compliance reviewer `step27_independent_review` returns PASS for this transport slice and its evidence. Step28 is the next implementation item after the focused commit; reservation semantics, all remaining corpus/native qualification and Phase A closure remain open.
