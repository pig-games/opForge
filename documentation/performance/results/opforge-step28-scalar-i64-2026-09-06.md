# Step 28: native signed scalar authority

Status: qualified; required checks and independent compliance passed. Step 28
closes with the focused commit containing this report. Phase A remains open.
The active `AGENTS.md` remains binding; no integration is claimed before commit.

## Implemented boundary

The native evaluator retains signed i64 pairs in its eight logical stack slots.
Add/subtract/multiply/power wrap to 64 bits; division/remainder and comparisons
are signed, and shifts retain Rust's masked count and logical-right semantics.
Symbol, resolver and current-address values originate u32 and zero-extend.
Legacy status/D3-low callers remain supported, with explicit runtime and bridge
high-result getters whose availability is cleared before evaluation/parsing.

The bridge accepts full-width numeric forms and separators, emits both literal
words, and rejects a `%` prefix without an immediate binary digit. The service
consumes request fields before publishing results: extension lengths below 20
receive no result write, 20–27 receive low at +16, and lengths at least 28 receive
low/high/width at +16/+20/+24. The +28 resolver is copied before execution.
The width marker is cleared before a potentially failing evaluation. Signed
`VALUE` text retains a signed-32-bit fast path and returns a zero-extended length.

## Performance contribution and cumulative evidence

This is principally a correctness prerequisite for Step 29 reservation handling.
The guarded common-digit path avoids the wide accumulator's 16-byte register
save/restore: high must be zero and low at most `0x0fffffff`; all validated
radices/digits then fit an unsigned low word after multiply/add. The full pair
path remains available. This is a bounded mechanism, not a measured speedup.
Paired shifts use a constant number of instructions rather than a per-bit loop.

Fixed product storage grows by 54 B of declared BSS: 32 B for the paired stack,
6 B each for runtime/bridge result state, 2 B for the retained extension length,
and 8 B for the larger decimal buffer. The wide decimal powers add 152 B of
constant data. There is no per-symbol or per-statement table growth. Executable
code growth is unmeasured; canonical source budgets are recorded below.

No matched step runtime comparison or initial-to-current Phase A runtime series
exists. Cumulative runtime gain is unmeasured. Earlier independent evidence
remains: source DOS reads 1,608→8 (1,600 avoided, 99.5025% fewer), and validation
wall time 1,889.188706→278.294873 s (85.2691% lower). These are different metrics;
no timings from unrelated workloads are compounded into a total speedup.

## Proof disposition

- Level A: live Rust arithmetic, source parsing, signed formatting, and explicit
  overflow-domain checks; existing focused expression contracts are retained.
- Level D direct runtime: the stored `scalar-native-third` receipt is FAIL
  because its separate service test caught stale high bits in the returned
  length. Its raw runtime subtest reported 414 cases across both native opcode
  versions with fresh zero exit and exact payload. That result is historical:
  the two redundant TST instructions were removed afterward. The subsequent
  `scalar-native-runtime-final` receipt passed all 414 cases across both native
  opcode versions with fresh zero exit and exact payload.
- Level D final parser/service: `scalar-native-fourth` passed the 27-value CLI
  fixture, 64 typed service cases, and four independent negative service cases.
  Each negative case required fresh completion, nonzero exit and its actual
  OTR923/OTR925/OTR922 diagnostic. That receipt precedes the two equivalent
  condition-code instruction deletions; the final runtime proof above covers them.
- `scalar-native-bridge-final` is an aggregate FAIL only because its old host
  assertion expected 25 bytes and received 26. Its native bridge FS-UAE case
  passed; the corrected host bridge receipt `scalar-host-bridge-final` also
  passed. The expanded 29-value CLI fixture adds high-bit symbol transport to
  existing accepted symbol grammar; the native qualification passed in the
  final bridge receipt.
- `scalar-capacity-fourth` failed only on stale canonical source-byte counts;
  its other five checks passed. `scalar-capacity-fifth` passed with measured
  loadable bytes 1,709,313, source rows 93,885, product bytes 3,562,944,
  imports 422, declarations 6,504, and name bytes 127,764. Final capacity is
  therefore recorded as passed. The completed regression and gate receipts
  below supersede the earlier pending dispositions.
  Host SKIP results are never native proof.

The completed contract reruns `scalar-contracts-5` through `scalar-contracts-10`
all passed. The complete scalar regression set 01–07 also passed. The
`scalar-quality-third` non-LSP Rust gate passed 1,626 assembler tests in
272.298948 s; earlier failed quality and contract receipts remain historical.

The first staged native gate failed on a permanent-negative harness-call false
positive. The checker was narrowed to the exact path with an exact `BSR.W`
exception; six guard unit tests passed and the other restrictions remain
active. The subsequent `scalar-staged-native-second` receipt passed. Workflow,
compliance and commit status remain open.

All native invocations use the existing fresh challenge/start/completion/exit
protocol, in-memory case oracle and ephemeral artifact cleanup. Zero-exit rows
containing evaluator errors prove successful harness rejection/status contracts;
they do not substitute for the separate negative-process proofs. Validation
wall time is not product runtime performance.

## Limits and preserved debt

Rust's ordinary minimum-i64 negation panics with overflow checks and wraps in
other profiles. Native deliberately fails closed; this is explicitly tested,
not claimed as release-profile successful-value parity. Rust minimum divided or
modulo minus one has no successful scalar authority and panics; native rejects.
Native bridge operator IDs predate this slice and differ from Rust portable
IDs. Tests encode the native ABI independently and call live Rust arithmetic.

The service harness sets a selected zero-label context and explicit versions;
it does not load a package or test a nonzero resolver. The separate CLI proof
covers package/facade integration. Failure-marker invalidation has source-order
review, while negative-process tests verify actual diagnostics and exits.

Independent technical review found no remaining Step 28 production blocker.
It recorded pre-existing compact-symbol grammar debt: `termLength` does not split
forms such as `label*2`, `label==2`, or `label&1` without whitespace. This remains
open Phase A frontend debt; no general grammar parity is claimed here.

The SHA-verified reservation draft remains preserved at
`/private/tmp/opforge-step28-reservation-draft-2a327798`. Step 29 must consume the
typed pair and remove temporary sign-text/duplicate-literal heuristics. B09
reservation/footer differences, unresolved relocation/encoding cases, timed-out
corpus cases, the six-byte engine initialization discrepancy, all outstanding
nonterminal native groups and frozen corpus requalification remain open.
LSP work remains last, Step 39. No Phase B work or Phase A closure is authorized
by these scalar receipts.

Workflow gate `scalar-workflow-second` passed all 136 workflow tests and the
native formatter. The first workflow attempt retained two stale module-count
failures; the refreshed tests assert the math module explicitly and preserve
all ownership limits. Independent review accepts the exact-path/exact-call
functional error-output exception; production debug calls, other harness debug
calls/labels, prohibited buffers and unknown macros remain rejected.

Independent formal plan compliance: **PASS**, `step28_scalar_review`, against
`agents/plan-compliance-reviewer.agent.md`. Independent Plan Quality: **PASS**,
`step20_tests`. The reviewer approved the mechanical completion/pointer/status
edits for this atomic commit. The final staged native gate also passed; exact
receipt hashes are in the JSON companion. Step 29 starts only after this commit.
