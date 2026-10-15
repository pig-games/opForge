# Lazy preparation: inventory, publication and lifetime

Status: opt-in first-use macro call-plan construction is measured and natively
qualified for the frozen workload, 2026-10-15. Earlier binary interface and
first-use binding foundations remain available. Ordinary eager execution remains
the default reference. No capture is retired early. Historical checkpoint details
are in Git history.

## First-use macro call-candidate construction experiment

Authorized and measured 2026-10-15. Keep the
ordinary eager path as reference. Opt-in `OPFORGE_DEFERRED_MACRO_CANDIDATES`
selects the binary token/spelling interface and first-use body-plan binding.
The hypothesis is that retaining one whole-line spelling owner, rather than an
additional call candidate, reduces heap and copying enough to justify rebuilding
selected call candidates at their first accepted macro invocation.

This is deliberately **candidate construction**, not fully lazy grammar checking.
Physical capture still runs descriptor/fragment grammar and validates its result.
Declaration packing still needs fragment overlap proof; selected call descriptors
are revalidated against their declaration map before publication. Invalid unused
macros must continue to reject. Headers, defaults and whole-line fragment plans
remain eager. A capture without a valid whole-line backing owner keeps its eager
call candidate. Template segments and other non-macro consumers acquire a selected
pending call immediately, rather than retaining a macro-only recipe.

No per-record binary-view copy is retained. Existing captured TKVM rows and the
whole-line plan's exact code spelling supply a transient binary view. Candidate
services consume that view directly without physical source or another lexer.
A first-use recipe stores the global capture offset, numeric declaration map and
an acquisition-role tag; it contains no pointers. The immutable selected capsule
owns the package programs for this native session. The current single-capsule
CPU selection limitation remains unchanged. Successful acquisition publishes one
normal plan handle and is cached before the existing body-ready publication.
Capture ownership remains live through replay; no earlier retirement is claimed.
The adapter adds a bounded 1,324-byte stack frame plus its register saves; the
existing binary-view builder also uses bounded staging stack. No stack high-water
claim is inferred from heap telemetry.

Stop or revise on differing output/failure behavior. Compare unused and repeated
nested calls, declaration shadowing, defaults, exact whitespace and quoting,
embedded substitutions, includes and template lifecycle cases. Then run fresh
whole frozen-input Rust/native comparisons. Measure serial release pairs on the
same source/package bytes and heap in a separate matched instrumented pair.
The incremental baseline enables both earlier binary-view and first-use-binding
features in both images; only call-candidate construction differs. Ordinary eager
executables are retained and compared for byte identity separately. Report the
cost of this restricted experiment separately from prior binary-view and binding results; do not infer fully deferred-grammar savings. Preserve the
working eager path regardless of the measured outcome.

### Candidate construction: measured memory and behavior

The serial qualification passes 20 lifecycle cases, ten macro controls on both
images (20 fresh positive cases), and unused/invoked malformed-call rejection on
both images (four fresh expected failures). The unused malformed-call boundary is
an existing non-equivalence: Rust accepts it while native eager preparation
rejects it. This experiment preserves that native rejection; it does not claim
to close that language gap. Exact argument whitespace, defaults, lexical names,
caller value resolution, definition-relative includes and repeated calls are
covered. Instrumented controls prove two acquisition recipes / zero constructions
for an unused body and two recipes / two constructions when invoked twice.

The complete frozen workload passes both matched memory runs with fresh
completion, zero exit and exact live Rust equality: 198 files / 2,614,471 source
bytes, producing 708,592 bytes. This is **not a current-source fixed-point
self-host qualification**. These runs use the unchanged expanded A4000/68040
FS-UAE profile, not the 2 MiB product target or A6000 hardware.

| Resource, bytes | Eager call construction, prior features enabled | First-use call construction | Difference |
|---|---:|---:|---:|
| Tracked heap peak | 43,997,496 | 42,684,744 | −1,312,752 (−2.98%) |
| Allocated/freed capacities, each | 108,200,280 | 102,579,560 | −5,620,720 (−5.19%) |
| Linked reservation, memory image | 802,056 | 803,224 | +1,168 |
| Memory executable file | 766,348 | 767,588 | +1,240 |

Both end with zero tracked ownership, allocation failures, profiling errors and
telemetry diagnostics. There is no additional retained binary spelling copy.
The release executable grows 755,060 → 756,072 bytes (+1,012) against this
incremental baseline. The ordinary eager release/memory images are freshly
rebuilt and remain byte-identical to the preceding qualified eager images.

The full candidate creates 312 declaration-selected acquisition recipes and
constructs 25 on first use; 287 remain unconstructed. These are call-plan recipes,
not macro counts or all physical candidates avoided. Unselected physical call
alternatives with valid whole-line backing are also no longer retained; dynamic
source acquisition uses the same capture body mode. Non-macro selected calls
acquire immediately. Whole-line plans and their proof remain eager; 18,823 total
deferred binding recipes and 2,284 bound recipes match the preceding binding
experiment's counts. The candidate retains 585,728 recipe bytes and 606,044 normal
plan bytes; both retained byte totals match the reference exactly. No capture is
retired earlier.

Instrumented elapsed times are 113.013735 seconds (reference) and 113.861861 seconds
(candidate). They are diagnostics, **not release timing samples**. The release
pairs below provide the separate runtime comparison.

### Candidate construction: release runtime and decision

Two serial release pairs, in reversed order, have no instrumentation or heavy
host builds overlapping their timings. All four complete with zero exit and exact
live Rust equality. The matched memory runs above bring the total to six fresh
whole-input comparisons. Neither release executable changed between pairs.

| Order | Eager call construction, seconds | First-use call construction, seconds |
|---|---:|---:|
| Reference then candidate | 110.381911 | 110.110084 |
| Candidate then reference | 110.033302 | 109.757023 |
| Mean | 110.207607 | 109.933554 |

The measured difference is −0.274053 seconds / −0.249%. Individual paired changes
are −0.246% and −0.251%. Treat runtime as **effectively unchanged**; two pairs do
not establish a reliable speedup. The 2.98% tracked-heap reduction and lower
allocated capacities are the useful outcome. This does not establish savings
from deferring grammar execution: descriptor/fragment validation remains eager,
and selected calls repeat services during declaration validation and first use.

**Retain as an opt-in storage experiment; do not promote the compounded path to
the default from this result alone.** The eager reference remains available.
Whole-line/fragment proof, numeric declaration binding and capture lifetimes are
still the larger remaining responsibilities; their deferred design must preserve
current diagnostics and source-effect order. No broader implementation is started
by this result. A current-source fixed-point self-host remains a separate check
before promotion or handing over a corresponding self-host bundle.

Evidence: `/tmp/opforge-acquisition-full-20261015/plan.json`, `results.json`,
`summary.json` and each case receipt. The plan records the actual serial order,
FS-UAE configuration, saved test-binary identity, source fingerprints and build
flags. Images: `/tmp/opforge-acquisition-images-qualified-20261015/images.json`.
Serial component logs/evidence use `/tmp/opforge-acquisition-{lifecycle,macros,negative}-serial-20261015`.
Interrupted overlapping component batches are excluded; all reported component
checks were rerun serially. The [FS-UAE guide](../../agents/rules/fs-uae.md) now
makes its process-local coordinator and cross-process serialization requirement
explicit.

ASM Clippy with warnings denied, Rust formatting, the nine affected native
formatter checks, 147 workflow tests, 61 lazy-tool tests and 44 A6000-runner tests
pass. The focused four-file native CCR scan has no blocking findings and 15
report-only findings; it excludes included `.i` files and does not qualify all
native code. Generated first-use-binding self-host configuration now explicitly
defines the disabled binary-view and acquisition constants as well as its binding
constant, preserving the latest source contract rather than leaving symbols
unresolved.

## Shared binary token/spelling interface experiment

Authorized 2026-10-15. Hypothesis: a shared pointer-free input can preserve macro
spelling while letting descriptor grammar traverse token structure, removing the
need for physical source at the service boundary. This establishes a prerequisite
for deferred acquisition; it does not defer work or discard existing plans.

Keep ordinary eager acquisition as the reference. The opt-in
`OPFORGE_BINARY_MACRO_VIEW` build uses a transient binary view for physical macro
candidate acquisition. Existing numeric declaration binding, package-selected
programs, template ownership, defaults and expansion CPU selection stay intact.
Generated expansion handling continues through its existing services.

The provisional block has a 16-byte header and 12-byte atom rows, followed by
owned raw spelling bytes. Integers are big endian and all references are offsets.
Token atoms retain their TKVM kind and dense token index; gap atoms retain exact
space/tab bytes. Quotes, escapes and numeric spelling are preserved. Comments
are excluded using the lexical tokens' last extent, without another comment
parser. Consumers validate complete bounds and canonical coverage before atomic
publication. Descriptor grammar uses token kinds for structural delimiters;
fragment matching reads selected atom spelling directly. No physical line is
reconstructed and passed to the source descriptor scanner.

Compare exact descriptor/recipe outcomes and final emitted bytes, covering tabs
versus spaces, defaults, escaped markers, embedded substitutions, nested lexical
scope, definition-relative includes and package changes. Include malformed-view,
capacity and budget failures. Stop or revise on semantic divergence; report any
unsupported boundary explicitly. Keep the experimental input versionless and do
not introduce native VM version dispatch.

Measure release runtime on identical frozen source/package bytes, with reversed
serial pairs. Measure tracked heap and linked reservations in a separate matched
instrumented pair. Account for bounded transient view/staging stack separately;
heap telemetry does not measure stack high-water. This slice measures interface
cost, not projected deferred-preparation savings. Require fresh native completion,
zero exit and whole live Rust equality for the full-input comparison. That
comparison is not a current-source fixed-point self-host claim.

### Interface boundaries and qualification

The [Rust view](../../crates/opforge-vm/src/binary_spelling_vm.rs) and
[native producer](../../native/motorola68000/amigaos/prvm/prvm_binary_spelling.asm)
share canonical bounds: 64 tokens, 129 atoms and 1,024 spelling bytes. The
[native descriptor service](../../native/motorola68000/amigaos/prvm/prvm_binary_macro_descriptors.asm)
uses token structure; the fragment service scans the canonical atom payload in
place. Native executors add no VM version dispatch. Invalid views have no
published output; native reports invalid-view status with offset zero, while the
Rust view parser can additionally identify a malformed container row.

The view is a transient per-tokenization service input, **not a retained source
format or a lazy-acquisition implementation**. It contains exact code spelling,
including indentation, and excludes comments. Existing owned token captures and
candidate plans remain. Before first-use acquisition, decide which spelling atoms
must remain owned after the physical input disappears; do not multiply this
prototype's full view across all retained records and assume a memory saving.
Declaration identities, selected package grammar and capture-body mode are still
owned by the existing frontend. Generated argument text uses an explicit eager
route with its own spelling owner; it never borrows the cached physical view.

A whole-line recipe must use the view's code-only extent as well. Keeping the
physical length after removing comments caused the first full-input trial to
reject a commented instruction in `debug/memory_telemetry.i`. The corrected
adapter normalizes the owned row before recipe capture and plan creation; a
commented macro-body control covers that boundary. The rejected trial supplies
no performance evidence. The comparison tool can now stop after a failure,
preserving completed results without producing an invalid performance summary.

Seven supported macro controls pass on both the reference and candidate (14
fresh native cases), including exact spaces/tabs, defaults, lexical declaration
binding and definition-relative includes. All 20 module/include/template lifecycle
controls pass on the candidate. The independent native service probe passes 358
vectors covering live-token builder bytes, descriptor/fragment records, malformed
views, UTF-8, capacity/budget failures and atomic publication. The complete Rust
VM suite passes 582 tests; focused live-TKVM interface and harness tests pass.
The final service probe is rerun against current native sources. ASM/VM Clippy
with warnings denied, Rust formatting, all nine changed native files' formatter
checks, 147 workflow tests and 59 lazy-comparison/tool tests pass. The focused
four-file native CCR scan has zero blocking findings and 18 report-only findings;
it does not cover included `.i` files or qualify all native assembly.

Switching one source module from 68020 to 6502 after declaring a macro is **not
qualified native behavior**: the existing compact capsule rejects a different
CPU from its selected pipeline. The Rust oracle is preserved, and this existing
scope limitation is separate from declaration-context preservation. No multi-CPU
language support was added or removed here. Comment-free service input also
charges its retained extent rather than discarded physical comment bytes; exact
budget thresholds for those different inputs are not an equivalence claim.

### Separately measured interface memory cost

Both profiles assemble the identical frozen input against its live Rust oracle.
The ordinary reference has no work/candidate observer. Memory telemetry settings
are identical, with only `OPFORGE_BINARY_MACRO_VIEW` added to the candidate.

| Resource, bytes | Eager reference | Binary interface | Difference |
|---|---:|---:|---:|
| Tracked heap peak | 47,587,208 | 47,592,440 | +5,232 (+0.011%) |
| Allocated/freed capacities, each | 112,389,800 | 112,394,776 | +4,976 (+0.0044%) |
| Linked reservation, memory image | 794,472 | 800,456 | +5,984 |
| Memory image file | 758,636 | 764,648 | +6,012 |

Both finish with zero tracked ownership, allocation failures and telemetry
diagnostics. The interface reserves 2,620 additional scratch bytes per frontend,
plus alignment, and the producer temporarily stages at most 2,588 additional
stack bytes. These bounds are separate from tracked heap; no stack high-water
measurement is claimed. Whole-line plans now retain code-only spelling, so the
net measured cost is not simply the scratch-size sum. Existing eager services
remain linked for reference and generated text; the experimental duplication is
intentional and is not proposed as a permanent dual implementation.

Memory-run elapsed times are 109.919181 seconds (reference) and 114.482681 seconds
(candidate); these are instrumented diagnostics, **not release timing samples**.

### Separately measured release runtime and decision

The complete frozen input is 198 files / 2,614,471 bytes and produces the live
Rust 708,592-byte Hunk. These are serial release runs without memory telemetry,
work/candidate observers or diagnostic clocks. No heavy host build overlaps the
reported samples. The existing expanded A4000/68040 FS-UAE profile is unchanged;
these results do not qualify the A6000 or the 2 MiB product memory target.

| Serial order | Eager reference, seconds | Binary interface, seconds |
|---|---:|---:|
| Reference then candidate | 105.304775 | 109.846687 |
| Candidate then reference | 107.349713 | 110.631202 |
| Mean | 106.327244 | 110.238945 |

The incremental cost is **3.911700 seconds / 3.68%**. Individual paired overheads
are 4.31% and 3.06%; two reversed pairs give a bounded comparison, not a confidence
interval. The release file grows 747,740 → 753,752 bytes (+6,012). Both eager
reference images are byte-identical to the pre-experiment images; current-source
rebuilds also match the measured candidate images exactly. Per-image source
fingerprints, build flags and executable digests are retained with the evidence.

All six final full-input runs have fresh completion, explicit zero exit and whole
live Rust equality. They assemble the frozen earlier source; **no current-source
fixed-point self-host qualification is claimed by this experiment**. Evidence:
`/tmp/opforge-binary-spelling-full-final-20261015/plan.json`, `summary.json`,
`results.json` and each case receipt. The plan identifies the actual saved host
test binary, environment and FS-UAE configuration. The qualified image manifest
is `/tmp/opforge-binary-spelling-qualified-images-20261015/images.json`.

**Retain this as an opt-in functional foundation, not a standalone optimization.**
Its runtime cost is measurable and its tracked heap cost is small. It proves
binary input at these VM boundaries while preserving tested macro behavior;
it does not prove any deferred-acquisition saving. A subsequent slice must own
required spelling and declaration configuration across capture retirement, avoid
unnecessary duplication with existing captures, and compare real first-use
acquisition against eager execution before considering promotion. Keep the eager
reference available. Do not infer a peak saving from the earlier 3.38 MB payload
opportunity or subtract sampled generation windows from these release timings.

## Acquisition ownership and reconstruction investigation

Authorized 2026-10-15. Keep all execution eager. Attribute original candidate
generation calls, retained candidate bytes and sampled acquisition time to
physical macro bodies with and without accepted consumers. Count each physical
capture once, independently of template instance counts. Keep non-macro and
unassigned ownership explicit; reject duplicate replay, overflow and incomplete
accounting. Include closing records in the measured body envelope and disclose
that distinction. Do not interpret all observed work as avoidable.

An additional opt-in observer records only scalar owner ordinals and timing data
in audit-only capture fields. Declaration ownership comes from the existing
template state; usage comes from accepted calls. A gross total and five disjoint
categories separate retained non-macro records, retained never-invoked and invoked
macro-body envelopes, retained unknowns, and temporary captures not retained.
Successful constructor publication records independent gross totals; region
release records retained totals. Their difference is not an unused-macro claim.
Classification happens before capture release, after preparation consumers are
known. Sampled E-clock times
are diagnostic instrumented windows, not release costs or projected speedups.
Compare ordinary eager release timing against observer-enabled eager timing on
identical frozen inputs; measure heap and linked reservations in a separate pair
with matched memory instrumentation. Keep prior work-observer accounting as the
incremental baseline and report that scope explicitly.

Also inspect the reconstruction boundary: retained normalized token/lexeme rows
currently lose spelling that descriptor/fragment generation consumes. Establish
which shared binary/spelling interface is missing before proposing to discard
candidate plans. This investigation does not add lazy acquisition, retain source
text as a workaround, alter VM grammar, retire captures or change error policies.

### Binary reconstruction finding

The current normalized token/lexeme view cannot preserve all macro behavior on
its own. The executed
[spelling witness](../../crates/opforge-asm/src/tests/binary_source_text_recipe_semantics.rs)
uses `.byte "@1"` inside a macro and compares arguments `1 + 2` and
`1<TAB>+<TAB>2`, with literal one-byte tabs. The tokenizer produces identical
tokens, lexemes and original byte spans, but Rust emits respectively
`31 20 2b 20 32` and `31 09 2b 09 32`. Substitution precedes string decoding.
Byte spans identify gaps but cannot recover their lost contents.

Current `frontend.sourceDescriptors` passes physical source and token spans to
the package-selected shared descriptor VM. `captureFragments` passes raw spelling
to the shared fragment VM, whose current entry rejects token input. Plan creation
copies selected spelling into the retained arena. Those plans make source-free
replay possible today; discarding them also discards some required spelling.

The missing foundation is a shared binary token/spelling view preserving
**semantically observable spelling atoms**, substitution boundaries and declaration
configuration. It does not follow that every original line, comment or trivia
byte must be retained. A bounded token/atom VM interface would maintain the
no-source-reparse direction; keeping complete physical lines and reusing the
source parser would be a different design. No such replacement was implemented
by this investigation. A future implementation must retain acquisition body mode
and the declaration's package context, as well as numeric lexical ownership.

### Acquisition attribution results

The frozen 198-file / 2,614,471-byte input produces the complete live Rust
708,592-byte Hunk. Native completion, exit zero and exact output are independently
required. The observer does not skip work, change binding or release captures.

| Original acquisition ownership | Records | Candidate calls | Plan bytes |
|---|---:|---:|---:|
| Retained non-macro | 95,183 | 193,816 | 1,783,566 |
| Retained never-invoked macro body | 16,435 | 49,221 | 3,382,688 |
| Retained invoked macro body | 2,484 | 7,440 | 483,166 |
| Retained unknown | 402 | 804 | 0 |
| Temporary, not retained | 103 | 309 | 45,124 |
| Gross total | 114,607 | 251,590 | 5,694,544 |

The retained 114,504 records own 5,649,420 candidate-plan bytes. Never-invoked
body envelopes account for **59.88% of retained plan bytes** and **19.56% of gross
candidate calls**. These are physical capture records, not macro instance counts
or body-binding calls. Envelopes include closing and non-binding records. Accepted
invocations are consumers even when they emit nothing or their caller's block is
later omitted. Unreplayed captures and discarded temporary work remain explicit;
neither is reassigned to unused macros. All observer errors and duplicate replays
are zero; original work totals and definition/consumer relationships reconcile.

These bytes establish a potential storage budget, not a peak-heap saving. A future
implementation still needs binary tokens, spelling atoms, structural validation,
declaration context, recipes and used-body plans. Preserve unused-body diagnostics;
the entire generation cost cannot be assumed removable.

### Separately measured observer memory overhead

| Resource | Prior eager work observer | Candidate observer | Added cost |
|---|---:|---:|---:|
| Tracked heap peak | 48,390,040 | 49,758,104 | 1,368,064 (+2.83%) |
| Allocated/freed capacities, each | 113,993,912 | 116,692,152 | 2,698,240 (+2.37%) |
| Linked reservation, memory image | 2,213,300 | 2,215,156 | 1,856 |
| Memory image file bytes | 771,776 | 773,200 | 1,424 |

The observer adds twelve bytes per retained capture and 532 fixed scalar bytes.
Capacity rounding makes the tracked peak different from the field-size sum.
Linked reservation includes code and static storage separately from tracked heap.
Both memory runs finish with zero tracked ownership, allocation failures,
profiling errors and decoder diagnostics. These are observer costs, not deferred
preparation savings. The sampled image uses period 64 / phase 0; its elapsed time
is diagnostic, separately from release overhead.

### Separately measured observer runtime overhead

Both executables execute the eager path. Counter-only release images omit memory
telemetry and candidate clocks; the baseline retains the preceding work/template
observer. This is the **incremental** cost of the new inventory, not the total
overhead of all measurement facilities or a production performance improvement.

| Serial order | Prior observer seconds | Candidate observer seconds |
|---|---:|---:|
| Prior then candidate | 111.725507 | 113.268668 |
| Candidate then prior | 111.522847 | 111.756279 |
| Mean | 111.624177 | 112.512473 |

Mean added cost is **0.888296 seconds / 0.80%**. The individual paired deltas are
1.38% and 0.21%; this is a small two-pair estimate. Release file size grows
758,224 → 759,000 bytes (+776), linked reservation 2,197,936 → 2,199,188 (+1,252).
The separate instrumented memory pair takes 115.730986 / 116.770025 seconds;
those elapsed times are diagnostic and are not additional release samples.

All six full frozen-input runs finish with fresh protocol, explicit zero exit,
whole live Rust equality and conserved observer/work/template accounting. They
assemble the frozen earlier source, not the current candidate's own source, so
these are **not current-source fixed-point self-host qualifications**. No heavy
host builds overlap the reported samples. Evidence and actual execution order:
`/tmp/opforge-candidate-full-final-20261015/plan.json`, `summary.json` and the six
case receipts. The actual test binary and maintained FS-UAE hardware/configuration
are identified in that plan. It uses the existing expanded A4000/68040 profile;
there is no A6000 or 2 MiB qualification in this investigation.

### Sampled acquisition time and decision

Two separately completed instrumented runs use different periods/phases. Counts,
retained payload, definition rows and tracked heap peak agree exactly; both require
the whole live Rust output. These estimates describe only original candidate
generation, excluding tokenization, construction/retained copying, binding and
later preparation. They include work-observer and clock-boundary cost.

| Acquisition class | Period 64 / phase 0 | Period 128 / phase 37 |
|---|---:|---:|
| All acquisition, estimated seconds | 6.8475 (1,791 samples) | 7.0639 (895 samples) |
| Never-invoked body, estimated seconds | 1.9079 (264 samples) | 1.9376 (117 samples) |
| Invoked body, estimated seconds | 0.3085 (44 samples) | 0.2984 (18 samples) |

Category estimates use each category's own mean; do not add them to reconstruct
the independently weighted global estimate. Clock observations are conserved;
extrapolated estimates need not be additive. Matching two configurations is a
sensitivity check, not a confidence interval or a release performance result.
The second complete run takes 119.541603 seconds with identical 49,758,104-byte
tracked heap peak. Evidence:
`/tmp/opforge-candidate-sensitivity-proof-20261015/analysis.json` and its fresh
`result/native-result.json`.

The hypothesis is supported primarily for **memory**: never-invoked body plans
occupy 3.38 MB. The sampled generation window suggests roughly two instrumented
seconds of opportunity, with validation and required spelling still to preserve.
It does not support a claim of a large overall runtime gain. Extending first-use
copy/remapping to more template kinds alone does not address this acquisition.

The next prerequisite is a bounded shared VM token/spelling interface experiment,
covering the observable whitespace witness, quoted escapes, defaults and embedded
substitutions before discarding any captured plans. Preserve declaration identities
and the descriptor grammar's acquisition configuration while retaining existing
CPU switching behavior during expansion. Its success would enable a later measured
first-use acquisition candidate and persistent binary templates. It was proposed by that investigation; the separately authorized interface
experiment above now investigates it.

All 20 lifecycle/module/include/template controls pass with the final six-category
observer. The independent native probe passes preservation, duplicate ownership,
unknown accounting and 64-bit accumulation checks. A focused imported-template
case proves the distinction between gross acquisition and discarded temporary
captures. Rust independently checks report conservation; the executed spelling
witness proves normalized tokens alone lose observable macro behavior.

Final Rust report/disabled-emission/spelling checks, asm Clippy with warnings
denied, Rust/native formatting, 57 lazy-tool tests, eight candidate-decoder tests
and 147 workflow tests pass. The helper now supplies the explicit experimental
include root required by cross-directory probes; include containment checks are
unchanged. After formatting, both measured candidate executables are byte-identical
to the qualified images. The ordinary release executable remains byte-identical
to the preceding commit (747,740 bytes). Identity evidence:
`/tmp/opforge-candidate-tools-20261015/final-image-identity.json`.

## Measured experiment: first-use macro-body plan binding

Authorized 2026-10-15. Hypothesis: keeping declaration-time numeric binding while
postponing captured descriptor copy/remapping avoids the measured never-invoked
macro work and reduces its plan arena, at an acceptable cost for compact recipes.
The compile-time `OPFORGE_DEFERRED_MACRO_PLANS` switch keeps the eager reference.
The shared `experimental/build_configuration.i` maps that Rust preprocessor
definition to the module-local `DEFER_MACRO_PLANS` constant (otherwise zero).
Owners use ordinary `.if` conditions; optional ABI fields reserve zero bytes when
disabled. A native source build can select the same mode by defining that constant
in the shared include, without adding native CLI define support.

The first slice covers ordinary captured macros. The writer still binds names and
produces their packed map at declaration. A recipe owns that map and numeric global
capture offset/candidate handle; it contains no borrowed pointers. A direct,
cursor-independent resolver reacquires an immutable capture view across includes
and region movement. First invocation binds all body plans once, caches normal
handles and then queues the macro. Header/default binding, invocation scope and
shared VM grammar stay with their existing owners. Segment/statement templates
and non-captured input stay eager. Captures already survive through replay;
this experiment does not retire them early.

Malformed-plan validation remains eager through shared validators. Compare the
new shared validation's eager path with the preceding eager executable as well
as comparing eager/first-use builds from the same final source. No behavior is
selected by telemetry source name, fixture, expected output or benchmark identity.

Success requires fresh exact Rust and eager-output parity, owned-map/relocation/
cache/invalid-input/preservation probes, and separately measured release runtime
and heap/linked memory on identical frozen inputs. Report actually created/bound
recipes and storage; do not count every retained header/recipe as removable. Then
run a separate full current-source self-host of the selected configuration.
Stop or revise on semantic/diagnostic drift or a material runtime/memory regression.

This postpones descriptor copy/remapping, not lexical name binding, tokenization,
capture construction or all expression preparation. The existing declaration
writer still establishes numeric identities. Recipe storage is a 20-byte header
plus two bytes per packed-map entry (including its sentinel). Closing records can
also create recipes that are never retained in a body; their overhead is included.
All body alternatives are bound together on first invocation. Smaller first-use
units and early capture retirement remain separate experiments.

### First-use memory comparison

Both fresh instrumented runs assemble the same frozen 198 files / 2,614,471 source
bytes into the complete live Rust 708,592-byte output. They report zero allocation
failures, profiling errors and terminal tracked heap. These measurements exclude
OS/stack/allocator metadata and do not establish the 2 MiB product goal.

| Resource | Eager reference | First use | Change |
|---|---:|---:|---:|
| Tracked heap peak | 47,587,208 | 43,992,264 | −3,594,944 (−7.55%) |
| Allocated/freed capacities, each | 112,389,800 | 108,195,304 | −4,194,496 (−3.73%) |
| Linked reservation, memory image | 794,472 | 796,072 | +1,600 |
| Memory image file bytes | 758,636 | 760,336 | +1,700 |
| Terminal tracked heap | 0 | 0 | 0 |

The experiment creates 18,823 recipes and materializes 2,284. The 16,539 remaining
recipes reconcile with the earlier inventory's 16,351 body-binding calls in
never-invoked instances plus 188 closing records in invoked instances. They are
not 16,539 unused macros. Used recipe storage is 585,728 bytes; the resulting normal
plan arena uses 606,198 bytes. Neither is a reserved-capacity or whole-session peak
measurement. Zero entry-cache hits is expected here: `BodyReady` skips repeat body
binding at the template boundary. The component probe separately exercises cache
reuse. Captures and template bodies still remain live under their original owners.

Evidence: `/tmp/opforge-first-use-full-final-20261015/memory-reference/result.json`
and `memory-audit/result.json`; the memory runs' elapsed times are diagnostic only.

### Separate release runtime comparison

The same frozen full input and package configuration is assembled serially by
uninstrumented eager/first-use executables. Every run requires fresh native
completion, exit zero and the entire live Rust output.

| Order | Eager seconds | First-use seconds |
|---|---:|---:|
| Eager then first use | 102.714222 | 101.621366 |
| First use then eager | 101.880230 | 102.866117 |
| Mean | 102.297226 | 102.243741 |

The mean difference is **−0.053485 seconds / −0.05%**: effectively unchanged,
well within the observed spread, not evidence of a speedup. The measured value is
memory reduction. The release image grows 747,740 → 749,048 bytes (+1,308), with
linked reservation 781,512 → 782,772 bytes (+1,260). These are distinct from the
instrumented-image costs above.

All six full frozen-input runs pass. Evidence:
`/tmp/opforge-first-use-full-final-20261015/summary.json`.
The eager path remains the default; the experiment remains opt-in.
The benefit is workload-dependent: this input has many never-invoked definitions.
Used bodies retain recipe storage as well as their materialized plans. These
results do not establish a memory or timing benefit for projects that invoke
every definition, and do not justify promoting the switch globally.

### Shared-validator refactor measured separately

The preceding eager executable assembles the identical frozen input in
102.535939 and 103.142029 seconds (mean 102.838984). The new eager path's two
samples above average 102.297226: **−0.541758 seconds / −0.53%**. This shows no
observed slowdown, not a demonstrated speedup at this sample size. It is separate
from first-use binding's −0.05% comparison. The refactor adds 248 release-image
bytes and 248 linked bytes; it introduces no new heap owner in the eager path.
Both old-executable runs finish with complete live Rust equality.
Evidence: `/tmp/opforge-first-use-validation-cost-20261015/summary.json`.

### Qualification

The six new macro controls pass on the first-use memory and component-probe images
(12 fresh native runs). They cover unused/repeated calls, mutable declaration
values versus caller shadowing, defaults, included definitions after source-slice
changes, definition-relative child includes, and the existing 68020/6502 macro
language matrix. The component probe covers malformed descriptors/maps, owned-map
mutation, capture relocation, failed realization, eager descriptor equality,
cache/reset behavior and register/CCR/stack preservation. All 20 prior lifecycle,
module/include, whole-unit and template controls pass on eager and first-use
memory images (40 more fresh native comparisons).

After adding source configuration, all five rebuilt release/memory/probe images
are **byte-for-byte identical** to these measured and controlled executables.
Source identity `fnv1a64:fe289ab69d98d9c2`:
`/tmp/opforge-first-use-images-config-width-20261015/images.json`.
Identity proof:
`/tmp/opforge-first-use-tools-20261015/source-configuration-image-identity.json`.
The configured source export also matches the globally defined feature build
exactly (708,512-byte `fnv1a64:91050030ced3cf89`), preventing a disabled-feature
build from being mistaken for the experimental self-host.

The separate **full current-source self-host with first-use binding enabled**
passes: 209 files / 2,643,887 source bytes, one embedded 68020 package, no memory
instrumentation, fresh START/DONE and exit zero. The complete **708,512-byte**
native output equals both the freshly assembled Rust oracle and the actual
bootstrap executable (`fnv1a64:91050030ced3cf89`). Native START-to-DONE duration is
**107.971166 seconds**. This is its own input/configuration, not another sample of
the frozen-input optimization comparison. Source manifest:
`fnv1a64:dc77a9a564b1057c`.
Receipt: `/tmp/opforge-first-use-current-config-width-proof-20261015/result/native-result.json`.
Qualified export: `/tmp/opforge-first-use-current-config-width-20261015/manifest.json`.

The default eager configuration also completes a separate current-source
fixed-point self-host: fresh native output, fresh Rust output and its actual
bootstrap all equal the complete **707,204-byte** executable
(`fnv1a64:0c34b11b04c4bcb1`), with exit zero and fresh START/DONE in
**103.837460 seconds**. Its 209-file source capture includes two unreferenced
files; this count is not a claim that all files contribute linked code. It uses
the default build-configuration include, exercising the zero-width optional
fields on native as well as Rust. These different configured self-hosts are
correctness checks, not a matched-input performance comparison.
Receipt: `/tmp/opforge-first-use-current-eager-proof-20261015/result/native-result.json`.

FS-UAE uses the maintained existing A4000/68040 memory profile; this is not A6000
hardware or 2 MiB qualification.

Focused Rust oracles, asm Clippy with warnings denied, Rust/native formatting,
44 self-host-tool tests, 12 comparison-tool tests, five memory-decoder tests and
147 workflow tests pass. The focused redundant-test guard reports no blocking
findings in changed assembly; report-only ABI probes remain visible. A capture
summary unit test now supplies its own layout fixture rather than depending on
live conditional assembly syntax; its artifact-proof assertions and validators
are unchanged. Independent reviews found no concrete implementation defect.

## Template inventory contract

Hypothesis: a material part of definition-body binding and retained template
payload belongs to definitions that the eager preparation path never invokes.
Inventory those instances and their accepted consumers before choosing a deferred
preparation experiment. Do not change execution, skip work or release payload early.

The opt-in work observer records numeric `(session, pool, handle)` identities,
completed definition/default/spelling/body extents, accepted invocations, nested
and default consumers, and definition-context binding calls/committed bytes.
Consumer relationships group caller definition, callee definition and source origin,
with one incoming invocation-line exemplar. Lookups and preview reads are not
consumers; empty accepted invocations are. Calls in subsequently discarded blocks
still count as eager preparation consumers, not evidence of final reachability.

Four concurrent numeric owner slots preserve the configuration/replay overlap.
Each instrumented template state gains an eight-byte owner identity/source floor. Ordinary state
and execution order remain unchanged; no borrowed pointers survive a hook. Fixed
inventory limits are 2,048 registered instances and 4,096 consumer relationships.
Overflow, exhausted bounds, open definitions, unfinished owners and failed
reconciliation explicitly prevent a complete inventory claim.

Reconcile closed-instance payload and explicit orphan/shared residual against live
pool Used values before release. Body binding bytes are committed preparation
deltas; all live plan-arena bytes remain unattributed as retained storage. Do not
infer unique retained-plan ownership from those deltas. Source paths are borrowed
for optional terminal diagnostics; generated/nested records may inherit invocation
lines, so exemplars are not guaranteed original physical source coordinates.

Success requires native lifecycle/default/nested/statement/include controls,
register/CCR/stack and reset/bounds probes, unchanged ordinary images, full-input
Rust byte equality and complete conserved inventory. Measure incremental runtime
without memory telemetry against the preceding work observer, then measure heap
and linked reservation in a separate identical-input memory pair. Stop or revise
on unreliable attribution or disproportionate diagnostic overhead. There is no
deferred implementation or savings claim in this slice.

## Template inventory results

The full frozen input registers **1,361 definition instances**, all macros in replay
session 2; configuration session 1 registers none. Both sessions finish. There are
**594 accepted calls** (547 direct, 47 nested), 203 grouped consumer relationships,
and two default-consuming calls/slots. Every row is closed, every call reconciles,
and observer errors/unattributed definition binding are zero. These are session
instances, not distinct source-level macro names or final-output reachability.

| Accepted consumers in its session | Instances | Body binding calls | Committed bound-plan bytes | Template payload bytes |
|---|---:|---:|---:|---:|
| None | 1,173 | 16,351 | 2,459,016 | 556,225 |
| At least one | 188 | 2,472 | 362,812 | 84,469 |
| Total | 1,361 | 18,823 | 2,821,828 | 640,694 |

Never-invoked instances account for **86.87% of definition-body binding calls**,
**87.14% of their committed bytes**, and **86.82% of template payload**. Their calls
are only **3.84% of all 425,557 binding calls**. All belong to telemetry macro
sources, instantiated in their owning contexts:

| Definition source | Never-invoked instances | Body binding calls | Committed bound-plan bytes |
|---|---:|---:|---:|
| `memory_telemetry.i` | 835 | 12,428 | 1,899,452 |
| `telemetry_macros.i` | 213 | 2,346 | 331,760 |
| `hotspot_telemetry.i` | 53 | 628 | 88,760 |
| `capture_telemetry.i` | 39 | 522 | 75,060 |
| `binding_telemetry.i` | 33 | 427 | 63,984 |

All 640,694 template-payload bytes reconcile without residual. Never-invoked
payload comprises 215,832 definition-header bytes and 340,393 retained body bytes;
there are no default/spelling bytes in that group. Headers and binary recipes can
still be necessary for a deferred implementation. The **3,082,134 live plan-arena
bytes remain entirely unattributed as retained storage**. Committed work deltas
do not prove unique allocation ownership, removable bytes or elapsed-time savings.

### Separate identical-input runtime overhead

The final observer and preceding observer use identical audit flags/packages and
the same frozen 198-file input. Timing builds omit memory/reporting telemetry.
All six full-input comparisons complete with fresh START/DONE, exit zero and full
708,592-byte live Rust equality. The final memory pair also requires complete
inventory and conserved work accounting.

| Serial pair | Previous observer seconds | Template inventory seconds |
|---|---:|---:|
| Reference then inventory | 108.424515 | 107.154250 |
| Inventory then reference | 108.366648 | 107.618555 |
| Mean | 108.395581 | 107.386402 |

The measured difference is **−1.009179 seconds / −0.93%**. There is no observed
slowdown in these final samples; the small sample and variation across attempts
make this insufficient evidence of a speedup. No preparation work was removed.
The earlier capacity-exhausted observer attempt measured +1.10%; it is not the
qualified final inventory or another optimization result.

### Separate identical-input memory overhead

| Resource | Previous observer | Template inventory | Increase |
|---|---:|---:|---:|
| Tracked heap peak | 48,390,024 | 48,390,040 | 16 |
| Allocated/freed capacities, each | 113,993,896 | 113,993,912 | 16 |
| Linked reservation, memory image | 1,814,960 | 2,213,048 | 398,088 |
| Terminal tracked heap | 0 | 0 | 0 |

Both fresh memory runs exit zero with full live Rust equality and no profiling
errors/allocation failures. All 18 original global work totals are identical.
The inventory's fixed tables account for 394,440 bytes; linked overhead also
includes observer code/diagnostics. This is opt-in diagnostic cost, not a release
memory regression; linked plus tracked heap excludes OS/stack/allocator metadata.

The initial 1,024-row attempt exhausted capacity and was **not** a complete
inventory despite successful assembly. The final limit is 2,048; definition,
consumer and path report bands are disjoint. The runner now rejects incomplete
inventory explicitly. A 1,361-row fixture crosses the old limit and phase boundary.

### Qualification and evidence

Forty final fresh native control runs cover all 20 controls on the repaired work
and component-probe images, with complete live Rust BIN/Hunk equality. The prior
80-run matrix also covered ordinary and L1-only images. The repair only enlarges
and relocates opt-in diagnostic tables/report bands. Independent reviews found no
further concrete defect. Six focused Rust tests, Clippy with warnings denied,
Rust/native formatting, 48 Python tests and 147 workflow tests pass.
The focused redundant-test guard has zero blocking findings in changed assembly
files. Its broad repository run still flags 17 existing sites in unchanged files;
this checkpoint does not repair or claim that broad guard passes.

Final full-input receipts/summary:
`/tmp/opforge-template-inventory-full-v9-20261014/summary.json`.
Images: `/tmp/opforge-template-inventory-images-v9-20261014/images.json`;
native source identity `fnv1a64:f00fd63f6962945c`.
Final controls: `/tmp/opforge-template-inventory-controls-v9-20261014`.

The separate ordinary **full current-source self-host** completes in **102.877735
seconds**: 203 linked files / 2,625,156 source bytes, fresh START/DONE and zero exit,
with complete Rust and own-bootstrap equality for the **706,956-byte** Hunk
`fnv1a64:ba4df89ee2249d95`. It embeds only 68020 and enables no audit/memory flags;
the ordinary executable is unchanged from the preceding checkpoint. This qualifies
that default current-source build, separately from the frozen-input observer runs;
it is not an audit-enabled current-source self-host or A6000/2 MiB qualification.
Receipt: `/tmp/opforge-template-inventory-current-selfhost-20261014/manifest.json`;
source manifest `fnv1a64:fe2dd20e35f70198`.

### Why first-use binding was selected

The evidence supports a **generic first-use macro-body binding experiment**:
retain the packed definition recipe and its declaration context, defer body-plan
binding until the first accepted invocation, then reuse that result. Preserve
parameter/default semantics, lexical and conditional ownership, nested/include
behavior and existing error policies. Keep the eager path as the reference.
Do not select behavior by telemetry source names or benchmark identity.

Start with ordinary macros and explicit controls for unused, empty, repeated,
nested, shadowed and default-consuming calls. Carry declaration identities/context
without retaining borrowed views; unknown dependencies remain conservative. Count
actually avoided work and measure runtime and memory separately before expanding
to segment/statement templates. The ordinary-macro experiment above implements
this candidate; it does not extend deferral to those other template kinds.

## Measured reference and remaining uncertainty

The preceding work observer (`bbb2f648`) reconciles all 1,415 published whole units
with zero uncertainty/accounting errors on the frozen self-host workload: 198 files,
2,614,471 source bytes and 708,592 output bytes. Known unreferenced whole units
account for only 1,205 / 425,557 bindings (0.28%) and 52,628 ordinary capture bytes
(0.23%). This is a lower bound on explained unneeded work, not an upper bound on
all deferred-processing opportunity.

Captured-plan binding is concentrated in template-definition context: 18,823 /
20,731 calls (90.8%) and 2,821,828 / 3,082,134 committed bound-plan bytes (91.6%).
Those are work deltas, not uniquely owned retained allocations. Absence of an
invocation owner does not prove a definition is unneeded; the current inventory
investigates that distinction.

Reference tracked heap peak is 48,390,024 bytes; allocated/freed capacities are
113,993,896 bytes each. Capture uses 23,613,704 bytes in 33,255,424 reserved bytes.
This includes four-byte work-observer headers. The observer itself owns 689,440
fixed bytes before the new template inventory. All 164 regions become potentially
retireable by the end of replay, with no observed later-access contradiction;
last access alone does not establish safe consumer closure or peak savings.

Evidence: `/tmp/opforge-lazy-unknown-full-v6-20261014/summary.json` and
`/tmp/opforge-lazy-unknown-images-v6-20261014/images.json`. The preceding ordinary
current-source self-host separately completed with full Rust/own-bootstrap equality.
These measurements use the expanded-memory FS-UAE A4000/68040 profile: 2 MiB chip,
8 MiB fast, 64 MiB Zorro III and 16 MiB graphics. They do not qualify A6000 hardware
or the 2 MiB product target.

## L1 implementation and measured scope

`OPFORGE_LAZY_AUDIT` enables publication/lifetime observation; additionally setting
`OPFORGE_LAZY_WORK` enables whole-unit work attribution and the current template
inventory. Reporting requires the existing debug/memory/progress gates. Timing
builds omit those reporting gates; separate memory pairs enable identical gates.
Disabled macros emit no code, imports or references.

The [region observer](../../native/motorola68000/amigaos/debug/opforge_lazy_audit.asm)
records retained capture ranges, scheduled consumers, suspended include pins and
replay checkpoints. The [unit observer](../../native/motorola68000/amigaos/debug/opforge_lazy_units.asm)
reconciles authoritative block publication and selection, including preceding labels,
internal labels and ordered-copy offsets. The
[work observer](../../native/motorola68000/amigaos/debug/opforge_lazy_work.asm)
attributes work to those units and independently classifies definition/expansion/
ordinary call-time contexts. Tables store numeric identities and offsets.

Region cuts are allocation boundaries, not semantic unit boundaries. Whole-unit
attribution checks the global capture extent and all overlapping regions; close
before open, out-of-bounds and absent publication endpoints remain errors. Physical
and generated ranges must not double-own their caller's capture.

This remains publication-time evidence. Acquisition-only unit discovery and
individual binding/publication links are not implemented. Preparation with no
published block remains explicitly unassigned. Templates, constants, schema and
configuration need consumers separate from emitted block selection.

The existing legacy capture ambiguity for `.endmacro`/`.endsegment` parameter
operands remains a later opcore parity issue. A8 head classification covers
`.statement`/`.endstatement`; this audit does not replace the macro/segment grammar.

## What the current code establishes

| Boundary | Implemented behavior and implication |
|---|---|
| Acquisition | [frontend.captureLine](../../native/motorola68000/amigaos/experimental/binary_frontend.asm) tokenizes and captures candidate plans before whole-block selection. Deferring expression compilation during replay does not avoid this acquisition work. |
| Ordering | [configureCapturedModule/orderCapturedSources](../../native/motorola68000/amigaos/experimental/binary_source_schedule.i) revisit captures while discovering/configuring dependencies. File EOF is not their last use. |
| Replay | `replayCapturedSources` prepares graph-ordered spans, then releases all captures. [replaySourceSlice/materializePendingSource](../../native/motorola68000/amigaos/experimental/binary_source_cursor.i) can interrupt a parent cursor to acquire and replay an active include. |
| Binding completion | `frontend.complete` calls `scopes.finish`, then refreshes NameCount: final member acquisition can add identities. [imports.finish](../../native/motorola68000/amigaos/experimental/binary_imports.asm) can grow the entry arena and rewrite member/proxy targets. |
| Selection | [app.prepare](../../native/motorola68000/amigaos/experimental/binary_app.asm) completes binding and materialization before block indexing/selection. Unused block omission currently saves execution, not all preparation. |
| Frozen session | [assembly.execute](../../native/motorola68000/amigaos/experimental/binary_assembly.asm) pins record base/extent, context, symbol count/cells, member keys and module boundaries. `advance` requires the complete frozen frontier. [repeat_inventory.refresh](../../native/motorola68000/amigaos/experimental/binary_repeat_inventory.asm) rejects a changed symbol count. |

Existing [measurements](native-fragment-preparation.md#first-experiment-results)
show a 47,587,208-byte tracked heap peak on the later full input. The fragment
wrapper adds 2,122,664 transient allocated bytes and avoids no preparation.
The earlier [header comparison](native-performance-diagnosis.md#compact-retained-capture-headers)
has 31,862,784 bytes of retained capture capacity and substantial binding time.
These are different inputs/configurations, not a combined before/after pair.
They motivate this work but do not quantify how much preparation is unnecessary.

## Inventory: identity before activity

Keep four identities distinct:

| Identity | Meaning and lifetime |
|---|---|
| Source occurrence | Acquisition owner ID plus record offset, with physical origin. Stable through owner movement. Generated occurrences also identify the parent occurrence and invocation/iteration path. |
| Structural unit | Owning module/scope and complete block, template or control range. Includes the preceding entry label where applicable; internal labels remain part of the same block. |
| Binding occurrence | A use/declaration in a lexical and configuration context. It can refer to a potential owner without claiming that owner is active or its value exists. |
| Published symbol/site | The authoritative numeric result of shared binding and expansion. One source occurrence can produce several results; synthetic member identities need not have a standalone declaration line. |

Do not reserve every final runtime symbol ID from physical lines. Templates,
iterations, embedded substitutions and final member acquisition make that promise
false. Existing numeric IDs remain authoritative within the sealed session;
object-local remapping belongs to the later persistent-object work.

The structural inventory records bounded ranges, enclosing owners, declaration
candidates, control nesting, template boundaries, origin and configuration context.
It reads binary captures, never original source text. Reuse shared statement/loop/
file classifiers and scope services: `source_control.scanBody/classifyLoop/fileAction`,
`scopes.statementRole`, and frontend role callbacks. Extend a shared classifier
when information is missing; do not recognize directives with another native parser.
Lexical tokens can retain normalized spelling until binding; published execution
continues using numeric identities.

Candidate references inside an unresolved alternative are **potential dependencies**,
not live roots. A reserved identity does not grant executed visibility, value
availability, immutable certification or reachability. These observations stay
separate, preserving longest-owner lookup and conditional ownership.

Whole referenced `.block` units retain all internal/fallthrough code. Constants,
schemas, configuration values and template definitions can be required without
emitting code. Macros/segments are expansion templates, not block-pruning units.
Proven inactive semantic alternatives trigger no file acquisition, package
activation or live block roots. Inspecting their already captured structure does
not establish activity; unresolved alternatives retain the existing pending/eager
policy. Module dependency discovery is distinct from executing those alternatives.
Discovery must preserve existing structural/error policies;
deferred preparation must not silently accept previously rejected input.

## Publication: proposed pre-session contract

The first lazy candidate operates entirely **before** `beginSizing`. It builds one
complete selected prepared stream and uses the existing frozen assembly driver.
It does not relax a session check, append records during sizing, or use an invented
preparation PC to evaluate assembly-dependent guards.

These are proposed operations, not existing callable APIs:

| Operation | Required contract |
|---|---|
| Inventory append | Add complete occurrence/unit rows after validating bounds and nesting. IDs are append-only during acquisition; no partial row is visible after failure. |
| Request unit | Record the requesting context and reason: output root, block reference, value/schema need or template expansion. Do not execute it just because it was discovered. |
| Prepare closure | Use existing bind/EXVM/PRVM/template owners in dependency and source-effect order. Prepare whole requested blocks and needed helpers; unknown dependencies remain pending. |
| Publish batch | Stage complete records, binding results, typed owners, origins and section/context mappings. Publish their consistent extents together only after validation. Never expose half a record or a pointer into retired staging storage. |
| Seal | Complete imports/member plans, reconcile symbol count and owner inventory, export cells/typed values/keys, and materialize the final stream. Rebuild offset indexes against that stream before `beginSizing`. |
| Retire | Release an acquisition owner only after consumer closure; release staging owners after commit or abort. Preserve the previously published state on failed staging. |

Request order must not become language execution order. Mutable assignments,
configuration changes and other effects retain their graph-ordered/source-ordered
meaning. Shared evaluators remain authoritative. Preparation-known immutable
dependencies may be requested early without publishing them as executed values.

When activity depends on placement, instruction width, a mutable value or an
unavailable forward value, the first lazy candidate prepares that enclosing unit
through the reference path. This is conservative **extra preparation**, not
permission to select an inactive branch or retain extra output. Record the fallback
reason. Final reachability, guard evaluation and convergence remain unchanged.

Actual-state `.include` acquisition during sizing requires the separate
[growing source-session work](native-source-execution.md#assembly-session-and-stable-publication):
appendable declaration enrollment, stable consumed references, transactional
cell/view growth and replay observations. Allocation relocation alone does not
provide those semantics. Current supported preparation-time active/expanded
includes remain covered; the new scheduling must not introduce eager file loads.

## Lifetime: release owners, not convenient source boundaries

[capture.create/resolve/planView](../../native/motorola68000/amigaos/experimental/binary_capture.asm)
copy inputs into an owned record and return borrowed bounded views. The temporary
candidate arena is already released after construction; the reusable CaptureLine
is copied into a retained region. A region can contain several semantic spans or
only part of a large file, so neither file EOF nor block end establishes retirement.

| Owner | Consumers and release condition |
|---|---|
| Capture region | Configuration scans, scheduled replay spans, physical peek/seek, suspended parent cursors and pending body/template work. Retire only after all relevant ranges are consumed or copied to independent owners. |
| Region directory | Logical range lookup and saved region indices. Retain slot identity while consumers exist. A retired slot must explicitly reject resolution; a null arena with old Base/End is insufficient. |
| Template/plan storage | `macro_plans.bind` creates a session-owned plan; `templates.storeBody` copies records into its BODY pool. Verify retained recipes and views are independent before counting this as transfer of ownership. |
| Prepared records | Own numeric records/capsules through sizing, emission and output provenance. The frozen session borrows this allocation; it cannot move or grow until session release. |
| Binding/typed metadata | Preparation owns scope/proxy work; export copies required plans, parameters, seeds and member keys before frontend retirement. Inactive metadata must not publish inactive symbols. |
| Origin paths/output provenance | Keep the diagnostic/listing information required by prepared records independently of capture payloads. Release after their final output/diagnostic consumers. |

Use owner ID and offsets in retained structures. Native allocation descriptors
may hold process pointers; cached views are reconstructed for bounded calls and
must not survive owner movement or retirement.

At first, use last-consumer positions for graph-ordered spans plus explicit pins
for suspended include cursors and retained/deferred ranges. Avoid a per-token
reference-counting system. Newly acquired include regions join the same accounting.
If a region mixes live and consumed ranges, retain it; copying retained subranges
into smaller owners is a later, separately measured option.

On failure, discard unpublished staged owners and keep prior published state
intact. Cleanup releases remaining live owners exactly once, including partially
acquired includes. Diagnostic origin survives long enough to report the failure.

## Choosing the optimization after L1

- **L2 lifetime candidate:** if closure is established, release consumed regions
  after their last replay consumer. This may reduce overlap/late live storage;
  acquisition peak can remain unchanged. Keep tombstones and failure cleanup valid.
- **L2 deferred-work candidate:** if inventory is sufficient and unused-body work
  is substantial, defer one class of plan/binding/expression preparation for complete
  unrequested blocks. Keep uncertain units eager and count the actual avoided calls
  and bytes. Avoiding replay compilation alone does not avoid captured plans.

Choose one candidate first from observed opportunity, not both in one comparison.
Preserve the working reference. Compare identical source/package/configuration
inputs with release timings and separate ownership measurements, then perform a
fresh full current-source self-host. Report each change's time and memory impact
independently. Package demand loading and persistent `.ofm` remain later stages.

## Validation and reproduction

The maintained controls compare reference, L1-only, work and component-probe images
against live Rust BIN/Hunk oracles. They cover guard outcomes, whole blocks,
preceding/internal labels, imports/forward members, generated names, loop shadowing,
schemas/configuration, empty/default/nested templates and include suspension/resumption.
Component probes check register/CCR/stack preservation, reset, overflow/capacity,
conservation, owner overlap and cross-region bounds. Any incomplete attribution
remains visible; it never changes eager execution.

The [comparison runner](../../scripts/performance/run_lazy_preparation_comparison.py)
checks file/image digests, actual defines, fresh completion and complete Rust output.
Its [decoder](../../scripts/performance/lazy_preparation_audit.py) reports inventory
coverage independently of native completion and memory measurements. A frozen-input
comparison is not a current-source self-host qualification; perform that separately.

```sh
OPFORGE_LAZY_SAVE_DIR=/tmp/new-images OPFORGE_LAZY_SAVE_PROBE=1 \
  cargo test -p asm native_lazy_audit_save_images -- --ignored --nocapture --test-threads=1

python3 scripts/performance/run_lazy_preparation_comparison.py \
  --work-attribution --baseline-images /absolute/previous-work-images \
  --workload-case /absolute/frozen-full-case/case.json \
  --images /tmp/new-images --output /tmp/new-results \
  --test-binary /absolute/asm-tests

# Acquisition inventory: compare two eager executables, adding only the new audit.
OPFORGE_LAZY_SAVE_ROLES=work OPFORGE_LAZY_SAVE_DIR=/tmp/prior-observer \
  cargo test -p asm native_lazy_audit_save_images -- --ignored --nocapture --test-threads=1

OPFORGE_LAZY_SAVE_ROLES=work OPFORGE_LAZY_SAVE_DIR=/tmp/candidate-observer \
  OPFORGE_LAZY_SAVE_CANDIDATES=1 OPFORGE_LAZY_SAVE_CANDIDATE_TIMING=1 \
  cargo test -p asm native_lazy_audit_save_images -- --ignored --nocapture --test-threads=1

python3 scripts/performance/run_lazy_preparation_comparison.py \
  --candidate-attribution --candidate-timing --work-attribution \
  --baseline-images /tmp/prior-observer --images /tmp/candidate-observer \
  --workload-case /absolute/frozen-full-case/case.json \
  --output /tmp/candidate-results --test-binary /absolute/asm-tests

OPFORGE_DEFERRED_SAVE_DIR=/tmp/new-first-use-images OPFORGE_DEFERRED_SAVE_PROBE=1 \
  cargo test -p asm native_deferred_macro_save_images -- --ignored --nocapture --test-threads=1

python3 scripts/performance/run_lazy_preparation_comparison.py \
  --experiment-define OPFORGE_DEFERRED_MACRO_PLANS \
  --workload-case /absolute/frozen-full-case/case.json \
  --images /tmp/new-first-use-images --output /tmp/new-first-use-results \
  --test-binary /absolute/asm-tests

# Incremental call-plan construction: both images enable the earlier interface
# and binding features; only OPFORGE_DEFERRED_MACRO_CANDIDATES differs.
OPFORGE_DEFERRED_SAVE_DIR=/tmp/new-acquisition-images \
  cargo test -p asm native_deferred_candidate_save_images -- --ignored --nocapture --test-threads=1

python3 scripts/performance/run_lazy_preparation_comparison.py \
  --experiment-define OPFORGE_DEFERRED_MACRO_CANDIDATES \
  --workload-case /absolute/frozen-full-case/case.json \
  --images /tmp/new-acquisition-images --output /tmp/new-acquisition-results \
  --test-binary /absolute/asm-tests --stop-on-failure

```

For a sampling sensitivity check, set builder variables
`OPFORGE_LAZY_SAVE_CANDIDATE_PERIOD=128` and
`OPFORGE_LAZY_SAVE_CANDIDATE_SEED=37`, with matching runner arguments
`--candidate-sample-period 128 --candidate-phase-seed 37`. Only the memory image
gets the numeric definitions; release images retain count-only behavior.
`OPFORGE_LAZY_SAVE_MEMORY_ONLY=1` can save only memory images for a standalone
case. The measured second profile uses its saved complete `case.json` directly
with the maintained `native_full_selfhost_comparison_fs_uae` test and
`OPFORGE_FULL_COMPARISON_CASE`; it is not another release timing sample.

For a separate current-source experimental self-host, set
`OPFORGE_COMPACT_EXPORT_DEFERRED_MACRO_PLANS=1`,
`OPFORGE_COMPACT_EXPORT_OUTPUT_EMBED=68020`, `OPFORGE_COMPACT_EXPORT_NATIVE=1`
and a fresh absolute `OPFORGE_COMPACT_EXPORT_DIR`, then run the ignored
`export_compact_self_host_bundle` test. Its oracle, bootstrap, relocated-source
check and guest command all use the same configured source. The exporter copies
the native source into a temporary build root and generates
`experimental/build_configuration.i`, defining `DEFER_MACRO_PLANS = 1` and explicitly disabling
`BINARY_MACRO_VIEW` and `ACQUIRE_MACRO_CANDIDATES`. The repository include maps the Rust preprocessor
switch to that constant, selecting zero by default. The
generated origin and configuration are explicit in the manifest; no native `-D`
option is needed or added. Hardware qualification remains a separate case.

Use the maintained FS-UAE environment and preserve evidence/tools outside `target`.
Do not overlap heavy host work with reported runtime samples. Compare release
runtime and separate tracked-heap/linked-reservation pairs on identical inputs.
A6000 is unavailable for this checkpoint.
