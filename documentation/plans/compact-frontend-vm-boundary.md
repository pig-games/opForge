# Compact frontend: VM boundary correction

Status: numeric normalization, composed-name recipes and the PRVM boundary/resume
foundation, macro descriptor services, compact descriptor storage integration,
ordinary macro/segment string fragment recipes, and generated-call argument
re-tokenization are implemented. The residual decoded-string fallback and
expression correction remain active. Counted packed `.for` replay now passes
focused real-native comparison; iterable `.for` and `.bfor` remain unsupported.
The remaining frontend boundary work and next self-host frontier are tracked
alongside the [native reset](native-runtime-reset.md#fixed-input-allocation-slice).
Further performance shortcuts are deferred until completed native self-host proof.

## Finding

The experimental compact CLI uses TKVM for initial tokenization and ExprVM for
expression evaluation, but does not make its entire parsing path VM-controlled.
Correct output on tested inputs does not prove the architectural boundary. This
was understated when preparation was described as just binary lowering.

The [canonical boundary](../vm-boundary-protocol-v1.md#3-canonical-boundary-matrix)
permits host-owned bootstrap and macro expansion. It assigns line tokenization,
statement/operand parsing and covered mathematical expression parsing to the VM.
[Package-controlled execution](package-execution-boundaries.md#outcome-and-scope)
allows substantial shared primitives: every character operation need not become a
small bytecode. The issue is who selects the grammar and owns its contract, not
whether a native routine contains branches.

## Inspected responsibilities

| Component | Current behavior | Assessment |
|---|---|---|
| `binary_frontend.line` | Runs TKVM, then calls the writer and preparation machinery. | Real VM tokenization, followed by additional native grammar. |
| TKVM `ComposeNames` / writer recipe branch | Package-selected placeholder policy produces bounded composed-name recipes. The writer validates extents and copies them. | VM-controlled lexical recognition; the three superseded writer parsers are removed. |
| TKVM `NormalizeNumbers` / writer numeric branch | Package-selected spelling rules produce an unsigned 64-bit value or deferred invalid/overflow metadata. The writer copies values that fit its existing u32 representation. | Literal normalization is VM-controlled; the superseded `binary_source.parseNumber` is removed. |
| `binary_source.literalString` | Copies bytes already decoded by TKVM. | Appropriate packing; no duplicated escape parser. |
| `binary_source.nameOperand` and binder | Classify the package-owned numeric-looking `.cpu` name and resolve identifiers to IDs. | Context and binding are necessary, but normalized-token changes must preserve this name/value distinction. |
| `binary_source.appendPlan` / `binary_macro_plans` | Copies VM-selected descriptor/spelling regions and appends an offset handle. | The raw call-region sidecar is removed; captured invocation plans support the retained spelling consumers; bound core directives do not receive invocation plans. |
| `binary_templates.rewriteCallText` | Still consumes decoded-string bytes in the residual body-token fallback. Captured generated calls now bypass it. | Explicitly unfinished until the remaining caller is replaced; scratch is not persisted as executable source. |
| `binary_templates.expandComposite` | Joins literal/argument spelling fragments and binds the generated name, or emits string bytes. | Generated names need spelling during preparation; this need not be source reparsing if recipes are explicit and already recognized. |
| `binary_expression.compile` | Implements precedence and associativity via `bitOr`, `product`, `power`, `unary`, `primary`; emits and folds expression bytecode. | A native mathematical parser outside the EXVM parser contract. |
| `binary_expression.evaluate` | Executes the compact expression through shared ExprVM. | VM evaluation; it does not establish VM-owned compilation. |
| `binary_prepare` | Recognizes statement and operand wrappers and chooses expression ranges. | Needs a separate PRVM/package-boundary audit when migrating expression compilation; not presumed correct from token IDs or package register IDs alone. |

Source anchors: [writer](../../native/motorola68000/amigaos/experimental/binary_source.asm),
[frontend](../../native/motorola68000/amigaos/experimental/binary_frontend.asm),
[templates](../../native/motorola68000/amigaos/experimental/binary_templates.asm),
[expressions](../../native/motorola68000/amigaos/experimental/binary_expression.asm),
[preparation](../../native/motorola68000/amigaos/experimental/binary_prepare.asm).
This is a bounded frontend audit, not an exhaustive encoding or language audit.

## Remaining lexical boundary

Rust `PortableTokenKind::Number` now carries optional normalized u64 metadata.
Native TKVM retains its 20-byte records, using the former reserved word for
numeric status and offsets into scratch for spelling followed by an eight-byte
value. Package opcode `0x13` selects normalization and its ordered radix rules.
The scanners remain deliberately permissive. Invalid and overflow metadata are
deferred until a value is required, preserving numeric-looking names and macro
fragments. Strings already carry decoded bytes. Composed-name and ordinary
macro/segment body string recipes are now VM metadata. Captured generated calls
also re-tokenize VM-bound fragments; the residual decoded-string path still
needs migration. The ordinary Rust expression path still uses core token
spelling; it does not yet consume the portable numeric metadata.

Default identifier continuation includes `@`, so `label@1` can be one identifier;
`@1suffix` can be `At` plus the permissive number spelling `1suffix`. Neither
represents an explicit substitution recipe. Normalization must distinguish these
forms before rejecting them as malformed ordinary numeric literals. It must also
preserve numeric-looking package names and ordinary numbers on the same line.

References: [portable tokens](../../crates/opforge-vm/src/portable_contract.rs),
[Rust scanners](../../crates/opforge-vm/src/tokenizer_runtime_utils.rs),
[native scanners](../../native/motorola68000/amigaos/tkvm/tkvm_scanner.asm),
[program generation](../../crates/opforge-vm/src/builder.rs),
[opcode contract](../../crates/opforge-package/src/package.rs).
The compact BSP4 preparation capsule embeds TKVM and macro descriptor programs; it does not embed an
EXVM expression-parser program. Existing canonical expression bytecode produced
by its native compiler must not be confused with that missing parser program.

## Proposed sequence

1. **Normalized lexical contract.** Define and implement VM-controlled literal
   normalization and explicit placeholder/composite lexical forms in Rust and
   native together, with package generation selecting the operation and policy.
   Preserve decoded strings, spans, numeric-looking names, macro syntax and the
   canonical scalar value domain. The current packed u32 field is a representation
   constraint, not permission to narrow the language globally. Begin with the
   numeric-value part as a bounded implementation checkpoint, accounting for
   placeholder/name contexts before conversion. The writer then packs values and
   binds names; remove its superseded literal parser when that seam is proven.
2. **Binary substitution recipes.** Finish explicit recipes for positional,
   named, full-list and embedded substitutions. Preserve observable spelling where
   textual substitution requires it, as bounded literal fragments owned by the
   binary format. Remove the raw call-region sidecar and its character-scanning
   substitution path. Retain generated-name interning, template storage and
   invocation state on the host. Resolve context-sensitive placeholders through
   the parser/macro contract rather than a context-blind tokenizer rule.
3. **VM-controlled expression compilation.** Select the covered grammar through
   EXVM/package contracts over packed tokens; retain compilation-once, folding and
   compact runtime evaluation. Audit the PRVM expression-range/operand wrapper
   seam with it. A new shared primitive is acceptable if its contract is explicit
   and package-selected; merely moving the recursive parser into a VM-named file
   or wrapping an uncontrolled callback in an opcode is not a correction.
4. Resume loop parity on explicit binary records with these boundaries recorded.
   Do not extend the current ad hoc text grammar to get past the self-host frontier.

For each implementation checkpoint compare Rust/native normalized records,
acceptance, diagnostics and final bytes on identical inputs; include malformed
literals, adjacency, quoted/embedded placeholders, name/value contexts and exact
expression precedence as relevant. Run bounded fresh native proofs under 68020 /
2 MiB and the unchanged release control. Record image, package, peak ownership and
unprofiled timing deltas. Stop and discuss a change that requires a larger grammar
or storage redesign rather than hiding it in a local helper.

Before 1.0 migrate the latest affected contracts and consumers together; do not
add legacy executors. Packed records contain offsets, not memory pointers.
The initial audit changed documentation only. The numeric checkpoint below
changes the latest TKVM contract and both consumers together; it introduces no
legacy executor. Explicit composite recipes and VM-controlled expression
compilation remain active work, rather than implied completion of the boundary.


## Numeric normalization checkpoint

TKVM opcode `0x13` selects an ordered package table of prefixes, suffixes,
radices and terminal-body flags. Both interpreters validate the table and produce
checked unsigned 64-bit values or deferred invalid/overflow metadata. Native
records retain their 20-byte layout; values occupy eight additional scratch bytes
following a copied spelling, reached through offsets. The writer packs normalized
values and enforces its existing u32 limit. It no longer interprets literal text.
The default fast Rust tokenizer uses the same operation and matches generic
execution including logical step accounting.

Fresh 68020 / 2 MiB native proof matches 24 live generic Rust numeric records,
including u64 limits, overflow, separators, alternate spellings and overlapping
prefix/suffix rejection. Mixed compact assembly matches all 44 output bytes.
A 14-byte scratch probe on `1 2` returns status 3, cursor 2, committed extent 11,
first value 1 and a still-raw second record. An independent Sol review found and
helped repair terminal-rule precedence and capacity-failure publication; final
review found no remaining actionable issue.

Reusable telemetry advances to MEM6: 20 opcode counters and 400 adjacent pairs
in a 1,916-byte record, 160 bytes more than MEM5 in instrumented builds only.
The positive mixed proof reconciles opcode/pair counts and reports zero profiling
errors and zero live tracked ownership after cleanup. Normalization contributes
to helper time; it does not inflate committed token or spelling-work counters.
Release builds retain no telemetry code or storage.

VM library tests pass 438/438 and package library tests 101/101. A broader
assembler library run reports 1,764 passed, 61 failed and 214 ignored; all 61
failures reproduce with the pre-change test executable. This is not a full
repository qualification claim. The generated default package also had existing
1,521-byte drift before this slice; refreshing it incorporates that drift plus
260 bytes for the four normalization programs. The smoke package grows by 65
bytes. No unrelated example or output goldens were regenerated.

Caller storage now reserves the previous spelling budget plus worst-case copied
number spellings and eight value bytes per token. Compact scratch grows from
1,024 to 2,560 bytes (+1,536 required scratch extent). This remains within
the existing geometric arena allocation on the measured alias cases. Each of
TKPKG's two shared scratch buffers grow from 256 to 1,024 bytes (+1,536 bytes combined for consumers
that link those buffers). The compact release linked reservation above does not
grow from this buffer change. Consumers use the same named bounds; the shared rejection buffer must grow with its bound.
This is provisional storage, not a claim of optimal numeric packing.

The unchanged unprofiled release control (84,687 source bytes, 121 templates)
produces all 1,701 Rust-identical bytes in 9.116 s, versus 8.862 s previously:
an observed 2.9% increase in single runs, not a statistical regression estimate.
The release Hunk grows from 71,712 to 72,344 bytes (+632), with linked reservation
from 82,936 to 83,560 (+624). Its m68020 capsule grows from 269,162 to 269,226
bytes (+64). Full-width literal native output also matches all 60 bytes.

Focused final host checks verify both generated package fixtures, numeric oracles,
numeric CPU aliases and release-transparent telemetry. The digest pin matches
the refreshed live package; its existing combined source-contract test then
fails at an obsolete compact-table version assertion. That source-contract
issue remains in the baseline failure set. Native proof and instrumentation
guards, their nine Python tests and the linked-source formatter pass. Workflow
links, selector and supply-chain checks pass; the architecture gate retains its
ten existing enforced findings, 25 enforced warnings and 565 outside warnings.

Final numeric-alias runs match Rust on both CPUs with zero live tracked ownership
and zero profiling errors. Peak ownership is 531,888 bytes for m68020 and
153,048 for m6502: 64 bytes above the identical allocation-checkpoint inputs,
matching capsule growth. The larger scratch extent stays inside existing arena
capacity on these cases. The fresh original 68020 / 2 MiB self-host probe still
rejects at `binary_source.asm` line 234, the same `.for 4` construct previously at
line 224. Peak tracked ownership is 1,011,120 bytes, with balanced cleanup and
unfinished-interval flag 16. No complete self-host artifact or duration is claimed.

A proposed 20-literal `.long` stress line rejected because `appendCallText`
already exports all leading dot-statement argument text and caps it at 251 bytes.
That source limit is distinct from numeric scratch capacity and remains part of
the binary-recipe correction. The scratch proof instead uses an assignment with
a long, zero-padded binary spelling and emits its value through a short `.long`.

That focused assignment proof passes freshly on native: the binary literal has
512 leading zero digits and 32 one digits, normalizes to u32 max, and emits four
`FF` bytes matching Rust. Its numeric spelling fits the original 1,024-byte
budget while its copied spelling and metadata require the expanded capacity.
Cleanup and telemetry reconciliation pass with zero profiling errors.

## Composed-name recipe checkpoint

This bounded part of the binary-substitution slice moves composed identifier
recognition into package-selected TKVM opcode `0x14`. It preserves lexical tokens
and adds explicit fragment recipes; the writer copies them and binds generated
names. Initial qualified name prefixes remain lexical content, while the package
selects positional markers/range and allowed suffix bytes. Bare positional tokens
and decoded strings retain their existing forms.

Hypothesis: this removes the writer's lexical grammar without losing observable
macro behavior or materially increasing native preparation cost. The comparison
baseline is `b36a3a7c`: unchanged unprofiled release control 9.116 s, Hunk 72,344
bytes, linked reservation 83,560 bytes, capsule 269,226 bytes. Correctness requires
live generic Rust/native recipe comparison, existing embedded/default/nested macro
output comparisons, and fast/generic Rust equivalence. Malformed policy, adjacency,
qualified prefixes and capacity publication need explicit checks.

Success means the writer's `leadingComposite`, `identifierComposite` and suffix
validator are gone and fresh native proofs pass. Stop for a consequential grammar
or storage redesign, or an unexplained output/performance regression. This
checkpoint deliberately retains `appendCallText`, `captureCallText` and
`rewriteCallText`; argument ranges and call/string recipes must replace those
together in the next checkpoint. Native expression compilation remains later work.


The package supplies opcode `0x14` marker/range/suffix operands; native and Rust
attach explicit recipes while preserving lexical records. The writer's three
composed-name recognition routines are removed. Initial qualified prefixes stay
verbatim. An independent Sol review found and resolved invalid-run traversal:
both executors now annotate only the attempted run's head and skip its extent.
The native number scanner also had a pre-existing continuation mismatch: `$`,
`%` and `@` were accepted inside number bodies. Bodies now match Rust's ASCII
alphanumeric/underscore rule; leading numeric-prefix handling remains separate.

Fresh native proof matches 24 live generic Rust recipe records, including custom
marker/range/suffix policy, qualified prefixes, multiple placeholders, whitespace
and malformed composition. Four additional token-count/kind comparisons cover
numeric boundaries. A 14-byte scratch probe rejects `name@1` with status 3,
cursor zero, committed extent six and no published recipe. The first capacity
comparison had its expected reporting blocks in the wrong order; the corrected
fresh comparison passes without changing runtime behavior.

Native policy validation and membership use a local 32-byte bitmap instead of
quadratic uniqueness scans. Caller scratch grows from 2,560 to 5,888 bytes in the
compact frontend, reserving copied spelling, literal-fragment headers and sidecar
headers conservatively. Each shared TKPKG scratch buffer grows from 1,024 to
2,048 bytes. These bounds are provisional; they do not claim optimal storage.
Reusable telemetry advances to MEM7: 21 opcode counters and 441 adjacent pairs
in 2,084 bytes, a 168-byte instrumented-only increase. Release telemetry remains
absent. The four default package programs grow by 272 bytes total; the smoke
fixture grows by 68 bytes. No example output goldens changed.

Host checks pass 440 VM tests, 101 package tests, 111 affected binary-source checks
and both live generated-package fixture comparisons. The final invalid-run fix
also passes its all-original-token regression. Native proof/instrumentation
checks and their nine tests pass. A broader 36-test native-guard selection has
34 passes and two failures on unchanged files: the existing value-execution
inventory hash and missing compact-CLI owner annotation. This is a checkpoint,
not full repository qualification.


The unchanged unprofiled release control (m6502, 84,687 source bytes, 121 templates)
passes all 1,701 output bytes in 9.887 s, versus 9.116 s at `b36a3a7c`: an observed
8.5% increase in single runs, not a statistical estimate. This checkpoint corrects
grammar ownership and adds a lexical pass; it is not a speedup claim. The release
Hunk is 72,620 bytes (+276), with linked reservation 83,828 bytes (+268).
Fresh native embedded/default/multiple-placeholder macro comparisons pass four
inputs, and all 24 numeric normalization records still match live generic Rust.

Fresh 68020 / 2 MiB telemetry comparisons on the unchanged CPU-alias inputs
report peak tracked ownership 531,952 bytes (m68020) and 153,112 bytes (m6502),
both +64 bytes. Cleanup is balanced, live ownership returns to zero and profiling
errors are zero. Opcode/pair counts reconcile, including seven ComposeNames
executions per input. Capsule sizes are 269,294 and 11,032 bytes respectively.
The expanded scratch fits the existing arena allocation on these inputs; this
is not a worst-case whole-program memory proof. Full self-host remains at the
previous `.for 4` frontier and was not rerun for this lexical checkpoint.

The linked native formatter checks 259 files with no changes or warnings.
Independent final review reports no remaining actionable finding. Remaining work
is explicit argument ranges and call/string substitution recipes, followed by
VM-controlled expression compilation and the preparation-boundary audit.

## Call and string recipe migration: design checkpoint

The next correction needs an explicit parser contract and a new latest compact
capsule/record layout. It cannot be an enlarged token-42 trailer: packed lines
must remain at most 256 bytes, and current trailers already duplicate spelling
and impose a 251-byte limit even on ordinary directives. Do not put argument or
header parsing into TKVM merely because BSP3 currently carries only that program.

### Proposed first implementation checkpoint

Embed a package-selected macro frontend PRVM program in the compact capsule.
Its input is the initial lexical records and source spans, plus binding identities
for core keywords and known templates. The VM recognizes the call/header shape,
optional outer parentheses, balanced argument boundaries, optional parameter
types and default boundaries. Host code resolves names, owns storage and manages
invocations; it does not choose delimiter or placeholder grammar.

Reuse the existing PRVM request/result contract with an explicit macro entry;
keep the OPASM statement-entry guard intact. The native opcode `0x41`
(`ScanTopLevelCommaBoundaries`) currently does no work, while `0x50` stops at
the first comma without nesting checks. Implement the shared boundary primitive
before using it for descriptors. Rust's boundary scan currently reaches the end
of the input: add a VM-owned active range, defaulting to the full token array,
which macro-envelope recognition narrows to the argument region. Preserve the
current Rust delimiter acceptance rather than accidentally tightening it.

Treat that prerequisite as a coherent PRVM state/consumer checkpoint, not an
isolated opcode patch. Scan activation, origin and end must survive cursor changes,
checkpoint/rollback and expression resumes. The existing 40-byte native resume
record has no spare fields: migrate the latest resume contract with its consumers
rather than hiding state in unrelated fields. Use one boundary walker with Rust's
three signed delimiter depths, including its current unmatched-close behavior.
Dynamic operand parsing without a scan produces no operands. The first empty
range supplies numeric zero; later empty ranges supply an expression error and
stop. Repair the Rust/native request bridges coherently where necessary.

Focused acceptance includes nested delimiters, leading/consecutive/trailing commas,
scan plus cursor movement, repeated scan, rollback, multiple resumes and error
stopping. Extend the existing real PRVM smoke harness for native proof. This
prerequisite does not yet introduce the macro entry or descriptor arena.

Emit descriptor events through the existing 32-byte result records. Define the
new event kinds coherently in Rust, native and documentation; existing record-kind
documentation already disagrees with native codes 6/7. Hosts validate events and
copy VM-selected spans; they do not rescan delimiters, whitespace or `=`.

Produce offset-only descriptors in an owned companion arena:

- Line plan: role, head token range, argument descriptor range and supplied-list
  spelling/formatting recipe handle.
- Argument: binary token range and spelling recipe handle.
- Formal: name/type identity, optional default token range and default spelling
  recipe handle.
- Spelling recipe: literal spans and explicit separator/whitespace spans. Individual
  arguments trim as Rust does; the full supplied list keeps spacing and excludes
  defaults.

Packed lines contain typed handles into that arena rather than copied call text.
All spans/handles are offsets within validated regions, never memory pointers.
The latest capsule replaces BSP3 when this descriptor contract is implemented;
there is no legacy executor. One spelling arena should replace duplicated trimmed
argument/full-list buffers. Template/default pools and nested frames refer to it
through validated offsets with explicit ownership and cleanup.

This checkpoint removes writer `appendCallText`, template `captureCallText` and
the header-default `=` scanner. The remaining substitution consumer is explicitly
unfinished until the next checkpoint; retaining it temporarily must not be described
as binary-only expansion or a complete boundary correction.

### Substitution ordering needs an explicit contract

The authoritative Rust processor recognizes substitutions on original spelling
before tokenization/string decoding. Native currently feeds already decoded
string bytes into `rewriteCallText`. These orders are observably different:

| Template / argument | Rust result | Fresh native result after module fix |
|---|---|---|
| `.byte "\x401"` / `A` | Literal bytes `@1` | `41` (`A`): decoding introduces a marker that is then substituted. |
| `.byte "\x2ename"` / `A` | Literal bytes `.name` | `41` (`A`): decoding introduces a named marker that is then substituted. |
| `.byte "@1"` / `A",7,"B` | Bytes `41 07 42` from three expressions | Bytes `41 22 2c 37 2c 22 42`: inserted spelling remains inside one string. |

Live full Rust CLI probes with an explicit `.module app` verify all three Rust
results. Probe filenames must not supply an invalid implicit module name; initial
hyphenated filenames caused unrelated module errors and were corrected rather
than interpreted as macro behavior. The committed test inputs use `input.asm`
and an explicit module. Initial fresh native runs completed with empty output on
all three inputs and on ordinary literal, substitution and numeric controls.
An implicit-module control passed, isolating an explicit-module selection defect:
block selection treated a source module binding as a dense graph-node index.
It now resolves the binding through the graph's existing hash lookup, preserving
the distinct binding used by import selection. This restores the ordinary controls
and exposes the actual substitution-order mismatches; it does not repair those
semantics. No missing `.org` / `.end` or forbidden macro-name explanation was
supported by the source or comparisons.

Escaped markers must stay literal whichever interpolation policy is chosen.
Preserving all current Rust behavior requires a package VM lexer/decoder over
binary fragment streams when substitution changes quote/escape/token structure.
There must be no rendered-source buffer handed back to host parsers. Ordinary
shape-stable substitutions should keep the token-splicing path; any bypass of
fragment lexing needs VM-owned eligibility and equivalence tests against forced
fragment execution on identical inputs. This avoids recreating an unproven fast
path. Erik chose to preserve the existing substitution semantics for now,
including substitutions that change token/quote structure. Implement the fragment
stream contract; do not reject those substitutions or narrow the Rust language.

### Subsequent implementation checkpoint

Compile positional, named and full-list references into explicit recipes while
original spans are available. Resolve named references to formal identities once;
unknown named references retain exact literal fallback. Host expansion copies or
splices binary ranges. It does not recognize sigils, resplit commas, scan `=` or
revisit original source. Strings and nested calls carry domain-tagged recipes;
introduced escape/quote boundaries follow the agreed package VM contract.
Remove `rewriteCallText` and the fake-sidecar path in `expandStringToken` only
when the complete nested/default/string comparisons pass.

Compare identical inputs against the live Rust CLI, including escaped markers,
quoted arguments, delimiter combinations, defaults, full-list spacing and nested
substitutions. Include exact native recipe/descriptor bounds and failed-publication
probes, fresh 68020 / 2 MiB output, the unchanged release control and peak ownership.
Baseline before the module-owner repair is `bfb4bc91`: release control 9.887 s, Hunk 72,620 bytes,
linked reservation 83,828 bytes; alias peaks 531,952 / 153,112 bytes.

### Design-checkpoint validation

The Rust regression test passes all six cases (three ordering cases and three
ordinary controls); the affected graph selection passes 12 host tests. Fresh
68020 / 2 MiB ordinary native controls pass after the module-owner fix. The final
graph lookup wrapper passes entry-root, reachability and transitive native cases,
the previous root-macro fixture and the minimal m68020 explicit-module regression.
The three ordering cases fail with the exact bytes shown above; their correction
belongs to the fragment-stream migration. Native formatting checks all 259 files
without changes/warnings, and the fresh-run proof-contract check passes. Focused
Rust formatting and diff whitespace checks pass. Workflow links, benchmark
selectors and the supply-chain check pass;
the architecture boundary check reports ten findings in unchanged native
encoding/mask files. Whole-workspace formatting also reports an existing
module-order difference in unchanged `crates/opforge-vm/src/lib.rs`.

The unchanged unprofiled m6502 release control (84,687 source bytes / 121 templates)
matches all 1,701 Rust output bytes in 9.780 s, compared with 9.887 s before this
repair. That is a 1.1% lower single-run observation, not a statistical speedup
claim. The compact Hunk is 72,676 bytes (+56), with linked reservation 83,880
bytes (+52). Allocation telemetry was not rerun for this lookup-only repair.


## PRVM boundary and resume foundation

The prerequisite now implements native `0x41` scan activation and nested dynamic
`0x50` range selection. It preserves Rust's three signed delimiter depths,
including unmatched-close behavior. Dynamic parsing uses the saved scan rather
than the current token cursor; repeated parsing restarts the scan. Checkpoints
and expression resumes preserve loaded/label metadata, predicates, scan state and
the cumulative step budget. A ready expression error stops that parsing invocation.

Expression requests advance to version 2 with an explicit static/dynamic range
mode. Only the first empty dynamic range becomes numeric zero; other empty ranges
remain expression errors. The scan ordinal is independent of accumulated result
slots. Native resume version 2 stores offsets and values only, with a 468-byte
record (+428) and a 428-byte runtime local frame (+244). All current callers use
the shared runtime size symbol; there is no legacy resume executor.

The host bridge tests compare ASTs against live Rust parsing. The native smoke
checks exact request ranges, ordinals, cursor and result counts, pause/resume,
rollback and malformed state rejection. These are primitive/service-contract
proofs. The existing full native CLI service still supplies opaque expression
slots for downstream parsing; this slice does not establish full AST parity or
remove its text parser. The compact macro frontend does not yet consume PRVM,
so no integrated assembler speedup is claimed and its unchanged release benchmark
was not rerun.

A blocking Rust Hunk proof bug is repaired: `.fill` counts control allocation,
while the repeated value supplies emitted bytes. Exported computed counts now
retain literal-data proof; symbolic address values still fail proof, and invalid
counts still fail without emitting bytes.

One language gap remains explicit: a harness operand such as
`move.l #5, runtime.PRVM_RESUME_LOCAL_STATE + runtime.LOCAL_CHECKPOINT_DEPTH(a4)`
causes lockstep AST span divergence (reference tuple starts at column 13; VM
starts at column 45). A named constant for that displacement keeps the harness
usable. This does not repair the general displacement-expression mismatch.

The next coherent slice remains package-selected macro descriptors and argument
ranges, followed by VM fragment streams that preserve the three recorded macro
substitution-order cases. This foundation does not change those cases.


Validation: the VM library passed 440 tests, and the added live cursor test passes.
All 13 request ABI tests and five bridge tests pass, as do the three `.fill`
regressions. The PRVM host selection has 23 passes and one ignored guest test;
its one failure is an unchanged ExprVM telemetry-include resolution issue.
Fresh 68020 / 2 MiB execution completes both PRVM guests with zero exits and
required markers, checking 17 boundary/state cases and eight malformed resume
probes. The 30.78-second two-guest test duration includes harness/launcher work
and is not an assembly benchmark.

Isolated runtime code grows from 3,156 to 4,240 bytes (+1,084). The expanded smoke
Hunk grows from 7,140 to 13,248 bytes, mostly its new test matrix; that is not the
compact executable size. The linked native formatter checks 259 files cleanly.
Fresh-run proof, canonical debug contracts, emulator invocation policy, evidence
classification, workflow links and benchmark-selector checks pass. Instrumentation
safety retains three existing `DiagnosticBuffer` label findings in the PRVM
harnesses, confirmed against the preceding commit. This is a focused checkpoint,
not a full qualification claim.


## Macro descriptor service checkpoint

This first implementation checkpoint introduces an explicit macro PRVM entry
(kind 2, version 2), independently of compact capsule/storage integration. The
statement entry remains kind 1 and cannot execute macro descriptor programs.
Package programs select call/header envelopes, optional labels/parentheses/leading
commas, quote-aware comments, saturated comma depths and first-raw-`=` defaults.
These are macro semantics, distinct from the statement parser's signed depths.

The source is traversed only by the VM during initial preparation. Hosts receive
selected original spelling spans and token identities; they copy literal fragments
and bind identities. This is necessary to preserve substitutions before string
escape decoding. It does not authorize the host to parse rendered source later.

The program operations are `0x80` envelope (mode, flags), `0x81` argument split
(depth policy, separator), `0x82` formals (default policy), `0x83` publication and
`0x00` end. Call mode is 1 and header mode is 2. Envelope flag bits select labels
(1), outer parentheses (2), optional leading comma (4, call only), and unquoted
semicolon comments (8). Default call/header policies are 15/11. Unknown policies,
invalid order, truncated programs and trailing operations fail explicitly.

Each descriptor occupies 32 big-endian bytes: kind/flags (two words), then token
start/end, source start/end and three auxiliary longs. Token/source spans are
half-open indices/offsets, never pointers. Kinds are line (8), argument (9), formal
(10) and default (11). Line flags distinguish call/macro/segment (1/2/3); its token
range selects the head name, source range preserves the supplied full list and
auxiliaries select first child, supplied child count and optional label token.
Formals select their name and carry optional type-token and default-descriptor
indices. Defaults are appended after the contiguous formal region. Empty defaults
remain distinct from absent defaults; empty arguments are errors.

Publication is atomic: at most 64 records, caller capacity checked independently,
no records returned or caller output bytes changed on failure. Native staging is
2,048 bytes plus local state on the call stack; the statement interpreter's
428-byte frame must not be allocated for this independent entry. Common native
request/status/result definitions move to one `prvm.amigaos.abi` owner, rather than
being duplicated in the two executors.

This checkpoint deliberately does not change BSP3 or compact template storage.
`appendCallText`, `captureCallText`, the header-default scanner and substitution
consumer remain until descriptors are integrated together. The three recorded
substitution-order discrepancies also remain. Initial descriptors diagnose forms
that cannot be represented precisely by the supplied lexical token spans; this is
not a claim of complete macro frontend parity.

The live generic TKVM/Rust oracle and fresh native release run cover 38 cases:
14 valid calls/headers (including the 64-record limit), plus 24 grammar, token,
capacity, budget and opcode-sequence failures. Every status, error offset, event
field and caller-buffer byte is compared. Failure records remain unpublished;
the native harness writes the actual caller buffer rather than a conditional
copy that could conceal writes. Six Rust service tests also compare spelling,
types and defaults with the existing core macro processor.

The release service contributes 3,438 code bytes. Combined PRVM code is 7,700
bytes, compared with the preceding statement-only 4,240 bytes (+3,460, including
22 dispatch bytes). The native macro entry uses 2,236 bytes of local state and
staging, plus 60 bytes of saved registers and call return addresses. It avoids
the statement entry's 428-byte local frame. The release harness Hunk is 9,444
bytes with 25,836 bytes of linked allocation; these are fixture costs, not the
compact CLI footprint. Enabled reusable telemetry adds 548 code bytes and 228
BSS bytes; the disabled release build emits neither.

Both the release and telemetry-enabled 38-case batches match the same live Rust
oracle, with fresh completion and zero guest exits. They complete in 0.758/0.775
host START-to-DONE seconds with the configured FS-UAE 68020/2 MiB profile.
This includes harness file I/O and protocol
overhead, and is not a speedup measurement or a hardware clock claim. Compact
assembly performance remains unchanged until storage/binding integration.

The existing fresh-native statement/resume and line-iterator smoke checks pass
after ABI extraction, as do 24 focused PRVM host checks. Canonical native
formatting checks 271 files without changes or warnings. Fresh-run proof,
instrumentation safety on the new modules/harness, debug evidence classification,
benchmark-selector and workflow-link checks pass. This is a focused checkpoint;
the existing three instrumentation-label findings in older PRVM harnesses and
the retained compact macro consumers have not been resolved here.


## Compact macro descriptor storage checkpoint

BSP4 replaces BSP3 for this experimental producer/consumer pair. Its 116-byte
header retains the earlier fields and appends offsets/lengths for initial call,
header, generated packed-call and generated spelling programs, plus macro contract
version 2. Preparation programs follow the runtime region. No old capsule executor
is retained. This is a host-to-native preparation capsule, not a persistent source
format or final runtime package design.

Initial TKVM spans remain live long enough for package-selected PRVM entry 2 to
produce plans. The writer supplies a lexical-to-packed offset map; the session
arena copies each selected supplied-list spelling once and stores the 32-byte
rows. Token ranges and optional label/type fields become packed offsets; spelling
ranges become arena offsets. A six-byte trailer carries an offset-plus-one handle.
The arena is released with its preparation session. Outside template capture,
ordinary generic directives do not receive call plans. Captured dot statements
can retain plans for placeholder spelling consumers. Inactive definitions
preserve their previous skip behavior.

Templates consume descriptor-selected formal/default/argument ranges. The writer's
`appendCallText`, template `captureCallText`, host comma-boundary walker and raw
header-default `=` scanner are removed. Generated calls retain their existing
executable packed tokens: entry 3 selects packed boundaries, while configured TKVM
and entry 2 select boundaries over transient generated spelling. Their counts and
kinds must agree before publication. Packed policy 2 preserves matched delimiters
and the 16-level bound; spelling policy remains separately selected. The VM owns
these boundaries, and the host owns copying, identity binding and expansion.
Each definition retains a four-byte header-plan handle. The remaining spelling
consumer reads formal names from the VM-selected spans rather than assuming
their executable IDs belong to the symbol table. This also covers formal names
that coincide with package names, such as `a` on 6502.

The existing placeholder/string consumers remain for the next recipe checkpoint.
This does not resolve the three recorded quote/substitution-order discrepancies,
and is not a claim of binary-only expansion or complete macro parity.

A first fresh native boundary comparison exposed stack exhaustion: the original
4,412-byte packed-service frame exceeded the default Amiga stack. Compact u16
offsets and contiguous token ranges remove the redundant end table; the frame is
now 2,884 bytes including a matched-delimiter stack. The separately supplied
spelling scratch does not live on the call stack. Atomic buffer checks compare
all caller bytes, so this corruption could not be hidden by copying only valid
records.

The unchanged release macro-repeat comparison uses 256 calls, 3,036 source bytes
and 512 output bytes. Both runs produce the same live Rust output under the same
configured 68020 / 2 MiB profile. START-to-DONE rises from 3.7871 to 5.5516 seconds
(+46.6%); linked allocation rises from 83,880 to 94,276 bytes (+10,396), including
code 67,588→77,928, data 568→588 and BSS 15,724→15,760. The preparation capsule
rises from 11,032 to 11,102 bytes (+70). These are single comparative observations,
not statistical estimates or physical Amiga clock claims. This slice corrects
ownership of grammar boundaries; it does not demonstrate a performance gain.
Reproduce with `scripts/performance/prepared_source_native.py --workload macro-repeat
--blocks 32 --compact-cli --compact-only --memory-profile 2m --cpus m6502`, supplying
the current native test executable and output directory. The preceding baseline
uses native sources from `f707b271`. Further work must account for this regression
before expanding the same mechanism broadly.

Focused qualification passes all 451 Rust VM library tests, capsule preparation,
the fresh 38-case initial and 19-case packed descriptor comparisons, scoped and
imported calls, exact full-list spacing, inactive headers, both conditional
branches, two-target omitted defaults, package-name formals with embedded string
substitution, and four quoted-argument/default/header cases. Existing native
statement/resume and line-iterator proofs also pass. A telemetry-enabled compact
run under the 2 MiB profile reports 532,024 peak owned bytes, balanced allocation
and free accounting, zero remaining owned bytes and zero profiling errors.

The compact formatter checks 48 files without changes or warnings. Fresh-run
proof, boundary contract, canonical native contracts, instrumentation checks on
the adapted production modules, debug classification and workflow links pass.
This is focused qualification, not a clean broad gate: the architecture checker
retains 10 baseline enforced findings, the runtime inventory retains its existing
`tkpkg.amigaos.value_execution` source mismatch, and three instrumentation-label
findings remain in older PRVM harnesses. These checks were not weakened.


## Macro descriptor cost reduction

The experiment uses `992fd712` as the working reference: remove unnecessary
publication and preparation work without moving grammar out of the package/VM,
changing output, or bypassing the retained spelling consumers. Stop on a fresh
native mismatch, invalid descriptor publication, or an unexplained regression.
The workload remains 256 macro calls, 3,036 source bytes and 512 output bytes.

Generated descriptors now publish directly into their session arena. This removes
an identity map of 257 offsets, a 2,590-byte staging frame and the second mapped
row pass. Count/kind/range validation remains; the arena's used extent is published
only after all rows validate. Spelling is copied once, and persistent plan fields
remain offsets. A shared private reservation helper serves initial and generated
publication.

Compact clients use the thin `prvm.amigaos.macro_runtime` entry, with the same
package-selected executors, request guards and optional telemetry. The general
PRVM entry delegates macro calls to it; compact linkage no longer brings in the
statement interpreter and its resume-state storage. Macro calls receive one VM
profiling enter/leave, rather than nested wrapper observations.

Bound core directive identities, including captured `.byte`, `.align` and `.res`,
no longer acquire macro invocation plans. Their packed substitutions still run;
embedded string substitution obtains formal spelling from the definition's
header plan. Actual and unresolved nested dot calls retain the generated packed
and spelling services. This changes the captured-core-plan policy recorded in the
preceding storage checkpoint; it does not remove the remaining spelling consumer
or complete the deferred fragment recipes.

The existing gated MEM7 record is now decoded by the compact macro comparison.
Use the same performance command above with `--compare-memory` to collect owned
memory, preparation stages and tokenizer work. These instrumented timings include
probe overhead and must be kept separate from release timings. No telemetry record
or bytecode contract changed.

On identical inputs, the instrumented comparison reduces tokenizer invocations
from 519 to 263: the 256 generated core-body scans disappear. Tokenizer instructions
fall from 20,678 to 12,742 and source reads from 19,124 to 12,396. Peak owned memory
falls from 307,552 to 262,496 bytes; prepared live memory remains 33,024 bytes.
Both runs balance allocation/free accounting, release all owned blocks and report
zero profiling errors. Instrumented preparation falls from 7.12 to 5.12 seconds;
assembly stays at 0.66 seconds. These stage observations precede the final cheap
scope-ID filter and describe the same eliminated work, not release speed.

The final release comparison under the configured 68020 / 2 MiB profile is:

| Metric | Reference `992fd712` | This checkpoint |
|---|---:|---:|
| START-to-DONE host seconds | 5.5516 | 4.3105 |
| Linked allocation bytes | 94,276 | 90,232 |
| Linked code bytes | 77,928 | 73,904 |
| Linked data bytes | 588 | 568 |
| Linked BSS bytes | 15,760 | 15,760 |
| Preparation capsule bytes | 11,102 | 11,102 |

This is a 22.4% elapsed-time reduction and 4,044 fewer linked bytes. It remains
13.8% slower than the preceding pre-descriptor observation of 3.7871 seconds;
the architectural migration's regression is reduced, not eliminated. Each run is
one observation, includes guest input/preparation/assembly/output and protocol
work, and is not a physical Amiga clock claim. Output matches the live Rust oracle
and independent workload bytes with fresh completion and zero guest exit.
Release runs contain no memory telemetry record.

Reproduction uses the unchanged command above. Input SHA-256 is
`053168a2f23a43bea9b22977381f4ce63e6629f751a834cf8dd1712ced6c5100`;
output SHA-256 is
`1b1fa0c425b18a5f3a3122ef7397480da69e69db9451689884a00aa2c284b6a5`.
Final native source SHA-256 is
`8ca9ff5dff15bb7b854cf01832e37c4e27175b2be02874f368341c17526e8188`;
release image digest is `fnv1a64:c10748bb7100628e`.

Focused qualification passes fresh two-target core-body string/default/positional
substitutions followed by nested calls, labeled segments, nested invocations,
exact full-list spacing, BSS `.res`/`.align` segment expansion with whole-Hunk
comparison, the telemetry-enabled 38-case descriptor batch, and shared statement/
resume and line-iterator smoke checks. Thirteen affected Rust oracle/descriptor
checks and three performance-tool tests pass. Native formatting checks 48 files
without changes or warnings. Proof, boundary, canonical contract, instrumentation
safety on adapted modules, debug classification, benchmark-selector and workflow
link checks pass. The runtime inventory still reports only the previously recorded
`tkpkg.amigaos.value_execution` mismatch; existing broad-gate limitations above
remain. This is a bounded checkpoint, not complete macro parity or broad
integration qualification.


## Generated-call fragment recipes checkpoint

Agreed scope: cache package-selected literal, positional, named and supplied-list
fragment descriptors for captured nested calls, then expand by copying those
records. Remove generated-call use of the native `rewriteCallText` scanner;
retain its decoded-string consumer for the following ordering correction. The
working reference is `c4449e11` (release macro-repeat 4.3105 seconds, linked
90,232 bytes). Keep the current native spelling policy in this bounded migration;
this does not claim every Rust named-marker form or complete macro parity.

PRVM entry 4 emits bounded offset-only fragment records from the original selected
call-list spelling. Host code binds named fragments against VM-selected formal
spans and copies invocation values; it does not recognize placeholder grammar.
The latest preparation capsule replaces BSP4 with BSP5 to carry this program.
Recipes are created during definition capture and reused across invocations.

Success requires Rust/native recipe-record agreement, live full Rust CLI output
for nested/default/named/positional/full-list cases, fresh zero guest exits and
unchanged release/memory control inputs. Include malformed program, capacity,
work-budget and unresolved-marker cases. Stop for an unexplained output mismatch,
unsafe publication, broader syntax/storage redesign or disproportionate cost.


PRVM entry 4 and its package-selected grammar are implemented in Rust and native.
During definition capture the frontend emits fragments once; the plan arena
stores a validated recipe region and copied spelling, referenced by offsets.
The shared plan header grows from eight to twelve bytes for an optional recipe
region offset. Normal and generated publication preserve atomic used-extent
updates. Generated definitions can also cache their captured call fragments.

A separate compact copier owns bounds checks, named-formal binding and copying.
It never scans placeholder grammar or scans inserted values again. Generated calls
now use this copier, so `rewriteCallText` is called only by the retained decoded
string path. Named-formal comparisons still occur during invocation; this slice
caches grammar recognition, not every binding result. The source spelling arena
and generated TKVM/PRVM boundary services remain; binary-only expansion and the
three recorded string-order discrepancies are not resolved here.

BSP5 has a 124-byte header and appends the fragment-program offset/length. All
native consumers use this latest capsule; no BSP4 reader remains. PRVM retains
frame ABI 1 and contract 2 with explicit entry 4. The cached records use no
persisted memory pointers. Recipe and copied-byte work use existing reusable
telemetry macros; release emission stays conditional at assembly time.

Fresh release and telemetry-enabled 29-case batches match Rust records, status,
error offsets and untouched failure buffers. Alternate marker/digit/brace programs
prove that operands select recognition. Integrated two-target nested macro/segment
calls, default/positional/named/braced/full-list substitutions, unresolved markers,
exact supplied-list spacing and four quoted-argument/header-state cases match the
live full Rust CLI. The mixed new fixture is 265/266 source bytes and produces
26 output bytes. These are functional proofs, not comparative timing claims.

The unchanged macro-repeat control gives:

| Metric | `c4449e11` | This checkpoint |
|---|---:|---:|
| Release START-to-DONE seconds | 4.3105 | 4.2688 |
| Linked allocation bytes | 90,232 | 92,720 |
| Linked code bytes | 73,904 | 76,384 |
| Linked data bytes | 568 | 568 |
| Linked BSS bytes | 15,760 | 15,768 |
| Preparation capsule bytes | 11,102 | 11,120 |
| Instrumented peak owned bytes | 262,496 | 262,512 |

The single observations show no material control timing change, not a demonstrated
speedup. This control has no nested captured calls; it measures carrying the new
service and plan layout, not the benefit of cached fragment execution. Instrumented
preparation/assembly remains 5.12/0.66 seconds, with 263 tokenizer invocations and
12,742 tokenizer instructions. All tracked allocations are released, capacities
balance and profiling errors are zero. Input/output hashes remain the preceding
control's hashes; release image digest is `fnv1a64:65900adfb2b94cee` and native
source SHA-256 is
`d32528e10bb9b8bd37904992ad46e9260f0437004e5a39ef6b61f5f55175e6dc`.
The same bounded performance command and separate `--compare-memory` run reproduce
these controls; instrumented times include probes.

Qualification includes 456 VM and 101 package library tests, the capsule bounds/
program test, the new mixed Rust CLI oracle and the 29-case host oracle. Native
formatting checks 50 files without changes or warnings. Focused proof, boundary,
canonical contracts, instrumentation safety, debug classification, benchmark
selectors and workflow links pass. The CPU architecture guard retains 10 baseline
findings, and the inventory retains only the existing `value_execution` mismatch.
This checkpoint does not claim clean broad qualification or complete macro parity.

The actual cached-copy fixture also passes with telemetry enabled on both target
packages under the 68020 / 2 MiB profile. Peak owned memory is 185,200 bytes for
the 6502 package and 532,296 bytes for the 68020 package, with zero profiling
errors and balanced cleanup. These are different package footprints, not a
before/after comparison. They verify the adapted copier's enabled instrumentation
rather than inferring preservation from a control that does not execute it.

### Checkpoint: whole-line string substitution ordering

Hypothesis: owned pre-decoding spelling recipes plus a TKVM fragment entry can
preserve canonical substitution order without introducing a host text parser.
The baseline is `c5a43659`, including the three recorded discrepancies above.

The first integrated checkpoint routes ordinary macro-body lines containing VM
string tokens through complete-line recipes. PRVM entry 4 selects literal and
parameter ranges while original spelling exists; the host binds borrowed fragment
views. TKVM privately materializes at most 1,024 logical bytes and executes the
package-selected tokenizer. Only lexical records and normalized lexemes return;
the packed writer receives no expanded source pointer. Persisted recipes remain
offsets into owned storage. Preparation-only line trailer 43 selects this route.
This is internal bounded materialization, not a direct streaming interpreter.

Success requires fresh native equality with the live full Rust CLI for escaped
markers, injected quote/comma structure, comments consuming later tokens and
escapes crossing fragment boundaries. Existing nested-call/default/segment checks
must stay passing. Repeat the unchanged macro control and tracked-memory run;
report size/runtime costs without claiming a speedup from individual observations.
Stop and revise if lexical state still depends on host decisions or native proof
fails. Known nested call spelling and segment-body consumers retain their prior
paths at this checkpoint; no eligibility shortcut or complete macro parity is
claimed. Removing the old decoded-string scanner depends on migrating those
consumers too.


The recipe source bound is now 1,024 bytes in Rust and native, matching TKVM's
logical input limit. It does not change the 256-byte packed-line limit. Escaped
spelling can therefore exceed the former 253-byte limit while its decoded packed
output stays small. Private materialization source reads use the existing gated
`TOKEN_WORK` macro; release builds emit no probes. The service uses about 1,080
bytes of stack, plus its caller and ordinary TKVM frames; tracked owned-memory
measurements do not include that stack usage.

The independent harness exposed a separate Rust Hunk relocation defect:
computed absolute destinations such as `FragmentFrame+fragments.Frame.Count`
were emitted without the required section relocation. One failed instruction
contained absolute destination `$22` and consequently wrote into low OS memory.
The harness now loads the owned frame/view base once and addresses symbolic
struct offsets through registers. This is not a tokenizer VM failure. Repairing
computed absolute expression relocation remains a separate follow-up; this
checkpoint does not claim to fix that host assembler defect. The debugger
captures were localization evidence only, followed by fresh normal proof runs.

Fresh normal proof passes the complete-line Rust/native batch: escaped positional
and named markers, quote/comma injection, an injected comment consuming a later
expression, an escape crossing fragments, full-list/braced/unknown references,
normalized arithmetic, long escaped spelling, twenty substitutions in one string,
and default/repeated invocation values. Enabled telemetry reports peak owned
memory of 194,672 bytes, balanced releases and zero profiling errors. Independent
native batches pass 19 tokenizer cases (including invalid length and untouched
failure buffers) and 31 recipe cases (including the 1,024/1,025 boundary). Existing
nested/default fragment consumers pass with telemetry on both target packages;
nested macro/segment expansion also passes on both targets.

The unchanged 3,036-byte / 263-line macro-repeat control produces the same 512
bytes under the configured 68020 / 2 MiB profile:

| Metric | `c5a43659` | This checkpoint |
|---|---:|---:|
| Release START-to-DONE seconds | 4.2688 | 4.3076 |
| Linked allocation bytes | 92,720 | 94,132 |
| Linked code bytes | 76,384 | 77,796 |
| Linked data bytes | 568 | 568 |
| Linked BSS bytes | 15,768 | 15,768 |
| Preparation capsule bytes | 11,120 | 11,120 |
| Instrumented peak owned bytes | 262,512 | 262,512 |

These individual observations show no demonstrated material timing change.
The control has no template strings, so it measures carrying the new path, not
its string-expansion throughput. Instrumented preparation/assembly takes
5.14/0.68 seconds; all owned allocations are released with zero profiling errors.
Tokenizer work remains 263 invocations, 12,742 instructions and 12,396 source
reads. The new string route's materialization reads are measured by gated probes
in its own functional comparison. Private stack usage remains separate from
owned-memory accounting. Input/output hashes match the preceding control.
Release image digest is `fnv1a64:b5453170b63ee590`; native source SHA-256 is
`d5f138103c8cc4bf2782d34c0553e8a9bce71d094ab42d4c304146f76d356924`.
Use the preceding bounded performance command and a separate `--compare-memory`
run to reproduce these controls.

Qualification passes 457 VM and 101 package library tests, focused live Rust
oracles, the fresh native comparisons above and the proof/boundary/23 canonical
contracts/instrumentation/debug-classification/selector/workflow-link guards.
Native formatting checks 51 files without changes or warnings. The architecture
checker retains its 10 baseline enforced findings; the inventory retains only
the existing `value_execution` source mismatch. No clean broad qualification,
complete macro parity or removal of the remaining decoded-string scanner is
claimed. At that checkpoint, the next migration consumers were nested call
spelling and segment strings; the newly localized Rust relocation defect warrants
a separate repair.

### Checkpoint: segment strings and macro-call classification

Ordinary segment-body string lines now use the same pre-decoding, VM-selected
fragment route as macro-body strings. The prior native path substituted after
escape decoding, so `"\x401"` with argument `A` incorrectly emitted `A` instead
of the literal `@1`. Fresh native comparisons with live Rust pass that case,
quote/comma injection, named substitution and a labeled segment call. The
labeled call needed the invocation label copied onto the fragment-produced
packed line; its `.word first` now resolves to the correct address. The shared
route was checked with m6502 and m68020 packages.

An independent bounded self-host probe exposed a regression introduced by the
macro descriptor integration at `992fd712`: an instruction operand such as
`move.l .value,d0` inside a macro body was mistaken for a nested macro call.
The call classifier now distinguishes package-bound instruction heads from
source labels before applying its unresolved-call fallback. Fresh native tests
pass the reduced inactive-conditional operand, the full telemetry macro, and
a forward nested call with a canonical source label. The source-form distinction
is still incomplete: an *unindented* instruction head inside a template can be
bound as a source name and rejected. That binder decision needs a separate fix;
the real native source here uses an indented instruction.

The unchanged 3,036-byte / 263-line macro-repeat control still emits 512 bytes
under the 68020 / 2 MiB profile. One release START-to-DONE observation was
4.2346 seconds versus 4.3076 seconds at the preceding checkpoint; this does
not establish a speed difference. Linked reservation is 94,324 versus 94,132
bytes (+192), while the preparation capsule remains 11,120 bytes. Enabled
telemetry still peaks at 262,512 owned bytes with balanced cleanup and zero
profiling errors. The control does not exercise segment strings.

The fresh bounded self-host entry probe now advances past the telemetry macro
and rejects at `experimental/binary_source.asm` line 281, `.for 4`. Its staged
graph has 58 files and 646,248 source bytes; peak tracked ownership is
1,044,040 bytes with balanced cleanup. The unfinished interval flag remains
set, so there is no completed native output or timing. The next planned language
frontier remains packed loop expansion after the remaining frontend VM-boundary
work. This is a diagnostic checkpoint, not self-host parity.

### Checkpoint: generated-call argument tokenization

The captured-call copier already substituted from VM-selected original-spelling
fragments, but expansion then reprocessed decoded packed string tokens through
`rewriteCallText`. With an outer call passing `A`, a nested call containing
`"\x401"` emitted `A` instead of Rust's literal `@1`. After bypassing that
second substitution, a nested `"@1"` whose argument introduces quotes and
commas still rejected: the packed argument boundaries described the old line.

Generated calls now bind the captured fragments, tokenize the resulting
argument list through TKVM, and combine those packed arguments with the already
bound call head before PRVM describes the generated call. A transient leading
space marks the argument-only token stream as indented; no expanded source
pointer enters the packed record. The retained decoded-string scanner is no
longer used for captured generated calls, but remains in the fallback for body
tokens without a whole-line recipe.

Fresh native output matches the live Rust CLI for the escaped marker, a
substitution that introduces three arguments, a segment forwarding that string,
a zero-argument nested call, and the existing default/positional/named/braced/
full-list fixture on both m6502 and m68020 packages. A labeled segment whose
first body line invokes another macro still rejects with no source position.
The same reduced labeled case rejects at the preceding `e565f21e` checkpoint
even without strings, so that is a separate existing parity gap.

The unchanged 68020 / 2 MiB macro-repeat control emits the same 512 bytes from
3,036 source bytes. One release START-to-DONE observation is 4.2854 seconds
versus 4.2346 seconds before this change; the control contains no generated
calls and cannot measure their execution cost. Linked reservation grows from
94,324 to 94,728 bytes (+404), with the 11,120-byte package unchanged. Enabled
telemetry still peaks at 262,512 owned bytes, with balanced cleanup and zero
profiling errors. On the same 26-byte-output generated-call fixture, observed
m6502-package times were 0.5133 seconds before and 0.7743/0.7667 seconds after;
m68020-package times were 1.0287 before and 1.0109/1.0322 after. These few
host-clock observations suggest that unconditional re-tokenization can matter
for short nested calls, but do not establish a stable throughput ratio. Defer
shape-stable shortcuts and other performance tuning until the compact native
CLI completes a fresh self-host assembly with Rust-identical output and native
timing. Then use that workload's profile to decide whether generated-call
re-tokenization warrants optimization. Any shortcut pursued must have VM-owned
eligibility and explicit equivalence checks against forced re-tokenization on
identical inputs. Correctness and language parity needed for self-host remain
the immediate work.

The bounded self-host entry probe still rejects at `experimental/binary_source.asm`
line 281, `.for 4`, after staging 58 files and 650,076 source bytes. The Rust
oracle Hunk is 83,668 bytes with 94,728 bytes linked reservation; the runtime
package is 269,382 bytes. This negative probe supplies no native output or
completed self-host timing. Expression compilation and the residual decoded-
string consumer remain frontend boundary work before packed loop parity.

The 12 focused Rust macro oracles pass; fresh native cases above complete with
exact output under 68020 / 2 MiB. Native formatting, proof-contract, workflow
links, benchmark-selector and supply-chain checks pass. The CPU architecture
guard still reports its 10 enforced findings in unchanged files and none in
the changed modules; broad integration qualification is not claimed.

## Counted packed-loop checkpoint

BSP6 binds shared `.for` and `.endfor` identities in the package, replacing two
unused header words without growing the 124-byte header. Preparation compiles
the count expression; the compact native assembler replays the same packed body
records in both passes, with bounded nesting and the Rust 65,536-iteration limit.
No source text or package spelling lookup enters replay. The loop state holds
transient pointers, while binary records retain only numeric IDs and expression
bytes. Labels inside an active unscoped loop reject. Iterable `.for`, `.bfor`,
and general loop-body parity are still future work.

Fresh FS-UAE runs under 68020 / 2 MiB exactly match live Rust output for zero,
one, nested, and named-constant counted loops on the m6502 package and for a
four-iteration 68020 instruction/data loop. A labeled-body negative case has a
fresh nonzero guest completion. The bounded self-host probe now passes the
original `.for 4` frontier and rejects at `tkvm/tkvm_runtime.asm` line 109,
`TK_CLASS_IDENTIFIER_START = 2`. That remains a negative probe: no completed
self-host output or native timing exists. The m68020 runtime package is 269,404
bytes and the Rust oracle Hunk is 84,208 bytes with 95,372 linked reserved
bytes; these differ from the prior source graph and are not a performance gain.
The unchanged five-byte compact CLI control also matches Rust on native, with
one 0.505-second START-to-DONE observation and 95,436 linked reserved bytes.
There is no matched pre-change timing for this control, so no speed ratio is
claimed.

## Package-word label checkpoint

The next self-host rejection was a package-word collision: the source declares
an unindented `end` label in `binary_scopes.asm`, while the BSP6 dictionary also
owns `end` for `.end`. The packed writer now distinguishes a dotted statement
head from an ordinary name. A column-one name binds as a source declaration;
an operand using the `.end` spelling binds as a source reference, including a
forward reference. A focused m68020 / 2 MiB FS-UAE run emitted exactly the
live Rust bytes for `bra.w end` followed by an `end` label and retained working
`.cpu`, `.byte` and `.end` directives. Other package-word collisions are not
claimed as supported: `.res long` shows why operand roles cannot all be
redirected to source symbols.

The expanded-memory bounded self-host probe passes the previous line-573
frontier and reads the whole 59-file source graph, but still rejects during
late preparation, before output or native self-host timing. The new provisional
failure diagnostic reports preparation step 2, the `frontend.complete` call
that resolves scoped identities and imports. Its tracked peak is 3,040,608
bytes with instrumentation enabled; that is not a 2 MiB feasibility result.
The ordinary 68020 / 2 MiB profile remains blocked by a separate memory
ceiling. The current bounded run rejects at file ordinal 40, line 67 with a
damaged source-path diagnostic after a 1,044,064-byte tracked owned peak;
it does not reach `frontend.complete`. The earlier checkpoint rejected at
`tkvm_runtime.asm` line 109. An
exact-size fixed preparation allocation was tried and reverted: it saved only
1,576 peak tracked bytes in the expanded-memory run and the 2 MiB self-host
probe timed out at its five-minute bound. No performance gain or completed
self-host parity is claimed. The next focused step is to identify the first
failing scoped identity in `frontend.complete`, then revisit the 2 MiB memory
ceiling with a measured allocation breakdown.

## Signed word data and full-input completion probe

On the expanded-memory FS-UAE profile, the current 59-file compact self-host
input (656,451 source bytes before this checkpoint) completes preparation but
exits 20 during native assembly. Its hexadecimal diagnostic identifies physical
file 1, line 0x10 (decimal 16, `lea DosName, a1`); a fresh Rust build succeeds,
but no native self-host output exists.
The guest rejection is explicit, rather than a timeout. The run takes roughly
five and a half minutes of host test wall time with memory telemetry enabled;
the harness does not provide a reliable guest START-to-DONE duration for this
negative case.

A separate focused comparison found that native `.word` rejected a negative
scalar even where Rust emitted the fitting signed 16-bit value. Shared data
emission now accepts signed -32768 through -1 and unsigned 0 through 65535 for
word units. Fresh 68020 / 2 MiB native cases exactly match Rust for a direct
negative symbol and for a referenced imported module followed by the self-host
constant pattern. A subsequent Hunk-section case with the same constants and
a `.word GET_ARG_STR` use also matches the fresh Rust Hunk bytes. This correction
did not move the full self-host rejection:
the next full exact-output attempt again exited 20 at file 1, line 0x10 after
preparation. That instruction crosses from the entry/code section to `DosName`
in data; the compact Hunk path currently rejects section-bearing instruction
operands before encoding, so an instruction-relocation probe is the next step.
Assembly startup now sets its record offset to an invalid sentinel so a failure
before any record cannot masquerade as the first source line. No speed claim or
full self-host parity follows from this checkpoint.

## Package instruction relocations and next self-host frontier

The first instruction rejection at source line 16 was the entry section's
`lea DosName,a1` referencing DATA. The diagnostic printed `00000010` in
hexadecimal; earlier notes treated it as decimal line 10. A focused Hunk
comparison established that Rust emits an absolute-long instruction extension
and a relocation at offset 2. The compact package now carries validated
`fixup` sequence stages and numeric target projections; native execution
passes the selected package fixup through a bounded numeric side channel to
the existing Hunk relocation collector. A higher-priority unsupported member
candidate also needed a precise packed-shape exclusion so a bare symbol can
reach the applicable package recipe. Fresh 68020/2 MiB native comparisons
exactly match Rust for a forward-symbol LEA in flat output, a literal LEA,
and a CODE-to-DATA LEA Hunk relocation. Member-form fixup targets remain
explicitly unsupported in the compact package until their numeric identity
can be bound; no spelling-specific relocation shortcut was added.

The next full 59-file attempt on the expanded FS-UAE profile still exited 20
after 338.7 seconds of host test time. It advanced to file 1, hexadecimal
line `00000034` (decimal 52), `move.l IncludeCount,d0`. This is another
instruction with a section-backed symbol operand; no native completion or
whole-file timing is claimed. The next focused case should establish the
Rust relocation form and whether package preparation or native execution
blocks this operand shape. The ordinary 68020/2 MiB full-input memory ceiling
remains unproven after this expanded-profile attempt.

A focused live Rust Hunk oracle for `move.l reserved,d0` confirms a
CODE-to-BSS relocation at the instruction extension's offset 2. The same
source explicitly exits 20 in compact native at that instruction. Flat
`move.l target,d0` and even `move.l 8,d0` also explicitly exit 20, so this
frontier precedes Hunk relocation and symbol resolution. Diagnostic-only
package variants that exclude unsupported candidates, the fixup stage, and
the match stage did not produce native output; those variants were removed.
The precise native selector/projection/encoding rejection point is not yet
established. The next investigation should observe the failing boundary in
the real guest before changing its semantics. The focused native parity tests
are retained as ignored known-failure cases, not as passing evidence.
