# Compact frontend: VM boundary correction

Status: numeric normalization and composed-name recipes implemented; the remaining
call/string and expression correction is active. This takes
precedence over the next packed-loop parity slice in the
[native reset](native-runtime-reset.md#fixed-input-allocation-slice).

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
| `binary_source.appendCallText` | Copies a leading dot statement's raw argument region into token-42 sidecar bytes, including ordinary directives. | Retains spelling beyond diagnostics and imposes a 251-byte raw-argument limit even on generic directives. |
| `binary_templates.rewriteCallText` | Scans those bytes for positional/named substitutions, including identifier-character rules. | Macro expansion is host-owned, but this is additional raw-text syntax recognition, not exclusively binary-token expansion. |
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
fragments. Strings already carry decoded bytes. Composed-name recipes are now VM metadata; call/string recipe emission is
still missing. The ordinary Rust expression path still uses core token spelling;
it does not yet consume the portable numeric metadata.

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
The compact BSP3 preparation capsule currently embeds TKVM; it does not embed an
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
