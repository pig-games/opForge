# Compact frontend: VM boundary correction

Status: source audit complete; correction proposed, not implemented. This takes
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
| `binary_source.leadingComposite` / `identifierComposite` | Recognize adjacent placeholder/name fragments, interpret positional digits and validate suffix characters. | Lexical recognition in a writer; not just serialization. |
| `binary_source.parseNumber` | Hardcodes decimal, `$` hex, `%` binary and `0x` hex conversion, separators and u32 overflow. | Second interpretation of literal spelling, with a narrower fixed grammar than the Rust token policy surface. |
| `binary_source.literalString` | Copies bytes already decoded by TKVM. | Appropriate packing; no duplicated escape parser. |
| `binary_source.nameOperand` and binder | Classify the package-owned numeric-looking `.cpu` name and resolve identifiers to IDs. | Context and binding are necessary, but normalized-token changes must preserve this name/value distinction. |
| `binary_source.appendCallText` | Copies the original dotted call's argument region into token-42 sidecar bytes. | Retains source spelling for execution, beyond diagnostics. |
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

## Why the current VM output is insufficient

Rust `PortableTokenKind::Number` carries text and base, not a numeric value;
native TKVM emits 20-byte kind/span/lexeme records with no normalized value.
The number scanners are deliberately permissive. Strings already carry decoded
bytes. The current TKVM opcode set has scanner primitives but no normalized
numeric-value or composite-recipe emission operation.

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
No production code, format or behavior was changed by this audit. Findings are
source observations; no new emulator execution or performance measurement was
performed.
