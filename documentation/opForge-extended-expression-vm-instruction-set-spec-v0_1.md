# Shared expression VM contract

The current expression evaluator is **EXPR v1**, containing the consolidated
scalar and typed capabilities. There is one supported opcode table and executor.
Pre-1.0 packages must be regenerated together with their runtime; superseded
scalar bytecode and v2 execution are not retained.

The expression parser is a separate **EXVM v1** contract. Each VM has its own
contract identifier. Native executors assume their compiled evaluator contract
and do not select, check or dispatch evaluator versions. Package/container bounds
and integrity validation remain required.

## Responsibility boundaries

- `opasm` selects the bounded expression token range within a statement or operand.
- `opcore` owns expression grammar, typed value construction, indexing, members,
  scalar projection, arithmetic and their diagnostics.
- The assembler supplies contextual bindings, section identity, current address,
  symbol stability, character encoding and struct definitions.
- CPU/family packages own operand interpretation and instruction encoding. Neither
  expression VM contains CPU-specific instruction semantics.

The canonical opcode and value definitions are in
[expr_vm.rs](../crates/opforge-core/src/expr_vm.rs); the assembler adapter is in
[vm_opcore.rs](../crates/opforge-vm/src/vm_opcore.rs). The shared parser grammar
compiles numeric programs in Rust and native through the existing EXVM backend.
The ordinary Rust AST compiler is also a producer of this same evaluator contract,
not a second bytecode version.

## Current opcode surface

Operands, validation and diagnostics are defined by the shared implementation.
Numeric literal payloads and symbol/field pool indices use little-endian encoding.
Names belong to the program/context tables; member evaluation does not reconstruct
an iteration scope name.

| Opcode | Operation |
|---|---|
| `00` | End |
| `01`, `02` | EmitDiag, Fail |
| `10`–`13` | PushLiteral, PushCurrentAddress, PushSymbol, PushStringLiteral |
| `20`–`22` | ApplyUnary, ApplyBinary, SelectTernary |
| `30`, `31` | PushRegisterRef, PushPlaceholder |
| `40`–`42` | WrapImmediate, WrapIndirect, WrapIndirectLong |
| `50`–`53` | BuildTuple, BuildList, BuildRange, BuildStructLiteral |
| `60`–`62` | GetMember, IndexValue, CallBuiltin |
| `70` | RequireScalar |

An opcode's presence in the contract does not promise every producer or executor
implements every form. Unknown calls and unsupported forms must fail explicitly.
The implemented compile-time builtin is `.len`, with numeric builtin ID and arity.

## Typed values and relocation provenance

The evaluator can carry scalars, strings, lists, ranges, structs and operand
wrappers. An addressable record has a default scalar and named typed members.
`IndexValue` retains the selected value, `GetMember` retains its member's type and
provenance, and scalar projection retains the default scalar's provenance.

For a labeled scoped repetition, the assembler publishes a list of records at
scope close. Each record snapshots that iteration's start address and immediate
local bindings; a nested named repetition can itself be a member. For example,
`items[1]` projects the second iteration's start address, while `items[1].entry`
selects that iteration's `entry` binding. Scalar members remain scalars; address
members retain their actual section, including cross-section aliases.

Scalar provenance has three states: absolute, section-relative address, and
unsupported address arithmetic. Adding/subtracting an absolute addend preserves
an address target. Subtracting two addresses in the same section produces an
absolute difference. Comparisons and logical operations produce absolute scalar
results. Unsupported arithmetic can still produce a flat numeric result, but
must not be silently encoded as a relocatable Hunk address.

Snapshots retain both numeric value and provenance. Relocation consumers must
use that result rather than reevaluate a mutable initializer or infer identity
from coincident numeric addresses.

## Execution modes and validation

Scalar and shape-preserving APIs use the same opcode table and executor. Scalar
mode requires an explicit valid projection; an arbitrary list, range or struct
has no implicit scalar value. An addressable record has its declared default.
Shape-preserving mode retains a selected record and its members.

The assembler adapter retains its existing scalar-list language boundary:
addressable records and address scalars qualify without being flattened; nested
lists without scalar projections do not. Existing integer-only struct-instance
fields reject address-bearing values rather than discard their provenance.

Rust rejects mismatched program/package contract identifiers. Executors validate
instruction operands, pool indices, stack shape, bounds and execution budgets.
Imported typed values are included in shape-budget accounting. Missing members,
invalid indices and unsafe relocation expressions need deterministic failures.

Native already executes the canonical scalar subset and a compact scalar runtime
representation. Choosing compact versus canonical encoding is a representation
choice, not simultaneous support for multiple VM contracts. Native also stores
typed iteration records and lists using arena-relative offsets and numeric member
identities. Shared evaluation selects members and projects record defaults while
preserving section provenance, including compact arithmetic over record aliases.
Plain dotted aliases use numeric occurrence bindings: complete qualified names
retain priority, while unresolved alternatives select actual record values in
shared evaluation. The [native frontend boundary](plans/compact-frontend-vm-boundary.md)
describes the supported subset, resource bounds and remaining frontend/replay
limits; this does not establish complete labeled-loop parity.
