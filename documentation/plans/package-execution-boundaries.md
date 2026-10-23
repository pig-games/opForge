# Package and shared-directive boundaries

## Current ownership contract

CPU, family and dialect semantics belong in package definitions and their
specialized execution boundaries. The shared assembler owns generic source
processing, symbol handling, layout and output. A package is data selected by the
source or CLI; package metadata does not make instruction-specific behavior
generic.

Generic directives such as `.byte`, `.word` and `.org` use shared directive and
expression processing. They must not dispatch through a CPU/family operand parser
or instruction encoder. Dot-prefixed statement names are resolved by shared
directive, macro and segment handling. If a dot-prefixed name cannot be resolved,
it reports a directive error before instruction processing.

Native package metadata and loading live in
[`packages/`](../../native/motorola68000/amigaos/packages/) and
[`native_package_build.rs`](../../crates/opforge-asm/src/native_package_build.rs).
The native source boundary is implemented in
[`prepare.asm`](../../native/motorola68000/amigaos/assembler/prepare/prepare.asm)
and the shared driver in
[`driver.asm`](../../native/motorola68000/amigaos/assembler/driver.asm). Keep
target-specific selection and encoding behind package-owned data and their
specialized VM or assembler services.

## Focused evidence and limits

[`native_shared_directive_boundary.rs`](../../crates/opforge-asm/src/tests/native_shared_directive_boundary.rs)
checks live Rust output for `.byte`, `.word`, `.long`, grouped and immediate
expressions, ordinary instructions and unknown dot-prefixed names. It also locks
the native preparation path's directive-before-instruction dispatch and shared
scalar expression route. The test covers this focused boundary only; it does not establish
complete expression, language, output or CPU-family parity.

Current native completion status, including full opcore and CPU-family gaps, is
tracked in the [native assembler plan](native-runtime-reset.md). Fresh completed
native proof must follow the
[native parity contract](../../agents/rules/native-rust-parity-porting.md).
