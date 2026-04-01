# opForge

opForge is a multi-target assembler with expressions, directives, macros, and true modules with visibility control alongside textual includes (`.include`).

It currently ships builtin support for:
- Intel 8080 family processors: 8080 alias, 8085, and Z80
- MOS 6502 family processors: 6502, 65C02, 65816, and 45GS02
- Motorola 6800 family processors: 6809 and HD6309
- Motorola 68000 family processors: `68000`, `68010`, `68020`, `68030`, and `68040` with the corresponding `m68000`/`mc68000` through `m68040`/`mc68040` aliases

Motorola 68000-family coverage currently spans the shipped integer surface for
`68000`, `68010`, `68020`, `68030`, and `68040`. `68010` keeps baseline
68000 addressing, `68020`/`68030`/`68040` add the shipped `68020+`
full-extension addressing surface, and `68040` adds `MOVE16` while rejecting
`CALLM`, `RTM`, and `MOVEC CAAR`.

The current MMU scope remains intentionally narrow: `PFLUSH` is accepted on
`68030` and `68040`, and the existing `68040` MMU-related `MOVEC` register
surface stays available. Broader PMMU/MMU families remain out of scope.

The current FPU scope is selector-driven and assembler-only. `.fpu 68881` and
`.fpu 68882` enable the external coprocessor surface on `68020` and `68030`,
while `.fpu 68040` enables the integrated `68040` core FPU subset on `68040`.
That integrated path intentionally excludes external-coprocessor-only
`FSIN`-class transcendental and extended-math mnemonics. Broader runtime
semantics are not modeled; opForge assembles the documented instruction surface
and keeps execution behavior out of scope.

The shipped Motorola 68000 example set now includes focused FPU fixtures such as
`68020_fpu_allmodes`, `68020_fpu_instruction_catalog`,
`68020_fpu_registers`, `68030_pflush_external_fpu`, and
`68040_integrated_fpu` so the current MMU/FPU surface is visible in
checked-in source and reference outputs.

It is originally inspired by [64tass](https://tass64.sourceforge.net) in terms of feature scope and notation style.

## Documentation

Detailed documentation:

- [opForge Reference Manual](documentation/opForge-reference-manual.md): assembler syntax, directives, expressions, formatter behavior, and CLI semantics
- [libopforge Developer Guide](documentation/libopforge-developer-guide.md): public Rust embedding surface, workspace layering, preview API boundary, and host integration guidance
- [Embedding Cookbook](documentation/libopforge-embedding-cookbook.md): embedding recipes for borrowed, owned, in-memory, and prepared-session hosts
- [Execution Modes and Lockstep Guide](documentation/libopforge-execution-modes-and-lockstep-guide.md): `Rust`, `Vm`, and `Lockstep` execution choices plus parity workflows
- [Diagnostics and Fixits Guide](documentation/libopforge-diagnostics-and-fixits-guide.md): diagnostics, fixits, source maps, and report consumption
- [VM Boundary & Protocol Specification](documentation/vm-boundary-protocol-v1.md): VM host/runtime protocol details
- [Assembler VM Path Guide](documentation/opforge-assembler-vm-path-guide-v0_1.md): contributor-facing walkthrough of the assembler VM path from source file through tokenization, parsing, expression handling, encoding, and artifact emission
- [`examples/`](examples): sample assembler programs and reference fixtures

`libopforge` and `opforge-ffi` are published and usable, but they are still
pre-1.0 host surfaces. Treat `v0.9.7` as a documented public preview rather
than a long-term stable API promise.

## Installation

Prerequisites:
- Rust toolchain (`cargo`, `rustc`)
- `cargo-audit` for security checks when running the audit lane (`cargo install cargo-audit`)

Build and install locally:

```sh
make build
cargo install --path crates/opforge-cli --bin opforge
```

## Quick Start

Assemble a single source file to listing + hex output:

```sh
opforge -l -x -i examples/helloworld.asm
```

Generate binary output from the emitted address range:

```sh
opforge -b -i examples/helloworld.asm
```

Format a source file in place:

```sh
opforge --fmt -i examples/helloworld.asm
```

The full CLI surface, output-routing rules, directive semantics, and formatter configuration are documented in the [reference manual](documentation/opForge-reference-manual.md) and `opforge --help`.

To inspect the currently registered CPU and capability surface directly, use
`opforge --print-cpusupport` or `opforge --print-capabilities`.

## Embedding

Embedding documentation:

- [libopforge Developer Guide](documentation/libopforge-developer-guide.md): current Rust embedding surface, workspace layering, and preview API boundaries
- [Embedding Cookbook](documentation/libopforge-embedding-cookbook.md): builder/session setup, in-memory hosts, prepared sessions, and FFI-oriented recipes
- [Execution Modes and Lockstep Guide](documentation/libopforge-execution-modes-and-lockstep-guide.md): runtime-mode selection and parity workflows
- [Diagnostics and Fixits Guide](documentation/libopforge-diagnostics-and-fixits-guide.md): diagnostics, fixits, and source-map handling
- [`crates/opforge-ffi/opforge.h`](crates/opforge-ffi/opforge.h): C-facing ABI contract

## Repository Map

- [`crates/opforge-lib`](crates/opforge-lib): published `libopforge` facade crate
- [`crates/opforge-core`](crates/opforge-core): generic language semantics (`opcore`)
- [`crates/opforge-asm`](crates/opforge-asm): assembler parsing, encoding, listings, and reports
- [`crates/opforge-engine`](crates/opforge-engine): orchestration, source loading, output routing, and runtime bootstrap
- [`crates/opforge-vm`](crates/opforge-vm): VM/runtime/package support
- [`crates/opforge-cli`](crates/opforge-cli): command-line frontend
- [`crates/opforge-ffi`](crates/opforge-ffi): C-facing ABI layer

## Development Shortcuts

Common local commands:

```sh
make build
make test
make reference-test
make build-ffi-release
```

Additional build and test lanes are listed in [`Makefile`](Makefile).
