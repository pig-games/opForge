# Native opForge Implementations

This tree contains opForge deliverables that are built with opForge itself and
run natively on one of opForge's supported targets.

These sources are intentionally separate from `examples/`. Example programs are
small instructional or fixture-oriented assembly inputs; native implementations
are product/runtime code whose host environment is itself an opForge target.

Status as of 2026-05-06: the AmigaOS tree contains a real native opForge CLI
deliverable plus runtime modules and FS-UAE validation harnesses. It is not yet
the full Rust VM assembler path in native form. The live native path currently
proves host bootstrap, package-backed tokenization, PRVM line routing for the
current module/use parser slice, a small two-pass 6502 smoke assembler path,
flat `.bin` output, and deterministic diagnostics for known unsupported cases.

## Current Layout

- `motorola68000/amigaos/opforge-cli/`: native AmigaOS opForge CLI entry point
  and package fixture.
- `motorola68000/amigaos/opcore/`: native opCore support modules.
- `motorola68000/amigaos/opasm/`: native opasm staging modules currently used
  for the first selector/encode request bridge.
- `motorola68000/amigaos/prvm/`: native parser VM runtime modules.
- `motorola68000/amigaos/tkpkg/`: native package-backed tokenizer/runtime
  modules and package fixtures.
- `motorola68000/amigaos/tokvm/`: native tokenizer VM runtime modules.
- `motorola68000/amigaos/test-harnesses/`: non-deliverable AmigaOS debug,
  smoke, and sample entrypoints used by tests and FS-UAE validation.

## Deliverable Versus Harness Code

The production-facing native deliverable is:

- `motorola68000/amigaos/opforge-cli/opforge_cli.asm`

The runtime modules it depends on are deliverable support code:

- `motorola68000/amigaos/tkpkg/*.asm`
- `motorola68000/amigaos/tokvm/*.asm`
- `motorola68000/amigaos/prvm/*.asm`
- `motorola68000/amigaos/opcore/*.asm`
- `motorola68000/amigaos/opasm/*.asm`

The files under `motorola68000/amigaos/test-harnesses/` are validation and
debug entrypoints only. They may be assembled and launched by tests, including
FS-UAE tests, but they are not product CLI deliverables.

Notable harnesses include `test-harnesses/tkpkg/tkpkg_entry.asm`, a tiny hunk
wrapper that links the tkpkg service for smoke/link validation. It intentionally
lives outside `motorola68000/amigaos/tkpkg/` because the production tkpkg
surface is the service/runtime modules, not an executable entry wrapper.

## Current Native CLI Surface

The native CLI accepts the current subset:

- positional `INPUT`
- `-i` / `--infile`
- `--bin [FILE]`
- `--hunk [FILE]`
- `-o` / `--outfile`
- `--cpu`
- `--opasm-package`
- `-M` / `--module-path`
- `--help`, `-h`, `--version`, `-V`

Important current limits:

- `--bin` is the only implemented artifact writer.
- `--hunk` is parsed, but reports `OPC-NCLI028` because native Hunk output is
  not implemented yet.
- Rust CLI flags such as listing, hex, S-record, defines, and include-path
  options are recognized as Rust-surface options that the native CLI does not
  implement yet.
- Quoted arguments are not supported by the native CLI subset.
- Multiple positional inputs are not supported.

## Current Pipeline Shape

The native CLI writes an `OPFORGE-NATIVE 1` textual report while it runs. That
report is a temporary observation and handoff contract for tests; it is not an
object file format.

The current live path is:

1. CLI parses AmigaDOS-style arguments and opens the input/package files.
2. `tkpkg` loads the selected package and pipeline.
3. Each source line is sent through package-backed tokenization.
4. PRVM line routing is invoked through the `ENTRY_ORD_PARSE_LINE` service
   envelope for the current parser/module-use slice.
5. The native CLI still owns a transitional assembly session for the small
   6502 smoke path: statement tables, labels, pass 1/pass 2, image bytes, and
   flat output writing.
6. The `opasm` selector stage builds a package encode request for the current
   small 6502 instruction subset.
7. `tkpkg` handles `ENTRY_ORD_ENCODE_INSTRUCTION` and writes encoded bytes
   back to the CLI image buffer.
8. The CLI writes flat `.bin` bytes when `--bin` is selected.

Architectural target notes:

- `tkpkg` is the runtime/service boundary for init, package load,
  set-pipeline, tokenize-line, parse-line, encode-instruction, and last-error
  behavior.
- `PRVM` owns statement/operand-shape routing for the parser slices currently
  implemented.
- `opcore` currently provides a scalar operand expression bridge for decimal,
  `$` hex, and label lookup needed by the small native 6502 path. It is not yet
  full Rust opcore/EXVM expression parity.
- `opasm` currently contains selector/request staging for the small native
  6502 path. It is not yet the full native assembly engine.
- The CLI still owns too much assembler state today. Moving that state into
  native `opasm` is planned work, not current behavior.

## Current 6502 Assembly Support

The current native 6502 path is a smoke slice, not full `m6502` parity.

Supported in the live smoke path:

- `.cpu 6502` / `--cpu m6502` selection through the staged package path.
- simple labels and forward label layout in the current two-pass session.
- `.org` for the current scalar expression bridge cases.
- `LDA #imm`, `STA abs`, and `JMP abs` in the staged native selector path.
- flat `.bin` output matching the Rust VM reference for the small native CLI
  smoke fixture.

Current diagnostic coverage includes:

- unknown native mnemonic: `OPC-NCLI025`
- unsupported native addressing mode: `OPC-NCLI026`
- unresolved native label: `OPC-NCLI022`
- invalid native `.org` expression: `OPC-NCLI027`
- duplicate native label: `OPC-NCLI021`
- image buffer capacity exceeded: `OPC-NCLI024`

Not yet implemented in the native 6502 path:

- the full `m6502` instruction/addressing matrix.
- first-run directive parity such as `.byte`, `.word`, `.text`, `.fill`,
  `.res`, constants/variables, and conditionals.
- full Rust-compatible expression parsing/evaluation.
- full source graph and macro/module semantics.
- output artifacts beyond flat `.bin`.

## Package Service ABI

The shared native service ABI lives in
`motorola68000/amigaos/tkpkg/tkpkg_abi.asm`.

Current entry ordinals:

- `ENTRY_ORD_INIT`
- `ENTRY_ORD_LOAD_PACKAGE`
- `ENTRY_ORD_SET_PIPELINE`
- `ENTRY_ORD_TOKENIZE_LINE`
- `ENTRY_ORD_PARSE_LINE`
- `ENTRY_ORD_ENCODE_INSTRUCTION`
- `ENTRY_ORD_LAST_ERROR`

The v1 control block is intentionally small and fixed-size for 68020-native
code. Callers use the control block input/output windows for request and result
payloads. Larger or richer payload contracts should be added as explicit
extensions rather than by teaching the CLI to parse package internals.

## Output Status

Current native output behavior:

- `.bin`: implemented as a flat byte writer from the current native image
  buffer.
- `.hunk`: recognized by the CLI but intentionally returns
  `OPC-NCLI028`.

Planned first-run output work for the 6502 completion plan:

- `.bin`
- `.prg`
- `.hex`
- `.lst`

The output architecture should become a native artifact subsystem owned below
the CLI. The CLI should request artifacts and write files, while artifact
renderers consume assembled session/image state.

## Validation

Rust tests provide static contract coverage for the native assembly sources and
opt-in FS-UAE coverage for actual AmigaOS execution.

Common focused checks:

```sh
cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture
cargo test -p asm external_fs_uae_opforge_native_cli_ -- --nocapture
```

FS-UAE tests are opt-in. They require `OPFORGE_FS_UAE_SMOKE=1` and
environment/configuration for the local FS-UAE executable and launcher
arguments. The helper code lives in `crates/opforge-asm/src/fs_uae_smoke.rs`.

The current FS-UAE native CLI coverage includes:

- native CLI module/use parser status reporting.
- native CLI small 6502 `.bin` output matching Rust VM reference bytes.
- native CLI failure-path diagnostics for the known 6502 smoke errors.
- tkpkg debug CLI file/manifest package cases.

## Related Documentation

- `documentation/opForge-native-vm-pipeline-report-v0_1.md`: temporary
  `OPFORGE-NATIVE 1` report and handoff record contract.
- `documentation/opforge-assembler-vm-path-guide-v0_1.md`: Rust VM assembler
  path guide used as the architecture reference.
- `documentation/plans/opforge-native-amigaos-6502-full-assembly-first-run-plan-v0_1.md`:
  plan for completing first-run native 6502 assembly support.
- `documentation/reviews/opforge-native-amigaos-deliverable-review-2026-05-06.md`:
  review of the native deliverable structure and architecture risks.

## Current Known Transitional Pieces

- The CLI still owns pass/session/image state for the small 6502 path.
- Native `opasm` is a selector/request staging module, not the final assembly
  engine.
- Native `opcore` expression support is a scalar bridge, not complete EXVM
  parity.
- Native output is flat `.bin` only.
- Some report records are compatibility observations for tests rather than
  final stable external CLI output.
