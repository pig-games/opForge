# Native opForge Implementations

This tree contains opForge deliverables that are built with opForge itself and
run natively on one of opForge's supported targets.

These sources are intentionally separate from `examples/`. Example programs are
small instructional or fixture-oriented assembly inputs; native implementations
are product/runtime code whose host environment is itself an opForge target.

The AmigaOS tree retains the legacy CLI and the compact experimental replacement.
The compact implementation uses packed source and package-backed VM execution;
its current capability and qualification are tracked in the
[runtime reset plan](../documentation/plans/native-runtime-reset.md).
Neither path is a claim of full Rust language, target or output parity.

## Compact CLI input and output checkpoints (experimental)

The separate entry is `motorola68000/amigaos/experimental/opforge_compact_cli.asm`.
Its provisional invocation uses ordinary named options:

```text
opforge_compact --cpu 68020 project --bin output.bin
opforge_compact --runtime-package p.bin -i main.asm --hunk output.hunk
opforge_compact --cpu 68020 . --bin
opforge_compact --cpu 68020 project -x records.hex -g 00C000
```

One positional file or directory replaces `-i`. A directory selects `main.asm`;
`.` and omitted input mean the current directory. The resolved root-file directory
is the default module/include search root; an include first searches its own
directory. Additional `-M`/`-I` roots retain command order. Root input anchors
discovery, not module execution order. Quoted Amiga Shell paths, `--name=value`,
attached short values and `--` are supported. Help/version need no package/input.

An initial `--cpu` or `--runtime-package` is optional. The compact CLI can select
its package from `.cpu NAME` in the unconditional root preamble, after comments,
blank lines and `.module NAME`. Numeric aliases and quoted names are accepted;
explicit CLI selection takes precedence. If the preamble contains no `.cpu`, the
generated engine default is used. Discovery stops at the first other statement;
it does not inspect includes or conditional/macro bodies. Later package switching
remains unsupported. See [initial package selection](../documentation/plans/native-runtime-reset.md#source-selected-initial-package--initial-preamble-qualified)
for bounds and fresh qualification. With no output
request, the compact CLI validates the assembly and writes no artifact. An
omitted output filename derives from the input basename unless output metadata
selects another base. `-o` is still unsupported in compact. Relative output names
resolve under the effective output base parent; the default base is the input
basename in the working directory. Source `.output` paths are literal paths
relative to the process working directory, and can name multiple artifacts
without CLI output options.

The compact source-output subset accepts `.output "path", format=bin|prg|hunk,
sections=name,...`; specify `format` before `sections`. Sections are required.
`bin` and `prg` accept one or two contiguous placed sections. `hunk` uses the
source-configured Hunk sections. Repeated Hunk outputs must use the same section
list in the same order. One explicit CLI `--bin` or `--hunk` request can add an
artifact alongside source-selected outputs. An explicit `-x`/`--hex [FILE]`
or `-s`/`--srec [FILE]` adds one Intel HEX or Motorola S-record artifact;
`-g`/`--go ADDRESS` supplies a record start address of 4–8 hexadecimal digits.
Record output supports flat assembly, including placed flat sections. Converting
Hunk output to record output is rejected. The renderer can consume sparse
numeric address spans, but CLI `.org` sparse parity is not yet claimed. Record
rendering uses an 84-byte frame and a caller-owned 4096-byte streaming buffer;
ordinary binary and Hunk output allocate neither the buffer nor captured spans.
The compact writer creates
missing parent directories.

Quoted inline root-module metadata can request Hex without output flags:

```asm
.meta.output.name "build/program"
.meta.output.hex
```

`.meta.output.hex "records"` selects a named Hex file, adding `.hex` when absent.
CLI Hex takes precedence over that filename. A CLI binary/Hunk/S-record request
can coexist with source Hex; its omitted filename follows `.meta.output.name`.
Output naming alone produces no artifact. Active conditionals control metadata;
imported modules and nested scopes cannot set root output metadata. Descriptive
`.meta.name "..."` and `.meta.version "..."` are accepted without an output effect.
This is an inline quoted subset: unquoted values, configuration blocks, CPU
metadata overrides, source BIN/FILL/S-record metadata, listings, defines and general source-dependent
CPU switching remain future work. The BS16 runtime package option is distinct from
Rust's canonical `.opasm` option. Old three-positional compact commands are retired;
current export and hardware-runner commands use the named options.

The preparation-order repair captures unbound binary records before
configuration/dependency scanning, then performs semantic preparation in dependency
order. Checkpoint `76a1d11e` has complete fresh self-host proof with an exact
442,140-byte Rust Hunk artifact under an expanded-memory investigation profile;
this is not a 2 MiB or full feature-parity qualification. The subsequent initial
CPU-selection change has focused native proof, but no repeated full self-host run.
See the runtime reset plan for current evidence.

## Current Layout

- `motorola68000/amigaos/opforge-cli/`: native AmigaOS opForge CLI entry point
  and package fixture.
- `motorola68000/amigaos/exprvm/`: native expression VM runtime modules.
- `motorola68000/amigaos/opcore/`: native opCore support modules.
- `motorola68000/amigaos/opasm/`: native opasm staging modules currently used
  for the first selector/encode request bridge.
- `motorola68000/amigaos/prvm/`: native parser VM runtime modules.
- `motorola68000/amigaos/tkpkg/`: native package-backed tokenizer/runtime
  modules and package fixtures.
- `motorola68000/amigaos/tkvm/`: native tokenizer VM runtime modules.
- `motorola68000/amigaos/test-harnesses/`: non-deliverable AmigaOS debug,
  smoke, and sample entrypoints used by tests and FS-UAE validation.

## Deliverable Versus Harness Code

The production-facing native deliverable is:

- `motorola68000/amigaos/main.asm`

The runtime modules it depends on are deliverable support code:

- `motorola68000/amigaos/tkpkg/*.asm`
- `motorola68000/amigaos/tkvm/*.asm`
- `motorola68000/amigaos/prvm/*.asm`
- `motorola68000/amigaos/exprvm/*.asm`
- `motorola68000/amigaos/opcore/*.asm`
- `motorola68000/amigaos/opasm/*.asm`

The files under `motorola68000/amigaos/test-harnesses/` are validation and
debug entrypoints only. They may be assembled and launched by tests, including
FS-UAE tests, but they are not product CLI deliverables.

Notable harnesses include `test-harnesses/tkpkg/tkpkg_entry.asm`, a tiny hunk
wrapper that links the tkpkg service for smoke/link validation. It intentionally
lives outside `motorola68000/amigaos/tkpkg/` because the production tkpkg
surface is the service/runtime modules, not an executable entry wrapper.

## Legacy Native CLI Surface

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

Current legacy native CLI output behavior:

- `.bin`: implemented as a flat byte writer from the current native image
  buffer.
- `.hunk`: recognized by the CLI but intentionally returns
  `OPC-NCLI028`.

Compact CLI output currently supports:

- `.bin`
- `.prg`
- `.hunk` with source-configured sections
- Intel HEX through CLI options or inline source metadata
- Motorola S-record through CLI options

The compact CLI has a native artifact subsystem below the CLI. The legacy CLI
still writes flat `.bin` only. Neither surface yet provides native listings.

## Build And Run

Run these commands from the repository root.

The direct Rust CLI path is useful as a fast host-side assembly/listing check:

```sh
mkdir -p target/native-amigaos
cargo run -p cli --bin opforge -- \
  --cpu 68020 \
  -l target/native-amigaos/opforge_cli.lst \
  -M native/motorola68000/amigaos/tkpkg \
  -M native/motorola68000/amigaos/tkvm \
  -M native/motorola68000/amigaos/prvm \
  -M native/motorola68000/amigaos/exprvm \
  -M native/motorola68000/amigaos/opcore \
  -M native/motorola68000/amigaos/opasm \
  native/motorola68000/amigaos/main.asm
```

The repository-native formatter path for supported Motorola 68000 AmigaOS
sources uses the shared root config and workflow wrapper:

```sh
make native-68000-format-check
make native-68000-format
```

The current canonical Hunk build path is the same assembly helper used by the
native contract tests. It emits a listing and AmigaOS Hunk executable under a
fresh `crates/opforge-asm/target/test-m68000-opforge-native-cli-*` directory:

```sh
cargo test -p asm motorola68020_opforge_native_cli_shell_assembles_with_stage_stub -- --nocapture
mkdir -p target/native-amigaos
NATIVE_OPFORGE_HUNK="$(
  find crates/opforge-asm/target -path '*/test-m68000-opforge-native-cli-*/build/opforge_cli' -type f -exec ls -t {} + \
    | head -n 1
)"
cp "$NATIVE_OPFORGE_HUNK" target/native-amigaos/opforge_cli
```

Prepare a minimal AmigaOS `Work:` volume for a manual native CLI smoke run:

```sh
mkdir -p target/native-amigaos/Work
cp target/native-amigaos/opforge_cli target/native-amigaos/Work/opforge_cli
cp native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm \
  target/native-amigaos/Work/opforge_cli_package.opasm
cat > target/native-amigaos/Work/opforge_6502_native_cli_smoke.asm <<'EOF'
start   lda #$42
  sta $20
  lda $20,x
  sta $0200
  lda $0200,x
  lda $0200,y
done    jmp done
EOF
```

Boot an AmigaOS environment with `target/native-amigaos/Work` mounted as
`Work:`. From an AmigaShell, run:

```text
Work:
opforge_cli opforge_6502_native_cli_smoke.asm --bin opforge_native_out.bin --cpu m6502 --opasm-package opforge_cli_package.opasm
```

Expected result for the current smoke slice:

- stdout starts with `OPFORGE-NATIVE 1`.
- the run reports `STATUS output-ok`.
- `Work:opforge_native_out.bin` is written with the same bytes as the Rust VM
  reference for the smoke program.

On an actual Amiga, copy or otherwise place the native Hunk executable,
`opforge_cli_package.opasm`, and the source file in the same AmigaOS directory.
The file transfer method is outside this guide. From an AmigaShell in that
directory, run:

```text
opforge_cli opforge_6502_native_cli_smoke.asm --bin opforge_native_out.bin --cpu m6502 --opasm-package opforge_cli_package.opasm
```

The current smoke build expects a 68020-capable AmigaOS host. The command writes
`opforge_native_out.bin` in the current directory and prints the same
`OPFORGE-NATIVE 1` status report used by the FS-UAE checks.

To launch the same prepared `Work:` volume with the local FS-UAE template used
by the test harness:

```sh
awk -v work="$(pwd)/target/native-amigaos/Work" '
  BEGIN { replaced = 0 }
  /^hard_drive_1[[:space:]]*=/ { print "hard_drive_1 = " work; replaced = 1; next }
  { print }
  END { if (!replaced) print "hard_drive_1 = " work }
' "$HOME/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae" > target/native-amigaos/opforge-native.fs-uae
'/Applications/FS-UAE.app/Contents/MacOS/fs-uae' target/native-amigaos/opforge-native.fs-uae
```

The native CLI Hunk is currently built by the test/helper assembly path because
the production native CLI itself is still a first-run target deliverable. Keep
the build/run commands above in sync with `crates/opforge-asm/src/tests.rs` and
`crates/opforge-asm/src/fs_uae_smoke.rs` until a dedicated native build command
or script exists.

## Validation

Rust tests provide static contract coverage for the native assembly sources and
opt-in FS-UAE coverage for actual AmigaOS execution.

Common focused checks:

```sh
cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture
```

Normal non-mutating redundant-`tst` detection for native Motorola 68000 sources:

```sh
python3 scripts/workflow/check_native_68000_redundant_tests.py native/motorola68000 --fail
```

When you want to apply the mechanically safe cleanup, use the explicit
bookended workflow wrapper instead of the normal quality gate:

```sh
make native-68000-ccr-cleanup-round
```

The cleanup wrapper runs the assembler/native-oriented baseline tests before
mutating files, applies `--write --explain`, runs native 68000 formatting, and
reruns the same tests after cleanup. If the post-cleanup test phase fails,
inspect for brittle source-shape expectations before assuming the assembly
behavior itself changed.

FS-UAE tests are opt-in. They require `OPFORGE_FS_UAE_SMOKE=1` and
environment/configuration for the local FS-UAE executable and launcher
arguments. The helper code lives in `crates/opforge-asm/src/fs_uae_smoke.rs`.
Policy marker: opt-in-allowed for focused local tests; the configured native
reference completion command below is fail closed and mandatory for completion.
When running from a sandboxed agent, make sure the command has GUI/process
access before interpreting a FS-UAE `SIGABRT` during `UAE: Initializing core
derived from WinUAE` as an opForge failure.

Full local FS-UAE validation uses the same configuration template rewrite as
the manual launch above and runs the emulator-backed tests serially. Prefer the
one-shot environment form in agent shells:

```sh
OPFORGE_FS_UAE_SMOKE=1 \
OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' \
OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' \
OPFORGE_FS_UAE_ARGS='{fsuae_config}' \
cargo test -p asm external_fs_uae_ -- --nocapture --test-threads=1
```

For completion of native implementation work, the required active reference
gate is `make native-reference-parity-completion` with those same environment
variables. It is fail closed on missing configuration, skips, zero discovered
tests, crashes, or parity failures; it attempts every named test before
reporting the aggregate result. Its scope is exclusively 6502/65C02 and
includes `mos_forward_ref_stability.asm` as a separate proof plus all four
uncapped opcore shards.

Focused FS-UAE checks:

```sh
OPFORGE_FS_UAE_SMOKE=1 \
OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' \
OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' \
OPFORGE_FS_UAE_ARGS='{fsuae_config}' \
cargo test -p asm external_fs_uae_hunk_smoke -- --nocapture --test-threads=1

OPFORGE_FS_UAE_SMOKE=1 \
OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' \
OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' \
OPFORGE_FS_UAE_ARGS='{fsuae_config}' \
cargo test -p asm external_fs_uae_opforge_native_cli_6502_writes_rust_matching_bin -- --nocapture --test-threads=1

OPFORGE_FS_UAE_SMOKE=1 \
OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' \
OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' \
OPFORGE_FS_UAE_ARGS='{fsuae_config}' \
cargo test -p asm external_fs_uae_tkpkg_native_mos6502_family_corpus_matches_vm_authoritative_rows -- --nocapture --test-threads=1
```

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

## Current Known Transitional Pieces

- The CLI still owns pass/session/image state for the small 6502 path.
- Native `opasm` is a selector/request staging module, not the final assembly
  engine.
- Native `opcore` expression support is a scalar bridge, not complete EXVM
  parity.
- Legacy native CLI output is flat `.bin` only; the compact CLI also supports
  source-selected `.bin`, `.prg`, and `.hunk` artifacts.
- Some report records are compatibility observations for tests rather than
  final stable external CLI output.
