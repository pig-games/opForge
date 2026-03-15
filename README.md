# opForge
Multi-target assembler with expressions, directives, and preprocessor macros. It also supports true modules
with visibility control alongside textual includes (`.include`).

This is a multi-target assembler for:
- Intel 8080 family processors (currently 8080 alias, 8085, and Z80).
- MOS 6502 family processors (currently 6502, 65C02, 65816, and 45GS02).
- Motorola 6800 family processors (currently 6809 and HD6309).

It is partly inspired by [64tass](https://tass64.sourceforge.net) in terms of features and notational style.
It produces optional Intel Hex, listing, and binary image outputs selected by command-line arguments.
For single-input builds, opForge can default to list+hex output when an output base is available
(`.meta.output.name` or `-o`).

It also supports patterned `.statement` definitions for custom statement syntax, with typed captures using
`type:name` and quoted literal commas (use `","`). Statement labels may include dots (e.g. `move.b`).
Recent directive/expression additions include range/list values, `.len(...)`, `.for/.bfor/.while/.bwhile`,
`.struct/.endstruct`, and typed struct literal instances (`Type { field: expr, ... }`).

For full documentation on features and syntax, read the [opForge Reference Manual](documentation/opForge-reference-manual.md).
For library architecture and embedding guidance, read the [libopforge Developer Guide](documentation/libopforge-developer-guide.md).
For VM host/boundary semantics, see [VM Boundary & Protocol Specification (v1)](documentation/vm-boundary-protocol-v1.md).
For latest release-scope implementation notes, see [RELEASE_NOTES_v0.9.5.md](RELEASE_NOTES_v0.9.5.md).

## Installation

Prerequisites:
- Rust toolchain (`cargo`, `rustc`)
- `cargo-audit` for security checks (`cargo install cargo-audit`)

Build and install locally:

    make build
    cargo install --path crates/opforge-cli --bin opforge

## Getting started

Assemble a single source file to listing + hex output:

    opForge -l -x -i examples/helloworld.asm

Generate binary output from emitted address range:

    opForge -b -i examples/helloworld.asm

For full syntax and directive semantics, see the [reference manual](documentation/opForge-reference-manual.md).

## Ranges, lists, and repetition

opForge now supports compile-time ranges/lists and structured repetition:

```asm
vals = {1, 2, 3}
.for n in 0..=4:2
    .byte vals[n/2]
.endfor
```

Scoped repetition (`.bfor`, `.bwhile`) allows per-iteration local labels and labeled indexed access (`label[n]`, `label[n].field` when struct-typed).

Typed struct literals are assignable to symbols through `.const/.var/.set` and assignment forms:

```asm
Point .struct
x .byte ?
y .byte ?
.endstruct

p0 .const Point { x: 24, y: 50 }
p1 .var   Point { x: 40, y: 60 }
.byte p0.x, p1.y
```

Resolution for dotted names is unified in one symbol namespace:
- First resolve exact dotted symbols (for example `p0.x` if defined explicitly as a symbol).
- If no exact symbol exists, fall back to typed member access (`base.field`) when `base` is struct-typed.

Reference examples:
- `examples/ranges_lists_basic.asm`
- `examples/for_counter_basic.asm`
- `examples/for_collection_basic.asm`
- `examples/bfor_labeled_struct_basic.asm`
- `examples/while_basic.asm`
- `examples/struct_literal_instance_basic.asm`

## Architecture overview

- `libopforge`: top-level curated embedding facade and stable Rust entrypoint.
- `opcore`: non-assembler language semantics (expressions, modules, macros, conditionals, scopes).
- `asm`: assembler-specific parsing/evaluation/encoding/output flow.
- `vm`: shared VM/runtime/package execution support.
- `registry` + `families`: assembler registry layer plus builtin CPU/family implementations.
- `engine`: session orchestration, source loading, output routing, and default runtime/bootstrap helpers.
- `formatter`, `lsp`, `cli`, `cli-core`, `ffi`: host-facing tooling layers over the split library.

Current public API note:
- The implemented Rust embedding API is usable now and centered on `libopforge::asm::Assembler`.
- The next API phase is the Rust-first libopforge API Aesthetics Improvement Plan, which further refines the public module layout while keeping the current execution architecture intact. See [libopforge API Aesthetics Improvement Plan](documentation/libopforge-api-aesthetics-improvement-plan-v0_1.md).

Stable public module map today:
- `libopforge::asm`
- `libopforge::asm::opasm`
- `libopforge::opcore`
- `libopforge::diagnostics`
- `libopforge::io`
- `libopforge::processing`
- `libopforge::registry`
- `libopforge::lockstep`
- `libopforge::unstable` for advanced or transitional exports that are not part of the normal supported embedding path

Build:

    make
    # or: make build

Run:

    cargo run -p cli --bin opforge -- <args>

Release build:

    make release

Compare Rust outputs with references:

    make reference-test

Run the full test suite:

    make test

Run the core assembler lane explicitly (no optional VM feature lanes):

    make test-core

Run VM parity smoke checks (optional feature lane):

    make test-vm-parity

Run MOS6502 runtime/package parity checks:

    make test-vm-runtime

Run MOS6502 runtime/package artifact-mode checks (optional feature lane):

    make test-vm-runtime-artifact

Run Intel8080-family runtime/package parity checks:

    make test-vm-runtime-intel

Run rollout policy/criteria gate checks:

    make test-vm-rollout-criteria

Run the local MOS6502 CI gate bundle:

    make ci-vm-mos6502

Run the local Intel8080-family CI gate bundle:

    make ci-vm-intel8080

Run the local core CI gate bundle:

    make ci-core

Validation lane boundaries:
- `ci-core` / `test-core`: core assembler behavior without optional VM feature lanes.
- `ci-vm-mos6502` and `ci-vm-intel8080`: VM-focused lanes layered on top of `test-core`.
- This split prevents VM feature-path regressions from silently altering core-lane expectations.

VM rollout status (VM runtime is default):
- Authoritative package-runtime family: `mos6502` (`m6502`, `65c02`, `65816`).
- Authoritative package-runtime family: `intel8080` (`8085`, `z80`).
- Authoritative package-runtime family: `motorola6800` (`m6809`, `hd6309`).
- Runtime rollout criteria coverage includes Motorola 6800-family assertions for both `m6809` and `hd6309` via `make test-vm-rollout-criteria`.

Optional on-disk runtime package artifact mode:
- Enable feature `vm-runtime-opasm-artifact`.
- Runtime then uses `.opasm` bytes at `target/vm/opforge-vm-runtime.opasm` as the default artifact path. Outside `vm-runtime-only`, that path can still be refreshed from the registry-built package; `vm-runtime-only` builds do not regenerate it implicitly.
- Rust-table-driven package generation remains the supported authoring path for new families/CPUs (`build_hierarchy_package_from_registry`).

Explicit runtime package selection:
- Use `--opasm-package <FILE>` (or `OPFORGE_OPASM_PACKAGE`) to force a specific `.opasm` package.
- Explicit package selection takes precedence over artifact and bundled/runtime-generated package sources.

VM-only package source modes:
- Embedded/default mode: feature `vm-runtime-only` for the CLI and high-level assembly/session path (bundled/runtime-generated fallback allowed there).
- Artifact mode: feature `vm-runtime-opasm-artifact` adds default path `target/vm/opforge-vm-runtime.opasm`.
- Unbundled mode: feature `vm-runtime-opasm-unbundled` disables bundled fallback.
    - In vm-only unbundled mode, opForge requires either `--opasm-package <FILE>` or the default artifact path (when artifact feature is enabled).

Lower-level engine/editor-helper APIs do not inherit the CLI package-source fallback rules. In `vm-runtime-only` mode, `libopforge::processing` and related editor helpers should either:
- receive an explicit runtime model through the `*_with_model` entrypoints, or
- run with `vm-runtime-opasm-artifact` enabled and the default artifact file present at `target/vm/opforge-vm-runtime.opasm`.

If neither is available, those lower-level helpers report that the runtime model is unavailable instead of regenerating or bundling one implicitly.

Cargo feature flags:
- `vm-runtime-opasm-artifact`: enables on-disk runtime package artifact mode at `target/vm/opforge-vm-runtime.opasm`; non-`vm-runtime-only` paths may refresh it from the registry-built package, but `vm-runtime-only` paths require the artifact to exist already.
- `vm-runtime-opasm-unbundled`: disables bundled/runtime-generated package fallback; runtime package must come from explicit path or artifact.
- `vm-parity`: enables parity-focused VM test lanes and CI checks.

VM-only build target variants:
- `make vm-only-build-embedded`
- `make vm-only-build`
- `make vm-only-build-unbundled`
- `make vm-only-build-unbundled-artifact`

VM package-source validation target:
- `make test-vm-opasm-modes`

Rebuild reference outputs (updates examples/reference/*.lst and *.hex):

    make reference

The reference set includes additional examples to exercise the newer syntax
(dot-prefixed conditionals, preprocessor directives, and 64tass-style
expressions).

## Rust embedding and output contract

The normal Rust embedding path goes through `libopforge::asm::Assembler`.
At the root, `libopforge` exposes the stable module layout directly:
- `libopforge::asm`
- `libopforge::asm::opasm`
- `libopforge::opcore`
- `libopforge::diagnostics`
- `libopforge::io`
- `libopforge::processing`
- `libopforge::registry`
- `libopforge::lockstep`

Advanced and transitional exports remain available under `libopforge::unstable`,
but they are not part of the normal stable embedding path.

Today, the stable Rust embedding surface includes:
- `libopforge::asm::Assembler`
- `libopforge::asm::AssemblerConfig`
- `libopforge::asm::OwnedAssemblerConfig`
- `libopforge::asm::AssemblerSessionBuilder`
- `libopforge::asm::AssemblerSession`
- `libopforge::asm::PreparedAssemblySession`
- `libopforge::asm::prepare(...)`
- `libopforge::asm::assemble(...)`
- filesystem-backed defaults plus in-memory `SourceProvider` / `OutputSink`
- explicit execution-mode selection (`Rust`, `Vm`, `Lockstep`)

This surface is supported and exercised by tests and examples, but it is not
yet the final polished module-partitioned API. The next phase is documented in
[libopforge API Aesthetics Improvement Plan](documentation/libopforge-api-aesthetics-improvement-plan-v0_1.md).

Ownership choice now changes ergonomics rather than capability for normal
supported assembler workflows:

- borrowed Rust hosts can use `Assembler::with_config(...)` with
  `libopforge::asm::AssemblerConfig`
- non-borrowing hosts can use `AssemblerSession::with_config(...)` with
  `libopforge::asm::OwnedAssemblerConfig`
- both paths expose the same grouped source/execution/output/diagnostics
  concerns and assemble through the same stable high-level API boundary

The root facade is module-first on purpose:

- prefer `libopforge::asm::Assembler` over `libopforge::Assembler`
- prefer `libopforge::io::MemorySourceProvider` over flat root imports
- prefer `libopforge::diagnostics::Diagnostic` over flat root imports

`AssemblerConfig` currently accepts an `input_base` (the caller-chosen output
base, usually the input file path stripped of its extension) together with an
optional `out_dir`.

Output-base selection follows this precedence (implemented in
`resolve_output_base` in `crates/opforge-vm/src/output_model.rs`):

| Condition | Selected base |
|-----------|--------------|
| `out_dir` is **not** set and `outfile_override` is provided | `outfile_override` |
| `out_dir` is **not** set and `.meta.output.name` is present | metadata output name |
| `out_dir` is **not** set, no override or metadata name | `input_base` |
| `out_dir` is set (any of the above cases) | only the final file-name component of the selected base is kept; `out_dir` supplies the directory |

When `out_dir` is present, the directory portion is always rewritten — even
when `input_base` is an absolute path.  Only the final path component (the
stem/filename) is preserved from the chosen base.

Example: `input_base = "/src/prog"`, `out_dir = "/build"` → effective base
`"/build/prog"`, regardless of the absolute source path.

## FFI consumer contract

For non-Rust hosts, the `ffi` crate now exposes stable C-facing groups over the
same Rust API that tool authors use through `libopforge`.

- Include `crates/opforge-ffi/opforge.h` for the consumer-facing contract.
- `execution_mode` is a validated `uint32_t` scalar using the
  `OPFORGE_EXECUTION_MODE_*` constants.
- Use the `opforge_asm_request` surface with
  `opforge_asm_*_with_request(...)` for high-level embedding work.
- `root_path` is required; all non-null string inputs must be NUL-terminated
  UTF-8.
- Unknown scalar values or invalid strings return `OPFORGE_STATUS_INVALID_REQUEST`.
- In-memory `opforge_asm_check_memory_with_request(...)` paths only require
  callbacks when buffered outputs actually exist; enabling `emit_outputs`
  alone does not make callback configuration a precondition for a no-output
  `check`.
- In-memory `opforge_asm_assemble_memory_with_request(...)` paths now fail with
  `OPFORGE_STATUS_INVALID_REQUEST` if the assembly actually buffers outputs and
  the caller did not provide callbacks to receive them, including directive-
  driven or metadata-driven outputs. `suppress_outputs` remains the way to
  prevent those buffered outputs when the host wants a diagnostics-only run.
- `opforge_asm_report_message()` and the `opforge_diag_*_from_asm_report(...)`
  string accessors return pointers borrowed from `opforge_asm_report`; they
  remain valid until `opforge_asm_report_free()` and must not be freed by the
  caller.

Current stable FFI groups include:
- `opforge_asm_*` for high-level assembler-oriented workflows
- `opforge_opcore_*` for lower-level `opcore` services such as tokenization and expression parsing
- `opforge_opasm_*` for lower-level assembler processor services
- `opforge_diag_*` for diagnostics and result enumeration
- `opforge_io_*` for in-memory and callback-based I/O
- `opforge_processing_*` for processing traces
- `opforge_lockstep_*` for lockstep reports
- `opforge_registry_*` for read-only registry and capability queries

The FFI remains intentionally thinner and more explicit than the primary Rust
API.

High-level `opforge_asm_*` reports now let C hosts enumerate not only primary
severity/message/span data, but also diagnostic file paths, related spans,
help text, and fix-it payloads through the `opforge_diag_*` accessors.

The `opforge_asm_request` leaf-config model is the stable high-level FFI
contract for assembler-oriented embedding.

The header is manually maintained for now. Any ABI-affecting change must update
both `crates/opforge-ffi/src/lib.rs` and `crates/opforge-ffi/opforge.h`, and
the crate-level integration tests under `crates/opforge-ffi/tests/` are meant
to stay green as the review backstop for that contract. The header ABI compile
check requires a C compiler, so CI should keep `cc`, `clang`, or `gcc`
available.

## Usage
Syntax is:

    opForge [OPTIONS] [INPUT]...

Arguments:

    [INPUT]...                    Optional migration-friendly positional input.
                                 Exactly one positional INPUT is accepted and
                                 treated like -i INPUT. Multiple positional
                                 inputs require explicit -i/--infile.

    -i, --infile <FILE|FOLDER>   Input assembly file or folder (repeatable). Files must end with .asm.
                                Folder inputs must contain exactly one main.* root module.

    -I, --include-path <DIR>     Additional include search root (repeatable).
                                 Include resolution order is: including file
                                 directory, then include roots in command-line
                                 order.

    -M, --module-path <DIR>      Additional module search root (repeatable).
                                 Module roots are searched in this order: input
                                 root directory, then module roots in
                                 command-line order.

    -l, --list [FILE]            Emit a listing file. FILE is optional; when omitted, the
                                 output base is used and a .lst extension is added.
                                 
    -x, --hex [FILE]             Emit an Intel Hex file. FILE is optional; when omitted,
                                 the output base is used and a .hex extension is added.
                                 
    -o, --outfile <BASE>         Output filename base when -l/-x are used without a filename.
                                 Also used for -b outputs that omit a filename. Defaults to the
                                 input filename base.
    --dependencies <FILE>        Write Makefile-compatible dependency rules to FILE.
    --dependencies-append        Append dependency rules to --dependencies FILE.
    --make-phony                 Emit phony targets for each dependency path in generated dependency output.
    --labels <FILE>              Write assembled symbol labels to FILE.
    --vice-labels                Write --labels output in VICE-compatible format.
    --ctags-labels               Write --labels output in ctags-compatible format.
    -b, --bin [FILE:ssss:eeee|ssss:eeee|FILE]
                                 Emit a binary image file (repeatable). A range is optional.
                                 Use ssss:eeee to use the output base, FILE:ssss:eeee to
                                 override the filename, or FILE to emit the full output range.
                                 Range values are 4-8 hex digits per side.
                                 If FILE has no extension, .bin is added.
                                 If multiple -b ranges are provided without filenames, each file
                                 is named <base>-ssss.bin to avoid collisions.
    -g, --go <aaaa>              Set execution start address (4-8 hex digits). Adds a Start
                                 Address record to the hex output. Requires hex output.
    -f, --fill <hh>              Fill byte for binary output (2 hex digits). Defaults to FF.
    -D, --define <NAME[=VAL]>    Predefine a macro (repeatable). If VAL is omitted, it
                                 defaults to 1.
    -c, --cond-debug             Append conditional state to listing lines.
    --line-numbers               Compatibility flag for listing line-number column (enabled by default).
    --tab-size <N>               Expand tab characters in listing source text using N spaces.
    --verbose-list               Compatibility flag reserved for expanded listing sections.
    -q, --quiet                  Suppress diagnostics for successful runs.
    -E, --error <FILE>           Write diagnostics to FILE instead of stderr.
    --error-append               Append diagnostics to --error FILE.
    --no-error                   Disable diagnostic output routing.
    -w, --no-warn                Suppress warning diagnostics.
    --Wall                       Enable all warning classes (reserved for future groups).
    --Werror                     Treat warnings as errors.
    --format <text|json>         Select global CLI output format.
    --diagnostics-style <classic|rustc>
                                 Select text diagnostics rendering style (default: rustc).
    --fixits-dry-run             Plan machine-applicable fixits without writing files.
    --apply-fixits               Apply machine-applicable fixits.
    --fixits-output <FILE>       Write one invocation-level fixit planning/apply report JSON to FILE.
    --fmt                        Format input files in place (shorthand for --fmt-write). Folder inputs also format linked module files.
    --fmt-check                  Check formatting for input files without writing changes. Folder inputs include linked module files.
    --fmt-write                  Apply formatter changes in place for input files. Folder inputs include linked module files.
    --fmt-stdout                 Format exactly one input file and write result to stdout.
    --fmt-config <FILE>          Formatter config path (requires a formatter mode flag).
    --cpu <ID>                   Set initial CPU before parsing source directives.
    --opasm-package <FILE>       Load VM runtime package (.opasm) from FILE and prefer it over bundled/artifact package sources.
    --print-capabilities         Print deterministic capability metadata and exit.
    --print-cpusupport           Print deterministic CPU support metadata and exit.
    --pp-macro-depth <N>         Maximum preprocessor macro expansion depth (default 64, minimum 1).
    --max-loop-iterations <N>    Maximum .for/.while iterations before reporting an error (default 65536, minimum 1).
    --input-asm-ext <EXT>        Additional accepted source-file extension for direct file inputs.
    --input-inc-ext <EXT>        Additional accepted root-module extension for folder inputs.
    -h, --help                   Print help.
    -V, --version                Print version.

For multiple inputs, at least one output option (`-l`, `-x`, or `-b`) must be selected.
For a single input with no explicit outputs, opForge defaults to list+hex when an output base is
available from `.meta.output.name` or `-o`; otherwise output selection is required. Output selection can
also be provided by `.meta.output.list`, `.meta.output.hex`, and `.meta.output.bin` in the root module;
`.meta.output.fill` sets the binary fill byte. CLI flags always take precedence when both are present.

The `-g` option adds a Start Segment Address record for 16-bit values and a Start Linear
Address record for wider values in the output hex file.

If `test.asm` is specified as the input with `-i` and `-l`/`-x` are used without filenames (and `-o` is not used), the outputs will be named `test.lst` and `test.hex`. Bytes not present in the assembly source are initialized to `FF` in binary image files.

When multiple inputs are provided, `-o` must be a directory and explicit output filenames are not allowed; each input uses its own base name under the output directory.
Formatter mode (`--fmt`, `--fmt-check`, `--fmt-write`, `--fmt-stdout`) requires at least one input and cannot be combined with assembler output flags or fixit options.
`--fmt-stdout` requires exactly one input.

### Examples
    opForge -l -x -i test02.asm
creates test02.lst and test02.hex.

    opForge -l -x -b 7eff:7fff -b f000:ffff -i prog.asm
creates:
* The assembler listing in prog.lst
* The hex records in prog.hex
* A 512 byte binary image file prog-7eff.bin
* A 4096 byte binary image file prog-f000.bin

    opForge -o build/out -l -x -i prog.asm
creates:
* The assembler listing in build/out.lst
* The hex records in build/out.hex

    opForge -b out.bin:8000:8fff -i prog.asm
creates:
* A 4096 byte binary image file out.bin

    opForge -b -i prog.asm
creates:
* A binary image file containing the emitted output range

    opForge -x -g 123456 -b out.bin:123400:12341f -i examples/65816_wide_image.asm
creates:
* A hex file with wide-address records (ELA + start linear address)
* A binary image file out.bin covering `$123400..$12341F`

    opForge --fmt prog.asm
formats `prog.asm` in place.

    opForge --fmt project/
formats the resolved `main.*` root module and linked module files in `project/`.

    opForge --fmt-check -i prog.asm
checks formatting and exits non-zero when changes are required.

    opForge --fmt-stdout -i prog.asm
prints formatted source to stdout.

Formatter config files (`--fmt-config`) currently support these keys:

```toml
[formatter]
profile = "safe-preserve"            # only supported profile in Phase 1
preserve_line_endings = true
preserve_final_newline = true
label_alignment_column = 8           # alias: code_column
max_consecutive_blank_lines = 1      # alias: max_blank_lines
align_unlabeled_instructions = true  # align unlabeled opcodes to code column (data directives also align)
split_long_label_instructions = true  # if label exceeds column, move mnemonic to next line
label_colon_style = "keep"           # keep|with|without
directive_case = "keep"              # keep|upper|lower
label_case = "keep"                  # keep|upper|lower
mnemonic_case = "keep"               # keep|upper|lower (alias: opcode_case)
register_case = "keep"               # keep|upper|lower
hex_literal_case = "keep"            # keep|upper|lower
```

For an 8-space mnemonic column with long-label wrapping:

```toml
[formatter]
label_alignment_column = 8
align_unlabeled_instructions = true
split_long_label_instructions = true
```

`--fmt-config` uses strict validation: unknown keys, duplicate keys, invalid
values, and unsupported profile values are reported as formatter errors.
Without `--fmt-config`, opForge always uses built-in formatter defaults and does
not auto-discover `.opforgefmt.toml`.
V2 note: `label_case` is planned to become symbol-aware so label usage tokens
are case-normalized alongside label definitions.

## Linker Regions Workflow

Use explicit region placement and output directives for section-based builds.

Minimal flow:

```asm
.module main

.region ram, $1000, $10ff

.section code
start:
    .byte $42, $43
.endsection

.place code in ram

.output "build/minimal.bin", format=bin, sections=code
.mapfile "build/minimal.map", symbols=public
.exportsections dir="build/minimal_sections", format=bin

.endmodule
```

Grouped placement flow:

```asm
.pack in rom : code, data, zero
.output "build/full.prg", format=prg, contiguous=false, sections=code,data
.output "build/full-image.bin", format=bin, image="$8000..$8010", fill=$ff, contiguous=false, sections=code,data
```

Examples:
- `examples/linker_regions_minimal.asm`
- `examples/linker_regions_full.asm`

### Diagnostic + Fixit Examples

Directive typo diagnostics with machine-applicable fixits:
- [examples/directive_typo_endif_fixit_error.asm](examples/directive_typo_endif_fixit_error.asm) → [examples/reference/directive_typo_endif_fixit_error.err](examples/reference/directive_typo_endif_fixit_error.err)
- [examples/directive_typo_elseif_fixit_error.asm](examples/directive_typo_elseif_fixit_error.asm) → [examples/reference/directive_typo_elseif_fixit_error.err](examples/reference/directive_typo_elseif_fixit_error.err)
- [examples/directive_typo_endmodule_fixit_error.asm](examples/directive_typo_endmodule_fixit_error.asm) → [examples/reference/directive_typo_endmodule_fixit_error.err](examples/reference/directive_typo_endmodule_fixit_error.err)
- [examples/directive_typo_endsection_fixit_error.asm](examples/directive_typo_endsection_fixit_error.asm) → [examples/reference/directive_typo_endsection_fixit_error.err](examples/reference/directive_typo_endsection_fixit_error.err)
- [examples/directive_typo_endmatch_fixit_error.asm](examples/directive_typo_endmatch_fixit_error.asm) → [examples/reference/directive_typo_endmatch_fixit_error.err](examples/reference/directive_typo_endmatch_fixit_error.err)

Dialect-oriented diagnostics with mnemonic replacement suggestions:
- [examples/dialect_mnemonic_fixit_error.asm](examples/dialect_mnemonic_fixit_error.asm) → [examples/reference/dialect_mnemonic_fixit_error.err](examples/reference/dialect_mnemonic_fixit_error.err)
- [examples/dialect_parser_fixit_error.asm](examples/dialect_parser_fixit_error.asm) → [examples/reference/dialect_parser_fixit_error.err](examples/reference/dialect_parser_fixit_error.err)
