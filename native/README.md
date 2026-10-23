# Native opForge

This tree contains opForge deliverables assembled by opForge and run on supported
targets. The AmigaOS 68020 assembler is the maintained native product. Its entry
is [`motorola68000/amigaos/main.asm`](motorola68000/amigaos/main.asm); the entry
selects `.cpu 68020`, includes the maintained external-only package catalog, and
declares the Hunk output `build/opforge`.

Build the executable and package assets with the maintained package builder. Use
a fresh absolute output directory whose parent already exists:

```sh
cargo run -p asm --example build_native_packages -- \
  --out-dir /tmp/opforge-native-build-20261023
```

The builder retains the assembly output at `build/opforge` and copies the
deployable executable to `opforge` at the top of that directory, beside
`packages/`. Disk-loaded packages are the default: ship that executable and its
sibling package directory together. Add `--embed m68020` to opt into embedding
the m68020 package in the executable; any non-embedded target packages remain in
`packages/`. The checked-in `packages/catalog.i` contains catalog metadata only.
Directly assembling `main.asm` uses that external-only catalog and therefore
needs the matching package files in its package root. The Rust product CLI and
its command-line behavior are unchanged by this native promotion.

Host package generation shares the Rust hierarchy/runtime model.
[`native_runtime_package.rs`](../crates/opforge-asm/src/native_runtime_package.rs)
lowers a selected target into the BS38 binary consumed by the native assembler;
[`native_package_build.rs`](../crates/opforge-asm/src/native_package_build.rs)
handles package files, catalogs and embedding configuration. Package generation
does not consume project source and currently runs on the Rust host.

Normal native builds use one module object directory, `build/ofm`, or the path
selected by `--incremental-dir`. The linker reads the same `.ofm` files used for
incremental reuse. Unchanged modules leave their object files untouched; stale
modules replace their existing files after recompilation. A companion `.ofi`
records timestamps and publication metadata, and is published after the complete
object. Builds no longer accumulate `objects-1`, `objects-2`, etc. Capture and
preparation scratch is separate from this persistent directory, and defaults to
`build/scratch`. Both directories are relative to the current working directory
and are created when needed. `--capture-dir DIR` overrides temporary storage.

From an exported bundle's directory, `opforge src` builds `src/main.asm` using
its `.cpu` and `.output` declarations. `opforge src/main.asm` also works. The
root file's directory supplies the default module and include search root;
explicit `-M`/`-I` paths add search locations. Product debug includes are relative
to their containing files, so no separate `-I src/debug` is required. Ordinary
CLI builds use the bounded module pipeline and reuse `build/ofm` automatically.

The product covers a substantial shared assembler and package-backed runtime
surface. Earlier focused Rust parity and full-source self-host evidence are
recorded in the linked plans for their identified source snapshots. Promotion
does not establish complete Rust language, CPU-family,
or output parity. In particular, full opcore and CPU-family parity and the 2 MiB
memory target remain open. The live parity contract is
[documented here](../agents/rules/native-rust-parity-porting.md); current
completion status and qualified limits are in the
[native assembler plan](../documentation/plans/native-runtime-reset.md).

## Source layout

- `motorola68000/amigaos/main.asm`: native product composition root.
- `motorola68000/amigaos/cli/`: command-line parsing, input selection, target
  selection and progress reporting.
- `motorola68000/amigaos/assembler/`: source acquisition, preparation, symbols,
  values, language processing, modules, layout and output.
- `motorola68000/amigaos/vm/`: tokenizer, parser, expression, encoding and value
  virtual machines.
- `motorola68000/amigaos/packages/`: maintained CPU/package catalog and package
  loading and validation.
- `motorola68000/amigaos/platform/`: operating-system and memory services.
- `motorola68000/amigaos/debug/`: bounded diagnostics, assertions and profiling.
- `motorola68000/amigaos/tests/`: test entrypoints and harnesses, not product
  executables.

Module names use the `opforge.*` namespace. Short local aliases are used at
composition boundaries. Module semantic exports are owned by
`assembler/modules/semantic_exports.asm`; the assembler-facing interface is
`assembler/modules/assembly_interface.asm`.

## Validation and performance records

The default external-package release completes full self-hosts using exactly
`opforge src --rebuild` cold and `opforge src` warm. Under 8 MiB total RAM,
installed Workbench with a plain background and a 64 KiB stack, cold takes
**279 s**, incremental **58 s**. All 190 modules rebuild cold and all 190 are
reused warm from one `build/ofm` set. Both exit zero and produce the full
**348,824-byte executable exactly matching live Rust**, using the separate
478,042-byte m68020 package. Loading that package still consumes RAM; peak heap
was not remeasured. These are FS-UAE 68020/unrestricted-speed durations.

The preceding external-package snapshot (348,752-byte executable) also passes
on the physical A6000: **1,066 s cold (17m 46s)** and **90 s incremental**, with
exact complete Rust output matches and all 190 modules reused warm. Hardware
RAM was unrestricted. This new bare-CLI bundle awaits physical A6000 testing.
See the [current qualification and remaining limits](../documentation/plans/native-runtime-reset.md#current-product-state).
Earlier hardware and memory measurements identify their own source snapshots.

For manual A6000 testing, upload an exported bundle without starting assembly:

```sh
python3 scripts/performance/run_a6000_incremental.py --bundle DIR --copy-only
```

This discovers the host with `ash -l`, copies to `Development:`, verifies the
transferred files, and prints the on-device directory and commands. It also
copies `run-selfhost` for optional execution on the device. The single-build
runner `run_a6000_selfhost.py` accepts the same option. Copying files does not
establish native completion or output parity.

Native Motorola 68000 assembly formatting is checked with:

```sh
make native-68000-format-check
```

Focused native and emulator checks, including the required fresh-run proof for
completion claims, are described in the
[native parity contract](../agents/rules/native-rust-parity-porting.md) and the
[workflow guide](../documentation/workflow/README.md).
