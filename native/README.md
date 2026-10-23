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

The default external-package release completes full cold and unchanged incremental
self-hosts under 8 MiB total RAM, installed Workbench with a plain background, a 64 KiB
stack and disk-backed capture. Cold takes **282 s**, incremental **59 s**;
all 190 modules rebuild cold and all 190 are reused warm. Both exit zero and
produce the full **348,664-byte executable exactly matching live Rust**, using
the separate 478,042-byte m68020 package. Loading that package still consumes RAM;
the executable size reduction is not a peak-memory measurement.
These are FS-UAE 68020/unrestricted-speed durations; A6000 validation of this
bundle is pending. Peak heap was not remeasured. See the
[current qualification and remaining limits](../documentation/plans/native-runtime-reset.md#current-product-state).
Earlier hardware and memory measurements identify their own source snapshots.

Native Motorola 68000 assembly formatting is checked with:

```sh
make native-68000-format-check
```

Focused native and emulator checks, including the required fresh-run proof for
completion claims, are described in the
[native parity contract](../agents/rules/native-rust-parity-porting.md) and the
[workflow guide](../documentation/workflow/README.md).
