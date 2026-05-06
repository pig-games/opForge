# Native opForge Implementations

This tree contains opForge deliverables that are built with opForge itself and
run natively on one of opForge's supported targets.

These sources are intentionally separate from `examples/`. Example programs are
small instructional or fixture-oriented assembly inputs; native implementations
are product/runtime code whose host environment is itself an opForge target.

Current layout:

- `motorola68000/amigaos/opforge-cli/`: native AmigaOS opForge CLI entry point
  and package fixture.
- `motorola68000/amigaos/opcore/`: native opCore support modules.
- `motorola68000/amigaos/prvm/`: native parser VM modules and smoke harnesses.
- `motorola68000/amigaos/tkpkg/`: native package-backed tokenizer/runtime
  modules and debug harness.
- `motorola68000/amigaos/tokvm/`: native tokenizer VM modules and sample input.
