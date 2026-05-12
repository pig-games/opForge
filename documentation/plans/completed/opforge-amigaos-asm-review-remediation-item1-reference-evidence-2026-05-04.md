# AmigaOS ASM Remediation Item 1 Reference Evidence

## Scope

This evidence belongs to Item 1 of the AmigaOS assembler review remediation
plan for `RVW-2026-05-04-006`.

## Allowed Reference Updates

The following reference/golden artifacts were refreshed intentionally after
adding chunk-end bounds checks to the tkpkg native pipeline and token-policy
walkers. The assembled bytes changed because the runtime now checks selected
chunk bounds before reading or skipping package records.

- `examples/reference/motorola68000/amigaos/opforge/opforge_cli.hunk`
- `examples/reference/motorola68000/amigaos/opforge/opforge_cli.lst`
- `examples/reference/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.hunk`
- `examples/reference/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.lst`

## Refresh Command

```sh
scripts/workflow/update_references.sh \
  examples/reference/motorola68000/amigaos/opforge/opforge_cli.hunk \
  examples/reference/motorola68000/amigaos/opforge/opforge_cli.lst \
  examples/reference/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.hunk \
  examples/reference/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.lst \
  -- cargo test -p asm examples_match_reference_outputs -- --nocapture
```

## Result

Passed: `examples_match_reference_outputs` completed successfully with the
explicit allowlisted wrapper.
