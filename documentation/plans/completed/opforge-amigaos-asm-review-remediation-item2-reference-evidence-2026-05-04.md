# Item 2 Reference Refresh Evidence

## Scope

This evidence belongs to Item 2 of
`dev-docs/reviews/opforge_amigaos_asm_review_remediation_plan_2026-05-04.md`.

## Allowed Reference Updates

The active pipeline identifier capacity guard changes the native tkpkg runtime
bytes and listings used by AmigaOS reference examples. The following reference
artifacts were refreshed after verifying the focused Item 2 tkpkg tests:

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

Passed: `examples_match_reference_outputs` completed successfully with update
mode enabled through the explicit allowlisted wrapper.
