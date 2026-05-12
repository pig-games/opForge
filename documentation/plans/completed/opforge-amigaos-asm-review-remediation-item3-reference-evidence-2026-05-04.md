# Item 3 Reference Refresh Evidence

## Scope

This evidence belongs to Item 3 of
`dev-docs/reviews/opforge_amigaos_asm_review_remediation_plan_2026-05-04.md`.

## Allowed Reference Updates

The selected TKVM record bound checks add native tokenizer VM decode
instructions to the AmigaOS tkpkg runtime. The FS-UAE validation also exposed a
native token-policy locator preservation bug in the same package-backed runtime
path; preserving the incoming owner locator changed the embedded native bytes.
The assembled bytes and listings changed only for the examples that embed that
runtime:

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
