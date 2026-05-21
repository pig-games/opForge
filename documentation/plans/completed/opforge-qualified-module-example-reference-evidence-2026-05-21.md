# Qualified Module Example Reference Evidence

## Scope

This evidence belongs to the post-implementation example coverage for
`documentation/plans/completed/opforge-module-qualified-use-section-mapping-implementation-plan-v0_1-completed-2026-05-20T205411Z.md`.

## Allowed Reference Updates

The new `module_qualified_section_map.asm` fixture intentionally adds checked-in
reference coverage for qualified selective imports, logical section maps, and
reachable-unit output. The reference refresh is scoped to the new example's
payload and listing:

- `examples/reference/opcore/module_qualified_section_map.hex`
- `examples/reference/opcore/module_qualified_section_map.lst`

## Refresh Command

```sh
scripts/workflow/update_references.sh \
  examples/reference/opcore/module_qualified_section_map.hex \
  examples/reference/opcore/module_qualified_section_map.lst \
  -- cargo test -p asm examples_match_reference_outputs -- --nocapture
```

## Result

Completed: the explicit allowlisted wrapper generated the new opcore reference
payload and listing. The wrapper also detected an unrelated existing
`6502_native_cli_smoke.lst` header drift outside the allowlist; that generated
change was restored, and only the two allowlisted opcore reference files remain
changed for this slice.
