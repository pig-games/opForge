# Finding Closure Report

## Finding

- ID: `RVW-2026-05-06-001`
- Original summary: the tkpkg ABI advertised `ENTRY_ORD_PARSE_LINE`, but the
  dispatcher routed it to deferred-runtime error handling while the native CLI
  bypassed tkpkg and called `prvm_route_line_68000` directly.

## Claimed Fix

- Plan item: Item 1 - Route native parse through tkpkg service and remove the
  direct CLI shortcut.
- Implementation slice or commit: pre-commit Item 1 native parse service slice
  on `main`
- Changed files:
  - `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm`
  - `native/motorola68000/amigaos/opforge-cli/opforge_cli.asm`
  - `crates/opforge-asm/src/tests.rs`

## Validation Evidence

- Command or check: implementation diff inspection for the Item 1 slice
- Result: PASS; `ENTRY_ORD_PARSE_LINE` now dispatches to
  `tkpkgServiceHandleParseLine`, which validates a PRVM route-frame payload and
  invokes `prvm_route_line_68000` from the tkpkg service module.
- Command or check: implementation diff inspection for the native CLI shortcut
- Result: PASS; `opforge_cli.asm` no longer imports or calls
  `prvm_route_line_68000` directly, and its parse envelope sends the prepared
  PRVM route frame through `tkpkg_service_dispatch_v1` with
  `ENTRY_ORD_PARSE_LINE`.
- Command or check: `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`
- Result: PASS; all 31 focused tkpkg tests passed, including the composed
  tkpkg entry and debug CLI assembly surfaces.
- Command or check: `cargo test -p asm motorola68020_prvm_line_router_ -- --nocapture`
- Result: PASS; all 6 focused PRVM line-router tests passed.
- Command or check: `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`
- Result: PASS; all 4 focused native CLI tests passed.
- Command or check: `scripts/workflow/run_rust_quality_gate.sh`
- Result: PASS; the canonical Rust quality gate completed successfully.

## Closure Status

- Status: fixed
- Residual risk: low; this slice keeps the existing PRVM route-frame and parser
  result contract intact while moving the live parse entrypoint behind tkpkg.
- Closure rationale: the advertised parse ABI entrypoint is now live in the
  tkpkg dispatcher, the production CLI no longer owns a private PRVM parse
  shortcut, and focused plus full Rust validation stayed green.

## Notes

- The Rust assembly harness now supplies the sibling native PRVM module path for
  tkpkg entry/debug assemblies because `tkpkg.service` legitimately imports the
  PRVM line router.