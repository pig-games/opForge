# opForge v0.9.7 Release Notes

## Scope

This release is centered on the Motorola 68000-family expansion that landed on
this branch: the shipped assembler surface now spans `68000`, `68010`,
`68020`, `68030`, and `68040`, adds selector-driven FPU support, adds the
currently supported narrow MMU slice, and introduces external-oracle
infrastructure for curated parity checks against existing assemblers.

## Highlights

- Shipped the Motorola 68000-family CPU lineage from `68000` through `68040`,
  including the later-family addressing and instruction deltas needed for
  `68010`, `68020`, `68030`, and `68040`.
- Added selector-driven FPU support with `.fpu 68881`, `.fpu 68882`, and
  `.fpu 68040`, plus example/reference coverage for both external and
  integrated FPU paths.
- Added the current narrow MMU surface for the 68000 family: `PFLUSH` on
  `68030` and `68040`, plus the already-shipped `68040` MMU-related `MOVEC`
  control-register surface.
- Introduced external-oracle A/B infrastructure, curated `vasm` fixture sets,
  and contributor guidance for installing and using the local `vasm` wrappers.
- Closed branch review/remediation follow-up with additional legality,
  divergence, and parity fixes across the new 68k surface.

## Added

- New Motorola 68000-family CPU targets: `68010`, `68020`, `68030`, and
  `68040`, with the corresponding `m680xx` and `mc680xx` aliases.
- `68020+` full-extension addressing support on `68020`, `68030`, and `68040`.
- New later-family instruction coverage including representative `MOVEC`,
  `MOVES`, `RTD`, `CALLM`, `RTM`, `CAS`, `CAS2`, `CHK2`, `CMP2`, bit-field,
  `MOVE16`, and long integer instruction slices where architecturally legal.
- Selector-driven FPU support:
  - `.fpu 68881` and `.fpu 68882` on `68020` and `68030`
  - `.fpu 68040` on `68040`
- Curated 68k MMU/FPU examples and references such as
  `68020_fpu_allmodes`, `68020_fpu_instruction_catalog`,
  `68030_pflush_external_fpu`, `68040_integrated_fpu`, and
  `68040_movec_mmu_registers`.
- External-oracle infrastructure in `opforge-asm` plus curated `vasm`
  differential fixtures for Motorola 68000-family and MOS 6502-family
  coverage.
- Contributor documentation for installing `vasm` and the `opforge-vasm68k` /
  `vasm68k` wrappers used by the oracle workflow.

## Changed

- The public README and reference manual now describe the shipped Motorola
  68000-family support as a current capability set, including the later-family
  CPU lineage, `.fpu` selection rules, and the intentionally narrow MMU scope.
- The 68k example and reference corpus was expanded substantially so canonical
  addressing, aliases, later-family deltas, FPU selection, and MMU smoke
  coverage are visible in checked-in source and golden outputs.
- The external-oracle workflow now distinguishes shared parity, negative
  parity, and documented divergence fixtures instead of treating all
  cross-assembler differences the same way.

## Fixed

- Review-remediation fixes tightened later-family legality and parity behavior,
  including representative `CMP`, PC-relative scalar/data legality, long-divide
  aliases, `FNOP`, `CALLM`/`RTM`, and `MOVEC` matrix behavior.
- The external-oracle workflow and curated `vasm` corpora now classify several
  previously misbucketed 68k fixtures correctly as positive, negative, or
  documented-divergence cases.
- Public docs and release surfaces no longer understate the shipped `68000`
  through `68040` support or describe the new 68k family work as merely
  planned.

## Validation

Release validation for this branch included:

- `cargo fmt --all`
- `cargo clippy --workspace -- -D warnings`
- `cargo audit`
- `make test`
- focused 68k assembler, example/reference, and external-oracle test slices
- focused version-surface validation such as `cargo test version_flag_reports_build_profile`
- focused golden-output validation such as `cargo test -p asm examples_match_reference_outputs -- --nocapture`

## Upgrade Notes

- `.cpu` now accepts the full shipped Motorola 68000-family lineage from
  `68000` through `68040`, including the corresponding `m680xx` and `mc680xx`
  aliases.
- FPU assembly is now explicit and selector-driven: use `.fpu 68881` or
  `.fpu 68882` on `68020`/`68030`, `.fpu 68040` on `68040`, and `.fpu none`
  to disable optional FPU acceptance on active 68k targets.
- MMU support remains intentionally narrow. Source that expects broad PMMU/MMU
  coverage beyond `PFLUSH` and the current `68040` MMU-related `MOVEC`
  register surface is still out of scope.
- The external oracle workflow expects a local `vasm` installation and the
  `opforge-vasm68k` wrapper when running the curated differential tests.
