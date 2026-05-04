# Native AmigaOS 68020 Tkpkg Optimization Remediation Plan

## Metadata

- Source: `documentation/reviews/opforge-native-amigaos-68020-optimization-review-2026-05-03.md`
- Mode: remediation
- Owner: GitHub Copilot

## Objective

Implement the reviewed low-risk 68020 native tkpkg optimizations without
widening scope beyond the six recorded findings. The work must preserve current
package-resolution behavior while reducing instruction count in fixed-record
scan, copy, and clear paths.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to the review findings `RVW-2026-05-03-001` through
  `RVW-2026-05-03-006`.
- Alignment-sensitive longword clear/copy changes must verify or establish
  aligned storage before replacing byte operations.
- Validation must keep the native tokenizer/package path green; do not trade
  behavior for code-size cleanup.

## Version Impact

- Affected component(s): native AmigaOS `tkpkg` package loader and pipeline
  resolver under `examples/motorola68000/amigaos/tkpkg/`
- Impact class: internal optimization only
- Owned contract: native package loading, pipeline resolution, token-policy
  resolution, and tokenizer-VM selection behavior remain unchanged
- Rationale: the reviewed changes reduce code size and runtime work in fixed
  record handling without changing public CLI or package data contracts

## Work Items

- [x] Item 1
  - Source requirement or finding IDs: `RVW-2026-05-03-001`, `RVW-2026-05-03-002`, `RVW-2026-05-03-006`
  - Expected files: `examples/motorola68000/amigaos/tkpkg/tkpkg_token_policy.asm`, `examples/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm`
  - Full quality gates: `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`; `cargo test -p vm motorola68000_tokenizer_vm_staged_corpus_matches_host_for_example_lines -- --nocapture`; `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_tkpkg_native_motorola68000_family_corpus_matches_vm_authoritative_rows -- --nocapture`; all three checks must pass for the slice to be complete
  - Plan-compliance review evidence: one slice summary limited to scan-path skip consolidation and `MOVEA.L` address-register copies, plus the focused validation result
  - Commit outcome: the native tkpkg scan paths use single-displacement `LEA` for fixed record skips and `MOVEA.L` for address-register copies where no indexed address calculation is required
  - Definition of done: findings `001`, `002`, and `006` are fully addressed with no behavior changes and no incidental refactors outside the reviewed tkpkg resolver paths

- [ ] Item 2
  - Source requirement or finding IDs: `RVW-2026-05-03-003`, `RVW-2026-05-03-004`
  - Expected files: `examples/motorola68000/amigaos/tkpkg/tkpkg_token_policy.asm`, `examples/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm`, any shared native buffer-layout file required to guarantee aligned locator storage
  - Full quality gates: `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`; `cargo test -p vm motorola68000_tokenizer_vm_staged_corpus_matches_host_for_example_lines -- --nocapture`; `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_tkpkg_native_motorola68000_family_corpus_matches_vm_authoritative_rows -- --nocapture`; all three checks must pass after confirming caller post-increment semantics remain unchanged
  - Plan-compliance review evidence: alignment justification, caller-side effect audit, and focused validation result
  - Commit outcome: locator records are explicitly aligned and fixed four-byte locator clear/copy helpers use longword operations with preserved semantics
  - Definition of done: findings `003` and `004` are fully addressed, the storage alignment is explicit in code, and the record helpers still feed the same resolver/commit paths correctly

- [ ] Item 3
  - Source requirement or finding IDs: `RVW-2026-05-03-005`
  - Expected files: `examples/motorola68000/amigaos/tkpkg/tkpkg_package_loader.asm`, any shared native buffer-layout file required to guarantee aligned package-state clearing
  - Full quality gates: `cargo test -p asm motorola68020_tkpkg_ -- --nocapture`; `cargo test -p vm motorola68000_tokenizer_vm_staged_corpus_matches_host_for_example_lines -- --nocapture`; `OPFORGE_FS_UAE_SMOKE=1 OPFORGE_FS_UAE_BIN='/Applications/FS-UAE.app/Contents/MacOS/fs-uae' OPFORGE_FS_UAE_CONFIG_TEMPLATE='/Users/erik/Documents/FS-UAE/Configurations/opforge-tkpkg-test.fs-uae' OPFORGE_FS_UAE_ARGS='{fsuae_config}' cargo test --manifest-path crates/opforge-asm/Cargo.toml external_fs_uae_tkpkg_native_motorola68000_family_corpus_matches_vm_authoritative_rows -- --nocapture`; all three checks must pass after the aligned 160-byte clear-loop replacement
  - Plan-compliance review evidence: aligned clear-region evidence, loop-size calculation evidence, and focused validation result
  - Commit outcome: package loaded-state reset uses a longword clear loop over the fixed state region with unchanged reset coverage
  - Definition of done: finding `005` is fully addressed, the cleared region boundary is explicit, and package load/reset behavior remains unchanged

## Milestones

- [x] Milestone 1: scan-path skip and address-copy optimizations landed
- [ ] Milestone 2: aligned locator-record longword clear/copy optimizations landed
- [ ] Milestone 3: aligned package-state longword clear optimization landed

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` passes before commit
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping