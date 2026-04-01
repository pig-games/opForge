# Review Report

## Scope

Full review of the checked-out opForge worktree on branch `codex/validate-68000-spec-and-plan`; review is not limited to the branch diff against `main`.

## Findings

### RVW-2026-04-01-001

- Severity: medium
- File: `crates/opforge-families/src/m68020/handler.rs:791-800`, `crates/opforge-families/src/m68020/handler.rs:923-932`
- Issue: Two duplicated `if !source_ok` guards make the later span-aware error paths unreachable in the m68020 FPU encoder.
- Why it matters: Invalid source effective addresses for these FPU forms currently return a generic error string instead of the intended location-aware diagnostic, which makes operand errors materially harder to debug and leaves dead error-handling code in a correctness-critical encoder path.
- Fix direction: Remove the first `if !source_ok` branch at each site and keep the later `EncodeResult::error_with_span(...)` branch so invalid source operands report the specific source span.

### RVW-2026-04-01-002

- Severity: low
- File: `crates/opforge-asm/src/external_oracle.rs:351-366`, `crates/opforge-asm/src/external_oracle.rs:431-459`, `crates/opforge-asm/src/external_oracle.rs:1284-1293`, `crates/opforge-asm/src/external_oracle.rs:1748-1760`
- Issue: External-oracle sidecar reports are keyed only by `fixture.source_path`, while manifest validation enforces unique fixture IDs but does not reject multiple fixtures that resolve to the same source path.
- Why it matters: When two fixtures share one source file, a later passing fixture can delete or overwrite a failing fixture's `*.mismatch.txt`, `*.bytes_diff.txt`, or `*.error_report.txt` sidecar, leaving misleading local triage artifacts and hiding the real failing case.
- Fix direction: Reject manifests that contain multiple fixtures resolving to the same canonical source path before execution begins.

### RVW-2026-04-01-003

- Severity: low
- File: `AGENTS.md:12-13`, `AGENTS.md:36-40`, `AGENTS.md:61-79`, `AGENTS.md:87-88`, `AGENTS.md:109-123`
- Issue: The worktree guidance file hardcodes contributor-specific absolute filesystem paths in agent, template, and helper-script references.
- Why it matters: Those references break as soon as the repository is checked out in a different worktree, on another machine, or in CI, which makes the branch-local workflow instructions unreliable for the exact automation they are supposed to drive.
- Fix direction: Replace the absolute references with repository-relative paths throughout `AGENTS.md`.

## Testing Gaps

- No targeted test exercises the invalid effective-address path for the affected m68020 FPU encoders, so the dead span-aware error branches in `handler.rs` were able to persist unnoticed.
- The external-oracle tests do not cover two fixtures sharing the same canonical source file while verifying that sidecar reports remain attributable to the failing fixture.
- The branch-local workflow files are not validated in a path-portable environment, so broken absolute references in `AGENTS.md` are not caught by automation.

## Residual Risks

- `cargo test --quiet` passed in this review session, but the external-oracle `vasm`-backed paths remain environment-dependent and were not independently revalidated against a live oracle during this pass.
- This is a large multi-crate workspace with parser, package-codec, VM, FFI, and assembler subsystems; the final report includes only issues that were concretely evidenced during inspection and does not claim exhaustive proof of absence of defects elsewhere.

## Brief Summary

I reviewed the full checked-out worktree rather than the branch diff and found one medium-severity functional issue plus two low-severity workflow/test-infrastructure issues. The most important bug is in the m68020 FPU encoder, where duplicated guards suppress the intended span-aware diagnostic for invalid source operands. The remaining findings concern external-oracle sidecar report collisions when fixtures reuse a source file and non-portable absolute paths in the repository's own workflow guidance. Existing automated tests passed in this environment.
