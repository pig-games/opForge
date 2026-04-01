# Full 680x0 external-oracle corpus expansion plan

## Metadata

- Source: user request to expand the A/B fixtures to the full Motorola 680x0 family (`68000` through `68040`) with separate corpora per CPU and optional FPU/MMU profile; existing external-oracle proposal constraints in `documentation/opForge-external-assembler-ab-testing-proposal-v0_1.md`
- Mode: `implementation`
- Owner: Copilot

## Objective

Expand the current narrow `68000`/`68010` external-oracle corpus into a family-wide Motorola 680x0 corpus with separate manifests per CPU and optional FPU/MMU profile, while keeping the harness opt-in, skip-safe, and compatible with the existing default `examples/reference` and workspace test paths.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at all times during plan execution.
- The default local test path and default CI/workspace path must remain dependency-free and must not require `vasm`.
- External-oracle fixtures must remain under `examples/ab/...` and must not be merged into `examples/reference`.
- Separate manifests/corpora per CPU and optional FPU/MMU profile are required for this expansion.
- Existing repo examples under `examples/motorola68000/` are the primary source for curated fixture derivation.
- The current `vasm` adapter must grow beyond `68010` before new family manifests can execute end to end.
- FPU and some MMU surfaces already exist in repo coverage, but opForge still intentionally rejects or defers parts of that surface; those gaps must stay visible through `documented_divergence` fixtures rather than being hidden.
- Every slice must explicitly prove that `cargo test -p asm examples_match_reference_outputs` remains clean.

## Planning decisions captured up front

- Manifest layout will move from one monolithic family file to separate manifest roots per CPU/profile so fixture growth stays reviewable and commit-sized.
- Integer corpora will be seeded before FPU/MMU-profile corpora.
- FPU and MMU profile corpora will use `documented_divergence` fixtures where opForge intentionally rejects or defers behavior that `vasm` accepts.
- Workflow validation stays incremental: every relevant slice reruns the default reference guard, and the explicit opt-in entrypoint stays green as manifests are added.

## Work Items

- [x] Work item 1: add per-CPU/profile manifest discovery plus profile-capable `vasm` plumbing through `68040`
  - Source requirement or finding IDs: user request (`68000`-`68040`, separate corpora per CPU/profile), `REQ-EXTAB-003`, `REQ-EXTAB-007`, `REQ-EXTAB-010`, `DC-EXTAB-004`, `DC-EXTAB-005`
  - Expected files:
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/oracle/vasm.rs`
    - `crates/opforge-asm/src/tests.rs`
    - `examples/ab/motorola68000/vasm/**/fixtures.toml`
    - one minimal profile-specific manifest root proving non-CPU-only discovery/execution
  - Full quality gates:
    - add tests covering multi-manifest discovery and stable per-manifest execution ordering
    - add tests proving `vasm` CPU-flag support for `68020`, `68030`, and `68040`
    - add tests proving profile-specific oracle command construction/execution works for a non-default manifest root before MMU/FPU corpus seeding starts
    - run `cargo test -p asm external_oracle_`
    - run `make test-external-oracle OPFORGE_VASM_BIN=<configured local vasm path>`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to manifest-layout/discovery changes, profile-capable oracle plumbing, and adapter CPU support only
    - compliance note explicitly states that broad corpus seeding is deferred to later commits
  - Commit outcome:
    - the harness can execute separate per-CPU/profile manifests, construct profile-aware oracle runs, and the `vasm` adapter can target `68000` through `68040`
  - Definition of done:
    - one manifest no longer has to carry the whole family
    - the harness can locate and run multiple manifests deterministically
    - the adapter no longer hard-stops at `68010`
    - profile-aware oracle execution is proven before MMU/FPU corpus seeding begins
    - default reference behavior remains unchanged

- [x] Work item 2: seed the curated `68000` integer corpus
  - Source requirement or finding IDs: user request (`whole family`), `REQ-EXTAB-001`, `REQ-EXTAB-002`, `REQ-EXTAB-004`, `REQ-EXTAB-007`, `DC-EXTAB-001`, `DC-EXTAB-002`
  - Expected files:
    - `examples/ab/motorola68000/vasm/68000/fixtures.toml`
    - `examples/ab/motorola68000/vasm/68000/**/*.asm`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - derive fixtures from existing `examples/motorola68000/68000_*` sources
    - run `cargo test -p asm external_oracle_`
    - run `make test-external-oracle OPFORGE_VASM_BIN=<configured local vasm path>`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the `68000` corpus only
    - compliance note explicitly states that `68010+` family slices remain deferred
  - Commit outcome:
    - the A/B corpus contains a dedicated `68000` manifest with curated integer coverage
  - Definition of done:
    - a separate `68000` manifest exists and runs
    - the fixture set is sourced from existing repo examples rather than ad hoc new coverage

- [x] Work item 3: seed the curated `68010` integer corpus
  - Source requirement or finding IDs: user request (`whole family`), `REQ-EXTAB-001`, `REQ-EXTAB-002`, `REQ-EXTAB-004`, `REQ-EXTAB-007`
  - Expected files:
    - `examples/ab/motorola68000/vasm/68010/fixtures.toml`
    - `examples/ab/motorola68000/vasm/68010/**/*.asm`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - derive fixtures from existing `68010_delta.asm` and related repo-backed surfaces
    - run `cargo test -p asm external_oracle_`
    - run `make test-external-oracle OPFORGE_VASM_BIN=<configured local vasm path>`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the `68010` corpus only
    - compliance note explicitly states that `68020+` slices remain deferred
  - Commit outcome:
    - the A/B corpus contains a dedicated `68010` manifest with curated integer coverage
  - Definition of done:
    - a separate `68010` manifest exists and runs
    - `68010`-specific deltas are represented explicitly in the corpus

- [x] Work item 4: seed the curated `68020` integer corpus
  - Source requirement or finding IDs: user request (`whole family`), `REQ-EXTAB-001`, `REQ-EXTAB-002`, `REQ-EXTAB-004`, `REQ-EXTAB-007`
  - Expected files:
    - `examples/ab/motorola68000/vasm/68020/fixtures.toml`
    - `examples/ab/motorola68000/vasm/68020/**/*.asm`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - derive fixtures from existing `68020_*` repo examples, including later-family integer/addressing surfaces
    - run `cargo test -p asm external_oracle_`
    - run `make test-external-oracle OPFORGE_VASM_BIN=<configured local vasm path>`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the `68020` integer corpus only
    - compliance note explicitly states that `68030`, `68040`, FPU, and MMU slices remain deferred
  - Commit outcome:
    - the A/B corpus contains a dedicated `68020` manifest with curated integer coverage
  - Definition of done:
    - a separate `68020` manifest exists and runs
    - `68020`-specific integer/addressing surfaces are visible in the corpus

- [x] Work item 5: seed the curated `68030` integer corpus
  - Source requirement or finding IDs: user request (`68030`), `REQ-EXTAB-001`, `REQ-EXTAB-002`, `REQ-EXTAB-004`, `REQ-EXTAB-007`
  - Expected files:
    - `examples/ab/motorola68000/vasm/68030/fixtures.toml`
    - `examples/ab/motorola68000/vasm/68030/**/*.asm`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - derive fixtures from existing `68030_*` repo examples
    - run `cargo test -p asm external_oracle_`
    - run `make test-external-oracle OPFORGE_VASM_BIN=<configured local vasm path>`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the `68030` integer corpus only
    - compliance note explicitly states that `68040`, MMU, and FPU slices remain deferred
  - Commit outcome:
    - the A/B corpus contains a dedicated `68030` manifest with curated integer coverage
  - Definition of done:
    - a separate `68030` manifest exists and runs
    - `68030` carry-forward coverage is represented explicitly in the corpus

- [x] Work item 6: seed the curated `68040` integer corpus
  - Source requirement or finding IDs: user request (`68040`), `REQ-EXTAB-001`, `REQ-EXTAB-002`, `REQ-EXTAB-004`, `REQ-EXTAB-007`
  - Expected files:
    - `examples/ab/motorola68000/vasm/68040/fixtures.toml`
    - `examples/ab/motorola68000/vasm/68040/**/*.asm`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - derive fixtures from existing `68040_*` repo examples such as MOVE16 and family-delta surfaces
    - run `cargo test -p asm external_oracle_`
    - run `make test-external-oracle OPFORGE_VASM_BIN=<configured local vasm path>`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the `68040` integer corpus only
    - compliance note explicitly states that MMU and FPU-profile slices remain deferred
  - Commit outcome:
    - the A/B corpus contains a dedicated `68040` manifest with curated integer coverage
  - Definition of done:
    - a separate `68040` manifest exists and runs
    - `68040`-specific integer surfaces are visible in the corpus

- [x] Work item 7: add supported MMU profile corpora
  - Source requirement or finding IDs: user request (`supported MMU`), `REQ-EXTAB-006`, `REQ-EXTAB-007`, `DC-EXTAB-004`, `DC-EXTAB-005`
  - Expected files:
    - `examples/ab/motorola68000/vasm/68030-mmu/fixtures.toml`
    - `examples/ab/motorola68000/vasm/68040-mmu/fixtures.toml`
    - `examples/ab/motorola68000/vasm/68030-mmu/**/*.asm`
    - `examples/ab/motorola68000/vasm/68040-mmu/**/*.asm`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/external_oracle.rs` only if MMU-specific documented-divergence reporting needs new fields
  - Full quality gates:
    - cover repo-backed MOVEC/PFLUSH/MMU-register surfaces
    - verify intentional MMU acceptance/rejection gaps are represented as documented divergences, not ad hoc skips
    - run `cargo test -p asm external_oracle_`
    - run `make test-external-oracle OPFORGE_VASM_BIN=<configured local vasm path>`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to supported MMU profile corpora
    - compliance note explicitly states that FPU-profile slices remain deferred
  - Commit outcome:
    - supported MMU-related surfaces are visible in dedicated profile corpora
  - Definition of done:
    - MMU-related fixtures live in explicit profile manifests
    - intentional MMU deltas are visible through documented divergence

- [x] Work item 8: add the `68881` external FPU profile corpus
  - Source requirement or finding IDs: user request (`including FPU`), `REQ-EXTAB-005`, `REQ-EXTAB-006`, `REQ-EXTAB-007`, `REQ-EXTAB-010`, `DC-EXTAB-003`, `DC-EXTAB-004`
  - Expected files:
    - `examples/ab/motorola68000/vasm/68020-fpu-68881/fixtures.toml`
    - `examples/ab/motorola68000/vasm/68020-fpu-68881/**/*.asm`
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - derive fixtures from existing FPU catalog/register/addressing examples
    - use documented-divergence fixtures wherever opForge still defers 68881 encoding while `vasm` succeeds
    - run `cargo test -p asm external_oracle_`
    - run `make test-external-oracle OPFORGE_VASM_BIN=<configured local vasm path>`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the `68881` profile corpus and any strictly required reporting support
    - compliance note explicitly states that `68882` and integrated `68040` FPU remain deferred
  - Commit outcome:
    - the corpus contains a dedicated `68881` profile manifest with visible documented divergences for current opForge gaps
  - Definition of done:
    - a separate `68881` corpus exists and runs
    - unsupported/deferred 68881 surfaces stay visible rather than silently skipped

- [x] Work item 9: add the `68882` external FPU profile corpus
  - Source requirement or finding IDs: user request (`including FPU`), `REQ-EXTAB-005`, `REQ-EXTAB-006`, `REQ-EXTAB-007`, `REQ-EXTAB-010`, `DC-EXTAB-003`, `DC-EXTAB-004`
  - Expected files:
    - `examples/ab/motorola68000/vasm/68020-fpu-68882/fixtures.toml`
    - `examples/ab/motorola68000/vasm/68020-fpu-68882/**/*.asm`
    - `crates/opforge-asm/src/external_oracle.rs`
    - `crates/opforge-asm/src/tests.rs`
  - Full quality gates:
    - derive fixtures from existing FPU catalog/register/addressing examples where `68882` profile matters
    - use documented-divergence fixtures wherever opForge still defers 68882 encoding while `vasm` succeeds
    - run `cargo test -p asm external_oracle_`
    - run `make test-external-oracle OPFORGE_VASM_BIN=<configured local vasm path>`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to the `68882` profile corpus and any strictly required reporting support
    - compliance note explicitly states that integrated `68040` FPU remains deferred
  - Commit outcome:
    - the corpus contains a dedicated `68882` profile manifest with visible documented divergences for current opForge gaps
  - Definition of done:
    - a separate `68882` corpus exists and runs
    - unsupported/deferred 68882 surfaces stay visible rather than silently skipped

- [x] Work item 10: add the integrated `68040` FPU profile corpus and refresh explicit workflow coverage for the whole manifest set
  - Source requirement or finding IDs: user request (`68040`, `including FPU`), `REQ-EXTAB-006`, `REQ-EXTAB-007`, `REQ-EXTAB-010`, `DC-EXTAB-004`, `DC-EXTAB-005`
  - Expected files:
    - `examples/ab/motorola68000/vasm/68040-fpu/fixtures.toml`
    - `examples/ab/motorola68000/vasm/68040-fpu/**/*.asm`
    - `Makefile`
    - `crates/opforge-asm/src/tests.rs`
    - related user-facing notes only if the command surface changes materially
  - Full quality gates:
    - cover integrated `68040` FPU-supported and explicitly rejected coprocessor-only surfaces
    - ensure the opt-in workflow runs the whole expanded manifest set end to end
    - verify the dedicated external-oracle target still skips cleanly when `vasm` is unavailable
    - verify documented-divergence reporting remains visible for integrated `68040` FPU gaps
    - if any documented-divergence fixture now fully matches, verify it is reported as a reclassification candidate
    - run `make test-external-oracle`
    - run `make test-external-oracle OPFORGE_VASM_BIN=<configured local vasm path>`
    - run `cargo test --workspace`
    - run `cargo test -p asm examples_match_reference_outputs`
  - Plan-compliance review evidence:
    - `plan-compliance-reviewer` returns `PASS` for a slice limited to integrated `68040` FPU corpus coverage and opt-in workflow refresh
    - compliance note explicitly states that default local/CI execution remains dependency-free
  - Commit outcome:
    - the external-oracle A/B workflow can run the expanded 680x0 family corpus end to end through an explicit opt-in command
  - Definition of done:
    - integrated `68040` FPU coverage is represented in its own profile corpus
    - the opt-in workflow exercises the full expanded manifest set
    - default workspace and reference paths still pass without `vasm`

## Milestones

- [x] Milestone 1: per-CPU/profile manifest layout and family CPU support are landed (`Work item 1`)
- [x] Milestone 2: the integer family corpus through `68040` is landed (`Work item 2` through `Work item 6`)
- [x] Milestone 3: supported MMU and external FPU profile corpora are landed (`Work item 7` through `Work item 9`)
- [x] Milestone 4: integrated `68040` FPU coverage and refreshed workflow coverage are landed (`Work item 10`)

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- do not widen a slice beyond the active work item’s declared CPU/profile surface
- do not hide unsupported family/FPU/MMU gaps through silent skips; use explicit documented-divergence fixtures instead
- do not let external-tool availability affect default workspace or reference test success
