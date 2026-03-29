# 68000-Family Assembler Remediation Plan (2026-03-29)

## Metadata

- Source: `documentation/reviews/68000-family-review-2026-03-29-01.md`
- Mode: remediation
- Owner: implementation agent

## Objective

Fix all nine primary findings from the 68000-family assembler review dated
2026-03-29 (Items 1–9), and author a follow-up specification for the four
non-MMU residual risks identified in that same review (Item 10). Two primary
findings are critical code-generation bugs affecting every assembled program;
three are high-severity ISA gaps in the 68010–68040 family chain; three are
medium-severity issues; one is a low-severity maintenance item. Testing gaps
directly tied to primary findings are addressed within their respective work
items. The 68030/040 PMMU residual risk is explicitly out of scope for both
the remediation work and the follow-up spec.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Each work item must end in exactly one new commit before the next work item
  begins; no item may advance on failed validation.
- No commit is allowed until `plan-compliance-reviewer` returns `PASS`.
- No commit is allowed until the full quality gates listed for the active work
  item pass. For Items 1–9 this means `cargo build -p families` clean
  and `cargo test -p families` all pass. For Item 10 this means the
  spec-quality gate (`agents/spec-quality-reviewer.agent.md`) returns `PASS`.
- Scope for Items 1–9 is limited to the nine primary findings in the source
  review. Item 10 covers only the four non-MMU residual risks; the 68030/040
  PMMU gap must not be addressed in any item of this plan.

## Work Items

- [x] Item 1 — Fix branch displacement formula for `.W`, auto-size, and `.L` forms
  - Source requirement or finding IDs: RVW-2026-03-29-001; Testing Gap 1
  - Expected closure: fully closes RVW-2026-03-29-001; fully addresses Testing Gap 1
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs` — lines 3268, 3292, 3343:
      change displacement calculations from `current_address + 4` / `current_address + 6`
      to `current_address + 2`, matching the `.B` and DBcc forms
    - Integration test files that assert `.W` and `.L` branch expected byte
      values — update to hardware-correct displacements
  - Full quality gates: `cargo build -p families` clean;
    `cargo test -p families` all pass
  - Plan-compliance review evidence: show before/after displacement value at
    each changed line; confirm all previously-passing branch tests still pass
    with corrected expected bytes; confirm `.B` and DBcc forms are unmodified
  - Commit outcome: branch displacement is hardware-correct for all size forms;
    tests lock in the correct values
  - Definition of done: `.W`, auto-size, and `.L` displacement calculations use
    `current_address + 2` uniformly; no test encodes the former `+ 4` / `+ 6`
    formulas; a positive round-trip test exists asserting
    `displacement = target − (instr_addr + 2)` for a word branch to a
    known-address label

- [x] Item 2 — Fix `MULS.L` / `MULU.L` single-register extension word encoding
  - Source requirement or finding IDs: RVW-2026-03-29-002; Testing Gap 2
  - Expected closure: fully closes RVW-2026-03-29-002; fully addresses Testing Gap 2
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs` — line 2800
      (`encode_long_data_register_multiply`): remove `| (1 << 10)` and OR in
      `dst_reg as u16` for bits [2:0]
    - Integration test files for `MULS.L` / `MULU.L` single-register form —
      update expected extension word bytes
  - Full quality gates: `cargo build -p families` clean;
    `cargo test -p families` all pass
  - Plan-compliance review evidence: show extension word value for
    `MULS.L <ea>, D2` before and after; confirm bit 10 = 0 and bits [2:0]
    = 2; repeat for at least one other destination register
  - Commit outcome: 32-bit multiply result form is encoded correctly; D0 is no
    longer implicitly clobbered for destinations D1–D7
  - Definition of done: `encode_long_data_register_multiply` sets bit 10 = 0
    and ORs in `dst_reg as u16`; tests validate the extension word for each
    destination register D0–D7

- [x] Item 3 — Lift 24-bit absolute address cap for 68020 / 68030 / 68040
  - Source requirement or finding IDs: RVW-2026-03-29-003; Testing Gap 5
  - Expected closure: fully closes RVW-2026-03-29-003; fully addresses Testing Gap 5
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs` — lines 5240–5244:
      convert `encode_absolute_long` and `encode_absolute_word` from static
      functions to methods parameterized by address bus width (or make
      overridable via the CPU handler trait)
    - `crates/opforge-families/src/m68020/handler.rs` — supply 32-bit limit
    - `crates/opforge-families/src/m68030/handler.rs` — supply 32-bit limit
    - `crates/opforge-families/src/m68040/handler.rs` — supply 32-bit limit
    - Integration tests — positive test for absolute `.L` address above
      `0x00FFFFFF` on a 68020+ target; confirm 68000 still rejects it
  - Full quality gates: `cargo build -p families` clean;
    `cargo test -p families` all pass
  - Plan-compliance review evidence: show that assembling `$01000000.L` on
    m68020 succeeds; show that the same address on m68000 produces the expected
    rejection diagnostic
  - Commit outcome: 68020/030/040 accept 32-bit absolute addresses; 68000
    retains the 24-bit limit; no "68000 absolute .L address out of 24-bit
    range" error fires on later CPUs
  - Definition of done: `encode_absolute_long` and `encode_absolute_word` are
    parameterized by bus width or overridable; M68020/030/040 CPU handlers
    supply 32-bit limits; positive test exists for an address above 16 MB on
    later CPUs

- [x] Item 4 — Restore `BKPT` and `RTD` on 68020 / 68030 / 68040
  - Source requirement or finding IDs: RVW-2026-03-29-004; Testing Gap 3 (partial)
  - Expected closure: fully closes RVW-2026-03-29-004; partially addresses Testing Gap 3 (BKPT and RTD coverage only; MOVE CCR coverage addressed in Item 5)
  - Expected files:
    - `crates/opforge-families/src/m68020/handler.rs` — line 256: route
      `M68010MnemonicKind::Bkpt` and `M68010MnemonicKind::Rtd` to the family
      handler's existing encoders instead of returning `EncodeResult::NotFound`;
      add both to `M68020CpuHandler::supports_mnemonic`
  - Full quality gates: `cargo build -p families` clean;
    `cargo test -p families` all pass
  - Plan-compliance review evidence: show `BKPT #3` and `RTD #-4` assemble
    correctly on m68020, m68030, and m68040 targets with correct opcodes
  - Commit outcome: `BKPT #n` and `RTD #d16` are encodable on 68020, 68030,
    and 68040; both appear in `supports_mnemonic` for m68020+
  - Definition of done: `M68020CpuHandler::encode_instruction` delegates `Bkpt`
    and `Rtd` to the family handler's encoders; `supports_mnemonic` includes
    both; positive encoding tests exist for each on m68020, m68030, and m68040

- [x] Item 5 — Add `MOVE CCR,<ea>` delegation to 68020 / 68030 / 68040
  - Source requirement or finding IDs: RVW-2026-03-29-005; Testing Gap 3 (partial)
  - Expected closure: fully closes RVW-2026-03-29-005; fully closes Testing Gap 3 (combined with Item 4)
  - Expected files:
    - `crates/opforge-families/src/m68020/handler.rs` — add CCR-source MOVE
      interception before family-handler delegation, using the same
      `encode_move_from_ccr` delegation that `M68010CpuHandler` uses (lines
      376–383 of m68010/handler.rs); m68030 and m68040 inherit through m68020
  - Full quality gates: `cargo build -p families` clean;
    `cargo test -p families` all pass
  - Plan-compliance review evidence: show `MOVE CCR, D0` assembles correctly
    on m68020, m68030, and m68040 targets
  - Commit outcome: `MOVE CCR,<ea>` is encodable on 68020, 68030, and 68040
    via `encode_move_from_ccr` delegation
  - Definition of done: `M68020CpuHandler::encode_instruction` intercepts the
    CCR-source MOVE form; m68030/040 inherit through m68020; positive tests
    exist for all three later CPUs

- [ ] Item 6 — Change `encode_chk` to return `NotFound` for `.L`; add `CHK.L` to `M68020CpuHandler`
  - Source requirement or finding IDs: RVW-2026-03-29-006; Testing Gap 4
  - Expected closure: fully closes RVW-2026-03-29-006; fully addresses Testing Gap 4
  - Expected files:
    - `crates/opforge-families/src/m68k/handler.rs` — lines 2814–2816: return
      `EncodeResult::NotFound` for `.L` size instead of `EncodeResult::error`
    - `crates/opforge-families/src/m68020/handler.rs` — add explicit `CHK.L`
      handler with opcode base `0x4100 | (Dn << 9) | EA`
    - Integration test for `CHK.L <ea>, Dn` on a 68020+ target
  - Full quality gates: `cargo build -p families` clean;
    `cargo test -p families` all pass
  - Plan-compliance review evidence: show `CHK.L` assembles on m68020; show
    that `CHK.L` on m68000 yields a CPU-level rejection (not a hard "baseline
    68000" error from the family handler)
  - Commit outcome: family handler no longer hard-errors on `CHK.L`; m68020
    CPU handler handles it; m68000 handler issues a proper CPU-level rejection
  - Definition of done: `encode_chk` returns `NotFound` for `.L`; m68020
    `CHK.L` path is implemented; positive test for `CHK.L` on 68020 passes

- [ ] Item 7 — Add 68040 MMU / translation-control registers to `ControlRegisterKind` and MOVEC handler
  - Source requirement or finding IDs: RVW-2026-03-29-007; Testing Gap 6
  - Expected closure: fully closes RVW-2026-03-29-007; fully addresses Testing Gap 6
  - Expected files:
    - `crates/opforge-families/src/m68k/operand.rs` — lines 38–46: add
      variants TC, ITT0, ITT1, DTT0, DTT1, MMUSR, URP, SRP to
      `ControlRegisterKind`
    - `crates/opforge-families/src/m68k/handler.rs` — lines 124–130: add
      parser table entries for the eight new register names
    - `crates/opforge-families/src/m68040/handler.rs` — lines 52–61: map the
      eight new variants in `M68040CpuHandler::movec_control_register_code`
      with correct PRM register codes (TC=0x003, ITT0=0x004, ITT1=0x005,
      DTT0=0x006, DTT1=0x007, MMUSR=0x805, URP=0x806, SRP=0x807)
    - Integration tests for `MOVEC TC, D0`, `MOVEC URP, A0`, and at least two
      other new registers on an m68040 target
  - Full quality gates: `cargo build -p families` clean;
    `cargo test -p families` all pass
  - Plan-compliance review evidence: show `MOVEC TC, D0` and `MOVEC MMUSR, D1`
    assemble with correct extension words on m68040
  - Commit outcome: all eight 68040 MMU/translation-control registers are
    parseable and encodable via MOVEC
  - Definition of done: `ControlRegisterKind` contains the eight new variants;
    parser table maps their string names; `movec_control_register_code` returns
    correct PRM codes; positive tests exist

- [ ] Item 8 — Remove spurious 68020-mnemonic advertising from `M68010CpuHandler::supports_mnemonic`
  - Source requirement or finding IDs: RVW-2026-03-29-008
  - Expected closure: fully closes RVW-2026-03-29-008
  - Expected files:
    - `crates/opforge-families/src/m68010/handler.rs` — line 424: remove the
      `has_m68020_mnemonic(mnemonic)` clause from `supports_mnemonic`
  - Full quality gates: `cargo build -p families` clean;
    `cargo test -p families` all pass
  - Plan-compliance review evidence: show that `supports_mnemonic` returns
    `false` for a representative 68020-only mnemonic (e.g., BFTST) on m68010;
    show it still returns `true` for a valid 68010 mnemonic (e.g., MOVES)
  - Commit outcome: `M68010CpuHandler::supports_mnemonic` no longer advertises
    68020+ mnemonics
  - Definition of done: the `has_m68020_mnemonic(mnemonic)` arm is removed;
    only base 68000 and genuine 68010 mnemonics return `true`

- [ ] Item 9 — Remove duplicate `encode_moves` from `M68010CpuHandler`; delegate to family handler

  - Source requirement or finding IDs: RVW-2026-03-29-009
  - Expected closure: fully closes RVW-2026-03-29-009
  - Expected files:
    - `crates/opforge-families/src/m68010/handler.rs` — lines 212–281: remove
      private `encode_moves`; line 418: replace the direct call with
      `self.family.encode_moves_instruction(parsed.size, operands, ctx)`,
      matching the m68020 delegation pattern
  - Full quality gates: `cargo build -p families` clean;
    `cargo test -p families` all pass
  - Plan-compliance review evidence: show `MOVES` still assembles correctly on
    m68010 after removing the private copy; diff confirms no behavioral change,
    only call-site indirection
  - Commit outcome: `M68010CpuHandler` no longer maintains a private copy of
    `encode_moves`; it delegates to the canonical family handler implementation
  - Definition of done: private `encode_moves` is removed; call site uses
    `self.family.encode_moves_instruction`; all existing MOVES tests pass
    unchanged

- [ ] Item 10 — Write follow-up specification for non-MMU residual risks
  - Source requirement or finding IDs: review residual risks 1, 2, 3, 5 from
    `documentation/reviews/68000-family-review-2026-03-29-01.md` (Residual
    Risks section):
    - Long divide (`DIVS.L` / `DIVU.L`) not implemented on 68020+
    - MOVES EA validation — PC-relative modes not rejected
    - CAS2 `Rn` field validation — data registers silently accepted
    - Multi-pass branch displacement masking risk (interacts with RVW-001 fix)
  - Expected closure: fully addresses residual risks 1, 2, 3, and 5 by producing a spec suitable to drive a follow-up implementation plan; residual risk 4 (68030/040 PMMU absent) is explicitly deferred and not covered
  - Expected files:
    - New spec artifact under `documentation/`, named following the repo
      convention (e.g.,
      `opForge-m68000-family-residual-risks-spec-v0_1.md`), authored using
      `templates/spec-template.md` and the `skills/opforge-spec-authoring/SKILL.md`
      workflow
  - Full quality gates: spec passes the spec-quality gate
    (`agents/spec-quality-reviewer.agent.md` returns `PASS`); no build or test
    changes in this item
  - Plan-compliance review evidence: provide the spec-quality gate output
    showing `PASS`; confirm the spec covers all four non-MMU residual risks and
    explicitly defers MMU/PMMU work
  - Commit outcome: spec artifact committed; spec-quality gate result recorded
    alongside it
  - Definition of done: a spec file exists for the four non-MMU residual risks;
    it covers behavioral requirements, boundary conditions, and acceptance
    criteria sufficient to drive a follow-up implementation plan; the
    spec-quality gate has returned `PASS`; the 68030/040 PMMU instruction-set
    gap is explicitly deferred in the spec's out-of-scope section

## Milestones

- [x] Milestone 1 — Critical bugs resolved (Items 1–2)
- [x] Milestone 2 — High-severity ISA gaps resolved (Items 3–5)
- [ ] Milestone 3 — Medium-severity issues resolved (Items 6–8)
- [ ] Milestone 4 — Low-severity maintenance resolved (Item 9)
- [ ] Milestone 5 — Follow-up spec authored for non-MMU residual risks (Item 10)

## Out-of-Scope (Residual Risks — deferred)

The following residual risk is explicitly excluded from this plan and from the
follow-up spec (Item 10). It requires dedicated MMU/PMMU research and a
separate planning effort:

- 68030/040 PMMU instruction set absent (PMOVE, PLOAD, PSTORE, PFLUSHA,
  PFLUSHN, PTEST, etc.) — deferred; no diagnostic improvement or stub work
  to be done during this plan

## Blocking Rules

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- No commit before all quality gates pass.
- No commit before `plan-compliance-reviewer` returns `PASS`.
- Each work item or phase must end in exactly one new commit before the next
  item starts.
- No advancing to the next item on failed validation.
- Checkbox updates are mandatory bookkeeping.
