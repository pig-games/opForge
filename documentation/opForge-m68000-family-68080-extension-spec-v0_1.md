# opForge Motorola 68000 Family 68080 Extension Specification (v0.1)

## Summary
This specification defines the AC68080 extension surface for opForge within
the existing `motorola68000` family and `motorola68k` dialect model.

The specification intentionally separates two extension classes:
- 68080 compatibility extensions on top of existing 68000-68040 support
- 68080-native Apollo extensions (BANK, E/B registers, Line-A Apollo forms,
  AMMX)

The objective is to establish a behavior-first contract that is explicit,
bounded, and testable, so a later implementation plan can execute without
scope drift.

## Implementation Status (April 2026)
The current implementation for this specification revision ships a bounded
subset that is intentionally narrower than full AC68080 PRM coverage:
- CPU identity and aliases: `m68080`, `68080`, `mc68080`
- Register/model substrate: `E0-E23`, `B0-B7` with non-68080 deterministic
  rejection
- Apollo directive handling on `m68080`, with `.apollo on` accepted as an
  explicit no-op in the shipped full-profile build and disabling forms rejected
  deterministically because strict compatibility mode is not implemented
- Representative integer slice: `ADDIW.L`, `CMPIW.L`, `MOVIW.L`, `MOV3Q`
- Representative AMMX slice: `LOAD`, `PADD`, `PACK3216`, `VPERM`

All other AC68080 instruction families remain deferred and out of scope for
this revision unless explicitly added in a follow-up specification.

## Problem
opForge currently ships Motorola 68000-family CPU targets through `m68080`.
The AC68080 PRM introduces additional surfaces that are not represented in the
current CPU identity set, register model, runtime directives, or mnemonic
matrix.

Without a specification for 68080 extensions, implementation work is likely to
drift in at least four ways:
- mixing compatibility behavior with Apollo-specific behavior
- over-claiming instruction support without explicit legality gates
- introducing parser-only acceptance that lacks deterministic encode-time
  diagnostics
- changing baseline 68000-68040 behavior unintentionally while extending the
  family

## Goals
- [ ] `REQ-68080-001`: Add first-class CPU identity `m68080` with aliases
      `68080` and `mc68080` under existing family id `motorola68000`.
- [ ] `REQ-68080-002`: Preserve existing `m68000`, `m68010`, `m68020`,
      `m68030`, and `m68040` behavior and diagnostics unless `.cpu 68080` (or
      alias) is selected.
- [ ] `REQ-68080-003`: Define explicit capability boundaries for 68080 between
      compatibility forms and Apollo-native forms.
- [ ] `REQ-68080-004`: Extend operand and register recognition to support
      68080 register namespaces needed by in-scope mnemonics: `E0-E23` and
      `B0-B7`.
- [ ] `REQ-68080-005`: Define a deterministic gating model for Apollo-only
      forms that require Apollo mode semantics (manual Apollo bit behavior),
      including deterministic error messages when not enabled.
- [ ] `REQ-68080-006`: Define initial in-scope 68080 integer extension
      mnemonics and operand contracts for implementation readiness.
- [ ] `REQ-68080-007`: Define initial in-scope AMMX extension mnemonics and
      operand contracts for implementation readiness.
- [ ] `REQ-68080-008`: Keep parse/encode ownership boundaries explicit:
      family-common parse in `m68k` layer, CPU legality/feature gates in
      `m68080` module.
- [ ] `REQ-68080-009`: Define testable acceptance criteria for CPU aliases,
      register legality, Apollo gating, and representative integer/AMMX
      encoding slices.

## Non-Goals
- [ ] `NREQ-68080-001`: Full AC68080 PRM completion in one step.
- [ ] `NREQ-68080-002`: Cycle-accurate 68080 scheduling, instruction fusing,
      dual-pipe timing, or microarchitectural performance modeling.
- [ ] `NREQ-68080-003`: Runtime/emulator execution semantics parity with Apollo
      hardware.
- [ ] `NREQ-68080-004`: Re-architecting the existing family registry, parser
      framework, or formatter architecture.
- [ ] `NREQ-68080-005`: Enabling all documented AMMX and floating-point forms
      in this first extension spec.
- [ ] `NREQ-68080-006`: Introducing compatibility claims against external
      assemblers for Apollo-only syntax not natively supported by those tools.
- [ ] `NREQ-68080-007`: Mixing implementation sequence, milestones, or commit
      ordering into this specification.

## Invariants / Constraints
- Family id remains `motorola68000`.
- Canonical dialect id remains `motorola68k`.
- Existing 68000-68040 legality behavior remains authoritative unless `.cpu`
  resolves to `m68080`.
- `m68080` defaults to big-endian and `native_word_size_bytes() = 2`.
- Parse success does not imply legality; 68080-only features must be rejected
  deterministically on non-68080 CPUs.
- `E*` and `B*` register names are reserved to 68080 scope and must not be
  treated as legal integer/address registers on 68000-68040.
- Apollo-only forms must be explicitly mode-gated. When Apollo mode is off,
  diagnostics must explain that the form is 68080 Apollo-gated.
- This specification is behavioral. It does not require reproducing PRM timing
  tables, execution-unit details, or speculative optimization behavior.

## Behavioral Contract

### CPU identity and alias contract
- `m68080` is a selectable CPU id in the `motorola68000` family.
- Aliases `68080` and `mc68080` resolve to `m68080`.
- CPU capability discovery surfaces must report `m68080` distinctly from
  `m68040`.

### Register contract
- Under `.cpu 68080`, the following additional register names are legal where
  instruction forms permit them:
  - data-bank extension registers: `E0-E23`
  - address-bank extension registers: `B0-B7`
- Under non-68080 CPUs, `E*` and `B*` registers are illegal and must emit a
  deterministic unsupported-CPU diagnostic.

### Apollo gating contract
- In the current shipped full-profile build, Apollo-native forms are enabled by
  default under `.cpu 68080`.
- Runtime directive gate is `.apollo <state>` where `<state>` is one of:
  - `on`
  - `off`
  - `1` (alias of `on`)
  - `0` (alias of `off`)
- Directive behavior is forward-only for subsequent source lines in the current
  assembly context; it does not retroactively re-evaluate prior lines.
- `.apollo on` is accepted as an explicit no-op reaffirmation of the default
  shipped profile.
- `.apollo off` and equivalent disabling forms are rejected deterministically
  because strict compatibility mode is not implemented in the shipped
  full-profile build.

### 68080 extension surface for this spec
This specification defines a bounded first extension surface for implementation.

1. Integer extension slice (in scope)
- `ADDIW.L #imm,<ea>`
- `CMPIW.L #imm,<ea>`
- `MOVE2 <ea>,Dn:Dn` and inverse register-pair forms where documented by the
  existing operand model
- `MOVEX <ea>,Rn` family forms accepted by the canonical 68080 contract
- `MOVEH <ea>,Rn` family forms accepted by the canonical 68080 contract
- `MOVIW.L #imm,<ea>` (Apollo-gated)
- `MOV3Q #imm,<ea>` (Apollo-gated)
- `MOVS <ea>,Dn` (Apollo-gated forms only)
- `MOVZ <ea>,Dn` (Apollo-gated forms only)
- `MOVZ2 <ea>,Dn:Dn`
- `TOUCH (#,An,Rn)`

2. AMMX extension slice (in scope)
- `LOAD`, `LOADI`, `STORE`, `STOREI`, `STOREC`, `STOREILM`
- `PADD`, `PSUB`, `PMUL` (including documented mode selectors under `PMUL`)
- `PAND`, `PANDN`, `POR`, `PEOR`
- `BSEL`, `PCMPCCB`, `PCMPCCW`
- `PACK3216`, `PACKUSWB`, `UNPACK1632`, `VPERM`

3. FPU extension surface in this spec
- No new 68080 FPU instruction enablement is required by this spec revision.
- Existing m68k FPU directive behavior for 68020/68030/68040 remains unchanged
  in this revision.
- Any 68080-specific FPU enablement requires a dedicated follow-up spec.

### CPU legality matrix
| Surface | m68000 | m68010 | m68020 | m68030 | m68040 | m68080 |
| --- | --- | --- | --- | --- | --- | --- |
| Existing shipped behavior | Yes | Yes | Yes | Yes | Yes | Yes |
| `.apollo` directive accepted | No | No | No | No | No | Yes |
| E0-E23 register parsing | No | No | No | No | No | Yes |
| B0-B7 register parsing | No | No | No | No | No | Yes |
| Apollo-gated integer forms | No | No | No | No | No | Yes |
| AMMX forms | No | No | No | No | No | Yes |
| ADDIW/CMPIW forms | No | No | No | No | No | Yes |

### Diagnostics contract
- Unsupported 68080 mnemonics on non-68080 CPUs must report unsupported CPU
  feature (not unknown mnemonic if the mnemonic exists in family tables).
- Apollo-gated forms used without Apollo mode must report gating explicitly,
  including that `.apollo on` is required under `.cpu 68080`.
- Illegal E/B register usage on non-68080 CPUs must identify register and CPU
  incompatibility.
- Operand-shape violations inside in-scope 68080 mnemonics must produce
  deterministic operand diagnostics consistent with existing m68k style.

## Boundary Cases
- Selecting `.cpu 68080` without enabling Apollo mode:
  - non-Apollo 68080 forms are legal
  - Apollo-gated forms fail with gating diagnostics
- Using `E*`/`B*` in non-68080 modes must fail even if mnemonic is otherwise
  legal on selected CPU.
- Switching CPU mid-file from 68080 to earlier CPU invalidates subsequent
  E/B register forms and Apollo-native forms.
- Ambiguous register tokens (for example symbol names colliding with `E0`)
  must follow existing register-token precedence rules in the family parser.
- AMMX instructions that require register-pair alignment constraints must fail
  deterministically when alignment constraints are violated.
- 68080-prefixed forms must not silently degrade to 68020/68040 encodings.

## Acceptance Criteria
- [ ] `AC-68080-001`: `cpusupport`/capability discovery includes `m68080` with
      aliases `68080` and `mc68080`.
- [ ] `AC-68080-002`: Existing 68000-68040 positive fixtures remain unchanged
      in bytes and diagnostics.
- [ ] `AC-68080-003`: Under `.cpu 68080`, representative in-scope integer
      forms assemble successfully with expected bytes for agreed fixtures.
- [ ] `AC-68080-004`: Under non-68080 CPUs, representative 68080 mnemonics are
      rejected as unsupported CPU features.
- [ ] `AC-68080-005`: Under `.cpu 68080`, Apollo-gated forms fail when Apollo
      mode is not enabled and pass when enabled.
- [ ] `AC-68080-006`: Under non-68080 CPUs, E/B registers are rejected with
      deterministic CPU-compatibility diagnostics.
- [ ] `AC-68080-007`: Under `.cpu 68080`, representative in-scope AMMX forms
      assemble successfully for legal operand shapes and fail deterministically
      for illegal shape/alignment cases.
- [ ] `AC-68080-008`: New diagnostics normalize to the existing classification
      model used by assembler comparison tests.

## Validation Expectations
- Add assembler tests for CPU alias resolution and capabilities output.
- Add focused positive and negative fixtures for:
  - integer 68080 extension slice
  - Apollo gating transitions
  - AMMX representative instruction families
  - cross-CPU rejection on 68000-68040
- Run project spec validator and preserve a PASS quality-gate artifact.
- Validate no regression in existing 68000-68040 fixture suites.

## Open Questions
- [x] `Q-68080-001`: Canonical Apollo mode gate is `.apollo <state>` with
  accepted states `on`, `off`, `1`, and `0`. Default state is `off`.
- [x] `Q-68080-002`: Canonical AMMX spellings for this revision are the exact
  uppercase mnemonic roots listed in the bounded in-scope AMMX slice
  (`LOAD`, `LOADI`, `STORE`, `STOREI`, `STOREC`, `STOREILM`, `PADD`,
  `PSUB`, `PMUL`, `PAND`, `PANDN`, `POR`, `PEOR`, `BSEL`, `PCMPCCB`,
  `PCMPCCW`, `PACK3216`, `PACKUSWB`, `UNPACK1632`, `VPERM`). Parser
  matching remains case-insensitive under existing family rules.
- [x] `Q-68080-003`: `m68080` exposes dedicated capability metadata for
  Apollo-gated and AMMX surfaces in capability/cpusupport output, rather
  than relying solely on mnemonic-level discoverability.
- [x] `Q-68080-004`: External-oracle parity is not required for Apollo-only
  forms in this revision; opForge-defined canonical behavior is
  authoritative for gated 68080-native surfaces.