# opForge Motorola 68000 Family 68020/68030/68040 MMU and FPU Extension Spec (v0.1)

## Summary
This specification defines a combined future MMU and FPU support contract for
the shipped Motorola 68000-family lineage in opForge.

The combined scope is intentionally asymmetric:

- MMU support is a low-priority, minimal compatibility surface. It is limited
  to the already-landed MMU-related `MOVEC` register access and a narrow future
  `PFLUSH` slice.
- FPU support remains the larger future extension surface. It covers
  assembler-facing support for external `MC68881` and `MC68882` coprocessors on
  `m68020` and `m68030`, plus the integrated `m68040` FPU.
- Both MMU and FPU support remain assembler-facing only: parsing, legality,
  encoding, reporting, examples, and documentation. CPU, MMU, and FPU
  execution semantics remain out of scope for opForge VM.

This specification does not widen the already-shipped integer `m68000` through
`m68040` CPU contract by default.

## Problem
The current later-family 68k support in opForge intentionally excludes general
MMU and FPU surfaces. That shipping boundary is valid, but future work still
needs a precise source-backed contract.

For MMU support, the priority is now known to be very low. Review remediation
work has already added `MOVEC` access to the relevant MMU control registers,
and `PFLUSH` appears to be the only additional MMU instruction family with
meaningful real-world demand. A spec that still assumes a full `68851` or
`68030` PMMU bring-up would be wider than the likely value.

For FPU support, the situation is different. Real source compatibility can
benefit from a broader assembler-facing FPU surface, especially because
`m68020` and `m68030` use external `MC68881` or `MC68882` coprocessors while
`m68040` integrates an FPU with a similar visible programmer's model.

Finally, discussions about MMU and FPU support can drift into runtime
semantics, numeric behavior, page-table behavior, or software assist packages.
That is not the opForge VM scope. The specification must keep the contract on
assembler-facing behavior only.

## Goals
- [ ] `REQ-M68KMF-001`: Preserve the existing `motorola68000` family id,
      `motorola68k` dialect id, and shipped integer CPU identities
      `m68000` through `m68040`.
- [ ] `REQ-M68KMF-002`: Preserve the already-landed MMU-related `MOVEC`
      register access exactly as part of the later-family CPU surface.
- [ ] `REQ-M68KMF-003`: Keep future MMU work narrowly bounded to `PFLUSH`
      rather than reopening the full `68851` or `68030` PMMU instruction set.
- [ ] `REQ-M68KMF-004`: Add explicit FPU target selection orthogonal to `.cpu`,
      with accepted values `none`, `68881`, `68882`, and `68040`.
- [ ] `REQ-M68KMF-005`: Keep FPU selection disabled by default so existing
      integer-only `m68000` through `m68040` source behavior remains unchanged
      unless a user opts in explicitly.
- [ ] `REQ-M68KMF-006`: Define deterministic host-CPU and optional-unit
      legality rules so unsupported MMU/FPU selections and instructions fail
      with clear source-facing diagnostics.
- [ ] `REQ-M68KMF-007`: Support the `MC68881` and `MC68882` floating-point
      instruction surface on `m68020` and `m68030` when `.fpu 68881` or
      `.fpu 68882` is active.
- [ ] `REQ-M68KMF-008`: Support the integrated `m68040` floating-point
      programming surface on `m68040` when `.fpu 68040` is active.
- [ ] `REQ-M68KMF-009`: Keep capability reporting, documentation, examples, and
      reference artifacts honest about the minimal MMU scope and the broader
      FPU scope.
- [ ] `REQ-M68KMF-010`: State the no-emulation boundary explicitly so future
      implementation work stays in assembler scope only.

## Non-Goals
- [ ] `NREQ-M68KMF-001`: Execute MMU or FPU instructions inside opForge VM.
- [ ] `NREQ-M68KMF-002`: Emulate numeric floating-point results, rounding,
      exceptions, NaN propagation, denormal handling, or trap delivery.
- [ ] `NREQ-M68KMF-003`: Emulate MMU translation walks, page tables, ATC
      behavior, address-translation side effects, or privilege checks.
- [ ] `NREQ-M68KMF-004`: Implement the broad `68851` or `68030` PMMU
      instruction families such as `PMOVE`, `PLOAD`, `PTEST`, `PBcc`,
      `PDBcc`, `PScc`, `PTRAPcc`, `PVALID`, `PSAVE`, or `PRESTORE`.
- [ ] `NREQ-M68KMF-005`: Model operating-system or monitor software assist
      packages such as `M68040FPSP`.
- [ ] `NREQ-M68KMF-006`: Support `68LC040`, `68EC040`, `68EC030`, `68060`, or
      any non-listed CPU or variant in this specification.
- [ ] `NREQ-M68KMF-007`: Redesign the existing integer `m68000` through
      `m68040` CPU contract.
- [ ] `NREQ-M68KMF-008`: Mix implementation sequencing, commit slicing, or
      milestone planning into this specification.

## Invariants / Constraints
- The family id remains `motorola68000`.
- The canonical dialect id remains `motorola68k`.
- The currently shipped integer CPU ids remain unchanged.
- `.cpu` alone must not enable FPU instructions.
- Existing integer-only source for `m68000` through `m68040` must keep the same
  bytes and diagnostics when no FPU selector is active.
- MMU-related `MOVEC` register legality remains CPU-gated and does not require
  a new `.mmu` selector in this specification.
- Future MMU support in this specification is limited to `PFLUSH`.
- `68882` is modeled as a distinct target id for capability and documentation
  honesty, but it shares the same assembler-visible instruction surface as
  `68881` in this specification.
- Instruction acceptance in this specification means assembler legality and
  encoding support only. It does not imply hardware execution parity, software
  assist availability, or VM execution support.

## Behavioral Contract

### Target selection and discovery
This specification introduces one new optional target selector:

- `.fpu <target>`

Accepted `.fpu` values:

- `none`
- `68881`
- `68882`
- `68040`

Defaults:

- after `.cpu`, the effective FPU target is `none` until `.fpu` says otherwise

Legal host pairings:

| Host CPU | Legal `.fpu` targets |
| --- | --- |
| `m68000` | `none` |
| `m68010` | `none` |
| `m68020` | `none`, `68881`, `68882` |
| `m68030` | `none`, `68881`, `68882` |
| `m68040` | `none`, `68040` |

Illegal pairings must fail deterministically with diagnostics that name both
the selected CPU and the incompatible FPU target.

Capability and support-reporting surfaces must expose FPU availability as an
explicit optional support tied to the FPU selector.

MMU support remains CPU-gated in this specification:

- the already-shipped MMU-related `MOVEC` register access remains part of the
  relevant later-family CPU behavior
- no `.mmu` selector is introduced by this specification
- future `PFLUSH` support, if implemented, is legal only where the CPU matrix
  below says it is legal

### MMU support summary
| Host CPU | MMU assembler surface in this spec | Notes |
| --- | --- | --- |
| `m68020` | None | `68851` PMMU support remains out of scope |
| `m68030` | `PFLUSH` only | No broad `68030` PMMU instruction-family bring-up |
| `m68040` | Existing MMU-related `MOVEC` register access, plus `PFLUSH` | Minimal practical compatibility surface |

### MMU support matrix
| MMU surface | `m68020` | `m68030` | `m68040` | Notes |
| --- | --- | --- | --- | --- |
| MMU-related `MOVEC` register access already shipped | No | No | Yes | Existing later-family behavior retained |
| `PFLUSH` | No | Yes | Yes | Only new MMU instruction family in scope |
| `PMOVE` | No | No | No | Explicitly out of scope |
| `PLOAD` | No | No | No | Explicitly out of scope |
| `PTEST` | No | No | No | Explicitly out of scope |
| `PBcc`, `PDBcc`, `PScc`, `PTRAPcc` | No | No | No | Explicitly out of scope |
| `PVALID`, `PSAVE`, `PRESTORE` | No | No | No | Explicitly out of scope |

MMU-related `MOVEC` registers already accepted by the shipped later-family
surface include:

- `TC`
- `ITT0`
- `ITT1`
- `DTT0`
- `DTT1`
- `MMUSR`
- `URP`
- `SRP`

### FPU support summary
| Host configuration | FPU model | Registers / model | Instruction surface | Notes |
| --- | --- | --- | --- | --- |
| `m68020` + `.fpu 68881` | External floating-point coprocessor | `FP0`-`FP7`, `FPCR`, `FPSR`, `FPIAR` | Full `68881` floating-point assembler surface | Classic coprocessor model |
| `m68020` + `.fpu 68882` | External floating-point coprocessor | `FP0`-`FP7`, `FPCR`, `FPSR`, `FPIAR` | Same assembler-visible surface as `68881` | Faster compatible coprocessor |
| `m68030` + `.fpu 68881` or `.fpu 68882` | External floating-point coprocessor | `FP0`-`FP7`, `FPCR`, `FPSR`, `FPIAR` | Same assembler-visible surface as on `m68020` hosts | `m68030` itself does not integrate the FPU |
| `m68040` + `.fpu 68040` | Integrated FPU | `FP0`-`FP7`, `FPCR`, `FPSR`, `FPIAR` | Broadly `68881/68882`-compatible floating-point assembler surface | Runtime execution details are out of scope |

### FPU instruction matrix
| FPU instruction family | `.fpu 68881` | `.fpu 68882` | `.fpu 68040` | Notes |
| --- | --- | --- | --- | --- |
| `FMOVE`, `FMOVEM` | Yes | Yes | Yes | Core register and operand movement |
| Core arithmetic and compare/test families (`FADD`, `FSUB`, `FMUL`, `FDIV`, `FSQRT`, `FCMP`, `FTST`, conversions) | Yes | Yes | Yes | Assembler-visible legality and encoding only |
| Floating-point conditional families (`FBcc`, `FDBcc`, `FScc`, `FTRAPcc`) | Yes | Yes | Yes | Accepted as assembler-visible FPU ISA |
| `FSAVE`, `FRESTORE` | Yes | Yes | Yes | Frame-format execution semantics remain out of scope |
| Transcendentals and extended math families (`FSIN`, `FCOS`, and related families in the PRM) | Yes | Yes | Yes | `68040` runtime assist expectations are not modeled by opForge |

For this specification, `68881` and `68882` are accepted as distinct target ids
but share the same instruction legality matrix, register model, and source
syntax.

### Assembler-only scope and diagnostics
MMU and FPU support in opForge under this specification means:

- the parser recognizes MMU and FPU mnemonics, registers, and operand forms
- legality checks depend on the selected `.cpu` and `.fpu`
- the assembler emits the architecturally correct instruction words and
  extension words for the selected target
- capability and support reporting reflect the optional FPU surface and the
  narrow MMU surface
- examples, references, and manuals describe only the support actually shipped

MMU and FPU support in this specification does not mean:

- VM execution of MMU or FPU instructions
- floating-point numeric evaluation
- emulation of exceptions, traps, page translation, or software assist
  packages
- verification that a produced binary will execute without OS or monitor
  support on real hardware

Diagnostics must be explicit about missing optional support. Examples:

- `PFLUSH` under `.cpu 68020` must fail as unsupported on the selected CPU
- `PTEST` under `.cpu 68030` must fail as unsupported in the intentionally
  narrowed MMU scope, not as silently planned future support
- `MOVEC URP,D0` under `.cpu 68040` must remain legal as part of the already
  shipped MMU-related `MOVEC` surface
- `FMOVE` under `.cpu 68040` with `.fpu none` must fail as an FPU-disabled
  instruction
- `FSIN` under `.cpu 68040` with `.fpu 68040` must assemble successfully when
  the opcode is architecturally part of the accepted assembler-visible FPU ISA,
  without implying runtime execution support inside opForge

## Boundary Cases
- `PFLUSH` is illegal under `.cpu 68020`.
- `PTEST`, `PMOVE`, `PLOAD`, `PBcc`, `PDBcc`, `PScc`, `PTRAPcc`, `PVALID`,
  `PSAVE`, and `PRESTORE` remain illegal under every CPU in this
  specification.
- MMU-related `MOVEC` registers such as `URP` or `TC` remain legal only where
  the already-shipped CPU-specific later-family support accepts them.
- `.fpu 68881` and `.fpu 68882` are illegal under `.cpu 68040`.
- `.fpu 68040` is illegal under `.cpu 68020` or `.cpu 68030`.
- Selecting `.cpu 68040` does not implicitly enable `.fpu 68040`; explicit
  opt-in remains required.
- Floating-point registers `FP0` through `FP7` and `FPCR`/`FPSR`/`FPIAR` must
  remain illegal unless an FPU selector is active.
- Existing integer `MOVEC` support for `SFC`, `DFC`, `VBR`, `CACR`, `CAAR`,
  `MSP`, and `ISP` remains governed by the integer lineage spec and must not
  require `.fpu`.

## Acceptance Criteria
- [ ] `AC-M68KMF-001`: With no FPU selector active, existing integer-only
      `m68000` through `m68040` behavior is unchanged.
- [ ] `AC-M68KMF-002`: Source can select legal FPU targets explicitly with
      `.fpu`, and illegal host pairings fail deterministically.
- [ ] `AC-M68KMF-003`: The MMU support matrix in this specification is
      reflected exactly in legality and encoding behavior, including the
      intentionally narrow `PFLUSH`-only future MMU slice.
- [ ] `AC-M68KMF-004`: The already-shipped MMU-related `MOVEC` register access
      remains legal where currently shipped and does not regress.
- [ ] `AC-M68KMF-005`: The FPU instruction matrix in this specification is
      reflected exactly in legality and encoding behavior.
- [ ] `AC-M68KMF-006`: Diagnostics distinguish between unknown mnemonics,
      FPU-disabled instructions, CPU-unsupported MMU instructions, and
      intentionally out-of-scope MMU families.
- [ ] `AC-M68KMF-007`: Capability reporting and user-facing documentation show
      the FPU surface as optional and the MMU surface as deliberately minimal.

## Validation Expectations
- Add focused regression tests proving the shipped MMU-related `MOVEC`
  registers still assemble where currently supported.
- Add focused positive and negative tests for `PFLUSH` on `m68030` and
  `m68040`, and rejection on `m68020`.
- Add focused negative tests for `PMOVE`, `PLOAD`, `PTEST`, `PBcc`, `PDBcc`,
  `PScc`, `PTRAPcc`, `PVALID`, `PSAVE`, and `PRESTORE` so the intentionally
  narrowed MMU boundary is explicit in behavior.
- Add focused legality tests for the `.cpu` + `.fpu` host matrix.
- Add focused positive and negative tests for the FPU instruction matrix on
  `68881`, `68882`, and `68040`.
- Add examples and references for at least:
  - one `m68030` or `m68040` `PFLUSH` fixture
  - one `m68020` or `m68030` external-FPU fixture
  - one `m68040` integrated-FPU fixture
- Keep the active worktree `AGENTS.md` workflow and execution rules binding at
  all times for any follow-on plan or implementation work derived from this
  specification.

## Source Basis
- Motorola / NXP, *M68000 Family Programmer’s Reference Manual*:
  [M68000PRM.pdf](https://www.nxp.com/docs/en/reference-manual/M68000PRM.pdf)
- Motorola / NXP, *MC68030 User’s Manual* MMU chapter source:
  [MC68030UM-P2.pdf](https://www.nxp.com/docs/en/reference-manual/MC68030UM-P2.pdf)
- Motorola / NXP, *MC68881/MC68882 User’s Manual*:
  [MC68881UM.pdf](https://www.nxp.com/docs/en/reference-manual/MC68881UM.pdf)
- Motorola / NXP, *M68040 User’s Manual*:
  [MC68040UM.pdf](https://www.nxp.com/docs/en/reference-manual/MC68040UM.pdf)
- NXP product page for the `MC68882` coprocessor:
  [MC68882](https://www.nxp.com/products/no-longer-manufactured/math-coprocessor%3AMC68882)
- NXP product page for the `MC68040` family:
  [MC68040 family](https://www.nxp.com/products/no-longer-manufactured/including-ec-lc-and-v%3AMC68040)

## Open Questions
- Should `PFLUSH` be accepted on both `m68030` and `m68040` in the first slice,
  or should the implementation start with only one CPU and widen later?
- Should capability reporting render the narrow MMU surface inline under the
  CPU entry, or as a separate notes field distinct from the selector-driven FPU
  surface?
