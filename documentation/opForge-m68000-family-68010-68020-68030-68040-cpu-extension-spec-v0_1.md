# opForge Motorola 68000 Family 68010/68020/68030/68040 CPU Extension Spec (v0.1)

## Summary
This specification widens the shipped Motorola 68000-family support from the
current `m68000` baseline to the full non-MMU, non-FPU integer instruction-set
surfaces for `m68010`, `m68020`, `m68030`, and `m68040`, while preserving the
existing `motorola68000` family id and `motorola68k` dialect model.

The widened surface is bounded and testable. It includes:
- the full additional non-MMU, non-FPU integer instruction families introduced
  by `m68010`, `m68020`, `m68030`, and `m68040`
- `68020+` full-extension addressing on `m68020`, `m68030`, and `m68040`
- explicit per-CPU legality gates, including `m68010` remaining on the
  baseline `68000` addressing model
- explicit `m68040` restrictions relative to `m68020` and `m68030` where they
  affect the non-MMU, non-FPU integer surface
- deterministic diagnostics, honest documentation, and validation that proves
  the full-surface claim without hidden gaps

This specification builds on the existing `m68000` baseline source contract
rather than replacing it. Baseline `m68000` behavior remains authoritative
unless this document explicitly widens legality for later CPUs.

## Problem
The current later-CPU source artifact is intentionally narrow and does not
define the full non-MMU, non-FPU integer CPU surfaces for `m68010`, `m68020`,
`m68030`, and `m68040`. That leaves the implementation boundary underspecified
in three ways.

First, it does not define the full later-family instruction-set deltas. A
planner or implementer cannot tell whether later-family instructions such as
`MOVES`, `MOVEC`, `CAS`, `CAS2`, `CHK2`, `CMP2`, the bit-field family, long
integer multiply and divide, `TRAPcc`, `PACK`, `UNPK`, or `MOVE16` are in
scope.

Second, it does not define the full `68020+` addressing contract. Without an
explicit source-backed rule for brief extension, full extension, memory
indirect, base suppression, index suppression, outer displacement, and CPU
gating, later-family implementation work can either under-ship the intended
surface or accidentally backport `68020+` addressing to `m68000` or `m68010`.

Third, it does not define a testable CPU-delta matrix for `m68030`
carry-forward behavior and `m68040` restrictions. Without that matrix,
documentation and diagnostics can over-claim parity or hide missing removals
and incompatibilities.

## Goals
- [ ] `REQ-M68KLINEAGE-001`: Add first-class CPU identities for `m68010`,
      `m68020`, `m68030`, and `m68040` with accepted aliases `68010` and
      `mc68010`, `68020` and `mc68020`, `68030` and `mc68030`, and `68040`
      and `mc68040`.
- [ ] `REQ-M68KLINEAGE-002`: Preserve the existing `motorola68000` family id,
      `motorola68k` dialect model, and `m68000` baseline behavior.
- [ ] `REQ-M68KLINEAGE-003`: Keep family-common parsing, operand modeling,
      alias normalization, and shared encode logic in
      `crates/opforge-families/src/m68k/*`, while keeping CPU-specific
      legality and capability boundaries in per-CPU modules.
- [ ] `REQ-M68KLINEAGE-004`: Keep `m68010` on the baseline `68000` addressing
      model unless an authoritative later requirement is explicitly adopted in a
      future spec. This spec adopts no addressing widening for `m68010`.
- [ ] `REQ-M68KLINEAGE-005`: Make the full non-MMU, non-FPU `m68010` integer
      delta in scope, including `BKPT`, `MOVEC`, `MOVES`, `RTD`, and
      `MOVE.W CCR,<ea>`.
- [ ] `REQ-M68KLINEAGE-006`: Make the full non-MMU, non-FPU `m68020` integer
      delta in scope, including `68020+` full-extension addressing and all
      later integer instruction families introduced by `m68020` that are not
      MMU, PMMU, FPU, coprocessor, or cache-control surfaces.
- [ ] `REQ-M68KLINEAGE-007`: Treat `m68030` as a distinct CPU target that
      carries the full in-scope `m68020` non-MMU, non-FPU integer surface
      forward, without adding out-of-scope MMU or coprocessor surfaces.
- [ ] `REQ-M68KLINEAGE-008`: Treat `m68040` as a distinct CPU target that
      carries the in-scope later-family non-MMU, non-FPU integer surface
      forward, adds the in-scope `m68040` integer addition `MOVE16`, and
      enforces explicit `m68040` removals or restrictions relative to `m68020`
      and `m68030`.
- [ ] `REQ-M68KLINEAGE-009`: Keep `68020+` full-extension addressing fully in
      scope for `m68020`, `m68030`, and `m68040`, and explicitly out of scope
      for `m68000` and `m68010`.
- [ ] `REQ-M68KLINEAGE-010`: Bound the full-surface claim with an explicit
      CPU-delta matrix, control-register matrix, addressing contract, and
      restriction matrix so that planning and implementation can proceed without
      hidden gaps.
- [ ] `REQ-M68KLINEAGE-011`: Provide deterministic diagnostics for unsupported
      later-family instructions, unsupported later-family addressing forms,
      unsupported later-family control registers, and `m68040` removals.
- [ ] `REQ-M68KLINEAGE-012`: Keep documentation, capabilities output,
      examples, and reference artifacts honest about the exact later-family
      surface actually shipped.
- [ ] `REQ-M68KLINEAGE-013`: Preserve already-shipped `m68000` bytes and
      diagnostics unless a later-CPU-only code path is selected explicitly.

## Non-Goals
- [ ] `NREQ-M68KLINEAGE-001`: Support `68EC020`, `68060`, `CPU32`, ColdFire,
      or any non-listed 68k-family CPU in this spec.
- [ ] `NREQ-M68KLINEAGE-002`: Support MMU, PMMU, FPU, coprocessor, or
      cache-control instruction families or directives, including `PFLUSH*`,
      `PLOAD*`, `PMOVE*`, `PTEST*`, `FS*`, `FDBcc`, `FNOP`, `FTRAP*`,
      `CINV*`, and `CPUSH*`.
- [ ] `NREQ-M68KLINEAGE-003`: Backport `68020+` full-extension addressing to
      `m68000` or `m68010`.
- [ ] `NREQ-M68KLINEAGE-004`: Add broad compatibility dialect shims,
      vendor-emulation dialects, or undocumented alias families beyond the
      canonical and narrowly accepted alias surface defined here.
- [ ] `NREQ-M68KLINEAGE-005`: Redesign the existing family registry, dialect
      model, parser architecture, or formatter architecture.
- [ ] `NREQ-M68KLINEAGE-006`: Claim authoritative VM encode or runtime parity
      for later-family behavior.
- [ ] `NREQ-M68KLINEAGE-007`: Mix implementation sequencing, commit planning,
      or milestone ordering into this specification.

## Invariants / Constraints
- The family id remains `motorola68000`.
- The canonical dialect id remains `motorola68k`.
- The baseline CPU id remains `m68000`.
- The existing `m68000` baseline behavior remains unchanged unless this
  specification explicitly widens a form for a later CPU.
- Family-common parsing, operand normalization, register recognition, and
  encode helpers remain in `crates/opforge-families/src/m68k/*`.
- CPU identity, aliases, metadata, legality gates, and explicit rejection of
  unsupported forms remain in per-CPU modules.
- `m68000` and `m68010` keep `max_program_address() = 0x00FF_FFFF`.
- `m68020`, `m68030`, and `m68040` use
  `max_program_address() = 0xFFFF_FFFF`.
- All CPUs in this family remain big-endian and report
  `native_word_size_bytes() = 2`.
- `m68010` remains baseline-addressing in this spec. A mnemonic added on
  `m68010` may use only baseline `68000` effective-address forms unless the
  instruction itself is architecturally register-only or immediate-only.
- `68020+` full-extension addressing is legal only on `m68020`, `m68030`, and
  `m68040`.
- When a base displacement or outer displacement is explicitly present in a
  `68020+` full-extension form, its width must be explicit as `.W` or `.L`.
- Parse availability does not imply CPU legality. The family layer may parse a
  later-family form that a selected CPU must later reject.
- Unsupported later-family syntax must fail deterministically. The assembler
  must not silently rewrite unsupported later-family syntax to a different
  addressing mode or earlier-CPU encoding.
- Documentation and examples must describe only the surface actually shipped by
  this specification.

## Behavioral Contract

### CPU identity and discovery
- The family id remains `motorola68000`.
- The canonical dialect id remains `motorola68k`.
- The later CPU ids are `m68010`, `m68020`, `m68030`, and `m68040`.
- The accepted aliases are:
  - `m68010`: `68010`, `mc68010`
  - `m68020`: `68020`, `mc68020`
  - `m68030`: `68030`, `mc68030`
  - `m68040`: `68040`, `mc68040`
- `.cpu 68010`, `.cpu m68010`, and `.cpu mc68010` must resolve identically.
  The same rule applies to `68020`, `68030`, and `68040`.
- Registry-derived discovery surfaces, including `cpusupport` and capabilities
  reporting, must list the later CPUs as distinct CPU identities under the
  existing `motorola68000` family.

### Ownership boundaries
- The family-common `m68k` layer owns:
  - later-family register-token recognition
  - later-family operand parsing and normalization
  - structured representation of `68020+` brief-extension and full-extension
    addressing
  - family-common instruction lookup and encode helpers where behavior is
    genuinely shared
  - deterministic parse-level diagnostics for malformed later-family syntax
- Per-CPU modules own:
  - CPU id, aliases, and metadata
  - whether a later-family instruction is legal on that CPU
  - whether a later-family operand form is legal on that CPU
  - whether a later-family control register is legal on that CPU
  - deterministic rejection of unsupported instructions, unsupported
    addressing, and unsupported control registers

### CPU-delta matrix
| Surface | m68010 | m68020 | m68030 | m68040 | Notes |
| --- | --- | --- | --- | --- | --- |
| Existing `m68000` baseline surface | Yes | Yes | Yes | Yes | Baseline remains authoritative |
| Baseline `68000` addressing only | Yes | No | No | No | `m68010` does not widen addressing |
| `68020+` full-extension addressing | No | Yes | Yes | Yes | Never legal on `m68000` or `m68010` |
| `BKPT` | Yes | Yes | Yes | Yes | Added on `m68010` |
| `MOVEC` non-MMU control-register subset | Yes | Yes | Yes | Yes | See control-register matrix |
| `MOVES` | Yes | Yes | Yes | Yes | Uses selected CPU addressing legality |
| `MOVE.W CCR,<ea>` | Yes | Yes | Yes | Yes | Illegal on `m68000` |
| `RTD` | Yes | Yes | Yes | Yes | Added on `m68010` |
| `BRA.L`, `BSR.L`, `Bcc.L` | No | Yes | Yes | Yes | `.L` required explicitly |
| `CALLM` | No | Yes | Yes | No | Removed on `m68040` |
| `CAS`, `CAS2` | No | Yes | Yes | Yes | In scope on `m68020+` |
| `CHK2`, `CMP2` | No | Yes | Yes | Yes | In scope on `m68020+` |
| `EXTB.L` | No | Yes | Yes | Yes | Added on `m68020` |
| `LINK.L` | No | Yes | Yes | Yes | Added on `m68020` |
| Long integer multiply and divide | No | Yes | Yes | Yes | `MULS.L`, `MULU.L`, `DIVS.L`, `DIVU.L` and legal long-result forms |
| Bit-field family | No | Yes | Yes | Yes | `BFTST`, `BFEXTU`, `BFCHG`, `BFEXTS`, `BFCLR`, `BFFFO`, `BFSET`, `BFINS` |
| `PACK`, `UNPK` | No | Yes | Yes | Yes | Added on `m68020` |
| `RTM` | No | Yes | No | No | `m68020`-only in this family slice |
| `TRAPcc` family | No | Yes | Yes | Yes | Added on `m68020` |
| `MOVE16` | No | No | No | Yes | Added on `m68040` |
| MMU, PMMU, FPU, coprocessor, cache-control surfaces | No | No | No | No | Explicitly out of scope |

### 68010 delta
The full non-MMU, non-FPU integer `m68010` delta in scope is:
- `BKPT #imm`
- `MOVEC Rc,Rn` and `MOVEC Rn,Rc` for the non-MMU control-register subset
  listed below
- `MOVES` in its architecturally legal size and operand forms, using only
  baseline `68000` addressing when memory operands are involved
- `MOVE.W CCR,<ea>` in architecturally legal destination forms
- `RTD #imm`

The assembler must reject all `68020+` addressing forms on `m68010`, even when
the mnemonic itself exists on later CPUs.

`BKPT` requires an immediate vector in the architecturally valid `0..=7`
range.

`RTD` requires an immediate that fits the architecturally defined `16`-bit
displacement field.

### Non-MMU control-register matrix for MOVEC
The `MOVEC` positive surface for this spec is limited to non-MMU control
registers only.

- `m68010` must accept `SFC`, `DFC`, and `VBR`.
- `m68020` must accept `SFC`, `DFC`, `VBR`, `CACR`, `CAAR`, `MSP`, and `ISP`.
- `m68030` must accept the same non-MMU `MOVEC` register surface as `m68020`.
- `m68040` must accept `SFC`, `DFC`, `VBR`, `CACR`, `MSP`, and `ISP`.
- `m68040` must reject `CAAR`.
- MMU, PMMU, transparent-translation, and other MMU-associated control
  registers remain out of scope on every CPU in this spec even if the
  architecture defines them.

### 68020+ full-extension addressing
The full `68020+` non-MMU integer addressing surface is in scope on `m68020`,
`m68030`, and `m68040`.

The contract is semantic first and syntax-bounded second.

#### Addressing semantics in scope
The family layer must model and encode all architecturally legal non-MMU
`68020+` indexed and memory-indirect families, including:
- brief extension carry-forward from the baseline family
- full extension with explicit base displacement width
- scaled indexing with `*1`, `*2`, `*4`, and `*8`
- word and long index sizes
- address-register and PC-relative base forms where architecturally legal
- base suppression where architecturally legal
- index suppression where architecturally legal
- memory-indirect preindexed forms
- memory-indirect postindexed forms
- explicit outer displacement width where architecturally legal

#### Canonical syntax
The canonical later-family full-extension syntax is Motorola-style explicit
field syntax using commas and brackets.

Accepted canonical forms are:
- non-indirect full extension: `(BD,BASE,INDEX.SIZE*SCALE)`
- preindexed memory indirect: `([BD,BASE,INDEX.SIZE*SCALE],OD)`
- postindexed memory indirect: `([BD,BASE],INDEX.SIZE*SCALE,OD)`

Where:
- `BD` is an optional base displacement expression with explicit `.W` or `.L`
  width when present
- `BASE` is `An` or `PC`
- `INDEX` is `Dn` or `An`
- `SIZE` is `.W` or `.L`
- `SCALE` is `*1`, `*2`, `*4`, or `*8`
- `OD` is an optional outer displacement expression with explicit `.W` or `.L`
  width when present

The canonical syntax uses omission to express suppression or absence:
- an omitted `BD` means no base displacement
- an omitted `OD` means no outer displacement
- an empty `BASE` slot means base suppression where architecturally legal
- an empty `INDEX` slot means index suppression where architecturally legal

Examples of canonical omission patterns that must parse on `m68020`, `m68030`,
and `m68040` when architecturally valid are:
- `(,A0,D1.L*4)`
- `([foo.L,A2],D3.W*2,bar.W)`
- `([foo.W,PC,D4.L*8])`
- `([foo.L,A3],,bar.L)`

The implementation must reject syntactically malformed omission patterns
deterministically.

#### Narrow accepted aliases
To preserve continuity with the shipped baseline dialect model, the following
narrow aliases are also accepted:
- existing baseline brief-indexed forms such as `d8(An,Xn.SIZE)` and
  `d8(PC,Xn.SIZE)`
- identity-scale omission, where `*1` may be omitted
- canonical omission forms where an omitted base displacement or outer
  displacement has the architecturally defined null value, including:
  - `([An,INDEX.SIZE*SCALE],OD)`
  - `([PC,INDEX.SIZE*SCALE],OD)`
  - `([BD,An,INDEX.SIZE*SCALE])`
  - `([BD,PC,INDEX.SIZE*SCALE])`
  - `([An],INDEX.SIZE*SCALE,OD)`
  - `([PC],INDEX.SIZE*SCALE,OD)`
  - `([BD,An],INDEX.SIZE*SCALE)`
  - `([BD,PC],INDEX.SIZE*SCALE)`
  - `([An],INDEX.SIZE*SCALE)`
  - `([PC],INDEX.SIZE*SCALE)`
- non-bracketed displacement-leading sugar for non-indirect full extension when
  `BASE` is present:
  - `BD(An,INDEX.SIZE*SCALE)`
  - `BD(PC,INDEX.SIZE*SCALE)`
  - `(BD,An,INDEX.SIZE*SCALE)`
  - `(BD,PC,INDEX.SIZE*SCALE)`

No broader compatibility-dialect alias surface is implied.

#### CPU gating
- `m68000` and `m68010` must reject:
  - bracketed memory-indirect forms
  - explicit `68020+` full-extension field syntax
  - scale factors `*2`, `*4`, and `*8`
  - base-suppressed full-extension forms
  - index-suppressed full-extension forms that depend on `68020+` semantics
  - explicit outer displacement fields
- `m68020`, `m68030`, and `m68040` must accept the full `68020+` non-MMU,
  non-FPU addressing surface for any in-scope mnemonic and operand role where
  the selected CPU architecture permits that addressing mode.

### Later-family instruction families in scope
The later-family non-MMU, non-FPU instruction families in scope are bounded by
the following lists.

#### m68010-introduced surface
- `BKPT`
- `MOVEC`
- `MOVES`
- `MOVE.W CCR,<ea>`
- `RTD`

#### m68020-introduced later-family surface
- long conditional branches:
  - `BRA.L`
  - `BSR.L`
  - `Bcc.L`
- register and sign-extension forms:
  - `LINK.L`
  - `EXTB.L`
- integer multiply and divide extensions:
  - `MULS.L`
  - `MULU.L`
  - `DIVS.L`
  - `DIVU.L`
  - and the architecturally legal long-result register forms for those
    families
- compare-and-swap families:
  - `CAS`
  - `CAS2`
- compare and bounds families:
  - `CHK2`
  - `CMP2`
- call and return families:
  - `CALLM`
  - `RTM`
- packed-decimal helpers:
  - `PACK`
  - `UNPK`
- trap family:
  - `TRAPcc`
- bit-field family:
  - `BFTST`
  - `BFEXTU`
  - `BFCHG`
  - `BFEXTS`
  - `BFCLR`
  - `BFFFO`
  - `BFSET`
  - `BFINS`

The NXP M68000 Family Programmer's Reference Manual is an additional guide for
the architecture behind these families, but the local opForge planning contract
is the syntax summary below. Operand shapes not listed here remain out of scope
for v0.1 even if the architecture defines additional encodings.

#### Local syntax summary for grouped later-family forms
- long branches:
  - `BRA.L <label>`
  - `BSR.L <label>`
  - `Bcc.L <label>`
- register and sign-extension forms:
  - `LINK.L An,#<data>`
  - `EXTB.L Dn`
- signed divide forms:
  - `DIVS.L <ea>,Dq`
  - `DIVS.L <ea>,Dr:Dq`
  - `DIVSL.L <ea>,Dr:Dq`
- unsigned divide forms:
  - `DIVU.L <ea>,Dq`
  - `DIVU.L <ea>,Dr:Dq`
  - `DIVUL.L <ea>,Dr:Dq`
- signed multiply forms:
  - `MULS.L <ea>,Dl`
  - `MULS.L <ea>,Dh:Dl`
- unsigned multiply forms:
  - `MULU.L <ea>,Dl`
  - `MULU.L <ea>,Dh:Dl`
- compare-and-swap forms:
  - `CAS Dc,Du,<ea>` using memory-alterable effective addresses
  - `CAS2 Dc1:Dc2,Du1:Du2,(Rn1):(Rn2)` in word or long size only
- bounds-check forms:
  - `CHK2 <ea>,Rn`
  - `CMP2 <ea>,Rn`
  - for both families, `<ea>` supplies the bounds pair and is limited to the
    control addressing surface accepted by the selected CPU
- bit-field forms using Motorola brace syntax:
  - `BFTST <ea>{offset:width}`
  - `BFCHG <ea>{offset:width}`
  - `BFCLR <ea>{offset:width}`
  - `BFSET <ea>{offset:width}`
  - `BFEXTU <ea>{offset:width},Dn`
  - `BFEXTS <ea>{offset:width},Dn`
  - `BFFFO <ea>{offset:width},Dn`
  - `BFINS Dn,<ea>{offset:width}`
- packed-decimal helper forms:
  - `PACK Dx,Dy,#<adjustment>`
  - `PACK -(Ax),-(Ay),#<adjustment>`
  - `UNPK Dx,Dy,#<adjustment>`
  - `UNPK -(Ax),-(Ay),#<adjustment>`
- trap and module forms:
  - unsized `TRAPcc`
  - `TRAPcc.W #<data>`
  - `TRAPcc.L #<data>`
  - `CALLM #<data>,<ea>`
  - `RTM Rn`

Within those syntax classes, CPU-specific effective-address legality remains
governed by the selected instruction family, the selected CPU, and the explicit
later-family addressing rules in this specification.

### Carry-forward policy for m68030
- `m68030` is a distinct CPU target, not an informal alias of `m68020`.
- `m68030` carries forward the in-scope non-MMU, non-FPU `m68020`
  integer and addressing surface except for `RTM`.
- `m68030` adds no further positive integer mnemonic families beyond that
  carry-forward surface in this spec.
- `RTM` must reject on `m68030`; it remains `m68020`-only in this spec.
- `m68030` must continue to reject PMMU, MMU, coprocessor, FPU, and any other
  excluded system surface deterministically.

### Carry-forward policy and restrictions for m68040
- `m68040` is a distinct CPU target, not an informal alias of `m68030`.
- `m68040` carries the in-scope later-family non-MMU, non-FPU addressing
  surface forward.
- `m68040` carries the in-scope later-family integer mnemonic surface forward
  except where this spec states an explicit `m68040` restriction.

The explicit `m68040` additions and restrictions for this spec are:
- `MOVE16` is legal only on `m68040`.
- `CALLM` is illegal on `m68040` and must be rejected even though it is legal
  on `m68020` and `m68030`.
- `RTM` is illegal on `m68040` and must be rejected; it is legal on `m68020`
  but not carried forward to `m68030` in this spec.
- `CAAR` is illegal in `MOVEC` on `m68040`.
- MMU, PMMU, FPU, coprocessor, cache-management, and cache/MMU configuration
  surfaces remain out of scope and must be rejected on `m68040`.

### Diagnostics
The assembler must provide deterministic diagnostics for:
- unknown later CPU ids or aliases
- later-family-only mnemonics selected on too-early a CPU
- `68020+` addressing selected on `m68000` or `m68010`
- unsupported or malformed full-extension syntax
- unsupported or malformed memory-indirect syntax
- missing required `.W` or `.L` width on base or outer displacement fields
- unsupported later-family control registers in `MOVEC`
- `m68040`-removed forms such as `CALLM`, `RTM`, and `CAAR`
- unsupported MMU, PMMU, FPU, coprocessor, cache-control, or cache/MMU
  configuration surfaces
- illegal mnemonic and effective-address combinations within the in-scope CPU
  surfaces

### Documentation honesty
- `README`, reference-manual, capabilities, and example surfaces must describe
  exactly the later-family slice shipped by this spec.
- Documentation must explicitly say that `m68010` remains baseline-addressing
  in this slice.
- Documentation must explicitly say that `68020+` full-extension addressing is
  not legal on `m68000` or `m68010`.
- Documentation must explicitly say that `m68030` is a full carry-forward of
  the in-scope `m68020` non-MMU, non-FPU integer surface.
- Documentation must explicitly say that `m68040` carries the in-scope later
  integer surface forward, adds `MOVE16`, and rejects the explicit restrictions
  in this spec.
- Documentation and examples must not imply support for `68EC020`, `68060`,
  `CPU32`, MMU, PMMU, FPU, coprocessor, cache-control, or cache/MMU
  configuration surfaces.

## Boundary Cases
- A source file targeting `m68010` that uses scale `2`, `4`, or `8` in indexed
  addressing must be rejected even if the same base syntax shape is accepted on
  `m68020` or later.
- A source file targeting `m68000` or `m68010` that uses bracketed
  memory-indirect syntax must be rejected even if the syntax parses
  successfully at the family layer.
- A source file targeting `m68020`, `m68030`, or `m68040` that uses the
  accepted canonical memory-indirect syntax and the corresponding accepted
  canonical omission forms must normalize to the same semantic operand and
  produce the same bytes when they describe the same addressing form.
- Base-suppressed and index-suppressed forms must be rejected on `m68000` and
  `m68010` and accepted only on `m68020+` where architecturally legal.
- `BKPT` with an out-of-range breakpoint vector must fail deterministically.
- `RTD` with an immediate that does not fit the selected field width must fail
  deterministically.
- `MOVE.W CCR,<ea>` must continue to enforce architecturally legal destination
  forms and must remain rejected on `m68000`.
- `CALLM`, `RTM`, and `MOVEC CAAR` must reject deterministically on `m68040`.
- `MOVE16` must reject deterministically on every CPU except `m68040`.
- The addition of later CPUs must not change byte generation or diagnostics for
  already-shipped `m68000` baseline programs.

## Acceptance Criteria
- [ ] `AC-M68KLINEAGE-001`: `.cpu 68010`, `.cpu m68010`, and `.cpu mc68010`
      resolve to `m68010`, and the same alias-resolution behavior is verified
      for `m68020`, `m68030`, and `m68040`.
- [ ] `AC-M68KLINEAGE-002`: Registry-derived discovery surfaces list
      `motorola68000` with distinct `m68000`, `m68010`, `m68020`, `m68030`,
      and `m68040` CPU identities.
- [ ] `AC-M68KLINEAGE-003`: Representative `BKPT`, `MOVEC`, `MOVES`, `RTD`,
      and `MOVE.W CCR,<ea>` programs assemble correctly on `m68010` and fail
      deterministically on `m68000` where the form is later-only.
- [ ] `AC-M68KLINEAGE-004`: Representative `68020+` full-extension direct,
      preindexed, postindexed, base-suppressed, and index-suppressed forms
      assemble correctly on `m68020`, `m68030`, and `m68040` and fail
      deterministically on `m68000` and `m68010`.
- [ ] `AC-M68KLINEAGE-005`: Representative canonical later-family addressing
      forms and their accepted aliases normalize consistently and assemble
      identically when semantically equivalent.
- [ ] `AC-M68KLINEAGE-006`: The in-scope `m68020`-introduced families
      `BRA.L`, `BSR.L`, `Bcc.L`, `LINK.L`, `EXTB.L`, long integer
      multiply/divide, `CAS`, `CAS2`, `CHK2`, `CMP2`, bit-field families,
      `PACK`, `UNPK`, `TRAPcc`, `CALLM`, and `RTM` assemble correctly on
      `m68020` and fail deterministically on earlier CPUs.
- [ ] `AC-M68KLINEAGE-007`: `m68030` tests demonstrate the carried-forward
      positive integer and addressing surface from `m68020` except for `RTM`,
      plus deterministic rejection of `RTM` and out-of-scope MMU or
      coprocessor forms.
- [ ] `AC-M68KLINEAGE-008`: `m68040` tests demonstrate the carried-forward
  later-family surface with representative positive cases for carried-
  forward `CAS` or `CAS2`, `CHK2` or `CMP2`, bit-field forms,
  `PACK` or `UNPK`, and `TRAPcc`, plus legal `MOVE16`, and deterministic
  rejection of `CALLM`, `RTM`, `MOVEC CAAR`, and out-of-scope system
  surfaces.
- [ ] `AC-M68KLINEAGE-009`: Existing `m68000` baseline tests continue to pass
      without byte or diagnostic regression.
- [ ] `AC-M68KLINEAGE-010`: Focused examples or reference fixtures exist for
      the `68010` delta, `68020+` addressing, the later `m68020` instruction
      families, and the `m68040`-specific `MOVE16` plus restriction behavior.
- [ ] `AC-M68KLINEAGE-011`: `README`, reference-manual, and capabilities output
      describe only the shipped later-family slice and its explicit exclusions.

## Validation Expectations
- Add focused family-layer tests for later-family operand parsing, alias
  normalization, and later-family addressing diagnostics.
- Add focused CPU-layer tests for per-CPU legality differences between
  `m68000`, `m68010`, `m68020`, `m68030`, and `m68040`.
- Add assembler integration tests for:
  - CPU alias resolution and capabilities reporting
  - representative positive byte-encoding cases for the `m68010` delta
  - representative positive byte-encoding cases for the full `68020+`
    addressing surface, including preindexed, postindexed, base-suppressed, and
    index-suppressed forms where legal
  - representative positive byte-encoding cases for the `m68020`-introduced
    instruction families grouped by class: long branches, `LINK.L`, `EXTB.L`,
    long multiply/divide, `CAS`/`CAS2`, `CHK2`/`CMP2`, bit fields,
    `PACK`/`UNPK`, `TRAPcc`, `CALLM`, `RTM` on `m68020`, and `RTM` rejection on
    `m68030`
  - representative positive byte-encoding cases for carried-forward `m68040`
    later-family classes such as `CAS`/`CAS2`, `CHK2`/`CMP2`, bit fields,
    `PACK`/`UNPK`, and `TRAPcc`, plus `MOVE16`
  - representative negative diagnostics for earlier-CPU rejection of
    later-family forms
  - representative negative diagnostics for unsupported `MOVEC` control
    registers and `m68040` restrictions
  - representative negative diagnostics for unsupported MMU, PMMU, FPU,
    coprocessor, and cache-control surfaces
- Add focused parity tests proving that accepted alias spellings assemble
  identically to their corresponding canonical later-family forms.
- Add regression tests proving that existing `m68000` behavior is unchanged.
- Add at least one focused example or reference artifact for each major
  promised behavior group:
  - `m68010` delta
  - `68020+` addressing
  - `m68020` later instruction families
  - `m68040` `MOVE16` and restriction behavior
- Validate that examples and reference outputs remain stable under the
  repository's normal example and reference comparison workflow.
- Validate that `README`, reference-manual, and capabilities output match the
  actual shipped later-family slice and do not over-claim excluded surfaces.
- Run the repository's required implementation gates for this work, including
  `cargo fmt`, `cargo clippy -- -D warnings`, `cargo audit`, and `make test`.

## Open Questions
None for v0.1. This specification widens the later-family source contract
materially but keeps the exclusion boundary explicit enough to avoid hidden
scope creep into MMU, PMMU, FPU, coprocessor, or cache-control space.
