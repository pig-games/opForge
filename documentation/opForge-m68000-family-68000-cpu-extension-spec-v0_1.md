# opForge Motorola 68000 Family Extension Spec (v0.1, 68000 baseline)

## Summary
This specification defines the minimum behavior required to add first-class
Motorola 68000-family support to the current crate-based opForge architecture.
The v0.1 delivery is intentionally narrow: one new family, one baseline CPU,
one canonical dialect, deterministic diagnostics, and documentation that only
claims shipped behavior.

## Problem
opForge currently ships Intel 8080-family, MOS 6502-family, and Motorola
6800-family support, but it has no Motorola 68000-family registration,
operand model, encode path, or user-facing documentation. That gap blocks
`.cpu 68000` source, prevents 68k capability discovery, and leaves the current
family/CPU/dialect architecture without a 24-bit big-endian 68k target.

## Goals

- [ ] `REQ-M68K-001`: Add a new `motorola68000` family and baseline `m68000`
      CPU with aliases `68000` and `mc68000` using the current crate-based
      registry stack.
- [ ] `REQ-M68K-002`: Support a narrow but useful canonical `motorola68k`
      syntax for baseline 68000 operands and instructions, plus a narrow set of
      deterministic idiomatic baseline aliases that map directly to those
      canonical forms.
- [ ] `REQ-M68K-003`: Preserve deterministic native-assembler behavior for
      two-pass resolution, size legality, branch sizing, diagnostics, and
      big-endian data emission.
- [ ] `REQ-M68K-004`: Expose 68000 support through registry discovery,
      `cpusupport`/capabilities reporting, tests, examples, and user-facing
      documentation.
- [ ] `REQ-M68K-005`: Keep family-level ownership extensible for later
      `68010`/`68020` work without redesigning the family/CPU boundary during
      v0.1.

## Non-Goals

- [ ] `NREQ-M68K-001`: Enable post-68000 CPUs or instructions in v0.1
      (`68010`, `68EC020`, `68020`, or later).
- [ ] `NREQ-M68K-002`: Add broad compatibility dialects or syntax shims beyond
      canonical `motorola68k` and the explicit baseline aliases listed in this
      spec.
- [ ] `NREQ-M68K-003`: Redesign directives, macro language, listing format, or
      general expression semantics for 68k-specific preferences.
- [ ] `NREQ-M68K-004`: Make `motorola68000` authoritative for VM encode,
      expression VM, or parser VM rollout in v0.1.
- [ ] `NREQ-M68K-005`: Widen the preview `libopforge` facade beyond normal
      builtin registry/discovery surfaces just to expose internal
      implementation details.

## Invariants / Constraints
- Shared operand parsing, family-common encode tables, and family-common
  diagnostics belong in a new 68000 family implementation under
  `crates/opforge-families`.
- CPU identity, aliases, default dialect, target metadata, and future CPU-only
  gating belong in a separate `m68000` CPU module under
  `crates/opforge-families`.
- The implementation must fit the current workspace layout rather than the
  older monolithic `src/` layout. The primary integration surfaces are
  `crates/opforge-families`, `crates/opforge-asm`, `crates/opforge-engine`,
  `crates/opforge-formatter`, and selected `crates/opforge-vm` smoke coverage
  where registry-derived hierarchy data must stay coherent.
- The family module namespace for this work is `m68k`, leaving `m68000` for the
  baseline CPU module namespace.
- Dialect modules may rewrite syntax into canonical forms but must not emit
  bytes directly.
- Two-pass assembly behavior remains in force.
- While `.cpu m68000` is active, target metadata must report
  `max_program_address() = 0x00FF_FFFF`, `native_word_size_bytes() = 2`, and
  big-endian multi-byte emission.
- Reference outputs may change only for intentional behavior deltas and only
  after tests demonstrate the new behavior.

## Behavioral Contract
### CPU identity and discovery
- The family id is `motorola68000`.
- The canonical dialect id is `motorola68k`.
- The baseline CPU id is `m68000` with aliases `68000` and `mc68000`.
- `.cpu 68000`, `.cpu m68000`, and `.cpu mc68000` resolve to the same CPU.
- Registry-derived discovery surfaces, including `cpusupport` and capabilities
  reports, must list the new family and CPU deterministically.

### Ownership boundaries
- Family-level behavior lives in `crates/opforge-families/src/m68k.rs` and
  `crates/opforge-families/src/m68k/*` and owns:
  - register recognition for data registers, address registers, PC, and any
    family-common special registers needed for the baseline slice
  - canonical 68000 effective-address parsing
  - family-common instruction lookup and encoding
  - branch displacement and effective-address legality checks shared by future
    68000-family CPUs
- CPU-level behavior lives in `crates/opforge-families/src/m68000.rs` and
  `crates/opforge-families/src/m68000/*` and owns:
  - CPU id, aliases, default dialect, and target metadata
  - explicit rejection of later-CPU instructions if future family tables widen
    beyond baseline 68000
- Formatter integration in v0.1 is limited to keeping builtin hook
  registration coherent for the new family/cpu. It does not promise
  68k-specific reformatting rules beyond preserving current generic formatter
  behavior.
- VM integration in v0.1 is limited to keeping registry-derived
  hierarchy-package generation coherent when `motorola68000` is registered. It
  does not claim authoritative family-specific encode/runtime parity.

### Canonical operand baseline
The v0.1 canonical syntax must accept these operand families:
1. Data register direct: `Dn`
2. Address register direct: `An`
3. Address register indirect: `(An)`
4. Postincrement and predecrement: `(An)+`, `-(An)`
5. Address-register displacement: `d16(An)`
6. 68000 indexed address form: `d8(An,Xn.SIZE)` where `SIZE` is `.W` or `.L`
7. Absolute short and long: `(expr).W`, `(expr).L`
8. PC-relative displacement and indexed forms: `d16(PC)`, `d8(PC,Xn.SIZE)`
9. Immediate: `#expr`

### Supported idiomatic baseline aliases
The current v0.1 surface also accepts these deterministic baseline aliases:
1. Motorola-style parenthesized displacement aliases for baseline displacement
   modes: `(d16,An)` and `(d16,PC)`
2. Motorola-style parenthesized indexed aliases for baseline indexed modes:
   `(d8,An,Xn)`, `(d8,An,Xn.W)`, `(d8,An,Xn.L)`, `(d8,PC,Xn)`,
   `(d8,PC,Xn.W)`, and `(d8,PC,Xn.L)`
3. Identity-scale indexed aliases for the existing baseline indexed forms:
  `d8(An,Xn.SIZE*1)`, `(d8,An,Xn.SIZE*1)`, `(An,Xn*1)`, `(An,Xn.W*1)`,
  `(An,Xn.L*1)`, `d8(PC,Xn.SIZE*1)`, `(d8,PC,Xn.SIZE*1)`, `(PC,Xn*1)`,
  `(PC,Xn.W*1)`, and `(PC,Xn.L*1)`
4. Zero-displacement indexed aliases: `(An,Xn)`, `(An,Xn.W)`, `(An,Xn.L)`,
  `(PC,Xn)`, `(PC,Xn.W)`, and `(PC,Xn.L)`
5. Zero-displacement PC-relative shorthand alias: `(PC)`
6. Non-parenthesized absolute-width aliases that preserve explicit width:
   `expr.W` and `expr.L`
7. Unsuffixed baseline branch mnemonics `BRA`, `BSR`, and `Bcc`, which default
   to `.W`

The v0.1 baseline does not include 68020-style scaled indexes (`*2`, `*4`,
`*8`), memory-indirect forms, base suppression, ambiguous absolute width
inference, or broader compatibility alias sets beyond the forms listed above.

### Baseline instruction coverage
The v0.1 baseline must cover representative instructions across these classes:
- data movement: `MOVE`, `MOVEA`, `LEA`, `PEA`
- arithmetic and logic: `ADD`, `ADDA`, `SUB`, `SUBA`, `CMP`, `AND`, `OR`,
  `EOR`
- control flow: `BRA`, `BSR`, `Bcc`, `JMP`, `JSR`, `RTS`
- shifts and rotates: `ASL`, `ASR`, `LSL`, `LSR`, `ROL`, `ROR`
- quick and immediate-sensitive forms: `MOVEQ`, `ADDQ`, `SUBQ`

### Size, address, and branch policy
- Supported operation sizes in the baseline are `.B`, `.W`, and `.L` where the
  instruction encoding legally permits them.
- Size legality is enforced in the encode tables, not by ad hoc parser-only
  checks.
- Unsuffixed instructions may use canonical 68000 defaults only when the
  resulting encoding is deterministic; otherwise the assembler must emit a
  diagnostic that requires an explicit size suffix.
- Unsuffixed `BRA`, `BSR`, and `Bcc` default to word displacement in the
  shipped baseline alias surface.
- Absolute width selection must be explicit via `.W` or `.L` in the canonical
  syntax for the baseline slice.
- Multi-byte data emission under `.cpu m68000` follows big-endian order for the
  directives that already consult CPU endianness in the current assembler
  pipeline.

### Diagnostics
The baseline implementation must provide deterministic diagnostics for:
- unknown register tokens
- invalid effective-address forms for a mnemonic
- invalid size suffixes for a mnemonic or operand combination
- immediate values that do not fit the selected size
- branch displacements outside the selected encoding range
- non-deterministic branch or size selection that requires an explicit suffix
- attempts to use later-CPU-only instructions while targeting `m68000`, if
  wider family tables exist

## Boundary Cases
- A register token valid in another architecture must still diagnose as unknown
  in 68000 mode if it is not part of the 68000 register set.
- PC-relative forms must reject illegal combinations rather than silently
  rewriting them.
- Accepted idiomatic aliases must assemble to the same bytes as their
  equivalent canonical baseline forms.
- Negative displacements and immediate values must respect signed range rules
  for the chosen encoding size.
- The assembler must not silently choose absolute short vs absolute long when
  the canonical syntax omitted the required `.W` or `.L`.
- Big-endian emission under `.cpu m68000` must affect existing size-aware data
  directives without changing behavior for other CPUs.

## Acceptance Criteria

- [ ] `AC-M68K-001`: `.cpu 68000`, `.cpu m68000`, and `.cpu mc68000` resolve to
      `m68000`, and registry discovery surfaces list `motorola68000` and
      `m68000`.
- [ ] `AC-M68K-002`: Baseline operand families and instruction classes in this
      spec assemble to correct bytes in native assembler tests.
- [ ] `AC-M68K-003`: Size, branch, and effective-address validation errors are
      deterministic and covered by negative tests.
- [ ] `AC-M68K-004`: Size-aware data emission under `.cpu m68000` uses
      big-endian byte order where the current assembler pipeline consults CPU
      endianness.
- [ ] `AC-M68K-005`: A small 68000 example/reference corpus exists and is
      validated with the repository's normal example/reference workflow.
- [ ] `AC-M68K-006`: README and reference-manual updates describe only shipped
      `m68000` baseline behavior and do not over-claim later-CPU or
      authoritative VM support.
- [ ] `AC-M68K-007`: The supported idiomatic baseline aliases in this spec
      assemble identically to their canonical forms in focused alias-parity
      tests.

## Validation Expectations
- Add focused unit tests in the new 68000 family and CPU modules under
  `crates/opforge-families`.
- Add integration coverage in `crates/opforge-asm/src/tests.rs` for CPU alias
  resolution, registry discovery, native data-emission behavior,
  representative 68000 assembly, and canonical-versus-alias parity.
- Verify `crates/opforge-engine` and any registry-derived hierarchy-package
  smoke coverage still succeed once the new family stack is registered.
- If formatter hooks are registered, add a focused formatter registry smoke
  test rather than broad formatter redesign work.
- Run the repository's required implementation gates for execution work:
  `cargo fmt`, `cargo clippy -- -D warnings`, `cargo audit`, and `make test`.

## Open Questions
None for v0.1. The module naming, branch-size policy, narrow alias scope,
canonical absolute-width syntax, formatter scope, and VM-scope decisions above
are the decisions this plan is intended to implement.
