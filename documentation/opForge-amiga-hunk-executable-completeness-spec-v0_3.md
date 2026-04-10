# opForge Amiga Hunk Executable Completeness Spec v0.3

## Summary

This specification defines the next Hunk scope after the completed `v0.2`
regular-executable subset. The `v0.2` work delivered a practical Hunk
executable path with generic fixups, `HUNK_RELOC32` support for the current
matrix, explicit-placement executable output, and a tightly scoped first
notation slice.

The `v0.3` goal is to make the executable experience feel genuinely full for
single-file AmigaDOS executable authoring. In concrete terms, that means:

- broad natural bare-symbol notation for executable-safe symbolic forms
- removal of the mandatory `.place` or `.pack` gate for Hunk executable output
  when section metadata and fixups are sufficient
- removal of any mandatory `.region` prerequisite for the same supported
  executable path
- broader relocation and fixup coverage for executable Hunk output beyond the
  current first matrix, defined by an explicit compatibility matrix instead of
  fuzzy “normal source” language

Later Hunk phases beyond executable completeness are explicitly listed here, but
they are not part of this specification. They remain “to be spec’d/planned
later.”

## Problem

The completed `v0.2` Hunk plan made opForge capable and useful, but it still
does not feel like “full” Hunk executable support from an authoring
perspective.

Current remaining gaps include:

- many natural Motorola or Amiga bare-symbol instruction forms still require
  explicit `.L` spelling
- Hunk output still requires selected sections to satisfy the explicit
  placement gate before emission
- executable relocation support is still centered on the current
  `HUNK_RELOC32` subset and a bounded matrix of fixup-producing forms
- the remaining scope boundary is fuzzy unless it is anchored to a concrete
  public compatibility target
- the feature name “full support” has become misleading because the completed
  `v0.2` scope is full only for a deliberately narrowed executable subset

Without a clearer successor scope, opForge risks having a technically solid
Hunk executable path that still feels incomplete in everyday use.

## Goals

- [ ] Make natural bare-symbol executable notation a first-class supported path
  for the common unambiguous Motorola or Amiga source forms used in executables.
- [ ] Remove the requirement that `format=hunk` inputs must be explicitly
  `.place`d or `.pack`ed when the selected sections, kinds, order, and fixups
  are sufficient to emit a valid relocatable executable.
- [ ] Remove the requirement that supported executable Hunk output must declare
  a `.region` when no explicit placement policy is requested by the source.
- [ ] Broaden executable fixup capture and Hunk relocation rendering beyond the
  `v0.2` first matrix for an explicit Hunk-executable compatibility matrix
  anchored to public `vasmm68k_mot` Motorola-syntax behavior.
- [ ] Preserve exact user-declared `sections=` order and continue to require
  the first emitted segment to be code.
- [ ] Keep executable Hunk diagnostics deterministic whenever a symbolic form,
  relocation kind, or output precondition remains unsupported.
- [ ] Preserve the current working AmigaOS examples while making them cleaner
  where the broader notation and placement support genuinely allows it.
- [ ] Keep this scope centered on Hunk executable authoring behavior, not on
  expanding the underlying M68k instruction set surface that opForge already
  supports outside Hunk.
- [ ] Keep the active worktree `AGENTS.md` workflow and execution rules binding
  for any plan derived from this specification.

## Non-Goals

- [ ] Do not define Hunk object-file output in this specification.
- [ ] Do not define debug hunks, symbol hunks, overlays, or loader-side
  metadata in this specification.
- [ ] Do not define Workbench startup support, CLI argument parsing, or a full
  AmigaOS runtime layer here.
- [ ] Do not require FS-UAE in the default quality gate.
- [ ] Do not silently reinterpret ambiguous source forms without defining one
  canonical relocatable encoding.
- [ ] Do not bundle post-executable Hunk feature work into this specification;
  later phases must remain explicitly deferred.

## Invariants / Constraints

- The completed `v0.2` executable subset remains the baseline floor.
- Existing runnable AmigaOS examples must continue to assemble and run while
  broader notation and placement support is added.
- `v0.3` executable completeness is defined against Hunk-output behavior and
  canonical relocatable encodings, not against general instruction-set parity.
- Public `vasmm68k_mot` documentation and behavior are reference anchors for
  executable-source expectations, but opForge remains free to reject forms that
  cannot be mapped honestly onto its supported executable Hunk relocation
  subset.
- `format=hunk` must continue to preserve `sections=` order exactly.
- `format=hunk` must continue to reject outputs whose first emitted segment is
  not code.
- The Hunk writer must remain explicit about relocation kinds it supports.
- Notation support must be defined in terms of canonical relocatable encodings,
  not best-effort heuristics tied to temporary addresses.
- If the explicit placement gate is removed for Hunk executables, the resulting
  emitted segment order and fixup indexing semantics must remain deterministic.
- If `.region` becomes optional for supported Hunk executables, the emitted
  result must still be determined entirely by `sections=...`, section kinds,
  section contents, and fixups.
- Future non-executable Hunk work must not be smuggled into this scope.

## Behavioral Contract

### A. Executable Notation Completeness

The `v0.3` executable-notation goal is to support natural bare-symbol forms for
the common symbolic absolute-address cases used in Amiga executable code, where
one canonical relocatable encoding can be defined.

This notation work is in scope only when it exposes or normalizes Hunk-safe
symbolic executable forms. It is not a general instruction-set expansion
project.

Target examples include forms such as:

- `LEA label,A1`
- `PEA label`
- `MOVE.L D0,label`
- `MOVE.L label,D0`
- `MOVEA.L label,A0`
- `JMP label`
- `JSR label`

For covered forms:

- the assembler must select the canonical relocatable long encoding
- that decision must not depend on temporary section or region addresses
- the canonical encoding must match the executable Hunk relocation model

For forms that are still genuinely ambiguous after `v0.3` scoping:

- opForge must require explicit notation
- the diagnostic must explain why the form remains ambiguous

### B. No Mandatory Explicit Placement Gate For Hunk Executables

The `v0.3` executable model should allow:

- `.output ..., format=hunk, sections=code,data`
- `.output ..., format=hunk` for a source with no explicit `.section`
- CLI `--hunk [FILE]` for a source with no `.output` directive

without requiring `.region`, `.place`, or `.pack`, provided that:

- all selected sections exist
- each selected section has a valid `kind`
- section order is well-defined by `sections=...`
- the first emitted segment is code
- fixups required by the emitted executable subset are present and supported

Explicit placement and regions remain legal and useful, but no longer mandatory
for Hunk executables in the supported subset.

If no explicit `.section` is defined and `sections=` is omitted, the source is
treated as one implicit `code` section for Hunk output. `.org` is not required
for that shorthand because executable hunks are relocatable; the implicit code
hunk is built from the emitted flat source bytes. The shorthand must also work
for opForge's implicit module form so single-file AmigaOS examples do not need
`.module`/`.endmodule` wrappers solely for output metadata.

The same implicit code-hunk rule applies when Hunk output is selected via CLI
`--hunk [FILE]` instead of an in-source `.output` directive. In that form, the
source may omit `.output`; the CPU can also be supplied by CLI `--cpu <ID>`
when the source intentionally has no `.cpu` directive.

This matches the executable Hunk file model itself: the HUNK_HEADER describes
the number of segments and the amount of memory to reserve for each segment,
not fixed load addresses, and relocation treats hunks on disk as if each hunk
started at address 0 before the loader adjusts references.
Section memory attributes are also metadata in this model: `.section ...,
memory=chip|fast|slow|any` may constrain Hunk allocation flags, but does not
create a fixed load address.

This does not require changing the meaning of `.place` or `.pack` for other
formats.

For the unplaced executable path covered by `v0.3`:

- emitted segment order is determined only by user-declared `sections=...`
  when explicit sections are selected, or by the single implicit code section
  when no explicit `.section` exists and `sections=` is omitted
- `.region` declarations that are not used by explicit `.place` or `.pack`
  must not constrain emitted segment order or otherwise change output
  semantics
- deterministic relocation target indexing follows from that emitted order and
  the existing rule that empty selected non-BSS sections are omitted

Regions therefore become opt-in metadata for:

- explicit `.place` or `.pack` workflows where the user is intentionally
  requesting author-controlled layout policy
- future memory-policy or segment-placement features outside this `v0.3`
  executable scope
- cross-format workflows where region modeling still matters

### C. Broader Executable Relocation Coverage

The `v0.3` executable target must broaden relocation support beyond the current
bounded first matrix by defining an explicit executable compatibility matrix.

The compatibility anchor for this matrix is:

- `vasmm68k_mot` Motorola syntax
- single-file Amiga Hunk executable authoring
- public VASM documentation and behavior as the reference expectation surface

The `v0.3` matrix is defined in terms of source or encoding classes, not
taste-based phrases such as “normal source.”

#### v0.3 Executable Compatibility Matrix

Supported in `v0.3`:

- `.long label`
  - canonical form: one relocatable absolute-long data field
  - fixup model: `HUNK_RELOC32`
- `.long label+const`
  - canonical form: one relocatable absolute-long data field with constant addend
  - fixup model: `HUNK_RELOC32`
- `LEA label,A1`
  - canonical form: absolute-long symbolic address load
  - fixup model: `HUNK_RELOC32`
- `PEA label`
  - canonical form: absolute-long symbolic effective address push
  - fixup model: `HUNK_RELOC32`
- `MOVE.L #label,Dn`
  - canonical form: immediate long symbolic value
  - fixup model: `HUNK_RELOC32`
- `MOVE.L label,Dn`
  - canonical form: absolute-long symbolic source operand
  - fixup model: `HUNK_RELOC32`
- `MOVE.L Dn,label`
  - canonical form: absolute-long symbolic destination operand
  - fixup model: `HUNK_RELOC32`
- `MOVEA.L label,An`
  - canonical form: absolute-long symbolic source operand into address register
  - fixup model: `HUNK_RELOC32`
- `JMP label`
  - canonical form: absolute-long symbolic control-transfer target
  - fixup model: `HUNK_RELOC32`
- `JSR label`
  - canonical form: absolute-long symbolic control-transfer target
  - fixup model: `HUNK_RELOC32`

Explicit-only in `v0.3`:

- symbolic forms whose size is not fixed to long by the mnemonic or data form
- symbolic instruction forms where both absolute-word and absolute-long remain
  materially legal and no single canonical relocatable rule is declared
- symbolic expression forms more complex than `label+const` for executable data
  fixups
- symbolic indexed or full-extension-addressing cases that would require new
  executable fixup semantics beyond the declared `v0.3` matrix

Deferred beyond `v0.3`:

- executable forms requiring relocation kinds beyond the `v0.3`
  `HUNK_RELOC32`-based executable subset
- object-style or linker-style symbolic forms not required for single-file
  executable Hunk authoring

The matrix above is the complete `v0.3` executable-compatibility target for
notation and relocation coverage. Additional bare-symbol executable forms are
not implicitly included; they are deferred to later specification work unless
they are added explicitly to this matrix in a future revision.

Each supported entry must map to:

- one canonical relocatable encoding
- one supported executable Hunk fixup model
- deterministic diagnostics for unsupported relocation kinds or unresolved
  ambiguity

This specification does not require every Hunk relocation kind ever defined.
But it does require the executable-support story to cover an explicit,
documented Hunk-executable matrix rather than an intuition-based source style.

### Diagnostics

Diagnostics must keep these cases distinct:

- supported bare-symbol executable form
- unsupported but potentially future bare-symbol form
- ambiguous bare-symbol form requiring explicit notation
- unsupported relocation kind for Hunk executable output
- missing fixup metadata for a requested executable Hunk output
- unsupported unplaced-section scenario, if any remain after `v0.3`

## Boundary Cases

Bare-symbol instruction at low address:

- a covered form such as `LEA label,A1` must not silently pick an absolute-word
  encoding just because the current provisional value fits.

Unplaced but selected executable sections:

- `format=hunk` must succeed without `.region`, `.place`, or `.pack` when
  section order, kinds, and supported fixups are sufficient for the executable
  subset.

Implicit single-code executable:

- if no explicit `.section` is defined, `format=hunk` may omit `sections=` and
  must emit the flat source bytes as a single code hunk without requiring
  `.org`.
- if no explicit `.output` is defined, CLI `--hunk [FILE]` must select the same
  implicit single-code Hunk output path.

Explicitly placed executable sections:

- explicitly placed inputs remain valid and must behave consistently with the
  new unplaced executable path.

Region-declared executable sections without placement:

- declaring a `.region` without using it for explicit placement must not be
  required for successful Hunk executable emission, and must not change output
  semantics for the covered subset by itself.

Ambiguous symbolic instruction:

- if two materially different encodings remain legal and no canonical
  relocatable rule is defined, assembly must fail with an explicit notation
  diagnostic.

Relocation kind outside executable subset:

- if a source form would require a relocation kind beyond the implemented
  executable Hunk subset, assembly must fail explicitly rather than degrade.

Empty selected non-BSS section:

- empty selected non-BSS sections remain omitted deterministically, and emitted
  segment indices used by relocations remain stable under that rule.

## Acceptance Criteria

- [ ] Covered bare-symbol executable instruction forms assemble without explicit
  `.L` notation and produce the canonical relocatable encoding.
- [ ] The concrete `v0.3` executable compatibility matrix in this specification
  is what the implementation lands; the plan may sequence it, but it must not
  redefine it.
- [ ] `format=hunk` can emit supported executable outputs without requiring
  explicit `.region`, `.place`, or `.pack`.
- [ ] For the unplaced executable path covered by `v0.3`, emitted segment
  order is determined solely by user-declared `sections=...`, subject only to
  the existing requirement that the first emitted segment is code and that
  unsupported fixups still fail explicitly.
- [ ] For sources with no explicit `.section`, `.output ..., format=hunk`
  emits one implicit code hunk and does not require `.org`.
- [ ] For sources with no explicit `.output`, CLI `--hunk [FILE]` emits the
  same implicit code hunk and may rely on CLI `--cpu <ID>` for CPU selection.
- [ ] Explicitly placed and unplaced executable Hunk inputs behave consistently
  for the covered subset.
- [ ] Broader executable symbolic instruction and data forms emit correct
  section-relative addends and Hunk relocations in the supported subset.
- [ ] The `v0.3` scope is expressed as an explicit Hunk-executable
  compatibility matrix anchored to public `vasmm68k_mot` expectations, rather
  than to vague “normal source” wording.
- [ ] Ambiguous or unsupported symbolic forms continue to fail with
  deterministic diagnostics.
- [ ] Existing AmigaOS examples remain runnable, and can be simplified where
  the new notation or placement support genuinely permits it.

## Validation Expectations

Expected validation for a derived plan includes:

- a checked-in compatibility matrix for the covered executable forms
- focused tests for covered bare-symbol instruction forms
- focused diagnostics tests for still-ambiguous forms
- focused Hunk tests for unplaced-section executable emission
- focused Hunk tests for broader multi-section fixup coverage
- example/reference workflow checks
- opt-in FS-UAE smoke tests for at least the AmigaOS examples after meaningful
  behavior changes

## To Be Spec’d / Planned Later

These phases are intentionally out of scope for `v0.3`, but should be kept
explicit so “spec complete” Hunk work remains visible:

- richer chipset-specific section concepts beyond the basic executable Hunk
  `memory=chip|fast|slow|any` section attribute now supported, such as
  hardware-oriented section kinds or placement policies
- Amiga hardware include and symbol surface needed by custom-chip, CIA,
  copper, blitter, and interrupt-driven examples
- executable examples that depend on hardware ownership or OS takeover patterns
  rather than only regular Hunk executable authoring
- Hunk object-file output
- additional Hunk relocation kinds beyond the executable subset where needed
- symbol hunks
- debug hunks
- overlay support
- richer memory-policy customization beyond basic executable Hunk CHIP/FAST
  allocation flags
- richer linker or loader metadata if later required by real Amiga workflows

## Open Questions

- Which additional executable relocation kinds are truly necessary before
  non-executable Hunk phases are considered?
