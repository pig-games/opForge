# opForge Amiga Hunk Full-Support Spec v0.2

## Summary

This specification defines the follow-on Hunk work needed after the current
working `format=hunk` subset. opForge can already emit runnable AmigaOS Hunk
executables for a narrow set of relocation-free and `HUNK_RELOC32`-compatible
cases, including the current `helloworld` and `writefile` examples.

The next goal is to move from that selective subset to practical full support
for regular AmigaDOS executable generation within opForge's current scope:
generic relocation capture, unplaced-section executable emission, broader m68k
instruction or data fixups, and more natural Motorola-style notation for common
symbol-bearing forms.

This is still an internal output-component and assembler-model specification,
not a dynamic plugin ABI and not a full Amiga object-file specification.

## Problem

The current Hunk implementation proves that opForge can generate real AmigaOS
executables, but it is still too selective to feel like normal assembler
support.

Current working limitations include:

- relocation capture is still specialized rather than generic
- the live path is centered on `HUNK_RELOC32` and a subset of compatible data
  and instruction forms
- the Hunk writer still expects selected sections to have assigned bases, which
  is stricter than a natural relocatable-executable workflow
- common source notation often still needs explicit absolute-long spelling such
  as `label.L` or `#label` in carefully supported forms
- notation resolution and relocation capture are not yet unified enough to
  support normal Amiga-style source without format-aware workarounds

Without a follow-on design, opForge risks getting stuck in a state where Hunk
output is impressive for curated examples but awkward for real AmigaOS program
authoring.

## Goals

- [ ] Define the current shipped Hunk subset as the baseline and extend it
  toward practical regular-executable support rather than starting from a blank
  slate.
- [ ] Replace format-specific relocation hacks with a generic assembler or
  linker fixup model that Hunk output can consume.
- [ ] Support `format=hunk` for selected sections that are not pre-assigned a
  final base address, as long as the required relocation information exists.
- [ ] Preserve exact user-declared `sections=` order for Hunk executables and
  continue to require the first emitted segment to be code.
- [ ] Broaden data relocation support so ordinary symbol-bearing data works
  naturally for Hunk output.
- [ ] Broaden m68k instruction relocation support so common AmigaOS executable
  forms no longer depend on a tiny set of hand-recognized instruction layouts.
- [ ] Define notation improvements for common symbolic absolute-address forms so
  regular Motorola-style source becomes usable without forcing explicit `.L`
  spelling everywhere.
- [ ] Keep diagnostics deterministic when a requested relocation or notation
  form is unsupported, ambiguous, or would silently truncate an address.
- [ ] Keep the active worktree `AGENTS.md` workflow and execution rules binding
  for any plan derived from this specification.

## Non-Goals

- [ ] Do not define a dynamic runtime plugin ABI for output formats.
- [ ] Do not define Amiga Hunk object-file output, overlays, debug hunks, or
  symbol hunks in this specification.
- [ ] Do not require FS-UAE for the default repository quality gate.
- [ ] Do not require Workbench startup support, CLI argument parsing, or a full
  AmigaOS runtime layer as part of this Hunk-support specification.
- [ ] Do not promise compatibility with every shorthand accepted by every
  historical Amiga assembler in one step.
- [ ] Do not silently downgrade a relocatable symbolic form into a word-sized
  or otherwise non-relocatable encoding just because it currently fits.
- [ ] Do not tie notation resolution to one output format in a hidden or
  ad-hoc way.

## Invariants / Constraints

The current working Hunk subset is a floor, not a disposable prototype.

- existing runnable examples such as
  [examples/motorola68000/amigaos/helloworld.asm](examples/motorola68000/amigaos/helloworld.asm)
  and
  [examples/motorola68000/amigaos/writefile.asm](examples/motorola68000/amigaos/writefile.asm)
  must remain buildable throughout the follow-on work
- `format=hunk` must continue to preserve the exact `sections=` order declared
  by the user
- `format=hunk` must continue to reject outputs whose first emitted segment is
  not code
- the Hunk writer must continue to emit explicit big-endian record words
- relocation support must be driven by explicit fixup metadata; the writer must
  not infer relocation safety from the mere absence of relocation records
- notation improvements must live in the assembler or family resolution layer,
  not as Hunk-only parse exceptions inside the final payload writer
- ambiguous or truncating symbolic forms must fail explicitly unless the
  specification defines one canonical relocatable encoding

The follow-on design must separate three concerns cleanly:

- generic fixup capture inside the assembler or linker model
- format-specific relocation rendering in the Hunk writer
- source-notation resolution in the family or expression layer

## Behavioral Contract

### Baseline Contract

The current shipped behavior remains valid:

- `format=hunk` is a first-class output format
- the writer emits regular AmigaDOS executable segments in declared section
  order
- `HUNK_CODE`, `HUNK_DATA`, `HUNK_BSS`, and `HUNK_RELOC32` are the active
  executable-level record forms in scope
- existing relocation-free and already-supported `HUNK_RELOC32` cases remain
  supported

### Generic Fixup Model

The follow-on design must replace Hunk-specific relocation capture with a
generic fixup model carried by the assembler or linker state.

The minimum generic model must carry enough information to render executable
relocations without re-parsing source text:

- source section
- byte offset inside the initialized payload
- relocation width and kind
- target section or symbol identity
- encoded addend value that must be stored in section bytes before output
- compatibility information describing whether the fixup can be rendered by the
  selected output format

The model does not need to expose every future relocation kind in v0.2, but it
must be shaped so the Hunk writer is no longer coupled to one format-specific
collection path.

### Hunk Executable Contract

For regular executable output, `format=hunk` must accept selected sections that
are not pre-assigned a final load address when the generic fixup model provides
enough information to emit the required relocation records.

The v0.2 executable target remains regular executable output, not object-file
output. Within that scope:

- selected CODE, DATA, and BSS sections map to executable hunks in the user
  declared order
- the writer may no longer require assigned section bases as a prerequisite for
  otherwise valid relocatable executable emission
- section bytes for relocatable absolute references must contain the section
  relative addend that the loader expects, not the transient assembly-time base
  address
- supported fixups must render into deterministic `HUNK_RELOC32` groups ordered
  by target segment index
- unsupported fixups must fail with a deterministic diagnostic that names
  `format=hunk`

The current working `HUNK_RELOC32` path is the minimum supported relocation
kind. Additional Hunk relocation kinds may be added later, but they are not
required for this spec to succeed.

### Data Relocation Contract

Ordinary symbol-bearing data must become first-class relocation input, not a
special case.

At minimum, the supported path must cover ordinary longword-oriented pointer or
address data such as:

- `.long label`
- `.long label + constant`
- multi-entry pointer tables where several longwords in one section reference
  one or more target sections

If a data expression cannot be represented by the active Hunk executable
relocation subset, the assembler must fail explicitly rather than emitting a
quietly incorrect constant.

### Instruction Relocation Contract

Instruction-side relocation support must expand beyond a small hand-picked
subset of m68k operand layouts.

The follow-on implementation must support a broader common subset of 68000
absolute or immediate address forms that are expected in AmigaOS executable
source, including both source-side and destination-side symbolic addresses when
their encoded form is a relocatable absolute longword.

The contract must allow:

- more than one supported instruction family to participate
- deterministic relocation offset computation even when extension-word layouts
  are longer than the first narrow examples
- multiple relocation-bearing instructions in the same section

The contract does not require every legal 68k encoding to be supported in one
step, but it must stop depending on a tiny list of special mnemonics.

### Notation Improvement Contract

Notation improvements are part of the full-support goal, but they must be
implemented honestly.

For common m68k symbolic address forms used in executable AmigaOS source,
opForge should accept natural bare-symbol notation where the intended encoding
is unambiguously a relocatable absolute long. Examples of target forms include:

- `LEA label,A1`
- `PEA label`
- `MOVE.L #label,D1`
- `MOVE.L D0,label`
- `.long label`

For these covered cases:

- the assembler must choose the canonical relocatable long encoding
- the choice must not depend on the eventual runtime load address
- the choice must not silently downgrade to an absolute word encoding because a
  temporary value happens to fit

For ambiguous cases where several legal encodings exist and no canonical
relocatable choice is defined, opForge must require explicit notation and emit
an explanatory diagnostic.

### Diagnostic Contract

The implementation must keep failure behavior explicit.

Diagnostics must clearly distinguish at least these classes:

- unsupported output-format relocation kind
- unsupported but potentially valid source notation
- ambiguous bare-symbol notation requiring an explicit suffix or form
- relocation-compatible output requested without enough fixup metadata
- source expression that cannot be represented by the supported executable Hunk
  relocation subset

## Boundary Cases

Unplaced sections with supported relocations:

- `.output "x", format=hunk, sections=code,data` must succeed without assigned
  final bases when all required fixups are supported by the executable Hunk
  subset.

Placed sections with supported relocations:

- explicitly placed sections remain valid input, but the emitted relocation
  addends must still be section-relative rather than fixed load addresses.

Bare symbol that would fit in absolute word:

- a covered relocatable symbolic address form such as `LEA label,A1` must not
  silently choose absolute word just because the provisional value is small.

Ambiguous bare symbol form:

- if a bare-symbol instruction operand could map to multiple materially
  different encodings and no canonical relocatable choice is defined, assembly
  fails with a deterministic notation diagnostic.

Multiple relocations in one section:

- a section with several supported `.long label` entries or several supported
  absolute-long instructions must emit all required `HUNK_RELOC32` records.

Unsupported relocation-bearing expression:

- if an expression shape cannot be represented by the current executable Hunk
  subset, assembly fails explicitly rather than folding it to a constant.

Same-section PC-relative reference:

- naturally PC-relative references remain valid and must not be forced through
  Hunk relocation emission when the encoding is already self-contained.

Non-code first segment:

- `format=hunk` continues to fail if the first emitted segment would be DATA or
  BSS, even after unplaced-section support is added.

## Acceptance Criteria

- [ ] A follow-on implementation derived from this spec can emit runnable
  `format=hunk` executables for selected CODE, DATA, and BSS sections without
  requiring pre-assigned final load addresses when the required fixups are in
  the supported executable subset.
- [ ] The assembler carries a generic fixup model that the Hunk writer consumes
  without relying on Hunk-specific relocation capture paths as the primary
  contract.
- [ ] Ordinary longword symbol-bearing data such as `.long label` and
  `.long label + constant` produce correct executable Hunk relocation output in
  the supported subset.
- [ ] A broader common subset of m68k absolute-long and immediate-address
  instruction forms produces correct executable Hunk relocation output in the
  supported subset.
- [ ] Covered bare-symbol notation forms such as `LEA label,A1` and
  `MOVE.L #label,D1` assemble into the canonical relocatable long encoding
  without requiring explicit `.L` suffixes.
- [ ] Ambiguous or unsupported symbolic forms fail with deterministic
  diagnostics rather than with silent truncation or accidental flat-image
  behavior.
- [ ] The existing `helloworld` and `writefile` AmigaOS example programs remain
  buildable and runnable throughout the follow-on work.
- [ ] Focused byte-level tests, reference-output tests, and opt-in FS-UAE smoke
  coverage can validate the follow-on work without requiring FS-UAE in the
  default workspace gate.

## Validation Expectations

The derived plan should validate each vertical slice with the smallest focused
tests that prove behavior moved forward, plus the full required repository
gates before each commit.

Expected validation categories include:

- focused unit tests for fixup capture and encoded addend rewriting
- byte-level Hunk payload tests for header, segment, and relocation records
- focused assembler tests for covered bare-symbol notation forms and their
  diagnostics
- example/reference workflow tests for AmigaOS examples
- opt-in FS-UAE smoke runs for runnable example programs after meaningful Hunk
  behavior changes

## Open Questions

- Should the first notation-improvement slice choose canonical relocatable long
  resolution only for a tightly enumerated set of common m68k forms, or for a
  wider family-level class of absolute symbolic operands?
- Which additional Hunk relocation kinds, if any, are worth supporting before
  object-file output is considered, beyond `HUNK_RELOC32`?
- Should memory-type customization for executable segments remain deferred until
  after generic relocation and notation work is complete?
