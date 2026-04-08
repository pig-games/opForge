# opForge Amiga Hunk Output Component Spec v0.1

## Summary

This specification defines the desired output-generation contract for adding
AmigaOS Hunk executable output to opForge and for making future file formats
pluggable as output components.

The first Hunk target is a classic AmigaDOS executable file suitable for
eventual FS-UAE smoke tests of small 68000 programs, including drafts such as
the tokenizer VM. The component model is an internal Rust extension seam, not a
runtime dynamic-plugin system.

## Problem

opForge currently treats linker-style `.output` formats as a small fixed set.
The existing path is sufficient for raw binary and C64 PRG payloads, but Hunk
output has different semantics:

- the file format is record-oriented and big-endian
- AmigaDOS loads relocatable segments rather than a flat image at a fixed
  address
- code, data, BSS, memory attributes, and relocation data have format-specific
  rules
- FS-UAE execution tests need an executable artifact, not only a flat `.bin`

Adding Hunk directly as more special-case logic in the current payload builder
would make the output path harder to extend and would blur generic output
validation with target-format validation.

## Goals

- [ ] Define an internal output-component registry keyed by `.output format=...`.
- [ ] Preserve the existing `bin` and `prg` output behavior while moving their
  format-specific rules behind output components.
- [ ] Define the first Amiga Hunk executable component as
  `.output ..., format=hunk`.
- [ ] Specify Hunk executable payload structure well enough for a first
  implementation that can generate a single-file AmigaDOS executable.
- [ ] Preserve the user-declared `sections=` order for Hunk output and require
  the first emitted segment to be executable code.
- [ ] Keep relocation support explicit: either encoded when relocation metadata
  exists or rejected with a deterministic diagnostic when it would be required.
- [ ] Make FS-UAE validation a consumer of the output artifact, not part of the
  generic output writer.
- [ ] Keep the active worktree `AGENTS.md` workflow and execution rules binding
  for any plan derived from this specification.

## Non-Goals

- [ ] Do not implement this specification in the spec artifact itself.
- [ ] Do not add a dynamic shared-library plugin ABI.
- [ ] Do not define a full Amiga object-file writer in the first Hunk output
  component.
- [ ] Do not require FS-UAE to be installed for normal unit tests.
- [ ] Do not redesign section placement, symbol resolution, or instruction
  encoding as part of the first Hunk output slice.
- [ ] Do not silently emit non-relocatable Hunk executables when relocations are
  required but unavailable.
- [ ] Do not change the meaning of existing `.output format=bin` or
  `.output format=prg`.

## Invariants / Constraints

The output path must keep generic concerns separate from format-specific
concerns.

Generic concerns:

- parsing the `.output` directive path and option key-value pairs
- storing the selected `format` identifier and the raw `.output` option bag
  without prematurely applying format-specific semantics
- resolving output paths against the configured output root
- collecting referenced sections by name
- preserving section order only when the resolved format contract says to do so
- writing the final payload bytes to the requested sink

Format-specific concerns:

- deciding whether `image`, `fill`, `loadaddr`, `contiguous`, and `sections`
  are meaningful
- choosing the byte order for file-format records
- choosing how code, data, and BSS sections map to records or segments
- choosing the emitted segment order when the file format has an entry-segment
  rule
- enforcing relocation requirements and memory-attribute constraints
- rendering diagnostics that name the selected output format

The component setup must be internal and static for v0.1. A production design
may use a trait object, enum-backed registry, or table of function pointers, but
the behavioral contract is that each format is resolved through a component
entry rather than open-coded in the generic payload builder.

Amiga Hunk executable output must follow the classic executable-file shape from
the AmigaDOS binary format:

- all hunk words are big-endian 32-bit values unless the hunk type defines
  otherwise
- executable files start with `HUNK_HEADER` (`0x000003f3`)
- the first longword after `HUNK_HEADER` is the resident-library string count;
  for regular v0.1 executables it must be `0`
- the next three header longwords are the segment-table size, first segment
  index, and last segment index; for regular non-overlay executables they must
  be `segment_count`, `0`, and `segment_count - 1`
- each segment-table entry encodes the segment allocation size in longwords plus
  any Amiga memory-type bits; v0.1 must emit zero memory-type bits
  (`MEMF_ANY`) for every segment
- segment allocation size is the reserved in-memory size rounded up to a
  longword count; it is distinct from initialized payload length
- code payloads use `HUNK_CODE` (`0x000003e9`)
- data payloads use `HUNK_DATA` (`0x000003ea`)
- BSS payloads use `HUNK_BSS` (`0x000003eb`)
- `HUNK_CODE`, `HUNK_DATA`, and `HUNK_BSS` each encode their own size as a
  longword count
- code and data payload bytes are padded to a 4-byte boundary
- executable entry starts at the first byte of segment 0, so the first emitted
  segment in v0.1 must be `HUNK_CODE`
- each segment is terminated by `HUNK_END` (`0x000003f2`)

The first implementation may support a narrow Hunk executable subset, but it
must label that subset honestly in diagnostics and tests. In particular,
relocatable AmigaDOS loading means absolute inter-segment references are not
equivalent to flat binary addresses unless relocation records are available.

Reference material used by this spec:

- AmigaOS 3 DOS Reference Manual, "Binary File Structure", section 11.2
  (`HUNK_HEADER`, executable syntax, payload hunks, and `HUNK_END`):
  https://developer.amigaos3.net/sites/default/files/downloads/2024-10/Amiga_ROM_Kernel_Reference_Manual_DOS.pdf

## Behavioral Contract

`.output` format resolution must be registry-based.

For this spec, the `.output` parser must stop normalizing format-specific
options into the directive data model before component resolution. The minimum
contract is:

- `LinkerOutputDirective` stores the output path
- `LinkerOutputDirective` stores the user-selected format identifier
- `LinkerOutputDirective` stores the raw `.output` option bag needed for
  component-side validation and normalization
- shared parsing may reject only malformed syntax, duplicate keys that are
  forbidden independent of format, or a missing `format` key

The resolved component then owns semantic validation. The required behavior is:

1. parse `format=<id>` as a format identifier
2. resolve `<id>` against the built-in output component registry
3. reject unknown identifiers with a diagnostic that lists currently supported
   built-ins
4. delegate option validation and payload construction to the resolved component

Each output component must expose these conceptual operations:

- `format_id`: stable identifier used in `.output format=...`
- `validate_options`: validate component-specific option compatibility
- `collect`: convert referenced `SectionState` values into the component input
  model
- `build_payload`: return the final byte vector or a deterministic
  `ArtifactBuildError`

The built-in components for v0.1 are:

- `bin`: raw payload component preserving current flat-section and image-span
  behavior
- `prg`: C64 PRG component preserving current little-endian 16-bit load-address
  prefix behavior
- `hunk`: AmigaDOS executable component

For v0.1, the only required user-facing Amiga executable format identifier is
`hunk`.

`format=hunk` must reject options that encode flat-image semantics unless a
future spec explicitly defines them for Hunk output. At minimum, `image`,
`fill`, `loadaddr`, and `contiguous` must be rejected for Hunk output because
AmigaDOS loads ordered segments rather than honoring a fixed flat image
address.

The Hunk component must accept named sections and map each selected section to
exactly one executable segment in the user-declared `sections=` order. The
first acceptable mapping is:

- code sections become `HUNK_CODE`
- data sections become `HUNK_DATA`
- BSS sections become `HUNK_BSS`
- empty non-BSS sections may be omitted or emitted as zero-sized segments, but
  the chosen behavior must be deterministic and documented in tests

For v0.1 executable output:

- the `sections=` order must be preserved exactly; Hunk segment order must not
  be derived by sorting placed sections by base address
- the first emitted segment must map to `HUNK_CODE`
- if the preserved first selected section would emit `HUNK_DATA` or `HUNK_BSS`,
  the component must fail with a deterministic diagnostic

The minimum Hunk component input model must carry enough information to emit a
correct regular executable:

- section name
- section kind
- initialized bytes
- reserved allocation size in bytes
- memory type selection, which defaults to `MEMF_ANY` in v0.1
- relocation capability information

Relocation capability information must be explicit. The model must carry either:

- relocation records that can be rendered into Hunk relocation hunks, or
- an explicit upstream proof flag that the selected sections are relocation-free

The Hunk component must not infer relocation-free safety from the absence of
records.

The Hunk component must not depend on host endianness. All hunk identifiers,
sizes, indexes, and relocation fields must be emitted with explicit big-endian
encoding.

The Hunk component must track relocation capability explicitly:

- if the input model carries relocation-free proof for the selected sections,
  payload construction may emit no relocation hunks
- if the input model carries relocation records, payload construction must emit
  the required Hunk relocation hunks for the supported relocation kinds
- if relocation metadata is required but unavailable, payload construction must
  fail with a deterministic diagnostic
- when relocation metadata becomes available, `HUNK_RELOC32` support should be
  added as a separate, testable capability

FS-UAE integration must consume a generated Hunk executable through a separate
test harness layer. The output component produces a file. It does not launch an
emulator, write AmigaDOS startup scripts, mount host directories, or interpret
program output.

## Boundary Cases

Unknown output format:

- `.output "x", format=foo, sections=code` fails at directive parsing or
  directive validation with a message that includes supported format IDs.

Hunk with flat-image options:

- `.output "x", format=hunk, image="$1000..$10ff", fill=$00, sections=code`
  fails because `image` and `fill` are raw-image semantics.

Hunk with `loadaddr`:

- `.output "x", format=hunk, loadaddr=$1000, sections=code` fails because
  AmigaDOS chooses segment load addresses.

Hunk with unplaced sections:

- a selected section without an assigned base must fail unless a future
  relocation-aware model explicitly permits unplaced Hunk segment emission.

Hunk with BSS:

- BSS must not write payload bytes. Its segment size must be encoded in
  longwords using the same rounding rules as code/data allocation sizes.

Hunk with initialized bytes smaller than reserved size:

- code/data payload hunks must encode the initialized payload length, while the
  header segment-table entry still encodes the reserved allocation size.

Hunk with non-longword payload length:

- code/data payload bytes must be padded to a 4-byte boundary in the file, and
  the encoded size must describe the padded longword count.

Hunk with non-code first section:

- if the first section named in `sections=` does not map to `HUNK_CODE`, the
  component must fail because AmigaDOS executable entry begins at segment 0.

Hunk with absolute relocations:

- if opForge cannot prove that the payload is relocation-free and cannot emit
  relocation hunks from explicit relocation records, the component must fail
  rather than emit a Hunk executable that only works accidentally at one
  address.

FS-UAE not installed:

- normal unit and reference tests must still pass. FS-UAE tests must be
  opt-in, skipped, or separately gated.

## Acceptance Criteria

- [ ] `.output "build/out.hunk", format=hunk, sections=code` is accepted by
  directive parsing once Hunk output is implemented.
- [ ] The generic output path resolves `bin`, `prg`, and `hunk` through output
  components rather than hardcoding all format behavior in one payload builder.
- [ ] The `.output` directive model preserves the selected format identifier and
  raw option bag until component-side validation runs.
- [ ] Existing `bin` and `prg` tests pass unchanged or with only expectation
  updates required by diagnostic wording.
- [ ] A minimal code-only Hunk payload emits `HUNK_HEADER`, resident-library
  count `0`, segment-table size `1`, first-segment index `0`, last-segment
  index `0`, exactly one segment-table entry with `MEMF_ANY`, one `HUNK_CODE`
  payload, padded code bytes, and `HUNK_END` using big-endian 32-bit words.
- [ ] Hunk output rejects `image`, `fill`, and `loadaddr` with format-specific
  diagnostics.
- [ ] Hunk output rejects `contiguous` and preserves `sections=` order instead
  of sorting selected sections by base address.
- [ ] Hunk output rejects cases that require relocation records before opForge
  has relocation metadata.
- [ ] BSS sections selected for Hunk output are represented with `HUNK_BSS`
  allocation size and no payload bytes.
- [ ] Code/data sections whose reserved size exceeds initialized payload length
  encode those two sizes distinctly in the Hunk header and payload hunks.
- [ ] Hunk output rejects configurations where the preserved first emitted
  segment would not be `HUNK_CODE`.
- [ ] An optional external FS-UAE test can run a generated Hunk executable when
  FS-UAE is installed, but FS-UAE absence does not fail the default workspace
  test suite.

## Validation Expectations

Spec validation:

- `python3 scripts/workflow/check_spec_artifact.py documentation/opForge-amiga-hunk-output-component-spec-v0_1.md`

Expected implementation validation derived from this spec:

- focused unit tests for output component resolution
- focused payload-byte tests for minimal `format=hunk` output, including the
  exact regular-executable header words
- existing linker-output tests for `bin` and `prg`
- a negative test for `format=hunk` with flat-image options
- a negative test for relocation-required Hunk output until relocation metadata
  exists
- a byte-level test that distinguishes reserved allocation size from initialized
  payload length for code/data and BSS segments
- a negative test that verifies preserved `sections=` order and rejection of a
  non-code first segment
- an opt-in external test that runs the generated executable under FS-UAE once
  the harness exists

## Resolved v0.1 Decisions

- The required user-facing format identifier is `hunk`.
- Hunk executable output emits one segment per selected opForge section and
  preserves the user-declared `sections=` order.
- The first emitted segment must be `HUNK_CODE`; configurations that would make
  segment 0 data or BSS are rejected.
- Hunk header segment-table entries default to zero memory-type bits
  (`MEMF_ANY`) until a later spec adds per-section Amiga memory attributes.
- Safe relocation omission requires an explicit relocation-free proof flag;
  otherwise the model must carry relocation records, or output must fail.
- FS-UAE launcher or ABI-wrapper details are follow-on harness work and are not
  part of the Hunk payload writer contract in this spec.

## Open Questions

- None for v0.1. The format identifier, segment ordering, relocation contract,
  and default memory-type behavior are fixed by this spec revision.
