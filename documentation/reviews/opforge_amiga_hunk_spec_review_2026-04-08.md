# Review Report

## Scope

Review [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md) for accuracy against the cited AmigaOS 3 DOS Reference Manual section 11.2 executable Hunk format, and for implementability against the current opForge `.output` parsing and artifact-emission path in [crates/opforge-asm/src/asmline_directives_metadata.rs](crates/opforge-asm/src/asmline_directives_metadata.rs#L860-L1143), [crates/opforge-vm/src/output_model.rs](crates/opforge-vm/src/output_model.rs#L17-L214), [crates/opforge-vm/src/output_artifacts.rs](crates/opforge-vm/src/output_artifacts.rs#L31-L229), [crates/opforge-asm/src/output.rs](crates/opforge-asm/src/output.rs#L33-L38), and [crates/opforge-engine/src/lib.rs](crates/opforge-engine/src/lib.rs#L580-L604). Focus: official Hunk executable syntax and constraints, required or omitted fields, and the minimum opForge model changes needed to implement the spec faithfully.

## Findings

### RVW-2026-04-08-001

- Severity: high
- File: [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L38-L42), [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L103-L105), [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L162-L168), [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L232-L233), [crates/opforge-vm/src/output_model.rs](crates/opforge-vm/src/output_model.rs#L17-L27), [crates/opforge-vm/src/output_model.rs](crates/opforge-vm/src/output_model.rs#L175-L214), [crates/opforge-vm/src/output_artifacts.rs](crates/opforge-vm/src/output_artifacts.rs#L31-L74)
- Issue: The spec allows a no-relocation path without defining the upstream artifact data that would let opForge prove relocation is unnecessary. In the reviewed implementation path, the Hunk writer would only receive placed section bytes and the fixed `.output` directive fields; it does not receive relocation records, fixups, or any explicit relocation-free proof.
- Why it matters: A Hunk executable loader relies on relocation hunks when absolute inter-segment addresses exist. With the current bytes-only output model, a writer cannot soundly distinguish “no relocation required” from “relocation required but unavailable,” which creates a real risk of emitting executables that only work accidentally at one load address or of forcing blanket rejection.
- Fix direction: Make relocation or fixup capture an explicit prerequisite in the spec and require the Hunk component input model to carry relocation records, or an explicit upstream-proven relocation-free flag, instead of leaving that decision to the final payload builder.

### RVW-2026-04-08-002

- Severity: high
- File: [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L88-L101), [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L198-L204), [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L227-L235), [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L266-L267), [crates/opforge-vm/src/output_model.rs](crates/opforge-vm/src/output_model.rs#L17-L38)
- Issue: The HUNK_HEADER contract is incomplete relative to the official executable format. The spec names the header words and payload hunks, but it does not require the regular-executable invariants that the first post-header word is the legacy resident-library count and must be zero, that non-overlay executables use `tnum = 0` and `tmax = tsize - 1`, or that each segment-table word encodes memory-type bits together with size in longwords. It also does not clearly separate segment allocation size from CODE or DATA payload size.
- Why it matters: An implementation can follow the current spec and still emit a structurally wrong executable header, especially for BSS or partially initialized segments. The current opForge section model also has no field for the memory-type bits the header must encode, so the omission hides a required design decision rather than an optional enhancement.
- Fix direction: Amend the spec to define the exact regular-executable HUNK_HEADER fields, require segment-table entries to encode memory type plus size in longwords, state a v0.1 default memory type such as `MEMF_ANY`, and explicitly distinguish reserved segment size from emitted payload length.

### RVW-2026-04-08-003

- Severity: medium
- File: [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L68-L71), [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L149-L154), [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L227-L229), [crates/opforge-vm/src/output_artifacts.rs](crates/opforge-vm/src/output_artifacts.rs#L64-L74)
- Issue: The spec defines kind-to-hunk mapping but never defines emitted segment order or guarantees that segment 0 is code. The current collector sorts selected sections by absolute base address before payload construction.
- Why it matters: The official executable format starts execution at the first byte of the first segment, and the manual states that the first segment of an executable should be represented by `HUNK_CODE`. If the current collector behavior is reused as-is, a lower-address data or BSS section can become segment 0, producing a formally structured file that is not a valid executable entry layout.
- Fix direction: Add an explicit Hunk segment-order rule to the spec that preserves the user-declared `sections=` order for `format=hunk` and rejects the directive unless the first emitted segment maps to `HUNK_CODE`.

### RVW-2026-04-08-004

- Severity: medium
- File: [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L63-L81), [documentation/opForge-amiga-hunk-output-component-spec-v0_1.md](documentation/opForge-amiga-hunk-output-component-spec-v0_1.md#L116-L146), [crates/opforge-asm/src/asmline_directives_metadata.rs](crates/opforge-asm/src/asmline_directives_metadata.rs#L860-L1143), [crates/opforge-vm/src/output_model.rs](crates/opforge-vm/src/output_model.rs#L175-L214)
- Issue: The spec says generic `.output` handling stays separate from format-specific option validation and that components conceptually own `validate_options`, but the current parser and stored directive schema do not support that behavior yet. `.output` parsing currently hardcodes `bin` and `prg`, rejects unknown formats before resolution, and enforces `image` or `fill` semantics generically before any format-specific component could see the option set.
- Why it matters: Within the current opForge framework, adding `format=hunk` is not just a new payload builder. The parser and directive model must also change if the implementation is expected to match the spec’s delegated validation and future pluggability claims. Without that clarification, the spec understates the minimum framework work required.
- Fix direction: Update the spec to require `LinkerOutputDirective` to carry a format identifier plus a raw `.output` option bag so the parser only performs shared syntactic validation and all format-specific validation happens after component resolution.

## Testing Gaps

- No acceptance test requires the exact regular-executable `HUNK_HEADER` words, including the zero resident-library count, `tnum = 0`, `tmax = tsize - 1`, and memory-type bits in each segment-table entry.
- No byte-level test distinguishes segment allocation size from emitted payload size for partially initialized CODE or DATA segments, or for `HUNK_BSS` segments with no payload bytes.
- No test requires executable Hunk output to emit a code segment first or reject configurations where data or BSS would become segment 0.
- No planned test exercises the relocation safety boundary with actual relocation-bearing input artifacts; the current reviewed output path has no relocation-bearing model to validate against.

## Residual Risks

- This review covered the spec and the current `.output` parsing and artifact-emission path only. I did not audit the full 68000 encoding pipeline for every place relocation metadata might eventually be captured.
- The review used the cited AmigaDOS manual section 11.2 for executable syntax, not full loader compatibility testing under FS-UAE or a real AmigaOS loader.
- Optional Hunk forms such as debug, symbol, overlay, or `HUNK_RELOC32SHORT` behavior remain intentionally out of scope here; additional compatibility constraints may surface when those are added later.

## Brief Summary

The spec is directionally correct about the top-level Hunk executable shape and about keeping FS-UAE outside the generic output writer, but it is not yet accurate enough to guarantee a faithful implementation in the current opForge framework. The main gaps are the missing relocation data contract, an underspecified `HUNK_HEADER`, and the absence of a deterministic executable segment-order rule, plus understated parser or directive-model work for the proposed component seam.