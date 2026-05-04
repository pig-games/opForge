# Review Report

## Scope

Targeted 68020 native AmigaOS opForge optimization review. The review focused
on the assembly code used by the native CLI/tokenizer package path under:

- `examples/motorola68000/amigaos/opforge/`
- `examples/motorola68000/amigaos/tkpkg/`
- `examples/motorola68000/amigaos/prvm/`

The review specifically looked for material code-size or runtime patterns like
fragmented pointer bumps, byte-by-byte clearing/copying of fixed records, and
unnecessarily large address-register copy idioms.

## Findings

### RVW-2026-05-03-001

- Severity: low
- File: `examples/motorola68000/amigaos/tkpkg/tkpkg_token_policy.asm:107`
- Issue: `tkpkg_token_policy_skip_toks_entry_v1` advances `A2` over a fixed
  9-byte record prefix using three separate instructions: `ADDQ.W #1,A2`,
  `ADDQ.W #4,A2`, and `ADDQ.W #4,A2`.
- Why it matters: This routine is used while scanning token-policy records, so
  the extra instructions are paid once per skipped or matched `toks` entry. It
  also obscures that the code is simply skipping a fixed-size prefix before the
  first variable-length string.
- Fix direction (one direction only; resolve competing options before finalizing): Replace the three increments with one fixed displacement address update, `LEA 9(A2),A2`, and add a named record-prefix constant if the layout needs to remain self-documenting.

### RVW-2026-05-03-002

- Severity: low
- File: `examples/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm:517`
- Issue: `tkpkg_pipeline_skip_tokenizer_vm_entry_v1` has the same fragmented
  fixed-field skipping pattern in two places: `ADDQ.W #2,A2` plus
  `ADDQ.W #2,A2` for the prefix, then `ADDQ.W #2,A2`, `ADDQ.W #1,A2`, and four
  `ADDQ.W #4,A2` instructions for the fixed tail.
- Why it matters: Tokenizer VM records are scanned while resolving the active
  pipeline. Collapsing the fixed prefix and fixed tail reduces instruction
  count and listing noise in a path that can walk multiple package records.
- Fix direction (one direction only; resolve competing options before finalizing): Replace the fixed prefix with `LEA 4(A2),A2` and the fixed tail with `LEA 19(A2),A2`, using named constants for the prefix and tail sizes.

### RVW-2026-05-03-003

- Severity: low
- File: `examples/motorola68000/amigaos/tkpkg/tkpkg_token_policy.asm:52`
- Issue: Locator state is cleared byte-by-byte with five separate byte clears
  in `tkpkg_token_policy_find_owner_v1`: four bytes of locator plus one owner
  tag. The same locator-clear shape appears in `tkpkg_pipeline.asm` for pending
  dialect and optional locator state.
- Why it matters: These are fixed-size runtime records, not byte streams. The
  current code burns four to five instructions where a long clear plus, where
  needed, one byte clear would express the record shape more directly and reduce
  code size. This also makes it harder to audit which fields belong to a single
  locator record.
- Fix direction (one direction only; resolve competing options before finalizing): Define aligned locator-record storage and clear locators with `CLR.L (Ax)+`, followed by `CLR.B (Ax)` only for the trailing owner byte where that fifth byte is part of the state being reset.

### RVW-2026-05-03-004

- Severity: low
- File: `examples/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm:586`
- Issue: `tkpkg_pipeline_copy_record_locator_v1` copies a fixed four-byte
  locator record using four byte moves.
- Why it matters: This helper is used when committing resolved pipeline
  selections. Four byte moves make the helper larger and slower than the
  record-sized operation it represents, and they preserve the broader pattern
  of treating locator records as loose byte runs instead of structured fields.
- Fix direction (one direction only; resolve competing options before finalizing): Align locator records and replace the helper body with one long move, `MOVE.L (A3),(A2)`, or `MOVE.L (A3)+,(A2)+` if callers should continue receiving post-incremented pointers.

### RVW-2026-05-03-005

- Severity: low
- File: `examples/motorola68000/amigaos/tkpkg/tkpkg_package_loader.asm:82`
- Issue: `tkpkg_package_loader_clear_loaded_state_v1` clears 160 bytes with a
  byte loop (`CLR.B (A3)+` / `DBF`) starting at `packageStateFlags`.
- Why it matters: Package loading resets this state before loading a package,
  and 160 byte iterations are far more work than the fixed clear requires. A
  long-sized loop would cut the loop count to 40 and make the state-reset
  boundary more explicit.
- Fix direction (one direction only; resolve competing options before finalizing): Make the cleared package-state region explicitly even-aligned and replace the byte loop with a long clear loop over 40 longwords.

### RVW-2026-05-03-006

- Severity: low
- File: `examples/motorola68000/amigaos/tkpkg/tkpkg_pipeline.asm:199`
- Issue: The pipeline resolver repeatedly uses `LEA 0(Ax),Ay` as an
  address-register copy, for example `LEA 0(A1),A5`, `LEA 0(A4),A1`, and
  `LEA 0(A5),A2`. The same idiom also appears in
  `tkpkg_token_policy.asm`.
- Why it matters: `LEA 0(Ax),Ay` encodes as a larger effective-address
  calculation where the code only needs to copy an address register. These
  copies occur inside record-scanning and string-compare paths, so the repeated
  extra bytes and work accumulate.
- Fix direction (one direction only; resolve competing options before finalizing): Replace zero-displacement `LEA 0(Ax),Ay` address-register copies with `MOVEA.L Ax,Ay` throughout the native tkpkg resolver code.

## Testing Gaps

No cycle-count or hunk-size benchmark was run for this review. The findings are
based on source inspection and generated-listing patterns, not measured FS-UAE
timings.

The long-clear and long-copy recommendations require an alignment check or an
alignment adjustment before implementation. That check should be part of the
remediation slice rather than assumed from the current byte-oriented BSS layout.

## Residual Risks

These findings are optimization-focused and intentionally avoid claiming
behavioral regressions. The main implementation risk is changing pointer
post-increment side effects while collapsing byte operations. Remediation should
therefore update one helper at a time and keep the existing native tokenizer
and FS-UAE gates green.

## Brief Summary

The native 68020 package path has several small but real code-size and runtime
optimization opportunities. The highest-value cleanup is to treat fixed package
records as records: use single displacement `LEA` for fixed skips, long-sized
clear/copy operations for aligned locator records, and `MOVEA.L` for address
register copies.
