# Review Report

## Scope

Factual accuracy review of `documentation/68000-divergence-analysis.md` against
authoritative Motorola 680x0 CPU specifications and the opForge internal
specification documents:

- `documentation/opForge-m68000-family-68010-68020-68030-68040-cpu-extension-spec-v0_1.md`
- `documentation/opForge-m68000-family-68020-68030-68040-mmu-fpu-extension-spec-v0_1.md`
- M68000 Family Programmer's Reference Manual (Motorola)
- M68020/M68030/M68040 User's Manuals (Motorola)
- M68881/M68882 Floating-Point User's Manual (Motorola)

The review is not limited to a branch diff. It verifies each claim in the
divergence analysis — ISA membership, instruction encoding correctness, and
valid/invalid instruction sets per CPU variant.

Three model reviewer subagents (GPT-5.4, Claude Opus 4.6, Gemini 3.1 Pro) were
run read-only in parallel. Their candidate findings were adjudicated against
direct evidence from the internal specification files before being merged.

## Findings

### RVW-2026-03-31-001

- Severity: critical
- File: `documentation/68000-divergence-analysis.md`, "opForge Issues" table, row `68030_callm_rtm.asm`
- Issue: The document states "CALLM/RTM were removed from the 68030 ISA" and classifies opForge as "too permissive" for assembling them on 68030. This is factually wrong. Per `opForge-m68000-family-68010-68020-68030-68040-cpu-extension-spec-v0_1.md` lines 187 and 195, CALLM and RTM have the availability matrix `No/Yes/Yes/No` (68000/68020/68030/68040) with the note "Removed on m68040." The spec is explicit at line 454: "CALLM is illegal on m68040 and must be rejected even though it is legal on m68020 and m68030." CALLM and RTM are valid 68030 instructions. opForge is correct to assemble them. vasm is the defective party.
- Why it matters: This misclassification sends fix effort to the wrong assembler. Any engineer acting on this document would attempt to add a rejection guard in opForge for 68030 — which is exactly wrong. The summary currently counts this as an "opForge permissiveness bug to fix" when it is a vasm bug.
- Fix direction: Move `68030_callm_rtm.asm` from the "opForge Issues" table to the "vasm Issues" table. Update the problem description to read "CALLM/RTM were removed from the 68040 ISA (not 68030); vasm incorrectly rejects them on 68030 while opForge correctly assembles them." Update the Summary counts accordingly.

### RVW-2026-03-31-002

- Severity: critical
- File: `documentation/68000-divergence-analysis.md`, "vasm Issues" table, row `68040_fnop_error.asm`
- Issue: The document states "FNOP is coprocessor-only. vasm too permissive on integrated 68040 FPU." This is factually wrong. Per `opForge-m68000-family-68020-68030-68040-mmu-fpu-extension-spec-v0_1.md`, the FPU instruction matrix lists "Floating-point conditional families (FBcc, FDBcc, FScc, FTRAPcc) | Yes | Yes | Yes" for all three FPU targets including `.fpu 68040`. FNOP is architecturally encoded as FBF.W *+2 — it is a member of the FBcc family and therefore valid on the 68040 integrated FPU. The M68040 User's Manual Table 9-1 confirms FNOP as an on-chip FPU instruction. The fixture kind is `opforge_error_oracle_success`, meaning opForge rejects FNOP and vasm accepts it. vasm is correct.
- Why it matters: This misclassification marks a real opForge bug as a vasm defect. opForge should accept `FNOP` under `.fpu 68040` because the FBcc family is in scope. Leaving the current label in place will block the fix from being identified and will mislead reviewers about which assembler needs to change.
- Fix direction: Move `68040_fnop_error.asm` from the "vasm Issues" table to the "opForge Issues" table. Add FNOP explicitly to the opForge 68040 FPU allowlist (or confirm FBcc dispatch covers it). Update the Summary counts accordingly.

### RVW-2026-03-31-003

- Severity: high
- File: `documentation/68000-divergence-analysis.md`, Summary table
- Issue: The Summary counts reflect the two misclassifications above, producing incorrect totals on three rows: (a) "opForge permissiveness bugs to fix: 2" is overstated by one — CALLM/RTM on 68030 is a vasm bug, not an opForge bug; (b) "vasm too permissive (opForge correct): 3" is overstated by one — FNOP on 68040 is an opForge bug, not a vasm defect; (c) the "opForge encoding bugs to fix: 5" arithmetic is independently wrong regardless of reclassification: LINK.L ×3 + DIVSL/DIVUL ×1 + FNOP byte ×2 = 6, not 5. The FNOP-on-68040 opForge bug also needs a new entry in the opForge fix list.
- Why it matters: Incorrect summary counts make the document unreliable for planning and triage. Reviewers and engineers who trust the summary will have the wrong picture of how many opForge bugs remain versus vasm bugs.
- Fix direction: After correcting RVW-2026-03-31-001 and RVW-2026-03-31-002, recount all summary rows: "opForge permissiveness bugs to fix" becomes 1 (reclassification-candidate only); "vasm too permissive" becomes 2 (FSIN, FETOX only); add a new "opForge rejection bug on 68040 FPU: 1 (FNOP)"; fix the encoding count arithmetic to 6; add FNOP-on-68040 as item 6 in the Priority opForge Fixes list.

### RVW-2026-03-31-004

- Severity: medium
- File: `documentation/68000-divergence-analysis.md`, "opForge Issues" table, rows `68020_link_long.asm`, `68030_link_long.asm`, `68040_link_long.asm`
- Issue: The document attributes the LINK.L byte divergence to opForge with the note "Likely an opForge encoding bug." The canonical LINK.L encoding per the M68020/M68030/M68040 Programmer's Reference Manual is opcode `0100 1000 0000 1nnn` followed by a 32-bit displacement word. For `LINK.L A6,#-8` this yields `480E FFFF FFF8`. This encoding is also confirmed by the internal spec (line 191 of the CPU extension spec: `LINK.L | No | Yes | Yes | Yes | Added on m68020`). The most likely explanation for the divergence is that vasm silently optimizes an explicit `.L` suffix to `.W` when the displacement fits in 16 bits — a size-reduction optimization — not a Motorola spec violation by opForge. Without capturing vasm's actual output bytes for this case, calling it an "opForge encoding bug" is unsupported attribution.
- Why it matters: If the divergence is a vasm optimization (not an encoding error), the fix direction reverses: opForge is not wrong, and the document would be misleading three fixtures and the top priority fix item toward the wrong component.
- Fix direction: Capture the actual bytes vasm emits for `LINK.L A6,#-8` on 68020. If vasm emits the 4-byte LINK.W form (`4E56 FFF8`), reclassify these three entries as "vasm size-optimization divergence" with opForge correct; if vasm emits a different 6-byte LINK.L form than opForge, document the exact difference with both byte sequences before attributing the bug to either side.

### RVW-2026-03-31-005

- Severity: medium
- File: `documentation/68000-divergence-analysis.md`, "vasm Issues" table, rows `68040_fsin_error.asm` and `68040_fetox_error.asm`; also fixture comment lines in `68040_fsin_error.asm`, `68040_fnop_error.asm`, `68040_fetox_error.asm`
- Issue: The document and the `.asm` comment lines describe FSIN, FETOX, and (before correction per RVW-2026-03-31-002) FNOP as "external-coprocessor-only" mnemonics. This terminology is inaccurate for the 68040 context. On the 68040, these instructions are "unimplemented" — the hardware recognizes their opcodes, generates an F-line emulation exception (vector 55), and expects the FPSP (Motorola's Floating-Point Software Package) to service them. They are not routed to an external coprocessor bus. The internal spec correctly describes these as staying "external-coprocessor-only in the shipped 68040 integrated-core surface," which is a design-scope statement, not a hardware claim. The `.asm` comment lines use language that will be read against hardware architecture documentation and will appear inconsistent.
- Why it matters: A developer reading "external-coprocessor-only" in the context of a 68040 system will look for a coprocessor interface, not an F-line exception handler. The imprecise terminology obscures the real constraint and could generate confusion in future documentation that cites these fixture comments.
- Fix direction: Replace "external-coprocessor-only" in the `68040_fsin_error.asm` and `68040_fetox_error.asm` comment lines, and in the corresponding analysis table rows, with "not directly executed by the 68040 integrated FPU — requires FPSP F-line emulation exception handling." Add a parenthetical in the analysis table noting that this is an opForge assembler-scope decision to exclude the FPSP-dependent surface.

### RVW-2026-03-31-006

- Severity: low
- File: `documentation/68000-divergence-analysis.md`, "Syntax Divergence" table, row `68020_fpu_fsincos_pair.asm`
- Issue: The document notes that vasm rejects the opForge `.pair(FPs,FPc)` syntax for FSINCOS but does not state what the canonical Motorola syntax is. The M68881/M68882 Floating-Point User's Manual defines FSINCOS syntax as `FSINCOS.<fmt> FPm,FPc:FPs` — a colon-separated register pair where the first register after the comma receives the cosine and the second receives the sine. The `.pair(FPs,FPc)` form is an opForge-specific extension with no Motorola precedent, and its internal register assignment order (cosine vs. sine) is not documented in the analysis.
- Why it matters: Without the canonical syntax stated, the "decision required" call-to-action gives no concrete target. Without documenting the `.pair()` register order convention, any engineer implementing a syntax migration could silently swap sine and cosine registers, producing wrong outputs without an obvious test failure.
- Fix direction: Add the canonical M68881/M68882 syntax `FSINCOS.<fmt> FPm,FPc:FPs` to the analysis row and document that `FPc` (cosine) comes first in the colon notation. Document the `.pair()` argument order explicitly as opForge-only so register assignment is traceable during any future migration to canonical syntax.

## Testing Gaps

- No byte-level golden-output annotations exist for any divergence fixture. Adding `opforge_bytes` and `oracle_bytes` fields to the TOML manifests would make root-cause analysis (e.g., LINK.L attribution) conclusive rather than inferential.
- No positive-case fixture asserts that CALLM and RTM assemble correctly on 68030 with expected bytes. The only 68030 CALLM/RTM fixture is in `documented_divergence/` and is currently misclassified; after correction a positive fixture with byte assertion is needed to lock in correct behavior.
- No positive fixture asserts that FNOP assembles correctly under `.fpu 68040` after the expected opForge fix (RVW-2026-03-31-002).
- No unit test explicitly validates that FNOP encoding under `.fpu 68881`/`.fpu 68882` matches the canonical M68881 FNOP opcode (`F280 0000`).
- No rejection test exists for CALLM/RTM on 68040 (the CPU where they are correctly illegal), creating a risk that fixing the 68030 misclassification could silently regress 68040 rejection.

## Residual Risks

- **LINK.L root cause unresolved**: Without captured vasm output bytes for the LINK.L divergence, it is not possible to confirm whether the issue is an opForge encoding error or a vasm size-optimization divergence. The fix direction from RVW-2026-03-31-004 must be carried out before any code change is made for these three fixtures.
- **DIVSL.L / DIVUL.L encoding**: The document's "bytes_differ" classification is plausible given the M68020 extension-word SIZE bit semantics, but no byte-level verification was performed in this review. Treating this as a confirmed opForge encoding bug is reasonable as a working hypothesis but should be validated against opForge's actual output before committing a fix.
- **FNOP canonical encoding on 68020+68881/68882**: The document records a `bytes_differ` divergence for FNOP under external FPU profiles. The canonical FNOP per the M68881 manual is `F280 0000` (FBF.W *+2). If opForge emits a different byte sequence, the encoding is wrong. The actual divergent bytes are not recorded, so the specific opForge error cannot be diagnosed from the document alone.
- **vasm CALLM/RTM 68030 mode**: vasm may intentionally exclude CALLM/RTM from its 68030 target as a design choice rather than a defect. This does not change the factual verdict (CALLM/RTM are valid 68030 instructions per Motorola specifications and the opForge internal spec), but the divergence note should acknowledge this as possible deliberate vasm behavior rather than a straightforward vasm bug.

## Brief Summary

The divergence analysis is **broadly structurally sound** but contains two critical
factual errors that invert the blame assignment for specific fixtures, and four
supporting findings of medium to low severity.

**Critical error 1 (RVW-2026-03-31-001):** CALLM and RTM are valid 68030
instructions. The document incorrectly states they were "removed from the 68030
ISA." They were removed from the 68040. opForge is correct to assemble them on
68030; this fixture belongs in the vasm Issues section.

**Critical error 2 (RVW-2026-03-31-002):** FNOP is a valid 68040 integrated-FPU
instruction (architecturally FBF.W, part of the FBcc family, which the spec
lists as Yes for `.fpu 68040`). The document incorrectly calls vasm "too
permissive." opForge is the party with the bug; this fixture belongs in the
opForge Issues section.

Both errors produce wrong Summary counts (RVW-2026-03-31-003). The LINK.L
attribution to opForge is unverified and likely reversed once vasm bytes are
captured (RVW-2026-03-31-004). The "external-coprocessor-only" terminology for
68040 FSIN/FETOX is technically imprecise (RVW-2026-03-31-005). The FSINCOS
section omits the canonical syntax needed to act on its own decision request
(RVW-2026-03-31-006).

The claims that are factually correct and require no change: PACK negative
immediates are valid per M68020 spec (vasm too restrictive); FSIN and FETOX are
correctly excluded from the 68040 integrated-core FPU surface; the `.pair()`
FSINCOS syntax is non-standard; the OPT directive exclusion is intentional by
design; DIVSL.L/DIVUL.L bytes_differ classification is a plausible working
hypothesis pending byte verification.
