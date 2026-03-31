# Motorola 68000 Documented Divergence Analysis

Source: `examples/ab/motorola68000/vasm/*/documented_divergence/`

## Legend

| `documented_divergence_kind` | Meaning |
|---|---|
| `opforge_error_oracle_success` | vasm succeeds, opForge errors |
| `opforge_success_oracle_success_bytes_differ` | Both assemble but produce different bytes |
| `opforge_success_oracle_error` | opForge assembles, vasm rejects |

---

## 🔴 opForge Issues (opForge is wrong)

| Fixture | CPU(s) | Kind | Problem |
|---|---|---|---|
| `reclassification_candidate.asm` | 68000 | `opforge_error_oracle_success` | opForge errors on basic 68000 code (`moveq`, `move.w`, `addq`, `rts`). Acknowledged reclassification candidate — should be a positive case. |
| `68040_fnop_error.asm` | 68040-fpu | `opforge_error_oracle_success` | FNOP is architecturally FBF.W — part of the FBcc family, which is valid on the 68040 integrated FPU. opForge incorrectly rejects FNOP under `.fpu 68040`. vasm is correct. |
| `68020_link_long.asm` | 68020 | `bytes_differ` | LINK.L produces different byte encoding vs. vasm. Root cause unverified — may be a vasm size-optimization (LINK.W) rather than an opForge encoding error; capture vasm output bytes before fixing. |
| `68030_link_long.asm` | 68030 | `bytes_differ` | Same LINK.L encoding divergence on 68030. Root cause unverified; same caveat applies. |
| `68040_link_long.asm` | 68040 | `bytes_differ` | Same LINK.L encoding divergence on 68040. Root cause unverified; same caveat applies. |
| `68020_div_long_aliases.asm` | 68020 | `bytes_differ` | DIVSL.L and DIVUL.L encode differently from vasm — opForge encoding likely wrong. |
| `68020_fpu_fnop.asm` | 68020+68881 | `bytes_differ` | FNOP encodes differently from vasm under the 68881 FPU profile. |
| `68020_fpu_fnop.asm` | 68020+68882 | `bytes_differ` | FNOP encodes differently from vasm under the 68882 FPU profile. |

---

## 🟡 vasm Issues (vasm is wrong or too restrictive)

| Fixture | CPU | Kind | Problem |
|---|---|---|---|
| `68030_callm_rtm.asm` | 68030 | `opforge_success_oracle_error` | CALLM/RTM are valid 68030 instructions — they were removed from the **68040** ISA, not the 68030. vasm incorrectly rejects them on 68030. opForge is correct. |
| `68040_fsin_error.asm` | 68040-fpu | `opforge_error_oracle_success` | vasm allows FSIN on the 68040 integrated FPU; FSIN is not directly executed by 68040 hardware and requires FPSP F-line emulation (vector 55). opForge correctly rejects it. |
| `68040_fetox_error.asm` | 68040-fpu | `opforge_error_oracle_success` | Same: FETOX is not directly executed by the 68040 integrated FPU; requires FPSP F-line emulation. opForge correctly rejects it. vasm too permissive. |
| `68020_pack_negative.asm` | 68020 | `opforge_success_oracle_error` | The PACK adjustment word is a 16-bit field; negative values are valid per spec. vasm incorrectly rejects them. opForge is correct. |

---

## 🔵 Syntax Divergence (decision required)

| Fixture | CPU(s) | Kind | Problem |
|---|---|---|---|
| `68020_fpu_fsincos_pair.asm` | 68020+68881, 68020+68882 | `opforge_success_oracle_error` | opForge uses `.pair(FPs,FPc)` syntax for FSINCOS that vasm does not accept. The canonical M68881/M68882 syntax is `FSINCOS.<fmt> FPm,FPc:FPs` (colon-separated; FPc receives cosine, FPs receives sine). The `.pair()` form is an opForge-specific extension with no Motorola precedent. Decision required: migrate to canonical colon syntax, or keep extension with register-assignment order explicitly documented. |

---

## ⚪ Intentional / By-Design

| Fixture | CPU | Kind | Notes |
|---|---|---|---|
| `opt_directive.asm` | 68000 | `opforge_error_oracle_success` | vasm's `OPT` directive is intentionally outside the shared opForge/vasm subset. Not a bug. |

---

## Summary

| Category | Count |
|---|---|
| opForge encoding bugs to fix | 6 (LINK.L ×3 ⚠️, DIVSL/DIVUL ×1, FNOP byte ×2) |
| opForge rejection bugs to fix | 2 (reclassification-candidate, FNOP on 68040-fpu) |
| vasm too permissive (opForge correct) | 2 (68040-fpu: FSIN, FETOX) |
| vasm too restrictive (vasm bug) | 2 (PACK negative immediate, CALLM/RTM on 68030) |
| Syntax decision needed | 1 (FSINCOS `.pair` syntax) |
| Intentional design gap | 1 (OPT directive) |

⚠️ LINK.L: attribution unverified — divergence may be a vasm size-optimization (LINK.W emission) rather than an opForge encoding error. Capture vasm output bytes before making any code change.

### Priority opForge fixes

1. **FNOP rejected on 68040 integrated FPU** — FNOP is a valid FBcc instruction; opForge must accept it under `.fpu 68040`
2. **FNOP byte encoding** — 68020+68881 and 68020+68882 (bytes differ from vasm)
3. **DIVSL.L / DIVUL.L encoding** — 68020
4. **LINK.L encoding** — 68020, 68030, 68040 (verify vasm output bytes first; may not be an opForge bug)
5. **Reclassification candidate** — basic 68000 code incorrectly erroring
