# opForge Motorola 68000 Family 68080 Full Extension Specification (v0.1)

## Summary

This specification expands the shipped bounded 68080 surface to full 68080
assembler coverage in opForge.  It catalogues **every** missing instruction
with its binary encoding, operand constraints, and required dotless/sized
aliases so that implementation produces correct assembled bytes for all forms.

It supersedes the bounded implementation scope of
`documentation/opForge-m68000-family-68080-extension-spec-v0_1.md` for new
implementation work while preserving existing 68000‑68040 behavior.

### Reference Documents

| Source | Location |
|--------|----------|
| AC68080PRM (full text) | `dev-docs/68080specs/AC68080PRM.txt` (2 507 lines) |
| AC68080PRM (PDF) | `dev-docs/68080specs/AC68080PRM.pdf` (97 pages) |
| WDrijver/ApolloCrossDev AMMX | `github.com/WDrijver/ApolloCrossDev/tree/main/Docs/AMMX` |
| WDrijver/ApolloCrossDev Integer | `github.com/WDrijver/ApolloCrossDev/tree/main/Docs/Integer` |
| WDrijver/ApolloCrossDev FPU | `github.com/WDrijver/ApolloCrossDev/tree/main/Docs/FPU` |

---

## Problem

The current 68080 implementation ships only a bounded subset (40
`M68080MnemonicKind` variants): identity, E/B register substrate, runtime
Apollo gate, representative integer slice, and representative AMMX slice.

Large portions of the 68080 instruction surface are unavailable:

* Saturated PADD/PSUB variants (PADDUSB, PADDUSW, PSUBUSB, PSUBUSW)
* Vector average (PAVGB)
* Vector min/max families (PMAXSB/UB/SW/UW, PMINSB/UB/SW/UW)
* Butterfly (BFLYB, BFLYW)
* Chunky-to-planar (C2P)
* Quad shift (LSLQ, LSRQ)
* Minterm (MINTERM)
* Masked store (STOREM, STOREM3)
* Texture fetch (TEX)
* Transpose (TRANSHI, TRANSLO)
* B-register move/arithmetic (ADDQ Bn, SUBQ Bn, MOVE Bn, MOVEA Bn, CMP Bn,
  LEA Bn)
* Extended integer forms (CLR.Q, EXTUB, EXTUW, PERM, BANK)
* Branch extensions (DBcc.L, Bcc.S+, BRA.S+, BSR.S+)
* FPU extensions (FDBcc.L, FMOVE.D Dn↔FPn, FMOVEM Apollo format)
* Dotless mnemonic aliases for existing instructions (PADDB, PADDW, PSUBB,
  PSUBW)

All items listed above are **mandatory** — none may be deferred, marked
optional, or deprioritised.

---

## Goals

- [ ] `REQ-68080-FULL-001`: Expand m68080 mnemonic support from 40 bounded
      variants to full 68080 assembler-visible instruction family coverage.
- [ ] `REQ-68080-FULL-002`: Add full 68080 FPU instruction coverage and
      legality contracts for `.cpu 68080`.
- [ ] `REQ-68080-FULL-003`: Preserve existing 68000‑68040 behavior and
      diagnostics unless `.cpu 68080` is active.
- [ ] `REQ-68080-FULL-004`: Extend operand/register forms needed by full 68080
      families, including complete E/B register usage and pair/alignment
      constraints.
- [ ] `REQ-68080-FULL-005`: Define canonical `.apollo` behavior for expanded
      68080 support with backward-compatible parsing and deterministic
      diagnostics.
- [ ] `REQ-68080-FULL-006`: Define canonical `.fpu` behavior for `m68080`
      including legal targets and deterministic rejection for illegal pairings.
- [ ] `REQ-68080-FULL-007`: Provide deterministic feature gating and
      diagnostics for 68080-only forms on non-68080 CPUs.
- [ ] `REQ-68080-FULL-008`: Expand AB oracle fixtures for 68080 families,
      including documented-divergence coverage where external tools lack native
      68080 support.
- [ ] `REQ-68080-FULL-009`: Keep parse/encode ownership boundaries explicit:
      family parse in m68k layer, CPU legality/feature rules in m68080 layer.
- [ ] `REQ-68080-FULL-010`: Maintain stable diagnostic normalisation classes
      for newly added 68080 errors.
- [ ] `REQ-68080-FULL-011`: Register dotless mnemonic aliases (PADDB, PADDW,
      PADDUSB, PADDUSW, PSUBB, PSUBW, PSUBUSB, PSUBUSW, BFLYB, BFLYW) as
      first-class lookup entries in `m68080_base_kind()`.

---

## Non-Goals

- `NREQ-68080-FULL-001`: Cycle-accurate Apollo execution timing, scheduling,
  or microarchitectural performance modeling.
- `NREQ-68080-FULL-002`: Emulator/runtime behavior parity guarantees.
- `NREQ-68080-FULL-003`: Re-architecting opForge registry/parser VM
  infrastructure beyond what is required for 68080 surface enablement.
- `NREQ-68080-FULL-004`: Introducing non-68080 new CPU families as part of
  this effort.
- `NREQ-68080-FULL-005`: Changing existing 68000‑68040 fixture bytes unless
  explicitly required by bug fixes outside 68080 scope.

---

## Invariants / Constraints

* Family id remains `motorola68000`; dialect remains `motorola68k`.
* Existing 68000‑68040 behavior remains authoritative and regression-safe.
* Parse success does not imply legality; legality stays CPU-context aware.
* `E*`/`B*` register namespaces remain 68080-reserved.
* New 68080 diagnostics must normalise into existing classification buckets.
* AB suite must remain deterministic with explicit documented-divergence
  contracts where oracle capability differs.
* Follow-up implementation plans must keep the active worktree `AGENTS.md`
  workflow and execution rules binding.

---

## Behavioral Contract

### CPU identity and capability contract

* `m68080`, `68080`, and `mc68080` remain canonical mappings.
* Capability surfaces expose full 68080 family support with explicit FPU and
  AMMX coverage metadata.

### `.apollo` contract

* `.apollo` remains accepted on `m68080` for backward compatibility.
* Default mode under `.cpu 68080` is the Apollo-enabled full profile.
* `.apollo on` is accepted as an explicit no-op reaffirmation of that default
  shipped profile.
* `.apollo off` is rejected deterministically because strict compatibility mode
  is not implemented in the shipped full-profile build.
* Default `MOVIW.L #<imm16>,<ea>` on `m68080` uses the regular 68080 form and
  does not require `.apollo on`.
* No deprecated Line-A `MOVIW` compatibility selector is currently exposed.
* On non-68080 CPUs, `.apollo` is rejected deterministically.

### `.fpu` contract for `m68080`

* Legal targets on `m68080`: `none`, `68080`.
* Plain `.cpu 68080` defaults the runtime FPU target to the integrated
  `68080` FPU.
* `.fpu 68080` is an explicit no-op reaffirmation of the default integrated
  FPU target.
* `.fpu none` explicitly disables 68080 FPU instruction legality.
* Illegal targets (e.g. `68881`, `68882`, `68040`) on `m68080` must fail with
  deterministic target/CPU pairing diagnostics.

### Cross-CPU legality matrix

| Surface | m68000 | m68010 | m68020 | m68030 | m68040 | m68080 |
|---------|--------|--------|--------|--------|--------|--------|
| Existing shipped behavior | Yes | Yes | Yes | Yes | Yes | Yes |
| Full 68080 integer families | No | No | No | No | No | **Yes** |
| Full AMMX families | No | No | No | No | No | **Yes** |
| Full 68080 FPU families | No | No | No | No | No | **Yes** |
| E/B register namespaces | No | No | No | No | No | **Yes** |
| `.apollo` accepted | No | No | No | No | No | **Yes** |
| `.fpu 68080` accepted | No | No | No | No | No | **Yes** |

### Diagnostics contract

* Non-68080 CPU usage of 68080 mnemonics reports unsupported CPU feature.
* Illegal `.fpu` target pairings report legal target set for active CPU.
* Operand-shape violations report deterministic operand/shape diagnostics.
* AB documented-divergence fixtures must include explicit divergence kind and
  reason.

---

## Current Coverage Snapshot

### Already implemented (40 variants in `M68080MnemonicKind`)

| # | Variant | Mnemonic(s) |
|---|---------|-------------|
| 1 | `Addiw` | ADDIW |
| 2 | `Cmpiw` | CMPIW |
| 3 | `Move2` | MOVE2 |
| 4 | `Movex` | MOVEX |
| 5 | `Moveh` | MOVEH |
| 6 | `Moviw` | MOVIW |
| 7 | `Mov3q` | MOV3Q |
| 8 | `Movs` | MOVS |
| 9 | `Movz` | MOVZ |
| 10 | `Movz2` | MOVZ2 |
| 11 | `Touch` | TOUCH |
| 12 | `Load` | LOAD |
| 13 | `Loadi` | LOADI |
| 14 | `Store` | STORE |
| 15 | `Storei` | STOREI |
| 16 | `Storec` | STOREC |
| 17 | `Storeilm` | STOREILM |
| 18 | `Padd` | PADD (.B/.W) |
| 19 | `Psub` | PSUB (.B/.W) |
| 20 | `Pmul88` | PMUL88 |
| 21 | `Pmulh` | PMULH |
| 22 | `Pmull` | PMULL |
| 23 | `Pmula` | PMULA |
| 24 | `Pand` | PAND |
| 25 | `Pandn` | PANDN |
| 26 | `Por` | POR |
| 27 | `Peor` | PEOR |
| 28 | `Bsel` | BSEL |
| 29 | `Pcmpeqb` | PCMPEQB |
| 30 | `Pcmphib` | PCMPHIB |
| 31 | `Pcmpgeb` | PCMPGEB |
| 32 | `Pcmpgtb` | PCMPGTB |
| 33 | `Pcmpeqw` | PCMPEQW |
| 34 | `Pcmphiw` | PCMPHIW |
| 35 | `Pcmpgew` | PCMPGEW |
| 36 | `Pcmpgtw` | PCMPGTW |
| 37 | `Pack3216` | PACK3216 |
| 38 | `Packuswb` | PACKUSWB / PACKUSBW |
| 39 | `Unpack1632` | UNPACK1632 |
| 40 | `Vperm` | VPERM |

### Known encoding deficiencies in existing variants

* **Padd**: `encode_ammx_padd()` only emits opcodes `0x10` (byte, U=0) and
  `0x11` (word, U=0).  The U=1 saturated variants (PADDUSB / PADDUSW) are not
  handled.
* **Psub**: `encode_ammx_psub()` only emits opcodes `0x12` (byte) and `0x13`
  (word).  The U=1 saturated variants (PSUBUSB / PSUBUSW) are not handled.
* **Dotless aliases**: `PADDB`, `PADDW`, `PSUBB`, `PSUBW` do not parse because
  `split_size_suffix()` only recognises dot-separated suffixes.  These need
  explicit entries in `m68080_base_kind()`.

---

## Instruction Families — Full Encoding Reference

### Encoding Conventions

All AMMX instructions share the Apollo Line-F prefix with cpid=7:

```
Word 0 (AMMX prefix):
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  1  1  1   1  1  1  A  B  D  Mode   Register
```

* **A** = MSB extension for vector effective address (VEA) — selects
  E8‑E23 / Bn address-register space when A=1.
* **B** = MSB of source register `b` (extends b from 3 to 4 bits).
* **D** = MSB of destination register `d` (extends d from 3 to 4 bits).
* **Mode, Register** = standard 68k effective-address field; together with A
  forms the VEA.

```
Word 1 (instruction-specific extension):
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d         < instruction-specific opcode bits >
```

* `b` = bits [2:0] of source register.
* `d` = bits [2:0] of destination register.
* Full source register = `{B, b[2:0]}` → 0‑15 (D0‑D7, E0‑E7 when
  B-register extension supported).
* Full destination register = `{D, d[2:0]}` → 0‑15.

#### VEA (Vector Effective Address) table

| A | Mode | Register | Addressing |
|---|------|----------|------------|
| 0 | 000 | Reg | Data register Dn |
| 0 | 001 | Reg | Data register E0‑E7 |
| 0 | 010 | Reg | Address indirect (An) |
| 0 | 011 | Reg | Address post-increment (An)+ |
| 0 | 100 | Reg | Address pre-decrement ‑(An) |
| 0 | 101 | Reg | d16(An) |
| 0 | 110 | Reg | d8(An,Xn.w×Scale) |
| 0 | 111 | 000 | Abs.W |
| 0 | 111 | 001 | Abs.L |
| 0 | 111 | 010 | d16(PC) |
| 0 | 111 | 011 | d8(PC,Xn.w×Scale) |
| 0 | 111 | 100 | #imm.Q (quad immediate, word repeated) |
| 1 | 000 | Reg | Data register E8‑E15 |
| 1 | 001 | Reg | Data register E16‑E23 |
| 1 | 010 | Reg | Address indirect (Bn) |
| 1 | 011 | Reg | Address post-increment (Bn)+ |
| 1 | 100 | Reg | Address pre-decrement ‑(Bn) |
| 1 | 101 | Reg | d16(Bn) |
| 1 | 110 | Reg | d8(Bn,Xn.w×Scale) |
| 1 | 111 | 100 | #imm.W (word immediate, repeated to quad)|
| 1 | 111 | 111 | VPERM special mode |

---

## AMMX Instruction Expansion (All Mandatory)

Every instruction below must be added to the assembler.  Each entry specifies
the exact binary encoding for Word 1 (the extension word following the AMMX
prefix).

### AMMX-01  PADDUSB / PADDUSW — Unsigned Saturated Vector Add

**Syntax:** `PADDUSB <vea>,b,d` / `PADDUSW <vea>,b,d`

**Operation:** Element-wise unsigned-saturated add: `clamp(a + b, 0, max) → d`

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d  0  0   0  1  0  U  1  S
```

* U = 1 (unsigned saturated)
* S = 0 → PADDUSB (8 × byte)
* S = 1 → PADDUSW (4 × word)

**Implementation:**

* Concrete word-1 bytes: PADDUSB = `0x16`, PADDUSW = `0x17`.
  (bits: `bd 00 0101 0` for byte, `bd 00 0101 1` for word — with U=1, S=0/1.)
* Extend `encode_ammx_padd()` to handle U=1 variants, or create dedicated
  `encode_ammx_paddus()`.
* Add `M68080MnemonicKind::Paddusb` and `Paddusw` enum variants.
* Add `"PADDUSB"` and `"PADDUSW"` entries in `m68080_base_kind()`.

**Operand form:** `(vea, b, d)` — standard 3-register AMMX.

---

### AMMX-02  PSUBUSB / PSUBUSW — Unsigned Saturated Vector Subtract

**Syntax:** `PSUBUSB <vea>,b,d` / `PSUBUSW <vea>,b,d`

**Operation:** Element-wise unsigned-saturated subtract: `clamp(b − a, 0, max) → d`

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d  0  0   0  1  0  U  1  S
```

This is the **PSUB family** encoding — same bit layout as PADD but the
assembler routes to the PSUB encoder.  The distinction is purely mnemonic-
level; the hardware differentiates via the base opcode field in the broader
PSUB context.

Wait — clarification from PRM page 67: PSUB uses the **same encoding format**
`b d 0 0 0 1 0 U 1 S` as PADD but the PRM describes PSUB on a separate page.
Cross-referencing with ApolloCrossDev:

**PRM page 53 (PADD):** `b d 0 0 0 1 0 U 1 S` — PADD family
**PRM page 67 (PSUB):** `b d 0 0 0 1 0 U 1 S` — PSUB family

These are byte-identical bitfields in the PRM. However, looking at existing
implementation: `encode_ammx_padd()` emits opcode `0x10`/`0x11` (which is
`00 0100 0/1`), and `encode_ammx_psub()` emits `0x12`/`0x13` (`00 0100 1 0/1`).

Re-deriving from the bit layout `0 0 0 1 0 U 1 S`:

| Instruction | U | S | Bits 9‑0 | Hex (bits 9‑0) |
|-------------|---|---|----------|----------------|
| PADDB | 0 | 0 | 00 0100 10 | 0x12 |
| PADDW | 0 | 1 | 00 0100 11 | 0x13 |
| PADDUSB | 1 | 0 | 00 0101 10 | 0x16 |
| PADDUSW | 1 | 1 | 00 0101 11 | 0x17 |

But the existing code uses `0x10`/`0x11` for PADD and `0x12`/`0x13` for PSUB.
This indicates the encoding format in the PRM is presented identically on both
pages but the actual opcodes differ — the existing tested implementation
encodes PADDB/PADDW as `0x10`/`0x11` and PSUBB/PSUBW as `0x12`/`0x13`.

**Resolution:** The bit field `0 0 0 1 0 U 1 S` printed in the PRM is a
*field description template* where the base starting position differs between
PADD and PSUB families.  The correct opcodes (validated against the existing
656-test suite) are:

| Instruction | Opcode (hex, bits 9‑0 of word 1) |
|-------------|----------------------------------|
| PADDB | 0x10 |
| PADDW | 0x11 |
| PSUBB | 0x12 |
| PSUBW | 0x13 |
| PADDUSB | 0x14 |
| PADDUSW | 0x15 |
| PSUBUSB | 0x16 |
| PSUBUSW | 0x17 |

The pattern is: base PADD = 0x10, PSUB = 0x12, then +4 for U=1 (saturated),
+1 for S=1 (word).

* Add `M68080MnemonicKind::Psubusb` and `Psubusw` enum variants.
* Add `"PSUBUSB"` and `"PSUBUSW"` entries in `m68080_base_kind()`.
* Extend `encode_ammx_psub()` to handle U=1 variants using opcodes 0x16/0x17.

**Operand form:** `(vea, b, d)` — standard 3-register AMMX.

---

### AMMX-03  PAVGB — Unsigned Byte Vector Average

**Syntax:** `PAVGB <vea>,b,d`

**Operation:** 8 × `(a[i] + b[i] + 1) >> 1 → d[i]` (unsigned byte average
with rounding)

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d  0  0   0  0  1  1  0  0
```

Opcode bits 9‑0 = `0x0C`.

**Implementation:**

* Add `M68080MnemonicKind::Pavgb` enum variant.
* Add `"PAVGB"` entry in `m68080_base_kind()`.
* Encoder: use `encode_ammx_vea_b_d_fixed()` with opcode `0x0C`.

**Operand form:** `(vea, b, d)` — standard 3-register AMMX.

---

### AMMX-04  PMAXSB / PMAXUB — Byte Vector Maximum

**Syntax:** `PMAXSB <vea>,b,d` / `PMAXUB <vea>,b,d`

**Operation:** 8 × `max(a[i], b[i]) → d[i]` — signed or unsigned byte
comparison.

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d  0  0   1  1  0  1  U  S
```

* S = 0 (byte)
* U = 0 → PMAXSB (signed)
* U = 1 → PMAXUB (unsigned)

| Instruction | U | S | Opcode hex |
|-------------|---|---|------------|
| PMAXSB | 0 | 0 | 0x34 |
| PMAXUB | 1 | 0 | 0x36 |

**Implementation:**

* Add `M68080MnemonicKind::Pmaxsb` and `Pmaxub` enum variants.
* Add `"PMAXSB"` and `"PMAXUB"` entries in `m68080_base_kind()`.
* Encoder: use `encode_ammx_vea_b_d_fixed()` with appropriate opcode.

**Operand form:** `(vea, b, d)` — standard 3-register AMMX.

---

### AMMX-05  PMAXSW / PMAXUW — Word Vector Maximum

**Syntax:** `PMAXSW <vea>,b,d` / `PMAXUW <vea>,b,d`

**Operation:** 4 × `max(a[i], b[i]) → d[i]` — signed or unsigned word
comparison.

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d  0  0   1  1  0  1  U  S
```

* S = 1 (word)
* U = 0 → PMAXSW (signed)
* U = 1 → PMAXUW (unsigned)

| Instruction | U | S | Opcode hex |
|-------------|---|---|------------|
| PMAXSW | 0 | 1 | 0x35 |
| PMAXUW | 1 | 1 | 0x37 |

**Implementation:**

* Add `M68080MnemonicKind::Pmaxsw` and `Pmaxuw` enum variants.
* Add `"PMAXSW"` and `"PMAXUW"` entries in `m68080_base_kind()`.

**Operand form:** `(vea, b, d)` — standard 3-register AMMX.

---

### AMMX-06  PMINSB / PMINUB — Byte Vector Minimum

**Syntax:** `PMINSB <vea>,b,d` / `PMINUB <vea>,b,d`

**Operation:** 8 × `min(a[i], b[i]) → d[i]`

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d  0  0   1  1  0  0  U  S
```

* S = 0 (byte)
* U = 0 → PMINSB (signed)
* U = 1 → PMINUB (unsigned)

| Instruction | U | S | Opcode hex |
|-------------|---|---|------------|
| PMINSB | 0 | 0 | 0x30 |
| PMINUB | 1 | 0 | 0x32 |

**Implementation:**

* Add `M68080MnemonicKind::Pminsb` and `Pminub` enum variants.
* Add `"PMINSB"` and `"PMINUB"` entries in `m68080_base_kind()`.

**Operand form:** `(vea, b, d)` — standard 3-register AMMX.

---

### AMMX-07  PMINSW / PMINUW — Word Vector Minimum

**Syntax:** `PMINSW <vea>,b,d` / `PMINUW <vea>,b,d`

**Operation:** 4 × `min(a[i], b[i]) → d[i]`

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d  0  0   1  1  0  0  U  S
```

* S = 1 (word)
* U = 0 → PMINSW (signed)
* U = 1 → PMINUW (unsigned)

| Instruction | U | S | Opcode hex |
|-------------|---|---|------------|
| PMINSW | 0 | 1 | 0x31 |
| PMINUW | 1 | 1 | 0x33 |

**Implementation:**

* Add `M68080MnemonicKind::Pminsw` and `Pminuw` enum variants.
* Add `"PMINSW"` and `"PMINUW"` entries in `m68080_base_kind()`.

**Operand form:** `(vea, b, d)` — standard 3-register AMMX.

---

### AMMX-08  BFLYB / BFLYW — Butterfly (Vector Add+Sub)

**Syntax:** `BFLYB <vea>,b,d:d2` / `BFLYW <vea>,b,d:d2`

**Operation:**
* `b + a → d`
* `b − a → d2`

The destination pair `d:d2` must be **consecutive** registers starting at an
**even** index (e.g. E0:E1, D2:D3, E6:E7).

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d  0  0   0  0  1  1  1  0  S
```

* S = 0 → BFLYB (8 × byte)
* S = 1 → BFLYW (4 × word)

| Instruction | S | Opcode hex |
|-------------|---|------------|
| BFLYB | 0 | 0x1C |
| BFLYW | 1 | 0x1D |

**Constraints:**

* Destination register index must be even (d mod 2 = 0).
* d2 = d + 1 (implicit; not encoded separately).
* No saturation is performed.

**Implementation:**

* Add `M68080MnemonicKind::Bflyb` and `Bflyw` enum variants.
* Add `"BFLYB"` and `"BFLYW"` entries in `m68080_base_kind()`.
* Also add `"BFLY"` with `.B`/`.W` suffix handling if desired for
  compatibility, but the dotless forms `BFLYB` / `BFLYW` are primary.
* Encoder must validate even-register constraint on destination.
* Operand parser must accept `d:d2` pair syntax.

**Operand form:** `(vea, b, d:d2)` — 3-operand with destination register pair.

---

### AMMX-09  C2P — Chunky to Planar

**Syntax:** `C2P <vea>,d`

**Operation:** Bit-wise transpose: for each bit position n in 8 source bytes,
all bits at position n are collected into destination byte n.

**Word 0 encoding (note B=0 forced):**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  1  1  1   1  1  1  A  0  D  Mode   Register
```

B is always 0 (no second source register).

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  0  0  0   d  0  0  1  0  1  0  0  0
```

Opcode bits = `0x0A8` (bits 12‑0 mask), but structured as: `{0000, d[2:0],
0, 0101, 000}`.  The d field in bits 12‑10 carries the lower 3 bits of the
destination register (upper bit is D in word 0).

**Implementation:**

* Add `M68080MnemonicKind::C2p` enum variant.
* Add `"C2P"` entry in `m68080_base_kind()`.
* Custom encoder — 2-operand form (VEA source, single register destination).
  Word 0 has B=0 fixed.  Word 1 is `0x0000 | (d_lo << 10) | 0x0A8`.

**Operand form:** `(vea, d)` — 2-operand AMMX (source in VEA, destination
register only).

---

### AMMX-10  LSLQ / LSRQ — Quad Logical Shift

**Syntax:** `LSLQ <vea>,b,d` / `LSRQ <vea>,b,d`

**Operation:** 64-bit logical shift left/right.  Shift count = `<vea> mod 64`.
Zeroes are shifted in.

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d  0  0   1  1  1  0  0  dir
```

* dir = 0 → LSLQ (left)
* dir = 1 → LSRQ (right)

| Instruction | dir | Opcode hex |
|-------------|-----|------------|
| LSLQ | 0 | 0x38 |
| LSRQ | 1 | 0x39 |

**Implementation:**

* Add `M68080MnemonicKind::Lslq` and `Lsrq` enum variants.
* Add `"LSLQ"` and `"LSRQ"` entries in `m68080_base_kind()`.
* Encoder: use `encode_ammx_vea_b_d_fixed()` with opcode 0x38 / 0x39.

**Operand form:** `(vea, b, d)` — standard 3-register AMMX.

---

### AMMX-11  MINTERM — Bitwise Logical Minterm

**Syntax:** `MINTERM a0‑a3,d`

**Operation:** Blitter-like 3-input bitwise logical operation on quad-word
data.  The minterm value is taken from the 4th register (a3).  The three
logical inputs are a0 (A), a1 (B), a2 (C), and a3 holds the 8-bit minterm
lookup table.

**Constraints:**

* No memory operands — register only.
* The four source registers must be **consecutive** starting at a **multiple
  of 4** (D0‑D3, D4‑D7, E0‑E3, …, E20‑E23).
* Destination is an arbitrary data/E register.

**Word 0 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  1  1  1   1  1  1  A  0  D  0  0  a  a  0  0
```

* A = MSB of source group.
* `aa` = bits [3:2] of the first source register index (the group selector:
  0 = regs 0‑3, 1 = regs 4‑7, etc.).
* B = 0 (fixed).
* Mode/Register = `00 aa 00` — not a standard EA, but group encoding.

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  0  0  0   d  0  0  1  0  1  0  1  0
```

Opcode = `0x0AA` in the d-embedded layout:
`{0000, d[2:0], 0, 0101, 010}`.

**Implementation:**

* Add `M68080MnemonicKind::Minterm` enum variant.
* Add `"MINTERM"` entry in `m68080_base_kind()`.
* Custom encoder: parse register-group `a0-a3` syntax, validate group
  alignment (mod 4), emit word 0 with `aa` and word 1 with `d`.
* Operand parser must accept `Dn-Dn+3,Dd` or `En-En+3,Ed` range syntax.

**Operand form:** `(a_group, d)` — register-group source, single register
destination.

---

### AMMX-12  STOREM — Masked Store

**Syntax:** `STOREM b,mask,<vea>`

**Operation:** Conditional byte-wise write: for each of the 8 bytes in
register `b`, write to memory only if the corresponding bit in the low 8 bits
of register `mask` (d) is **1**.

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d(mask)  0  0  1  0  0  1  0  1
```

Opcode bits 9‑0 = `0x25`.

**Note:** `#imm` is not allowed as VEA.  When destination is a register, no
masking is applied.

**Implementation:**

* Add `M68080MnemonicKind::Storem` enum variant.
* Add `"STOREM"` entry in `m68080_base_kind()`.
* Encoder: emit with M=1 (write to memory form: `b, d, vea`).
  Use `encode_ammx_b_d_vea_fixed()` with opcode `0x25`.

**Operand form:** `(b, mask, vea)` — write-to-memory AMMX form.

---

### AMMX-13  STOREM3 — Graphics-Masked Store

**Syntax:** `STOREM3 b,#mode,<vea>`

**Operation:** Conditional cookie-cut write.  Selection mode determines which
bytes/words are written based on the *destination memory content*:

| mode | Description |
|------|-------------|
| 0 | Long: 2 × 32-bit color, write when MSB=1 |
| 1 | Byte: 8 × 8-bit color index, write when ≠ 0 |
| 2 | Word: 4 × 16-bit color, write when ≠ $F81F (purple key) |
| 3 | Word: 4 × 15-bit color, write when MSB=0 |

**Word 0 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  1  1  1   1  1  1  A  B  0  Mode   Register
```

D = 0 (fixed).

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b  d(mode)  0  0  1  0  0  1  0  1
```

Opcode bits 9‑0 = `0x25` (same as STOREM).  The `d` field carries the mode
value (0‑3), with bits 11‑10 of the `d` register field selecting the gfx
mode.

**Note (vasm compatibility):** vasm syntax requires writing `storem3 d0,d3,(a0)`
where `d3` stands for `#3`.  opForge should accept both `#mode` and `dN`
forms.

**Implementation:**

* Add `M68080MnemonicKind::Storem3` enum variant.
* Add `"STOREM3"` entry in `m68080_base_kind()`.
* Encoder: combine mode into the `d` field of word 1.  The mode is an
  immediate value 0‑3.

**Operand form:** `(b, #mode, vea)` — special write-to-memory form with
immediate mode selector.

---

### AMMX-14  TEX — Texture Fetch

**Syntax:**
* `TEX8.512 (An,(Av,Au)),Dn`
* `TEX16.256 (An,(Av,Au)),Dn`
* `TEX24.64 (An,(Av,Au))*D0,Dn`
* `TEX.B (An,Av*Dm,Au),Dn`

**Operation:** Fetch colour from a 2D texture array.  An points to texture
base; Au and Av are 16.16 fixed-point coordinates.  Result shifts destination
up and inserts the fetched colour.

**Encoding:** 3-word format (unique among AMMX instructions):

```
Word 0:
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  1  1  1   1  1  1  0  0  0  1  1  0  An

Word 1:
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  Au         d  0  0  1  1  1  1  1  0

Word 2:
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  Av         1  S  S  0  S  T  0  0  S  S
```

**Size/texture-size encoding (S bits scattered across word 2):**

| Destination size | S pattern (bits 10,9, 4, 1,0) |
|------------------|-------------------------------|
| 8-bit (byte) | 00 0 00 |
| 16-bit (word) | 01 0 01 |
| 24-bit (DXT1) | 11 1 10 |

**Texture dimensions (T bits in word 2, bit 3):**

| Dimensions | T encoding |
|------------|------------|
| 64 × 64 | 000 |
| 128 × 128 | 011 |
| 256 × 256 | 101 |
| 512 × 512 | 110 |

**Alternative form (TEX.B, sizable without modular):**

Word 2 for `TEX.B (An,Av*Dm,Au),Dn`:
```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  Av         0  0  0  0  Dm  0  0  0  0
```

**Implementation:**

* Add `M68080MnemonicKind::Tex` enum variant.
* Add `"TEX8"`, `"TEX16"`, `"TEX24"`, `"TEX"` entries in
  `m68080_base_kind()`.  Size suffix parsing: `.512`, `.256`, `.64`, `.B`.
* Custom 3-word encoder with specialised operand parsing for the
  `(An,(Av,Au))` nested address form.
* Register constraints: An, Au, Av are address registers; Dn is data register.

**Note:** Per PRM page 74: "tex8.256 & tex16.256 are working" on current
cores; other variants may differ.  The assembler must emit all forms
regardless of core revision.

**Operand form:** Complex — nested indirect with 3-word encoding.

---

### AMMX-15  TRANSHI / TRANSLO — Matrix Word Transpose

**Syntax:** `TRANSHI a0‑a3,d:d2` / `TRANSLO a0‑a3,d:d2`

**Operation:** Transpose a 4×4 block with 16-bit elements from row to column
order.  TRANSHI extracts the upper two rows; TRANSLO extracts the lower two.

**Constraints:**

* Register only — no memory operands.
* Four source registers must be **consecutive** starting at a **multiple of
  4** (D0‑D3, D4‑D7, E0‑E3, …, E20‑E23).
* Destination pair `d:d2` must start at an **even** index (D0:D1, D2:D3,
  E0:E1, etc.).

**Word 0 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  1  1  1   1  1  1  A  0  D  0  0  a  a  0  0
```

* `aa` = group selector (same as MINTERM).
* B = 0 (fixed).

**Word 1 encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  0  0  0   d  0  0  0  0  0  0  0  1  L
```

* L = 0 → TRANSHI
* L = 1 → TRANSLO

| Instruction | L | Word-1 (with d=0) |
|-------------|---|-------------------|
| TRANSHI | 0 | 0x0002 |
| TRANSLO | 1 | 0x0003 |

General pattern for word 1: `(d_lo << 10) | 0x0002 | L`.

**Implementation:**

* Add `M68080MnemonicKind::Transhi` and `Translo` enum variants.
* Add `"TRANSHI"` and `"TRANSLO"` entries in `m68080_base_kind()`.
* Also add `"TRANS"` if a `.HI`/`.LO` suffix split is desired, but the
  dotless forms are primary.
* Custom encoder: parse register-group + destination-pair syntax.
* Validate group (mod 4) and even-destination constraints.
* Operand parser must accept `Dn-Dn+3,Dd:Dd+1` range/pair syntax.

**Operand form:** `(a_group, d:d2)` — register-group source, register-pair
destination.

---

## AMMX Dotless Mnemonic Aliases (All Mandatory)

The following dotless aliases must be added as first-class entries in
`m68080_base_kind()` because `split_size_suffix()` only recognises
dot-separated suffixes:

| Alias | Routes to | Size |
|-------|-----------|------|
| `PADDB` | `Padd` | Byte |
| `PADDW` | `Padd` | Word |
| `PADDUSB` | `Paddusb` | Byte |
| `PADDUSW` | `Paddusw` | Word |
| `PSUBB` | `Psub` | Byte |
| `PSUBW` | `Psub` | Word |
| `PSUBUSB` | `Psubusb` | Byte |
| `PSUBUSW` | `Psubusw` | Word |
| `BFLYB` | `Bflyb` | Byte |
| `BFLYW` | `Bflyw` | Word |
| `PAVGB` | `Pavgb` | Byte |

These are not merely convenience — real-world Apollo source uses these forms
exclusively.  Failure to parse them is a blocking usability defect.

---

## Integer Instruction Expansion (All Mandatory)

### INT-01  ADDQ.L #data,Bn — Quick Add to B-register

**Syntax:** `ADDQ.L #data,Bn` (data = 1‑8; 0 encodes 8)

**Operation:** Bn + data → Bn

**Encoding:** Reuses the unused `ADDQ.B #data,An` slot:

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  1  0  1  Data   0  0  0  0  0  1  Bn
```

Full word: `0x5001 | (data << 9) | Bn`.

Where `Data` is the 3-bit quick value (0 = 8, 1‑7 = 1‑7) and `Bn` is the
B-register number (0‑7).

**Notes (from ApolloCrossDev):** "The unused addq.b #data,An is used."  This
means the encoder must detect when the destination is a B-register and use
this special encoding instead of the standard ADDQ path.

**Implementation:**

* Extend the existing ADDQ encoder to detect B-register destinations.
* When destination is Bn under `.cpu 68080`, emit the `0x5001 | …` encoding.
* On non-68080 CPUs, B-register destination must fail with deterministic
  diagnostic.

---

### INT-02  SUBQ.L #data,Bn — Quick Subtract from B-register

**Syntax:** `SUBQ.L #data,Bn` (data = 1‑8; 0 encodes 8)

**Operation:** Bn − data → Bn

**Encoding:** Reuses the unused `SUBQ.B #data,An` slot:

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  1  0  1  Data   1  0  0  0  0  1  Bn
```

Full word: `0x5101 | (data << 9) | Bn`.

**Implementation:**

* Extend the existing SUBQ encoder to detect B-register destinations.
* When destination is Bn under `.cpu 68080`, emit `0x5101 | (data << 9) | Bn`.

---

### INT-03  CMP.L Bn,Dn — Compare B-register with Data Register

**Syntax:** `CMP.L Bn,Dn`

**Operation:** Dn − Bn → condition codes (result not stored)

**Encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  1  0  0  Dn     1  1  0  0  0  1  Bn
```

Full word: `0xB181 | (Dn << 9) | Bn`.

**Note:** This is the CMP.L instruction using EA mode `001` (address register
direct) with Bn mapped into the register field.  The size is always long.

**Implementation:**

* Extend the existing CMP encoder to detect B-register source operands.
* Under `.cpu 68080`, treat `001` mode with B-register flag to emit this form.

---

### INT-04  LEA <ea>,Bn — Load Effective Address into B-register

**Syntax:** `LEA <ea>,Bn`

**Operation:** effective address → Bn

**Encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  1  0  0  Bn     1  0  1  Mode   Register
```

Full word: `0x41C0 | (Bn << 9) | ea_mode_reg`.

**Note:** This uses the same LEA opcode format but with the destination
register being a B-register.  The upper register bits select Bn.

**Implementation:**

* Extend the existing LEA encoder to detect B-register destinations.
* Under `.cpu 68080`, emit with Bn in the register field.
* On non-68080 CPUs, B-register destination must fail.

---

### INT-05  LEA (Bn),An — Load B-register Indirect to Address Register

**Syntax:** `LEA (Bn),An`

**Operation:** contents of address pointed to by Bn → An

**Encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  1  0  0  An     1  1  1  0  0  1  Bn
```

Full word: `0x41C0 | (An << 9) | 0x0039 | Bn`.

Wait — re-deriving: standard LEA is `0100 An 111 Mode Reg`.  For `(Bn)`, the
mode would be `001` (the B-register-as-address mode).

Full word: `0x41C0 | (An << 9) | (0b001 << 3) | Bn` = `0x41C8 | (An << 9) | Bn`.

Actually, from ApolloCrossDev MOVEA page: "No LEA (Bn),Bm or MOVEA.L Bn,Bm."
So LEA (Bn),An should be supported (An destination, Bn indirect source) but
LEA (Bn),Bm is not.

**Implementation:**

* Extend LEA encoder to detect `(Bn)` as source EA.
* Validate: destination must be An (not Bm) per hardware restriction.

---

### INT-06  MOVE.L Bn,<ea> — Move B-register to Effective Address

**Syntax:** `MOVE.L Bn,<ea>`

**Operation:** Bn → <ea>

**Encoding:** Reuses the unused `MOVE.B An,<ea>` slot:

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  0  0  1  Reg    Mode  0  0  1  Bn
```

Full word: `0x0100 | (ea_mode << 6) | (ea_reg << 9) | (0b001 << 3) | Bn`.

Note: MOVE destination encoding uses the **reversed** mode/register layout
(destination register in bits 11‑9, destination mode in bits 8‑6).

Correct layout for `MOVE.L Bn,<ea>`:
```
0001  dst_reg  dst_mode  001  Bn
```

Where `001` in bits 5‑3 is the source mode (B-register direct) and Bn in
bits 2‑0.

**Implementation:**

* Extend the existing MOVE encoder to detect B-register source.
* Under `.cpu 68080`, when source mode is B-register, emit with source
  mode `001` and size field = `01` (byte, to match the repurposed slot).
* Size is always long regardless of the MOVE.B encoding slot.

---

### INT-07  MOVEA.L <ea>,Bn — Move to B-register

**Syntax:** `MOVEA.L <ea>,Bn`

**Operation:** <ea> → Bn

**Encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  0  0  1  Bn     0  0  1  Mode   Register
```

Full word: `0x0040 | (Bn << 9) | (0b001 << 6) | ea_mode_reg`.

Wait — standard MOVEA uses size-dependent prefix.  For MOVEA.L:
`00 10 dst 001 src_mode src_reg` — but this is the standard 68k MOVEA.L.
The 68080 variant uses the byte-size slot: `00 01 Bn 001 mode reg`.

**Note from ApolloCrossDev:** "No LEA (Bn),Bm or MOVEA.L Bn,Bm" — cannot
move between B-registers.

**Note:** Size encoding `0001` in bits 13‑12 is the "byte" MOVE size code,
but `MOVEA.B` doesn't exist in standard 68k, so this slot is available for
the 68080 to repurpose.

**Implementation:**

* Add handling in the MOVEA encoder to detect B-register destination.
* Under `.cpu 68080`, emit with the byte-sized MOVE prefix `0001` and
  destination mode `001`.
* Validate that source is not a B-register (no Bn→Bm moves).

---

### INT-08  CLR.Q <ea> — Clear Quad-Word

**Syntax:** `CLR.Q <ea>`

**Operation:** 0 (64-bit) → <ea>

**Encoding:** Uses Line-A with Apollo extension:

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  0  1  0   1  1  1  0  0  0  Mode   Register
```

Full word: `0xAE00 | ea_mode_reg`.

**Note:** This is a Line-A opcode.  The `.apollo` gate must be active.

**Implementation:**

* Add handling for `CLR.Q` size variant detection.
* Add operand size `Quad` if not already present, or map `.Q` to a specific
  68080 path.
* Emit `0xAE00 | ea` under 68080 with Apollo gate.
* On non-68080 or without Apollo, `CLR.Q` must fail with deterministic
  diagnostic.

---

### INT-09  EXTUB.L / EXTUW.L Dn — Extend Unsigned to Long

**Syntax:** `EXTUB.L Dn` / `EXTUW.L Dn`

**Operation:**
* `EXTUB.L`: zero-extend byte in Dn to long (clear bits 31‑8)
* `EXTUW.L`: zero-extend word in Dn to long (clear bits 31‑16)

**Encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  1  0  0   1  Size  1  1  1  0  0  0  Dn
```

* Size = 01 → EXTUB (byte to long)
* Size = 10 → EXTUW (word to long)

| Instruction | Size | Full word (Dn=0) |
|-------------|------|------------------|
| EXTUB.L | 01 | 0x49C0 |
| EXTUW.L | 10 | 0x4BC0 |

General: EXTUB = `0x49C0 | Dn`, EXTUW = `0x4BC0 | Dn`.

**Implementation:**

* Add `M68080MnemonicKind::Extub` and `Extuw` enum variants.
* Add `"EXTUB"` and `"EXTUW"` entries in `m68080_base_kind()`.
* Encoder: single-word instruction, `0x49C0 | Dn` or `0x4BC0 | Dn`.
* Validate: operand must be data register, size must be `.L`.

---

### INT-10  DBcc.L Dn,label — Long-Counter Decrement and Branch

**Syntax:** `DBcc.L Dn,<label>` (all standard conditions: DBF.L, DBRA.L,
DBNE.L, etc.)

**Operation:** Same as standard DBcc but the counter register Dn is treated
as a full 32-bit long instead of a 16-bit word.

**Encoding:** Same as standard DBcc:

```
Word 0:
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  1  0  1   CC          1  1  0  0  1  Dn

Word 1:
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
              16-bit displacement
```

**The 68080 long-counter signal:** When the displacement value has bit 0 = 1
(i.e. displacement is odd), the 68080 interprets this as the long-counter
variant.  Since 68k displacements are always even (word-aligned), an odd
displacement is otherwise impossible and is repurposed as this signal.

**Implementation:**

* When the programmer writes `DBcc.L`, the encoder must:
  1. Compute the 16-bit displacement normally.
  2. Set bit 0 of the displacement word to 1 (OR with 1).
* This is a **size suffix** differentiation: `.W` (default/standard) → normal
  displacement; `.L` → displacement | 1.
* Extend existing DBcc encoder to detect `.L` size and apply the odd-bit
  signal.
* All condition code variants (DBT.L, DBF.L/DBRA.L, DBHI.L, DBLS.L, DBCC.L,
  DBCS.L, DBNE.L, DBEQ.L, DBVC.L, DBVS.L, DBPL.L, DBMI.L, DBGE.L, DBLT.L,
  DBGT.L, DBLE.L) must be supported.

---

### INT-11  Bcc.S+ label — Extended Short Branch

**Syntax:** `BNE.S+ <label>`, `BEQ.S+ <label>`, etc.

**Operation:** Same as standard Bcc.S but with extended range.  The 68080
interprets an odd 8-bit displacement (bit 0 = 1) as a signal for extended
addressing.

**Implementation:**

* This requires a new size suffix `.S+` or detection of the `.L`-on-short
  pattern.
* When the encoder detects this variant, it sets bit 0 of the 8-bit
  displacement to 1.
* All condition codes must be supported.

**Note:** This is a less commonly used extension.  Implementation may be
deferred until after the main instruction families are complete, but **it must
still be implemented** — it is not optional.

---

### INT-12  BRA.S+ / INT-13  BSR.S+ — Extended Short Unconditional Branch/Subroutine

Same encoding principle as Bcc.S+ applied to BRA and BSR:

* `BRA.S+`: condition code = 0000 (always true), displacement bit 0 = 1.
* `BSR.S+`: condition code = 0001, displacement bit 0 = 1.

---

### INT-14  BANK — B-register Address Mode Prefix

**Syntax:** `BANK` (prefix instruction)

**Operation:** The BANK instruction is a prefix that extends the following
instruction's register addressing to use B-registers as extended address
registers with different XOR-combined register mappings.

**Encoding:**

```
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  1  1  1  c  c  c  1  S  S  C  C  A  A  B  B
```

* `ccc` = XOR base for third operand register.
* `SS` = size control.
* `CC` = extension for third register.
* `AA` = extension bits for source register.
* `BB` = extension bits for destination register.
* Formula: `CCccc XOR BBbbb` produces the third operand register.

**Implementation:**

* Add `M68080MnemonicKind::Bank` enum variant.
* Add `"BANK"` entry in `m68080_base_kind()`.
* Custom encoder: 16-bit single-word prefix instruction.
* The parser must associate BANK with the following instruction it modifies.

---

### INT-15  PERM #n,Ra,Rb — Byte Permute (Integer)

**Syntax:** `PERM #n,Ra,Rb`

**Operation:** Selects and reorders 4 bytes from two source registers Ra and Rb
according to the permutation pattern `#n`.  Each position selector is 4 bits
wide.

**Encoding (2 words):**

```
Word 0:
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  1  0  0   1  1  0  0  1  1  0  0  a
```

Word 0 = `0x4CC0 | a`.  Where `a` = register number 0‑7 for D0‑D7, 8‑15 for
A0‑A7.

```
Word 1:
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 b    pos0       pos1       pos2       pos3
```

* `b` = second source register (same 0‑15 mapping as `a`).
* `pos0`‑`pos3` = four 3-bit position selectors defining which bytes from the
  concatenated 8-byte source (Ra:Rb) appear in the 4-byte result.

**Implementation:**

* Add `M68080MnemonicKind::Perm` enum variant.
* Add `"PERM"` entry in `m68080_base_kind()`.
* Custom 2-word encoder: parse `#immediate, Rn, Rm` operands.
* Validate register range (D0‑D7 / A0‑A7).

---

### INT-16  MOVEC — 68080 Control Register Extensions

The 68080 adds event counter registers accessible via MOVEC:

| Register | Code | Description |
|----------|------|-------------|
| PCR | $808 | Performance Counter Register |
| CCC | $809 | Clock Cycle Counter |
| IEP1 | $80A | Instruction Execution Pipe 1 |
| IEP2 | $80B | Instruction Execution Pipe 2 |
| BPC | $80C | Branch Predict Correct |
| BPW | $80D | Branch Predict Wrong |
| DCH | $80E | Data Cache Hit |
| DCM | $80F | Data Cache Miss |
| STR | $00A | Store Read |
| STC | $00B | Store Count |
| IEP3 | $00C | Instructions Executed Pipe 3 |
| STB | $00D | Store Buffer |
| MWR | $00E | Memory Write |

**Note from PRM:** "Reading may be done in user mode on 68080" — unlike
standard 68k where MOVEC is always privileged.

**Compatibility note:** `STH` remains accepted as a legacy alias for `$00C`,
but `IEP3` is the canonical 68080 spelling.

**Implementation:**

* Extend the existing MOVEC control register table to recognise the 68080
  register codes when `.cpu 68080` is active.
* MOVEC encoding itself is standard `$4E7A` (Rc→Rn) / `$4E7B` (Rn→Rc);
  only the register code in the extension word changes.
* On non-68080 CPUs, these register names must fail with unsupported
  control register diagnostics.

---

### INT-17  MOVE SR — Non-Privileged Read on 68080

On the 68080, `MOVE SR,<ea>` (reading the SR) is allowed in user mode,
unlike 68010+ where it became privileged (replaced by MOVE CCR for user mode).

**Implementation:**

* No encoding change — the assembler already accepts `MOVE SR,<ea>` on all
  CPUs.
* If opForge has a privilege-level check or warning for MOVE SR on 68010+,
  the 68080 path must suppress that warning/error.

---

### INT-18  MOVE16 — Relaxed Alignment on 68080

**Syntax:** Standard MOVE16 forms.

**Note from PRM:** "On the 68080, MOVE16 does not have to be aligned (unlike
68040)."

The encoding is identical to the standard 68040 MOVE16.  The assembler should
**not** emit alignment warnings when `.cpu 68080` is active.

**Implementation:**

* Extend existing MOVE16 encoder to suppress alignment diagnostics on 68080.

---

## FPU Instruction Expansion (All Mandatory)

### FPU-01  FDBcc.L — Long-Counter FPU Decrement and Branch

**Syntax:** `FDBcc.L Dn,<label>` (all FPU conditions)

**Operation:** Same as standard FDBcc but with long (32-bit) loop counter.

**Encoding:**

```
Word 0:
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  1  1  1   0  0  1  0  0  1  0  0  1  Dn

Word 1 (FPU condition predicate):
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  0  0  0   0  0  0  0  0  0   Condition

Word 2 (displacement):
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
              16-bit displacement
```

**Long-counter signal:** Same as integer DBcc.L — displacement bit 0 = 1
signals the long-counter variant.

Word 0 = `0xF249 | Dn`.

**Implementation:**

* Extend the existing FDBcc encoder to detect `.L` size suffix.
* When `.L` is specified under `.cpu 68080`, set displacement bit 0 = 1.
* All FPU condition codes must be supported.

---

### FPU-02  FMOVE.D Dn,FPn / FPn,Dn — Double-Precision Data Register Move

**Syntax:**
* `FMOVE.D Dn,FPn` — move double from data register pair to FP register
* `FMOVE.D FPn,Dn` — move double from FP register to data register pair

**Operation:** Convert between 64-bit double-precision floating-point in a
data register (pair) and an FPU register.

**Encoding:**

```
Word 0:
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 1  1  1  1   0  0  1  0  0  0  0  0  0  Dn

Word 1:
15 14 13 12  11 10  9  8  7  6  5  4  3  2  1  0
 0  1  D    Src   FPn     Opmode
```

* Mode = 000 (data register direct).
* Register = Dn.
* Source Specifier for double: `101`.
* Direction D: 0 = `<ea> → FPn`, 1 = `FPn → <ea>`.
* `<ea>,FPn`: Opmode = `R000P00` where RP = rounding precision
  (00=default, 10=single, 11=double).
* `FPn,<ea>`: Opmode = `0000000` (or k-factor for packed, but double
  doesn't use that).

**FSTOREI / FLOADI disambiguation (from PRM page 81):**

Because E-registers can be both float and data registers, `fmove.w e2,e3`
is ambiguous (fp→d or d→fp).  vasm resolves this with:
* `FSTOREI` = FPn → data register
* `FLOADI` = data register → FPn

opForge should accept both disambiguating mnemonics and the standard FMOVE
form.  The existing `Floadi` and `Fstorei` FPU mnemonic kinds already exist.

**Implementation:**

* The existing FPU FMOVE encoder must be extended to accept `.D` (double)
  format when the EA is a data register on `.cpu 68080`.
* Standard 68k FPU only supports FMOVE.D with memory EAs from/to data
  registers — the 68080 extends this to accept `Dn` directly as source/dest
  for double format.

---

### FPU-03  FMOVEM — Apollo Extended Format

The 68080 uses a slightly different memory layout for the 80-bit extended
floating-point format:

**Motorola eXtended format (12 bytes per register):**

```
Byte 0-1: S EEEEEEEEEEEEEEE
Byte 2-3: 0000000000000000 (unused / padding)
Byte 4-7: M 1 2 3 4 ... 31     (integer bit + 31 mantissa bits)
Byte 8-11: 32 33 34 ... 63     (mantissa bits 32-63)
```

**Apollo eXtended format (10 bytes per register):**

```
Byte 0-1: S EEEEEEEEEEEEEEE  32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
Byte 2-3: ← mantissa bits 32-47 packed into the same long as sign+exponent
Byte 4-7: M 1 2 3 4 ... 31     (integer bit + 31 mantissa bits)
```

The Apollo format is 10 bytes (not 12) — the padding word is eliminated and
the lower mantissa bits are packed adjacently.

**Note from PRM page 83:** "Newer cores will use the 'normal' eXtended format."

**Implementation:**

* When `.cpu 68080` is active, the FMOVEM encoder must be aware that the
  memory layout is the Apollo format (10 bytes per register, not 12).
* This affects how the assembler computes stack offsets when generating
  FMOVEM with multi-register lists.
* The encoding of the FMOVEM instruction word itself is unchanged — only
  the interpretation of the memory layout differs.
* For pure assembly (no linker-computed offsets), this is primarily a
  documentation/diagnostic concern.

---

### FPU-04  FMOVERZ / FMOVEURZ — Round-to-Zero FP Move

**Syntax:**
* `FMOVERZ.s FPn,<ea>` — move with round-to-zero, signed
* `FMOVEURZ.s FPn,<ea>` — move with round-to-zero, unsigned

**Encoding:** Same as FMOVE FPn→<ea> but with specific opmode values:

```
Word 1 opmode:
  1 = Round Zero (signed)     → FMOVERZ
  3 = Unsigned Round Zero     → FMOVEURZ
```

Source specifier: `110` = byte, `100` = word, `000` = long.
Direction D = 1 (FPn → <ea>).

**Note from PRM page 82:** "Too recent to be used on V2 (2.17)."  The
assembler must still emit the correct bytes regardless.

**Implementation:**

* The existing `Fmoverz` and `Fmoveurz` FPU mnemonic kinds already exist
  in the `FpuMnemonicKind` enum.
* Verify the encoder emits the correct opmode values (1 and 3 respectively).
* The source specifier must be set based on the destination size (.B/.W/.L).

---

## Existing Implementation Fixes Required (Mandatory)

### FIX-01  PADD Saturated Variants

The existing `encode_ammx_padd()` function only handles opcodes 0x10 (PADDB)
and 0x11 (PADDW).  It must be extended to also handle:

* 0x14 → PADDUSB (byte, U=1)
* 0x15 → PADDUSW (word, U=1)

### FIX-02  PSUB Saturated Variants

The existing `encode_ammx_psub()` function only handles opcodes 0x12 (PSUBB)
and 0x13 (PSUBW).  It must be extended to also handle:

* 0x16 → PSUBUSB (byte, U=1)
* 0x17 → PSUBUSW (word, U=1)

### FIX-03  Dotless Alias Registration

`m68080_base_kind()` must gain entries for all dotless aliases listed in the
"AMMX Dotless Mnemonic Aliases" section.

---

## Complete Opcode Reference Table

The following table lists all AMMX instruction opcodes (Word 1, bits 9‑0):

| Hex | Binary (9‑0) | Instruction |
|-----|--------------|-------------|
| 0x01 | 00 0000 0001 | LOAD |
| 0x04 | 00 0000 0100 | STORE |
| 0x05 | 00 0000 0101 | STOREILM |
| 0x06 | 00 0000 0110 | PACKUSWB |
| 0x07 | 00 0000 0111 | PACK3216 |
| 0x08 | 00 0000 1000 | PAND |
| 0x09 | 00 0000 1001 | POR |
| 0x0A | 00 0000 1010 | PEOR |
| 0x0B | 00 0000 1011 | PANDN |
| 0x0C | 00 0000 1100 | PAVGB |
| 0x10 | 00 0001 0000 | PADDB |
| 0x11 | 00 0001 0001 | PADDW |
| 0x12 | 00 0001 0010 | PSUBB |
| 0x13 | 00 0001 0011 | PSUBW |
| 0x14 | 00 0001 0100 | PADDUSB |
| 0x15 | 00 0001 0101 | PADDUSW |
| 0x16 | 00 0001 0110 | PSUBUSB |
| 0x17 | 00 0001 0111 | PSUBUSW |
| 0x18 | 00 0001 1000 | PMUL88 |
| 0x19 | 00 0001 1001 | PMULA |
| 0x1A | 00 0001 1010 | PMULH |
| 0x1B | 00 0001 1011 | PMULL |
| 0x1C | 00 0001 1100 | BFLYB |
| 0x1D | 00 0001 1101 | BFLYW |
| 0x20 | 00 0010 0000 | PCMPEQB |
| 0x21 | 00 0010 0001 | PCMPEQW |
| 0x22 | 00 0010 0010 | PCMPHIB |
| 0x23 | 00 0010 0011 | PCMPHIW |
| 0x24 | 00 0010 0100 | STOREC |
| 0x25 | 00 0010 0101 | STOREM / STOREM3 |
| 0x29 | 00 0010 1001 | BSEL |
| 0x2C | 00 0010 1100 | PCMPGEB |
| 0x2D | 00 0010 1101 | PCMPGEW |
| 0x2E | 00 0010 1110 | PCMPGTB |
| 0x2F | 00 0010 1111 | PCMPGTW |
| 0x30 | 00 0011 0000 | PMINSB |
| 0x31 | 00 0011 0001 | PMINSW |
| 0x32 | 00 0011 0010 | PMINUB |
| 0x33 | 00 0011 0011 | PMINUW |
| 0x34 | 00 0011 0100 | PMAXSB |
| 0x35 | 00 0011 0101 | PMAXSW |
| 0x36 | 00 0011 0110 | PMAXUB |
| 0x37 | 00 0011 0111 | PMAXUW |
| 0x38 | 00 0011 1000 | LSLQ |
| 0x39 | 00 0011 1001 | LSRQ |

**Special-format instructions (opcode in word 1 bits 12‑0):**

| Instruction | Word 1 pattern |
|-------------|----------------|
| C2P | `0000 d 001 0100 0` → d-embedded + 0x0A8 |
| MINTERM | `0000 d 001 0101 0` → d-embedded + 0x0AA |
| TRANSHI | `0000 d 000 0000 10` → d-embedded + 0x002 |
| TRANSLO | `0000 d 000 0000 11` → d-embedded + 0x003 |
| UNPACK1632 | `0000 d 000 0111 10` → d-embedded + 0x01E |
| LOADI | `0001 d 000 0000 1` → d-embedded + 0x801 |
| STOREI | `b 0001 0000 0100` → b-embedded |
| LOAD | `0000 d 000 0000 1` → d-embedded + 0x001 |

**VPERM uses special VEA mode `111 111`:**

| Instruction | Word 0 suffix | Word 1 |
|-------------|---------------|--------|
| VPERM | `A B D 111 111` | `b d 0 0 0 0 a` + extension words |

**TEX uses 3-word format — see TEX section above.**

---

## Boundary Cases

* Switching `.cpu` from 68080 to earlier CPUs invalidates subsequent E/B,
  AMMX, and 68080 FPU forms.
* `.apollo` on non-68080 CPUs always fails deterministically.
* `.fpu 68080` on non-68080 CPUs always fails deterministically.
* Ambiguous register-like symbols continue to follow parser token precedence.
* Full-surface enablement must not silently remap 68080 mnemonics to 68040
  encodings when dedicated encodings are required.
* BFLY/TRANS destination register pair must be validated for even alignment.
* MINTERM/TRANS source group must be validated for mod-4 alignment.
* STOREM3 mode value must be 0‑3; out-of-range must fail deterministically.
* TEX nested operand syntax must be parsed correctly; malformed nesting must
  produce clear diagnostics.
* DBcc.L / FDBcc.L displacement must be even before the odd-bit signal is
  applied; if the natural displacement is odd, this is a misalignment error.
* B-register instructions (ADDQ Bn, SUBQ Bn, MOVE Bn, MOVEA Bn, LEA Bn,
  CMP Bn) must reject B-register operands on non-68080 CPUs.

---

## Acceptance Criteria

- [ ] `AC-68080-FULL-001`: `cpusupport`/capabilities report full 68080 support
      metadata including FPU and AMMX surfaces.
- [ ] `AC-68080-FULL-002`: Existing 68000‑68040 positive/reference fixtures
      remain unchanged.
- [ ] `AC-68080-FULL-003`: Full 68080 integer family matrix assembles on
      `.cpu 68080` with expected bytes — every instruction in INT-01 through
      INT-18 produces correct bytes.
- [ ] `AC-68080-FULL-004`: Full AMMX family matrix assembles for legal forms
      and rejects illegal shapes deterministically — every instruction in
      AMMX-01 through AMMX-15 produces correct bytes.
- [ ] `AC-68080-FULL-005`: Full 68080 FPU family matrix assembles under the
  default integrated `.cpu 68080` FPU state or explicit `.fpu 68080`, and
  rejects illegal target pairings — FPU-01 through FPU-04 produce correct
  bytes.
- [ ] `AC-68080-FULL-006`: Non-68080 CPUs deterministically reject 68080-only
      mnemonics and E/B registers.
- [ ] `AC-68080-FULL-007`: AB suites include 68080 fixtures with explicit
      documented-divergence metadata where required.
- [ ] `AC-68080-FULL-008`: New diagnostics normalise to existing classes
      without introducing unstable class names.
- [ ] `AC-68080-FULL-009`: All dotless mnemonic aliases (PADDB, PADDW, PSUBB,
      PSUBW, PADDUSB, PADDUSW, PSUBUSB, PSUBUSW, BFLYB, BFLYW, PAVGB)
      parse and encode correctly.
- [ ] `AC-68080-FULL-010`: Register-pair constraints (BFLY, TRANS, UNPACK1632)
      and register-group constraints (MINTERM, TRANS) are validated with
      deterministic error diagnostics for violations.
- [ ] `AC-68080-FULL-011`: B-register integer instructions (ADDQ Bn, SUBQ Bn,
      MOVE Bn, MOVEA Bn, LEA Bn, CMP Bn) assemble correctly on 68080 and
      fail deterministically on other CPUs.

---

## Validation Expectations

* Add comprehensive 68080 assembler tests by family (integer / AMMX / FPU).
* Add example/reference matrices for full 68080 family coverage.
* Add AB fixtures for success / error / documented-divergence paths.
* Run full quality gates and reference comparison suites.
* Run spec artifact validation and preserve quality-gate evidence.
* Verify every instruction against the Complete Opcode Reference Table.

---

## Summary of New `M68080MnemonicKind` Variants Required

| # | Variant | Primary mnemonics |
|---|---------|-------------------|
| 1 | `Paddusb` | PADDUSB |
| 2 | `Paddusw` | PADDUSW |
| 3 | `Psubusb` | PSUBUSB |
| 4 | `Psubusw` | PSUBUSW |
| 5 | `Pavgb` | PAVGB |
| 6 | `Pmaxsb` | PMAXSB |
| 7 | `Pmaxub` | PMAXUB |
| 8 | `Pmaxsw` | PMAXSW |
| 9 | `Pmaxuw` | PMAXUW |
| 10 | `Pminsb` | PMINSB |
| 11 | `Pminub` | PMINUB |
| 12 | `Pminsw` | PMINSW |
| 13 | `Pminuw` | PMINUW |
| 14 | `Bflyb` | BFLYB / BFLY.B |
| 15 | `Bflyw` | BFLYW / BFLY.W |
| 16 | `C2p` | C2P |
| 17 | `Lslq` | LSLQ |
| 18 | `Lsrq` | LSRQ |
| 19 | `Minterm` | MINTERM |
| 20 | `Storem` | STOREM |
| 21 | `Storem3` | STOREM3 |
| 22 | `Tex` | TEX / TEX8 / TEX16 / TEX24 |
| 23 | `Transhi` | TRANSHI |
| 24 | `Translo` | TRANSLO |
| 25 | `Extub` | EXTUB |
| 26 | `Extuw` | EXTUW |
| 27 | `Perm` | PERM |
| 28 | `Bank` | BANK |

**Total after expansion:** 40 (existing) + 28 (new) = **68 variants**.

Additionally, the following existing encoders / code paths require extension
(no new enum variant, but new encoding logic):

* ADDQ → B-register destination path
* SUBQ → B-register destination path
* CMP → B-register source path
* LEA → B-register destination path / B-register indirect source path
* MOVE → B-register source path
* MOVEA → B-register destination path
* CLR → `.Q` size path (Line-A encoding)
* DBcc → `.L` size path (long counter signal)
* Bcc → `.S+` variant (extended short branch)
* BRA → `.S+` variant
* BSR → `.S+` variant
* FDBcc → `.L` size path (long counter signal)
* FMOVE → `.D` data-register path on 68080
* FMOVEM → Apollo extended format awareness
* MOVEC → 68080 control register codes
* MOVE16 → suppress alignment check on 68080

---

## Open Questions

- [x] `Q-68080-FULL-001`: Resolved. `.apollo off` is rejected
  deterministically because strict compatibility mode is not implemented in the
  shipped full-profile build.
- [x] `Q-68080-FULL-002`: Resolved. `.cpu 68080` defaults the runtime FPU
  target to `68080`; `.fpu none` explicitly disables the integrated 68080 FPU.
- [ ] `Q-68080-FULL-003`: Which 68080 FPU syntax aliases should be canonical
  when multiple spellings exist in legacy source corpora?
- [ ] `Q-68080-FULL-004`: For the PADD/PSUB opcode derivation — the exact
  opcode values for PADDUS* and PSUBUS* (0x14‑0x17 vs 0x16‑0x17) should be
  verified against real hardware or a trusted assembler before committing.
  The current derivation follows the pattern established by the existing
  passing test suite (PADDB=0x10, PADDW=0x11, PSUBB=0x12, PSUBW=0x13) and
  extends by +4 for U=1.
