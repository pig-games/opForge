# Motorola 68000 Family External-Oracle A/B Test Report

Source corpus: `examples/ab/motorola68000/vasm/`

Validation command used for this report:

```bash
OPFORGE_EXTERNAL_ORACLE_VASM=1 cargo test -p asm external_oracle_ -- --nocapture
```

## Executive summary

The current Motorola 68000-family external-oracle corpus contains `47` fixtures:

- `32` shared-subset success fixtures that assemble to matching bytes
- `5` shared-subset negative fixtures that fail with matching normalized error classes
- `10` documented-divergence fixtures

This branch resolved the previously identified opForge-side divergences for:

- `DIVSL.L` / `DIVUL.L` alias encoding on `68020+`
- `FNOP` encoding on `.fpu 68881`
- `FNOP` encoding on `.fpu 68882`
- `FNOP` legality on `.fpu 68040`

The documented-divergence ledger currently breaks down like this:

| Bucket | Count | Guidance |
| --- | ---: | --- |
| opForge is wrong | 0 | No documented divergence currently shows opForge on the wrong side of the Motorola-spec-backed shared subset |
| vasm is wrong or too opinionated for this corpus | 7 | explicit `LINK.L` shortening on `68020/68030/68040`, negative `PACK`, `CALLM` on `68030`, and `FSIN`/`FETOX` on plain `68040` |
| syntax-policy / shared-subset issue | 2 | `FSINCOS ... .pair(...)` is opForge-specific syntax; use Motorola canonical syntax for portable source |
| intentional non-shared-subset gap | 1 | `OPT` is a vasm directive, not a Motorola ISA feature |

## Corpus summary

| Profile | Fixtures | Matching success | Matching negative | Documented divergence |
| --- | ---: | ---: | ---: | ---: |
| `68000` | 19 | 16 | 2 | 1 |
| `68010` | 1 | 1 | 0 | 0 |
| `68020` | 5 | 3 | 0 | 2 |
| `68020-fpu-68881` | 3 | 2 | 0 | 1 |
| `68020-fpu-68882` | 3 | 2 | 0 | 1 |
| `68030` | 4 | 1 | 1 | 2 |
| `68030-mmu` | 2 | 2 | 0 | 0 |
| `68040` | 5 | 1 | 3 | 1 |
| `68040-fpu` | 4 | 2 | 0 | 2 |
| `68040-mmu` | 2 | 2 | 0 | 0 |

That means the shared subset is already healthy: the pass/fail surfaces match on
`38` non-divergence fixtures, and the remaining work is concentrated in a small
set of explicitly tracked disagreements.

## How to decide which assembler is right

When opForge and `vasm` disagree in this corpus, use this order of precedence:

1. If the source uses an explicit Motorola size suffix or explicit Motorola
   syntax, the Motorola manual wins over assembler shortening or rewriting.
   `LINK.L A6,#-8` is the clearest example.
2. For integer instruction availability, use the CPU applicability stated in
   the Motorola manuals, not the behavior of one assembler. `CALLM` remains
   valid on `68030`, but `RTM` is `68020`-only; `MOVE16` is `68040`-only; and
   `CAAR` is not valid on `68040`.
3. For `68881`/`68882` syntax and encodings, the
   *MC68881/MC68882 User's Manual* is the authority. This resolves both
   `FNOP` encoding and canonical `FSINCOS` syntax.
4. For `.fpu 68040`, first decide whether the target means the integrated
   hardware only or hardware plus `M68040FPSP` software assist. This corpus
   models the integrated hardware surface only, so the plain `MC68040 User's
   Manual` implemented/unimplemented instruction tables win.
5. If the disagreement is about an assembler-local directive or convenience
   syntax, do not treat it as a CPU-spec dispute. `OPT` and `.pair(...)` fall
   into this category.

## Detailed divergence verdicts

### 1. Intentional non-shared-subset gap

| Fixture | Verdict | Why | Reference |
| --- | --- | --- | --- |
| `68000/documented_divergence/opt_directive.asm` | Do not use either assembler as an ISA oracle here | `OPT` is a `vasm` directive, not Motorola 68000 instruction syntax. This is outside the shared subset by design. | No Motorola ISA reference applies; this is assembler-local syntax |

### 2. vasm is wrong or too opinionated for this corpus

| Fixture(s) | Verdict | Why | Motorola reference(s) |
| --- | --- | --- | --- |
| `68020_link_long.asm`, `68030_link_long.asm`, `68040_link_long.asm` | **opForge is right** | The source is explicitly `LINK.L A6,#-8`. The PRM defines `LINK` with size `Word, Long*`, says long-size operation uses a long displacement, and shows a distinct long instruction format. `vasm` emits `LINK.W` bytes (`4E56 FFF8`) instead of the explicit long form (`480E FFFFFFF8`). In this corpus, explicit Motorola source must win over assembler shortening. | *M68000 Family Programmer's Reference Manual* (PRM), `LINK Link and Allocate`, PDF p. 215 / manual p. 4-111 |
| `68020_pack_negative.asm` | **opForge is right** | The PRM defines `PACK Dx,Dy,#<adjustment>` / `PACK -(Ax),-(Ay),#<adjustment>` with a `16-bit extension` adjustment word. The entry does not restrict the adjustment to non-negative values, so `#-1` is a valid two's-complement 16-bit adjustment. | PRM, `PACK Pack`, PDF p. 260 / manual p. 4-156 |
| `68030_callm.asm` | **opForge is right** | The PRM includes `CALLM` as an `MC68020`-introduced instruction, and the `MC68040 User's Manual` appendix table carries `CALLM` forward to `MC68030` while removing it on `MC68040`. `vasm` is therefore too restrictive for `68030` on `CALLM`. `RTM` is tracked separately and should not be used as a `68030` carry-forward instruction. | PRM, `CALLM Call Module`, PDF pp. 168-169 / manual pp. 4-64 to 4-65; *M68040 User's Manual*, Appendix D instruction-set-extension tables, PDF pp. 430-431 |
| `68040_fsin_error.asm` | **opForge is right for this corpus** | The plain integrated `68040` FPU does **not** implement `FSIN`. The `MC68040 User's Manual` lists `FSIN` among unimplemented instructions, while Appendix E shows that `FSIN` is available only through `M68040FPSP`. Since this corpus models `.fpu 68040` as integrated hardware, `vasm` is too permissive here. | *M68040 User's Manual*, Table 9-10 `Unimplemented Instructions`, PDF p. 265; Appendix E `M68040FPSP Floating-Point Instructions`, PDF p. 434 |
| `68040_fetox_error.asm` | **opForge is right for this corpus** | Same reasoning as `FSIN`: `FETOX` is listed as unimplemented by the integrated `68040` FPU and only appears in the `FPSP` appendix table. | *M68040 User's Manual*, Table 9-10, PDF p. 265; Appendix E, PDF p. 434 |
### 3. Resolved in this branch

| Fixture(s) | Resolution |
| --- | --- |
| `68000/documented_divergence/reclassification_candidate.asm` | Reclassified to positive after the oracle run confirmed full byte parity |
| `68020_div_long_aliases.asm` | Fixed: opForge now matches the Motorola `DIVSL.L` / `DIVUL.L` 32/32 encoding used by `vasm` |
| `68020_fpu_fnop.asm` under `68881` and `68882` | Fixed: opForge now emits the Motorola/`vasm` `FNOP` encoding |
| `68040_fnop_error.asm` | Fixed: opForge now accepts `FNOP` for the integrated `68040` FPU and the fixture is reclassified to positive |

### 4. Syntax-policy disagreement, not an ISA disagreement

| Fixture(s) | Verdict | Why | Motorola reference(s) |
| --- | --- | --- | --- |
| `68020_fpu_fsincos_pair.asm` under `68881` and `68882` | Prefer Motorola canonical syntax in shared-subset fixtures | The Motorola syntax is `FSINCOS.<fmt> <ea>,FPc:FPs` or `FSINCOS.X FPm,FPc:FPs`: first destination register is cosine (`FPc`), second is sine (`FPs`). opForge currently accepts `.pair(FPc,FPs)`, which is not Motorola syntax even though it follows the same destination ordering internally. For portable A/B fixtures, use the colon form. | *MC68881/MC68882 User's Manual*, `FSINCOS Simultaneous Sine and Cosine`, PDF p. 162 / manual p. 4-104; FSINCOS encoding tables, PDF pp. 204 and 213 |

## Practical guidance for future disagreements

### If the source spells a size explicitly, honor it

Do not let `vasm` short forms overrule explicit Motorola source. If the fixture
says `LINK.L`, the `LINK` long-form entry in the PRM is the tie-breaker.

### Distinguish instruction availability from assembler habit

The `68030`/`68040` boundary is where this matters most:

- `CALLM` is valid on `68030`
- `RTM` is only valid on `68020`
- `MOVE16` is `68040`-only
- `MOVEC CAAR` is not valid on `68040`

Use the Motorola applicability tables before trusting either assembler's default
policy.

### Treat `68881/68882` manuals as the authority for FPU syntax and bytes

This resolves both main external-FPU disputes in the current corpus:

- `FNOP` encoding should follow the `FBcc.W false, displacement 0` form with
  coprocessor ID `001`
- `FSINCOS` shared-subset syntax should use `FPc:FPs`, not `.pair(...)`

### For `.fpu 68040`, decide whether `FPSP` is in scope before judging

The plain integrated `68040` FPU and `68040 + M68040FPSP` are different targets.
If a future corpus wants the software-assisted surface, it should use a distinct
profile and say so explicitly. Without that, use the integrated-hardware tables
and treat `FSIN`, `FETOX`, and `FSINCOS` as unsupported.

### Reclassify stale documented divergences promptly

If the harness says a documented divergence now matches fully, move it to the
normal success or error corpus. Stale divergences make the report less useful.

## Primary Motorola references used

- *M68000 Family Programmer's Reference Manual* (`M68000PRM.pdf`)
  - `CALLM Call Module`, PDF pp. 168-169
  - `DIVS, DIVSL Signed Divide`, PDF pp. 196-199
  - `DIVU, DIVUL Unsigned Divide`, PDF pp. 200-203
  - `LINK Link and Allocate`, PDF p. 215
  - `MOVE16 Move 16-Byte Block`, PDF p. 230
  - `PACK Pack`, PDF p. 260
  - `RTM Return from Module`, PDF p. 271
  - `MOVEC Move Control Register`, PDF p. 477
- *MC68881/MC68882 User's Manual* (`MC68881UM.pdf`)
  - `FNOP No Operation`, PDF pp. 142-143
  - `FSINCOS Simultaneous Sine and Cosine`, PDF p. 162
  - arithmetic / extension-field encoding tables, PDF pp. 186, 204, 213
- *M68040 User's Manual* (`MC68040UM.pdf`)
  - Section 9 FPU overview, PDF p. 245
  - Table 9-10 `Unimplemented Instructions`, PDF p. 265
  - floating-point timing table including `FNOP`, PDF p. 320
  - Appendix D CPU-extension tables, PDF pp. 430-431
  - Appendix E implemented `MC68040` instructions, PDF p. 433
  - Appendix E `M68040FPSP` instruction table, PDF p. 434
