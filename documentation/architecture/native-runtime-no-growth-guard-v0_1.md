# Native routine growth and ownership guard

The guard protects selected routine boundaries and owner provenance. It does not
use line counts as a proxy for responsibility or prove runtime parity. Run:

```text
python3 scripts/workflow/check_native_runtime_no_growth.py [--staged]
```

It is part of `run_native_porting_quality_gate.py`, alongside the
[import contract](native-runtime-boundary-contract-v0_1.md) and
[source inventory](native-runtime-boundary-inventory-v0_1.md).

## Selected routine boundaries

The baseline records existing top-level `.block` routines for four live owners:

| Source under `native/motorola68000/amigaos/` | Current responsibility |
| --- | --- |
| `assembler/driver.asm` | Build/session and subsystem orchestration. |
| `assembler/prepare/prepare.asm` | Shared numeric statement/operand lowering. |
| `assembler/layout/assembly.asm` | Shared sizing, directive dispatch and emission. |
| `vm/exprvm/i64_math.asm` | Generic scalar arithmetic. |

These existing routines include substantive implementations; they are not all
facades. Their reviewed names remain in the baseline. Removing or delegating a
routine is permitted. A new top-level routine in one of these files must declare
its owner and a narrow facade/delegation role immediately above its definition:

```asm
; @opforge-owner: opforge.assembler.prepare.example
; @opforge-role: delegation
forwardRequest .block
```

Only `facade` and `delegation` are accepted. Do not label new semantics as delegation
or expand the baseline just to admit a routine. A consequential responsibility
change needs review and an intentional guard change.

This routine-name rule inspects the four top-level files, not all helper blocks.
The complementary inventory covers their literal helper closures and rejects
helper-only edits until the changed contents and responsibilities are reviewed.
The combined gate therefore does not treat moving code into an `.i` file as an
unreviewed escape from the audited surface.

## Module owner provenance and reviewed exemptions

A non-exempt production `.asm` module with executable blocks must name its owner
in its first 40 lines. Blocks in literal includes and macro definitions count too;
a module consisting only of an include cannot bypass this rule. This conservative
provenance test differs from the inventory's unexpanded routine counts.

```asm
; @opforge-owner: opforge.assembler.prepare.example
```

Test and debug modules have their own evidence/instrumentation rules and are not
subject to this production-module header requirement. Data/schema-only modules
without blocks do not need this annotation.

`native_runtime_ownership_baseline.toml` retains ten annotation exemptions for
live original VM modules and the `main` composition root. Those sources lack owner
headers but still implement arithmetic, expression/parser/tokenizer execution,
request routing or executable composition. The exemption is only from this header
requirement; import, CPU-boundary and applicable inventory checks still apply.
Missing exempt sources fail so deleted entries cannot silently accumulate.

The redundant exemptions for `vm/tkvm/state.asm` and `vm/tkvm/demo_program.asm`
were removed after source/caller review: both are live data/schema modules with
no executable blocks. Their source files and consumers remain maintained.

## CPU ownership and evidence limits

The guard invokes `check_cpu_specific_arch_boundary.py` in matching full/staged
mode with `--no-report`, so nested enforcement does not rewrite advisory reports.
Its reviewed vocabulary allowlist remains the CPU/family authority. No CPU
exceptions were removed or broadened in this contract cleanup.

Positive and negative workflow tests cover current sources, declared delegations,
undeclared routine growth and missing owner provenance, including executable
helpers. These are structural checks. They do not prove direct mutable-table
access is absent, validate callback providers or qualify native output.

Use the [workflow](../workflow/README.md) and
[native parity contract](../../agents/rules/native-rust-parity-porting.md) to select
runtime checks when behavior changes. The separate
`scripts/workflow/run_native_existing_parity_completion.sh --verify` command runs
the active opcore, MOS and Motorola reference corpus groups and rejects missing,
skipped or zero-test execution; it is not run by the deterministic gate.
