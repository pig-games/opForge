# CPU-specific architecture boundary

The generic opForge Rust VM and Native VM implementations must not contain
target CPU, family, dialect, register, addressing-mode, or instruction-specific
logic.

The generic implementation executes package-provided semantics. It does not know
what CPU it is serving.

Builder and package-authoring code may know concrete CPU families, selector
shape vocabularies, operand plans, and mode metadata because that layer is the
source of package-defined behavior.

Runtime VM code may execute generic selector matching, operand-plan evaluation,
bytecode walking, and expression evaluation against package-provided records.
Runtime VM code must not derive CPU-family selector meaning from source syntax,
register names, mnemonic spellings, or addressing-mode names.

Native VM implementations follow the same rule: they may consume package-defined
selector and operand-plan metadata, but they must not become package builders or
family-specific selector resolvers.

Current native transitional seam:
- `native/motorola68000/amigaos/tkpkg/tkpkg_service.asm` still contains one
  table-driven lookup seam that collapses package-owned selector shape and mode
  tags into compact local surface codes before operand evaluation.
- That seam is transitional and must stay isolated. New native selector work
  should extend package data or package-owned adapters instead of reintroducing
  per-shape compare ladders or scattered tag-name helpers.

Current deterministic enforcement focuses on architecture-neutral core parser,
shared type, root `src/`, workflow implementation paths, and `native/**`.
Native assembly is scanned structurally for implementation-owned labels,
constants, macro names, and section/module metadata. The guard intentionally
ignores assembler-language syntax such as mnemonics and ordinary directives so
that violations come from CPU-specific implementation details rather than the
host assembler language itself.

The workflow also runs a warning-only scan across selected broader Rust
implementation crates so future tightening candidates stay visible without
failing the gate. Rust test files are excluded from both scopes.

## Allowed

- package VM definitions
- family packages
- dialect packages
- examples
- fixtures
- tests
- documentation
- reviewed allowlist entries with concrete rationale

## Forbidden in generic implementation paths

- hardcoded CPU names
- hardcoded instruction mnemonics
- hardcoded addressing modes
- register-specific branches
- instruction-width assumptions
- page-crossing or branch-displacement CPU behavior
- target CPU status flag logic
- selector-shape derivation from target-specific source syntax
- inferring selector semantics from registers such as `A`, `X`, or `Y`
- deriving package selector choices from CPU-specific mnemonics such as bit-branch or block-move spellings

## Quality gate

Run:

```sh
python3 scripts/workflow/check_cpu_specific_arch_boundary.py
```

The same check should be part of the standard quality gates.

The check reports two categories:

- enforced scope findings that fail the gate
- warning-scan findings that are advisory only for now

The checker keeps normal stdout summary-oriented. Full finding dumps are written
to report files when present:

- `build/reports/cpu_specific_arch_boundary_enforced_findings.txt` for enforced-scope findings
- `build/reports/cpu_specific_arch_boundary_warning_scan.txt` for advisory warning-scan findings

If the check fails, prefer moving the behavior into package VM definitions. Only
use the allowlist when the term is genuinely user-facing metadata, diagnostic
text, test data, or another reviewed false positive.
