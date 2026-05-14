# CPU-specific architecture boundary

The generic opForge Rust VM and Native VM implementations must not contain
target CPU, family, dialect, register, addressing-mode, or instruction-specific
logic.

The generic implementation executes package-provided semantics. It does not know
what CPU it is serving.

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