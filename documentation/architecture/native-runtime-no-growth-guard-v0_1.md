# Native Runtime No-Growth and Ownership Guard v0.1

## Purpose

Item 5.12 converts the certified native runtime boundaries into deterministic
workflow enforcement. The guard protects ownership and dependency structure;
it is not a semantic parity test and does not use file length as a proxy for
responsibility.

The entrypoint is:

```text
python3 scripts/workflow/check_native_runtime_no_growth.py [--staged]
```

It is also part of `run_native_porting_quality_gate.py`, so staged native work
cannot bypass it. The guard invokes the existing CPU-specific architecture
checker in matching full or staged mode; that checker and its reviewed
allowlist remain the sole CPU/family vocabulary authority. Nested enforcement
uses its report-free mode so a read-only structural check does not create,
clear, or rewrite advisory artifacts; direct checker invocations retain their
normal report behavior.

## Enforced contracts

### Certified hotspot routine sets

The structural baseline records the routine names present when ownership was
certified for:

- `opasm_assembly_driver.asm`;
- `tkpkg_service.asm`;
- `opcore_expr_bridge.asm`.

Removing or delegating existing routines remains possible. A new routine is
rejected unless its immediately preceding comment block declares all three:

```asm
; @opforge-owner: opasm.amigaos.example_owner
; @opforge-slice: documentation/plans/slices/example.toml
; @opforge-role: delegation
exampleDelegateV1 .block
```

The only permitted roles are `facade` and `delegation`, and the named slice
must exist. This allows narrow compatibility surfaces while rejecting silent
private semantic growth. Updating the baseline merely to admit a new routine
is prohibited; the declaration is the reviewable exception mechanism.

### tkpkg to opasm mutable-state direction

Every production tkpkg assembly source is scanned for direct references to
engine-owned context, session, source, statement, label, or image symbols.
Package consumers must use the neutral runtime context and documented engine
getter adapter instead of mutable engine tables.

### New semantic module provenance

The baseline lists production native modules that existed at certification.
A later production `.asm` file containing both `.module` and `.block` must name
an owner and an existing slice in its first 40 lines:

```asm
; @opforge-owner: tkpkg.amigaos.example
; @opforge-slice: documentation/plans/slices/example.toml
```

Test harnesses and debug tools are classified separately and are outside this
production-module rule. They remain subject to their evidence and
instrumentation policies.

### CPU/family ownership

`check_native_runtime_no_growth.py` runs
`check_cpu_specific_arch_boundary.py`. CPU, family, dialect, register,
addressing-mode, and mnemonic vocabulary outside package/CPU owners therefore
fails unless an existing narrow allowlist rule supplies a reviewed reason.

## Evidence boundary

Positive and negative workflow tests prove that the structural checks accept
the certified sources and declared delegations, and reject undeclared hotspot
routines, direct mutable-table access, and missing module provenance. They are
Level B ownership evidence only. The established real-CLI Level D corpus must
still pass separately; the guard itself makes no runtime or output claim. The
fail-closed aggregate command is:

```text
scripts/workflow/run_native_existing_parity_completion.sh --verify
```

It serializes the established reference/CLI, macro, expansion, flow, struct,
expression, text, module-local, layout, and package selection/encoding groups,
rejects a skipped or zero-test filter, accepts staged ownership work, and
writes no source-identity receipt.
