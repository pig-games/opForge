# Native Rust-to-68000 Porting Workflow

This is the normative workflow for making the native 68000/AmigaOS
implementation match opForge Rust VM or CLI behavior. Its purpose is to keep
each parity change narrow, make evidence honest, and prevent diagnostics from
changing the behavior being observed.

The active `AGENTS.md` remains binding.

## Required rule packs

- `agents/rules/native-rust-parity-porting.md`
- `agents/rules/native-parity-failure-triage.md` when triaging a failure
- `agents/rules/native-68000-safe-instrumentation.md` before instrumenting
- `agents/rules/native-68000.md` when editing 68000 assembly
- `agents/rules/fs-uae.md` when running FS-UAE

## Slice contract

Before production edits, name one invariant and record:

```text
Slice name:
Rust reference files/functions:
Native target files/functions:
Boundary type:
Contract:
Expected native inputs:
Expected native outputs:
Known non-equivalences:
Proof-level tests:
Fast proof:
FS-UAE proof:
```

Native implementation details may differ for memory layout, calling convention,
register pressure, fixed buffers, AmigaOS I/O, and 68000 control flow. Semantics
must continue to match the named Rust contract.

## Boundary-first loop

1. Reproduce the real native failure once.
2. Capture the exact source, request, session, and output evidence.
3. Compare Rust and native behavior from source read through output artifact.
4. Stop at the first divergent boundary.
5. Create the smallest host-side discriminator that faithfully represents that
   boundary, when possible.
6. Change production code only at that boundary.
7. Run the focused proof and required quality gates.
8. Run the named FS-UAE confirmation with `--test-threads=1`.
9. Remove temporary probes; retain only stable, documented contract assertions.

The boundary order is source line, tokenization, parser/portable statement,
native statement/session record, expression envelope, EXVM result, selector,
encoder, session bytes, and output artifact.

## Evidence classification

| Level | Meaning | Valid use |
|---|---|---|
| A | Pure Rust semantic oracle | Establish reference behavior |
| B | Rust-side package/native harness | Prove a harness boundary |
| C | Host-side native request simulator | Prove request shape or host contract |
| D | Native 68000/AmigaOS through FS-UAE | Confirm real native behavior |
| E | Temporary localization/debug probe | Locate a divergence only |

For every cited test, state:

```text
This test proves:
This test does not prove:
```

Levels A-C do not prove actual 68000 execution. Level D does not by itself show
which internal boundary diverged. Level E never supports a completion claim.
Reduced or prefix fixtures are Level E unless their semantic completeness,
including pass-2 symbols and forward references, is documented.

## Failure triage and claims

Keep a hypothesis ledger with evidence for and against each hypothesis, its
status, and the next discriminator. Avoid simultaneous changes to capacity,
request shape, source handling, selectors, expression bridges, and diagnostics.

A completed slice states the corrected invariant, previous evidence, production
change, minimal proving test and proof level, result, any distinct remaining
failure, and whether instrumentation was removed or stabilized. A moved failure
frontier is localization evidence, not proof of a fix.

## Safe instrumentation

Ad-hoc 68000 instrumentation is forbidden. Approved debug/assert macros must be
build-flagged, fixed-size at call sites, preserve their documented registers and
SR/CCR, balance the stack, avoid request/service/error buffers, and leave
production control flow unchanged. Never instrument between a flag-setting
instruction and its branch.

Every instrumentation change records its point, macro or routine, preservation
contract, stack delta, buffers touched, branch-neutrality argument, and
removal/stabilization plan. Until those properties are proven, instrumentation
is treated as production code and its observations are untrusted.
