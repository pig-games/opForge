# libopforge Execution Modes and Lockstep Guide

This guide explains when to choose `ExecutionMode::Vm`,
`ExecutionMode::Rust`, or `ExecutionMode::Lockstep { continuation_head: ... }`
in the published preview `libopforge` facade.

The mode names and defaults below describe `v0.9.6`; they are not yet a
long-term stable API commitment.

## 1. Quick chooser

| Mode | Use it when... | What you get | Avoid it when... |
|---|---|---|---|
| `ExecutionMode::Vm` | you want the normal current default path | the current default execution model used by the high-level config surface | you specifically need the native Rust continuation head or parity comparison |
| `ExecutionMode::Rust` | you want the native Rust continuation head for a request | explicit execution through the Rust head without lockstep comparison | you are validating Rust/VM parity or you want the documented default path |
| `ExecutionMode::Lockstep { continuation_head }` | you need parity validation between the Rust and VM heads | a normal assembly/report flow plus a `LockstepReport` with matches, checkpoints, and divergences | you only need one execution head and do not intend to consume parity data |

## 2. Default behavior

The current config types default to `ExecutionMode::Vm`.

That default suits normal builds, validations, and host integrations that do not need to override the continuation head explicitly.

## 3. When to choose `Vm`

Choose `Vm` when you want the normal documented execution path without parity
analysis.

This is the right default for:

- regular CLI-style builds
- editor validation requests
- services or test harnesses that do not need to compare heads
- prepared flows where execution-head choice is not the experiment

If your host is mainly orchestrating assembly, diagnostics, and artifact emission, `Vm` should remain the baseline until a real need for another mode appears.

## 4. When to choose `Rust`

Choose `Rust` when you want the native Rust continuation head for the request and do not need lockstep parity output.

Typical reasons include:

- investigating head-specific behavior during development
- matching an existing native-only integration path
- comparing host behavior against the default `Vm` path in a controlled way

If parity evidence matters, move to `Lockstep` instead of building an ad hoc compare loop in the host.

## 5. When to choose `Lockstep`

Choose `Lockstep` when the host needs to validate parity between the Rust and VM heads and consume a structured `LockstepReport`.

This is the right fit for:

- parity-sensitive runtime work
- migration or certification efforts
- tests that must prove both heads stay aligned
- tooling that wants lockstep checkpoints or divergence categories

The `continuation_head` field selects which head continues as the primary path after comparison. Set it deliberately rather than treating it as cosmetic host metadata.

Reference example: `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`.

## 6. Continuation-head choice

Inside lockstep mode, choose the continuation head based on which runtime should remain authoritative for the surrounding workflow.

Use `ContinuationHead::Vm` when:

- the VM path is your normal default
- your host wants parity evidence but still follows VM-side continuation behavior

Use `ContinuationHead::Rust` when:

- the host is intentionally validating against a Rust-led continuation path
- you want parity evidence while keeping the native Rust head as the authoritative continuation result

The continuation head matters most for hosts that keep running after the comparison rather than treating lockstep as a pure diagnostics-only pass.

## 7. Typical workflows

### 7.1 Normal assembly with the default mode

Do nothing special. Let the builder or config default to `ExecutionMode::Vm` unless the request owns a concrete override.

### 7.2 Explicit native-head run

Set `.execution_mode(ExecutionMode::Rust)` on the builder or session when the request intentionally wants the Rust head.

### 7.3 Lockstep with prepared metadata

Use a prepared flow when your host needs dependency or source-map metadata before the parity-sensitive final run.

1. Configure `ExecutionMode::Lockstep { continuation_head: ... }` on `AssemblerSession::builder(...)`.
2. Call `prepare()` if the host needs metadata first.
3. Run `assemble()` and inspect `lockstep_report()` on the resulting `AsmRunReport`.

Reference example: `documentation/libopforge-developer-guide-examples/libopforge_lockstep.rs`.

## 8. Reading `LockstepReport`

The high-level report stays the same as other assembly flows, but lockstep runs additionally expose a `LockstepReport`.

Start with:

- `lockstep_report().matches()` when you want successful parity checkpoints
- divergence and checkpoint metadata when you need to explain where the heads stopped agreeing

Keep parity reporting in the structured lockstep model instead of flattening it into ad hoc log text early. That preserves enough detail for tests, CI annotation, or UI inspection.

## 9. Operational rules

- Use `check()` for validation-only requests regardless of execution mode.
- Use `assemble()` when the host expects output artifacts or the full assembly report.
- Prefer `Lockstep` over custom dual-run host logic when parity is the requirement.
- Keep execution-mode choice in the host request/config model so the reason for a non-default mode remains explicit.

Embedding recipes for borrowed, owned, in-memory, and FFI-oriented hosts are in `documentation/libopforge-embedding-cookbook.md`.
