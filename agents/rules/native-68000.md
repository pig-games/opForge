# Native Motorola 68000 Rule Pack

Load this only when touching supported `native/motorola68000/**/*.asm` files.

## Formatter

- Run `scripts/workflow/run_native_68000_format_gate.sh` or
  `make native-68000-format-check` before treating formatting as complete.
- If formatting changes are required, run
  `scripts/workflow/run_native_68000_format_gate.sh --write` or
  `make native-68000-format`, then re-run the check.
- Use the root `.opforgefmt.toml` unless the user explicitly requests otherwise.

## Routine structure

- Every logical routine must be enclosed in a `.block` / `.bend` pair.
- Put `.block` on the same line as the routine label.
- Put `.bend` after the routine's final `rts`, with a trailing comment naming the routine:

```asm
routineName .block
    ; ...
    rts
.bend  ; routineName
```

- Do not wrap ordinary branch targets or loop labels in their own `.block` unless
  they are standalone callable routines.
- Local control-flow labels belong inside the enclosing routine block.
- Group exported routines before internal helpers.
- Start the exported group with `.pub`; start helper/internal routines with `.priv`.
- Mark a symbol public only when another module intentionally consumes it.
- Treat public routines as module ABI. Public entry points must document or make
  clear their input/output register contract.
- Public routines should preserve caller-visible registers unless their contract
  explicitly says otherwise. Use balanced save/restore such as `movem.l` when needed.
