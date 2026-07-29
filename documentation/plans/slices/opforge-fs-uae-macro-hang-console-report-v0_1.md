<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# FS-UAE Macro-Hang Console Report v0.1

<!-- @opforge-evidence: level=E; role=diagnostic-report; authority=none; lifecycle=permanent -->

## Scope

This is a Level E diagnostic report for the native macro fixture
`examples/opcore/macro_invocation_native.asm`. It records the stock FS-UAE
console result and does not claim macro parity or a production fix.

## Reproduction

The diagnostic used the direct native CLI macro harness with the fixture mounted
as `Work:opforge_6502_native_cli_smoke.asm`, the normal disposable FS-UAE
configuration, and the reviewed command sequence:

```text
r
d {pc} 16
m {a7} 16
fl
Zl
H 32
q
```

The local, user-authorized invocation set
`OPFORGE_FS_UAE_CONSOLE_DEBUGGER=1` and
`OPFORGE_FS_UAE_CONSOLE_DEBUGGER_AUTOMATE=1`, then sent `Cmd+D` after ten
seconds. The launcher was the same `FailAt 999` / `Work:build/opforge_fsuae_smoke.hunk`
script used by the controlled console harness.

## Captured result

- FS-UAE: `3.1.66 (Built for macOS ?)`.
- Fixture: `examples/opcore/macro_invocation_native.asm`, source CPU `65c02`,
  dotted macro calls preserved.
- Generated configuration: `target/fs-uae-console-debugger-macro-hang-r7/console-debugger.config.fs-uae`.
- Transcript: `target/fs-uae-console-debugger-macro-hang-r7/console-debugger.transcript.txt`.
- Raw transcript SHA-256:
  `67852964a3a66c3b70b5887adc72ef6b14079000dee8e887493804b874692b7b`.
- Entry: `pty-command`; commands sent: `true`; cleanup: `complete`.
- Stop reason: `process-exit` after the reviewed `q` command.
- Parsed frame: PC `0x00F815CE`, SR `0x2000`, D0-D7 and A0-A7 recorded in
  the JSON report; A7 was `0x07802300` and its bounded stack dump is in the
  transcript.
- Disassembly at the reported PC is AmigaOS idle-loop code, not the native
  Hunk. CPU history likewise reports the idle branch at `0x00F815CC`.

## Finding

The installed stock debugger cannot identify the macro fixture's first
non-returning routine in this configuration. `Zl` returned `found 0 seglists`,
so no Hunk segment base or symbol mapping is available. `Cmd+D` pauses the
currently scheduled Amiga task; it repeatedly captured the operating-system
idle loop even while the bounded macro run had not produced its completion
marker. Therefore a PC from this transcript must not be attributed to the
native macro implementation.

The first independently observed native divergence remains the integrated CLI
frontend: earlier direct-harness guest output reached `STATUS tokenizer-ok` and
then reported `OPC-NCLI010: native tokenizer stage failed`. The isolated
macro-preprocessor harness exits successfully, so package selection and the
standalone capture/substitution contract are not the first failing boundary.

## Next discriminator

Item 6 must correct the integrated macro-line expansion invariant using a
focused host/native boundary proof, then confirm the untouched fixture through
the normal Level D FS-UAE parity run. A future debugger investigation requires
either loadable Hunk debug data/segments or a separately approved remote-debug
facility; neither is part of this plan.
