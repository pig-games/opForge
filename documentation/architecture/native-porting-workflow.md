# Native development workflow

Use the [repository workflow](../workflow/README.md) for scope, commits, validation
cadence and reporting. Native-specific guidance is maintained in:

- [Assembly conventions](../../agents/rules/native-68000.md)
- [Reference boundaries and native proof](../../agents/rules/native-rust-parity-porting.md)
- [Failure triage](../../agents/rules/native-parity-failure-triage.md)
- [Instrumentation safety](../../agents/rules/native-68000-safe-instrumentation.md)
- [FS-UAE execution](../../agents/rules/fs-uae.md)

The deterministic native checks are available through
`python3 scripts/workflow/run_native_porting_quality_gate.py`. Run real-native
confirmation separately when required by the affected behavior. Checkpoint commits
do not require plan metadata or a full emulator run.
