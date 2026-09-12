# Native parity failure triage

Use the [native parity contract](native-rust-parity-porting.md) to judge evidence
and the [FS-UAE guide](fs-uae.md) when running the emulator.

Compare the earliest unproven reference/native boundary. Use a focused hypothesis
and discriminator; maintain notes only when they help track a complex investigation.
Do not accumulate speculative fixes without evidence.

Reduced fixtures and prefix scans are localization probes unless their semantic
completeness is established. Check forward symbols, label-only endings and pass-two
meaning before using a reduction as proof of a corrected invariant.

Explain the corrected invariant, the previous failure and the focused evidence
that now proves it. A moved failure is localization progress, not proof of a fix.
Distinguish remaining failures and instrumentation artifacts from the original bug.
