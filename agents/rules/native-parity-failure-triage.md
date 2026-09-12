# Native failure triage

Use the [parity contract](native-rust-parity-porting.md) to judge evidence and the
[FS-UAE guide](fs-uae.md) when running the emulator.

Separate host launch problems, invalid test inputs, guest failures and unsafe
instrumentation. Compare the earliest unproven reference/native boundary and use
a focused hypothesis and discriminator. Notes are useful for a complex investigation;
a prescribed ledger or fix-report form is not required.

Before using a reduced fixture, check forward references, label-only endings,
source expansion and pass-two meaning. A prefix that removes required definitions
changes the problem. Treat it as a probe unless semantic completeness is established;
even a complete reduced case proves only its own scope.

Explain the corrected invariant, previous failure and focused evidence that now
proves it. A moved failure is localization progress, not proof of a fix. Distinguish
remaining failures from the original defect. Do not accumulate speculative changes
without evidence or treat a host-side simulation as real-native confirmation.

If observation requires code changes, use the [instrumentation guide](native-68000-safe-instrumentation.md).
Choose the next useful check using the repository workflow; no fixed debugging
sequence or full-suite run is required between every hypothesis.
