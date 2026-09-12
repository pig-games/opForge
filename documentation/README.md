# opForge documentation

## Using opForge

- [Project overview and setup](../README.md)
- [Language and assembler reference](opForge-reference-manual.md)
- [Examples](../examples/)
- [Native implementation and usage](../native/README.md)

## Library and contributor references

- [Developer guide](libopforge-developer-guide.md)
- [Embedding cookbook](libopforge-embedding-cookbook.md)
- [Diagnostics and fixits](libopforge-diagnostics-and-fixits-guide.md)
- [Execution modes and lockstep](libopforge-execution-modes-and-lockstep-guide.md)
- [CPU/family extension guide](libopforge-cpu-family-extension-guide.md)
- [Library specification](libopforge-specification.md)
- [Language-server design reference](opforge-language-server-spec-v0_1.md)
- [VM boundary protocol](vm-boundary-protocol-v1.md)
- [Assembler VM path](opforge-assembler-vm-path-guide-v0_1.md)
- [Existing architecture references](architecture/)
- [Performance tooling](performance/)

## Working in the repository

- [Operating contract](../AGENTS.md)
- [Workflow](workflow/README.md)
- [Shared workflow notebook](workflow/NOTES.md)
- [Optional artifact skills](../skills/README.md)
- [Proposed iterative VM/native reset](plans/native-runtime-reset.md)

Completed plans, reviews, backups and captured results belong in Git history.
The remaining technical specifications are retained for their design contracts;
they are not all verified descriptions of shipped behavior. In particular, the
[oracle design](opForge-external-oracle-ab-testing-improvement-spec-v0_1.md),
[native report contract](opForge-native-vm-pipeline-report-v0_1.md) and language-server
design above require product-level comparison before their implementation-status
claims can be relied on. This cleanup classified their documentary purpose; it
did not audit product behavior or authorize work on their proposals.

Use the product guides and actual implementation/tests to establish current
behavior. Retire a specification when its enduring contract is superseded by a
maintained reference. No architecture recovery workstream is activated here.
