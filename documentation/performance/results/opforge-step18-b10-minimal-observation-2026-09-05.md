# opForge Step18 B10 minimal observation — 2026-09-05

This report records one bounded observation. The active worktree `AGENTS.md` remains binding. Artifact provenance: source instruction `Step18 B10 minimal observation`; rule pack `agents/rules/workflow-artifacts.md`; the evidence JSON is generated from authoritative receipts and checked against current source hashes.

The module preflight is classified `preflight Level D exact-case`: buffered mode, platform profile, process and guest exit zero, exact output `[34]`, and native image `fnv1a64:3eafa22681a0d6c3`. It took 85.77191091608256 seconds wall time, with 51.188164167 seconds from guest start to done.

B10 is classified `B10 successful expected controlled abort (1 visit), Level E`. The fresh guest protocol completed without timeout, requested the controlled abort after one statement visit, returned exit status 1, and emitted `ERROR OPC-NCLI020: native pass engine failed`. This is Level E localization evidence: it is neither parity proof nor an unexpected semantic failure.

The exact B10 identity is case `0e8c45ef53ca91471524bbb030720fc4bb4e7601fa30f5b12b0e94d02ec9484c`, corpus `fece2121b487b37e1217b4854b74308366399938e26520e06d124ed63559aed9`, package `46a56a5bd436b012c596c65d1f7d85fe6cd8fadbd702362955804415e00c0d41`, and run ID `3794994541`. The platform diagnostic defines were exactly `OPFORGE_DEBUG_CONTRACTS`, `OPFORGE_PROGRESS_PLATFORM_COUNTERS`, `OPFORGE_PROGRESS_EXPORT_RECORDS`, plus the required abort control define `OPFORGE_PROGRESS_ABORT_VISITS=1`.

The decoded profile reports 23,770 aggregate source bytes consumed, 23,773 source reads, 16 module reads totaling 23,858 bytes, 1,358 logical lines, 7 module candidates, 18 short reads, and zero overflow bits. Raw OFPR is 128 bytes (`9f2afa5a764a54c055d6047af9075fd6b7fa81c19dddf391b5f5560b5ef0133d`); raw OFIO is 528 bytes (`56524d834d93218b46cb99c4e5fdd75b4f0ec2c0893d7a25278b58250c5b8466`).

The observation supports trying buffered source reading in the next bounded comparison. It does not support a speed claim: the 132.86583208292723-second whole attempt includes build and boot, so it is not a 120-second budget violation, and no elapsed-time share can be inferred. No source checkpoint was taken. The full non-LSP Rust gate passed in 252.75 seconds; the focused Rust contract, 11 Python corpus tests and workflow gate also passed. Independent Step18 compliance and Step19 plan-quality review passed after correction of the prose counter description and hashes.

The matching machine-readable record is [opforge-step18-b10-minimal-observation-2026-09-05.json](opforge-step18-b10-minimal-observation-2026-09-05.json).
