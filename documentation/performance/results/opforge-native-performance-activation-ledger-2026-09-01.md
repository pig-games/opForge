# opForge Native Performance Activation Ledger — 2026-09-01

## Activation decision

The user activated the Rust-first VM/native performance plan after the native
generation-one self-build remained CPU-active without completion or artifacts
for about six hours. This activation does not claim terminal self-hosting proof.
Item 40 and Milestone 8 of the native self-hosting plan remain unchecked and
resume only after measured performance work makes the run practicable.

## Repository and isolation

- Remote: `origin` (`https://github.com/pig-games/opForge.git`)
- Fetch result: successful on 2026-09-01
- Fetched `origin/main`: `68cc693c40fd27e30bed11e08974d3263d6cb6f6`
  (`Checkpoint native self-host parity before optimization`)
- Performance branch: `codex/rust-vm-native-performance`
- Performance worktree:
  `/Users/erik/Code/Retro/opForge-wt-rust-vm-native-performance`
- Worktree creation base: exact fetched `origin/main` above
- Reviewed planning branch merged at: `940a9e0d5684c1e5df79462d405231f1501f6f13`
- Planning commits retained: `e2afd2c7` and `86ac4eca`
- Primary checkout was not modified. Its pre-existing unrelated untracked files
  remained:
  `documentation/plans/opforge-riscv32-family-and-vm-package-plan-v0_1.md`, its
  quality sidecar, and
  `documentation/plans/slices/native-porting-slice-m68k-scalar-register-encoding-v2.toml`.
- No push, merge to `main`, or worktree removal is authorized by this item.

## Host and toolchain

- macOS 26.6.2 (25G83), Darwin 25.6.0 arm64, Apple T6041
- Rust: `rustc 1.95.0 (59807616e 2026-04-14)`
- Cargo: `cargo 1.95.0 (f2d3ce0bd 2026-03-21)`
- FS-UAE: 3.1.66 at `/Applications/FS-UAE.app/Contents/MacOS/fs-uae`
- Host sampler: `/usr/bin/xctrace`
- `cargo-flamegraph` and `samply`: not found on `PATH`

## Parked terminal-proof evidence

Checkpoint `68cc693c` records:

- canonical Rust package-only directory build: 29.14 seconds;
- Rust artifacts: 554,144-byte Hunk, 11-byte S-record, and 10,096,156-byte
  listing;
- unchanged native generation-one build: CPU-active under the reviewed
  A4000/68040 maximum-speed JIT profile for two hours and again for six hours;
- no guest `DONE`, guest exit, stdout/stderr diagnostic, or output artifact;
- both runs failed closed on timeout; generation two never started; and
- a later 24-hour-ceiling attempt was stopped by user direction and is not test
  evidence.

The exact terminal two-generation test and the generation-two-first 53-test
bonus wrapper remain deferred. Fresh challenges, explicit zero guest exits, and
byte-exact Hunk/S-record/list equality remain mandatory when they resume.

## Activation static baseline

- Embedded package:
  `native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm`
- Package bytes: 368,278
- Package SHA-256:
  `46a56a5bd436b012c596c65d1f7d85fe6cd8fadbd702362955804415e00c0d41`
- Package capacity: 393,216 bytes; headroom 24,938 bytes
- Source-record capacity: 100,000; packed source-text capacity: 4,194,304 bytes
- Statement capacity/row: 100,000 × 308 bytes
- Label capacity/row: 16,384 × 127 bytes; 256 four-byte hash heads
- Image, mapped-image, and presence arrays: three × 1,048,576 bytes
- Session header/tail: 92 + 12 bytes
- Exact capacity clear:
  `5,194,304 + 30,800,000 + 2,081,792 + 3,145,728 + 104 = 41,221,928`
  bytes
- Checkpoint source evidence: 90,441 Rust-expanded rows, 48,950
  nonblank/noncomment statement rows, 9,134 labels, longest scoped label 107
  bytes, worst of 256 exact-name hash buckets 49 entries

## Drift audit from planning base `94e23e2b` to `68cc693c`

The checkpoint changed native correctness paths, FS-UAE proof infrastructure,
and pass orchestration. It did not add a performance profiler. Relevant finding
dispositions are:

| Finding | Activation disposition |
|---|---|
| F1 byte-scaled source/module I/O | Mechanism persists; source/module/package I/O owners did not change in the audited range. Measure first. |
| F2 capacity session clear | Persists and grew from 38,207,265 to 41,221,928 bytes because label rows are 127 bytes and image arrays are 1 MiB each. |
| F3 copies/bytewise primitives | Embedded 368,278-byte package copy and bytewise generic primitives persist. Measure first. |
| F4 layout/final emission | Partially resolved: checkpoint has output-disabled layout retries and one explicit final pass. Residual 1 MiB presence clears and repeated label/flow/select/encode work remain. Items 18-18b now audit/finish rather than recreate the split. |
| F5 expression compile/evaluate | `runEvalProgram` still compiles before evaluation. Dynamic repeat rate unknown. |
| F6 directive/flow scans | Correctness fixes changed flow paths, but repeated classification/navigation remains unmeasured. |
| F7 fixed records | Statement rows remain 308 bytes; label rows grew to 127 bytes. Representation work remains profile-gated. |
| F8 symbol indexing | Exact hash, full final-component scan, and expression snapshot linear scan persist; line anchors moved. Dynamic candidates/bytes remain unknown. |
| F9 STVM decode/scan | No measured disposition; keep as hypothesis pending counters. |
| F10 native dispatch/ABI | Selection/runtime correctness changed; dispatch/ABI share remains unmeasured. |
| F11 Rust VM attribution | No stable VM/program/opcode/PC profiler was added. |
| F12 multi-hour native compute | Strengthened by committed two-/six-hour fail-closed checkpoint evidence; still unattributed. |

No optimization priority is inferred from changed-line volume. Items 0a-0f must
attribute work and establish numeric thresholds first.

## Commands and gates for activation

- `git fetch origin`
- `git worktree add -b codex/rust-vm-native-performance /Users/erik/Code/Retro/opForge-wt-rust-vm-native-performance origin/main`
- `git merge --no-ff codex/rust-vm-native-performance-plan`
- `python3 scripts/workflow/check_plan_checkboxes.py <plan>`
- `scripts/workflow/run_plan_workflow.sh --check-once ...` (through `bash` when
  the checkout lacks the executable bit)
- `make workflow-gate`
- independent plan-quality and plan-compliance reviews

## Historical next item at activation

Item 0a: add bounded native progress and coarse phase timing. It must load the
native parity, safe-instrumentation, 68000, and FS-UAE rule packs before any
production edit. Instrumentation is observation-only; incomplete snapshots are
localization evidence and never terminal proof.

## Step 08 / Item 0f continuation (2026-09-05)

- Prerequisite integration commit: `495e9a96` (reviewed LSP scheduling amendment); slice code base `8fd49290`, with reviewed plan/numbering amendments retained.
- Branch/worktree: `codex/rust-vm-native-performance`, `/Users/erik/Code/Retro/opForge-wt-rust-vm-native-performance`.
- Active item: Step 08 / Item 0f; bounded observation deliverable only.
- Expected commit: `docs(perf): close bounded attribution with tracked qualification debt`.
- Evidence inventory: `opforge-item0f-observation-inventory-2026-09-05.json`; all raw observations remain Level E, not parity proof.
- Required validation: focused capture/control/corpus/wrapper checks, explicit non-LSP Rust gate, staged workflow gate and independent compliance review.
- Integration dependency: no Step 09 implementation before the Step 08 focused commit. Native failed qualification remains owned by A-close; LSP failure cause remains unproven and repair is deferred to LSP-close.
- Current disposition: required non-LSP Rust and staged workflow gates PASS; see `opforge-item0f-completion-gates-2026-09-05.md`. Observation slice accepted subject to final compliance/commit; no native or LSP fix claimed.

## Step 11 / Item R0 decision (2026-09-05)

Base `9af54933`, same performance branch/worktree. Decision/report and embedded
attribution evidence: `opforge-step11-rust-cost-decision-2026-09-05.md` and
`opforge-step11-rust-attribution-evidence-2026-09-05.json`. GO for one explicit
package early return, with ~100ms discarded fallback construction directly timed
on B01/B10. No shipped speedup yet. Rust probe restored exactly; documentation
only. Required workflow and compliance receipts live in the active plan sidecar.
Expected commit: `docs(perf): select first measured Rust cost`. Step12 begins
after that commit and owns matched gain/correctness and native-transfer decisions.
