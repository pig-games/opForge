# Native Platform Profile — Range/Phase and I/O Attribution

Date: 2026-09-04

Status: Item 0e implementation and focused gates complete; independent
`platform_coverage_review` verdict PASS. Item 2 begins only after the focused
commit. No corpus performance conclusion or self-host profiling is implied.
Active `AGENTS.md` and the focused-gate policy remain binding.

## Implemented invariants

The initial range/phase checkpoint below is historical. The continuation at the
end supersedes its coverage gaps and executable measurements. Final staged
validation and plan-compliance approval passed as recorded below.

The provisional OFIO schema is now version 2, fixed at 528 bytes. It adds five
bulk range rows and nine phase rows. Each has clear/copy calls, requested bytes,
and completed bytes. These are marginal totals, not a cross-product matrix.
The host decoder rejects mismatched totals, completed bytes exceeding requests,
and successful records containing unfinished bulk work. Schema 1 is rejected.

Fixed-size production helpers count requests before their loop and completed
bytes after it returns. They restore the helper's actual output registers and
CCR, including the low-word-only fixed-string count and zero-length behavior.
Completion consumes the selected range. Variable C-string copies count their
actual returned length, including NUL, after completion.

Covered memory boundaries are the CLI copy/clear helpers, engine session clear
and fixed-string helper, package staging copy, package-derived state clear,
and the 1,048,576-byte image-presence clear at pass boundaries. None of these
loops is removed, shortened, consolidated, or otherwise optimized.

Platform-enabled profiling now begins before the initial CLI state clears;
the post-argument-parse boundary does not reset those counters. The observer
argument setup preserves the live DOS base, all temporary registers, and CCR.
The platform-disabled progress-only boundary remains unchanged.

Every current CLI DOS open/read/write/close call explicitly selects its class
immediately before the operation. Enclosing-source reads/closes therefore do
not inherit a nested module scan's classification. Source logical-line visits
include the final nonempty line without a newline and lines which subsequently
fail processing. Source bytes mean actual source-class read bytes, not unique
input size; bootstrap/module rereads have their own classes.

## Proof and limitations

- Level B: source contracts cover the single DOS operation/returned-state
  sequence, all current file-operation class sites, EOF line instrumentation,
  and startup ordering. They do not simulate arbitrary nested guest failures.
- Level C: platform-only and combined-counter harness/CLI builds pass. Fifteen
  decoder tests pass, including aggregate/range/phase consistency and partial
  work rejection. These are not guest performance or whole-program count proof.
- Level D: the actual counter/helper harness passes in 17.55s with fresh guest
  completion and explicit zero exit. It checks request/completion separation,
  range consumption, phase totals, actual buffer contents and boundaries,
  high-word preservation, zero-length null-pointer calls, CCR, saturation,
  and failed/successful repeated sealing.
- Level D: the all-counter real CLI passes exact equality with the actual
  bounded source's live Rust artifact and fresh explicit-zero guest completion.
- Level D: a real output-open failure passes in 47.67s, with a fresh completed
  protocol, explicit nonzero exit, and `OPC-NCLI043`. This is a negative proof,
  not a successful assembly or proof of arbitrary short-write handling.
- Native-porting wrapper, native formatter (238 files plus the explicitly
  checked new profile), architecture, runtime contract/inventory, and Rust
  formatter checks pass. The wrapper was run on the unstaged recovery diff;
  a staged run and plan-compliance review remain required before commit.
- The disabled Hunk remains exactly 554,500 bytes with SHA-256
  `17a2b25571799adfb840439ade1304c3d442723a9eaef178158a3d5f1d64d8ab`.
  This is the unchanged Item 0d release identity, not merely the same size.
- The same-tool all-counter Hunk is 562,372 bytes, SHA-256
  `de13f07899e719eb3a533f9d69a16c6eccd51947db40f2c84a960f9189848f93`:
  2,196 bytes above Item 0d's all-counter Hunk. This measures executable-file
  footprint, not the 528-byte resident record or runtime overhead.

No elapsed test time above is a throughput or physical-A6000 claim. B01-B10
attribution still belongs to Item 0f after the frozen corpus in Item 2.

## Focused failure ledger

| ID | Hypothesis | Evidence for | Evidence against | Status | Next discriminator |
|---|---|---|---|---|---|
| P1 | New helper cannot be resolved by the host harness | Missing `opforge.cli.copy` module | Guest path already included CLI modules | fixed | Added CLI module path; combined/independent builds pass |
| P2 | CCR capture form is unsupported in the combined harness | MOVE.W assembly diagnostic | Established stack capture form assembles and executes | fixed | Actual zero-length CCR oracle passes |
| P3 | Output-open failure changed guest behavior | New test initially failed | Guest completed with nonzero exit and both expected output diagnostics | invalid test artifact | Corrected host assertion: `run.success` denotes zero exit, not negative-proof validity; fresh rerun passes |

## Safety and remaining work

Instrumentation uses the existing debug profile framework and touches only its
private bounded record. Request/completion calls preserve D0-D7/A0-A6 and CCR;
call-site save/restore sequences preserve real helper return values and have
zero net stack delta. Calls remain outside flag-setting test/branch pairs.
No mutable request/service/last-error buffer or per-byte tracing is introduced.

Remaining requirements at the original checkpoint (superseded below):

1. Independently gate I/O and bulk subgroups and prove disabled-group records.
2. Audit remaining inline memory loops outside the covered helper/arena set;
   explicitly resolve the zero-seek field and any coverage exclusions.
3. Complete remaining short-read/short-write/error and representative-value
   evidence, along with counter perturbation calibration.
4. Run the staged focused gate and independent plan-compliance review; make
   the single Item 0e commit before beginning Item 2. Full Rust/native wrapper
   gates remain at Item 0f, not each smaller instrumentation edit.

The matching [record schema](../opforge-native-progress-record-v1.md), slice
metadata, ownership inventory, and active plan checkpoint are synchronized.
Public assembler CLI/directives, normal diagnostics, manual, README, changelog,
and release notes are unchanged; this internal default-off work needs no release
entry. The doc-sync helper's stale legacy paths remain a known audit limitation.
The eventual Item 0e commit title remains
`feat(native-perf): count platform io and memory work`.

## Coverage, subgroup, and real export continuation

The read-only `platform_coverage_review` audit found missing source-text,
label/hash, image-gap/main/mapped, layout-flag, PRG, operand and directive-buffer
boundaries. These are now observed without changing their loops. Request-buffer
copies no longer claim Session ownership. PRG completion uses actual pointer
advancement, not the requested longword count for its existing low-word loop.
Residual small copy/reset families and nonzero sentinel fills are explicitly
listed in the record contract; these counters are not total memory traffic.

I/O and bulk can independently compile to preserving return stubs, with enabled
bits in OFIO. Decoder tests reject nonzero observations in disabled groups and
nonzero unsupported seek data. Call-site envelopes remain in subgroup-disabled
builds: they are not release mode. Five host harness/CLI mode combinations pass
(157.20s), and four fresh guest subgroup oracles pass (69.45s total), including
the positive short-read/partial-write and Module/Package-class cases.

`OPFORGE_PROGRESS_EXPORT_RECORDS` adds a framework-owned, fixed-size export
after terminal sealing. All five companions are captured from the same fresh
guest, decoded together with matching state/run/phase/pass/exit, and removed
from disk before the test returns. Exact live Rust output is still required.
The real output-open failure also exports a strictly decoded incomplete record
with matching nonzero exit; it never becomes successful-assembly evidence.

Raw accepted captures, retained only as measurement reports (never as oracles):

- `opforge-native-platform-first-export-2026-09-04.json`: full observers, run
  3788719789, 66 ticks, exact artifact proof and decoder pass (49.14s harness).
- `opforge-native-platform-disabled-export-2026-09-04.json`: both platform
  observer groups disabled, run 3788728488, 65 ticks; every platform observation
  is zero and marked disabled. This and the exported negative proof passed in
  97.22s total.
- `opforge-native-platform-repeat-export-2026-09-04.json`: full observers, run
  3788734890, 67 ticks, identical operation/byte counts, exact artifact proof and
  decoder pass (49.41s harness). Captured executable size: 563,256 bytes,
  including all counters and the explicit export path.

Source authority: the actual 106-byte, eight-line
`FS_UAE_OPFORGE_NATIVE_CLI_DIRECTIVE_ROUTER_INPUT_TEXT`, invoked as
`{input} --bin {bin} --cpu m6502` with the embedded package. The live Rust output
is 18 bytes. Captures show 107 reads / 106 source bytes (0.991 bytes/read,
including EOF), one 18-byte artifact write, 28 clear calls / 45,092,395 bytes,
and 95 copy calls / 374,377 bytes. The package phase contains 41,224,606 cleared
bytes and 368,284 copied bytes; three presence-map resets clear 3,145,728 bytes.
No byte count alone establishes which instruction consumes elapsed time.

Configuration authority: the existing `opforge-tkpkg-test.fs-uae` template is
A4000 / **68040**, chip 2 MiB, fast 8 MiB, with the runner adding 64 MiB Zorro III
memory and replacing Work with a fresh disposable mount. The executable target
is 68020; these timings are **not** 68020 or physical-A6000 performance evidence.
Ticks are 50 Hz and exclude export. Fresh guest runs do not imply flushed host
filesystem caches. All other counter groups stay enabled in this calibration.
The 65 versus 66–67 tick observations do not justify a statistically reliable
overhead percentage; seven-run corpus calibration and a pinned performance CPU
remain Item 0f work, after Item 2 freezes the corpus.

The expanded default-off executable was independently rebuilt at
`/tmp/opforge-item0e-disabled.XFCRJu/build/opforge_cli`: 554,500 bytes,
SHA-256 `17a2b25571799adfb840439ade1304c3d442723a9eaef178158a3d5f1d64d8ab`,
identical to Item 0d. Profile-only branch widening repairs assembler-reported
short-branch reach failures without changing those release encodings.

Instrumentation safety note: new loop-boundary calls use the same private
platform observer ABI, preserve live registers, avoid flag-setting/branch
pairs, and balance their stack. Export preserves all registers/CCR and reads
only sealed records; its intentionally separate file writes happen after
timing. There is no request/service/last-error buffer use, per-byte trace, DOS
operation removal, or semantic optimization. Stabilization is the Item 0f
bounded capture bridge.

Final receipts: host five-mode harness/CLI build matrix passed again (163.06s);
the final fresh CLI proof with exact source/read/artifact and presence-reset
formulas passed (49.76s), run 3788744489, 65 ticks and identical counters. Thus
the three full-observer timings observed here are 65, 66, 67 ticks, overlapping
the single 65-tick disabled observation; no overhead estimate is established.
Staged native porting gate, plan bundle, inventory, architecture, explicit new
module formatting/safety, Rust formatting and diff checks pass. Independent
reviewer `platform_coverage_review` issued PASS after checking production,
export/decoder/proof scope, running decoder 16/16 and staged diff checks.
Full Rust/native suite and the actual B01-B10 attribution remain Item 0f gates.
