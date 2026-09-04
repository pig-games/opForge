# Native Platform Profile — Item 0e Recovery Checkpoint

Date: 2026-09-04

Historical recovery checkpoint. Subsequent range/phase and I/O attribution
work is recorded in [the continuation checkpoint](opforge-native-platform-range-phase-2026-09-04.md),
which supersedes the corresponding open items and the provisional 192-byte shape below.

Status: **in progress, not Item 0e completion**. The active worktree `AGENTS.md`
and the approved focused-sub-item/full-phase-closure gate policy remain binding.
No optimization, self-host measurement, or corpus-attribution claim is made.

## Starting state and recovered invariant

The optimization worktree was at `edcb6466` (completed Item 0d) with an
uncommitted Item 0e draft. The draft contained damaged conditional insertions,
missing routine labels, duplicate unconditional DOS reads/writes, damaged
source-reader stack/control flow, and invalid decoder syntax. The draft had
also enabled platform counters inside the existing Item 0d assembly tests,
but not in the guest runners.

The recovered invariant is that enabling observation does not duplicate DOS
operations or replace source-reader work, and disabling it preserves the prior
release exactly. Read/write observers retain the requested byte count separately,
then restore the actual DOS-returned D1 and CCR with zero stack delta. Misplaced
classification calls now execute inside their intended routines; the source
reader observer no longer separates a flag-setting test from its branch.

The decoder now accepts `--platform-record` at the actual CLI boundary and
validates correlation with OFPR. Terminal records must have cleared current
context. Native sealing is idempotent, so a second finish cannot turn failure
into success. Existing Item 0d tests retain their original flag composition;
new Item 0e tests cover independent and combined profile builds.

## Evidence and limits

- Level B: `native_platform_profile_dos_wrappers_keep_one_operation_and_return_state`
  checks exactly one READ/WRITE call and the saved-return restoration sequence.
  This is a source contract, not an OS simulator or exhaustive DOS-error proof.
- Level C: `native_platform_profile_harness_and_cli_assemble` passes for both
  platform-only and all-counter configurations. This proves composition, not
  arbitrary-source counts or guest execution.
- Level C: all 14 decoder tests pass, including malformed headers/states,
  correlation mismatches, overflow, terminal context, and actual CLI companion
  handling. This does not prove native values.
- Level D: the final focused platform guest passes in 17.48s with a fresh
  completed protocol and explicit exit zero. It covers direct counters,
  read/write error results, EOF, clear/copy saturation, unknown class, and
  failed/successful/repeated sealing. It does not establish full production
  call-site coverage.
- Level D: the bounded platform-enabled production CLI passed exact equality
  to the actual source's live in-memory Rust artifact, fresh completed guest
  protocol, and explicit zero exit. The final rerun passed in 49.09s.
  This is not a full self-host run or negative-diagnostic proof.
- The disabled Hunk is exactly 554,500 bytes, SHA-256
  `17a2b25571799adfb840439ade1304c3d442723a9eaef178158a3d5f1d64d8ab`,
  matching Item 0d's release baseline. Stripping only the new conditional
  platform blocks also reproduces every previously tracked touched native
  file's prior source (the new profile module has no prior version).
- Native format (including the new module explicitly), architecture,
  instrumentation, runtime contract/inventory, and recovery metadata are
  green. The plan artifact bundle and Rust formatter check pass. The unstaged
  native-porting wrapper passes for this checkpoint;
  a staged gate and plan-compliance review are still required before commit.

The early harness assembly failure was a fixture AST-span mismatch caused by
inline `OFFSET+constant(base)` operands. Named offset constants resolved it
without changing parser/package semantics. The host test diagnostic initially
requested an unavailable Debug implementation; field-specific diagnostics
resolved that compile failure. Neither failure was an Amiga runtime failure.

## Instrumentation safety note

- Points: existing DOS, source/module/package/artifact boundaries and bulk helpers.
- Routines: `debug.amigaos.platform_profile` passive observers under
  `OPFORGE_PROGRESS_PLATFORM_COUNTERS`.
- Registers: D0-D7/A0-A6 preserved by observers; GetRecord returns only A0.
- SR/CCR: CCR saved/restored; no privileged SR writes. DOS wrappers restore
  the OS-returned CCR after translating observer arguments.
- Stack delta: zero at observer/wrapper return.
- Shared buffers: only the private fixed record; no request/service/last-error
  buffers or DOS calls in the observer.
- Branch decisions: calls are outside flag-test/branch pairs; disabled native
  instruction streams match the recorded release.
- Stabilization: keep the flag default-off and the schema provisional until
  the remaining Item 0e coverage and proof contract below are finished.

## Remaining Item 0e work — do not promote current counts to attribution

1. Replace sticky class/range selection with audited attribution at every
   relevant operation, including nested source/module returns and every
   artifact path. Audit zero seek usage explicitly rather than treating an
   unpopulated scalar field as proof of per-class coverage.
2. Count clear/copy requested and actually completed work by range and phase,
   not just aggregates or a last-selected range. Audit all relevant arena
   clear/copy sites, including operations currently outside the CLI helpers.
3. Audit logical-line accounting (including EOF without newline) and source
   byte semantics. Distinguish physically reread bytes from unique input size.
4. Finish separately gated platform subgroups and bounded export/correlation
   in the production result path. The current 192-byte OFIO shape is provisional.
5. Add exact short-read, memory-range, DOS/error/CCR and negative diagnostic
   proofs; capture representative final counter values and disabled/counters
   overhead. Complete the staged focused gate and independent plan-compliance
   review, then make the single Item 0e commit before Item 2 starts.

Full Rust and complete native FS-UAE gates remain deferred to Item 0f under
the approved policy. The full self-host is excluded from profiling.

## Documentation impact

The internal ownership inventory, recovery slice metadata, and active plan
checkpoint are updated. Public CLI options, directives, diagnostics, README,
manual, and changelog are unchanged. No release entry is warranted for this
unfinished default-off instrumentation. The documentation-sync audit has stale
paths (`src/assembler/cli.rs` and `documentation/RELEASE_NOTES_v3_1.md`); its
output is not treated as full documentation proof.

Proposed eventual Item 0e commit: `feat(native-perf): count platform io and memory work`.
