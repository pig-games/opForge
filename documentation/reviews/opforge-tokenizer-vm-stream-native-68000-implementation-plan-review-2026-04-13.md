# Review Report

## Scope

Review of `documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md` against its cited authority documents and active worktree review rules. Scope includes internal consistency, plan completeness, and alignment with `documentation/opforge-assembler-vm-path-guide-v0_1.md` and `documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md`.

## Findings

### RVW-2026-04-13-001

- Severity: high
- File: [documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md](documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md#L56-L57), [documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md](documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md#L128), [documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md](documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md#L155), [documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md](documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md#L143-L151), [documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md](documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md#L279-L280)
- Issue: The plan hard-commits the first native tokenizer path to a line-buffer-backed `tokvm_run_68000` contract, then later defines the AmigaOS harness as reading one whole input file into the source buffer and invoking that entry point once. The cited harness spec still leaves one-file versus line-by-line processing open, so Work items 3 and 4 currently assume different execution models at the same ABI boundary.
- Why it matters: This is the central compatibility seam for the feature. An implementation can satisfy the current text either by feeding a whole file into a line-oriented contract or by silently handling only one line, and both outcomes would diverge from the current VM line model in materially different ways.
- Fix direction (one direction only; resolve competing options before finalizing): Amend the plan so the first native and harness slices are explicitly single-line only, with deterministic rejection of newline-containing input, and defer multi-line file processing to a later spec or plan item.

### RVW-2026-04-13-002

- Severity: medium
- File: [documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md](documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md#L128-L133), [documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md](documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md#L148-L151), [documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md](documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md#L159-L168), [documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md](documentation/opForge-amigaos-cli-fileio-tokenizer-vm-harness-spec-v0_1.md#L264)
- Issue: Work item 3 says the native 68000 slice will preserve a “defined token record/status contract” for one line buffer, but the cited authorities only define the call registers for `tokvm_run_68000` and the textual `OPFORGE-TOKVM 1` report shape. They do not define the in-memory token record layout or status-buffer ABI that the native interpreter and its host-side smoke test are supposed to preserve.
- Why it matters: Without an authoritative buffer contract, Work item 3 leaves the first native interpreter free to invent a private token/status layout. That makes the host-side validation target under-specified and weakens the plan as an execution input.
- Fix direction (one direction only; resolve competing options before finalizing): Add or cite the authoritative in-memory token record and status-buffer ABI before Work item 3 becomes active, then point the validation step at that contract explicitly.

### RVW-2026-04-13-003

- Severity: medium
- File: [documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md](documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md#L54), [documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md](documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md#L76-L82), [documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md](documentation/plans/opforge-tokenizer-vm-stream-native-68000-implementation-plan-v0_1.md#L97-L122), [documentation/opforge-assembler-vm-path-guide-v0_1.md](documentation/opforge-assembler-vm-path-guide-v0_1.md#L332-L333)
- Issue: The upfront planning decision says the first production slice removes host tokenizer delegation before any native 68000 assembly work begins, but Work item 1 still allows temporary fallback behavior and Work item 2 is the slice that actually removes `ScanCoreToken` and `DelegateCore` from the default tokenizer programs.
- Why it matters: That contradiction obscures the real milestone boundary. It makes Work item 1 easy to over-scope or misreport, because the plan’s summary claims delegation removal earlier than its detailed work-item sequence actually delivers it.
- Fix direction (one direction only; resolve competing options before finalizing): Rewrite the upfront decision so host-tokenizer delegation removal is explicitly the outcome of Work item 2, while Work item 1 is limited to stream-contract plumbing and compatibility scaffolding.

## Testing Gaps

- Work item 4 promises deterministic handling for usage, input-too-large, VM-failure, output-write, and file-open failures, but its validation list does not schedule the spec-required negative tests for missing arguments, input too large, partial-write failure, or report emission on nonzero exits when an output handle exists.
- Work item 4 does not reserve an explicit validation for quoted-path rejection when quoting support is intentionally out of scope, even though the cited harness spec treats that as a deterministic first-slice behavior.
- Work item 2 says unsupported tokenizer shapes must fail deterministically once `ScanCoreToken` is removed, but the listed validation emphasizes supported token classes and budget checks rather than one focused unsupported-shape failure-path test.

## Residual Risks

- The cited harness spec still leaves initial source-buffer, token-buffer, and lexeme-buffer sizing open, so even a corrected plan can still produce first-cut capacity choices that later need to be standardized.
- FS-UAE validation remains opt-in, so AmigaDOS-specific startup and file-I/O edge behavior will still be only partially exercised by the default host-side validation path.
- Quoted-path support remains unresolved in the harness authority, so AmigaDOS path compatibility for names with spaces remains a defined follow-on risk even if the first slice lands cleanly.

## Brief Summary

The plan is close to a usable tokenizer-only implementation sequence, but it still has one major execution-boundary problem and two material specification gaps. The native entry point and the AmigaOS harness do not yet share one unambiguous single-line versus whole-file model, Work item 3 assumes an in-memory token/status ABI that the cited authorities do not define, and the plan’s summary claims host-delegation removal one slice earlier than the detailed work items actually do.