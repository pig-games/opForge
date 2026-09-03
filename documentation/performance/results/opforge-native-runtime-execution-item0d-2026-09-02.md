# Native Runtime Execution Bridge — Item 0d Result

Date: 2026-09-02

Plan item: `0d — Count coarse native VM and service execution`

Scope: observation mechanism and focused parity evidence; no optimization or
self-host timing claim

## Outcome

The default-off `OFVE` bridge now provides fixed, correlated counters for four
CPU-neutral VM/program pairs, eight coarse service categories, selector and
encoder candidates, and marginal pass-one/layout/final/other totals. The
record is sufficient for the bounded Item 0f corpus to rank dispatch and
service work before any VM design is selected. Item 0d deliberately does not
collect a high-cardinality opcode/PC histogram or claim that VM dispatch is hot.

The focused deterministic guest proves exact VM/program, nested VM/program
restore, nested-service restore, candidate, phase, saturation, unknown-ID,
incomplete, and complete-terminal behavior, including EXVM and ExprVM
invocation totals. The representative production CLI case proves that enabling
all current Phase 0 counters preserves the live Rust artifact, diagnostics
contract, and explicit guest exit. Actual B01-B10 counts and slopes remain Item
0f evidence; the full self-host is not used as a profiling workload.

## Fixed storage and identities

- `OFVE` record: 192 bytes, big-endian, schema 1.
- Private state: two four-byte OFPR pointers, two two-byte nesting depths, one
  four-pair VM/program stack, and one four-word service stack (36 bytes).
- VM IDs: TKVM, PRVM, EXVM, ExprVM.
- Program IDs: tokenizer, parser, expression frontend, expression evaluator.
- Service IDs: expression, selection, encoding, operand, state, branch, fixup,
  value.
- Every counter saturates at `0xffffffff`; any overflow or unknown ID makes
  `--require-complete` reject the record as proof.
- IDs are provisional and CPU-neutral. Item 6b replaces them with Item 3's
  shared stable identities.

## Same-tool Hunk evidence

All three variants were assembled concurrently with the same fetched
`target/debug/opforge` binary and the same native source composition.

| Variant | Bytes | SHA-256 | Interpretation |
|---|---:|---|---|
| Unprofiled | 554,500 | `17a2b25571799adfb840439ade1304c3d442723a9eaef178158a3d5f1d64d8ab` | Exact recorded release identity |
| Item 0c detail, Item 0d disabled | 558,432 | `ae423af0513ca7a48743aff2830d658b23d8c2979c336fcc6aceee50039d110f` | Exact recorded Item 0c identity |
| Item 0c detail plus Item 0d | 560,176 | `043189729474fcba61036a05a845b018c9d86d5e3e19d069195faaa551e9c028` | New combined diagnostic build |

Item 0d adds 1,744 bytes to the detail-enabled Item 0c Hunk (0.312%) and 5,676
bytes to the unprofiled Hunk (1.024%). Exact prior digests prove the disabled
gate emits no Item 0d code or storage and that the two necessary shared-return
paths retain their original release instructions. The nested VM/program stack
accounts for the 132-byte increase over the earlier service-only nesting
measurement.

## Fresh Level D confirmations

FS-UAE 3.1.66 used the existing configured 68020 environment and fail-closed
fresh-challenge protocol.

| Case | Result | Elapsed test time | Proof boundary |
|---|---|---:|---|
| Focused `OFVE` deterministic harness | PASS | 17.23s | Actual profile routines; exact counter/nested-VM/nested-service/overflow/terminal oracle |
| Full CLI, all Phase 0 counters including `OFVE` | PASS | 48.79s | Fresh completion, explicit zero guest exit, exact live-Rust artifact |
| Adjacent progress-only control | PASS | 47.88s | Same bounded input and exact live-Rust artifact |

The single-run +0.91s/+1.90% difference is noise. It is neither an overhead
estimate nor a speed claim, and it says nothing about the physical A6000.

The first focused guest attempt completed the protocol with exit 34. That was a
harness-only pointer-lifetime error: after checking `OFVE`, A0 still named the
companion record when the established OFPR saturation oracle resumed. The
corrected invariant is that every companion-record oracle restores A0 to the
authoritative OFPR pointer before continuing. The unchanged production profile
had already composed successfully; the identical rerun then passed.

## Documentation applicability

The new behavior is internal, default-off diagnostic instrumentation. It does
not change public CLI syntax, normal output, package semantics, user-visible
diagnostics, or release behavior, so README, user manual, changelog, migration,
and release-note edits are not applicable. The internal record-format and
instrumentation documents and the audited runtime-boundary inventory are the
appropriate synchronization targets.

Suggested release-note summary if this diagnostic capability is ever exposed:
"Added an opt-in bounded native VM/service execution record for performance
attribution; normal assembly behavior and release output are unchanged."
