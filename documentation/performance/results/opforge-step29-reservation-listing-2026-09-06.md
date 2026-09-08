# Step 29 — checked reservation extent and listing retention

Status: Step29 qualified; all required final gates and independent compliance
pass. Phase A remains open.

Base: `914ad23a15871a4bb891d541f2359a92830d9a3a`. The active plan and `AGENTS.md`
remain binding. Exact commands, source hashes, failed attempts and all terminal
receipt hashes are retained in the adjacent JSON ledger.

## Implemented invariant

Rust `line.rs::res_directive_ast` and `update_addresses` own the reference:
exactly two operands in BSS, byte/selected-CPU word/long or nonnegative numeric
unit, positive unit, full-u32 count, checked multiplication, inclusive span and
next-PC bounds. Native consumes Step28's typed i64 result with explicit status,
capacity, width and high-word checks. It uses the same extent for PC advancement
and listing retention, avoiding formatted-value reparsing.

The engine stores extent and start address in existing statement fields. An
internal high-bit flag preserves the original NONE or GENERIC parser kind;
public reads mask only that flag. Ordinary emitted-byte access returns zero for
reservations, including zero extent. Pass reset clears the flag and retained
extent/address before flow traversal. No new per-statement table is allocated.
Listing renders the exact start and plus-prefixed four/six/eight-digit extent.

## Qualification and limits

- Level A/B: nine focused host tests and six measured capacity tests pass.
- Level D: nine complete mixed code/BSS listing-plus-BIN cases pass in
  `reservation-native-second`; all nine negative cases freshly complete with
  nonzero exit and their required diagnostic in the same receipt. The aggregate
  exits 101 because its two additional BSS-only cases fail; it is not a passing
  matrix receipt.
- Level D: the two final full-u32 cases pass in `reservation-u32-zero-first`
  (126.9562s): CODE and BSS at origin zero, unchanged compound counts
  `$80000000+0` and `($ffffffff+1)-1`, exact whole Rust listings and one-byte `AA`
  BINs. This proves accepted full-u32 counts and no BSS image contribution.
- Level D: `reservation-single-second` separately passes a complete original
  byte/four-count fixture, binding linkage and repeated pass behavior.
- Unchanged B09 checks all eleven artifacts: ten exact, listing only differs.
  Its first mismatch moves from reservation source row19 at byte1260 to the
  symbol footer at byte2600 (native2794 bytes, Rust2799). Source rows now match.
  B09 remains failed; its 98.3496s failed-run duration is not a benchmark gain.
- Native assembly, formatter, architecture, inventory, staged-native and
  workflow checks passed. Final non-LSP gate `reservation-quality-fifth` passes
  in 268.1005s; staged-native-third, workflow-third and plan-wrapper-third pass.
  Independent `step28_scalar_review` compliance passes; its gate conditions are
  satisfied and mechanical completion edits are approved.

The final source budget is 1,723,532 loadable bytes; processed graph 94,465 rows
and 3,576,807 bytes; 423 imports; 6,509 public declarations and 127,929 public-name
bytes. Hard bounds are unchanged. Step29 adds eight fixed BSS bytes and no
per-statement storage. Executable growth remains unmeasured.

## Retained failures and next boundaries

| Hypothesis / boundary | Evidence and disposition |
|---|---|
| Grouped unit keywords act like bare keywords | Falsified by actual Rust directive oracle; normalization reverted and negative case retained. |
| External calls return CCR from status | Falsified by callee epilogues and first native batch; explicit long/byte D0 tests now qualified. |
| Negative commands reached `.res` | First commands lacked output selection; corrected commands pass all nine named native rejections. |
| Reservations always arrive as GENERIC | Source fast path retains NONE; high-bit flag preserves both kinds and is qualified by complete native cases. |
| BSS-only empty BIN/listing | Unchanged `run.asm::checkImage` rejects zero-image non-SREC output with OPC-NCLI009. Two failed complete cases remain recorded and require a separate Phase A repair. |
| BSS origin cannot establish image base | Failed mixed CODE-at-$8000 cases violate the existing overlapping-origin contract: BSS `.org 0` creates an artificial $8000 gap; listing dumps $8001 bytes beyond its 24,000-byte buffer and fails OPC-NCLI044/023. Source analysis establishes this cause; BIN length was not directly verified in the failed guest proof. Separate Phase A repair required. |
| B09 symbol footer | Duplicate qualification, truncation and insertion order remain after the reservation correction. Other footer/image-presence and prior Phase A debt remain open. |

Independent review approved retaining the unchanged nine positive and nine
negative per-case proofs while replacing only the full-u32 fixture placement.
Failed empty-image and nonzero-placement runs are not relabeled as successes.
Exact failed sources and commands remain in the JSON ledger.

## Performance contribution

Step runtime gain: unmeasured. Typed-result consumption avoids reparsing VALUE
text; existing storage avoids a new statement table. A bounded kind-table scan
per pass is added, with unmeasured runtime cost. Total Phase A runtime gain is
also unmeasured without matched initial/current measurements. Separately
verified cumulative source DOS reads remain 1,608 to 8: 1,600 avoided,
99.5025% fewer calls. Earlier validation-time reduction is 85.27%; neither
quantity is a cumulative product runtime percentage. LSP remains last.
