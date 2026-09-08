# Step 30 — BSS image-origin and alignment suppression

Status: Step 30 qualified, pending focused commit. All six native cases, current
capacities, staged-native/workflow/plan gates and the single full non-LSP Rust
gate pass. Independent `step28_scalar_review` final compliance: PASS. Phase A
remains open.

The driver excludes active BSS `.org` from image-base discovery. Layout discards
BSS statement image writes before mapped/main selection, including `.align`
padding. Unplaced sections also reuse their retained starts during layout and final
emission; a raised image origin cannot change their sizes or labels between
passes. No runtime storage or capacity limit changes.

Rust authority is `line.rs::update_addresses`,
`asmline_directives_data.rs::align_directive_ast`, and
`image.rs::ImageStore::output_range`. Complete native cases carry their exact
source, command, shipped package and live in-memory Rust BIN oracle. The shared
runner requires a fresh challenge, exact guest start/completion, explicit zero
exit and exact artifact bytes; every case's temporary artifacts are removed.

## Proof and failure history

- Level A: complete Rust BIN oracles cover unplaced/placed BSS below CODE at
  $8000, full-U32 reservations, explicit nonzero BSS alignment, ordinary
  forward/backward origins, and the existing mapped logical-section fixture.
  A focused Rust semantic check proves `.res byte,1` then `.align 4` requests
  three padding bytes without emitting them.
- Level B source audit: native `emitAlign` obtains D3 from
  `readAlignPadForStatement`, passes it to `appendRepeatedByte`, and emits under
  the layout-selected image route. The BSS route returns discard; all non-BSS
  mapped/main/logical/unplaced decisions retain their prior ordering. This is
  ownership evidence, not a substitute for native execution.
- First representative: fresh native exit 1 after pass1-ok and two pass2-ok
  events, with OPC-NCLI020; no BIN proof. The first layout retry began at zero,
  then finalized the image origin to $8000. Final emission inherited $8000 as
  the unplaced BSS start; `.org 0` and reservation 4 therefore produced a
  wrapped section size and failed convergence. Independent source review
  confirmed this chain. Reseeding unplaced sections from retained section state
  corrects the first divergent state, without weakening convergence checks.
- Second representative: the same complete source passes fresh Level D proof
  with the exact one-byte BIN `AA`. The first five-case matrix passed all four
  BSS/origin cases but failed the added mapped-logical control. The identical
  mapped case also failed against exact prior production commit `a893636e`,
  with the same explicit exit and convergence trace; the protected baseline
  run restored and hash-verified both candidate native files.
- The mapped failure has the same independent-section start cause. The plan
  was independently amended to reuse retained starts for every unplaced
  section, preserving placed bases and mapped packing/reachability. Final-code
  mapped control now passes exact live-Rust BIN `20 00 00 60 60`; the final
  remaining five BSS/origin confirmations also pass on identical final native
  source. The mapped case was excluded only from the redundant rerun. Neither
  failed aggregate nor baseline failure counts as positive parity evidence.
- Initial capacity failures record stale expected snapshot values; subsequent
  final current-tree capacity checks pass all six contracts with the refreshed
  measured values and unchanged limits.

Exact sources, hashes, commands, failed and successful receipts are retained in
`opforge-step30-bss-image-origin-2026-09-08.json`. Step29's mixed full-U32
BIN/listing failures remain in its original results; this BIN-only slice does
not reclassify them as passing listing proofs.

## Performance contribution

The code removes unnecessary BSS image work. For CODE at $8000 and BSS at zero,
source analysis predicts 32,768 artificial gap bytes avoided before the one code
byte. The final one-byte BIN is directly verified; the previous BIN length was
not captured, so this is not a measured before/after artifact-size ratio or
runtime speedup. BSS alignment requests are discarded instead of materialized.

The candidate adds zero runtime storage. The source budget is 1,724,236 loadable
bytes, 94,489 processed rows / 3,577,511 bytes, 423 imports, 6,511 public
source declarations / 127,950 name bytes. Hard bounds are unchanged. Executable
byte growth and step runtime gain are unmeasured.

Cumulative source DOS reads remain 1,608 to 8: 1,600 avoided, or 99.5025% fewer.
There is no matched initial-to-current Phase A runtime series, so total runtime
gain remains unmeasured. Gate and emulator-harness durations measure validation
cost and must not be presented as product performance.

## Remaining scope

Listing ORG rows, generated image and symbol footers, empty-image CLI output,
and all other Phase A corpus/nonterminal debt remain open. A separate existing
section-start discrepancy remains: native unplaced sections first encountered under
a nonzero parent PC retain that start, whereas Rust's section starts at zero.
These start-zero proofs do not close that discrepancy; it is now explicitly
retained at A-close. LSP remains final Step 40. No Phase B or Phase A closure is
claimed.
