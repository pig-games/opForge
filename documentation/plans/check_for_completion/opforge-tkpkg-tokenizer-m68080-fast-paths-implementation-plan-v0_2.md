# opForge tkpkg Tokenizer Motorola 68080 Fast-Paths Implementation Plan v0.2

## Metadata

- Source: `User instruction on 2026-04-25: write a plan to implement the 68080-conditional tokenizer perf opportunities identified in the prior review`; the prior in-chat review of `examples/motorola68000/amigaos/tkpkg/tkpkg_tokenizer_vm.asm` and `examples/motorola68000/amigaos/tkpkg/tokvm_tokenizer_vm.asm`; the v0.1 plan-quality multi-agent gate FAIL adjudication on 2026-04-25 (findings F1, F2 blocking; Claude F3/F4 and Gemini F3 non-blocking, all folded into v0.2); the existing CPU-pipeline `.ifdef` convention in `examples/motorola68000/amigaos/tkpkg/tkpkg_debug_cli.asm` (`TKPKG_DEBUG_PIPELINE_M68080`); the active worktree `AGENTS.md`; the package-backed native tokenizer runtime authority in `documentation/plans/opforge-m68000-package-backed-native-tokenizer-runtime-implementation-plan-v0_1.md`; the staged tokenizer corpus authority in `crates/opforge-vm/src/runtime_tests.rs`; and the FS-UAE family parity authority in `crates/opforge-asm/src/fs_uae_smoke.rs`.
- Mode: `implementation`
- Owner: Codex

## Objective

Land an opt-in, conditionally-assembled Motorola 68080 fast path for the
tkpkg tokenizer hot loops, gated behind a new `TKPKG_CPU_M68080` define, while
keeping the existing 68000/68010/68020/68030/68040 baseline byte-exact and
fully tested.

The first slice is intentionally narrow: a single 68080-gated character-class
fast path used by the scanner identifier and number loops in
`examples/motorola68000/amigaos/tkpkg/tokvm_tokenizer_vm.asm`. Render-side
append helpers, AMMX vectorization, jump-table dispatch replacement, and any
other items from the review are explicitly deferred to later plans.

## Constraints

- The active worktree `AGENTS.md` workflow and execution rules remain binding at
  all times during plan execution.
- Scope is limited to opt-in 68080 fast paths inside the tkpkg tokenizer files
  under `examples/motorola68000/amigaos/tkpkg/`. This plan must not touch
  parser, encoder, package loader, pipeline selection, debug CLI dispatch
  semantics, or any non-tokenizer module beyond the minimum required to wire
  the new define.
- The default build (no `TKPKG_CPU_M68080` defined) must remain produced-output-
  equivalent and behavior-equivalent to the current baseline. "Produced-output-
  equivalent" means the assembled byte image emitted by `cargo run -p asm` (or
  the equivalent harness assemble call already used by `fs_uae_smoke.rs`) for
  the tkpkg tokenizer files is byte-identical against an Item-1 reference build
  for any input where `TKPKG_CPU_M68080` is not defined. This is not a `git
  diff` check on source files.
- Every 68080-gated fragment must be wrapped in `.ifdef TKPKG_CPU_M68080` /
  `.else` / `.endif` so that the legacy path is preserved verbatim. No
  unconditional rewrite of an existing helper is allowed in this plan.
- The 68080 fast path must not change tokenizer observable behavior. Token
  emission order, token kinds, token spans, lexeme bytes, diagnostic codes,
  diagnostic budgets, and `last_error` strings must remain identical between
  the baseline and 68080 builds for every input the staged Rust corpus and
  the FS-UAE family corpus exercise.
- The 68080 fast path must not require any new opcode, AMMX, MOVE16, BYTEREV,
  or other Apollo-only instruction in this first slice. The first slice is
  restricted to a 68020-legal table-lookup variant of the character-class
  predicate that is merely *enabled* by the 68080 define, so we land the
  scaffolding and parity proof before introducing CPU-specific instructions
  in a later plan.
- The new `TKPKG_CPU_M68080` define is independent from
  `TKPKG_DEBUG_PIPELINE_M68080`. The pipeline-ID string in `tkpkg_debug_cli.asm`
  must keep its current behavior. The two defines may be set together by a
  caller, but neither implies the other.
- The mechanism for injecting `TKPKG_CPU_M68080` into both the
  module-surface assemble path (`crates/opforge-asm/src/tests.rs`) and the
  FS-UAE smoke harness assemble path (`crates/opforge-asm/src/fs_uae_smoke.rs`)
  must be made explicit in Item 1. No work item later than Item 1 may rely on
  an undocumented or implicit injection path.
- This plan must not widen into:
  - Render/append helpers (`tkpkg_tokenizer_vm_append_bytes_v1`,
    `append_quoted_v1`, `append_byte_list_v1`, `append_u32_v1`,
    `append_upper_quoted_v1`, `append_char_v1`).
  - Symbol-dispatch ladder replacement in `tokvmScanSymbolToken`.
  - `tokvmCommitPendingToken` wide-store changes.
  - `tokvm_run_68000` step-counter or invariant-hoisting changes.
  - Endian helpers `tkpkg_tokenizer_vm_read_u16_le_v1` /
    `tkpkg_tokenizer_vm_read_u32_le_v1`.
  - Any AMMX-only or MOVE16-only fragment.
  These are explicitly deferred to follow-up plans (see Deferred Work).
- This plan must not change the FS-UAE smoke harness's CPU selection surface
  or default CPU. The 68080 build remains opt-in via Rust-side test gates
  only, with no change to default CI behavior.
- This plan must not become active until `plan-quality-reviewer` returns
  `PASS`.

## Planning Decisions Captured Up Front

- The first 68080-gated slice targets the highest-frequency hot path identified
  in the review: the per-byte character-class predicates
  (`tokvmIsIdentifierContinue`, `tokvmIsNumberBody`) called from
  `tokvmScanIdentifierLoop` and `tokvmScanNumberLoop`.
- The 68080 fragment in this slice is limited to a 256-byte class bitmap
  lookup variant that is legal on every 68020+ host but is only *selected*
  when `TKPKG_CPU_M68080` is defined. This proves the conditional-assembly
  scaffolding and parity gates without taking on AMMX/MOVE16 risk in the same
  commit.
- Class-bitmap tables live in the existing tokenizer data section so no new
  buffer/state files are created.
- A single new opt-in Rust test gate, `TKPKG_FS_UAE_M68080_FAST_PATHS=1`,
  drives a 68080-build copy of the existing FS-UAE family parity test against
  the staged top-level Motorola 68000 corpus. Default `cargo test` behavior is
  unchanged.
- The 68080-build parity test is introduced *before* any behavioral change,
  initially asserting baseline-vs-baseline parity (which is trivially true
  because Item 1's `.ifdef` arm is byte-identical to its `.else` arm). It
  then continues to gate the actual fast-path swap in the final item.
- The bitmap-vs-legacy Rust comparison test mechanically derives expected
  bitmaps by iterating bytes `0..=255` through a Rust port of the legacy
  `CMPI.B` chain (translated directly from the legacy `.else` assembly, not
  from any comment block), to defend against transcription errors in the
  human-authored range list.
- AMMX, MOVE16, jump-table dispatch, render-side wide copies, and `DIVU.L`
  decimal emit are explicitly staged for follow-up plans (see Deferred Work).

## Module Touch Set

- `examples/motorola68000/amigaos/tkpkg/tokvm_tokenizer_vm.asm`: add
  `TKPKG_CPU_M68080`-gated bitmap-lookup variants of
  `tokvmIsIdentifierContinue` and `tokvmIsNumberBody`, plus the small
  read-only class bitmap data they consume. The legacy implementations
  remain in the `.else` arm verbatim.
- `examples/motorola68000/amigaos/tkpkg/tkpkg_tokenizer_vm.asm`: no behavior
  change. May only receive a comment cross-referencing the new define if that
  is required to keep the module surface tests passing.
- `crates/opforge-asm/src/fs_uae_smoke.rs`: add one opt-in 68080 fast-paths
  parity test gated by env var `TKPKG_FS_UAE_M68080_FAST_PATHS=1`. Existing
  tests untouched. Plus the explicit define-injection plumbing identified in
  Item 1.
- `crates/opforge-asm/src/tests.rs`: add the `TKPKG_CPU_M68080`-defined
  module-surface assemble assertions (Item 1) and the bitmap-vs-legacy table
  comparison test (Item 3). Existing tests untouched.

No other crate, no other example, and no other doc is in scope.

## Work Items

- [ ] Item 1 — Scaffold `TKPKG_CPU_M68080` define, identity arms, and
  define-injection plumbing
  - Source requirement or finding IDs:
    - prior in-chat review item #1 ("character-class predicates"),
      scaffolding precondition.
    - v0.1 plan-quality finding F2 (Claude+Gemini): explicit
      define-injection mechanism and an Item-1 gate that exercises the
      `.ifdef` arm.
  - Expected files:
    - `examples/motorola68000/amigaos/tkpkg/tokvm_tokenizer_vm.asm`
    - `crates/opforge-asm/src/tests.rs`
    - `crates/opforge-asm/src/fs_uae_smoke.rs` (define-injection plumbing
      only; no new test added in this item)
    - `crates/opforge-asm/tests/goldens/tokvm_tokenizer_vm.m68080_undefined_baseline.bin` (new checked-in golden artifact)
  - Action:
    - Wrap the existing bodies of `tokvmIsIdentifierContinue` and
      `tokvmIsNumberBody` in `.ifdef TKPKG_CPU_M68080` / `.else` / `.endif`,
      where the `.ifdef` arm is *byte-identical* to the `.else` arm in this
      first item. This isolates the conditional-assembly scaffolding change
      from any behavioral change.
    - Identify the exact assemble call site in `fs_uae_smoke.rs` and the
      exact assemble call site in `tests.rs` where preprocessor defines are
      passed today, and document inline in this plan and in code comments at
      both call sites the chosen injection mechanism (e.g., the `defines`
      slice, a `-D` argv passthrough, or a `.define` source prelude). Wire a
      thin helper that lets opt-in tests pass `TKPKG_CPU_M68080` without
      changing default-build behavior.
    - Add `motorola68080_tokvm_interpreter_module_surface_assembles_with_cpu_define`
      to `crates/opforge-asm/src/tests.rs`. This test must, with the new
      injection helper, assemble `tokvm_tokenizer_vm.asm` *twice* (once
      without and once with `TKPKG_CPU_M68080` defined) and assert that the
      produced byte image is identical between the two assemblies for this
      item only. This is the "produced-output-equivalent" gate referenced in
      Constraints.
    - Capture the undefined-build produced byte image of
      `tokvm_tokenizer_vm.asm` as a checked-in golden artifact at
      `crates/opforge-asm/tests/goldens/tokvm_tokenizer_vm.m68080_undefined_baseline.bin`.
      The same Item-1 test must additionally assert that the freshly
      assembled undefined build is byte-equal to that committed golden.
      This golden is the canonical reference image used by Item 3's
      undefined-build equivalence assertion. Updates to this golden are
      forbidden by this plan and require a separate plan that intentionally
      changes the legacy `.else` arm.
  - Full quality gates:
    - `cargo fmt --all`
    - `cargo clippy --all-targets --all-features -- -D warnings`
    - `cargo audit`
    - `cargo test -p vm motorola68000_tokenizer_vm_staged_corpus_matches_host_for_example_lines -- --nocapture`
    - `cargo test -p asm motorola68020_tkpkg_tokenizer_parity_module_surface_locks_number_and_operator_debug_rendering -- --nocapture`
    - `cargo test -p asm motorola68020_tokvm_interpreter_module_surface_locks_hash_symbol_scan -- --nocapture`
    - `cargo test -p asm motorola68080_tokvm_interpreter_module_surface_assembles_with_cpu_define -- --nocapture`
    - Existing FS-UAE family parity test must still pass when its env gate is
      enabled (manual confirmation note in the commit body is acceptable).
  - Plan-compliance review evidence:
    - `agents/plan-compliance-reviewer.agent.md` invocation log attached to
      the commit message tail.
  - Commit outcome: one commit titled
    `Added TKPKG_CPU_M68080 conditional scaffolding to tokvm character-class predicates.`
  - Definition of done:
    - Both scanner predicates compile and execute identically with and
      without `TKPKG_CPU_M68080` defined, evidenced by the new
      `motorola68080_tokvm_interpreter_module_surface_assembles_with_cpu_define`
      test passing.
    - All listed quality gates pass on the default (undefined) build.
    - The chosen `TKPKG_CPU_M68080` injection mechanism is explicitly named
      in the commit body and in inline code comments at both call sites.
    - No tokenizer behavior change observable from the staged Rust corpus.

- [ ] Item 2 — Add opt-in 68080 FS-UAE family parity test (no behavioral
  change yet)
  - Source requirement or finding IDs:
    - prior in-chat review item #1, validation gate.
    - v0.1 plan-quality finding F1 (unanimous): test infrastructure must
      land before the behavioral fast path that depends on it.
  - Expected files:
    - `crates/opforge-asm/src/fs_uae_smoke.rs`
    - `crates/opforge-asm/src/tests.rs` (test surface registration only)
  - Action:
    - Add `motorola68080_tkpkg_tokenizer_fs_uae_family_parity` as a
      `#[ignore]` test that:
      - is enabled only when `TKPKG_FS_UAE_M68080_FAST_PATHS=1`,
      - assembles the tkpkg tokenizer with `TKPKG_CPU_M68080` defined plus
        the existing `TKPKG_DEBUG_PIPELINE_M68020` baseline (so the only
        differing factor is the new define), via the Item-1 injection
        helper,
      - runs the existing top-level Motorola 68000 example corpus through
        FS-UAE,
      - compares debug-row output byte-exact against the existing baseline
        corpus.
    - Reuse the existing FS-UAE harness; do not introduce a new harness.
    - At the end of Item 2, the `TKPKG_CPU_M68080` arm of the predicates is
      still byte-identical to the `.else` arm (Item 1 left it that way),
      so this test must trivially PASS in this commit. That is the
      intended state: the test infrastructure is locked in *before* the
      behavioral change in Item 3 lands.
  - Full quality gates: same set as Item 1, plus:
    - Opt-in: `TKPKG_FS_UAE_M68080_FAST_PATHS=1 cargo test -p asm motorola68080_tkpkg_tokenizer_fs_uae_family_parity -- --nocapture --ignored`
      must pass on the developer host.
  - Plan-compliance review evidence: same agent invocation requirement as
    Item 1, plus an explicit text note in the commit body confirming that
    the opt-in 68080 FS-UAE test was run and passed on the developer host
    against the byte-identical scaffolding.
  - Commit outcome: one commit titled
    `Added opt-in 68080 FS-UAE family parity test for tkpkg tokenizer fast paths.`
  - Definition of done:
    - Default `cargo test` behavior is unchanged.
    - With `TKPKG_FS_UAE_M68080_FAST_PATHS=1`, the test is selected and
      passes against the byte-identical scaffolding.
    - The test fails loudly (not silently) if the 68080 build diverges from
      the baseline build for any corpus line.

- [ ] Item 3 — Add 68080 character-class bitmap fast path
  - Source requirement or finding IDs:
    - prior in-chat review item #1 ("character-class predicates").
    - v0.1 plan-quality finding F4 (Claude): bitmap test must mechanically
      port the legacy `CMPI.B` chain.
    - v0.1 plan-quality finding F3-Gemini: promote the bitmap-vs-legacy
      test to an explicit Action with file path and test name.
  - Expected files:
    - `examples/motorola68000/amigaos/tkpkg/tokvm_tokenizer_vm.asm`
    - `crates/opforge-asm/src/tests.rs`
  - Action:
    - Replace the body of the `.ifdef TKPKG_CPU_M68080` arm of
      `tokvmIsIdentifierContinue` and `tokvmIsNumberBody` with a 256-byte
      read-only class bitmap lookup that returns the same boolean result the
      legacy chain returns for every input byte 0..255.
    - Add the read-only class bitmaps `tokvmIdentContinueClassMask` and
      `tokvmNumberBodyClassMask` (256 bytes each) to the existing data
      section.
    - The bitmap values are derived once, by hand, from the existing legacy
      `CMPI.B` chains; the legacy chains remain authoritative in the `.else`
      arm.
    - Add a comment block above each bitmap documenting which legacy ranges
      it encodes.
    - Add `motorola68080_tokvm_class_bitmaps_match_legacy_predicates` to
      `crates/opforge-asm/src/tests.rs`. This test mechanically ports the
      legacy `CMPI.B` chain bodies for `tokvmIsIdentifierContinue` and
      `tokvmIsNumberBody` into Rust by reading the `.else` arm of the
      assembly source (or transcribing it directly into the test, with the
      assembly source as the comment-cited authority), iterates bytes
      `0..=255` through the Rust port, and asserts that the resulting
      bitmap is byte-equal to the new `tokvmIdentContinueClassMask` and
      `tokvmNumberBodyClassMask` data emitted by the assembler. The Rust
      port must NOT be derived from the new bitmap comment block; it must
      be derived from the legacy assembly chain to defend against
      transcription errors.
  - Full quality gates: same set as Item 2, plus:
    - `cargo test -p asm motorola68080_tokvm_class_bitmaps_match_legacy_predicates -- --nocapture`
      must pass.
    - Opt-in `TKPKG_FS_UAE_M68080_FAST_PATHS=1` 68080 FS-UAE family parity
      test must continue to pass and now exercises the actual fast path.
  - Plan-compliance review evidence: same agent invocation requirement as
    Item 1, plus an explicit text note in the commit body confirming that
    the opt-in 68080 FS-UAE test was run and passed on the developer host.
  - Commit outcome: one commit titled
    `Added 68080 class-bitmap fast path for tokvm identifier and number scanners.`
  - Definition of done:
    - With `TKPKG_CPU_M68080` undefined, the produced assembled byte image
      for `tokvm_tokenizer_vm.asm` is byte-identical to the checked-in
      golden `crates/opforge-asm/tests/goldens/tokvm_tokenizer_vm.m68080_undefined_baseline.bin`
      captured in Item 1, evidenced by the existing Item-1
      `motorola68080_tokvm_interpreter_module_surface_assembles_with_cpu_define`
      test continuing to pass unmodified. This Item-3 commit must NOT
      regenerate or update that golden; if it diverges, treat it as a
      regression in the `.else` arm and stop. This is a *produced-output*
      check, not a `git diff` check on source. The two `.ifdef` arms are
      no longer expected to produce identical bytes after Item 3 (that was
      Item 1's invariant only); only the undefined arm vs. the golden
      remains an equivalence check.
    - With `TKPKG_CPU_M68080` defined, every byte 0..255 produces the same
      predicate result as the legacy chain, proven by:
      - the new `motorola68080_tokvm_class_bitmaps_match_legacy_predicates`
        Rust test passing, and
      - the opt-in 68080 FS-UAE family parity test from Item 2 continuing
        to pass against the new fast path on the developer host.

## Milestones

- [ ] Milestone 1: scaffolding-only commit lands (Item 1).
- [ ] Milestone 2: opt-in 68080 FS-UAE family parity test lands (Item 2).
- [ ] Milestone 3: 68080 class-bitmap fast path lands behind define (Item 3).

Each milestone is exactly one work item and exactly one commit.

## Validation Strategy

- Default-build (undefined `TKPKG_CPU_M68080`) parity is enforced by the
  existing staged Rust tokenizer corpus test and the existing tkpkg/tokvm
  module-surface tests on every work item, plus the new produced-output
  equivalence assertion in Item 1 (extended in Item 3).
- 68080-build parity is enforced by:
  - the opt-in FS-UAE family parity test introduced in Item 2 (initially
    against byte-identical scaffolding, then against the actual fast path
    in Item 3),
  - the bitmap-vs-legacy mechanical-port Rust test added in Item 3.
- No fixture is regenerated by this plan. If any existing fixture changes,
  treat it as a regression and stop.

## Deferred Work (Out of Scope for This Plan)

The following items from the prior review are explicitly out of scope and
must be planned separately. The short list below is the executive index;
Appendix A captures the full ranked review proposal verbatim so future
plan authors do not have to reconstruct it from chat history.

- AMMX byte-parallel class tests / wide find-first-non-class loops.
- AMMX or MOVE16 wide-copy variants of `tkpkg_tokenizer_vm_append_bytes_v1`
  and the per-char `tkpkg_tokenizer_vm_append_char_v1` bounds-check folding.
- `tkpkg_tokenizer_vm_append_u32_v1` `DIVU.L`-based decimal emit.
- `tkpkg_tokenizer_vm_append_byte_list_v1` packed digit formatter.
- `tkpkg_tokenizer_vm_append_upper_quoted_v1` AMMX upper variant.
- `tokvmScanSymbolToken` jump-table or AMMX-broadcast dispatch.
- `tokvmCommitPendingToken` wide-store via MOVE16.
- `tokvm_run_68000` step-counter register hoisting.
- `tkpkg_tokenizer_vm_read_u16_le_v1` / `read_u32_le_v1` `BYTEREV` variants.
- Any change to `TKPKG_DEBUG_PIPELINE_*` semantics or pipeline-ID strings.

## Blocking Rules

- the active worktree `AGENTS.md` rules must be followed throughout execution
- no commit before all quality gates pass
- no commit before `plan-compliance-reviewer` returns `PASS`
- each work item or phase must end in exactly one new commit before the next
  item starts
- no advancing to the next item on failed validation
- checkbox updates are mandatory bookkeeping
- if any default-build (un-defined `TKPKG_CPU_M68080`) test diverges from the
  current baseline, stop immediately and report a regression rather than
  regenerating fixtures
- if the 68080-build test diverges from the baseline build, stop and treat it
  as a correctness bug in the new fast path rather than a fixture issue

## Appendix A: Full ranked 68080 perf review (source for this plan)

The following list is the verbatim ranked review proposal that this plan was
derived from. It is captured here so future deferred-work plans have a
canonical, in-tree source rather than reconstructing it from chat history.
Items are ordered by expected per-tokenized-byte impact. Only review item #1
(scoped to a 68020-legal table-lookup variant, no AMMX) is in scope for this
plan; everything else is deferred.

Suggested gate name across all items: `TKPKG_CPU_M68080`, independent of
`TKPKG_DEBUG_PIPELINE_M68080`. Gate layout:

```
    .ifdef TKPKG_CPU_M68080
    ; 68080 fast path
    .else
    ; portable 68000/68020 baseline (current code)
    .endif
```

### High impact — scanner hot path (runs for every source byte)

1. **Character-class predicates → AMMX table lookup (or PEXT/PTEST-style).**
   Files/labels: `tokvmIsIdentifierStart`, `tokvmIsIdentifierContinue`,
   `tokvmIsNumberBody`, `tokvmIsWhitespace`, `tokvmIsQuoteChar` in
   `examples/motorola68000/amigaos/tkpkg/tokvm_tokenizer_vm.asm`. Baseline
   does 3–6 sequential `CMPI.B / BLO / BHI / BEQ` range checks per call,
   invoked once per scanned byte through `JSR`. 68080 fragment: replace the
   predicate body with a 256-byte class bitmap lookup
   (`MOVE.B (classMask, D0.W), D0`) or, better, an AMMX
   `PCMPGTB / PAND / PTST` block testing 8 bytes at once so identifier /
   number / whitespace loops consume 8 source bytes per iteration. Expected
   ~5–10× per byte vs. the branch ladder. *Note: this plan implements only
   the 256-byte bitmap variant of `tokvmIsIdentifierContinue` and
   `tokvmIsNumberBody`; AMMX vectorization is deferred.*

2. **`tokvmScanIdentifierLoop` / `tokvmScanNumberLoop` → AMMX
   "find-first-non-class".** Labels: `tokvmScanIdentifierLoop`,
   `tokvmScanNumberLoop`. Baseline: per byte — `MOVE.B`, `JSR
   tokvmIsIdentifierContinue`, `TST/BEQ`, scratch overflow check, ASCII
   lower-case range compare, `MOVE.B` to scratch, `ADDQ`. The `JSR` and
   predicate chain dominate any identifier or number longer than ~3 bytes.
   68080 fragment: AMMX byte-parallel class test plus bitscan to locate the
   first non-class byte in a 64-bit chunk, then `MOVE.L/MOVE.L` (or
   `MOVE16`) the run into scratch, plus a packed AMMX `POR #$20` for ASCII
   lower-case in the identifier case. Expected ~4–8× on long identifiers
   and numbers, which dominate realistic source lines.

3. **`tokvmScanSymbolToken` dispatch ladder → jump table / AMMX
   broadcast-equals.** Label: `tokvmScanSymbolToken`. Baseline: ~24 serial
   `CMPI.B #'x',D0 / BEQ` entries. 68080 fragment: in an `.ifdef` branch,
   use a compact 128-entry jump table
   `LEA symbolDispatch(PC), A1; MOVEA.L 0(A1, D0.W*4), A1; JMP (A1)` —
   already legal 68020+ syntax but worth gating to 68080 if the table cost
   is unwanted on plain 68020. Alternative 68080-only fragment: one AMMX
   `PCMPEQB` against a packed vector of lead-byte triggers, bitscan the
   match, use it as an index. Expected ~3–6× on symbol-heavy input.

4. **`tokvmProgramLoop` step accounting and bounds checks.** Every bytecode
   step does `MOVE.L LOCAL_STEP_COUNT`, `ADDQ`, `CMP`, `BHI`, reloads
   `LEA 0(A3,D7.L),A1`, `CMP.L D4,D2`, etc. 68080 fragment: keep the step
   counter in a spare data register and hoist the invariant `LEA end` out
   of the loop. Effectively a 68080-specific register-allocation variant,
   gated so the 68000 build retains its conservative memory-spill form.
   Expected ~1.2–1.5× on dispatch itself.

### Medium impact — render / append loops (runs per emitted token)

5. **`tkpkg_tokenizer_vm_append_bytes_v1` → wide copy / `MOVE16`.**
   Baseline: per byte — `MOVE.B (A1)+,D0`, `BSR
   tkpkg_tokenizer_vm_append_char_v1` (which itself does push/pop, bounds
   check, re-`LEA`, store, clear terminator). Called for every literal
   prefix (`"Identifier("`, `", base: "`, `"] }"`, etc.) and every run of
   source bytes in the debug render. 68080 fragment: inline a wide path —
   check remaining capacity once, then `MOVE.L (A1)+, (A0)+` in a 4-byte
   unrolled loop with residual tail, or `MOVE16 (A1)+, (A0)+` for 16-byte-
   aligned prefix-literals. Expected ~4× on long literals.

6. **`tkpkg_tokenizer_vm_append_char_v1` bounds-check overhead.** Baseline:
   per-char `MOVE.L A1,-(SP)` + capacity compare + store + clear terminator
   + `MOVEA.L (SP)+, A1`. 68080 fragment: provide an `append_run` variant
   taking `(src, len)` that does one capacity saturation then a wide copy —
   eliminates the per-byte push/pop and terminator rewrite. Pairs with #5;
   ship together.

7. **`tkpkg_tokenizer_vm_append_u32_v1` decimal emit → single `DIVU.L`.**
   Baseline: 10 outer iterations × up to 9 `CMP/SUB` per digit via
   `decimalPowers`, plus zero-suppression state. 68080 fragment: Apollo
   `DIVU.L #10,Dx:Dy` is effectively single-cycle; replace with
   straightforward `DIVU.L` per digit (reverse build, then reverse-copy)
   or precompute all 10 digits via a log2 table + multiply-reciprocal.
   Expected ~3–5× on number / byte-list rendering, the dominant path for
   `Number { ... }` debug rows and `bytes: [ ... ]` lists.

8. **`tkpkg_tokenizer_vm_append_byte_list_v1` → packed hex/decimal
   vectorizer.** Label: `tkpkgTokenizerByteListLoop`. Baseline: per source
   byte, one full `append_u32` (worst case 10 iterations of power-subtract)
   plus a `", "` re-append. For a 32-byte string payload that is ~320
   subtract/compare ops. 68080 fragment: compute all 4 bytes of a long at
   once using AMMX unpack + multiply-by-reciprocal to split into hundreds /
   tens / ones digits, packed ASCII-add `+'0'`, single wide store. Strictly
   AMMX-gated.

9. **`tkpkg_tokenizer_vm_append_upper_quoted_v1` ASCII upper loop.**
   Label: `tkpkgTokenizerUpperQuotedLoop`. Baseline: per byte range-check
   then `ANDI.B #$DF,D0`. 68080 fragment: AMMX byte-parallel
   `(b >= 'a' && b <= 'z') ? b & 0xDF : b` across 8 bytes at a time.
   Combines naturally with the wide-copy change from #5.

### Lower impact — commit / program load

10. **`tokvmCommitPendingToken` 20-byte record store.** The token record is
    20 bytes; on 68080 one `MOVE16` (plus a residual 4 bytes) performs the
    entire record write; baseline is several discrete `MOVE.L`/`MOVE.W`
    stores per token. Gated `MOVE16` writes here remove several
    instructions per emitted token.

11. **`tkpkg_tokenizer_vm_read_u16_le_v1` / `read_u32_le_v1`.** Baseline:
    byte-at-a-time `MOVE.B` + `LSL` assembly of a little-endian word/long
    (deliberately endian-safe for 68000). 68080 fragment: `MOVE.L (A2)+,
    D0` + `BYTEREV`/`ROL` pair for endian swap — one instruction vs eight.
    Low hotness (only during `tokenize_line` program load), but extremely
    cheap.

12. **`MOVEM.L D2-D7/A2-A6` prologue/epilogue.** Already cycle-free on
    68080; no change needed. Note: do **not** break these up into
    individual pushes in any 68080 variant, because the hardware is
    specifically optimized for the wide `MOVEM` form.

### Sequencing recommendation across all 12 items

Land #1 + #2 + #5 together as the first AMMX-enabled slice (they share the
class-table and wide-copy infrastructure and account for the bulk of the
realistic per-line speedup). #3 and #7 are the next most valuable and can
land independently. #9 onward is incremental.

This plan implements only the bitmap-only portion of #1 (no AMMX), as the
narrowest vertical slice that proves the conditional-assembly scaffolding
and parity gates. Every other item above requires its own plan with its
own plan-quality and execution-compliance gates.
