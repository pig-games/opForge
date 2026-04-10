# Review Report

## Scope

Full review of the current opForge worktree, not limited to a branch diff.

Focus: code-reduction opportunities with concrete evidence, especially duplicated production logic, oversized reduction hotspots, repeated protocol/codec wiring, large mixed-responsibility modules, and cross-crate bootstrap/setup paths that can be centralized without changing behavior.

## Findings

### RVW-2026-04-11-005

- Severity: high
- File: crates/opforge-families/src/m68k/handler.rs:94-246; crates/opforge-families/src/m68k/handler.rs:1290-1796; crates/opforge-families/src/m68k/handler.rs:1796-7034
- Issue: The M68K family core is an 11k-line mixed-responsibility module that combines register parsing, compatibility validation, operand parsing, instruction dispatch, effective-address encoding, and large instruction encoder groups in one file.
- Why it matters: This is the main structural blocker to broader code reduction in the M68k stack. Every change pays a very large review surface, shared logic is harder to extract cleanly, and CPU-specific wrappers are more likely to accumulate additional policy code instead of delegating to well-bounded shared components.
- Fix direction: Split the M68K core into stable submodules by contract: compatibility validation, operand parsing and normalization, effective-address encoding, and instruction encoder groups, with M68KFamilyHandler reduced to orchestration.

### RVW-2026-04-11-001

- Severity: high
- File: crates/opforge-families/src/m68020/handler.rs:35-78; crates/opforge-families/src/m68030/handler.rs:21-57; crates/opforge-families/src/m68040/handler.rs:33-76; crates/opforge-families/src/m68080/handler.rs:451-486
- Issue: The per-CPU M68k wrappers repeat the same .fpu target-name lookup, legal-target validation, and deferred "recognized but not yet implemented" diagnostic flow instead of delegating those checks through one shared capability layer.
- Why it matters: FPU policy or diagnostic-contract changes now require synchronized edits across four CPU handlers on top of an already large M68k stack, which is a concrete drift risk and keeps adding wrapper code instead of reducing it.
- Fix direction: Introduce one shared M68k FPU capability helper parameterized by CPU name and legal target set, and route the CPU-specific handlers through that helper for validation and deferred-diagnostic formatting.

### RVW-2026-04-11-006

- Severity: medium
- File: crates/opforge-ffi/src/lib.rs:919-1165; crates/opforge-ffi/src/lib.rs:1304-1461; crates/opforge-ffi/src/lib.rs:2106-2124
- Issue: The FFI boundary manually mirrors the portable syntax model several times: expression-tree flattening in push_expr, token and expression text reconstruction in portable_token_text and portable_expr_text, and token-kind remapping in map_portable_token_kind.
- Why it matters: Every portable schema addition or rename now requires coordinated updates across several manual mirrors inside an 8k-line boundary file, which increases drift risk at the public API boundary and keeps the FFI layer larger than it needs to be.
- Fix direction: Introduce one shared portable-schema adapter layer inside the FFI crate that derives node kind, display text, and token-kind mapping from one source of truth, then route the report builders through that adapter.

### RVW-2026-04-11-007

- Severity: medium
- File: crates/opforge-engine/src/lib.rs:341-464; crates/opforge-asm/src/runtime_model.rs:15-110
- Issue: Runtime-model bootstrap policy is implemented in parallel in the engine and assembler crates, including artifact-path resolution, load-from-path behavior, package-byte fallback, and package artifact persistence.
- Why it matters: Bootstrap policy changes have to be synchronized across crate boundaries, which expands the review surface and risks the editor-facing and assembler-facing runtime paths diverging over time.
- Fix direction: Move runtime-model bootstrap and artifact policy into one shared module used by both crates.

### RVW-2026-04-11-002

- Severity: medium
- File: crates/opforge-asm/src/engine.rs:955-1332; crates/opforge-asm/src/engine.rs:1387-1811
- Issue: execute_pass1_lines and execute_pass2_lines each implement their own recursive repetition walker, including the same conditional-skip handling, .for/.while matching, scoped-label restrictions, loop bookkeeping, and loop-body recursion.
- Why it matters: Repetition semantics are one of the assembler's more complex paths, and any behavioral change has to be made twice inside two long functions that only partially differ in pass-specific output handling.
- Fix direction: Extract one shared repetition traversal driver that owns directive matching, nesting, and iteration bookkeeping, then inject pass-specific hooks only for regular-line execution, listing emission, and pass-consistency checks.

### RVW-2026-04-11-003

- Severity: medium
- File: crates/opforge-package/src/package/codec.rs:916-1045; crates/opforge-package/src/package/codec.rs:1152-1605
- Issue: The package codec repeats the same hand-written encode/decode pattern for many chunk types: write count, encode owner, serialize ordered fields, mirror the decode path, then push the descriptor into a result vector.
- Why it matters: Schema changes must be mirrored manually in an already large codec file, which increases mismatch risk between serializer and deserializer and adds boilerplate every time a new chunk type is introduced.
- Fix direction: Move chunk field definitions into one shared schema-driven codec layer so each descriptor's field order is declared once and reused by both encode and decode paths.

### RVW-2026-04-11-004

- Severity: medium
- File: crates/opforge-lsp/src/protocol.rs:13-140; crates/opforge-lsp/tests/common/lsp_client.rs:11-261
- Issue: The LSP Content-Length framing logic exists twice, once in the production stdio protocol layer and again in the integration-test client, including the same header parsing, size cap, JSON payload decoding, and frame writing behavior.
- Why it matters: Protocol fixes require parallel edits, and the test client can drift into validating a stale local copy of the framing code instead of exercising the production implementation.
- Fix direction: Extract one shared LSP frame reader/writer module and use it from both the stdio server path and the test client.

## Testing Gaps

- I did not find one decomposition-oriented characterization suite around crates/opforge-families/src/m68k/handler.rs that separately locks operand parsing, effective-address encoding, and instruction-family behavior at the seams that a structural split would need.
- I did not find one table-driven cross-CPU characterization suite that locks the allowed .fpu target matrix and deferred-diagnostic behavior across m68020, m68030, m68040, and m68080. That gap matters if the duplicated M68k wrapper logic is consolidated.
- I did not find one parity suite for the FFI portable-schema boundary that would fail when PortableAstExpr or PortableTokenKind grows without the FFI mirrors being updated in lockstep.
- I did not find one cross-crate test that asserts the engine and assembler runtime-model bootstrap paths resolve the same artifact and fallback behavior under equivalent feature settings.
- I did not find one schema-driven round-trip test that iterates every package chunk codec through the same generic encode/decode contract. Current coverage is good in places, but it still relies on bespoke tests per chunk family.
- The LSP layer does not currently prove that production framing and test-client framing share the same implementation, which is the guardrail needed before that duplication is removed.

## Residual Risks

- This was a static full-worktree review focused on code-reduction hotspots. I did not run the test suite or measure clone percentages with a dedicated duplication tool.
- I prioritized the strongest structural and production-path reduction opportunities in opForge. Additional consolidation opportunities may still exist in very large surfaces such as crates/opforge-lib/src/lib.rs, crates/opforge-cli-core/src/cli.rs, and crates/opforge-asm/src/tests.rs.
- The findings above are maintainability and change-risk findings, not proof of current behavioral defects.

## Brief Summary

The strongest reduction opportunities are broader than the original draft suggested. The main structural candidates are the monolithic M68k family core, the repeated M68k CPU-wrapper policy layer, the manually mirrored portable-schema logic in the FFI boundary, and the duplicated runtime-model bootstrap path across engine and assembler. The earlier local findings still matter, but they sit under those larger architectural reduction seams.