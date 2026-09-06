# Step 28 scalar authority audit

Status: confirmed prerequisite; Step 28 and Phase A remain incomplete. No scope reduction is authorized by this audit. The planned general nonnegative-expression and full-u32 reservation contract still has to be implemented and proven.

## First lost information

Rust `line.rs::eval_expr_for_non_negative_directive` evaluates a signed i64, rejects negatives, then converts to u32 with range checking. The live Rust test `native_listing_reservation_host_scalar_domain_requires_more_than_low32` proves distinct results for `$80000000+0` (valid), `-2147483648` (negative), `$ffffffff+1` (out of range), and `($ffffffff+1)-1` (valid). This also rules out a sticky intermediate-overflow flag as a general replacement for a wide value.

Native `exprvm_runtime.asm::readI64Low32` discards four bytes from the serialized i64 and retains only D3.L. Its stack is eight longs. Arithmetic operates on long values, so information is already lost before the assembly driver sees the result. `opcore_expr_bridge.asm::emitPushLiteralD3` emits the low long with a zero high long; literal parsing and symbol/resolver authorities also need auditing. `tkpkg_expression_service.asm` publishes only D3.L in its extension result and formats that value as signed 32-bit text. Negative values and valid positive u32 high-bit values collide; overflowing compound expressions can wrap to a valid-looking result.

## Immediate consumer corrections

The reservation draft now prioritizes byte/word/long keywords, captures exactly two operands, checks unsigned literal overflow, and parses expression output text unconditionally. The old general result reader returns early for a nonzero binary result, so merely adding a sign flag to its text parser was ineffective. The new RES-specific evaluator also preserves D4-D5 around tkpkg dispatch, which clobbers both while the caller retains its unit. These corrections do not restore the information discarded by the scalar VM. Compound expression parity remains unproven and is known incomplete.

## Required next boundary

Preserve signed value/range information through literal decoding, symbol/binding resolution, arithmetic and the service result. Use the Rust evaluator as the semantic authority, including intermediate values, overflow behavior, shifts and comparisons. Keep existing legacy caller ABIs compatible, and expose a typed nonnegative-u32 result to the reservation consumer. Do not use source spelling or a high-bit heuristic to guess the sign of an evaluated result. Do not qualify Step 28 with only direct literals or small expressions.

Independent read-only audit: `step27_independent_review` identified both the ineffective sign flag and the scalar representation gap. A bounded follow-up is tracing the smallest shared implementation and compatibility boundary. No new scalar VM production implementation has been started yet.

## Evidence and limits

`target/workflow-logs/step28-reservation/host-scalar-domain.json`: focused host tests exit zero, including four Level A/B tests; two native test entrypoints explicitly SKIP without FS-UAE configuration. The positive test constructs seven live same-package whole listing/BIN oracles, covering zero count and 4/6/8-digit extents. This is not guest proof.

`assemble-fourth.json`: source-owned Hunk host assembly exits zero after earlier branch-range and symbolic MOVEQ corrections; its temporary output tree is removed. `assemble-third.json` is an invalid manual CLI output configuration (missing sections), not a guest or production failure. Earlier failing host receipts remain retained. No FS-UAE run was made for Step 28, and no full quality gate or compliance completion is claimed. Capacity snapshots still require refresh after production stabilizes.

Performance contribution: this audit prevents certifying incorrect reservation arithmetic. Step runtime gain and cumulative Phase A runtime gain remain unmeasured. Existing DOS-read and validation-time reductions remain separate scoped measurements.

## Bounded prerequisite design returned by independent review

The first implementation owner is `exprvm_runtime.asm`: the bytecode already carries i64 literals, so preserve both little-endian words, expand the existing eight logical stack slots from one long to two, implement pair-aware operators, and expose the last result high word without changing the existing D3-low/status caller ABI. This adds 32 stack bytes plus small result state, not new per-statement storage.

Rust `asmline_eval.rs::eval_symbol_leaf_for_vm_bridge` converts existing u32 symbol values with `i64::from(entry.val)`. Labels, scalar bindings, lexical-resolver results and the current address therefore inject a zero high word. No high-word symbol or snapshot table is required; sign-extending bit 31 would be incorrect.

The bridge must parse and emit full-width numeric literals, then retain the runtime result high word with its existing low-word ABI. The service can potentially use the existing 36-byte expression extension: preserve low at +16 and publish high plus an authoritative-width marker in consumed input/output union fields +20/+24. This is a design candidate requiring a complete field-lifetime and legacy-length audit before edits, not a proven wire contract. Existing short-extension clients must keep their valid low-word behavior. RES then uses the typed pair and accepts exactly high=0 before its checked multiply/span path, replacing temporary sign-text heuristics and duplicate literal handling.

Rust `opforge-core/src/expr.rs` is the operator authority: add/subtract/multiply/power wrap i64; bit operations use 64 bits; shifts mask RHS with 31 while shifting the 64-bit value and right shift is logical; comparisons/division/modulo are signed. Negative or above-u32 powers and zero divisors fail. Minimum-i64 unary/division edges need an explicit live oracle because current Rust uses direct negation/division there; do not invent their behavior.

Focused regression owners are direct ExprVM and bridge contracts; native unary/power/comparison/bitwise/logical/ternary/string expressions; expression-service and scalar operand encoding; then macro, conditional, repetition, forward-reference and module-visibility consumers. All Step 28 artifact, failure and Phase A closure requirements remain outstanding. Before implementing the shared prerequisite, amend/review the plan's item boundaries and preserve the current reservation draft so neither its work nor the warm target is lost.

## Plan amendment and draft preservation

The reviewed plan now contains 39 sequential steps: shared scalar authority is Step28, reservation is Step29, Phase A closure is Step30 and LSP is Step39. Plan Quality Reviewer `step27_independent_review` returned PASS against plan SHA-256 `518214fa4e7800b879b0abc586b725fb18130ea33340363587da965b1f73e663`. All original completion requirements remain binding.

Nine reservation draft files were copied and SHA-256 verified with a binary tracked-file patch and manifest at `/private/tmp/opforge-step28-reservation-draft-2a327798`. Their base is `2a3277987f2a04f49f5ad9ff599eb6cee012080f`. Only those owned tracked edits were restored to that base; the draft tests/slice/evidence are preserved in the verified copy. The saved work will be restored and adapted after the shared scalar prerequisite commits. The warm Cargo target remains in place.
