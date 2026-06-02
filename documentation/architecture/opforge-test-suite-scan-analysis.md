# opForge Test Suite Scan Analysis

## Purpose

This note summarizes a static scan of the opForge test suite and groups the main
test areas by the kind of protection they provide versus the amount of churn
they are likely to create during implementation work.

This is not a test execution report. It is a read-based analysis of the current
suite layout, representative test files, and the kinds of contracts those tests
appear to enforce.

## High-level shape

The executable test surface is concentrated heavily in a few areas:

- `crates/opforge-asm`
- `crates/opforge-vm`
- `crates/opforge-core`
- `crates/opforge-families`

The largest single suite is:

- `crates/opforge-asm/src/tests.rs`

That file acts as a mixed end-to-end, rollout, parity, integration, and
regression suite rather than a narrowly scoped unit-test file.

## Groups That Strongly Justify Their Existence

### 1. FFI ABI and panic-boundary tests

Representative files:

- `crates/opforge-ffi/tests/abi_contract.rs`
- `crates/opforge-ffi/tests/release_panic_boundary.rs`

These tests should exist.

Why they are valuable:

- They protect C ABI layout, offsets, enum values, and exported symbol shape.
- They verify cross-language ownership and report access contracts.
- They verify release-profile panic containment at the FFI boundary.

Why they are low-churn relative to value:

- The tested behavior is a public contract.
- Regressions here are expensive and difficult to detect by casual manual use.
- Internal refactors should not require frequent changes if the contract is
  stable.

Conclusion:

- Keep these aggressively.

### 2. Package/container codec and determinism tests

Representative file:

- `crates/opforge-package/src/package/tests.rs`

These tests should exist.

Why they are valuable:

- They verify encoding and decoding round trips.
- They verify deterministic output ordering and stable metadata snapshots.
- They verify malformed container rejection and bounded decode behavior.
- They protect compatibility-sensitive binary/container formats.

Why they are low-churn relative to value:

- The assertions are about stable format behavior, not incidental structure.
- The failure modes are exactly the kinds of problems that escape shallow
  testing.

Conclusion:

- Keep these as core contract tests.

### 3. Core parser, tokenizer, preprocess, and expression tests

Representative files:

- `crates/opforge-core/src/parser.rs`
- `crates/opforge-core/src/tokenizer.rs`
- `crates/opforge-core/src/preprocess.rs`
- `crates/opforge-core/src/macro_processor.rs`
- `crates/opforge-core/src/expr.rs`
- `crates/opforge-core/src/expr_vm.rs`

These tests should exist.

Why they are valuable:

- Syntax and expression semantics are central assembler behavior.
- The suite covers edge cases that users will absolutely hit.
- Parser and tokenizer regressions tend to spread widely across the product.

Why they are reasonably durable:

- The tests mostly exercise language behavior, not UI or transient internals.
- The feature surface is broad enough that explicit regression cases are
  justified.

Conclusion:

- Keep these as foundational protection.

### 4. CPU-family semantic tests

Representative files:

- `crates/opforge-families/src/m68k/handler.rs`
- `crates/opforge-families/src/m45gs02/handler.rs`
- `crates/opforge-families/src/intel8080/handler.rs`
- `crates/opforge-families/src/m65816/handler.rs`

These tests should exist.

Why they are valuable:

- CPU-specific legality, addressing-mode interpretation, and encoding shape are
  core product behavior.
- opForge is explicitly multi-family, so family-specific regressions matter.
- These tests protect the architecture boundary by keeping CPU logic in CPU
  packages and handlers rather than in generic paths.

Conclusion:

- Keep these as primary behavioral coverage.

### 5. Source graph and module-loading tests

Representative file:

- `crates/opforge-engine/src/source_graph_tests.rs`

These tests should exist.

Why they are valuable:

- Module loading, include behavior, dependency staging, and cycle reporting are
  user-visible system behavior.
- The bugs tend to be subtle and path-sensitive.

Why they are likely worth their maintenance cost:

- They cover integration behavior at a manageable scope.
- The assertions are understandable and close to user workflows.

Conclusion:

- Keep these.

### 6. LSP integration tests

Representative file:

- `crates/opforge-lsp/tests/lsp_client_integration.rs`

These tests should exist.

Why they are valuable:

- LSP behavior is primarily integration behavior.
- Features like debounce, overlay management, hover, definition, symbol search,
  and completion are hard to validate well with isolated unit tests.
- Many failures here are user-facing and editor-specific.

Why they are worth the cost:

- They test real message flow and state transitions instead of only helper
  functions.

Conclusion:

- Keep these as integration coverage.

### 7. Formatter golden and semantic-preservation tests

Representative file:

- `crates/opforge-formatter/src/fixture_tests.rs`

These tests should exist.

Why they are valuable:

- Formatter output is a visible contract.
- Idempotence is critical for trust.
- Semantic token preservation checks protect against “pretty but wrong”
  rewrites.

Conclusion:

- Keep these.

## Groups That Provide Real Protection But Also Carry More Churn Risk

### 8. The large end-to-end assembler suite

Representative file:

- `crates/opforge-asm/src/tests.rs`

This suite contains a lot of valuable coverage, including:

- end-to-end assembly behavior
- diagnostics behavior
- linker and map output behavior
- module visibility behavior
- language directive behavior
- example-program parity across families
- integration behavior around runtime mode selection

However, it also mixes several different categories in one place:

- user-facing contract tests
- migration parity tests
- rollout policy tests
- failpoint tests
- external-tool integration tests
- internal routing and fallback behavior tests

Why this matters:

- The file has become a broad regression harness rather than a single coherent
  suite.
- Internal implementation movement is more likely to force widespread test
  edits.
- It is harder to see which failures indicate broken user behavior and which
  failures only indicate internal pipeline reshaping.

Conclusion:

- Keep the suite, but treat it as the best place to prune or split when test
  maintenance starts slowing feature work.

### 9. VM runtime and host-vs-VM parity tests

Representative files:

- `crates/opforge-vm/src/runtime_tests.rs`
- `crates/opforge-vm/tests/parser_vm_v2_parity.rs`
- `crates/opforge-vm/tests/parser_vm_native_abi.rs`
- VM-heavy sections of `crates/opforge-asm/src/tests.rs`

These tests currently make sense.

Why they are valuable right now:

- The project is carrying multiple execution and parsing paths.
- Parity tests reduce migration risk while VM and native paths coexist.
- Native ABI decoding tests protect a fragile low-level boundary.

Why they are churn-prone:

- They often compare one internal implementation against another internal
  implementation rather than against an external user-facing contract.
- The same parity story appears to be checked in more than one layer.
- When rollout defaults, fallback rules, or internal contracts change, many
  tests may need coordinated updates.

Conclusion:

- Keep them during active migration and rollout work.
- Reassess them once one path becomes authoritative and the other is no longer a
  first-class fallback.

## Groups Most Likely To Create More Churn Than Additional Protection

This does not mean they are bad tests. It means they are the best candidates for
future consolidation.

### 10. Duplicated parity coverage across layers

Observed pattern:

- host-vs-VM parity is tested in `opforge-vm`
- similar parity expectations are also tested through `opforge-asm`

Risk:

- The second copy may not add much new signal if it uses nearly the same corpus
  and failure interpretation.
- Refactoring one internal layer can force updates in two suites that are
  conceptually proving the same thing.

Better long-term shape:

- keep one authoritative parity suite close to the implementation boundary
- keep a smaller end-to-end canary set at the assembler layer

### 11. Family-by-family failpoint matrix expansion

Observed pattern:

- multiple tests assert that VM-based parsing or evaluation survives host
  failpoints or refuses fallback under several family/runtime combinations

Risk:

- these are useful during rollout hardening
- they become combinatorial quickly
- they often assert backend routing policy more than user-visible behavior

Better long-term shape:

- keep a representative matrix
- avoid cloning the same backend-routing proof for every family unless the
  family has genuinely unique semantics

### 12. Exact output-text checks for non-public wording

Observed risk:

- if tests lock onto exact diagnostics or report wording where the real contract
  is structural, harmless phrasing edits can create noisy churn

This is not a blanket criticism. Exact text checks are correct when:

- the text is part of a public stable interface
- the message wording is intentionally standardized
- downstream tooling depends on the exact text

Conclusion:

- Prefer structural assertions over exact prose where the wording is not itself
  the public contract.

## Recommended Keep / Consolidate / Revisit Map

### Keep

- FFI ABI and panic-boundary tests
- package/container codec tests
- core parser/tokenizer/preprocess/expression tests
- CPU-family semantic tests
- engine source-graph tests
- LSP integration tests
- formatter fixture, idempotence, and semantic-preservation tests

### Keep, but prune or split carefully

- `crates/opforge-asm/src/tests.rs`
- VM/runtime selection and rollout tests that currently carry migration safety

### Best future consolidation targets

- duplicated host-vs-VM parity corpora across `opforge-vm` and `opforge-asm`
- backend failpoint tests replicated across many family combinations
- exact wording checks where a structural contract would be sufficient

## Practical conclusion

The suite does not look obviously bloated for an assembler, packaging,
formatter, VM, FFI, and LSP project of this scope.

The main concentration of likely churn is not in the classic unit-test areas. It
is in migration-era parity and rollout coverage:

- dual-path host vs VM comparisons
- fallback-policy assertions
- failpoint-driven backend-routing tests

That coverage is probably worth the cost today because it is protecting active
transitional architecture. It is also the clearest area to simplify once the
runtime and parser rollout story stabilizes.

## Suggested follow-up

If this analysis is used to guide cleanup, the least risky next step would be:

1. identify parity corpora asserted in both `opforge-vm` and `opforge-asm`
2. keep one authoritative parity owner for each corpus
3. retain only a compact end-to-end smoke set at the higher layer
4. keep externally visible contract tests unchanged

