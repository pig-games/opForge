<!-- workflow-provenance: skill=opforge-plan-authoring; entrypoint=run_plan_workflow.sh -->
# opForge Native Porting Workflow Improvement Specification and Plan v0.1

Version: 0.1  
Scope: opForge Rust-to-native 68000/AmigaOS parity workflow, native instrumentation, debug-contract asserts, rule packs, and deterministic quality gates.

---

## Metadata

- Source: User-requested quality review and hardening of the native porting workflow plan on 2026-07-01.
- Mode: implementation
- Owner: unassigned
- AGENTS binding: The active worktree `AGENTS.md` rules remain binding during execution.

## Goal

Create an enforceable Rust-to-native 68000/AmigaOS parity workflow, provide
safe debug-contract instrumentation, add deterministic workflow checks, and
prove the approach on one bounded diagnostic before a separate parity fix.

## Version Impact

- Affected component(s): native 68000 workflow policy, debug-contract modules, workflow validators, and native validation gates
- Impact class: minor
- Owned contract: native parity claims identify their Rust boundary, native boundary, proof level, focused proof, and FS-UAE confirmation
- Rationale: the existing workflow does not deterministically prevent unsafe instrumentation or overclaiming from incomplete evidence

## 1. Purpose

This specification defines a safer and more deterministic workflow for porting Rust VM/CLI behavior to the native 68000/AmigaOS opForge implementation.

The immediate problem this addresses is agentic debugging churn: broad speculative edits, unsafe instrumentation, unclear proof boundaries, noisy FS-UAE loops, and tests that appear to prove native behavior while actually only proving Rust-side or truncated-fixture behavior.

The goal is to turn native porting work into a contract-driven workflow:

```text
Rust reference contract
→ native boundary contract
→ safe debug-contract assert
→ focused implementation
→ deterministic host-side proof
→ FS-UAE confirmation
→ cleanup
```

This should make native debugging safer for humans and much harder for agents to derail.

---

## 2. Problem Statement

Recent native parity debugging showed several failure patterns:

1. **Untrusted evidence**
   - Host-side Rust harness tests were treated as proof of real native 68000 behavior.
   - Prefix/truncated fixtures were treated as equivalent to full-file parity tests.
   - "The failure moved" was treated as progress even when the root invariant had not been proven fixed.

2. **Unsafe instrumentation**
   - Debug probes in 68000 assembly modified condition codes.
   - Instrumentation was inserted near branch decisions.
   - Debug helpers sometimes failed to preserve registers or stack state.
   - Shared request, service, and diagnostic buffers were reused for debug output, producing misleading evidence.

3. **Layer confusion**
   - Pure Rust VM behavior, Rust-side native harness behavior, host-side native request simulations, and FS-UAE native execution were mixed without proof-level classification.

4. **Speculative production changes**
   - Capacity changes, request-shape changes, source-buffer changes, selector changes, expression-bridge changes, and diagnostic changes accumulated in one worktree.
   - This made it hard to distinguish real fixes from diagnostic side effects.

5. **Lack of deterministic workflow enforcement**
   - Rules existed, but there was no deterministic script checking whether the agent followed the required workflow.
   - Quality gates were run inconsistently or interpreted loosely.

---

## 3. Target Principles

### 3.1 Contract-first native porting

Native code should not be patched until the Rust reference behavior at the same semantic boundary is identified.

Every porting slice must define:

```text
Rust boundary:
Native boundary:
Contract:
Expected input:
Expected output:
Proof level:
Fast host-side proof:
FS-UAE confirmation:
```

### 3.2 Proof-level classification

Every test used as evidence must be classified:

| Level | Meaning |
|---|---|
| A | Pure Rust semantic oracle |
| B | Rust-side package/native harness contract |
| C | Host-side native request-shape simulator |
| D | Real native 68000/AmigaOS execution through FS-UAE |
| E | Temporary localization/debug probe only |

Each test summary must state:

```text
This test proves:
This test does not prove:
```

### 3.3 Debug instrumentation is production code until proven otherwise

Native debug/assert instrumentation must be safe by construction.

Ad-hoc instrumentation in 68000 assembly must be forbidden.

### 3.4 Assert as executable contract documentation

Debug asserts should document and enforce Rust-derived contracts at native boundaries.

A debug assert should say:

```text
Rust says this boundary must look like X.
Native asserts that X is true here.
If false, fail with a precise contract ID.
```

### 3.5 FS-UAE is the reality gate, not the inner-loop microscope

A normal debugging rhythm should be:

```text
FS-UAE reproduces real failure once
→ extract exact source/request/session evidence
→ reproduce the boundary with a host-side contract test if possible
→ patch native code
→ host-side proof
→ FS-UAE confirmation
```

### 3.6 One production invariant per slice

Each slice should fix one named invariant and prove it with one focused test before moving on.

"Failure frontier moved" is not sufficient evidence.

---

## 4. Proposed Repository Additions

### 4.1 New documentation

Add:

```text
documentation/architecture/native-porting-workflow.md
documentation/architecture/native-debug-contracts.md
documentation/architecture/native-instrumentation-framework.md
documentation/plans/native-porting-workflow-hardening-plan-v0_1.md
```

### 4.2 New agent rule packs

Add:

```text
agents/rules/native-rust-parity-porting.md
agents/rules/native-parity-failure-triage.md
agents/rules/native-68000-safe-instrumentation.md
```

### 4.3 Update existing rule files

Update:

```text
AGENTS.md
agents/rules/native-68000.md
agents/rules/fs-uae.md
```

### 4.4 New native debug/assert module

Add a new module family, exact paths to be adjusted to current native module layout:

```text
native/motorola68000/amigaos/debug/debug_contract_ids.asm
native/motorola68000/amigaos/debug/debug_assert.asm
native/motorola68000/amigaos/debug/debug_events.asm
native/motorola68000/amigaos/debug/debug_macros.i
native/motorola68000/amigaos/debug/debug_readme.md
```

Possible namespace:

```text
opforge.debug.contracts
opforge.debug.assert
opforge.debug.events
```

### 4.5 New deterministic Python workflow scripts

Add:

```text
scripts/workflow/check_native_porting_slice.py
scripts/workflow/check_native_instrumentation_safety.py
scripts/workflow/check_native_contract_asserts.py
scripts/workflow/check_fsuae_invocation_policy.py
scripts/workflow/run_native_porting_quality_gate.py
```

These scripts should be deterministic, local, and CI-friendly.

---

## 5. AGENTS.md Changes

AGENTS.md should contain only the high-level binding policy and route agents to the detailed rule packs.

Suggested addition:

```markdown
## Native Rust-to-68000 Porting Work

When modifying the native 68000/AmigaOS implementation to match Rust VM/CLI behavior, use the native Rust parity porting workflow.

Required rule packs:
- `agents/rules/native-rust-parity-porting.md`
- `agents/rules/native-parity-failure-triage.md`
- `agents/rules/native-68000-safe-instrumentation.md`

Hard requirements:
- Do not use ad-hoc native instrumentation.
- Native debug/assert instrumentation must use the approved debug/assert module.
- Every native parity slice must identify the Rust reference boundary and native boundary before changing production code.
- Every test used as evidence must declare its proof level.
- FS-UAE tests are confirmation gates, not the default inner-loop debugging microscope.
- Reduced fixtures and prefix scans are localization probes only unless their semantic completeness is explicitly documented.
- Do not claim success because a failure moved. A fix requires a named invariant and a focused proof.
```

---

## 6. Rule Pack: Native Rust Parity Porting

### 6.1 Trigger

Load this rule pack when:

- Porting Rust VM/CLI behavior to native 68000/AmigaOS.
- Fixing native behavior expected to match the Rust implementation.
- Adding native support for parser, expression, selector, encoder, output, or source/session behavior.

### 6.2 Required workflow

Before editing production code, create a boundary contract note:

```text
Slice name:
Rust reference files/functions:
Native target files/functions:
Boundary type:
  source reader / tokenizer / parser / statement store / expression request / EXVM result / selector / encoder / output
Contract:
Expected native inputs:
Expected native outputs:
Known non-equivalences:
Proof-level tests:
Fast proof:
FS-UAE proof:
```

### 6.3 Rust-reference requirement

Do not invent native behavior if a Rust behavior already exists.

Allowed native divergence must be limited to:

- memory layout,
- calling convention,
- register pressure,
- fixed-buffer constraints,
- AmigaOS host I/O,
- 68000 control-flow representation.

Divergence must preserve Rust semantics and be documented in the slice.

### 6.4 Boundary ladder

Use the following ladder to find the first Rust/native divergence:

```text
1. Source line read
2. Tokenization
3. Parser / portable AST / statement shape
4. Native statement/session record
5. Expression request envelope
6. EXVM/EXPR parse/eval result
7. Selector candidate
8. Encoder output
9. Session image bytes
10. Output artifact
```

Patch the first divergent boundary only.

---

## 7. Rule Pack: Native Parity Failure Triage

### 7.1 Trigger

Use when a native parity test fails or an FS-UAE run returns unexpected behavior.

### 7.2 Hypothesis ledger

Maintain a ledger during the slice:

| ID | Hypothesis | Evidence for | Evidence against | Status | Next discriminator |
|---|---|---|---|---|---|

Allowed statuses:

```text
open
confirmed
fixed
falsified
invalid test artifact
instrumentation artifact
blocked
```

### 7.3 Reduced fixture rules

Before using prefix scans or reduced fixtures, answer:

```text
Does the reduced fixture preserve all symbols needed by pass 2?
Does it end on a label-only line?
Does it omit later definitions used by earlier forward references?
Does pass-2 behavior mean the same thing as the full fixture?
Is this proof-level E only?
```

### 7.4 Fix claim template

A fix claim must include:

```text
Claimed fixed invariant:
Previous failing evidence:
Production change:
Minimal proving test:
Proof level:
Result:
Remaining failure:
Why the remaining failure is distinct:
Instrumentation removed/kept:
```

"Failure moved" is not a fix claim.

---

## 8. Rule Pack: Native 68000 Safe Instrumentation

### 8.1 Trigger

Use before adding any native debug output, assert, trace, event, or diagnostic in 68000 assembly.

### 8.2 Hard rules

Ad-hoc instrumentation is forbidden.

Instrumentation must use approved macros/routines from the native debug/assert module.

Instrumentation must:

- be behind debug/contract flags,
- preserve all documented registers,
- preserve SR/CCR unless explicitly using a no-flags variant,
- maintain stack balance,
- avoid request/service/last-error buffers,
- use structured event records instead of free-form text where possible,
- have a removal or stabilization plan.

Instrumentation must not:

- be inserted between `cmp`, `tst`, arithmetic/logical flag-setting instructions and a conditional branch,
- inline variable-length logic at call sites,
- print from mutable service/request buffers,
- increase event/request buffers as a diagnostic tactic without explicit approval,
- modify production control flow.

### 8.3 Required instrumentation safety note

Every instrumentation patch must include:

```text
Instrumentation point:
Macro/routine used:
Registers preserved:
SR/CCR preserved:
Stack delta at return:
Shared buffers touched:
Why this cannot change branch decisions:
Removal/stabilization plan:
```

---

## 9. Native Debug/Assert Module Design

### 9.1 Core architecture

Each assert/debug macro expands to a small fixed-size call-site stub that transfers control to a centralized debug/assert module routine.

The macro may only:

- load or push a contract/event ID,
- optionally load documented argument registers,
- call the assert/debug routine,
- restore its own pushed immediate arguments.

The macro must not:

- inline predicate logic,
- emit free-form text,
- touch service/request/last-error buffers,
- branch around local code except through approved fixed-size patterns,
- clobber registers or SR/CCR after returning.

### 9.2 `jsr`, not inline logic

Use `jsr`/`rts` style calls rather than inlined assert bodies.

Example call-site shape:

```asm
    move.w  #CONTRACT_EXPR_REQ_SPAN_VALID,-(sp)
    jsr     debugAssertSpanInText
    addq.l  #2,sp
```

The assert routine owns preservation:

```asm
debugAssertSpanInText:
    movem.l d0-d7/a0-a6,-(sp)
    move.w  sr,-(sp)

    ; evaluate predicate
    ; emit structured event on failure

    move.w  (sp)+,sr
    movem.l (sp)+,d0-d7/a0-a6
    rts
```

### 9.3 Predictable code size

Each enabled assert adds a small fixed amount of code at the call site.

This reduces branch-size churn in native modules.

Disabled behavior can be either:

1. zero bytes in release builds, or
2. fixed-size NOP sleds for layout-sensitive debug/release comparisons.

Default recommendation:

- release: zero bytes,
- debug-contract builds: enabled,
- special layout debugging: optional NOP sled mode.

### 9.4 Assert families

Do not create one routine per contract ID.

Create routines by predicate shape:

```text
debugAssertSpanInText
debugAssertPtrRange
debugAssertNoBufferOverlap
debugAssertStatementIndexValid
debugAssertPassValid
debugAssertResultSlotValid
debugAssertEqualWord
debugAssertEqualLong
debugAssertNonZeroWord
debugAssertZeroWord
```

The contract ID documents the semantic meaning.

The routine implements the generic predicate.

### 9.5 Structured event records

On failure, emit a structured event:

```text
event_kind: ASSERT_FAIL
contract_id: u16
routine_id: u16
statement_index: u16
line_number: u32
arg0: u32
arg1: u32
arg2: u32
arg3: u32
```

Avoid free-form text at assert sites.

Text decoding should happen on the host side when possible.

### 9.6 Debug event records

Passive debug events should follow the same architecture:

```asm
    move.w  #EVENT_EXPR_REQUEST,-(sp)
    jsr     debugEventU32x4
    addq.l  #2,sp
```

Event examples:

```text
EVENT_SOURCE_LINE_READ
EVENT_PRVM_PARSE_BEGIN
EVENT_PRVM_PARSE_OK
EVENT_STMT_STORE
EVENT_EXPR_REQUEST
EVENT_EXPR_RESULT
EVENT_SELECT_REQUEST
EVENT_ENCODE_RESULT
EVENT_ASSERT_FAIL
```

---

## 10. Debug-Contract Asserts

### 10.1 Purpose

Debug-contract asserts are executable documentation for Rust-derived native contracts.

Each assert should document:

```text
Contract ID:
Rust reference:
Native boundary:
Condition:
Failure meaning:
Allowed only in:
Stability:
```

### 10.2 Contract comment template

```asm
; CONTRACT_EXPR_REQ_001
; Rust reference:
;   crates/opforge-vm/src/vm_opasm_parse.rs
;   parse_portable_line_for_assembler
; Native boundary:
;   opasm prepare-evaluate-expression request -> tkpkg/opcore bridge
; Rule:
;   start/end are 1-based source columns; end is exclusive.
;   The selected slice must be fully within the request text.
; Failure means:
;   Native request construction is passing a malformed source window.
    DEBUG_ASSERT_SPAN_IN_TEXT CONTRACT_EXPR_REQ_001
```

### 10.3 Contract stability tags

Each contract must be tagged:

```text
stable
transitional
diagnostic-only
```

Transitional contracts must include removal criteria.

### 10.4 Initial contract ID set

#### Expression request contracts

```text
CONTRACT_EXPR_REQ_001 = expression request span is inside request text
CONTRACT_EXPR_REQ_002 = full operand request uses end = len + 1
CONTRACT_EXPR_REQ_003 = expression request text pointer is non-null when len > 0
CONTRACT_EXPR_REQ_004 = expression request kind matches boundary
```

#### Buffer contracts

```text
CONTRACT_BUF_001 = request buffer does not overlap last-error buffer
CONTRACT_BUF_002 = extension buffer does not overlap IO buffer
CONTRACT_BUF_003 = output/result buffer does not overlap scratch token buffer
```

#### Result-slot contracts

```text
CONTRACT_RESULT_001 = result slot pointer is valid
CONTRACT_RESULT_002 = result slot index matches request
CONTRACT_RESULT_003 = result slot write target is not scratch memory
```

#### Statement/session contracts

```text
CONTRACT_STMT_001 = statement index is valid
CONTRACT_STMT_002 = statement has source line metadata
CONTRACT_STMT_003 = operand bounds are inside source line
CONTRACT_STMT_004 = label-only statement has no mnemonic or operand
CONTRACT_STMT_005 = directive statement has expected directive kind
```

#### Pass/symbol contracts

```text
CONTRACT_PASS_001 = pass number is 1 or 2
CONTRACT_PASS_002 = pass2 label count is consistent with pass1
CONTRACT_PASS_003 = pass2 unresolved label is not reported as trailing text
CONTRACT_PASS_004 = pass1 unresolved label may produce placeholder sizing
```

#### Selector/encoder contracts

```text
CONTRACT_SEL_001 = selected shape comes from package/parser data
CONTRACT_SEL_002 = bare operand direct shape is preserved
CONTRACT_SEL_003 = selected candidate kind matches package candidate kind
CONTRACT_ENC_001 = encoded byte count matches selected plan
```

---

## 11. Python Workflow Scripts

### 11.1 `check_native_porting_slice.py`

Purpose:

Verify that a native porting slice includes required workflow metadata.

Inputs:

- changed files from git,
- optional slice metadata file,
- commit message or staged diff.

Checks:

- If native 68000 files changed, a native porting slice header exists.
- Rust reference boundary is named.
- Native boundary is named.
- Test proof levels are declared.
- FS-UAE tests are not used as the only proof when a host-side proof should exist.
- Reduced fixture tests declare proof-level E unless explicitly justified.

Possible invocation:

```bash
python3 scripts/workflow/check_native_porting_slice.py --staged
```

### 11.2 `check_native_instrumentation_safety.py`

Purpose:

Reject unsafe instrumentation patterns.

Checks:

- No raw debug/print calls outside approved debug/assert macros.
- No known debug calls between flag-setting instructions and conditional branches.
- No direct writes to last-error/request buffers from debug code.
- No new `DEBUG`/`DIAG` labels unless in approved modules or using approved macros.
- Any new instrumentation contract IDs are defined in `debug_contract_ids.asm`.
- Any new contract IDs are documented in `documentation/architecture/native-debug-contracts.md`.

Heuristic branch-safety check:

- Scan assembly for patterns:
  - `cmp*`, `tst*`, `add*`, `sub*`, `and*`, `or*`, etc.
  - followed by debug/assert call
  - followed by `bne/beq/bcc/bcs/blt/bgt/...`
- Fail unless the debug/assert macro is explicitly known to preserve SR/CCR and is marked safe.

### 11.3 `check_native_contract_asserts.py`

Purpose:

Ensure debug-contract asserts are documented and canonical.

Checks:

- Every `CONTRACT_*` used in ASM exists in `debug_contract_ids.asm`.
- Every `CONTRACT_*` exists in `native-debug-contracts.md`.
- Every documented contract has:
  - Rust reference,
  - native boundary,
  - condition,
  - failure meaning,
  - stability tag.
- Assert macros are from the approved list.

### 11.4 `check_fsuae_invocation_policy.py`

Purpose:

Ensure FS-UAE tests follow deterministic invocation policy.

Checks:

- FS-UAE tests use single-instance configuration.
- No fixture parity test requires `OPFORGE_FS_UAE_SMOKE=1` or `OPFORGE_FS_UAE_TESTS=1` unless explicitly allowed.
- Prefix/reduced fixture tests are named and marked as localization probes.
- FS-UAE env variables are documented in the test helper.
- Tests that spawn FS-UAE require `--test-threads=1`.

### 11.5 `run_native_porting_quality_gate.py`

Purpose:

One command to run all native porting workflow checks.

Suggested behavior:

```bash
python3 scripts/workflow/run_native_porting_quality_gate.py --staged
```

Runs:

```text
check_native_porting_slice.py
check_native_instrumentation_safety.py
check_native_contract_asserts.py
check_fsuae_invocation_policy.py
make native-68000-format-check
cargo test focused host-side tests if configured
```

Optional:

```bash
python3 scripts/workflow/run_native_porting_quality_gate.py \
  --staged \
  --fsuae-test external_fs_uae_opforge_native_cli_65c02_expr_syntax_matches_rust_bin
```

---

## 12. Deterministic Workflow Metadata

A native porting slice should include a small machine-checkable metadata block.

Possible file:

```text
target/native-porting-slice.toml
```

Or committed file under plans when the slice is large:

```text
documentation/plans/slices/native-porting-slice-YYYYMMDD-topic.md
```

Example TOML:

```toml
[slice]
name = "65c02 expression syntax directive span parity"
kind = "native-rust-parity"
rust_reference = [
  "crates/opforge-vm/src/vm_opasm_parse.rs::parse_portable_line_for_assembler",
  "crates/opforge-vm/src/vm_opcore.rs::evaluate_expression_for_assembler"
]
native_boundary = [
  "native/motorola68000/amigaos/opasm/opasm_engine.asm::prepareStatementEvaluateExpressionRequestV1"
]
invariant = "Directive expression requests must cover the full operand expression using 1-based exclusive-end source columns."

[[tests]]
name = "motorola68020_native_cli_parse_line_keeps_full_ternary_const_expression"
proof_level = "B"
proves = "Rust-side native parse-line harness keeps ternary expression structure."
does_not_prove = "Real Amiga-native session storage or pass evaluation."

[[tests]]
name = "external_fs_uae_opforge_native_cli_65c02_expr_syntax_matches_rust_bin"
proof_level = "D"
proves = "Real native opforge_cli assembled output matches Rust reference for the unchanged fixture."
does_not_prove = "Individual internal boundary behavior without supporting contract tests."
```

Scripts can validate this structure.

---

## Execution Preflight

Before activating Item 1, run `git status --short` and `git diff --check`,
record the explicit file scope, and identify unrelated modified or untracked
files. This is non-mutating execution setup, not a work item or phase. It does
not produce a commit, and all unrelated changes must remain untouched.

## Work Items

- [ ] Item 1: add workflow documentation and native parity rule packs
  - Source requirement or finding IDs: Sections 3-8
  - Expected files: `AGENTS.md`, native parity rule packs under `agents/rules/`, and `documentation/architecture/native-porting-workflow.md`
  - Full quality gates: `make workflow-gate`; `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for policy and documentation only
  - Commit outcome: `docs: codify native parity porting workflow`
  - Definition of done: proof levels, safe instrumentation policy, and workflow routing are normative and validator-visible

- [ ] Item 2: add the minimal native debug/assert framework and mandatory unit tests
  - Source requirement or finding IDs: Sections 9-10 and the Phase 2 test matrix below
  - Expected files: native debug modules, contract documentation, and focused native debug-contract test harnesses
  - Full quality gates: `make native-68000-format-check`; `cargo test -p asm native_debug_contract_`; `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for the framework skeleton and its positive, negative, boundary, preservation, and build-mode tests
  - Commit outcome: `native: add tested debug contract framework`
  - Definition of done: all mandatory Phase 2 tests pass and release/debug expansion behavior is proven

- [ ] Item 3: add deterministic native-porting validators and validator unit tests
  - Source requirement or finding IDs: Sections 11-12 and the Phase 3 validator matrix below
  - Expected files: native-porting workflow scripts and `scripts/workflow/tests/test_*native*.py`
  - Full quality gates: focused Python unit-test commands below; `python3 scripts/workflow/run_native_porting_quality_gate.py --staged`; `make workflow-gate`
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for deterministic local checks with no default FS-UAE or network launch
  - Commit outcome: `workflow: enforce tested native porting contracts`
  - Definition of done: every validator has passing positive, negative, malformed-input, and deterministic-wrapper tests

- [ ] Item 4: convert one existing diagnostic and add a focused regression test
  - Source requirement or finding IDs: Section 8 and the Phase 4 requirements below
  - Expected files: one selected native diagnostic site, its contract/event documentation, and one focused regression harness
  - Full quality gates: framework unit tests; focused regression test; `make native-68000-format-check`; `scripts/workflow/run_rust_quality_gate.sh`
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for exactly one diagnostic site and no parity bug fix
  - Commit outcome: `native: adopt debug contract framework at one boundary`
  - Definition of done: enabled and disabled behavior, event data, registers, SR/CCR, stack, and branch behavior are proven

- [ ] Item 5: execute the 65c02 expression parity fix as one governed invariant
  - Source requirement or finding IDs: the named FS-UAE failure and the Phase 5 boundary workflow below
  - Expected files: only files named by the approved slice metadata after the first divergent boundary is identified
  - Full quality gates: mandatory host-side regression; applicable boundary/negative tests; `scripts/workflow/run_rust_quality_gate.sh`; named FS-UAE confirmation with `--test-threads=1`
  - Plan-compliance review evidence: `plan-compliance-reviewer` returns `PASS` for one named invariant with classified proof levels
  - Commit outcome: one focused parity-fix commit named after the corrected invariant
  - Definition of done: host-side regression and real FS-UAE parity pass, while temporary instrumentation is removed

### Detailed rollout and test contracts

### Execution preflight details

If the current worktree contains unsafe instrumentation or speculative fixes,
inventory and isolate them without modifying or discarding user-owned changes.

Actions:

```bash
git status --short
git diff --check
```

Definition of done:

- The explicit file scope for Item 1 is recorded.
- Unrelated modified and untracked files remain untouched.
- No destructive reset, restore, or cleanup is authorized by this plan.

### Phase 1: Add workflow documentation and rule packs

Files:

```text
AGENTS.md
agents/rules/native-rust-parity-porting.md
agents/rules/native-parity-failure-triage.md
agents/rules/native-68000-safe-instrumentation.md
agents/rules/native-68000.md
agents/rules/fs-uae.md
documentation/architecture/native-porting-workflow.md
```

Validation:

```bash
make workflow-gate
```

Definition of done:

- Agents are explicitly routed to the new rule packs.
- Native parity work has a required proof-level workflow.
- Unsafe instrumentation is forbidden by policy.

### Phase 2: Add native debug/assert framework skeleton

Files:

```text
native/motorola68000/amigaos/debug/debug_contract_ids.asm
native/motorola68000/amigaos/debug/debug_assert.asm
native/motorola68000/amigaos/debug/debug_events.asm
native/motorola68000/amigaos/debug/debug_macros.i
documentation/architecture/native-debug-contracts.md
documentation/architecture/native-instrumentation-framework.md
```

Initial implementation:

- contract ID definitions,
- structured event record layout,
- one or two assert routines:
  - `debugAssertSpanInText`,
  - `debugAssertNoBufferOverlap`,
- one debug event routine,
- macros that expand to fixed-size call-site stubs.

Validation:

```bash
make native-68000-format-check
cargo test -p asm native_debug_contract_
scripts/workflow/run_rust_quality_gate.sh
```

Definition of done:

- Framework builds.
- Assert macro call-site expansion is fixed-size and documented.
- Module routines preserve all registers and SR/CCR by default.
- Focused tests are mandatory; “tests unavailable” is not an acceptable
  completion state.
- Positive tests cover a passing span, non-overlapping buffers, and one emitted
  four-argument event.
- Negative tests cover an invalid span, overlapping buffers, and an assert
  failure record with the expected contract ID and arguments.
- Boundary tests cover zero-length text, the largest representable valid span,
  adjacent non-overlapping buffers, and the configured event-buffer capacity.
- A register-preservation harness seeds every documented preserved register and
  SR/CCR, invokes each routine, and verifies the exact post-return values and
  zero stack delta.
- A release-build test proves disabled macros emit zero bytes; a debug-contract
  build test proves enabled call-site stubs have the documented fixed size.

### Phase 3: Add deterministic Python workflow checks

Files:

```text
scripts/workflow/check_native_porting_slice.py
scripts/workflow/check_native_instrumentation_safety.py
scripts/workflow/check_native_contract_asserts.py
scripts/workflow/check_fsuae_invocation_policy.py
scripts/workflow/run_native_porting_quality_gate.py
scripts/workflow/tests/test_check_native_porting_slice.py
scripts/workflow/tests/test_check_native_instrumentation_safety.py
scripts/workflow/tests/test_check_native_contract_asserts.py
scripts/workflow/tests/test_check_fsuae_invocation_policy.py
scripts/workflow/tests/test_run_native_porting_quality_gate.py
```

Validation:

```bash
python3 -m unittest discover -s scripts/workflow/tests -p 'test_*native*porting*.py'
python3 -m unittest scripts.workflow.tests.test_check_native_instrumentation_safety
python3 -m unittest scripts.workflow.tests.test_check_native_contract_asserts
python3 -m unittest scripts.workflow.tests.test_check_fsuae_invocation_policy
python3 scripts/workflow/run_native_porting_quality_gate.py --staged
make workflow-gate
```

Definition of done:

- Every validator has table-driven positive and negative unit tests.
- Tests cover malformed and missing slice metadata, every proof level A-E,
  missing `proves`/`does_not_prove` declarations, and justified versus
  unjustified absence of host-side proof.
- Instrumentation-safety tests cover raw debug calls, writes to prohibited
  buffers, unknown debug labels, an instrumentation call between a
  flag-setting instruction and conditional branch, and approved
  SR/CCR-preserving macro usage at the same boundary.
- Contract tests cover used-but-undefined IDs, defined-but-undocumented IDs,
  duplicate IDs, missing required documentation fields, invalid stability
  tags, approved macros, and unapproved assert macros.
- FS-UAE policy tests cover missing single-instance configuration, forbidden
  opt-in environment gates, reduced fixtures without localization metadata,
  missing `--test-threads=1`, and one fully valid invocation.
- Gate-wrapper tests prove deterministic check ordering, non-zero exit on the
  first failed check, propagation of actionable diagnostics, and no FS-UAE or
  network launch in the default `--staged` mode.
- Validator tests use repository-local temporary fixtures and do not depend on
  the caller's dirty worktree.
- Scripts fail on intentionally unsafe fixtures and pass intentionally safe
  fixtures.
- Scripts pass the current repository without rewriting files.
- Scripts are deterministic and do not require network access.

### Phase 4: Convert one existing diagnostic to safe framework

Pick one low-risk debug point.

Actions:

- Replace ad-hoc debug output with `DEBUG_EVENT_*` or `DEBUG_ASSERT_*`.
- Document associated contract ID.
- Run native format gate.
- Add a focused regression test that fails against the pre-conversion behavior
  or structure and passes only when the selected site uses the approved
  framework.
- Run the framework unit-test suite, the focused regression test, the native
  formatter gate, and the full Rust quality gate.

Definition of done:

- First real usage proves framework ergonomics.
- No branch-size or CCR issue appears.
- No service/last-error buffers touched by instrumentation.
- The regression test checks the selected contract/event ID, event arguments,
  and documented proof level.
- A preservation test proves the converted call site does not change live
  registers, SR/CCR, stack balance, or the following branch decision.
- Tests prove both enabled debug-contract behavior and disabled release
  behavior.
- Temporary probes are absent from both production code and fixtures.

### Phase 5: Restart 65c02 expression parity fix using new workflow

Actions:

1. Create slice metadata.
2. Identify Rust reference boundary.
3. Add/enable debug-contract assert at native boundary.
4. Reproduce FS-UAE failure once.
5. Extract exact failing boundary.
6. Add a mandatory fast host-side contract test for the named invariant. If a
   host-side test is technically impossible, stop and amend this plan with the
   concrete reason and replacement proof before production edits.
7. Patch native code.
8. Run host-side proof.
9. Run FS-UAE confirmation.
10. Remove or stabilize instrumentation.
11. Commit one invariant.

Definition of done:

- `external_fs_uae_opforge_native_cli_65c02_expr_syntax_matches_rust_bin` passes.
- The focused host-side regression test fails on the known-bad behavior and
  passes after the production fix.
- The regression test states `This test proves` and
  `This test does not prove`, and its proof level matches the executed path.
- At least one negative or boundary case protects the corrected invariant from
  malformed spans, truncated fixtures, or pass-2 forward-reference artifacts,
  as applicable to the diagnosed boundary.
- The relevant focused test suite and
  `scripts/workflow/run_rust_quality_gate.sh` pass before the FS-UAE
  confirmation is accepted.
- Debug-contract assertions remain only if stable and documented.
- Temporary probes removed.

---

## 14. Commit Strategy

Recommended commits:

### Commit 1

```text
docs: add native parity porting workflow rule packs
```

Contains:

- AGENTS.md routing,
- rule packs,
- rule updates,
- native porting workflow docs.

### Commit 2

```text
native: add debug contract assert framework skeleton
```

Contains:

- native debug module skeleton,
- contract IDs,
- event layout,
- initial safe assert macros/routines.

### Commit 3

```text
workflow: add deterministic native porting quality checks
```

Contains:

- Python workflow scripts,
- integration into existing workflow gate if appropriate.

### Commit 4

```text
native: convert first diagnostic to debug event framework
```

Contains:

- one safe real usage,
- test/format validation.

### Commit 5+

Actual native parity fixes, one invariant per commit.

---

## 15. Acceptance Criteria

The workflow hardening is complete when:

1. Native parity work is routed through dedicated rule packs.
2. AGENTS.md forbids ad-hoc native instrumentation.
3. The debug/assert module exists and has a documented safety contract.
4. Assert/debug macros use fixed-size call-site stubs into module routines.
5. Debug-contract asserts have stable IDs and documentation.
6. Python scripts can deterministically reject:
   - unsafe ad-hoc instrumentation,
   - undocumented contract IDs,
   - missing proof-level metadata,
   - questionable FS-UAE invocation patterns.
7. At least one real native diagnostic/assert has been converted to the framework.
8. The next native parity bugfix can be performed without speculative instrumentation or unclassified tests.

---

## 16. Open Design Questions

1. Should disabled assert macros compile to zero bytes or fixed-size NOP sleds?
   - Recommendation: zero bytes by default, optional fixed NOP mode for layout-sensitive comparison builds.

2. Should assert routines preserve SR/CCR always?
   - Recommendation: yes by default.
   - Add explicit no-flags variants only if truly needed and heavily restricted.

3. Should structured event records include text snapshots?
   - Recommendation: not initially.
   - Prefer IDs, statement index, line number, and numeric args.
   - Decode text from stable session/source storage host-side when possible.

4. Should contract slice metadata live in committed docs or temporary target files?
   - Recommendation:
     - small slices: temporary checked by scripts before commit,
     - large slices: committed plan/slice document.

5. Should Python checks be advisory or hard gates?
   - Recommendation:
     - hard gate for unsafe instrumentation and undocumented contracts,
     - warning/advisory for missing host-side proof where impossible.

---

## Blocking Rules

- no implementation begins until the plan-quality workflow returns `PASS`
- no commit before all gates for the active item pass
- `plan-compliance-reviewer` must return `PASS` before every plan-driven commit
- only one checkbox item may be active at a time
- every implementation item ends in exactly one focused commit before the next begins
- checkbox updates require commit and validation evidence
- unrelated dirty-worktree changes remain untouched
- no destructive reset, restore, or cleanup is authorized by this plan
- archive the completed plan with `scripts/workflow/archive_completed_plan.sh`

## 17. Guiding Rule

If a native parity slice cannot state:

```text
This is the Rust contract.
This is the native boundary.
This is the first divergence.
This assert documents the contract.
This test proves the fix at the right proof level.
```

then the slice is not ready for production edits.
