<!-- SPDX-License-Identifier: GPL-3.0-or-later -->

# VM Boundary & Protocol Specification (v1)

Status: active canonical spec  
Last updated: 2026-05-01

See also:
- [opForge Reference Manual](opForge-reference-manual.md) (Appendix: multi-CPU architecture)
- [VM Ultimate64 ABI Contract (v1)](vm-ultimate64-abi-contract-v1.md)

## 1. Purpose

This document normatively defines the host/VM boundary for the opForge VM integration.
It specifies:
- which responsibilities are host-owned vs VM-owned,
- where the host invokes VM contracts,
- strictness/fallback rules,
- rollout/override controls,
- and wire-level interaction patterns between host orchestration and the VM runtime model.

## 2. Scope

### 2.1 In scope
- Line-level assembly hot path: tokenization, parse envelope dispatch, expression parse/eval, instruction encode.
- Bootstrap path used before pass1/pass2: preprocessing, module graph loading, macro expansion, and module/use scanning.
- Runtime ownership precedence (`dialect -> cpu -> family`) and strictness behavior.
- Host override controls and their effects.

### 2.2 Out of scope
- CLI UX details unrelated to host/VM boundary semantics.
- Non-assembler tools that may consume VM packages (`*.opasm`).
- Future full VM replacement of preprocessor/macro engines.

## 3. Canonical Boundary Matrix

| Stage | Owner | VM Used? | Normative Behavior |
|---|---|---|---|
| Preprocessor file expansion | Host | No | Host `Preprocessor` expands source/includes/defines before module graph assembly. |
| Module graph dependency traversal | Host | No | Host builds graph/orchestration and uses compact hard-coded Rust scanning for bootstrap-only forms such as `.module` and `.use`. |
| Macro expansion | Host | No (engine) | Host `MacroProcessor` performs expansion/injection; VM is not the macro executor. |
| Per-line tokenization in assembler passes | VM | Yes | Per-line processing requires a runtime model and uses the VM tokenization path. |
| Per-line parser envelope | VM | Yes | Per-line parsing validates parser contracts and executes the parser VM envelope. |
| Expression parse/eval on assembly hot path | VM by default | Yes | `EXVM` parses covered mathematical expression token ranges; `EXPR` evaluates compiled portable expression programs. PRVM/opasm retains CPU-family operand-shape ownership. Strict contract/version checks are errors. |
| Instruction candidate resolution/encode | VM-first with strictness | Yes | VM encode path is authoritative for certified families; contract failures are explicit errors. |
| Pass orchestration, symbols, image/list/map output | Host | No | Host controls pass loop, symbol lifecycle, listings/map/hex/bin I/O. |

## 4. High-Level Architecture

```mermaid
flowchart TD
	A[Source Files] --> B[Host Preprocessor]
	B --> C[Host Module Graph Builder]
	C --> D[Host Macro Expansion]
	D --> E[Expanded Lines]
	E --> F[Host Pass1/Pass2 Orchestration]

	subgraph VM_HOT_PATH[VM-Authoritative Line Hot Path]
	  T[VM Tokenizer] --> P[VM Parser Envelope]
	  P --> X[EXVM Expression Parser]
	  X --> PE[EXPR Portable Evaluator]
	  PE --> I[VM Instruction/Directive Encode]
	end

	F --> T
	I --> G[Host Symbol/Image/LST/HEX/BIN/Map Outputs]
```

## 5. Bootstrap Protocol (Host-Orchestrated, Host-Owned Scanning)

Bootstrap entry (`run_one`) performs:
1. host preprocess,
2. host module graph load,
3. pass1/pass2 orchestration.

Within module graph load, host scanners use compact hard-coded Rust logic for
`.module` and `.use` extraction. This bootstrap scanner is intentionally not a
VM contract: project discovery and dependency layout are host policy surfaces
that can differ across C64-style flat projects, Amiga/Hunk projects, and modern
workspace-oriented hosts.

```mermaid
sequenceDiagram
	participant CLI as Host CLI
	participant ASM as Host Assembler(run_one)
	participant BOOT as Host Bootstrap
	participant SCAN as Host module scanner
	participant MP as Host MacroProcessor

	CLI->>ASM: run_one(input)
	ASM->>BOOT: expand_source_file(path, defines)
	BOOT-->>ASM: preprocessed root lines
	ASM->>BOOT: load_module_graph(root_lines)
	loop scan .module/.use
		BOOT->>SCAN: scan bootstrap structural forms(line)
		SCAN-->>BOOT: module/use metadata or diagnostic
	end
	BOOT->>MP: expand deps/root with import visibility
	MP-->>BOOT: expanded lines
	BOOT-->>ASM: ModuleGraphResult{lines, module_macro_names}
```

Normative note:
- The bootstrap scanner must stay narrow: it may recognize structural forms
  needed before pass execution, but it must not become a second pass-time
  statement parser. VM tokenizer/parser contracts remain authoritative for the
  assembler hot path, not for project discovery orchestration.

## 6. Assembly Hot Path Protocol (Host ↔ VM)

For each line in pass1/pass2, host uses VM-first parse/expr/encode contracts.

```mermaid
sequenceDiagram
	participant H as Host AsmLine
	participant TB as Token Bridge
	participant M as Runtime Execution Model
	participant R as Runtime Contracts (TOKS/TKVM/PARS/PRVM/EXVM/EXPR/TABL)

	H->>TB: parse_line_with_model(model, cpu, line)
	TB->>M: tokenize_portable_statement_for_assembler
	M->>R: resolve token policy + tokenizer VM
	R-->>M: portable tokens
	M-->>TB: core tokens + spans
	TB->>M: validate_parser_contract_for_assembler
	TB->>M: resolve_parser_vm_program
	TB->>M: execute parser VM envelope
	M-->>TB: LineAst
	TB-->>H: LineAst

	H->>M: encode_instruction_from_exprs / eval portable expr
	M->>R: resolve EXPR + TABL/MSEL/FORM by owner precedence
	R-->>M: bytes or deterministic error
	M-->>H: emitted bytes / error
```

`PRVM` is the statement and operand-shape parser. Its
`ParseOperandExprRange` opcode crosses into the expression parser contract only
for the pure mathematical token range inside an operand. `EXVM` owns that covered
expression grammar; immediate wrappers, m68k tuple/postincrement/predecrement
forms, register-pair operands, bitfield suffixes, long-indirect brackets, and
other CPU-family operand shapes remain PRVM/opasm responsibilities.

## 7. Ownership and Precedence

All runtime-resolved contracts are owner-scoped and resolved with this precedence:
1. dialect
2. cpu
3. family

This precedence applies uniformly to tokenizer policy/programs, parser contracts/programs, expression contracts, and encode tables/selectors.

## 8. Strictness and Failure Rules

Normative rules:
- Unknown/mismatched VM opcode versions are hard errors.
- Missing required VM program/contract for authoritative path is a hard error.
- Invalid VM output shape (for example empty non-comment token stream where forbidden) is a hard error.
- VM contract/version failures are never interpreted as soft host fallback signals.
- Covered `EXVM` grammar must not silently delegate to host expression parsing.
- Calls and placeholders are explicit compatibility/out-of-scope value nodes,
  not covered `EXVM` grammar; strict execution reports deterministic unsupported
  diagnostics for them.

Determinism requirements:
- Budget ceilings and diagnostics are deterministic for repeated runs over identical inputs.

## 9. Rollout Defaults and Override Controls

### 9.1 Current defaults (v1, active)
- Runtime/package path: authoritative for `mos6502` and `intel8080` families.
- Expression eval path: authoritative for `mos6502` and `intel8080` families.
- Expression parser path: authoritative for `mos6502` and `intel8080` families.
- Parser-VM expression subcalls route covered expression ranges through `EXVM`
  even when the surrounding CPU-family operand shape remains staged.

### 9.2 Host override controls

Environment controls recognized by assembler runtime:
- `OPFORGE_VM_EXPR_EVAL_OPT_IN_FAMILIES`
- `OPFORGE_VM_EXPR_EVAL_FORCE_HOST_FAMILIES`

Rules:
- `FORCE_HOST` disables default expression VM eval for matching family ids.
- `OPT_IN` enables expression VM eval for staged families.
- If both apply, `FORCE_HOST` wins.

Boundary caveat:
- These controls affect expression eval gating only.
- They do not replace host orchestration responsibilities (preprocess/module graph/macro/output orchestration).

## 10. Explicit Host Responsibilities (Non-VM)

The following remain host-owned by specification:
- Filesystem and module discovery.
- Preprocessor include/define expansion.
- Macro expansion and import visibility injection.
- Pass1/pass2 scheduling and line traversal.
- Symbol table lifecycle + diagnostics aggregation.
- Artifact emission (`.lst`, `.hex`, `.bin`, map/export/link outputs).

## 11. Compliance Criteria

An implementation is compliant with this spec when:
- The line hot path uses VM tokenizer/parser/expr/encode for authoritative families.
- Host orchestration boundary remains explicit as defined in Sections 3/10.
- Runtime precedence is `dialect -> cpu -> family`.
- Contract and opcode compatibility checks are enforced at runtime.
- Deterministic limits and diagnostics are preserved.

## 12. Supersession

This document supersedes prior VM boundary notes previously kept under `dev-docs/NextSteps`.
