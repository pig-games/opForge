# libopforge Architecture Specification

**Version:** 0.2-draft  
**Date:** March 9, 2026  
**Status:** Supersedes the earlier v0.1 draft in this branch

> **Draft interface notice:** This document defines the intended architecture and boundary contracts for the library split. Most workspace crate names have now converged on the short-name model, but semantic ownership is still partially transitional.

> **Execution guidance:** The earlier handoff documents are historical records of completed slices. Current follow-on public API work is tracked through the maintained `libopforge` developer guide and the active workflow artifacts, together with `/Users/erik/Code/Retro/opForge/AGENTS.md`.

> **Status note:** The currently published `libopforge` and `opforge-ffi`
> surfaces are pre-1.0 preview interfaces. References below to an eventual
> stable API describe the target state, not a claim that `v0.9.7` has already
> reached it.

---

## 1. Executive Summary

The original libopforge draft assumed a mostly assembler-shaped architecture with a "core" layer below it. That is not the long-term target.

The intended end state is:

- `opcore` is the non-assembler language engine.
- `asm` is the assembler engine.
- `opcore` and `asm` are siblings.
- both consume shared VM services from a lower VM layer.
- both preserve dual-headed execution: a native Rust head and a derived VM head.
- package domains are separated into `.opcore` and `.opasm`.
- the current CPU/family registry is treated as an assembler-specific registry, not as the final generic registry for the whole platform.

This matters because opForge is not only moving toward a library split of the current assembler. It is also moving toward a broader processing platform where assembler is one processor among others. Future processors may include Markdown-like document processing and eventually C-oriented processing. The architecture must therefore avoid hard-coding assembler concepts into the generic library foundation.

The practical consequence is that the library split must separate:

1. shared neutral types and VM foundations,
2. non-assembler language processing (`opcore`),
3. assembler processing (`asm`),
4. assembler-specific registration and family wiring,
5. high-level orchestration and public embedding APIs.

---

## 2. Architectural Principles

| Principle | Meaning |
|---|---|
| `opcore` and `asm` are siblings | Neither is conceptually "under" the other; both sit over shared lower layers |
| Keep assembler concepts out of generic layers | CPU, family, dialect, operand encoding, sections, listings, binaries must not leak into generic platform abstractions |
| VM is shared infrastructure | VM bytecode/runtime/package plumbing sits below both `opcore` and `asm` |
| Dual-headed execution is invariant | The refactor must preserve peer Rust and VM implementations for both `opcore` and `asm`; VM must not become a secondary compatibility path |
| Engine coordinates, processors parse | `engine` owns dispatch contracts and shared context; `opcore`/`asm` own the actual tokenization/parsing/processing routines |
| Package domains are explicit | `.opcore` and `.opasm` are separate domains with separate ownership and validation rules |
| No direct host I/O in library orchestration | Source and output flow through host-provided traits |
| Registry layering matters | The current registry is assembler-specific; a future generic processing registry will sit above processor-specific registries |
| Transitional crates are allowed | During migration, crate names and adapters may temporarily lag behind final conceptual ownership |

---

## 3. Target Layer Model

### 3.1 Conceptual stack

```text
                host applications
      (CLI, LSP, IDEs, GUI tools, FFI hosts, WASM)
                          |
                      libopforge
                          |
                       engine
                    /              \
                   opcore         asm
                    \              /
                     \            /
                           vm
                     /      |       \
               types    package IO   shared syntax/value contracts
```

### 3.2 Future expansion

The longer-term platform may grow into:

```text
              generic processing registry
             /            |             \
      assembler registry  markdown ...   c/other
             |
        asm families/cpus/dialects
```

This future layer is not a first implementation milestone, but current design choices must not prevent it.

---

## 4. Crate Layout

The crate names in the workspace now largely match the intended architecture:

```text
libopforge/
├── Cargo.toml
├── crates/
│   ├── types/        (package name `types`, directory still `opforge-types/`)
│   ├── vm/           (package name `vm`, directory still `opforge-vm/`)
│   ├── opcore/       (package name `opcore`, directory still `opforge-core/`)
│   ├── asm/          (package name `asm`, directory still `opforge-asm/`)
│   ├── registry/     (package name `registry`, directory still `opforge-registry/`)
│   ├── families/     (package name `families`, directory still `opforge-families/`)
│   ├── engine/       (package name `engine`, directory still `opforge-engine/`)
│   ├── api/          (transitional internal facade; the current published contract is the `libopforge` module layout)
│   ├── formatter/    (directory still `opforge-formatter/`)
│   ├── lsp/          (directory still `opforge-lsp/`)
│   ├── cli/          (directory still `opforge-cli/`)
│   ├── cli-core/     (directory still `opforge-cli-core/`)
│   └── ffi/          (directory still `opforge-ffi/`)
└── libopforge/       (root facade package)
```

### Transitional note

Directory names still carry the older `opforge-*` prefix in many cases. That is now mostly a filesystem/layout detail, not a statement about public crate naming.

In particular:

- current `opcore` is still a mixed language-plus-assembler front end in some areas,
- current `vm` still contains assembler-facing concerns,
- current `registry` should still be interpreted as the assembler registry layer,
- current root crate is now the curated published preview facade, not the main implementation owner.

---

## 5. Responsibilities by Layer

### 5.1 `types`

Shared neutral types only:

- diagnostics
- source locations and source maps
- symbol/value models that are not assembler-specific
- generic result/status payloads
- package metadata structs that are not processor-specific

`types` must not own:

- CPU/family/dialect concepts
- instruction encoding
- binary/listing/map emission
- assembler section/placement semantics

If a type needs CPU- or assembler-specific knowledge, it belongs elsewhere.

### 5.2 Shared syntax/value contracts

Both `opcore` and VM code need a shared representation for things such as:

- spans
- tokens, where they are truly processor-neutral
- expression AST or a portable expression contract
- statement contract payloads, where they are not assembler-specific

This shared contract may live in:

- a new `syntax` crate, or
- carefully scoped modules inside `types`

but it must not force `vm -> opcore` as a permanent dependency edge.

### 5.3 `vm`

Shared VM infrastructure:

- bytecode/runtime execution
- package container reading/writing
- contract/version validation
- shared VM diagnostics
- host/runtime bridging helpers that are processor-neutral

`vm` may internally host distinct subdomains:

- shared VM substrate
- VM support used by `opcore` (`.opcore` VM)
- VM support used by `asm` (`.opasm` VM)

but those must remain clearly separated in ownership even if they initially live in one crate.

`vm` must not become an assembler policy layer.

### 5.3.1 Dual-headed processing rule

For both processor domains, Rust and VM are peer implementations of the same processing model.

That means the correct long-term shape is:

- `opcore`
  - Rust implementation
  - `.opcore` VM implementation
- `asm`
  - Rust implementation
  - `.opasm` VM implementation
- `engine`
  - coordinates one head or both heads

The refactor must not collapse the architecture into:

- Rust as the only real implementation with VM as fallback, or
- VM as an assembler-only special subsystem.

### 5.3.2 Lockstep parity mode

After the basic refactor is complete, the architecture should support an optional lockstep mode where:

1. Rust and VM implementations of a processing stage run in parallel,
2. their normalized results are compared after each step,
3. any divergence is logged with enough context to reproduce and diagnose it.

To support that, processing stages must expose comparable checkpoints for:

- request/span input
- produced tokens
- parsed AST or portable AST
- diagnostics
- emitted output contribution
- important intermediate runtime decisions where needed

Lockstep is a validation/debugging mode, not the default execution path.

### 5.3.3 Lockstep mechanism

The lockstep mechanism is defined in terms of execution modes, stage checkpoints,
normalization rules, and divergence records.

#### Execution modes

The engine-facing execution model must support three modes:

- `RustOnly`
- `VmOnly`
- `Lockstep`

`Lockstep` is validation-oriented. It runs both heads for the selected stage,
compares normalized checkpoints, logs divergences, and then continues with a
configured continuation head.

That continuation head is a runtime policy, not an architectural statement that
one head is semantically authoritative.

#### Required runtime structures

Milestone J should implement runtime structures equivalent to:

- `ExecutionMode`
- `ContinuationHead`
- `LockstepStage`
- `LockstepCheckpoint`
- `LockstepDivergence`
- `LockstepReport`

Exact Rust type names may vary, but the roles must exist.

#### Checkpoint categories

| Category | Produced by | Required fields | Normalization rule |
|---|---|---|---|
| Request | engine before stage dispatch | processor id, request kind, source id, line/span, active cpu/dialect, stage id | compare exact ids after normalizing case for processor/cpu ids |
| Token stream | tokenization stages | token kind, normalized text/value, span | compare normalized portable-token view; ignore irrelevant host-only token metadata |
| AST | parse stages | stage-specific AST payload | compare normalized stage AST, not raw parser internals |
| Diagnostics | every stage | code/key, severity, primary span, stable parameters | compare normalized diagnostic record; rendered human message is secondary |
| Emitted output | encoding/output stages | bytes and stable metadata | compare bytes exactly; compare listings/maps only after line/run normalization |
| Runtime decision | selector/encoding stages where needed | selected mode/program/candidate ids, force state, budget profile | compare normalized decision record when the stage can diverge without changing bytes yet |

#### AST comparison policy

AST comparison is stage-specific:

- `ModuleItem` and core-structural stages compare normalized core AST.
- expression parsing compares normalized core expression AST.
- assembler statement parsing compares normalized portable line AST first.
- where a stage naturally yields both a portable AST and a core AST, Milestone J may compare both, but one primary comparison artifact must be chosen per stage.

Near-term primary AST choices are:

- `Opcore(ModuleItem)` -> normalized core AST
- `Opcore(Expr)` -> normalized core expression AST
- `asm statement parse` -> normalized `PortableLineAst`

#### Diagnostic comparison policy

Lockstep diagnostics must compare:

- stable diagnostic code/key
- severity
- primary span
- stable parameter payload, if present

Rendered human message text should not be the primary comparison artifact.
It may be included in divergence logs for debugging.

#### Emitted-output comparison policy

Output comparison is split into incremental and end-of-stage classes:

- incremental:
  - emitted instruction/data bytes
  - selector/encoding decisions
  - per-line portable AST or parse result
- end-of-stage:
  - listing text
  - map text
  - linker/export-section payloads
  - dependency/metadata files

Incremental artifacts should be compared as early as possible.
Human-oriented formatted outputs may be compared after stable normalization at
the end of the relevant stage or run.

#### Engine orchestration rule

For a lockstep-enabled stage, `engine` must:

1. create a single request context,
2. dispatch it to the Rust head,
3. dispatch the same logical request to the VM head,
4. normalize both checkpoints,
5. compare them,
6. record either a match or a divergence,
7. continue with the configured continuation head.

The first implementation may continue with the Rust head by default, but that
must remain a runtime policy choice, not a semantic design assumption.

#### Divergence record

A divergence record must minimally include:

- stage id
- processor domain (`opcore` / `asm`)
- request kind
- continuation head
- source id and line/span
- active cpu/dialect, if relevant
- normalized left checkpoint
- normalized right checkpoint
- comparison category
- stable mismatch reason/code

Optional but recommended fields:

- rendered human diagnostics
- raw Rust payload excerpt
- raw VM payload excerpt
- package/runtime model identifier
- request trace id

#### First lockstep implementation target

Milestone J should implement the first real lockstep stage on a narrow,
already-runnable stage:

- preferred first target: `Opcore(Expr)`
- acceptable second target: `asm statement parse`

Do not begin with formatted outputs or full assembly-run lockstep.

### 5.4 `opcore`

This is the non-assembler language engine.

It owns language features such as:

- expressions
- modules and `.use`
- conditionals
- repetitions and collections
- macro and segment expansion
- statement-definition and statement-expansion features
- scope rules that are part of the language rather than machine-code assembly

It does not own:

- CPU selection
- operand parsing for instructions
- instruction encoding
- section placement for machine images
- listing/hex/bin/map output
- assembler-oriented directives like `.org`, `.byte`, `.word`, `.align` when used for machine-code emission

`opcore` is backed by `.opcore` packages.

### 5.5 `asm`

This is the assembler engine.

It owns:

- assembler directives for data/layout/output
- CPU/dialect selection
- instruction parsing and encoding dispatch
- assembler-specific tokenization/parsing overlays
- sections, regions, placement, packing
- binary image assembly
- listing/map/hex/bin generation policy
- output payload construction

`asm` is backed by `.opasm` packages and assembler-family integration.

### 5.6 `registry`

Assembler-only registry layer:

- families
- CPUs
- dialects
- operand-set contracts
- assembler runtime metadata/capabilities needed for instruction processing

This is not the final generic platform registry.

Any future generic registry must live above this layer and must not expose assembler-only concepts as its public vocabulary.

### 5.7 `families`

Concrete assembler family/CPU implementations:

- intel8080, mos6502, m6800, etc.
- CPU extensions and dialect modules
- family registration helpers

This layer is feature-gated.

### 5.8 `engine`

Top-level orchestrator for a single run.

Responsibilities:

- processor/session construction
- package loading and VM wiring
- coordinating `opcore` and `asm`
- coordinating Rust-head vs VM-head execution selection
- owning processor hand-off contracts and dispatch policy
- host-facing source/output trait plumbing
- structured result/error production
- later owning optional Rust/VM lockstep comparison mode

The engine is above the sibling processors. It is not itself the home of language or assembler semantics.

The engine must not become a giant monolithic parser. Its role is to:

- hold shared source/cursor/diagnostic context,
- route explicit processor requests,
- resolve fallback ownership when a processor yields "unknown",
- return control to the requesting processor with the updated cursor/result.

#### Output-base selection contract

`engine` resolves the output base through `resolve_output_base`
(`crates/opforge-vm/src/output_model.rs`).  The stabilized rules are:

1. `output_base` is the caller-supplied output base (typically the input path
   stripped of its extension).  It is always the starting point when no
   higher-priority selector applies.
2. Without `out_dir`, precedence is: `outfile_override` → `.meta.output.name`
  (metadata output name) → `output_base`.
3. With `out_dir`, the directory portion of the selected base is replaced by
   `out_dir`.  Only the final file-name component (stem) is preserved — even
  when `output_base` is an absolute path.

This contract is verified by unit tests in `output_model.rs`.

### 5.9 `libopforge`

Public embedding API:

- builders/configuration
- source/output provider traits
- convenience in-memory and filesystem adapters
- structured assemble/check entry points
- capability queries

The first stable public API may still be assembler-centric, but it must not lock out future non-assembler processors.

The stable public module map is:

- `libopforge::asm`
- `libopforge::asm::opasm`
- `libopforge::opcore`
- `libopforge::diagnostics`
- `libopforge::io`
- `libopforge::processing`
- `libopforge::registry`
- `libopforge::lockstep`
- `libopforge::formatter`

---

## 6. Package Domains

Package ownership must be explicit.

### 6.1 `.opcore`

Owned by `opcore`.

Expected scope:

- expression/runtime helpers
- statement-expansion semantics
- module/import behavior support
- language-level processing contracts

### 6.2 `.opasm`

Owned by `asm`.

Expected scope:

- instruction selector data
- operand/encoding contracts
- family/cpu/dialect metadata used for machine-code assembly

### 6.3 Shared package plumbing

Container loading, byte encoding, schema versioning, and shared validation infrastructure may live in `vm`, but semantic ownership of package contents remains with the processor layer that consumes them.

---

## 7. Registry Model

### 7.1 Current reality

The current `ModuleRegistry` model is assembler-specific. It talks about:

- families
- CPUs
- dialects
- operand sets
- encode candidates

That is valid for assembler, but not for the future platform.

### 7.2 Required architecture rule

No new public architecture text should describe the current registry as a generic "processing registry".

Instead:

- current registry work should be documented as `asm` registry work,
- future processor-neutral registration should be treated as a later layer above processor-specific registries.

### 7.3 Future generic processing registry

The future generic registry may expose concepts such as:

- processor id
- accepted document/input kinds
- package domains required
- capabilities metadata
- processor factory/session creation

It should not expose:

- CPU family ids
- dialect ids
- instruction operand forms

---

## 8. Parsing and Front-End Boundaries

One of the main risks in the current branch is that the extracted parser still mixes language and assembler concerns.

### 8.1 Desired split

There should ultimately be three levels of front-end logic:

1. shared lexical/expression contracts, where truly neutral,
2. `opcore` statement parsing,
3. `asm` statement parsing and assembler directive handling.

This does **not** imply one permanent shared parser with processor-specific branches inside it.

The desired architecture is:

- processor-owned subparsers,
- engine-owned hand-off contracts,
- shared lexical/cursor infrastructure only where genuinely neutral.

### 8.2 Processor request model

Processors must be able to make explicit processing requests to `engine`.

Examples:

- `asm` encounters an instruction operand and explicitly requests `EXPR`
- `opcore` encounters an embedded processor region and explicitly requests another processor capability
- a future markdown or C processor may request `EXPR`, `MODULE_ITEM`, or other processor-defined subprocessing kinds

The engine then:

1. receives the request kind and current parse context,
2. delegates according to current routing policy,
3. receives either a completed result, a returned follow-up request, or an error,
4. continues routing until the work is complete or fails.

This allows fine-grained cooperation such as:

- `asm` owning instruction and operand structure,
- `opcore` owning expression parsing within those operands,
- `engine` coordinating the transition without owning the parse semantics itself.

### 8.3 Return-and-resume model

Some processor flows will not always know what they are looking at, or they may intentionally stop once they reach a subspan owned by another processor.

In those cases a processor should be able to return control to `engine` together with the next request that must be handled.

The engine may then resume processing, for example by:

- first applying its default routing rule,
- consulting the current processor/domain context,
- trying processors in a prioritized order,
- asking whether a processor can tokenize/parse the span under the current contract,
- delegating to the first processor that claims the span.

For the near-term parser split, the default routing rule should be:

1. `engine` delegates to `opcore` first,
2. if `opcore` returns `Unknown`, `engine` delegates to `asm`,
3. later, `engine` may expand this into a broader prioritized processor search.

This fallback search is likely not required for every first assembler milestone, but the architecture must reserve room for it because future multi-processor documents will need it.

### 8.4 Practical routing levels

Processor dispatch may happen at multiple granularities:

- whole file / package domain (`.opcore`, `.opasm`)
- line / statement
- operand / argument
- expression
- future embedded regions in mixed documents

The architecture should support all of these, even if implementation starts with only a subset.

### 8.5 Source-level disambiguation note

Future source forms should be allowed to declare processor boundaries explicitly when ambiguity would otherwise exist.

Examples may eventually include:

- explicit processor-region markers,
- processor-qualified directives,
- processor-qualified embedded blocks.

A strong candidate, building on the existing `[{ ... }]` boundary shape, is:

```text
[<processor id>{ ... }]
```

Examples:

```text
[<opcore>{ value + 1 }]
[<asm>{ lda value + 1 }]
[<markdown>{ # title }]
```

This shape is attractive because:

- it extends a boundary syntax the system already conceptually uses,
- it makes the target processor explicit at the source level,
- it works naturally for fine-grained embedded regions,
- it is compatible with a future plugin model where `<processor id>` is not known to the engine at compile time.

This is not required for the first implementation of the hand-off contract, but the architecture should reserve room for it so sources can opt out of heuristic routing where needed.

### 8.6 Examples

These belong to `opcore`:

- module/import syntax
- macro/segment definitions and expansion
- repetition constructs
- general expression forms
- conditionals, where they are language constructs rather than output-layout directives

These belong to `asm`:

- `.org`
- `.byte`, `.word`, `.text` when emitting machine data
- `.align` for assembled layout
- `.section`, `.place`, `.pack`
- CPU-selection directives
- instruction statements
- assembler-specific register-aware tokenization overlays

### 8.7 Processing flow examples (spec tests)

The following examples are normative architecture scenarios for the current assembler feature set.

They are called "spec tests" here because each one should eventually be backed by an automated test or traceable integration check.

#### Flow A — Instruction operand expression

Source:

```asm
    lda value + 1
```

Expected flow:

1. `engine` delegates the line to `opcore` by default.
2. `opcore` cannot classify `lda value + 1` as a core-owned statement and returns control to `engine` with a processor-scoped assembler request.
3. `engine` delegates the same line/span to `asm`.
4. `asm` recognizes `lda` and begins operand parsing.
5. `asm` determines that the operand requires an `Expr` subrequest.
6. `asm` returns control to `engine` with an `Expr` request and the current cursor/boundary.
7. `engine` delegates that request to `opcore`.
8. `opcore` tokenizes/parses `value + 1` until the operand boundary.
9. `engine` returns the parsed expression to `asm`.
10. `asm` resumes operand/instruction parsing and completes the line.

Pass criteria:

- default routing is `engine -> opcore` first,
- `opcore` can explicitly return control with a follow-up request,
- `asm` owns instruction shape and operand count rules.
- `opcore` owns the expression AST and expression diagnostics.
- `engine` owns only the request routing and context transfer.

#### Flow B — Data directive with expression list

Source:

```asm
    .byte base, base + 1, target - start
```

Expected flow:

1. `engine` delegates the line to `opcore` by default.
2. `opcore` returns control to `engine` with a processor-scoped assembler request for `.byte ...`.
3. `engine` delegates the line to `asm`.
4. `asm` recognizes `.byte` as an assembler emission directive.
5. For each comma-separated argument, `asm` returns an `Expr` request through `engine`.
6. `engine` delegates each expression request to `opcore`.
7. `opcore` parses each expression up to the comma or end-of-line boundary.
8. `asm` receives each parsed expression and builds the directive AST/payload.

Pass criteria:

- default routing falls through `opcore` before `asm`,
- argument splitting belongs to `asm`,
- expression parsing belongs to `opcore`,
- repeated hand-off over a single line is supported.

#### Flow C — Assembler directive with embedded expression option

Source:

```asm
    .place code in ram, align = page_size * 2
```

Expected flow:

1. `engine` delegates the line to `opcore` by default.
2. `opcore` returns control to `engine` with a processor-scoped assembler request for `.place ...`.
3. `engine` delegates the line to `asm`.
4. `asm` recognizes `.place` and parses the assembler-owned directive structure (`section`, `in`, `region`, option keys).
5. When `asm` reaches `align =`, it returns an `Expr` request.
6. `engine` delegates that expression request to `opcore`.
7. `opcore` parses `page_size * 2`.
8. `engine` returns the expression result.
9. `asm` completes `.place` parsing.

Pass criteria:

- default routing falls through `opcore` before `asm`,
- `.place` ownership remains entirely in `asm`,
- only the option value expression is delegated,
- expression errors return through `engine` to the assembler directive context.

#### Flow D — Core-owned statement in assembler-oriented source

Source:

```asm
    .use math as m
```

Expected flow:

1. `engine` receives the line in an assembler-hosted session.
2. `engine` delegates to `opcore` first by default.
3. `opcore` recognizes `.use` as core-owned and parses it directly.
4. `engine` returns the parsed core-language result to the surrounding session.

Pass criteria:

- `.use` is not permanently implemented as an assembler semantic feature,
- the hand-off supports core-language items appearing inside an assembler-oriented workflow.

#### Flow E — Module declaration routing

Source:

```asm
.module demo
```

Expected flow:

1. `engine` delegates to `opcore` first by default.
2. `opcore` recognizes `.module demo` as `ModuleItem` processing.
3. `opcore` parses `.module demo`.
4. `engine` resumes the surrounding orchestration with the returned module AST/result.

Pass criteria:

- `opcore` owns module syntax,
- this works even when the overall assembly session later returns to assembler-owned lines.

#### Flow F — Statement-definition / macro-style structural hand-off

Source:

```asm
    .statement op byte:lhs "," [{ word:rhs }]
```

Expected flow:

1. `engine` delegates to `opcore` first by default.
2. If `opcore` owns statement-definition parsing, it parses the directive and returns the result directly.
3. If `opcore` returns control with a follow-up request, `engine` delegates to `asm` or another configured processor.
4. If `asm` becomes the wrapper owner, it must explicitly delegate any core-owned signature/body subprocessing request back through `engine`.
5. The returned result is stored in the owning processor’s macro/statement system.

Pass criteria:

- statement-definition ownership is explicit,
- signature parsing is not duplicated independently in multiple processors,
- body ownership follows the chosen processor boundary consistently.

#### Flow G — Unknown-span fallback search

Source pattern:

```text
processor A encounters a span it cannot classify under the current contract
```

Expected flow:

1. `engine` delegates to `opcore` first by default.
2. `opcore` returns control to `engine` with a follow-up request or an unknown span.
3. `engine` delegates to `asm` as the next configured processor.
4. In later multi-processor configurations, `engine` may continue with a prioritized processor search.
5. The first processor that claims the span receives the parse request.
6. If no processor claims it, `engine` returns a stable diagnostic.

Pass criteria:

- fallback search is explicit and deterministic,
- processor priority is configurable/documented,
- `engine` does not silently absorb unknown syntax as its own semantics.

### 8.8 Draft processor contracts

The exact Rust API may change, but the architecture should move toward contracts in this shape:

```rust
pub enum OpcoreRequestKind {
    Expr,
    ModuleItem,
    Statement,
}

pub enum ProcessingRequestKind {
    Opcore(OpcoreRequestKind),
    Processor {
        processor: String,
        kind: String,
    },
}

pub struct ProcessingContext<'a> {
    /* shared source/cursor/diagnostics/session state */
}

pub enum ProcessingReturn {
    Request {
        request: ProcessingRequestKind,
        /* resume cursor / boundary / partial state */
    },
    Unknown,
}

pub enum ProcessingOutcome<T, E> {
    Done(T),
    Return(ProcessingReturn),
    Error(E),
}
```

This is intentionally not a closed enum over every future processor and request kind.

The architecture should assume:

- a stable set of built-in `opcore` request kinds is acceptable,
- processor-specific request kinds should be carried as `(processor, kind)` identifiers,
- future processors such as `asm`, markdown, C, or plugins must not require editing a closed global enum just to participate in hand-off.

Current implementation note:

- the live engine path currently uses `Opcore(Statement)`, `Opcore(Expr)`, and `Opcore(ModuleItem)`;
- `.module` / `.use` scanning on the source-graph path now routes through the explicit `ModuleItem` request rather than relying only on direct parser-helper scans.
- processors should be able to work until they cannot continue, then return the next request to `engine`,
- `Unknown` is only one kind of return path; the more general case is resumable hand-off.

The important design rule is semantic:

- processors own the parsing/processing logic for their request kinds,
- `engine` owns routing and fallback policy,
- shared lexical infrastructure does not imply a single mixed parser owner.

For a plugin-oriented future, the string identifiers may later be wrapped in stronger types or registries, but the contract should remain open-ended rather than exhaustively enumerated.

### 8.9 Transitional allowance

The parser may remain shared temporarily, but the spec target is semantic separation, not permanent cohabitation.

---

## 9. Public Rust API Direction

The initial stable API can remain assembler-first, but it should be shaped so that a future generic processing API can sit beside or above it.

### 9.1 Current published preview Rust embedding boundary

The current published preview Rust embedding boundary is assembler-first and
runs through the root `libopforge` facade and its current module map.

What is implemented today:

- the normal embedding path is `libopforge::asm::Assembler` with `libopforge::asm::AssemblerConfig`,
- free `prepare(...)` and `assemble(...)` helpers remain available,
- filesystem-backed defaults and in-memory `SourceProvider` / `OutputSink`
  adapters are both supported,
- execution mode selection (`Rust`, `Vm`, `Lockstep`) is part of the public
  host surface,
- formatter access and capability reporting now live under the published facade
  modules rather than a separate overflow namespace.

This surface is functional, published, and documented as the current
module-partitioned pre-1.0 host API for the current branch. It should not yet
be treated as a defended stable contract.

### 9.2 Near-term API redesign direction

The current preview Rust layout is:

- `libopforge::asm` is the high-level assembler-oriented API,
- `libopforge::asm::opasm` is the lower-level assembler processor API,
- `libopforge::opcore` is the sibling lower-level non-assembler processor API,
- `libopforge::formatter` is the current formatter API,
- `Assembler` is implemented on top of `engine`, `opasm`, `opcore`, and shared
  current cross-cutting modules such as diagnostics, I/O, registry, processing,
  and lockstep,
- higher-level APIs dog-food lower-level published APIs rather than bypassing
  them
  with private parallel entrypoints.

### 9.3 Implemented assembler-first API base shape

The public Rust API is no longer only a sketch. The implemented base shape is:

- `libopforge::asm::Assembler`
- `libopforge::asm::AssemblerConfig`
- `libopforge::asm::OwnedAssemblerConfig`
- `libopforge::asm::AssemblerSessionBuilder`
- `libopforge::asm::AssemblerSession`
- `libopforge::asm::PreparedAssemblySession`
- `libopforge::io::SourceProvider`
- `libopforge::io::OutputSink`
- filesystem-backed and in-memory I/O adapters
- explicit execution-mode selection for Rust, VM, and lockstep runs
- module-qualified imports at the root facade rather than a flat root export bag

Representative usage shape:

```rust
use std::path::Path;

use libopforge::asm::Assembler;

let report = Assembler::builder(Path::new("examples/helloworld.asm"))
    .execution_mode(libopforge::lockstep::ExecutionMode::Rust)
    .assemble()?;
```

For non-borrowing hosts, the owned/session API mirrors the same configuration
concerns through `OwnedAssemblerConfig`, `OwnedSourceOptions`,
`OwnedExecutionOptions`, and `OwnedOutputOptions`, but the preferred entry path
is still builder-first.

Representative owned-session usage shape:

```rust
use libopforge::asm::{AssemblerSession, ExecutionMode};

let report = AssemblerSession::builder("examples/helloworld.asm")
  .output_base("examples/helloworld")
  .execution_mode(ExecutionMode::Vm)
  .assemble()?;
```

The current libopforge API Aesthetics Improvement Plan is therefore not about
inventing the first public API. It is about refining the implemented base shape
into a cleaner and more strongly partitioned public surface.

The recommended root-facade import style is module-first:

- `libopforge::asm::Assembler`
- `libopforge::asm::AssemblerConfig`
- `libopforge::io::MemorySourceProvider`
- `libopforge::diagnostics::Diagnostic`

The root crate should not be treated as a flat re-export bag for these items.

### 9.4 API rule

These APIs must be implemented above the semantic split. They must not directly
expose root-crate internals or assembler-only registry details as part of the
published preview boundary.

### 9.5 Current preview facade policy

The current published preview Rust embedding boundary is assembler-first and
runs through the root `libopforge` crate and its current module layout.

This branch intentionally applies the libopforge API-aesthetics rename set as a
source-breaking cleanup. The old public names are not kept as compatibility
aliases in the preview facade.

- `libopforge` is a curated facade, not an escape hatch for all workspace crates.
- Types and functions exposed through `libopforge::asm`, `libopforge::opcore`, `libopforge::diagnostics`, `libopforge::io`, `libopforge::processing`, `libopforge::registry`, `libopforge::lockstep`, and `libopforge::formatter` define the current published pre-1.0 Rust host surface.
- Lower-level crates such as `asm`, `engine`, `vm`, `registry`, `families`, `formatter`, `lsp`, and `opcore` remain implementation crates or advanced dependencies, not part of the root facade contract.
- Consumers that need those lower-level crates should depend on them explicitly and treat them as more transitional than the curated `libopforge` preview modules.

This resolves the first public-boundary choice for the current slice: the root
crate remains assembler-oriented today rather than introducing a generic
processor-selection surface prematurely.

### 9.6 Current C ABI slice and validation status

The workspace also exposes a narrow C ABI through the optional `ffi` crate.
That layer is intentionally thin over the same `libopforge` assembly path used
by Rust hosts.

This is the current implemented FFI slice, not the final host-facing SDK shape.
The intended sequence is:

1. redesign and harden the Rust API,
2. then widen the C ABI to mirror that Rust API cleanly,
3. without creating a second independently-shaped orchestration layer.

Current exported ABI groups:

- `opforge_asm_*`
- `opforge_opcore_*`
- `opforge_opasm_*`
- `opforge_diag_*`
- `opforge_io_*`
- `opforge_processing_*`
- `opforge_lockstep_*`
- `opforge_registry_*`

High-level FFI note:

- the current assembler-oriented preview FFI surface is `opforge_asm_request`
  plus the
  `opforge_asm_*_with_request(...)` entrypoints.
- this branch intentionally renames the grouped request fields to
  `output_base` and `no_outputs`; the C ABI layout is preserved, but C/C++
  consumers using designated initializers or direct field access must update to
  the final names.

Current request/result contract:

- `root_path` is required and must be a valid NUL-terminated UTF-8 string.
- `output_base` and `out_dir` are optional NUL-terminated UTF-8 strings; null or empty values fall back to library defaults.
- `execution_mode` crosses the ABI as a validated `u32` scalar using the `OPFORGE_EXECUTION_MODE_*` constants declared in `crates/opforge-ffi/src/lib.rs` and mirrored in `crates/opforge-ffi/opforge.h`.
- Unknown `execution_mode` values return `InvalidRequest` instead of being interpreted as Rust enums through the ABI.
- `emit_outputs` is a `u8` flag inside `opforge_asm_output_options`.
- in-memory high-level `check` entrypoints only require callbacks when
  buffered outputs actually exist; setting `emit_outputs` does not by itself
  make `write_file` a precondition for a no-output `check` path.
- in-memory high-level `assemble` entrypoints fail with `InvalidRequest` when
  the operation actually buffers outputs but the caller did not provide output
  callbacks to receive them, including directive-driven or metadata-driven
  outputs that arise even when `emit_outputs` is zero.
- `no_outputs` is the explicit way to prevent those directive-driven or
  metadata-driven buffered outputs for diagnostics-only in-memory runs.
- `opforge_asm_report_message()` and the string-returning
- `opforge_diag_*_from_asm_report(...)` accessors instead return pointers
  borrowed from `opforge_asm_report`; they remain valid until
  `opforge_asm_report_free()` and must not be freed by the caller.
- `OpforgeStatus` remains a small result enum for Rust consumers and is mirrored in the manual C header as `opforge_status` / `OPFORGE_STATUS_*`.
- high-level `opforge_asm_*` reports now support rich `opforge_diag_*`
  enumeration for diagnostic code, file, related-span, help, and fix-it data
  in addition to the primary severity/message/span fields.
- the preferred grouped high-level FFI request mirrors the current Rust config
  families explicitly through `opforge_asm_source_options`,
  `opforge_asm_execution_options`, `opforge_asm_output_options`,
  `opforge_asm_diagnostics_options`, and the top-level
  `opforge_asm_request`.
- `opforge_asm_execution_options` includes the current Rust request-scoped
  execution override slice: `cpu_override`, `max_loop_iterations`, and
  `opasm_package_path`.
- `opforge_asm_output_options` includes the current Rust output override slice:
  `go_addr`, textual `bin_specs`, `fill_byte` with `fill_byte_set`, and
  `no_outputs`, in addition to the existing default-output and metadata
  path controls.
- grouped FFI `label_output_format = OPFORGE_LABEL_OUTPUT_FORMAT_DEFAULT` now
  means "use the current Rust facade default" rather than the renderer's
  separate `LabelOutputFormat::Default` variant; today that current default is
  VICE-style text labels.

Header and review policy:

- `crates/opforge-ffi/opforge.h` is manually maintained for this slice rather than generated.
- Any ABI-affecting change must update both the Rust definitions in `crates/opforge-ffi/src/lib.rs` and the checked-in header in the same review.
- `crates/opforge-ffi/tests/abi_contract.rs` exists to exercise the exported boundary and help catch drift between the Rust facade and consumer-facing expectations, including a C compile/static-assert check against `opforge.h` when a C compiler is available.
- CI should keep a C compiler available so this header drift check remains enforced rather than degrading to a skip.

Rust facade policy:

- `libopforge` is the published pre-1.0 public Rust facade.
- Advanced implementation crates remain explicit workspace dependencies rather than being re-exported through an overflow namespace in `libopforge`.

Current validation strategy:

- `crates/opforge-lib/src/lib.rs` carries focused in-memory facade tests for prepare/assemble/check plus Rust, VM, and lockstep execution paths.
- `crates/opforge-ffi/src/lib.rs` carries crate-local smoke and negative-path tests for null pointers, UTF-8 validation, scalar execution-mode validation, and message ownership.
- `crates/opforge-ffi/tests/abi_contract.rs` adds crate-level integration coverage over the exported FFI boundary.

The current FFI boundary is therefore real but intentionally narrow:

- it proves that the library can be consumed from C,
- it is appropriate for smoke-path consumers today,
- it is not yet the final ergonomic FFI surface for tool developers.

---

## 10. Feature Model

The v0.1 feature section no longer matches the real target closely enough.

The architecture should instead distinguish:

- always-on shared foundations
  - types
  - VM core
  - package/container plumbing
- optional assembler family/runtime bundle
- optional package bundling per domain
  - `.opcore`
  - `.opasm`
- optional FFI layer
- optional host adapters
  - filesystem providers
  - memory providers

Suggested eventual feature names:

| Feature | Effect |
|---|---|
| `asm-runtime` | Enables assembler processor and assembler-family wiring |
| `asm-families` | Enables built-in CPU/family modules |
| `packages-opcore-bundled` | Bundles default `.opcore` packages |
| `packages-opasm-bundled` | Bundles default `.opasm` packages |
| `ffi` | Builds C/C++ FFI layer |
| `providers-fs` | Filesystem source/sink adapters |
| `providers-mem` | In-memory source/sink adapters |

Exact names may change, but the semantic split should not.

---

## 11. Migration Phases

### Phase 1 — Neutral foundations

- finish extracting neutral shared types,
- define shared syntax/value contracts,
- establish VM-core boundaries,
- document current transitional crate roles.

### Phase 2 — Separate semantic ownership

- split current mixed `core` into non-assembler `opcore` and assembler-owned pieces,
- move assembler-only concepts out of `core`,
- treat current registry as assembler-specific.

### Phase 3 — Split VM usage by package domain

- make `.opcore` and `.opasm` ownership explicit,
- isolate assembler-specific VM helpers from language-core VM helpers,
- keep Rust and VM as peer heads while doing so.

### Phase 4 — Add lockstep parity infrastructure

- define the lockstep mechanism:
  - execution modes
  - stage checkpoints
  - normalization rules
  - divergence payload
  - continuation policy
- keep the mode optional and diagnosability-focused.

### Phase 5 — Implement runnable lockstep parity

- implement the runtime structures defined by the mechanism,
- run at least one processor stage in actual Rust/VM lockstep,
- record stage-by-stage parity coverage.

### Phase 6 — Library orchestration

- add source/output traits,
- move session orchestration into `engine`,
- expose public API through the current `libopforge` module layout.

### Phase 7 — Thin hosts

- rewire CLI to `libopforge`,
- rewire LSP to library-first paths,
- add FFI on top of current session boundaries.

### Phase 8 — Future processing platform preparation

- keep assembler registry separate,
- introduce generic processing-registry abstractions only when multiple processors exist,
- do not prematurely generalize assembler-specific contracts.

---

## 12. Done Criteria

The library split is not complete merely because files moved into workspace crates.

The architecture should be considered converged only when:

- root crate no longer owns the effective assembly session/orchestration path,
- `opcore` no longer owns assembler-specific directives or output semantics,
- assembler-only registry contracts are outside the language-core layer,
- VM-core does not depend on assembler policy concepts,
- `.opcore` and `.opasm` ownership are explicit,
- Rust and VM remain peer execution heads for both processor domains,
- CLI and LSP use `libopforge`,
- compatibility re-exports are reduced to a temporary, shrinking set.

---

## 13. Open Questions

| # | Question | Status |
|---|---|---|
| 1 | Should shared syntax/value contracts live in `types` or a dedicated `syntax` crate? | Open |
| 2 | Should VM splitting happen as separate crates immediately, or as clear internal submodules first? | Open |
| 3 | Which current directives belong semantically to `opcore` vs `asm` in edge cases such as `.segment`? | Open |
| 4 | Which request kinds should be first-class built-ins versus processor-scoped string kinds in the initial engine contract? | Open |
| 5 | How much unknown-span fallback search is needed in the first parser split, versus later multi-processor work? | Open |
| 6 | Should the first stable `libopforge` API remain assembler-only, or expose a processor selection model from day one? | Resolved for the current slice: assembler-only via the curated `libopforge` facade |
| 7 | When should a generic processing registry be introduced? | Deferred until at least one non-assembler processor exists |
| 8 | What license should public library crates use? | Open |
| 9 | Which checkpoints must be mandatory in Rust/VM lockstep comparison mode? | Specified in section 5.3.3; refine only if a concrete stage proves insufficient |

---

## 14. Current Branch Interpretation

For this branch, the following interpretation should be used during implementation reviews:

- `crates/opforge-core` (package `opcore`) is a transitional extraction crate, not the final `opcore`.
- `crates/opforge-vm` (package `vm`) is a transitional VM extraction crate, not yet the final explicit `.opcore` VM / `.opasm` VM split.
- `crates/opforge-registry` (package `registry`) should be treated as the assembler registry layer even if the directory name still carries the older prefix.
- progress should be measured by boundary convergence, not by crate count.
