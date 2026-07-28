# Native Runtime Boundary Contract v0.1

## Status and scope

This Item 5.3 contract turns the Item 5.2 inventory into the target ownership
model for the current native runtime.  It is an architecture and dependency
contract only: it moves no assembly, changes no ABI, and makes no parity claim.
The active `AGENTS.md` remains binding during every extraction.

The contract applies to the native CLI frontend, preprocessor staging, opasm
driver/engine, tkpkg facade and package runtimes, expression service, and
diagnostic/event projection.  CPU/family/dialect and instruction behavior
remain package-defined; generic runtime code must not acquire them.

## Ownership model

| Boundary | Sole long-term owner | Stable responsibility | May depend on | Must not own |
| --- | --- | --- | --- | --- |
| CLI frontend | `opforge-cli` | command/request construction, source staging, result/report presentation | engine API, driver callback adapter, tkpkg facade | pass/image/label storage; package interpretation |
| Preprocessor staging | existing preprocessor owners | bounded macro definition/invocation/substitution/source-frame transaction | frontend and documented engine record APIs | segment, statement, package, or driver semantics |
| Assembly driver | `opasm.amigaos.assembly_driver` | session/pass orchestration and subsystem callback dispatch | directive-router result, engine API, flow/text domain APIs, tkpkg bridge, event projection | directive strings, structural scans, operand request construction, selector adaptation, data/text/layout semantics after Items 5.8–5.9.4 |
| Directive router | `opasm.amigaos.directive_router` | bounded non-structural directive text classification | no runtime owner | callback orchestration, structural scans, handler semantics, CPU/family/dialect behavior |
| Assembly engine | `opasm.amigaos.engine` | statement collection, pass state, PC/image, labels, callback context, event/session state through documented APIs | event projection only | package selection/encoding and CLI presentation |
| tkpkg facade | `tkpkg.amigaos.service` | ABI dispatch, request validation/lifecycle, output projection, last-error entry | extracted tkpkg services and the neutral runtime-context adapter during migration | parser, expression, candidate, operand-plan, package encoding, and direct engine-table implementation |
| Expression service | `tkpkg.amigaos.expression_service` | request-envelope validation, neutral-context adaptation, result/diagnostic projection | expression frontend and neutral context only | scalar grammar, bytecode compilation/evaluation, direct engine table access, CLI output policy, package selection |
| Expression frontend | `opcore.amigaos.expr_bridge` | bounded scalar grammar/literal/symbol-index compilation into versioned ExprVM bytecode and runtime invocation | ExprVM runtime only | request envelopes, diagnostic projection, evaluator semantics, package selection, direct engine/context state |
| Package runtimes | pipeline, tokenizer VM, PRVM, selection/operand/encoding owners | package-defined hierarchy, parsing, selection, operand plans, and encoding | package data, neutral context, own runtime peers | opasm mutable tables, CLI lifecycle, CPU behavior in generic owners |
| Diagnostic/event projection | engine event owner plus facade status owner | engine event capture; facade status/last-error presentation | declared event/status contracts | hidden cross-boundary mutable state |

## Required dependency model

```text
CLI frontend -> preprocessor staging -> assembly driver -> engine API
                                            -> domain flow/text owners
                                            -> tkpkg bridge -> tkpkg facade
tkpkg facade -> request/status/parser/expression/selection/operand/encoding services
service/runtime consumers -> neutral runtime context -> engine adapter -> engine API
pipeline -> package hierarchy and CPU/family/dialect package data
expression service -> expression bridge -> expression VM runtime
```

The following edges are prohibited once the named migration is complete:

1. `opasm.amigaos.engine -> tkpkg.*`, `opcore.*`, or CLI frontend.
2. `opasm.amigaos.assembly_driver -> tkpkg.amigaos.service` directly; it uses
   only `opasm.amigaos.tkpkg_bridge`.
3. package runtimes (`tkpkg.pipeline`, tokenizer VM, PRVM, selection, operand,
   encoding) -> `opasm.amigaos.engine` or engine mutable storage.
4. expression service -> CLI frontend, tkpkg facade state, or engine mutable
   storage except through the Item 5.7 neutral-context adapter.
5. CLI/preprocessor -> package internals, bypassing the facade/bridge.

Item 5.7.2 removes the obsolete `tkpkg.amigaos.service ->
opasm.amigaos.engine` import and all selection/operand direct engine-table
consumers. The engine-context adapter is the sole tkpkg engine reader; any new
reverse edge fails this contract.

## Neutral runtime-context contract

Item 5.7 implements this contract as an ownership-only file split; Item 5.3
fixes its required shape. `tkpkg.amigaos.runtime_context` is the versioned
consumer façade, while `tkpkg.amigaos.engine_context_adapter` is the sole
transitional reader of documented engine getters. Consumer migration remains
deferred to Items 5.7.1 and 5.7.2. This contract does not add or validate CPU,
family, dialect, instruction, selector, plan, or encoding support.

| Field/service | Provider | Consumer meaning | Prohibited substitute |
| --- | --- | --- | --- |
| current pass | engine adapter | resolve pass-sensitive expression/selection behavior | reading engine globals |
| current address | engine adapter | form neutral evaluation requests | reading engine PC storage |
| symbol lookup | engine adapter | resolve a named symbol | scanning engine label tables |
| symbol stability/finalization | runtime-context façade over engine adapter | obtain a bounded neutral stability snapshot | inferring from engine label-table representation |
| symbol-table compatibility snapshot | runtime-context façade over engine adapter | supply bounded copied names/values to a retained legacy bridge | passing engine label-table pointers |
| diagnostic sink | runtime-context façade | report a neutral code/message/span result | writing service or engine diagnostic buffers directly |

The ABI must be pointer/register explicit, read-only to package consumers, and
versioned.  It returns defined absence/unresolved status rather than exposing
table layout.  The adapter is the only Item 5.7 transitional owner and is
deleted only after Items 5.7.1 and 5.7.2 prove no tkpkg consumer imports or
addresses engine mutable label-table storage.

## Extraction ledger

Every future extraction has one source owner, destination owner, temporary
adapter, and deletion criterion.  “None” means no adapter is authorized.

| Plan item | Source owner | Destination owner | Temporary adapter | Deletion criterion |
| --- | --- | --- | --- | --- |
| 5.4 | tkpkg facade status/error routines | tkpkg status service | facade-to-status call | facade has no reusable status/error implementation |
| 5.4.1 | tkpkg facade bootstrap/request routines | tkpkg request service | facade-to-request call | facade no longer decodes reusable control-block details |
| 5.5 | tkpkg facade parser route | tkpkg parser service | facade-to-parser call | dispatch delegates without parser envelope logic |
| 5.5.1 | tkpkg facade expression route | expression service | neutral-context transition adapter | expression service has no direct engine-table access |
| 5.6 | tkpkg facade selection traversal | tkpkg selection service | facade-to-selection call | facade has no selected-instruction/candidate traversal |
| 5.6.1 | selection/facade operand-plan handling | tkpkg operand runtime | selection-to-operand call | neither prior owner interprets plans |
| 5.6.2 | selection/facade package encoding | tkpkg encoding service | selection-to-encoding call | prior owners contain no encoding interpreter |
| 5.7 | direct service/engine context access | neutral context plus engine adapter | engine context adapter | consumers use the context ABI only |
| 5.7.1 | expression consumer direct context | expression service/context ABI | Item 5.7 adapter | expression service imports only neutral context; temporary context adapter deleted |
| 5.7.2 | selection/encoding consumer direct context | selection/encoding context ABI | Item 5.7 adapter | no tkpkg engine-table import/address remains |
| 5.8 | driver directive routing | directive router | driver-to-router dispatch | driver has no directive/mnemonic string chains |
| 5.8.1 | driver structural scans | structural-flow owner | driver-to-flow dispatch | driver has no domain terminator scans |
| 5.9 | driver operand/eval request construction | operand-evaluation owner | driver-to-eval dispatch | driver constructs no operand/eval requests |
| 5.9.1 | driver selector/encode adaptation | selector-encode owner | driver-to-selector dispatch | driver has no selector adaptation |
| 5.9.2 | driver numeric data work | data directive owner | driver-to-data dispatch | driver has no numeric size/emission implementation |
| 5.9.3 | driver text work | text directive owner | driver-to-text dispatch | driver has no text size/emission implementation |
| 5.9.4 | driver layout work | layout owner | driver-to-layout dispatch | driver has no region/section/place/alignment implementation |
| 5.10 | expression bridge audit | retained cohesive scalar frontend | none | only two public entries remain; compiler state, dependency, callers, long-term owner, and replacement criterion are explicit |
| 5.11 | engine/pipeline conditional audit | retained owner or one proved owner | none unless a violation is proved | audit records no-change or isolated extraction |

## Verification and evolution rules

- `scripts/workflow/check_native_runtime_boundary_contract.py` validates this
  record’s required ownership, context, prohibited-edge, and ledger fields and
  checks current source imports against the permitted transitional model.
- The affected slice metadata records this as a Level B/C architecture slice;
  no Level D claim is made until an extraction changes a real CLI path.
- A later extraction may update the contract only in the same focused commit
  that supplies its source/API and C/D evidence.  It may not loosen a
  prohibited edge without an explicit successor item and reviewer approval.
- Segment, statement, export, or new CPU semantics are not activated by this
  contract.
