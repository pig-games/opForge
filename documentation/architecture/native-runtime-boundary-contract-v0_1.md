# Native Runtime Boundary Contract v0.1

## Status and scope

This document records the existing native ownership constraints and context ABI.
It is a technical reference, not an active migration plan or a requirement to
preserve every current abstraction indefinitely. Its tables are responsibility
constraints rather than an exhaustive source inventory. The structural validators
check the implemented import boundaries; this document makes no runtime parity
claim. The [repository workflow](../workflow/README.md) governs changes.

The contract applies to the native CLI frontend, preprocessor staging, opasm
driver/engine, tkpkg facade and package runtimes, expression service, and
diagnostic/event projection.  CPU/family/dialect and instruction behavior
remain package-defined; generic runtime code must not acquire them.

## Ownership model

| Boundary | Boundary owner | Stable responsibility | May depend on | Must not own |
| --- | --- | --- | --- | --- |
| CLI frontend | `opforge-cli` | command/request construction, source staging, result/report presentation | engine API, driver callback adapter, tkpkg facade | pass/image/label storage; package interpretation |
| Preprocessor staging | existing preprocessor owners | bounded macro definition/invocation/substitution/source-frame transaction | frontend and documented engine record APIs | segment, statement, package, or driver semantics |
| Assembly driver | `opasm.amigaos.assembly_driver` | session/pass orchestration and subsystem callback dispatch | directive-router result, engine API, flow/text domain APIs, tkpkg bridge, event projection, default-off passive debug observation | directive strings, structural scans, operand request construction, selector adaptation, data/text/layout semantics |
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

The existing dependency restrictions are:

1. `opasm.amigaos.engine -> tkpkg.*`, `opcore.*`, or CLI frontend.
2. `opasm.amigaos.assembly_driver -> tkpkg.amigaos.service` directly; it uses
   only `opasm.amigaos.tkpkg_bridge`.
3. package runtimes (`tkpkg.pipeline`, tokenizer VM, PRVM, selection, operand,
   encoding) -> `opasm.amigaos.engine` or engine mutable storage.
4. expression service -> CLI frontend, tkpkg facade state, or engine mutable
   storage except through the neutral-context adapter.
5. CLI/preprocessor -> package internals, bypassing the facade/bridge.

Direct `tkpkg.amigaos.service -> opasm.amigaos.engine` imports and
selection/operand access to engine tables are prohibited. The engine-context adapter is the sole tkpkg engine reader; any new
reverse edge fails this contract.

## Neutral runtime-context contract

`tkpkg.amigaos.runtime_context` is the versioned consumer façade, while
`tkpkg.amigaos.engine_context_adapter` is the transitional reader of documented
engine getters. This contract does not add or validate CPU,
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
table layout. Consumers use the context ABI rather than engine mutable-table
storage. The adapter remains a documented transitional boundary. Any replacement
must preserve the context behavior and update affected source, callers, tests and
structural checks together; no historical plan item schedules that work.

## Verification

`scripts/workflow/check_native_runtime_boundary_contract.py` checks existing
source import restrictions. It does not validate plan ledgers or require a
planning artifact. These are structural checks, not runtime parity proof.
Use the [current workflow](../workflow/README.md) for development and validation
cadence. No future extraction is scheduled by this reference document.
