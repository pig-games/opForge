# Native runtime ownership contract

## Scope

This contract describes the promoted `opforge.*` native implementation. The
[workflow](../workflow/README.md) governs work and validation; the
[source inventory](native-runtime-boundary-inventory-v0_1.md) records selected
reviewed source surfaces. Neither document claims complete language parity or
native runtime qualification.

CPU/family/dialect semantics belong to package definitions and their specialized
owners. Generic directives remain shared core processing. Dot-prefixed statements
must resolve through directive/template handling, never fall through to CPU
instruction selection.

## Current owners

| Owner | Responsibility | Boundary |
| --- | --- | --- |
| `main` | Compose the executable entry and embedded/external package catalog. | Delegate Shell behavior to `opforge.cli.entry`. |
| `opforge.cli.*` | Parse arguments, resolve input/target defaults, present progress and construct the driver request. | Target preamble selection uses shared TKVM/PRVM programs; CLI code does not implement instruction grammar or encoding. |
| `opforge.assembler.driver` and its literal helpers | Source acquisition/order, session coordination, private compilation/publication, storage retirement, incremental reuse and finalized output routing. | Call shared source, language, module, layout and output owners. Driver helpers are part of this owner, not independent modules. |
| `opforge.assembler.prepare.*`, `source.*`, `language.*` | Token/record binding, source suspension, declarations, templates and execution of shared controls. | Shared EXVM/PRVM/TKVM own grammar; source/session glue supplies bounded requests and declaration context. The frontend still combines acquisition/binding/preparation responsibilities; this cleanup does not refactor it. |
| `opforge.assembler.symbols.*`, `values.*`, `modules.*` | Symbol/value ownership, scope/configuration, demanded exports, identity remapping, pointer-free module objects and stored interfaces. | Consumers borrow bounded views. Producer storage retires only after successful publication; persistent validity metadata is invalidated before object replacement. |
| `opforge.assembler.layout.*` | Availability, reachability, sizing, sections, address provenance and emission/fixups. | Generic directives dispatch before package-defined instruction candidates/encoding. No mnemonic or register semantics in generic layout. |
| `opforge.assembler.output.*` | Validate finalized geometry and serialize bytes, records and relocations. | No source acquisition, module discovery or instruction selection. |
| `opforge.packages.*` | Catalog/name/format contracts, external acquisition or borrowed embedded bytes, structural validation. | Loader delegates validation. Retained validation coupling is listed below. |
| `opforge.vm.*` | Execute bounded numeric package programs and construct generic results/requests. | No CLI or assembly orchestration. Retained assembler dependencies are explicit below. |
| `opforge.platform.*`, `debug.*` | OS/storage services and optional passive instrumentation. | Instrumentation preserves the approved registers/CCR/stack and release-time gating contracts. |

The working dependency direction is:

```text
main -> CLI -> assembler driver -> source / prepare / language / modules
                                 -> layout -> output
source / prepare / language / layout -> package data + VM execution
package loader -> catalog / format / validation + platform storage
VM execution -> VM peers + caller-supplied requests/services
```

This is a responsibility model, not an assertion that all imports form a strict
layered DAG. The current retained reverse dependencies are below.

## Reviewed assembler dependencies from VM/package owners

The import guard rejects `opforge.cli.*` and `opforge.assembler.*` dependencies
from VM/package source by default, with these **six exact source/import pairs**:

| Consumer | Permitted assembler import | Actual use |
| --- | --- | --- |
| `vm/exvm/runtime.asm` | `opforge.assembler.source.records` | Numeric token/wire sizes, offsets, kinds and qualifiers; no source-owner calls. |
| `vm/exprvm/values_runtime.asm` | `opforge.assembler.values.values` | Typed owner arena, work storage and scalar/compound services, including mutation/rollback. |
| `vm/exprvm/values_runtime.asm` | `opforge.assembler.values.records` | Typed record validation and projection. |
| `vm/exprvm/values_runtime.asm` | `opforge.assembler.values.structs` | Nominal structs and assembler literal policy. |
| `vm/exprvm/values_runtime.asm` | `opforge.assembler.symbols.bindings` | Typed symbol cells, binding lookup and declaration state. |
| `packages/validation.asm` | `opforge.assembler.layout.state` | Calls `state.validate` to validate package STVM tables. |

Paths in this table are relative to `native/motorola68000/amigaos/`. The five
semantic edges are existing boundary debt, not wire-only dependencies or
permission to add further assembler services. Relocating those services requires
separate design and caller migration. The wire exception likewise requires review
if its use changes from constants to executable services.

Exceptions attach to the defining source file, not its entire include closure.
A helper added to an exempt consumer cannot inherit that exemption. An exception
whose source/import disappears fails the check until reviewed and removed.

Two owners retain exact **possible import sets**, independent of order:

- `cli/entry.asm`: driver, CLI arguments, input and target owners.
- `packages/loader.asm`: catalog, format, validation, platform memory and the gated
  debug memory-profile import from `memory_telemetry.i`.

Unexpected, missing or duplicate imports fail. This preserves the narrow Shell
entry and acquisition boundaries while accounting for conditional instrumentation.

## Requests, callbacks and storage

VM requests carry bounded program/token/output spans, numeric identities and
explicit service/context pointers. EXVM uses caller-owned node/scratch arenas;
PRVM emits requests/continuations rather than invoking assembly orchestration.
ExprVM's string-provider configuration and PRVM router/iterator callbacks are
explicit service ABIs. Their callers own provider validity, register/status
contracts and storage lifetimes. An indirect callback does not create permission
to hide orchestration inside a generic interpreter.

Borrowed views must not outlive their backing owner, a view transition or producer
retirement. Stored module payloads/interfaces use offsets and local identities;
process pointers remain transient. ExprVM's current provider/last-result state is
session state and is not reentrant. The typed evaluator's arena coupling is the
retained semantic dependency above, not an isolated stateless evaluator.

Each VM's intended endpoint remains one consolidated current v1 contract, with
native execution assuming that contract and retaining container/bounds checks.
Existing multi-version checks/dispatch have not been removed by this cleanup;
see [current parity gaps](../plans/native-runtime-reset.md#structural-and-language-boundaries).

## What the checks prove

`check_native_runtime_boundary_contract.py` discovers all `.asm` and `.i` files
under `vm/` and `packages/`, checks their literal include closures (including
helpers outside those directories), and checks the two retained owners. It scans
possible `.use` edges in conditional and macro bodies, matching namespace
boundaries rather than loose name prefixes. Module owners must be imported rather
than textually included. Missing sources, cycles and unsupported include forms
fail closed. New files do not need registration in a frozen source list.

This is static import enforcement. It does not prove callback behavior, alias
use, direct fully-qualified symbol access, absence of mutable-table writes or
runtime semantics. Source review remains necessary. The inventory pins reviewed
source/include contents; the [ownership guard](native-runtime-no-growth-guard-v0_1.md)
checks selected routine growth and module provenance; the CPU boundary checker
remains the CPU-vocabulary authority.

Run `python3 scripts/workflow/run_native_porting_quality_gate.py` for the combined
native checks and format check. `--staged` is passed to change-oriented checks;
import boundaries and the inventory always inspect their complete current scope.
The gate does not launch FS-UAE or make a fresh native-completion claim.
