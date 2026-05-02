# Native AmigaOS Module/Use Port Specification v0.1

## Summary

This specification defines the first production-grade 68020 AmigaOS-native
behavior for `.module` and `.use` processing in the native `opforge_cli`
pipeline.

The Rust implementation currently handles module processing in three distinct
layers:

- parser surfaces produce `.module`, `.endmodule`, and `.use` AST shapes;
- source graph loading resolves module dependencies from the input root and
  `--module-path` roots;
- macro export injection makes imported public names available before assembly.

The native AmigaOS port must preserve that split. The immediate native goal is
not to recreate all host filesystem and macro functionality at once, but to move
from directive recognition to actual module/use processing with stable data
structures, deterministic diagnostics, and a path that can later consume PRVM
parser output directly.

## Problem

`examples/motorola68000/amigaos/opforge/opforge_cli.asm` currently tokenizes
input lines through the native package-backed tokenizer and then performs a
hard-coded source-line scan for `.module` and `.use`. The scan proves the CLI
can see these directives, but it does not yet implement Rust-equivalent
processing:

- `.module` does not create or select a current module context;
- `.endmodule` is not tracked;
- `.use` does not resolve dependencies, aliases, selected imports, or wildcard
  imports;
- `--module-path` is still treated as an unsupported CLI option in the native
  subset;
- no native module table or import table is produced for later parser/emitter
  stages.

The Rust behavior to port is visible in:

- `crates/opforge-core/src/parser.rs`
- `crates/opforge-core/src/modules.rs`
- `crates/opforge-engine/src/source_graph.rs`
- `crates/opforge-core/src/macro_processor.rs`
- `crates/opforge-vm/src/execution_model/directives.rs`
- `crates/opforge-cli-core/src/cli.rs`

## Goals

- [ ] Define native `.module` handling that records module identity, module
      nesting depth, selected root module, and deterministic malformed-module
      diagnostics.
- [ ] Define native `.endmodule` handling that closes module contexts and
      reports underflow or unterminated modules deterministically.
- [ ] Define native `.use` handling for the first supported Rust-compatible
      subset: module id, optional alias, bare import with no item list, selected items,
      wildcard item, and diagnostics for unsupported or malformed forms.
- [ ] Define native `--module-path` argument handling close to the Rust CLI
      shape while allowing a first hard-coded AmigaOS subset.
- [ ] Define native module and import tables that can be consumed by later PRVM,
      macro, and emission stages without re-scanning source text.
- [ ] Preserve the current package-backed tokenizer integration and make module
      processing happen only after tokenizer success for a line.

## Non-Goals

- [ ] Do not implement a full Rust-equivalent filesystem module loader in the
      first native slice.
- [ ] Do not implement generic expression evaluation for `.use ... with(...)`
      parameters in this module/use port; that belongs to the expression VM
      contract.
- [ ] Do not move CPU-specific assembler parsing into opcore or the native
      module processor.
- [ ] Do not implement native macro expansion in this spec; only define the
      module/import records needed by that later stage.
- [ ] Do not change Rust CLI behavior except where tests need to lock parity
      expectations for the native subset.
- [ ] Do not require FS-UAE full-corpus execution for every small slice; use it
      as a gate for slices that affect native runtime behavior.

## Invariants / Constraints

- The tokenizer remains the first native stage for source lines. Module/use
  processing must not run for a line whose tokenization failed.
- The native module/use processor must consume either PRVM parser output or a
  documented transitional directive-token subset. It must not grow into a
  general text parser long term.
- Rust remains the behavioral truth for accepted syntax, ordering, and
  diagnostics unless this specification explicitly names a smaller native
  subset.
- `.module` and `.endmodule` are opcore/module-item concerns. CPU-specific
  instruction parsing belongs outside this layer.
- `.use` AST content is generic module infrastructure. It may reference module
  names and selected item names, but it must not evaluate CPU-specific operands.
- Native data structures must use fixed capacities and deterministic overflow
  diagnostics rather than unbounded allocation.
- CLI spelling should stay close to Rust: `-M` and `--module-path` are the
  module root flags, and `OPFORGE_MODULE_PATHS` is the host-side Rust
  environment analogue.

## Behavioral Contract

Native `.module` processing must:

- accept `.module <module-id>` when `<module-id>` tokenizes as the same
  identifier-like module id accepted by the Rust module-item route;
- record the module id in a native module table;
- mark the first explicit root module as the selected root unless a later
  native CLI option explicitly chooses another root;
- maintain module depth so nested `.module` / `.endmodule` blocks can be
  represented;
- ignore directives in inactive conditionals only after native conditional
  state exists; until then, the native subset must document that conditional
  filtering is not implemented.

Native `.endmodule` processing must:

- close the current module context;
- reject `.endmodule` without an active module;
- report unterminated modules at end of input;
- leave enough state for later source-map and emission stages to know which
  module each accepted line belongs to.

Native `.use` processing must:

- accept `.use <module-id>`;
- accept `.use <module-id> as <alias>`;
- accept `.use <module-id> (<item>, <item> as <alias>)` at the parser-record
  layer, while recognizing that the current Rust source-graph import injection
  path stores selected item names and does not yet apply selected-item aliases;
- accept `.use <module-id> (*)`;
- reject empty selective lists, wildcard aliases, mixed wildcard/item lists, and
  trailing tokens with diagnostics that intentionally mirror the Rust parser
  messages where practical;
- store each import record with source line, module id, optional alias, import
  mode, and selected item names.

Native `--module-path` processing must:

- parse repeatable `-M DIR` and `--module-path DIR` values;
- store paths in CLI order after the implicit input root;
- report missing values using the same native missing-value diagnostic family
  as other value-taking flags;
- keep path count and path length capacity failures deterministic.

Module resolution on AmigaOS must start as a small explicit subset:

- root input file is always loaded;
- module roots are represented as CLI records and can be reported;
- the first executable native module-graph slice may resolve hard-coded files
  from `Work:` paths only;
- missing and ambiguous module diagnostics must be deterministic before broader
  filesystem scanning is attempted.

## Boundary Cases

- `.use` with no module id returns a module/use parse diagnostic.
- `.use math ()` returns the native equivalent of "Selective import list cannot
  be empty".
- `.use math (* as all)` returns the native equivalent of "Wildcard import
  cannot have an alias".
- `.use math (*, foo)` returns the native equivalent of "Wildcard import must be
  the only selective item".
- `.module` with no module id returns a module parse diagnostic.
- `.endmodule` before any `.module` returns a module-depth diagnostic.
- end-of-file with open module depth returns an unterminated-module diagnostic.
- repeated `--module-path` values preserve CLI order.
- module/import table overflow returns a deterministic native diagnostic and
  does not continue into parser/emitter stages.
- conditional filtering is explicitly unsupported until native conditional
  state exists; native tests must not claim parity for inactive conditional
  `.use` handling before that slice lands.

## Acceptance Criteria

- [ ] Native CLI accepts `-M` and `--module-path` as value-taking flags and
      stores at least two module roots in command-line order.
- [ ] Native CLI processes `.module main`, `.use math`, and `.endmodule` into
      native tables after tokenizer success.
- [ ] Native CLI reports module/import summaries from table state, not from the
      old direct print-only line scanner.
- [ ] Native CLI rejects malformed `.module`, `.endmodule`, and `.use` cases
      with deterministic `OPC-NCLI` diagnostics.
- [ ] Host assembly tests lock the new native labels, status strings, and hunk
      payload markers.
- [ ] Opt-in FS-UAE native CLI smoke covers one successful `.module`/`.use`
      source and at least one malformed module/use failure path.
- [ ] The plan derived from this spec keeps parser VM, module graph, macro
      injection, and emitter work in separate commit-sized slices.

## Validation Expectations

Minimum host-side validation for each implementation slice:

- `cargo fmt --all --check`
- `cargo test -p asm motorola68020_opforge_native_cli_ -- --nocapture`
- `cargo test -p asm examples_match_reference_outputs -- --nocapture`

Additional validation when native runtime behavior changes:

- opt-in FS-UAE native CLI smoke for the relevant success or failure path;
- reference refresh only when expected generated hunk/listing output changes;
- focused Rust tests when Rust parity expectations or CLI contracts are locked.

## Open Questions

- Should the first native module resolver scan a directory from `--module-path`,
  or should it first load explicitly named `Work:` module files to keep the
  AmigaOS filesystem slice smaller?
- Should native `.use ... with(...)` be rejected initially, or accepted into a
  record with unevaluated parameter token ranges until expression VM evaluation
  is available?
- Should selected-item aliases be stored only for parser parity initially, or
  should the native import table wait until Rust source-graph processing applies
  selected-item aliases semantically?
- What fixed capacities should the native module table, import table, selected
  item table, and module path table use for the first AmigaOS implementation?
- Should root-module selection remain "first explicit `.module`" initially, or
  should the native CLI expose a Rust-compatible root-selection option first?
