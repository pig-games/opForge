<!-- workflow-provenance: skill=opforge-spec-authoring; entrypoint=run_spec_workflow.sh -->
# Specification: opForge Qualified Module Use, Section Mapping, and Selective Inclusion v0.1

## Summary

Define concrete opForge module semantics for qualified `.use` imports, selected
root symbols, logical section mapping, and dependency-driven selective binary
inclusion. The feature lets source modules import a public module namespace once,
reference public symbols through a stable local qualifier such as `engine.start`,
map reusable module sections into consumer-owned concrete sections, and include
only referenced or explicitly selected units in final executable/library output.

This specification combines:

- `documentation/plans/opforge-module-qualified-use-symbol-resolution-plan-v0_1.md`
- `documentation/architecture/opforge-module-qualified-use-section-mapping-concept-spec-v0_1.md`

## Problem

Current `.use` semantics are deterministic but scale poorly for larger reusable
modules. Consumers often need long selective import lists and public symbols with
globally unique prefixes, which makes native opForge and target-library code hard
to read and reuse.

The desired authoring style is:

```asm
.module main
.cpu 68020

.use opasm.amigaos.engine as engine map {
    code -> app_code
    data -> app_data
    bss  -> app_bss
}

    jsr engine.sessionPass
.endmodule
```

The same model must also support root-composition modules:

```asm
.module main
.cpu 68020

.use opforge.cli.entry (start)
.output "build/opforge_cli", format=hunk, sections=entry, code, data, bss
.endmodule
```

where `start` is selected as an inclusion root even if the root module does not
directly call it.

## Goals

- [ ] Support qualified module namespace binding with explicit aliases, e.g.
  `.use my.module as mm` enables `mm.publicSymbol`.
- [ ] Support bare qualified `.use module.path` by deriving an implicit qualifier
  from the final path segment, e.g. `.use opasm.amigaos.engine` enables
  `engine.sessionPass`.
- [ ] Preserve existing direct selective import behavior, e.g.
  `.use my.module (startup)` makes `startup` available directly.
- [ ] Define selected symbols in qualified imports as inclusion roots while
  keeping source references qualified by default, e.g.
  `.use my.module (startup) as mm` enables `mm.startup`, not bare `startup`.
- [ ] Define logical section declarations for reusable modules and `.use ... map`
  rules that map included logical sections into consumer-owned concrete sections.
- [ ] Define dependency-driven selective inclusion where references and selected
  roots include only reachable units and dependencies, not entire used modules.
- [ ] Define output-policy expectations for integrated executables and linkable
  library/object outputs without requiring separate source architectures.
- [ ] Preserve `.pub` / `.priv` visibility and existing CPU-family operand
  semantics while allowing qualified imported symbols as operands.

## Non-Goals

- [ ] Do not redesign the assembler tokenizer or parser outside the minimum
  changes required for `.use ... map` and qualified symbol operands.
- [ ] Do not port these semantics to native AmigaOS assembly in this first
  contract; host/Rust opForge semantics come first.
- [ ] Do not require `.use my.module as mm` to include every public symbol from
  `my.module` in the final binary.
- [ ] Do not make private symbols addressable through aliases, full module paths,
  selected roots, wildcard imports, or section maps.
- [ ] Do not introduce CPU/family/dialect-specific semantics into generic VM,
  workflow, or CLI implementation paths.
- [ ] Do not require output formats to support every packaging mode immediately;
  unsupported policies must produce explicit diagnostics.

## Invariants / Constraints

- Module availability is separate from binary inclusion:

  ```text
  .use makes public symbols resolvable.
  references and selected roots make units reachable.
  .output decides how reachable units are packaged.
  ```

- Local qualifiers are source-level namespace bindings. Internal identity remains
  the full module path plus symbol name, e.g. `opforge.cli.args::parse`.
- A qualified import does not inject imported names into the local unqualified
  namespace unless a direct selective import also requests that behavior.
- Existing `.use module (item)`, `.use module (item as alias)`, wildcard import,
  and visibility behavior remain valid.
- Public/private visibility is enforced before code generation and output
  packaging.
- Logical sections belong to the defining module's contract. Concrete placement
  belongs to the consuming/root/output module.
- Section maps must be kind/capability compatible, including target-sensitive
  kinds such as zero page.
- Unit reachability is deterministic. Given the same root set, imports, section
  maps, and output policy, emitted units and diagnostics must be stable.
- Generic opForge implementation paths must remain CPU-architecture neutral as
  required by `AGENTS.md`.

## Behavioral Contract

### Qualified imports

`.use my.module as mm` binds the public namespace of `my.module` to local
qualifier `mm`.

Valid:

```asm
jsr mm.myRoutine
move.l #mm.myConstant, d0
lea mm.myTable, a0
```

Invalid unless separately imported directly:

```asm
jsr myRoutine
```

Bare `.use my.module` creates only an implicit qualifier from the final module
path segment. For `my.module`, the qualifier is `module`; for
`opasm.amigaos.engine`, the qualifier is `engine`. It does not directly import
every public symbol into the current unqualified namespace. Code that needs
unqualified names must use selective imports such as `.use my.module (symbol)`
or an existing wildcard form that explicitly requests unqualified import
behavior.

Explicit aliases override implicit qualifiers:

```asm
.use opasm.amigaos.engine as eng
jsr eng.sessionPass
```

`engine.sessionPass` is invalid in that module unless another import binds
`engine`.

### Direct selective imports and selected roots

`.use my.module (startup)` preserves the existing direct selective import
contract: `startup` is available in the current namespace and may act as a
selected inclusion root.

`.use my.module (startup) as mm` binds `mm` and selects `startup` as a root, but
does not make bare `startup` valid by default:

```asm
jsr mm.startup   ; valid
jsr startup      ; invalid unless also imported directly
```

### v0.1 `.use` syntax matrix

The v0.1 parser accepts `.use` forms in this order only:

```text
.use <module-path> [<selection-list>] [as <qualifier>] [map { <section-map-list> }]
```

Valid forms:

```asm
.use my.module
.use my.module as mm
.use my.module (startup)
.use my.module (startup) as mm
.use my.module map {
    code -> app_code
}
.use my.module as mm map {
    code -> app_code
}
.use my.module (startup, irqHandler) as mm map {
    code -> app_code
}
.use my.module (startup as start)
.use my.module (*)
```

Rules:

- `<module-path>` is a dotted module path.
- `<selection-list>` without a module qualifier uses the existing direct
  selective import item syntax, including existing `(item as alias)` and
  wildcard forms.
- `<selection-list>` with a module qualifier may contain only plain exported
  symbol names. These names are selected as roots and remain qualified by the
  module qualifier.
- `as <qualifier>` after the selection list binds a module namespace qualifier.
- If a module qualifier is present, selected symbols are selected as roots but
  are not directly imported.
- `map { ... }` must appear after any selection list and after any module
  qualifier.
- `.use my.module map { ... }` is valid and uses the implicit qualifier
  `module`.
- Rejected forms include `as` before the selection list, `map` before `as`, more
  than one module qualifier, more than one map block, a map block without a
  module namespace binding after implicit qualifier derivation,
  `.use my.module (startup as start) as mm`, and `.use my.module (*) as mm`.

### Qualified full module references

The resolver must accept full module-path references such as
`opasm.amigaos.engine.sessionPass` in v0.1 when the referenced module is
available through `.use`. Full-path lookup splits the dotted reference at an
imported module-path boundary: exactly one imported module path must match the
leading segments, and the remaining suffix is the requested exported symbol name.
If no imported module path matches, or more than one split is possible, the
reference fails with a deterministic ambiguity or unresolved-symbol diagnostic.
Successful full-path lookup must resolve to the same internal symbol identity as
alias lookup and must obey visibility.

Full-path lookup is not a substitute for `.use`: a module must still be made
available by an import before its public symbols can be referenced.

### Logical sections and maps

Reusable modules may declare logical sections:

```asm
.module opforge.tkvm.scanner

.section code, kind=code, logical
    ; scanner routines
.endsection

.section tables, kind=data, logical
    ; read-only data
.endsection

.section state, kind=bss, logical
    ; reserved state
.endsection
.endmodule
```

Consumers map logical sections into concrete sections:

```asm
.use opforge.tkvm.scanner as scanner map {
    code   -> app_code
    tables -> app_data
    state  -> app_bss
}
```

Only reachable units from `scanner` are placed through the map. Unreachable
units are excluded from executable output.

There is no implicit same-name default mapping in v0.1. Every reachable logical
section must have an explicit map entry. Missing entries diagnose before output
emission and must name the defining module and logical section.

Map targets are concrete sections in the consuming/root module. A target section
may be declared before or after the `.use`, but by validation time it must be
declared with a known `kind`/capability in the consuming/root module. `.output`
section lists may order or package sections, but they do not create section
kinds for map validation. A map target that is never declared fails with a
diagnostic naming the target section and import map entry.

### Dependency-driven selective inclusion

Modules compile into an intermediate graph with:

- public exports;
- private/internal symbols;
- logical section contracts;
- units that define symbols, reference symbols, belong to a section, and emit
  bytes or reservations.

A v0.1 unit begins at each top-level label, exported symbol, private symbol,
constant/value symbol, or data reservation/emission declaration that can be a
symbol-resolution target. The unit continues until the next top-level
symbol-resolution target, section boundary, or module boundary. Local/anonymous
labels, generated relocation metadata, debug metadata, and generated fixup data
attach to the containing unit. This granularity is intentionally conservative:
it may include adjacent local implementation details, but it must not include
unrelated later top-level symbols from the same module section.

A root reference to `scanner.scanNext` includes the unit defining `scanNext`,
then recursively includes units and data referenced by that unit. Public but
unreferenced exports remain available for resolution but are not emitted into an
integrated executable.

Selected symbols in `.use module (symbol)` are added to the initial root set
even when not directly referenced by source instructions.

### Output policies

Integrated executable output requires all selected/referenced roots and their
dependencies to resolve and emits only reachable loadable material. Unresolved
symbols are errors unless the selected output format explicitly permits
externals.

The v0.1 implementation target is integrated executable output for existing
host/Rust output paths. Library/object packaging is specified as a future policy
over the same module graph but is not required to emit new library/object formats
in v0.1. If source requests a library/object policy that is not implemented, the
assembler must produce an explicit unsupported-policy diagnostic rather than
silently emitting an integrated executable or all module contents.

The v0.1 unsupported-policy surface must recognize and reject these concrete
requests when encountered:

```asm
.output "build/lib.hunk", format=hunklib
.output "build/lib.o", format=hunk-object
.output "build/presenter.lib", format=c64os-library
```

Other unknown output formats may continue to use the existing unknown-format
diagnostic, but recognized library/object policies above must specifically say
that library/object packaging over the module graph is not implemented in v0.1.

## Boundary Cases

- Duplicate explicit aliases in one module must produce a deterministic `.use`
  diagnostic.
- Duplicate implicit qualifiers from different module paths must produce a
  deterministic `.use` diagnostic unless one import uses an explicit disambiguing
  alias.
- Explicit aliases that collide with local symbols, direct selective imports, or
  other namespace bindings must produce deterministic diagnostics.
- A qualified reference to a private symbol must fail even when the module is
  imported, selected, or mapped.
- A section map entry for an unknown logical section must fail with a diagnostic
  naming the missing logical section and defining module.
- A reachable logical section without a required concrete mapping must fail; no
  implicit default mapping exists in v0.1.
- A section map target that is not declared as a concrete section in the
  consuming/root module by validation time must fail before output emission.
- A map from incompatible section kinds, e.g. `kind=bss` to `kind=code`, must
  fail before output emission.
- Qualified symbol operands must not regress existing dotted CPU syntax such as
  M68K `.W` / `.L` suffixes, indexed registers, register pairs, or special
  registers.
- Circular module references are allowed only if unit reachability and symbol
  resolution remain finite and deterministic; unresolved cycles must diagnose the
  unresolved symbol path.
- Wildcard imports continue to follow existing behavior and must not bypass
  qualification, visibility, root selection, or inclusion rules.

## Acceptance Criteria

- [ ] `.use opasm.amigaos.engine` enables `engine.sessionPass` for public runtime
  symbols and internally resolves to the full module-path symbol identity.
- [ ] `.use opasm.amigaos.engine as eng` enables `eng.sessionPass` and does not
  also bind `engine.sessionPass` unless separately imported.
- [ ] `.use my.module (startup)` preserves direct `startup` lookup.
- [ ] `.use my.module (startup) as mm` selects `startup` as an inclusion root and
  requires source references to use `mm.startup` unless directly imported.
- [ ] Public qualified symbols are accepted as instruction operands for every
  supported CPU family through shared symbol resolution or the smallest necessary
  family-specific parser fixes.
- [ ] Private qualified symbols are rejected consistently for alias, implicit
  qualifier, full-path, selective, and wildcard lookup forms.
- [ ] A module with logical `code`, `data`, and `bss` sections can be consumed
  through `.use ... map { ... }`, and only reachable units are mapped into the
  output sections.
- [ ] `.use` syntax accepts only the v0.1 ordering
  `<module-path> [selection] [as qualifier] [map]` and rejects reordered or
  duplicate clauses deterministically.
- [ ] Per-item selection aliases and wildcard selection remain valid for direct
  selective imports without a module qualifier and are rejected when a module
  qualifier is also present.
- [ ] Incompatible section maps produce diagnostics that name the source logical
  section, source kind, target section, and target kind.
- [ ] Missing concrete map targets produce diagnostics that name the missing
  target section and import map entry.
- [ ] Integrated executable policy emits selected/referenced reachable units for
  existing host/Rust executable output paths and rejects unresolved non-external
  symbols.
- [ ] Library/object output requests that are not implemented in v0.1 produce
  explicit unsupported-policy diagnostics.
- [ ] Existing selective import, wildcard import, visibility, and CPU operand
  fixtures continue to pass.

## Validation Expectations

- Focused Rust tests for import metadata: explicit alias, implicit final-segment
  qualifier, duplicate alias diagnostics, and preservation of selective imports.
- Focused assembler tests for qualified public label/constant/value resolution,
  full module-path lookup, direct selective lookup, alias lookup, private-symbol
  rejection, and selected-root behavior.
- Focused CPU-family operand tests proving qualified imported public symbols can
  appear in representative call/jump/absolute-reference forms for all supported
  families.
- Focused `.use` syntax tests for accepted clause ordering, reordered/duplicate
  clause rejection, direct-only per-item alias/wildcard forms, and rejection of
  per-item alias/wildcard forms combined with a module qualifier.
- Focused section tests for logical section declaration parsing, map parsing,
  concrete map target lookup, kind compatibility, missing map diagnostics, and
  reachable-unit placement.
- Focused output-policy tests for integrated executable reachability and
  unsupported library/object policy diagnostics.
- Run `scripts/workflow/run_rust_quality_gate.sh` or `make quality-gate` before
  committing Rust implementation slices.
- Run `make workflow-gate` or the relevant workflow validators before committing
  workflow artifact changes.

## Open Questions

- Which later version should add real library/object packaging for AmigaOS hunk
  libraries, C64 OS libraries, or other target-specific linkable outputs?
- Should a future version add opt-in default section mapping for same-name,
  compatible logical and concrete sections, or should explicit maps remain the
  permanent rule?
- Should future reachability optimization split units more finely than the v0.1
  top-level-symbol granularity when doing so is safe for local labels and
  generated metadata?
