# libopforge CPU/Family Extension Guide

This guide is for contributors working below the stable `libopforge` facade on new CPU, family, or dialect behavior. It is not the normal embedding path for downstream hosts.

## 1. Scope

This guide covers:

- adding a CPU to an existing family
- introducing or adjusting a family or dialect mapping
- wiring registry metadata for new builtin architecture support
- validating extension work against the current stable host boundary

Builtin CPU, family, and capability lookup for host integrations is covered by `libopforge::registry` and the main developer guide.

## 2. Boundary map

| Change you need | Primary place to work | Keep out of the stable facade until... |
|---|---|---|
| inspect builtin CPUs, families, aliases, or capabilities | `libopforge::registry` consumer APIs | never widen the facade just for internal contributor convenience |
| add a CPU under an existing builtin family | workspace `registry`, `families`, and related CPU/runtime crates | the new behavior is stable enough that hosts need a new published capability or documentable contract |
| add a new family or dialect | workspace `registry`, `families`, parser/VM integration crates, and the architecture specification | the ownership boundary and validation story are stable |
| publish a new host-visible extension capability | architecture/spec work first, then `libopforge` once the contract is deliberate | the public boundary and long-form docs are agreed |

The stable facade's registry surface is primarily for lookup and introspection. Full custom family or CPU registration remains advanced lower-level workspace work.

## 3. Start from the architecture boundary

Before changing extension code, read `documentation/libopforge-specification.md`.

The specification covers:

- layer ownership between `libopforge`, `engine`, `opcore`, `asm`, `vm`, and registry/family crates
- public-boundary rules for what belongs in the stable facade versus internal implementation crates
- processor and runtime constraints that extension work must respect

This guide focuses on contributor workflow within those boundaries.

## 4. Typical extension paths

### 4.1 Add a CPU to an existing family

This path applies when the family model already exists and builtin coverage is expanding.

1. Add or update the family-specific instruction and operand behavior in the owning workspace crates.
2. Register the CPU metadata and resolution path through the registry layer.
3. Validate capability lookup through `libopforge::registry` so hosts can discover the new builtin CPU without reaching into internals.
4. Validate statement-level behavior through `libopforge::asm::opasm` or full assembly flows when appropriate.

## 4.2 Add a family or dialect

This path applies when the architecture split itself changes.

1. Recheck the architecture specification before adding new public assumptions.
2. Add the family or dialect behavior in the owning workspace crates rather than growing `libopforge` first.
3. Make registry metadata, parser/runtime integration, and validation converge before proposing new facade exposure.
4. Only after the internal ownership is stable should host-facing documentation or facade surface area expand.

## 4.3 Publish new host-visible extension behavior

Only move extension details into the stable facade when hosts genuinely need a new published contract.

That usually means all of the following are true:

- registry/capability discovery is not enough
- the ownership boundary is stable
- the examples and long-form docs can explain the contract clearly
- the public API can stay narrow and deliberate

## 5. Validation checklist

For extension work, validate from both directions:

- internal implementation tests in the owning workspace crates
- host-visible discovery or behavior through `libopforge::registry`, `libopforge::asm`, or `libopforge::asm::opasm`

Useful public-facing checks usually include:

- capability lookup or target-resolution coverage through `documentation/libopforge-developer-guide-examples/libopforge_registry.rs`
- statement-processing or parsing checks through `documentation/libopforge-developer-guide-examples/libopforge_opasm.rs`
- full assembly flows when the new CPU or family changes emitted behavior

## 6. Reference points in this repo

- `documentation/libopforge-specification.md`
- `documentation/libopforge-developer-guide-examples/libopforge_registry.rs`
- `documentation/libopforge-developer-guide-examples/libopforge_opasm.rs`
- `crates/opforge-engine/src/processing.rs` for editor-routing boundaries

The high-level host boundary is described in `documentation/libopforge-developer-guide.md`.
