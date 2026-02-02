# .use Module Loading: Recommendations & Open Questions

Date: 2026-02-02

## Goal

Make `.use` load the required module if not already loaded. `.include` remains for **literal text inclusion only**, and should not be required for module imports. `.use` should load modules and make **public symbols** available for resolution. Later, when `.dsection`/`.section` exist, `.use` can ensure those sections are processed.

------------------------------------------------------------------------

## Draft Spec: Module-id → File Mapping

### Inputs

- **Module id**: dotted identifier path (case-insensitive).
- **Search roots**:
   - Root module directory (derived from `-i`).
- **Extensions**: fixed list (`.asm`, `.inc`). CLI-configurable extensions are not implemented.

### Resolution Algorithm (deterministic)

1. For each search root (in order):
   - Check files in the **root folder first**, then subfolders (shallow-to-deep) for matches.
   - Match by module-id declarations inside files (case-insensitive).
2. First **unique** match wins.
3. If **no match**, error: “missing module source”.
4. If **multiple matches** across roots/extensions, error: “ambiguous module source”.

### Module Cache Identity

- Cache key: **(module-id, resolved file path, parameter hash)**.
- Module-id matching is case-insensitive; the cache key should store the **canonicalized id**.
- Parameter hash is derived from the `.use` parameter list **after expression evaluation in the importer**.

### `.use` Parameter Semantics

- Parameters are **parsed and stored** but do **not** affect module identity yet.
- Parameter values are parsed as expressions; they are not evaluated/consumed by the loader.

### Conflict Rules (Local vs Import)

- A local definition **must not** reuse a name introduced by selective import in the **same scope**.
- Local definitions in **inner scopes** may shadow selective imports from outer scopes.
- Alias names are module‑scope only and must be unique within the module.

### Diagnostics Format (Import Stack)

- Include an **import stack** for missing/ambiguous/duplicate modules:
   - `root → importer → ... → failing .use`.
- Report the **import site** line/column as the primary span.
- Duplicate module-id across files is a **hard error**; abort assembly.

### Preprocessor State Model

- Preprocessor state is **per‑module** (macros/defines do not leak across module boundaries).
- Explicit sharing must be done via `.use` and public exports, not textual inclusion.

### Conventions

- **Case-insensitive** matching for module ids and filenames.
- The resolved file may define **multiple modules**; only the requested module-id is registered.
- `.include` remains literal text inclusion and does **not** participate in module resolution.

### CLI Implications

- `-i` file → root module file; its directory is the primary search root.
- `-i` folder → project root; must contain a single `main.*` root module.
- `-o` overrides `.meta.output.name` and folder-derived outputs.

------------------------------------------------------------------------

## Recommendations (Implementation + Ordering)

### 1) Define module resolution model

- `.use <module-id>` loads module source by **module-id → file mapping**.
- Each module-id is loaded **once**, cached, and reused.
- A module may be requested by multiple `.use` statements without reloading.

### 2) Add a module loader layer

Introduce a loader stage that runs before assembly passes:

- Build a **module graph** from the entry module(s).
- Resolve `.use` recursively and **eager-load** modules.
- Detect **cycles** early (graph traversal).

### 3) Establish multi-module assembly pipeline

Separate loading from emitting:

- **Phase A (Load/Parse):** read sources, preprocess, macro-expand, parse `.module` blocks.
- **Phase B (Pass 1):** walk all loaded modules to collect symbols and sizes.
- **Phase C (Pass 2):** walk all loaded modules to emit bytes/listing.

### 4) Enforce `.use` semantics

- `.use` valid **only inside a module** and **at module scope**.
- Imported symbols must be **public**.
- `.include` is **literal text injection only**; discourage for modularization.

### 5) Add diagnostics

- Missing module source for a `.use`.
- Duplicate module-id across different files.
- Ambiguous module-id resolution (multiple candidate files).
- Import cycles.
- `.use` outside module scope.

### 6) Decide listing/output ordering

- Establish a deterministic order for listing and emission.
- Recommended: topological order of module graph, with stable source-order tie-breaks.

------------------------------------------------------------------------

## Suggested Implementation Order

1. **Spec decisions**: module-id → file mapping, file layout, ambiguity rules.
2. **Loader & cache**: module graph + cycle detection.
3. **Multi-module assembly**: pass1/pass2 across loaded modules.
4. **Public visibility enforcement** for `.use` imports.
5. **`.include` restriction** + diagnostics/documentation updates.
6. **Tests + examples** for multi-file module loading and error cases.

------------------------------------------------------------------------

## Root Module & Project Metadata (CLI)

- The `-i` input can point to either:
   - a **root module file**, or
   - a **root folder**.
- When `-i` is a folder, that folder is the **project root**, and it must contain **exactly one `main.*` root module** (case-insensitive, `.asm` or `.inc`).
- When `-i` is a file, its directory is the **project root**.
- Optional manifest metadata can be introduced later (name, version, dependencies).
- Future: add dependency management for **external modules** (package-style resolution).
- Alternative to a manifest: introduce **module-level metadata directives** (e.g., `.meta.name`, `.meta.version`, `.meta.output.name`, etc.).
   - Keeps metadata inside the root module.
   - Allows conditional builds via expressions/conditionals.
   - Enables target-specific output naming (e.g., output filename per CPU/target).
   - CLI can remain minimal: `-i` only, with outputs derived from root-module metadata.

------------------------------------------------------------------------

## Additional Recommendations

- Specify a concrete module-id → path resolution spec (extensions, search path precedence, ambiguity rules, CLI flags).
- Define a module cache key (module-id + resolved path + params) and invalidation rules.
- Include import-stack context in diagnostics for missing/duplicate modules.
- Clarify whether `.use` parameters affect module identity (same module with different params?).
- Define conflict rules between local names and selectively imported names across scopes.
- Decide whether per-module `.define` / preprocessor state is isolated or shared.
- Specify listing/output grouping for multi-module builds (per-module headers, stable order).
- Add a clear “no implicit emission” note for future `.section` usage.

------------------------------------------------------------------------

## Open Questions (Resolve First)

1. **Module-id → file mapping**
   - Search roots? extensions? explicit map? CLI flags?
      - Current: single root search directory; extensions fixed to `.asm`/`.inc`.
    - Case sensitivity rules?
       - Decision: case-insensitive module-id matching.

2. **Multiple modules per file**
    - If a file defines multiple modules, how does `.use` target one?
       - Decision: loader parses the file but registers only the targeted module-id.
    - Is module-id required to match filename, or just declared in the file?
       - Decision: declared in the file; filename matching is not required.

3. **Emission/ordering model**
   - Pure topological order? Or preserve source order with dependencies first?
     - Decision: emission is not implicit; symbol use determines output order. For future `.section`, follow 64tass-style ordering: emission into `.dsection` is based on processing order.

4. **Lazy vs eager loading**
   - Load modules immediately on `.use`, or defer until resolution needed?
     - Decision: eager-load on `.use` for now; optimize later if needed.

5. **Diagnostics location**
   - Should missing-module errors point to the **import site** or the **expected module file**?
     - Decision: point to the import site.

6. **Duplicate module-id handling**
   - Error on first duplicate? Which file wins (if any)?
     - Decision: error; no winner; abort assembly.

7. **Implicit root module behavior**
   - If entry file has no `.module`, how is its implicit module-id computed?
     - Decision: derived directly from the filename.

8. **Integration with future `.dsection`/`.section`**
    - When modules are loaded, should section declarations be registered immediately?
       - Decision: make this explicit (e.g., `.section-use my-mod.my-section`).
    - Are sections global or per-module?
       - Decision: sections are bound to the `.dsection` scope.

------------------------------------------------------------------------

## Notes

- `.include` should remain available for cases that need **literal source splicing**, but should not be used to implement modularity.
- `.use` should be the semantic path for **module dependency and symbol visibility**.
