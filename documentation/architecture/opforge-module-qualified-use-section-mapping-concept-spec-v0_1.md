# opForge Module Qualified Use, Section Mapping, and Selective Inclusion Concept Spec v0.1

## Status

Concept specification based on design discussion.

This document captures the current direction for extending opForge modules so that `.use` can support:

- compact qualified module imports;
- qualified symbol references such as `mm.myRoutine`;
- dependency-driven selective binary inclusion;
- section mapping from reusable library modules into consuming modules;
- both fully integrated executable builds and independently packaged libraries;
- output formats such as AmigaOS hunk executables/libraries and C64 OS applications/libraries.

The goal is to preserve opForge's explicitness while removing the current verbosity of long selective `.use` lists.

---

## 1. Problem Statement

The current opForge module approach supports explicit `.use` statements, including selective symbol imports. This is useful and deterministic, but in larger native modules the import list can become too long and too noisy.

Current style:

```asm
.use opforge.cli.constants (SYS_BASE, OPEN_LIBRARY, CLOSE_LIBRARY, GET_ARG_STR)
.use opforge.cli.constants (RETURN_OK, RETURN_USAGE, RETURN_FILE_FAILURE)
.use opforge.cli.state (NativeCliReturnCode, NativeCliDosBase)
.use opforge.cli.state (NativeCliInputPath, NativeCliOutputFormat, NativeCliBinPath)
.use opforge.cli.dos (opforgeNativeCliPutStr, opforgeNativeCliOpenInput)
.use opforge.cli.args (opforgeNativeCliParseArgs)
```

This is explicit, but it scales poorly. It pushes too much dependency detail into the top of each module and encourages long globally unique symbol names.

A preferred direction is:

```asm
.use opforge.cli.constants as c
.use opforge.cli.state as state
.use opforge.cli.dos as dos
.use opforge.cli.args as args

move.l #c.RETURN_USAGE, state.NativeCliReturnCode
jsr dos.putStr
jsr args.parse
```

This keeps dependencies explicit while moving symbol qualification to the use site, where it improves readability.

---

## 2. Core Principle

A `.use` statement should make a module's public namespace available. It should not necessarily include all of that module's code or data in the final binary.

The central distinction is:

```text
module availability != binary inclusion
```

A qualified `.use` makes symbols resolvable:

```asm
.use my.module as mm
```

This means:

```text
mm.* may now be resolved against my.module's public exports.
```

It should not mean:

```text
emit the entire module into the binary.
```

Binary inclusion should be driven by selected roots and the dependency graph of actually referenced symbols.

---

## 3. Qualified Module Imports

### 3.1 Basic Qualified Import

```asm
.use my.module as mm
```

Creates a local module alias `mm` for `my.module`.

Valid use:

```asm
jsr mm.myRoutine
move.l mm.myConstant, d0
lea mm.myTable, a0
```

The alias should not import names into the local namespace by default.

Invalid unless separately imported:

```asm
jsr myRoutine
```

### 3.2 Rationale

Qualified imports allow shorter names inside modules.

Instead of forcing public symbols like:

```asm
opforgeNativeCliParseArgs
opforgeNativeCliWriteFlatOutput
opforgeNativeCliPutStr
```

modules can eventually expose local public names:

```asm
.module opforge.cli.args
.pub
parse:
    ; ...
.endmodule
```

Used as:

```asm
.use opforge.cli.args as args
jsr args.parse
```

The full identity remains structural:

```text
opforge.cli.args::parse
```

but local code reads compactly:

```text
args.parse
```

---

## 4. Selective Imports and Root Selection

The existing selective import syntax remains useful:

```asm
.use my.module (startup)
```

Without an alias, this can continue to make `startup` available directly in the current namespace.

With an alias:

```asm
.use my.module (startup) as mm
```

This should mean:

```text
- bind my.module as qualified namespace mm;
- select startup from my.module;
- treat startup as a root/reachable symbol for inclusion;
- keep the symbol qualified as mm.startup by default.
```

So:

```asm
.use my.module (startup) as mm

jsr mm.startup   ; valid
jsr startup      ; invalid unless also imported directly
```

### 4.1 Root Selection Meaning

The parenthesized list in a qualified `.use` should be treated as selected exported symbols. Selected symbols become roots for dependency-driven inclusion.

Example:

```asm
.use opforge.cli.entry (start)
```

This can be used by a root composition file even if it does not directly call `start` itself. The selected symbol `start` becomes the entry root from which reachability begins.

---

## 5. Dependency-Driven Selective Inclusion

A qualified `.use` makes a namespace available. Referencing a symbol includes only the units needed for that symbol and its dependencies.

Example:

```asm
.use my.module as mm map {
    code -> app_code
    data -> app_data
    bss  -> app_bss
}

jsr mm.myRoutine
```

Conceptual inclusion:

```text
root references mm.myRoutine
include unit defining myRoutine
include units referenced by myRoutine
include data/tables/state required by those units
map included units' logical sections into app sections
exclude unrelated public symbols from my.module
```

This is especially important for constrained targets such as C64/C64 OS, where unused code/data elimination can determine whether a program fits in memory.

---

## 6. Modules, Symbols, Units, and Sections

To support selective inclusion cleanly, modules should compile into a graph-like intermediate representation.

Conceptual structure:

```text
module
  public exports
  internal symbols
  logical section contracts
  units
    unit
      defines symbols
      references symbols
      belongs to logical/concrete section
      contains emitted bytes or reservations
```

A unit is the practical inclusion granularity.

Example:

```text
module opforge.tkvm.scanner
  exports:
    scanNext      -> unit U1
    classifyChar  -> unit U2
    charTable     -> unit U3
  units:
    U1: section code, references classifyChar, charTable
    U2: section code
    U3: section tables
```

Referencing `scanner.scanNext` includes U1, U2, and U3, but not unrelated scanner routines.

---

## 7. Section Contracts and Section Mapping

### 7.1 Current Issue

Some modules currently define concrete sections internally:

```asm
.module opforge.cli.run
.section code, kind=code
    ; ...
.endsection
.endmodule
```

For application-specific modules, this is fine. But reusable library-like modules should not necessarily decide their final concrete section names. One consuming program may want a library's code in `code`, another in `overlay1_code`, another in a C64 OS library section, another in an Amiga hunk object.

### 7.2 Logical Sections

Reusable modules should be able to define logical section contracts:

```asm
.module opforge.tkvm.scanner

.section code, kind=code, logical
    ; scanner routines
.endsection

.section tables, kind=data, logical
    ; read-only tables
.endsection

.section state, kind=bss, logical
    ; reserved state
.endsection

.endmodule
```

These section names are local to the module. They describe what the emitted material is, not where it must finally live.

### 7.3 `.use ... map { ... }`

The importer maps logical module sections into local concrete sections:

```asm
.use opforge.tkvm.scanner as scanner map {
    code   -> code
    tables -> data
    state  -> bss
}
```

Meaning:

```text
opforge.tkvm.scanner::code   -> current module/output section code
opforge.tkvm.scanner::tables -> current module/output section data
opforge.tkvm.scanner::state  -> current module/output section bss
```

A complete form may combine selected roots, aliasing, and mapping:

```asm
.use my.module (startup, irqHandler) as mm map {
    code -> app_code
    data -> app_data
    bss  -> app_bss
}
```

This means:

```text
- use my.module as qualified namespace mm;
- select startup and irqHandler as inclusion roots;
- include their dependencies;
- map included logical code/data/bss units into the specified app sections.
```

---

## 8. Type and Capability Checking for Section Maps

Mappings should be checked against section kinds/capabilities.

Valid:

```asm
.use my.module as mm map {
    code  -> app_code
    state -> app_bss
}
```

Invalid:

```asm
.use my.module as mm map {
    state -> app_code
}
```

Diagnostic example:

```text
error: cannot map my.module::state kind=bss to app_code kind=code
suggestion: map state -> app_bss
```

For 6502/C64 OS, this becomes even more important for zero page.

Possible logical section:

```asm
.section scratch, kind=zp, logical
```

Mapping must target a compatible zero-page section:

```asm
.use presenter.renderer as renderer map {
    scratch -> app_zp
}
```

Invalid unless explicitly allowed:

```asm
.use presenter.renderer as renderer map {
    scratch -> app_bss
}
```

---

## 9. Root Composition Modules

A root composition file can select an entry symbol and define output policy without containing executable code itself.

Example:

```asm
; Native AmigaOS opForge CLI root composition.

.module main
.cpu 68020

.use opforge.cli.entry (start)

.output "build/opforge_cli", format=hunk, sections=entry, code, data, bss
.endmodule
```

Semantics:

```text
- main is the root composition module;
- opforge.cli.entry::start is selected as an inclusion root;
- dependency traversal begins at start;
- all reachable units are included;
- output is emitted as an AmigaOS hunk executable using sections entry, code, data, bss.
```

This keeps output ownership at the root level rather than inside reusable modules.

---

## 10. Fully Integrated Executables and Independent Libraries

The same module graph should support both:

```text
fully integrated executable builds
independent reusable library builds
```

These should be output policies over the same source/module graph, not separate source architectures.

### 10.1 Integrated Executable

```asm
.module main
.cpu 68020

.use opforge.cli.entry (start)

.output "build/opforge_cli", format=hunk, sections=entry, code, data, bss
.endmodule
```

Policy:

```text
selected roots are included
all dependencies are resolved
unresolved symbols are errors
final loadable executable is emitted
```

### 10.2 Library Build

Conceptual example:

```asm
.module build.tkvm
.cpu 68020

.use opforge.tkvm.scanner as scanner
.use opforge.tkvm.predicates as predicates
.use opforge.tkvm.state as state

.output "build/opforge_tkvm.lib",
    format=hunklib,
    exports=scanner.*, predicates.*, state.*
.endmodule
```

Policy:

```text
exported units are packaged as linkable library material
public symbols are preserved/indexed
relocation records are preserved
unresolved references may be emitted as externals
```

---

## 11. Output Format Mapping

The module/unit/section graph should be generic. Output formats consume this graph.

### 11.1 AmigaOS Hunk Executable

Mapping:

```text
opForge kind=code -> HUNK_CODE
opForge kind=data -> HUNK_DATA
opForge kind=bss  -> HUNK_BSS
symbol references -> relocation records
public symbols    -> optional symbols/debug/export records
```

Executable policy:

```text
unresolved references are errors
root symbols are fully resolved
loadable hunks are emitted
```

### 11.2 AmigaOS Hunk Object / Library

Object/library policy:

```text
unresolved references may become externals
relocations are preserved
public symbols are exported
units may become object/library members
```

This supports building native opForge as both:

```text
- one integrated executable;
- reusable native libraries such as tkvm.lib, prvm.lib, opcore.lib.
```

### 11.3 C64 OS Applications and Libraries

The same model should apply to 6502/C64 OS targets.

Application root example:

```asm
.module main
.cpu 6502

.use c64os.app.entry (start)
.use c64os.ui.menu as menu map {
    code   -> app_code
    rodata -> app_rodata
    bss    -> app_bss
    zp     -> app_zp
}

.output "build/MyApp", format=c64os-app,
    sections=header, app_code, app_rodata, app_data, app_bss
.endmodule
```

Library example:

```asm
.module build.presenter.lib
.cpu 6502

.use presenter.slide as slide
.use presenter.template as template
.use presenter.renderer as renderer

.output "build/presenter.lib",
    format=c64os-library,
    exports=slide.*, template.*, renderer.*
.endmodule
```

The same source modules can be built as integrated applications or independent C64 OS libraries.

---

## 12. Always-Included Symbols and Root Sets

Modules may need to express symbols that are intended to be included in some contexts: startup records, plugin descriptors, interrupt vectors, registration tables, test discovery records, etc.

However, `.use my.module as mm` should remain cheap and should not silently include arbitrary module-side symbols.

Recommended rule:

```text
.use makes symbols available.
references and selected roots make code/data included.
```

Instead of unconditional "always include on use", prefer explicit root selection or named root sets.

### 12.1 Explicit Selected Root

```asm
.use my.module (startup) as mm
```

Means `startup` is selected as a root.

### 12.2 Possible Future Root Sets

Module:

```asm
.module my.driver
.root driverDescriptor in registry
.root startup in default
.endmodule
```

Importer:

```asm
.use my.driver as drv roots(registry)
```

This keeps inclusion intentional and avoids surprising binary bloat.

---

## 13. Current Recommended Semantic Ladder

### Qualified lazy import

```asm
.use my.module as mm
```

Meaning:

```text
module namespace is available as mm
nothing is force-included
symbols are included only when referenced
```

### Qualified import with selected root

```asm
.use my.module (startup) as mm
```

Meaning:

```text
module namespace is available as mm
startup is selected as an inclusion root
startup remains qualified as mm.startup by default
```

### Direct selective import

```asm
.use my.module (startup)
```

Meaning:

```text
startup is selected/imported directly into current namespace
startup is an inclusion root if selected by a root composition module
```

### Qualified import with selected roots and section mapping

```asm
.use my.module (startup, irqHandler) as mm map {
    code -> app_code
    data -> app_data
    bss  -> app_bss
}
```

Meaning:

```text
module namespace is available as mm
startup and irqHandler are selected roots
included logical sections are mapped to local concrete sections
```

---

## 14. Impact on Native opForge Restructuring

This model supports the planned native restructuring by allowing modules to become smaller, more reusable, and less dependent on global symbol naming.

Possible direction:

```text
opforge.cli.entry
  concrete executable entry module

opforge.cli.run
  orchestration module

opforge.cli.args
  qualified service module, e.g. args.parse

opforge.cli.dos
  qualified service module, e.g. dos.putStr, dos.openInput

tkvm/*
  library-like modules with logical sections

prvm/*
  library-like modules with logical sections

opcore/*
  reusable VM/core modules, possibly packaged as independent libraries
```

A future native opForge build could produce:

```text
build/opforge_cli          integrated executable
build/opforge_tkvm.lib     tokenizer VM library
build/opforge_prvm.lib     parser VM library
build/opforge_opcore.lib   core VM/library package
```

without duplicating source modules.

---

## 15. Design Summary

The proposed model can be summarized as:

```text
.module defines capability
.use defines composition and namespace binding
map defines layout adaptation
.output defines packaging policy
references and selected roots define inclusion
```

This enables:

- compact qualified imports;
- shorter public names inside modules;
- explicit use-site qualification;
- selective inclusion of only used symbols and dependencies;
- logical section contracts for reusable modules;
- root composition files for executables;
- integrated executable builds;
- independent library builds;
- AmigaOS hunk executable/object/library support;
- C64 OS application/library support;
- future support for overlays, banked memory, and target-specific output models.

The key rule remains:

```text
.use makes symbols available.
references and selected roots make units included.
.output decides how included/reachable units are packaged.
```
