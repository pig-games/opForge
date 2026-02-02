# opForge Root Module Metadata Directives (v0.1)

Date: 2026-02-02

## Summary

This document specifies **root-module metadata directives** that allow a project’s root module to define output naming and optional descriptive metadata without a separate manifest file.

## Goals

- Keep project metadata **inside the root module**.
- Allow **conditional** metadata selection (per target or build configuration).
- Provide **deterministic output naming** for listings, hex, and binaries.
- Maintain simple CLI usage for small projects (`-i` only).

## Non-Goals

- Dependency resolution or external package management.
- Arbitrary key/value schemas beyond the keys defined here.

## Terminology

- **Root module**: the module associated with the input file (or the single root module inside a folder input) used as the assembly entry point.
- **Output base**: the filename base used to derive .lst/.hex and default .bin names.

## Directive Syntax

Directives are module-scoped and use the standard directive form:

- `.meta.<key> <value>`
- `.name <value>` (only allowed inside `.meta` blocks; sets metadata name unless inside `.output`)

`<value>` is a single operand, typically a string literal or identifier. The value is interpreted as **text** (not an expression for numeric evaluation). `.meta.output.list` and `.meta.output.hex` may omit the value to use the output base.

Examples:

- `.meta.name "My Demo"`
- `.meta.version "1.2.0"`
- `.meta.output.name "demo"`
- `.meta.output.z80.name "demo-z80"`
- `.meta.output.list`
- `.meta.output.hex "demo-hex"`
- `.meta.output.bin "0000:ffff"`
- `.meta.output.fill "ff"`
- `.name "My Demo"`

Output group block form:

```
.meta
  .output
    .name "demo"
    .list
    .z80
      .name "demo-z80"
    .endz80
  .endoutput
.endmeta
```

## Valid Keys

### `.meta.name`
Human-readable project name (does **not** affect output naming directly).

### `.meta.version`
Human-readable version string (semver or arbitrary text).

### `.meta.output.name`
Default **output base** name. If set, this value becomes the default output base for `.lst`, `.hex`, and `.bin` names.

### `.meta.output.<target>.name`
Target-specific output base, where `<target>` matches the selected CPU name (case-insensitive), e.g.:

- `.meta.output.z80.name "demo-z80"`
- `.meta.output.6502.name "demo-6502"`

If a target-specific output name is present, it **overrides** `.meta.output.name` for that CPU.

### `.meta.output.list`
Enable listing output. Accepts an optional filename; if omitted, the output base is used.

### `.meta.output.hex`
Enable Intel HEX output. Accepts an optional filename; if omitted, the output base is used.

### `.meta.output.bin`
Enable binary output with a range. Requires a `ssss:eeee` range value (as text).

### `.meta.output.fill`
Set the binary fill byte (two hex digits). Applies to `.meta.output.bin` output.

### `.name`
Inside a `.meta` block, `.name` sets the metadata name (alias of `.meta.name`).
Inside an `.output` block, `.name` sets the output base name.

## Scope and Placement Rules

- Metadata directives are **valid only inside a module**.
- Metadata directives are **intended for the root module**. Using them in non-root modules is an error.
- Metadata directives are **top-level only** (not inside `.block`/`.namespace` scopes). This avoids ambiguous scope resolution.
- `.list`/`.hex`/`.bin`/`.fill` are allowed only inside `.output` blocks (aliases for `.meta.output.list`/`.meta.output.hex`/`.meta.output.bin`/`.meta.output.fill`).

## Conditional Behavior

Metadata directives respect conditional processing. A directive only applies if it appears in an **active** branch of a conditional block. If multiple directives set the same key, the **last active** directive wins.

## Output Naming Precedence

When assembling a root module:

1. `-o/--outfile` (CLI) **always wins**.
2. `.meta.output.<target>.name` (CPU-specific).
3. `.meta.output.name`.
4. Root folder name (when `-i` is a folder input).
5. Root filename base.

If no output base can be determined, the CLI must provide `-o`.

## Output Selection Precedence

1. `-l/--list`, `-x/--hex`, `-b/--bin` (CLI) **always win**.
2. `.meta.output.list`, `.meta.output.hex`, `.meta.output.bin`.

## Diagnostics

Errors should be raised for:

- Metadata directives outside a module.
- Metadata directives inside non-root modules.
- Metadata directives inside nested scopes (`.block`, `.namespace`).
- Invalid or missing values.

## Example

```
.module main
  .meta.name "LED Demo"
  .meta.version "1.0.0"
  .meta.output.name "led-demo"
  .meta.output.z80.name "led-demo-z80"

  ; ... program ...
.endmodule
```

With CPU `Z80`, outputs default to:

- `led-demo-z80.lst`
- `led-demo-z80.hex`
- `led-demo-z80.bin`

With CPU `8085`, outputs default to:

- `led-demo.lst`
- `led-demo.hex`
- `led-demo.bin`
