# libopforge Library Specification

**Version:** 0.1-draft
**Date:** March 3, 2026
**Status:** Proposal

---

## 1. Executive Summary

opForge is currently structured as a CLI-first assembler with library-like modules internally, but no formal public API boundary. This specification defines a clean, layered library (`libopforge`) with a stable Rust API surface, a C/C++ FFI binding layer, and explicit separation between: (1) opForge language-core semantics (modules, macros, segments, expressions, repetitions, conditionals), (2) assembler-specific orchestration (passes, CPU selection, listing/map/bin output), (3) CPU/family registry, (4) VM/runtime services, and (5) package loading for `.opcore` and `.opcpu` artifacts.

The goal: any host (Rust crate, C/C++ application, WASM module, GUI IDE, language server) can embed opForge as a library and assemble source to binary without touching CLI code, filesystem assumptions, or global state.

---

## 2. Design Principles

| Principle | Rationale |
|---|---|
| **No global mutable state** | Library must be safely embeddable in multi-threaded hosts |
| **No direct I/O** | All file access goes through caller-provided trait objects; library never touches stdout/stderr/filesystem directly |
| **Builder pattern for configuration** | Replaces CLI arg parsing; host constructs an `AssemblerConfig` |
| **Opaque handles for FFI** | C callers get `OpForgeContext*`; all mutation goes through functions |
| **Error-as-value** | No panics cross the API boundary; all errors are structured and recoverable |
| **Feature-gated layers** | Core-language, assembler, VM/runtime, and FFI can be composed independently |
| **Separate package domains** | `.opcore` (language/runtime core features) and `.opcpu` (CPU/ISA encoding features) are independently loadable |

---

## 3. Crate Layout

```
opforge/
├── Cargo.toml                    # workspace root
├── crates/
│   ├── opforge-core/             # Language-core semantics: modules/macros/segments/expressions/repetitions/conditionals + diagnostics
│   │   └── Cargo.toml
│   ├── opforge-asm/              # Assembler-oriented behavior: passes, directives, listing/map/bin policy
│   │   └── Cargo.toml
│   ├── opforge-registry/         # Assembler-specific family/CPU/dialect registry (no package loading)
│   │   └── Cargo.toml
│   ├── opforge-package/          # VM package loader/validator for `.opcore` and `.opcpu`
│   │   └── Cargo.toml
│   ├── opforge-families/         # Host-pipeline family modules (feature-gated)
│   │   └── Cargo.toml
│   ├── opforge-vm/               # VM runtime, token bridge, encoding bridge
│   │   └── Cargo.toml
│   ├── opforge-engine/           # Pipeline orchestration over core + asm + registry + package + VM
│   │   └── Cargo.toml
│   ├── opforge-lib/              # Public Rust API surface (re-exports + builder)
│   │   └── Cargo.toml
│   ├── opforge-ffi/              # C/C++ FFI binding layer (cdylib + staticlib)
│   │   ├── Cargo.toml
│   │   └── include/
│   │       └── opforge.h         # Generated C header
│   └── opforge-cli/              # CLI binary (thin wrapper over opforge-lib)
│       └── Cargo.toml
└── opforge-lsp/                  # Language server (consumes opforge-lib)
    └── Cargo.toml
```

### Dependency graph

```
opforge-cli ──► opforge-lib ──► opforge-engine ──► opforge-asm ──► opforge-registry
                                    │                    │
opforge-lsp ──► opforge-lib         ▼                    ▼
                                opforge-core       opforge-families (feature-gated)
                                    │
opforge-ffi ──► opforge-lib         ▼
                                opforge-package ──► opforge-vm
```

---

## 4. Public Rust API (`opforge-lib`)

### 4.1 Core Types

```rust
/// Re-export everything a library consumer needs.
pub use opforge_core::diagnostics::{Diagnostic, DiagnosticLevel, DiagnosticCode};
pub use opforge_core::symbol::{SymbolTable, Symbol, SymbolKind};
pub use opforge_core::image::{ImageStore, Segment};
pub use opforge_package::{PackageManager, OpcorePackageSource, OpcpuPackageSource};
pub use opforge_registry::{Registry, CpuId, FamilyId, DialectId};
pub use opforge_engine::listing::ListingLine;

/// Assembled output from a successful assembly run.
pub struct AssemblyResult {
    /// Raw binary image, keyed by segment/region name.
    pub images: ImageStore,
    /// Symbol table after final pass.
    pub symbols: SymbolTable,
    /// Listing lines (if listing was requested).
    pub listing: Option<Vec<ListingLine>>,
    /// Hex output string (if hex was requested).
    pub hex: Option<String>,
    /// Map file content (if map was requested).
    pub map: Option<String>,
    /// Diagnostics (warnings that did not prevent assembly).
    pub diagnostics: Vec<Diagnostic>,
}

/// All errors from a failed assembly run.
pub struct AssemblyError {
    /// Fatal and error-level diagnostics.
    pub diagnostics: Vec<Diagnostic>,
}
```

### 4.2 Configuration Builder

```rust
/// Output format selection.
#[derive(Debug, Clone, Default)]
pub struct OutputConfig {
    pub emit_listing: bool,
    pub emit_hex: bool,
    pub emit_bin: bool,
    pub emit_map: bool,
    pub bin_range: Option<(u64, u64)>,
}

/// Controls how the assembler resolves source and includes.
pub trait SourceProvider: Send + Sync {
    /// Read the contents of a source file by path.
    /// The path is as written in `.include` directives or the root input.
    fn read_source(&self, path: &str) -> Result<String, std::io::Error>;

    /// Resolve a relative include path against a parent file.
    /// Returns the canonical path the library should use for deduplication.
    fn resolve_include(&self, parent: &str, relative: &str) -> Result<String, std::io::Error>;

    /// List files in a directory (for module discovery with `-i`).
    fn list_directory(&self, path: &str) -> Result<Vec<String>, std::io::Error>;
}

/// Controls where assembled output goes.
pub trait OutputSink: Send + Sync {
    /// Write binary output for a named segment/file.
    fn write_binary(&self, name: &str, data: &[u8]) -> Result<(), std::io::Error>;

    /// Write text output (listing, hex, map).
    fn write_text(&self, name: &str, content: &str) -> Result<(), std::io::Error>;
}

/// Main configuration for an assembly session.
pub struct AssemblerConfig {
    /// Root source file path (as understood by the SourceProvider).
    pub root_source: String,

    /// CPU to target (e.g. "z80", "8085", "65c02", "45gs02").
    /// If None, must be set via `.cpu` directive in source.
    pub cpu: Option<String>,

    /// Preprocessor defines (-D equivalent).
    pub defines: Vec<(String, Option<String>)>,

    /// Additional include search directories (-i equivalent).
    pub include_paths: Vec<String>,

    /// Output configuration.
    pub output: OutputConfig,

    /// Explicit package configuration for language core and CPU extensions.
    pub package_config: PackageConfig,

    /// Case-sensitive labels (default: true).
    pub case_sensitive: bool,

    /// Maximum errors before aborting (0 = unlimited).
    pub max_errors: usize,
}

/// Package source configuration for VM-backed features.
#[derive(Debug, Clone, Default)]
pub struct PackageConfig {
    /// Optional explicit `.opcore` package source.
    pub opcore_package: Option<OpcorePackageSource>,
    /// Optional explicit `.opcpu` package source.
    pub opcpu_package: Option<OpcpuPackageSource>,
}

/// Where to source the `.opcore` package.
pub enum OpcorePackageSource {
    /// Load from a file path.
    File(String),
    /// Load from raw bytes (e.g. embedded by host).
    Bytes(Vec<u8>),
    /// Use the bundled/default package (if available in this build).
    Bundled,
}

/// Where to source the `.opcpu` package.
pub enum OpcpuPackageSource {
    /// Load from a file path.
    File(String),
    /// Load from raw bytes (e.g. embedded by host).
    Bytes(Vec<u8>),
    /// Use the bundled/default package (if available in this build).
    Bundled,
}
```

### 4.3 Assembler Entry Point

```rust
/// An opForge assembler session.
///
/// Holds registry, VM state, and configuration for one assembly run.
/// Not reusable across runs; create a new instance per assembly.
pub struct Assembler {
    config: AssemblerConfig,
    registry: Registry,
    package_manager: PackageManager,
    // ... internal state
}

impl Assembler {
    /// Create a new assembler with the given configuration.
    pub fn new(config: AssemblerConfig) -> Result<Self, AssemblyError> {
        // Initializes registry, initializes package manager,
        // loads `.opcore` / `.opcpu` packages if specified,
        // validates CPU selection.
        todo!()
    }

    /// Run the full assembly pipeline.
    ///
    /// Source is read via `source`, output is written via `sink`.
    /// Returns structured results on success, structured errors on failure.
    pub fn assemble(
        &mut self,
        source: &dyn SourceProvider,
        sink: &dyn OutputSink,
    ) -> Result<AssemblyResult, AssemblyError> {
        todo!()
    }

    /// Run assembly but only return diagnostics (no output written).
    /// Useful for IDE/LSP "check" mode.
    pub fn check(
        &mut self,
        source: &dyn SourceProvider,
    ) -> Vec<Diagnostic> {
        todo!()
    }

    /// Query the registry for supported CPUs.
    pub fn supported_cpus(&self) -> Vec<CpuInfo> {
        todo!()
    }

    /// Query the registry for supported families.
    pub fn supported_families(&self) -> Vec<FamilyInfo> {
        todo!()
    }

    /// Query build profile information.
    pub fn build_profile(&self) -> BuildProfile {
        todo!()
    }
}

/// Metadata about a supported CPU.
#[derive(Debug, Clone)]
pub struct CpuInfo {
    pub id: String,
    pub family: String,
    pub aliases: Vec<String>,
    pub dialects: Vec<String>,
}

/// Metadata about a supported family.
#[derive(Debug, Clone)]
pub struct FamilyInfo {
    pub id: String,
    pub cpus: Vec<String>,
    pub description: String,
}

/// Build configuration metadata.
#[derive(Debug, Clone)]
pub struct BuildProfile {
    pub version: String,
    pub runtime_mode: String,   // "full-runtime" or "vm-only"
    pub package_mode: String,   // "bundled", "unbundled", etc.
}
```

### 4.4 Convenience Implementations

```rust
use std::path::PathBuf;
use std::collections::HashMap;

/// Filesystem-backed source provider (default for CLI usage).
pub struct FileSystemSource {
    /// Base directory for relative path resolution.
    pub base_dir: PathBuf,
    /// Additional include search paths.
    pub include_paths: Vec<PathBuf>,
}

impl SourceProvider for FileSystemSource {
    fn read_source(&self, path: &str) -> Result<String, std::io::Error> { todo!() }
    fn resolve_include(&self, parent: &str, relative: &str) -> Result<String, std::io::Error> { todo!() }
    fn list_directory(&self, path: &str) -> Result<Vec<String>, std::io::Error> { todo!() }
}

/// In-memory source provider (useful for testing, IDE integration, WASM).
pub struct InMemorySource {
    pub files: HashMap<String, String>,
}

impl SourceProvider for InMemorySource {
    fn read_source(&self, path: &str) -> Result<String, std::io::Error> { todo!() }
    fn resolve_include(&self, parent: &str, relative: &str) -> Result<String, std::io::Error> { todo!() }
    fn list_directory(&self, path: &str) -> Result<Vec<String>, std::io::Error> { todo!() }
}

/// Filesystem-backed output sink (default for CLI usage).
pub struct FileSystemSink {
    pub output_dir: PathBuf,
}

impl OutputSink for FileSystemSink {
    fn write_binary(&self, name: &str, data: &[u8]) -> Result<(), std::io::Error> { todo!() }
    fn write_text(&self, name: &str, content: &str) -> Result<(), std::io::Error> { todo!() }
}

/// In-memory output sink (captures output without filesystem).
pub struct InMemorySink {
    pub binaries: HashMap<String, Vec<u8>>,
    pub texts: HashMap<String, String>,
}

impl OutputSink for InMemorySink {
    fn write_binary(&self, name: &str, data: &[u8]) -> Result<(), std::io::Error> { todo!() }
    fn write_text(&self, name: &str, content: &str) -> Result<(), std::io::Error> { todo!() }
}
```

### 4.5 Diagnostic Model

```rust
/// Severity level for a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum DiagnosticLevel {
    Info,
    Warning,
    Error,
    Fatal,
}

/// Structured diagnostic code (matches existing asm### codes).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DiagnosticCode(pub String);

/// Source location for a diagnostic.
#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: Option<usize>,
}

/// A single diagnostic message.
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: DiagnosticLevel,
    pub code: DiagnosticCode,
    pub message: String,
    pub location: Option<SourceLocation>,
    /// The source line text (for display).
    pub source_line: Option<String>,
    /// Optional suggested fix or additional context.
    pub notes: Vec<String>,
}
```

---

## 5. C/C++ FFI Layer (`opforge-ffi`)

### 5.1 Design

- Built as both `cdylib` (shared) and `staticlib` (static).
- All public symbols prefixed with `opforge_`.
- Opaque handle types (`OpForgeContext`, `OpForgeResult`).
- Strings passed as `const char*` (UTF-8, null-terminated); returned strings are library-owned and freed via `opforge_string_free()`.
- Errors communicated via return codes + `opforge_last_error()`.
- Thread-safe: each `OpForgeContext` is independent.

### 5.2 C Header

```c
#ifndef OPFORGE_H
#define OPFORGE_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ── Opaque handles ─────────────────────────────────────── */

typedef struct OpForgeContext OpForgeContext;
typedef struct OpForgeResult  OpForgeResult;
typedef struct OpForgeDiagnosticIterator OpForgeDiagnosticIterator;

/* ── Error codes ────────────────────────────────────────── */

typedef enum {
    OPFORGE_OK              = 0,
    OPFORGE_ERR_INVALID_ARG = 1,
    OPFORGE_ERR_IO          = 2,
    OPFORGE_ERR_ASSEMBLY    = 3,
    OPFORGE_ERR_CONFIG      = 4,
    OPFORGE_ERR_INTERNAL    = 99,
} OpForgeStatus;

typedef enum {
    OPFORGE_DIAG_INFO    = 0,
    OPFORGE_DIAG_WARNING = 1,
    OPFORGE_DIAG_ERROR   = 2,
    OPFORGE_DIAG_FATAL   = 3,
} OpForgeDiagLevel;

/* ── Source provider callback table ─────────────────────── */

typedef struct {
    /// Read file contents. Caller provides buffer; callback fills it.
    /// Returns 0 on success, non-zero on error.
    /// If buf is NULL, set *len to required size and return 0.
    int (*read_source)(
        void* user_data,
        const char* path,
        char* buf,
        size_t* len
    );

    /// Resolve an include path relative to a parent.
    /// Writes resolved path into out_path (max out_len bytes).
    int (*resolve_include)(
        void* user_data,
        const char* parent_path,
        const char* relative_path,
        char* out_path,
        size_t out_len
    );

    /// List files in a directory. Writes null-separated list into buf.
    int (*list_directory)(
        void* user_data,
        const char* path,
        char* buf,
        size_t* len
    );

    /// Opaque pointer passed to all callbacks.
    void* user_data;
} OpForgeSourceProvider;

/* ── Output sink callback table ─────────────────────────── */

typedef struct {
    /// Write binary data for a named output.
    int (*write_binary)(
        void* user_data,
        const char* name,
        const uint8_t* data,
        size_t len
    );

    /// Write text data for a named output.
    int (*write_text)(
        void* user_data,
        const char* name,
        const char* content,
        size_t len
    );

    void* user_data;
} OpForgeOutputSink;

/* ── Lifecycle ──────────────────────────────────────────── */

/**
 * Create a new assembler context.
 * root_source: path to the main source file (as understood by provider).
 * Returns NULL on failure; call opforge_last_error() for details.
 */
OpForgeContext* opforge_context_new(const char* root_source);

/** Destroy an assembler context and free all resources. */
void opforge_context_free(OpForgeContext* ctx);

/* ── Configuration ──────────────────────────────────────── */

/** Set target CPU (e.g. "z80", "6502", "45gs02"). */
OpForgeStatus opforge_set_cpu(OpForgeContext* ctx, const char* cpu);

/** Add a preprocessor define. value may be NULL. */
OpForgeStatus opforge_add_define(
    OpForgeContext* ctx,
    const char* name,
    const char* value
);

/** Add an include search path. */
OpForgeStatus opforge_add_include_path(
    OpForgeContext* ctx,
    const char* path
);

/** Enable/disable listing output. */
OpForgeStatus opforge_set_emit_listing(OpForgeContext* ctx, int enable);

/** Enable/disable hex output. */
OpForgeStatus opforge_set_emit_hex(OpForgeContext* ctx, int enable);

/** Enable/disable binary output. */
OpForgeStatus opforge_set_emit_bin(OpForgeContext* ctx, int enable);

/** Enable/disable map output. */
OpForgeStatus opforge_set_emit_map(OpForgeContext* ctx, int enable);

/** Set binary output range (e.g. 0x0000, 0x7FFF). */
OpForgeStatus opforge_set_bin_range(
    OpForgeContext* ctx,
    uint64_t start,
    uint64_t end
);

/** Load an opcore package from file. */
OpForgeStatus opforge_load_opcore_package_file(
    OpForgeContext* ctx,
    const char* path
);

/** Load an opcore package from memory. */
OpForgeStatus opforge_load_opcore_package_bytes(
    OpForgeContext* ctx,
    const uint8_t* data,
    size_t len
);

/** Load an opcpu package from file. */
OpForgeStatus opforge_load_opcpu_package_file(
    OpForgeContext* ctx,
    const char* path
);

/** Load an opcpu package from memory. */
OpForgeStatus opforge_load_opcpu_package_bytes(
    OpForgeContext* ctx,
    const uint8_t* data,
    size_t len
);

/** Set maximum error count (0 = unlimited). */
OpForgeStatus opforge_set_max_errors(OpForgeContext* ctx, size_t count);

/* ── Assembly ───────────────────────────────────────────── */

/**
 * Run the full assembly pipeline.
 * Returns OPFORGE_OK on success, OPFORGE_ERR_ASSEMBLY on errors.
 * Diagnostics are available via opforge_diagnostic_*() functions.
 */
OpForgeStatus opforge_assemble(
    OpForgeContext* ctx,
    const OpForgeSourceProvider* source,
    const OpForgeOutputSink* sink
);

/**
 * Check-only mode: parse and validate, no output.
 * Returns OPFORGE_OK; diagnostics available via opforge_diagnostic_*().
 */
OpForgeStatus opforge_check(
    OpForgeContext* ctx,
    const OpForgeSourceProvider* source
);

/* ── Results & Diagnostics ──────────────────────────────── */

/** Get the number of diagnostics from the last run. */
size_t opforge_diagnostic_count(const OpForgeContext* ctx);

/** Get diagnostic severity at index. */
OpForgeDiagLevel opforge_diagnostic_level(
    const OpForgeContext* ctx,
    size_t index
);

/** Get diagnostic code string at index. Caller must NOT free. */
const char* opforge_diagnostic_code(
    const OpForgeContext* ctx,
    size_t index
);

/** Get diagnostic message at index. Caller must NOT free. */
const char* opforge_diagnostic_message(
    const OpForgeContext* ctx,
    size_t index
);

/** Get diagnostic file path at index. May return NULL. */
const char* opforge_diagnostic_file(
    const OpForgeContext* ctx,
    size_t index
);

/** Get diagnostic line number at index. Returns 0 if unknown. */
size_t opforge_diagnostic_line(
    const OpForgeContext* ctx,
    size_t index
);

/* ── Introspection ──────────────────────────────────────── */

/**
 * Get symbol value by name after assembly.
 * Returns OPFORGE_OK and writes value if found.
 */
OpForgeStatus opforge_symbol_value(
    const OpForgeContext* ctx,
    const char* name,
    int64_t* out_value
);

/**
 * Get binary image for a named segment.
 * Sets *out_data and *out_len. Data is owned by ctx; do not free.
 */
OpForgeStatus opforge_image_data(
    const OpForgeContext* ctx,
    const char* segment_name,
    const uint8_t** out_data,
    size_t* out_len
);

/** Get number of supported CPUs. */
size_t opforge_cpu_count(const OpForgeContext* ctx);

/** Get CPU name at index. Caller must NOT free. */
const char* opforge_cpu_name(const OpForgeContext* ctx, size_t index);

/** Get CPU family at index. Caller must NOT free. */
const char* opforge_cpu_family(const OpForgeContext* ctx, size_t index);

/* ── Build info ─────────────────────────────────────────── */

/** Get library version string. Caller must NOT free. */
const char* opforge_version(void);

/** Get runtime mode ("full-runtime" or "vm-only"). */
const char* opforge_runtime_mode(void);

/** Get opcore package mode ("bundled", "unbundled", etc.). */
const char* opforge_opcore_package_mode(void);

/** Get opcpu package mode ("bundled", "unbundled", etc.). */
const char* opforge_opcpu_package_mode(void);

/* ── Utility ────────────────────────────────────────────── */

/** Get last error message (thread-local). Caller must NOT free. */
const char* opforge_last_error(void);

/** Free a string returned by opforge (when documented as caller-owned). */
void opforge_string_free(char* s);

#ifdef __cplusplus
}
#endif

#endif /* OPFORGE_H */
```

### 5.3 C++ Wrapper (optional, header-only)

```cpp
#pragma once
#include "opforge.h"
#include <string>
#include <vector>
#include <stdexcept>
#include <memory>

namespace opforge {

struct Diagnostic {
    OpForgeDiagLevel level;
    std::string code;
    std::string message;
    std::string file;
    size_t line;
};

class Context {
    struct Deleter {
        void operator()(OpForgeContext* p) const { opforge_context_free(p); }
    };
    std::unique_ptr<OpForgeContext, Deleter> ctx_;

    void check(OpForgeStatus s) const {
        if (s != OPFORGE_OK) {
            const char* msg = opforge_last_error();
            throw std::runtime_error(msg ? msg : "opforge error");
        }
    }

public:
    explicit Context(const std::string& root_source)
        : ctx_(opforge_context_new(root_source.c_str()))
    {
        if (!ctx_) {
            const char* msg = opforge_last_error();
            throw std::runtime_error(msg ? msg : "failed to create context");
        }
    }

    void set_cpu(const std::string& cpu) {
        check(opforge_set_cpu(ctx_.get(), cpu.c_str()));
    }

    void add_define(const std::string& name, const std::string& value = "") {
        check(opforge_add_define(
            ctx_.get(), name.c_str(),
            value.empty() ? nullptr : value.c_str()
        ));
    }

    void add_include_path(const std::string& path) {
        check(opforge_add_include_path(ctx_.get(), path.c_str()));
    }

    void set_emit_listing(bool enable) {
        check(opforge_set_emit_listing(ctx_.get(), enable ? 1 : 0));
    }

    void set_emit_hex(bool enable) {
        check(opforge_set_emit_hex(ctx_.get(), enable ? 1 : 0));
    }

    void load_opcpu_package(const std::string& path) {
        check(opforge_load_opcpu_package_file(ctx_.get(), path.c_str()));
    }

    void load_opcore_package(const std::string& path) {
        check(opforge_load_opcore_package_file(ctx_.get(), path.c_str()));
    }

    OpForgeStatus assemble(
        const OpForgeSourceProvider& source,
        const OpForgeOutputSink& sink
    ) {
        return opforge_assemble(ctx_.get(), &source, &sink);
    }

    std::vector<Diagnostic> diagnostics() const {
        std::vector<Diagnostic> out;
        size_t n = opforge_diagnostic_count(ctx_.get());
        out.reserve(n);
        for (size_t i = 0; i < n; ++i) {
            Diagnostic d;
            d.level   = opforge_diagnostic_level(ctx_.get(), i);
            d.code    = opforge_diagnostic_code(ctx_.get(), i) ? opforge_diagnostic_code(ctx_.get(), i) : "";
            d.message = opforge_diagnostic_message(ctx_.get(), i) ? opforge_diagnostic_message(ctx_.get(), i) : "";
            d.file    = opforge_diagnostic_file(ctx_.get(), i) ? opforge_diagnostic_file(ctx_.get(), i) : "";
            d.line    = opforge_diagnostic_line(ctx_.get(), i);
            out.push_back(std::move(d));
        }
        return out;
    }

    static std::string version() {
        return opforge_version();
    }
};

} // namespace opforge
```

---

## 6. Feature Flags

| Feature | Default | Effect |
|---|---|---|
| `core-language` | **yes** | Enables language-core semantics from `opforge-core` |
| `asm-engine` | **yes** | Enables assembler orchestration from `opforge-asm` / `opforge-engine` |
| `full-runtime` | **yes** | Includes host family/CPU pipeline modules |
| `vm-runtime-only` | no | Excludes host family pipeline; VM-only encoding path |
| `packages-opcore-bundled` | **yes** | Bundles default `.opcore` package |
| `packages-opcore-unbundled` | no | No bundled `.opcore`; host must provide package |
| `packages-opcpu-bundled` | **yes** | Bundles default `.opcpu` package |
| `packages-opcpu-unbundled` | no | No bundled `.opcpu`; host must provide package |
| `ffi` | no | Builds C/C++ FFI layer (`cdylib` + `staticlib`) |
| `providers-fs` | **yes** | Includes `FileSystemSource` / `FileSystemSink` |
| `providers-mem` | **yes** | Includes `InMemorySource` / `InMemorySink` |

---

## 7. Cargo.toml Structure (opforge-lib)

```toml
[package]
name = "opforge-lib"
version = "0.10.0"
edition = "2021"
description = "opForge cross-CPU assembler library"
license = "MIT OR Apache-2.0"

[features]
default = ["core-language", "asm-engine", "full-runtime", "providers-fs", "providers-mem"]
core-language = ["opforge-core"]
asm-engine = ["opforge-asm", "opforge-engine"]
full-runtime = ["opforge-families"]
vm-runtime-only = ["opforge-vm"]
packages-opcore-bundled = ["opforge-package/opcore-bundled"]
packages-opcore-unbundled = ["opforge-package/opcore-unbundled"]
packages-opcpu-bundled = ["opforge-package/opcpu-bundled"]
packages-opcpu-unbundled = ["opforge-package/opcpu-unbundled"]
providers-fs = []
providers-mem = []

[dependencies]
opforge-core = { path = "../opforge-core" }
opforge-asm = { path = "../opforge-asm" }
opforge-registry = { path = "../opforge-registry" }
opforge-engine = { path = "../opforge-engine" }
opforge-package = { path = "../opforge-package" }
opforge-vm = { path = "../opforge-vm", optional = true }
opforge-families = { path = "../opforge-families", optional = true }
```

---

## 8. Usage Examples

### 8.1 Rust — Assemble from files

```rust
use opforge_lib::*;
use opforge_lib::providers::{FileSystemSource, FileSystemSink};
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let config = AssemblerConfig {
        root_source: "main.asm".into(),
        cpu: Some("z80".into()),
        defines: vec![("DEBUG".into(), Some("1".into()))],
        include_paths: vec!["./include".into()],
        output: OutputConfig {
            emit_listing: true,
            emit_hex: true,
            emit_bin: true,
            ..Default::default()
        },
        package_config: PackageConfig {
            opcore_package: None,
            opcpu_package: None,
        },
        case_sensitive: true,
        max_errors: 0,
    };

    let mut asm = Assembler::new(config)?;

    let source = FileSystemSource {
        base_dir: PathBuf::from("."),
        include_paths: vec![PathBuf::from("./include")],
    };

    let sink = FileSystemSink {
        output_dir: PathBuf::from("./build"),
    };

    let result = asm.assemble(&source, &sink)?;

    println!("Assembly succeeded: {} symbols defined", result.symbols.len());
    for diag in &result.diagnostics {
        eprintln!("[{:?}] {}: {}", diag.level, diag.code.0, diag.message);
    }

    Ok(())
}
```

### 8.2 Rust — Assemble from memory (IDE/LSP)

```rust
use opforge_lib::*;
use opforge_lib::providers::{InMemorySource, InMemorySink};
use std::collections::HashMap;

fn check_source(source_text: &str) -> Vec<Diagnostic> {
    let config = AssemblerConfig {
        root_source: "editor.asm".into(),
        cpu: Some("6502".into()),
        ..Default::default()
    };

    let mut asm = Assembler::new(config).unwrap();

    let mut files = HashMap::new();
    files.insert("editor.asm".into(), source_text.into());

    let source = InMemorySource { files };

    asm.check(&source)
}
```

### 8.3 C — Assemble from files

```c
#include "opforge.h"
#include <stdio.h>
#include <string.h>

/* Simple file-based source provider */
static int read_file(void* ud, const char* path, char* buf, size_t* len) {
    FILE* f = fopen(path, "rb");
    if (!f) return 1;
    fseek(f, 0, SEEK_END);
    size_t sz = (size_t)ftell(f);
    if (!buf) { *len = sz; fclose(f); return 0; }
    if (sz > *len) { fclose(f); return 1; }
    fseek(f, 0, SEEK_SET);
    *len = fread(buf, 1, sz, f);
    fclose(f);
    return 0;
}

static int write_bin(void* ud, const char* name,
                     const uint8_t* data, size_t len) {
    char path[256];
    snprintf(path, sizeof(path), "build/%s", name);
    FILE* f = fopen(path, "wb");
    if (!f) return 1;
    fwrite(data, 1, len, f);
    fclose(f);
    return 0;
}

int main(void) {
    printf("opForge %s (%s, opcore=%s, opcpu=%s)\n",
           opforge_version(),
           opforge_runtime_mode(),
           opforge_opcore_package_mode(),
           opforge_opcpu_package_mode());

    OpForgeContext* ctx = opforge_context_new("main.asm");
    if (!ctx) {
        fprintf(stderr, "Error: %s\n", opforge_last_error());
        return 1;
    }

    opforge_set_cpu(ctx, "8085");
    opforge_add_define(ctx, "MFORTH_CHANGE", "1201");
    opforge_add_include_path(ctx, "./src");
    opforge_set_emit_bin(ctx, 1);
    opforge_set_emit_listing(ctx, 1);
    opforge_set_emit_hex(ctx, 1);

    OpForgeSourceProvider src = {
        .read_source = read_file,
        .resolve_include = NULL,  /* use default resolution */
        .list_directory = NULL,
        .user_data = NULL
    };

    OpForgeOutputSink sink = {
        .write_binary = write_bin,
        .write_text = NULL,  /* discard text output */
        .user_data = NULL
    };

    OpForgeStatus status = opforge_assemble(ctx, &src, &sink);

    size_t n = opforge_diagnostic_count(ctx);
    for (size_t i = 0; i < n; i++) {
        const char* file = opforge_diagnostic_file(ctx, i);
        printf("[%d] %s:%zu: %s: %s\n",
               opforge_diagnostic_level(ctx, i),
               file ? file : "<unknown>",
               opforge_diagnostic_line(ctx, i),
               opforge_diagnostic_code(ctx, i),
               opforge_diagnostic_message(ctx, i));
    }

    if (status == OPFORGE_OK) {
        const uint8_t* data;
        size_t len;
        if (opforge_image_data(ctx, "default", &data, &len) == OPFORGE_OK) {
            printf("Image: %zu bytes\n", len);
        }
    }

    opforge_context_free(ctx);
    return status == OPFORGE_OK ? 0 : 1;
}
```

---

## 9. Migration Plan

### Phase 1 — Extract `opforge-core` (v0.10.0)
- Move language-core modules (modules, macros, segments, expressions, repetitions, conditionals, diagnostics) into `crates/opforge-core/`.
- No public API changes; CLI binary re-exports from internal path.
- All tests pass unchanged.

### Phase 2 — Extract `opforge-asm` + `opforge-registry` + `opforge-engine` (v0.11.0)
- Split assembler behavior, registry, and orchestration into separate crates.
- Keep `opforge-registry` assembler-specific (family/CPU/dialect only).
- Introduce `SourceProvider` / `OutputSink` traits internally.
- CLI still owns I/O; engine uses traits.

### Phase 3 — Introduce `opforge-package` + dual package loading (v0.12.0)
- Add package loader/validator crate dedicated to `.opcore` and `.opcpu` artifacts.
- Remove VM package loading responsibilities from `opforge-registry`.
- Define package precedence rules (explicit path/bytes > bundled fallback).

### Phase 4 — Publish `opforge-lib` (v0.13.0)
- Create `opforge-lib` re-export crate with builder API.
- Refactor CLI to be a thin wrapper over `opforge-lib`.
- Refactor LSP to consume `opforge-lib`.
- All existing tests pass; add library-level integration tests.

### Phase 5 — FFI layer (v0.14.0)
- Implement `opforge-ffi` crate.
- Generate `opforge.h` (manually maintained or via `cbindgen`).
- Add C integration tests.
- Ship `libopforge.a` + `libopforge.so`/`.dylib`/`.dll` in release artifacts.

### Phase 6 — Feature-gate families and package bundles (v0.15.0)
- Move family modules behind `full-runtime` feature in `opforge-families`.
- VM-only builds exclude `opforge-families` entirely.
- Allow independent bundle/unbundle selection for `.opcore` and `.opcpu` defaults.
- Measure and document binary size reduction.

---

## 10. Testing Strategy

| Layer | Test Type | Location |
|---|---|---|
| `opforge-core` | Unit tests for language-core semantics (modules/macros/segments/expressions/repetitions/conditionals) | `crates/opforge-core/src/*/tests.rs` |
| `opforge-asm` | Unit tests for assembler directives, pass behavior, listing/map policy | `crates/opforge-asm/src/*/tests.rs` |
| `opforge-registry` | Unit tests for registration, lookup, alias resolution | `crates/opforge-registry/src/tests.rs` |
| `opforge-package` | Unit tests for `.opcore` / `.opcpu` manifest loading and validation | `crates/opforge-package/src/tests.rs` |
| `opforge-engine` | Integration tests using `InMemorySource`/`InMemorySink` | `crates/opforge-engine/tests/` |
| `opforge-lib` | End-to-end library API tests | `crates/opforge-lib/tests/` |
| `opforge-ffi` | C integration tests (compiled + run via `cargo test`) | `crates/opforge-ffi/tests/` |
| `opforge-cli` | Existing example/reference comparison tests | `crates/opforge-cli/tests/` |
| Cross-feature | Build-combo matrix (`make test-build-combo-smoke`) | CI workflow |

---

## 11. Versioning & Compatibility

- Library follows SemVer.
- Rust API stability begins at `1.0.0` (after Phase 3 stabilizes).
- C ABI stability: functions in `opforge.h` are stable once published; new functions are additive; removed functions go through a deprecation cycle.
- `opforge.h` includes version macros:

```c
#define OPFORGE_VERSION_MAJOR 0
#define OPFORGE_VERSION_MINOR 10
#define OPFORGE_VERSION_PATCH 0
```

---

## 12. Open Questions

| # | Question | Impact |
|---|---|---|
| 1 | Should `SourceProvider` be async-capable for WASM/IDE use? | Trait design |
| 2 | Should the library expose incremental/partial assembly (e.g. single-file re-check)? | API surface |
| 3 | Should package precedence be configurable per package domain (`.opcore`, `.opcpu`) or fixed globally? | Runtime/package behavior |
| 4 | Should `opforge-ffi` use `cbindgen` for header generation or maintain `opforge.h` manually? | Build tooling |
| 5 | Should diagnostic codes be numeric enums in C or remain string-based? | FFI ergonomics |
| 6 | Should the library support streaming/callback-based listing output for large files? | Memory model |
| 7 | What is the minimum supported Rust version (MSRV) for the library crates? | CI + consumer compatibility |