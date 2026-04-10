# Review Report

## Scope

Full review of the current opForge worktree, not limited to the branch diff, with focus on whether the shipped LSP and editor experience is complete for the recent AmigaOS executable Hunk changes and the allowed-notation changes. This review includes the real Rust language server in [crates/opforge-lsp/src/lib.rs](/Users/erik/Code/Retro/opForge/crates/opforge-lsp/src/lib.rs) and the VS Code integration in [opforge-vscode/package.json](/Users/erik/Code/Retro/opforge-vscode/package.json) and [client.ts](/Users/erik/Code/Retro/opforge-vscode/src/client/client.ts) where that integration affects opForge LSP functionality.

## Findings

### RVW-2026-04-10-001

- Severity: high
- File: [package.json](/Users/erik/Code/Retro/opforge-vscode/package.json#L54), [client.ts](/Users/erik/Code/Retro/opforge-vscode/src/client/client.ts#L40), [config.rs](/Users/erik/Code/Retro/opForge/crates/opforge-lsp/src/config.rs#L23), [session.rs](/Users/erik/Code/Retro/opForge/crates/opforge-lsp/src/session.rs#L658), [helloworld.asm](/Users/erik/Code/Retro/opForge/examples/motorola68000/amigaos/helloworld.asm#L1)
- Issue: the shipped `opforge-vscode` client does not expose or forward the server configuration surface that `opforge-lsp` already depends on. The Rust server reads settings only from the `opforgeLsp` namespace, including `roots`, `includePaths`, `modulePaths`, `defines`, `defaultCpu`, and validation settings, but the extension contributes only `opforge.lspPath` and starts the `LanguageClient` without `initializationOptions` or configuration synchronization.
- Why it matters: the recent no-frills AmigaOS Hunk path in [helloworld.asm](/Users/erik/Code/Retro/opForge/examples/motorola68000/amigaos/helloworld.asm#L1) deliberately omits in-source `.cpu` and relies on `--cpu 68000 --hunk ...` from the CLI. In the shipped editor integration there is no way to provide `defaultCpu=68000` or related workspace settings to the server, so completion, hover, include or module-path resolution, and validation context can fall back to the wrong defaults even though the assembler feature is shipped.
- Fix direction: align `opforge-vscode` with the real `opforge-lsp` contract by contributing the `opforgeLsp.*` settings namespace and forwarding it through `initializationOptions` plus `workspace/didChangeConfiguration`.

### RVW-2026-04-10-002

- Severity: high
- File: [helloworld.asm](/Users/erik/Code/Retro/opForge/examples/motorola68000/amigaos/helloworld.asm#L1), [validation_runner.rs](/Users/erik/Code/Retro/opForge/crates/opforge-lsp/src/validation_runner.rs#L54), [session.rs](/Users/erik/Code/Retro/opForge/crates/opforge-lsp/src/session.rs#L658), [lib.rs](/Users/erik/Code/Retro/opForge/crates/opforge-engine/src/lib.rs#L3508), [tests.rs](/Users/erik/Code/Retro/opForge/crates/opforge-asm/src/tests.rs#L14919)
- Issue: the LSP has no way to validate or contextualize the CLI-only Hunk workflow that the recent AmigaOS example and docs now advertise. The engine’s real flat-source Hunk path depends on `cpu_override: Some("68000")` and `hunk_name_override: Some(...)`, but the LSP validation runner only replays `defines`, include or module paths, and an optional CPU override before calling `builder.check()`. There is no output-mode or Hunk-selection input in the LSP contract, so the editor cannot execute the same context as `--cpu 68000 --hunk ...` for files with no in-source `.output`.
- Why it matters: the newly shipped allowed-notation behavior is not just generic 68000 parsing; parts of the recent acceptance and rejection matrix are Hunk-specific. Without an LSP-side Hunk mode, diagnostics for CLI-only files cannot enforce the same relocation or notation boundary that the assembler enforces in the real executable Hunk path, and completion or hover cannot reliably reflect the same feature mode.
- Fix direction: add explicit output-mode context to the LSP contract and drive validation through the same diagnostics-safe Hunk path as the CLI workflow, including support for the implicit single-code-hunk case.

### RVW-2026-04-10-003

- Severity: medium
- File: [lsp.test.ts](/Users/erik/Code/Retro/opforge-vscode/tests/integration/lsp.test.ts#L38), [extension.test.ts](/Users/erik/Code/Retro/opforge-vscode/tests/unit/extension.test.ts#L1), [lsp_client_integration.rs](/Users/erik/Code/Retro/opForge/crates/opforge-lsp/tests/lsp_client_integration.rs#L65), [tests.rs](/Users/erik/Code/Retro/opForge/crates/opforge-asm/src/tests.rs#L14919)
- Issue: the extension-side test suite is still placeholder-level and does not exercise any shipped opForge assembly behavior, while the Rust LSP integration suite has no focused coverage for the recent AmigaOS Hunk examples or the newer executable-notation matrix. The VS Code integration tests use JavaScript-like `let x =` content, assert formatting behavior the reviewed server does not advertise, and the unit test still checks a placeholder extension id.
- Why it matters: the recent Hunk and notation work now has substantial assembler-side coverage, but the editor path has almost no regression net for the exact scenarios the user-facing examples rely on. That makes LSP completeness drift likely and hard to detect.
- Fix direction: replace the placeholder extension tests with opForge-specific editor tests that open the AmigaOS Hunk examples and assert CPU context, hover or completion, diagnostics behavior, and settings propagation for the real `opforge-lsp` contract.

## Testing Gaps

- I found no LSP integration coverage for [helloworld.asm](/Users/erik/Code/Retro/opForge/examples/motorola68000/amigaos/helloworld.asm#L1), [writefile.asm](/Users/erik/Code/Retro/opForge/examples/motorola68000/amigaos/writefile.asm#L1), the implicit single-code Hunk path, or the recent 68000 bare-symbol executable forms that are covered in the assembler tests near [tests.rs](/Users/erik/Code/Retro/opForge/crates/opforge-asm/src/tests.rs#L14919).
- There is no extension-side test proving that workspace settings are forwarded into `initialize` or `workspace/didChangeConfiguration`, even though the server behavior depends on that path in [config.rs](/Users/erik/Code/Retro/opForge/crates/opforge-lsp/src/config.rs#L31) and [session.rs](/Users/erik/Code/Retro/opForge/crates/opforge-lsp/src/session.rs#L185).
- The current `opforge-vscode` tests do not validate any real opForge syntax, Hunk output selection, or AmigaOS example behavior.

## Residual Risks

- I did not run the `opforge-vscode` test suite, so runtime packaging or activation issues in that extension were not independently validated.
- I ran two focused Rust LSP tests, `cargo test -p lsp completion_uses_nearest_prior_cpu_context -- --nocapture` and `cargo test -p lsp initialize_reports_core_capabilities -- --nocapture`, and both passed, but those tests do not cover the recent Hunk-specific scenarios.
- I did not inspect every 68000-family hover or completion edge case beyond the configuration and validation paths reviewed here, so additional editor-only gaps may still exist outside the recent Hunk slice.

## Brief Summary

The assembler side of the recent AmigaOS Hunk and allowed-notation work is materially implemented and tested, but the shipped editor story is still incomplete. The separate VS Code client does not forward the server configuration surface needed to recover correct workspace and CPU context, the Rust LSP still lacks a first-class way to model the CLI-only Hunk workflow used by the new `helloworld.asm` path, and the extension-side tests would not catch those gaps.