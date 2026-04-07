# opforge-lsp VS Code Reference Client

This folder contains a reference VS Code client for the `opforge-lsp` server.

## Development

1. Build the server:
   `cargo build --bin opforge-lsp`
2. Install extension dependencies:
   `npm install`
3. Compile extension:
   `npm run compile`
4. Launch an Extension Development Host from VS Code.

## Settings

The extension forwards these settings to the language server:

- `opforgeLsp.roots`
- `opforgeLsp.includePaths`
- `opforgeLsp.modulePaths`
- `opforgeLsp.defines`
- `opforgeLsp.defaultCpu`
- `opforgeLsp.validation.debounceMs`
- `opforgeLsp.validation.onSave`

The extension itself can be pointed at a specific language-server binary with:

- `opforgeLsp.serverPath`

`opforgeLsp.serverPath` is client-side only. It supports absolute paths, `${workspaceFolder}` expansion, `~/` home expansion, and workspace-relative paths. Updating it restarts the language client so the new server binary is picked up immediately.

`opforgeLsp.opforgePath` is deprecated and ignored. Validation now runs in-process inside `opforge-lsp` rather than shelling out to the CLI.
