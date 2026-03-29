# Root Folder Example

This folder demonstrates the intended root-folder layout:

- main.asm is the single root module.
- util.asm is a dependency loaded via .use.

When folder inputs are supported, the assembler should treat this folder as the project root and assemble main.asm as the entry point.
