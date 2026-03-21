// SPDX-License-Identifier: GPL-3.0-or-later

fn main() -> Result<(), Box<dyn std::error::Error>> {
    lsp::protocol::run_stdio_with_registry(lsp::build_default_asm_registry())?;
    Ok(())
}
