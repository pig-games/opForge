// SPDX-License-Identifier: GPL-3.0-or-later

use std::collections::HashMap;
use types::symbol::{SymbolTable, SymbolTableResult, SymbolVisibility};
use vm::output_artifacts::build_mapfile_text;
use vm::output_model::{MapFileDirective, MapSymbolsMode};

const HEADER: &str =
    "Regions\nname start end used free align\n\nSections\nname base size kind region\n";
const SYMBOLS: &str = "\nSymbols\nname value visibility\n";

fn render(symbols: &SymbolTable, mode: MapSymbolsMode) -> String {
    build_mapfile_text(
        &MapFileDirective {
            path: "unused.map".into(),
            symbols: mode,
        },
        &HashMap::new(),
        &HashMap::new(),
        symbols,
    )
}

#[test]
fn map_symbols_preserve_canonical_names_order_values_and_visibility() {
    let mut symbols = SymbolTable::new();
    for (name, value, visibility) in [
        ("main.Zebra", 0x12345678, SymbolVisibility::Private),
        (
            "main.a_very_long_canonical_name",
            0x123456,
            SymbolVisibility::Public,
        ),
        ("main._prefix", 0, SymbolVisibility::Private),
        ("main.Alpha", 0xffff, SymbolVisibility::Public),
        ("Main.a.child", 0x10000, SymbolVisibility::Private),
    ] {
        assert_eq!(
            symbols.add(name, value, false, visibility, Some("main")),
            SymbolTableResult::Ok
        );
    }
    assert_eq!(render(&symbols, MapSymbolsMode::All), format!(
        "{HEADER}{SYMBOLS}main._prefix 0000 private\nMain.a.child 010000 private\nmain.a_very_long_canonical_name 123456 public\nmain.Alpha FFFF public\nmain.Zebra 12345678 private\n"
    ));
    assert_eq!(
        render(&symbols, MapSymbolsMode::Public),
        format!(
        "{HEADER}{SYMBOLS}main.a_very_long_canonical_name 123456 public\nmain.Alpha FFFF public\n"
    )
    );
    assert_eq!(render(&symbols, MapSymbolsMode::None), HEADER);
}

#[test]
fn empty_map_symbol_modes_preserve_headers() {
    let symbols = SymbolTable::new();
    for mode in [MapSymbolsMode::All, MapSymbolsMode::Public] {
        assert_eq!(render(&symbols, mode), format!("{HEADER}{SYMBOLS}"));
    }
    assert_eq!(render(&symbols, MapSymbolsMode::None), HEADER);
}
