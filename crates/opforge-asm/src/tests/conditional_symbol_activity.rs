//! Forward guard convergence replaces symbol availability with executed owners.
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

struct SourceFiles(PathBuf);
impl Drop for SourceFiles {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

#[test]
fn conditional_symbol_activity_imported_forward_layout_guards() {
    for cpu in ["6502", "68020"] {
        for (guard, declaration, query, expected) in [
            ("enabled", "", "", Some(vec![11, 22, 0, 7])),
            ("!enabled", "stale:\n", ",provider.routine.stale", None),
            ("!enabled", "stale:\n", "", Some(vec![11, 0, 7])),
        ] {
            let dir = SourceFiles(std::env::temp_dir().join(format!(
                "opforge-conditional-activity-{}-{}",
                process::id(),
                unix_nanos()
            )));
            fs::create_dir_all(&dir.0).unwrap();
            let entry = dir.0.join("entry.asm");
            let output = dir.0.join("out.bin");
            fs::write(
                &entry,
                format!(".module app\n.cpu {cpu}\n.use provider\n.byte provider.routine.saved1.value.value{query}\n.endmodule\n"),
            ).unwrap();
            fs::write(
                dir.0.join("provider.asm"),
                format!(
                    ".module provider\n.cpu {cpu}\n.pub\nroutine .block\n\
                     Info .struct\nvalue .byte ?\n.endstruct\n\
                     items .bfor 1\nvalue .const Info {{value:7}}\n.endfor\nsaved1 .const items[0]\n\
                     .byte 11\n.if {guard}\n{declaration}.byte 22\n.endif\n\
                     .for 0\nprovider.routine.saved1.value .const missing\n.endfor\n\
                     enabled .const start>0\nstart:\n.byte 0\n.bend\n.endmodule\n"
                ),
            ).unwrap();
            let cli = Cli::parse_from([
                "opforge".to_string(),
                entry.to_string_lossy().into_owned(),
                "--bin".to_string(),
                output.to_string_lossy().into_owned(),
            ]);
            let config = validate_cli(&cli).unwrap();
            let result = run_with_validated_cli_with_context(&cli, &config);
            if let Some(expected) = expected {
                assert!(result.is_ok(), "{cpu}/{guard}: {result:?}");
                assert_eq!(fs::read(&output).unwrap(), expected, "{cpu}/{guard}");
            } else {
                assert!(result.is_err(), "{cpu}: inactive label remained available");
                assert!(!output.exists(), "{cpu}: rejected source published output");
            }
        }
    }
}

#[test]
fn conditional_symbol_activity_root_removes_inactive_label_keeps_live_forward_labels() {
    for cpu in ["6502", "68020"] {
        let source = format!(".cpu {cpu}\n.byte 11\n.if !enabled\nstale\n.byte 22\n.endif\nenabled .const start>0\nstart\n.byte 0\n");
        let assembler = run_passes(&source.lines().collect::<Vec<_>>());
        assert!(assembler.symbols.entry("stale").is_none(), "{cpu}");
        assert_eq!(assembler.symbols.lookup("start"), Some(1), "{cpu}");
        assert_eq!(assembler.image().entries().unwrap(), vec![(0, 11), (1, 0)]);

        let source = format!(".cpu {cpu}\n.byte finish-start\n.if enabled\n.byte 22\n.endif\nenabled .const start>0\nstart\n.byte 0\nfinish\n");
        let assembler = run_passes(&source.lines().collect::<Vec<_>>());
        assert_eq!(
            assembler.image().entries().unwrap(),
            vec![(0, 1), (1, 22), (2, 0)]
        );
    }
}

#[test]
fn conditional_symbol_activity_preserves_incoming_readonly_configuration() {
    let lines = ".cpu 6502\n.byte CONFIG\n.if enabled\n.byte 22\n.endif\nenabled .const start>0\nstart:\n.byte 0\n"
        .lines().map(str::to_string).collect::<Vec<_>>();
    let mut assembler = Assembler::new();
    assert_eq!(
        assembler
            .symbols
            .add("CONFIG", 9, false, SymbolVisibility::Private, None),
        SymbolTableResult::Ok
    );
    assert_eq!(
        assembler.pass1(&lines).errors,
        0,
        "{:?}",
        assembler.diagnostics
    );
    let mut listing_out = Vec::new();
    let mut listing = ListingWriter::new(&mut listing_out, false);
    assert_eq!(assembler.pass2(&lines, &mut listing).unwrap().errors, 0);
    assert_eq!(assembler.symbols.lookup("CONFIG"), Some(9));
    assert_eq!(
        assembler.image().entries().unwrap(),
        vec![(0, 9), (1, 22), (2, 0)]
    );
}
