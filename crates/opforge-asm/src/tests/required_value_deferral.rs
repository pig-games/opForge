//! Required scalar encoding constraints wait for unresolved lexical bindings.
use super::*;

fn assemble_bytes(source: &str) -> Vec<u8> {
    run_passes(&source.lines().collect::<Vec<_>>())
        .image()
        .entries()
        .unwrap()
        .into_iter()
        .map(|(_, byte)| byte)
        .collect()
}

fn program(count: &str, body: &str) -> String {
    format!(".module main\n.cpu 68020\n{count}\nrun .block\n{body}\n.bend\n.long run\n.endmodule")
}

#[test]
fn required_value_deferral_enclosing_counts_match_qualified() {
    for instruction in ["lsl.w", "addq.w"] {
        let bare = program("COUNT=2", &format!(" {instruction} #COUNT,d6\n rts"));
        let qualified = program("COUNT=2", &format!(" {instruction} #main.COUNT,d6\n rts"));
        assert_eq!(
            assemble_bytes(&bare),
            assemble_bytes(&qualified),
            "{instruction}"
        );
    }
}

#[test]
fn required_value_deferral_preserves_forward_local_shadow() {
    for instruction in ["lsl.w", "addq.w"] {
        let shadow = program(
            "COUNT=7",
            &format!(" {instruction} #COUNT,d6\nCOUNT=2\n rts"),
        );
        let literal = program("", &format!(" {instruction} #2,d6\n rts"));
        assert_eq!(
            assemble_bytes(&shadow),
            assemble_bytes(&literal),
            "{instruction}"
        );
        let forward = program("", &format!(" {instruction} #COUNT,d6\nCOUNT=2\n rts"));
        assert_eq!(
            assemble_bytes(&forward),
            assemble_bytes(&literal),
            "{instruction}"
        );
    }
}

#[test]
fn required_value_deferral_rejects_final_invalid_counts_and_missing_names() {
    for instruction in ["lsl.w", "addq.w"] {
        for (definition, count) in [
            ("", "0"),
            ("", "9"),
            ("COUNT=0", "COUNT"),
            ("COUNT=9", "COUNT"),
            ("", "missing"),
        ] {
            let source = program(definition, &format!(" {instruction} #{count},d6\n rts"));
            let lines = source.lines().map(str::to_string).collect::<Vec<_>>();
            let mut assembler = Assembler::new();
            let pass1 = assembler.pass1(&lines);
            if count == "missing" {
                assert_eq!(pass1.errors, 0, "{:?}", assembler.diagnostics);
            }
            if pass1.errors == 0 {
                let mut output = Vec::new();
                let mut listing = ListingWriter::new(&mut output, false);
                let pass2 = assembler.pass2(&lines, &mut listing).unwrap();
                assert!(pass2.errors > 0, "{source}");
            } else {
                assert_ne!(count, "missing");
            }
            assert!(
                assembler
                    .diagnostics
                    .iter()
                    .any(|diagnostic| diagnostic.severity == Severity::Error),
                "{source}"
            );
        }
    }
}
