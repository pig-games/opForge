//! Public address labels must resolve through the same canonical import path as values.
use super::*;

const CONSUMER: &str = ".module consumer\n.cpu m68020\n.use example.state\nRead .block\n move.w state.Flag,d0\n move.w state.Tail,d1\n rts\n.bend\n.word Read\n.endmodule\n";
const OWNER: &str = ".module example.state\n.cpu m68020\n.pub\nFlag\n.word 0\nTail\n.word 0\n.endmodule\n";
const CONSTANT_OWNER: &str = ".module example.state\n.cpu m68020\n.pub\nFlag = 7\nTail = 9\n.endmodule\n";

fn cases(owner: &str) -> Vec<Vec<(&'static str, String)>> {
    vec![
        vec![("main.asm", format!("{CONSUMER}{owner}"))],
        vec![("main.asm", format!("{owner}{CONSUMER}"))],
        vec![
            ("main.asm", CONSUMER.into()),
            ("library/state.asm", owner.into()),
        ],
    ]
}

#[test]
fn compact_imported_public_address_labels_rust_oracles() {
    for owner in [OWNER, CONSTANT_OWNER] {
        let mut first = None;
        for sources in cases(owner) {
            let files: Vec<_> = sources
                .iter()
                .map(|(name, text)| (*name, text.as_str()))
                .collect();
            let roots: &[&str] = if files.len() == 1 { &[] } else { &["library"] };
            let bytes = oracle_with_roots(&files, roots).unwrap();
            assert!(!bytes.is_empty());
            if let Some(expected) = &first {
                assert_eq!(&bytes, expected, "physical source order must not change bindings");
            } else {
                first = Some(bytes);
            }
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; public bare labels through implicit module qualifier"]
fn compact_imported_public_address_labels_fs_uae() {
    for owner in [OWNER, CONSTANT_OWNER] {
        for sources in cases(owner) {
            let files: Vec<_> = sources
                .iter()
                .map(|(name, text)| (*name, text.as_str()))
                .collect();
            let roots: &[&str] = if files.len() == 1 { &[] } else { &["library"] };
            let expected = oracle_with_roots(&files, roots).unwrap();
            compact_cli_cpu(&files, roots, &[], Some(&expected), false, "m68020");
        }
    }
}
