//! Module capacity counts modules, independent of intervening symbol identities.
use super::*;
use std::fmt::Write as _;

fn sources() -> Vec<(String, String)> {
    let mut entry = String::from(".module main\n.cpu m6502\n");
    // Force discovered module names beyond the former 512-symbol graph bound.
    // These are actual constants, with no generated output or test-only runtime.
    for index in 0..600 {
        writeln!(entry, "padding{index} = {index}").unwrap();
    }
    entry.push_str(".use alpha\n.use beta\n.byte 4\n.endmodule\n.end\n");
    vec![
        ("main.asm".into(), entry),
        ("library/beta.asm".into(), B.into()),
        ("library/shared.asm".into(), SHARED.into()),
        ("library/alpha.asm".into(), A.into()),
    ]
}

#[test]
fn sparse_module_identity_rust_oracle() {
    let sources = sources();
    let files = sources
        .iter()
        .map(|(name, source)| (name.as_str(), source.as_str()))
        .collect::<Vec<_>>();
    assert_eq!(
        oracle_with_roots(&files, &["library"]).unwrap(),
        [1, 2, 3, 4]
    );
}

#[test]
#[ignore = "requires configured FS-UAE; sparse symbol IDs and repeated graph discovery"]
fn compact_cli_sparse_module_identity_fs_uae() {
    let sources = sources();
    let files = sources
        .iter()
        .map(|(name, source)| (name.as_str(), source.as_str()))
        .collect::<Vec<_>>();
    let expected = oracle_with_roots(&files, &["library"]).unwrap();
    assert_eq!(expected, [1, 2, 3, 4]);
    compact_cli(&files, &["library"], &[], Some(&expected), false);
}

fn module_source(count: usize) -> String {
    let mut source = String::new();
    for index in 0..count {
        writeln!(
            source,
            ".module unit{index}\n.cpu m6502\n.byte {}\n.endmodule",
            index % 256
        )
        .unwrap();
    }
    source.push_str(".end\n");
    source
}

#[test]
fn module_capacity_rust_oracle() {
    for count in [512, 513] {
        let source = module_source(count);
        assert_eq!(
            oracle(&[("main.asm", &source)]).unwrap(),
            (0..count)
                .map(|index| (index % 256) as u8)
                .collect::<Vec<_>>()
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; module-count capacity independent of symbol IDs"]
fn compact_cli_module_capacity_fs_uae() {
    let source = module_source(512);
    let files = &[("main.asm", source.as_str())];
    let expected = oracle(files).unwrap();
    compact_cli(files, &[], &[], Some(&expected), false);

    // Rust accepts this graph; native's deliberate module capacity stays bounded.
    let source = module_source(513);
    let files = &[("main.asm", source.as_str())];
    assert_eq!(oracle(files).unwrap().len(), 513);
    compact_cli(files, &[], &[], None, false);
}
