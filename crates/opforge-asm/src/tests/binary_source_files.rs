//! F7: real files and explicit caller order share one numeric assembly session.
use super::*;

const COPY: &[(&str, &str)] = &[
    (
        "library.asm",
        include_str!("../../fixtures/binary-source/files/m6502/library.asm"),
    ),
    (
        "consumer.asm",
        include_str!("../../fixtures/binary-source/files/m6502/consumer.asm"),
    ),
    (
        "main.asm",
        include_str!("../../fixtures/binary-source/files/m6502/main.asm"),
    ),
];
const CONTROL: &[(&str, &str)] = &[
    (
        "library.asm",
        include_str!("../../fixtures/binary-source/files/m68000/library.asm"),
    ),
    (
        "consumer.asm",
        include_str!("../../fixtures/binary-source/files/m68000/consumer.asm"),
    ),
    (
        "main.asm",
        include_str!("../../fixtures/binary-source/files/m68000/main.asm"),
    ),
];
const COPY_BYTES: &[u8] = &[
    0xa2, 8, 0x46, 0x20, 0x26, 0x21, 0xca, 0xd0, 0xf9, 0x60, 0x20, 0, 0x10, 0xa9, 15, 0x60, 0,
    0x10, 0x0a, 0x10, 0xa9, 15, 0x60,
];
const CONTROL_BYTES: &[u8] = &[
    2, 0x80, 0, 0, 0, 0xff, 0, 0x80, 0, 0, 0x3a, 0, 0x4e, 0x75, 0x70, 7, 0x4e, 0x75, 0x10, 0, 0x10,
    0x0e, 0x72, 7, 0x4e, 0x75,
];

fn joined(files: &[(&str, &str)]) -> String {
    // Each physical file's terminator ends that input, not the whole manifest.
    let mut source = String::new();
    for (_, text) in files {
        for line in text
            .lines()
            .filter(|line| !line.trim().eq_ignore_ascii_case(".end"))
        {
            source.push_str(line);
            source.push('\n');
        }
    }
    source.push_str(".end\n");
    source
}

fn source_bytes(source: &str) -> Vec<u8> {
    let (entries, diagnostics) =
        assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
            .expect("joined live Rust oracle");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

fn real_file_bytes(cpu: &str) -> Vec<u8> {
    let root = workspace_root()
        .join("crates/opforge-asm/fixtures/binary-source/files")
        .join(cpu)
        .join("main.asm");
    let (entries, diagnostics) = assemble_example_entries_with_runtime_mode(&root, true)
        .expect("real-file module graph oracle");
    assert!(diagnostics.is_empty(), "{cpu}: {diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn binary_files_real_file_oracles() {
    for (cpu, expected) in [("m6502", COPY_BYTES), ("m68000", CONTROL_BYTES)] {
        assert_eq!(real_file_bytes(cpu), expected, "{cpu}");
    }
}

#[test]
fn binary_files_joined_oracles() {
    assert_eq!(source_bytes(&joined(COPY)), COPY_BYTES);
    assert_eq!(source_bytes(&joined(CONTROL)), CONTROL_BYTES);
}

// Explicit multi-file native sessions require closed named modules per file.
// These implicit global inputs remain valid when joined for ordinary Rust assembly.
const GLOBAL_FILES: &[(&str, &str)] = &[
    ("first.asm", ".cpu m6502\n.org $1000\n.byte 1\n.end\n"),
    ("second.asm", ".byte 2\n.end\n"),
];

const FORWARD_ALIASES: &[(&str, &str)] = &[
    ("clients.asm", ".module first\n.cpu m6502\n.org $1000\nfirst.pick = 1\nfirst.left = 2\nfirst.as = 4\n.byte pick.value\n.use left as pick\n.endmodule\n.module second\n.use right as PICK\n.byte pick.value,LEFT.value\n.endmodule\n.end\n"),
    ("definitions.asm", ".module left\n.pub\nvalue = 3\n.endmodule\n.module right\n.pub\nvalue = 7\n.endmodule\n.end\n"),
];

#[test]
fn binary_files_forward_alias_oracle() {
    assert_eq!(source_bytes(&joined(FORWARD_ALIASES)), [3, 7, 3]);
}

#[test]
#[ignore = "requires configured FS-UAE; aliases owned by distinct modules"]
fn binary_files_aliases_fs_uae() {
    let oracle = source_bytes(&joined(FORWARD_ALIASES));
    assert_eq!(oracle, [3, 7, 3]);
    assert_binary_files(FORWARD_ALIASES, "m6502", oracle);
}

const OWNER: &str =
    ".module owner\n.cpu m6502\n.org $1000\nsecret = 7\n.pub\nvalue = 3\n.endmodule\n.end\n";
const REJECTIONS: &[(&str, &str)] = &[
    ("ambiguous", ".module client\n.use owner as first\n.use owner as second\n.byte owner.value\n.endmodule\n.end\n"),
    ("private", ".module client\n.use owner as dep\n.byte dep.secret\n.endmodule\n.end\n"),
    ("duplicatealias", ".module client\n.use owner as dep\n.use owner as DEP\n.endmodule\n.end\n"),
    ("missingmodule", ".module client\n.use absent\n.endmodule\n.end\n"),
    ("scopeimport", ".module client\nchild .block\n.use owner\n.bend\n.endmodule\n.end\n"),
    ("outsideimport", ".use owner\n.end\n"),
    ("missingmembernofallback", ".module shadow\n.pub\nmissing = 9\n.endmodule\n.module client\n.use owner as shadow\n.byte shadow.missing\n.endmodule\n.end\n"),
    ("duplicate_module", ".module OWNER\n.endmodule\n.end\n"),
    ("openEOF", ".module client\n.byte 1\n"),
    ("preparation_location", ".module client\n.cpu m6502\n unknown_instruction\n.endmodule\n.end\n"),
    ("assembly_location", ".module client\n.cpu m6502\n.long $+2147483647\n.endmodule\n.end\n"),
];

#[test]
fn binary_files_rejection_oracles() {
    assert_eq!(source_bytes(&joined(GLOBAL_FILES)), [1, 2]);
    for (name, source) in REJECTIONS {
        let text = joined(&[("owner.asm", OWNER), ("bad.asm", source)]);
        if *name == "assembly_location" {
            // Rust supports this wider result; native expressions deliberately
            // reject values outside signed32. PC keeps evaluation in assembly.
            assert_eq!(source_bytes(&text), [0xff, 0x0f, 0, 0x80]);
            continue;
        }
        let result =
            assemble_source_entries_with_runtime_mode(&text.lines().collect::<Vec<_>>(), true);
        assert!(
            !matches!(result, Ok((_, ref diagnostics)) if diagnostics.is_empty()),
            "{name}"
        );
    }
}

fn compare(files: &[(&str, &str)], cpu: &str, expected: &[u8]) {
    let oracle = real_file_bytes(cpu);
    assert_eq!(oracle, expected);
    assert_eq!(source_bytes(&joined(files)), oracle);
    if std::env::var_os("OPFORGE_FILES_JOINED").is_some() {
        let source = joined(files);
        assert_binary_files(&[("joined.asm", &source)], cpu, oracle);
    } else {
        assert_binary_files(files, cpu, oracle);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; explicit file copy/reversal"]
fn binary_files_copy_fs_uae() {
    compare(COPY, "m6502", COPY_BYTES);
}

#[test]
#[ignore = "requires configured FS-UAE; explicit file control words"]
fn binary_files_control_fs_uae() {
    compare(CONTROL, "m68000", CONTROL_BYTES);
}

#[test]
#[ignore = "requires OPFORGE_FILE_REJECTION and configured FS-UAE"]
fn binary_files_rejection_fs_uae() {
    let key = std::env::var("OPFORGE_FILE_REJECTION").unwrap();
    if key == "global_inputs" {
        assert_eq!(source_bytes(&joined(GLOBAL_FILES)), [1, 2]);
        assert_native_files_rejection(GLOBAL_FILES, "m6502", None);
        return;
    }
    let (_, source) = REJECTIONS.iter().find(|(name, _)| *name == key).unwrap();
    if key == "assembly_location" {
        let text = joined(&[("owner.asm", OWNER), ("bad.asm", source)]);
        assert_eq!(source_bytes(&text), [0xff, 0x0f, 0, 0x80]);
    }
    let diagnostic = matches!(key.as_str(), "preparation_location" | "assembly_location")
        .then_some("[file 00000002, line 00000003]");
    assert_native_files_rejection(
        &[("owner.asm", OWNER), ("bad.asm", source)],
        "m6502",
        diagnostic,
    );
}
