//! Block transitions and target overlays share the dotted metadata writer.
use super::*;

pub(super) fn cases() -> Vec<MetadataCase> {
    let mut cases = vec![
        case("metadata-block-description", ".meta\n.name \"Description\"\n.version \"1.2\"\n.endmeta", "", &[], false),
        case("metadata-block-bin", ".meta\n.output\n.name \"@ROOT@base\"\n.bin\n.endoutput\n.endmeta", "", &["base.bin"], false),
        case("metadata-block-inline-equivalence", ".meta\n.output\n.name \"@ROOT@base\"\n.hex \"records\"\n.bin \"@BIN_ROOT@payload\"\n.endoutput\n.endmeta", "", &["records.hex", "payload.bin"], false),
        case("metadata-target-inline-name", ".meta.output.68020.name \"@ROOT@selected\"\n.meta.output.bin", "", &["selected.bin"], false),
        case("metadata-target-canonical-name", ".meta\n.output\n.name \"@ROOT@default\"\n.bin\n.m68020\n.name \"@ROOT@selected\"\n.endm68020\n.endoutput\n.endmeta", "", &["selected.bin"], false),
        case("metadata-target-alias-name", ".meta\n.output\n.name \"@ROOT@default\"\n.bin\n.mc68020\n.name \"@ROOT@selected\"\n.endmc68020\n.endoutput\n.endmeta", "", &["selected.bin"], false),
        case("metadata-target-numeric-close", ".meta\n.output\n.bin\n.m68020\n.name \"@ROOT@selected\"\n.end68020\n.endoutput\n.endmeta", "", &["selected.bin"], false),
        case("metadata-target-mismatch-default", ".meta\n.output\n.name \"@ROOT@default\"\n.bin\n.m6502\n.name \"@ROOT@other\"\n.bin \"@BIN_ROOT@other\"\n.endm6502\n.endoutput\n.endmeta", "", &["default.bin"], false),
        case("metadata-target-bin-replaces-default", ".meta\n.output\n.bin \"@BIN_ROOT@default\"\n.m68020\n.bin \"@BIN_ROOT@selected\"\n.endm68020\n.endoutput\n.endmeta", "", &["selected.bin"], false),
        case("metadata-target-fill-inherits", ".meta\n.output\n.fill \"aa\"\n.m68020\n.bin \"@BIN_ROOT@range.bin:0ffe:1003\"\n.endm68020\n.endoutput\n.endmeta", "", &["range.bin"], false),
        case("metadata-target-fill-overrides", ".meta\n.output\n.bin \"@BIN_ROOT@range.bin:0ffe:1003\"\n.fill \"aa\"\n.m68020\n.fill \"bb\"\n.endm68020\n.endoutput\n.endmeta", "", &["range.bin"], false),
        case("metadata-target-default-hex-inherits", ".meta\n.output\n.name \"@ROOT@default\"\n.hex \"records\"\n.m68020\n.name \"@ROOT@nested/selected\"\n.bin\n.endm68020\n.endoutput\n.endmeta", "", &["nested/records.hex", "nested/selected.bin"], false),
        case("metadata-block-inactive-controls", ".if 0\n.meta\n.output\n.m68020\n.name \"@ROOT@inactive\"\n.bin\n.endm68020\n.endoutput\n.endmeta\n.else\n.meta\n.output\n.name \"@ROOT@active\"\n.bin\n.endoutput\n.endmeta\n.endif", "", &["active.bin"], false),
        case("metadata-block-inactive-close", ".meta\n.output\n.if 0\n.endoutput\n.endmeta\n.endif\n.name \"@ROOT@active\"\n.bin\n.endoutput\n.endmeta", "", &["active.bin"], false),
    ];
    for (name, output) in [
        ("metadata-block-bin", "base.bin"),
        ("metadata-block-inline-equivalence", "payload.bin"),
        ("metadata-target-inline-name", "selected.bin"),
        ("metadata-target-canonical-name", "selected.bin"),
        ("metadata-target-alias-name", "selected.bin"),
        ("metadata-target-numeric-close", "selected.bin"),
        ("metadata-target-mismatch-default", "default.bin"),
        ("metadata-target-bin-replaces-default", "selected.bin"),
        (
            "metadata-target-default-hex-inherits",
            "nested/selected.bin",
        ),
        ("metadata-block-inactive-controls", "active.bin"),
        ("metadata-block-inactive-close", "active.bin"),
    ] {
        // Store independent output bytes; paths and contents are both checked.
        let item = cases.iter_mut().find(|item| item.name == name).unwrap();
        item.binary_outputs = match output {
            "base.bin" => &[("base.bin", &[0x12, 0x34])],
            "payload.bin" => &[("payload.bin", &[0x12, 0x34])],
            "selected.bin" => &[("selected.bin", &[0x12, 0x34])],
            "default.bin" => &[("default.bin", &[0x12, 0x34])],
            "nested/selected.bin" => &[("nested/selected.bin", &[0x12, 0x34])],
            "active.bin" => &[("active.bin", &[0x12, 0x34])],
            _ => unreachable!(),
        };
    }
    set_binary_outputs(
        &mut cases,
        "metadata-target-fill-inherits",
        &[("range.bin", &[0xaa, 0xaa, 0x12, 0x34, 0xaa, 0xaa])],
    );
    set_binary_outputs(
        &mut cases,
        "metadata-target-fill-overrides",
        &[("range.bin", &[0xbb, 0xbb, 0x12, 0x34, 0xbb, 0xbb])],
    );
    cases
}

pub(super) fn negative_cases() -> Vec<NegativeMetadataCase> {
    [
        ("metadata-block-unclosed-meta", ".meta"),
        ("metadata-block-unclosed-output", ".meta\n.output"),
        ("metadata-block-unclosed-target", ".meta\n.output\n.m68020"),
        ("metadata-block-close-without-open", ".endmeta"),
        ("metadata-block-output-outside-meta", ".output"),
        (
            "metadata-block-nested-meta",
            ".meta\n.meta\n.endmeta\n.endmeta",
        ),
        (
            "metadata-block-target-close-mismatch",
            ".meta\n.output\n.m6502\n.endm68020\n.endoutput\n.endmeta",
        ),
        (
            "metadata-block-target-nested",
            ".meta\n.output\n.m68020\n.m6502\n.endm6502\n.endm68020\n.endoutput\n.endmeta",
        ),
        (
            "metadata-block-ordinary-statement",
            ".meta\n.output\n.byte 7\n.endoutput\n.endmeta",
        ),
        (
            "metadata-block-ordinary-assignment",
            ".meta\nvalue = 7\n.endmeta",
        ),
        (
            "metadata-block-nested-namespace",
            ".namespace inner\n.meta\n.endmeta\n.endnamespace",
        ),
    ]
    .into_iter()
    .map(|(name, metadata)| rejection(name, source(metadata, "Work:", false), None))
    .collect()
}

#[test]
fn compact_cli_metadata_block_oracles() {
    let root = scratch();
    let _cleanup = EphemeralArtifactDir(root.clone());
    for case in cases() {
        rust_oracle(&root, &case);
    }
}

#[test]
fn compact_cli_metadata_block_rejection_oracles() {
    let root = scratch();
    let _cleanup = EphemeralArtifactDir(root.clone());
    for case in negative_cases() {
        let input = root.join("entry.asm");
        fs::write(&input, case.source).unwrap();
        let cli = Cli::parse_from([
            "opForge".to_string(),
            input.display().to_string(),
            "--cpu".into(),
            "68020".into(),
            "--bin".into(),
            root.join("output.bin").display().to_string(),
        ]);
        let config = validate_cli(&cli).unwrap();
        assert!(
            run_with_validated_cli_with_context(&cli, &config).is_err(),
            "{} must reject in Rust",
            case.name
        );
    }
}
