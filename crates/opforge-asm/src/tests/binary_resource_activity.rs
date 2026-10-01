//! Activity and filename transport use shared assembler condition/loop semantics.
use super::*;
fn run(source: &str, files: &[(&str, &[u8])]) -> Result<Vec<u8>, String> {
    let root = create_temp_dir("binary-resource-activity");
    let result = (|| {
        fs::write(root.join("main.asm"), source).unwrap();
        for (name, bytes) in files {
            let path = root.join(name);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, bytes).unwrap();
        }
        let cli = Cli::parse_from([
            "opForge".to_string(),
            root.join("main.asm").to_string_lossy().into_owned(),
            "--cpu".into(),
            "m6502".into(),
            "--bin".into(),
            root.join("output.bin").to_string_lossy().into_owned(),
            "--dependencies".into(),
            root.join("output.d").to_string_lossy().into_owned(),
        ]);
        let config = validate_cli(&cli).unwrap();
        run_with_validated_cli_with_context(&cli, &config).map_err(|error| match error {
            CliRunError::Assembler { error, .. } => error
                .diagnostics()
                .iter()
                .map(|diag| diag.error.message().to_string())
                .collect::<Vec<_>>()
                .join("\n"),
            CliRunError::Workflow { error, .. } => error.to_string(),
            CliRunError::WarningsAsErrors { .. } => "warnings treated as errors".into(),
        })?;
        let dependencies = fs::read_to_string(root.join("output.d")).unwrap();
        for (name, _) in files.iter().filter(|(name, _)| name.ends_with(".bin")) {
            assert!(
                dependencies.contains(&name.replace(' ', "\\ ")),
                "{dependencies}"
            );
        }
        assert!(!dependencies.contains("missing-loop.bin"));
        assert!(!dependencies.contains("missing-macro.bin"));
        fs::read(root.join("output.bin")).map_err(|error| error.to_string())
    })();
    fs::remove_dir_all(root).unwrap();
    result
}
#[test]
fn binary_resource_active_loops_and_definition_relative_macro() {
    let source=".org 0\n.include \"defs/macros.i\"\n.for 0\n.incbin \"missing-loop.bin\"\n.endfor\n.for 2\n.Emit 'data.bin'\n.endfor\n.end\n";
    let body=b"Emit .macro filename\n.incbin .filename\n.endmacro\nUnused .macro\n.incbin \"missing-macro.bin\"\n.endmacro\n";
    assert_eq!(
        run(
            source,
            &[("defs/macros.i", body), ("defs/data.bin", &[0, 128, 255])]
        )
        .unwrap(),
        [0, 128, 255, 0, 128, 255]
    );
}
#[test]
fn binary_resource_preserves_filename_quote_spellings_and_bare_paths() {
    let source =
        ".org 0\nA .incbin 'a space.bin'\nB: .incbin b.bin\n.incbin \"c.bin\"\n.byte B-A\n.end\n";
    assert_eq!(
        run(
            source,
            &[("a space.bin", &[17]), ("b.bin", &[34]), ("c.bin", &[51])]
        )
        .unwrap(),
        [17, 34, 51, 1]
    );
}
#[test]
fn binary_resource_missing_active_asset_reports_explicit_failure() {
    let error = run(".org 0\n.incbin \"missing.bin\"\n", &[]).unwrap_err();
    assert!(error.contains("INCBIN file not found"), "{error}");
}

#[test]
fn binary_resource_survives_reachable_module_relayout() {
    let source = ".module dep\n.pub\n.section data, kind=data, logical\npayload .incbin \"data.bin\"\n.endsection\n.endmodule\n.module main\n.use dep as d\n.section data, kind=data, align=1\n.long d.payload\n.endsection\n.region rom, 0, $ffff\n.place data in rom\n.output \"output.bin\", format=bin, sections=data\n.endmodule\n";
    let bytes = run(source, &[("data.bin", &[17, 34, 51])]).unwrap();
    assert_eq!(bytes, [17, 34, 51, 0, 0, 0, 0]);
}
