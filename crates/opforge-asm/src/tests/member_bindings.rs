//! Qualified declaration ownership precedes typed member interpretation.
use super::*;

fn bytes(source: &str) -> Vec<u8> {
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    assembler
        .image()
        .entries()
        .unwrap()
        .into_iter()
        .map(|(_, byte)| byte)
        .collect()
}

#[test]
fn member_bindings_longer_struct_owner_retains_only_its_block() {
    let source = ".module provider\n.pub\nroutine .block\nInfo .struct\nvalue .byte ?\n.endstruct\nsaved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\nprovider.routine.saved1.value .const provider.routine.Info {value:later}\nlater .const 66\n.byte 22\n.bend\n.endmodule\n.module app\n.use provider as lib\n.byte lib.routine.saved1.value.value\n.endmodule";
    assert_eq!(bytes(source), [22, 66]);
}

#[test]
fn member_bindings_declared_fullname_precedes_shorter_struct_member() {
    assert_eq!(bytes("Info .struct\nvalue .byte ?\n.endstruct\nsaved .const Info {value:7}\nsaved.value .const 66\n.byte saved.value"), [66]);
}

#[test]
fn member_bindings_scalar_declaration_keeps_physical_owner_and_fallthrough() {
    assert_eq!(bytes(".module provider\n.pub\nroutine .block\n.byte 11\n.bend\nother .block\nprovider.routine.answer .const 66\ninternal:\n.byte 22\nnext:\n.byte 23\n.bend\n.endmodule\n.module app\n.use provider\n.byte provider.routine.answer\n.endmodule"), [22,23,66]);
}

#[test]
fn member_bindings_inactive_longer_declaration_does_not_own_reference() {
    assert_eq!(bytes(".module provider\n.pub\nroutine .block\nInfo .struct\nvalue .byte ?\n.endstruct\nsaved .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if 0\nprovider.routine.saved.value .const 66\n.endif\n.byte 22\n.bend\n.endmodule\n.module app\n.use provider\n.byte provider.routine.saved.value\n.endmodule"), [11,7]);
}

#[test]
fn member_bindings_longer_scalar_cannot_fall_back_to_shorter_compound() {
    let source = "Info .struct\nvalue .byte ?\n.endstruct\nsaved .const Info {value:7}\nsaved.value .const 66\n.byte saved.value.value";
    let assembler = run_pass1(&source.lines().collect::<Vec<_>>());
    assert!(
        assembler
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error
                && d.error
                    .message()
                    .contains("Member expression requires struct base value")),
        "{:?}",
        assembler.diagnostics
    );
}

#[test]
fn member_bindings_pruned_schema_preserves_nested_namespace() {
    assert_eq!(bytes(".module provider\n.pub\nroutine .block\n.namespace inner\nInfo .struct\nvalue .byte ?\n.endstruct\n.endnamespace\nsaved .const inner.Info {value:7}\n.byte 11\n.bend\nother .block\nprovider.routine.saved.value .const provider.routine.inner.Info {value:66}\n.byte 22\n.bend\n.endmodule\n.module app\n.use provider as lib\n.byte lib.routine.saved.value.value\n.endmodule"), [22,66]);
}

#[test]
fn member_bindings_schema_replay_does_not_publish_private_or_inactive_schema() {
    for declaration in [
        ".private\nInfo .struct\nvalue .byte ?\n.endstruct\n.pub",
        ".if 0\nInfo .struct\nvalue .byte ?\n.endstruct\n.endif",
    ] {
        let source = format!(".module provider\n.pub\nroutine .block\n{declaration}\n.byte 11\n.bend\n.endmodule\n.module app\n.use provider\nvalue .const provider.routine.Info {{value:66}}\n.byte value.value\n.endmodule");
        let assembler = run_pass1(&source.lines().collect::<Vec<_>>());
        assert!(
            assembler
                .diagnostics
                .iter()
                .any(|d| d.severity == Severity::Error),
            "unavailable schema accepted: {source}"
        );
    }
}

#[test]
fn member_bindings_pruned_schema_preserves_module_parameter_layout_snapshot() {
    let source = ".module provider\n.pub\nroutine .block\nInfo .struct\nvalue .res width\n.endstruct\n.byte 11\n.bend\nother .block\nsaved .const provider.routine.Info {value:66}\n.byte 22\n.bend\n.endmodule\n.module app\n.use provider\n.byte provider.other.saved.value\n.endmodule";
    let lines = source.lines().map(str::to_owned).collect::<Vec<_>>();
    let mut assembler = Assembler::new();
    assembler.module_parameters.insert(
        "provider".into(),
        HashMap::from([("width".into(), types::asm_value::AsmValue::Scalar(3))]),
    );
    assert_eq!(
        assembler.pass1(&lines).errors,
        0,
        "{:?}",
        assembler.diagnostics
    );
    let mut output = Vec::new();
    let mut listing = ListingWriter::new(&mut output, false);
    assert_eq!(
        assembler.pass2(&lines, &mut listing).unwrap().errors,
        0,
        "{:?}",
        assembler.diagnostics
    );
    assert_eq!(assembler.symbols.lookup("provider.routine.Info"), Some(3));
    assert_eq!(
        assembler
            .image()
            .entries()
            .unwrap()
            .into_iter()
            .map(|(_, byte)| byte)
            .collect::<Vec<_>>(),
        [22, 66]
    );
}

#[test]
fn member_bindings_executed_schema_supersedes_carried_metadata() {
    let mut symbols = SymbolTable::new();
    let registry = default_registry();
    let mut asm = make_asm_line(&mut symbols, &registry);
    let definition = |size| types::asm_value::StructDef {
        name: "Info".into(),
        size,
        fields: vec![types::asm_value::StructField {
            name: "value".into(),
            offset: 0,
            size,
        }],
    };
    asm.seed_schema_replay(&[crate::line::SchemaReplaySnapshot {
        definition: definition(1),
        visibility: SymbolVisibility::Private,
        module: None,
    }])
    .unwrap();
    assert_eq!(process_line(&mut asm, ".pub", 0, 1), LineStatus::Ok);
    assert_eq!(
        asm.register_struct_definition(definition(2)),
        LineStatus::Ok
    );
    assert_eq!(asm.symbols().lookup("Info"), Some(2));
    assert_eq!(
        asm.symbols().entry("Info").unwrap().visibility,
        SymbolVisibility::Public
    );
    let value = asm
        .eval_value_ast(&Expr::Identifier("Info".into(), Span::default()))
        .unwrap();
    let types::asm_value::AsmValue::Struct(def) = value else {
        panic!("schema became a scalar");
    };
    assert_eq!(def.fields[0].size, 2);
    // A second actual source declaration remains a duplicate.
    assert_eq!(
        asm.register_struct_definition(definition(3)),
        LineStatus::Error
    );
}

#[test]
fn member_bindings_filegraph_forward_compound_survives_layout_stabilization() {
    let dir = create_temp_dir("member-forward-filegraph");
    for cpu in ["68020", "6502"] {
        fs::write(dir.join("entry.asm"), format!(".module app\n.cpu {cpu}\n.use provider\n.byte provider.routine.saved1.value\n.endmodule\n")).unwrap();
        fs::write(dir.join("provider.asm"), format!(".module provider\n.cpu {cpu}\n.pub\nroutine .block\nInfo .struct\nvalue .byte ?\n.endstruct\nsaved1 .const later\nlater .const Info {{value:66}}\n.bend\n.endmodule\n")).unwrap();
        let (entries, diagnostics) =
            assemble_example_entries_with_runtime_mode(&dir.join("entry.asm"), true).unwrap();
        assert!(diagnostics.is_empty(), "{cpu}: {diagnostics:?}");
        assert_eq!(
            entries
                .into_iter()
                .map(|(_, byte)| byte)
                .collect::<Vec<_>>(),
            [66],
            "{cpu}"
        );
        let output = dir.join("output.bin");
        let cli = Cli::parse_from([
            "opforge".to_string(),
            dir.join("entry.asm").to_string_lossy().into_owned(),
            "--bin".to_string(),
            output.to_string_lossy().into_owned(),
        ]);
        let config = validate_cli(&cli).unwrap();
        run_with_validated_cli_with_context(&cli, &config).unwrap();
        assert_eq!(fs::read(output).unwrap(), [66], "{cpu} CLI");
    }
    fs::remove_dir_all(dir).unwrap();
}

#[test]
fn member_bindings_filegraph_parameter_schema_snapshot() {
    let dir = create_temp_dir("member-schema-parameter-filegraph");
    fs::write(dir.join("entry.asm"), ".module app\n.cpu 68020\n.use provider with (width=3)\n.byte provider.other.saved.value,provider.routine.Info\n.endmodule\n").unwrap();
    fs::write(dir.join("provider.asm"), ".module provider\n.cpu 68020\n.pub\nroutine .block\nInfo .struct\nvalue .res width\n.endstruct\n.byte 11\n.bend\nother .block\nsaved .const provider.routine.Info {value:66}\n.byte 22\n.bend\n.endmodule\n").unwrap();
    let output = dir.join("out.bin");
    let cli = Cli::parse_from([
        "opforge".to_string(),
        dir.join("entry.asm").to_string_lossy().into_owned(),
        "--bin".to_string(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = validate_cli(&cli).unwrap();
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    assert_eq!(fs::read(output).unwrap(), [11, 22, 66, 3]);
    fs::remove_dir_all(dir).unwrap();
}

#[test]
fn member_bindings_schema_dimensions_keep_forward_and_local_shadow_rules() {
    for cpu in ["68020", "6502"] {
        assert_eq!(bytes(&format!(".cpu {cpu}\nInfo .struct\nvalue .res count\n.endstruct\ncount .const 3\n.byte Info")), [3]);
        assert_eq!(bytes(&format!(".module provider\n.cpu {cpu}\nwidth .const 3\nroutine .block\nInfo .struct\nvalue .res width\n.endstruct\nwidth .const 5\n.byte Info\n.bend\n.endmodule")), [5]);
        assert_eq!(bytes(&format!(".module provider\n.cpu {cpu}\nwidth .const 3\nroutine .block\nInfo .struct\nvalue .res width\n.endstruct\n.byte Info\n.bend\n.endmodule")), [3]);
    }
    let lines = [
        "Info .struct",
        "value .res missing",
        ".endstruct",
        ".byte Info",
    ]
    .map(str::to_owned);
    let mut assembler = Assembler::new();
    assert!(assembler.pass1(&lines).errors > 0);
    assert!(
        assembler
            .diagnostics
            .iter()
            .any(|diagnostic| diagnostic.error.message().contains("Label not found")),
        "{:?}",
        assembler.diagnostics
    );
}

#[test]
fn member_bindings_longer_record_owner_precedes_shorter_struct() {
    assert_eq!(bytes("items .bfor 1\nvalue .const 66\n.endfor\nInfo .struct\nvalue .byte ?\n.endstruct\nsaved .const Info {value:7}\nsaved.value .const items[0]\n.byte saved.value.value"), [66]);
}
