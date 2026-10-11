//! Fresh artifact proof for labeled numeric conditional controls.
use super::*;

fn native_cases() -> Vec<super::native_source_cases::Case> {
    let mut cases = Vec::new();
    for cpu in ["6502", "68020"] {
        // Placed flat addresses must retain their absolute value and target
        // byte order, even when logical imports use independent geometry.
        let expected = match cpu {
            "6502" => vec![0x02, 0x09, 0x60],
            "68020" => vec![0x09, 0x02, 0x4e, 0x75],
            _ => unreachable!(),
        };
        cases.push(super::native_source_cases::Case {
            name: format!("labeled-controls-mapped-word-{cpu}"),
            cpu: cpu.into(),
            files: vec![("entry.asm", format!(".module main\n.cpu {cpu}\n.use provider map {{ logical -> code }}\n.region ram,$0900,$09ff\n.section header,kind=data\n.word provider.entry\n.endsection\n.section code,kind=code\n.endsection\n.place header in ram\n.place code in ram\n.endmodule\n.module provider\n.cpu {cpu}\n.pub\n.section logical,kind=code,logical\nentry .block\n\trts\n.bend\n.endsection\n.endmodule\n"))],
            expected: Some(expected),
        });
        for (name, control, payload) in [
            ("unconditional", ".byte 2", vec![9, 3, 1, 2]),
            ("literal-true", ".if 1\n.byte 2\n.endif", vec![9, 3, 1, 2]),
            ("literal-false", ".if 0\n.byte 2\n.endif", vec![9, 3, 1]),
        ] {
            let source = format!(".module main\n.cpu {cpu}\n.use provider map {{ logical -> code }}\n.region ram,0,$ffff\n.section data,kind=data\n.byte 9,provider.anchor\n.endsection\n.section code,kind=code\n.byte 1\n.endsection\n.place data in ram\n.place code in ram\n.endmodule\n.module provider\n.cpu {cpu}\n.pub\n.section logical,kind=code,logical\nanchor\n{control}\n.endsection\n.endmodule\n");
            cases.push(super::native_source_cases::Case {
                name: format!("labeled-controls-mapped-baseline-{cpu}-{name}"),
                cpu: cpu.into(),
                files: vec![("entry.asm", source)],
                expected: Some(payload),
            });
        }
        for (form, colon) in [("canonical", ""), ("colon", ":")] {
            let body = format!(".org 16\nopening{colon} .if opening == $\n.byte 1\nalternative{colon} .elseif 1\n.byte 99\nterminal{colon} .else\n.byte 98\nclosing{colon} .endif\nfalse_open{colon} .if 0\n.byte 97\nfalse_arm{colon} .elseif false_arm != $\n.byte 96\ntrue_else{colon} .else\n.byte 2\nfalse_end{colon} .endif\n.byte opening,alternative,terminal,closing,false_open,false_arm,true_else,false_end\n");
            cases.push(super::native_source_cases::Case {
                name: format!("labeled-controls-{cpu}-{form}"),
                cpu: cpu.into(),
                files: vec![(
                    "entry.asm",
                    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n"),
                )],
                expected: Some(vec![1, 2, 16, 17, 17, 17, 17, 17, 17, 18]),
            });
            cases.push(super::native_source_cases::Case {
                name: format!("labeled-controls-mapped-{cpu}-{form}"),
                cpu: cpu.into(),
                files: vec![(
                    "entry.asm",
                    crate::tests::labeled_conditionals::mapped_source(cpu, colon),
                )],
                expected: Some(vec![9, 4, 5, 1, 2, 4, 5, 5, 5]),
            });
            let body = format!("items .bfor i in {{0,1}}\nopening{colon} .if i == 0\n.byte 3\nalternative{colon} .elseif i == 1\n.byte 4\nterminal{colon} .else\n.byte 99\nclosing{colon} .endif\n.endfor\n.byte items[0].opening,items[0].closing,items[1].opening,items[1].closing\n");
            cases.push(super::native_source_cases::Case {
                name: format!("labeled-controls-scoped-{cpu}-{form}"),
                cpu: cpu.into(),
                files: vec![(
                    "entry.asm",
                    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n"),
                )],
                expected: Some(vec![3, 4, 0, 1, 1, 2]),
            });
        }
        for (name, query, expected) in [
            ("inactive-parent", "outer,outer_end", Some(vec![0, 0])),
            ("inactive-label-rejection", "inner", None),
        ] {
            cases.push(super::native_source_cases::Case {
                name: format!("labeled-controls-{cpu}-{name}"),
                cpu: cpu.into(),
                files: vec![("entry.asm", format!(".module main\n.cpu {cpu}\nouter .if 0\ninner .if 1\ninner_arm .elseif 1\ninner_else .else\ninner_end .endif\nouter_end .endif\n.byte {query}\n.endmodule\n"))],
                expected,
            });
        }
    }
    cases
}

#[test]
fn labeled_conditionals_native_matrix_has_live_rust_oracles() {
    for case in native_cases() {
        let dir = super::native_iterable_loops::scratch();
        match case.expected {
            Some(expected) => assert_eq!(
                super::native_active_includes::live_oracle(&dir.0, &case.files),
                expected,
                "{}",
                case.name
            ),
            None => assert!(
                super::native_iterable_loops::oracle(&dir.0, &case.files[0].1).is_err(),
                "{}",
                case.name
            ),
        }
    }
}

#[test]
#[ignore = "requires fresh FS-UAE native completion and current compact CLI image"]
fn native_labeled_conditionals_exact_artifact_matrix() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = super::native_iterable_loops::scratch();
    let image = match std::env::var_os("OPFORGE_CORPUS_IMAGE") {
        Some(path) => fs::read(path).expect("prebuilt current compact CLI image"),
        None => super::native_iterable_loops::build_image(&root, &dir.0),
    };
    let filter = std::env::var("OPFORGE_LABELED_CONTROLS_FILTER").unwrap_or_default();
    let cases = native_cases()
        .into_iter()
        .filter(|case| {
            filter.is_empty()
                || filter
                    .split(',')
                    .any(|part| case.name.contains(part.trim()))
        })
        .collect::<Vec<_>>();
    assert!(
        !cases.is_empty(),
        "labeled control filter selected no cases"
    );
    super::native_source_cases::run(&root, &image, &cases);
}

fn cli_hunk_source(cpu: &str, directive: &str) -> String {
    format!(".module main\n.cpu {cpu}\n.use provider map {{ logical -> code }}\n.region ram,0,$ffff\n.section code,kind=code\n{directive} provider.target\n.endsection\n.section data,kind=data\n.byte 9\n.endsection\n.place code in ram\n.place data in ram\n.endmodule\n.module provider\n.cpu {cpu}\n.pub\n.section logical,kind=code,logical\ntarget .byte 7\n.endsection\n.endmodule\n")
}

fn cli_hunk_oracle(dir: &Path, source: &str) -> Result<Vec<u8>, String> {
    use clap::Parser;
    let input = dir.join("entry.asm");
    let output = dir.join("out.hunk");
    fs::write(&input, source).unwrap();
    let cli = cli_core::Cli::parse_from([
        "opForge".into(),
        input.to_string_lossy().into_owned(),
        "--hunk".into(),
        output.to_string_lossy().into_owned(),
    ]);
    let config = cli_core::validate_cli(&cli).map_err(|e| e.to_string())?;
    cli_core::run_with_validated_cli_with_context(&cli, &config).map_err(|e| format!("{e:?}"))?;
    fs::read(output).map_err(|e| e.to_string())
}

#[test]
fn cli_hunk_without_source_output_requires_section_selection() {
    for cpu in ["6502", "68020"] {
        let dir = super::native_iterable_loops::scratch();
        for directive in [".long", ".byte"] {
            assert!(cli_hunk_oracle(&dir.0, &cli_hunk_source(cpu, directive)).is_err());
        }
    }
}

#[test]
#[ignore = "requires fresh FS-UAE proof for CLI-only Hunk relocation policy"]
fn native_cli_hunk_without_source_output_rejections() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = super::native_iterable_loops::scratch();
    let image = std::env::var_os("OPFORGE_CORPUS_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| super::native_iterable_loops::build_image(&root, &dir.0));
    for cpu in ["6502", "68020"] {
        for directive in [".long", ".byte"] {
            let source = cli_hunk_source(cpu, directive);
            let expected = cli_hunk_oracle(&dir.0, &source);
            assert!(
                expected.is_err(),
                "CLI-only Hunk needs a source section selection"
            );
            super::native_while_layouts::native_case(
                &root,
                &image,
                &format!("cli-hunk-only-{cpu}-{directive}"),
                &source,
                "Work:entry.asm --hunk Work:out.hunk",
                &[],
                true,
            );
        }
        let source = source_hunk(cpu);
        let expected = super::native_while_layouts::hunk_oracle(&dir.0, &source);
        super::native_while_layouts::native_case(
            &root,
            &image,
            &format!("source-hunk-{cpu}-long"),
            &source,
            "Work:entry.asm --hunk Work:out.hunk",
            &expected,
            false,
        );
        let narrow = source.replace(".long target", ".byte target");
        assert!(cli_hunk_oracle(&dir.0, &narrow).is_err());
        super::native_while_layouts::native_case(
            &root,
            &image,
            &format!("source-hunk-{cpu}-byte"),
            &narrow,
            "Work:entry.asm --hunk Work:out.hunk",
            &[],
            true,
        );
    }
}

#[test]
fn source_hunk_preserves_long_relocations_and_rejects_narrow_addresses() {
    for cpu in ["6502", "68020"] {
        let dir = super::native_iterable_loops::scratch();
        let source = source_hunk(cpu);
        let image = super::native_while_layouts::hunk_oracle(&dir.0, &source);
        let words = image
            .chunks_exact(4)
            .map(|w| u32::from_be_bytes(w.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert!(words.windows(5).any(|w| w == [1004, 1, 1, 0, 0]));
        assert!(cli_hunk_oracle(&dir.0, &source.replace(".long target", ".byte target")).is_err());
    }
}

fn source_hunk(cpu: &str) -> String {
    format!(".module main\n.cpu {cpu}\n.section code,kind=code\n.long target\n.endsection\n.section data,kind=data\ntarget .byte 7\n.endsection\n.output \"out.hunk\",format=hunk,sections=code,data\n.endmodule\n")
}
