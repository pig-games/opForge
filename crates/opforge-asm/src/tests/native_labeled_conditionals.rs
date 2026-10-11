//! Fresh artifact proof for labeled numeric conditional controls.
use super::*;

fn native_cases() -> Vec<super::native_source_cases::Case> {
    let mut cases = Vec::new();
    for cpu in ["6502", "68020"] {
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
    super::native_source_cases::run(&root, &image, &native_cases());
}
