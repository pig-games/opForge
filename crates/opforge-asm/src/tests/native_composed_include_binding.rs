//! Retained positional recipes must bind active members and reject ordinary @0 names.
use super::native_active_includes::{live_oracle, selected_names};
use super::native_iterable_loops::{build_image, oracle, scratch};
use super::native_source_cases::{self, Case};
use super::*;

fn provider(cpu: &str) -> String {
    format!(
        ".module provider\n.cpu {cpu}\n.pub\nroutine .block\nInfo .struct\nvalue .byte ?\n.endstruct\nsaved1 .const Info {{value:66}}\n.bend\n.endmodule\n"
    )
}

fn active_files(cpu: &str) -> Vec<(&'static str, String)> {
    vec![
        (
            "entry.asm",
            format!(
                ".module app\n.cpu {cpu}\n.use provider\nSCAN .macro suffix\n.for 1\n.if 1\n.byte provider.routine.saved@1.value\n.endif\n.include \"leaf.inc\"\n.endfor\n.endmacro\n.SCAN 1\n.endmodule\n"
            ),
        ),
        ("provider.asm", provider(cpu)),
        ("leaf.inc", ".byte 60\n".into()),
    ]
}

type FileTree = Vec<(&'static str, String)>;
type PositiveCase = (&'static str, FileTree, Vec<u8>);

fn positive_cases(cpu: &str) -> Vec<PositiveCase> {
    let active = active_files(cpu);
    let direct = vec![
        ("entry.asm", format!(".module app\n.cpu {cpu}\n.use provider\n.byte provider.routine.saved1.value\n.endmodule\n")),
        ("provider.asm", provider(cpu)),
    ];
    let mut without_include = active.clone();
    without_include[0].1 = without_include[0].1.replace(".include \"leaf.inc\"\n", "");
    let mut emitted_provider = active.clone();
    emitted_provider[1].1 = emitted_provider[1]
        .1
        .replace(".bend\n", ".byte 99\n.bend\n");
    let mut unreferenced_provider = active.clone();
    unreferenced_provider[0].1 = unreferenced_provider[0]
        .1
        .replace(".byte provider.routine.saved@1.value\n", ".byte 7\n");
    let shadowed_alias = vec![
        ("entry.asm", format!(".module app\n.cpu {cpu}\n.use provider (routine as saved1)\n.namespace inner\nInfo .struct\nvalue .byte ?\n.endstruct\nsaved1 .const Info {{value:7}}\nSCAN .macro suffix\n.for 1\n.byte saved@1.value\n.include \"leaf.inc\"\n.endfor\n.endmacro\n.SCAN 1\n.endnamespace\n.endmodule\n")),
        ("provider.asm", provider(cpu).replace(".bend\n", ".byte 99\n.bend\n")),
        ("leaf.inc", ".byte 60\n".into()),
    ];
    let cases = vec![
        ("active-composed-block-reference", active, vec![66, 60]),
        ("direct-qualified-block-member", direct, vec![66]),
        ("active-composed-without-include", without_include, vec![66]),
        (
            "active-composed-emitted-provider",
            emitted_provider,
            vec![99, 66, 60],
        ),
        (
            "unreferenced-empty-provider",
            unreferenced_provider,
            vec![7, 60],
        ),
        (
            "composed-member-shadows-outer-alias",
            shadowed_alias,
            vec![7, 60],
        ),
    ];
    let names = cases.iter().map(|case| case.0).collect::<Vec<_>>();
    let selected = selected_names("OPFORGE_COMPOSED_BINDING_CASES", &names);
    cases
        .into_iter()
        .filter(|case| selected.contains(&case.0))
        .collect()
}

#[test]
fn native_composed_include_binding_rust_oracles() {
    for cpu in selected_names("OPFORGE_INCLUDE_CPUS", &["68020", "6502"]) {
        for (name, files, expected) in positive_cases(cpu) {
            let dir = scratch();
            assert_eq!(live_oracle(&dir.0, &files), expected, "{name} cpu={cpu}");
        }

        let malformed = scratch();
        fs::write(malformed.0.join("provider.asm"), provider(cpu)).unwrap();
        let source = format!(
            ".module app\n.cpu {cpu}\n.use provider\n.byte provider.routine.saved@0.value\n.endmodule\n"
        );
        let error = oracle(&malformed.0, &source)
            .expect_err("ordinary malformed @0 member must not become a retained recipe");
        assert!(
            !error.is_empty(),
            "cpu={cpu}: missing malformed-name diagnostic"
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; active composed member and source acquisition"]
fn native_composed_include_binding_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let mut cases = Vec::new();
    for cpu in selected_names("OPFORGE_INCLUDE_CPUS", &["68020", "6502"]) {
        for (name, files, expected) in positive_cases(cpu) {
            cases.push(Case {
                name: format!("{name}-{cpu}"),
                cpu: cpu.into(),
                files,
                expected: Some(expected),
            });
        }
        let malformed = vec![
            (
                "entry.asm",
                format!(".module app\n.cpu {cpu}\n.use provider\n.byte provider.routine.saved@0.value\n.endmodule\n"),
            ),
            ("provider.asm", provider(cpu)),
        ];
        cases.push(Case {
            name: format!("ordinary-invalid-composed-name-{cpu}"),
            cpu: cpu.into(),
            files: malformed,
            expected: None,
        });
    }
    native_source_cases::run(&root, &image, &cases);
}
