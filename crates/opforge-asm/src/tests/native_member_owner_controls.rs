//! Declared binding ownership and schema metadata survive numeric preparation.
use super::native_active_includes::{live_oracle, selected_names};
use super::native_iterable_loops::{build_image, oracle, scratch};
use super::native_source_cases::{self, Case};
use super::*;

fn cases(cpu: &str) -> Vec<Case> {
    let entries = [
        (
            "schema-only-owner",
            "saved1 .const Info {value:7}\n",
            "saved .const provider.routine.Info {value:later}\nlater .const 66\n",
            ".byte provider.other.saved.value\n",
            Some(vec![22, 66]),
        ),
        (
            "inactive-longer-declaration",
            "saved1 .const Info {value:7}\n",
            ".if 0\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\n",
            ".byte provider.routine.saved1.value\n",
            Some(vec![11, 7]),
        ),
        (
            "deferred-inactive-longer-declaration",
            "saved1 .const Info {value:7}\n",
            ".if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const 0\n",
            ".byte provider.routine.saved1.value\n",
            Some(vec![11, 7]),
        ),
        (
            "deferred-active-longer-declaration",
            "saved1 .const Info {value:7}\n",
            ".if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const 1\n",
            ".byte provider.routine.saved1.value.value\n",
            Some(vec![22, 66]),
        ),
        (
            "longest-scalar-rejects-member",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n",
            "provider.routine.saved1.value .const 66\n",
            ".byte provider.routine.saved1.value.value\n",
            None,
        ),
    ];
    let names = entries.iter().map(|entry| entry.0).collect::<Vec<_>>();
    let selected = selected_names("OPFORGE_MEMBER_OWNER_CASES", &names);
    entries
        .into_iter()
        .filter(|entry| selected.contains(&entry.0))
        .map(|(name, routine, other, body, expected)| Case {
            name: format!("{name}-{cpu}"),
            cpu: cpu.into(),
            files: vec![
                ("entry.asm", format!(".module app\n.cpu {cpu}\n.use provider\n{body}.endmodule\n")),
                ("provider.asm", format!(".module provider\n.cpu {cpu}\n.pub\nroutine .block\nInfo .struct\nvalue .byte ?\n.endstruct\n{routine}.byte 11\n.bend\nother .block\n{other}.byte 22\n.bend\n.endmodule\n")),
            ],
            expected,
        })
        .collect()
}

#[test]
fn native_member_owner_controls_rust_oracles() {
    for cpu in selected_names("OPFORGE_INCLUDE_CPUS", &["68020", "6502"]) {
        for case in cases(cpu) {
            let dir = scratch();
            if let Some(expected) = case.expected {
                assert_eq!(live_oracle(&dir.0, &case.files), expected, "{}", case.name);
            } else {
                for (path, source) in &case.files {
                    fs::write(dir.0.join(path), source).unwrap();
                }
                let error = oracle(&dir.0, &case.files[0].1).unwrap_err();
                assert!(
                    error.contains("Member expression requires struct base value"),
                    "{}: {error}",
                    case.name
                );
            }
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; actual declaration and schema owner controls"]
fn native_member_owner_controls_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let all = selected_names("OPFORGE_INCLUDE_CPUS", &["68020", "6502"])
        .into_iter()
        .flat_map(cases)
        .collect::<Vec<_>>();
    native_source_cases::run(&root, &image, &all);
}
