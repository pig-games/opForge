//! Binding owner selection must account for forward values and mutable kinds.
use super::native_active_includes::{live_oracle, selected_names};
use super::native_iterable_loops::{build_image, scratch};
use super::native_source_cases::{self, Case};
use super::*;

fn cases(cpu: &str) -> Vec<Case> {
    let entries: &[(&str, &str, &str, &[u8])] = &[
        (
            "forward-field-value",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:later}\nlater .const 66\n",
            &[66],
        ),
        (
            "forward-compound-base",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const later\nlater .const Info {value:66}\n",
            &[66],
        ),
        (
            "mutable-compound-before-scalar",
            ".byte provider.routine.saved1.value\nprovider.routine.saved1 .set 0\n.byte 7\n",
            "saved1 .var Info {value:66}\n",
            &[66, 7],
        ),
        (
            "mutable-compound-kind-restored",
            ".byte provider.routine.saved1.value\nprovider.routine.saved1 .set 0\nprovider.routine.saved1 .set provider.routine.Info {value:67}\n.byte provider.routine.saved1.value\n",
            "saved1 .var Info {value:66}\n",
            &[66, 67],
        ),
        // The longest declaration is physically inside other, although its
        // spelling begins with routine. Keep other (22), prune routine (11).
        (
            "unknown-longer-prefix-different-owner",
            ".byte provider.routine.saved1.value.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\nprovider.routine.saved1.value .const provider.routine.Info {value:later}\nlater .const 66\n.byte 22\n",
            &[22, 66],
        ),
    ];
    let names = entries.iter().map(|entry| entry.0).collect::<Vec<_>>();
    let selected = selected_names("OPFORGE_BINDING_OWNER_CASES", &names);
    entries
        .iter()
        .filter(|entry| selected.contains(&entry.0))
        .map(|(name, body, declarations, expected)| Case {
            name: format!("{name}-{cpu}"),
            cpu: cpu.into(),
            files: vec![
                ("entry.asm", format!(".module app\n.cpu {cpu}\n.use provider\n{body}.endmodule\n")),
                ("provider.asm", format!(".module provider\n.cpu {cpu}\n.pub\nroutine .block\nInfo .struct\nvalue .byte ?\n.endstruct\n{declarations}.bend\n.endmodule\n")),
            ],
            expected: Some(expected.to_vec()),
        })
        .collect()
}

#[test]
fn native_binding_owner_states_rust_oracles() {
    for cpu in selected_names("OPFORGE_INCLUDE_CPUS", &["68020", "6502"]) {
        for case in cases(cpu) {
            let dir = scratch();
            assert_eq!(
                live_oracle(&dir.0, &case.files),
                case.expected.unwrap(),
                "{}",
                case.name
            );
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; forward and mutable member owner states"]
fn native_binding_owner_states_fs_uae() {
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
