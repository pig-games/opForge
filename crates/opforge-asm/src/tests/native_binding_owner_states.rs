//! Binding owner selection must account for forward values and mutable kinds.
use super::native_active_includes::{live_oracle, selected_names};
use super::native_iterable_loops::{build_image, oracle, scratch};
use super::native_source_cases::{self, Case};
use super::*;

fn cases(cpu: &str) -> Vec<Case> {
    let entries: &[(&str, &str, &str, Option<&[u8]>)] = &[
        (
            "forward-field-value",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:later}\nlater .const 66\n",
            Some(&[66]),
        ),
        (
            "forward-compound-base",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const later\nlater .const Info {value:66}\n",
            Some(&[66]),
        ),
        (
            "mutable-compound-before-scalar",
            ".byte provider.routine.saved1.value\nprovider.routine.saved1 .set 0\n.byte 7\n",
            "saved1 .var Info {value:66}\n",
            Some(&[66, 7]),
        ),
        (
            "mutable-compound-kind-restored",
            ".byte provider.routine.saved1.value\nprovider.routine.saved1 .set 0\nprovider.routine.saved1 .set provider.routine.Info {value:67}\n.byte provider.routine.saved1.value\n",
            "saved1 .var Info {value:66}\n",
            Some(&[66, 67]),
        ),
        // The longest declaration is physically inside other, although its
        // spelling begins with routine. Keep other (22), prune routine (11).
        (
            "unknown-longer-prefix-different-owner",
            ".byte provider.routine.saved1.value.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\nprovider.routine.saved1.value .const provider.routine.Info {value:later}\nlater .const 66\n.byte 22\n",
            Some(&[22, 66]),
        ),
        // A retained schema descriptor alone must not expose an inactive type.
        // The forward field forces runtime construction rather than a known value.
        (
            "inactive-schema-not-visible",
            ".byte provider.other.saved.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if 0\nHidden .struct\nvalue .byte ?\n.endstruct\n.endif\nsaved .const Hidden {value:later}\nlater .const 66\n.byte 22\n",
            None,
        ),
        (
            "deferred-inactive-schema-not-visible",
            ".byte provider.other.saved.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nHidden .struct\nvalue .byte ?\n.endstruct\n.endif\nenabled .const 0\nsaved .const Hidden {value:later}\nlater .const 66\n.byte 22\n",
            None,
        ),
        (
            "inactive-longer-declaration",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if 0\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\n.byte 22\n",
            Some(&[11, 7]),
        ),
        (
            "forward-enabled-longer-owner",
            ".byte provider.routine.saved1.value.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 66]),
        ),
        (
            "forward-enabled-longer-owner-alias",
            "saved .const provider.routine.saved1.value\n.byte saved.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 66]),
        ),
        (
            "forward-enabled-longer-owner-arithmetic",
            ".byte provider.routine.saved1.value.value+1\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 67]),
        ),
        (
            "forward-disabled-longer-owner",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const 0\n.byte 22\n",
            Some(&[11, 7]),
        ),
        // A shorter address record has a valid nested member path. Selecting
        // the longer scalar must reject rather than fall back to that record.
        (
            "executed-longest-scalar-rejects-member-fallback",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const 66\n.endif\nenabled .const 1\n.byte 22\n",
            None,
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
            expected: expected.map(|bytes| bytes.to_vec()),
        })
        .collect()
}

#[test]
fn native_binding_owner_states_rust_oracles() {
    for cpu in selected_names("OPFORGE_INCLUDE_CPUS", &["68020", "6502"]) {
        for case in cases(cpu) {
            let dir = scratch();
            match case.expected {
                Some(expected) => {
                    assert_eq!(live_oracle(&dir.0, &case.files), expected, "{}", case.name);
                }
                None => {
                    for (path, source) in &case.files {
                        fs::write(dir.0.join(path), source).unwrap();
                    }
                    assert!(
                        oracle(&dir.0, &case.files[0].1).is_err(),
                        "Rust accepted expected rejection {}",
                        case.name
                    );
                }
            }
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
