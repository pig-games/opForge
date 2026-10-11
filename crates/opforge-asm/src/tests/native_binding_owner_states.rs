//! Binding owner selection must account for forward values and mutable kinds.
use super::native_active_includes::{live_oracle, selected_names};
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::native_source_cases::{self, Case};
use super::*;

const SIGNED_GUARD_DECLARATIONS: &str = "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if helper == -2\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nhelper .const n/7\nn .const -17\n.byte 22\n";

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
        (
            "forward-enabled-whole-signed-scalar",
            "saved .const provider.routine.saved1.value\n.byte saved\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const -17\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 239]),
        ),
        (
            "forward-enabled-whole-address-alias",
            "saved .const provider.routine.saved1.value\n.byte saved-provider.other.start\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\nstart:\n.if enabled\nprovider.routine.saved1.value .const start\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 0]),
        ),
        (
            "forward-enabled-transitive-immutable-guard",
            ".byte provider.routine.saved1.value.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const helper\nhelper .const 1\n.byte 22\n",
            Some(&[22, 66]),
        ),
        (
            "exclusive-longer-declaration-same-id",
            ".byte provider.routine.saved1.value.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.else\nprovider.routine.saved1.value .const provider.routine.Info {value:77}\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 66]),
        ),
        (
            "forward-transitive-signed-guard",
            ".byte provider.routine.saved1.value.value\n",
            SIGNED_GUARD_DECLARATIONS,
            Some(&[22, 66]),
        ),
        (
            "same-block-longest-scalar-rejects-member-fallback",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\nprovider.routine.saved1.value .const 66\n.byte 11\n",
            None,
        ),
        // Neither a mutable dependency closure nor a PC initializer may certify
        // this conditional declaration from a preparation-time value snapshot.
        (
            "transitive-mutable-guard-scalar-rejects-fallback",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\nflag .var 1\nhelper .const flag\nenabled .const helper\nflag .set 0\n.if enabled\nprovider.routine.saved1.value .const 66\n.endif\n.byte 11\n",
            None,
        ),
        (
            "transitive-pc-guard-scalar-rejects-fallback",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.byte 11\norigin .const *\nenabled .const origin\n.if enabled\nprovider.routine.saved1.value .const 66\n.endif\n",
            None,
        ),
        (
            "unmaterialized-loop-inactive-longer-keeps-shorter",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {0}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.endfor\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unmaterialized-loop-active-longer-rejects-declaration",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {1}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.endfor\n.byte 11\n",
            None,
        ),
        (
            "unmaterialized-loop-inactive-repeated-longer-keeps-shorter",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {0}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.if flag\nprovider.routine.saved1.value .const 67\n.endif\n.endfor\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unmaterialized-loop-active-repeated-longer-rejects-declaration",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {1}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.if flag\nprovider.routine.saved1.value .const 67\n.endif\n.endfor\n.byte 11\n",
            None,
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

fn control_source() -> String {
    // The app is the physical entry root. The inline imported provider retains
    // exactly the same alternative owners as the file-tree signed guard case.
    format!(
        ".module app\n.cpu 68020\n.use provider\n.byte provider.routine.saved1.value.value\n.endmodule\n.module provider\n.cpu 68020\n.pub\nroutine .block\nInfo .struct\nvalue .byte ?\n.endstruct\n{SIGNED_GUARD_DECLARATIONS}.bend\n.endmodule\n"
    )
}

#[test]
fn native_binding_owner_control_rust_oracle() {
    let dir = scratch();
    assert_eq!(oracle(&dir.0, &control_source()).unwrap(), [22, 66]);
}

#[test]
#[ignore = "requires configured FS-UAE; timed and MEMD conditional owner control"]
fn native_binding_owner_control_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    run_case(
        &root,
        &dir.0,
        &image,
        "conditional-owner-signed-control",
        "68020",
        &control_source(),
        false,
    );
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
