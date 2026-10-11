//! Retained positional recipes must bind active members and reject ordinary @0 names.
use super::native_active_includes::{
    live_oracle, run_binary_files, run_rejected_files, selected_names,
};
use super::native_iterable_loops::{build_image, oracle, scratch};
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

#[test]
fn native_composed_include_binding_rust_oracles() {
    for cpu in selected_names("OPFORGE_INCLUDE_CPUS", &["68020", "6502"]) {
        let dir = scratch();
        assert_eq!(
            live_oracle(&dir.0, &active_files(cpu)),
            [66, 60],
            "cpu={cpu}"
        );

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
    let mut failures = Vec::new();
    for cpu in selected_names("OPFORGE_INCLUDE_CPUS", &["68020", "6502"]) {
        if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            run_binary_files(
                &root,
                &image,
                "active-composed-block-reference",
                cpu,
                &active_files(cpu),
                &[66, 60],
            );
        }))
        .is_err()
        {
            failures.push(format!("active cpu={cpu}"));
        }
        let malformed = vec![
            ("entry.asm", format!(".module app\n.cpu {cpu}\n.use provider\n.byte provider.routine.saved@0.value\n.endmodule\n")),
            ("provider.asm", provider(cpu)),
        ];
        if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            run_rejected_files(&root, &image, "ordinary-invalid-composed-name", &malformed);
        }))
        .is_err()
        {
            failures.push(format!("malformed cpu={cpu}"));
        }
    }
    assert!(
        failures.is_empty(),
        "composed binding failures: {failures:?}"
    );
}
