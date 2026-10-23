//! Static source acquisition precedes graph ordering; runtime graph changes fail.
use super::native_iterable_loops::{build_image, oracle_with_module_roots, scratch};
use super::native_source_cases::{
    run_discovery_batch, run_source_language_batch, run_with_packages, Case,
};
use super::*;

fn cases(cpu: &str) -> Vec<Case> {
    let module = |body: &str| format!(".module main\n.cpu {cpu}\n{body}.endmodule\n");
    let make = |name: &str, files: Vec<(&'static str, String)>, expected| Case {
        name: format!("static-include-{cpu}-{name}"),
        cpu: cpu.into(),
        files,
        expected,
    };
    vec![
        make("root-modules", vec![
            ("entry.asm", format!(".include \"lib.inc\"\n{}", module(".use lib.math as M\n.use lib.math (VALUE as V)\n.byte V,M.VALUE\n"))),
            ("lib.inc", format!(".module lib.math\n.cpu {cpu}\n.pub\nVALUE .const 5\n.endmodule\n")),
        ], Some(vec![5,5])),
        make("root-sections", vec![
            ("entry.asm", format!(".include \"lib.inc\"\n{}", module(".use lib (LIBVAL)\n.region ram,$1000,$10ff\n.section data\n.byte 1,LIBVAL\n.endsection\n.place data in ram\n"))),
            ("lib.inc", format!(".module lib\n.cpu {cpu}\n.pub\nLIBVAL .const 4\n.section data\n.byte 2,3\n.endsection\n.endmodule\n")),
        ], Some(vec![2,3,1,4])),
        make("nested-use-edge", vec![
            ("entry.asm", format!(".include \"library/chosen.asm\"\n.include \"library/helper.asm\"\n{}", module(".use chosen\n.byte 3\n"))),
            ("library/chosen.asm", format!(".module chosen\n.cpu {cpu}\n.include \"detail/outer.inc\"\n.endmodule\n")),
            ("library/detail/outer.inc", ".include \"inner.inc\"\n.byte 2\n".into()),
            ("library/detail/inner.inc", ".use helper\nmark\n.byte helper.value-$1000\n".into()),
            ("library/helper.asm", format!(".module helper\n.cpu {cpu}\n.org $1000\n.pub\nvalue\n.byte 1\n.endmodule\n")),
            ("library/unused.asm", ".module unused\n.include \"missing.inc\"\n.endmodule\n".into()),
        ], Some(vec![1,0,2,3])),
        make("definition-relative-asset", vec![
            ("entry.asm", module(".include \"defs/outer.inc\"\n.byte 7\n")),
            ("defs/outer.inc", ".include \"leaf.inc\"\n".into()),
            ("defs/leaf.inc", ".incbin \"data.bin\"\n".into()),
            ("defs/data.bin", "AB".into()),
            ("data.bin", "XX".into()),
        ], Some(vec![65,66,7])),
        make("inactive-preprocessor", vec![
            ("entry.asm", format!(".ifdef ABSENT\n.include \"missing.inc\"\n.endif\n.include \"lib.inc\"\n{}", module(".use lib (VALUE)\n.byte VALUE\n"))),
            ("lib.inc", format!(".module lib\n.cpu {cpu}\n.pub\nVALUE=7\n.endmodule\n")),
        ], Some(vec![7])),
        make("runtime-data", vec![
            ("entry.asm", module(".if 1\n.include \"data.inc\"\n.endif\n.byte 8\n")),
            ("data.inc", ".byte 7\n".into()),
        ], Some(vec![7,8])),
        make("runtime-frontier-preserved", vec![
            ("entry.asm", module(".if 0\n.include \"missing.inc\"\n.endif\n.include \"change.inc\"\n")),
            ("change.inc", ".module introduced\n.endmodule\n".into()),
        ], None),
        make("missing-selected-sibling", vec![
            ("entry.asm", format!(".include \"mixed.inc\"\n{}", module(".use chosen\n.byte 7\n"))),
            ("mixed.inc", ".module unused\n.include \"missing.inc\"\n.endmodule\n.module chosen\n.endmodule\n".into()),
        ], None),
        make("static-cycle", vec![
            ("entry.asm", module(".include \"a.inc\"\n")),
            ("a.inc", ".include \"b.inc\"\n".into()),
            ("b.inc", ".include \"a.inc\"\n".into()),
        ], None),
        make("runtime-use-change", vec![
            ("entry.asm", module(".if 1\n.include \"change.inc\"\n.endif\n")),
            ("change.inc", ".use helper\n".into()),
        ], None),
        make("expanded-module-change", vec![
            ("entry.asm", module("LOAD .macro\n.include \"change.inc\"\n.endmacro\n.LOAD\n")),
            ("change.inc", ".module introduced\n.endmodule\n".into()),
        ], None),
    ]
}

fn default_cases() -> Vec<Case> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    [
        ("module_use_include", "module_use_lib.inc", vec![5, 5]),
        (
            "section_module_use_include",
            "section_module_use_lib.inc",
            vec![2, 3, 1, 4],
        ),
    ]
    .into_iter()
    .map(|(name, include, expected)| Case {
        name: format!("static-default-{name}"),
        cpu: engine::default_cpu().as_str().into(),
        files: vec![
            (
                "entry.asm",
                fs::read_to_string(root.join("examples/opcore").join(format!("{name}.asm")))
                    .unwrap(),
            ),
            (
                include,
                fs::read_to_string(root.join("examples/opcore").join(include)).unwrap(),
            ),
        ],
        expected: Some(expected),
    })
    .collect()
}

fn discovery_cases(cpu: &str) -> Vec<Case> {
    let make = |name: &str, files: Vec<(&'static str, String)>, expected| Case {
        name: format!("static-discovery-{cpu}-{name}"),
        cpu: cpu.into(),
        files,
        expected,
    };
    let entry = format!(".module main\n.cpu {cpu}\n.use chosen\n.byte 3\n.endmodule\n.end\n");
    vec![
        make("nested-use-edge", vec![
            ("entry.asm", entry.clone()),
            ("library/chosen.asm", format!(".module chosen\n.cpu {cpu}\n.include \"detail/outer.inc\"\n.endmodule\n.end\n")),
            ("library/detail/outer.inc", ".include \"inner.inc\"\n.byte 2\n".into()),
            ("library/detail/inner.inc", ".use helper\nmark\n.byte helper.value-$1000\n".into()),
            ("library/helper.asm", format!(".module helper\n.cpu {cpu}\n.org $1000\n.pub\nvalue\n.byte 1\n.endmodule\n.end\n")),
            ("library/unused.asm", ".module unused\n.include \"missing.inc\"\n.endmodule\n".into()),
        ], Some(vec![1,0,2,3])),
        make("missing-selected-sibling", vec![
            ("entry.asm", entry.clone()),
            ("library/mixed.asm", format!(".module unused\n.include \"missing.inc\"\n.endmodule\n.module chosen\n.cpu {cpu}\n.byte 1\n.endmodule\n.end\n")),
        ], None),
        make("unselected-file-unopened", vec![
            ("entry.asm", entry),
            ("library/chosen.asm", format!(".module chosen\n.cpu {cpu}\n.byte 1\n.endmodule\n.end\n")),
            ("library/unused.asm", ".module unused\n.include \"missing.inc\"\n.endmodule\n".into()),
        ], Some(vec![1,3])),
    ]
}

fn assert_oracles(cases: Vec<Case>, roots: &[&str]) {
    for case in cases {
        let dir = scratch();
        for (path, source) in &case.files {
            let path = dir.0.join(path);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, source).unwrap();
        }
        let actual = oracle_with_module_roots(&dir.0, &case.files[0].1, roots);
        match case.expected {
            Some(expected) => assert_eq!(actual.unwrap(), expected, "{}", case.name),
            None => assert!(actual.is_err(), "{}", case.name),
        }
    }
}

#[test]
fn native_static_include_graph_rust_oracles() {
    assert_oracles(default_cases(), &[]);
    for cpu in ["68020", "6502"] {
        assert_oracles(cases(cpu), &[]);
        assert_oracles(discovery_cases(cpu), &["library"]);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; static includes contribute to the numeric graph"]
fn native_static_include_graph_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_STATIC_INCLUDE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_STATIC_INCLUDE_FILTER").unwrap_or_default();
    let cases = ["68020", "6502"]
        .into_iter()
        .flat_map(cases)
        .filter(|case| filter.is_empty() || case.name.contains(&filter))
        .collect::<Vec<_>>();
    if !cases.is_empty() {
        run_source_language_batch(&root, &image, &cases, 32);
    }
    let discovery = ["68020", "6502"]
        .into_iter()
        .flat_map(discovery_cases)
        .filter(|case| filter.is_empty() || case.name.contains(&filter))
        .collect::<Vec<_>>();
    if !discovery.is_empty() {
        run_discovery_batch(&root, &image, &discovery, 32);
    }
    let defaults = default_cases()
        .into_iter()
        .filter(|case| filter.is_empty() || case.name.contains(&filter))
        .collect::<Vec<_>>();
    if !defaults.is_empty() {
        // The engine default remains authoritative. The ordinary current
        // producer package supplies this target absent from the frozen embed set.
        let registry = engine::build_default_asm_registry();
        let core = vm::runtime_model_core::RuntimeModelCore::from_registry(&registry).unwrap();
        let resolved = core
            .resolve_pipeline(engine::default_cpu().as_str(), None)
            .unwrap();
        let bytes = crate::native_runtime_package::prepare_package(&core, &resolved).unwrap();
        let path = format!("packages/{}--{}.bin", resolved.cpu_id, resolved.dialect_id);
        run_with_packages(
            &root,
            &image,
            &defaults,
            &[OpforgeNativeCliGuestFile {
                relative_path: &path,
                bytes: &bytes,
            }],
        );
    }
}
