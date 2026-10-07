//! Synthetic package policy regressions; both executors receive the same selector edits.
use super::*;
use opcore::{parser::Expr, tokenizer::Span};
use registry::family::AssemblerContext;
use types::symbol::SymbolTable;
use vm::{
    binary_source_package::{BinarySourcePackage, CandidateRecipe},
    execution_model::HierarchyExecutionModel,
};

struct GuardContext {
    symbols: SymbolTable,
    state: std::collections::HashMap<String, u32>,
}
impl AssemblerContext for GuardContext {
    fn eval_expr(&self, expr: &Expr) -> Result<i64, String> {
        match expr {
            Expr::Number(value, _) => value
                .parse()
                .map_err(|_| "invalid numeric test operand".into()),
            Expr::Immediate(inner, _) => self.eval_expr(inner),
            _ => Err("unexpected test operand".into()),
        }
    }
    fn symbols(&self) -> &SymbolTable {
        &self.symbols
    }
    fn has_symbol(&self, _: &str) -> bool {
        false
    }
    fn symbol_is_finalized(&self, _: &str) -> Option<bool> {
        None
    }
    fn current_address(&self) -> u32 {
        0
    }
    fn pass(&self) -> u8 {
        2
    }
    fn cpu_state_flag(&self, key: &str) -> Option<u32> {
        self.state.get(key).copied()
    }
}

fn fixture(hard: bool, fallback: bool) -> (RuntimeModelCore, HierarchyExecutionModel) {
    let mut chunks =
        vm::builder::build_hierarchy_chunks_from_registry(&default_registry()).unwrap();
    let mut candidate = chunks
        .selectors
        .iter()
        .find(|row| {
            row.mnemonic.eq_ignore_ascii_case("MOVEQ")
                && row.shape_key == "immediate_register"
                && row.operand_plan.starts_with("semv.inputs.v1:")
        })
        .unwrap()
        .clone();
    chunks
        .selectors
        .retain(|row| !row.mnemonic.eq_ignore_ascii_case("MOVEQ"));
    let state = package::decode_state_program(
        chunks.state_programs[0].opcode_version,
        &chunks.state_programs[0].program,
    )
    .unwrap();
    let key = &state.keys[0].id;
    let mut guarded = candidate.clone();
    guarded.priority = 0;
    guarded.operand_plan = format!("state.require.v1:{key}=4294967295{};semv.reject.v1:encoding.moveq.immediate.range@reg0.class999",if hard { "?encoding.moveq.immediate.range" } else { "" });
    chunks.selectors.push(guarded);
    if fallback {
        candidate.priority = 1;
        chunks.selectors.push(candidate);
    }
    (
        RuntimeModelCore::from_chunks(chunks.clone()).unwrap(),
        HierarchyExecutionModel::from_chunks(chunks).unwrap(),
    )
}

fn reference(
    core: &RuntimeModelCore,
    model: &HierarchyExecutionModel,
) -> Result<Option<Vec<u8>>, String> {
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let state = core
        .initial_unique_package_state(&resolved, "m68020")
        .unwrap()
        .unwrap();
    let context = GuardContext {
        symbols: SymbolTable::new(),
        state,
    };
    let span = Span::default();
    model
        .encode_instruction_from_exprs(
            "m68020",
            None,
            "MOVEQ",
            &[
                Expr::Immediate(Box::new(Expr::Number("1".into(), span)), span),
                Expr::Register("D0".into(), span),
            ],
            &context,
        )
        .map_err(|e| e.to_string())
}

#[test]
fn compact_state_false_guard_policy_host_contract() {
    for (hard, fallback) in [(false, true), (true, true), (true, false)] {
        let (core, model) = fixture(hard, fallback);
        let resolved = core.resolve_pipeline("m68020", None).unwrap();
        let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
        let guarded = numeric
            .candidates
            .iter()
            .find(|row| numeric.names[row.mnemonic as usize] == "moveq" && row.state_guard != 0)
            .unwrap();
        assert!(matches!(
            guarded.recipe,
            CandidateRecipe::Unsupported { .. }
        ));
        let clause = &numeric.state.guards[guarded.state_guard as usize - 1].clauses[0];
        assert_eq!(clause.reject, hard);
        assert!(!clause
            .values
            .contains(&numeric.state.defaults[clause.key as usize]));
        let wire = prepare_package(&core, &resolved).unwrap();
        assert_eq!(&wire[..4], b"BS31");
        let long =
            |offset| u32::from_be_bytes(wire[offset..offset + 4].try_into().unwrap()) as usize;
        let word = |offset| u16::from_be_bytes(wire[offset..offset + 2].try_into().unwrap());
        let rows = (0..long(20))
            .map(|index| long(16) + index * crate::binary_source_experiment::ROW)
            .filter(|row| word(*row) == guarded.mnemonic)
            .collect::<Vec<_>>();
        assert_eq!(rows.len(), if fallback { 2 } else { 1 });
        assert_eq!(
            word(rows[0] + 30),
            guarded.state_guard,
            "guarded row must execute before fallback"
        );
        assert_eq!(
            wire[rows[0] + 5],
            6,
            "guarded unsupported recipe remains an explicit barrier"
        );
        if fallback {
            assert_eq!(word(rows[1] + 30), 0);
            assert_ne!(
                wire[rows[1] + 5],
                6,
                "later fallback must remain executable"
            );
        }

        let outcome = reference(&core, &model);
        if fallback {
            assert_eq!(outcome.unwrap(), Some(vec![0x70, 1]));
        } else {
            assert!(
                outcome.is_err(),
                "diagnostic guard must reject before nested register mismatch"
            );
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; synthetic same-package guard ordering and fallback policy"]
fn compact_state_false_guard_policy_fs_uae() {
    let mut failures = Vec::new();
    for (hard, fallback) in [(false, true), (true, true), (true, false)] {
        let result = std::panic::catch_unwind(|| {
            let (core, model) = fixture(hard, fallback);
            let expected = reference(&core, &model).ok().flatten();
            assert_eq!(expected.is_some(), fallback);
            let resolved = core.resolve_pipeline("m68020", None).unwrap();
            let package = prepare_package(&core, &resolved).unwrap();
            let result = crate::fs_uae_smoke::run_compact_cli_files_from_env(
                &workspace_root(),
                &package,
                &[(
                    "main.asm",
                    b".module app\n.cpu m68020\n moveq #1,d0\n.endmodule\n.end\n".as_slice(),
                )],
                &[],
                &[],
                expected.as_deref(),
                false,
            )
            .expect("fresh synthetic guard completion");
            let FsUaeSmokeOutcome::Completed { runs } = result else {
                panic!("native execution required")
            };
            assert_eq!(runs.len(), 1);
            assert!(runs[0].protocol_completed);
            assert_eq!(runs[0].success, fallback);
            assert_eq!(runs[0].exit_code, Some(if fallback { 0 } else { 20 }));
        });
        if result.is_err() {
            failures.push((hard, fallback));
        }
    }
    assert!(
        failures.is_empty(),
        "synthetic state guard cases failed: {failures:?}"
    );
}

fn actual_state_binding_source(root: &std::path::Path) -> String {
    let mut source = String::from(".module state_probe\n.cpu 68020\n.use experimental.amigaos.binary_state as state\n.use experimental.amigaos.binary_package as package\n.section code,kind=code\nentry .block\n");
    for function in ["validate", "reset", "find", "apply", "check"] {
        source.push_str(&format!(" bsr.w state.{function}\n"));
    }
    for (record, fields) in [
        (
            "Header",
            &[
                "Keys",
                "Directives",
                "Guards",
                "Reserved",
                "Defaults",
                "DirectiveRows",
                "GuardRows",
            ][..],
        ),
        (
            "Directive",
            &["Head", "Key", "Arguments", "Reserved", "Rows"][..],
        ),
        ("Argument", &["Kind", "Allowed", "Match", "Value"][..]),
        ("Guard", &["Clauses", "Reserved", "Rows"][..]),
        (
            "Clause",
            &["Key", "Values", "Failure", "Reserved", "Rows"][..],
        ),
    ] {
        for field in fields {
            source.push_str(&format!(" move.w state.{record}.{field}(a0),d0\n"));
        }
    }
    source.push_str(" move.l #package.Header.StatePlan,d0\n move.l #package.Header.StatePlanBytes,d0\n rts\n.bend\n.endsection\n.output \"state-binding.hunk\",format=hunk,sections=code,bss\n.endmodule\n");
    for name in ["binary_package.asm", "binary_state.asm"] {
        source.push_str(
            &std::fs::read_to_string(
                root.join("native/motorola68000/amigaos/experimental")
                    .join(name),
            )
            .unwrap(),
        );
        source.push('\n');
    }
    source.push_str(".end\n");
    source
}

#[test]
#[ignore = "requires configured FS-UAE; actual BS31 state/package modules imported by a small Hunk consumer"]
fn compact_actual_state_package_binding_probe_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, OpforgeNativeCliGuestFile,
        OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let root = workspace_root();
    let dir = std::env::temp_dir().join(format!(
        "opforge-state-binding-probe-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&dir).unwrap();
    struct Cleanup(std::path::PathBuf);
    impl Drop for Cleanup {
        fn drop(&mut self) {
            let _ = std::fs::remove_dir_all(&self.0);
        }
    }
    let _cleanup = Cleanup(dir.clone());
    let source = actual_state_binding_source(&root);
    let input = dir.join("entry.asm");
    std::fs::write(&input, &source).unwrap();
    let cli = Cli::parse_from(["opForge", input.to_str().unwrap(), "--cpu", "68020"]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.clone());
    run_with_validated_cli_with_context(&cli, &config)
        .expect("fresh Rust oracle for actual state/package modules");
    let oracle = std::fs::read(dir.join("state-binding.hunk")).unwrap();
    assert!(!oracle.is_empty());
    let registry = engine::build_default_asm_registry();
    let build = crate::native_package_build::build_native_packages(
        &registry,
        &dir.join("native"),
        &root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &crate::native_package_build::EmbedSelection::Targets(vec!["68020".into()]),
    )
    .unwrap();
    let image = crate::fs_uae_smoke::compact_cli_input::assemble_cli(&root, &build);
    let files = [OpforgeNativeCliGuestFile {
        relative_path: "entry.asm",
        bytes: source.as_bytes(),
    }];
    let case = OpforgeNativeCliParityCase {
        name: "actual-bs23-state-package-bindings",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("--cpu 68020 Work:entry.asm"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/state-binding.hunk",
            rust_oracle: &oracle,
        },
    };
    let result = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh actual state/package binding probe completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    assert!(runs[0].success);
    eprintln!(
        "ACTUAL_STATE_PACKAGE_BINDING_PROBE source_bytes={} oracle_bytes={} native_seconds={:?}",
        source.len(),
        oracle.len(),
        runs[0].start_to_done_host_seconds
    );
}
