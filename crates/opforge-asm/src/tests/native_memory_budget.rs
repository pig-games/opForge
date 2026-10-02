//! Caller-owned raw-block budgets, independent of source/package semantics.
use super::*;

const HARNESS: &str =
    "native/motorola68000/amigaos/test-harnesses/experimental/binary_memory_budget_harness.asm";

#[test]
fn native_memory_budget_harness_assembles() {
    let output = create_temp_dir("native-memory-budget");
    let result = assemble_example_with_base_and_defines(
        &workspace_root().join(HARNESS),
        &output,
        "binary_memory_budget_harness",
        false,
        &[],
    );
    let diagnostics =
        fs::read_to_string(output.join("binary_memory_budget_harness.err")).unwrap_or_default();
    fs::remove_dir_all(output).expect("remove allocator assembly scratch");
    result.unwrap_or_else(|error| panic!("assemble allocator probe: {error}\n{diagnostics}"));
}

#[test]
#[ignore = "requires configured FS-UAE; fresh allocator ownership and ABI proof"]
fn native_memory_budget_fs_uae() {
    let outcome = crate::fs_uae_smoke::run_native_memory_budget_from_env(&workspace_root())
        .expect("fresh allocator budget proof");
    let crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required");
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(
        run.success && run.protocol_completed && run.exit_code == Some(0),
        "allocator ownership or public ABI assertion failed: {}\n{}",
        run.stdout,
        run.stderr
    );
}
