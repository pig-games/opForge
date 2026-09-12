use super::{
    deterministic_fuzz_lines, parity_registry, register_motorola68000_family_stack,
    tokenize_with_vm_program, tokenizer_edge_case_lines, HierarchyExecutionModel,
    RuntimeTokenizerVmProgram,
};
use types::vm_work;

type TokenResult = Result<Vec<super::PortableToken>, String>;

fn normalized<T>(result: Result<T, impl std::fmt::Display>) -> Result<T, String> {
    result.map_err(|error| error.to_string())
}

fn compare_paths(
    model: &HierarchyExecutionModel,
    cpu_id: &str,
    line: &str,
    line_num: u32,
    program: &RuntimeTokenizerVmProgram,
    policy: &crate::runtime_model_types::RuntimeTokenPolicy,
) -> (TokenResult, TokenResult) {
    let generic = normalized(tokenize_with_vm_program(
        model, cpu_id, line, line_num, program,
    ));
    let fast =
        normalized(model.tokenize_with_default_dispatch_core(line, line_num, policy, program));
    (generic, fast)
}

#[test]
fn tokenizer_fast_path_matches_generic_vm_across_families_and_edge_inputs() {
    let mut registry = parity_registry();
    register_motorola68000_family_stack(&mut registry);
    let model = HierarchyExecutionModel::from_registry(&registry).expect("execution model build");
    let mut corpus = tokenizer_edge_case_lines();
    corpus.extend([
        "move.b d0,d1".to_string(),
        "λabel: .byte \"é\"".to_string(),
        "LONG_IDENTIFIER_".repeat(256),
    ]);
    corpus.extend(deterministic_fuzz_lines(0xFA57_2026, 128, 48));

    for cpu_id in ["m6502", "z80", "m68000"] {
        let route = model
            .resolve_tokenizer_vm_route_for_assembler(cpu_id, None)
            .expect("resolve tokenizer route");
        assert!(
            route.use_default_dispatch_fast_path,
            "{cpu_id} route did not select default-dispatch fast path"
        );

        for (index, line) in corpus.iter().enumerate() {
            let line_num = index as u32 + 1;
            let (generic, fast) = compare_paths(
                &model,
                cpu_id,
                line,
                line_num,
                &route.tokenizer_vm_program,
                &route.token_policy,
            );
            assert_eq!(
                fast, generic,
                "fast/generic tokenizer mismatch for {cpu_id} corpus index {index}, line {line:?}"
            );
        }
    }
}

#[test]
fn tokenizer_fast_path_preserves_reduced_tokenizer_budgets() {
    let registry = parity_registry();
    let model = HierarchyExecutionModel::from_registry(&registry).expect("execution model build");
    let route = model
        .resolve_tokenizer_vm_route_for_assembler("m6502", None)
        .expect("resolve tokenizer route");
    assert!(route.use_default_dispatch_fast_path);

    for (line, change) in [("LDA #$42", 0), ("LONG_IDENTIFIER", 1), ("LDA", 2)] {
        let mut program = route.tokenizer_vm_program.clone();
        match change {
            0 => program.limits.max_tokens_per_line = 1,
            1 => program.limits.max_lexeme_bytes = 3,
            _ => program.limits.max_steps_per_line = 3,
        }
        let (generic, fast) =
            compare_paths(&model, "m6502", line, 7, &program, &route.token_policy);
        assert_eq!(
            fast, generic,
            "fast/generic tokenizer budget mismatch (change {change}) for {line:?}"
        );
        assert!(fast.is_err(), "reduced budget should reject {line:?}");
    }
}

#[test]
fn tokenizer_fast_path_records_logical_steps_alongside_generic_opcode_count() {
    let registry = parity_registry();
    let model = HierarchyExecutionModel::from_registry(&registry).expect("execution model build");
    let route = model
        .resolve_tokenizer_vm_route_for_assembler("m6502", None)
        .expect("resolve tokenizer route");
    assert!(route.use_default_dispatch_fast_path);

    let _session = vm_work::install();
    {
        let _phase = vm_work::phase("generic");
        let result = tokenize_with_vm_program(
            &model,
            "m6502",
            "LDA #$42, X",
            1,
            &route.tokenizer_vm_program,
        );
        assert!(result.is_ok(), "generic tokenization failed: {result:?}");
    }
    {
        let _phase = vm_work::phase("fast");
        let result = model.tokenize_with_default_dispatch_core(
            "LDA #$42, X",
            1,
            &route.token_policy,
            &route.tokenizer_vm_program,
        );
        assert!(result.is_ok(), "fast tokenization failed: {result:?}");
    }

    let report = vm_work::snapshot().expect("work report");
    assert_eq!(report["overflow"], false, "work report overflowed");
    let programs = report["programs"].as_array().expect("program list");
    let rows = report["rows"].as_array().expect("work rows");
    let generic_opcodes: u64 = rows
        .iter()
        .filter(|row| row["phase"] == "generic")
        .filter(|row| programs[row["program"].as_u64().unwrap() as usize]["engine"] == "tokenizer")
        .map(|row| row["steps"].as_u64().unwrap())
        .sum();
    let fast_logical_steps: u64 = report["events"]
        .as_array()
        .expect("event list")
        .iter()
        .filter(|event| event["phase"] == "fast")
        .filter(|event| event["label"] == "tokenizer.fast.logical_budget_steps")
        .map(|event| event["count"].as_u64().unwrap())
        .sum();
    assert!(generic_opcodes > 0, "generic opcode count was not recorded");
    assert!(
        fast_logical_steps > 0,
        "fast logical step count was not recorded"
    );
    assert_eq!(
        fast_logical_steps, generic_opcodes,
        "successful fast logical work must match generic tokenizer dispatch"
    );
}
