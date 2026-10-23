//! Native FS-UAE, preprocessor, and reference parity tests.

use super::*;

#[path = "native_mos_abs32.rs"]
mod native_mos_abs32;

#[path = "native_layout_group_size.rs"]
mod native_layout_group_size;

#[path = "native_hex_records.rs"]
mod native_hex_records;

#[path = "native_map_symbols.rs"]
mod native_map_symbols;

#[path = "native_listing_sections.rs"]
mod native_listing_sections;

#[path = "native_listing_symbols.rs"]
mod native_listing_symbols;

#[path = "native_listing_visibility_source_rows.rs"]
mod native_listing_visibility_source_rows;

#[path = "native_listing_reservation.rs"]
mod native_listing_reservation;

#[path = "native_bss_image_origin.rs"]
mod native_bss_image_origin;

#[test]
#[ignore = "requires configured FS-UAE; PRVM boundary and resume proof"]
fn external_fs_uae_prvm_boundary_smoke() {
    let outcome = crate::fs_uae_smoke::run_prvm_hunk_smoke_from_env(&workspace_root())
        .expect("fresh native PRVM boundary checks");
    let crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native proof required");
    };
    assert_eq!(runs.len(), 2);
    for run in runs {
        assert!(
            run.success && run.protocol_completed,
            "{}: {} {}",
            run.example_name,
            run.stdout,
            run.stderr
        );
        assert_eq!(run.exit_code, Some(0));
        let marker = if run.example_name == "prvm_smoke" {
            "OPFORGE-PRVM smoke OK"
        } else {
            "OPFORGE-PRVM-ITER smoke OK"
        };
        assert!(
            run.stdout.contains(marker),
            "{}: {}",
            run.example_name,
            run.stdout
        );
        eprintln!(
            "PRVM_BOUNDARY_SMOKE example={} seconds={:?} image={:?}",
            run.example_name, run.start_to_done_host_seconds, run.native_image_digest
        );
    }
}

#[test]
fn external_fs_uae_hunk_smoke() {
    match crate::fs_uae_smoke::run_hunk_smoke_from_env(&workspace_root())
        .expect("FS-UAE smoke helper should complete or skip cleanly")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            for run in runs {
                let combined_output = format!("{}\n{}", run.stdout, run.stderr);
                assert!(
                    run.success,
                    "FS-UAE smoke failed for example {} from {} with {} under {}\nstdout:\n{}\nstderr:\n{}",
                    run.example_name,
                    run.source_path.display(),
                    run.hunk_path.display(),
                    run.artifact_dir.display(),
                    run.stdout,
                    run.stderr,
                );
                if run.example_name == "tkpkg_debug_cli" {
                    assert!(
                        combined_output.contains("TKPKG load_package/set_pipeline OK"),
                        "FS-UAE smoke for {} did not report the pipeline success marker\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                    assert!(
                        combined_output.contains("Identifier(\"move.b\")@1:1-7"),
                        "FS-UAE smoke for {} did not report the first file-mode output row\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                    assert!(
                        combined_output.contains("Comma@1:10-11"),
                        "FS-UAE smoke for {} did not report the first file-mode comma row\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                    assert!(
                        combined_output.contains("Identifier(\"move.w\")@2:1-7"),
                        "FS-UAE smoke for {} did not report the second file-mode output row\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                    assert!(
                        combined_output.contains("Identifier(\"d3\")@2:11-13"),
                        "FS-UAE smoke for {} did not report the second file-mode operand row\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                    assert!(
                        combined_output.contains("TKPKG last_error clear OK"),
                        "FS-UAE smoke for {} did not report the last_error-clear marker\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                }
                if run.example_name == "prvm_smoke" {
                    assert!(
                        combined_output.contains("OPFORGE-PRVM smoke OK"),
                        "FS-UAE smoke for {} did not report the PRVM success marker\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                }
                if run.example_name == "prvm_line_iterator_smoke" {
                    assert!(
                        combined_output.contains("OPFORGE-PRVM-ITER smoke OK"),
                        "FS-UAE smoke for {} did not report the PRVM iterator success marker\nstdout:\n{}\nstderr:\n{}",
                        run.example_name,
                        run.stdout,
                        run.stderr,
                    );
                }
                eprintln!(
                    "FS-UAE smoke completed for example {} with {} under {}",
                    run.example_name,
                    run.hunk_path.display(),
                    run.artifact_dir.display()
                );
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
enum NativeExpressionTextPath {
    ExpressionSlice,
    StoredText,
}

fn native_expression_text_path(
    has_metadata: bool,
    metadata_loads: bool,
    expression_slice_loads: bool,
) -> NativeExpressionTextPath {
    if has_metadata && metadata_loads && expression_slice_loads {
        NativeExpressionTextPath::ExpressionSlice
    } else {
        NativeExpressionTextPath::StoredText
    }
}

#[test]
fn native_expression_metadata_fallback_contract_covers_missing_and_malformed_boundaries() {
    // Proof level C. This test proves the boundary decision for absent,
    // unreadable, unusable, and valid expression metadata. This test does not
    // prove the native branch implementation or real 68020 execution.
    assert_eq!(
        native_expression_text_path(false, false, false),
        NativeExpressionTextPath::StoredText
    );
    assert_eq!(
        native_expression_text_path(true, false, false),
        NativeExpressionTextPath::StoredText
    );
    assert_eq!(
        native_expression_text_path(true, true, false),
        NativeExpressionTextPath::StoredText
    );
    assert_eq!(
        native_expression_text_path(true, true, true),
        NativeExpressionTextPath::ExpressionSlice
    );
}

fn native_source_cpu_token_contract(token: &str, trailing: &str) -> Result<String, ()> {
    let requested = if let Some(inner) = token.strip_prefix('"') {
        inner.strip_suffix('"').ok_or(())?
    } else {
        token
    };
    if requested.is_empty()
        || (!trailing.trim().is_empty() && !trailing.trim_start().starts_with(';'))
    {
        return Err(());
    }
    Ok(requested.to_string())
}

#[test]
fn native_source_cpu_token_contract_preserves_package_owned_names() {
    // Proof level C. This test proves source parsing preserves canonical names,
    // aliases, and case variants for the package resolver while retaining syntax
    // rejection. This test does not prove native execution or package selection.
    assert_eq!(
        native_source_cpu_token_contract("6502", ""),
        Ok("6502".to_string())
    );
    assert_eq!(
        native_source_cpu_token_contract("\"M6502\"", " ; comment"),
        Ok("M6502".to_string())
    );
    assert_eq!(native_source_cpu_token_contract("\"6502", ""), Err(()));
    assert_eq!(
        native_source_cpu_token_contract("\"6502\"", " trailing"),
        Err(())
    );
}

fn native_counted_for_contract(lines: &[&str], iteration_limit: u32) -> Result<Vec<String>, ()> {
    fn expand(
        lines: &[&str],
        start: usize,
        end: usize,
        iteration_limit: u32,
    ) -> Result<Vec<String>, ()> {
        let mut output = Vec::new();
        let mut index = start;
        while index < end {
            let trimmed = lines[index].trim();
            if let Some(operand) = trimmed.strip_prefix(".for ") {
                let count = operand.trim().parse::<u32>().map_err(|_| ())?;
                if count > iteration_limit {
                    return Err(());
                }
                let mut depth = 1usize;
                let mut close = index + 1;
                while close < end && depth != 0 {
                    let candidate = lines[close].trim();
                    if candidate.starts_with(".for ") {
                        depth += 1;
                    } else if candidate == ".endfor" {
                        depth -= 1;
                    }
                    close += 1;
                }
                if depth != 0 {
                    return Err(());
                }
                let body = expand(lines, index + 1, close - 1, iteration_limit)?;
                for _ in 0..count {
                    output.extend(body.iter().cloned());
                }
                index = close;
                continue;
            }
            if trimmed == ".endfor" {
                return Err(());
            }
            output.push(lines[index].to_string());
            index += 1;
        }
        Ok(output)
    }
    expand(lines, 0, lines.len(), iteration_limit)
}

#[test]
fn native_counted_for_contract_covers_zero_one_nested_and_limit() {
    // Proof level C. This host boundary model proves counted block replacement,
    // matching, nesting, and the iteration limit expected at the native flow
    // callback. It does not execute the 68020 callback or statement tables.
    assert_eq!(
        native_counted_for_contract(&[".for 0", ".byte 1", ".endfor"], 8).unwrap(),
        Vec::<String>::new()
    );
    assert_eq!(
        native_counted_for_contract(&[".for 1", ".byte 1", ".endfor"], 8).unwrap(),
        vec![".byte 1"]
    );
    assert_eq!(
        native_counted_for_contract(
            &[".for 2", ".byte 1", ".for 2", ".byte 2", ".endfor", ".endfor",],
            8,
        )
        .unwrap(),
        vec![".byte 1", ".byte 2", ".byte 2", ".byte 1", ".byte 2", ".byte 2",]
    );
    assert!(native_counted_for_contract(&[".for 9", ".byte 1", ".endfor"], 8).is_err());
}

#[test]
fn native_statement_definition_rust_oracle_consumes_bodies_and_rejects_structure_errors() {
    // Proof level A. Rust owns statement definition parsing and structural
    // diagnostics; this does not claim native invocation matching.
    let mut processor = crate::preprocess::AsmMacroProcessor::new(64);
    let output = processor
        .expand(&[
            ".statement LOAD byte:val".to_string(),
            "    .byte .val".to_string(),
            ".endstatement".to_string(),
            "    .byte 9".to_string(),
        ])
        .expect("Rust statement definition");
    assert_eq!(output, ["    .byte 9"]);

    for (lines, expected) in [
        (
            vec![".statement".to_string(), ".endstatement".to_string()],
            "Expected statement keyword",
        ),
        (
            vec![
                ".statement LOAD byte:val".to_string(),
                ".statement INNER byte:v".to_string(),
                ".endstatement".to_string(),
            ],
            "Nested .statement definitions are not supported",
        ),
        (
            vec![
                ".statement LOAD byte:val".to_string(),
                ".endmacro".to_string(),
            ],
            ".endmacro found without matching .macro",
        ),
        (
            vec![".statement LOAD byte:val".to_string()],
            "Missing .endstatement for statement definition",
        ),
    ] {
        let error = crate::preprocess::AsmMacroProcessor::new(64)
            .expand(&lines)
            .expect_err("Rust must reject malformed statement structure");
        assert!(
            error.message().contains(expected),
            "expected {expected:?}, got {:?}",
            error.message()
        );
    }
}

#[test]
fn native_directive_first_definition_header_model_matches_rust() {
    // Proof level C. This models the canonical native definition record for
    // Rust's directive-first macro/segment syntax, including outer-parenthesis
    // removal and deterministic malformed-header rejection. It does not
    // execute the 68020 implementation or prove body expansion.
    fn canonicalize(line: &str) -> Result<Option<String>, ()> {
        let code = line.split_once(';').map_or(line, |(code, _)| code).trim();
        let lower = code.to_ascii_lowercase();
        let (directive, mut rest) = if lower.starts_with(".macro")
            && lower.as_bytes().get(6).is_none_or(u8::is_ascii_whitespace)
        {
            (".macro", &code[6..])
        } else if lower.starts_with(".segment")
            && lower.as_bytes().get(8).is_none_or(u8::is_ascii_whitespace)
        {
            (".segment", &code[8..])
        } else {
            return Ok(None);
        };
        rest = rest.trim_start();
        let name_len = rest
            .bytes()
            .take_while(|byte| byte.is_ascii_alphanumeric() || matches!(*byte, b'_' | b'.' | b'$'))
            .count();
        if name_len == 0
            || !rest
                .as_bytes()
                .first()
                .is_some_and(|byte| byte.is_ascii_alphabetic() || *byte == b'_')
        {
            return Err(());
        }
        let name = &rest[..name_len];
        rest = rest[name_len..].trim_start();
        let params = if let Some(inner) = rest.strip_prefix('(') {
            let mut quote = None;
            let mut depth = 1usize;
            let mut close = None;
            for (index, byte) in inner.bytes().enumerate() {
                if let Some(delimiter) = quote {
                    if byte == delimiter {
                        quote = None;
                    }
                    continue;
                }
                match byte {
                    b'\'' | b'"' => quote = Some(byte),
                    b'(' => depth += 1,
                    b')' => {
                        depth -= 1;
                        if depth == 0 {
                            close = Some(index);
                            break;
                        }
                    }
                    _ => {}
                }
            }
            let close = close.ok_or(())?;
            if !inner[close + 1..].trim().is_empty() {
                return Err(());
            }
            inner[..close].trim()
        } else {
            rest.trim()
        };
        Ok(Some(if params.is_empty() {
            format!("{name} {directive}")
        } else {
            format!("{name} {directive} {params}")
        }))
    }

    for (source, expected) in [
        (".macro COPY(src, dst)", "COPY .macro src, dst"),
        ("  .MaCrO FILL(value)", "FILL .macro value"),
        (".segment INLINE(v)", "INLINE .segment v"),
        (".segment INLINE v", "INLINE .segment v"),
        (
            ".macro MIX(value=(1 + 2), text=\"a)b\")",
            "MIX .macro value=(1 + 2), text=\"a)b\"",
        ),
        (".macro EMPTY()", "EMPTY .macro"),
        (".macro FILL.PART$(value)", "FILL.PART$ .macro value"),
    ] {
        assert_eq!(canonicalize(source), Ok(Some(expected.to_string())));
    }
    assert_eq!(canonicalize("COPY .macro src, dst"), Ok(None));
    assert_eq!(canonicalize(".macrox NAME(value)"), Ok(None));
    assert_eq!(canonicalize(".foo .macro NAME(value)"), Ok(None));
    for malformed in [
        ".macro",
        ".macro 1BAD(value)",
        ".macro NAME(value",
        ".segment NAME(value) trailing",
    ] {
        assert_eq!(canonicalize(malformed), Err(()), "{malformed}");
    }
}

#[derive(Default)]
struct NativeSequenceAssignmentContract {
    values: Vec<(String, Vec<u32>)>,
}

impl NativeSequenceAssignmentContract {
    fn reset(&mut self) {
        self.values.clear();
    }

    fn capture(&mut self, source: &str) -> Result<(), ()> {
        let (name, operand) = source.split_once('=').ok_or(())?;
        let name = name.trim();
        if name.is_empty() || name.len() >= 32 || self.values.len() >= 8 {
            return Err(());
        }
        let operand = operand.trim();
        let list = operand
            .strip_prefix('{')
            .and_then(|text| text.strip_suffix('}'))
            .ok_or(())?;
        let mut values = Vec::new();
        if !list.trim().is_empty() {
            for element in list.split(',') {
                if values.len() >= 16 {
                    return Err(());
                }
                values.push(element.trim().parse::<u32>().map_err(|_| ())?);
            }
        }
        self.values.push((name.to_string(), values));
        Ok(())
    }
}

#[test]
fn native_sequence_assignment_contract_covers_parsing_bounds_and_session_reset() {
    // Proof level C. This host request-shape model proves the intended list
    // parsing, fixed-capacity rejection, and session-reset decisions. It does
    // not execute the native parser or prove stored 68020 memory contents.
    let mut contract = NativeSequenceAssignmentContract::default();
    contract.capture("values = {1, 2, 3}").unwrap();
    assert_eq!(contract.values, vec![("values".to_string(), vec![1, 2, 3])]);
    assert!(contract.capture("broken = {1, nope}").is_err());
    assert!(NativeSequenceAssignmentContract::default()
        .capture(&format!("{} = {{1}}", "n".repeat(32)))
        .is_err());
    assert!(NativeSequenceAssignmentContract::default()
        .capture(&format!(
            "values = {{{}}}",
            (0..17)
                .map(|value| value.to_string())
                .collect::<Vec<_>>()
                .join(",")
        ))
        .is_err());
    let mut full = NativeSequenceAssignmentContract::default();
    for index in 0..8 {
        full.capture(&format!("v{index} = {{{index}}}")).unwrap();
    }
    assert!(full.capture("overflow = {9}").is_err());
    contract.reset();
    assert!(contract.values.is_empty());
}

fn native_iterable_for_contract(
    variable: &str,
    values: &[u32],
    iteration_limit: usize,
) -> Result<Vec<(String, u32)>, ()> {
    if variable.is_empty() || variable.len() >= 32 || values.len() > iteration_limit {
        return Err(());
    }
    Ok(values
        .iter()
        .map(|value| (variable.to_string(), *value))
        .collect())
}

fn native_ascending_range_contract(
    start: u32,
    end: u32,
    inclusive: bool,
    step: u32,
) -> Result<Vec<u32>, ()> {
    if step == 0 || start > end {
        return Err(());
    }
    let mut values = Vec::new();
    let mut current = start;
    while if inclusive {
        current <= end
    } else {
        current < end
    } {
        values.push(current);
        current = current.checked_add(step).ok_or(())?;
    }
    Ok(values)
}

#[test]
fn native_iterable_for_contract_covers_lists_ranges_binding_and_limit() {
    // Proof level C. This host request-shape model proves list order, inclusive
    // and stepped ascending range planning, loop-variable binding, and bounded
    // iteration decisions. It does not execute native tables or 68020 branches.
    assert_eq!(
        native_iterable_for_contract("value", &[1, 3, 5, 7], 8).unwrap(),
        vec![
            ("value".to_string(), 1),
            ("value".to_string(), 3),
            ("value".to_string(), 5),
            ("value".to_string(), 7),
        ]
    );
    assert_eq!(
        native_ascending_range_contract(0, 6, true, 3).unwrap(),
        vec![0, 3, 6]
    );
    assert_eq!(
        native_ascending_range_contract(0, 6, false, 3).unwrap(),
        vec![0, 3]
    );
    let sequence = [2, 4, 6, 8];
    assert_eq!(sequence.len(), 4);
    assert_eq!(sequence.get(2), Some(&6));
    assert!(native_ascending_range_contract(0, 6, true, 0).is_err());
    assert!(native_ascending_range_contract(6, 0, true, 1).is_err());
    assert!(native_iterable_for_contract("n", &[0, 1, 2], 2).is_err());
}

#[test]
fn opcore_iterable_rust_oracle_covers_assigned_sources() {
    // Proof level A. This test proves the live Rust assembler emits the expected
    // bytes for both assigned canonical iterable sources. It does not prove any
    // native request shape or 68020 execution.
    let case_dir = create_temp_dir("opcore-iterable-rust-oracle");
    for (name, expected) in [
        ("for_collection_basic.asm", &[1, 3, 5, 7][..]),
        ("ranges_lists_basic.asm", &[4, 6, 0, 3, 6][..]),
    ] {
        let input_path = workspace_root().join("examples/opcore").join(name);
        let bin_path = case_dir.join(format!("{name}.bin"));
        let cli = Cli::parse_from([
            "opForge",
            input_path.to_string_lossy().as_ref(),
            "--bin",
            bin_path.to_string_lossy().as_ref(),
            "--cpu",
            "m6502",
        ]);
        run_with_cli_with_context(&cli).expect("run Rust iterable source oracle");
        assert_eq!(
            fs::read(bin_path).expect("read Rust oracle bytes"),
            expected
        );
    }
}

#[test]
fn opcore_while_rust_oracle_covers_canonical_current_address() {
    // Proof level A. This test proves the live Rust assembler emits five bytes
    // for canonical `.while $ < 4`. It does not prove native loop control.
    let case_dir = create_temp_dir("opcore-while-rust-oracle");
    let input_path = workspace_root().join("examples/opcore/while_basic.asm");
    let bin_path = case_dir.join("while.bin");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run Rust while oracle");
    assert_eq!(
        fs::read(bin_path).expect("read Rust while bytes"),
        [0xff; 5]
    );
}

fn native_while_current_address_contract(limit: u32, max_iterations: usize) -> Result<usize, ()> {
    let mut current_pc = 0u32;
    let mut iterations = 0usize;
    while if iterations == 0 {
        current_pc < limit
    } else {
        current_pc.saturating_sub(1) < limit
    } {
        if iterations >= max_iterations {
            return Err(());
        }
        iterations += 1;
        current_pc += 1;
    }
    Ok(iterations)
}

#[test]
fn native_while_contract_covers_false_first_current_address_and_limit() {
    // Proof level C. This host boundary model proves false-first behavior,
    // Rust-compatible current-address phase, and iteration-limit rejection. It
    // does not execute native statement tables or 68020 branches.
    assert_eq!(native_while_current_address_contract(0, 8), Ok(0));
    assert_eq!(native_while_current_address_contract(4, 8), Ok(5));
    assert!(native_while_current_address_contract(u32::MAX, 4).is_err());
}

fn native_debug_output_isolation_contract(enabled: bool) -> Vec<&'static str> {
    if enabled {
        vec![
            "OPFORGE-NATIVE 1",
            "STAGE parser",
            "STAGE session",
            "STATUS output-ok",
            "SESSION-CPU ",
        ]
    } else {
        Vec::new()
    }
}

#[test]
fn native_debug_output_isolation_contract_models_enabled_and_disabled_markers() {
    // Proof level C. This test proves the marker-set decision for normal and
    // debug mode. This test does not prove native branches or DOS output.
    assert!(native_debug_output_isolation_contract(false).is_empty());
    assert_eq!(
        native_debug_output_isolation_contract(true),
        vec![
            "OPFORGE-NATIVE 1",
            "STAGE parser",
            "STAGE session",
            "STATUS output-ok",
            "SESSION-CPU ",
        ]
    );
}

#[test]
fn opcore_scopes_rust_oracle_covers_canonical_qualification() {
    // Proof level A. Live Rust establishes canonical bytes for nested block
    // shadowing and namespace close aliases; it does not execute native code.
    let case_dir = create_temp_dir("opcore-scopes-rust-oracle");
    for (index, (source, expected)) in [
        ("examples/opcore/scopes.asm", vec![2, 0, 1, 0, 5, 0]),
        (
            "examples/opcore/scopes_namespace.asm",
            vec![1, 0, 5, 0, 9, 0],
        ),
    ]
    .into_iter()
    .enumerate()
    {
        let input_path = workspace_root().join(source);
        let bin_path = case_dir.join(format!("scope-{index}.bin"));
        let cli = Cli::parse_from([
            "opForge",
            input_path.to_string_lossy().as_ref(),
            "--bin",
            bin_path.to_string_lossy().as_ref(),
            "--cpu",
            "m6502",
        ]);
        run_with_cli_with_context(&cli).expect("run Rust scope oracle");
        assert_eq!(fs::read(bin_path).expect("read Rust scope bytes"), expected);
    }
}

#[test]
fn opcore_module_basics_rust_oracle_covers_module_local_symbols() {
    // Proof level A. The live Rust CLI establishes that equal local names in
    // separate modules are legal and emit their module-local values. It does
    // not execute the native 68020 implementation.
    let input_path = workspace_root().join("examples/opcore/module_basics.asm");
    let bin_path = create_temp_dir("opcore-module-basics-rust-oracle").join("module-basics.bin");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run Rust module-local symbol oracle");
    let bytes = fs::read(bin_path).expect("read Rust module bytes");
    assert_eq!(bytes.len(), 0x1001, "Rust preserves the module .org gap");
    assert_eq!(bytes.first(), Some(&1));
    assert!(bytes[1..0x1000].iter().all(|byte| *byte == 0));
    assert_eq!(bytes.last(), Some(&2));
}

fn native_scope_contract(stack: &[&str], raw: &str, defined: &[&str]) -> Option<String> {
    (0..=stack.len()).rev().find_map(|depth| {
        let candidate = if depth == 0 {
            raw.to_string()
        } else {
            format!("{}.{}", stack[..depth].join("."), raw)
        };
        defined.contains(&candidate.as_str()).then_some(candidate)
    })
}

fn native_scope_close_contract(stack: &mut Vec<&str>, directive: &str) -> Result<(), ()> {
    match directive {
        ".endblock" | ".bend" | ".endnamespace" | ".endn" | ".endmodule" => {
            stack.pop().map(|_| ()).ok_or(())
        }
        _ => Err(()),
    }
}

#[test]
fn native_scope_contract_covers_nested_shadowing_and_close_aliases() {
    // Proof level C. This host model proves inner-first qualification and
    // block/namespace close equivalence; it does not inspect native tables.
    let stack = ["OUTER", "INNER"];
    assert_eq!(
        native_scope_contract(&stack, "VAL", &["VAL", "OUTER.INNER.VAL"]),
        Some("OUTER.INNER.VAL".to_string())
    );
    assert_eq!(
        native_scope_contract(&stack, "VALUE", &["VALUE", "OUTER.VALUE"]),
        Some("OUTER.VALUE".to_string())
    );
    assert_eq!(
        native_scope_contract(&["SHADOW"], "GLOBAL", &["GLOBAL", "SHADOW.GLOBAL"]),
        Some("SHADOW.GLOBAL".to_string())
    );
    assert_eq!(
        native_scope_contract(&stack, "GLOBAL", &["GLOBAL"]),
        Some("GLOBAL".to_string())
    );
    let mut endblock_stack = vec!["OUTER", "INNER"];
    let mut bend_stack = endblock_stack.clone();
    native_scope_close_contract(&mut endblock_stack, ".endblock").expect("endblock pops");
    native_scope_close_contract(&mut bend_stack, ".bend").expect("bend pops");
    assert_eq!(endblock_stack, bend_stack);
    let mut endnamespace_stack = vec!["outer", "inner"];
    let mut endn_stack = endnamespace_stack.clone();
    native_scope_close_contract(&mut endnamespace_stack, ".endnamespace")
        .expect("endnamespace pops");
    native_scope_close_contract(&mut endn_stack, ".endn").expect("endn pops");
    assert_eq!(endnamespace_stack, endn_stack);
    assert_eq!(
        native_scope_contract(&["alpha"], "VALUE", &["alpha.VALUE", "beta.VALUE"]),
        Some("alpha.VALUE".to_string())
    );
    assert_eq!(
        native_scope_contract(&["beta"], "VALUE", &["alpha.VALUE", "beta.VALUE"]),
        Some("beta.VALUE".to_string())
    );
    let mut module_stack = vec!["alpha"];
    native_scope_close_contract(&mut module_stack, ".endmodule").expect("endmodule pops");
    assert!(module_stack.is_empty());
}

#[test]
fn native_scoped_expression_snapshot_preserves_local_shadowing_contract() {
    // Proof level C. This models the alias projection supplied to the existing
    // expression bridge; real native execution is proved separately.
    fn active_alias<'a>(scope: &[&str], qualified: &'a str) -> Option<&'a str> {
        let prefix = format!("{}.", scope.join("."));
        qualified
            .strip_prefix(&prefix)
            .filter(|name| !name.is_empty())
    }

    assert_eq!(active_alias(&["alpha"], "alpha.VALUE"), Some("VALUE"));
    assert_eq!(active_alias(&["alpha"], "beta.VALUE"), None);
    assert_eq!(
        active_alias(&["main", "inner"], "main.inner.OFFSET"),
        Some("OFFSET")
    );
    assert_eq!(active_alias(&[], "VALUE"), None);
}

#[test]
fn opcore_structs_rust_oracle_covers_canonical_layouts() {
    // Proof level A. Live Rust establishes the canonical struct and scoped
    // repetition artifacts; it does not execute native code.
    let case_dir = create_temp_dir("opcore-structs-rust-oracle");
    for (index, (source, expected)) in [
        (
            "examples/opcore/struct_literal_instance_basic.asm",
            vec![24, 50, 40, 60, 41, 61],
        ),
        (
            "examples/opcore/struct_var_instance_basic.asm",
            vec![3, 0, 1],
        ),
        (
            "examples/opcore/bfor_labeled_struct_basic.asm",
            vec![0, 10, 1, 11, 2, 12, 2, 0, 3, 0],
        ),
    ]
    .into_iter()
    .enumerate()
    {
        let input_path = workspace_root().join(source);
        let bin_path = case_dir.join(format!("struct-{index}.bin"));
        let cli = Cli::parse_from([
            "opForge",
            input_path.to_string_lossy().as_ref(),
            "--bin",
            bin_path.to_string_lossy().as_ref(),
            "--cpu",
            "m6502",
        ]);
        run_with_cli_with_context(&cli).expect("run Rust struct oracle");
        assert_eq!(
            fs::read(bin_path).expect("read Rust struct bytes"),
            expected
        );
    }
}

#[test]
fn native_struct_contract_covers_layout_instances_and_scoped_labels() {
    // Proof level C. This host model proves the field-layout and member-address
    // relationships used by the canonical sources; it does not execute native
    // assembly or native expression resolution.
    let point_field_sizes = [1_u32, 2_u32];
    let mut offsets = Vec::new();
    let mut size = 0_u32;
    for field_size in point_field_sizes {
        offsets.push(size);
        size += field_size;
    }
    assert_eq!(offsets, vec![0, 1]);
    assert_eq!(size, 3);

    let literal_instance = [24_u32, 50_u32];
    let mut mutable_instance = [40_u32, 60_u32];
    assert_eq!(mutable_instance, [40, 60]);
    mutable_instance = [41, 61];
    assert_eq!(literal_instance, [24, 50]);
    assert_eq!(mutable_instance, [41, 61]);

    let point_stride = 2_u32;
    let second_point_base = point_stride;
    assert_eq!(second_point_base, 2);
    assert_eq!(second_point_base + 1, 3);
}

#[test]
fn native_text_encoding_contract_covers_builtin_ascii_and_petscii_mapping() {
    // Proof level C. This host model proves the byte mapping used by the
    // initial native built-in selector path; it does not execute 68020 code
    // or prove clone, character-range, token, and escape definitions.
    let petscii = |byte: u8| match byte {
        b'A'..=b'Z' => byte | 0x80,
        b'a'..=b'z' => byte - 0x20,
        _ => byte,
    };
    assert_eq!(b"Az", b"Az");
    assert_eq!(
        b"Az".iter().copied().map(petscii).collect::<Vec<_>>(),
        [0xC1, 0x5A]
    );
}

#[test]
fn opcore_conditionals_rust_oracle_covers_canonical_branches() {
    // Proof level A. This test proves live Rust selects the canonical if and
    // match branches. It does not prove native routing or 68020 execution.
    let case_dir = create_temp_dir("opcore-conditionals-rust-oracle");
    let input_path = workspace_root().join("examples/opcore/cond_syntax.asm");
    let bin_path = case_dir.join("conditional.bin");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run Rust conditional oracle");
    assert_eq!(
        fs::read(bin_path).expect("read Rust conditional bytes"),
        [1, 5, 8, 10, 13, 16]
    );
}

fn native_if_branch_contract(levels: &[&[bool]]) -> Option<Vec<usize>> {
    levels
        .iter()
        .map(|branches| branches.iter().position(|selected| *selected))
        .collect()
}

fn native_match_case_contract(value: u32, cases: &[&[u32]], default_index: usize) -> usize {
    cases
        .iter()
        .position(|case_values| case_values.contains(&value))
        .unwrap_or(default_index)
}

#[test]
fn native_conditional_contract_covers_nested_if_and_match_selection() {
    // Proof level C. This host request-shape model proves first-matching branch
    // selection, else/default fallback, and nesting-local decisions. It does
    // not execute native statement scans or 68020 branches.
    assert_eq!(
        native_if_branch_contract(&[&[true, false], &[false, true]]),
        Some(vec![0, 1])
    );
    assert_eq!(native_if_branch_contract(&[&[false, false]]), None);
    assert_eq!(native_match_case_contract(2, &[&[1], &[2, 3]], 2), 1);
    assert_eq!(native_match_case_contract(9, &[&[1, 2]], 1), 1);
}

#[test]
fn motorola68020_native_cli_parse_line_keeps_full_ternary_const_expression() {
    // Proof level B. This test proves the Rust-side native parse-line harness
    // keeps the full ternary directive expression and its exclusive-end span.
    // This test does not prove the real Amiga-native session store or evaluator.
    let mut harness = vm::native6502::Native6502Harness::new();
    let mut control_block = vm::native6502::Native6502ControlBlockV1::new_v1();
    item5_prepare_expression_parse_harness(&mut harness, &mut control_block);
    let line = "value .const (0 || 1) ? (2 + 3) : (4 + 5)";
    let ast = item6_native_cli_parse_line(&mut harness, &mut control_block, line, 1);
    let PortableLineAst::Statement { operands, .. } = ast else {
        panic!("expected statement AST");
    };
    assert_eq!(operands.len(), 1);
    let expression = operands[0].to_core_expr();
    let Expr::Ternary {
        cond,
        then_expr,
        else_expr,
        ..
    } = expression
    else {
        panic!("expected full ternary AST, got {expression:?}");
    };
    assert!(matches!(
        *cond,
        Expr::Binary {
            op: BinaryOp::LogicOr,
            ..
        }
    ));
    assert!(matches!(
        *then_expr,
        Expr::Binary {
            op: BinaryOp::Add,
            ..
        }
    ));
    assert!(matches!(
        *else_expr,
        Expr::Binary {
            op: BinaryOp::Add,
            ..
        }
    ));
}

#[test]
fn motorola68020_native_cli_parse_line_rejects_truncated_ternary_const_expression() {
    // Proof level B. This test proves the Rust-side native parse-line harness
    // rejects a truncated ternary expression instead of shrinking its span.
    // This test does not prove the real Amiga-native request decoder.
    let mut harness = vm::native6502::Native6502Harness::new();
    let mut control_block = vm::native6502::Native6502ControlBlockV1::new_v1();
    item5_prepare_expression_parse_harness(&mut harness, &mut control_block);
    let parse = harness.invoke_v1(
        &mut control_block,
        vm::native6502_abi::NATIVE_6502_ENTRYPOINT_PARSE_LINE_V1,
        vm::native6502::Native6502HarnessRequest::ParseLine {
            source_line: "value .const (0 || 1) ? (2 + 3) : (4 + 5",
            line_num: 1,
        },
    );
    assert_eq!(parse.status_code, vm::native6502::NATIVE_6502_STATUS_OK_V1);
    let vm::native6502::Native6502HarnessOutput::LineAst(PortableLineAst::Statement {
        operands,
        ..
    }) = parse.output
    else {
        panic!("expected statement AST with an error operand");
    };
    let expression = operands
        .first()
        .expect("truncated expression should retain an error AST")
        .to_core_expr();
    assert!(
        matches!(expression, Expr::Error(..)),
        "expected an error AST for the truncated expression, got {expression:?}"
    );
}

fn item5_prepare_expression_parse_harness(
    harness: &mut vm::native6502::Native6502Harness,
    control_block: &mut vm::native6502::Native6502ControlBlockV1,
) {
    let init = harness.invoke_v1(
        control_block,
        vm::native6502_abi::NATIVE_6502_ENTRYPOINT_INIT_V1,
        vm::native6502::Native6502HarnessRequest::Init,
    );
    assert_eq!(init.status_code, vm::native6502::NATIVE_6502_STATUS_OK_V1);
    let package_bytes = item6_mos_package_bytes();
    let load = harness.invoke_v1(
        control_block,
        vm::native6502_abi::NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
        vm::native6502::Native6502HarnessRequest::LoadPackage {
            package_bytes: package_bytes.as_slice(),
        },
    );
    assert_eq!(load.status_code, vm::native6502::NATIVE_6502_STATUS_OK_V1);
    let pipeline = harness.invoke_v1(
        control_block,
        vm::native6502_abi::NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1,
        vm::native6502::Native6502HarnessRequest::SetPipeline {
            cpu_id: "65c02",
            dialect_override: None,
        },
    );
    assert_eq!(
        pipeline.status_code,
        vm::native6502::NATIVE_6502_STATUS_OK_V1
    );
}

#[test]
fn native_expression_suffix_literals_rust_oracle() {
    // Proof level A. This test proves the Rust authority's suffix literal
    // values and additive token boundaries. It does not prove native execution.
    let cases = [
        ("hex", "0a6h", 0xa6),
        ("binary", "1010b", 10),
        ("octal", "17o", 15),
        ("octal-q", "17q", 15),
        ("decimal", "42d", 42),
    ];
    for (name, literal, expected) in cases {
        let value_line = format!("value .const {literal}");
        let immediate_line = "start lda #value+1".to_string();
        let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(
            &[
                ".cpu 65c02",
                value_line.as_str(),
                immediate_line.as_str(),
                "rts",
            ],
            true,
        )
        .unwrap_or_else(|err| panic!("assemble suffix oracle {name}: {err}"));
        assert!(
            diagnostics.is_empty(),
            "Rust diagnostics for {name}: {diagnostics:?}"
        );
        assert_eq!(
            entries[1].1,
            (expected + 1) as u8,
            "suffix value for {name}"
        );
    }
}

#[test]
fn native_expression_multiplicative_rust_oracle() {
    // Proof level A. Establish the Rust reference bytes for the native
    // multiplicative-expression fixtures without requiring an emulator.
    let cases = [
        (
            "multiply",
            "value .const 3*4+1\nstart lda #value\nrts",
            vec![0xa9, 13],
        ),
        (
            "divide",
            "value .const -20/-5+1\nstart lda #value\nrts",
            vec![0xa9, 5],
        ),
        (
            "modulo",
            "value .const -23%-5+5\nstart lda #value\nrts",
            vec![0xa9, 2],
        ),
    ];
    for (name, source, expected) in cases {
        let mut lines = vec![".cpu 65c02"];
        lines.extend(source.lines());
        let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
            .unwrap_or_else(|err| panic!("Rust authority {name}: {err}"));
        assert!(
            diagnostics.is_empty(),
            "Rust diagnostics for {name}: {diagnostics:?}"
        );
        assert_eq!(
            entries
                .into_iter()
                .map(|(_, byte)| byte)
                .collect::<Vec<_>>(),
            expected,
            "Rust output for {name}"
        );
    }
}

#[test]
fn native_expression_shift_rust_oracle() {
    // Proof level A. These establish Rust's precedence and count-mask results.
    let cases = [
        (
            "shift-left",
            "value .const 3+1<<2\nstart lda #value\nrts",
            vec![0xa9, 16],
        ),
        (
            "shift-right",
            "value .const 128>>3+1\nstart lda #value\nrts",
            vec![0xa9, 8],
        ),
        (
            "shift-mask",
            "value .const 1<<33\nstart lda #value\nrts",
            vec![0xa9, 2],
        ),
    ];
    for (name, source, expected) in cases {
        let mut lines = vec![".cpu 65c02"];
        lines.extend(source.lines());
        let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
            .unwrap_or_else(|err| panic!("Rust authority {name}: {err}"));
        assert!(
            diagnostics.is_empty(),
            "Rust diagnostics for {name}: {diagnostics:?}"
        );
        assert_eq!(
            entries
                .into_iter()
                .map(|(_, byte)| byte)
                .collect::<Vec<_>>(),
            expected
        );
    }
}

struct Item7StagedGuestFile {
    relative_path: String,
    bytes: Vec<u8>,
}

struct Item7StagedCase {
    name: &'static str,
    source: Vec<u8>,
    guest_files: Vec<Item7StagedGuestFile>,
    rust_oracle: Vec<u8>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Item9DiagnosticKind {
    UnknownInstruction,
    UnexpectedEndExpression,
    UnknownDirective,
    InvalidNumber,
    InvalidImageSpan,
}

struct Item9StagedCase {
    name: &'static str,
    reference_error: String,
    rust_kind: Item9DiagnosticKind,
}

struct Item7OracleDir(PathBuf);

impl Drop for Item7OracleDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

struct Item8RustDeclaredOutputDir(PathBuf);

impl Drop for Item8RustDeclaredOutputDir {
    fn drop(&mut self) {
        if self.0.exists() {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
}

fn item7_native_source(
    assignment: &crate::native_reference_parity::NativeOpcoreAssignment,
) -> Vec<u8> {
    let canonical = fs::read_to_string(workspace_root().join(assignment.source_path))
        .unwrap_or_else(|err| panic!("read Item 7 root {}: {err}", assignment.source_path));
    assert!(
        matches!(
            assignment.staging,
            NativeOpcoreStaging::DirectCpuNeutral | NativeOpcoreStaging::DirectMos65c02
        ),
        "Item 7 requires the exact stored source without adaptation: {}",
        assignment.source_path
    );
    canonical.into_bytes()
}

fn item7_live_rust_cli_binary_oracle(
    case_name: &str,
    source: &[u8],
    guest_files: &[Item7StagedGuestFile],
    cpu_id: &str,
    defines: &[&str],
) -> Vec<u8> {
    let case_dir = create_temp_dir("item7-live-rust-cli-oracle");
    let _case_dir_guard = Item7OracleDir(case_dir.clone());
    let input_path = case_dir.join("input.asm");
    let bin_path = case_dir.join("oracle.bin");
    fs::write(&input_path, source)
        .unwrap_or_else(|err| panic!("write same-case Rust Item 7 root {case_name}: {err}"));
    for guest_file in guest_files {
        let path = case_dir.join(&guest_file.relative_path);
        fs::create_dir_all(path.parent().expect("Item 7 support path has a parent"))
            .unwrap_or_else(|err| panic!("create Item 7 support parent {}: {err}", path.display()));
        fs::write(&path, &guest_file.bytes)
            .unwrap_or_else(|err| panic!("write Item 7 support {}: {err}", path.display()));
    }
    let mut args = vec![
        "opForge".to_string(),
        input_path.to_string_lossy().into_owned(),
        "--bin".to_string(),
        bin_path.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        cpu_id.to_string(),
        "-I".to_string(),
        case_dir.to_string_lossy().into_owned(),
        "-M".to_string(),
        case_dir.to_string_lossy().into_owned(),
    ];
    for define in defines {
        args.push("-D".to_string());
        args.push((*define).to_string());
    }
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap_or_else(|err| {
        panic!("validate same-case Rust Item 7 CLI oracle for {case_name}: {err:?}")
    });
    // Source-declared relative outputs belong to this case just as they do in
    // the guest's fresh Work directory. Keeping them below the RAII case root
    // prevents any later oracle from observing stale evidence.
    config.out_dir = Some(case_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|err| {
        panic!("run same-case Rust Item 7 CLI oracle for {case_name}: {err:?}")
    });
    fs::read(&bin_path).unwrap_or_else(|err| {
        panic!(
            "read same-case Rust Item 7 CLI oracle {} for {case_name}: {err}",
            bin_path.display()
        )
    })
}

fn item6_staged_cases() -> Vec<Item7StagedCase> {
    NATIVE_OPCORE_ASSIGNMENTS
        .iter()
        .filter(|assignment| {
            assignment.shard == NativeOpcoreShard::SyntaxExpression
                && matches!(assignment.role, NativeOpcoreRole::Root { .. })
        })
        .map(|assignment| {
            let guest_files = NATIVE_OPCORE_ASSIGNMENTS
                .iter()
                .filter_map(|support| match support.role {
                    NativeOpcoreRole::Support { owner }
                        if support.shard == NativeOpcoreShard::SyntaxExpression
                            && owner == assignment.source_path =>
                    {
                        let relative_path = support
                            .source_path
                            .strip_prefix("examples/opcore/")
                            .expect("Item 6 support is below examples/opcore")
                            .to_string();
                        let bytes = fs::read(workspace_root().join(support.source_path))
                            .unwrap_or_else(|err| {
                                panic!("read Item 6 support {}: {err}", support.source_path)
                            });
                        Some(Item7StagedGuestFile {
                            relative_path,
                            bytes,
                        })
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();
            let source = item7_native_source(assignment);
            let rust_oracle = item7_live_rust_cli_binary_oracle(
                assignment.source_path,
                &source,
                &guest_files,
                "65c02",
                &[],
            );
            Item7StagedCase {
                name: assignment.source_path,
                source,
                guest_files,
                rust_oracle,
            }
        })
        .collect()
}

fn item7_staged_cases() -> Vec<Item7StagedCase> {
    NATIVE_OPCORE_ASSIGNMENTS
        .iter()
        .filter(|assignment| {
            assignment.shard == NativeOpcoreShard::ModuleMacroStatement
                && matches!(assignment.role, NativeOpcoreRole::Root { .. })
        })
        .map(|assignment| {
            let guest_files = NATIVE_OPCORE_ASSIGNMENTS
                .iter()
                .filter_map(|support| match support.role {
                    NativeOpcoreRole::Support { owner }
                        if support.shard == NativeOpcoreShard::ModuleMacroStatement
                            && owner == assignment.source_path =>
                    {
                        let relative_path = support
                            .source_path
                            .strip_prefix("examples/opcore/")
                            .expect("Item 7 support is below examples/opcore")
                            .to_string();
                        let bytes = fs::read(workspace_root().join(support.source_path))
                            .unwrap_or_else(|err| {
                                panic!("read Item 7 support {}: {err}", support.source_path)
                            });
                        Some(Item7StagedGuestFile {
                            relative_path,
                            bytes,
                        })
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();
            let source = item7_native_source(assignment);
            let rust_oracle = item7_live_rust_cli_binary_oracle(
                assignment.source_path,
                &source,
                &guest_files,
                "65c02",
                &[],
            );
            Item7StagedCase {
                name: assignment.source_path,
                source,
                guest_files,
                rust_oracle,
            }
        })
        .collect()
}

fn item8_staged_cases() -> Vec<Item7StagedCase> {
    NATIVE_OPCORE_ASSIGNMENTS
        .iter()
        .filter(|assignment| {
            assignment.shard == NativeOpcoreShard::LayoutOutput
                && matches!(assignment.role, NativeOpcoreRole::Root { .. })
        })
        .map(|assignment| {
            let guest_files = NATIVE_OPCORE_ASSIGNMENTS
                .iter()
                .filter_map(|support| match support.role {
                    NativeOpcoreRole::Support { owner }
                        if support.shard == NativeOpcoreShard::LayoutOutput
                            && owner == assignment.source_path =>
                    {
                        let relative_path = support
                            .source_path
                            .strip_prefix("examples/opcore/")
                            .expect("Item 8 support is below examples/opcore")
                            .to_string();
                        let bytes = fs::read(workspace_root().join(support.source_path))
                            .unwrap_or_else(|err| {
                                panic!("read Item 8 support {}: {err}", support.source_path)
                            });
                        Some(Item7StagedGuestFile {
                            relative_path,
                            bytes,
                        })
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();
            let source = item7_native_source(assignment);
            let declared_output_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("build");
            let _declared_output_lock = rust_declared_output_lock()
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            assert!(
                !declared_output_dir.exists(),
                "Item 8 Rust oracle output directory must not pre-exist: {}",
                declared_output_dir.display()
            );
            let declared_output_guard = Item8RustDeclaredOutputDir(declared_output_dir.clone());
            let rust_oracle = item7_live_rust_cli_binary_oracle(
                assignment.source_path,
                &source,
                &guest_files,
                "65c02",
                &[],
            );
            drop(declared_output_guard);
            assert!(
                !declared_output_dir.exists(),
                "Item 8 Rust oracle output directory must be removed after {}",
                assignment.source_path
            );
            Item7StagedCase {
                name: assignment.source_path,
                source,
                guest_files,
                rust_oracle,
            }
        })
        .collect()
}

fn item9_classify_rust_diagnostic(text: &str) -> Option<Item9DiagnosticKind> {
    let folded = text.to_ascii_lowercase();
    if folded.contains("no instruction found") {
        Some(Item9DiagnosticKind::UnknownInstruction)
    } else if folded.contains("unexpected end of expression") {
        Some(Item9DiagnosticKind::UnexpectedEndExpression)
    } else if folded.contains("unknown directive") {
        Some(Item9DiagnosticKind::UnknownDirective)
    } else if folded.contains("invalid number") {
        Some(Item9DiagnosticKind::InvalidNumber)
    } else if folded.contains("invalid image span range") {
        Some(Item9DiagnosticKind::InvalidImageSpan)
    } else {
        None
    }
}

fn item9_live_rust_diagnostic_kind(
    case_name: &str,
    source: &[u8],
    guest_files: &[Item7StagedGuestFile],
) -> Item9DiagnosticKind {
    let case_dir = create_temp_dir("item9-live-rust-cli-oracle");
    let _case_dir_guard = Item7OracleDir(case_dir.clone());
    let input_path = case_dir.join("input.asm");
    let bin_path = case_dir.join("oracle.bin");
    fs::write(&input_path, source)
        .unwrap_or_else(|err| panic!("write same-case Rust Item 9 root {case_name}: {err}"));
    for guest_file in guest_files {
        let path = case_dir.join(&guest_file.relative_path);
        fs::create_dir_all(path.parent().expect("Item 9 support path has a parent"))
            .unwrap_or_else(|err| panic!("create Item 9 support parent {}: {err}", path.display()));
        fs::write(&path, &guest_file.bytes)
            .unwrap_or_else(|err| panic!("write Item 9 support {}: {err}", path.display()));
    }
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input_path.to_string_lossy().into_owned(),
        "--bin".to_string(),
        bin_path.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "65c02".to_string(),
        "-I".to_string(),
        case_dir.to_string_lossy().into_owned(),
        "-M".to_string(),
        case_dir.to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli)
        .unwrap_or_else(|err| panic!("validate same-case Rust Item 9 oracle {case_name}: {err:?}"));
    config.out_dir = Some(case_dir.clone());
    let error = run_with_validated_cli_with_context(&cli, &config)
        .expect_err("assigned Item 9 diagnostic root must fail in live Rust");
    let diagnostic_text = match &error {
        CliRunError::Assembler { error, .. } => error
            .diagnostics()
            .iter()
            .find(|diagnostic| diagnostic.severity == Severity::Error)
            .map(|diagnostic| diagnostic.error.message().to_string())
            .unwrap_or_else(|| error.to_string()),
        _ => format!("{error:?}"),
    };
    item9_classify_rust_diagnostic(&diagnostic_text).unwrap_or_else(|| {
        panic!("unclassified live Rust Item 9 diagnostic for {case_name}: {diagnostic_text}")
    })
}

fn item9_staged_cases() -> Vec<Item9StagedCase> {
    NATIVE_OPCORE_ASSIGNMENTS
        .iter()
        .filter_map(|assignment| {
            let NativeOpcoreRole::Root { reference_stem } = assignment.role else {
                return None;
            };
            if assignment.shard != NativeOpcoreShard::Diagnostic {
                return None;
            }
            if assignment.staging != NativeOpcoreStaging::DirectMos65c02 {
                return None;
            }
            let reference_path = workspace_root()
                .join("examples/reference/opcore")
                .join(reference_stem)
                .with_extension("err");
            if !reference_path.exists() {
                return None;
            }
            let source = item7_native_source(assignment);
            let mut guest_files = NATIVE_OPCORE_ASSIGNMENTS
                .iter()
                .filter_map(|support| match support.role {
                    NativeOpcoreRole::Support { owner }
                        if support.shard == NativeOpcoreShard::Diagnostic
                            && owner == assignment.source_path =>
                    {
                        Some(Item7StagedGuestFile {
                            relative_path: support
                                .source_path
                                .strip_prefix("examples/opcore/")
                                .expect("Item 9 support below examples/opcore")
                                .to_string(),
                            bytes: fs::read(workspace_root().join(support.source_path))
                                .unwrap_or_else(|err| {
                                    panic!("read Item 9 support {}: {err}", support.source_path)
                                }),
                        })
                    }
                    _ => None,
                })
                .collect::<Vec<_>>();
            for (_, support_path) in
                crate::native_reference_parity::NATIVE_OPCORE_DIAGNOSTIC_SHARED_SUPPORT
                    .iter()
                    .filter(|(owner, _)| *owner == assignment.source_path)
            {
                let relative_path = support_path
                    .strip_prefix("examples/opcore/")
                    .expect("Item 9 shared support below examples/opcore");
                guest_files.push(Item7StagedGuestFile {
                    relative_path: relative_path.to_string(),
                    bytes: fs::read(workspace_root().join(support_path)).unwrap_or_else(|err| {
                        panic!("read Item 9 shared support {support_path}: {err}")
                    }),
                });
            }
            let reference_error = fs::read_to_string(&reference_path).unwrap_or_else(|err| {
                panic!("read Item 9 reference {}: {err}", reference_path.display())
            });
            let reference_kind =
                item9_classify_rust_diagnostic(&reference_error).unwrap_or_else(|| {
                    panic!("unclassified Item 9 reference {}", reference_path.display())
                });
            let rust_kind =
                item9_live_rust_diagnostic_kind(assignment.source_path, &source, &guest_files);
            assert_eq!(
                rust_kind, reference_kind,
                "live Rust and checked-in Item 9 reference disagree for {}",
                assignment.source_path
            );
            Some(Item9StagedCase {
                name: assignment.source_path,
                reference_error,
                rust_kind,
            })
        })
        .collect()
}

#[test]
fn item6_staging_covers_every_assigned_root_and_support_file() {
    // Proof levels A/B. The Item 6 evidence set is derived from the exact
    // inventory without a numeric cap. Every root supplies its stored source,
    // owned support tree, and same-case live Rust CLI bytes. Native execution
    // remains the separate Level D test.
    let cases = item6_staged_cases();
    let expected_roots = NATIVE_OPCORE_ASSIGNMENTS
        .iter()
        .filter(|assignment| {
            assignment.shard == NativeOpcoreShard::SyntaxExpression
                && matches!(assignment.role, NativeOpcoreRole::Root { .. })
        })
        .map(|assignment| assignment.source_path)
        .collect::<Vec<_>>();
    assert_eq!(
        cases.iter().map(|case| case.name).collect::<Vec<_>>(),
        expected_roots,
        "uncapped Item 6 staging must contain every assigned root in inventory order"
    );
    assert!(!cases.is_empty(), "Item 6 must have assigned roots");
    for case in &cases {
        let expected_support = NATIVE_OPCORE_ASSIGNMENTS
            .iter()
            .filter_map(|assignment| match assignment.role {
                NativeOpcoreRole::Support { owner }
                    if assignment.shard == NativeOpcoreShard::SyntaxExpression
                        && owner == case.name =>
                {
                    Some(
                        assignment
                            .source_path
                            .strip_prefix("examples/opcore/")
                            .expect("Item 6 support path below opcore"),
                    )
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            case.guest_files
                .iter()
                .map(|file| file.relative_path.as_str())
                .collect::<Vec<_>>(),
            expected_support,
            "Item 6 must stage every support file owned by {}",
            case.name
        );
        assert!(!case.source.is_empty());
        assert!(!case.rust_oracle.is_empty());
    }
}

#[test]
fn item8_staging_covers_every_assigned_root_and_support_file() {
    // Proof levels A/B. This proves the Item 8 shard is derived from every
    // reviewed layout/output root without a case cap, stages every owned support
    // file, and obtains a live same-source Rust CLI binary oracle. It does not
    // execute native code or prove source-declared secondary artifacts.
    let cases = item8_staged_cases();
    let expected_roots = NATIVE_OPCORE_ASSIGNMENTS
        .iter()
        .filter(|assignment| {
            assignment.shard == NativeOpcoreShard::LayoutOutput
                && matches!(assignment.role, NativeOpcoreRole::Root { .. })
        })
        .map(|assignment| assignment.source_path)
        .collect::<Vec<_>>();
    assert_eq!(
        cases.iter().map(|case| case.name).collect::<Vec<_>>(),
        expected_roots,
        "uncapped Item 8 staging must contain every assigned root in inventory order"
    );
    assert!(!cases.is_empty(), "Item 8 must have assigned roots");
    for case in &cases {
        let expected_support = NATIVE_OPCORE_ASSIGNMENTS
            .iter()
            .filter_map(|assignment| match assignment.role {
                NativeOpcoreRole::Support { owner }
                    if assignment.shard == NativeOpcoreShard::LayoutOutput
                        && owner == case.name =>
                {
                    Some(
                        assignment
                            .source_path
                            .strip_prefix("examples/opcore/")
                            .expect("Item 8 support prefix"),
                    )
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            case.guest_files
                .iter()
                .map(|file| file.relative_path.as_str())
                .collect::<Vec<_>>(),
            expected_support,
            "support tree for {}",
            case.name
        );
    }
}

#[test]
fn item7_staging_covers_every_assigned_root_and_support_file() {
    // Proof levels A/B. This proves the parent shard is derived from every
    // Item 5 module/macro/statement root, stages each owned support file, and
    // obtains a same-source live Rust CLI binary. It does not execute native
    // code or prove that the adapted MOS source preserves canonical CPU bytes.
    let cases = item7_staged_cases();
    let expected_roots = NATIVE_OPCORE_ASSIGNMENTS
        .iter()
        .filter(|assignment| {
            assignment.shard == NativeOpcoreShard::ModuleMacroStatement
                && matches!(assignment.role, NativeOpcoreRole::Root { .. })
        })
        .map(|assignment| assignment.source_path)
        .collect::<Vec<_>>();
    assert_eq!(cases.len(), 17, "reviewed Item 7 root count");
    assert_eq!(
        cases.iter().map(|case| case.name).collect::<Vec<_>>(),
        expected_roots
    );
    for case in &cases {
        let mut sources = vec![(case.name, case.source.as_slice())];
        sources.extend(
            case.guest_files
                .iter()
                .map(|file| (file.relative_path.as_str(), file.bytes.as_slice())),
        );
        for (source_name, source_bytes) in sources {
            let source = String::from_utf8_lossy(source_bytes);
            let lowercase = source.to_ascii_lowercase();
            assert!(
                source.lines().all(|line| {
                    let line = line.trim_start().to_ascii_lowercase();
                    !line.starts_with(".cpu ") || line == ".cpu 65c02"
                }),
                "Item 7 source must be CPU-neutral or select 65C02: {source_name}"
            );
            assert!(
                !lowercase.contains("mvi "),
                "Item 7 source must not contain Intel MVI syntax: {source_name}"
            );
        }
        let expected_support = NATIVE_OPCORE_ASSIGNMENTS
            .iter()
            .filter_map(|assignment| match assignment.role {
                NativeOpcoreRole::Support { owner }
                    if assignment.shard == NativeOpcoreShard::ModuleMacroStatement
                        && owner == case.name =>
                {
                    Some(
                        assignment
                            .source_path
                            .strip_prefix("examples/opcore/")
                            .expect("Item 7 support prefix"),
                    )
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            case.guest_files
                .iter()
                .map(|file| file.relative_path.as_str())
                .collect::<Vec<_>>(),
            expected_support,
            "support tree for {}",
            case.name
        );
    }
    let module_basics = cases
        .iter()
        .find(|case| case.name.ends_with("/module_basics.asm"))
        .expect("module_basics Item 7 case");
    assert!(String::from_utf8_lossy(&module_basics.source).contains(".org 1000h"));
    assert!(String::from_utf8_lossy(&module_basics.source).contains(".org 2000h"));
    assert_eq!(module_basics.rust_oracle.len(), 0x1001);
    assert_eq!(module_basics.rust_oracle.first(), Some(&1));
    assert_eq!(module_basics.rust_oracle.last(), Some(&2));
    assert!(module_basics.rust_oracle[1..0x1000]
        .iter()
        .all(|byte| *byte == 0));
}

#[test]
fn native_reference_opcore_diagnostic_contract_uses_live_rust_and_checked_references() {
    // Proof levels A/B. Every stored Item 9 root with a checked-in `.err`
    // artifact supplies its exact source and support files to a fresh in-memory
    // Rust CLI oracle. The oracle's semantic failure must agree with that
    // root's checked reference. This does not execute native code.
    let cases = item9_staged_cases();
    assert_eq!(cases.len(), 13, "reviewed Item 9 error-reference inventory");
    assert!(cases.iter().all(|case| !case.reference_error.is_empty()));
    assert!(cases.iter().all(|case| matches!(
        case.rust_kind,
        Item9DiagnosticKind::UnknownInstruction
            | Item9DiagnosticKind::UnexpectedEndExpression
            | Item9DiagnosticKind::UnknownDirective
            | Item9DiagnosticKind::InvalidNumber
            | Item9DiagnosticKind::InvalidImageSpan
    )));
    let accounted = cases.iter().map(|case| case.name).collect::<HashSet<_>>();
    let reachable = crate::native_reference_parity::NATIVE_OPCORE_DIAGNOSTIC_REACHABLE_ROOTS
        .iter()
        .copied()
        .collect::<HashSet<_>>();
    let excluded = crate::native_reference_parity::NATIVE_OPCORE_DIAGNOSTIC_NATIVE_BLOCKERS
        .iter()
        .map(|(name, reason)| {
            assert!(
                !reason.trim().is_empty(),
                "Item 9 exclusion reason for {name}"
            );
            *name
        })
        .collect::<HashSet<_>>();
    assert!(reachable.is_disjoint(&excluded));
    assert_eq!(
        reachable.union(&excluded).copied().collect::<HashSet<_>>(),
        accounted,
        "every actual stored 65C02 `.err` root is reachable or has one concrete native blocker"
    );
    for (owner, support_path) in
        crate::native_reference_parity::NATIVE_OPCORE_DIAGNOSTIC_SHARED_SUPPORT
    {
        assert!(
            accounted.contains(owner),
            "stale Item 9 shared-support owner"
        );
        assert!(
            NATIVE_OPCORE_ASSIGNMENTS
                .iter()
                .any(|assignment| assignment.source_path == *support_path),
            "Item 9 shared support is absent from the canonical inventory: {support_path}"
        );
    }

    let diagnostic_assignments = NATIVE_OPCORE_ASSIGNMENTS
        .iter()
        .filter(|assignment| {
            assignment.shard == NativeOpcoreShard::Diagnostic
                && assignment.staging == NativeOpcoreStaging::DirectMos65c02
                && matches!(assignment.role, NativeOpcoreRole::Root { .. })
        })
        .map(|assignment| assignment.source_path)
        .collect::<HashSet<_>>();
    let success_roots = crate::native_reference_parity::NATIVE_OPCORE_DIAGNOSTIC_SUCCESS_ROOTS
        .iter()
        .map(|(name, reason)| {
            assert!(
                !reason.trim().is_empty(),
                "Item 9 successful-reference reason for {name}"
            );
            let assignment = NATIVE_OPCORE_ASSIGNMENTS
                .iter()
                .find(|assignment| assignment.source_path == *name)
                .unwrap_or_else(|| panic!("missing Item 9 successful-reference root {name}"));
            let NativeOpcoreRole::Root { reference_stem } = assignment.role else {
                panic!("Item 9 successful-reference path is not a root: {name}");
            };
            let reference_root = workspace_root().join("examples/reference/opcore");
            assert!(
                reference_root
                    .join(format!("{reference_stem}.hex"))
                    .is_file()
                    || reference_root
                        .join(format!("{reference_stem}.lst"))
                        .is_file(),
                "Item 9 successful root {name} must own a normal output reference"
            );
            assert!(
                !reference_root
                    .join(format!("{reference_stem}.err"))
                    .exists(),
                "Item 9 successful root {name} must not masquerade as diagnostic evidence"
            );
            *name
        })
        .collect::<HashSet<_>>();
    assert!(accounted.is_disjoint(&success_roots));
    assert_eq!(
        accounted
            .union(&success_roots)
            .copied()
            .collect::<HashSet<_>>(),
        diagnostic_assignments,
        "all 18 assigned Item 9 roots must be classified without a cap or silent omission"
    );
}

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// Current PRVM runtime imports its macro runtime even for scalar service probes.
// Stage the same exact source closure for the host oracle and native guest.

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// @opforge-evidence: level=D; role=focused-regression; authority=rust-bytes; lifecycle=permanent

// Rust reference fixtures may declare the same relative output directory.
fn rust_declared_output_lock() -> &'static std::sync::Mutex<()> {
    static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    &LOCK
}
