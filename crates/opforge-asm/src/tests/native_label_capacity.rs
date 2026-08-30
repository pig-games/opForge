//! Native label-table and symbol-snapshot capacity parity proofs.

use super::*;

#[test]
fn native_item38_qualified_label_name_budget_matches_rust_product() {
    // Proof level B. Rust has no native fixed-row label ceiling, so inventory
    // the exact current product maximum including active block/namespace scope
    // segments. Prove that only native label rows (not every generic token row)
    // retain the required terminating NUL.
    let root = workspace_root();
    let amigaos = root.join("native/motorola68000/amigaos");
    let mut maximum = (0usize, String::new(), PathBuf::new(), 0usize);
    let mut bucket_counts = [0usize; 256];
    let mut label_count = 0usize;
    for directory in [
        "opforge-cli",
        "tkpkg",
        "tkvm",
        "prvm",
        "exprvm",
        "opcore",
        "opasm",
        "debug",
    ] {
        for entry in fs::read_dir(amigaos.join(directory))
            .unwrap_or_else(|error| panic!("read native product directory {directory}: {error}"))
        {
            let path = entry.expect("read native product entry").path();
            if path.extension().and_then(|value| value.to_str()) != Some("asm") {
                continue;
            }
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
            let mut scopes = Vec::<String>::new();
            for (line_index, line) in source.lines().enumerate() {
                let code = line.split(';').next().unwrap_or("").trim_end();
                let fields = code.split_whitespace().collect::<Vec<_>>();
                if fields.first() == Some(&".module") && fields.len() >= 2 {
                    scopes.clear();
                    scopes.push(fields[1].to_string());
                    continue;
                }
                if fields.first() == Some(&".endmodule") {
                    scopes.clear();
                    continue;
                }
                if matches!(
                    fields.first().copied(),
                    Some(".bend" | ".endblock" | ".endn" | ".endnamespace")
                ) {
                    let _ = scopes.pop();
                    continue;
                }
                if fields.first() == Some(&".namespace") && fields.len() >= 2 {
                    scopes.push(fields[1].to_string());
                    continue;
                }
                let Some(first) = code.as_bytes().first() else {
                    continue;
                };
                if scopes.is_empty() || !(first.is_ascii_alphabetic() || *first == b'_') {
                    continue;
                }
                let label = fields[0].trim_end_matches(':');
                let qualified = if label.contains('.') {
                    label.to_string()
                } else {
                    format!("{}.{}", scopes.join("."), label)
                };
                let bucket = qualified.bytes().fold(0u32, |hash, byte| {
                    hash.wrapping_mul(33) ^ u32::from(byte.to_ascii_lowercase())
                }) as u8;
                bucket_counts[usize::from(bucket)] += 1;
                label_count += 1;
                if qualified.len() > maximum.0
                    || (qualified.len() == maximum.0 && qualified < maximum.1)
                {
                    maximum = (
                        qualified.len(),
                        qualified.clone(),
                        path.clone(),
                        line_index + 1,
                    );
                }
                if fields.get(1) == Some(&".block") {
                    scopes.push(label.to_string());
                }
            }
        }
    }
    eprintln!(
        "ITEM38_QUALIFIED_LABEL_BUDGET max_bytes={} label={} path={} line={}",
        maximum.0,
        maximum.1,
        maximum
            .2
            .strip_prefix(&root)
            .expect("workspace-relative maximum path")
            .display(),
        maximum.3,
    );
    assert_eq!(maximum.0, 107);
    assert_eq!(
        maximum.1,
        "opforge.cli.metadata.opforgeNativeCliRouteRootMetadataLineV1.routeStructuralTargetBoundaryV1.returnBoundary"
    );
    let max_bucket = *bucket_counts.iter().max().expect("nonempty label buckets");
    eprintln!(
        "ITEM38_LABEL_HASH labels={} buckets=256 max_bucket={}",
        label_count, max_bucket
    );
    assert!(
        label_count > 9_000,
        "full product label inventory unexpectedly small"
    );
    assert!(
        max_bucket < 128,
        "native label hash distribution degenerated"
    );
    let native_hash = |text: &str| {
        text.bytes().fold(0u32, |hash, byte| {
            hash.wrapping_mul(33) ^ u32::from(byte.to_ascii_lowercase())
        }) as u8
    };
    assert_eq!(
        native_hash("collision.module.Label0008"),
        native_hash("collision.module.Label0080"),
        "directed guest collision pair must remain in one native bucket"
    );

    let rust_source = ".module opforge.cli.metadata\n.cpu 68020\n.org 0\nopforgeNativeCliRouteRootMetadataLineV1 .block\nrouteStructuralTargetBoundaryV1 .block\nreturnBoundary = 7\n.byte returnBoundary\n.bend\n.bend\n.endmodule\n.end\n";
    let rust_lines = rust_source.lines().collect::<Vec<_>>();
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_lines, true)
        .expect("live Rust 107-byte fully scoped label authority");
    assert!(diagnostics.is_empty(), "Rust diagnostics: {diagnostics:?}");
    assert_eq!(
        entries
            .into_iter()
            .map(|(_, byte)| byte)
            .collect::<Vec<_>>(),
        [7]
    );

    let engine = fs::read_to_string(amigaos.join("opasm/opasm_engine.asm"))
        .expect("read native opasm engine");
    assert!(engine.contains("LABEL_NAME_CAPACITY             = 108"));
    assert!(engine.contains("NATIVE_LABEL_HASH_BUCKET_CAPACITY = 256"));
    assert!(engine.contains(".res byte, NATIVE_STATEMENT_TABLE_CAPACITY * LABEL_NAME_CAPACITY"));
    assert!(engine.contains(".res byte, NATIVE_LABEL_TABLE_CAPACITY * LABEL_NAME_CAPACITY"));
    assert!(engine.contains(".res byte, NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY"));
    assert!(engine
        .contains("OPASM_ENGINE_SESSION_STATEMENT_BYTES = NATIVE_STATEMENT_TABLE_CAPACITY * 308"));
    assert!(engine.contains(
        "OPASM_ENGINE_SESSION_LABEL_BYTES = (NATIVE_LABEL_TABLE_CAPACITY * 123) + (NATIVE_LABEL_HASH_BUCKET_CAPACITY * 4)"
    ));
    assert!(engine.contains("OpasmEngineLabelHashNextTable"));
    assert!(engine.contains("OpasmEngineLabelHashHeadTable"));
    assert!(source_contains_in_order(
        &engine,
        &[
            "clr.w OpasmEngineLabelCount.l",
            "lea OpasmEngineLabelHashHeadTable.l, a0",
            "move.w #NATIVE_LABEL_HASH_BUCKET_CAPACITY - 1, d0",
            "clearHashLoop",
            "clr.l (a0)+",
        ]
    ));
    assert!(engine.contains("findExactLabelIndexV1"));
    assert!(engine.contains("indexLabelNameV1"));

    let rust_symbols = fs::read_to_string(root.join("crates/opforge-types/src/symbol.rs"))
        .expect("read Rust symbol-table authority");
    assert!(rust_symbols.contains("index: HashMap<String, usize>"));
    assert!(rust_symbols.contains("self.index.get(key.as_ref())"));

    let scopes = fs::read_to_string(amigaos.join("opasm/opasm_flow_scopes.asm"))
        .expect("read native scope owner");
    assert!(scopes.contains("OPASM_SCOPE_TEXT_CAPACITY = 108"));
    assert!(scopes.contains("cmpi.l #OPASM_SCOPE_TEXT_CAPACITY - 1, d1"));
    assert!(source_contains_in_order(
        &scopes,
        &[
            "qualifyStatementLabelIfScopedV1\t.block",
            "jsr eng.opasmEngineGetStatementOwnerTextV1",
            "haveStatementOwner",
            "moveq #1, d3",
            "move.w ScopeDepth.l, d2",
            "statementOwnerScopeLoop",
            "statementOwnerPrefixReady",
        ]
    ));

    let expr = fs::read_to_string(amigaos.join("opcore/opcore_expr_bridge.asm"))
        .expect("read native expression bridge");
    assert!(expr.contains("LABEL_NAME_CAPACITY             = 108"));
    assert!(source_contains_in_order(
        &expr,
        &[
            "move.l d5, d1",
            "move.l d5, d2",
            "move.l d5, d0",
            "lsl.l #2, d5",
            "lsl.l #3, d1",
            "lsl.l #5, d2",
            "lsl.l #6, d0",
            "add.l d1, d5",
            "add.l d2, d5",
            "add.l d0, d5",
        ]
    ));
}

#[test]
fn native_label_capacity_tracks_complete_source_record_domain() {
    // Proof level B. This proves native label storage and both read-only symbol
    // snapshot paths cover the complete bounded 16,384-label session domain.
    // It does not prove real 68020 allocation or execution.
    let root = workspace_root();
    let engine =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_engine.asm"))
            .expect("read native opasm engine");
    assert!(engine.contains("NATIVE_LABEL_TABLE_CAPACITY     = 16384"));
    assert!(engine.contains("move.w #NATIVE_LABEL_TABLE_CAPACITY - 1, d0"));

    let context = fs::read_to_string(
        root.join("native/motorola68000/amigaos/tkpkg/tkpkg_runtime_context.asm"),
    )
    .expect("read native runtime context");
    assert!(context.lines().any(|line| {
        line.split_whitespace().collect::<Vec<_>>()
            == ["RUNTIME_CONTEXT_STABILITY_CAPACITY", "=", "16384"]
    }));

    let operand =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_operand_eval.asm"))
            .expect("read native operand evaluator");
    assert!(operand.lines().any(|line| {
        line.split_whitespace().collect::<Vec<_>>()
            == ["SCOPED_SNAPSHOT_SOURCE_CAPACITY", "=", "16384"]
    }));
    assert!(operand.lines().any(|line| {
        line.split_whitespace().collect::<Vec<_>>() == ["SCOPED_SNAPSHOT_CAPACITY", "=", "32768"]
    }));

    let driver = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native assembly driver");
    let section_start = driver
        .find("processSectionDirectiveForStatement\t.block")
        .expect("section driver block");
    let section = &driver[section_start..];
    let section_end = section
        .find("\t.bend  ; processSectionDirectiveForStatement")
        .expect("section driver end");
    assert!(source_contains_in_order(
        &section[..section_end],
        &[
            "jsr eng.opasmEngineGetSessionCurrentPcV1",
            "jsr layout.captureSectionParentPcV1",
        ]
    ));

    let layout =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_layout.asm"))
            .expect("read native layout");
    let endsection_start = layout
        .find("processEndsectionV1\t.block")
        .expect("endsection layout block");
    let endsection = &layout[endsection_start..];
    let endsection_end = endsection
        .find("\t.bend  ; processEndsectionV1")
        .expect("endsection layout end");
    assert!(source_contains_in_order(
        &endsection[..endsection_end],
        &[
            "move.l OpasmLayoutSectionParentPc.l, d0",
            "jsr eng.opasmEngineSetCurrentPcV1",
        ]
    ));
}

#[test]
fn native_label_snapshot_over_512_compound_res_fs_uae() {
    // Proof level D. A fresh guest crosses the former 512-symbol snapshot
    // boundary, evaluates a same-module constant inside the exact compound
    // `.res` shape that the full product uses, and must emit Rust's bytes.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("label-snapshot FS-UAE lock poisoned");
    let root = workspace_root();
    const FILLER_LABELS: usize = 2048;
    let mut source =
        String::from("        .module tkpkg.amigaos.buffers\n        .cpu 68020\n        .org 0\n");
    for index in 0..FILLER_LABELS {
        source.push_str(&format!("VALUE{index:03} .const {index}\n"));
    }
    source.push_str(
        "TOKENIZER_VM_STATE_TABLE_CAPACITY = 32\n        .section bss, kind=bss\n        .res byte, TOKENIZER_VM_STATE_TABLE_CAPACITY * 4\n        .endsection\n        .byte $4e,$71\n        .endmodule\n",
    );

    let rust_lines = source.lines().collect::<Vec<_>>();
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_lines, true)
        .expect("live Rust compound reservation authority");
    assert!(diagnostics.is_empty(), "Rust diagnostics: {diagnostics:?}");
    let rust_bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(rust_bytes, [0x4e, 0x71]);

    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read exact native full-product package");
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "label-snapshot-513-compound-res",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some("{input} --bin {bin} --cpu 68020 --opasm-package {package}"),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/opforge_native_out.bin",
            rust_oracle: &rust_bytes,
        },
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &[case])
        .expect("compound reservation label-snapshot FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(run.protocol_completed);
            assert!(run.success, "native failure:\n{}", run.stderr);
            assert_eq!(run.exit_code, Some(0));
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_bytes);
            eprintln!(
                "ITEM38_LABEL_SNAPSHOT runs={} labels={} protocol_completed={} guest_exit={:?} rust_bytes={} native_bytes={} exact_match=true",
                runs.len(),
                FILLER_LABELS + 1,
                run.protocol_completed,
                run.exit_code,
                rust_bytes.len(),
                native.len(),
            );
        }
    }
}

#[test]
fn native_label_capacity_over_16_fs_uae() {
    // Proof level D. This proves the real native CLI stores more than sixteen
    // labels and exposes the last one to selected-instruction expression
    // evaluation. It does not prove the full 512-record overflow boundary.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("label-capacity FS-UAE lock poisoned");
    let root = workspace_root();
    let mut source = String::from("start\n");
    for index in 0..20 {
        source.push_str(&format!("value{index:02} .const {index}\n"));
    }
    source.push_str("        lda #value19\n        rts\n");

    let mut rust_lines = vec![".cpu 65c02"];
    rust_lines.extend(source.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_lines, true)
        .expect("live Rust label-capacity authority");
    assert!(
        diagnostics.is_empty(),
        "Rust label-capacity diagnostics: {diagnostics:?}"
    );
    let rust_bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(rust_bytes, [0xa9, 0x13, 0x60]);

    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "label-capacity-over-16",
        cpu_id: "65c02",
        source: source.as_bytes(),
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bytes),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("label-capacity FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one label-capacity run");
            let run = &runs[0];
            assert!(
                run.success,
                "native label-capacity source failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_bytes, "native label-capacity bytes differ");
        }
    }
}
