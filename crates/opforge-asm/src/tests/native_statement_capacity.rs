//! Native statement-record capacity boundary proofs.

use super::*;

#[test]
fn native_item38_full_product_source_line_budget_contract() {
    // Proof level B. The exact canonical product graph must remain readable by
    // the native 512-byte line owner. This is a source/allocation boundary, not
    // guest execution or full-product artifact evidence.
    let root = workspace_root();
    let amigaos = root.join("native/motorola68000/amigaos");
    let mut measured = Vec::new();
    let mut loadable_source_bytes = fs::metadata(amigaos.join("main.asm"))
        .expect("measure native main source")
        .len() as usize;
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
            .unwrap_or_else(|error| panic!("read full product directory {directory}: {error}"))
        {
            let path = entry.expect("read full product entry").path();
            let extension = path.extension().and_then(|value| value.to_str());
            if !matches!(extension, Some("asm" | "i")) {
                continue;
            }
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
            if directory != "debug" {
                loadable_source_bytes += source.len();
            }
            for (index, line) in source.lines().enumerate() {
                measured.push((line.len(), path.clone(), index + 1));
            }
        }
    }
    measured.sort_by_key(|(bytes, path, line)| (*bytes, path.clone(), *line));
    let (max_bytes, max_path, max_line) = measured.last().expect("nonempty product graph");
    eprintln!(
        "ITEM38_SOURCE_BUDGET max_bytes={} max_path={} max_line={} loadable_bytes={}",
        max_bytes,
        max_path
            .strip_prefix(&root)
            .expect("workspace-relative path")
            .display(),
        max_line,
        loadable_source_bytes,
    );
    assert_eq!(*max_bytes, 262, "full-product maximum source line changed");
    assert_eq!(
        max_path
            .strip_prefix(&root)
            .expect("workspace-relative path"),
        Path::new("native/motorola68000/amigaos/opforge-cli/constants.asm")
    );
    assert_eq!(*max_line, 189);
    assert!(
        *max_bytes < 512,
        "full-product source line exceeds the native 512-byte owner"
    );
    assert_eq!(
        loadable_source_bytes, 1_615_563,
        "canonical loadable source-byte budget changed"
    );
    assert!(
        loadable_source_bytes < 2_097_152,
        "canonical source bytes exceed the native packed-source owner"
    );

    let main_path = amigaos.join("main.asm");
    let root_lines =
        expand_source_file(&main_path, &[], &[], 64).expect("expand canonical native product root");
    let module_paths = example_module_paths(&main_path);
    let graph = load_module_graph(&main_path, root_lines, &[], &[], &module_paths, 64)
        .expect("load canonical native product graph");
    let rust_processed_source_bytes = graph.lines.iter().map(|line| line.len() + 1).sum::<usize>();
    eprintln!(
        "ITEM38_RUST_PRODUCT_GRAPH lines={} bytes={}",
        graph.lines.len(),
        rust_processed_source_bytes
    );
    assert_eq!(
        graph.lines.len(),
        91_775,
        "Rust-processed row budget changed"
    );
    assert_eq!(
        rust_processed_source_bytes, 3_516_613,
        "Rust-processed byte budget changed"
    );
    assert!(
        rust_processed_source_bytes < 4_194_304,
        "Rust-processed product graph exceeds the native packed-source owner"
    );

    let constants = fs::read_to_string(amigaos.join("opforge-cli/constants.asm"))
        .expect("read native CLI constants");
    assert!(constants.contains("SOURCE_LINE_BUFFER_CAPACITY     = 512"));
    assert!(constants.contains("NATIVE_SOURCE_RECORD_CAPACITY   = 100000"));
    assert!(constants.contains("NATIVE_SOURCE_TEXT_POOL_CAPACITY = 4194304"));
    assert!(constants.contains("NATIVE_STATEMENT_TABLE_CAPACITY = 100000"));
    assert!(constants.contains(
        "NATIVE_ASSEMBLY_SESSION_STATEMENT_BYTES = NATIVE_STATEMENT_TABLE_CAPACITY * 308"
    ));
    assert!(constants.contains(
        "NATIVE_ASSEMBLY_SESSION_BYTES = NATIVE_ASSEMBLY_SESSION_HEADER_BYTES + NATIVE_ASSEMBLY_SESSION_SOURCE_BYTES + NATIVE_ASSEMBLY_SESSION_STATEMENT_BYTES + NATIVE_ASSEMBLY_SESSION_LABEL_BYTES + NATIVE_ASSEMBLY_SESSION_TAIL_BYTES + NATIVE_ASSEMBLY_SESSION_IMAGE_BYTES"
    ));
}

#[test]
fn native_item38_full_product_import_budget_contract() {
    // Proof level B. Measure every retained `.use` request in the exact native
    // product graph and keep the native table above that Rust-owned graph.
    // Guest execution is proved separately at the former 32-row boundary.
    let root = workspace_root();
    let amigaos = root.join("native/motorola68000/amigaos");
    let mut sources = vec![amigaos.join("main.asm")];
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
            .unwrap_or_else(|error| panic!("read full product directory {directory}: {error}"))
        {
            let path = entry.expect("read full product entry").path();
            if matches!(
                path.extension().and_then(|value| value.to_str()),
                Some("asm" | "i")
            ) {
                sources.push(path);
            }
        }
    }
    let use_count = sources
        .iter()
        .map(|path| {
            fs::read_to_string(path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()))
                .lines()
                .filter(|line| line.trim_start().starts_with(".use "))
                .count()
        })
        .sum::<usize>();
    eprintln!("ITEM38_IMPORT_BUDGET uses={use_count} capacity=512");
    assert_eq!(use_count, 379, "full-product `.use` graph changed");
    let constants = fs::read_to_string(amigaos.join("opforge-cli/constants.asm"))
        .expect("read native CLI constants");
    assert!(constants.contains("NATIVE_IMPORT_TABLE_CAPACITY    = 512"));
    assert!(use_count < 512, "full-product imports exceed native budget");
}

#[test]
fn native_item38_full_product_module_resolve_depth_budget_contract() {
    // Proof level B. Measure the canonical `.use` graph rooted at main and tie
    // the native fixed stack to Rust's recursive push/load/pop ordering. Guest
    // execution across the former eight-frame boundary is proved separately.
    fn visit<'a>(
        module: &'a str,
        uses: &'a HashMap<String, Vec<String>>,
        stack: &mut Vec<&'a str>,
        maximum: &mut Vec<String>,
    ) {
        assert!(
            !stack.contains(&module),
            "canonical product module cycle at {module}"
        );
        stack.push(module);
        if stack.len() > maximum.len() {
            *maximum = stack.iter().map(|name| (*name).to_string()).collect();
        }
        if let Some(dependencies) = uses.get(module) {
            for dependency in dependencies {
                visit(dependency, uses, stack, maximum);
            }
        }
        stack.pop();
    }

    let root = workspace_root();
    let amigaos = root.join("native/motorola68000/amigaos");
    let mut sources = vec![amigaos.join("main.asm")];
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
            .unwrap_or_else(|error| panic!("read full product directory {directory}: {error}"))
        {
            let path = entry.expect("read full product entry").path();
            if path.extension().and_then(|value| value.to_str()) == Some("asm") {
                sources.push(path);
            }
        }
    }
    sources.sort();
    let mut uses = HashMap::<String, Vec<String>>::new();
    for path in sources {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        let mut active_module = None::<String>;
        for line in source.lines() {
            let code = line.split(';').next().unwrap_or("").trim();
            let mut words = code.split_ascii_whitespace();
            match words.next() {
                Some(".module") => {
                    let module = words.next().expect("module name").to_ascii_lowercase();
                    uses.entry(module.clone()).or_default();
                    active_module = Some(module);
                }
                Some(".endmodule") => active_module = None,
                Some(".use") => {
                    if let (Some(owner), Some(dependency)) = (active_module.as_ref(), words.next())
                    {
                        uses.entry(owner.clone())
                            .or_default()
                            .push(dependency.trim_end_matches('(').to_ascii_lowercase());
                    }
                }
                _ => {}
            }
        }
    }
    let mut maximum = Vec::new();
    visit("main", &uses, &mut Vec::new(), &mut maximum);
    assert_eq!(maximum.len(), 19, "canonical module depth changed");
    assert_eq!(maximum.first().map(String::as_str), Some("main"));
    assert_eq!(
        maximum.last().map(String::as_str),
        Some("opasm.amigaos.callback_abi")
    );

    let rust = fs::read_to_string(root.join("crates/opforge-engine/src/source_graph.rs"))
        .expect("read Rust module-graph authority");
    assert!(source_contains_in_order(
        &rust,
        &[
            "ctx.stack.push(module_id.to_string());",
            "for dep in collect_use_directives_from_processing(&module_lines)",
            "load_module_recursive(&dep, ctx, &info.path, &module_lines)?;",
            "ctx.stack.pop();",
        ]
    ));
    let constants = fs::read_to_string(amigaos.join("opforge-cli/constants.asm"))
        .expect("read native CLI constants");
    assert!(constants.contains("NATIVE_MODULE_RESOLVE_DEPTH_LIMIT = 32"));
    assert!(constants.contains("NATIVE_STATEMENT_OWNER_DEPTH_CAPACITY = 32"));
    assert!(maximum.len() < 32);
    let owners = fs::read_to_string(amigaos.join("opforge-cli/statement_owners.asm"))
        .expect("read statement-owner stack");
    assert!(owners.contains("cmpi.w #constants.NATIVE_STATEMENT_OWNER_DEPTH_CAPACITY, d2"));
    assert!(owners.contains(".res word, constants.NATIVE_STATEMENT_OWNER_DEPTH_CAPACITY"));
    assert!(owners.contains(
        ".res byte, constants.NATIVE_STATEMENT_OWNER_DEPTH_CAPACITY * constants.TOKEN_BUFFER_CAPACITY"
    ));
    let scopes = fs::read_to_string(amigaos.join("opasm/opasm_flow_scopes.asm"))
        .expect("read engine module-scope stack");
    assert!(scopes.contains("OPASM_MODULE_PARENT_DEPTH_CAPACITY = 32"));
    assert!(scopes.contains("cmpi.w #OPASM_MODULE_PARENT_DEPTH_CAPACITY, d1"));
    assert!(scopes.contains(".res long, OPASM_MODULE_PARENT_DEPTH_CAPACITY"));
}

#[test]
fn native_item38_full_product_public_export_budget_contract() {
    // Proof level B. Count every canonical declaration candidate appearing
    // while module visibility is public. This is a conservative source-level
    // budget: conditional declarations can reduce the live Rust symbol set,
    // but native must have room before it knows which exports later imports
    // will select.
    let root = workspace_root();
    let amigaos = root.join("native/motorola68000/amigaos");
    let mut public_declaration_count = 0usize;
    let mut public_declaration_name_bytes = 0usize;
    for directory in [
        "opforge-cli",
        "tkpkg",
        "tkvm",
        "prvm",
        "exprvm",
        "opcore",
        "opasm",
    ] {
        for entry in fs::read_dir(amigaos.join(directory))
            .unwrap_or_else(|error| panic!("read full product directory {directory}: {error}"))
        {
            let path = entry.expect("read full product entry").path();
            if path.extension().and_then(|value| value.to_str()) != Some("asm") {
                continue;
            }
            let source = fs::read_to_string(&path)
                .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
            let mut public = false;
            for line in source.lines() {
                let trimmed = line.trim_start();
                if trimmed.starts_with(".module") || trimmed.starts_with(".endmodule") {
                    public = false;
                    continue;
                }
                if trimmed == ".pub" {
                    public = true;
                    continue;
                }
                if trimmed == ".priv" {
                    public = false;
                    continue;
                }
                if public
                    && line
                        .as_bytes()
                        .first()
                        .is_some_and(|byte| byte.is_ascii_alphabetic() || *byte == b'_')
                {
                    public_declaration_count += 1;
                    public_declaration_name_bytes += trimmed
                        .split_ascii_whitespace()
                        .next()
                        .expect("public declaration name")
                        .len()
                        + 1;
                }
            }
        }
    }
    eprintln!(
        "ITEM38_EXPORT_BUDGET declarations={} name_bytes={} declaration_capacity=8192 name_capacity=262144",
        public_declaration_count,
        public_declaration_name_bytes,
    );
    assert_eq!(
        public_declaration_count, 6_503,
        "canonical public-declaration budget changed"
    );
    assert_eq!(
        public_declaration_name_bytes, 126_581,
        "canonical packed public-export name budget changed"
    );
    let constants = fs::read_to_string(amigaos.join("opforge-cli/constants.asm"))
        .expect("read native CLI constants");
    assert!(constants.contains("NATIVE_ORDINARY_EXPORT_CAPACITY = 8192"));
    assert!(constants.contains("NATIVE_ORDINARY_EXPORT_NAME_POOL_CAPACITY = 262144"));
    assert!(
        public_declaration_count < 8_192,
        "full-product public declarations exceed native budget"
    );
    assert!(
        public_declaration_name_bytes < 262_144,
        "full-product public names exceed native packed-name budget"
    );
    let state =
        fs::read_to_string(amigaos.join("opforge-cli/state.asm")).expect("read native CLI state");
    assert!(state.contains("NativeCliModuleOrdinaryExportHeadTable"));
    assert!(state.contains("NativeCliOrdinaryExportNextTable"));
    assert!(state.contains("NativeCliOrdinaryExportNameOffsetTable"));
    assert!(state.contains("NativeCliOrdinaryExportNamePool"));
    assert!(!state.contains("NativeCliOrdinaryExportStatementIndexTable"));
    assert!(!state.contains("NativeCliOrdinaryExportNameTable"));
    let module_use = fs::read_to_string(amigaos.join("opforge-cli/module_use.asm"))
        .expect("read native module/use owner");
    assert!(source_contains_in_order(
        &module_use,
        &[
            "lea state.NativeCliModuleOrdinaryExportHeadTable, a0",
            "move.l 0(a0, d6.l), d6",
            "subq.l #1, d6",
            "lea state.NativeCliOrdinaryExportNameOffsetTable, a0",
            "lea state.NativeCliOrdinaryExportNextTable, a0",
            "move.l 0(a0, d0.l), d6",
        ]
    ));
}

#[test]
fn native_item38_full_product_conditional_depth_budget_contract() {
    // Proof level B. Measure the exact canonical staged product graph rather
    // than treating the native conditional stack as an arbitrary constant.
    // The directed guest proof separately executes this maximum depth.
    let root = workspace_root();
    let amigaos = root.join("native/motorola68000/amigaos");
    let mut sources = vec![amigaos.join("main.asm")];
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
            .unwrap_or_else(|error| panic!("read full product directory {directory}: {error}"))
        {
            let path = entry.expect("read full product entry").path();
            if matches!(
                path.extension().and_then(|value| value.to_str()),
                Some("asm" | "i")
            ) {
                sources.push(path);
            }
        }
    }
    sources.sort();

    let mut maximum = (0usize, PathBuf::new(), 0usize);
    for path in sources {
        let source = fs::read_to_string(&path)
            .unwrap_or_else(|error| panic!("read {}: {error}", path.display()));
        let mut depth = 0usize;
        for (index, line) in source.lines().enumerate() {
            let directive = line.trim_start().split_ascii_whitespace().next();
            match directive {
                Some(".if" | ".ifdef" | ".ifndef") => {
                    depth += 1;
                    if depth > maximum.0 {
                        maximum = (depth, path.clone(), index + 1);
                    }
                }
                Some(".endif") => {
                    assert!(
                        depth > 0,
                        "conditional underflow at {}:{}",
                        path.display(),
                        index + 1
                    );
                    depth -= 1;
                }
                _ => {}
            }
        }
        assert_eq!(depth, 0, "unclosed conditional in {}", path.display());
    }

    assert_eq!(maximum.0, 32, "canonical conditional depth changed");
    assert_eq!(
        maximum
            .1
            .strip_prefix(&root)
            .expect("workspace-relative path"),
        Path::new("native/motorola68000/amigaos/opforge-cli/strings.asm")
    );
    assert_eq!(maximum.2, 402, "deepest canonical conditional moved");

    let constants = fs::read_to_string(amigaos.join("opforge-cli/constants.asm"))
        .expect("read native CLI constants");
    assert!(constants.contains("NATIVE_PREPROCESS_CONDITIONAL_DEPTH_CAPACITY = 64"));
    assert!(
        maximum.0 < 64,
        "canonical conditional depth exceeds native stack headroom"
    );
}

#[test]
fn native_item38_full_product_macro_depth_budget_contract() {
    // Proof level B. Rust recursively expands substituted macro bodies and is
    // authoritative for that logic. The exact full-product source needs two
    // active invocations (an emit* helper calling emitLe32); native retains
    // four fixed frames as bounded AmigaOS headroom.
    let root = workspace_root();
    let rust = fs::read_to_string(root.join("crates/opforge-core/src/macro_processor.rs"))
        .expect("read Rust macro authority");
    assert!(rust.contains("max_depth: 64"));
    assert!(rust.contains("self.expand_lines(&expanded, depth + 1)?"));

    let amigaos = root.join("native/motorola68000/amigaos");
    let product_macros = fs::read_to_string(amigaos.join("tkvm/tkvm_demo_program.asm"))
        .expect("read full-product macro graph");
    for nested_edge in [
        "emitJumpTarget\t.macro opcode, target",
        "emitClassJump\t.macro class_id, target",
        "emitByteJump\t.macro byte_value, target",
        "\t.emitLe32 .target",
    ] {
        assert!(
            product_macros.contains(nested_edge),
            "full-product nested macro edge changed: {nested_edge}"
        );
    }

    let constants = fs::read_to_string(amigaos.join("opforge-cli/constants.asm"))
        .expect("read native CLI constants");
    assert!(constants.contains("NATIVE_PREPROCESS_EXPANSION_DEPTH_LIMIT = 4"));
    assert!(constants.contains("NATIVE_PREPROCESS_INVOCATION_DEPTH_LIMIT = 4"));
    let state =
        fs::read_to_string(amigaos.join("opforge-cli/state.asm")).expect("read native CLI state");
    assert!(state.contains("NativeCliPreprocessSavedInvocationArgs"));
    assert!(state.contains("NativeCliPreprocessSavedInvocationFullArgs"));
    assert!(state.contains("NativeCliPreprocessSavedInvocationLabel"));
    let preprocessor = fs::read_to_string(amigaos.join("opforge-cli/preprocessor.asm"))
        .expect("read native preprocessor");
    assert!(source_contains_in_order(
        &preprocessor,
        &[
            "opforgeNativeCliBeginMacroInvocationFrameV1\t.block",
            "bsr.w saveInvocationFrame",
            "opforgeNativeCliEndMacroInvocationFrameV1\t.block",
            "bsr.w restoreInvocationFrame",
        ]
    ));
}

#[test]
fn native_statement_capacity_tracks_bounded_source_record_capacity() {
    // Proof level B. Source and statement rows have independent measured
    // domains: expanded source text is packed once, statements reference that
    // record, and opasm rejects either bounded domain before writing beyond it.
    let root = workspace_root();
    let cli_constants =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/constants.asm"))
            .expect("read native CLI constants");
    let engine =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_engine.asm"))
            .expect("read native opasm engine");
    let assembly_session = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/assembly_session.asm"),
    )
    .expect("read native assembly session");
    let line_processor = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opforge-cli/line_processor.asm"),
    )
    .expect("read native line processor");
    let metadata =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opforge-cli/metadata.asm"))
            .expect("read native metadata router");

    for (owner, source) in [("CLI", &cli_constants), ("opasm", &engine)] {
        assert!(
            source.contains("NATIVE_SOURCE_RECORD_CAPACITY   = 100000"),
            "{owner} expanded-source capacity must stay explicitly bounded"
        );
        assert!(
            source.contains("NATIVE_SOURCE_TEXT_POOL_CAPACITY = 4194304"),
            "{owner} packed source-text capacity must stay explicitly bounded"
        );
        assert!(
            source.contains("NATIVE_STATEMENT_TABLE_CAPACITY = 100000"),
            "{owner} statement capacity must retain measured headroom"
        );
    }
    assert!(source_contains_in_order(
        &engine,
        &[
            "cmpi.l #NATIVE_STATEMENT_TABLE_CAPACITY, d0",
            "bhs.w fail",
            "bsr.w storeStatementRecord",
            "tst.l d0",
            "bne.s fail",
        ]
    ));
    assert!(source_contains_in_order(
        &engine,
        &[
            "cmpi.l #NATIVE_SOURCE_RECORD_CAPACITY, d2",
            "bhs.s fail",
            "cmpi.l #NATIVE_SOURCE_TEXT_POOL_CAPACITY, d3",
            "bhi.s fail",
            "lea OpasmEngineSourceLineOffsetTable.l, a0",
            "lea OpasmEngineSourceLineTextTable.l, a1",
        ]
    ));
    assert!(source_contains_in_order(
        &engine,
        &[
            "lea OpasmEngineStmtSourceRecordIndexTable.l, a0",
            "move.l d0, 0(a0, d1.l)",
        ]
    ));
    assert!(!engine.contains("OpasmEngineStmtSourceLineTextTable"));
    assert!(!engine.contains("OpasmEngineStmtMnemNameTable"));
    assert!(source_contains_in_order(
        &engine,
        &[
            "bsr.w getStatementSourceLineTextV1",
            "lea OpasmEngineStmtMnemStartTable.l, a1",
            "adda.l d2, a0",
            "move.l a0, OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a2)",
        ]
    ));
    assert!(source_contains_in_order(
        &assembly_session,
        &[
            "opforgeNativeCliRecordSourceLine\t.block",
            "movem.l d1/a0, -(sp)",
            "jsr engine.opasmEngineRecordSourceLineV1",
            "movem.l (sp)+, d1/a0",
            "rts",
        ]
    ));
    assert!(source_contains_in_order(
        &line_processor,
        &[
            "record",
            "jsr assembly_session.opforgeNativeCliRecordSourceLine",
            "bne.w fail",
        ]
    ));
    assert!(source_contains_in_order(
        &line_processor,
        &[
            "opforgeNativeCliProcessExpandedScopeLineV1\t.block",
            "jsr assembly_session.opforgeNativeCliRecordSourceLine",
            "bne.s expandedScopeRecordDone",
            "jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine",
            "expandedScopeRecordDone",
            "move.l d0, -(sp)",
            "jsr preprocessor_expansion.opforgeNativeCliEndExpandedLineV1.l",
            "jsr engine.opasmEngineRollbackCollectionV1",
        ]
    ));
    assert!(source_contains_in_order(
        &metadata,
        &[
            "handled",
            "jsr assembly_session.opforgeNativeCliRecordSourceLine",
            "tst.l d0",
            "bne.s malformed",
            "moveq #1, d0",
        ]
    ));
}

#[test]
fn native_statement_index_over_u16_fs_uae() {
    // Proof level D. Cross the complete unsigned-word index range with
    // zero-width layout statements, then emit one byte. This proves the native
    // parser, expression metadata, both pass loops, and final emission retain
    // the same 32-bit statement identity as the live Rust authority.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("statement-index FS-UAE lock poisoned");
    let root = workspace_root();
    const ZERO_WIDTH_STATEMENTS: usize = 65_537;
    let mut source = String::with_capacity(ZERO_WIDTH_STATEMENTS * 19 + 16);
    for _ in 0..ZERO_WIDTH_STATEMENTS {
        source.push_str("        .org $1000\n");
    }
    source.push_str("        nop\n");

    let mut rust_lines = vec![".cpu 65c02"];
    rust_lines.extend(source.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_lines, true)
        .expect("Rust unsigned-word statement-index authority");
    assert!(diagnostics.is_empty(), "Rust diagnostics: {diagnostics:?}");
    let rust_bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(rust_bytes, [0xea], "one final 65C02 NOP byte");

    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "statement_index_65537",
        cpu_id: "65c02",
        source: source.as_bytes(),
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bytes),
    };

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("unsigned-word statement-index FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one fresh statement-index guest ran");
            let run = &runs[0];
            assert!(run.protocol_completed, "fresh guest protocol must complete");
            assert_eq!(run.exit_code, Some(0), "guest must explicitly exit zero");
            assert!(
                run.success,
                "native 65,538-statement source failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_bytes, "native final byte differs from Rust");
            eprintln!(
                "ITEM38_STATEMENT_U16 runs={} statements={} protocol_completed={} guest_exit={:?} rust_bytes={} native_bytes={} exact_match=true",
                runs.len(),
                ZERO_WIDTH_STATEMENTS + 1,
                run.protocol_completed,
                run.exit_code,
                rust_bytes.len(),
                native.len()
            );
        }
    }
}

#[test]
fn native_statement_capacity_over_160_fs_uae() {
    // Proof level D. This proves the real Amiga-native CLI stores and emits 161
    // ordinary statements exactly like Rust. It does not prove unrelated label,
    // expression, source-line, or image capacity behavior.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("statement-capacity FS-UAE lock poisoned");
    let root = workspace_root();
    let mut source = String::from("        .org $1000\n");
    for _ in 0..161 {
        source.push_str("        nop\n");
    }
    let mut rust_lines = vec![".cpu 65c02"];
    rust_lines.extend(source.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_lines, true)
        .expect("Rust statement-capacity authority");
    assert!(diagnostics.is_empty(), "Rust diagnostics: {diagnostics:?}");
    let rust_bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(rust_bytes.len(), 161, "one emitted byte per NOP");
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "statement_capacity_161",
        cpu_id: "65c02",
        source: source.as_bytes(),
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bytes),
    };

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("statement-capacity FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one native statement-capacity run");
            let run = &runs[0];
            assert!(
                run.success,
                "native 161-statement source failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_bytes, "native 161-statement bytes differ");
        }
    }
}

#[test]
fn native_statement_capacity_over_512_fs_uae() {
    // Proof level D. Cross the former 512-row source/statement allocation in
    // the real Amiga-native CLI and require the exact in-memory Rust result.
    // This isolates packed-session storage; it is not full-product evidence.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("statement-capacity FS-UAE lock poisoned");
    let root = workspace_root();
    let mut source = String::from("        .org $1000\n");
    for _ in 0..513 {
        source.push_str("        nop\n");
    }
    let mut rust_lines = vec![".cpu 65c02"];
    rust_lines.extend(source.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_lines, true)
        .expect("Rust packed-session authority");
    assert!(diagnostics.is_empty(), "Rust diagnostics: {diagnostics:?}");
    let rust_bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(rust_bytes.len(), 513, "one emitted byte per NOP");
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "statement_capacity_513",
        cpu_id: "65c02",
        source: source.as_bytes(),
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bytes),
    };

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("packed-session FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one native packed-session run");
            let run = &runs[0];
            assert!(run.protocol_completed, "fresh guest protocol must complete");
            assert_eq!(run.exit_code, Some(0), "guest must explicitly exit zero");
            assert!(
                run.success,
                "native 513-statement source failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_bytes, "native 513-statement bytes differ");
            eprintln!(
                "ITEM38_PACKED_SESSION runs={} protocol_completed={} guest_exit={:?} rust_bytes={} native_bytes={} exact_match=true",
                runs.len(),
                run.protocol_completed,
                run.exit_code,
                rust_bytes.len(),
                native.len()
            );
        }
    }
}

#[test]
fn native_statement_index_crosses_sign_bit_fs_uae() {
    // Proof level D localization. Row 32,768 is the first statement whose
    // 16-bit index has its sign bit set. The real guest must still address all
    // word/long statement tables with an unsigned long byte offset and emit
    // the exact in-memory Rust image.
    static LOCK: std::sync::OnceLock<std::sync::Mutex<()>> = std::sync::OnceLock::new();
    let _guard = LOCK
        .get_or_init(|| std::sync::Mutex::new(()))
        .lock()
        .expect("statement-index FS-UAE lock poisoned");
    let root = workspace_root();
    let mut source = String::with_capacity(327_712);
    source.push_str("        .org $1000\n");
    for _ in 0..32_769 {
        source.push_str("        nop\n");
    }
    let mut rust_lines = vec![".cpu 65c02"];
    rust_lines.extend(source.lines());
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&rust_lines, true)
        .expect("Rust statement-index authority");
    assert!(diagnostics.is_empty(), "Rust diagnostics: {diagnostics:?}");
    let rust_bytes = entries
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(rust_bytes.len(), 32_769, "one emitted byte per NOP");
    let package = item6_mos_package_bytes();
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "statement_index_32769",
        cpu_id: "65c02",
        source: source.as_bytes(),
        package_bytes: package.as_slice(),
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_bytes),
    };

    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(&root, &[case])
        .expect("statement-index FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => {
            eprintln!("SKIP: {reason}");
        }
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one native statement-index run");
            let run = &runs[0];
            assert!(run.protocol_completed, "fresh guest protocol must complete");
            assert_eq!(run.exit_code, Some(0), "guest must explicitly exit zero");
            assert!(
                run.success,
                "native statement-index source failed\nstdout:\n{}\nstderr:\n{}",
                run.stdout, run.stderr
            );
            let native = verified_fs_uae_output(run);
            assert_eq!(native, rust_bytes, "native statement-index bytes differ");
            eprintln!(
                "ITEM38_STATEMENT_SIGN_BIT runs={} protocol_completed={} guest_exit={:?} rust_bytes={} native_bytes={} exact_match=true",
                runs.len(),
                run.protocol_completed,
                run.exit_code,
                rust_bytes.len(),
                native.len()
            );
        }
    }
}
