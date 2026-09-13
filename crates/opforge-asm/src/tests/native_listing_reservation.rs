//! Reservation listing and semantic transport contracts.

use super::*;

#[test]
fn native_listing_reservation_host_unit_scaling_and_empty_image() {
    // Level A: live Rust proves sizing and non-emission, not native behavior.
    for (unit, count, expected) in [
        ("byte", "4", 4),
        ("word", "2", 4),
        ("long", "1", 4),
        ("(WoRd)", "2", 4),
        ("(( long ))", "1", 4),
        ("1+2", "2", 6),
        ("byte", "0", 0),
    ] {
        let mut symbols = SymbolTable::new();
        let registry = default_registry();
        let mut asm = make_asm_line(&mut symbols, &registry);
        assert_eq!(process_line(&mut asm, ".cpu 68000", 0, 1), LineStatus::Ok);
        assert_eq!(
            process_line(&mut asm, ".section vars, kind=bss", 0, 1),
            LineStatus::Ok
        );
        let source = format!(".res {unit}, {count}");
        assert_eq!(
            process_line(&mut asm, &source, 0x8001, 1),
            LineStatus::DirDs
        );
        assert_eq!(asm.aux_value(), expected, "{source}");
        assert!(asm.bytes().is_empty(), "RES must not emit image bytes");
    }
}

#[test]
fn native_listing_reservation_host_scalar_domain_requires_more_than_low32() {
    // Level A: these distinct live Rust outcomes define the missing native
    // scalar boundary. They do not establish native parity for this domain.
    for (count, expected) in [
        ("$80000000+0", Some(0x8000_0000_u32)),
        ("-2147483648", None),
        ("$ffffffff+1", None),
        ("($ffffffff+1)-1", Some(u32::MAX)),
    ] {
        let mut symbols = SymbolTable::new();
        let registry = default_registry();
        let mut asm = make_asm_line(&mut symbols, &registry);
        assert_eq!(process_line(&mut asm, ".cpu 68020", 0, 1), LineStatus::Ok);
        assert_eq!(
            process_line(&mut asm, ".section zero, kind=bss", 0, 1),
            LineStatus::Ok
        );
        let status = process_line(&mut asm, &format!(".res byte, {count}"), 0, 1);
        match expected {
            Some(extent) => {
                assert_eq!(
                    status,
                    LineStatus::DirDs,
                    "{count}: {}",
                    asm.error_message()
                );
                assert_eq!(asm.aux_value(), extent, "{count}");
                assert!(asm.bytes().is_empty());
            }
            None => assert_eq!(status, LineStatus::Error, "{count}"),
        }
    }
}

#[test]
fn native_listing_reservation_host_rejects_invalid_extent() {
    // Level A: live Rust rejection authority, not native diagnostic proof.
    for (source, needle) in [
        (".res 0, 1", "greater than zero"),
        (".res -1, 2", "non-negative"),
        (".res byte, -1", "non-negative"),
        (".res $ffffffff, 2", "overflow"),
    ] {
        let mut symbols = SymbolTable::new();
        let registry = default_registry();
        let mut asm = make_asm_line(&mut symbols, &registry);
        assert_eq!(process_line(&mut asm, ".cpu 68000", 0, 1), LineStatus::Ok);
        assert_eq!(
            process_line(&mut asm, ".section vars, kind=bss", 0, 1),
            LineStatus::Ok
        );
        assert_eq!(
            process_line(&mut asm, source, 0x8001, 1),
            LineStatus::Error,
            "{source}"
        );
        assert!(
            asm.error_message().to_ascii_lowercase().contains(needle),
            "{source}: {}",
            asm.error_message()
        );
    }

    let mut symbols = SymbolTable::new();
    let registry = default_registry();
    let mut asm = make_asm_line(&mut symbols, &registry);
    assert_eq!(process_line(&mut asm, ".cpu 6502", 0, 1), LineStatus::Ok);
    assert_eq!(
        process_line(&mut asm, ".section vars, kind=bss", 0, 1),
        LineStatus::Ok
    );
    assert_eq!(
        process_line(&mut asm, ".res byte, 2", 0xffff, 1),
        LineStatus::Error
    );
    assert!(asm.error_message().contains(".res span"));
}

#[test]
fn native_listing_reservation_whole_program_rejects_next_pc_past_cpu_max() {
    // Level A: the primitive reservation span ends at $ffff, but the complete
    // Rust section lifecycle rejects its resulting next PC of $010000.
    let source = b".module main\n.cpu 6502\n.section edge, kind=bss\n.org $ffff\n.res byte, 1\n.endsection\n.endmodule\n";
    let oracle_dir = create_temp_dir("native-listing-reservation-max-pc-oracle");
    struct OracleDir(std::path::PathBuf);
    impl Drop for OracleDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _oracle_guard = OracleDir(oracle_dir.clone());
    let input = oracle_dir.join("input.asm");
    let bin = oracle_dir.join("edge.bin");
    fs::write(&input, source).expect("write max-PC reservation source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--bin".to_string(),
        bin.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "6502".to_string(),
        "--opasm-package".to_string(),
        workspace_root()
            .join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).expect("validate max-PC Rust CLI");
    config.out_dir = Some(oracle_dir);
    let error = run_with_validated_cli_with_context(&cli, &config)
        .expect_err("complete max-PC reservation source must fail");
    let diagnostic = match &error {
        CliRunError::Assembler { error, .. } => error
            .diagnostics()
            .iter()
            .find(|diagnostic| diagnostic.severity == Severity::Error)
            .map(|diagnostic| diagnostic.error.message().to_string())
            .unwrap_or_else(|| error.to_string()),
        _ => format!("{error:?}"),
    };
    assert!(diagnostic.contains("program counter $010000 exceeds max $FFFF"));
}

#[test]
fn native_listing_reservation_engine_contract_owns_full_extent_and_zero_emission() {
    // Level B: retained ownership and pass lifecycle; not guest execution.
    let root = workspace_root();
    let engine =
        fs::read_to_string(root.join("native/motorola68000/amigaos/opasm/opasm_engine.asm"))
            .expect("read native engine");
    let driver = fs::read_to_string(
        root.join("native/motorola68000/amigaos/opasm/opasm_assembly_driver.asm"),
    )
    .expect("read native assembly driver");
    assert!(engine.contains("OPASM_ENGINE_STMT_RESERVATION_FLAG = $8000"));
    assert!(engine.contains("OPASM_ENGINE_STMT_PARSER_KIND_MASK = $7fff"));
    let setter = routine_body(&engine, "opasmEngineSetStatementReservationV1")
        .expect("reservation setter body");
    assert!(source_contains_in_order(
        &setter,
        &[
            "move.w 0(a0, d2.l), d0",
            "andi.w #OPASM_ENGINE_STMT_PARSER_KIND_MASK, d0",
            "beq.s store",
            "cmpi.w #OPASM_ENGINE_STMT_KIND_GENERIC, d0",
            "bne.s fail",
            "ori.w #OPASM_ENGINE_STMT_RESERVATION_FLAG, 0(a0, d2.l)",
            "move.l d1, 0(a0, d2.l)",
        ]
    ));
    let public_kind = routine_body(&engine, "opasmEngineGetStatementKindV1")
        .expect("public parser-kind getter body");
    assert!(public_kind.contains("andi.w #OPASM_ENGINE_STMT_PARSER_KIND_MASK, d0"));
    let reservation = routine_body(&engine, "opasmEngineGetStatementReservationV1")
        .expect("reservation getter body");
    assert!(source_contains_in_order(
        &reservation,
        &[
            "tst.w 0(a0, d2.l)",
            "bpl.s absent",
            "move.l 0(a0, d2.l), d1",
        ]
    ));
    let emitted = routine_body(&engine, "opasmEngineGetStatementOutputByteCountV1")
        .expect("emitted-byte getter body");
    assert!(source_contains_in_order(
        &emitted,
        &["tst.w 0(a0, d1.l)", "bmi.s noBytes"]
    ));
    let reset =
        routine_body(&engine, "clearStatementReservationsV1").expect("reservation reset body");
    assert!(source_contains_in_order(
        &reset,
        &[
            "tst.w (a0)",
            "bpl.s next",
            "andi.w #OPASM_ENGINE_STMT_PARSER_KIND_MASK, (a0)",
            "clr.l 0(a1, d1.l)",
            "clr.l 0(a1, d1.l)",
        ]
    ));
    for pass in ["runPassOne", "runPassTwo"] {
        let body = engine
            .split(&format!("{pass}\t.block"))
            .nth(1)
            .expect("pass body");
        assert!(source_contains_in_order(
            body,
            &[
                "bsr.w clearStatementReservationsV1",
                "OPASM_ENGINE_CTX_FLOW_CONTROL_CB"
            ]
        ));
    }
    assert_eq!(
        engine
            .matches("bsr.w opasmEngineGetStatementKindV1")
            .count(),
        2
    );
    assert!(source_contains_in_order(
        &driver,
        &[
            "res",
            "bsr.w evaluateResOperandSlice",
            "move.l d3, d5",
            "divu.l d5, d2:d3",
            "mulu.l d4, d3",
            "resExtentReady",
            "subq.l #1, d3",
            "add.l d2, d3",
            "bcs.w resSpanInvalid",
            "addq.l #1, d3",
            "bcs.w resPcInvalid",
            "cmp.l d5, d3",
            "bhi.w resPcInvalid",
            "jsr eng.opasmEngineSetStatementReservationV1",
            "move.l d4, d3",
            "bra.w advanceLayoutD3",
        ]
    ));
    let evaluator =
        routine_body(&driver, "evaluateResOperandSlice").expect("typed reservation evaluator body");
    assert!(source_contains_in_order(
        &evaluator,
        &[
            "bsr.w prepareEvaluateExpressionRequest",
            "jsr operand_eval.prepareExpressionExtensionV1",
            "tst.l d0",
            "jsr tkpkg.dispatchEvaluateExpressionV1",
            "tst.b d0",
            "cmpi.w #28, abi.OPASM_SERVICE_EVAL_EXTENSION_BYTES(a0)",
            "movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a0",
            "cmpi.l #64, 24(a0)",
            "tst.l 20(a0)",
            "move.l 16(a0), d3",
        ]
    ));
    for forbidden_fallback in [
        "parseDirectiveLiteralValue",
        "parseEvaluateExpressionOutputValue",
        "OpasmDriverLastEvalWasNegative",
        "OpasmDriverLastEvalOutOfRange",
    ] {
        assert!(!evaluator.contains(forbidden_fallback));
    }
}

#[test]
fn native_listing_reservation_bin_and_listing_fs_uae() {
    // Level D when enabled: exact whole same-package Rust artifacts, fresh
    // challenge/completion and explicit zero exit. Does not prove B09 or speed.
    let specs = [
        ("byte", "6502", "byte", "4", "$80ff", "+0004"),
        ("word", "68000", "word", "2", "$80ff", "+0004"),
        ("long", "68000", "long", "1", "$80ff", "+0004"),
        ("numeric", "68000", "1+2", "2", "$80ff", "+0006"),
        ("zero", "68000", "byte", "0", "$80ff", "+0000"),
        ("wide6", "68020", "byte", "$10000", "$2ffff", "+010000"),
        (
            "wide8",
            "68020",
            "byte",
            "$1000000",
            "$2ffffff",
            "+01000000",
        ),
        (
            "positive-high-bit-unit-zero-count",
            "68020",
            "$80000000+0",
            "0",
            "$80ff",
            "+0000",
        ),
        (
            "wrapped-wide-unit-zero-count",
            "68020",
            "($ffffffff+1)-1",
            "0",
            "$80ff",
            "+0000",
        ),
    ];
    run_reservation_listing_cases(&specs);
}

#[test]
fn native_listing_reservation_final_binary_fs_uae() {
    // Level D when enabled: the original complete byte/count-four fixture is
    // the focused final-linkage proof after an equivalent CCR cleanup.
    run_reservation_listing_cases(&[("final-byte-four", "6502", "byte", "4", "$80ff", "+0004")]);
}

fn run_reservation_listing_cases(specs: &[(&str, &str, &str, &str, &str, &str)]) {
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native coordinator");
    let root = workspace_root();
    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped package");
    let sources = specs.iter().map(|(_, cpu, unit, count, end, _)| format!(
        ".module main\n.cpu {cpu}\n.region rom, $8000, {end}\n.section code, align=1\n.byte $aa\n.endsection\n.section zero, kind=bss, align=1\n.res {unit}, {count}\n.endsection\n.pack in rom : code,zero\n.endmodule\n"
    )).collect::<Vec<_>>();
    let oracles = sources
        .iter()
        .zip(specs.iter())
        .map(|(source, spec)| {
            let oracle = build_reservation_listing_oracle(source.as_bytes(), "reservation");
            assert_eq!(oracle.bin, [0xaa], "{}: BSS must not emit bytes", spec.0);
            let listing = std::str::from_utf8(&oracle.listing).expect("Rust listing UTF-8");
            let row = listing
                .lines()
                .find(|row| row.contains(".res "))
                .expect("RES row");
            assert!(row.starts_with("8001"), "{row}");
            assert!(row.contains(spec.5), "{row}");
            oracle
        })
        .collect::<Vec<_>>();
    let expected = oracles
        .iter()
        .map(|oracle| {
            [
                crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
                    relative_path: "Work/build/reservation.bin",
                    rust_oracle: &oracle.bin,
                },
                crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
                    relative_path: "Work/build/reservation.lst",
                    rust_oracle: &oracle.listing,
                },
            ]
        })
        .collect::<Vec<_>>();
    let cases = specs.iter().zip(sources.iter()).zip(expected.iter()).map(|((spec, source), expected)|
        crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: spec.0,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source.as_bytes()),
            command_template: Some("{input} --bin {guest_work_dir}build/reservation.bin --list {guest_work_dir}build/reservation.lst --cpu 6502 --opasm-package {package}"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifacts(expected),
        }
    ).collect::<Vec<_>>();
    // The production batch runner attempts every case and aggregates errors.
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &cases)
        .expect("reservation listing native proof")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), cases.len());
            for (run, oracle) in runs.iter().zip(&oracles) {
                assert!(run.protocol_completed && run.success);
                assert_eq!(run.exit_code, Some(0));
                assert_eq!(
                    captured_fs_uae_artifact(run, "Work/build/reservation.bin"),
                    oracle.bin
                );
                assert_eq!(
                    captured_fs_uae_artifact(run, "Work/build/reservation.lst"),
                    oracle.listing
                );
            }
        }
    }
}

#[test]
fn native_listing_reservation_full_u32_counts_fs_uae() {
    // Level D when enabled: complete mixed CODE+BSS sources prove that accepted
    // compound expressions retain their full U32 count through native
    // placement and listing while the placed CODE byte remains the exact BIN.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("native coordinator");
    let root = workspace_root();
    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped package");
    let specs = [
        (
            "positive-high-bit-count",
            "68020",
            "0",
            "$80000000+0",
            "0000",
            "+80000000",
        ),
        (
            "wrapped-maximum-count",
            "68020",
            "0",
            "($ffffffff+1)-1",
            "0000",
            "+FFFFFFFF",
        ),
    ];
    let sources = specs
        .iter()
        .map(|(_, cpu, origin, count, _, _)| {
            format!(
                ".module main\n.cpu {cpu}\n.region rom, $0000, $00ff\n.section code, align=1\n.byte $aa\n.endsection\n.section zero, kind=bss, align=1\n.org {origin}\n.res byte, {count}\n.endsection\n.place code in rom\n.endmodule\n"
            )
        })
        .collect::<Vec<_>>();
    let oracles = sources
        .iter()
        .zip(specs.iter())
        .map(|(source, spec)| {
            let oracle = build_reservation_listing_oracle(source.as_bytes(), spec.0);
            assert_eq!(oracle.bin, [0xaa], "{}: exact placed CODE byte", spec.0);
            let listing = std::str::from_utf8(&oracle.listing).expect("Rust listing UTF-8");
            let row = listing
                .lines()
                .find(|row| row.contains(".res "))
                .expect("RES row");
            assert!(row.starts_with(spec.4), "{row}");
            assert!(row.contains(spec.5), "{row}");
            oracle
        })
        .collect::<Vec<_>>();
    let expected = oracles
        .iter()
        .map(|oracle| {
            [
                crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
                    relative_path: "Work/build/reservation.bin",
                    rust_oracle: &oracle.bin,
                },
                crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
                    relative_path: "Work/build/reservation.lst",
                    rust_oracle: &oracle.listing,
                },
            ]
        })
        .collect::<Vec<_>>();
    let cases = specs
        .iter()
        .zip(sources.iter())
        .zip(expected.iter())
        .map(|((spec, source), expected)| crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name: spec.0,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source.as_bytes()),
            command_template: Some("{input} --bin {guest_work_dir}build/reservation.bin --list {guest_work_dir}build/reservation.lst --cpu 68020 --opasm-package {package}"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifacts(expected),
        })
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &cases)
        .expect("full-U32 reservation native proof")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), cases.len());
            for (run, oracle) in runs.iter().zip(&oracles) {
                assert!(run.protocol_completed && run.success);
                assert_eq!(run.exit_code, Some(0));
                assert_eq!(
                    captured_fs_uae_artifact(run, "Work/build/reservation.bin"),
                    oracle.bin
                );
                assert_eq!(
                    captured_fs_uae_artifact(run, "Work/build/reservation.lst"),
                    oracle.listing
                );
            }
        }
    }
}

#[test]
fn native_listing_reservation_negative_cases_fs_uae() {
    // Level D: fresh explicit guest failure with the named production diagnostic.
    // Does not prove positive artifact parity or all expression semantics.
    // The grouped-keyword case characterizes a known native rejection gap:
    // Rust accepts grouping here. Its native failure is not Rust parity.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let root = workspace_root();
    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped package");
    let cases = [
        ("unit-zero", b".module main\n.cpu 68000\n.section bss, kind=bss\n.res 0, 2\n.endsection\n.endmodule\n".as_slice(), "greater than zero"),
        ("negative-unit", b".module main\n.cpu 68000\n.section bss, kind=bss\n.res -1, 2\n.endsection\n.endmodule\n".as_slice(), "non-negative"),
        ("grouped-keyword-unit", b".module main\n.cpu 68000\n.section bss, kind=bss\n.res (WoRd), 2\n.endsection\n.endmodule\n".as_slice(), "Expected byte, word, long, or a non-negative value for .res unit"),
        ("negative-count", b".module main\n.cpu 68000\n.section bss, kind=bss\n.res byte, -1\n.endsection\n.endmodule\n".as_slice(), "non-negative"),
        ("minimum-i32-count", b".module main\n.cpu 68020\n.section bss, kind=bss\n.res byte, -2147483648\n.endsection\n.endmodule\n".as_slice(), "non-negative"),
        ("wide-count-overflow", b".module main\n.cpu 68020\n.section bss, kind=bss\n.res byte, $ffffffff+1\n.endsection\n.endmodule\n".as_slice(), "Expected non-negative value for .res count"),
        ("overflow", b".module main\n.cpu 68000\n.section bss, kind=bss\n.res $ffffffff, 2\n.endsection\n.endmodule\n".as_slice(), "overflow"),
        ("span", b".module main\n.cpu 6502\n.region rom, $0000, $ffff\n.section bss, kind=bss\n.org $ffff\n.res byte, 2\n.endsection\n.place bss in rom\n.endmodule\n".as_slice(), ".res span"),
        ("next-pc", b".module main\n.cpu 6502\n.section edge, kind=bss\n.org $ffff\n.res byte, 1\n.endsection\n.endmodule\n".as_slice(), ".res program counter exceeds max"),
    ];
    let native_cases = cases
        .iter()
        .map(
            |(name, source, diagnostic)| crate::fs_uae_smoke::OpforgeNativeCliParityCase {
                name,
                cpu_override: "68020",
                extra_assembly_defines: &[],
                source_override: Some(*source),
                command_template: Some(
                    "{input} --bin {bin} --cpu m68000 --opasm-package {package}",
                ),
                package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
                extra_guest_files: &[],
                proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExpectedFailureContaining(
                    diagnostic,
                ),
            },
        )
        .collect::<Vec<_>>();
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &native_cases)
        .expect("reservation negative FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), cases.len());
            for run in runs {
                assert!(run.protocol_completed);
                assert!(!run.success);
                assert!(run.exit_code.is_some_and(|code| code != 0));
            }
        }
    }
}

struct ReservationListingOracle {
    bin: Vec<u8>,
    listing: Vec<u8>,
}

fn build_reservation_listing_oracle(source: &[u8], stem: &str) -> ReservationListingOracle {
    let oracle_dir = create_temp_dir("native-listing-reservation-oracle");
    struct OracleDir(std::path::PathBuf);
    impl Drop for OracleDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _oracle_guard = OracleDir(oracle_dir.clone());
    fs::create_dir_all(oracle_dir.join("build")).expect("create reservation oracle directory");
    let input = oracle_dir.join("input.asm");
    let bin = oracle_dir.join(format!("build/{stem}.bin"));
    let listing = oracle_dir.join(format!("build/{stem}.lst"));
    fs::write(&input, source).expect("write reservation oracle source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--bin".to_string(),
        bin.to_string_lossy().into_owned(),
        "--list".to_string(),
        listing.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "6502".to_string(),
        "--opasm-package".to_string(),
        workspace_root()
            .join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).expect("validate reservation Rust CLI");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("run reservation Rust CLI");
    ReservationListingOracle {
        bin: fs::read(bin).expect("read reservation Rust BIN"),
        listing: fs::read(listing).expect("read reservation Rust listing"),
    }
}
