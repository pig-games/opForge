//! BSS image-origin and alignment routing parity contracts.

use super::*;

const UNPLACED_SMALL: &[u8] = b".module main\n.cpu 68020\n.region rom, $8000, $80ff\n.section code, align=1\n.byte $aa\n.endsection\n.section zero, kind=bss, align=1\n.org 0\n.res byte, 4\n.endsection\n.place code in rom\n.endmodule\n";
const PLACED_SMALL: &[u8] = b".module main\n.cpu 68020\n.region rom, $8000, $80ff\n.region ram, $0000, $00ff\n.section code, align=1\n.byte $aa\n.endsection\n.section zero, kind=bss, align=1\n.org 0\n.res byte, 4\n.endsection\n.place code in rom\n.place zero in ram\n.endmodule\n";
const UNPLACED_FULL_U32: &[u8] = b".module main\n.cpu 68020\n.region rom, $8000, $80ff\n.section code, align=1\n.byte $aa\n.endsection\n.section zero, kind=bss, align=1\n.org 0\n.res byte, ($ffffffff+1)-1\n.endsection\n.place code in rom\n.endmodule\n";
const PLACED_ALIGNED: &[u8] = b".module main\n.cpu 68020\n.region rom, $8000, $80ff\n.region ram, $0000, $00ff\n.section code, align=1\n.byte $aa\n.endsection\n.section zero, kind=bss, align=4\n.org 0\n.res byte, 1\n.align 4\n.endsection\n.place code in rom\n.place zero in ram\n.endmodule\n";
const ORDINARY_ORIGINS: &[u8] = b".module main\n.cpu 68020\n.org $1000\n.byte $11\n.org $1002\n.byte $22\n.org $1001\n.byte $33\n.endmodule\n";
const MAPPED_LOGICAL: &[u8] =
    include_bytes!("../../../../examples/opcore/module_qualified_section_map.asm");

const EXPECTED_CODE_ONLY: &[u8] = &[0xaa];
const EXPECTED_ORDINARY_ORIGINS: &[u8] = &[0x11, 0x33, 0x22];
const EXPECTED_MAPPED_LOGICAL: &[u8] = &[0x20, 0x00, 0x00, 0x60, 0x60];

#[test]
fn native_bss_image_origin_live_rust_bss_align_pad() {
    // Level A: the Rust directive owner records three padding bytes after a
    // one-byte BSS reservation at address zero. It emits no image bytes.
    let mut symbols = SymbolTable::new();
    let registry = default_registry();
    let mut asm = make_asm_line(&mut symbols, &registry);
    assert_eq!(process_line(&mut asm, ".cpu 68020", 0, 1), LineStatus::Ok);
    assert_eq!(
        process_line(&mut asm, ".section zero, kind=bss", 0, 1),
        LineStatus::Ok
    );
    assert_eq!(
        process_line(&mut asm, ".res byte, 1", 0, 1),
        LineStatus::DirDs
    );
    assert_eq!(asm.aux_value(), 1);
    assert!(asm.bytes().is_empty());
    assert_eq!(process_line(&mut asm, ".align 4", 1, 1), LineStatus::DirDs);
    assert_eq!(asm.aux_value(), 3);
    assert!(asm.bytes().is_empty());
}

#[test]
fn native_bss_image_origin_live_rust_bin_oracles() {
    // Level A: complete live Rust CLI runs own the exact BIN for each source.
    // This does not prove native routing or native execution.
    for (name, source, expected) in case_specs() {
        assert_eq!(build_bin_oracle(source, name), expected, "{name}");
    }
}

#[test]
fn native_bss_image_origin_unplaced_small_fs_uae() {
    // Level D when enabled: the representative complete source proves a BSS
    // origin below placed CODE cannot create a flat-image gap.
    run_native_cases(&[("unplaced-small", UNPLACED_SMALL, EXPECTED_CODE_ONLY)]);
}

#[test]
fn native_bss_image_origin_matrix_fs_uae() {
    // Level D when enabled: attempt-all proof covers placed and unplaced BSS,
    // a full-U32 reservation, nonzero BSS alignment, and ordinary origin
    // overwrite behavior. It does not prove mapped-logical routing,
    // listing/footer parity, or speed.
    run_native_cases(&[
        ("placed-small", PLACED_SMALL, EXPECTED_CODE_ONLY),
        ("unplaced-full-u32", UNPLACED_FULL_U32, EXPECTED_CODE_ONLY),
        ("placed-aligned", PLACED_ALIGNED, EXPECTED_CODE_ONLY),
        (
            "ordinary-forward-backward-origins",
            ORDINARY_ORIGINS,
            EXPECTED_ORDINARY_ORIGINS,
        ),
    ]);
}

#[test]
fn native_bss_image_origin_mapped_logical_control_fs_uae() {
    // Level D when enabled: this separate regression control binds the
    // established `.use ... map` source to its exact live Rust BIN. It does
    // not prove BSS-origin suppression or classify an observed failure.
    run_native_cases(&[(
        "mapped-logical-non-bss",
        MAPPED_LOGICAL,
        EXPECTED_MAPPED_LOGICAL,
    )]);
}

fn case_specs() -> [(&'static str, &'static [u8], &'static [u8]); 6] {
    [
        ("unplaced-small", UNPLACED_SMALL, EXPECTED_CODE_ONLY),
        ("placed-small", PLACED_SMALL, EXPECTED_CODE_ONLY),
        ("unplaced-full-u32", UNPLACED_FULL_U32, EXPECTED_CODE_ONLY),
        ("placed-aligned", PLACED_ALIGNED, EXPECTED_CODE_ONLY),
        (
            "ordinary-forward-backward-origins",
            ORDINARY_ORIGINS,
            EXPECTED_ORDINARY_ORIGINS,
        ),
        (
            "mapped-logical-non-bss",
            MAPPED_LOGICAL,
            EXPECTED_MAPPED_LOGICAL,
        ),
    ]
}

fn run_native_cases(specs: &[(&str, &[u8], &[u8])]) {
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let root = workspace_root();
    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped package");
    let oracles = specs
        .iter()
        .map(|(name, source, literal)| {
            let oracle = build_bin_oracle(source, name);
            assert_eq!(&oracle, literal, "{name}: live Rust BIN authority");
            oracle
        })
        .collect::<Vec<_>>();
    let expected = oracles
        .iter()
        .map(|oracle| {
            [crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/build/bss-origin.bin",
                rust_oracle: oracle,
            }]
        })
        .collect::<Vec<_>>();
    let cases = specs
        .iter()
        .zip(expected.iter())
        .map(|((name, source, _), expected)| crate::fs_uae_smoke::OpforgeNativeCliParityCase {
            name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source),
            command_template: Some("{input} --bin {guest_work_dir}build/bss-origin.bin --cpu 68020 --opasm-package {package}"),
            package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
            extra_guest_files: &[],
            proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifacts(expected),
        })
        .collect::<Vec<_>>();

    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &cases)
        .expect("BSS image-origin native proof")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), cases.len());
            for (run, oracle) in runs.iter().zip(&oracles) {
                assert!(run.protocol_completed && run.success);
                assert_eq!(run.exit_code, Some(0));
                assert_eq!(
                    captured_fs_uae_artifact(run, "Work/build/bss-origin.bin"),
                    *oracle
                );
            }
        }
    }
}

fn build_bin_oracle(source: &[u8], stem: &str) -> Vec<u8> {
    let oracle_dir = create_temp_dir("native-bss-image-origin-oracle");
    struct OracleDir(std::path::PathBuf);
    impl Drop for OracleDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _oracle_guard = OracleDir(oracle_dir.clone());
    fs::create_dir_all(oracle_dir.join("build")).expect("create BSS-origin oracle directory");
    let input = oracle_dir.join("input.asm");
    let bin = oracle_dir.join(format!("build/{stem}.bin"));
    fs::write(&input, source).expect("write BSS-origin oracle source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--bin".to_string(),
        bin.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "68020".to_string(),
        "--opasm-package".to_string(),
        workspace_root()
            .join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).expect("validate BSS-origin Rust CLI");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("run BSS-origin live Rust CLI");
    fs::read(bin).expect("read BSS-origin Rust BIN oracle")
}
