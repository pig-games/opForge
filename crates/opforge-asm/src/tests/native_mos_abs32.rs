// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

#[test]
fn serialized_mos_abs32_fixup_matches_live_cross_section_long_oracle() {
    // Level B: serialized family program versus a live Rust relocation oracle.
    // Does not prove native execution or timing.
    let assembler = run_passes(&[
        ".module main",
        ".cpu 6502",
        ".region ram, $2000, $20ff",
        ".section code, kind=code",
        "pointer: .long target + 5",
        ".endsection",
        ".section data, kind=data",
        ".byte $aa",
        "target: .byte 0",
        ".endsection",
        ".pack in ram : code, data",
        ".output \"build/out.hunk\", format=hunk, sections=code,data",
        ".endmodule",
    ]);
    let code = assembler.sections().get("code").expect("code section");
    let [oracle] = code.output_fixups.as_slice() else {
        panic!("live Rust assembler must produce one relocation oracle")
    };
    assert_ne!(oracle.encoded_addend, 0);
    let offset = usize::try_from(oracle.offset).expect("host offset");
    assert_eq!(
        &code.bytes[offset..offset + 4],
        &oracle.encoded_addend.to_le_bytes()
    );

    let package_bytes = build_hierarchy_package_from_registry(&default_registry())
        .expect("serialize default package");
    let model = load_opasm_model_from_package_bytes(&package_bytes);
    let resolved = model
        .resolve_pipeline("m6502", None)
        .expect("resolve serialized m6502 package");
    let projected = model
        .execute_fixup_program(
            &resolved,
            families::mos6502::package_programs::FIXUP_ABSOLUTE_LONG,
            &[vm::fixup_vm::PortableFixupInput {
                value: vm::fixup_vm::PortableDeferredValue::Resolved(i64::from(
                    oracle.encoded_addend,
                )),
                target_reference: true,
                relocation_target: oracle.target_section_name().map(str::to_string),
            }],
            vm::fixup_vm::PortableFixupContext { position: 0x2000 },
        )
        .expect("execute serialized MOS absolute-long fixup program");

    assert_eq!(projected.bytes, code.bytes);
    let [fixup] = projected.fixups.as_slice() else {
        panic!("package program must produce one portable relocation")
    };
    assert_eq!(fixup.offset, oracle.offset);
    assert_eq!(fixup.width, 4);
    assert_eq!(fixup.kind, vm::fixup_vm::PortableOutputFixupKind::Absolute);
    assert_eq!(fixup.target, oracle.target_section_name().unwrap());
    assert_eq!(fixup.encoded_addend, oracle.encoded_addend);

    // Native re-encodes the normalized scalar without a target reference.
    // This must preserve the exact bytes without creating a second relocation.
    let normalized = model
        .execute_fixup_program(
            &resolved,
            families::mos6502::package_programs::FIXUP_ABSOLUTE_LONG,
            &[vm::fixup_vm::PortableFixupInput {
                value: vm::fixup_vm::PortableDeferredValue::Resolved(i64::from(
                    oracle.encoded_addend,
                )),
                target_reference: false,
                relocation_target: None,
            }],
            vm::fixup_vm::PortableFixupContext { position: 0x2000 },
        )
        .expect("encode section-relative scalar without another relocation");
    assert_eq!(normalized.bytes, projected.bytes);
    assert!(normalized.fixups.is_empty());
}

#[test]
fn native_mos_abs32_long_fs_uae() {
    // Proof level D. One fresh guest proves the real native CLI applies the
    // serialized MOS absolute-long role to both BIN and relocation-bearing Hunk.
    let _guard = fs_uae_native_cli_smoke_lock()
        .lock()
        .expect("recovering native CLI coordinator");
    let root = workspace_root();
    let source = b".module main\n.cpu 6502\n.region ram, $8000, $80ff\n.section code, kind=code\n.long target\n.endsection\n.section data, kind=data\n.byte $aa\ntarget: .byte 0\n.endsection\n.pack in ram : code, data\n.output \"build/mos-abs32.hunk\", format=hunk, sections=code,data\n.endmodule\n";
    let oracle_dir = create_temp_dir("native-mos-abs32-oracle");
    struct OracleDir(std::path::PathBuf);
    impl Drop for OracleDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _oracle_guard = OracleDir(oracle_dir.clone());
    fs::create_dir_all(oracle_dir.join("build")).expect("create MOS absolute-long oracle dir");
    let input_path = oracle_dir.join("input.asm");
    let bin_path = oracle_dir.join("build/mos-abs32.bin");
    fs::write(&input_path, source).expect("write MOS absolute-long oracle source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input_path.to_string_lossy().into_owned(),
        "--bin".to_string(),
        bin_path.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "m6502".to_string(),
    ]);
    let mut config = validate_cli(&cli).expect("validate MOS absolute-long Rust CLI");
    config.out_dir = Some(oracle_dir.clone());
    run_with_validated_cli_with_context(&cli, &config)
        .expect("run MOS absolute-long live Rust CLI oracle");
    let rust_bin = fs::read(&bin_path).expect("read MOS absolute-long Rust BIN oracle");
    let rust_hunk = fs::read(oracle_dir.join("build/mos-abs32.hunk"))
        .expect("read MOS absolute-long Rust Hunk oracle");
    // Rust encodes section-relative addends for symbolic long directives.
    assert_eq!(&rust_bin[..4], &[0x01, 0x00, 0x00, 0x00]);

    let package =
        fs::read(root.join("native/motorola68000/amigaos/opforge-cli/opforge_cli_package.opasm"))
            .expect("read shipped candidate package");
    let expected = [
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/mos-abs32.bin",
            rust_oracle: &rust_bin,
        },
        crate::fs_uae_smoke::OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/mos-abs32.hunk",
            rust_oracle: &rust_hunk,
        },
    ];
    let case = crate::fs_uae_smoke::OpforgeNativeCliParityCase {
        name: "mos_abs32_long",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source),
        command_template: Some(
            "{input} --bin {guest_work_dir}build/mos-abs32.bin --cpu m6502 --opasm-package {package}",
        ),
        package_mode: crate::fs_uae_smoke::OpforgeNativeCliPackageMode::Explicit(&package),
        extra_guest_files: &[],
        proof: crate::fs_uae_smoke::OpforgeNativeCliProof::ExactArtifacts(&expected),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_parity_cases_from_env(&root, &[case])
        .expect("MOS absolute-long FS-UAE helper")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1, "one MOS absolute-long run");
            let run = &runs[0];
            assert!(run.protocol_completed);
            assert!(run.success);
            assert_eq!(run.exit_code, Some(0));
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/mos-abs32.bin"),
                rust_bin
            );
            assert_eq!(
                captured_fs_uae_artifact(run, "Work/build/mos-abs32.hunk"),
                rust_hunk
            );
        }
    }
}
