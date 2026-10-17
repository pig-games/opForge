//! Preserve independently built observer and reference images; no emulator launch.
use super::native_iterable_loops::scratch;
use super::*;
use crate::native_package_build::{build_native_packages, EmbedSelection};

fn copy_tree(from: &Path, to: &Path) {
    fs::create_dir_all(to).unwrap();
    for entry in fs::read_dir(from).unwrap() {
        let entry = entry.unwrap();
        let target = to.join(entry.file_name());
        if entry.file_type().unwrap().is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            fs::copy(entry.path(), target).unwrap();
        }
    }
}

fn prepare_component_probe_source(source_root: &Path, snapshot: &Path) {
    prepare_component_probe_source_for(
        source_root,
        snapshot,
        "tests.amigaos.binary_lazy_audit_probe",
    );
}

fn prepare_component_probe_source_for(source_root: &Path, snapshot: &Path, probe_module: &str) {
    copy_tree(&source_root.join("native"), &snapshot.join("native"));
    let app = snapshot.join("native/motorola68000/amigaos/experimental/binary_app.asm");
    let text = fs::read_to_string(&app).unwrap();
    let import = "\t.use experimental.amigaos.binary_assembly as assembly\n";
    assert_eq!(text.matches(import).count(), 1);
    let text = text.replace(
        import,
        &format!("{import}\t.use {probe_module} as lazyProbe\n"),
    );
    let entry = "prepare\t.block\n";
    assert_eq!(text.matches(entry).count(), 1);
    fs::write(app, text.replace(entry, &format!("{entry}\tjsr lazyProbe.run\n\tmove.l d0, PackageStatus\n\tbne.w bad\n\t.LAZY_CLEAR\n\t.WORK_CALL lazy_work.clear\n"))).unwrap();
}

#[test]
fn native_lazy_audit_probe_injection_preserves_reference_source() {
    let dir = scratch();
    let source = dir.0.join("reference");
    let snapshot = dir.0.join("probe");
    let relative = "native/motorola68000/amigaos/experimental/binary_app.asm";
    let app = source.join(relative);
    fs::create_dir_all(app.parent().unwrap()).unwrap();
    let original =
        "\t.use experimental.amigaos.binary_assembly as assembly\nprepare\t.block\n\trts\n";
    fs::write(&app, original).unwrap();
    prepare_component_probe_source(&source, &snapshot);
    assert_eq!(fs::read_to_string(app).unwrap(), original);
    let injected = fs::read_to_string(snapshot.join(relative)).unwrap();
    assert_eq!(
        injected
            .matches("tests.amigaos.binary_lazy_audit_probe as lazyProbe")
            .count(),
        1
    );
    assert_eq!(injected.matches("jsr lazyProbe.run").count(), 1);
    assert!(injected.contains("prepare\t.block\n\tjsr lazyProbe.run\n\tmove.l d0, PackageStatus\n\tbne.w bad\n\t.LAZY_CLEAR\n\t.WORK_CALL lazy_work.clear\n"));
    let lifetime_snapshot = dir.0.join("lifetime-probe");
    prepare_component_probe_source_for(
        &source,
        &lifetime_snapshot,
        "tests.amigaos.capture_lifetime_probe",
    );
    let lifetime = fs::read_to_string(lifetime_snapshot.join(relative)).unwrap();
    assert!(lifetime.contains(".use tests.amigaos.capture_lifetime_probe as lazyProbe"));
    assert!(!lifetime.contains("tests.amigaos.binary_lazy_audit_probe"));
    let disk_snapshot = dir.0.join("disk-probe");
    prepare_component_probe_source_for(
        &source,
        &disk_snapshot,
        "tests.amigaos.capture_disk_store_probe",
    );
    let disk = fs::read_to_string(disk_snapshot.join(relative)).unwrap();
    assert!(disk.contains(".use tests.amigaos.capture_disk_store_probe as lazyProbe"));
    assert_eq!(fs::read_to_string(source.join(relative)).unwrap(), original);
}

fn tree_digest(root: &Path) -> String {
    fn visit(root: &Path, directory: &Path, contents: &mut Vec<u8>) {
        let mut entries: Vec<_> = fs::read_dir(directory)
            .unwrap()
            .map(|entry| entry.unwrap())
            .collect();
        entries.sort_by_key(|entry| entry.file_name());
        for entry in entries {
            if entry.file_type().unwrap().is_dir() {
                visit(root, &entry.path(), contents);
            } else {
                contents.extend_from_slice(
                    entry
                        .path()
                        .strip_prefix(root)
                        .unwrap()
                        .to_string_lossy()
                        .as_bytes(),
                );
                contents.push(0);
                let bytes = fs::read(entry.path()).unwrap();
                contents.extend_from_slice(&(bytes.len() as u64).to_be_bytes());
                contents.extend_from_slice(&bytes);
            }
        }
    }
    let mut contents = Vec::new();
    visit(root, root, &mut contents);
    opforge_self_host_package_digest(&contents)
}

#[test]
#[ignore = "builds the independent capture-store component probe outside target"]
fn native_capture_store_probe_save_image() {
    save_capture_store_probe(
        "tests.amigaos.capture_store_probe",
        "OPFORGE_CAPTURE_STORE_SAVE_DIR",
        "capture-store-probe-memory.hunk",
    );
}

#[test]
#[ignore = "builds the independent disk capture-store component probe outside target"]
fn native_capture_disk_store_probe_save_image() {
    save_capture_store_probe(
        "tests.amigaos.capture_disk_store_probe",
        "OPFORGE_CAPTURE_DISK_STORE_SAVE_DIR",
        "capture-disk-store-probe-memory.hunk",
    );
}

#[test]
#[ignore = "builds the independent ordered-record component probe outside target"]
fn native_ordered_records_probe_save_image() {
    save_capture_store_probe(
        "tests.amigaos.ordered_records_probe",
        "OPFORGE_ORDERED_RECORDS_SAVE_DIR",
        "ordered-records-probe-memory.hunk",
    );
}

#[test]
#[ignore = "builds the independent record-growth component probe outside target"]
fn native_record_growth_probe_save_image() {
    save_capture_store_probe(
        "tests.amigaos.record_growth_probe",
        "OPFORGE_RECORD_GROWTH_SAVE_DIR",
        "record-growth-probe-memory.hunk",
    );
}

fn save_capture_store_probe(probe_module: &str, destination_variable: &str, filename: &str) {
    let root = super::native_fragment_controls::root();
    let destination = PathBuf::from(
        std::env::var_os(destination_variable)
            .unwrap_or_else(|| panic!("set {destination_variable} outside target")),
    );
    fs::create_dir_all(&destination).unwrap();
    let destination = destination.canonicalize().unwrap();
    assert!(!destination.starts_with(root.join("target")));
    let dir = scratch();
    let snapshot = dir.0.join("probe-source");
    prepare_component_probe_source_for(&root, &snapshot, probe_module);
    // Fault injection belongs only to this isolated component-test snapshot.
    // Replace the transport call, retaining the helper's actual failure branch.
    let growth_fault = if probe_module == "tests.amigaos.record_growth_probe" {
        std::env::var_os("OPFORGE_RECORD_GROWTH_FAULT").map(|value| {
            let mode = match value.to_str() {
                Some("write") => "write",
                Some("read") => "read",
                _ => panic!("OPFORGE_RECORD_GROWTH_FAULT must be write or read"),
            };
            let helper =
                snapshot.join("native/motorola68000/amigaos/experimental/binary_record_growth.asm");
            let source = fs::read_to_string(&helper).unwrap();
            let call = format!("\tjsr io.{mode}\n");
            assert_eq!(source.matches(&call).count(), 1, "expected one {mode} call");
            fs::write(&helper, source.replacen(&call, "\tmoveq #1, d0\n", 1)).unwrap();
            mode
        })
    } else {
        None
    };
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &dir.0.join("probe-build"),
        &snapshot.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["68020".into(), "6502".into()]),
    )
    .unwrap();
    let mut defines = vec![
        "OPFORGE_DEBUG_CONTRACTS",
        "OPFORGE_MEMORY_TELEMETRY",
        "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
        "OPFORGE_PREPARATION_PROGRESS",
        "OPFORGE_CAPTURE_TELEMETRY",
    ];
    if probe_module == "tests.amigaos.ordered_records_probe"
        && std::env::var_os("OPFORGE_ORDERED_RECORDS_EXPECT_REUSE").is_some()
    {
        defines.push("OPFORGE_ORDERED_RECORDS_EXPECT_REUSE");
    }
    match growth_fault {
        Some("write") => defines.push("OPFORGE_GROWTH_EXPECT_WRITE_FAILURE"),
        Some("read") => defines.push("OPFORGE_GROWTH_EXPECT_READ_FAILURE"),
        None => {}
        Some(_) => unreachable!("growth fault was validated above"),
    }
    let image = super::compact_cli_input::assemble_cli_with_defines(&snapshot, &build, &defines);
    let digest = tree_digest(&snapshot.join("native"));
    fs::write(destination.join(filename), &image).unwrap();
    fs::write(
        destination.join("images.json"),
        serde_json::to_vec_pretty(
            &serde_json::json!({"native_source_fnv1a64": digest, "images": [{
                "file": filename, "defines": defines, "bytes": image.len(),
                "record_growth_fault": growth_fault,
                "fnv1a64": opforge_self_host_package_digest(&image),
                "native_source_fnv1a64": digest,
            }]}),
        )
        .unwrap(),
    )
    .unwrap();
}

#[test]
fn native_lazy_audit_disabled_macros_emit_zero_bytes() {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let dir = scratch();
    fs::create_dir_all(dir.0.join("build")).unwrap();
    fs::copy(
        super::native_fragment_controls::root()
            .join("native/motorola68000/amigaos/debug/lazy_audit.i"),
        dir.0.join("lazy_audit.i"),
    )
    .unwrap();
    fs::copy(
        super::native_fragment_controls::root()
            .join("native/motorola68000/amigaos/debug/lazy_work.i"),
        dir.0.join("lazy_work.i"),
    )
    .unwrap();
    fs::copy(
        super::native_fragment_controls::root()
            .join("native/motorola68000/amigaos/debug/lazy_work_context.i"),
        dir.0.join("lazy_work_context.i"),
    )
    .unwrap();
    fs::copy(
        super::native_fragment_controls::root()
            .join("native/motorola68000/amigaos/debug/lazy_template_hooks.i"),
        dir.0.join("lazy_template_hooks.i"),
    )
    .unwrap();
    fs::copy(
        super::native_fragment_controls::root()
            .join("native/motorola68000/amigaos/debug/candidate_audit.i"),
        dir.0.join("candidate_audit.i"),
    )
    .unwrap();
    fs::copy(
        super::native_fragment_controls::root()
            .join("native/motorola68000/amigaos/debug/macro_packing_audit.i"),
        dir.0.join("macro_packing_audit.i"),
    )
    .unwrap();
    for include in ["capture_telemetry.i", "capture_telemetry_noop.i"] {
        fs::copy(
            super::native_fragment_controls::root()
                .join("native/motorola68000/amigaos/debug")
                .join(include),
            dir.0.join(include),
        )
        .unwrap();
    }
    // Undefined layouts and operands, and no observer module, prove disabled
    // sites neither emit instructions nor acquire their gated import.
    let packing_sites = ".PACKING_WRITE_BEGIN a6,MISSING_TEMPLATE,MISSING_WRITER,#Missing\n.PACKING_WRITE_END Missing,Missing\n.PACKING_COUNT MISSING_FIELD,#Missing\n.PACKING_STORED a6,Missing\n";
    let sites = ".LAZY_CLEAR\n.LAZY_PREPARED Missing,d0,d1,d2,d3\n.LAZY_REMAP d0,d1,d2\n.LAZY_CLASSIFY\n.LAZY_SELECTED a0,d0\n.LAZY_COUNT 0,#1\n.LAZY_REGION Missing, d0\n.LAZY_LOOKUP\n.LAZY_PIN 1\n.LAZY_SCHEDULE Missing,d0,12\n.LAZY_SPAN d0,d1,d2,d3\n.LAZY_SPAN_END d0\n.LAZY_BLOCKS a0,d0\n.LAZY_REPORT a6\n";
    let sites = format!("{sites}.WORK_CALL Missing\n.WORK_COUNT 0,#1\n.WORK_ACQUIRE 0,a0\n.WORK_CAPTURE_RESET a0\n.WORK_BOUND_BEGIN d0\n.WORK_BOUND_END d0\n.WORK_PUBLICATION a0\n.WORK_CAPTURE_REGION a0,#128\n.WORK_SELECTED a0,d0\n.WORK_REPORT a6\n.WORK_EFFECT a0\n.WORK_TEMPLATE_DELTA d1,d2\n.WORK_SAMPLE_BEGIN #1\n");
    let sites = format!("{sites}.WORK_PLAN_CONTEXT #0\n.WORK_CONTEXT 0,#1\n.WORK_TEMPLATE_CONTEXT a6,MISSING_OFFSET\n.WORK_TEMPLATE_FLOOR a6,MISSING_OFFSET,Missing\n.WORK_ORIGIN_PATHS\n");
    let sites = format!("{sites}.WORK_TEMPLATE_BEGIN a0\n.WORK_TEMPLATE_DEFINE a0,a1,#0,#0\n.WORK_TEMPLATE_CLOSE a0,a1,#0\n.WORK_TEMPLATE_CALL a0,a1\n.WORK_TEMPLATE_POOLS a0\n.WORK_TEMPLATE_LINE a0,Missing,#1,Missing\n");
    let sites = format!("{sites}.CAPTURE_ORDERED_COUNT MissingCounter,#1\n.CAPTURE_ORDERED_REUSE MissingBytes,MissingCapacity,MissingUsed,MissingBlock,MissingLimit\n.CAPTURE_ORDERED_CLEAR MissingCalls,MissingReuses,MissingCopied,MissingAvoided,MissingCapacity\n.CAPTURE_ORDERED_REPORT MissingDos,MissingCalls,MissingReuses,MissingCopied,MissingAvoided,MissingCapacity\n");
    let sites = format!(
        "{sites}.CANDIDATE_BEGIN\n.CANDIDATE_END a4\n.CANDIDATE_CREATED a4\n.CANDIDATE_OWNER\n{packing_sites}"
    );
    let input = dir.0.join("disabled.asm");
    fs::write(&input, format!(".module disabled\n.cpu 68020\n.include \"lazy_audit.i\"\n.include \"lazy_work_context.i\"\n.include \"lazy_template_hooks.i\"\n.include \"candidate_audit.i\"\n.include \"macro_packing_audit.i\"\n.include \"capture_telemetry.i\"\n.region ram,0,$ffff\n.section code,kind=code\n{sites}\trts\n.endsection\n.place code in ram\n.output \"build/disabled\",format=bin,sections=code\n.endmodule\n")).unwrap();
    let cli = Cli::parse_from(["opforge".into(), input.to_string_lossy().into_owned()]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.0.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    assert_eq!(
        fs::read(dir.0.join("build/disabled")).unwrap(),
        [0x4e, 0x75]
    );
    fs::write(&input, format!(".module disabled\n.cpu 68020\n.include \"macro_packing_audit.i\"\n.region ram,0,$ffff\n.section code,kind=code\n{packing_sites}\trts\n.endsection\n.place code in ram\n.output \"build/disabled\",format=bin,sections=code\n.endmodule\n")).unwrap();
    for defines in [
        &["OPFORGE_MACRO_PACKING_AUDIT"][..],
        &["OPFORGE_LAZY_AUDIT", "OPFORGE_MACRO_PACKING_AUDIT"][..],
        &["OPFORGE_LAZY_WORK", "OPFORGE_MACRO_PACKING_AUDIT"][..],
        &["OPFORGE_LAZY_AUDIT", "OPFORGE_LAZY_WORK"][..],
    ] {
        let mut args = vec!["opforge".into(), input.to_string_lossy().into_owned()];
        for define in defines {
            args.extend(["--define".into(), (*define).into()]);
        }
        let cli = Cli::parse_from(args);
        let mut config = validate_cli(&cli).unwrap();
        config.out_dir = Some(dir.0.clone());
        run_with_validated_cli_with_context(&cli, &config).unwrap();
        assert_eq!(
            fs::read(dir.0.join("build/disabled")).unwrap(),
            [0x4e, 0x75],
            "packing macros must disappear with gates {defines:?}"
        );
    }
}

#[test]
fn native_capture_lifetime_disabled_macros_emit_zero_bytes_with_other_audits() {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    let dir = scratch();
    fs::create_dir_all(dir.0.join("build")).unwrap();
    fs::copy(
        super::native_fragment_controls::root()
            .join("native/motorola68000/amigaos/debug/capture_lifetime.i"),
        dir.0.join("capture_lifetime.i"),
    )
    .unwrap();
    let input = dir.0.join("disabled-life.asm");
    let sites = ".CAPTURE_LIFE_CLEAR\n.CAPTURE_LIFE_ALLOC #256\n.CAPTURE_LIFE_FREE #256\n.CAPTURE_LIFE_RETAIN_BEGIN\n.CAPTURE_LIFE_RETAIN_END\n.CAPTURE_LIFE_PHASE #1\n.CAPTURE_LIFE_REGION #0,#1\n.CAPTURE_LIFE_RECORD MissingBlock,MissingLayout,#0,#256,#60\n.CAPTURE_LIFE_CURVE #1,#0,#0,#0,#0\n.CAPTURE_LIFE_REPORT MissingDosBase\n";
    fs::write(&input, format!(".module disabled_life\n.cpu 68020\n.include \"capture_lifetime.i\"\n.region ram,0,$ffff\n.section code,kind=code\n{sites}\trts\n.endsection\n.place code in ram\n.output \"build/disabled-life\",format=bin,sections=code\n.endmodule\n")).unwrap();
    for defines in [
        &[][..],
        &[
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_MEMORY_TELEMETRY",
            "OPFORGE_PREPARATION_PROGRESS",
            "OPFORGE_LAZY_AUDIT",
            "OPFORGE_LAZY_WORK",
        ][..],
    ] {
        let mut argv = vec!["opforge".to_owned(), input.to_string_lossy().into_owned()];
        for define in defines {
            argv.extend(["--define".to_owned(), (*define).to_owned()]);
        }
        let cli = Cli::parse_from(argv);
        let mut config = validate_cli(&cli).unwrap();
        config.out_dir = Some(dir.0.clone());
        run_with_validated_cli_with_context(&cli, &config).unwrap();
        assert_eq!(
            fs::read(dir.0.join("build/disabled-life")).unwrap(),
            [0x4e, 0x75]
        );
    }
}
