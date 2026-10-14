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
#[ignore = "builds release and memory reference/audit images outside target"]
fn native_lazy_audit_save_images() {
    let root = super::native_fragment_controls::root();
    let destination = PathBuf::from(
        std::env::var_os("OPFORGE_LAZY_SAVE_DIR")
            .expect("set OPFORGE_LAZY_SAVE_DIR outside target"),
    );
    fs::create_dir_all(&destination).unwrap();
    let destination = destination.canonicalize().unwrap();
    assert!(!destination.starts_with(root.join("target")));
    let dir = scratch();
    let source_root = dir.0.join("frozen-source");
    copy_tree(&root.join("native"), &source_root.join("native"));
    let native_source_digest = tree_digest(&source_root.join("native"));
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &dir.0.join("packages"),
        &source_root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
        &EmbedSelection::Targets(vec!["68020".into(), "6502".into()]),
    )
    .unwrap();
    let mut images = Vec::new();
    let selected_roles = std::env::var("OPFORGE_LAZY_SAVE_ROLES").ok();
    if let Some(roles) = &selected_roles {
        assert!(
            roles
                .split(',')
                .all(|role| ["audit", "reference", "work"].contains(&role)),
            "OPFORGE_LAZY_SAVE_ROLES accepts audit,reference,work"
        );
    }
    for (role, audit, work) in [
        ("audit", true, false),
        ("reference", false, false),
        ("work", true, true),
    ] {
        if selected_roles
            .as_ref()
            .is_some_and(|roles| !roles.split(',').any(|selected| selected == role))
        {
            continue;
        }
        for memory in [false, true] {
            let mut defines = Vec::new();
            if audit {
                defines.push("OPFORGE_LAZY_AUDIT");
            }
            if work {
                defines.push("OPFORGE_LAZY_WORK");
            }
            if memory {
                defines.extend([
                    "OPFORGE_DEBUG_CONTRACTS",
                    "OPFORGE_MEMORY_TELEMETRY",
                    "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
                    "OPFORGE_PREPARATION_PROGRESS",
                ]);
            }
            let image =
                super::compact_cli_input::assemble_cli_with_defines(&source_root, &build, &defines);
            let filename = format!(
                "{}-{}.hunk",
                role,
                if memory { "memory" } else { "release" }
            );
            images.push(serde_json::json!({"file": filename, "defines": defines, "fnv1a64": opforge_self_host_package_digest(&image), "bytes": image.len()}));
            fs::write(destination.join(filename), image).unwrap();
        }
    }
    fs::write(
        destination.join("images.json"),
        serde_json::to_vec_pretty(
            &serde_json::json!({"native_source_fnv1a64":native_source_digest,"images":images}),
        )
        .unwrap(),
    )
    .unwrap();
    if std::env::var("OPFORGE_LAZY_SAVE_PROBE").as_deref() == Ok("1") {
        let snapshot = dir.0.join("probe-source");
        copy_tree(&source_root.join("native"), &snapshot.join("native"));
        let app = snapshot.join("native/motorola68000/amigaos/experimental/binary_app.asm");
        let text = fs::read_to_string(&app).unwrap();
        let import = "\t.use experimental.amigaos.binary_assembly as assembly\n";
        assert_eq!(text.matches(import).count(), 1);
        let text = text.replace(
            import,
            &format!("{import}\t.use tests.amigaos.binary_lazy_audit_probe as lazyProbe\n"),
        );
        let entry = "prepare\t.block\n";
        assert_eq!(text.matches(entry).count(), 1);
        fs::write(app, text.replace(entry, &format!("{entry}\tjsr lazyProbe.run\n\tmove.l d0, PackageStatus\n\tbne.w bad\n\t.LAZY_CLEAR\n\t.WORK_CALL lazy_work.clear\n"))).unwrap();
        let probe_build = build_native_packages(
            &engine::build_default_asm_registry(),
            &dir.0.join("probe-build"),
            &snapshot.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
            &EmbedSelection::Targets(vec!["68020".into(), "6502".into()]),
        )
        .unwrap();
        let base_defines = [
            "OPFORGE_LAZY_AUDIT",
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_MEMORY_TELEMETRY",
            "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
            "OPFORGE_PREPARATION_PROGRESS",
        ];
        for work in [false, true] {
            let mut defines = base_defines.to_vec();
            if work {
                defines.push("OPFORGE_LAZY_WORK");
            }
            let image = super::compact_cli_input::assemble_cli_with_defines(
                &snapshot,
                &probe_build,
                &defines,
            );
            let filename = if work {
                "work-probe-memory.hunk"
            } else {
                "probe-memory.hunk"
            };
            images.push(serde_json::json!({"file":filename, "defines":defines, "fnv1a64":opforge_self_host_package_digest(&image), "bytes":image.len()}));
            fs::write(destination.join(filename), image).unwrap();
        }
    }

    fs::write(
        destination.join("images.json"),
        serde_json::to_vec_pretty(
            &serde_json::json!({"native_source_fnv1a64":native_source_digest,"images":images}),
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
    let sites = ".LAZY_CLEAR\n.LAZY_PREPARED Missing,d0,d1,d2,d3\n.LAZY_REMAP d0,d1,d2\n.LAZY_CLASSIFY\n.LAZY_SELECTED a0,d0\n.LAZY_COUNT 0,#1\n.LAZY_REGION Missing, d0\n.LAZY_LOOKUP\n.LAZY_PIN 1\n.LAZY_SCHEDULE Missing,d0,12\n.LAZY_SPAN d0,d1,d2,d3\n.LAZY_SPAN_END d0\n.LAZY_BLOCKS a0,d0\n.LAZY_REPORT a6\n";
    let sites = format!("{sites}.WORK_CALL Missing\n.WORK_COUNT 0,#1\n.WORK_ACQUIRE 0,a0\n.WORK_CAPTURE_RESET a0\n.WORK_BOUND_BEGIN d0\n.WORK_BOUND_END d0\n.WORK_PUBLICATION a0\n.WORK_CAPTURE_REGION a0\n.WORK_SELECTED a0,d0\n.WORK_REPORT a6\n.WORK_EFFECT a0\n.WORK_TEMPLATE_DELTA d1,d2\n.WORK_SAMPLE_BEGIN #1\n");
    let input = dir.0.join("disabled.asm");
    fs::write(&input, format!(".module disabled\n.cpu 68020\n.include \"lazy_audit.i\"\n.region ram,0,$ffff\n.section code,kind=code\n{sites}\trts\n.endsection\n.place code in ram\n.output \"build/disabled\",format=bin,sections=code\n.endmodule\n")).unwrap();
    let cli = Cli::parse_from(["opforge".into(), input.to_string_lossy().into_owned()]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.0.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    assert_eq!(
        fs::read(dir.0.join("build/disabled")).unwrap(),
        [0x4e, 0x75]
    );
}
