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
    let probe_only = std::env::var("OPFORGE_LAZY_SAVE_PROBE_ONLY").as_deref() == Ok("1");
    assert!(
        !probe_only || std::env::var("OPFORGE_LAZY_SAVE_PROBE").as_deref() == Ok("1"),
        "probe-only builds require OPFORGE_LAZY_SAVE_PROBE=1"
    );
    let candidates = std::env::var("OPFORGE_LAZY_SAVE_CANDIDATES").as_deref() == Ok("1");
    let candidate_timing =
        std::env::var("OPFORGE_LAZY_SAVE_CANDIDATE_TIMING").as_deref() == Ok("1");
    assert!(
        !candidate_timing || candidates,
        "candidate timing requires candidates"
    );
    let candidate_period: u32 = std::env::var("OPFORGE_LAZY_SAVE_CANDIDATE_PERIOD")
        .map(|value| value.parse().expect("candidate period must be an integer"))
        .unwrap_or(64);
    let candidate_seed: u32 = std::env::var("OPFORGE_LAZY_SAVE_CANDIDATE_SEED")
        .map(|value| value.parse().expect("candidate seed must be an integer"))
        .unwrap_or(0);
    assert!(
        [64, 128, 256].contains(&candidate_period),
        "candidate period must be 64, 128 or 256"
    );
    assert!(
        candidate_seed < candidate_period,
        "candidate seed must be below period"
    );
    assert!(
        candidate_timing || (candidate_period == 64 && candidate_seed == 0),
        "nondefault sampling requires candidate timing"
    );
    let sampling_defines: Vec<String> = [
        (
            candidate_period != 64,
            format!("OPFORGE_CANDIDATE_SAMPLE_PERIOD={candidate_period}"),
        ),
        (
            candidate_seed != 0,
            format!("OPFORGE_CANDIDATE_PHASE_SEED={candidate_seed}"),
        ),
    ]
    .into_iter()
    .filter_map(|(changed, define)| changed.then_some(define))
    .collect();
    let memory_only = std::env::var("OPFORGE_LAZY_SAVE_MEMORY_ONLY").as_deref() == Ok("1");
    let selected_roles = std::env::var("OPFORGE_LAZY_SAVE_ROLES").ok();
    if let Some(roles) = &selected_roles {
        assert!(
            roles
                .split(',')
                .all(|role| ["audit", "reference", "work"].contains(&role)),
            "OPFORGE_LAZY_SAVE_ROLES accepts audit,reference,work"
        );
    }
    assert!(
        !candidates
            || selected_roles
                .as_ref()
                .is_some_and(|roles| roles.split(',').any(|role| role == "work")),
        "candidate images require OPFORGE_LAZY_SAVE_ROLES including work"
    );
    for (role, audit, work) in [
        ("audit", true, false),
        ("reference", false, false),
        ("work", true, true),
    ] {
        if probe_only {
            continue;
        }
        if selected_roles
            .as_ref()
            .is_some_and(|roles| !roles.split(',').any(|selected| selected == role))
        {
            continue;
        }
        for memory in [false, true] {
            if memory_only && !memory {
                continue;
            }
            let mut defines = Vec::new();
            if audit {
                defines.push("OPFORGE_LAZY_AUDIT");
            }
            if work {
                defines.push("OPFORGE_LAZY_WORK");
                if candidates {
                    defines.push("OPFORGE_LAZY_CANDIDATES");
                    if memory && candidate_timing {
                        defines.push("OPFORGE_CANDIDATE_TIMING");
                        defines.extend(sampling_defines.iter().map(String::as_str));
                    }
                }
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
        prepare_component_probe_source(&source_root, &snapshot);
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
                if candidates {
                    defines.push("OPFORGE_LAZY_CANDIDATES");
                    if candidate_timing {
                        defines.push("OPFORGE_CANDIDATE_TIMING");
                        defines.extend(sampling_defines.iter().map(String::as_str));
                    }
                }
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
#[ignore = "builds eager and deferred macro-binding images outside target"]
fn native_deferred_macro_save_images() {
    save_macro_experiment_images("OPFORGE_DEFERRED_MACRO_PLANS");
}

#[test]
#[ignore = "builds eager and binary token/spelling images outside target"]
fn native_binary_spelling_save_images() {
    save_macro_experiment_images("OPFORGE_BINARY_MACRO_VIEW");
}

#[test]
#[ignore = "builds eager and deferred macro-candidate images outside target"]
fn native_deferred_candidate_save_images() {
    save_macro_experiment_images("OPFORGE_DEFERRED_MACRO_CANDIDATES");
}

#[test]
#[ignore = "builds eager and deferred whole-line images outside target"]
fn native_deferred_line_save_images() {
    save_macro_experiment_images("OPFORGE_DEFERRED_MACRO_LINES");
}

#[test]
#[ignore = "builds baseline and passive macro packing observer images outside target"]
fn native_macro_packing_save_images() {
    save_macro_experiment_images("OPFORGE_MACRO_PACKING_AUDIT");
}

#[test]
#[ignore = "builds baseline and passive capture lifetime observer images outside target"]
fn native_capture_lifetime_save_images() {
    save_macro_experiment_images("OPFORGE_CAPTURE_LIFETIME_AUDIT");
}

fn save_macro_experiment_images(experiment: &str) {
    let root = super::native_fragment_controls::root();
    let destination = PathBuf::from(
        std::env::var_os("OPFORGE_DEFERRED_SAVE_DIR")
            .expect("set OPFORGE_DEFERRED_SAVE_DIR outside target"),
    );
    fs::create_dir_all(&destination).unwrap();
    let destination = destination.canonicalize().unwrap();
    assert!(!destination.starts_with(root.join("target")));
    let save_probe = std::env::var("OPFORGE_DEFERRED_SAVE_PROBE").as_deref() == Ok("1");
    assert!(
        !save_probe
            || matches!(
                experiment,
                "OPFORGE_DEFERRED_MACRO_PLANS"
                    | "OPFORGE_DEFERRED_MACRO_LINES"
                    | "OPFORGE_MACRO_PACKING_AUDIT"
                    | "OPFORGE_CAPTURE_LIFETIME_AUDIT"
            ),
        "the selected experiment does not support the component probe"
    );
    let packing = experiment == "OPFORGE_MACRO_PACKING_AUDIT";
    let capture_lifetime = experiment == "OPFORGE_CAPTURE_LIFETIME_AUDIT";
    let packing_period: u32 = std::env::var("OPFORGE_PACKING_SAVE_SAMPLE_PERIOD")
        .map(|value| {
            value
                .parse()
                .expect("packing sample period must be an integer")
        })
        .unwrap_or(64);
    let packing_seed: u32 = std::env::var("OPFORGE_PACKING_SAVE_PHASE_SEED")
        .map(|value| {
            value
                .parse()
                .expect("packing phase seed must be an integer")
        })
        .unwrap_or(0);
    assert!(
        [64, 128, 256].contains(&packing_period),
        "packing period must be 64, 128 or 256"
    );
    assert!(packing_seed <= 255, "packing phase seed must be 0..255");
    assert!(
        packing || (packing_period == 64 && packing_seed == 0),
        "packing sampling options require the packing experiment"
    );
    let packing_sampling_defines: Vec<String> = [
        (
            packing_period != 64,
            format!("OPFORGE_PACKING_SAMPLE_PERIOD={packing_period}"),
        ),
        (
            packing_seed != 0,
            format!("OPFORGE_PACKING_PHASE_SEED={packing_seed}"),
        ),
    ]
    .into_iter()
    .filter_map(|(changed, define)| changed.then_some(define))
    .collect();
    let mut available = vec![
        "reference-release.hunk",
        "reference-memory.hunk",
        "deferred-release.hunk",
        "deferred-memory.hunk",
    ];
    if save_probe {
        available.push("deferred-probe-memory.hunk");
    }
    let selected: Vec<String> = match std::env::var("OPFORGE_DEFERRED_SAVE_IMAGES") {
        Ok(value) => {
            let names: Vec<_> = value.split(',').map(str::trim).collect();
            assert!(
                names.iter().all(|name| !name.is_empty()),
                "OPFORGE_DEFERRED_SAVE_IMAGES entries must be nonempty"
            );
            assert!(
                names.iter().all(|name| available.contains(name)),
                "OPFORGE_DEFERRED_SAVE_IMAGES contains an unknown image filename"
            );
            assert_eq!(
                names.iter().collect::<std::collections::HashSet<_>>().len(),
                names.len(),
                "OPFORGE_DEFERRED_SAVE_IMAGES contains duplicate filenames"
            );
            names.into_iter().map(str::to_owned).collect()
        }
        Err(std::env::VarError::NotPresent) => {
            available.iter().map(|name| (*name).to_owned()).collect()
        }
        Err(error) => panic!("invalid OPFORGE_DEFERRED_SAVE_IMAGES: {error}"),
    };
    let dir = scratch();
    let source_root = dir.0.join("frozen-source");
    copy_tree(&root.join("native"), &source_root.join("native"));
    let native_source_digest = tree_digest(&source_root.join("native"));
    let build = selected
        .iter()
        .any(|name| name != "deferred-probe-memory.hunk")
        .then(|| {
            build_native_packages(
                &engine::build_default_asm_registry(),
                &dir.0.join("packages"),
                &source_root
                    .join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
                &EmbedSelection::Targets(vec!["68020".into(), "6502".into()]),
            )
            .unwrap()
        });
    let baseline_defines: &[&str] = if packing || capture_lifetime {
        &[
            "OPFORGE_DEFERRED_MACRO_PLANS",
            "OPFORGE_BINARY_MACRO_VIEW",
            "OPFORGE_DEFERRED_MACRO_CANDIDATES",
            "OPFORGE_DEFERRED_MACRO_LINES",
            "OPFORGE_LAZY_AUDIT",
            "OPFORGE_LAZY_WORK",
        ]
    } else if experiment == "OPFORGE_DEFERRED_MACRO_LINES" {
        &[
            "OPFORGE_DEFERRED_MACRO_PLANS",
            "OPFORGE_BINARY_MACRO_VIEW",
            "OPFORGE_DEFERRED_MACRO_CANDIDATES",
        ]
    } else if experiment == "OPFORGE_DEFERRED_MACRO_CANDIDATES" {
        &["OPFORGE_DEFERRED_MACRO_PLANS", "OPFORGE_BINARY_MACRO_VIEW"]
    } else {
        &[]
    };
    let mut images = Vec::new();
    for (role, deferred) in [("reference", false), ("deferred", true)] {
        for memory in [false, true] {
            let mut defines = baseline_defines.to_vec();
            if deferred {
                defines.push(experiment);
                if packing && memory {
                    defines.push("OPFORGE_PACKING_TIMING");
                    defines.extend(packing_sampling_defines.iter().map(String::as_str));
                }
            }
            if memory {
                if capture_lifetime {
                    defines.push("OPFORGE_CAPTURE_TELEMETRY");
                }
                defines.extend([
                    "OPFORGE_DEBUG_CONTRACTS",
                    "OPFORGE_MEMORY_TELEMETRY",
                    "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
                    "OPFORGE_PREPARATION_PROGRESS",
                ]);
            }
            let filename = format!("{role}-{}.hunk", if memory { "memory" } else { "release" });
            if !selected.contains(&filename) {
                continue;
            }
            let image = super::compact_cli_input::assemble_cli_with_defines(
                &source_root,
                build.as_ref().unwrap(),
                &defines,
            );
            images.push(serde_json::json!({"file": filename, "defines": defines, "fnv1a64": opforge_self_host_package_digest(&image), "bytes": image.len()}));
            fs::write(destination.join(filename), image).unwrap();
        }
    }
    if selected
        .iter()
        .any(|name| name == "deferred-probe-memory.hunk")
    {
        let snapshot = dir.0.join("probe-source");
        if capture_lifetime {
            prepare_component_probe_source_for(
                &source_root,
                &snapshot,
                "tests.amigaos.capture_lifetime_probe",
            );
        } else {
            prepare_component_probe_source(&source_root, &snapshot);
        }
        let probe_source_digest = tree_digest(&snapshot.join("native"));
        let probe_build = build_native_packages(
            &engine::build_default_asm_registry(),
            &dir.0.join("probe-build"),
            &snapshot.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
            &EmbedSelection::Targets(vec!["68020".into(), "6502".into()]),
        )
        .unwrap();
        let mut defines = baseline_defines.to_vec();
        defines.push(experiment);
        if capture_lifetime {
            defines.push("OPFORGE_CAPTURE_TELEMETRY");
        }
        if packing {
            defines.push("OPFORGE_PACKING_TIMING");
            defines.extend(packing_sampling_defines.iter().map(String::as_str));
        }
        for define in [
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_MEMORY_TELEMETRY",
            "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
            "OPFORGE_PREPARATION_PROGRESS",
            "OPFORGE_LAZY_AUDIT",
            "OPFORGE_LAZY_WORK",
        ] {
            if !defines.contains(&define) {
                defines.push(define);
            }
        }
        let image =
            super::compact_cli_input::assemble_cli_with_defines(&snapshot, &probe_build, &defines);
        if selected.iter().any(|name| name == "deferred-memory.hunk") {
            assert_ne!(
                image,
                fs::read(destination.join("deferred-memory.hunk")).unwrap(),
                "component probe image must differ from the ordinary candidate"
            );
        }
        let filename = "deferred-probe-memory.hunk";
        images.push(serde_json::json!({"file": filename, "defines": defines, "fnv1a64": opforge_self_host_package_digest(&image), "bytes": image.len(), "native_source_fnv1a64": probe_source_digest}));
        fs::write(destination.join(filename), image).unwrap();
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
    // Undefined layouts and operands, and no observer module, prove disabled
    // sites neither emit instructions nor acquire their gated import.
    let packing_sites = ".PACKING_WRITE_BEGIN a6,MISSING_TEMPLATE,MISSING_WRITER,#Missing\n.PACKING_WRITE_END Missing,Missing\n.PACKING_COUNT MISSING_FIELD,#Missing\n.PACKING_STORED a6,Missing\n";
    let sites = ".LAZY_CLEAR\n.LAZY_PREPARED Missing,d0,d1,d2,d3\n.LAZY_REMAP d0,d1,d2\n.LAZY_CLASSIFY\n.LAZY_SELECTED a0,d0\n.LAZY_COUNT 0,#1\n.LAZY_REGION Missing, d0\n.LAZY_LOOKUP\n.LAZY_PIN 1\n.LAZY_SCHEDULE Missing,d0,12\n.LAZY_SPAN d0,d1,d2,d3\n.LAZY_SPAN_END d0\n.LAZY_BLOCKS a0,d0\n.LAZY_REPORT a6\n";
    let sites = format!("{sites}.WORK_CALL Missing\n.WORK_COUNT 0,#1\n.WORK_ACQUIRE 0,a0\n.WORK_CAPTURE_RESET a0\n.WORK_BOUND_BEGIN d0\n.WORK_BOUND_END d0\n.WORK_PUBLICATION a0\n.WORK_CAPTURE_REGION a0,#128\n.WORK_SELECTED a0,d0\n.WORK_REPORT a6\n.WORK_EFFECT a0\n.WORK_TEMPLATE_DELTA d1,d2\n.WORK_SAMPLE_BEGIN #1\n");
    let sites = format!("{sites}.WORK_PLAN_CONTEXT #0\n.WORK_CONTEXT 0,#1\n.WORK_TEMPLATE_CONTEXT a6,MISSING_OFFSET\n.WORK_TEMPLATE_FLOOR a6,MISSING_OFFSET,Missing\n.WORK_ORIGIN_PATHS\n");
    let sites = format!("{sites}.WORK_TEMPLATE_BEGIN a0\n.WORK_TEMPLATE_DEFINE a0,a1,#0,#0\n.WORK_TEMPLATE_CLOSE a0,a1,#0\n.WORK_TEMPLATE_CALL a0,a1\n.WORK_TEMPLATE_POOLS a0\n.WORK_TEMPLATE_LINE a0,Missing,#1,Missing\n");
    let sites = format!(
        "{sites}.CANDIDATE_BEGIN\n.CANDIDATE_END a4\n.CANDIDATE_CREATED a4\n.CANDIDATE_OWNER\n{packing_sites}"
    );
    let input = dir.0.join("disabled.asm");
    fs::write(&input, format!(".module disabled\n.cpu 68020\n.include \"lazy_audit.i\"\n.include \"lazy_work_context.i\"\n.include \"lazy_template_hooks.i\"\n.include \"candidate_audit.i\"\n.include \"macro_packing_audit.i\"\n.region ram,0,$ffff\n.section code,kind=code\n{sites}\trts\n.endsection\n.place code in ram\n.output \"build/disabled\",format=bin,sections=code\n.endmodule\n")).unwrap();
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
