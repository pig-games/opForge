//! Shared-section source producer proof; actual source and live Rust bytes.
use super::native_iterable_loops::scratch;
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use vm::runtime_model_core::RuntimeModelCore;

const REAL: &str =
    include_str!("../../../../native/motorola68000/amigaos/assembler/language/text_names.asm");
const SYNTHETIC: &str = r#".module shared
.cpu 68020
.section code,kind=code
first .block
 .long second,ending
 bsr.w second
 bra.w done
 .byte 1,2
done
 rts
ending
.bend
second .block
 bra.w done
 .byte 3,4
done
 rts
.bend
third .block
 rts
.bend
.endsection
.endmodule
"#;
const BASES: [usize; 3] = [0x1000, 0x1400, 0x1800];
struct Fixture {
    source: String,
    oracle: Vec<u8>,
    sizes: Vec<usize>,
    package: Vec<u8>,
}

fn assemble(dir: &Path, source: &str, name: &str, extra: &[String]) {
    let path = dir.join(format!("{name}.asm"));
    fs::write(&path, source).unwrap();
    let mut args = vec!["opforge".to_owned(), path.to_string_lossy().into_owned()];
    args.extend_from_slice(extra);
    let cli = Cli::parse_from(args);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|e| match e {
        cli_core::CliRunError::Assembler { error, .. } => {
            let diagnostics = error
                .diagnostics()
                .iter()
                .take(12)
                .map(|d| {
                    format!(
                        "{}:{}: {}",
                        d.file.as_deref().unwrap_or(name),
                        d.line,
                        d.error.message()
                    )
                })
                .collect::<Vec<_>>()
                .join("\n");
            panic!("{name}: {}\n{diagnostics}", error.summary());
        }
        cli_core::CliRunError::Workflow { error, .. } => panic!("{name}: {error}"),
        cli_core::CliRunError::WarningsAsErrors { .. } => {
            panic!("{name}: warnings treated as errors")
        }
    });
}

fn prepare(dir: &Path, body: &str) -> Fixture {
    // The producer gets the actual source with only output request metadata added.
    let source = body.replace(
        ".endmodule",
        ".output \"shared.hunk\",format=hunk,sections=code\n.endmodule",
    );
    let mut oracle_source = String::new();
    let mut names = Vec::new();
    for line in body.lines() {
        let words = line.split_whitespace().collect::<Vec<_>>();
        if words.get(1) == Some(&".block") {
            oracle_source.push_str(&format!(" .org ${:x}\n", BASES[names.len()]));
            names.push(words[0].to_owned());
        }
        if words.first() == Some(&".bend") {
            oracle_source.push_str("__proofEnd\n");
        }
        if words.first() == Some(&".endsection") {
            oracle_source.push_str(" .org $2000\n");
            for name in &names {
                oracle_source.push_str(&format!(" .long {name}.__proofEnd-{name}\n"));
            }
        }
        // Raw CLI output uses the default segment; omit section metadata only.
        if matches!(words.first(), Some(&".section" | &".endsection")) {
            continue;
        }
        oracle_source.push_str(line);
        oracle_source.push('\n');
    }
    assert_eq!(names.len(), 3);
    let raw = dir.join("oracle-placed.bin");
    assemble(
        dir,
        &oracle_source,
        "oracle-placed",
        &["--bin".into(), raw.to_string_lossy().into_owned()],
    );
    fs::rename(dir.join("oracle-placed.asm"), dir.join("oracle-placed.txt")).unwrap();
    let bytes = fs::read(raw).unwrap();
    let tail = &bytes[0x1000..];
    assert_eq!(tail.len(), 12, "live end-label extent table");
    let sizes = tail
        .chunks_exact(4)
        .map(|b| u32::from_be_bytes(b.try_into().unwrap()) as usize)
        .collect::<Vec<_>>();
    let oracle = [2, 0, 1]
        .into_iter()
        .flat_map(|i| {
            bytes[BASES[i] - BASES[0]..BASES[i] - BASES[0] + sizes[i]]
                .iter()
                .copied()
        })
        .collect::<Vec<_>>();
    let core = RuntimeModelCore::from_registry(&engine::build_default_asm_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = crate::native_runtime_package::prepare_package(&core, &resolved).unwrap();
    fs::write(dir.join("module_shared_source.txt"), &source).unwrap();
    fs::write(dir.join("module_shared_package.bin"), &package).unwrap();
    fs::write(
        dir.join("module_shared_fixture.i"),
        format!(
            "FIRST_BYTES={}\nSECOND_BYTES={}\nTHIRD_BYTES={}\nRESULT_BYTES={}\n",
            sizes[0],
            sizes[1],
            sizes[2],
            oracle.len()
        ),
    )
    .unwrap();
    Fixture {
        source,
        oracle,
        sizes,
        package,
    }
}

fn probe(root: &Path, dir: &Path, rejections: bool) -> Vec<u8> {
    let source = fs::read_to_string(
        root.join("native/motorola68000/amigaos/tests/assembler/module_shared_probe.asm"),
    )
    .unwrap();
    let native = root.join("native/motorola68000/amigaos");
    let mut args = vec![
        "-M".into(),
        native.to_string_lossy().into_owned(),
        "-I".into(),
        native.to_string_lossy().into_owned(),
        "-I".into(),
        native.join("debug").to_string_lossy().into_owned(),
    ];
    if rejections {
        args.extend(["--define".into(), "OPFORGE_MODULE_SHARED_REJECTIONS".into()]);
    }
    if std::env::var("OPFORGE_MODULE_SHARED_MEMORY").as_deref() == Ok("1") {
        for name in [
            "OPFORGE_DEBUG_CONTRACTS",
            "OPFORGE_MEMORY_TELEMETRY",
            "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
        ] {
            args.extend(["--define".into(), name.into()]);
        }
    }
    assemble(dir, &source, "module_shared_probe", &args);
    fs::read(dir.join("module-shared-probe.hunk")).unwrap()
}

fn export(root: &Path, dir: &Path, f: &Fixture, image: &[u8]) {
    let Some(path) = std::env::var_os("OPFORGE_MODULE_SHARED_EXPORT_DIR") else {
        return;
    };
    let output = PathBuf::from(path);
    assert!(
        output.is_absolute() && !output.starts_with(root.canonicalize().unwrap().join("target"))
    );
    fs::create_dir(&output).expect("fresh export directory");
    fs::write(output.join("opforge"), image).unwrap();
    fs::write(output.join("oracle.bin"), &f.oracle).unwrap();
    for name in [
        "module_shared_source.txt",
        "module_shared_package.bin",
        "module_shared_fixture.i",
        "module_shared_probe.asm",
        "oracle-placed.txt",
    ] {
        fs::copy(dir.join(name), output.join(name)).unwrap();
    }
    let manifest = serde_json::json!({"schema":1,"kind":"native-module-shared-export","image":{"path":"opforge","digest":opforge_self_host_package_digest(image)},"oracle":{"path":"oracle.bin","digest":opforge_self_host_package_digest(&f.oracle)},"source_digest":opforge_self_host_package_digest(f.source.as_bytes()),"package_digest":opforge_self_host_package_digest(&f.package),"fragment_sizes":f.sizes,"bases":BASES,"selection":[3,1,2],"expected_output_bytes":f.oracle.len(),"instrumented":std::env::var("OPFORGE_MODULE_SHARED_MEMORY").as_deref()==Ok("1"),"outputs":["output.bin","metrics.bin"],"native_validation":"not_run","scope":"shared-section native compiler component; no CLI savings claim"});
    fs::write(
        output.join("manifest.json"),
        serde_json::to_vec_pretty(&manifest).unwrap(),
    )
    .unwrap();
}

#[test]
fn native_module_shared_sections_host_assembles() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    for source in [REAL, SYNTHETIC] {
        let dir = scratch();
        let f = prepare(&dir.0, source);
        let image = probe(&root, &dir.0, false);
        assert!(!image.is_empty());
        if source == REAL {
            export(&root, &dir.0, &f, &image);
        }
    }
}

#[test]
#[ignore = "requires FS-UAE; actual shared-section producer and moved recipe replay"]
fn native_module_shared_sections_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    for source in [REAL, SYNTHETIC] {
        let dir = scratch();
        let f = prepare(&dir.0, source);
        let image = probe(&root, &dir.0, false);
        let case = OpforgeNativeCliParityCase {
            name: "native-module-shared-sections",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(f.source.as_bytes()),
            command_template: Some(""),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/output.bin",
                rust_oracle: &f.oracle,
            },
        };
        let outcome =
            run_prebuilt_compact_cli_case_from_env(&root, &case, &image).expect("fresh native run");
        let FsUaeSmokeOutcome::Completed { runs } = outcome else {
            panic!("native execution required")
        };
        assert_eq!(runs.len(), 1);
        let run = &runs[0];
        assert!(run.protocol_completed && run.success);
        assert_eq!(run.exit_code, Some(0));
        assert_eq!(run.verified_output.as_deref(), Some(f.oracle.as_slice()));
        let metrics = run
            .captured_artifacts
            .get(&PathBuf::from("Work/metrics.bin"))
            .expect("fresh component counters");
        let words = metrics
            .chunks_exact(4)
            .map(|b| u32::from_be_bytes(b.try_into().unwrap()))
            .collect::<Vec<_>>();
        assert_eq!(words.len(), 5);
        assert_eq!(words[0], 0x4d534d31);
        assert_eq!(words[1], 1);
        assert!(words[2] > 0);
        assert_eq!(words[3], 3);
        assert_eq!(words[4], 0);
        if std::env::var("OPFORGE_MODULE_SHARED_MEMORY").as_deref() == Ok("1") {
            let bytes = run
                .captured_artifacts
                .get(&PathBuf::from("Work/memory.bin"))
                .expect("fresh MEMD record");
            let memory = super::native_package_loading_performance::memory(bytes);
            let retained = u32::from_be_bytes(bytes[20..24].try_into().unwrap());
            eprintln!(
                "MODULE_SHARED_MEMORY {}",
                serde_json::json!({"memory": memory, "retained_after_publication_bytes":retained,"scope":"tracked component heap; separate instrumentation run"})
            );
        }
        eprintln!(
            "MODULE_SHARED_COMPONENT {}",
            serde_json::json!({"native_compiles":words[1],"relocation_rows":words[2],"stored_fragments":words[3],"output_bytes":f.oracle.len(),"source_bytes":f.source.len(),"source_digest":opforge_self_host_package_digest(f.source.as_bytes()),"package_digest":opforge_self_host_package_digest(&f.package),"image_bytes":image.len(),"image_digest":opforge_self_host_package_digest(&image),"linked_static_reserved_bytes":super::native_package_loading_performance::linked_static_bytes(&image),"instrumented":std::env::var("OPFORGE_MODULE_SHARED_MEMORY").as_deref()==Ok("1"),"native_component_seconds":run.start_to_done_host_seconds,"scope":"shared-section component only; no CLI/full-build savings"})
        );
    }
}

fn prepare_rejections(dir: &Path) {
    for (name, statement) in [
        ("data", " .long second-first"),
        ("instruction", " move.l #second-first,d0"),
        ("typed", " .long (1 ? second-first : 0)"),
        ("alias", " .long Distance"),
        ("mixed", " .long (second-first)+third"),
        ("comparison", " .long (second==first)+third"),
        ("conditional", " .long (first==second ? first : third)"),
        ("pc", " .long ($-first)+second"),
        ("numeric_branch", " bsr.w 0"),
    ] {
        let mut source = SYNTHETIC.replace(" .long second,ending", statement);
        if name == "alias" {
            source = source.replace(".section code", "Distance=second-first\n.section code");
        }
        // Check every rejected component input is valid ordinary assembler input.
        eprintln!("MODULE_SHARED_REJECTION_ORACLE {name}");
        prepare(dir, &source);
        fs::write(
            dir.join(format!("module_shared_bad_{name}.txt")),
            source.replace(
                ".endmodule",
                ".output \"shared.hunk\",format=hunk,sections=code\n.endmodule",
            ),
        )
        .unwrap();
    }
}

#[test]
fn native_module_shared_rejection_rust_oracles() {
    let dir = scratch();
    prepare_rejections(&dir.0);
}

#[test]
#[ignore = "requires FS-UAE; unsupported layout cancellation must reject without publication"]
fn native_module_shared_sections_rejections_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let fixture = prepare(&dir.0, SYNTHETIC);
    prepare_rejections(&dir.0);
    // Restore the positive fixture's package/source/include after oracle checks.
    let _ = prepare(&dir.0, SYNTHETIC);
    let image = probe(&root, &dir.0, true);
    let expected = [1u8; 9];
    let case = OpforgeNativeCliParityCase {
        name: "native-module-shared-layout-rejections",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(fixture.source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/output.bin",
            rust_oracle: &expected,
        },
    };
    let outcome =
        run_prebuilt_compact_cli_case_from_env(&root, &case, &image).expect("fresh rejection run");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].protocol_completed && runs[0].success);
    assert_eq!(runs[0].exit_code, Some(0));
    assert_eq!(
        runs[0].verified_output.as_deref(),
        Some(expected.as_slice())
    );
    if std::env::var("OPFORGE_MODULE_SHARED_MEMORY").as_deref() == Ok("1") {
        super::native_package_loading_performance::memory(
            &runs[0].captured_artifacts[&PathBuf::from("Work/memory.bin")],
        );
    }
}
