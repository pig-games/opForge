//! Independent component probe images and fresh store-reader artifact proof.
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

fn prepare_component_probe_source_for(source_root: &Path, snapshot: &Path, probe_module: &str) {
    copy_tree(&source_root.join("native"), &snapshot.join("native"));
    let app = snapshot.join("native/motorola68000/amigaos/assembler/driver.asm");
    let text = fs::read_to_string(&app).unwrap();
    let import = "\t.use opforge.assembler.layout.assembly as assembly\n";
    assert_eq!(text.matches(import).count(), 1);
    let text = text.replace(
        import,
        &format!("{import}\t.use {probe_module} as componentProbe\n"),
    );
    let entry = "prepare\t.block\n";
    assert_eq!(text.matches(entry).count(), 1);
    fs::write(
        app,
        text.replace(
            entry,
            &format!("{entry}\tjsr componentProbe.run\n\tmove.l d0, PackageStatus\n\tbne.w bad\n"),
        ),
    )
    .unwrap();
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
        "opforge.tests.assembler.capture_store_probe",
        "OPFORGE_CAPTURE_STORE_SAVE_DIR",
        "capture-store-probe-memory.hunk",
    );
}

#[test]
#[ignore = "builds the independent disk capture-store component probe outside target"]
fn native_capture_disk_store_probe_save_image() {
    save_capture_store_probe(
        "opforge.tests.assembler.capture_disk_store_probe",
        "OPFORGE_CAPTURE_DISK_STORE_SAVE_DIR",
        "capture-disk-store-probe-memory.hunk",
    );
}

#[test]
#[ignore = "builds the incremental graph publication component probe outside target"]
fn native_graph_publication_probe_save_image() {
    save_capture_store_probe(
        "opforge.tests.assembler.graph_publication_probe",
        "OPFORGE_GRAPH_PUBLICATION_SAVE_DIR",
        "graph-publication-probe-memory.hunk",
    );
}

#[test]
#[ignore = "builds the independent ordered-record component probe outside target"]
fn native_ordered_records_probe_save_image() {
    save_capture_store_probe(
        "opforge.tests.assembler.ordered_records_probe",
        "OPFORGE_ORDERED_RECORDS_SAVE_DIR",
        "ordered-records-probe-memory.hunk",
    );
}

#[test]
#[ignore = "builds the bounded prepared-record store/view component probe outside target"]
fn native_prepared_store_probe_save_image() {
    save_capture_store_probe(
        "opforge.tests.assembler.prepared_store_probe",
        "OPFORGE_PREPARED_STORE_SAVE_DIR",
        "prepared-store-probe-memory.hunk",
    );
}

#[test]
#[ignore = "requires prepared-store component probe image and fresh FS-UAE completion"]
fn native_prepared_store_reader_fs_uae() {
    let root = super::native_fragment_controls::root();
    let image = fs::read(
        std::env::var_os("OPFORGE_PREPARED_STORE_PROBE_IMAGE")
            .expect("set OPFORGE_PREPARED_STORE_PROBE_IMAGE"),
    )
    .expect("current prepared-store component probe image");
    let control = super::native_source_controls::controls()
        .into_iter()
        .next()
        .expect("real source control");
    let (binary, hunk) = super::native_source_controls::oracle(&control);
    let artifacts = [
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/fragment.bin",
            rust_oracle: &binary,
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/fragment.hunk",
            rust_oracle: &hunk,
        },
    ];
    let mut files = vec![
        OpforgeNativeCliGuestFile {
            relative_path: "entry.asm",
            bytes: control.source.as_bytes(),
        },
        OpforgeNativeCliGuestFile {
            relative_path: "build/.keep",
            bytes: b"",
        },
    ];
    files.extend(
        control
            .includes
            .iter()
            .map(|(path, contents)| OpforgeNativeCliGuestFile {
                relative_path: path,
                bytes: contents.as_bytes(),
            }),
    );
    let case = OpforgeNativeCliParityCase {
        name: "prepared-store-early-reader-component",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(control.source.as_bytes()),
        command_template: Some("Work:entry.asm"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: OpforgeNativeCliProof::ExactArtifacts(&artifacts),
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh prepared-store probe and real source completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native completion required")
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(
        run.protocol_completed && run.success && run.exit_code == Some(0),
        "{} {}",
        run.stdout,
        run.stderr
    );
    let memory = run
        .captured_artifacts
        .get(&PathBuf::from("Work/memory.bin"))
        .map(|bytes| super::native_package_loading_performance::memory(bytes));
    eprintln!(
        "PREPARED_STORE_READER exact=bin+hunk seconds={:?} memory={memory:?}",
        run.start_to_done_host_seconds
    );
}

#[test]
#[ignore = "builds the bounded lexical sealing component probe outside target"]
fn native_lexical_store_probe_save_image() {
    save_capture_store_probe(
        "opforge.tests.assembler.lexical_store_probe",
        "OPFORGE_LEXICAL_STORE_SAVE_DIR",
        "lexical-store-probe-memory.hunk",
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
    let build = build_native_packages(
        &engine::build_default_asm_registry(),
        &dir.0.join("probe-build"),
        &snapshot.join("native/motorola68000/amigaos/main.asm"),
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
    if probe_module == "opforge.tests.assembler.ordered_records_probe"
        && std::env::var_os("OPFORGE_ORDERED_RECORDS_EXPECT_REUSE").is_some()
    {
        defines.push("OPFORGE_ORDERED_RECORDS_EXPECT_REUSE");
    }
    let image = super::compact_cli_input::assemble_cli_with_defines(&snapshot, &build, &defines);
    let digest = tree_digest(&snapshot.join("native"));
    fs::write(destination.join(filename), &image).unwrap();
    fs::write(
        destination.join("images.json"),
        serde_json::to_vec_pretty(
            &serde_json::json!({"native_source_fnv1a64": digest, "images": [{
                "file": filename, "defines": defines, "bytes": image.len(),
                "fnv1a64": opforge_self_host_package_digest(&image),
                "native_source_fnv1a64": digest,
            }]}),
        )
        .unwrap(),
    )
    .unwrap();
}
