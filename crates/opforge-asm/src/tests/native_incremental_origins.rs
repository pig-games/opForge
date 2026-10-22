//! Closed template origins must retain their source-path identity across runs.
use super::*;

const ROOT_ORIGINS: &str = ".module main\n.cpu 68020\n.use provider (LOAD)\n.section code,kind=code\n.LOAD\n.endsection\n.section data,kind=data\n.byte 0\n.endsection\n.region ram,0,$ffff\n.place code in ram\n.place data in ram\n.output \"build/module.bin\",format=bin,sections=code,data\n.output \"build/module.hunk\",format=hunk,sections=code,data\n.endmodule\n";
const PROVIDER: &str = ".module provider\n.cpu 68020\n.include \"templates.i\"\n.endmodule\n";
const TEMPLATES: &str = ".pub\nLOAD .macro\n.include \"parts/payload.i\"\n.endmacro\n";
const TREE: &[(&str, &str)] = &[
    ("library/provider.asm", PROVIDER),
    ("library/templates.i", TEMPLATES),
    ("library/parts/payload.i", ".byte 7\n"),
    ("parts/payload.i", ".byte 99\n"),
    (
        "empty.i",
        "; no emitted bytes; changes the source-origin inventory\n",
    ),
];

fn changed_root() -> String {
    ROOT_ORIGINS.replacen(".use provider", ".include \"empty.i\"\n.use provider", 1)
}

fn origin_oracle(root: &str) -> Vec<u8> {
    let dir = scratch();
    for (relative, contents) in TREE {
        let path = dir.0.join(relative);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }
    oracle_with_module_roots(&dir.0, root, &["library"]).expect("live template-origin Rust oracle")
}

#[test]
fn incremental_template_origins_live_rust_oracles() {
    let original = origin_oracle(ROOT_ORIGINS);
    assert_eq!(original, [7, 0]);
    assert_eq!(origin_oracle(&changed_root()), original);
}

#[test]
#[ignore = "requires fresh FS-UAE compact CLI template-origin cache invalidation"]
fn native_incremental_template_origin_ids() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_INCREMENTAL_IMAGE")
        .map(|path| fs::read(path).expect("current incremental compact CLI image"))
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let expected = origin_oracle(ROOT_ORIGINS);
    let changed = changed_root();
    assert_eq!(expected, [7, 0]);
    assert_eq!(origin_oracle(&changed), expected);
    let files: Vec<_> = TREE
        .iter()
        .map(|(path, contents)| OpforgeNativeCliGuestFile {
            relative_path: path,
            bytes: contents.as_bytes(),
        })
        .chain([
            OpforgeNativeCliGuestFile {
                relative_path: "entry.asm",
                bytes: ROOT_ORIGINS.as_bytes(),
            },
            // Keep alternate source out of discovery's .asm inputs.
            OpforgeNativeCliGuestFile {
                relative_path: "changed.txt",
                bytes: changed.as_bytes(),
            },
        ])
        .collect();
    let command = "Work:entry.asm -M Work:library --incremental-dir Work:cache --rebuild >Work:cold.txt\nEcho $RC >Work:cold.rc\nC:Copy Work:build/module.bin Work:cold.bin\nWork:build/opforge_compact Work:entry.asm -M Work:library --incremental-dir Work:cache >Work:warm.txt\nEcho $RC >Work:warm.rc\nC:Copy Work:build/module.bin Work:warm.bin\nC:Copy Work:changed.txt Work:entry.asm\nEcho $RC >Work:change.rc\nWork:build/opforge_compact Work:entry.asm -M Work:library --incremental-dir Work:cache";
    let artifacts = [
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/cold.bin",
            rust_oracle: &expected,
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/warm.bin",
            rust_oracle: &expected,
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/module.bin",
            rust_oracle: &expected,
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/cold.rc",
            rust_oracle: b"0\n",
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/warm.rc",
            rust_oracle: b"0\n",
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/change.rc",
            rust_oracle: b"0\n",
        },
    ];
    let case = OpforgeNativeCliParityCase {
        name: "incremental-template-origin-ids",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(ROOT_ORIGINS.as_bytes()),
        command_template: Some(command),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: OpforgeNativeCliProof::ExactArtifacts(&artifacts),
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh template-origin CLI completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.protocol_completed);
    assert!(run.success);
    assert_eq!(run.exit_code, Some(0));
    let read_counts = |path: &str| {
        let bytes = run
            .captured_artifacts
            .get(&PathBuf::from(path))
            .unwrap_or_else(|| panic!("missing {path}"));
        counters(std::str::from_utf8(bytes).unwrap())
    };
    assert_eq!(read_counts("Work/cold.txt"), (0, 1));
    assert_eq!(read_counts("Work/warm.txt"), (1, 0));
    assert_eq!(
        counters(&run.stdout),
        (0, 1),
        "changed origin IDs must invalidate the unchanged provider"
    );
}
