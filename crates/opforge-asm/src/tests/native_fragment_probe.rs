//! Disposable test-only instrumentation and separately retained experiment images.
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

#[test]
#[ignore = "builds baseline, candidate and probe release/MEMD images; no emulator execution"]
fn native_fragment_save_images() {
    let root = super::native_fragment_controls::root();
    let destination = PathBuf::from(
        std::env::var_os("OPFORGE_FRAGMENT_SAVE_DIR")
            .expect("set OPFORGE_FRAGMENT_SAVE_DIR outside target"),
    );
    fs::create_dir_all(&destination).unwrap();
    let destination = destination.canonicalize().unwrap();
    assert!(
        !destination.starts_with(root.join("target")),
        "preserve deliverables outside target"
    );
    let dir = scratch();
    let snapshot = dir.0.join("probe");
    copy_tree(&root.join("native"), &snapshot.join("native"));
    let app = snapshot.join("native/motorola68000/amigaos/experimental/binary_app.asm");
    let text = fs::read_to_string(&app).unwrap();
    let import = "\t.use experimental.amigaos.binary_assembly as assembly\n";
    assert_eq!(text.matches(import).count(), 1);
    let text = text.replace(
        import,
        &format!("{import}\t.use tests.amigaos.binary_fragment_probe as fragmentProbe\n"),
    );
    assert_eq!(text.matches("\tjsr fragment.run\n").count(), 1);
    fs::write(
        app,
        text.replace("\tjsr fragment.run\n", "\tjsr fragmentProbe.run\n"),
    )
    .unwrap();
    for (source_root, variants) in [
        (&root, &[("reference", false), ("candidate", true)][..]),
        (&snapshot, &[("probe", true)][..]),
    ] {
        if source_root == &root
            && std::env::var("OPFORGE_FRAGMENT_PROBE_ONLY").as_deref() == Ok("1")
        {
            continue;
        }
        let build = build_native_packages(
            &engine::build_default_asm_registry(),
            &dir.0.join(format!("{}-build", variants[0].0)),
            &source_root.join("native/motorola68000/amigaos/experimental/opforge_compact_cli.asm"),
            &EmbedSelection::Targets(vec!["68020".into(), "6502".into()]),
        )
        .unwrap();
        for &(name, fragment) in variants {
            for memory in [false, true] {
                if std::env::var("OPFORGE_FRAGMENT_SAVE_MEMORY_ONLY").as_deref() == Ok("1")
                    && !memory
                {
                    continue;
                }
                let mut defines = Vec::new();
                if fragment {
                    defines.push("OPFORGE_FRAGMENT_SOURCE");
                }
                if memory {
                    defines.extend([
                        "OPFORGE_DEBUG_CONTRACTS",
                        "OPFORGE_MEMORY_TELEMETRY",
                        "OPFORGE_MEMORY_TELEMETRY_LOCAL_EXPORT",
                        "OPFORGE_PREPARATION_PROGRESS",
                    ]);
                }
                let image = super::compact_cli_input::assemble_cli_with_defines(
                    source_root,
                    &build,
                    &defines,
                );
                fs::write(
                    destination.join(format!(
                        "{name}-{}.hunk",
                        if memory { "memory" } else { "release" }
                    )),
                    image,
                )
                .unwrap();
            }
        }
    }
}
