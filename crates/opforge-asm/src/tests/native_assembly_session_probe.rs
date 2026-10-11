//! Fresh native proof for the experimental assembly session lifecycle probe.
use super::native_iterable_loops::{build_image, scratch};
use super::native_source_cases::{self, Case};
use super::*;
use std::path::Path;

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

fn probe_snapshot(root: &Path, dir: &Path) -> PathBuf {
    // Package generation reads native inputs from this disposable copy. Only
    // its compact CLI source is changed; production sources stay untouched.
    let snapshot = dir.join("native-snapshot");
    copy_tree(&root.join("native"), &snapshot.join("native"));
    let app = snapshot.join("native/motorola68000/amigaos/experimental/binary_app.asm");
    let source = fs::read_to_string(&app).unwrap();
    let import = "\t.use experimental.amigaos.binary_assembly as assembly\n";
    assert_eq!(source.matches(import).count(), 1);
    let source = source.replace(
        import,
        &format!("{import}\t.use tests.amigaos.binary_assembly_session_probe as probe\n"),
    );
    assert_eq!(source.matches("\tjsr assembly.assemble\n").count(), 1);
    fs::write(
        &app,
        source.replace("\tjsr assembly.assemble\n", "\tjsr probe.run\n"),
    )
    .unwrap();
    snapshot
}

#[test]
#[ignore = "requires configured FS-UAE; fresh assembly session cleanup and output proof"]
fn native_assembly_session_probe_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let snapshot = probe_snapshot(&root, &dir.0);
    let image = build_image(&snapshot, &dir.0);
    let cases = cases();
    native_source_cases::run(&root, &image, &cases);
}

fn cases() -> [Case; 2] {
    [
        Case {
            name: "session-probe-text-map-escapes-modules".into(),
            cpu: "68020".into(),
            files: vec![(
                "entry.asm",
                r#".module first
.cpu 68020
.encode custom
.cdef "A","B",7
.edef "{cr}",13
.endencode
.enc custom
.byte "A{cr}"
.endmodule
.module second
.cpu 68020
.enc petscii
.byte "Az"
.endmodule
"#
                .into(),
            )],
            expected: Some(vec![7, 13, 0xc1, 0x5a]),
        },
        Case {
            name: "session-probe-text-module-reset".into(),
            cpu: "68020".into(),
            files: vec![(
                "entry.asm",
                r#".module first
.cpu 68020
.encode custom
.tdef "xy",9
.endencode
.enc custom
.byte "xy"
.endmodule
.module second
.cpu 68020
.byte "xy"
.endmodule
"#
                .into(),
            )],
            expected: Some(vec![9, 10, b'x', b'y']),
        },
    ]
}

#[test]
fn native_assembly_session_probe_rust_controls() {
    for case in cases() {
        let dir = scratch();
        let actual = super::native_active_includes::live_oracle(&dir.0, &case.files);
        assert_eq!(Some(actual), case.expected, "{}", case.name);
    }
}
