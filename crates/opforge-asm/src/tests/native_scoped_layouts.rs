//! Per-iteration label values retain their section identity in Hunk output.
use super::native_iterable_loops::{build_image, run_case, scratch};
use super::native_while_layouts::{hunk_oracle, native_case};
use super::*;

fn source(cpu: &str) -> String {
    format!(".module entry\n.cpu {cpu}\n.section code,kind=code\n.bfor 2\nstart .byte 7\n.long start,finish\nfinish\n.endfor\n.endsection\n.section data,kind=data\n.byte 9\n.endsection\n.output \"out.hunk\",format=hunk,sections=code,data\n.endmodule\n")
}

fn checked_oracle(dir: &Path, cpu: &str) -> Vec<u8> {
    let bytes = hunk_oracle(dir, &source(cpu));
    let sections = hunk::segments(&bytes).unwrap();
    assert_eq!(sections.len(), 2);
    assert_eq!(sections[0].kind, 0x3e9);
    let expected = match cpu {
        "68020" => [7, 0, 0, 0, 0, 0, 0, 0, 9, 7, 0, 0, 0, 9, 0, 0, 0, 18, 0, 0],
        "6502" => [7, 0, 0, 0, 0, 9, 0, 0, 0, 7, 9, 0, 0, 0, 18, 0, 0, 0, 0, 0],
        _ => panic!("unsupported scoped Hunk fixture CPU {cpu}"),
    };
    assert_eq!(sections[0].payload, expected);
    assert_eq!(sections[0].relocations, [(1, 0), (5, 0), (10, 0), (14, 0)]);
    assert_eq!(sections[1].kind, 0x3ea);
    assert_eq!(sections[1].payload, [9, 0, 0, 0]);
    bytes
}

#[test]
fn native_scoped_layouts_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        checked_oracle(&dir.0, cpu);
    }
}

fn alias_source(cpu: &str) -> String {
    format!(".module entry\n.cpu {cpu}\n.section data,kind=data\n.bfor 2\nstart .byte 7\nsaved .const start\n.bfor 2\ninside .const saved\n.long inside\n.endfor\n.endfor\n.endsection\n.section code,kind=code\n.byte 9\n.endsection\n.output \"out.hunk\",format=hunk,sections=code,data\n.endmodule\n")
}

fn checked_alias_oracle(dir: &Path, cpu: &str) -> Vec<u8> {
    let bytes = hunk_oracle(dir, &alias_source(cpu));
    let sections = hunk::segments(&bytes).unwrap();
    assert_eq!(sections.len(), 2);
    assert_eq!(sections[0].payload, [9, 0, 0, 0]);
    let expected = match cpu {
        "68020" => [7, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 9, 0, 0],
        "6502" => [7, 0, 0, 0, 0, 0, 0, 0, 0, 7, 9, 0, 0, 0, 9, 0, 0, 0, 0, 0],
        _ => panic!("unsupported scoped Hunk fixture CPU {cpu}"),
    };
    assert_eq!(sections[1].payload, expected);
    assert_eq!(sections[1].relocations, [(1, 1), (5, 1), (10, 1), (14, 1)]);
    bytes
}

#[test]
fn native_scoped_layouts_alias_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        checked_alias_oracle(&dir.0, cpu);
    }
}

#[test]
#[ignore = "requires FS-UAE; nested readonly aliases retain whole-Hunk relocation proof"]
fn native_scoped_layouts_alias_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_SCOPED_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    for cpu in ["68020", "6502"] {
        let expected = checked_alias_oracle(&dir.0, cpu);
        native_case(
            &root,
            &image,
            &format!("scoped-readonly-alias-{cpu}"),
            &alias_source(cpu),
            "Work:entry.asm --hunk Work:out.hunk",
            &expected,
            false,
        );
    }
}

#[test]
#[ignore = "requires FS-UAE; complete Hunk equality and iteration-local relocations"]
fn native_scoped_layouts_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_SCOPED_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    for cpu in ["68020", "6502"] {
        let expected = checked_oracle(&dir.0, cpu);
        native_case(
            &root,
            &image,
            &format!("scoped-label-relocations-{cpu}"),
            &source(cpu),
            "Work:entry.asm --hunk Work:out.hunk",
            &expected,
            false,
        );
    }
}

// Scoped storage must leave the enclosing constant resolver and its error
// status intact even when the source contains no scoped repetition.
fn global_graph_source(cpu: &str, cyclic: bool) -> String {
    let body = if cyclic {
        "first=second+1\nsecond=first+1\n.byte first\n"
    } else {
        "before=after+1\nafter=4\nacc .var 2\nsaved .const acc+before\nacc .set 9\n.byte before,saved,acc\n.for i in {before,saved}\n.byte i\n.endfor\n"
    };
    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n")
}

#[test]
fn native_scoped_global_graph_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        assert_eq!(
            super::native_iterable_loops::oracle(&dir.0, &global_graph_source(cpu, false)).unwrap(),
            [5, 7, 9, 5, 7]
        );
        assert!(
            super::native_iterable_loops::oracle(&dir.0, &global_graph_source(cpu, true)).is_err()
        );
    }
}

#[test]
#[ignore = "requires FS-UAE; global graph behavior across scoped-storage migration"]
fn native_scoped_global_graph_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_SCOPED_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    for cpu in ["68020", "6502"] {
        for cyclic in [false, true] {
            run_case(
                &root,
                &dir.0,
                &image,
                &format!("global-graph-{cpu}-cycle-{cyclic}"),
                cpu,
                &global_graph_source(cpu, cyclic),
                cyclic,
            );
        }
    }
}

// Sizing replays must finish before Hunk section extents/output are frozen.
fn retained_source(cpu: &str) -> String {
    format!(".module entry\n.cpu {cpu}\n.section code,kind=code\n.bfor 2\n.if local\n.byte 5\n.endif\nlocal .var 1\n.endfor\n.endsection\n.output \"out.hunk\",format=hunk,sections=code\n.endmodule\n")
}

fn retained_oracle(dir: &Path, cpu: &str) -> Vec<u8> {
    let bytes = hunk_oracle(dir, &retained_source(cpu));
    let sections = hunk::segments(&bytes).unwrap();
    assert_eq!(sections.len(), 1);
    assert_eq!(sections[0].kind, 0x3e9);
    assert_eq!(sections[0].payload, [5, 5, 0, 0]);
    assert!(sections[0].relocations.is_empty());
    bytes
}

#[test]
fn native_scoped_retained_hunk_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        retained_oracle(&dir.0, cpu);
    }
}

#[test]
#[ignore = "requires FS-UAE; retained scoped guards size before Hunk emission"]
fn native_scoped_retained_hunk_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_SCOPED_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    for cpu in ["68020", "6502"] {
        let expected = retained_oracle(&dir.0, cpu);
        native_case(
            &root,
            &image,
            &format!("scoped-retained-hunk-{cpu}"),
            &retained_source(cpu),
            "Work:entry.asm --hunk Work:out.hunk",
            &expected,
            false,
        );
    }
}
