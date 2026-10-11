//! Shared compact `.ds` sizing, placement and rejection controls.
use super::*;

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

const COMPACT_DS_POSITIVE: &[(&str, &str)] = &[
    ("same-region-bss-res", ".module main\n.cpu 68020\n.region image,0,255\n.section vars,kind=bss\nbegin:\n.res byte,4\nfinish:\n.endsection\n.section data,kind=data\n.byte finish-begin\n.endsection\n.place vars in image\n.place data in image\n.endmodule\n"),
    ("separate-region-bss", ".module main\n.cpu 68020\n.region ram,0,255\n.region rom,256,511\n.section vars,kind=bss\nbegin:\n.ds 4\nfinish:\n.endsection\n.section data,kind=data\n.byte finish-begin\n.endsection\n.place vars in ram\n.place data in rom\n.endmodule\n"),
    ("flat-leading-trailing-gap", ".module main\n.cpu 68020\n.org $2000\n.ds 3\n.byte $5a\n.ds 4\n.endmodule\n"),
    ("initialized-section-gap", ".module main\n.cpu 68020\n.region image,0,255\n.section data,kind=data\nbegin .byte 1\n.ds 3\nafter .byte after-begin\n.endsection\n.place data in image\n.endmodule\n"),
    ("flat-arithmetic-forward-gap", ".module main\n.cpu 68020\n.org $2000\n.byte $a5\nbuffer .ds Size*4\nafter .byte after-buffer\n.ds 0\n.byte $5a\nSize .const 2\n.endmodule\n"),
    ("placed-bss-byte-reservation", ".module main\n.cpu 68020\n.region image,0,255\n.section vars,kind=bss\nbegin:\n.ds 4\n.ds 0\nfinish:\n.endsection\n.section data,kind=data\n.byte finish-begin\n.endsection\n.place vars in image\n.place data in image\n.endmodule\n"),
];
const COMPACT_DS_NEGATIVE: &[(&str, &str)] = &[
    ("list-count", ".ds [1,2]\n"),
    ("negative-count", ".ds -1\n"),
    ("count-overflows-u32", ".ds $ffffffff+1\n"),
    ("missing-count", ".ds\n"),
    ("unknown-count", ".ds Missing\n"),
];

#[test]
fn native_compact_ds_live_rust_controls() {
    use super::native_iterable_loops::{oracle, scratch};
    let dir = scratch();
    for (name, source) in COMPACT_DS_POSITIVE {
        let bytes = oracle(&dir.0, source).unwrap_or_else(|error| panic!("{name}: {error}"));
        let expected: &[u8] = match *name {
            "flat-leading-trailing-gap" => &[0x5a],
            "flat-arithmetic-forward-gap" => &[0xa5, 0, 0, 0, 0, 0, 0, 0, 0, 8, 0x5a],
            "placed-bss-byte-reservation" | "same-region-bss-res" | "separate-region-bss" => &[4],
            "initialized-section-gap" => &[1, 0, 0, 0, 4],
            _ => unreachable!(),
        };
        assert_eq!(bytes, expected, "{name}");
    }
    for (name, body) in COMPACT_DS_NEGATIVE {
        let source = format!(".module main\n.cpu 68020\n{body}.endmodule\n");
        assert!(oracle(&dir.0, &source).is_err(), "accepted {name}");
    }
    for source in [
        ".module main\n.cpu 6502\n.org $ffff\n.ds 1\n.endmodule\n",
        ".module main\n.cpu 68020\n.org $ffffffff\n.ds 1\n.endmodule\n",
    ] {
        assert!(oracle(&dir.0, source).is_err(), "accepted next-PC overflow");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; fresh shared compact .ds reservation proof"]
fn native_compact_ds_fs_uae() {
    use super::native_iterable_loops::{build_image, oracle, scratch};
    use super::native_source_cases::{self, Case};
    let dir = scratch();
    let root = workspace_root();
    let image = std::env::var_os("OPFORGE_DS_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let selected = std::env::var("OPFORGE_DS_CASE").ok();
    let mut cases = Vec::new();
    for cpu in ["68020", "6502"] {
        for (name, source) in COMPACT_DS_POSITIVE {
            if selected.as_deref().is_some_and(|value| value != *name) {
                continue;
            }
            let source = source.replace(".cpu 68020", &format!(".cpu {cpu}"));
            let expected = oracle(&dir.0, &source).expect("current .ds positive oracle");
            cases.push(Case {
                name: format!("ds-{name}-{cpu}"),
                cpu: cpu.into(),
                files: vec![("entry.asm", source)],
                expected: Some(expected),
            });
        }
        for (name, body) in COMPACT_DS_NEGATIVE {
            if selected.as_deref().is_some_and(|value| value != *name) {
                continue;
            }
            cases.push(Case {
                name: format!("ds-{name}-{cpu}"),
                cpu: cpu.into(),
                files: vec![(
                    "entry.asm",
                    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n"),
                )],
                expected: None,
            });
        }
        if selected
            .as_deref()
            .is_none_or(|value| value == "next-pc-overflow")
        {
            let maximum = if cpu == "6502" { "$ffff" } else { "$ffffffff" };
            cases.push(Case {
                name: format!("ds-next-pc-overflow-{cpu}"),
                cpu: cpu.into(),
                files: vec![(
                    "entry.asm",
                    format!(".module main\n.cpu {cpu}\n.org {maximum}\n.ds 1\n.endmodule\n"),
                )],
                expected: None,
            });
        }
    }
    assert!(
        !cases.is_empty(),
        "reservation localization selected no cases"
    );
    native_source_cases::run(&root, &image, &cases);
}
