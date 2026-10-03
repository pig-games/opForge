//! Package-defined member fields retain their base symbol and Hunk relocation.
use super::*;

fn source(joined: bool) -> String {
    let target = |name: &str| {
        if joined {
            format!("{name}.l")
        } else {
            format!("({name}).l")
        }
    };
    format!(
        ".module member_probe\n.cpu m68020\n.use state\nFrame .struct\nw .byte ?\n.endstruct\nprobe .macro\n cmpi.w #7,Target.l\n.endmacro\n.section code,kind=code\nentry\n cmpi.w #7,{}\n cmpi.w #7,{}\n cmpi.w #7,(state.Value+4).l\n cmpi.w #7,Target\n .probe\n lea {},a1\n cmpa.l {},a0\n adda.w (Target+4).l,a0\n adda.w (8).w,a0\n move.w 4(a0,d3.w),d0\n rts\n.endsection\n.section data,kind=data\n.byte Frame.w\n.endsection\n.section bss,kind=bss\nTarget .res long,2\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n.module state\n.cpu m68020\n.pub\n.section bss,kind=bss\nValue .res long,2\n.endsection\n.endmodule\n",
        target("Target"),
        target("state.Value"),
        target("Target"),
        target("state.Value"),
    )
}

#[test]
fn compact_members_live_rust_hunk() {
    let joined = hunk_sections::rust_hunk_bytes(&source(true));
    let wrapped = hunk_sections::rust_hunk_bytes(&source(false));
    assert_eq!(
        joined, wrapped,
        "joined and explicit member syntax must agree"
    );
    assert!(joined.windows(4).any(|bytes| bytes == [0x0c, 0x79, 0, 7]));
    assert!(joined.windows(2).any(|bytes| bytes == [0x43, 0xf9]));
}

#[test]
#[ignore = "requires configured FS-UAE; package members, qualified bases and Hunk addends"]
fn compact_members_hunk_fs_uae() {
    for joined in [false, true] {
        hunk_sections::native_hunk_source_with_allocation(&source(joined), 16);
    }
}

#[test]
fn compact_members_cpu_spellings_rust_oracle() {
    let expected = hunk_sections::rust_hunk_bytes(&source(true));
    for spelling in ["68020", "\"m68020\""] {
        let input = source(true).replace(".cpu m68020", &format!(".cpu {spelling}"));
        assert_eq!(hunk_sections::rust_hunk_bytes(&input), expected);
    }
}

#[test]
#[ignore = "requires FS-UAE; numeric/quoted CPU lowering survives contextual member callbacks"]
fn compact_members_cpu_spellings_fs_uae() {
    for (joined, spelling) in [(false, "68020"), (true, "\"m68020\"")] {
        let input = source(joined).replace(".cpu m68020", &format!(".cpu {spelling}"));
        hunk_sections::native_hunk_source_with_allocation(&input, 16);
    }
}

// Inline heads must preserve the same member identities and Hunk relocations.
fn inline_source(colon: bool) -> String {
    source(true).replacen(
        " cmpi.w #7,Target.l",
        if colon {
            "inline: cmpi.w #7,Target.l"
        } else {
            "inline cmpi.w #7,Target.l"
        },
        1,
    )
}

#[test]
fn compact_members_inline_label_rust_oracle() {
    let expected = hunk_sections::rust_hunk_bytes(&source(true));
    for colon in [false, true] {
        assert_eq!(
            hunk_sections::rust_hunk_bytes(&inline_source(colon)),
            expected
        );
    }
}

#[test]
#[ignore = "requires FS-UAE; member operands after bare and colon inline labels"]
fn compact_members_inline_label_fs_uae() {
    for colon in [false, true] {
        hunk_sections::native_hunk_source_with_allocation(&inline_source(colon), 16);
    }
}
