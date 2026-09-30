// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

fn hunk_field_instruction(instruction: &str, cpu: &str, placed: bool) -> Assembler {
    let instruction = format!("    {instruction}");
    run_passes(&[
        ".module main",
        cpu,
        "Frame .struct",
        "pointer .long ?",
        "tail .long ?",
        ".endstruct",
        "offset = Frame.tail",
        ".region codeRam, $2000, $20ff",
        ".region dataRam, $3000, $30ff",
        ".section code, kind=code",
        &instruction,
        "    rts",
        ".endsection",
        ".section bss, kind=bss",
        ".res byte, 8",
        "target .res byte, 16",
        ".endsection",
        if placed { ".place code in codeRam" } else { "" },
        if placed { ".place bss in dataRam" } else { "" },
        ".output \"build/fields.hunk\", format=hunk, sections=code,bss",
        ".endmodule",
    ])
}

#[test]
fn hunk_instruction_named_addends_retain_base_relocation() {
    let cases = [
        ("move.l target+Frame.tail,d3", [0x26, 0x39], 12u32),
        ("move.l d3,target+Frame.tail", [0x23, 0xc3], 12),
        ("lea target+Frame.tail,a3", [0x47, 0xf9], 12),
        ("move.l offset+target,d3", [0x26, 0x39], 12),
        ("move.l target-offset,d3", [0x26, 0x39], 4),
        ("move.l target+Frame.tail*2,d3", [0x26, 0x39], 16),
        ("move.l target+4,d3", [0x26, 0x39], 12),
        ("move.l target+main.Frame.tail,d3", [0x26, 0x39], 12),
    ];
    for (cpu, placed) in [(".cpu 68000", true), (".cpu 68020", false)] {
        for (instruction, opcode, addend) in cases {
            let assembler = hunk_field_instruction(instruction, cpu, placed);
            let code = &assembler.sections()["code"];
            assert_eq!(&code.bytes[..2], &opcode, "{instruction}");
            assert_eq!(&code.bytes[2..6], &addend.to_be_bytes(), "{instruction}");
            assert_eq!(code.output_fixups.len(), 1, "{instruction}");
            assert_eq!(code.output_fixups[0].offset, 2, "{instruction}");
            assert_eq!(code.output_fixups[0].target_section_name(), Some("bss"));

            let output = &assembler.root_metadata.linker_outputs[0];
            let hunk = build_linker_output_payload(output, assembler.sections()).unwrap();
            let words = hunk
                .chunks_exact(4)
                .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
                .collect::<Vec<_>>();
            assert!(
                words.windows(5).any(|words| words == [1004, 1, 1, 2, 0]),
                "CODE extension must relocate to BSS at offset 2: {instruction}"
            );
        }
    }
}

#[test]
fn hunk_instruction_absolute_fields_and_displacements_do_not_relocate() {
    for instruction in [
        "move.l #Frame.tail,d3",
        "move.l Frame.tail,d3",
        "move.l Frame.tail(a0),d3",
        "move.l #offset+Frame.tail,d3",
    ] {
        let assembler = hunk_field_instruction(instruction, ".cpu 68020", false);
        assert!(
            assembler.sections()["code"].output_fixups.is_empty(),
            "{instruction}"
        );
        build_linker_output_payload(
            &assembler.root_metadata.linker_outputs[0],
            assembler.sections(),
        )
        .expect("absolute fields remain valid without relocations");
    }
}

#[test]
#[ignore = "known package path certifies unsupported address arithmetic; tracked in compact frontend plan"]
fn hunk_instruction_addends_cannot_contain_another_address_base() {
    for instruction in ["move.l target+target,d3", "move.l #Frame.tail-target,d3"] {
        let assembler = hunk_field_instruction(instruction, ".cpu 68020", false);
        assert!(
            build_linker_output_payload(
                &assembler.root_metadata.linker_outputs[0],
                assembler.sections()
            )
            .is_err(),
            "unsupported address arithmetic must fail closed: {instruction}"
        );
    }
}

#[test]
#[ignore = "known complex immediate expression bypasses relocation; tracked in compact frontend plan"]
fn hunk_instruction_named_immediate_addends_require_relocation() {
    let assembler = hunk_field_instruction("move.l #target+Frame.tail,d3", ".cpu 68020", false);
    let code = &assembler.sections()["code"];
    assert_eq!(&code.bytes[2..6], &12u32.to_be_bytes());
    assert_eq!(
        code.output_fixups.len(),
        1,
        "long immediate must retain the target relocation"
    );
}

#[test]
#[ignore = "known placed-section stabilization rebases symbols twice; tracked in compact frontend plan"]
fn hunk_68020_placed_section_stabilization_must_not_rebase_twice() {
    // Literal and bare-label controls reproduce the independent placement bug.
    for (instruction, addend) in [("move.l target,d3", 8u32), ("move.l target+4,d3", 12)] {
        let assembler = hunk_field_instruction(instruction, ".cpu 68020", true);
        assert_eq!(
            &assembler.sections()["code"].bytes[2..6],
            &addend.to_be_bytes()
        );
    }
}

#[test]
fn hunk_struct_offsets_are_absolute_while_address_symbols_relocate() {
    let source = br#".module main
.cpu 68020
Frame .struct
pointer .long ?
tail .word ?
.endstruct
frameBytes = Frame.tail + 2
.section code, kind=code
start:
	lea -frameBytes(sp),sp
	.long target
	rts
.endsection
.section data, kind=data
target: .long 0
.endsection
.output "build/struct-constants.hunk", format=hunk, sections=code,data
.endmodule
"#;
    let out = create_temp_dir("hunk-struct-absolute-constants");
    struct TempDir(std::path::PathBuf);
    impl Drop for TempDir {
        fn drop(&mut self) {
            let _ = fs::remove_dir_all(&self.0);
        }
    }
    let _guard = TempDir(out.clone());
    fs::create_dir_all(out.join("build")).expect("create Hunk output directory");
    let input = out.join("input.asm");
    fs::write(&input, source).expect("write struct constant source");
    let cli = Cli::parse_from([
        "opForge".to_string(),
        input.to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "m68020".to_string(),
    ]);
    let mut config = validate_cli(&cli).expect("validate struct constant Hunk CLI");
    config.out_dir = Some(out.clone());
    run_with_validated_cli_with_context(&cli, &config)
        .expect("assemble struct-derived stack displacement and address relocation");

    let hunk = fs::read(out.join("build/struct-constants.hunk")).expect("read Hunk output");
    let words = hunk
        .chunks_exact(4)
        .map(|chunk| u32::from_be_bytes(chunk.try_into().expect("Hunk word")))
        .collect::<Vec<_>>();
    assert!(
        hunk.windows(4)
            .any(|bytes| bytes == [0x4f, 0xef, 0xff, 0xfa]),
        "Frame.tail + 2 must encode as the absolute -6 stack displacement: {hunk:02X?}"
    );
    assert!(
        words.contains(&1004),
        "the real target address must still emit a HUNK_RELOC32 record: {words:08X?}"
    );
}
