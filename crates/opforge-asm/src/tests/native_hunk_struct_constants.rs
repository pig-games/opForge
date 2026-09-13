// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

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
