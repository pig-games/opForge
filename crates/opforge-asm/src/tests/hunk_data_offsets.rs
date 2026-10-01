// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

#[test]
fn hunk_data_offsets_same_section_are_absolute_with_optional_pointer_relocation() {
    for kind in ["code", "data"] {
        for pointer in [false, true] {
            let assemble = |placed: bool| {
                let mut lines = vec![
                    ".module main".to_string(),
                    ".cpu 68020".to_string(),
                    ".region ram, $2000, $20ff".to_string(),
                    ".section entry, kind=code".to_string(),
                    " rts".to_string(),
                    ".endsection".to_string(),
                    format!(".section body, kind={kind}"),
                    "start: .byte 0".to_string(),
                    " .byte end-start".to_string(),
                    " .word start-end".to_string(),
                    " .long ((end-start)+2)*3".to_string(),
                    " .emit long, end-start+1".to_string(),
                    if pointer { "end: .long start" } else { "end:" }.to_string(),
                    ".endsection".to_string(),
                ];
                if placed {
                    lines.push(".place entry in ram".to_string());
                    lines.push(".place body in ram".to_string());
                }
                lines.extend([
                    ".output \"build/offsets.hunk\", format=hunk, sections=entry,body".to_string(),
                    ".endmodule".to_string(),
                ]);
                run_passes(&lines.iter().map(String::as_str).collect::<Vec<_>>())
            };
            let mut payloads = Vec::new();
            for placed in [false, true] {
                let assembler = assemble(placed);
                let section = &assembler.sections()["body"];
                let mut expected = vec![0, 12, 0xff, 0xf4, 0, 0, 0, 42, 0, 0, 0, 13];
                if pointer {
                    expected.extend_from_slice(&[0; 4]);
                }
                assert_eq!(section.bytes, expected, "{kind} placed={placed}");
                assert_eq!(section.output_fixups.len(), usize::from(pointer));
                if pointer {
                    assert_eq!(section.output_fixups[0].offset, 12);
                    assert_eq!(section.output_fixups[0].target_section_name(), Some("body"));
                }
                let payload = build_linker_output_payload(
                    &assembler.root_metadata.linker_outputs[0],
                    assembler.sections(),
                )
                .expect("same-section offsets are absolute Hunk data");
                let mut expected_words = vec![
                    1011,
                    0,
                    2,
                    0,
                    1,
                    1,
                    expected.len() as u32 / 4,
                    1001,
                    1,
                    0x4e750000,
                    1010,
                ];
                expected_words.extend([
                    if kind == "code" { 1001 } else { 1002 },
                    expected.len() as u32 / 4,
                ]);
                expected_words.extend(
                    expected
                        .chunks_exact(4)
                        .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap())),
                );
                if pointer {
                    expected_words.extend([1004, 1, 1, 12, 0]);
                }
                expected_words.push(1010);
                let expected_payload: Vec<_> = expected_words
                    .into_iter()
                    .flat_map(u32::to_be_bytes)
                    .collect();
                assert_eq!(
                    payload, expected_payload,
                    "{kind} placed={placed} pointer={pointer}"
                );
                payloads.push(payload);
            }
            assert_eq!(payloads[0], payloads[1]);
        }
    }
}

#[test]
fn hunk_data_offsets_cross_section_difference_remains_unsupported() {
    let assembler = run_passes(&[
        ".module main",
        ".section first, kind=data",
        "start: .long end-start",
        ".endsection",
        ".section second, kind=data",
        "end: .long 0",
        ".endsection",
        ".output \"build/offsets.hunk\", format=hunk, sections=first,second",
        ".endmodule",
    ]);
    let error = build_linker_output_payload(
        &assembler.root_metadata.linker_outputs[0],
        assembler.sections(),
    )
    .expect_err("cross-section subtraction still requires unsupported fixups");
    assert!(
        error.message().contains("symbolic .long expression"),
        "{}",
        error.message()
    );
}
