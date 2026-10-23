// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

#[test]
fn serialized_mos_abs32_fixup_matches_live_cross_section_long_oracle() {
    // Level B: serialized family program versus a live Rust relocation oracle.
    // Does not prove native execution or timing.
    let assembler = run_passes(&[
        ".module main",
        ".cpu 6502",
        ".region ram, $2000, $20ff",
        ".section code, kind=code",
        "pointer: .long target + 5",
        ".endsection",
        ".section data, kind=data",
        ".byte $aa",
        "target: .byte 0",
        ".endsection",
        ".pack in ram : code, data",
        ".output \"build/out.hunk\", format=hunk, sections=code,data",
        ".endmodule",
    ]);
    let code = assembler.sections().get("code").expect("code section");
    let [oracle] = code.output_fixups.as_slice() else {
        panic!("live Rust assembler must produce one relocation oracle")
    };
    assert_ne!(oracle.encoded_addend, 0);
    let offset = usize::try_from(oracle.offset).expect("host offset");
    assert_eq!(
        &code.bytes[offset..offset + 4],
        &oracle.encoded_addend.to_le_bytes()
    );

    let package_bytes = build_hierarchy_package_from_registry(&default_registry())
        .expect("serialize default package");
    let model = load_opasm_model_from_package_bytes(&package_bytes);
    let resolved = model
        .resolve_pipeline("m6502", None)
        .expect("resolve serialized m6502 package");
    let projected = model
        .execute_fixup_program(
            &resolved,
            families::mos6502::package_programs::FIXUP_ABSOLUTE_LONG,
            &[vm::fixup_vm::PortableFixupInput {
                value: vm::fixup_vm::PortableDeferredValue::Resolved(i64::from(
                    oracle.encoded_addend,
                )),
                target_reference: true,
                relocation_target: oracle.target_section_name().map(str::to_string),
                relocation_addend: None,
            }],
            vm::fixup_vm::PortableFixupContext { position: 0x2000 },
        )
        .expect("execute serialized MOS absolute-long fixup program");

    assert_eq!(projected.bytes, code.bytes);
    let [fixup] = projected.fixups.as_slice() else {
        panic!("package program must produce one portable relocation")
    };
    assert_eq!(fixup.offset, oracle.offset);
    assert_eq!(fixup.width, 4);
    assert_eq!(fixup.kind, vm::fixup_vm::PortableOutputFixupKind::Absolute);
    assert_eq!(fixup.target, oracle.target_section_name().unwrap());
    assert_eq!(fixup.encoded_addend, oracle.encoded_addend);

    // Native re-encodes the normalized scalar without a target reference.
    // This must preserve the exact bytes without creating a second relocation.
    let normalized = model
        .execute_fixup_program(
            &resolved,
            families::mos6502::package_programs::FIXUP_ABSOLUTE_LONG,
            &[vm::fixup_vm::PortableFixupInput {
                value: vm::fixup_vm::PortableDeferredValue::Resolved(i64::from(
                    oracle.encoded_addend,
                )),
                target_reference: false,
                relocation_target: None,
                relocation_addend: None,
            }],
            vm::fixup_vm::PortableFixupContext { position: 0x2000 },
        )
        .expect("encode section-relative scalar without another relocation");
    assert_eq!(normalized.bytes, projected.bytes);
    assert!(normalized.fixups.is_empty());
}
