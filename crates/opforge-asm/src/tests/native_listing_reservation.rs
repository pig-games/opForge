//! Reservation listing and semantic transport contracts.

use super::*;

#[test]
fn native_listing_reservation_host_unit_scaling_and_empty_image() {
    // Level A: live Rust proves sizing and non-emission, not native behavior.
    for (unit, count, expected) in [
        ("byte", "4", 4),
        ("word", "2", 4),
        ("long", "1", 4),
        ("(WoRd)", "2", 4),
        ("(( long ))", "1", 4),
        ("1+2", "2", 6),
        ("byte", "0", 0),
    ] {
        let mut symbols = SymbolTable::new();
        let registry = default_registry();
        let mut asm = make_asm_line(&mut symbols, &registry);
        assert_eq!(process_line(&mut asm, ".cpu 68000", 0, 1), LineStatus::Ok);
        assert_eq!(
            process_line(&mut asm, ".section vars, kind=bss", 0, 1),
            LineStatus::Ok
        );
        let source = format!(".res {unit}, {count}");
        assert_eq!(
            process_line(&mut asm, &source, 0x8001, 1),
            LineStatus::DirDs
        );
        assert_eq!(asm.aux_value(), expected, "{source}");
        assert!(asm.bytes().is_empty(), "RES must not emit image bytes");
    }
}

#[test]
fn native_listing_reservation_host_scalar_domain_requires_more_than_low32() {
    // Level A: these distinct live Rust outcomes define the missing native
    // scalar boundary. They do not establish native parity for this domain.
    for (count, expected) in [
        ("$80000000+0", Some(0x8000_0000_u32)),
        ("-2147483648", None),
        ("$ffffffff+1", None),
        ("($ffffffff+1)-1", Some(u32::MAX)),
    ] {
        let mut symbols = SymbolTable::new();
        let registry = default_registry();
        let mut asm = make_asm_line(&mut symbols, &registry);
        assert_eq!(process_line(&mut asm, ".cpu 68020", 0, 1), LineStatus::Ok);
        assert_eq!(
            process_line(&mut asm, ".section zero, kind=bss", 0, 1),
            LineStatus::Ok
        );
        let status = process_line(&mut asm, &format!(".res byte, {count}"), 0, 1);
        match expected {
            Some(extent) => {
                assert_eq!(
                    status,
                    LineStatus::DirDs,
                    "{count}: {}",
                    asm.error_message()
                );
                assert_eq!(asm.aux_value(), extent, "{count}");
                assert!(asm.bytes().is_empty());
            }
            None => assert_eq!(status, LineStatus::Error, "{count}"),
        }
    }
}

#[test]
fn native_listing_reservation_host_rejects_invalid_extent() {
    // Level A: live Rust rejection authority, not native diagnostic proof.
    for (source, needle) in [
        (".res 0, 1", "greater than zero"),
        (".res -1, 2", "non-negative"),
        (".res byte, -1", "non-negative"),
        (".res $ffffffff, 2", "overflow"),
    ] {
        let mut symbols = SymbolTable::new();
        let registry = default_registry();
        let mut asm = make_asm_line(&mut symbols, &registry);
        assert_eq!(process_line(&mut asm, ".cpu 68000", 0, 1), LineStatus::Ok);
        assert_eq!(
            process_line(&mut asm, ".section vars, kind=bss", 0, 1),
            LineStatus::Ok
        );
        assert_eq!(
            process_line(&mut asm, source, 0x8001, 1),
            LineStatus::Error,
            "{source}"
        );
        assert!(
            asm.error_message().to_ascii_lowercase().contains(needle),
            "{source}: {}",
            asm.error_message()
        );
    }

    let mut symbols = SymbolTable::new();
    let registry = default_registry();
    let mut asm = make_asm_line(&mut symbols, &registry);
    assert_eq!(process_line(&mut asm, ".cpu 6502", 0, 1), LineStatus::Ok);
    assert_eq!(
        process_line(&mut asm, ".section vars, kind=bss", 0, 1),
        LineStatus::Ok
    );
    assert_eq!(
        process_line(&mut asm, ".res byte, 2", 0xffff, 1),
        LineStatus::Error
    );
    assert!(asm.error_message().contains(".res span"));
}
