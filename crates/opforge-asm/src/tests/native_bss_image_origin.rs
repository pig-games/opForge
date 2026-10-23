//! BSS image-origin and alignment routing parity contracts.

use super::*;

// The importing section occupies $1000..$1003; its dependency follows at $1004.
// Both dependency labels are bare labels in one section, so retain their
// complete section rather than treating them as independently reachable blocks.

#[test]
fn native_bss_image_origin_live_rust_bss_align_pad() {
    // Level A: the Rust directive owner records three padding bytes after a
    // one-byte BSS reservation at address zero. It emits no image bytes.
    let mut symbols = SymbolTable::new();
    let registry = default_registry();
    let mut asm = make_asm_line(&mut symbols, &registry);
    assert_eq!(process_line(&mut asm, ".cpu 68020", 0, 1), LineStatus::Ok);
    assert_eq!(
        process_line(&mut asm, ".section zero, kind=bss", 0, 1),
        LineStatus::Ok
    );
    assert_eq!(
        process_line(&mut asm, ".res byte, 1", 0, 1),
        LineStatus::DirDs
    );
    assert_eq!(asm.aux_value(), 1);
    assert!(asm.bytes().is_empty());
    assert_eq!(process_line(&mut asm, ".align 4", 1, 1), LineStatus::DirDs);
    assert_eq!(asm.aux_value(), 3);
    assert!(asm.bytes().is_empty());
}
