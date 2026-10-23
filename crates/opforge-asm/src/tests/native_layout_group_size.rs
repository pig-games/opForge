// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use super::*;

#[test]
fn native_layout_group_size_multi_member_structural_name_group() {
    // Level A: Rust's layout authority merges both concrete members with the
    // same structural section name and reserves their complete byte sum.
    let assembler = run_passes(&[
        ".module main",
        ".cpu 6502",
        ".region ram, $4000, $40ff",
        ".section code, align=1",
        ".byte 1,2",
        ".endsection",
        ".section code, align=1",
        ".byte 3,4,5",
        ".endsection",
        ".place code in ram",
        ".endmodule",
    ]);
    let code = assembler
        .sections()
        .get("code")
        .expect("merged code section");
    assert_eq!(code.base_addr, Some(0x4000));
    assert_eq!(code.bytes, [1, 2, 3, 4, 5]);
    assert_eq!(
        assembler.regions().get("ram").expect("RAM region").cursor,
        0x4005
    );
}
