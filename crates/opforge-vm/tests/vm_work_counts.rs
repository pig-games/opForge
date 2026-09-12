// SPDX-License-Identifier: GPL-3.0-or-later

use types::vm_work;
use vm::bytecode::{execute_program, OP_EMIT_OPERAND, OP_EMIT_U8, OP_END};

#[test]
fn real_dispatch_counts_exclude_operands_and_include_failing_dispatch() {
    let _session = vm_work::install();
    let program = [OP_EMIT_U8, 42, OP_EMIT_OPERAND, 0, OP_END];
    assert_eq!(execute_program(&program, &[&[8, 9]]).unwrap(), [42, 8, 9]);
    assert!(execute_program(&program, &[]).is_err());
    let report = vm_work::snapshot().unwrap();
    let row = &report["rows"][0];
    assert_eq!(row["calls"], 2);
    assert_eq!(row["steps"], 5); // 3 successful dispatches + 2 before operand error
    assert_eq!(row["repeated_within_call"], 0);
    assert_eq!(row["positions"][0]["position"], 0);
    assert_eq!(row["positions"][1]["position"], 2);
    assert_eq!(row["positions"][2]["position"], 4);
}
