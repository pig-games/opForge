//! Package-owned call arguments and single-class masks use numeric operands.
use super::*;

const BODY: &str = " fsincos fp0,.pair(fp6,fp7)\n fmovem fp0/fp2,(a0)\n fmovem fp4/fp6,-(a1)\n fmovem (a0)+,fp1/fp3\n fmovem (a1)+,fp5/fp7\n fmovem fp0-fp7,(a2)\n fsincos fp0,.pair(fp6,fp7,42,fp0)\n fmovem.l fpcr/fpsr,(a3)\n fmovem.l (a4),fpcr/fpiar\n";

fn input(cpu: &str, body: &str) -> String {
    source(&format!(
        ".cpu {cpu}\n.fpu {}\n{body}",
        if cpu == "m68040" { "68040" } else { "68881" }
    ))
}

#[test]
fn compact_fpu_operand_rust_oracles() {
    let text = input("m68020", BODY);
    let bytes = oracle(&[("main.asm", &text)]).unwrap();
    assert_eq!(&bytes[..4], [0xf2, 0, 3, 0xb6]);
    assert_eq!(bytes.len(), 36);
    for body in [
        " fsincos fp0,.pair(fp6,d7)\n",
        " fsincos fp0,.pair(fp6)\n",
        " fmovem fp0/d2,(a0)\n",
        " fmovem fp7-fp0,(a0)\n",
    ] {
        assert!(
            oracle(&[("main.asm", &input("m68020", body))]).is_err(),
            "{body}"
        );
    }
}

// Canonical selection ignores unused call arguments. These legal inputs mark
// the remaining compact preparation boundary; they are not native parity cases.
#[test]
fn compact_fpu_unused_argument_rust_contract() {
    for extra in ["\"long\"", ".pair(fp1,fp2)", "missing"] {
        let text = input("m68020", &format!(" fsincos fp0,.pair(fp6,fp7,{extra})\n"));
        assert_eq!(oracle(&[("main.asm", &text)]).unwrap(), [0xf2, 0, 3, 0xb6]);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; numeric FPU call and mask projections"]
fn compact_fpu_operands_fs_uae() {
    let text = input("m68020", BODY);
    let expected = oracle(&[("main.asm", &text)]).unwrap();
    compact_cli_cpu(
        &[("main.asm", &text)],
        &[],
        &[],
        Some(&expected),
        false,
        "m68020",
    );
}

#[test]
#[ignore = "requires configured FS-UAE; integrated FPU single-class masks"]
fn compact_fpu_integrated_masks_fs_uae() {
    let text = input("m68040", " fmovem fp0/fp2,(a0)\n fmovem (a0)+,fp1/fp3\n");
    let expected = oracle(&[("main.asm", &text)]).unwrap();
    compact_cli_cpu(
        &[("main.asm", &text)],
        &[],
        &[],
        Some(&expected),
        false,
        "m68040",
    );
}

#[test]
#[ignore = "requires configured FS-UAE; invalid call and register-mask forms"]
fn compact_fpu_operand_rejections_fs_uae() {
    for body in [
        " fsincos fp0,.pair(fp6,d7)\n",
        " fsincos fp0,.pair(fp6)\n",
        " fmovem fp0/d2,(a0)\n",
        " fmovem fp7-fp0,(a0)\n",
    ] {
        let text = input("m68020", body);
        assert!(oracle(&[("main.asm", &text)]).is_err(), "{body}");
        compact_cli_cpu(&[("main.asm", &text)], &[], &[], None, false, "m68020");
    }
}
