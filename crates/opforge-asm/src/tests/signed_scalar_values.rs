//! Scalar expression values retain their sign independently of address shadows.
use super::*;

fn longs(cpu: &str, body: &str) -> Vec<i32> {
    let source = if body.starts_with(".module") {
        body.to_string()
    } else {
        format!(".cpu {cpu}\n{body}")
    };
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    let bytes = assembler
        .image()
        .entries()
        .unwrap()
        .into_iter()
        .map(|(_, byte)| byte)
        .collect::<Vec<_>>();
    assert_eq!(bytes.len() % 4, 0);
    bytes
        .chunks_exact(4)
        .map(|word| {
            let word = word.try_into().unwrap();
            if cpu == "m6502" {
                i32::from_le_bytes(word)
            } else {
                i32::from_be_bytes(word)
            }
        })
        .collect()
}

#[test]
fn signed_scalar_definition_forms() {
    for cpu in ["m6502", "m68020"] {
        for definition in [
            "n = -17",
            "n .const -17",
            "n := -17",
            "n .var -17",
            "n .set -17",
            "n :?= -17\nn :?= 9",
        ] {
            assert_eq!(
                longs(
                    cpu,
                    &format!("{definition}\ncopy = n\n.long n/7,n%7,copy<0\n")
                ),
                [-2, -3, 1],
                "{cpu}: {definition}"
            );
        }
    }
}

#[test]
fn signed_scalar_forward_dependency_chain() {
    for cpu in ["m6502", "m68020"] {
        assert_eq!(longs(cpu, "first = second/7\nsecond .const third\nthird = -17\n.long first,second%7,first<0\n"), [-2,-3,1]);
    }
}

#[test]
fn signed_scalar_mutable_updates() {
    for cpu in ["m6502", "m68020"] {
        assert_eq!(longs(cpu, "n := -17\nsaved = n\nn /= 7\n.long n,saved/7\nn -= 1\nn *= 7\nn %= 8\n.long n\nn += -4\n.long n\nn .set -17\n.long n/7\n"), [-2,-2,-5,-9,-2]);
    }
}

#[test]
fn signed_scalar_structured_to_scalar_reassignment() {
    assert_eq!(longs("m6502", "n .var {-17,-9}\n.long n[0]/7\nn .set -17\n.long n/7\nn .set {3,4}\n.long n[1]\nn .set -9\n.long n%7\n"), [-2,-2,4,-2]);
}

#[test]
fn signed_scalar_struct_fields_and_exact_binding_precedence() {
    assert_eq!(
        longs("m6502", "Pair .struct\ndelta .long ?\n.endstruct\np := Pair { delta: -17 }\n.long p.delta/7,Pair.delta\n"),
        [-2, 0]
    );
    // Exact dotted constants take precedence even before their definition.
    assert_eq!(
        longs("m6502", "Pair .struct\ndelta .long ?\n.endstruct\np := Pair { delta: -17 }\n.long p.delta/7\np.delta = -9\n.long p.delta/7\n"),
        [-1, -1]
    );
}

#[test]
fn signed_scalar_unsigned_constants_and_high_address_labels() {
    for cpu in ["m6502", "m68020"] {
        assert_eq!(longs(cpu, "unsigned = $ffffffff\nnegative = -1\nwide = $100000001\n.long unsigned/7,unsigned>0,negative/7,negative<0,wide/$100000000\n"), [613566756,1,0,1,1]);
    }
    assert_eq!(
        longs("m68020", ".org $80000000\nhigh\n.long high/2,high>0\n"),
        [0x40000000, 1]
    );
}

#[test]
fn signed_scalar_qualified_import_reference() {
    assert_eq!(longs("m6502", ".module library\n.cpu m6502\n.pub\nvalue = -17\n.endmodule\n.module consumer\n.cpu m6502\n.use library as lib\ncopy = lib.value\n.long copy/7,lib.value%7\n.endmodule\n"), [-2,-3]);
}

#[test]
fn signed_scalar_host_and_vm_instruction_leaves() {
    for force_host in [false, true] {
        let mut symbols = SymbolTable::new();
        let registry = default_registry();
        let mut asm = AsmLine::with_cpu(&mut symbols, m68000_cpu_id, &registry);
        if force_host {
            asm.opthread_expr_eval_force_host_families
                .push("motorola68000".into());
        }
        assert_eq!(asm.process("n = -17", 1, 0, 2), LineStatus::DirEqu);
        assert_eq!(asm.symbols().lookup("n"), Some((-17i32) as u32));
        assert_eq!(
            asm.process(" move.l #n/7,d0", 2, 0, 2),
            LineStatus::Ok,
            "{:?}",
            asm.error()
        );
        assert_eq!(
            asm.bytes(),
            [0x20, 0x3c, 0xff, 0xff, 0xff, 0xfe],
            "forced host: {force_host}"
        );
        assert_eq!(
            asm.process("minimum = -9223372036854775807-1", 3, 0, 2),
            LineStatus::DirEqu
        );
        for expression in ["minimum/-1", "minimum%-1", "-minimum"] {
            assert_eq!(
                asm.process(&format!(".long {expression}"), 4, 0, 2),
                LineStatus::Error
            );
            assert!(
                asm.error().unwrap().message().contains("overflow"),
                "forced host: {force_host}; {expression}"
            );
        }
    }
}
