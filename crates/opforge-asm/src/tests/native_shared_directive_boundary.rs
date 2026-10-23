//! Shared directive ownership: live Rust oracles and bounded real-native cases.
use super::*;

fn data_source(cpu: &str) -> String {
    let mut source = format!(".cpu {cpu}\n.org 0\nBASE .const $20\n");
    for index in 0..8 {
        source.push_str(&format!(
            "data{index} .byte (BASE+{index}),0,#7\n.word (2+3)*4\n.long $1234+1\n"
        ));
    }
    source.push_str(if cpu == "m6502" {
        " lda ($20),y\n nop\n"
    } else {
        " move.l (a0)+,d0\n nop\n"
    });
    source
}

fn switched_data_source() -> String {
    ["m68020", "m6502", "m68020"]
        .into_iter()
        .map(|cpu| format!(".cpu {cpu}\n.byte $80\n.word $1234\n.long $12345678\n"))
        .collect()
}

#[test]
fn native_shared_directive_switch_rust_oracle() {
    // Level A: byte order changes with the active CPU; byte data is invariant.
    assert_eq!(
        rust_bytes(&switched_data_source()),
        [
            0x80, 0x12, 0x34, 0x12, 0x34, 0x56, 0x78, 0x80, 0x34, 0x12, 0x78, 0x56, 0x34, 0x12,
            0x80, 0x12, 0x34, 0x12, 0x34, 0x56, 0x78,
        ]
    );
}

fn rust_bytes(source: &str) -> Vec<u8> {
    let lines = source.lines().collect::<Vec<_>>();
    let (entries, diagnostics) = assemble_source_entries_with_runtime_mode(&lines, true)
        .expect("shared directive Rust assembly");
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    entries.into_iter().map(|(_, byte)| byte).collect()
}

#[test]
fn native_shared_directive_rust_oracles() {
    // Level A: complete source outputs, including a retained instruction surface.
    for cpu in ["m6502", "m68020"] {
        let bytes = rust_bytes(&data_source(cpu));
        let mut expected = Vec::new();
        for index in 0..8 {
            expected.extend_from_slice(&[0x20 + index, 0, 7]);
            expected.extend_from_slice(if cpu == "m6502" {
                &[20, 0, 0x35, 0x12, 0, 0]
            } else {
                &[0, 20, 0, 0, 0x12, 0x35]
            });
        }
        expected.extend_from_slice(if cpu == "m6502" {
            &[0xb1, 0x20, 0xea]
        } else {
            &[0x20, 0x18, 0x4e, 0x71]
        });
        assert_eq!(bytes, expected, "{cpu}");
        let source = format!(".cpu {cpu}\nlabel .nop\n");
        let (_, diagnostics) =
            assemble_source_entries_with_runtime_mode(&source.lines().collect::<Vec<_>>(), true)
                .expect("unknown dot Rust diagnostic");
        assert!(diagnostics.iter().any(|d| d.contains("Unknown directive")));
    }
}

#[test]
fn native_shared_directive_boundary_contract() {
    let root = workspace_root().join("native/motorola68000/amigaos");
    let prepare = fs::read_to_string(root.join("assembler/prepare/prepare.asm")).unwrap();
    assert!(source_contains_in_order(
        &prepare,
        &[
            "statement",
            "cmpi.b #7, (a0)",
            "beq.w directive",
            "bsr.w name",
            "bra.w operands",
            "directive",
        ]
    ));
    for role in ["ByteDirective", "WordDirective", "LongDirective"] {
        assert!(prepare.contains(&format!(
            "cmp.w package.Header.{role}(a2), d7\n\tbeq.w dataScalar"
        )));
    }
    let scalar = prepare
        .split_once("\nscalar\n")
        .unwrap()
        .1
        .split_once("\noneScalar\n")
        .unwrap()
        .0;
    assert!(scalar.contains("bsr.w compile"));
    assert!(!scalar.contains("operands"));
}
