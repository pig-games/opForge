//! Native implicit-origin parity proofs.

use super::*;

const IMPLICIT_ORIGIN_SOURCE: &[u8] =
    b"        .word target-1\n        nop\n        jmp exec\nexec    nop\ntarget  .byte $aa\n";

const EXPLICIT_ORIGIN_SOURCE: &[u8] = b"        .org $1200\n        .word target-1\n        nop\n        jmp exec\nexec    nop\ntarget  .byte $aa\n";

const PRE_ORG_OUTPUT_SOURCE: &[u8] = b"        .byte $aa\n        .org $10\n        .byte $bb\n";

const MIXED_UNPLACED_AND_PLACED_SOURCE: &[u8] = b".module app.main\n        .cpu 65c02\n        .region rom, $1000, $10ff\n        .section low, kind=code\n        .byte $aa\n        .endsection\n        .section high, kind=code\n        .byte $bb\n        .endsection\n        .place high in rom\n.endmodule\n.end\n";

struct OriginOracleDir(PathBuf);

impl Drop for OriginOracleDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn rust_origin_bytes(source: &[u8]) -> Vec<u8> {
    let case_dir = create_temp_dir("native-default-origin-live-rust-cli");
    let _guard = OriginOracleDir(case_dir.clone());
    let input_path = case_dir.join("input.asm");
    let output_path = case_dir.join("oracle.bin");
    fs::write(&input_path, source).expect("write origin oracle source");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        output_path.to_string_lossy().as_ref(),
        "--cpu",
        "65c02",
    ]);
    let mut config = validate_cli(&cli).expect("validate origin Rust CLI oracle");
    config.out_dir = Some(case_dir);
    run_with_validated_cli_with_context(&cli, &config).expect("run origin Rust CLI oracle");
    fs::read(output_path).expect("read origin Rust CLI oracle")
}

#[test]
fn native_default_origin_rust_oracle() {
    // Proof level A. This proves live Rust begins an implicit session at zero
    // and honors an explicit nonzero .org in both label arithmetic and an
    // absolute instruction operand. It does not prove native initialization.
    assert_eq!(
        rust_origin_bytes(IMPLICIT_ORIGIN_SOURCE),
        vec![0x06, 0x00, 0xea, 0x4c, 0x06, 0x00, 0xea, 0xaa]
    );
    assert_eq!(
        rust_origin_bytes(EXPLICIT_ORIGIN_SOURCE),
        vec![0x06, 0x12, 0xea, 0x4c, 0x06, 0x12, 0xea, 0xaa]
    );
    assert_eq!(
        rust_origin_bytes(PRE_ORG_OUTPUT_SOURCE),
        [vec![0xaa], vec![0; 15], vec![0xbb]].concat()
    );
    assert_eq!(rust_origin_bytes(MIXED_UNPLACED_AND_PLACED_SOURCE), [0xbb]);
}
