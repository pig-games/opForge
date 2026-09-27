use super::*;

#[test]
fn hunk_fill_qualified_computed_count_preserves_literal_data_proof() {
    let assembler = run_passes(&[
        ".module lengths",
        ".pub",
        "N = 40 + 428",
        ".endmodule",
        ".module app",
        ".use lengths as lengths",
        ".cpu m68020",
        ".section code, kind=code",
        ".fill byte, lengths.N, 0",
        ".endsection",
        ".output \"build/out.hunk\", format=hunk, sections=code",
        ".endmodule",
    ]);
    let output = assembler.root_metadata.linker_outputs.first().unwrap();
    assert_eq!(
        output.relocation_disposition,
        LinkerOutputRelocationDisposition::ProvenRelocationFree
    );
    let payload = build_linker_output_payload(output, assembler.sections()).expect("Hunk proof");
    assert!(payload.len() >= 468);
    let section = assembler.sections().get("code").expect("code section");
    assert_eq!(section.bytes, vec![0; 468]);
}

#[test]
fn hunk_fill_symbolic_value_still_requires_relocation_proof() {
    let assembler = run_passes(&[
        ".module app",
        ".cpu m68020",
        ".region ram, $2000, $20ff",
        ".section code, kind=code",
        "entry: .fill long, 1, entry",
        ".endsection",
        ".place code in ram",
        ".output \"build/out.hunk\", format=hunk, sections=code",
        ".endmodule",
    ]);
    let output = assembler.root_metadata.linker_outputs.first().unwrap();
    assert_eq!(
        output.relocation_disposition,
        LinkerOutputRelocationDisposition::Unknown
    );
    let error = build_linker_output_payload(output, assembler.sections())
        .expect_err("unproven address bytes");
    assert!(
        error.message().contains("explicit relocation-free proof"),
        "{}",
        error.message()
    );
}

#[test]
fn hunk_fill_invalid_counts_remain_errors() {
    for count in ["-1", "missing", "4294967296"] {
        let mut symbols = SymbolTable::new();
        let registry = default_registry();
        let mut asm = make_asm_line(&mut symbols, &registry);
        let status = process_line(&mut asm, &format!(" .fill byte, {count}, 0"), 0x1000, 2);
        assert_eq!(status, LineStatus::Error, "{count}");
        assert!(asm.bytes().is_empty(), "{count}");
    }
}
