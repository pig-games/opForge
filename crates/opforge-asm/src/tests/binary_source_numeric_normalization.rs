//! Package-selected TKVM values, with fresh native metadata and assembly proof.
use super::*;
use vm::portable_contract::PortableNormalizedNumber;
use vm::runtime_portable_types::{PortableTokenizeRequest, PortableTokenizerByteStream};

fn batch() -> (Vec<u8>, Vec<u8>) {
    use PortableNormalizedNumber::{Invalid, Overflow, Value};
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let template = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let policy = model.resolve_token_policy("m68020", None).unwrap();
    let cases = [
        ("0", Value(0)),
        ("4294967295", Value(u64::from(u32::MAX))),
        ("4294967296", Value(1u64 << 32)),
        ("18446744073709551615", Value(u64::MAX)),
        ("18446744073709551616", Overflow),
        ("184467440737095516160z", Overflow),
        ("$aBcD", Value(0xabcd)),
        ("0xBB", Value(0xbb)),
        ("%1010", Value(10)),
        ("0b1010", Value(10)),
        ("0o17", Value(15)),
        ("17q", Value(15)),
        ("10b", Value(2)),
        ("2b", Value(2)),
        ("0B8H", Value(184)),
        ("55d", Value(55)),
        ("1_234", Value(1234)),
        ("0_x_FF", Value(255)),
        ("12_h_", Value(18)),
        ("1suffix", Invalid),
        ("12z", Invalid),
        ("0b1b", Invalid),
        ("0b9b", Invalid),
        ("0x1h", Invalid),
    ];
    let mut input = (cases.len() as u32).to_be_bytes().to_vec();
    let mut oracle = Vec::new();
    for (source, expected) in cases {
        let request = PortableTokenizeRequest {
            family_id: "motorola68000",
            cpu_id: "m68020",
            dialect_id: "motorola68k",
            source_line: source,
            source_stream: PortableTokenizerByteStream::from_source_line(source),
            line_num: 1,
            token_policy: policy.clone(),
        };
        let tokens = model.tokenize_with_vm_core(&request, &template).unwrap();
        assert_eq!(tokens.len(), 1, "{source}");
        let PortableTokenKind::Number {
            normalized: Some(actual),
            ..
        } = tokens[0].kind
        else {
            panic!("normalized number required for {source}: {:?}", tokens[0]);
        };
        assert_eq!(actual, expected, "{source}");
        input.extend(template.limits.max_steps_per_line.to_be_bytes());
        input.extend((template.program.len() as u16).to_be_bytes());
        // Harness capture flag affects reporting only, never VM behavior.
        input.extend((0x8000 | source.len() as u16).to_be_bytes());
        input.extend(&template.program);
        input.extend(source.as_bytes());
        if source.len() % 2 != 0 {
            input.push(0);
        }
        oracle.extend(0u32.to_be_bytes());
        let (status, value) = match actual {
            Value(value) => (1u32, value),
            Invalid => (2, 0),
            Overflow => (3, 0),
        };
        oracle.extend(status.to_be_bytes());
        oracle.extend(value.to_be_bytes());
    }
    assert!(input.len() < 8192);
    (input, oracle)
}

#[test]
fn numeric_normalization_live_oracle() {
    batch();
}

#[test]
#[ignore = "requires configured FS-UAE; fresh normalized u64 metadata comparison"]
fn numeric_normalization_fs_uae() {
    let (input, oracle) = batch();
    let result =
        crate::fs_uae_smoke::run_tkvm_branch_harness_from_env(&workspace_root(), &input, &oracle)
            .expect("fresh numeric VM comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native proof required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    eprintln!(
        "NUMERIC_NORMALIZATION cases={} seconds={:?}",
        oracle.len() / 16,
        runs[0].start_to_done_host_seconds
    );
}

const SOURCE: &str = ".cpu 68020\n.long 0,1_234,$BB,0x2A,%1010,0b11,0o17,17q,2b,0B8H,55d\n.end\n";

#[test]
fn numeric_normalization_assembly_oracle() {
    assert_eq!(
        graph::oracle_with_roots(&[("input.asm", SOURCE)], &[])
            .unwrap()
            .len(),
        44
    );
}

#[test]
#[ignore = "requires configured FS-UAE; numeric values through compact assembly"]
fn numeric_normalization_assembly_fs_uae() {
    assert_binary_source(SOURCE.into(), "m68020".into());
}

#[test]
#[ignore = "requires configured FS-UAE; failed normalization preserves prior commit and raw record"]
fn numeric_normalization_capacity_fs_uae() {
    let model = load_opasm_model_from_package_bytes(&tkpkg_smoke_package_bytes());
    let template = model
        .resolve_tokenizer_vm_program("m68020", None)
        .unwrap()
        .unwrap();
    let mut input = 1u32.to_be_bytes().to_vec();
    input.extend(template.limits.max_steps_per_line.to_be_bytes());
    input.extend((template.program.len() as u16).to_be_bytes());
    // Reporting-only capacity probe flag selects a fixed 14-byte scratch buffer.
    input.extend(0x4003u16.to_be_bytes());
    input.extend(&template.program);
    input.extend(b"1 2\0");
    // Runtime status, cursor, committed bytes, first/second status, first u64.
    let oracle: Vec<u8> = [3u32, 2, 11, 1, 0, 0, 1]
        .into_iter()
        .flat_map(u32::to_be_bytes)
        .collect();
    let result =
        crate::fs_uae_smoke::run_tkvm_branch_harness_from_env(&workspace_root(), &input, &oracle)
            .expect("fresh numeric capacity comparison");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native proof required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
}

#[test]
#[ignore = "requires configured FS-UAE; metadata preserves the prior dense spelling budget"]
fn numeric_normalization_dense_fs_uae() {
    // Assignment avoids the existing 251-byte raw dot-call sidecar limit.
    // Its original spelling fits 1024 bytes; spelling plus metadata does not.
    let literal = format!("%{}{}", "0".repeat(512), "1".repeat(32));
    let source = format!(".cpu m68020\nvalue = {literal}\n.long value\n.end\n");
    let oracle = graph::oracle_with_roots(&[("input.asm", &source)], &[]).unwrap();
    assert_eq!(oracle, vec![255; 4]);
    assert_binary_source(source, "m68020".into());
}
