use super::*;

use registry::registry::VmEncodeCandidate;
use vm::runtime_model_core::RuntimeModelCore;

const BATCH_VERSION: u16 = 1;
const RECORD_BYTES: usize = 72;
const PAYLOAD_BYTES: usize = 64;
const STATUS_OK: u32 = 0;
const STATUS_FAILURE: u32 = 1;
const STATUS_RUNTIME_ERROR: u32 = 3;
const INVALID_MAGIC: &[u8] = b"OPC001: invalid package magic";
const COMPACT_MALFORMED: &[u8] = b"OTR901: compact table malformed";
const UNRESOLVED_CPU: &[u8] = b"OTR004: unresolved package cpu id";

#[derive(Clone)]
enum CompactMemoCommand {
    Load(Vec<u8>),
    Select(Vec<u8>),
    Lookup { request: Vec<u8>, mode: Vec<u8> },
}

fn fixed_program(byte: u8) -> Vec<u8> {
    vec![0x01, byte, 0xff]
}

fn package_has_chunk(bytes: &[u8], tag: &[u8; 4]) -> bool {
    let Some(count_bytes) = bytes.get(8..10) else {
        return false;
    };
    let count = u16::from_le_bytes(count_bytes.try_into().unwrap()) as usize;
    (0..count).any(|index| {
        let start = 12 + index * 12;
        bytes.get(start..start + 4) == Some(tag.as_slice())
    })
}

fn malformed_compact_package(valid: &[u8]) -> Vec<u8> {
    let mut malformed = valid.to_vec();
    let range = compact_chunk_range(&malformed);
    assert!(range.len() >= 8, "CTBL must contain a row");
    // Corrupt the final program index, after native directory allocation.
    malformed[range.end - 2..range.end].copy_from_slice(&u16::MAX.to_le_bytes());
    assert!(
        package::decode_hierarchy_chunks(&malformed).is_err(),
        "Rust package decoder must reject the malformed CTBL program index"
    );
    assert!(
        RuntimeModelCore::from_package_bytes(&malformed).is_err(),
        "Rust runtime model must reject the malformed CTBL"
    );
    malformed
}

fn compact_memo_package(nop_68020_byte: u8, perturb_layout: bool) -> Vec<u8> {
    let mut chunks = package::decode_hierarchy_chunks(&tkpkg_smoke_package_bytes())
        .expect("decode focused tkpkg package fixture");
    chunks.tables = vec![
        package::VmProgramDescriptor {
            owner: ScopedOwner::Family("motorola68000".into()),
            mnemonic: "other".into(),
            mode_key: "implied".into(),
            program: fixed_program(0x10),
        },
        package::VmProgramDescriptor {
            owner: ScopedOwner::Family("motorola68000".into()),
            mnemonic: "shape".into(),
            mode_key: "implied".into(),
            program: fixed_program(0x31),
        },
        package::VmProgramDescriptor {
            owner: ScopedOwner::Family("motorola68000".into()),
            mnemonic: "shape".into(),
            mode_key: "explicit".into(),
            program: fixed_program(0x32),
        },
        package::VmProgramDescriptor {
            owner: ScopedOwner::Family("motorola68000".into()),
            mnemonic: "m".repeat(33),
            mode_key: "implied".into(),
            program: fixed_program(0x33),
        },
        package::VmProgramDescriptor {
            owner: ScopedOwner::Family("motorola68000".into()),
            mnemonic: "modewide".into(),
            mode_key: "q".repeat(65),
            program: fixed_program(0x41),
        },
        package::VmProgramDescriptor {
            owner: ScopedOwner::Family("motorola68000".into()),
            mnemonic: "nop".into(),
            mode_key: "implied".into(),
            program: fixed_program(0x08),
        },
        package::VmProgramDescriptor {
            owner: ScopedOwner::Cpu("m68000".into()),
            mnemonic: "nop".into(),
            mode_key: "implied".into(),
            program: fixed_program(0x20),
        },
        package::VmProgramDescriptor {
            owner: ScopedOwner::Cpu("m68020".into()),
            mnemonic: "nop".into(),
            mode_key: "implied".into(),
            program: fixed_program(nop_68020_byte),
        },
    ];
    if perturb_layout {
        chunks.tables.push(package::VmProgramDescriptor {
            owner: ScopedOwner::Family("motorola68000".into()),
            mnemonic: "aaa_compact_memo_layout".into(),
            mode_key: "implied".into(),
            program: fixed_program(0x05),
        });
    }
    for index in 0..512 {
        chunks.tables.push(package::VmProgramDescriptor {
            owner: ScopedOwner::Family("motorola68000".into()),
            mnemonic: format!("compact_memo_filler_{index:04}"),
            mode_key: "implied".into(),
            program: fixed_program(0x70),
        });
    }
    let bytes = package::encode_hierarchy_chunks_from_chunks(&chunks)
        .expect("encode compact-memo package fixture");
    assert!(
        bytes.len() <= 393_216,
        "fixture must fit native package storage"
    );
    assert!(
        package_has_chunk(&bytes, b"CTBL"),
        "fixture must exercise CTBL"
    );
    assert!(
        !package_has_chunk(&bytes, b"TABL"),
        "fixture must not fall back to TABL"
    );

    let model = RuntimeModelCore::from_package_bytes(&bytes).expect("load compact-memo fixture");
    for (alias, canonical) in [("68000", "m68000"), ("68020", "m68020")] {
        let resolved = model
            .resolve_pipeline(alias, Some("motorola68k"))
            .unwrap_or_else(|error| panic!("fixture alias {alias} must resolve: {error}"));
        assert_eq!(resolved.cpu_id, canonical);
    }
    bytes
}

fn compact_chunk_range(bytes: &[u8]) -> std::ops::Range<usize> {
    let count = u16::from_le_bytes([bytes[8], bytes[9]]) as usize;
    for index in 0..count {
        let start = 12 + index * 12;
        if bytes.get(start..start + 4) == Some(b"CTBL") {
            let offset =
                u32::from_le_bytes(bytes[start + 4..start + 8].try_into().unwrap()) as usize;
            let length =
                u32::from_le_bytes(bytes[start + 8..start + 12].try_into().unwrap()) as usize;
            return offset..offset + length;
        }
    }
    panic!("missing CTBL chunk")
}

fn unique_program_offset(bytes: &[u8], program: &[u8]) -> usize {
    let range = compact_chunk_range(bytes);
    let offsets = bytes[range.clone()]
        .windows(program.len())
        .enumerate()
        .filter_map(|(offset, candidate)| (candidate == program).then_some(range.start + offset))
        .collect::<Vec<_>>();
    assert_eq!(offsets.len(), 1, "program must occur once within CTBL");
    offsets[0]
}

fn select_request(cpu: &str) -> Vec<u8> {
    let mut request = cpu.as_bytes().to_vec();
    request.push(0);
    request.extend_from_slice(b"motorola68k");
    request
}

fn lookup_request(mnemonic: &str, shaped: bool) -> Vec<u8> {
    assert!(mnemonic.len() <= u8::MAX as usize);
    let mut request = vec![1, 0, 0, 0];
    request.extend_from_slice(if shaped { &[1, 0, 0, 0] } else { &[0, 0, 0, 0] });
    request.push(mnemonic.len() as u8);
    request.extend_from_slice(mnemonic.as_bytes());
    request
}

fn push_record(output: &mut Vec<u8>, status: u32, payload: &[u8]) {
    assert!(payload.len() <= PAYLOAD_BYTES);
    output.extend_from_slice(&status.to_be_bytes());
    output.extend_from_slice(&(payload.len() as u16).to_be_bytes());
    output.extend_from_slice(&0u16.to_be_bytes());
    output.extend_from_slice(payload);
    output.resize(output.len() + PAYLOAD_BYTES - payload.len(), 0);
}

fn resolved_table_program(
    package_bytes: &[u8],
    cpu: &str,
    mnemonic: &str,
    mode: &str,
) -> Option<Vec<u8>> {
    let model = RuntimeModelCore::from_package_bytes(package_bytes)
        .expect("live Rust runtime model package decode");
    let resolved = model
        .resolve_pipeline(cpu, Some("motorola68k"))
        .expect("live Rust pipeline resolution");
    let emitted = model
        .encode_candidates(
            &resolved,
            mnemonic,
            &[VmEncodeCandidate {
                mode_key: mode.to_string(),
                operand_bytes: vec![],
            }],
        )
        .expect("live Rust candidate execution")?;
    assert_eq!(emitted.len(), 1, "fixture programs must emit one byte");

    let chunks =
        package::decode_hierarchy_chunks(package_bytes).expect("live Rust compact table decode");
    let expected_program = fixed_program(emitted[0]);
    let matches = chunks
        .tables
        .iter()
        .filter(|entry| {
            entry.mnemonic.eq_ignore_ascii_case(mnemonic)
                && entry.mode_key.eq_ignore_ascii_case(mode)
                && entry.program == expected_program
        })
        .collect::<Vec<_>>();
    assert_eq!(
        matches.len(),
        1,
        "emitted byte must identify one decoded CTBL row for {cpu}/{mnemonic}/{mode}"
    );
    Some(matches[0].program.clone())
}

fn compact_memo_batch() -> (Vec<u8>, Vec<u8>) {
    let package_a = compact_memo_package(0x21, false);
    let package_b = compact_memo_package(0x51, true);
    let malformed_compact = malformed_compact_package(&package_a);
    let target_a_offset = unique_program_offset(&package_a, &fixed_program(0x20));
    let target_b_offset = unique_program_offset(&package_b, &fixed_program(0x20));
    assert_ne!(
        target_a_offset, target_b_offset,
        "last-cached m68000 NOP program must move on reload"
    );
    let package_b_compact_range = compact_chunk_range(&package_b);
    assert!(
        package_b_compact_range.contains(&target_a_offset)
            && package_b_compact_range.contains(&(target_a_offset + 2)),
        "old cached offset must remain in bounds after reload"
    );
    assert_ne!(
        package_b.get(target_a_offset..target_a_offset + 3),
        Some(fixed_program(0x51).as_slice()),
        "package B at package A's cached offset must not mimic the new target"
    );
    let invalid_package = b"NOPE".to_vec();
    let long_mnemonic = "m".repeat(33);
    let long_mode = "q".repeat(65);
    let commands = vec![
        CompactMemoCommand::Load(package_a.clone()),
        CompactMemoCommand::Select(select_request("68020")),
        CompactMemoCommand::Lookup {
            request: vec![1, 0, 0, 0, 0, 0, 0, 0],
            mode: vec![],
        },
        CompactMemoCommand::Lookup {
            request: lookup_request("absent", false),
            mode: vec![],
        },
        CompactMemoCommand::Lookup {
            request: lookup_request("nop", false),
            mode: vec![],
        },
        CompactMemoCommand::Lookup {
            request: lookup_request("nop", false),
            mode: vec![],
        },
        CompactMemoCommand::Lookup {
            request: lookup_request("NoP", false),
            mode: vec![],
        },
        CompactMemoCommand::Lookup {
            request: lookup_request("other", false),
            mode: vec![],
        },
        CompactMemoCommand::Lookup {
            request: lookup_request("shape", false),
            mode: vec![],
        },
        CompactMemoCommand::Lookup {
            request: lookup_request("shape", true),
            mode: b"explicit".to_vec(),
        },
        CompactMemoCommand::Lookup {
            request: lookup_request("shape", false),
            mode: vec![],
        },
        CompactMemoCommand::Lookup {
            request: lookup_request(&long_mnemonic, false),
            mode: vec![],
        },
        CompactMemoCommand::Lookup {
            request: lookup_request("modewide", true),
            mode: long_mode.as_bytes().to_vec(),
        },
        CompactMemoCommand::Lookup {
            request: lookup_request("nop", false),
            mode: vec![],
        },
        CompactMemoCommand::Select(select_request("68000")),
        CompactMemoCommand::Lookup {
            request: lookup_request("nop", false),
            mode: vec![],
        },
        CompactMemoCommand::Select(select_request("not-a-cpu")),
        CompactMemoCommand::Lookup {
            request: lookup_request("nop", false),
            mode: vec![],
        },
        CompactMemoCommand::Load(package_b.clone()),
        CompactMemoCommand::Lookup {
            request: lookup_request("nop", false),
            mode: vec![],
        },
        CompactMemoCommand::Select(select_request("68020")),
        CompactMemoCommand::Lookup {
            request: lookup_request("nop", false),
            mode: vec![],
        },
        CompactMemoCommand::Load(invalid_package),
        CompactMemoCommand::Lookup {
            request: lookup_request("nop", false),
            mode: vec![],
        },
        CompactMemoCommand::Load(malformed_compact),
        CompactMemoCommand::Lookup {
            request: lookup_request("nop", false),
            mode: vec![],
        },
        CompactMemoCommand::Load(package_a.clone()),
        CompactMemoCommand::Select(select_request("68020")),
        CompactMemoCommand::Lookup {
            request: lookup_request("nop", false),
            mode: vec![],
        },
    ];
    assert!(commands.len() <= 64);

    let mut input = Vec::new();
    let mut expected = Vec::new();
    input.extend_from_slice(&BATCH_VERSION.to_be_bytes());
    input.extend_from_slice(&(commands.len() as u16).to_be_bytes());
    let mut active_package: Option<&[u8]> = None;
    let mut active_cpu: Option<String> = None;
    for command in &commands {
        match command {
            CompactMemoCommand::Load(package) => {
                input.push(1);
                input.extend_from_slice(&(package.len() as u32).to_be_bytes());
                input.extend_from_slice(package);
                let valid = RuntimeModelCore::from_package_bytes(package).is_ok();
                push_record(
                    &mut expected,
                    if valid { STATUS_OK } else { STATUS_FAILURE },
                    if valid {
                        &[]
                    } else if package_has_chunk(package, b"CTBL") {
                        COMPACT_MALFORMED
                    } else {
                        INVALID_MAGIC
                    },
                );
                active_package = valid.then_some(package.as_slice());
                active_cpu = None;
            }
            CompactMemoCommand::Select(request) => {
                input.push(2);
                input.extend_from_slice(&(request.len() as u16).to_be_bytes());
                input.extend_from_slice(request);
                let cpu_len = request.iter().position(|byte| *byte == 0).unwrap();
                let cpu = std::str::from_utf8(&request[..cpu_len]).unwrap();
                let resolved = active_package.and_then(|package| {
                    RuntimeModelCore::from_package_bytes(package)
                        .ok()?
                        .resolve_pipeline(cpu, Some("motorola68k"))
                        .ok()
                });
                push_record(
                    &mut expected,
                    if resolved.is_some() {
                        STATUS_OK
                    } else {
                        STATUS_RUNTIME_ERROR
                    },
                    if resolved.is_some() {
                        &[]
                    } else {
                        UNRESOLVED_CPU
                    },
                );
                if let Some(resolved) = resolved {
                    active_cpu = Some(resolved.cpu_id);
                }
            }
            CompactMemoCommand::Lookup { request, mode } => {
                input.push(3);
                input.extend_from_slice(&(request.len() as u16).to_be_bytes());
                input.extend_from_slice(&(mode.len() as u16).to_be_bytes());
                input.extend_from_slice(request);
                input.extend_from_slice(mode);
                let payload = match (active_package, active_cpu.as_deref()) {
                    (Some(package), Some(cpu)) if request.len() >= 9 => {
                        let shaped = request[4..8].iter().any(|byte| *byte != 0);
                        let mnemonic_len = request[8] as usize;
                        let mnemonic = request
                            .get(9..9 + mnemonic_len)
                            .and_then(|bytes| std::str::from_utf8(bytes).ok());
                        mnemonic
                            .and_then(|mnemonic| {
                                let effective_mode = if shaped {
                                    std::str::from_utf8(mode).ok()?
                                } else {
                                    "implied"
                                };
                                resolved_table_program(package, cpu, mnemonic, effective_mode)
                            })
                            .unwrap_or_default()
                    }
                    _ => Vec::new(),
                };
                push_record(&mut expected, STATUS_OK, &payload);
            }
        }
    }
    assert_eq!(expected.len(), commands.len() * RECORD_BYTES);
    assert!(input.len() <= 2 * 393_216 + 65_536);
    (input, expected)
}

struct CompactMemoOracleDir(PathBuf);

impl Drop for CompactMemoOracleDir {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
    }
}

fn compact_memo_live_rust_cli_oracle(source: &str) -> Vec<u8> {
    let case_dir = create_temp_dir("native-compact-memo-cli-oracle");
    let _case_dir_guard = CompactMemoOracleDir(case_dir.clone());
    let input_path = case_dir.join("input.asm");
    let bin_path = case_dir.join("oracle.bin");
    fs::write(&input_path, source).expect("write compact-memo Rust oracle source");
    let cli = Cli::parse_from([
        "opForge",
        input_path.to_string_lossy().as_ref(),
        "--bin",
        bin_path.to_string_lossy().as_ref(),
        "--cpu",
        "m6502",
    ]);
    run_with_cli_with_context(&cli).expect("run compact-memo live Rust CLI oracle");
    fs::read(&bin_path).expect("read compact-memo live Rust CLI oracle")
}

#[test]
fn native_compact_memo_live_rust_oracle() {
    // Level A/B: the package codec and RuntimeModelCore pipeline selector and
    // executor own each successful returned program. This also locks the
    // dynamic batch request and fixed-record response ABI.
    let (input, expected) = compact_memo_batch();
    assert!(input.len() > 4);
    assert_eq!(expected.len() % RECORD_BYTES, 0);
}

#[test]
fn native_compact_memo_mixed_cli_live_rust_oracle() {
    // Level A plus optional Level D: exercise a real statement mix whose NOPs
    // cross repeated prepared-table lookups and intervening mnemonics. The Rust CLI owns
    // the exact artifact supplied to the existing fail-closed native runner.
    let source = "    .org $0800\n    nop\n    lda #$12\n    nop\n    bne done\n    nop\ndone\n    nop\n    rts\n";
    let rust_oracle = compact_memo_live_rust_cli_oracle(source);
    assert_eq!(
        rust_oracle,
        vec![0xea, 0xa9, 0x12, 0xea, 0xd0, 0x01, 0xea, 0xea, 0x60]
    );
    let package_bytes = build_hierarchy_package_from_registry(&default_registry())
        .expect("build native compact-memo CLI package");
    let case = crate::fs_uae_smoke::OpforgeNativeCliMosFixtureCase {
        name: "native-compact-memo-mixed-cli",
        cpu_id: "m6502",
        source: source.as_bytes(),
        package_bytes: &package_bytes,
        proof: crate::fs_uae_smoke::OpforgeNativeCliMosProof::ExactRustBytes(&rust_oracle),
    };
    match crate::fs_uae_smoke::run_opforge_native_cli_mos_fixture_outputs_from_env(
        &workspace_root(),
        &[case],
    )
    .expect("native compact-memo mixed CLI proof")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(run.success && run.protocol_completed && run.exit_code == Some(0));
            assert_eq!(run.verified_output.as_deref(), Some(rust_oracle.as_slice()));
        }
    }
}

#[test]
fn native_compact_memo_fs_uae() {
    // Level D: one immutable dynamic batch, fresh challenged guest, explicit
    // zero exit, and byte-exact comparison with the in-memory Rust oracle.
    // This proves prepared-table load, invalidation, malformed-load recovery,
    // and lookup behavior at the native request boundary.
    let (input, expected) = compact_memo_batch();
    match crate::fs_uae_smoke::run_compact_memo_harness_from_env(
        &workspace_root(),
        &input,
        &expected,
    )
    .expect("native compact memo lifecycle proof")
    {
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Skipped(reason) => eprintln!("SKIP: {reason}"),
        crate::fs_uae_smoke::FsUaeSmokeOutcome::Completed { runs } => {
            assert_eq!(runs.len(), 1);
            let run = &runs[0];
            assert!(
                run.success && run.protocol_completed && run.exit_code == Some(0),
                "compact memo harness failed: {}\n{}",
                run.stdout,
                run.stderr
            );
        }
    }
}
