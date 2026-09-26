//! Packed arithmetic uses canonical precedence and the shared checked ExprVM.
use super::*;

const EXPRESSIONS: &[(&str, i32)] = &[
    ("PATH_BYTES/4-1", 63),
    ("17/7", 2),
    ("17%7", 3),
    ("%101%10", 5),
    ("%101 % %10", 1),
    ("(-magnitude)/denominator", -2),
    ("(-magnitude)%denominator", -3),
    ("17/-7", -2),
    ("17%-7", 3),
    ("-17/-7", 2),
    ("-17%-7", -3),
    ("100/5/2", 10),
    ("100%7*3+1", 7),
    ("2+3*4/2", 8),
    ("2**3**2", 512),
    // Canonical unary precedence is tighter than power.
    ("-2**2", 4),
    ("2**(1+2)", 8),
    ("2**0", 1),
    ("0**0", 1),
    ("forward", 33),
    ("denominator**2", 49),
    ("(-2147483647-1)/3", -715827882),
    ("(-2147483647-1)%3", -2),
    ("2147483647/65537", 32767),
    ("2147483647%65537", 32768),
];

fn source(cpu: &str) -> String {
    let instruction = if cpu == "m6502" {
        "lda #PATH_BYTES/4-1"
    } else {
        "move.w #PATH_BYTES/4-1,d0"
    };
    let mut source = format!(".module app\n.cpu {cpu}\nPATH_BYTES = 256\ndenominator = 7\nmagnitude = 17\nforward = later/3\n.org $1000\nstart\n {instruction}\n");
    for (expression, _) in EXPRESSIONS {
        source.push_str(&format!(".long {expression}\n"));
    }
    source.push_str(&format!(".long {}\n", nested_power(6)));
    source.push_str(
        ".long (finish-start)/4,(finish-start)%4\nfinish\nlater = 99\n.endmodule\n.end\n",
    );
    source
}

fn oracle(source: &str) -> Vec<u8> {
    graph::oracle_with_roots(&[("main.asm", source)], &[]).unwrap()
}

fn expected(cpu: &str) -> Vec<u8> {
    let mut bytes = if cpu == "m6502" {
        vec![0xa9, 63]
    } else {
        vec![0x30, 0x3c, 0, 63]
    };
    let distance = (bytes.len() + 4 * (EXPRESSIONS.len() + 3)) as i32;
    for value in EXPRESSIONS
        .iter()
        .map(|(_, value)| *value)
        .chain([1, distance / 4, distance % 4])
    {
        bytes.extend(if cpu == "m6502" {
            value.to_le_bytes()
        } else {
            value.to_be_bytes()
        });
    }
    bytes
}

#[test]
fn compact_arithmetic_rust_oracles() {
    for cpu in ["m6502", "m68020"] {
        assert_eq!(oracle(&source(cpu)), expected(cpu));
    }
}

fn memory(run: &crate::fs_uae_smoke::FsUaeSmokeRun) {
    let record = &run.captured_artifacts[&PathBuf::from("Work/memory.bin")];
    assert_eq!(record.len(), 1756);
    let words = record
        .chunks_exact(4)
        .map(|word| u32::from_be_bytes(word.try_into().unwrap()))
        .collect::<Vec<_>>();
    assert_eq!(words[0], 0x4d454d35);
    assert_eq!(words[1], 0);
    assert_eq!(words[3], words[4]);
    assert_eq!(words[11], 0);
    assert_eq!(words[29], 0);
    eprintln!(
        "COMPACT_ARITHMETIC_MEMORY peak_owned_bytes={} compiled={} evaluated={} program_bytes={}",
        words[2], words[16], words[17], words[18]
    );
}

fn native(cpu: &str) {
    let source = source(cpu);
    let expected = oracle(&source);
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline(cpu, None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    let outcome = crate::fs_uae_smoke::run_compact_cli_from_env(
        &workspace_root(),
        &package,
        source.as_bytes(),
        Some(&expected),
    )
    .expect("fresh packed arithmetic comparison");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required");
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed);
    assert_eq!(runs[0].exit_code, Some(0));
    if std::env::var("OPFORGE_COMPARE_MEMORY").as_deref() == Ok("1") {
        memory(&runs[0]);
    }
    let image = &runs[0].captured_artifacts[&PathBuf::from("Work/build/opforge_compact")];
    eprintln!("COMPACT_ARITHMETIC cpu={cpu} source_bytes={} output_bytes={} seconds={:?} image_bytes={} linked_reserved_bytes={}", source.len(), expected.len(), runs[0].start_to_done_host_seconds, image.len(), hunk::allocation(image).unwrap().total());
}

#[test]
#[ignore = "requires configured FS-UAE; packed arithmetic with little-endian target output"]
fn compact_arithmetic_little_fs_uae() {
    native("m6502");
}

#[test]
#[ignore = "requires configured FS-UAE; packed arithmetic with self-host immediate form"]
fn compact_arithmetic_big_fs_uae() {
    native("m68020");
}

const INVALID: &[(&str, &str)] = &[
    ("division-zero", ".long 7/0"),
    ("remainder-zero", ".long 7%0"),
    ("dynamic-zero", "denominator = 0\n.long 7/denominator"),
    ("negative-power", ".long 2**-1"),
    ("missing-power-operand", ".long 2**"),
];

#[test]
fn compact_arithmetic_rejection_oracles() {
    for (name, body) in INVALID {
        let source = format!(".cpu m68020\n{body}\n.end\n");
        assert!(
            graph::oracle_with_roots(&[("main.asm", &source)], &[]).is_err(),
            "{name}"
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; division/remainder zero and invalid power"]
fn compact_arithmetic_rejection_fs_uae() {
    for (name, body) in INVALID {
        eprintln!("COMPACT_ARITHMETIC_REJECTION case={name}");
        assert_native_rejection(&format!(".cpu m68020\n{body}\n.end\n"), "m68020");
    }
}

// Ten parenthesis levels leave six of the shared sixteen syntax levels for power.
fn nested_power(operators: usize) -> String {
    format!(
        "{}1{}{}",
        "(".repeat(10),
        "**1".repeat(operators),
        ")".repeat(10)
    )
}

#[test]
fn compact_arithmetic_depth_rust_oracle() {
    for operators in [6, 7] {
        assert_eq!(
            oracle(&format!(
                ".cpu m68020\n.long {}\n.end\n",
                nested_power(operators)
            )),
            [0, 0, 0, 1]
        );
    }
}

#[test]
#[ignore = "requires configured FS-UAE; shared syntax-depth bound for recursive power"]
fn compact_arithmetic_depth_bound_fs_uae() {
    assert_native_rejection(
        &format!(".cpu m68020\n.long {}\n.end\n", nested_power(7)),
        "m68020",
    );
}
