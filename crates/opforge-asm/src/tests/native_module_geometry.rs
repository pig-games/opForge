//! Actual producer geometry: moved code/data/BSS and fresh alignment replay.
use super::native_iterable_loops::{oracle, scratch};
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
use vm::runtime_model_core::RuntimeModelCore;

const CODE_BASE: usize = 0x1001;
const DATA_BASE: usize = 0x2003;
const BSS_BASE: usize = 0x3001;
const TABLE_BASE: usize = 0x4000;
const SOURCE: &str = r#".module geometry_fixture
.cpu 68020
.section code,kind=code
 .word $1111
outer .block
beforeZero
 .align 2
afterZero
nested .block
 .long beforeZero,afterZero,beforePad,afterPad,store,scratch
 bsr.w tail
 lea store.l,a0
 .byte $a1
.bend
beforePad
 .align 8
afterPad
 rts
.bend
tail .block
 .word $2222
.bend
.endsection
.section data,kind=data
 .byte $33
store .block
 .long outer.beforeZero,outer.afterZero,outer.beforePad,outer.afterPad,scratch
 .align 4
 .byte $44,$55,$66
.bend
.endsection
.section bss,kind=bss
 .res byte,1
scratch .block
before
 .res byte,2
 .align 8
after
 .res byte,7
.bend
.endsection
.output "geometry.hunk",format=hunk,sections=code,data,bss
.endmodule
"#;

struct GeometryCase {
    name: &'static str,
    source: &'static str,
    // Owner, section, kind, alignment, allocation extent.
    fragments: &'static [[u32; 5]],
}

const CASES: &[GeometryCase] = &[
    GeometryCase {
        name: "code-only",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\nentry .block\n .word $1111\n rts\n.bend\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code\n.endmodule\n",
        fragments: &[[1, 0, 1, 1, 4]],
    },
    GeometryCase {
        name: "data-bss",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\nentry .block\n .word $1111\n rts\n.bend\n.endsection\n.section data,kind=data\nstore .block\n .byte $33\n.bend\n.endsection\n.section bss,kind=bss\nscratch .block\n .res byte,7\n.bend\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n",
        fragments: &[[1, 0, 1, 1, 4], [2, 1, 2, 1, 1], [3, 2, 3, 1, 7]],
    },
    GeometryCase {
        name: "absolute",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\nentry .block\n .long store,scratch\n rts\n.bend\n.endsection\n.section data,kind=data\nstore .block\n .byte $33\n.bend\n.endsection\n.section bss,kind=bss\nscratch .block\n .res byte,7\n.bend\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n",
        fragments: &[[1, 0, 1, 1, 10], [2, 1, 2, 1, 1], [3, 2, 3, 1, 7]],
    },
    GeometryCase {
        name: "alignment",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\n .word $1111\nouter .block\n .align 2\n .byte $a1\n .align 8\n .word $2222\n.bend\n.endsection\n.section data,kind=data\n .byte $33\n.endsection\n.section bss,kind=bss\n .res byte,1\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n",
        fragments: &[[0, 0, 1, 1, 2], [1, 0, 1, 1, 0], [1, 0, 1, 2, 1], [1, 0, 1, 8, 2], [0, 1, 2, 1, 1], [0, 2, 3, 1, 1]],
    },
    GeometryCase {
        name: "nested",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\nouter .block\n .word $1111\nnested .block\n .word $2222\n.bend\n rts\n.bend\n.endsection\n.section data,kind=data\n .byte $33\n.endsection\n.section bss,kind=bss\n .res byte,1\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n",
        fragments: &[[1, 0, 1, 1, 6], [0, 1, 2, 1, 1], [0, 2, 3, 1, 1]],
    },
    GeometryCase {
        name: "relative",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\nfirst .block\n bsr.w second\n rts\n.bend\nsecond .block\n rts\n.bend\n.endsection\n.section data,kind=data\n .byte $33\n.endsection\n.section bss,kind=bss\n .res byte,1\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n",
        fragments: &[[1, 0, 1, 1, 6], [2, 0, 1, 1, 2], [0, 1, 2, 1, 1], [0, 2, 3, 1, 1]],
    },
    GeometryCase {
        name: "instruction-absolute",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\nentry .block\n lea store.l,a0\n rts\n.bend\n.endsection\n.section data,kind=data\nstore .block\n .byte $33\n.bend\n.endsection\n.section bss,kind=bss\n .res byte,1\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n",
        fragments: &[[1, 0, 1, 1, 8], [2, 1, 2, 1, 1], [0, 2, 3, 1, 1]],
    },
    GeometryCase {
        name: "label-alignment",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\n .word $1111\nentry .block\nbeforeZero\n .align 2\nafterZero\n .long beforeZero,afterZero\nbeforePad\n .align 8\nafterPad\n .long beforePad,afterPad\n rts\n.bend\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code\n.endmodule\n",
        fragments: &[[0, 0, 1, 1, 2], [1, 0, 1, 1, 0], [1, 0, 1, 2, 8], [1, 0, 1, 8, 10]],
    },
    GeometryCase {
        name: "qualified-label-alignment",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\n .word $aaaa\nouter .block\nbeforeZero\n .align 2\nafterZero\n .word $1111\n.bend\nstore .block\n .long outer.beforeZero+3,outer.afterZero+5\n.bend\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code\n.endmodule\n",
        fragments: &[[0, 0, 1, 1, 2], [1, 0, 1, 1, 0], [1, 0, 1, 2, 2], [2, 0, 1, 1, 8]],
    },
    GeometryCase {
        name: "bss-alignment",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\n .word $1111\n.endsection\n.section data,kind=data\n .byte $33\n.endsection\n.section bss,kind=bss\n .res byte,1\nscratch .block\n .res byte,2\n .align 8\n .res byte,7\n.bend\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n",
        fragments: &[[0, 0, 1, 1, 2], [0, 1, 2, 1, 1], [0, 2, 3, 1, 1], [1, 2, 3, 1, 2], [1, 2, 3, 8, 7]],
    },
    GeometryCase {
        name: "explicit-cross-align",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\nentry .block\n bsr.w target\n .align 8\ntarget\n rts\n.bend\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code\n.endmodule\n",
        fragments: &[[1, 0, 1, 1, 4], [1, 0, 1, 8, 2]],
    },
    GeometryCase {
        name: "auto-same-fragment",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\nentry .block\n bsr target\n .word $1111\ntarget\n rts\n.bend\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code\n.endmodule\n",
        fragments: &[[1, 0, 1, 1, 8]],
    },
    GeometryCase {
        name: "counter-same-fragment",
        source: ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\nentry .block\nbackward\n .word $1111\n dbra d0,forward\n dbf d7,backward\n dbra d3,backward\nforward\n dbf d5,forward\n rts\n.bend\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code\n.endmodule\n",
        fragments: &[[1, 0, 1, 1, 20]],
    },
    GeometryCase {
        name: "full",
        source: SOURCE,
        fragments: &[[0, 0, 1, 1, 2], [1, 0, 1, 1, 0], [1, 0, 1, 2, 35], [1, 0, 1, 8, 2], [3, 0, 1, 1, 2], [0, 1, 2, 1, 1], [4, 1, 2, 1, 20], [4, 1, 2, 4, 3], [0, 2, 3, 1, 1], [5, 2, 3, 1, 2], [5, 2, 3, 8, 7]],
    },
];

fn selected_cases() -> Vec<&'static GeometryCase> {
    let selected = std::env::var("OPFORGE_MODULE_GEOMETRY_CASES").unwrap_or_else(|_| "full".into());
    let names = selected.split(',').collect::<Vec<_>>();
    let cases = CASES
        .iter()
        .filter(|case| names.contains(&case.name))
        .collect::<Vec<_>>();
    assert_eq!(cases.len(), names.len(), "unknown geometry case");
    cases
}

struct Fixture {
    oracle: Vec<u8>,
    package: Vec<u8>,
}

fn prepare(dir: &Path, case: &GeometryCase) -> Fixture {
    let automatic_cross_alignment =
        include_str!("../../fixtures/binary-source/module-auto-cross-alignment.asm");
    oracle(dir, automatic_cross_alignment)
        .expect("ordinary assembler supports automatic cross-alignment branch");
    fs::write(
        dir.join("module_branch_width_auto.asm"),
        automatic_cross_alignment,
    )
    .unwrap();
    // Counter branches have no general cross-fragment relocation recipe.
    // Ordinary Rust accepts both; the private producer must reject atomically.
    for (name, source) in [
        ("module_counter_cross_align.asm", ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\nentry .block\n dbra d2,target\n .align 8\ntarget\n rts\n.bend\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code\n.endmodule\n"),
        ("module_counter_cross_block.asm", ".module geometry_fixture\n.cpu 68020\n.section code,kind=code\nfirst .block\n dbf d6,second\n rts\n.bend\nsecond .block\n rts\n.bend\n.endsection\n.output \"geometry.hunk\",format=hunk,sections=code\n.endmodule\n"),
    ] {
        oracle(dir, source).expect("ordinary assembler supports counter branch target");
        fs::write(dir.join(name), source).unwrap();
    }
    // Rust assembles the same statements at their new addresses. Raw output
    // additionally carries a live end-label table, allowing exact section slices.
    // Code/data flatten into the raw image; BSS retains its section boundary
    // so .res and .align advance allocation without emitting initialized bytes.
    let mut reference = String::new();
    let mut section = None;
    let mut present = [false; 3];
    for line in case.source.lines() {
        if line.starts_with(".section ") {
            let (name, base) = if line.contains("kind=code") {
                ("code", CODE_BASE)
            } else if line.contains("kind=data") {
                ("data", DATA_BASE)
            } else {
                ("bss", BSS_BASE)
            };
            present[match name {
                "code" => 0,
                "data" => 1,
                _ => 2,
            }] = true;
            section = Some(name);
            if name == "bss" {
                reference.push_str(line);
                reference.push('\n');
            }
            reference.push_str(&format!(" .org ${base:x}\n"));
        } else if line == ".endsection" {
            let name = section.take().unwrap();
            reference.push_str(&format!("__proof_{name}_end\n"));
            if name == "bss" {
                reference.push_str(".endsection\n");
            }
        } else if line.starts_with(".output ") {
            for (index, (name, base)) in
                [("code", CODE_BASE), ("data", DATA_BASE), ("bss", BSS_BASE)]
                    .into_iter()
                    .enumerate()
            {
                if !present[index] {
                    reference.push_str(&format!("__proof_{name}_end=${base:x}\n"));
                }
            }
            reference.push_str(&format!(" .org ${TABLE_BASE:x}\n"));
            reference.push_str(" .long __proof_code_end,__proof_data_end,__proof_bss_end\n");
        } else {
            reference.push_str(line);
            reference.push('\n');
        }
    }
    let assembled = oracle(dir, &reference).expect("live placed geometry oracle");
    let table = &assembled[TABLE_BASE - CODE_BASE..];
    assert_eq!(table.len(), 12);
    let ends = table
        .chunks_exact(4)
        .map(|word| u32::from_be_bytes(word.try_into().unwrap()) as usize)
        .collect::<Vec<_>>();
    let code_bytes = ends[0] - CODE_BASE;
    let data_bytes = ends[1] - DATA_BASE;
    if case.name == "full" {
        assert_eq!((code_bytes, data_bytes, ends[2] - BSS_BASE), (43, 24, 14));
    }
    let mut expected = assembled[..code_bytes].to_vec();
    expected.extend_from_slice(&assembled[DATA_BASE - CODE_BASE..ends[1] - CODE_BASE]);
    expected.extend_from_slice(table);
    // The originally zero-byte alignment now inserts one byte, so these two
    // label pointers must differ. The source's nonzero padding also changes.
    if case.name == "full" {
        assert_eq!(&expected[3..11], &[0, 0, 0x10, 3, 0, 0, 0x10, 4]);
    }
    let core = RuntimeModelCore::from_registry(&engine::build_default_asm_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = crate::binary_source_experiment::prepare_package(&core, &resolved).unwrap();
    fs::write(dir.join("module_geometry_source.txt"), case.source).unwrap();
    fs::write(dir.join("module_geometry_package.bin"), &package).unwrap();
    let mut rows = String::new();
    for row in case.fragments {
        rows.push_str(&format!(
            "\t.long {},{},{},{},{}\n",
            row[0], row[1], row[2], row[3], row[4]
        ));
    }
    fs::write(dir.join("module_geometry_expectations.i"), rows).unwrap();
    let expected_relocations = if case.name == "counter-same-fragment" {
        0
    } else {
        -1
    };
    let last = case.fragments.last().unwrap();
    fs::write(
        dir.join("module_geometry_fixture.i"),
        format!(
            "CODE_BASE=${CODE_BASE:x}\nDATA_BASE=${DATA_BASE:x}\nBSS_BASE=${BSS_BASE:x}\nCODE_BYTES={code_bytes}\nDATA_BYTES={data_bytes}\nRESULT_BYTES={}\n",
            expected.len()
        ) + &format!("FRAGMENT_COUNT={}\nREGISTRY_FRAGMENT={}\nREGISTRY_KIND={}\nREGISTRY_EXTENT={}\nEXPECTED_RELOCATIONS={expected_relocations}\n", case.fragments.len(), case.fragments.len(), last[2], last[4]),
    )
    .unwrap();
    Fixture {
        oracle: expected,
        package,
    }
}

fn probe(root: &Path, dir: &Path) -> Vec<u8> {
    let path = dir.join("module_geometry_probe.asm");
    fs::copy(
        root.join(
            "native/motorola68000/amigaos/test-harnesses/experimental/module_geometry_probe.asm",
        ),
        &path,
    )
    .unwrap();
    let native = root.join("native/motorola68000/amigaos");
    let cli = Cli::parse_from([
        "opforge".to_owned(),
        path.to_string_lossy().into_owned(),
        "-M".to_owned(),
        native.to_string_lossy().into_owned(),
        "-I".to_owned(),
        native.join("debug").to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.to_path_buf());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => {
            let diagnostics = error
                .diagnostics()
                .iter()
                .take(12)
                .map(|d| {
                    format!(
                        "{}:{}: {}",
                        d.file.as_deref().unwrap_or("geometry"),
                        d.line,
                        d.error.message()
                    )
                })
                .collect::<Vec<_>>()
                .join("\n");
            panic!("geometry harness: {}\n{diagnostics}", error.summary());
        }
        _ => panic!("geometry harness failed"),
    });
    fs::read(dir.join("module-geometry-probe.hunk")).unwrap()
}

#[test]
fn native_module_geometry_host_assembles() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    for case in selected_cases() {
        let dir = scratch();
        prepare(&dir.0, case);
        assert!(!probe(&root, &dir.0).is_empty());
    }
}

#[test]
#[ignore = "requires FS-UAE; actual producer and moved code/data/BSS alignment replay"]
fn native_module_geometry_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
    let mut failures = Vec::new();
    for selected in selected_cases() {
        let dir = scratch();
        let fixture = prepare(&dir.0, selected);
        let image = probe(&root, &dir.0);
        let case = OpforgeNativeCliParityCase {
            name: "native-module-fragment-geometry",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(selected.source.as_bytes()),
            command_template: Some(""),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &[],
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: "Work/output.bin",
                rust_oracle: &fixture.oracle,
            },
        };
        let outcome = match run_prebuilt_compact_cli_case_from_env(&root, &case, &image) {
            Ok(outcome) => outcome,
            Err(error) => {
                eprintln!("MODULE_FRAGMENT_GEOMETRY_FAILED {}: {error}", selected.name);
                failures.push(format!("{}: {error}", selected.name));
                continue;
            }
        };
        let FsUaeSmokeOutcome::Completed { runs } = outcome else {
            failures.push(format!("{}: real native execution required", selected.name));
            continue;
        };
        assert_eq!(runs.len(), 1);
        let run = &runs[0];
        assert!(run.protocol_completed && run.success);
        assert_eq!(run.exit_code, Some(0));
        assert_eq!(
            run.verified_output.as_deref(),
            Some(fixture.oracle.as_slice())
        );
        eprintln!(
            "MODULE_FRAGMENT_GEOMETRY {}",
            serde_json::json!({
                "case": selected.name,
                "source_digest": opforge_self_host_package_digest(selected.source.as_bytes()),
                "package_digest": opforge_self_host_package_digest(&fixture.package),
                "image_digest": opforge_self_host_package_digest(&image),
                "fragments": selected.fragments.len(),
                "sections": ["code", "data", "bss"],
                "source_and_original_image_retired": true,
                "fresh_protocol": run.protocol_completed,
                "exact_rust_bytes": fixture.oracle.len(),
                "scope": "actual producer component; no CLI or self-host qualification",
            })
        );
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
