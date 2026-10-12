//! One numeric initialized image must drive every flat output format.
use super::native_iterable_loops::{build_image, scratch};
use super::*;
use clap::Parser;

struct ImageCase {
    name: &'static str,
    body: String,
    bin: Vec<u8>,
}

fn cases() -> Vec<ImageCase> {
    let mut cases = vec![
        ImageCase {
            name: "origin-forward",
            body: concat!(".org $1000\n", ".byte 1\n", ".org $1003\n", ".byte 2\n").into(),
            bin: vec![1, 0, 0, 2],
        },
        ImageCase {
            name: "origin-backward",
            body: concat!(".org $1003\n", ".byte 2\n", ".org $1000\n", ".byte 1\n").into(),
            bin: vec![1, 0, 0, 2],
        },
        ImageCase {
            name: "origin-overlay-middle",
            body: concat!(
                ".org $1000\n",
                ".byte 1,2,3,4,5\n",
                ".org $1001\n",
                ".byte 9,8\n"
            )
            .into(),
            bin: vec![1, 9, 8, 4, 5],
        },
        ImageCase {
            name: "origin-overlay-across-ranges",
            body: concat!(
                ".org $1000\n",
                ".byte 1,2\n",
                ".org $1004\n",
                ".byte 5,6\n",
                ".org $1001\n",
                ".byte 9,8,7,6\n"
            )
            .into(),
            bin: vec![1, 9, 8, 7, 6, 6],
        },
        ImageCase {
            name: "origin-leading-trailing-reservations",
            body: concat!(
                ".org $1000\n",
                ".ds 3\n",
                ".byte 1\n",
                ".ds 2\n",
                ".org $2000\n"
            )
            .into(),
            bin: vec![1],
        },
        ImageCase {
            name: "origin-pending-reservation-reset",
            body: concat!(
                ".org $1000\n",
                ".byte 1\n",
                ".ds 10\n",
                ".org $1003\n",
                ".byte 2\n"
            )
            .into(),
            bin: vec![1, 0, 0, 2],
        },
        ImageCase {
            name: "origin-conditional-macro",
            body: concat!(
                "WRITE .macro address,value\n",
                ".org .address\n",
                ".byte .value\n",
                ".endmacro\n",
                ".if 1\n",
                ".WRITE $1003,2\n",
                ".else\n",
                ".WRITE $2000,99\n",
                ".endif\n",
                ".WRITE $1000,1\n"
            )
            .into(),
            bin: vec![1, 0, 0, 2],
        },
        ImageCase {
            name: "origin-two-modules",
            body: concat!(
                ".org $1003\n",
                ".byte 2\n",
                ".endmodule\n",
                ".module other\n",
                ".org $1000\n",
                ".byte 1\n"
            )
            .into(),
            bin: vec![1, 0, 0, 2],
        },
        ImageCase {
            name: "origin-empty-reservation",
            body: concat!(".org $1000\n", ".ds 10\n", ".org $2000\n").into(),
            bin: vec![],
        },
    ];
    let mut body = concat!(".org $1000\n", ".byte 1\n", ".org $3001\n").to_owned();
    // The zero-filled gap exceeds the bounded transport buffer.
    body.push_str(".byte 2\n");
    let mut bin = vec![0; 8194];
    bin[0] = 1;
    bin[8193] = 2;
    cases.push(ImageCase {
        name: "origin-streamed-hole",
        body,
        bin,
    });
    cases
}

struct Prepared {
    name: String,
    source: String,
    command: String,
    oracle: Vec<u8>,
}

fn prepare(dir: &Path) -> Vec<Prepared> {
    let mut prepared = Vec::new();
    for cpu in ["68020", "6502"] {
        for case in cases() {
            let source = format!(
                concat!(".module main\n", ".cpu {cpu}\n", "{}.endmodule\n"),
                case.body,
                cpu = cpu
            );
            for (flag, extension) in [("--bin", "bin"), ("--hex", "hex"), ("--srec", "srec")] {
                let name = format!("{}-{cpu}-{extension}", case.name);
                let base = dir.join(&name);
                fs::create_dir_all(&base).unwrap();
                let input = base.join("entry.asm");
                let output = base.join(format!("out.{extension}"));
                fs::write(&input, &source).unwrap();
                let cli = cli_core::Cli::parse_from([
                    "opForge".to_string(),
                    input.display().to_string(),
                    flag.into(),
                    output.display().to_string(),
                ]);
                cli_core::run_with_cli_with_context(&cli).expect("live origin/output Rust oracle");
                let oracle = fs::read(output).unwrap();
                if extension == "bin" {
                    assert_eq!(oracle, case.bin, "{name}: independent binary image");
                }
                prepared.push(Prepared {
                    name,
                    source: source.clone(),
                    oracle,
                    command: format!("Work:entry.asm {flag} Work:out.{extension}"),
                });
            }
        }
    }
    prepared
}

#[test]
fn native_origin_outputs_live_oracles() {
    let dir = scratch();
    assert_eq!(prepare(&dir.0).len(), 60);
}

#[test]
#[ignore = "fresh compact CLI origin/output breadth; requires configured FS-UAE"]
fn native_origin_outputs_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let mut prepared = prepare(&dir.0);
    if let Ok(selection) = std::env::var("OPFORGE_ORIGIN_CASES") {
        prepared.retain(|case| selection.split(',').any(|part| case.name.contains(part)));
    }
    assert!(!prepared.is_empty(), "origin selector must execute cases");
    let files: Vec<_> = prepared
        .iter()
        .map(|case| {
            vec![OpforgeNativeCliGuestFile {
                relative_path: "entry.asm",
                bytes: case.source.as_bytes(),
            }]
        })
        .collect();
    let paths: Vec<_> = prepared
        .iter()
        .map(|case| format!("Work/out.{}", case.name.rsplit('-').next().unwrap()))
        .collect();
    let native: Vec<_> = prepared
        .iter()
        .zip(&files)
        .zip(&paths)
        .map(|((case, files), path)| OpforgeNativeCliParityCase {
            name: &case.name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(case.source.as_bytes()),
            command_template: Some(&case.command),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: files,
            proof: OpforgeNativeCliProof::ExactArtifact {
                relative_path: path,
                rust_oracle: &case.oracle,
            },
        })
        .collect();
    let results = run_prebuilt_compact_cli_breadth_from_env(&root, &native, &image, 16).unwrap();
    assert_eq!(results.len(), prepared.len());
    let mut failures = Vec::new();
    for (case, result) in prepared.iter().zip(results) {
        if let Some(error) = result.error {
            failures.push(format!("{}: {error}", case.name));
        } else if !result
            .run
            .is_some_and(|run| run.protocol_completed && run.success && run.exit_code == Some(0))
        {
            failures.push(format!(
                "{}: absent fresh zero-exit artifact proof",
                case.name
            ));
        } else {
            eprintln!("{}: exact fresh artifact", case.name);
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
