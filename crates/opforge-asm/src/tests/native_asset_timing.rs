//! Identical valid ASCII assets compare old text transport with opaque bytes.
//! Full package bytes failed on the old path and cannot supply a timing baseline.
use super::*;
use crate::fs_uae_smoke::{
    hunk, run_prebuilt_compact_cli_case_from_env, FsUaeSmokeOutcome, OpforgeNativeCliGuestFile,
    OpforgeNativeCliPackageMode, OpforgeNativeCliParityCase, OpforgeNativeCliProof,
};

const SOURCE: &str = ".module asset_timing\n.cpu m68020\n.section code,kind=code\n rts\n.endsection\n.section data,kind=data\nPayload .incbin \"asset.bin\"\n.endsection\n.output \"asset.hunk\",format=hunk,sections=code,data\n.endmodule\n";

fn inputs() -> (Vec<u8>, Vec<u8>) {
    let core =
        vm::runtime_model_core::RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = crate::binary_source_experiment::prepare_package(&core, &resolved).unwrap();
    let alphabet = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    let asset = (0..package.len())
        .map(|index| alphabet[index % alphabet.len()])
        .collect();
    (package, asset)
}

fn oracle(asset: &[u8]) -> Vec<u8> {
    let bytes = super::binary_source_incbin::oracle(
        "m68020",
        &[("source.asm", SOURCE.as_bytes()), ("asset.bin", asset)],
        &[],
        "asset.hunk",
        false,
    )
    .unwrap();
    let segments = hunk::segments(&bytes).unwrap();
    assert_eq!(segments.len(), 2);
    assert_eq!(&segments[1].payload[..asset.len()], asset);
    assert!(segments[1].payload[asset.len()..]
        .iter()
        .all(|byte| *byte == 0));
    bytes
}

fn sha256(path: &Path) -> String {
    let output = std::process::Command::new("shasum")
        .args(["-a", "256"])
        .arg(path)
        .output()
        .expect("SHA-256 fixture digest");
    assert!(output.status.success());
    let digest = String::from_utf8(output.stdout)
        .unwrap()
        .split_whitespace()
        .next()
        .unwrap()
        .to_owned();
    assert_eq!(digest.len(), 64);
    digest
}

#[test]
fn ascii_asset_timing_rust_oracle() {
    let (package, asset) = inputs();
    if let Some(path) = std::env::var_os("OPFORGE_ASSET_TIMING_IMAGE") {
        let image = fs::read(path).unwrap();
        assert!(
            image.windows(package.len()).any(|view| view == package),
            "prebuilt image must contain this exact package"
        );
    }
    assert!(asset.len() > 4096 && asset.iter().all(u8::is_ascii));
    oracle(&asset);
}

#[test]
#[ignore = "requires configured FS-UAE and OPFORGE_ASSET_TIMING_IMAGE; identical ASCII asset timing"]
fn ascii_asset_timing_fs_uae() {
    let root = workspace_root();
    let image_path = PathBuf::from(
        std::env::var_os("OPFORGE_ASSET_TIMING_IMAGE")
            .expect("explicit before/after prebuilt image"),
    );
    let image = fs::read(&image_path).unwrap();
    let (package, asset) = inputs();
    assert!(
        image.windows(package.len()).any(|view| view == package),
        "prebuilt image must contain this exact package"
    );
    let expected = oracle(&asset);
    let dir = create_temp_dir("ascii-asset-timing");
    let digests = [
        ("package.bin", package.as_slice()),
        ("asset.bin", asset.as_slice()),
        ("source.asm", SOURCE.as_bytes()),
        ("oracle.hunk", expected.as_slice()),
    ]
    .map(|(name, bytes)| {
        let path = dir.join(name);
        fs::write(&path, bytes).unwrap();
        sha256(&path)
    });
    fs::remove_dir_all(&dir).unwrap();
    let files = [
        OpforgeNativeCliGuestFile {
            relative_path: "source.asm",
            bytes: SOURCE.as_bytes(),
        },
        OpforgeNativeCliGuestFile {
            relative_path: "asset.bin",
            bytes: &asset,
        },
    ];
    let case = OpforgeNativeCliParityCase {
        name: "identical-ascii-asset-timing",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(SOURCE.as_bytes()),
        command_template: Some("Work:source.asm --hunk Work:asset.hunk"),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/asset.hunk",
            rust_oracle: &expected,
        },
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh identical-input asset proof");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.success && run.protocol_completed && run.exit_code == Some(0));
    eprintln!("ASSET_TIMING cpu=m68020 package_bytes={} package_sha256={} asset_bytes={} asset_sha256={} source_sha256={} oracle_hunk_bytes={} oracle_sha256={} image_bytes={} image_sha256={} start_to_done_host_seconds={:?} fresh_completion=true scope=valid_default_encoding_ascii_asset", package.len(),digests[0],asset.len(),digests[1],digests[2],expected.len(),digests[3],image.len(),sha256(&image_path),run.start_to_done_host_seconds);
}
