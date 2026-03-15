// SPDX-License-Identifier: GPL-3.0-or-later

use std::env;
use std::fs;
use std::path::PathBuf;

fn artifact_path_from_args() -> PathBuf {
    let mut args = env::args().skip(1);
    if let Some(path) = args.next() {
        PathBuf::from(path)
    } else {
        PathBuf::from("target/vm/hierarchy.opasm")
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let output_path = artifact_path_from_args();
    if let Some(parent) = output_path.parent() {
        fs::create_dir_all(parent)?;
    }

    let bytes = api::unstable::build_default_runtime_package_bytes()
        .ok_or_else(|| "failed to build default runtime package".to_string())?;
    fs::write(&output_path, &bytes)?;

    println!("wrote {} bytes to {}", bytes.len(), output_path.display());
    Ok(())
}
