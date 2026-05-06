use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;

use clap::Parser;
use vm::native6502::{
    encode_wire_line_payload, encode_wire_set_pipeline_payload, Native6502ControlBlockV1,
    Native6502Harness, NATIVE_6502_STATUS_OK_V1,
};
use vm::native6502_abi::{
    NATIVE_6502_ENTRYPOINT_INIT_V1, NATIVE_6502_ENTRYPOINT_LAST_ERROR_V1,
    NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1, NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1,
    NATIVE_6502_ENTRYPOINT_TOKENIZE_LINE_V1,
};

#[derive(Parser, Debug)]
#[command(
    name = "tkpkg-tokenize-file",
    about = "Tokenize an assembly file through the current tkpkg native tokenizer wire contract"
)]
struct Args {
    #[arg(value_name = "INPUT")]
    input: PathBuf,
    #[arg(
        long = "package",
        value_name = "FILE",
        default_value_os_t = default_package_path(),
        help = "Path to the .opasm package used by the native tokenizer harness"
    )]
    package: PathBuf,
    #[arg(long = "cpu", default_value = "m68020")]
    cpu: String,
    #[arg(long = "dialect", default_value = "motorola68k")]
    dialect: String,
}

fn main() {
    let args = Args::parse();
    if let Err(message) = run(args) {
        eprintln!("{message}");
        std::process::exit(1);
    }
}

fn run(args: Args) -> Result<(), String> {
    let package_bytes = fs::read(&args.package)
        .map_err(|err| format!("read package {}: {err}", args.package.display()))?;
    let source = fs::read_to_string(&args.input)
        .map_err(|err| format!("read input {}: {err}", args.input.display()))?;
    let rendered = tokenize_source_text(
        package_bytes.as_slice(),
        args.cpu.as_str(),
        Some(args.dialect.as_str()),
        source.as_str(),
    )?;
    io::stdout()
        .write_all(rendered.as_bytes())
        .map_err(|err| format!("write stdout: {err}"))?;
    Ok(())
}

fn tokenize_source_text(
    package_bytes: &[u8],
    cpu_id: &str,
    dialect_override: Option<&str>,
    source: &str,
) -> Result<String, String> {
    let mut harness = Native6502Harness::new();
    let mut control_block = Native6502ControlBlockV1::new_v1();

    let init = harness.invoke_wire_v1(&mut control_block, NATIVE_6502_ENTRYPOINT_INIT_V1, &[]);
    ensure_ok(
        &mut harness,
        &mut control_block,
        init.status_code,
        "init",
        None,
    )?;

    let load = harness.invoke_wire_v1(
        &mut control_block,
        NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
        package_bytes,
    );
    ensure_ok(
        &mut harness,
        &mut control_block,
        load.status_code,
        "load_package",
        None,
    )?;

    let set_pipeline_payload = encode_wire_set_pipeline_payload(cpu_id, dialect_override)?;
    let set_pipeline = harness.invoke_wire_v1(
        &mut control_block,
        NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1,
        set_pipeline_payload.as_slice(),
    );
    ensure_ok(
        &mut harness,
        &mut control_block,
        set_pipeline.status_code,
        "set_pipeline",
        None,
    )?;

    let mut rendered = String::new();
    for (line_num, line_text) in collect_source_lines(source) {
        let payload = encode_wire_line_payload(line_num, line_text.as_str());
        let tokenize = harness.invoke_wire_v1(
            &mut control_block,
            NATIVE_6502_ENTRYPOINT_TOKENIZE_LINE_V1,
            payload.as_slice(),
        );
        ensure_ok(
            &mut harness,
            &mut control_block,
            tokenize.status_code,
            "tokenize_line",
            Some(line_num),
        )?;
        if !tokenize.output_payload.is_empty() {
            let chunk = String::from_utf8(tokenize.output_payload)
                .map_err(|_| format!("line {line_num}: tokenizer output is not UTF-8"))?;
            rendered.push_str(chunk.as_str());
        }
    }

    Ok(rendered)
}

fn ensure_ok(
    harness: &mut Native6502Harness,
    control_block: &mut Native6502ControlBlockV1,
    status_code: u16,
    operation: &str,
    line_num: Option<u32>,
) -> Result<(), String> {
    if status_code == NATIVE_6502_STATUS_OK_V1 {
        return Ok(());
    }

    let last_error =
        harness.invoke_wire_v1(control_block, NATIVE_6502_ENTRYPOINT_LAST_ERROR_V1, &[]);
    let mut message = String::from_utf8(last_error.output_payload)
        .unwrap_or_else(|_| format!("{operation} failed with non-UTF-8 last_error payload"));
    if message.is_empty() {
        message = format!("{operation} failed with status {status_code}");
    }
    if let Some(line) = line_num {
        Err(format!("line {line}: {message}"))
    } else {
        Err(message)
    }
}

fn collect_source_lines(source: &str) -> Vec<(u32, String)> {
    if source.is_empty() {
        return Vec::new();
    }

    let mut lines = Vec::new();
    let mut line_num = 1u32;
    let mut start = 0usize;
    while start < source.len() {
        let end = match source[start..].find('\n') {
            Some(offset) => start + offset,
            None => source.len(),
        };
        let mut line = &source[start..end];
        if let Some(stripped) = line.strip_suffix('\r') {
            line = stripped;
        }
        lines.push((line_num, line.to_string()));
        line_num = line_num.saturating_add(1);
        if end == source.len() {
            break;
        }
        start = end + 1;
    }
    lines
}

fn default_package_path() -> PathBuf {
    workspace_root()
        .join("native")
        .join("motorola68000")
        .join("amigaos")
        .join("tkpkg")
        .join("tkpkg_debug_cli_package.opasm")
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
}

#[cfg(test)]
mod tests {
    use super::*;

    fn smoke_package_bytes() -> Vec<u8> {
        let path = default_package_path();
        fs::read(&path).unwrap_or_else(|err| panic!("read package {}: {err}", path.display()))
    }

    #[test]
    fn collect_source_lines_preserves_blank_lines_and_crlf() {
        let lines = collect_source_lines("move.b d0,d1\r\n\r\nmove.b d2,d3\n");

        assert_eq!(
            lines,
            vec![
                (1, "move.b d0,d1".to_string()),
                (2, "".to_string()),
                (3, "move.b d2,d3".to_string()),
            ]
        );
    }

    #[test]
    fn tokenize_source_text_runs_each_line_through_native_tkpkg_harness() {
        let rendered = tokenize_source_text(
            smoke_package_bytes().as_slice(),
            "m68020",
            Some("motorola68k"),
            "move.b d0,d1\nmove.b d2,d3\n",
        )
        .expect("tokenize source text");

        assert!(rendered.contains("Identifier(\"move.b\")@1:1-7"));
        assert!(rendered.contains("Identifier(\"d0\")@1:8-10"));
        assert!(rendered.contains("Identifier(\"move.b\")@2:1-7"));
        assert!(rendered.contains("Identifier(\"d2\")@2:8-10"));
    }
}
