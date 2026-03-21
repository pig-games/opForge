use libopforge::asm::{
    AssemblerSession, ExecutionMode, LabelOutputFormat, OutputFormat,
};
use std::error::Error;
use std::fs;
use std::process;
use std::time::{SystemTime, UNIX_EPOCH};

pub fn run_example() -> Result<(), Box<dyn Error>> {
    let nanos = SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos();
    let work_dir = std::env::temp_dir().join(format!(
        "libopforge-filesystem-example-{}-{nanos}",
        process::id()
    ));
    fs::create_dir_all(&work_dir)?;

    let source_path = work_dir.join("main.asm");
    let out_dir = work_dir.join("out");
    fs::create_dir_all(&out_dir)?;
    fs::write(&source_path, ".module main\n    .byte $00\n.endmodule\n")?;
    let output_base = source_path
        .with_extension("")
        .to_string_lossy()
        .into_owned();

    let report = AssemblerSession::builder(source_path.clone())
        .output_base(output_base)
        .execution_mode(ExecutionMode::Vm)
        .out_dir(out_dir.clone())
        .output_format(OutputFormat::Text)
        .label_output_format(LabelOutputFormat::Vice)
        .header_title("libopforge filesystem example")
        .assemble()?;

    let listing_path = out_dir.join("main.lst");
    let hex_path = out_dir.join("main.hex");
    let listing = fs::read_to_string(&listing_path)?;
    let hex = fs::read_to_string(&hex_path)?;

    assert!(listing.contains(".byte $00"), "listing:\n{listing}");
    assert!(hex.contains(":0100000000FF"), "hex:\n{hex}");

    println!("errors: {}", report.error_count());
    println!("listing path: {}", listing_path.display());
    println!("hex path: {}", hex_path.display());

    Ok(())
}

#[allow(dead_code)]
fn main() -> Result<(), Box<dyn Error>> {
    run_example()
}