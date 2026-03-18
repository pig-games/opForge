use libopforge::asm::{AssemblerSession, LabelOutputFormat, OutputFormat};
use libopforge::io::{MemoryOutputSink, MemorySourceProvider};
use std::error::Error;

pub fn run_example() -> Result<(), Box<dyn Error>> {
    let source_provider = MemorySourceProvider::new().with_file(
        "/virtual/main.asm",
        ".module main\n    .byte $00\n.endmodule\n",
    );
    let output_sink = MemoryOutputSink::new();

    let prepared = AssemblerSession::builder("/virtual/main.asm")
        .output_base("/virtual/main")
        .source_provider(source_provider.clone())
        .output_sink(output_sink.clone())
        .output_format(OutputFormat::Text)
        .label_output_format(LabelOutputFormat::Vice)
        .prepare()?;

    assert_eq!(prepared.root_module_id(), "main");
    assert!(!prepared.cpu_name().is_empty());
    assert!(prepared
        .dependency_files()
        .iter()
        .any(|path| path.ends_with("main.asm")));

    let report = prepared.assemble()?;
    assert_eq!(report.error_count(), 0);

    Ok(())
}

#[allow(dead_code)]
fn main() -> Result<(), Box<dyn Error>> {
    run_example()
}