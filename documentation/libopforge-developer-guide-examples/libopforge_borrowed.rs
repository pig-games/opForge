use libopforge::asm::{Assembler, LabelOutputFormat, OutputFormat};
use libopforge::io::{MemoryOutputSink, MemorySourceProvider};
use std::error::Error;
use std::io;
use std::path::Path;

pub fn run_example() -> Result<(), Box<dyn Error>> {
    let source_provider = MemorySourceProvider::new().with_file(
        "/virtual/main.asm",
        ".module main\nstart:\n    .byte $2a\n.endmodule\n",
    );
    let output_sink = MemoryOutputSink::new();

    let report = Assembler::builder(Path::new("/virtual/main.asm"))
        .output_base("/virtual/main")
        .output_format(OutputFormat::Text)
        .label_output_format(LabelOutputFormat::Vice)
        .source_provider(&source_provider)
        .output_sink(&output_sink)
        .assemble()?;

    let listing = output_sink
        .text("/virtual/main.lst")?
        .ok_or_else(|| io::Error::other("missing listing"))?;
    assert_eq!(report.error_count(), 0);
    assert!(listing.contains(".byte $2a"), "listing:\n{listing}");

    Ok(())
}

#[allow(dead_code)]
fn main() -> Result<(), Box<dyn Error>> {
    run_example()
}