use libopforge::asm::{
    AssemblerSession, ContinuationHead, ExecutionMode, LabelOutputFormat, OutputFormat,
};
use libopforge::io::{MemoryOutputSink, MemorySourceProvider};
use std::error::Error;

pub fn run_example() -> Result<(), Box<dyn Error>> {
    let source_provider = MemorySourceProvider::new().with_file(
        "/virtual/main.asm",
        ".module main\n    lda #$42\n.endmodule\n",
    );
    let output_sink = MemoryOutputSink::new();

    let report = AssemblerSession::builder("/virtual/main.asm")
        .output_base("/virtual/main")
        .source_provider(source_provider.clone())
        .output_sink(output_sink.clone())
        .execution_mode(ExecutionMode::Lockstep {
            continuation_head: ContinuationHead::Vm,
        })
        .output_format(OutputFormat::Text)
        .label_output_format(LabelOutputFormat::Vice)
        .assemble()?;

    assert_eq!(report.error_count(), 0);
    assert!(!report.lockstep_report().matches().is_empty());

    Ok(())
}

#[allow(dead_code)]
fn main() -> Result<(), Box<dyn Error>> {
    run_example()
}