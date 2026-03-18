use libopforge::asm::{Assembler, AssemblerWorkflowErrorKind, LabelOutputFormat, OutputFormat};
use libopforge::diagnostics::AsmErrorKind;
use libopforge::io::{MemoryOutputSink, MemorySourceProvider};
use std::error::Error;
use std::path::Path;

pub fn run_example() -> Result<(), Box<dyn Error>> {
    let source_provider = MemorySourceProvider::new().with_file(
        "/virtual/main.asm",
        ".module main\n.this_is_not_a_real_directive\n.endmodule\n",
    );
    let output_sink = MemoryOutputSink::new();

    let err = match Assembler::builder(Path::new("/virtual/main.asm"))
        .output_base("/virtual/main")
        .output_format(OutputFormat::Text)
        .label_output_format(LabelOutputFormat::Vice)
        .source_provider(&source_provider)
        .output_sink(&output_sink)
        .assemble()
    {
        Ok(_) => panic!("invalid source should fail"),
        Err(err) => err,
    };

    assert_eq!(err.kind(), AssemblerWorkflowErrorKind::Assemble);
    assert_eq!(err.code(), "asm.workflow.assemble");
    assert_eq!(err.as_assemble().expect("assemble payload").kind(), AsmErrorKind::Assembler);

    Ok(())
}

#[allow(dead_code)]
fn main() -> Result<(), Box<dyn Error>> {
    run_example()
}