use libopforge::asm::{
    AssemblerSession, ContinuationHead, DiagnosticsOptions, ExecutionMode, LabelOutputFormat,
    OutputFormat, OwnedAssemblerConfig, OwnedExecutionOptions, OwnedOutputOptions,
    OwnedSourceOptions,
};
use libopforge::io::{MemoryOutputSink, MemorySourceProvider};
use std::error::Error;
use std::sync::Arc;

fn main() -> Result<(), Box<dyn Error>> {
    let source_provider = MemorySourceProvider::new().with_file(
        "/virtual/main.asm",
        ".module main\n    .byte $00\n.endmodule\n",
    );
    let output_sink = MemoryOutputSink::new();

    let prepared = AssemblerSession::with_config(
        "/virtual/main.asm",
        OwnedAssemblerConfig {
            source: OwnedSourceOptions {
                input_base: "/virtual/main".to_string(),
                source_provider: Some(Arc::new(source_provider.clone())),
                ..OwnedSourceOptions::default()
            },
            execution: OwnedExecutionOptions {
                execution_mode: ExecutionMode::Lockstep {
                    continuation_head: ContinuationHead::Vm,
                },
                ..OwnedExecutionOptions::default()
            },
            output: OwnedOutputOptions {
                output_format: OutputFormat::Text,
                label_output_format: LabelOutputFormat::Vice,
                header_title: "libopforge example".to_string(),
                output_sink: Some(Arc::new(output_sink.clone())),
                ..OwnedOutputOptions::default()
            },
            diagnostics: DiagnosticsOptions::default(),
        },
    )
    .prepare()?;

    let report = prepared.assemble()?;

    let listing = output_sink
        .text("/virtual/main.lst")
        .ok_or("missing listing")?;
    let hex = output_sink.text("/virtual/main.hex").ok_or("missing hex")?;
    assert!(listing.contains(".byte $00"), "listing:\n{listing}");
    assert!(hex.contains(":0100000000FF"), "hex:\n{hex}");

    println!("errors: {}", report.error_count());
    println!("cpu: {}", prepared.cpu_name());
    println!(
        "lockstep matches: {}",
        report.lockstep_report().matches().len()
    );
    println!("listing:\n{listing}");
    println!("hex:\n{hex}");

    Ok(())
}
