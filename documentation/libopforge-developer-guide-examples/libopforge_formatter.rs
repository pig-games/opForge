use libopforge::formatter::{FormatterConfig, FormatterEngine};
use std::error::Error;

pub fn run_example() -> Result<(), Box<dyn Error>> {
    let engine = FormatterEngine::new(FormatterConfig::default());
    let output = engine.format_source_with_diagnostics("start:  lda #$10,x ; comment\n");

    assert_eq!(output.rendered, "start:  lda #$10, x  ; comment\n");
    assert!(output.diagnostics.is_empty());

    Ok(())
}

#[allow(dead_code)]
fn main() -> Result<(), Box<dyn Error>> {
    run_example()
}