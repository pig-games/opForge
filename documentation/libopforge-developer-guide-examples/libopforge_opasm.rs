use libopforge::asm::opasm;
use libopforge::opcore;
use libopforge::processing;
use std::error::Error;
use std::io;

pub fn run_example() -> Result<(), Box<dyn Error>> {
    let tokenized = opasm::tokenize_statement(opasm::StatementRequest::new(".byte 1, 2", 1))
        .map_err(|err| io::Error::other(err.message.clone()))?;
    assert!(!tokenized.tokens.is_empty());

    let processed =
        opasm::process_statement(opasm::StatementRequest::new(".module demo", 2), None)
            .map_err(|err| io::Error::other(err.message.clone()))?;
    assert!(matches!(processed.parsed.ast, opcore::LineAst::Statement(..)));
    assert_eq!(
        processed.trace.requests(),
        &[processing::ProcessingRequestKind::Processor {
            processor: "asm".to_string(),
            kind: "statement".to_string(),
        }]
    );

    Ok(())
}

#[allow(dead_code)]
fn main() -> Result<(), Box<dyn Error>> {
    run_example()
}