use libopforge::opcore;
use libopforge::processing;
use std::error::Error;
use std::io;

pub fn run_example() -> Result<(), Box<dyn Error>> {
    let tokenized =
        opcore::tokenize_line("1 + 2", 1).map_err(|err| io::Error::other(err.to_string()))?;
    let expr = opcore::parse_expression(tokenized)
        .map_err(|err| io::Error::other(err.message.clone()))?;
    assert!(matches!(expr, opcore::Expr::Binary { .. }));

    let module_item = opcore::process_module_item(".use math as m", 2);
    match module_item {
        processing::ProcessingOutcome::Done(opcore::LineAst::Use(use_ast)) => {
            assert_eq!(use_ast.module_id, "math");
            assert_eq!(use_ast.alias.as_deref(), Some("m"));
        }
        other => panic!("expected .use AST, got {other:?}"),
    }

    Ok(())
}

#[allow(dead_code)]
fn main() -> Result<(), Box<dyn Error>> {
    run_example()
}