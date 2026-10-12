// SPDX-License-Identifier: GPL-3.0-or-later
//! Invocation scope and source-boundary controls against independent core matching.
use super::AsmMacroProcessor;

#[test]
fn prvm_statement_source_selection_preserves_prefix_fallback_and_ambiguity() {
    let mut processor = AsmMacroProcessor::new(64);
    processor
        .expand(&[
            ".statement N byte:x".into(),
            ".byte .x".into(),
            ".endstatement".into(),
            ".statement NX char:x[{byte:y}]".into(),
            ".byte .x,.y".into(),
            ".endstatement".into(),
        ])
        .unwrap();
    assert_eq!(
        processor.expand(&["    NXname".into()]).unwrap(),
        vec!["    NXname"]
    );
    assert_eq!(
        processor.expand(&["    Nd0".into()]).unwrap(),
        vec![".byte d0"]
    );
    assert_eq!(
        processor.expand(&["    NXd0".into()]).unwrap(),
        vec![".byte d,0"]
    );
    for source in ["here : N 7", "    Z \"unterminated", "    .N 7"] {
        assert_eq!(processor.expand(&[source.into()]).unwrap(), vec![source]);
    }
    processor
        .expand(&[".statement N byte:y".into(), ".endstatement".into()])
        .unwrap();
    let error = processor.expand(&["    Nd0".into()]).unwrap_err();
    assert!(error.message().contains("Ambiguous statement signature"));
}

#[test]
fn prvm_statement_source_adapter_matches_reference_token_boundaries() {
    use opcore::tokenizer::{TokenKind, Tokenizer};
    for signature in [
        "byte:x",
        "\"[\"[{byte:x}]\"],y\"",
        "\"$ff\"",
        "char:x[{byte:y}]",
    ] {
        let mut producer = AsmMacroProcessor::new(64);
        producer
            .expand(&[format!(".statement N {signature}"), ".endstatement".into()])
            .unwrap();
        let definitions = producer.inner.statement_defs();
        for (input, reference) in [
            ("7", "7"),
            ("$ff", "$ff"),
            ("255", "255"),
            ("256", "256"),
            ("[$05],y", "[$05],y"),
            ("[ $05],y", "[ $05],y"),
            ("[$05 ],y", "[$05 ],y"),
            ("[$05], y", "[$05], y"),
            ("d0", "d0"),
        ] {
            let mut lexer = Tokenizer::new(reference, 1);
            let mut tokens = Vec::new();
            loop {
                let token = lexer.next_token().unwrap();
                if matches!(token.kind, TokenKind::End) {
                    break;
                }
                tokens.push(token);
            }
            let signatures = [definitions[0].signature().clone()];
            let expected =
                opcore::parser::select_and_match_statement_signature_texts(&signatures, &tokens)
                    .unwrap();
            for (source, label) in [
                (format!("    N{input}"), None),
                (format!("    N {input}"), None),
                (format!("here: N {input}"), Some("here")),
            ] {
                let actual = vm::statement_invocation_source::select_source(
                    &package::statement_program::match_program(),
                    &source,
                    1,
                    &definitions,
                )
                .unwrap();
                // The independent matcher covers original tokenization. The
                // extra decimal fallback has separate numeric controls.
                if signature == "char:x[{byte:y}]" && input == "d0" {
                    continue;
                }
                assert_eq!(
                    actual.as_ref().map(|v| &v.captures),
                    expected.as_ref().map(|(_, captures)| captures),
                    "{signature}: {source}"
                );
                if let Some(actual) = actual {
                    assert_eq!(actual.label.as_deref(), label);
                }
            }
        }
    }
}

#[test]
fn prvm_statement_imports_preserve_caller_visible_keywords() {
    let mut producer = AsmMacroProcessor::new(64);
    producer
        .expand(&[
            ".pub".into(),
            ".statement PUSH byte:v".into(),
            ".byte .v".into(),
            ".endstatement".into(),
        ])
        .unwrap();
    let exports = producer.take_native_exports();
    let plan = exports.statements[0].signature().prepared_plan().unwrap();
    for (head, mode) in [("PUSH", 0), ("X", 1), ("Q.PUSH", 2)] {
        let mut consumer = AsmMacroProcessor::new(64);
        match mode {
            0 => consumer.inject_all(&exports),
            1 => consumer.inject_from(&exports, &["PUSH".into()], &["X".into()]),
            _ => consumer.inject_qualified(&exports, "q"),
        }
        let definitions = consumer.inner.statement_defs();
        assert_eq!(definitions[0].keyword(), head);
        assert!(std::sync::Arc::ptr_eq(
            plan,
            definitions[0].signature().prepared_plan().unwrap()
        ));
        assert_eq!(
            consumer.expand(&[format!("    {head} 7")]).unwrap(),
            vec![".byte 7"]
        );
        if mode != 0 {
            assert_eq!(
                consumer.expand(&["    PUSH 7".into()]).unwrap(),
                vec!["    PUSH 7"]
            );
        }
        assert!(consumer.take_native_exports().statements.is_empty());
    }
}
