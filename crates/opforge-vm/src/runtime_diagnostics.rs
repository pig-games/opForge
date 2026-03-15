// SPDX-License-Identifier: GPL-3.0-or-later

//! Shared VM runtime diagnostic value types used across bridge adapters.

use opcore::tokenizer::Span;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RuntimeBridgeDiagnostic {
    pub code: String,
    pub message: String,
    pub span: Option<Span>,
}

impl RuntimeBridgeDiagnostic {
    pub fn new(code: impl Into<String>, message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
            span,
        }
    }

    pub fn render(&self) -> String {
        let code = self.code.trim();
        if code.is_empty() {
            self.message.clone()
        } else if self.message.is_empty() {
            code.to_string()
        } else {
            format!("{}: {}", code, self.message)
        }
    }
}
