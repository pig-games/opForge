// SPDX-License-Identifier: GPL-3.0-or-later

//! Shared parse-error bridge helpers used by runtime-backed parser adapters.

use opcore::parser::ParseError;
use opcore::tokenizer::Span;

use crate::runtime_diagnostics::RuntimeBridgeDiagnostic;
use crate::runtime_error::RuntimeBridgeError;
use crate::tokenizer_runtime_utils::parser_end_metadata;

pub fn parse_error_at_end(line: &str, line_num: u32, message: impl Into<String>) -> ParseError {
    let end_span = parse_span_at_end(line, line_num);
    ParseError {
        message: message.into(),
        span: end_span,
    }
}

pub fn parse_span_at_end(line: &str, line_num: u32) -> Span {
    let (end_span, _) = parser_end_metadata(line, line_num, &[]);
    end_span
}

pub fn runtime_bridge_error_to_parse_error(
    err: RuntimeBridgeError,
    fallback_span: Span,
) -> ParseError {
    match err {
        RuntimeBridgeError::Diagnostic(RuntimeBridgeDiagnostic {
            message,
            span,
            code,
        }) => {
            let rendered = if code.trim().is_empty() {
                message
            } else if message.is_empty() {
                code
            } else {
                format!("{}: {}", code, message)
            };
            ParseError {
                message: rendered,
                span: span.unwrap_or(fallback_span),
            }
        }
        other => ParseError {
            message: other.to_string(),
            span: fallback_span,
        },
    }
}
