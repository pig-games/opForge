// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use std::io::{self, BufReader, Write};
use std::sync::mpsc::{self, RecvTimeoutError};
use std::thread;
use std::time::Duration;

use serde_json::{json, Value};

use crate::framing::{read_lsp_message, write_lsp_message};
use crate::lsp::session::{LspSession, OutboundMessage};
use libopforge::registry::{default_asm_registry, AsmRegistry};

pub fn run_stdio() -> io::Result<()> {
    run_stdio_with_registry(default_asm_registry())
}

pub fn run_stdio_with_registry(registry: AsmRegistry) -> io::Result<()> {
    let stdout = io::stdout();
    let mut writer = io::BufWriter::new(stdout.lock());
    let inbound_rx = spawn_stdin_reader();
    let mut session = LspSession::with_registry(registry);

    loop {
        let async_outbound = session.poll_async_notifications();
        for item in async_outbound {
            write_lsp_message(&mut writer, &outbound_to_json(item))?;
        }
        writer.flush()?;

        match inbound_rx.recv_timeout(Duration::from_millis(25)) {
            Ok(InboundMessage::Payload(message)) => {
                let outbound = session.handle_message(&message);
                for item in outbound {
                    write_lsp_message(&mut writer, &outbound_to_json(item))?;
                }
                writer.flush()?;
                if session.should_exit() {
                    break;
                }
            }
            Ok(InboundMessage::ProtocolError(message)) => {
                write_lsp_message(&mut writer, &protocol_error_payload(message.as_str()))?;
                writer.flush()?;
                break;
            }
            Ok(InboundMessage::Eof) => break,
            Err(RecvTimeoutError::Timeout) => continue,
            Err(RecvTimeoutError::Disconnected) => break,
        }
    }
    Ok(())
}

enum InboundMessage {
    Payload(Value),
    ProtocolError(String),
    Eof,
}

fn spawn_stdin_reader() -> mpsc::Receiver<InboundMessage> {
    let (tx, rx) = mpsc::channel();
    thread::spawn(move || {
        let stdin = io::stdin();
        let mut reader = BufReader::new(stdin.lock());
        loop {
            match read_lsp_message(&mut reader) {
                Ok(Some(value)) => {
                    if tx.send(InboundMessage::Payload(value)).is_err() {
                        break;
                    }
                }
                Ok(None) => {
                    let _ = tx.send(InboundMessage::Eof);
                    break;
                }
                Err(err) => {
                    let _ = tx.send(InboundMessage::ProtocolError(err.to_string()));
                    break;
                }
            }
        }
    });
    rx
}

fn outbound_to_json(message: OutboundMessage) -> Value {
    match message {
        OutboundMessage::Response { id, result } => json!({
            "jsonrpc": "2.0",
            "id": id,
            "result": result,
        }),
        OutboundMessage::Error { id, code, message } => json!({
            "jsonrpc": "2.0",
            "id": id,
            "error": {
                "code": code,
                "message": message,
            }
        }),
        OutboundMessage::Notification { method, params } => json!({
            "jsonrpc": "2.0",
            "method": method,
            "params": params,
        }),
    }
}

fn protocol_error_payload(message: &str) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": Value::Null,
        "error": {
            "code": -32700,
            "message": message,
        }
    })
}

#[cfg(test)]
mod tests {
    use std::io::Cursor;

    use super::*;

    #[test]
    fn read_lsp_message_parses_valid_frame() {
        let input = b"Content-Length: 2\r\n\r\n{}";
        let mut reader = Cursor::new(input.as_slice());

        let value = read_lsp_message(&mut reader)
            .expect("valid frame")
            .expect("payload");

        assert_eq!(value, json!({}));
    }

    #[test]
    fn read_lsp_message_rejects_missing_content_length() {
        let input = b"Content-Type: application/vscode-jsonrpc; charset=utf-8\r\n\r\n{}";
        let mut reader = Cursor::new(input.as_slice());

        let err = read_lsp_message(&mut reader).expect_err("missing Content-Length must fail");

        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
        assert!(err.to_string().contains("missing Content-Length"));
    }

    #[test]
    fn read_lsp_message_rejects_oversized_frame() {
        let input = format!(
            "Content-Length: {}\r\n\r\n",
            crate::framing::MAX_LSP_MESSAGE_BYTES + 1
        );
        let mut reader = Cursor::new(input.into_bytes());

        let err = read_lsp_message(&mut reader).expect_err("oversized Content-Length must fail");

        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
        assert!(err.to_string().contains("exceeds maximum size"));
    }

    #[test]
    fn read_lsp_message_rejects_invalid_json_payload() {
        let input = b"Content-Length: 1\r\n\r\n{";
        let mut reader = Cursor::new(input.as_slice());

        let err = read_lsp_message(&mut reader).expect_err("invalid JSON must fail");

        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
        assert!(err.to_string().contains("invalid JSON payload"));
    }

    #[test]
    fn protocol_write_lsp_message_emits_valid_frame() {
        let mut output = Vec::new();

        write_lsp_message(&mut output, &json!({"jsonrpc": "2.0"})).expect("write frame");

        assert_eq!(
            String::from_utf8(output).expect("utf8 output"),
            "Content-Length: 17\r\n\r\n{\"jsonrpc\":\"2.0\"}"
        );
    }
}
