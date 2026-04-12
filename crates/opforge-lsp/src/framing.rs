// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use std::io::{self, BufRead, Write};

use serde_json::Value;

pub const MAX_LSP_MESSAGE_BYTES: usize = 8 * 1024 * 1024;

pub fn read_lsp_message(reader: &mut impl BufRead) -> io::Result<Option<Value>> {
    let mut content_length: Option<usize> = None;
    let mut saw_headers = false;

    loop {
        let mut line = String::new();
        let read = reader.read_line(&mut line)?;
        if read == 0 {
            if saw_headers {
                return Err(io::Error::new(
                    io::ErrorKind::UnexpectedEof,
                    "unexpected EOF while reading LSP headers",
                ));
            }
            return Ok(None);
        }
        saw_headers = true;

        let trimmed = line.trim_end_matches(['\r', '\n']);
        if trimmed.is_empty() {
            break;
        }

        let Some((name, value)) = trimmed.split_once(':') else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("invalid LSP header line: {trimmed}"),
            ));
        };

        if name.eq_ignore_ascii_case("Content-Length") {
            let length = value.trim().parse::<usize>().map_err(|_| {
                io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("invalid Content-Length header: {}", value.trim()),
                )
            })?;
            if length > MAX_LSP_MESSAGE_BYTES {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    format!("LSP message exceeds maximum size of {MAX_LSP_MESSAGE_BYTES} bytes"),
                ));
            }
            content_length = Some(length);
        }
    }

    let Some(length) = content_length else {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "missing Content-Length header",
        ));
    };

    let mut body = vec![0u8; length];
    reader.read_exact(&mut body)?;
    let value = serde_json::from_slice::<Value>(&body).map_err(|err| {
        io::Error::new(
            io::ErrorKind::InvalidData,
            format!("invalid JSON payload: {err}"),
        )
    })?;
    Ok(Some(value))
}

pub fn write_lsp_message(writer: &mut impl Write, payload: &Value) -> io::Result<()> {
    let body = payload.to_string();
    write!(writer, "Content-Length: {}\r\n\r\n{}", body.len(), body)
}
