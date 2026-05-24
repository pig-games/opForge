// SPDX-License-Identifier: GPL-3.0-or-later

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt::Write as FmtWrite;
use std::io::Write;

use opcore::conditional::ConditionalContext;
use opcore::parser::ParseError;
use opcore::text_utils::{is_ident_start, split_comment, Cursor};
use types::artifacts::format_addr;
use types::assembler::{LineStatus, PassCounts};
use types::diagnostics::{build_context_lines, Diagnostic, Severity};
use types::symbol::{SymbolTable, SymbolVisibility};

/// Data for a single listing line.
pub struct ListingLine<'a> {
    pub addr: u32,
    pub bytes: &'a [u8],
    pub status: LineStatus,
    pub aux: u32,
    pub line_num: u32,
    pub source: &'a str,
    pub section: Option<&'a str>,
    pub cond: Option<&'a ConditionalContext>,
}

/// Writer for listing file output.
pub struct ListingWriter<W: Write> {
    out: W,
    enabled: bool,
    show_cond: bool,
    tab_size: Option<usize>,
}

impl<W: Write> ListingWriter<W> {
    pub fn new(out: W, show_cond: bool) -> Self {
        Self {
            out,
            enabled: true,
            show_cond,
            tab_size: None,
        }
    }

    pub fn new_with_options(out: W, show_cond: bool, tab_size: Option<usize>) -> Self {
        Self {
            out,
            enabled: true,
            show_cond,
            tab_size,
        }
    }

    pub fn disabled(out: W) -> Self {
        Self {
            out,
            enabled: false,
            show_cond: false,
            tab_size: None,
        }
    }

    pub fn disabled_with_options(out: W, show_cond: bool, tab_size: Option<usize>) -> Self {
        Self {
            out,
            enabled: false,
            show_cond,
            tab_size,
        }
    }

    pub fn header(&mut self, title: &str) -> std::io::Result<()> {
        if !self.enabled {
            return Ok(());
        }
        writeln!(self.out, "{title}")?;
        writeln!(self.out, "ADDR    BYTES                    LINE  SOURCE")?;
        writeln!(self.out, "------  -----------------------  ----  ------")?;
        Ok(())
    }

    pub fn write_line(&mut self, line: ListingLine<'_>) -> std::io::Result<()> {
        if !self.enabled {
            return Ok(());
        }
        let (loc, bytes_col) = match line.status {
            LineStatus::DirEqu => (
                Cow::Borrowed("----"),
                Cow::Owned(format!("EQU {}", format_addr(line.aux))),
            ),
            LineStatus::DirDs => (
                Cow::Owned(format_addr(line.addr)),
                Cow::Owned(format!("+{}", format_addr(line.aux))),
            ),
            _ => {
                if line.bytes.is_empty() {
                    (Cow::Borrowed("----"), Cow::Borrowed(""))
                } else {
                    (
                        Cow::Owned(format_addr(line.addr)),
                        Cow::Owned(format_bytes(line.bytes)),
                    )
                }
            }
        };
        let normalized_source = normalize_leading_label_colon(line.source);
        let normalized_source = strip_ansi_sgr(normalized_source.as_ref());
        let source = if let Some(tab_size) = self.tab_size {
            expand_tabs(normalized_source.as_ref(), tab_size)
        } else {
            normalized_source
        };

        write!(
            self.out,
            "{:<6}  {:<23}  {:>4}  {}",
            loc, bytes_col, line.line_num, source
        )?;
        if let Some(name) = line.section {
            write!(self.out, "  ; [section {name}]")?;
        }
        if self.show_cond {
            if let Some(cond) = line.cond {
                write_cond(&mut self.out, cond)?;
            }
        }
        writeln!(self.out)
    }

    pub fn write_diagnostic(
        &mut self,
        kind: &str,
        msg: &str,
        line_num: u32,
        column: Option<usize>,
        source_lines: &[String],
        _parser_error: Option<&ParseError>,
    ) -> std::io::Result<()> {
        if !self.enabled {
            return Ok(());
        }
        let context = build_context_lines(line_num, column, Some(source_lines), None, false);
        for line in context {
            writeln!(self.out, "{}", strip_ansi_sgr(&line))?;
        }
        writeln!(self.out, "{kind}: {msg}")
    }

    pub fn write_diagnostic_with_annotations(
        &mut self,
        diagnostic: &Diagnostic,
        source_lines: &[String],
    ) -> std::io::Result<()> {
        let kind = match diagnostic.severity {
            Severity::Warning => "WARNING",
            Severity::Error => "ERROR",
        };
        self.write_diagnostic(
            kind,
            diagnostic.error.message(),
            diagnostic.line,
            diagnostic.column,
            source_lines,
            None,
        )?;
        for item in diagnostic.help() {
            writeln!(self.out, "help: {}", strip_ansi_sgr(item))?;
        }
        for fixit in diagnostic.fixits() {
            writeln!(
                self.out,
                "suggestion: replace {} with {:?}",
                format_span_bounds(fixit.line, fixit.col_start, fixit.col_end),
                fixit.replacement
            )?;
        }
        Ok(())
    }

    pub fn footer(
        &mut self,
        counts: &PassCounts,
        symbols: &SymbolTable,
        total_mem: usize,
    ) -> std::io::Result<()> {
        self.footer_with_generated_output(counts, symbols, total_mem, &[])
    }

    pub fn footer_with_generated_output(
        &mut self,
        counts: &PassCounts,
        symbols: &SymbolTable,
        total_mem: usize,
        generated_output: &[(u32, u8)],
    ) -> std::io::Result<()> {
        if !self.enabled {
            return Ok(());
        }
        writeln!(
            self.out,
            "\nLines: {}  Errors: {}  Warnings: {}",
            counts.lines, counts.errors, counts.warnings
        )?;
        writeln!(self.out, "\nSYMBOL TABLE\n")?;
        write_symbol_table(&mut self.out, symbols)?;
        writeln!(self.out, "\nTotal memory is {} bytes", total_mem)?;
        self.write_generated_output(generated_output)?;
        Ok(())
    }

    fn write_generated_output(&mut self, generated_output: &[(u32, u8)]) -> std::io::Result<()> {
        writeln!(self.out, "\nGENERATED OUTPUT\n")?;
        if generated_output.is_empty() {
            writeln!(self.out, "(none)")?;
            return Ok(());
        }

        let mut resolved = BTreeMap::new();
        for (addr, value) in generated_output {
            resolved.insert(*addr, *value);
        }

        writeln!(self.out, "ADDR    BYTES")?;
        writeln!(self.out, "------  -----------------------")?;

        let mut line_addr: Option<u32> = None;
        let mut prev_addr: Option<u32> = None;
        let mut line_bytes: Vec<u8> = Vec::new();

        for (addr, value) in resolved {
            let split = match prev_addr {
                Some(prev) => addr != prev.wrapping_add(1) || line_bytes.len() >= 16,
                None => false,
            };
            if split {
                if let Some(start) = line_addr {
                    writeln!(
                        self.out,
                        "{}    {}",
                        format_addr(start),
                        format_bytes(&line_bytes)
                    )?;
                }
                line_bytes.clear();
                line_addr = Some(addr);
            }
            if line_addr.is_none() {
                line_addr = Some(addr);
            }
            line_bytes.push(value);
            prev_addr = Some(addr);
        }

        if let Some(start) = line_addr {
            writeln!(
                self.out,
                "{}    {}",
                format_addr(start),
                format_bytes(&line_bytes)
            )?;
        }

        Ok(())
    }
}

fn write_symbol_table<W: Write>(out: &mut W, symbols: &SymbolTable) -> std::io::Result<()> {
    if symbols.entries().is_empty() {
        writeln!(out, "(none)")?;
        return Ok(());
    }

    let mut entries: Vec<_> = symbols
        .entries()
        .iter()
        .map(|entry| (entry.name.to_ascii_uppercase(), entry))
        .collect();
    entries.sort_by(|left, right| {
        left.0
            .cmp(&right.0)
            .then_with(|| left.1.name.cmp(&right.1.name))
    });

    writeln!(out, "NAME             VALUE     VIS  KIND")?;
    writeln!(out, "---------------  --------  ---  ----")?;
    for (_, entry) in entries {
        let visibility = match entry.visibility {
            SymbolVisibility::Public => "pub",
            SymbolVisibility::Private => "prv",
        };
        let kind = if entry.rw { "var" } else { "lbl" };
        writeln!(
            out,
            "{:<15}  {:<8}  {:<3}  {:<4}",
            entry.name,
            format_addr(entry.val),
            visibility,
            kind
        )?;
    }

    Ok(())
}

fn format_span_bounds(line: u32, col_start: Option<usize>, col_end: Option<usize>) -> String {
    match (col_start, col_end) {
        (Some(start), Some(end)) => format!("{line}:{start}-{end}"),
        (Some(start), None) => format!("{line}:{start}"),
        _ => format!("{line}"),
    }
}

fn normalize_leading_label_colon(source: &str) -> Cow<'_, str> {
    let (code, comment) = split_comment(source);
    let mut cursor = Cursor::new(code);
    cursor.skip_ws();
    let indent_end = cursor.pos();
    let Some(first) = cursor.peek() else {
        return Cow::Borrowed(source);
    };
    if matches!(first, b'.' | b'*' | b';' | b'#') {
        return Cow::Borrowed(source);
    }
    if !is_ident_start(first) {
        return Cow::Borrowed(source);
    }
    let Some(label) = cursor.take_ident() else {
        return Cow::Borrowed(source);
    };
    if cursor.peek() != Some(b':') {
        return Cow::Borrowed(source);
    }
    cursor.next();

    let remainder = &code[cursor.pos()..];
    let mut normalized = String::with_capacity(source.len() + 1);
    normalized.push_str(&code[..indent_end]);
    normalized.push_str(&label);
    if !remainder.is_empty() {
        let needs_space = !remainder.starts_with(' ') && !remainder.starts_with('\t');
        if needs_space {
            normalized.push(' ');
        }
        normalized.push_str(remainder);
    }
    normalized.push_str(comment);
    Cow::Owned(normalized)
}

fn expand_tabs(source: &str, tab_size: usize) -> Cow<'_, str> {
    if tab_size == 0 {
        return Cow::Borrowed(source);
    }
    if !source.as_bytes().contains(&b'\t') {
        return Cow::Borrowed(source);
    }
    let mut expanded = String::new();
    let mut column = 0usize;
    for ch in source.chars() {
        if ch == '\t' {
            let spaces = tab_size - (column % tab_size);
            for _ in 0..spaces {
                expanded.push(' ');
            }
            column += spaces;
        } else {
            expanded.push(ch);
            column += 1;
        }
    }
    Cow::Owned(expanded)
}

fn strip_ansi_sgr(text: &str) -> Cow<'_, str> {
    if !text.as_bytes().contains(&0x1b) {
        return Cow::Borrowed(text);
    }
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\u{1b}' && matches!(chars.peek(), Some('[')) {
            chars.next();
            for next in chars.by_ref() {
                if next.is_ascii_alphabetic() {
                    break;
                }
            }
            continue;
        }
        out.push(ch);
    }

    Cow::Owned(out)
}

/// Format bytes as hex string for listing.
pub fn format_bytes(bytes: &[u8]) -> String {
    let Some((first, rest)) = bytes.split_first() else {
        return String::new();
    };
    let mut formatted = String::with_capacity(bytes.len().saturating_mul(3).saturating_sub(1));
    write!(&mut formatted, "{first:02X}").expect("writing to String cannot fail");
    for byte in rest {
        write!(&mut formatted, " {byte:02X}").expect("writing to String cannot fail");
    }
    formatted
}

fn write_cond<W: Write>(out: &mut W, ctx: &ConditionalContext) -> std::io::Result<()> {
    let matched = if ctx.matched { '+' } else { ' ' };
    let skipping = if ctx.skipping { '-' } else { ' ' };
    write!(
        out,
        "  [{}{}{}{}]",
        matched, ctx.nest_level, ctx.skip_level, skipping
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn disabled_listing_writer_suppresses_all_listing_text() {
        let mut output = Vec::new();
        {
            let mut listing = ListingWriter::disabled(&mut output);
            listing.header("test").expect("header");
            listing
                .write_line(ListingLine {
                    addr: 0x1000,
                    bytes: &[0xea],
                    status: LineStatus::Ok,
                    aux: 0,
                    line_num: 1,
                    source: "nop",
                    section: None,
                    cond: None,
                })
                .expect("line");
            listing
                .write_diagnostic("ERROR", "boom", 1, None, &["nop".to_string()], None)
                .expect("diagnostic");
        }

        assert!(output.is_empty());
    }
}
