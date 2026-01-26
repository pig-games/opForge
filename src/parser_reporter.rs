// Reporter for parser errors with source context.

use crate::parser::ParseError;

pub fn format_parse_error(
    err: &ParseError,
    file: Option<&str>,
    lines: Option<&[String]>,
    use_color: bool,
) -> String {
    let header = match file {
        Some(file) => format!("{file}:{}: ERROR", err.span.line),
        None => format!("{}: ERROR", err.span.line),
    };

    let mut out = String::new();
    out.push_str(&header);
    out.push('\n');

    let line_num = err.span.line;
    let line_idx = line_num.saturating_sub(1) as usize;
    let line_text = lines
        .and_then(|lines| lines.get(line_idx))
        .map(|s| s.as_str())
        .unwrap_or("<source unavailable>");

    let highlighted = highlight_line(line_text, err.span.col_start, use_color);
    out.push_str(&format!("{:>5} | {}", line_num, highlighted));
    out.push('\n');
    out.push_str(&format!("ERROR: {}", err.message));
    out
}

pub fn format_parse_error_listing(
    err: &ParseError,
    lines: Option<&[String]>,
    use_color: bool,
) -> String {
    let line_num = err.span.line;
    let line_idx = line_num.saturating_sub(1) as usize;
    let line_text = lines
        .and_then(|lines| lines.get(line_idx))
        .map(|s| s.as_str())
        .unwrap_or("<source unavailable>");

    let highlighted = highlight_line(line_text, err.span.col_start, use_color);
    let mut out = String::new();
    out.push_str(&format!("{:>5} | {}", line_num, highlighted));
    out.push('\n');
    out.push_str(&format!("ERROR: {}", err.message));
    out
}

fn highlight_line(line: &str, column: usize, use_color: bool) -> String {
    if column == 0 {
        return line.to_string();
    }
    let idx = column.saturating_sub(1);
    if idx >= line.len() {
        if use_color {
            return format!("{line}\x1b[31m^\x1b[0m");
        }
        return format!("{line}^");
    }
    let (head, tail) = line.split_at(idx);
    let ch = tail.chars().next().unwrap_or(' ');
    let rest = &tail[ch.len_utf8()..];
    if use_color {
        format!("{head}\x1b[31m{ch}\x1b[0m{rest}")
    } else {
        format!("{head}{ch}{rest}")
    }
}
