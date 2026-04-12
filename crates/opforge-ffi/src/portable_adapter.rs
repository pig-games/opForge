use crate::{OpforgeExprNodeKind, OpforgeTokenKind};

use api::opcore::portable::{
    PortableAstExpr, PortableOperatorKind, PortableSpan, PortableToken, PortableTokenKind,
};

pub(crate) fn token_kind(kind: &PortableTokenKind) -> OpforgeTokenKind {
    match kind {
        PortableTokenKind::Identifier(_) => OpforgeTokenKind::Identifier,
        PortableTokenKind::Register(_) => OpforgeTokenKind::Register,
        PortableTokenKind::Number { .. } => OpforgeTokenKind::Number,
        PortableTokenKind::String { .. } => OpforgeTokenKind::String,
        PortableTokenKind::Comma => OpforgeTokenKind::Comma,
        PortableTokenKind::Colon => OpforgeTokenKind::Colon,
        PortableTokenKind::Dollar => OpforgeTokenKind::Dollar,
        PortableTokenKind::Dot => OpforgeTokenKind::Dot,
        PortableTokenKind::Hash => OpforgeTokenKind::Hash,
        PortableTokenKind::Question => OpforgeTokenKind::Question,
        PortableTokenKind::OpenBracket => OpforgeTokenKind::OpenBracket,
        PortableTokenKind::CloseBracket => OpforgeTokenKind::CloseBracket,
        PortableTokenKind::OpenBrace => OpforgeTokenKind::OpenBrace,
        PortableTokenKind::CloseBrace => OpforgeTokenKind::CloseBrace,
        PortableTokenKind::OpenParen => OpforgeTokenKind::OpenParen,
        PortableTokenKind::CloseParen => OpforgeTokenKind::CloseParen,
        PortableTokenKind::Operator(_) => OpforgeTokenKind::Operator,
    }
}

pub(crate) fn token_text(token: &PortableToken) -> String {
    match &token.kind {
        PortableTokenKind::Identifier(name) | PortableTokenKind::Register(name) => name.clone(),
        PortableTokenKind::Number { text, .. } => text.clone(),
        PortableTokenKind::String { raw, .. } => raw.clone(),
        PortableTokenKind::Comma => ",".to_string(),
        PortableTokenKind::Colon => ":".to_string(),
        PortableTokenKind::Dollar => "$".to_string(),
        PortableTokenKind::Dot => ".".to_string(),
        PortableTokenKind::Hash => "#".to_string(),
        PortableTokenKind::Question => "?".to_string(),
        PortableTokenKind::OpenBracket => "[".to_string(),
        PortableTokenKind::CloseBracket => "]".to_string(),
        PortableTokenKind::OpenBrace => "{".to_string(),
        PortableTokenKind::CloseBrace => "}".to_string(),
        PortableTokenKind::OpenParen => "(".to_string(),
        PortableTokenKind::CloseParen => ")".to_string(),
        PortableTokenKind::Operator(op) => operator_text(*op).to_string(),
    }
}

pub(crate) fn expr_span(expr: &PortableAstExpr) -> PortableSpan {
    match expr {
        PortableAstExpr::Number(_, span)
        | PortableAstExpr::Identifier(_, span)
        | PortableAstExpr::Register(_, span)
        | PortableAstExpr::List(_, span)
        | PortableAstExpr::Index { span, .. }
        | PortableAstExpr::Member { span, .. }
        | PortableAstExpr::StructLiteral { span, .. }
        | PortableAstExpr::Call { span, .. }
        | PortableAstExpr::Placeholder(span)
        | PortableAstExpr::Indirect(_, span)
        | PortableAstExpr::Dollar(span)
        | PortableAstExpr::String(_, span)
        | PortableAstExpr::Immediate(_, span)
        | PortableAstExpr::IndirectLong(_, span)
        | PortableAstExpr::Tuple(_, span)
        | PortableAstExpr::Error(_, span)
        | PortableAstExpr::Ternary { span, .. }
        | PortableAstExpr::Unary { span, .. }
        | PortableAstExpr::Binary { span, .. }
        | PortableAstExpr::Range { span, .. } => *span,
    }
}

pub(crate) fn expr_node_kind(expr: &PortableAstExpr) -> OpforgeExprNodeKind {
    match expr {
        PortableAstExpr::Number(_, _) => OpforgeExprNodeKind::Number,
        PortableAstExpr::Identifier(_, _) => OpforgeExprNodeKind::Identifier,
        PortableAstExpr::Register(_, _) => OpforgeExprNodeKind::Register,
        PortableAstExpr::List(_, _) => OpforgeExprNodeKind::List,
        PortableAstExpr::Index { .. } => OpforgeExprNodeKind::Index,
        PortableAstExpr::Member { .. } => OpforgeExprNodeKind::Member,
        PortableAstExpr::StructLiteral { .. } => OpforgeExprNodeKind::StructLiteral,
        PortableAstExpr::Call { .. } => OpforgeExprNodeKind::Call,
        PortableAstExpr::Placeholder(_) => OpforgeExprNodeKind::Placeholder,
        PortableAstExpr::Indirect(_, _) => OpforgeExprNodeKind::Indirect,
        PortableAstExpr::Dollar(_) => OpforgeExprNodeKind::Dollar,
        PortableAstExpr::String(_, _) => OpforgeExprNodeKind::String,
        PortableAstExpr::Immediate(_, _) => OpforgeExprNodeKind::Immediate,
        PortableAstExpr::IndirectLong(_, _) => OpforgeExprNodeKind::IndirectLong,
        PortableAstExpr::Tuple(_, _) => OpforgeExprNodeKind::Tuple,
        PortableAstExpr::Error(_, _) => OpforgeExprNodeKind::Error,
        PortableAstExpr::Ternary { .. } => OpforgeExprNodeKind::Ternary,
        PortableAstExpr::Unary { .. } => OpforgeExprNodeKind::Unary,
        PortableAstExpr::Binary { .. } => OpforgeExprNodeKind::Binary,
        PortableAstExpr::Range { .. } => OpforgeExprNodeKind::Range,
    }
}

pub(crate) fn expr_node_text(expr: &PortableAstExpr) -> Option<String> {
    match expr {
        PortableAstExpr::Number(text, _) => Some(text.clone()),
        PortableAstExpr::Identifier(name, _) => Some(name.clone()),
        PortableAstExpr::Register(name, _) => Some(name.clone()),
        PortableAstExpr::Member { field, .. } => Some(field.clone()),
        PortableAstExpr::StructLiteral { type_name, .. } => Some(type_name.clone()),
        PortableAstExpr::Call { name, .. } => Some(name.clone()),
        PortableAstExpr::String(bytes, _) => Some(String::from_utf8_lossy(bytes).to_string()),
        PortableAstExpr::Error(message, _) => Some(message.clone()),
        PortableAstExpr::Unary { op, .. } => Some(format!("{op:?}")),
        PortableAstExpr::Binary { op, .. } => Some(format!("{op:?}")),
        PortableAstExpr::Range { inclusive, .. } => {
            Some(if *inclusive { "..=" } else { ".." }.to_string())
        }
        PortableAstExpr::List(_, _)
        | PortableAstExpr::Index { .. }
        | PortableAstExpr::Placeholder(_)
        | PortableAstExpr::Indirect(_, _)
        | PortableAstExpr::Dollar(_)
        | PortableAstExpr::Immediate(_, _)
        | PortableAstExpr::IndirectLong(_, _)
        | PortableAstExpr::Tuple(_, _)
        | PortableAstExpr::Ternary { .. } => None,
    }
}

pub(crate) fn expr_display_text(expr: &PortableAstExpr) -> String {
    match expr {
        PortableAstExpr::Number(text, _) => text.clone(),
        PortableAstExpr::Identifier(name, _) => name.clone(),
        PortableAstExpr::Register(name, _) => name.clone(),
        PortableAstExpr::List(items, _) => format!(
            "{{{}}}",
            items
                .iter()
                .map(expr_display_text)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        PortableAstExpr::Index { base, index, .. } => {
            format!("{}[{}]", expr_display_text(base), expr_display_text(index))
        }
        PortableAstExpr::Member { base, field, .. } => {
            format!("{}.{}", expr_display_text(base), field)
        }
        PortableAstExpr::StructLiteral {
            type_name, fields, ..
        } => format!(
            "{}{{{}}}",
            type_name,
            fields
                .iter()
                .map(|(name, value)| format!("{name}:{}", expr_display_text(value)))
                .collect::<Vec<_>>()
                .join(",")
        ),
        PortableAstExpr::Call { name, args, .. } => format!(
            "{}({})",
            name,
            args.iter()
                .map(expr_display_text)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        PortableAstExpr::Placeholder(_) => "?".to_string(),
        PortableAstExpr::Indirect(inner, _) => format!("({})", expr_display_text(inner)),
        PortableAstExpr::Dollar(_) => "$".to_string(),
        PortableAstExpr::String(bytes, _) => String::from_utf8_lossy(bytes).to_string(),
        PortableAstExpr::Immediate(inner, _) => format!("#{}", expr_display_text(inner)),
        PortableAstExpr::IndirectLong(inner, _) => format!("[{}]", expr_display_text(inner)),
        PortableAstExpr::Tuple(items, _) => format!(
            "({})",
            items
                .iter()
                .map(expr_display_text)
                .collect::<Vec<_>>()
                .join(", ")
        ),
        PortableAstExpr::Error(message, _) => message.clone(),
        PortableAstExpr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => format!(
            "{} ? {} : {}",
            expr_display_text(cond),
            expr_display_text(then_expr),
            expr_display_text(else_expr)
        ),
        PortableAstExpr::Unary { op, expr, .. } => format!("{op:?} {}", expr_display_text(expr)),
        PortableAstExpr::Binary {
            left, op, right, ..
        } => format!(
            "{} {:?} {}",
            expr_display_text(left),
            op,
            expr_display_text(right)
        ),
        PortableAstExpr::Range {
            start,
            end,
            step,
            inclusive,
            ..
        } => {
            let mut text = format!(
                "{}{}{}",
                expr_display_text(start),
                if *inclusive { "..=" } else { ".." },
                expr_display_text(end)
            );
            if let Some(step) = step {
                text.push(':');
                text.push_str(&expr_display_text(step));
            }
            text
        }
    }
}

pub(crate) fn visit_expr_children(
    expr: &PortableAstExpr,
    mut visit: impl FnMut(Option<&str>, &PortableAstExpr),
) {
    match expr {
        PortableAstExpr::List(items, _) | PortableAstExpr::Tuple(items, _) => {
            for item in items {
                visit(None, item);
            }
        }
        PortableAstExpr::Index { base, index, .. } => {
            visit(None, base);
            visit(None, index);
        }
        PortableAstExpr::Member { base, .. }
        | PortableAstExpr::Indirect(base, _)
        | PortableAstExpr::Immediate(base, _)
        | PortableAstExpr::IndirectLong(base, _)
        | PortableAstExpr::Unary { expr: base, .. } => visit(None, base),
        PortableAstExpr::StructLiteral { fields, .. } => {
            for (field_name, value) in fields {
                visit(Some(field_name.as_str()), value);
            }
        }
        PortableAstExpr::Call { args, .. } => {
            for arg in args {
                visit(None, arg);
            }
        }
        PortableAstExpr::Ternary {
            cond,
            then_expr,
            else_expr,
            ..
        } => {
            visit(None, cond);
            visit(None, then_expr);
            visit(None, else_expr);
        }
        PortableAstExpr::Binary { left, right, .. } => {
            visit(None, left);
            visit(None, right);
        }
        PortableAstExpr::Range {
            start, end, step, ..
        } => {
            visit(None, start);
            visit(None, end);
            if let Some(step) = step {
                visit(None, step);
            }
        }
        PortableAstExpr::Number(_, _)
        | PortableAstExpr::Identifier(_, _)
        | PortableAstExpr::Register(_, _)
        | PortableAstExpr::Placeholder(_)
        | PortableAstExpr::Dollar(_)
        | PortableAstExpr::String(_, _)
        | PortableAstExpr::Error(_, _) => {}
    }
}

fn operator_text(op: PortableOperatorKind) -> &'static str {
    match op {
        PortableOperatorKind::Range => "..",
        PortableOperatorKind::RangeInclusive => "..=",
        PortableOperatorKind::Plus => "+",
        PortableOperatorKind::Minus => "-",
        PortableOperatorKind::Multiply => "*",
        PortableOperatorKind::Power => "^",
        PortableOperatorKind::Divide => "/",
        PortableOperatorKind::Mod => "%",
        PortableOperatorKind::Shl => "<<",
        PortableOperatorKind::Shr => ">>",
        PortableOperatorKind::BitNot => "~",
        PortableOperatorKind::LogicNot => "!",
        PortableOperatorKind::BitAnd => "&",
        PortableOperatorKind::BitOr => "|",
        PortableOperatorKind::BitXor => "^",
        PortableOperatorKind::LogicAnd => "&&",
        PortableOperatorKind::LogicOr => "||",
        PortableOperatorKind::LogicXor => "^^",
        PortableOperatorKind::Eq => "==",
        PortableOperatorKind::Ne => "!=",
        PortableOperatorKind::Ge => ">=",
        PortableOperatorKind::Gt => ">",
        PortableOperatorKind::Le => "<=",
        PortableOperatorKind::Lt => "<",
    }
}
