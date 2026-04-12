use crate::{OpforgeExprNodeKind, OpforgeTokenKind};

use api::opcore::portable::{
    PortableAstExpr, PortableOperatorKind, PortableSpan, PortableToken, PortableTokenKind,
};

struct TokenProjection<'a> {
    kind: OpforgeTokenKind,
    text: TokenText<'a>,
}

enum TokenText<'a> {
    Borrowed(&'a str),
    Static(&'static str),
    Operator(PortableOperatorKind),
}

impl TokenProjection<'_> {
    fn owned_text(&self) -> String {
        match &self.text {
            TokenText::Borrowed(text) => (*text).to_string(),
            TokenText::Static(text) => (*text).to_string(),
            TokenText::Operator(op) => operator_text(*op).to_string(),
        }
    }
}

pub(crate) struct ExprProjection<'a> {
    span: PortableSpan,
    kind: OpforgeExprNodeKind,
    text: ExprNodeText<'a>,
}

enum ExprNodeText<'a> {
    None,
    Borrowed(&'a str),
    Utf8Lossy(&'a [u8]),
    Owned(String),
}

impl ExprProjection<'_> {
    pub(crate) fn span(&self) -> PortableSpan {
        self.span
    }

    pub(crate) fn kind(&self) -> OpforgeExprNodeKind {
        self.kind
    }

    pub(crate) fn owned_text(&self) -> Option<String> {
        self.text.owned_text()
    }
}

impl ExprNodeText<'_> {
    fn owned_text(&self) -> Option<String> {
        match self {
            ExprNodeText::None => None,
            ExprNodeText::Borrowed(text) => Some((*text).to_string()),
            ExprNodeText::Utf8Lossy(bytes) => Some(String::from_utf8_lossy(bytes).to_string()),
            ExprNodeText::Owned(text) => Some(text.clone()),
        }
    }
}

fn project_token(kind: &PortableTokenKind) -> TokenProjection<'_> {
    match kind {
        PortableTokenKind::Identifier(name) => TokenProjection {
            kind: OpforgeTokenKind::Identifier,
            text: TokenText::Borrowed(name.as_str()),
        },
        PortableTokenKind::Register(name) => TokenProjection {
            kind: OpforgeTokenKind::Register,
            text: TokenText::Borrowed(name.as_str()),
        },
        PortableTokenKind::Number { text, .. } => TokenProjection {
            kind: OpforgeTokenKind::Number,
            text: TokenText::Borrowed(text.as_str()),
        },
        PortableTokenKind::String { raw, .. } => TokenProjection {
            kind: OpforgeTokenKind::String,
            text: TokenText::Borrowed(raw.as_str()),
        },
        PortableTokenKind::Comma => TokenProjection {
            kind: OpforgeTokenKind::Comma,
            text: TokenText::Static(","),
        },
        PortableTokenKind::Colon => TokenProjection {
            kind: OpforgeTokenKind::Colon,
            text: TokenText::Static(":"),
        },
        PortableTokenKind::Dollar => TokenProjection {
            kind: OpforgeTokenKind::Dollar,
            text: TokenText::Static("$"),
        },
        PortableTokenKind::Dot => TokenProjection {
            kind: OpforgeTokenKind::Dot,
            text: TokenText::Static("."),
        },
        PortableTokenKind::Hash => TokenProjection {
            kind: OpforgeTokenKind::Hash,
            text: TokenText::Static("#"),
        },
        PortableTokenKind::Question => TokenProjection {
            kind: OpforgeTokenKind::Question,
            text: TokenText::Static("?"),
        },
        PortableTokenKind::OpenBracket => TokenProjection {
            kind: OpforgeTokenKind::OpenBracket,
            text: TokenText::Static("["),
        },
        PortableTokenKind::CloseBracket => TokenProjection {
            kind: OpforgeTokenKind::CloseBracket,
            text: TokenText::Static("]"),
        },
        PortableTokenKind::OpenBrace => TokenProjection {
            kind: OpforgeTokenKind::OpenBrace,
            text: TokenText::Static("{"),
        },
        PortableTokenKind::CloseBrace => TokenProjection {
            kind: OpforgeTokenKind::CloseBrace,
            text: TokenText::Static("}"),
        },
        PortableTokenKind::OpenParen => TokenProjection {
            kind: OpforgeTokenKind::OpenParen,
            text: TokenText::Static("("),
        },
        PortableTokenKind::CloseParen => TokenProjection {
            kind: OpforgeTokenKind::CloseParen,
            text: TokenText::Static(")"),
        },
        PortableTokenKind::Operator(op) => TokenProjection {
            kind: OpforgeTokenKind::Operator,
            text: TokenText::Operator(*op),
        },
    }
}

pub(crate) fn expr_projection(expr: &PortableAstExpr) -> ExprProjection<'_> {
    match expr {
        PortableAstExpr::Number(text, span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Number,
            text: ExprNodeText::Borrowed(text.as_str()),
        },
        PortableAstExpr::Identifier(name, span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Identifier,
            text: ExprNodeText::Borrowed(name.as_str()),
        },
        PortableAstExpr::Register(name, span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Register,
            text: ExprNodeText::Borrowed(name.as_str()),
        },
        PortableAstExpr::List(_, span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::List,
            text: ExprNodeText::None,
        },
        PortableAstExpr::Index { span, .. } => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Index,
            text: ExprNodeText::None,
        },
        PortableAstExpr::Member { field, span, .. } => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Member,
            text: ExprNodeText::Borrowed(field.as_str()),
        },
        PortableAstExpr::StructLiteral {
            type_name, span, ..
        } => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::StructLiteral,
            text: ExprNodeText::Borrowed(type_name.as_str()),
        },
        PortableAstExpr::Call { name, span, .. } => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Call,
            text: ExprNodeText::Borrowed(name.as_str()),
        },
        PortableAstExpr::Placeholder(span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Placeholder,
            text: ExprNodeText::None,
        },
        PortableAstExpr::Indirect(_, span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Indirect,
            text: ExprNodeText::None,
        },
        PortableAstExpr::Dollar(span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Dollar,
            text: ExprNodeText::None,
        },
        PortableAstExpr::String(bytes, span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::String,
            text: ExprNodeText::Utf8Lossy(bytes.as_slice()),
        },
        PortableAstExpr::Immediate(_, span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Immediate,
            text: ExprNodeText::None,
        },
        PortableAstExpr::IndirectLong(_, span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::IndirectLong,
            text: ExprNodeText::None,
        },
        PortableAstExpr::Tuple(_, span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Tuple,
            text: ExprNodeText::None,
        },
        PortableAstExpr::Error(message, span) => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Error,
            text: ExprNodeText::Borrowed(message.as_str()),
        },
        PortableAstExpr::Ternary { span, .. } => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Ternary,
            text: ExprNodeText::None,
        },
        PortableAstExpr::Unary { op, span, .. } => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Unary,
            text: ExprNodeText::Owned(format!("{op:?}")),
        },
        PortableAstExpr::Binary { op, span, .. } => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Binary,
            text: ExprNodeText::Owned(format!("{op:?}")),
        },
        PortableAstExpr::Range {
            inclusive, span, ..
        } => ExprProjection {
            span: *span,
            kind: OpforgeExprNodeKind::Range,
            text: ExprNodeText::Owned(if *inclusive { "..=" } else { ".." }.to_string()),
        },
    }
}

pub(crate) fn token_kind(kind: &PortableTokenKind) -> OpforgeTokenKind {
    project_token(kind).kind
}

pub(crate) fn token_text(token: &PortableToken) -> String {
    project_token(&token.kind).owned_text()
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
