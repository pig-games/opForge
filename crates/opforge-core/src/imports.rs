// SPDX-License-Identifier: GPL-3.0-or-later

//! Bridges parser-owned `.use` syntax into shared symbol import types.

use crate::parser::{UseItem, UseParam};
use crate::tokenizer::Span;
use types::symbol::{ImportItem, ImportParam, ModuleImport, SourceSpan};

#[must_use]
pub fn span_to_source_span(span: Span) -> SourceSpan {
    SourceSpan {
        line: span.line,
        col_start: span.col_start,
        col_end: span.col_end,
    }
}

#[must_use]
pub fn import_item_from_use_item(item: UseItem) -> ImportItem {
    ImportItem {
        name: item.name,
        alias: item.alias,
        span: span_to_source_span(item.span),
    }
}

#[must_use]
pub fn import_param_from_use_param(param: UseParam) -> ImportParam {
    ImportParam {
        name: param.name,
        value_repr: format!("{:?}", param.value),
        span: span_to_source_span(param.span),
    }
}

#[must_use]
pub fn module_import_from_parser(
    module_id: String,
    alias: Option<String>,
    items: Vec<UseItem>,
    params: Vec<UseParam>,
    span: Span,
) -> ModuleImport {
    ModuleImport {
        module_id,
        alias,
        items: items.into_iter().map(import_item_from_use_item).collect(),
        params: params
            .into_iter()
            .map(import_param_from_use_param)
            .collect(),
        span: span_to_source_span(span),
    }
}
