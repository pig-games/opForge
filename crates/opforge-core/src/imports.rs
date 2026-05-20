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
pub fn implicit_qualifier(module_id: &str) -> Option<String> {
    module_id
        .rsplit('.')
        .find(|segment| !segment.is_empty())
        .map(str::to_string)
}

#[must_use]
pub fn module_import_from_parser(
    module_id: String,
    alias: Option<String>,
    items: Vec<UseItem>,
    params: Vec<UseParam>,
    span: Span,
) -> ModuleImport {
    let parsed_items: Vec<ImportItem> = items.into_iter().map(import_item_from_use_item).collect();
    let has_selection = !parsed_items.is_empty();
    let selected_roots: Vec<ImportItem> = parsed_items
        .iter()
        .filter(|item| item.name != "*" || item.alias.is_some())
        .cloned()
        .collect();
    let qualifier = alias.clone().or_else(|| {
        (!has_selection)
            .then(|| implicit_qualifier(&module_id))
            .flatten()
    });
    let direct_items = if alias.is_some() {
        Vec::new()
    } else {
        parsed_items
    };
    ModuleImport {
        module_id,
        alias,
        qualifier,
        items: direct_items,
        selected_roots,
        params: params
            .into_iter()
            .map(import_param_from_use_param)
            .collect(),
        span: span_to_source_span(span),
    }
}

#[cfg(test)]
mod tests {
    use super::{implicit_qualifier, module_import_from_parser};
    use crate::parser::UseItem;
    use crate::tokenizer::Span;

    fn span() -> Span {
        Span {
            line: 1,
            col_start: 1,
            col_end: 1,
        }
    }

    #[test]
    fn bare_import_uses_final_segment_as_implicit_qualifier() {
        let import = module_import_from_parser(
            "opasm.amigaos.engine".to_string(),
            None,
            Vec::new(),
            Vec::new(),
            span(),
        );

        assert_eq!(import.alias, None);
        assert_eq!(import.qualifier.as_deref(), Some("engine"));
    }

    #[test]
    fn explicit_alias_is_the_only_qualifier() {
        let import = module_import_from_parser(
            "opasm.amigaos.engine".to_string(),
            Some("eng".to_string()),
            Vec::new(),
            Vec::new(),
            span(),
        );

        assert_eq!(import.alias.as_deref(), Some("eng"));
        assert_eq!(import.qualifier.as_deref(), Some("eng"));
    }

    #[test]
    fn direct_selective_import_preserves_unqualified_metadata() {
        let item_span = span();
        let import = module_import_from_parser(
            "opasm.amigaos.engine".to_string(),
            None,
            vec![UseItem {
                name: "sessionPass".to_string(),
                alias: None,
                span: item_span,
            }],
            Vec::new(),
            span(),
        );

        assert_eq!(import.alias, None);
        assert_eq!(import.qualifier, None);
        assert_eq!(import.items[0].name, "sessionPass");
        assert_eq!(import.selected_roots[0].name, "sessionPass");
    }

    #[test]
    fn qualified_selective_import_records_root_without_direct_item() {
        let item_span = span();
        let import = module_import_from_parser(
            "opasm.amigaos.engine".to_string(),
            Some("engine".to_string()),
            vec![UseItem {
                name: "sessionPass".to_string(),
                alias: None,
                span: item_span,
            }],
            Vec::new(),
            span(),
        );

        assert_eq!(import.alias.as_deref(), Some("engine"));
        assert_eq!(import.qualifier.as_deref(), Some("engine"));
        assert!(import.items.is_empty());
        assert_eq!(import.selected_roots[0].name, "sessionPass");
    }

    #[test]
    fn implicit_qualifier_ignores_empty_path_segments() {
        assert_eq!(
            implicit_qualifier("opasm.amigaos.engine").as_deref(),
            Some("engine")
        );
        assert_eq!(implicit_qualifier("engine").as_deref(), Some("engine"));
        assert_eq!(implicit_qualifier("engine.").as_deref(), Some("engine"));
    }
}
