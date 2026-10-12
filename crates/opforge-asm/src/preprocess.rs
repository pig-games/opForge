// SPDX-License-Identifier: GPL-3.0-or-later

use opcore::macro_processor::{
    substitute_macro_line_with_named_args, CompileTimeVisibility, MacroError, MacroExports,
    MacroProcessor, NativeStatementExport, StatementSupportHooks,
};
use opcore::parser::{StatementCaptureText, StatementSignature};
use opcore::text_utils::to_upper;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use types::processing::{LineProcessingTrace, ProcessingRequestKind};
use vm::packed_statement_vm::parse_statement_definition_from_line;

pub type AsmMacroError = MacroError;

#[derive(Debug, Clone, Default)]
pub struct AsmMacroExports {
    core: MacroExports,
    statements: Vec<NativeStatementExport>,
}

impl AsmMacroExports {
    pub fn names(&self) -> HashSet<String> {
        let mut names = self.core.names();
        names.extend(self.statements.iter().map(|def| def.keyword().to_string()));
        names
    }

    pub fn visibility_index(&self) -> HashMap<String, CompileTimeVisibility> {
        let mut vis = self.core.visibility_index();
        for def in &self.statements {
            vis.entry(def.keyword().to_string())
                .and_modify(|entry| {
                    if matches!(def.visibility(), CompileTimeVisibility::Public) {
                        *entry = CompileTimeVisibility::Public;
                    }
                })
                .or_insert(def.visibility());
        }
        vis
    }
}

#[derive(Debug, Clone)]
pub struct AsmMacroProcessor {
    inner: MacroProcessor,
    runtime_processing_traces: Vec<(u8, u32, LineProcessingTrace)>,
}

thread_local! {
    static PREPROCESS_RUNTIME_PROCESSING_TRACES: RefCell<Vec<(u8, u32, LineProcessingTrace)>> =
        const { RefCell::new(Vec::new()) };
}

impl AsmMacroProcessor {
    pub(crate) fn for_source_execution(max_depth: usize) -> Self {
        let mut processor = Self::new(max_depth);
        processor.inner = processor.inner.with_runtime_streaming();
        processor
    }
    pub fn source_execution_seed(&self) -> MacroProcessor {
        self.inner.clone()
    }
    pub(crate) fn seed_templates_from(&mut self, seed: &MacroProcessor) {
        self.inner.seed_templates_from(seed);
    }
    pub fn new(max_depth: usize) -> Self {
        Self {
            inner: MacroProcessor::new()
                .with_max_depth(max_depth)
                .with_statement_support_hooks(StatementSupportHooks {
                    classify_directive: vm::packed_statement_vm::classify_directive,
                    parse_definition: asm_parse_statement_definition,
                    expand_invocation: asm_expand_statement_invocation,
                }),
            runtime_processing_traces: Vec::new(),
        }
    }

    pub fn expand(&mut self, lines: &[String]) -> Result<Vec<String>, AsmMacroError> {
        clear_preprocess_runtime_processing_traces();
        let result = self.inner.expand(lines);
        self.runtime_processing_traces
            .extend(take_preprocess_runtime_processing_traces());
        result
    }

    pub fn expand_with_origins(
        &mut self,
        lines: &[String],
        origins: &[types::source_map::SourceOrigin],
    ) -> Result<(Vec<String>, Vec<types::source_map::SourceOrigin>), AsmMacroError> {
        clear_preprocess_runtime_processing_traces();
        let result = self.inner.expand_with_origins(lines, origins);
        self.runtime_processing_traces
            .extend(take_preprocess_runtime_processing_traces());
        result
    }

    pub fn inject_from(
        &mut self,
        exports: &AsmMacroExports,
        names: &[String],
        imported_names: &[String],
    ) {
        self.inner.inject_from(&exports.core, names, imported_names);
        self.inner
            .inject_statement_defs_from(&exports.statements, names, imported_names);
    }

    pub fn inject_all(&mut self, exports: &AsmMacroExports) {
        self.inner.inject_all(&exports.core);
        self.inner.inject_all_statement_defs(&exports.statements);
    }

    pub fn inject_qualified(&mut self, exports: &AsmMacroExports, qualifier: &str) {
        self.inner.inject_qualified(&exports.core, qualifier);
        self.inner
            .inject_qualified_statement_defs(&exports.statements, qualifier);
    }

    pub fn take_native_exports(&mut self) -> AsmMacroExports {
        AsmMacroExports {
            core: self.inner.take_native_exports(),
            statements: self.inner.take_native_statement_defs(),
        }
    }

    pub fn take_runtime_processing_traces(&mut self) -> Vec<(u8, u32, LineProcessingTrace)> {
        std::mem::take(&mut self.runtime_processing_traces)
    }
}

fn clear_preprocess_runtime_processing_traces() {
    PREPROCESS_RUNTIME_PROCESSING_TRACES.with(|traces| traces.borrow_mut().clear());
}

fn take_preprocess_runtime_processing_traces() -> Vec<(u8, u32, LineProcessingTrace)> {
    PREPROCESS_RUNTIME_PROCESSING_TRACES.with(|traces| std::mem::take(&mut *traces.borrow_mut()))
}

fn record_preprocess_runtime_trace(line_num: u32, kind: &str) {
    let mut trace = LineProcessingTrace::default();
    trace.push(ProcessingRequestKind::Processor {
        processor: "asm".to_string(),
        kind: kind.to_string(),
    });
    PREPROCESS_RUNTIME_PROCESSING_TRACES
        .with(|traces| traces.borrow_mut().push((0, line_num, trace)));
}

fn asm_parse_statement_definition(
    code: &str,
    line_num: u32,
) -> Result<(String, StatementSignature), MacroError> {
    record_preprocess_runtime_trace(line_num, "preprocess.statement_definition");
    let def = parse_statement_definition_from_line(code, line_num)
        .map_err(|err| MacroError::new(err.message, Some(line_num), Some(err.span.col_start)))?;
    Ok((def.keyword, def.signature))
}

fn asm_expand_statement_invocation(
    code: &str,
    line_num: u32,
    depth: usize,
    processor: &mut MacroProcessor,
) -> Result<Option<Vec<String>>, AsmMacroError> {
    record_preprocess_runtime_trace(line_num, "preprocess.statement_invocation");
    let defs = processor.statement_defs();
    if defs.is_empty() {
        return Ok(None);
    }

    let Some(invocation) = vm::statement_invocation_source::select_source(
        &package::statement_program::match_program(),
        code,
        line_num,
        &defs,
    )
    .map_err(|err| MacroError::new(err.message, Some(line_num), Some(err.span.col_start)))?
    else {
        return Ok(None);
    };
    let definition = &defs[invocation.definition];
    let captures = invocation.captures;
    let label = invocation.label;
    let (positional, named, full_list) = build_statement_args(&captures);
    let mut expanded = Vec::new();
    for line in definition.body() {
        expanded.push(substitute_macro_line_with_named_args(
            line,
            &positional,
            &named,
            &full_list,
        ));
    }

    if let Some(label) = &label {
        attach_label_to_expansion(label, &mut expanded);
    }

    let nested = processor.expand_nested_statement_lines_with_origins(
        &expanded,
        definition.body_origins(),
        depth + 1,
    )?;
    Ok(Some(nested))
}

fn build_statement_args(
    captures: &[StatementCaptureText],
) -> (Vec<String>, HashMap<String, String>, String) {
    let mut positional = Vec::new();
    let mut named = HashMap::new();
    for capture in captures {
        positional.push(capture.text.clone());
        named.insert(to_upper(&capture.name), capture.text.clone());
    }
    let full_list = positional.join(", ");
    (positional, named, full_list)
}

fn attach_label_to_expansion(label: &str, expanded: &mut Vec<String>) {
    if let Some(first) = expanded.first_mut() {
        let trimmed = first.trim_start();
        if trimmed.is_empty() {
            *first = label.to_string();
        } else {
            *first = format!("{label} {trimmed}");
        }
    } else {
        expanded.push(label.to_string());
    }
}

#[cfg(test)]
#[path = "preprocess_statement_invocation_tests.rs"]
mod statement_invocation_tests;

#[cfg(test)]
mod tests {
    use opcore::macro_processor::CompileTimeVisibility;

    use super::AsmMacroProcessor;

    #[test]
    fn asm_macro_processor_expands_statement_support() {
        let mut processor = AsmMacroProcessor::new(64);
        let lines = vec![
            ".statement LOAD byte:val".to_string(),
            "    .byte .val".to_string(),
            ".endstatement".to_string(),
            "    LOAD 7".to_string(),
        ];

        let out = processor.expand(&lines).expect("expand");
        assert!(out.contains(&"    .byte 7".to_string()));
    }

    #[test]
    fn asm_macro_exports_preserve_statement_defs_for_import() {
        let mut producer = AsmMacroProcessor::new(64);
        let lines = vec![
            ".pub".to_string(),
            ".statement LOAD byte:val".to_string(),
            "    .byte .val".to_string(),
            ".endstatement".to_string(),
        ];
        let _ = producer.expand(&lines).expect("expand");
        let exports = producer.take_native_exports();

        let mut consumer = AsmMacroProcessor::new(64);
        consumer.inject_all(&exports);
        let out = consumer
            .expand(&["    LOAD 7".to_string()])
            .expect("expand");

        assert!(out.contains(&"    .byte 7".to_string()));
        assert_eq!(
            exports.visibility_index().get("LOAD"),
            Some(&CompileTimeVisibility::Public)
        );
    }

    #[test]
    fn asm_macro_processor_expands_statement_with_boundary_signature() {
        let mut processor = AsmMacroProcessor::new(64);
        let lines = vec![
            ".statement lda \"[\"[{byte:val}]\"],y\"".to_string(),
            "    .byte .val".to_string(),
            ".endstatement".to_string(),
            "    lda [$05],y".to_string(),
        ];

        let out = processor.expand(&lines).expect("expand");
        assert!(out.contains(&"    .byte $05".to_string()));
    }

    #[test]
    fn asm_macro_processor_expands_dotted_statement_with_split_register_digit_args() {
        let mut processor = AsmMacroProcessor::new(64);
        let lines = vec![
            ".statement move.b char:dst[{byte:dstnum}] \",\" char:src[{byte:srcnum}]".to_string(),
            "    .byte 'b'".to_string(),
            "    .byte '.dst', .dstnum".to_string(),
            "    .byte '.src', .srcnum".to_string(),
            ".endstatement".to_string(),
            "    move.b d0,d2".to_string(),
        ];

        let out = processor.expand(&lines).expect("expand");
        assert!(out.contains(&"    .byte 'b'".to_string()));
        assert!(out.contains(&"    .byte 'd', 0".to_string()));
        assert!(out.contains(&"    .byte 'd', 2".to_string()));
    }

    #[test]
    fn prvm_statement_production_definition_labels_reject_and_end_labels_close() {
        for definition in ["label .statement N byte:x", "label: .statement N byte:x"] {
            let error = AsmMacroProcessor::new(64)
                .expand(&[definition.into(), ".endstatement".into()])
                .unwrap_err();
            assert_eq!(error.message(), "Expected .statement definition");
        }
        for end in [
            ".endstatement",
            "label .endstatement",
            "label: .endstatement",
            ".endstatement extra",
        ] {
            let out = AsmMacroProcessor::new(64)
                .expand(&[
                    ".statement N byte:x".into(),
                    ".byte .x".into(),
                    end.into(),
                    "    N 7".into(),
                ])
                .unwrap();
            assert_eq!(out, vec![".byte 7"]);
        }
    }

    #[test]
    fn prvm_statement_expansion_retains_label_definition_origin_and_export_plan() {
        use types::source_map::SourceOrigin;
        let mut producer = AsmMacroProcessor::new(64);
        let lines: Vec<String> = [".pub", ".statement N byte:x", ".byte .x", ".endstatement"]
            .into_iter()
            .map(str::to_owned)
            .collect();
        let origins: Vec<_> = (1..=4)
            .map(|line| SourceOrigin::new(Some("defs/library.asm".into()), line))
            .collect();
        producer.expand_with_origins(&lines, &origins).unwrap();
        let exports = producer.take_native_exports();
        let retained = exports.statements[0]
            .signature()
            .prepared_plan()
            .unwrap()
            .clone();
        let mut consumer = AsmMacroProcessor::new(64);
        consumer.inject_all(&exports);
        let imported = consumer.inner.statement_defs();
        assert!(std::sync::Arc::ptr_eq(
            &retained,
            imported[0].signature().prepared_plan().unwrap()
        ));
        let (out, out_origins) = consumer
            .expand_with_origins(
                &["here: N $ff".into()],
                &[SourceOrigin::new(Some("app/main.asm".into()), 9)],
            )
            .unwrap();
        assert_eq!(out, vec!["here .byte $ff"]);
        assert_eq!(out_origins, vec![origins[2].clone()]);
    }

    #[test]
    fn prvm_statement_empty_body_label_uses_invocation_origin() {
        use types::source_map::SourceOrigin;
        let mut processor = AsmMacroProcessor::new(64);
        let definition = vec![".statement EMPTY".into(), ".endstatement".into()];
        processor
            .expand_with_origins(
                &definition,
                &[
                    SourceOrigin::new(Some("defs/library.asm".into()), 1),
                    SourceOrigin::new(Some("defs/library.asm".into()), 2),
                ],
            )
            .unwrap();
        let caller = SourceOrigin::new(Some("app/main.asm".into()), 9);
        let (lines, origins) = processor
            .expand_with_origins(&["here: EMPTY".into()], std::slice::from_ref(&caller))
            .unwrap();
        assert_eq!(lines, vec!["here"]);
        assert_eq!(origins, vec![caller]);
    }
}
