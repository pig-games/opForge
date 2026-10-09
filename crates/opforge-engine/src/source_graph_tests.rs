// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use crate::expand_source_file_with_dependencies;
use crate::load_module_graph;
use crate::module_search_root_for_path as module_search_root;

mod tests {
    use super::{expand_source_file_with_dependencies, load_module_graph, module_search_root};
    use crate::route_module_item_line;
    use crate::OpcoreRequestKind;
    use crate::ProcessingRequestKind;
    use std::fs;
    use std::path::{Path, PathBuf};
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::{SystemTime, UNIX_EPOCH};
    use types::asm_value::AsmValue;

    fn temp_dir() -> PathBuf {
        static COUNTER: AtomicU64 = AtomicU64::new(0);
        let mut path = std::env::temp_dir();
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let unique = COUNTER.fetch_add(1, Ordering::Relaxed);
        path.push(format!("opforge-bootstrap-test-{nanos}-{unique}"));
        fs::create_dir_all(&path).unwrap();
        path
    }

    #[test]
    fn selected_template_aliases_expand_without_importing_original_names() {
        let project = temp_dir();
        let root = project.join("main.asm");
        fs::write(&root, ".module main\n.use dep (EMIT as renamed, INLINE as segment_alias, WORD as statement_alias, HIDDEN as hidden_alias)\n.use dep (EMIT, WORD) as q\n.q.EMIT 6\n    q.WORD 7\n.renamed 3\n.segment_alias 4\n    statement_alias 5\n.EMIT 9\n.INLINE 9\n    WORD 9\n.hidden_alias\n.endmodule\n").unwrap();
        fs::write(project.join("dep.asm"), ".module dep\n.pub\nEMIT .macro value\n.byte .value\n.endmacro\nINLINE .segment value\n.byte .value\n.ends\n.statement WORD byte:value\n.byte .value\n.endstatement\n.priv\nHIDDEN .macro\n.byte 99\n.endmacro\n.endmodule\n").unwrap();
        let root_lines = crate::expand_source_file(&root, &[], &[], 64).unwrap();
        let graph = load_module_graph(&root, root_lines, &[], &[], &[], 64).unwrap();
        for value in [3, 4, 5, 6, 7] {
            assert!(
                graph
                    .lines
                    .iter()
                    .any(|line| line.trim() == format!(".byte {value}")),
                "value {value}: {:?}",
                graph.lines
            );
        }
        for unimported in [".EMIT 9", ".INLINE 9", "WORD 9", ".hidden_alias"] {
            assert!(graph.lines.iter().any(|line| line.trim() == unimported));
        }
        let expanded = graph
            .lines
            .iter()
            .position(|line| line.trim() == ".byte 3")
            .unwrap();
        let origin = &graph.source_map.origins()[expanded];
        assert_eq!(
            origin.file.as_deref(),
            Some(project.join("dep.asm").to_str().unwrap())
        );
        assert_eq!(origin.line, 4);
        assert!(
            graph.module_macro_names["main"].is_empty(),
            "imported aliases must not become native exports"
        );
        fs::remove_dir_all(project).unwrap();
    }

    #[test]
    fn module_search_root_uses_current_dir_for_bare_filename() {
        assert_eq!(
            module_search_root(Path::new("main.asm")),
            PathBuf::from(".")
        );
    }

    #[test]
    fn module_search_root_preserves_explicit_parent() {
        assert_eq!(
            module_search_root(Path::new("examples/main.asm")),
            PathBuf::from("examples")
        );
    }

    #[test]
    fn expand_source_allows_parent_relative_include_within_root_tree() {
        let project = temp_dir();
        let src = project.join("src");
        let modules = src.join("modules");
        fs::create_dir_all(&modules).unwrap();
        let root = src.join("main.asm");
        let module = modules.join("mforth.base.asm");
        let shared = src.join("mforth.shared.inc");

        fs::write(&shared, "VALUE .const 7\n").unwrap();
        fs::write(&module, ".include \"../mforth.shared.inc\"\n.byte VALUE\n").unwrap();
        fs::write(&root, ".include \"modules/mforth.base.asm\"\n").unwrap();

        let (lines, deps) = expand_source_file_with_dependencies(&root, &[], &[], 64).unwrap();

        assert!(lines.iter().any(|line| line.contains("VALUE .const 7")));
        assert!(deps.iter().any(|p| p.ends_with("mforth.shared.inc")));
    }

    #[test]
    fn route_module_item_line_traces_module_request_kind() {
        let (ast, trace) = route_module_item_line(".module mforth.base", 1).unwrap();
        assert!(ast.is_some());
        assert_eq!(
            trace.requests(),
            &[ProcessingRequestKind::Opcore(OpcoreRequestKind::ModuleItem)]
        );
    }

    #[test]
    fn load_module_graph_resolves_mforth_style_use_directives() {
        let project = temp_dir();
        let src = project.join("src");
        let modules = src.join("modules");
        fs::create_dir_all(&modules).unwrap();

        let root = src.join("main.asm");
        fs::write(
            &root,
            ".meta\n    .name \"MFORTH\"\n.endmeta\n\n.cpu 8085\n.use mforth.base\n.use mforth.kernel (*)\n.use mforth.wordsets (*)\n\nmain: jmp main\n",
        )
        .unwrap();

        fs::write(
            modules.join("mforth.base.asm"),
            ".module mforth.base\nBASE_VALUE = 1\n.endmodule\n",
        )
        .unwrap();
        fs::write(
            modules.join("mforth.kernel.asm"),
            ".module mforth.kernel\nkernel_label: nop\n.endmodule\n",
        )
        .unwrap();
        fs::write(
            modules.join("mforth.wordsets.asm"),
            ".module mforth.wordsets\nlast_task: nop\nlast_assembler: nop\n.endmodule\n",
        )
        .unwrap();

        let (root_lines, root_deps) =
            expand_source_file_with_dependencies(&root, &[], &[], 64).unwrap();
        let graph = load_module_graph(
            &root,
            root_lines,
            &[],
            &[],
            std::slice::from_ref(&modules),
            64,
        )
        .expect("module graph should resolve all .use directives");

        let mut all_deps = root_deps;
        all_deps.extend(graph.dependency_files);
        let all_dep_strings: Vec<String> = all_deps
            .iter()
            .map(|path| path.to_string_lossy().to_string())
            .collect();

        assert!(
            all_dep_strings
                .iter()
                .any(|path| path.ends_with("mforth.base.asm")),
            "expected mforth.base module dependency in graph"
        );
        assert!(
            all_dep_strings
                .iter()
                .any(|path| path.ends_with("mforth.kernel.asm")),
            "expected mforth.kernel module dependency in graph"
        );
        assert!(
            all_dep_strings
                .iter()
                .any(|path| path.ends_with("mforth.wordsets.asm")),
            "expected mforth.wordsets module dependency in graph"
        );
    }

    #[test]
    fn load_module_graph_preserves_implicit_dependency_line_origins() {
        let project = temp_dir();
        let root = project.join("main.asm");
        let dependency = project.join("dep.asm");
        fs::write(
            &root,
            ".module main\n.use dep as d\n.byte d.value\n.endmodule\n",
        )
        .unwrap();
        fs::write(&dependency, ".cpu m6502\n.pub\nvalue = 7\n").unwrap();

        let root_lines = crate::expand_source_file(&root, &[], &[], 64).unwrap();
        let graph = load_module_graph(&root, root_lines, &[], &[], &[], 64).unwrap();
        let declaration = graph
            .lines
            .iter()
            .position(|line| line == "value = 7")
            .unwrap();
        assert_eq!(graph.lines[declaration - 3], ".module dep");
        assert_eq!(graph.lines[declaration + 1], ".endmodule");
        assert_eq!(graph.source_map.origins().len(), graph.lines.len());
        let origin = &graph.source_map.origins()[declaration];
        assert_eq!(origin.file.as_deref(), Some(dependency.to_str().unwrap()));
        assert_eq!(origin.line, 3);
        fs::remove_dir_all(project).unwrap();
    }

    #[test]
    fn load_module_graph_ignores_use_directives_in_inactive_conditionals() {
        let project = temp_dir();
        let src = project.join("src");
        fs::create_dir_all(&src).unwrap();

        let root = src.join("main.asm");
        fs::write(
            &root,
            ".module main\n.if 0\n.use missing.module\n.endif\nmain: nop\n.endmodule\n",
        )
        .unwrap();

        let (root_lines, _) = expand_source_file_with_dependencies(&root, &[], &[], 64).unwrap();
        let graph = load_module_graph(&root, root_lines, &[], &[], &[], 64)
            .expect("inactive conditional imports must not participate in bootstrap");

        assert!(
            graph.dependency_files.is_empty(),
            "inactive conditional import should not add dependency files"
        );
    }

    #[test]
    fn load_module_graph_uses_active_entry_declaration_after_inactive_duplicate() {
        let project = temp_dir();
        let root = project.join("main.asm");
        fs::write(
            &root,
            ".if 0\n.module main\n.byte 9\n.endmodule\n.endif\n.module main\n.byte 1\n.endmodule\n",
        )
        .unwrap();

        let (root_lines, _) = expand_source_file_with_dependencies(&root, &[], &[], 64).unwrap();
        let graph = load_module_graph(&root, root_lines, &[], &[], &[], 64).unwrap();
        let active_start = graph
            .lines
            .iter()
            .rposition(|line| line == ".module main")
            .unwrap();
        assert_eq!(graph.lines[active_start + 1], ".byte 1");
        fs::remove_dir_all(project).unwrap();
    }

    #[test]
    fn load_module_graph_scans_imports_from_every_entry_module() {
        let project = temp_dir();
        let src = project.join("src");
        fs::create_dir_all(&src).unwrap();

        let root = src.join("main.asm");
        fs::write(
            &root,
            ".module helper\n.use missing.module\n.endmodule\n.module main\nmain: nop\n.endmodule\n",
        )
        .unwrap();

        let (root_lines, _) = expand_source_file_with_dependencies(&root, &[], &[], 64).unwrap();
        let err = load_module_graph(&root, root_lines, &[], &[], &[], 64)
            .expect_err("every entry module participates in dependency discovery");
        assert!(err.to_string().contains("unknown module: missing.module"));
    }

    #[test]
    fn load_module_graph_unknown_dotted_module_reports_use_site_error() {
        let project = temp_dir();
        let src = project.join("src");
        fs::create_dir_all(&src).unwrap();

        let root = src.join("main.asm");
        fs::write(
            &root,
            ".module main\n.use opforge.cli.missing (foo)\n.endmodule\n",
        )
        .unwrap();

        let (root_lines, _) = expand_source_file_with_dependencies(&root, &[], &[], 64).unwrap();
        let err = load_module_graph(&root, root_lines, &[], &[], &[], 64)
            .expect_err("missing module should be a normal graph error");
        let message = err.to_string();
        assert!(
            message.contains("unknown module: opforge.cli.missing"),
            "unexpected error: {message}"
        );
        let diag = err
            .diagnostics()
            .first()
            .expect("missing module should include .use-site diagnostic");
        assert_eq!(diag.line, 2);
        assert_eq!(diag.column, Some(2));
    }

    #[test]
    fn load_module_graph_direct_cycle_reports_import_chain() {
        let project = temp_dir();
        let src = project.join("src");
        fs::create_dir_all(&src).unwrap();

        let root = src.join("main.asm");
        fs::write(&root, ".module main\n.use a\n.endmodule\n").unwrap();
        fs::write(src.join("a.asm"), ".module a\n.use a\n.endmodule\n").unwrap();

        let (root_lines, _) = expand_source_file_with_dependencies(&root, &[], &[], 64).unwrap();
        let err = load_module_graph(&root, root_lines, &[], &[], &[], 64)
            .expect_err("direct import cycle should be rejected");
        let message = err.to_string();
        assert!(
            message.contains("cyclic module import: a -> a"),
            "unexpected error: {message}"
        );
    }

    #[test]
    fn load_module_graph_indirect_cycle_reports_import_chain() {
        let project = temp_dir();
        let src = project.join("src");
        fs::create_dir_all(&src).unwrap();

        let root = src.join("main.asm");
        fs::write(&root, ".module main\n.use a\n.endmodule\n").unwrap();
        fs::write(src.join("a.asm"), ".module a\n.use b\n.endmodule\n").unwrap();
        fs::write(src.join("b.asm"), ".module b\n.use c\n.endmodule\n").unwrap();
        fs::write(src.join("c.asm"), ".module c\n.use a\n.endmodule\n").unwrap();

        let (root_lines, _) = expand_source_file_with_dependencies(&root, &[], &[], 64).unwrap();
        let err = load_module_graph(&root, root_lines, &[], &[], &[], 64)
            .expect_err("indirect import cycle should be rejected");
        let message = err.to_string();
        assert!(
            message.contains("cyclic module import: a -> b -> c -> a"),
            "unexpected error: {message}"
        );
    }

    #[test]
    fn load_module_graph_rejects_cycles_through_entry_modules() {
        for entry_use in ["main", "helper"] {
            let project = temp_dir();
            let root = project.join("main.asm");
            fs::write(
                &root,
                format!(".module main\n.use {entry_use}\n.endmodule\n"),
            )
            .unwrap();
            fs::write(
                project.join("helper.asm"),
                ".module helper\n.use main\n.endmodule\n",
            )
            .unwrap();
            let (lines, _) = expand_source_file_with_dependencies(&root, &[], &[], 64).unwrap();
            let err = load_module_graph(&root, lines, &[], &[], &[], 64).unwrap_err();
            assert!(err.to_string().contains("cyclic module import: main ->"));
        }
    }

    #[test]
    fn load_module_graph_orders_entry_modules_and_shared_dependencies_once() {
        let project = temp_dir();
        let root = project.join("main.asm");
        // The supplied prepared source intentionally differs from disk.
        fs::write(&root, ".module stale\n.endmodule\n").unwrap();
        fs::write(project.join("deps.asm"), ".module left\n.use shared\n.endmodule\n.module right\n.use shared\n.endmodule\n.module unused\n.use missing\n.endmodule\n").unwrap();
        fs::write(project.join("shared.asm"), ".module shared\n.endmodule\n").unwrap();
        let source = "; prefix\n.module main\n.use helper\n.use right\n.endmodule\n; between\n.module helper\n.use left\n.endmodule\n.end\n";
        let graph = load_module_graph(
            &root,
            source.lines().map(str::to_owned).collect(),
            &[],
            &[],
            &[],
            64,
        )
        .unwrap();
        let modules = graph
            .lines
            .iter()
            .filter(|line| line.starts_with(".module"))
            .map(String::as_str)
            .collect::<Vec<_>>();
        assert_eq!(
            modules,
            [
                ".module shared",
                ".module left",
                ".module helper",
                ".module right",
                ".module main"
            ]
        );
        assert_eq!(graph.lines.first().unwrap(), "; prefix");
        assert_eq!(graph.lines.last().unwrap(), ".end");
        let helper = graph
            .lines
            .iter()
            .position(|line| line == ".module helper")
            .unwrap();
        assert_eq!(graph.source_map.origins()[helper].line, 7);
        assert_eq!(
            graph.source_map.origins()[helper].file.as_deref(),
            root.to_str()
        );
        assert_eq!(
            graph
                .lines
                .iter()
                .filter(|line| *line == "; between")
                .count(),
            1
        );
    }

    #[test]
    fn load_module_graph_exports_macros_between_entry_modules() {
        let project = temp_dir();
        let root = project.join("main.asm");
        let source = ".module main\n.use helper\n.helper.emit\n.endmodule\n.module helper\n.pub\nemit .macro\n.byte 7\n.endmacro\n.endmodule\n";
        fs::write(&root, source).unwrap();
        let graph = load_module_graph(
            &root,
            source.lines().map(str::to_owned).collect(),
            &[],
            &[],
            &[],
            64,
        )
        .unwrap();
        assert!(graph.lines.iter().any(|line| line.trim() == ".byte 7"));
        assert!(!graph.lines.iter().any(|line| line.trim() == ".helper.emit"));
    }

    fn compound_graph(
        source: &str,
    ) -> Result<crate::source_graph::ModuleGraphResult, asm::error::AsmRunError> {
        let project = temp_dir();
        let root = project.join("main.asm");
        fs::write(&root, source).unwrap();
        let result = load_module_graph(
            &root,
            source.lines().map(str::to_owned).collect(),
            &[],
            &[],
            &[],
            64,
        );
        fs::remove_dir_all(project).unwrap();
        result
    }

    #[test]
    fn configured_scalar_updates_follow_source_order() {
        for (op, left, right, expected) in [
            ("+=", "1", "2", "3"),
            ("-=", "4294967297", "2", "4294967295"),
            ("*=", "4294967297", "-2", "-8589934594"),
            ("/=", "-8589934601", "3", "-2863311533"),
            ("%=", "-8589934601", "3", "-2"),
            ("**=", "3", "21", "10460353203"),
            ("|=", "4294967296", "1", "1"),
            ("^=", "4294967297", "1", "0"),
            ("&=", "-1", "4294967296", "0"),
            ("||=", "4294967296", "4294967296", "0"),
            ("&&=", "4294967296", "1", "0"),
            ("<<=", "1", "33", "2"),
            (">>=", "2147483648", "33", "1073741824"),
            ("..=", "18", "0", "4608"),
            ("<?=", "4294967300", "-4294967301", "-4294967301"),
            (">?=", "-4294967301", "4294967300", "4294967300"),
            (".=", "1", "-8589934593", "-8589934593"),
        ] {
            let graph = compound_graph(&format!(
                ".module main\nn .var {left}\nn {op} {right}\n.if n==({expected})\n.use dep with (FEATURE=n)\n.else\n.use missing\n.endif\n.endmodule\n.module dep\n.byte FEATURE\n.endmodule\n"
            )).unwrap_or_else(|error| panic!("{op}: {error:?}"));
            assert_eq!(
                graph.module_parameters["dep"]["FEATURE"],
                AsmValue::Scalar(expected.parse().unwrap()),
                "{op}"
            );
        }
    }

    #[test]
    fn configured_failed_updates_invalidate_mutable_values() {
        for (left, update) in [
            ("1", "/= 0"),
            ("1", "%= 0"),
            ("1", "**= -1"),
            ("1", "**= 64"),
            ("1", "+= {1,2}"),
            ("{1,2}", "+= 1"),
            ("1", "+= missing"),
        ] {
            let error = compound_graph(&format!(
                ".module main\nn .var {left}\nn {update}\n.use dep with (FEATURE=n)\n.endmodule\n.module dep\n.endmodule\n"
            )).err().unwrap_or_else(|| panic!("accepted {left}/{update}"));
            assert!(
                error.error().message().contains(".use parameter FEATURE"),
                "{error:?}"
            );
        }
    }

    #[test]
    fn configured_compounds_snapshot_and_chain() {
        let graph = compound_graph(concat!(
            ".module main\n",
            "changing .var {2,4,6}\nlistSnapshot=changing\n",
            "changing .set 3..=9:3\nrangeSnapshot=changing\nchanging .set 17\n",
            ".use middle with (SCALAR=changing, ITEMS=listSnapshot, RANGE=rangeSnapshot)\n",
            ".endmodule\n.module middle\n",
            ".if SCALAR==17 && .len(ITEMS)==3 && ITEMS[1]==4 && RANGE[2]==9\n",
            ".use leaf with (ITEMS=ITEMS, RANGE=RANGE, COUNT=.len(RANGE))\n",
            ".else\n.use missing\n.endif\n.endmodule\n",
            ".module leaf\n.endmodule\n"
        ))
        .unwrap();
        for module in ["middle", "leaf"] {
            assert_eq!(
                graph.module_parameters[module]["ITEMS"],
                AsmValue::List(vec![2, 4, 6])
            );
            assert_eq!(
                graph.module_parameters[module]["RANGE"],
                AsmValue::Range {
                    start: 3,
                    end: 10,
                    step: 3
                }
            );
        }
        assert_eq!(
            graph.module_parameters["middle"]["SCALAR"],
            AsmValue::Scalar(17)
        );
        assert_eq!(
            graph.module_parameters["leaf"]["COUNT"],
            AsmValue::Scalar(3)
        );
        assert_eq!(graph.lines.len(), graph.source_map.origins().len());
    }

    #[test]
    fn configured_compounds_compare_full_values() {
        let source = |second: &str| {
            format!(
            ".module main\n.use dep with (ITEMS={{2,4}}, RANGE=3..=9:3)\n.use dep with (ITEMS={second}, RANGE=3..10:3)\n.endmodule\n.module dep\n.endmodule\n"
        )
        };
        compound_graph(&source("{2,4}")).unwrap();
        let error = compound_graph(&source("{2,5}")).unwrap_err();
        assert!(error
            .error()
            .message()
            .contains("conflicting .use parameters"));
    }

    #[test]
    fn configured_compounds_reject_unavailable_and_malformed_values() {
        for body in [
            ".use dep with (ITEMS=missing)\n",
            ".use dep with (ITEMS=later)\nlater={1,2}\n",
            "hidden .block\nvalues={1,2}\n.bend\n.use dep with (ITEMS=values)\n",
            ".use dep with (ITEMS=0..4:0)\n",
            "values={1,2}\n.use dep with (ITEMS=values+1)\n",
            ".use dep with (ITEMS={1,{2}})\n",
            ".use dep with (ITEMS={1,$})\n",
            ".use dep with (ITEMS=1?7:$)\n",
        ] {
            let source = format!(".module main\n{body}.endmodule\n.module dep\n.endmodule\n");
            let error = compound_graph(&source).unwrap_err();
            assert!(
                error.error().message().contains(".use parameter ITEMS"),
                "{body}: {}",
                error.error().message()
            );
        }
    }

    #[test]
    fn configured_compounds_preserve_outer_values_and_failed_replacement() {
        let graph = compound_graph(concat!(
            ".module main\nvalues={1,2}\n",
            "hidden .block\nvalues={8,9,10}\n.bend\n",
            ".if .len(values)==2\n.use dep with (ITEMS=values)\n.else\n.use missing\n.endif\n",
            ".endmodule\n.module dep\n.endmodule\n"
        ))
        .unwrap();
        assert_eq!(
            graph.module_parameters["dep"]["ITEMS"],
            AsmValue::List(vec![1, 2])
        );
        let error = compound_graph(concat!(
            ".module main\nvalues .var {1,2}\nvalues .set missing\n",
            ".use dep with (ITEMS=values)\n.endmodule\n.module dep\n.endmodule\n"
        ))
        .unwrap_err();
        assert!(error.error().message().contains(".use parameter ITEMS"));
    }
    #[test]
    fn compile_time_value_evaluation_is_strict_without_changing_pass_one() {
        use asm::line::AsmLine;
        use opcore::parser::{Expr, Parser};
        let registry = registry::ModuleRegistry::new();
        let mut symbols = types::symbol::SymbolTable::new();
        let mut evaluator = AsmLine::new(&mut symbols, &registry);
        let expr = Expr::Identifier(
            "forward".into(),
            opcore::tokenizer::Span {
                line: 1,
                col_start: 1,
                col_end: 8,
            },
        );
        assert_eq!(
            evaluator.eval_value_ast(&expr).unwrap(),
            types::asm_value::AsmValue::Scalar(0)
        );
        assert!(evaluator
            .eval_value_ast_requiring_defined_symbols(&expr)
            .is_err());
        assert_eq!(evaluator.pass_number(), 1);
        assert_eq!(
            evaluator.eval_value_ast(&expr).unwrap(),
            types::asm_value::AsmValue::Scalar(0)
        );
        let ast = Parser::from_line("value={1,2}", 1)
            .unwrap()
            .parse_compat_mixed_line()
            .unwrap();
        let opcore::parser::LineAst::Assignment(assignment) = ast else {
            panic!("assignment expected");
        };
        assert_eq!(
            evaluator
                .eval_value_ast_requiring_defined_symbols(&assignment.expr)
                .unwrap(),
            types::asm_value::AsmValue::List(vec![1, 2])
        );
        assert_eq!(evaluator.pass_number(), 1);
    }
    #[test]
    fn configured_compound_conditionals_keep_dependency_macro_imports() {
        let graph = compound_graph(concat!(
            ".module main\n.use middle with (ITEMS={2,4})\n.endmodule\n",
            ".module middle\n.if .len(ITEMS)==2 && ITEMS[1]==4\n.use leaf as child\n.endif\n.child.emit\n.endmodule\n",
            ".module leaf\n.pub\nemit .macro\n.byte 7\n.endmacro\n.endmodule\n"
        )).unwrap();
        assert!(graph.lines.iter().any(|line| line.trim() == ".byte 7"));
        assert!(!graph.lines.iter().any(|line| line.trim() == ".child.emit"));
    }
    #[test]
    fn configured_compound_chain_schedules_reversed_modules_and_ignores_inactive_cycles() {
        let graph = compound_graph(concat!(
            ".module leaf\n.if 0\n.use main\n.use missing\n.endif\n.endmodule\n",
            ".module middle\n.if .len(ITEMS)==2 && ITEMS[1]==4\n.use leaf with (ITEMS=ITEMS)\n.else\n.use missing\n.endif\n.endmodule\n",
            ".module main\n.use middle with (ITEMS={2,4})\n.endmodule\n"
        )).unwrap();
        let modules: Vec<_> = graph
            .lines
            .iter()
            .filter(|line| line.starts_with(".module "))
            .map(String::as_str)
            .collect();
        assert_eq!(modules, [".module leaf", ".module middle", ".module main"]);
        for module in ["middle", "leaf"] {
            assert_eq!(
                graph.module_parameters[module]["ITEMS"],
                AsmValue::List(vec![2, 4])
            );
        }
    }

    fn assemble_graph(graph: crate::source_graph::ModuleGraphResult) -> Result<Vec<u8>, String> {
        let mut assembler = asm::engine::Assembler::with_cpu_and_registry(
            crate::default_cpu(),
            crate::build_default_asm_registry(),
        );
        assembler.module_parameters = graph.module_parameters;
        assembler.module_macro_names = graph.module_macro_names;
        assembler.set_source_resources(graph.source_map);
        let first = assembler.pass1(&graph.lines);
        if first.errors != 0 {
            return Err(format!("{:?}", assembler.diagnostics));
        }
        let mut listing = asm::listing::ListingWriter::new(Vec::new(), false);
        let second = assembler.pass2(&graph.lines, &mut listing).unwrap();
        if second.errors != 0 {
            return Err(format!("{:?}", assembler.diagnostics));
        }
        Ok(assembler
            .image
            .entries()
            .unwrap()
            .into_iter()
            .map(|(_, value)| value)
            .collect())
    }

    #[test]
    fn configured_structs_keep_nominal_module_identity() {
        for value in ["Point", "Point{x:3}"] {
            let error = compound_graph(&format!(
                ".module first\nPoint .struct\nx .byte ?\n.endstruct\n.use dep with (TYPE={value})\n.endmodule\n.module second\nPoint .struct\nx .byte ?\n.endstruct\n.use dep with (TYPE={value})\n.endmodule\n.module dep\n.endmodule\n"
            )).unwrap_err();
            assert!(
                error
                    .error()
                    .message()
                    .contains("conflicting .use parameters"),
                "{error:?}"
            );
        }
        let project = temp_dir();
        let root = project.join("implicit.asm");
        let source =
            "Point .struct\nx .byte ?\n.endstruct\n.use dep with (TYPE=Point)\n.byte dep.result\n";
        fs::write(&root, source).unwrap();
        fs::write(
            project.join("dep.asm"),
            "made=TYPE{x:7}\n.pub\nresult=made.x\n",
        )
        .unwrap();
        let graph = load_module_graph(
            &root,
            source.lines().map(str::to_owned).collect(),
            &[],
            &[],
            &[],
            64,
        )
        .unwrap();
        let AsmValue::Struct(definition) = &graph.module_parameters["dep"]["TYPE"] else {
            panic!("type lost")
        };
        assert_eq!(definition.name, "implicit.Point");
        assert_eq!(graph.lines.len(), graph.source_map.origins().len());
        assert_eq!(assemble_graph(graph).unwrap(), [7]);
        fs::remove_dir_all(project).unwrap();
    }

    #[test]
    fn configured_struct_geometry_requires_available_sizes() {
        let graph = compound_graph(concat!(
            ".module main\n",
            "width .var 3\nPoint .struct\nx .byte ?\ny .res width\n.endstruct\n",
            "Holder .struct\ncontents .res Point\n.endstruct\nwidth .set 9\n",
            ".if Point\n.use dep with (TYPE=Holder)\n.else\n.use missing\n.endif\n",
            ".byte Point,Holder,dep.result\n.endmodule\n.module dep\n.pub\nresult=TYPE.contents\n.endmodule\n"
        )).unwrap();
        let AsmValue::Struct(definition) = &graph.module_parameters["dep"]["TYPE"] else {
            panic!("type lost")
        };
        assert_eq!(definition.name, "main.Holder");
        assert_eq!(definition.size, 4);
        assert_eq!(assemble_graph(graph).unwrap(), [4, 4, 0]);
        for size in ["later", "$", "0?$:3"] {
            let error = compound_graph(&format!(
                ".module main\nPoint .struct\nx .res {size}\n.endstruct\n.use dep with (TYPE=Point)\nlater=3\n.endmodule\n.module dep\n.endmodule\n"
            )).unwrap_err();
            assert!(
                error.error().message().contains(".use parameter TYPE"),
                "{size}: {error:?}"
            );
        }
        // Capturing is best effort: ordinary assembly retains its own forward
        // and imported-symbol layout processing when no schema is configured.
        for size in ["later", "dep.Span"] {
            let graph = compound_graph(&format!(
                ".module main\n.use dep\nPoint .struct\nx .res {size}\n.endstruct\nlater=3\n.byte Point\n.endmodule\n.module dep\n.pub\nSpan=3\n.endmodule\n"
            )).unwrap();
            assert_eq!(assemble_graph(graph).unwrap(), [3], "{size}");
        }
    }

    #[test]
    fn configured_struct_geometry_never_guesses_conditional_branches() {
        for body in [
            ".if later\nPoint .struct\nx .byte ?\n.endstruct\n.else\nPoint .struct\nx .long ?\n.endstruct\n.endif\n",
            "Point .struct\n.if later\nx .byte ?\n.else\nx .long ?\n.endif\n.endstruct\n",
            ".match later\n.case 1\nPoint .struct\nx .byte ?\n.endstruct\n.default\nPoint .struct\nx .long ?\n.endstruct\n.endmatch\n",
        ] {
            let error = compound_graph(&format!(".module main\n{body}.use dep with (TYPE=Point)\nlater=1\n.endmodule\n.module dep\n.endmodule\n")).unwrap_err();
            assert!(error.error().message().contains(".use parameter TYPE"), "{error:?}");
        }
        for body in [
            ".if 0\n.if missing\nOther .struct\nx .long ?\n.endstruct\n.endif\n.else\nPoint .struct\nx .byte ?\n.endstruct\n.endif\n",
            ".if 1\nPoint .struct\nx .byte ?\n.endstruct\n.elseif missing\nOther .struct\nx .long ?\n.endstruct\n.endif\n",
            ".match 1\n.case 1,missing\nPoint .struct\nx .byte ?\n.endstruct\n.default\nOther .struct\nx .long ?\n.endstruct\n.endmatch\n",
        ] {
            let graph = compound_graph(&format!(".module main\n{body}.use dep with (TYPE=Point)\n.endmodule\n.module dep\n.endmodule\n")).unwrap();
            let AsmValue::Struct(definition) = &graph.module_parameters["dep"]["TYPE"] else { panic!("type lost") };
            assert_eq!(definition.size, 1);
        }
    }

    #[test]
    fn configured_struct_geometry_invalidates_uncertain_value_writes() {
        for writes in [
            ".if later\nwidth .set 1\n.else\nwidth .set 4\n.endif\n",
            ".if later\nwidth .set 1\n.endif\n",
            ".if later\nwidth += 1\n.endif\n",
            ".if later\nwidth := 1\n.endif\n",
            ".if later\n.if 1\nwidth .set 1\n.endif\n.endif\n",
            ".match later\n.case 1\nwidth .set 1\n.default\nwidth .set 4\n.endmatch\n",
            ".match 1\n.case later\nwidth .set 1\n.endmatch\n",
            ".if later\nwidth .set 1\n.endif\nwidth :?= 7\n",
        ] {
            let error = compound_graph(&format!(
                ".module main\nwidth .var 3\n{writes}Point .struct\nx .res width\n.endstruct\n.use dep with (TYPE=Point)\nlater=1\n.endmodule\n.module dep\n.endmodule\n"
            )).unwrap_err();
            assert!(
                error.error().message().contains(".use parameter TYPE"),
                "{writes}: {error:?}"
            );
        }
        for (writes, size) in [
            (".if 0\nwidth .set 1\n.endif\n", 3),
            (".if later\n.if 0\nwidth .set 1\n.endif\n.endif\n", 3),
            (".if 1\n.elseif later\nwidth .set 1\n.endif\n", 3),
            (
                ".match 1\n.case 1,later\n.case later\nwidth .set 1\n.endmatch\n",
                3,
            ),
            (
                ".match 1\n.case later\n.case 2\nwidth .set 1\n.endmatch\n",
                3,
            ),
            (".if later\nlocal .block\nwidth .var 1\n.bend\n.endif\n", 3),
            (".if later\nwidth :?= 7\n.endif\n", 3),
            (".if later\nwidth .set 1\n.endif\nwidth .set 5\n", 5),
        ] {
            let graph = compound_graph(&format!(
                ".module main\nwidth .var 3\n{writes}Point .struct\nx .res width\n.endstruct\n.use dep with (TYPE=Point)\nlater=1\n.endmodule\n.module dep\n.endmodule\n"
            )).unwrap();
            let AsmValue::Struct(definition) = &graph.module_parameters["dep"]["TYPE"] else {
                panic!("type lost")
            };
            assert_eq!(definition.size, size, "{writes}");
        }
        let graph = compound_graph(concat!(
            ".module main\nwidth .var 3\nsaved=width\n",
            ".if later\nwidth .set 1\n.endif\n",
            "Point .struct\nx .res saved\n.endstruct\n",
            ".use dep with (TYPE=Point)\nlater=1\n.endmodule\n.module dep\n.endmodule\n"
        ))
        .unwrap();
        let AsmValue::Struct(definition) = &graph.module_parameters["dep"]["TYPE"] else {
            panic!("type lost")
        };
        assert_eq!(definition.size, 3);
    }

    #[test]
    fn configured_struct_capture_does_not_execute_runtime_repetition() {
        for repetition in [
            ".for item in {}\nPoint .struct\nx .byte ?\n.endstruct\n.endfor\n",
            ".for item in {1}\nPoint .struct\nx .byte ?\n.endstruct\n.endfor\n",
            ".bfor item in {}\nPoint .struct\nx .byte ?\n.endstruct\n.endfor\n",
            ".bfor item in {1}\nPoint .struct\nx .byte ?\n.endstruct\n.endfor\n",
            ".while 0\nPoint .struct\nx .byte ?\n.endstruct\n.endwhile\n",
            "Point .struct\n.for 0\nx .byte ?\n.endfor\n.endstruct\n",
        ] {
            let error = compound_graph(&format!(
                ".module main\n{repetition}.use dep with (TYPE=Point)\n.endmodule\n.module dep\n.endmodule\n"
            )).unwrap_err();
            assert!(
                error.error().message().contains(".use parameter TYPE"),
                "{repetition}: {error:?}"
            );
        }
        for repetition in [
            ".for item in {}\nwidth += 1\n.endfor\n",
            ".for item in {1}\nwidth += 1\n.endfor\n",
            ".while 0\nwidth += 1\n.endwhile\n",
            ".bwhile 0\nmain.width .set 1\n.endwhile\n",
            ".bfor item in {1}\nmain.width .set 1\n.endfor\n",
            ".for item in {1}\n.if item\nwidth += 1\n.endif\n.endfor\n",
        ] {
            let error = compound_graph(&format!(
                ".module main\nwidth .var 3\nitem=0\n{repetition}Point .struct\nx .res width\n.endstruct\n.use dep with (TYPE=Point)\n.endmodule\n.module dep\n.endmodule\n"
            )).unwrap_err();
            assert!(
                error.error().message().contains(".use parameter TYPE"),
                "{repetition}: {error:?}"
            );
        }
        let graph = compound_graph(concat!(
            ".module main\nwidth .var 3\nsaved=width\nPoint .struct\nx .res width\n.endstruct\n",
            ".bfor width in {1}\nwidth .var 7\n.endfor\n",
            ".use dep with (TYPE=Point,SIZE=width,SNAPSHOT=saved)\n.endmodule\n",
            ".module dep\n.byte TYPE.x,SIZE,SNAPSHOT\n.endmodule\n"
        ))
        .unwrap();
        let AsmValue::Struct(definition) = &graph.module_parameters["dep"]["TYPE"] else {
            panic!("type lost")
        };
        assert_eq!(definition.size, 3);
        assert_eq!(graph.module_parameters["dep"]["SIZE"], AsmValue::Scalar(3));
        assert_eq!(
            graph.module_parameters["dep"]["SNAPSHOT"],
            AsmValue::Scalar(3)
        );
        assert_eq!(assemble_graph(graph).unwrap(), [0, 3, 3]);

        let graph = compound_graph(concat!(
            ".module main\nwidth .var 3\nsaved=width\nPoint .struct\nx .res width\n.endstruct\n",
            ".bfor item in {1}\nmain.width .set 1\n.endfor\n",
            ".use dep with (TYPE=Point,SNAPSHOT=saved)\n.endmodule\n",
            ".module dep\n.byte TYPE.x,SNAPSHOT\n.endmodule\n"
        ))
        .unwrap();
        let AsmValue::Struct(definition) = &graph.module_parameters["dep"]["TYPE"] else {
            panic!("type lost")
        };
        assert_eq!(definition.size, 3);
        assert_eq!(assemble_graph(graph).unwrap(), [0, 3]);

        let error = compound_graph(concat!(
            ".module main\nPoint .struct\nx .byte ?\n.endstruct\nTYPE=Point\n",
            ".for TYPE in {1}\n.use dep with (VALUE=TYPE{x:1})\n.endfor\n",
            ".endmodule\n.module dep\n.endmodule\n"
        ))
        .unwrap_err();
        assert!(
            error.error().message().contains(".use parameter VALUE"),
            "{error:?}"
        );
        let graph = compound_graph(concat!(
            ".module main\nPoint .struct\nx .byte ?\n.endstruct\nTYPE=Point\n",
            ".for TYPE in {1}\n.for TYPE in {2}\n.byte TYPE\n.endfor\n.byte TYPE\n.endfor\n",
            ".use dep with (VALUE=TYPE{x:7})\n.endmodule\n.module dep\n.byte VALUE.x\n.endmodule\n"
        ))
        .unwrap();
        assert_eq!(assemble_graph(graph).unwrap(), [7, 2, 1]);

        // Ordinary assembly still executes repetition and its local declarations.
        let graph = compound_graph(".module main\nitems .bfor item in {1,2}\nPoint .struct\nx .byte ?\n.endstruct\n.byte item,Point\n.endfor\n.endmodule\n").unwrap();
        assert_eq!(assemble_graph(graph).unwrap(), [1, 1, 2, 1]);
    }

    #[test]
    fn ordinary_typed_bindings_obey_visibility_at_the_source_binding() {
        let definition = ".module dep\nShape .struct\nx .byte ?\n.endstruct\nHidden=Shape\nsecret=Shape{x:3}\n.pub\nAlias=Shape\nvisible=secret\n.endmodule\n";
        for body in [
            "leak=d.Hidden\n",
            "made=d.Hidden{x:7}\n",
            "leak=d.secret\n",
            ".byte d.secret.x\n",
        ] {
            let graph = compound_graph(&format!(
                "{definition}.module main\n.use dep as d\n{body}.endmodule\n"
            ))
            .unwrap();
            let error = assemble_graph(graph).unwrap_err();
            assert!(error.contains("Symbol is private"), "{body}: {error}");
        }
        let graph = compound_graph(&format!("{definition}.module main\n.use dep as d\nmade=d.Alias{{x:7}}\n.byte made.x,d.visible.x\n.endmodule\n")).unwrap();
        assert_eq!(assemble_graph(graph).unwrap(), [7, 3]);
    }
}
