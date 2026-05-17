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
    fn load_module_graph_scans_root_uses_only_from_selected_root_module() {
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
        let graph = load_module_graph(&root, root_lines, &[], &[], &[], 64)
            .expect("non-root helper module imports must not participate in root bootstrap");

        assert!(
            graph.dependency_files.is_empty(),
            "helper module import should not be treated as root dependency"
        );
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
    fn load_module_graph_allows_importing_available_main() {
        let project = temp_dir();
        let src = project.join("src");
        fs::create_dir_all(&src).unwrap();

        let root = src.join("main.asm");
        fs::write(
            &root,
            ".module main\n.use main\n.use helper\nmain: nop\n.endmodule\n",
        )
        .unwrap();
        fs::write(
            src.join("helper.asm"),
            ".module helper\n.use main\nhelper_label: nop\n.endmodule\n",
        )
        .unwrap();

        let (root_lines, _) = expand_source_file_with_dependencies(&root, &[], &[], 64).unwrap();
        let graph = load_module_graph(&root, root_lines, &[], &[], &[], 64)
            .expect("available root module imports should not be treated as cycles");
        assert!(
            graph
                .lines
                .iter()
                .any(|line| line.trim().eq_ignore_ascii_case(".module helper")),
            "helper module should still load"
        );
    }
}
