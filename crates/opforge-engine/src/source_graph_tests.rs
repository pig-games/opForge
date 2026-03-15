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
}
