//! Native statement-record capacity boundary proofs.

use super::*;

fn retained_import_count(
    root: &Path,
    include_paths: &[PathBuf],
    module_paths: &[PathBuf],
) -> usize {
    let root_lines = expand_source_file(root, &[], include_paths, 64)
        .expect("expand import-budget root including its include files");
    let graph = load_module_graph(root, root_lines, &[], include_paths, module_paths, 64)
        .expect("load retained import-budget module graph");
    graph
        .lines
        .iter()
        .filter(|line| {
            line.split_ascii_whitespace()
                .next()
                .is_some_and(|directive| directive.eq_ignore_ascii_case(".use"))
        })
        .count()
}

#[test]
fn native_item38_import_budget_counts_retained_include_requests() {
    let root = create_temp_dir("native-retained-import-budget");
    let main = root.join("main.asm");
    let imports = root.join("imports.i");
    write_file(&main, ".module main\n.include \"imports.i\"\n.endmodule\n");
    write_file(
        &imports,
        ".use dependency as first\n.use dependency as second\n",
    );
    write_file(
        &root.join("dependency.asm"),
        ".module dependency\n.include \"dependency.i\"\n.endmodule\n",
    );
    write_file(&root.join("dependency.i"), ".use leaf\n");
    write_file(&root.join("leaf.asm"), ".module leaf\n.endmodule\n");
    // An indexed but unreachable module must not consume product import rows,
    // even if its requests would exceed the table or fail module resolution.
    write_file(
        &root.join("unreachable.asm"),
        &format!(
            ".module unreachable\n{}.endmodule\n",
            ".use absent\n".repeat(513)
        ),
    );
    assert_eq!(retained_import_count(&main, &[], &[]), 3);

    // Loading a module once must not deduplicate its callers' retained requests.
    // The same requests become a genuine over-budget graph when rooted.
    write_file(&imports, &".use dependency\n".repeat(513));
    assert_eq!(retained_import_count(&main, &[], &[]), 514);
    fs::remove_dir_all(root).expect("remove import-budget fixture");
}
