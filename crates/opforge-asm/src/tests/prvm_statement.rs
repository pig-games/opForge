// SPDX-License-Identifier: GPL-3.0-or-later
use super::*;

#[test]
fn prvm_statement_unchanged_local_and_cross_module_fixtures_match_references() {
    let root = workspace_root();
    let output = root
        .join("target")
        .join(format!("prvm-statement-fixtures-{}", process::id()));
    fs::create_dir_all(&output).unwrap();
    for name in [
        "statement_boundary_span",
        "statement_capture_types",
        "statement_signatures",
        "statement_expansion",
        "statement_cross_module_ok",
    ] {
        let source = root.join("examples/opcore").join(format!("{name}.asm"));
        assemble_example(&source, &output, false).unwrap_or_else(|err| panic!("{name}: {err}"));
        for extension in ["hex", "lst"] {
            let filename = format!("{name}.{extension}");
            let actual = fs::read(output.join(&filename))
                .unwrap_or_else(|err| panic!("Missing {filename}: {err}"));
            let expected =
                fs::read(root.join("examples/reference/opcore").join(&filename)).unwrap();
            if extension == "lst" {
                assert_eq!(
                    normalize_listing_for_reference_compare(&String::from_utf8_lossy(&actual)),
                    normalize_listing_for_reference_compare(&String::from_utf8_lossy(&expected)),
                    "{filename}"
                );
            } else {
                assert_eq!(actual, expected, "{filename}");
            }
        }
    }
    fs::remove_dir_all(output).unwrap();
}
