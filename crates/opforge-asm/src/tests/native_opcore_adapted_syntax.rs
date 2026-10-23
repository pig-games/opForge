//! Additive MOS fixture proofs for mixed-CPU opcore syntax roots.

use super::*;

const ADAPTED_FIXTURE_DIR: &str = "crates/opforge-asm/fixtures/native-opcore-adapted";

fn adapted_fixture_cases() -> [(&'static str, &'static str); 4] {
    [
        ("expr_syntax", "examples/opcore/expr_syntax.asm"),
        ("grouping", "examples/opcore/grouping.asm"),
        ("syntax", "examples/opcore/syntax.asm"),
        ("testexpr", "examples/opcore/testexpr.asm"),
    ]
}

#[test]
fn native_opcore_additive_mos_fixtures_preserve_complete_source_mapping() {
    // Proof level A. This proves every canonical source line is either retained
    // byte-for-byte or named by an explicit CPU spelling adaptation, and that
    // every canonical .org line is unchanged. It does not prove either Rust or
    // native artifact execution.
    let root = workspace_root();
    for (name, canonical_rel) in adapted_fixture_cases() {
        let adapted_rel = format!("{ADAPTED_FIXTURE_DIR}/{name}.asm");
        let canonical = fs::read_to_string(root.join(canonical_rel))
            .unwrap_or_else(|err| panic!("read canonical {canonical_rel}: {err}"));
        let adapted = fs::read_to_string(root.join(&adapted_rel))
            .unwrap_or_else(|err| panic!("read adapted {adapted_rel}: {err}"));
        let mapping = fs::read_to_string(
            root.join(ADAPTED_FIXTURE_DIR)
                .join(format!("{name}.adapt.tsv")),
        )
        .unwrap_or_else(|err| panic!("read adaptation map for {name}: {err}"));

        assert!(
            mapping
                .lines()
                .any(|line| line == format!("# canonical={canonical_rel}")),
            "{name} map must name its canonical source"
        );
        assert!(
            mapping
                .lines()
                .any(|line| line == format!("# adapted={adapted_rel}")),
            "{name} map must name its additive fixture"
        );

        let mut mapped = BTreeMap::new();
        for row in mapping
            .lines()
            .filter(|line| !line.is_empty() && !line.starts_with('#'))
        {
            let mut fields = row.splitn(3, '\t');
            let line_number = fields
                .next()
                .expect("mapping line number")
                .parse::<usize>()
                .unwrap_or_else(|err| panic!("invalid {name} mapping row '{row}': {err}"));
            let kind = fields.next().expect("mapping adaptation kind");
            let reason = fields.next().expect("mapping adaptation reason");
            assert!(
                matches!(kind, "cpu-target" | "instruction" | "operand-form"),
                "unsupported {name} adaptation kind in '{row}'"
            );
            assert!(
                !reason.trim().is_empty(),
                "{name} adaptation reason must be non-empty"
            );
            assert!(
                mapped.insert(line_number, (kind, reason)).is_none(),
                "duplicate {name} mapping for line {line_number}"
            );
        }

        let canonical_lines = canonical.lines().collect::<Vec<_>>();
        let adapted_lines = adapted.lines().collect::<Vec<_>>();
        assert_eq!(
            adapted_lines.len(),
            canonical_lines.len(),
            "{name} additive fixture must preserve line count"
        );
        for (index, (canonical_line, adapted_line)) in
            canonical_lines.iter().zip(adapted_lines.iter()).enumerate()
        {
            let line_number = index + 1;
            let changed = canonical_line != adapted_line;
            assert_eq!(
                mapped.contains_key(&line_number),
                changed,
                "{name}:{line_number} must be either byte-identical or explicitly mapped"
            );
            if canonical_line
                .trim_start()
                .to_ascii_lowercase()
                .starts_with(".org")
            {
                assert_eq!(
                    adapted_line, canonical_line,
                    "{name}:{line_number} must preserve canonical .org semantics"
                );
            }
        }
        assert!(
            mapped
                .keys()
                .all(|line_number| *line_number <= canonical_lines.len()),
            "{name} map contains an out-of-range source line"
        );
    }
}

#[test]
fn native_opcore_additive_mos_fixture_artifacts_match_live_rust_cli() {
    // Proof level B. This proves each checked-in fixture-local HEX artifact is
    // reproduced exactly by the live Rust CLI under the 65C02 override. It does
    // not prove native execution or equivalence of CPU-specific instruction effects.
    let root = workspace_root();
    for (name, _) in adapted_fixture_cases() {
        let fixture = root.join(ADAPTED_FIXTURE_DIR).join(format!("{name}.asm"));
        let expected = root.join(ADAPTED_FIXTURE_DIR).join(format!("{name}.hex"));
        let temp = create_temp_dir(&format!("native-opcore-adapted-{name}"));
        let actual = temp.join(format!("{name}.hex"));
        let args = vec![
            "opForge".to_string(),
            fixture.to_string_lossy().into_owned(),
            "--hex".to_string(),
            actual.to_string_lossy().into_owned(),
            "--cpu".to_string(),
            "65c02".to_string(),
        ];
        let cli = Cli::parse_from(args);
        run_with_cli_with_context(&cli)
            .unwrap_or_else(|err| panic!("live Rust CLI failed for {name}: {err:?}"));
        assert_eq!(
            fs::read(&actual).expect("read actual adapted HEX"),
            fs::read(&expected).expect("read expected adapted HEX"),
            "fixture-local Rust HEX drift for {name}"
        );
    }
}



#[test]
fn native_opcore_adapted_expr_syntax_fs_uae() {
    run_adapted_fixture_fs_uae("expr_syntax");
}

#[test]
fn native_opcore_adapted_grouping_fs_uae() {
    run_adapted_fixture_fs_uae("grouping");
}

#[test]
fn native_opcore_adapted_syntax_fs_uae() {
    run_adapted_fixture_fs_uae("syntax");
}

#[test]
fn native_opcore_adapted_testexpr_fs_uae() {
    run_adapted_fixture_fs_uae("testexpr");
}
