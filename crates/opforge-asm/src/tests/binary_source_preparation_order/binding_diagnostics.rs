//! Failure snapshots remain observations of a rejected assembly.
use super::*;

fn missing_import(with_other_declaration: bool) -> String {
    let mut source = String::from(
        ".module consumer\n.cpu m68020\n.use example.state\n.byte state.MissingValue\n.endmodule\n.module example.state\n.cpu m68020\n.byte 0\n.endmodule\n",
    );
    if with_other_declaration {
        source.push_str(".module other\n.cpu m68020\n.pub\nMissingValue = 7\n.endmodule\n");
    }
    source
}

#[test]
fn compact_missing_import_diagnostic_rust_rejects() {
    for other in [false, true] {
        let source = missing_import(other);
        assert!(oracle(&[("main.asm", source.as_str())]).is_err());
    }
}

fn progress_fields(stdout: &str, phase: u32) -> [u32; 3] {
    let prefix = format!("progress p={phase:08X} ");
    let rows: Vec<_> = stdout
        .lines()
        .filter(|line| line.starts_with(&prefix))
        .collect();
    assert_eq!(rows.len(), 1, "one complete fresh diagnostic row: {prefix}");
    let fields: Vec<_> = rows[0].split_whitespace().skip(2).take(3).collect();
    fields
        .try_into()
        .map(|fields: [&str; 3]| {
            fields.map(|field| u32::from_str_radix(field.split_once('=').unwrap().1, 16).unwrap())
        })
        .unwrap()
}

#[test]
#[ignore = "requires instrumented FS-UAE; bounded declaration search on actual rejection"]
fn compact_binding_declaration_diagnostic_fs_uae() {
    assert_eq!(std::env::var("OPFORGE_COMPARE_MEMORY").as_deref(), Ok("1"));
    assert_eq!(
        std::env::var("OPFORGE_PREPARATION_PROGRESS").as_deref(),
        Ok("1")
    );
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let package = prepare_package(&core, &resolved).unwrap();
    for (other, stage, name) in [
        (false, 14, "example.state.missingvalue"),
        (true, 16, "other.missingvalue"),
    ] {
        let source = missing_import(other);
        let result = crate::fs_uae_smoke::run_compact_cli_files_from_env(
            &workspace_root(),
            &package,
            &[("main.asm", source.as_bytes())],
            &[],
            &[],
            None,
            false,
        )
        .expect("fresh instrumented import rejection");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real native execution required");
        };
        assert_eq!(runs.len(), 1);
        let run = &runs[0];
        assert!(run.protocol_completed && !run.success);
        assert_eq!(run.exit_code, Some(20));
        assert_eq!(progress_fields(&run.stdout, 32)[0], stage);
        let length = progress_fields(&run.stdout, 33)[2] as usize;
        let mut bytes = Vec::new();
        for phase in 64..=85 {
            let fields = progress_fields(&run.stdout, phase);
            for field in fields.into_iter().take(if phase == 85 { 1 } else { 3 }) {
                bytes.extend_from_slice(&field.to_be_bytes());
            }
        }
        assert!(bytes[..length].eq_ignore_ascii_case(name.as_bytes()));
        if stage == 16 {
            let primary = progress_fields(&run.stdout, 32);
            let owner = progress_fields(&run.stdout, 96);
            assert_eq!(owner[0], stage);
            assert_eq!(owner[2], primary[1]);
            assert_eq!(progress_fields(&run.stdout, 97)[2], 5);
            let owner_name = progress_fields(&run.stdout, 128);
            let owner_bytes = owner_name
                .into_iter()
                .flat_map(u32::to_be_bytes)
                .collect::<Vec<_>>();
            assert!(owner_bytes[..5].eq_ignore_ascii_case(b"other"));
            for phase in [160, 224] {
                let neighbor = progress_fields(&run.stdout, phase);
                assert_eq!(neighbor[0], stage);
                assert_eq!(neighbor[2], primary[1]);
            }
        }
        eprintln!(
            "BINDING_DECLARATION_DIAGNOSTIC stage={stage} seconds={:?}",
            run.start_to_done_host_seconds
        );
    }
}
