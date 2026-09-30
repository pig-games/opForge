//! A single relocation base plus an absolute addend retains its identity.
use super::*;

fn source(body: &str) -> String {
    format!(
        r#".module probe
.cpu m68020
Offset=2
.section bss,kind=bss
Bytes .res byte,8
.endsection
.section data,kind=data
Payload .long 0,0
.endsection
.section code,kind=code
entry
{body} rts
.endsection
.output "build/sections.hunk",format=hunk,sections=code,data,bss
.endmodule
"#
    )
}

const STORES: &str = r#" move.w d5,Bytes
 move.w d4,Bytes+2
 move.w d3,2+Bytes
 move.w d2,Bytes+Offset
 move.w d1,Bytes-2
 move.w d0,Payload+2
 move.w d0,entry+2
"#;

#[test]
fn compact_address_addends_rust_oracle() {
    let bytes = hunk_sections::rust_hunk_source_with_allocation(&source(STORES), 8);
    assert_eq!(hunk::allocation(&bytes).unwrap().bss, 8);
    for opcode in [
        [0x33, 0xc5],
        [0x33, 0xc4],
        [0x33, 0xc3],
        [0x33, 0xc2],
        [0x33, 0xc1],
        [0x33, 0xc0],
    ] {
        assert!(bytes.windows(2).any(|word| word == opcode));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; exact Hunk relocation bases plus absolute addends"]
fn compact_address_addends_fs_uae() {
    hunk_sections::native_hunk_source_with_allocation(&source(STORES), 8);
}

const READS_AND_CONSTANTS: &str = r#" move.w Bytes+2,d4
 move.l #Bytes+2,d3
 move.w Payload+Offset,d2
 move.w entry+2,d1
 move.w 2+3*4,d0
 move.w d0,Offset*4
 move.w d0,Bytes+Offset*2
 move.w d0,Bytes-Offset-1
"#;

#[test]
fn compact_address_addend_controls_rust_oracle() {
    hunk_sections::rust_hunk_source_with_allocation(&source(READS_AND_CONSTANTS), 8);
}

#[test]
#[ignore = "requires configured FS-UAE; address-addend loads, immediate and absolute-only algebra"]
fn compact_address_addend_controls_fs_uae() {
    hunk_sections::native_hunk_source_with_allocation(&source(READS_AND_CONSTANTS), 8);
}

#[test]
#[ignore = "requires configured FS-UAE; multiple bases and unsafe address algebra fail closed"]
fn compact_address_addend_unsafe_rejection_fs_uae() {
    for expression in [
        "Bytes+Bytes",
        "Bytes+Payload",
        "Bytes*2",
        "2-Bytes",
        "-Bytes",
    ] {
        let source = source(&format!(" move.w d4,{expression}\n"));
        let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
        let resolved = core.resolve_pipeline("m68020", None).unwrap();
        let package = prepare_package(&core, &resolved).unwrap();
        let result = crate::fs_uae_smoke::run_compact_cli_files_from_env(
            &workspace_root(),
            &package,
            &[("input.asm", source.as_bytes())],
            &[],
            &[],
            None,
            false,
        )
        .expect("fresh completed compact CLI Hunk rejection");
        let FsUaeSmokeOutcome::Completed { runs } = result else {
            panic!("real FS-UAE execution required");
        };
        assert_eq!(runs.len(), 1);
        assert!(runs[0].protocol_completed);
        assert_eq!(runs[0].exit_code, Some(20));
        let diagnostic = runs[0]
            .captured_artifacts
            .iter()
            .filter(|(path, _)| {
                matches!(
                    path.file_name().and_then(|name| name.to_str()),
                    Some("opforge_fsuae_smoke.stdout" | "opforge_fsuae_smoke.stderr")
                )
            })
            .map(|(_, bytes)| String::from_utf8_lossy(bytes).into_owned())
            .collect::<Vec<_>>()
            .join("\n");
        assert!(
            diagnostic.contains("[file 00000001, line 0000000C]"),
            "{expression}: {diagnostic}"
        );
    }
}

const FORWARD: &str = r#".module probe
.cpu m68020
Offset=2
.section code,kind=code
entry
 move.w d4,Bytes+2
 move.l #Payload+Offset,d3
 rts
.endsection
.section bss,kind=bss
Bytes .res byte,8
.endsection
.section data,kind=data
Payload .long 0,0
.endsection
.output "build/sections.hunk",format=hunk,sections=code,data,bss
.endmodule
"#;

#[test]
fn compact_address_addend_forward_rust_oracle() {
    hunk_sections::rust_hunk_source_with_allocation(FORWARD, 8);
}

#[test]
#[ignore = "requires configured FS-UAE; unresolved forward BSS and DATA bases with addends"]
fn compact_address_addend_forward_fs_uae() {
    hunk_sections::native_hunk_source_with_allocation(FORWARD, 8);
}
