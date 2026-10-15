//! Shared whole-block, relocation, guard and placement controls.
use super::native_iterable_loops::scratch;
use super::*;
use clap::Parser;
use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

pub(super) fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

pub(super) fn source(guard: u8, placement: u32) -> String {
    format!(
        r#".module main
.cpu 68020
.use dep
.region ram, ${placement:x}, ${end:x}
.section code,kind=code
.long dep.helper
.long dep.entry.inside
.long dep.payload
.endsection
.section data,kind=data
.byte $77
.endsection
.place code in ram
.place data in ram
.output "build/fragment.bin",format=bin,sections=code,data
.output "build/fragment.hunk",format=hunk,sections=code,data
.endmodule
.module dep
.cpu 68020
.pub
.section code,kind=code
entry .block
.byte $11
inside:
.long helper
.if forward
.byte $31
.else
.byte $32
.endif
.for item in {{2,4}}
.byte item
.endfor
.byte $22
.bend
helper .block
.byte $44
.bend
unused .block
.byte $99
.bend
forward = {guard}
.endsection
.section data,kind=data
payload .byte $55
.endsection
.endmodule
.module empty
.cpu 68020
.endmodule
"#,
        end = placement + 0xff
    )
}

fn oracle(source: &str) -> (Vec<u8>, Vec<u8>) {
    let dir = scratch();
    fs::create_dir_all(dir.0.join("build")).unwrap();
    let input = dir.0.join("entry.asm");
    let output = dir.0.join("build/fragment.bin");
    fs::write(&input, source).unwrap();
    let cli = Cli::parse_from(["opforge".into(), input.to_string_lossy().into_owned()]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.0.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap();
    (
        fs::read(output).unwrap(),
        fs::read(dir.0.join("build/fragment.hunk")).unwrap(),
    )
}

#[test]
fn native_whole_block_live_rust_controls() {
    for guard in [0, 1] {
        for placement in [0x1000, 0x2000] {
            let (_, hunk) = oracle(&source(guard, placement));
            let segments = hunk::segments(&hunk).unwrap();
            assert_eq!(segments.len(), 2);
            assert!(segments[0].payload.contains(&0x11));
            assert!(segments[0].payload.contains(&0x22));
            assert!(segments[0].payload.contains(&0x44));
            assert!(!segments[0].payload.contains(&0x99));
            assert!(segments[0]
                .payload
                .contains(&(if guard == 1 { 0x31 } else { 0x32 })));
            assert_eq!(segments[0].relocations.len(), 4);
        }
    }
}
