//! Public address labels must resolve through the same canonical import path as values.
use super::*;

const CONSUMER: &str = ".module consumer\n.cpu m68020\n.use example.state\nRead .block\n move.w state.Flag,d0\n move.w state.Tail,d1\n rts\n.bend\n.word Read\n.endmodule\n";
const OWNER: &str =
    ".module example.state\n.cpu m68020\n.pub\nFlag\n.word 0\nTail\n.word 0\n.endmodule\n";
const CONSTANT_OWNER: &str =
    ".module example.state\n.cpu m68020\n.pub\nFlag = 7\nTail = 9\n.endmodule\n";

fn cases(owner: &str) -> Vec<Vec<(&'static str, String)>> {
    vec![
        vec![("main.asm", format!("{CONSUMER}{owner}"))],
        vec![("main.asm", format!("{owner}{CONSUMER}"))],
        vec![
            ("main.asm", CONSUMER.into()),
            ("library/state.asm", owner.into()),
        ],
    ]
}

#[test]
fn compact_imported_public_address_labels_rust_oracles() {
    for owner in [OWNER, CONSTANT_OWNER] {
        let mut first = None;
        for sources in cases(owner) {
            let files: Vec<_> = sources
                .iter()
                .map(|(name, text)| (*name, text.as_str()))
                .collect();
            let roots: &[&str] = if files.len() == 1 { &[] } else { &["library"] };
            let bytes = oracle_with_roots(&files, roots).unwrap();
            assert!(!bytes.is_empty());
            if let Some(expected) = &first {
                assert_eq!(
                    &bytes, expected,
                    "physical source order must not change bindings"
                );
            } else {
                first = Some(bytes);
            }
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; public bare labels through implicit module qualifier"]
fn compact_imported_public_address_labels_fs_uae() {
    for owner in [OWNER, CONSTANT_OWNER] {
        for sources in cases(owner) {
            let files: Vec<_> = sources
                .iter()
                .map(|(name, text)| (*name, text.as_str()))
                .collect();
            let roots: &[&str] = if files.len() == 1 { &[] } else { &["library"] };
            let expected = oracle_with_roots(&files, roots).unwrap();
            compact_cli_cpu(&files, roots, &[], Some(&expected), false, "m68020");
        }
    }
}

// Keep the owner/import/section structure while omitting tokenizer VM behavior.
const SECTIONED_HUNK: &str = r#".module example.entry
.cpu m68020
.use example.control
.section entry,kind=code
 jsr control.Read
 rts
.endsection
.section bss,kind=bss
Scratch
.res byte,4
.endsection
.output "build/sections.hunk",format=hunk,sections=entry,code,data,bss
.endmodule
.module example.control
.cpu m68020
.pub
.use example.program
.use example.state
.section code,kind=code
.pub
Read .block
 moveq #0,d0
 move.w state.StatusKind,d0
 moveq #0,d1
 move.w state.StatusOperand,d1
 rts
.bend
.endsection
.endmodule
.module example.state
.cpu m68020
.pub
.use example.program
DEFAULT_BUDGET = 2048
.section data,kind=data
.pub
Budget
.long DEFAULT_BUDGET
ProgramTable
.long program.Table
ProgramCount
.long 1
Start
.word 0
StatusKind
.word 0
StatusOperand
.word 0
.endsection
.endmodule
.module example.program
.cpu m68020
.pub
.section data,kind=data
.pub
Table
.long 0
.endsection
.endmodule
"#;

fn sectioned_hunk_source(real_state_names: bool) -> String {
    let mut source = SECTIONED_HUNK.to_owned();
    if real_state_names {
        // One imported ordinary macro enables dotted-head template probing.
        // Repeated .word probes must preserve the names interned between them.
        source = source.replace(
            "\nTable\n",
            "\nProbe .macro value\n.byte .value\n.endmacro\nTable\n",
        );
        // Keep the exact owner prefix length and public state-label shape.
        for (old, new) in [
            ("example.entry", "tkvm.fixture.entry"),
            ("example.control", "tkvm.amigaos.control"),
            ("example.state", "tkvm.amigaos.state"),
            ("example.program", "tkvm.amigaos.demo_program"),
            ("program.Table", "demo_program.DemoStateEntryOffsets"),
            ("\nTable\n", "\nDemoStateEntryOffsets\n"),
            ("DEFAULT_BUDGET", "TKVM_DEFAULT_MAX_STEPS_PER_LINE"),
            ("\nBudget\n", "\nTkvmStepBudget\n"),
            ("\nProgramTable\n", "\nTkvmProgramStateTablePtr\n"),
            ("\nProgramCount\n", "\nTkvmProgramStateCount\n"),
            ("\nStart\n", "\nTkvmProgramStartState\n"),
            ("StatusKind", "TkvmLastFailureKind"),
            ("StatusOperand", "TkvmLastFailureOperand"),
        ] {
            source = source.replace(old, new);
        }
    }
    source
}

fn sectioned_hunk_oracle(source: &str) -> Vec<u8> {
    let oracle = hunk_sections::rust_hunk_bytes(source);
    let allocation = hunk::allocation(&oracle).expect("valid four-module Rust Hunk");
    assert_eq!(allocation.segments, 4);
    assert_eq!(allocation.bss, 4);
    oracle
}

#[test]
fn compact_imported_public_address_labels_sections_rust_hunk_oracle() {
    let control = sectioned_hunk_oracle(&sectioned_hunk_source(false));
    assert!(!control.is_empty());
    assert_eq!(
        sectioned_hunk_oracle(&sectioned_hunk_source(true)),
        control,
        "names and unused macro must preserve every output Hunk byte"
    );
}

#[test]
#[ignore = "requires configured FS-UAE; sectioned public labels and nested ordinary import"]
fn compact_imported_public_address_labels_sections_fs_uae() {
    for real_state_names in [false, true] {
        let source = sectioned_hunk_source(real_state_names);
        let oracle = sectioned_hunk_oracle(&source);
        compact_cli_cpu(
            &[("main.asm", &source)],
            &[],
            &[],
            Some(&oracle),
            false,
            "m68020",
        );
    }
}

fn identity_growth_source() -> String {
    // Dependency ordering prepares the owner and its consumer before padding.
    // The long names then grow both the entry and name arenas; capture storage
    // also crosses its one-MiB region boundary in this single physical file.
    let mut source = String::from(CONSUMER);
    source.push_str(".module padding\n.cpu m68020\n");
    let suffix = "X".repeat(210);
    for index in 0..3000 {
        let name = format!("PaddingIdentity_{index:04}_{suffix}");
        assert!("padding.".len() + name.len() < 255);
        source.push_str(&format!("{name} = {}\n", index % 256));
    }
    // Keep this entry-file module selected and observable in the output.
    source.push_str(".byte $5a\n.endmodule\n");
    source.push_str(OWNER);
    source
}

#[test]
fn compact_imported_public_address_labels_growth_rust_oracle() {
    let source = identity_growth_source();
    let expected = oracle_with_roots(&[("main.asm", &source)], &[]).unwrap();
    assert!(!expected.is_empty());
    assert_eq!(expected.last(), Some(&0x5a));
    eprintln!(
        "identity growth: {} source bytes, {} lines, 3 modules, 3000 padding constants",
        source.len(),
        source.lines().count()
    );
}

#[test]
#[ignore = "requires configured FS-UAE; imported labels across identity and capture arena growth"]
fn compact_imported_public_address_labels_growth_fs_uae() {
    let source = identity_growth_source();
    let files = [("main.asm", source.as_str())];
    let expected = oracle_with_roots(&files, &[]).unwrap();
    assert_eq!(expected.last(), Some(&0x5a));
    compact_cli_cpu(&files, &[], &[], Some(&expected), false, "m68020");
}

fn sectioned_discovery_sources(real_state_names: bool) -> Vec<(&'static str, String)> {
    let paths = [
        "main.asm",
        "library/legacy_control.asm",
        "library/tkvm_state.asm",
        "library/tkvm_program.asm",
    ];
    let source = sectioned_hunk_source(real_state_names);
    let units: Vec<_> = source.split_inclusive(".endmodule\n").collect();
    assert_eq!(units.len(), paths.len());
    let mut files: Vec<_> = paths
        .into_iter()
        .zip(units)
        .map(|(path, unit)| (path, unit.to_owned()))
        .collect();
    let mut decoy = String::from(
        ".module other.state\n.cpu m68020\n.pub\nStatusKind\n.word $ffff\nStatusOperand\n.word $ffff\n.endmodule\n",
    );
    if real_state_names {
        decoy = decoy
            .replace("StatusKind", "TkvmLastFailureKind")
            .replace("StatusOperand", "TkvmLastFailureOperand");
    }
    files.push(("library/other_state.asm", decoy));
    files
}

fn sectioned_discovery_hunk_oracle(files: &[(&str, &str)]) -> Vec<u8> {
    let dir = create_temp_dir("compact-imported-label-discovery-rust-oracle");
    fs::create_dir_all(dir.join("build")).expect("create Hunk output directory");
    for (name, source) in files {
        let path = dir.join(name);
        fs::create_dir_all(path.parent().unwrap()).expect("create source directory");
        fs::write(path, source).expect("write split Hunk source");
    }
    let cli = Cli::parse_from([
        "opForge".to_string(),
        dir.join(files[0].0).to_string_lossy().into_owned(),
        "--cpu".to_string(),
        "68020".to_string(),
        "-M".to_string(),
        dir.join("library").to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli).expect("validate split Rust Hunk oracle");
    config.out_dir = Some(dir.clone());
    run_with_validated_cli_with_context(&cli, &config).expect("assemble discovered Hunk sources");
    let oracle = fs::read(dir.join("build/sections.hunk")).expect("read split Rust Hunk oracle");
    fs::remove_dir_all(dir).expect("remove split Rust oracle directory");
    let allocation = hunk::allocation(&oracle).expect("valid discovered Rust Hunk");
    assert_eq!(allocation.segments, 4);
    assert_eq!(allocation.bss, 4);
    oracle
}

#[test]
fn compact_imported_public_address_labels_discovery_rust_hunk_oracle() {
    for real_state_names in [false, true] {
        let sources = sectioned_discovery_sources(real_state_names);
        let files: Vec<_> = sources
            .iter()
            .map(|(name, source)| (*name, source.as_str()))
            .collect();
        assert!(!sectioned_discovery_hunk_oracle(&files).is_empty());
    }
}

#[test]
#[ignore = "requires configured FS-UAE; explicit module discovery with mismatched stems and decoy"]
fn compact_imported_public_address_labels_discovery_fs_uae() {
    for real_state_names in [false, true] {
        let sources = sectioned_discovery_sources(real_state_names);
        let files: Vec<_> = sources
            .iter()
            .map(|(name, source)| (*name, source.as_str()))
            .collect();
        let oracle = sectioned_discovery_hunk_oracle(&files);
        compact_cli_cpu(&files, &["library"], &[], Some(&oracle), false, "m68020");
    }
}
