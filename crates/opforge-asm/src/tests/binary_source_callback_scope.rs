//! Private forward callbacks using an aliased public frame from another module.
use super::*;
use vm::binary_source_package::BinarySourcePackage;

const SOURCE: &str = r#".module experimental.amigaos.binary_app
.cpu 68020
.use experimental.amigaos.binary_assembly as assembly
.section code,kind=code
.pub
entry .block
 bsr.w run
 rts
.bend
.priv
run .block
 move.l #Context,assembly.Frame.Context(a0)
 move.l #allocateOutput,assembly.Frame.Allocate(a0)
 move.l #appendReloc,assembly.Frame.AddReloc(a0)
 move.l #payload,assembly.Frame.Output(a0)
 rts
.bend
allocateOutput .block
 rts
.bend
appendReloc .block
 rts
.bend
.align 4
.endsection
.section bss,kind=bss
.priv
Context .res long,3
.endsection
.section data,kind=data
payload .byte $aa,$bb,$cc
.endsection
.output "build/sections.hunk",format=hunk,sections=code,bss,data
.endmodule
.module experimental.amigaos.binary_assembly
.cpu 68020
.pub
Frame .struct
Records .long ?
RecordBytes .long ?
Context .long ?
Output .long ?
Capacity .long ?
Used .long ?
Line .word ?
Reserved .word ?
Allocate .long ?
RecordOffset .long ?
Sections .long ?
AddReloc .long ?
.endstruct
.endmodule
"#;

fn assert_callback_bytes(oracle: &[u8]) {
    assert!(oracle.windows(32).any(|bytes| bytes
        == [
            0x21, 0x7c, 0, 0, 0, 0, 0, 8, 0x21, 0x7c, 0, 0, 0, 40, 0, 28, 0x21, 0x7c, 0, 0, 0, 42,
            0, 40, 0x21, 0x7c, 0, 0, 0, 0, 0, 12,
        ]));
}

fn callback_isolation_source() -> String {
    SOURCE.replace(" bsr.w run\n", " .long run\n")
}

#[test]
fn compact_callback_private_branch_scope_rust_hunk_oracle() {
    assert_callback_bytes(&hunk_sections::rust_hunk_source(SOURCE));
}

#[test]
fn compact_callback_private_imported_scope_rust_hunk_oracle() {
    let oracle = hunk_sections::rust_hunk_source(&callback_isolation_source());
    assert_callback_bytes(&oracle);
    assert!(
        oracle
            .chunks_exact(4)
            .map(|bytes| u32::from_be_bytes(bytes.try_into().unwrap()))
            .collect::<Vec<_>>()
            .windows(6)
            .any(|words| words == [0x3ec, 3, 0, 0, 16, 24]),
        "CODE entry and both callback addresses must have relocation records"
    );
}

#[test]
#[ignore = "requires configured FS-UAE; private forward branch and callback Hunk parity"]
fn compact_callback_private_branch_scope_fs_uae() {
    hunk_sections::native_hunk_source(SOURCE);
}

#[test]
#[ignore = "requires configured FS-UAE; private forward callbacks and aliased imported frame"]
fn compact_callback_private_imported_scope_fs_uae() {
    hunk_sections::native_hunk_source(&callback_isolation_source());
}

fn branch_constant_source() -> String {
    SOURCE
        .replace(".cpu 68020\n", ".cpu 68020\nBranchLimit=6\n")
        .replace(" bsr.w run\n", " bsr.w BranchLimit\n")
}

fn branch_numeric_source() -> String {
    SOURCE.replace(" bsr.w run\n", " bsr.w 6\n")
}

#[test]
fn compact_branch_numeric_and_constant_rust_hunk_oracle() {
    let symbolic = hunk_sections::rust_hunk_source(SOURCE);
    for source in [branch_constant_source(), branch_numeric_source()] {
        assert_eq!(hunk_sections::rust_hunk_source(&source), symbolic);
    }
}

#[test]
#[ignore = "requires configured FS-UAE; numeric and absolute-constant branch targets"]
fn compact_branch_numeric_and_constant_fs_uae() {
    hunk_sections::native_hunk_source(&branch_numeric_source());
    hunk_sections::native_hunk_source(&branch_constant_source());
}

#[test]
#[ignore = "requires configured FS-UAE; cross-section branch cannot cancel relocation"]
fn compact_branch_cross_section_rejection_fs_uae() {
    let source = SOURCE.replace(" bsr.w run\n", " bsr.w payload\n");
    assert_native_files_rejection(
        &[("input.asm", &source)],
        "m68020",
        Some("[file 00000001, line 00000007]"),
    );
}

#[test]
#[ignore = "requires configured FS-UAE; compound branch targets lack exact identity transport"]
fn compact_branch_compound_target_rejection_fs_uae() {
    for target in ["run+0", "run+allocateOutput"] {
        let source = SOURCE.replace(" bsr.w run\n", &format!(" bsr.w {target}\n"));
        assert_native_files_rejection(
            &[("input.asm", &source)],
            "m68020",
            Some("[file 00000001, line 00000007]"),
        );
    }
}

fn branch_wire() -> (Vec<u8>, usize) {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let wire = prepare_package(&core, &resolved).unwrap();
    let name = numeric.names.iter().position(|name| name == "bsr").unwrap() as u16;
    let qualifier = numeric
        .qualifiers
        .iter()
        .position(|name| name == "w")
        .unwrap() as u8
        + 1;
    let rows = u32::from_be_bytes(wire[16..20].try_into().unwrap()) as usize;
    let count = u32::from_be_bytes(wire[20..24].try_into().unwrap()) as usize;
    let row = (0..count)
        .map(|index| rows + index * 32)
        .find(|&row| {
            u16::from_be_bytes(wire[row..row + 2].try_into().unwrap()) == name
                && wire[row + 2] == qualifier
                && wire[row + 3] == 1
                && u16::from_be_bytes(wire[row + 6..row + 8].try_into().unwrap()) == 0
        })
        .unwrap();
    assert_eq!(wire[row + 5], 5);
    let inputs = u32::from_be_bytes(wire[row + 12..row + 16].try_into().unwrap()) as usize;
    (wire, inputs + 12)
}

#[test]
fn compact_branch_package_binds_optional_exact_identity() {
    let (wire, target) = branch_wire();
    assert_eq!(&wire[..4], b"BS15");
    assert_eq!(&wire[target..target + 2], &[0, 0]);
    assert_eq!(
        u16::from_be_bytes(wire[target + 10..target + 12].try_into().unwrap()),
        1
    );
    assert_eq!(
        u16::from_be_bytes(wire[target - 2..target].try_into().unwrap()),
        0,
        "opcode input must not request target identity"
    );
}

fn reject_wire(wire: &[u8], source: &str, origin: Option<&str>) {
    let result = crate::fs_uae_smoke::run_binary_source_rejection_from_env(
        &workspace_root(),
        wire,
        &[("input.asm", source.as_bytes())],
        origin,
    )
    .expect("fresh native rejection for invalid scalar identity metadata");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("real FS-UAE execution required");
    };
    assert_eq!(runs.len(), 1);
}

#[test]
#[ignore = "requires configured FS-UAE; superseded package magic must reject"]
fn compact_branch_superseded_contract_rejection_fs_uae() {
    let (mut wire, _) = branch_wire();
    wire[..4].copy_from_slice(b"BS10");
    reject_wire(&wire, &callback_isolation_source(), None);
}

#[test]
#[ignore = "requires configured FS-UAE; unknown scalar flags and non-target slots must reject"]
fn compact_branch_scalar_flags_rejection_fs_uae() {
    let (wire, target) = branch_wire();
    for (field, flags) in [(target + 10, 2u16), (target - 2, 1u16)] {
        let mut invalid = wire.clone();
        invalid[field..field + 2].copy_from_slice(&flags.to_be_bytes());
        reject_wire(&invalid, SOURCE, Some("[file 00000001, line 00000007]"));
    }
}

#[test]
#[ignore = "requires configured FS-UAE; exact identity flag is invalid in non-branch projections"]
fn compact_branch_nonbranch_identity_flag_rejection_fs_uae() {
    let core = RuntimeModelCore::from_registry(&default_registry()).unwrap();
    let resolved = core.resolve_pipeline("m68020", None).unwrap();
    let numeric = BinarySourcePackage::prepare(&core, &resolved).unwrap();
    let mut wire = prepare_package(&core, &resolved).unwrap();
    let name = numeric
        .names
        .iter()
        .position(|name| name == "move")
        .unwrap() as u16;
    let qualifier = numeric
        .qualifiers
        .iter()
        .position(|name| name == "l")
        .unwrap() as u8
        + 1;
    let rows = u32::from_be_bytes(wire[16..20].try_into().unwrap()) as usize;
    let count = u32::from_be_bytes(wire[20..24].try_into().unwrap()) as usize;
    let row = (0..count)
        .map(|index| rows + index * 32)
        .find(|&row| {
            u16::from_be_bytes(wire[row..row + 2].try_into().unwrap()) == name
                && wire[row + 2] == qualifier
                && wire[row + 3] == 8
                && u16::from_be_bytes(wire[row + 6..row + 8].try_into().unwrap()) == 115
        })
        .unwrap();
    let stage = u32::from_be_bytes(wire[row + 12..row + 16].try_into().unwrap()) as usize;
    let input = u32::from_be_bytes(wire[stage + 8..stage + 12].try_into().unwrap()) as usize;
    assert_eq!(wire[input], 0);
    wire[input + 10..input + 12].copy_from_slice(&1u16.to_be_bytes());
    let source = callback_isolation_source().replace("#Context", "#8");
    reject_wire(&wire, &source, Some("[file 00000001, line 0000000C]"));
}
