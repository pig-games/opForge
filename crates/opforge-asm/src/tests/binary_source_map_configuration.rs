//! Configuration maps survive dependency replay without borrowing scope identities.
use super::*;

const MAIN: &str = ".module main\n.cpu m6502\nBASE=$20\n.region rom,$1000,$10ff\n.use dep (entry) as d with (VALUE=BASE+1) map { code -> app_code }\n.section app_code\n.byte BASE\n.word d.entry\n.endsection\n.place app_code in rom\n.endmodule\n.end\n";
const DEP: &str = ".module dep\n.cpu m6502\n.pub\n.section code,logical\n.byte $b0\nentry .block\n.byte VALUE\n.bend\n.byte $b1\n.endsection\n.endmodule\n.end\n";

fn files(main: &str) -> [(&str, &str); 2] {
    [("main.asm", main), ("library/dep.asm", DEP)]
}

fn inactive_maps() -> String {
    MAIN.replace(".region rom", ".if 0\n.use absent_a map { missing_a -> unused_a }\n.use absent_b map { missing_b -> unused_b }\n.endif\n.region rom")
}

fn late_map() -> String {
    let import = ".use dep (entry) as d with (VALUE=BASE+1) map { code -> app_code }\n";
    MAIN.replace(import, "")
        .replace(".place app_code", &format!("{import}.place app_code"))
}

fn late_second_map() -> String {
    let import = ".use dep_b (entry) as right map { code_b -> app_b }\n";
    TWO_MAPPED_REGIONS[0]
        .1
        .replace(import, "")
        .replace(".section app_b", &format!("{import}.section app_b"))
}

fn two_map_files(main: &str) -> [(&str, &str); 3] {
    [
        ("main.asm", main),
        TWO_MAPPED_REGIONS[1],
        TWO_MAPPED_REGIONS[2],
    ]
}

#[test]
fn map_configuration_rust_oracles() {
    for main in [MAIN.to_string(), inactive_maps(), late_map()] {
        assert_eq!(
            oracle_with_roots(&files(&main), &["library"]).unwrap(),
            [0x20, 0x04, 0x10, 0xb0, 0x21, 0xb1]
        );
    }
    assert_eq!(
        oracle_address_ordered_with_roots(&two_map_files(&late_second_map()), &["library"])
            .unwrap(),
        [0xa0, 0x04, 0x10, 0xb0, 0x10, 0xa1, 0x09, 0x10, 0xc0, 0x20]
    );
}

#[test]
#[ignore = "fresh native transfer of maps and scalar import parameters"]
fn compact_map_configuration_parameters_fs_uae() {
    let expected = oracle_with_roots(&files(MAIN), &["library"]).unwrap();
    compact_cli(&files(MAIN), &["library"], &[], Some(&expected), false);
}

#[test]
#[ignore = "fresh native inactive imports must not consume transferred map slots"]
fn compact_map_configuration_inactive_fs_uae() {
    let main = inactive_maps();
    let expected = oracle_with_roots(&files(&main), &["library"]).unwrap();
    compact_cli(&files(&main), &["library"], &[], Some(&expected), false);
}

#[test]
#[ignore = "fresh native retains its unsupported late section-map boundary"]
fn compact_map_configuration_late_fs_uae() {
    let main = late_map();
    assert!(oracle_with_roots(&files(&main), &["library"]).is_ok());
    compact_cli(&files(&main), &["library"], &[], None, false);
}

#[test]
#[ignore = "fresh native all maps from an importer precede its concrete sections"]
fn compact_map_configuration_late_second_fs_uae() {
    let main = late_second_map();
    assert!(oracle_address_ordered_with_roots(&two_map_files(&main), &["library"]).is_ok());
    compact_cli(&two_map_files(&main), &["library"], &[], None, false);
}
