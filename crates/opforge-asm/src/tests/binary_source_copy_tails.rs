//! Capture/replay preserves uneven strings, identifier lengths and byte order.
use super::*;

const SOURCE: &str = r#".module app
.cpu m68020

; Empty and comment-only lines also pass through capture.
a = $12345678
ab = $90abcdef
abc = 3
abcd = 4
abcde = 5
abcdef = 6
abcdefg = 7
abcdefgh = 8
.long a,ab
.byte abc,abcd,abcde,abcdef,abcdefg,abcdefgh
.word 'AB'
.byte "1","12","123","1234","12345","123456","1234567","12345678"
.endmodule
"#;

const EXPECTED: &[u8] = b"\x12\x34\x56\x78\x90\xab\xcd\xef\x03\x04\x05\x06\x07\x08AB112123123412345123456123456712345678";

#[test]
fn compact_copy_tails_rust_oracle() {
    assert_eq!(oracle(&[("main.asm", SOURCE)]).unwrap(), EXPECTED);
}

#[test]
#[ignore = "requires configured FS-UAE; owned-record copy tails and endian conversion"]
fn compact_copy_tails_fs_uae() {
    let expected = oracle(&[("main.asm", SOURCE)]).unwrap();
    assert_eq!(expected, EXPECTED);
    compact_cli_cpu(
        &[("main.asm", SOURCE)],
        &[],
        &[],
        Some(&expected),
        false,
        "m68020",
    );
}
