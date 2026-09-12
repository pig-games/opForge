// SPDX-License-Identifier: GPL-3.0-or-later

use super::*;
use proptest::prelude::*;

// A complete CMSE chunk with a string table and no selectors. Version 2+
// strings use a zero-length prefix here, so arbitrary wire orders remain valid.
fn string_table(version: u16, strings: &[&str]) -> Vec<u8> {
    let mut bytes = Vec::new();
    bytes.extend_from_slice(&version.to_le_bytes());
    bytes.extend_from_slice(&0u16.to_le_bytes()); // owners
    bytes.extend_from_slice(&(strings.len() as u16).to_le_bytes());
    for string in strings {
        if version >= 2 {
            bytes.extend_from_slice(&0u16.to_le_bytes());
        }
        bytes.extend_from_slice(&(string.len() as u32).to_le_bytes());
        bytes.extend_from_slice(string.as_bytes());
    }
    bytes.extend_from_slice(&0u32.to_le_bytes()); // selectors
    bytes
}

fn is_duplicate(result: Result<Vec<ModeSelectorDescriptor>, OpcpuCodecError>) -> bool {
    matches!(result, Err(OpcpuCodecError::InvalidChunkFormat { chunk, detail })
        if chunk == "CMSE" && detail == "duplicate string table entry")
}

#[test]
fn uniqueness_is_exact_and_independent_of_wire_order() {
    for version in 1..=7 {
        for strings in [vec!["z", "a", "A", "é", "É", ""], vec!["", "a", "z"]] {
            assert!(decode_compact_msel_chunk(&string_table(version, &strings)).is_ok());
        }
        assert!(is_duplicate(decode_compact_msel_chunk(&string_table(
            version,
            &["z", "a", "z"]
        ))));
    }
}

#[test]
fn duplicate_error_precedes_later_truncated_string() {
    let mut bytes = string_table(2, &["same", "same"]);
    bytes[4..6].copy_from_slice(&3u16.to_le_bytes());
    bytes.truncate(bytes.len() - 4);
    assert!(is_duplicate(decode_compact_msel_chunk(&bytes)));
}

#[test]
fn reconstructed_prefix_strings_are_checked_for_duplicates() {
    let mut bytes = string_table(2, &["abc", ""]);
    // Second entry reproduces all of the previous string with no suffix.
    bytes[15..17].copy_from_slice(&3u16.to_le_bytes());
    assert!(is_duplicate(decode_compact_msel_chunk(&bytes)));
}

proptest! {
    #![proptest_config(ProptestConfig { cases: 128, .. ProptestConfig::default() })]

    #[test]
    fn decoded_table_matches_original_linear_uniqueness_rule(
        version in 1u16..=7,
        strings in prop::collection::vec("[a-zA-ZéÉ]{0,8}", 0..80),
    ) {
        let has_duplicate = strings.iter().enumerate()
            .any(|(index, value)| strings[..index].contains(value));
        let refs: Vec<_> = strings.iter().map(String::as_str).collect();
        let result = decode_compact_msel_chunk(&string_table(version, &refs));
        if has_duplicate {
            prop_assert!(is_duplicate(result));
        } else {
            prop_assert!(result.is_ok());
        }
    }
}
