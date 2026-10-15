//! Optional copied source paths; scalar origin identity remains authoritative.
use std::collections::{BTreeMap, BTreeSet};

pub(super) fn assert_report(
    rows: &BTreeMap<u32, [u32; 3]>,
    definitions: &[[u32; 24]],
    edges: &[[u32; 9]],
) {
    if rows.range(34999..182456).next().is_none() {
        return;
    }
    assert_eq!(
        rows[&34999],
        [1, definitions.len() as u32, edges.len() as u32],
        "source path dictionary schema/counts"
    );
    let mut expected = BTreeSet::from([34999]);
    for (slot, origin) in definitions
        .iter()
        .map(|row| row[5])
        .chain(edges.iter().map(|edge| edge[2]))
        .enumerate()
    {
        let start = 35000 + slot as u32 * 24;
        let [reported_origin, length, ordinal] = rows[&start];
        assert_eq!(reported_origin, origin, "source path origin");
        assert_eq!(ordinal, slot as u32 + 1, "source path slot");
        assert!(length < 256 && (origin != 0 || length == 0));
        let chunks = length.div_ceil(12);
        expected.extend(start..start + 1 + chunks);
        let bytes: Vec<_> = (0..chunks)
            .flat_map(|part| {
                rows[&(start + 1 + part)]
                    .into_iter()
                    .flat_map(u32::to_be_bytes)
            })
            .collect();
        assert!(
            bytes[length as usize..].iter().all(|byte| *byte == 0),
            "source path padding"
        );
        std::str::from_utf8(&bytes[..length as usize]).expect("source path UTF-8");
    }
    assert!(
        rows.range(34999..182456)
            .all(|(phase, _)| expected.contains(phase)),
        "unexpected source path chunk"
    );
}

#[test]
fn optional_template_source_paths_validate_provenance_and_bounded_utf8() {
    let mut definition = [0; 24];
    definition[5] = 9;
    let mut edge = [0; 9];
    edge[2] = 9;
    assert_report(&BTreeMap::new(), &[definition], &[edge]);
    let path = "src/é.asm".as_bytes();
    let mut chunk = [0; 12];
    chunk[..path.len()].copy_from_slice(path);
    let rows = BTreeMap::from([
        (34999, [1, 1, 1]),
        (35000, [9, path.len() as u32, 1]),
        (
            35001,
            std::array::from_fn(|index| {
                u32::from_be_bytes(chunk[index * 4..index * 4 + 4].try_into().unwrap())
            }),
        ),
        (35024, [9, 0, 2]), // A path may be unavailable for a known origin.
    ]);
    assert_report(&rows, &[definition], &[edge]);
    let mut with_candidates = rows.clone();
    with_candidates.insert(400000, [1, 0, 532]);
    assert_report(&with_candidates, &[definition], &[edge]);
    assert_report(
        &BTreeMap::from([(400000, [1, 0, 532])]),
        &[definition],
        &[edge],
    );
    for (phase, field, value) in [
        (34999, 0, 2),
        (34999, 1, 0),
        (35000, 0, 8),
        (35000, 1, 256),
        (35000, 2, 2),
        (35001, 2, 1),
        (35001, 0, 0xff000000),
    ] {
        let mut bad = rows.clone();
        bad.get_mut(&phase).unwrap()[field] = value;
        assert!(std::panic::catch_unwind(|| assert_report(&bad, &[definition], &[edge])).is_err());
    }
    for phase in [34999, 35001, 35024] {
        let mut bad = rows.clone();
        bad.remove(&phase);
        assert!(std::panic::catch_unwind(|| assert_report(&bad, &[definition], &[edge])).is_err());
    }
    let mut bad = rows.clone();
    bad.insert(35025, [0; 3]);
    assert!(std::panic::catch_unwind(|| assert_report(&bad, &[definition], &[edge])).is_err());
    let mut bad = rows.clone();
    bad.insert(35000, [0, path.len() as u32, 1]);
    definition[5] = 0;
    assert!(std::panic::catch_unwind(|| assert_report(&bad, &[definition], &[edge])).is_err());
}
