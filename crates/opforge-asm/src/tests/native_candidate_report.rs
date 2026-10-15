//! Independent conservation checks for passive acquisition attribution.
use std::collections::BTreeMap;

pub(super) fn assert_report(rows: &BTreeMap<u32, [u32; 3]>) {
    for phase in (400000..=400003).chain(400010..=400039) {
        assert!(rows.contains_key(&phase), "missing candidate phase {phase}");
    }
    assert_eq!(rows[&400000], [1, 0, 532], "candidate schema/errors/state");
    let [created, plans, duplicates] = rows[&400001];
    assert_eq!(duplicates, 0, "no duplicate physical replay");
    assert!(
        rows.range(400000..)
            .all(|(&phase, _)| (400000..=400003).contains(&phase)
                || (400010..=400039).contains(&phase)),
        "unexpected candidate detail"
    );
    // Native order: total, non-macro, never-invoked macro, invoked macro,
    // retained unknown, not retained.
    let vectors: Vec<[u64; 15]> = (0..6)
        .map(|group| {
            std::array::from_fn(|field| {
                u64::from(rows[&(400010 + group * 5 + field as u32 / 3)][field % 3])
            })
        })
        .collect();
    let total = vectors[0];
    for field in (0..=10).chain(13..=14) {
        assert_eq!(
            total[field],
            vectors[1..].iter().map(|v| v[field]).sum::<u64>(),
            "candidate category conservation field {field}"
        );
    }
    let ticks = |v: &[u64; 15]| (u128::from(v[11]) << 32) | u128::from(v[12]);
    assert_eq!(
        ticks(&total),
        vectors[1..].iter().map(ticks).sum::<u128>(),
        "candidate clock conservation"
    );
    for vector in &vectors {
        assert_eq!(&vector[13..], &[0, 0], "candidate reserved words");
        assert!(vector[10] <= vector[0], "at most one sample per record");
        if vector[10] == 0 {
            assert_eq!(ticks(vector), 0, "no clock ticks without samples");
        }
    }
    assert_eq!(total[0], u64::from(created), "all created records released");
    assert_eq!(total[2], u64::from(plans), "all created plans released");
    assert_eq!(
        (total[0] - vectors[5][0]) * 16,
        u64::from(rows[&2003][2]),
        "work capture count"
    );
    for (field, phase, word) in [
        (1, 2014, 1),
        (2, 2013, 2),
        (4, 2013, 0),
        (5, 2013, 1),
        (6, 2014, 2),
        (7, 2015, 0),
        (8, 2015, 1),
        (9, 2015, 2),
    ] {
        assert_eq!(
            if field < 6 {
                total[field] - vectors[5][field]
            } else {
                total[field]
            },
            u64::from(rows[&phase][word]),
            "candidate/work metric {field}"
        );
    }
    let [frequency, period, seed] = rows[&400002];
    assert!(
        [64, 128, 256].contains(&period) && seed < period,
        "sampling configuration"
    );
    let [calls, samples, unassigned] = rows[&400003];
    assert_eq!(u64::from(samples), total[10], "released samples");
    assert!(samples <= calls && calls <= created);
    assert!(
        u64::from(unassigned) <= vectors[4][0],
        "unreplayed records are unknown"
    );
    if frequency == 0 {
        assert_eq!(samples, 0, "untimed report has no samples");
    }
}

#[test]
fn candidate_report_conserves_empty_and_unassigned_records() {
    let mut rows = BTreeMap::from([
        (400000, [1, 0, 532]),
        (400001, [0; 3]),
        (400002, [0, 64, 0]),
        (400003, [0; 3]),
        (2003, [0; 3]),
    ]);
    for phase in (400010..=400039).chain(2010..=2015) {
        rows.insert(phase, [0; 3]);
    }
    assert_report(&rows);
    rows.insert(400001, [1, 0, 0]);
    rows.insert(400003, [0, 0, 1]);
    rows.insert(400010, [1, 72, 0]);
    rows.insert(400030, [1, 72, 0]);
    rows.insert(2003, [0, 72, 16]);
    rows.insert(2014, [0, 72, 0]);
    assert_report(&rows);
    // A temporary acquisition contributes gross work but no retained capture.
    rows.insert(400001, [2, 4, 0]);
    rows.insert(400010, [2, 152, 4]);
    rows.insert(400011, [5, 2, 1]);
    rows.insert(400012, [1, 2, 3]);
    rows.insert(400013, [4, 0, 0]);
    rows.insert(400035, [1, 80, 4]);
    rows.insert(400036, [5, 2, 1]);
    rows.insert(400037, [1, 2, 3]);
    rows.insert(400038, [4, 0, 0]);
    rows.insert(2014, [0, 72, 1]);
    rows.insert(2015, [2, 3, 4]);
    assert_report(&rows);
    for (phase, word) in [
        (400000, 0),
        (400000, 1),
        (400001, 2),
        (400030, 0),
        (400039, 2),
        (2014, 1),
    ] {
        let mut bad = rows.clone();
        bad.get_mut(&phase).unwrap()[word] += 1;
        assert!(std::panic::catch_unwind(|| assert_report(&bad)).is_err());
    }
}
