//! L1 report decoder. A deferred report is evidence, not retirement authorization.
use std::collections::BTreeMap;

pub(super) fn rows(stdout: &str) -> BTreeMap<u32, [u32; 3]> {
    let mut rows = BTreeMap::new();
    for line in stdout
        .lines()
        .filter(|line| line.starts_with("progress p="))
    {
        let fields: BTreeMap<_, _> = line
            .split_whitespace()
            .skip(1)
            .map(|field| {
                let (key, value) = field.split_once('=').expect("progress field");
                (
                    key,
                    u32::from_str_radix(value, 16).expect("hex progress word"),
                )
            })
            .collect();
        let phase = fields["p"];
        if (700..=1831).contains(&phase) {
            assert!(
                rows.insert(phase, [fields["f"], fields["l"], fields["r"]])
                    .is_none(),
                "duplicate audit phase {phase}"
            );
        }
    }
    rows
}

pub(super) fn assert_report(stdout: &str) -> BTreeMap<u32, [u32; 3]> {
    let rows = rows(stdout);
    for phase in (700..=708).chain(710..=714) {
        assert!(rows.contains_key(&phase), "missing audit phase {phase}");
    }
    assert_eq!(rows[&700][0], 1, "report schema");
    assert_eq!(rows[&700][1], 0, "observer errors");
    assert_eq!(
        rows[&700][2], 15,
        "acquisition, schedule, index and selection observed"
    );
    let [_, limit, count] = rows[&701];
    assert!(count > 0 && count <= limit);
    assert_eq!(
        rows[&703][0], rows[&703][1],
        "all scheduled spans completed"
    );
    assert_eq!(rows[&704][0], 0, "include pins balanced");
    assert!(rows[&705][2] <= rows[&706][0], "capture used/capacity");
    assert_eq!(rows[&707][2], rows[&713][0], "eligible capacity summary");
    assert!(rows[&713][0] <= rows[&713][1], "eligible peak");
    assert_eq!(
        u64::from(rows[&713][0]) + u64::from(rows[&713][2]),
        u64::from(rows[&706][0]),
        "eligible plus hypothetical residency covers actual capacity"
    );
    assert_eq!(
        u64::from(rows[&714][0]) + u64::from(rows[&714][1]),
        u64::from(rows[&701][2]),
        "all regions have conservative lifetime accounting"
    );
    assert_eq!(rows[&714][2], 0, "no access after proposed closure");
    let [detail_limit, unit_limit, map_limit] = rows[&708];
    assert!(rows[&705][0] <= unit_limit && rows[&705][1] <= map_limit);
    let region_details = count.min(detail_limit);
    let unit_details = rows[&705][0].min(detail_limit);
    for index in 0..region_details {
        let [base, end, _file] = rows[&(720 + 3 * index)];
        let [used, capacity, _accesses] = rows[&(721 + 3 * index)];
        assert!(base <= end && used <= capacity);
        assert!(
            rows.contains_key(&(722 + 3 * index)),
            "region consumer/retention row"
        );
    }
    for index in 0..unit_details {
        let [start, end, _entry] = rows[&(1120 + 2 * index)];
        assert!(start <= end, "whole block extent");
        let [parent, live, reserved] = rows[&(1121 + 2 * index)];
        assert!(parent <= rows[&705][0], "unit parent exists");
        assert!(live <= 1, "selection flag is Boolean");
        assert_eq!(reserved, 0, "reserved unit word");
    }
    for index in 0..rows[&705][1].min(detail_limit) {
        let [start, end, _file] = rows[&(1400 + 2 * index)];
        let [prepared_start, prepared_end, reserved] = rows[&(1401 + 2 * index)];
        assert!(start <= end && prepared_start <= prepared_end);
        assert_eq!(reserved, 0);
        let [_, used, capacity] = rows[&(1800 + 2 * index)];
        let [eligible, predicted, reserved] = rows[&(1801 + 2 * index)];
        assert!(used <= capacity && eligible <= capacity);
        assert_eq!(
            predicted,
            capacity - eligible,
            "hypothetical residency curve"
        );
        assert_eq!(reserved, 0);
    }
    assert_eq!(
        rows[&710].iter().map(|word| u64::from(*word)).sum::<u64>(),
        u64::from(rows[&705][0]),
        "unit reconciliation accounts for every unit"
    );
    assert_eq!(
        rows[&710][2], 0,
        "bounded controls have no unexplained unit mismatches"
    );
    assert_eq!(rows[&711][0], 0, "unit observer errors");
    assert_eq!(rows[&711][2], 0, "publication nesting balanced");
    assert!(rows[&711][1] <= rows[&712][1], "event capacity");
    for index in 0..rows[&711][1].min(rows[&712][2]) {
        for part in 0..5 {
            assert!(
                rows.contains_key(&(1700 + 5 * index + part)),
                "publication detail"
            );
        }
        let [open, end, previous] = rows[&(1700 + 5 * index)];
        assert!(
            previous <= open && open <= end,
            "original publication offsets"
        );
        let [end, _previous, unit] = rows[&(1703 + 5 * index)];
        let open = rows[&(1702 + 5 * index)][2];
        assert!(open <= end, "materialized publication offsets");
        assert!(unit <= rows[&705][0], "publication unit relationship");
    }
    rows
}

#[test]
fn native_lazy_audit_report_decodes_hex_and_rejects_duplicates() {
    assert_eq!(
        rows("progress p=000002BC f=00000001 l=00000000 r=00000002\n")[&700],
        [1, 0, 2]
    );
    assert!(std::panic::catch_unwind(|| rows(
        "progress p=000002BC f=1 l=0 r=0\nprogress p=000002BC f=1 l=0 r=0\n"
    ))
    .is_err());
}
