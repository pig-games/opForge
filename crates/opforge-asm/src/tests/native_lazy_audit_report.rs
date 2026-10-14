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
        if (700..=2466).contains(&phase) {
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
    if rows.contains_key(&2000) {
        assert_work_report(&rows);
    }
    rows
}

fn assert_work_report(rows: &BTreeMap<u32, [u32; 3]>) {
    let [version, errors, count] = rows[&2000];
    assert_eq!(version, 1, "work schema");
    assert_eq!(errors, 0, "work observer errors/overflow");
    let [_, limit, reported] = rows[&2001];
    assert_eq!(limit, 16);
    assert_eq!(reported, count.min(limit));
    assert_eq!(rows[&2002][0], 60, "capture header contract");
    assert_eq!(rows[&2002][1], 4, "audit header overhead");
    assert_eq!(
        rows[&2041][2], rows[&2011][2],
        "all retained templates are shared"
    );
    for category in [2020, 2030, 2050] {
        assert_eq!(
            rows[&(category + 1)][2],
            0,
            "templates have no exclusive owner"
        );
    }
    for part in 0..6 {
        for (field, value) in rows[&(2040 + part)].iter().enumerate() {
            if part != 1 || field != 2 {
                assert_eq!(*value, 0, "only template retention is shared");
            }
        }
    }
    for part in 0..6 {
        for (field, total) in rows[&(2010 + part)].iter().enumerate() {
            let total = u64::from(*total);
            let categories: u64 = [2020, 2030, 2040, 2050]
                .iter()
                .map(|start| u64::from(rows[&(start + part)][field]))
                .sum();
            assert_eq!(total, categories, "exclusive category conservation");
            {
                let details: u64 = (0..reported)
                    .map(|index| u64::from(rows[&(2061 + index * 7 + part)][field]))
                    .sum();
                assert!(
                    details <= total,
                    "exclusive unit details are bounded by total"
                );
            }
        }
    }
    if rows.contains_key(&2200) {
        assert_eq!(rows[&2200], [1, 3, 36], "unknown extension ABI");
        for metric in 0..18u32 {
            let part = metric / 3;
            let field = (metric % 3) as usize;
            assert_eq!(
                u64::from(rows[&(2210 + part)][field]) + u64::from(rows[&(2220 + part)][field]),
                u64::from(rows[&(2050 + part)][field]),
                "unknown split conservation"
            );
            if count == reported {
                let expected: u64 = (0..reported)
                    .filter(|index| rows[&(2060 + index * 7)][2] != 0)
                    .map(|index| {
                        if metric == 5 {
                            0
                        } else {
                            u64::from(rows[&(2061 + index * 7 + part)][field])
                        }
                    })
                    .sum();
                assert_eq!(
                    u64::from(rows[&(2210 + part)][field]),
                    expected,
                    "reason-marked unit conservation"
                );
            }
        }
        let contexts: Vec<_> = (2282..=2285).flat_map(|phase| rows[&phase]).collect();
        assert_eq!(
            contexts
                .iter()
                .step_by(2)
                .map(|v| u64::from(*v))
                .sum::<u64>(),
            u64::from(rows[&2010][2]),
            "context plan-bind conservation"
        );
        assert_eq!(
            contexts
                .iter()
                .skip(1)
                .step_by(2)
                .map(|v| u64::from(*v))
                .sum::<u64>(),
            u64::from(rows[&2011][1]),
            "context bound-byte conservation"
        );
        if rows.range(2300..=2466).next().is_some() {
            for slot in 0..7u32 {
                let start = 2300 + slot * 24;
                let [origin, length, ordinal] = rows[&start];
                let provenance = if slot < 3 {
                    rows[&(2242 + slot * 9)][0]
                } else {
                    rows[&(2271 + (slot - 3) * 3)][2]
                };
                assert_eq!(origin, provenance);
                assert_eq!(ordinal, slot + 1);
                assert!(length < 256);
                assert!(origin != 0 || length == 0);
                assert!(
                    rows.range(start + 1 + length.div_ceil(12)..start + 24)
                        .next()
                        .is_none(),
                    "unexpected source path chunk"
                );
                let bytes: Vec<_> = (0..length.div_ceil(12))
                    .flat_map(|index| {
                        rows[&(start + 1 + index)]
                            .into_iter()
                            .flat_map(u32::to_be_bytes)
                    })
                    .collect();
                assert!(bytes[length as usize..].iter().all(|byte| *byte == 0));
                std::str::from_utf8(&bytes[..length as usize]).expect("source path UTF-8");
            }
        }
        assert_eq!(rows[&2231][2], 0, "reserved unknown word");
        for index in 0..4u32 {
            let start = 2270 + index * 3;
            let [event, unit, cause] = rows[&start];
            let [open, close, _origin] = rows[&(start + 1)];
            let [_line, base, end] = rows[&(start + 2)];
            let count = if index < 3 {
                rows[&2230][index as usize]
            } else {
                rows[&2231][0]
            };
            assert_eq!(event != 0, count != 0);
            if event == 0 {
                assert_eq!(cause, 0);
                assert_eq!(unit, 0);
                assert_eq!(rows[&(start + 1)], [0; 3]);
                assert_eq!(rows[&(start + 2)], [0; 3]);
            } else {
                assert_eq!(cause, index + 1);
                assert!(match index {
                    0 => close < open,
                    1 => close >= end,
                    2 => true,
                    _ => open < base,
                });
            }
        }
        for (index, metric) in [13u32, 2, 4].into_iter().enumerate() {
            let start = 2240 + index as u32 * 9;
            let [offset, event, unit] = rows[&start];
            assert_eq!(offset, metric * 4);
            let reasons = rows[&(start + 1)][0];
            assert!(event <= count);
            for part in 0..6 {
                for (field, value) in rows[&(start + 3 + part)].iter().enumerate() {
                    assert!(*value <= rows[&(2210 + part)][field]);
                }
            }
            if count == reported {
                let winner = (0..reported)
                    .filter(|i| rows[&(2060 + i * 7)][2] != 0)
                    .filter(|i| rows[&(2061 + i * 7 + metric / 3)][(metric % 3) as usize] != 0)
                    .max_by_key(|i| {
                        (
                            rows[&(2061 + i * 7 + metric / 3)][(metric % 3) as usize],
                            std::cmp::Reverse(rows[&(2060 + i * 7)][0]),
                        )
                    });
                if let Some(i) = winner {
                    assert_eq!([event, unit, reasons], rows[&(2060 + i * 7)]);
                    for part in 0..6 {
                        let mut expected = rows[&(2061 + i * 7 + part)];
                        if part == 1 {
                            expected[2] = 0;
                        }
                        assert_eq!(rows[&(start + 3 + part)], expected);
                    }
                } else {
                    assert_eq!(event, 0);
                    for phase in start + 1..start + 9 {
                        assert_eq!(rows[&phase], [0; 3]);
                    }
                    assert_eq!(unit, 0);
                }
            }
        }
    }
    let mut events = std::collections::BTreeSet::new();
    for index in 0..reported {
        let [event, _, _] = rows[&(2060 + index * 7)];
        assert!(event != 0 && events.insert(event), "unique work event");
    }
    // On small controls independently rebuild the categories from final whole
    // block retention. Nested units are retained with their outer owner even
    // when their own Live word is zero. Global residual work has no unit row.
    if count == reported && rows[&705][0] <= 16 {
        let mut expected = [[0u64; 18]; 2];
        for index in 0..reported {
            let [_, mut unit, reasons] = rows[&(2060 + index * 7)];
            if reasons != 0 || unit == 0 {
                continue;
            }
            let mut depth = 0;
            loop {
                assert!(unit <= rows[&705][0]);
                let [parent, live, _] = rows[&(1121 + (unit - 1) * 2)];
                if parent == 0 {
                    let category = if live == 1 { 0 } else { 1 };
                    for metric in 0..18u32 {
                        if metric != 5 {
                            // Template retention is always shared.
                            expected[category][metric as usize] += u64::from(
                                rows[&(2061 + index * 7 + metric / 3)][(metric % 3) as usize],
                            );
                        }
                    }
                    break;
                }
                unit = parent;
                depth += 1;
                assert!(depth <= rows[&705][0], "acyclic parent ownership");
            }
        }
        for (category, start) in [2020, 2030].into_iter().enumerate() {
            for metric in 0..18u32 {
                assert_eq!(
                    expected[category][metric as usize],
                    u64::from(rows[&(start + metric / 3)][(metric % 3) as usize]),
                    "whole-unit retention category, metric {metric}"
                );
            }
        }
    }
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

#[test]
fn native_lazy_audit_work_report_requires_shared_template_provenance() {
    let mut rows = BTreeMap::from([
        (705, [0, 0, 0]),
        (2000, [1, 0, 0]),
        (2001, [0, 16, 0]),
        (2002, [60, 4, 0]),
    ]);
    for category in [2010, 2020, 2030, 2040, 2050] {
        for part in 0..6 {
            rows.insert(category + part, [0; 3]);
        }
    }
    // No whole-unit publications: preparation stays unknown, while committed
    // macro template payload is shared even when it emits no records.
    rows.get_mut(&2010).unwrap()[0] = 3;
    rows.get_mut(&2050).unwrap()[0] = 3;
    rows.get_mut(&2011).unwrap()[2] = 48;
    rows.get_mut(&2041).unwrap()[2] = 48;
    assert_work_report(&rows);
    rows.get_mut(&2041).unwrap()[2] = 0;
    rows.get_mut(&2051).unwrap()[2] = 48;
    assert!(std::panic::catch_unwind(|| assert_work_report(&rows)).is_err());
}
