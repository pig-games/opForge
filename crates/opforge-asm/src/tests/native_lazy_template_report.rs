//! Complete scalar template inventory reconciliation, independent of the decoder.
use std::collections::BTreeMap;

#[path = "native_lazy_template_paths.rs"]
mod source_paths;

// Independently reconcile the complete scalar inventory against the old work
// totals. Accepted queue calls, including empty bodies, establish usage; body
// binding deltas are eager work and do not measure unique retained plans.
pub(super) fn assert_report(rows: &BTreeMap<u32, [u32; 3]>) {
    for phase in 3000..=3014 {
        assert!(
            rows.contains_key(&phase),
            "missing inventory header {phase}"
        );
    }
    let [schema, errors, epoch] = rows[&3000];
    assert_eq!(schema, 1, "inventory schema");
    assert_eq!(errors, 0, "inventory observer errors");
    assert_eq!(&rows[&3001][1..], &[2048, 4096], "inventory capacities");
    let [count, edge_count, finished] = rows[&3002];
    assert!(
        rows[&3001][0] > 0 && rows[&3001][0].is_multiple_of(4),
        "static table alignment"
    );
    assert!(finished <= epoch, "finished owner lifetimes");
    assert!(rows[&3014][1] <= 4, "live owner capacity");
    assert!(count <= 2048 && edge_count <= 4096);
    assert_eq!(&rows[&3011][..2], &[0, 0], "closed, finished inventory");
    assert_eq!(&rows[&3014][1..], &[0, 0], "inactive session/reserved word");
    assert!(
        rows.range(3000..).all(|(&phase, _)| {
            (3000..=3014).contains(&phase)
                || (3100..3100 + count * 8).contains(&phase)
                || (20000..20000 + edge_count * 3).contains(&phase)
                || (34999..182456).contains(&phase)
        }),
        "unexpected inventory detail"
    );
    let definitions: Vec<[u32; 24]> = (0..count)
        .map(|index| {
            std::array::from_fn(|word| rows[&(3100 + index * 8 + word as u32 / 3)][word % 3])
        })
        .collect();
    let edges: Vec<[u32; 9]> = (0..edge_count)
        .map(|index| {
            std::array::from_fn(|word| rows[&(20000 + index * 3 + word as u32 / 3)][word % 3])
        })
        .collect();
    source_paths::assert_report(rows, &definitions, &edges);
    let mut identities = std::collections::BTreeSet::new();
    let mut payload = [0u64; 5];
    for row in &definitions {
        assert!(row[0] > 0 && row[0] <= epoch && row[1] != 0);
        assert!(
            identities.insert((row[0], row[2], row[1])),
            "unique epoch/pool/handle"
        );
        assert!(row[3] <= 2);
        assert_eq!(row[2], u32::from(row[3] == 2), "definition pool/kind");
        assert_eq!(row[13], 1, "closed definition");
        assert!(row[9] >= row[8], "body offsets");
        assert_eq!(row[23], 0, "reserved definition word");
        assert!(row[17] <= count);
        payload[row[2] as usize] += u64::from(row[10]);
        payload[2] += u64::from(row[11]);
        payload[3] += u64::from(row[12]);
        payload[4] += u64::from(row[9] - row[8]);
    }
    assert_eq!(rows[&3008][2], count, "closed count");
    let mut relationships = std::collections::BTreeSet::new();
    for edge in &edges {
        let [caller, callee, origin, _, calls, defaults, slots, r1, r2] = *edge;
        assert!(caller <= count && callee > 0 && callee <= count && calls > 0);
        assert!(
            relationships.insert((caller, callee, origin)),
            "unique grouped consumer"
        );
        assert!(defaults <= calls && slots >= defaults);
        assert_eq!(defaults != 0, slots != 0);
        assert_eq!([r1, r2], [0, 0]);
        if caller != 0 {
            assert_eq!(
                definitions[(caller - 1) as usize][0],
                definitions[(callee - 1) as usize][0],
                "same session consumer"
            );
        }
    }
    let sum_rows = |field: usize| {
        definitions
            .iter()
            .map(|row| u64::from(row[field]))
            .sum::<u64>()
    };
    let sum_edges = |field: usize| edges.iter().map(|edge| u64::from(edge[field])).sum::<u64>();
    for (index, row) in definitions.iter().enumerate() {
        let consumers: Vec<_> = edges
            .iter()
            .filter(|edge| edge[1] == index as u32 + 1)
            .collect();
        for (row_field, edge_field) in [(14, 4), (16, 5), (22, 6)] {
            assert_eq!(
                u64::from(row[row_field]),
                consumers
                    .iter()
                    .map(|edge| u64::from(edge[edge_field]))
                    .sum::<u64>(),
                "per-definition consumer conservation"
            );
        }
        assert_eq!(
            u64::from(row[15]),
            consumers
                .iter()
                .filter(|edge| edge[0] != 0)
                .map(|edge| u64::from(edge[4]))
                .sum::<u64>(),
            "per-definition nested calls"
        );
        if row[14] == 0 {
            assert_eq!(
                &row[17..20],
                &[0; 3],
                "no first consumer for unused definition"
            );
        } else {
            assert!(
                consumers
                    .iter()
                    .any(|edge| [edge[0], edge[2], edge[3]] == [row[17], row[18], row[19]]),
                "first consumer exemplar"
            );
        }
    }
    let pool: Vec<u64> = rows[&3005]
        .into_iter()
        .chain(rows[&3006])
        .map(u64::from)
        .collect();
    let residual: Vec<u64> = rows[&3009]
        .into_iter()
        .chain(rows[&3010])
        .map(u64::from)
        .collect();
    let attributed: Vec<u64> = rows[&3007]
        .into_iter()
        .chain(rows[&3008][..2].iter().copied())
        .map(u64::from)
        .collect();
    assert_eq!(payload.as_slice(), attributed, "row payload components");
    for index in 0..5 {
        assert_eq!(
            payload[index] + residual[index],
            pool[index],
            "pool residual conservation"
        );
    }
    assert_eq!(pool[5], residual[5], "plan arena explicitly unattributed");
    assert_eq!(
        pool[..5].iter().sum::<u64>(),
        u64::from(rows[&2011][2]),
        "old global template bytes"
    );
    let calls = sum_rows(14);
    assert_eq!(calls, sum_edges(4), "row/edge accepted calls");
    assert_eq!(calls, u64::from(rows[&3003][2]));
    assert_eq!(calls, u64::from(rows[&3012][1]));
    assert_eq!(calls, u64::from(rows[&3012][2]));
    let nested = edges
        .iter()
        .filter(|edge| edge[0] != 0)
        .map(|edge| u64::from(edge[4]))
        .sum::<u64>();
    assert_eq!(nested, sum_rows(15));
    assert_eq!(nested, u64::from(rows[&3004][0]));
    for (field, header, summary) in [
        (5, rows[&3004][1], rows[&3013][2]),
        (6, rows[&3004][2], rows[&3014][0]),
    ] {
        assert_eq!(sum_edges(field), u64::from(header));
        assert_eq!(sum_edges(field), u64::from(summary));
    }
    assert_eq!(sum_rows(20), u64::from(rows[&3013][0]));
    assert_eq!(sum_rows(21), u64::from(rows[&3013][1]));
    assert_eq!(
        sum_rows(20) + u64::from(rows[&3011][2]),
        u64::from(rows[&3003][0]),
        "body/unattributed binds"
    );
    assert_eq!(
        sum_rows(21) + u64::from(rows[&3012][0]),
        u64::from(rows[&3003][1]),
        "body/unattributed bytes"
    );
    let contexts: Vec<_> = (2282..=2285).flat_map(|phase| rows[&phase]).collect();
    assert_eq!(
        u64::from(rows[&3003][0]),
        u64::from(contexts[0]) + u64::from(contexts[6]),
        "definition context calls"
    );
    assert_eq!(
        u64::from(rows[&3003][1]),
        u64::from(contexts[1]) + u64::from(contexts[7]),
        "definition context bytes"
    );
}

#[test]
fn native_template_inventory_conserves_payload_calls_and_body_work() {
    let rows = BTreeMap::from([
        (2011, [1, 2, 108]),
        (2282, [3, 2, 0]),
        (2283, [0, 0, 0]),
        (2284, [0, 0, 0]),
        (2285, [0, 0, 0]),
        (3000, [1, 0, 1]),
        (3001, [238760, 2048, 4096]),
        (3002, [3, 3, 1]),
        (3003, [3, 2, 4]),
        (3004, [1, 2, 3]),
        (3005, [40, 32, 4]),
        (3006, [12, 20, 40]),
        (3007, [40, 32, 4]),
        (3008, [12, 20, 3]),
        (3009, [0, 0, 0]),
        (3010, [0, 0, 40]),
        (3011, [0, 0, 0]),
        (3012, [0, 4, 4]),
        (3013, [3, 2, 2]),
        (3014, [3, 0, 0]),
        (3100, [1, 1, 0]),
        (3101, [1, 10, 1]),
        (3102, [2, 1000, 100]),
        (3103, [120, 24, 4]),
        (3104, [8, 1, 3]),
        (3105, [1, 2, 0]),
        (3106, [1, 10, 2]),
        (3107, [2, 3, 0]),
        (3108, [1, 1, 1]),
        (3109, [2, 20, 2]),
        (3110, [3, 1001, 200]),
        (3111, [200, 32, 0]),
        (3112, [4, 1, 1]),
        (3113, [0, 0, 0]),
        (3114, [2, 20, 1]),
        (3115, [0, 0, 0]),
        (3116, [1, 2, 0]),
        (3117, [0, 30, 1]),
        (3118, [4, 0, 300]),
        (3119, [300, 16, 0]),
        (3120, [0, 1, 0]),
        (3121, [0, 0, 0]),
        (3122, [0, 0, 0]),
        (3123, [0, 0, 0]),
        (20000, [0, 1, 1]),
        (20001, [10, 2, 1]),
        (20002, [1, 0, 0]),
        (20003, [2, 1, 2]),
        (20004, [11, 1, 1]),
        (20005, [2, 0, 0]),
        (20006, [0, 2, 2]),
        (20007, [20, 1, 0]),
        (20008, [0, 0, 0]),
    ]);
    assert_report(&rows);
    let mut full = rows.clone();
    let count = 1361;
    let added_bytes = (count - 3) * 184;
    full.get_mut(&3002).unwrap()[0] = count;
    full.get_mut(&3008).unwrap()[2] = count;
    full.get_mut(&3005).unwrap()[0] += added_bytes;
    full.get_mut(&3007).unwrap()[0] += added_bytes;
    full.get_mut(&2011).unwrap()[2] += added_bytes;
    for index in 3..count {
        let mut definition = [0; 24];
        definition[0] = 1;
        definition[1] = index + 10;
        definition[5] = 1;
        definition[10] = 184;
        definition[13] = 1;
        for part in 0..8 {
            full.insert(
                3100 + index * 8 + part,
                std::array::from_fn(|field| definition[part as usize * 3 + field]),
            );
        }
    }
    assert_report(&full); // Cross the former table/edge phase overlap.
    let mut overlap = rows.clone();
    overlap.get_mut(&3000).unwrap()[2] = 2;
    overlap.get_mut(&3002).unwrap()[2] = 2;
    overlap.get_mut(&3108).unwrap()[0] = 2;
    overlap.get_mut(&3116).unwrap()[0] = 2;
    overlap.get_mut(&3116).unwrap()[1] = 1; // Reused pool/handle in epoch 2.
    overlap.get_mut(&20003).unwrap()[0] = 1; // Recursive call within epoch 1.
    assert_report(&overlap);
    overlap.get_mut(&20003).unwrap()[0] = 2;
    assert!(std::panic::catch_unwind(|| assert_report(&overlap)).is_err());
    let mut empty = rows.clone();
    empty.retain(|phase, _| *phase < 3100);
    for phase in 3002..=3014 {
        empty.insert(phase, [0; 3]);
    }
    empty.insert(3000, [1, 0, 0]);
    empty.insert(2011, [0; 3]);
    empty.insert(2282, [0, 0, 3]);
    empty.insert(2283, [2, 0, 0]);
    assert_report(&empty); // A reset or directive-only probe has no session.
                           // The second definition has an accepted call despite an empty body; only
                           // the third definition is never invoked. Handles may repeat across pools.
    for (phase, field) in [
        (3003, 0),
        (3003, 1),
        (3003, 2),
        (3004, 0),
        (3004, 1),
        (3004, 2),
        (3005, 0),
        (3007, 0),
        (3008, 2),
        (3009, 1),
        (3010, 2),
        (3012, 1),
        (3012, 2),
        (3013, 0),
        (3013, 1),
        (3013, 2),
        (3014, 0),
        (3001, 0),
        (3002, 2),
        (3104, 2),
        (3105, 0),
        (3107, 1),
    ] {
        let mut bad = rows.clone();
        bad.get_mut(&phase).unwrap()[field] += 1;
        assert!(
            std::panic::catch_unwind(|| assert_report(&bad)).is_err(),
            "malformed phase {phase}, field {field}"
        );
    }
    for phase in [3000, 3014, 3107, 20008] {
        let mut bad = rows.clone();
        bad.remove(&phase);
        assert!(std::panic::catch_unwind(|| assert_report(&bad)).is_err());
    }
    let mut bad = rows.clone();
    bad.insert(20003, rows[&20000]);
    assert!(std::panic::catch_unwind(|| assert_report(&bad)).is_err());
    let mut bad = rows.clone();
    for part in 0..8 {
        bad.insert(3116 + part, rows[&(3100 + part)]);
    }
    assert!(std::panic::catch_unwind(|| assert_report(&bad)).is_err());
}
