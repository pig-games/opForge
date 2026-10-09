//! Direct packed literals retain the signed scalar width of the live Rust oracle.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

const POSITIVE: &[(&str, &str, &[u8])] = &[
    (
        "compound-literals",
        "values .const {4294967296,9223372036854775807}\n.byte .len(values)==2,values[0]==$100000000,values[1]==$7fffffffffffffff\nspan .const 4294967296..=4294967298\n.byte .len(span)==3,span[1]==4294967297\n",
        &[1; 5],
    ),
    (
        "radix-boundaries",
        r#".byte 4294967295==$ffffffff
.byte 4294967296==$100000000
.byte 4294967297==$100000001
.byte 9223372036854775807==$7fffffffffffffff
.byte 4294967296==%100000000000000000000000000000000
.byte 4294967297==%100000000000000000000000000000001
.byte 9223372036854775807==%111111111111111111111111111111111111111111111111111111111111111
.byte $100000001>$ffffffff,$100000000!=$0,$100000001!=$1
.byte $7fffffffffffffff>$100000000
"#,
        &[1; 11],
    ),
    (
        "signed-wrapping",
        r#"minimum .const -9223372036854775807-1
maximum .const 9223372036854775807
.byte minimum<0,maximum>0,minimum<maximum
.byte -9223372036854775807==minimum+1
.byte +9223372036854775807==maximum
.byte ~4294967296==(-4294967297)
.byte 9223372036854775807+1==minimum
.byte minimum-1==9223372036854775807
.byte 4294967296*4294967296==0
.byte -4294967297<(-4294967296)
.byte 9223372036854775807/4294967296==2147483647
.byte 9223372036854775807%4294967296==4294967295
"#,
        &[1; 12],
    ),
    (
        "mutable",
        r#"n .var 4294967297
.byte n==$100000001
n += 4294967296
.byte n==8589934593
n -= $100000001
.byte n==4294967296
n *= 2147483648
.byte n==(-9223372036854775807-1)
n .set 9223372036854775807
n += 1
.byte n==(-9223372036854775807-1)
n -= 1
.byte n==9223372036854775807
n .set -4294967297
n /= 4294967296
.byte n==(-1)
n .set 9223372036854775807
n %= 4294967296
.byte n==4294967295
"#,
        &[1; 8],
    ),
    (
        "macro-conditional",
        r#"check .macro value
.byte .value==4294967297
.endmacro
unused .macro
.byte 9223372036854775808
.endmacro
.if 0
.byte $ffffffffffffffff
.endif
.check 4294967297
.check $100000001
.check %100000000000000000000000000000001
defaulted .macro value=$100000001
.byte .value==4294967297,@1==4294967297
.endmacro
.defaulted 4294967297
defaultOnly .macro value=$100000001
.byte .value==4294967297
.endmacro
.defaultOnly
.if 4294967296
.byte 7
.else
.byte 99
.endif
.if 9223372036854775807>4294967296
.byte 8
.else
.byte 99
.endif
.if -9223372036854775807<0
.byte 9
.else
.byte 99
.endif
"#,
        &[1, 1, 1, 1, 1, 1, 7, 8, 9],
    ),
];

// The scalar oracle accepts i64 literals, not the entire unsigned payload range.
// Even direct -9223372036854775808 rejects: its positive inner token is out of range.
const BAD: &[(&str, &str)] = &[
    ("decimal-signed-limit", ".byte 9223372036854775808==0\n"),
    ("hex-signed-limit", ".byte $8000000000000000==0\n"),
    (
        "binary-signed-limit",
        ".byte %1000000000000000000000000000000000000000000000000000000000000000==0\n",
    ),
    ("decimal-u64-max", ".byte 18446744073709551615==0\n"),
    ("hex-u64-max", ".byte $ffffffffffffffff==0\n"),
    (
        "binary-u64-max",
        ".byte %1111111111111111111111111111111111111111111111111111111111111111==0\n",
    ),
    ("decimal-u64-overflow", ".byte 18446744073709551616==0\n"),
    ("hex-u64-overflow", ".byte $10000000000000000==0\n"),
    (
        "binary-u64-overflow",
        ".byte %10000000000000000000000000000000000000000000000000000000000000000==0\n",
    ),
    ("direct-negative-minimum", ".byte -9223372036854775808==0\n"),
    (
        "negation-overflow",
        "minimum .const -9223372036854775807-1\n.byte -minimum==0\n",
    ),
    (
        "division-overflow",
        "minimum .const -9223372036854775807-1\n.byte minimum/-1==0\n",
    ),
    (
        "modulus-overflow",
        "minimum .const -9223372036854775807-1\n.byte minimum%-1==0\n",
    ),
];

fn source(cpu: &str, body: &str) -> String {
    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n")
}

fn long_low_halves(cpu: &str) -> (&'static str, Vec<u8>) {
    let bytes = [0u32, 1, u32::MAX, 0, u32::MAX]
        .into_iter()
        .flat_map(|value| {
            if cpu == "6502" {
                value.to_le_bytes()
            } else {
                value.to_be_bytes()
            }
        })
        .collect();
    (
        ".long 4294967296,$100000001,9223372036854775807,-9223372036854775807-1,-4294967297\n",
        bytes,
    )
}

fn combined(cpu: &str) -> (String, Vec<u8>) {
    let mut body = String::new();
    let mut expected = Vec::new();
    for (index, (_, case, bytes)) in POSITIVE.iter().enumerate() {
        body.push_str(&format!(".namespace case_{index}\n{case}.endnamespace\n"));
        expected.extend_from_slice(bytes);
    }
    let (longs, bytes) = long_low_halves(cpu);
    body.push_str(longs);
    expected.extend(bytes);
    (body, expected)
}

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

#[test]
fn native_wide_literals_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        for (name, body, expected) in POSITIVE {
            assert_eq!(
                oracle(&dir.0, &source(cpu, body))
                    .unwrap_or_else(|error| panic!("{cpu}/{name}: {error}")),
                *expected,
                "{cpu}/{name}"
            );
        }
        let (body, expected) = combined(cpu);
        assert_eq!(
            oracle(&dir.0, &source(cpu, &body)).unwrap(),
            expected,
            "{cpu}/combined-positive"
        );
        for (name, body) in BAD {
            assert!(
                oracle(&dir.0, &source(cpu, body)).is_err(),
                "Rust accepted {cpu}/{name}"
            );
        }
    }
}

#[test]
fn native_wide_literals_host_assembles() {
    let dir = scratch();
    let image = build_image(&root(), &dir.0);
    assert!(!image.is_empty());
    if let Some(path) = std::env::var_os("OPFORGE_WIDE_SAVE_IMAGE") {
        fs::write(path, image).unwrap();
    }
}

#[test]
#[ignore = "requires configured FS-UAE; direct wide packed literal parity"]
fn native_wide_literals_fs_uae() {
    let root = root();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_WIDE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    if let Some(path) = std::env::var_os("OPFORGE_WIDE_SAVE_IMAGE") {
        fs::write(path, &image).unwrap();
    }
    let filter = std::env::var("OPFORGE_WIDE_FILTER").unwrap_or_default();
    let selected =
        |name: &str| filter.is_empty() || filter.split(',').any(|item| name.contains(item.trim()));
    let cpus = std::env::var("OPFORGE_WIDE_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let mut failures = Vec::new();
    let mut attempted = 0;
    for cpu in cpus.split(',').map(str::trim) {
        assert!(matches!(cpu, "68020" | "6502"));
        let (body, expected) = combined(cpu);
        // Independent expected bytes qualify the grouped live oracle before launch.
        assert_eq!(oracle(&dir.0, &source(cpu, &body)).unwrap(), expected);
        let (longs, _) = long_low_halves(cpu);
        for (name, body, failure) in std::iter::once(("combined-positive", body.as_str(), false))
            .chain(
                POSITIVE
                    .iter()
                    .filter(|_| !filter.is_empty())
                    .map(|(name, body, _)| (*name, *body, false)),
            )
            .chain(
                std::iter::once(("long-low-halves", longs, false)).filter(|_| !filter.is_empty()),
            )
            .chain(BAD.iter().map(|(name, body)| (*name, *body, true)))
        {
            if !selected(name) {
                continue;
            }
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_case(
                    &root,
                    &dir.0,
                    &image,
                    &format!("wide-{cpu}/{name}"),
                    cpu,
                    &source(cpu, body),
                    failure,
                );
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
    }
    assert!(attempted > 0, "filter selected no wide literal cases");
    assert!(failures.is_empty(), "wide literal failures: {failures:?}");
}
