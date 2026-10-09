//! Scalar updates use live Rust output, including contextual words and full-width comparisons.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

const POSITIVE: &[(&str, &str, &[u8])] = &[
    ("macro-namespace-parent", ".namespace outer\nn .var 1\nbump .macro amount\nn += .amount\n.endmacro\n.bump 3\n.byte n\n.endnamespace\n", &[4]),
    ("macro-outer-rhs", "n .var 3\nbump .macro\nlocal .var 1\nlocal += n\n.byte local\n.endmacro\n.bump\n", &[4]),
    ("macro-local-nested-namespace", "bump .macro\nn .var 1\n.namespace nested\nn += 3\n.byte n\n.endnamespace\n.byte n\n.endmacro\n.bump\n", &[4,4]),
    ("conditional-after-update", "n .var 1\nn += 2\n.if n==3\n.byte 7\n.else\n.byte 99\n.endif\n", &[7]),
    ("namespace-parent-conditional", "n .var 1\n.namespace inner\nn += 2\n.if n==3\n.byte 7\n.else\n.byte 99\n.endif\n.endnamespace\n.byte n\n", &[7,3]),
    ("self-rhs", "wide .const (1<<16)<<16\nn .var (wide+1)\nn += n\n.byte n==(wide*2+2)\nn ^= n\n.byte n==0\n", &[1,1]),
    ("signed-arithmetic", r#"wide .const (1<<16)<<16
n .var wide+9
n += wide+3
.byte n==(wide*2+12)
n -= wide+20
.byte n==4294967288
n *= -3
.byte n==(-(wide*2+4294967272))
n /= -3
.byte n==4294967288
n .set -(wide*2+9)
n /= 3
.byte n==(-2863311533)
n .set -(wide*2+9)
n %= 3
.byte n==(-2)
n .set 3
n **= 21
.byte n==(wide*2+1870418611)
n .set -2
n **= 33
.byte n==(-(wide*2))
"#, &[1; 8]),
    ("signed-boundaries", r#"wide .const (1<<16)<<16
n .var (wide*2147483647+4294967295)
n += 1
.byte n==(-(wide*2147483647+4294967295)-1)
n -= 1
.byte n==(wide*2147483647+4294967295)
n .set -1
n **= 63
.byte n==(-1)
n **= 0
.byte n==1
n .set -1
n &= (wide)
.byte n==0
n .set 1
n <<= -1
.byte n==2147483648
"#, &[1; 6]),
    ("u32-bit-logic-shifts", r#"wide .const (1<<16)<<16
n .var (wide)
n |= 2147483649
.byte n==2147483649
n ^= (wide+1)
.byte n==2147483648
n &= 4294967295
.byte n==2147483648
n >>= 33
.byte n==1073741824
n <<= 34
.byte n==0
n .set (wide+1)
n <<= 32
.byte n==1
n >>= (wide+32)
.byte n==1
n .set (wide)
n ||= (wide)
.byte n==0
n .set (wide)
n ||= 1
.byte n==1
n .set (wide)
n &&= 1
.byte n==0
n .set -1
n &&= (wide+1)
.byte n==1
"#, &[1; 11]),
    ("concat-width", r#"n .var $12
n ..= 0
.byte n==$1200
n .set $12
n ..= $abcdef
.byte n==$12abcdef
n .set $12
n ..= $89abcdef
.byte n==$89abcdef
"#, &[1; 3]),
    ("byte-repeat-bounds", r#"n .var $12
n X = 0
.byte n==0
n .set $12
n X = 1
.byte n==$12
n X = 2
.byte n==$1212
n X = 3
.byte n==$121212
n X = 4
.byte n==$12121212
n X = 5
.byte n==$12121212
n X = -1
.byte n==$12121212
wide .const (1<<16)<<16
n X = wide
.byte n==0
n .set $12
n X = wide+2
.byte n==$1212
n .set (wide+258)
n X = 2
.byte n==$202
"#, &[1; 10]),
    ("byte-repeat-ordinary-rhs", "x .var 2\nn .var $12\nn X = x\n.byte n==$1212,x\nn += x\n.byte n==$1214\n", &[1, 2, 1]),
    ("byte-repeat-namespace-rhs-shadow", "x .var 2\n.namespace inner\nx .var 3\nn .var $12\nn X = x\n.byte n==$121212\n.endnamespace\n", &[1]),
    ("byte-repeat-macro-qualified-target", "copybytes .macro count\nmain.n X = .count\n.endmacro\nn .var $12\n.copybytes 2\n.byte n==$1212\n", &[1]),
    ("byte-repeat-scoped-loop", "n .var $12\n.bfor count in {1,2}\nn X = count\n.byte n==$1212\n.endfor\n", &[0, 1]),
    ("byte-repeat-inactive-undefined", ".if 0\nmissing X = 2\n.endif\n.byte 7\n", &[7]),
    ("signed-min-max-member", r#"wide .const (1<<16)<<16
n .var (wide+4)
n <?= -(wide+5)
.byte n==(-(wide+5))
n >?= (wide+6)
.byte n==(wide+6)
n .= -(wide*2+1)
.byte n==(-(wide*2+1))
n .set (wide+4)
n <?= (wide+3)
.byte n==(wide+3)
n >?= (wide+5)
.byte n==(wide+5)
"#, &[1; 5]),
    ("scoped-while-outer", "n .var 0\n.bwhile n<3\n.byte n\nn += 1\n.endwhile\n.byte n\n", &[0,1,2,3]),
    ("scoped-for-shadow-snapshots", "n .var 9\nitems .bfor item in {2,4}\nn .var item\nn *= 3\n.byte n\n.endfor\n.byte n,items[0].n,items[1].n\n", &[6,12,9,6,12]),
    ("macro-expanded", "bump .macro amount\nmain.n += .amount\n.endmacro\nn .var 1\n.bump 3\n.byte n\n.bwhile n<6\n.bump 1\n.byte n\n.endwhile\n", &[4,5,6]),
    ("qualified-update", "n .var 3\ninner .bfor 1\nmain.n += 4\n.byte main.n\n.endfor\n.byte n\n", &[7,7]),
    ("inactive-invalid", "n .var 8\n.if 0\nn /= 0\nn %= 0\nn **= -1\nn **= 64\nmissing += 1\n.endif\n.byte n\n", &[8]),
];

const BAD: &[(&str, &str)] = &[
    (
        "named-block-module-parent",
        "n .var 1\nchild .block\nn += 3\n.byte n\n.bend\n.byte n\n",
    ),
    (
        "macro-unqualified-update",
        "bump .macro amount\nn += .amount\n.endmacro\nn .var 1\n.bump 3\n.byte n\n",
    ),
    (
        "division-overflow",
        "wide .const (1<<16)<<16\nn .var -(wide*2147483647+4294967295)-1\nn /= -1\n.byte 0\n",
    ),
    (
        "modulus-overflow",
        "wide .const (1<<16)<<16\nn .var -(wide*2147483647+4294967295)-1\nn %= -1\n.byte 0\n",
    ),
    ("readonly", "n .const 2\nn += 1\n.byte n\n"),
    ("repeat-readonly", "n .const 2\nn X = 2\n.byte n\n"),
    ("undefined", "missing += 1\n.byte 0\n"),
    ("typed-lhs", "n .var {1,2}\nn += 1\n.byte 0\n"),
    ("typed-rhs", "n .var 1\nn += {1,2}\n.byte n\n"),
    ("repeat-invalid-rhs", "n .var 1\nn X = {1,2}\n.byte n\n"),
    ("zero-divisor", "n .var 7\nn /= 0\n.byte n\n"),
    ("zero-modulus", "n .var 7\nn %= 0\n.byte n\n"),
    ("negative-power", "n .var 7\nn **= -1\n.byte n\n"),
    ("large-power", "n .var 7\nn **= 64\n.byte n\n"),
    (
        "ordinary-for-update",
        "n .var 0\n.for item in {1,2}\nn += item\n.endfor\n.byte n\n",
    ),
    (
        "ordinary-for-repeat-update",
        "n .var 1\n.for item in {1,2}\nn X = item\n.endfor\n.byte n\n",
    ),
];

fn source(cpu: &str, body: &str) -> String {
    format!(".module main\n.cpu {cpu}\n{body}.endmodule\n")
}

fn configuration_source(cpu: &str) -> String {
    format!(".module dep\n.cpu {cpu}\n.byte FEATURE\n.endmodule\n.module main\n.cpu {cpu}\nn .var 1\nn += 2\n.use dep with (FEATURE=n)\n.endmodule\n")
}

fn repeat_configuration_source(cpu: &str) -> String {
    format!(".module dep\n.cpu {cpu}\n.byte FEATURE==$1212\n.endmodule\n.module main\n.cpu {cpu}\nn .var $12\nn x=2\n.use dep with (FEATURE=n)\n.endmodule\n")
}

fn ordinary_operand(cpu: &str) -> (&'static str, &'static [u8]) {
    match cpu {
        "6502" => ("    lda $12\n    lda $20,x\n", &[0xa5, 0x12, 0xb5, 0x20]),
        "68020" => (
            "x .const 4\n    bra.s x\n    nop\n.fpu 68881\n    fmove.x -(a1),fp5\n",
            &[0x60, 0x02, 0x4e, 0x71, 0xf2, 0x21, 0x4a, 0x80],
        ),
        _ => unreachable!(),
    }
}

fn combined() -> (String, Vec<u8>) {
    let mut body = String::new();
    let mut expected = Vec::new();
    for (index, (_, case, bytes)) in POSITIVE.iter().enumerate() {
        let case = case.replace("main.", &format!("main.case_{index}."));
        body.push_str(&format!(".namespace case_{index}\n{case}.endnamespace\n"));
        expected.extend_from_slice(bytes);
    }
    (body, expected)
}

fn root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
}

#[test]
fn native_compound_updates_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        assert_eq!(
            oracle(&dir.0, &configuration_source(cpu)).unwrap(),
            [3],
            "{cpu}/configuration-use"
        );
        assert_eq!(
            oracle(&dir.0, &repeat_configuration_source(cpu)).unwrap(),
            [1],
            "{cpu}/repeat-configuration-use"
        );
        let (operand, expected) = ordinary_operand(cpu);
        assert_eq!(
            oracle(&dir.0, &source(cpu, operand)).unwrap(),
            expected,
            "{cpu}/ordinary-x-operand"
        );
        for (name, body, expected) in POSITIVE {
            assert_eq!(
                oracle(&dir.0, &source(cpu, body))
                    .unwrap_or_else(|error| panic!("{cpu}/{name}: {error}")),
                *expected,
                "{cpu}/{name}"
            );
        }
        let (body, expected) = combined();
        assert_eq!(oracle(&dir.0, &source(cpu, &body)).unwrap(), expected);
        for (name, body) in BAD {
            assert!(
                oracle(&dir.0, &source(cpu, body)).is_err(),
                "Rust accepted {cpu}/{name}"
            );
        }
    }
}

#[test]
fn native_compound_updates_host_assembles() {
    let dir = scratch();
    let image = build_image(&root(), &dir.0);
    assert!(!image.is_empty());
    if let Some(path) = std::env::var_os("OPFORGE_COMPOUND_SAVE_IMAGE") {
        fs::write(path, image).unwrap();
    }
}

#[test]
#[ignore = "requires configured FS-UAE; scalar compound update parity"]
fn native_compound_updates_fs_uae() {
    let root = root();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_COMPOUND_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let filter = std::env::var("OPFORGE_COMPOUND_FILTER").unwrap_or_default();
    let selected =
        |name: &str| filter.is_empty() || filter.split(',').any(|item| name.contains(item.trim()));
    let cpus = std::env::var("OPFORGE_COMPOUND_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let (body, _) = combined();
    let mut failures = Vec::new();
    let mut attempted = 0;
    for cpu in cpus.split(',').map(str::trim) {
        assert!(matches!(cpu, "68020" | "6502"));
        for (name, input) in [
            ("repeat-configuration-use", repeat_configuration_source(cpu)),
            ("ordinary-x-operand", source(cpu, ordinary_operand(cpu).0)),
        ] {
            if !selected(name) {
                continue;
            }
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_case(
                    &root,
                    &dir.0,
                    &image,
                    &format!("compound-{cpu}/{name}"),
                    cpu,
                    &input,
                    false,
                );
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
        if selected("configuration-use") {
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                run_case(
                    &root,
                    &dir.0,
                    &image,
                    &format!("compound-{cpu}/configuration-use"),
                    cpu,
                    &configuration_source(cpu),
                    false,
                );
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/configuration-use"));
            }
        }
        for (name, body, failure) in std::iter::once(("combined-positive", body.as_str(), false))
            .chain(
                POSITIVE
                    .iter()
                    .filter(|_| !filter.is_empty())
                    .map(|(n, b, _)| (*n, *b, false)),
            )
            .chain(BAD.iter().map(|(n, b)| (*n, *b, true)))
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
                    &format!("compound-{cpu}/{name}"),
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
    assert!(attempted > 0, "filter selected no compound update cases");
    assert!(
        failures.is_empty(),
        "compound update failures: {failures:?}"
    );
}
