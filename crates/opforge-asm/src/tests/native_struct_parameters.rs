//! Struct configuration, source-site snapshots and forwarding use live Rust oracles.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

const POINT: &str = "Point .struct\nx .byte ?\ny .byte ?\n.endstruct\n";
const OTHER: &str = "Other .struct\nx .byte ?\ny .byte ?\n.endstruct\n";

type Case = (&'static str, String, &'static [u8]);

fn source(cpu: &str, dependency: &str, body: &str) -> String {
    format!(".module dep\n.cpu {cpu}\n{dependency}.endmodule\n.module main\n.cpu {cpu}\n{POINT}{body}.endmodule\n")
}

fn positive_cases(cpu: &str) -> Vec<Case> {
    let instance = ".pub\nresult=VALUE.x+VALUE.y\n";
    vec![
        ("type-parameter", source(cpu, "Alias=TYPE\np=Alias{x:2,y:4}\n.pub\nresult=p.x+p.y+TYPE.y\n", ".use dep as imported with (TYPE=Point)\n.byte imported.result\n"), &[7]),
        ("inline-instance", source(cpu, instance, ".use dep as imported with (VALUE=Point{x:2,y:4})\n.byte imported.result\n"), &[6]),
        ("existing-instance-member", source(cpu, ".pub\nresult=VALUE.x+VALUE.y+SCALAR\n", "p=Point{x:2,y:4}\n.use dep as imported with (VALUE=p, SCALAR=p.x)\n.byte imported.result\n"), &[8]),
        ("referenced-block-parameters", source(cpu, ".pub\nentry .block\nAlias=TYPE\nmade=Alias{x:VALUE.x+1,y:VALUE.y+2}\n.byte TYPE.y,made.x,made.y\n.bend\n", "p=Point{x:2,y:4}\n.use dep (entry) as imported with (TYPE=Point, VALUE=p)\n.word imported.entry\n"), &[1,3,6,0,0]),
        ("combined-forwarding-snapshots-reimports", combined_source(cpu), &[32,32,8,4,99,20,40,5,8]),
        ("parameter-dependent-schema", parameter_schema_source(cpu), &[1,4,7]),
        ("inactive-schema-alternative", source(cpu, "Alias=TYPE\np=Alias{x:3,y:4}\n.pub\nresult=TYPE.y+p.x+p.y\n", ".if 0\nUnused .struct\nbad .res missing\n.endstruct\n.else\n.use dep as imported with (TYPE=Point)\n.endif\n.byte imported.result\n"), &[8]),
        ("register-field-configuration", source(cpu, ".pub\nresult=VALUE.a+VALUE.d0+VALUE.sp+TYPE.d0\n", "Registers .struct\na .byte ?\nd0 .byte ?\nsp .byte ?\n.endstruct\n.use dep as imported with (TYPE=Registers, VALUE=Registers{sp:12,d0:2,a:8})\n.byte imported.result\n"), &[23]),
        ("readonly-uncertain-snapshot", source(cpu, instance, "p .var Point{x:2,y:4}\nsaved=p\n.if later\np .set Point{x:8,y:9}\n.endif\n.use dep as imported with (VALUE=saved)\nlater=1\n.byte imported.result,p.x\n"), &[6,8]),
    ]
}

// Keep each scenario's declarations/imports separate while qualifying them in
// one fresh guest run. This avoids repeated emulator startup for tiny inputs.
fn positive_matrix(cpu: &str) -> (String, Vec<u8>) {
    let mut input = String::new();
    let mut expected = Vec::new();
    for (index, (_, source, bytes)) in positive_cases(cpu).into_iter().enumerate() {
        for line in source.lines() {
            if line.starts_with(".module ") || line.starts_with(".use ") {
                let (directive, rest) = line.split_once(' ').unwrap();
                input.push_str(&format!("{directive} case{index}.{rest}\n"));
            } else {
                input.push_str(line);
                input.push('\n');
            }
        }
        expected.extend_from_slice(bytes);
    }
    (input, expected)
}

// A dependency constructs a schema from an incoming scalar, then forwards its
// type and instance. Both nominal and member IDs cross configuration scopes.
fn parameter_schema_source(cpu: &str) -> String {
    format!(
        ".module leaf\n.cpu {cpu}\nAlias=TYPE\nmade=Alias{{head:4,body:7}}\n.pub\nresult=TYPE.body\nhead=made.head\nbody=VALUE.body\n.endmodule\n.module middle\n.cpu {cpu}\nPacket .struct\nhead .byte ?\nbody .res WIDTH\n.endstruct\nsaved=Packet{{head:2,body:7}}\n.use leaf as child with (TYPE=Packet, VALUE=saved)\n.pub\nresult=child.result\nhead=child.head\nbody=child.body\n.endmodule\n.module main\n.cpu {cpu}\n.use middle as imported with (WIDTH=3)\n.byte imported.result,imported.head,imported.body\n.endmodule\n"
    )
}

fn combined_source(cpu: &str) -> String {
    format!(
        r#".module leaf
.cpu {cpu}
.pub
result=VALUE.x+VALUE.y+ORIGINAL.x+SCALAR+ITEMS[0]+SPAN[0]+TYPE.y
.endmodule
.module middle
.cpu {cpu}
Alias=TYPE
made=Alias{{x:VALUE.x+SCALAR,y:ITEMS[1]+SPAN[1]}}
savedItems=ITEMS
savedSpan=SPAN
savedPoint=VALUE
.use leaf as child with (TYPE=Alias, VALUE=made, ORIGINAL=savedPoint, SCALAR=SCALAR+1, ITEMS=savedItems, SPAN=savedSpan)
.pub
result=child.result
.endmodule
.module main
.cpu {cpu}
{POINT}p .var Point{{x:2,y:4}}
savedPoint=p
scalar .var 2
items .var {{3,5}}
span .var 6..10:2
savedItems=items
savedSpan=span
.use middle as first with (TYPE=Point, VALUE=p, SCALAR=scalar, ITEMS=items, SPAN=span)
p .set Point{{x:8,y:9}}
scalar .set 99
items .set {{20,30}}
span .set 40..44:2
.use middle as second with (TYPE=Point, VALUE=Point{{Y:4,X:2}}, SCALAR=2, ITEMS=savedItems, SPAN=savedSpan)
.byte first.result,second.result,p.x,savedPoint.y,scalar,items[0],span[0],savedItems[1],savedSpan[1]
.endmodule
"#
    )
}

fn compound_control(cpu: &str) -> String {
    format!(
        r#".module leaf
.cpu {cpu}
.pub
result=SCALAR+ITEMS[1]+SPAN[1]
.endmodule
.module middle
.cpu {cpu}
savedItems=ITEMS
savedSpan=SPAN
.use leaf as child with (SCALAR=SCALAR+1, ITEMS=savedItems, SPAN=savedSpan)
.pub
result=child.result
.endmodule
.module main
.cpu {cpu}
scalar .var 2
items .var {{3,5}}
span .var 6..10:2
savedItems=items
savedSpan=span
.use middle as first with (SCALAR=scalar, ITEMS=items, SPAN=span)
scalar .set 99
items .set {{20,30}}
span .set 40..44:2
.use middle as second with (SCALAR=2, ITEMS=savedItems, SPAN=savedSpan)
.byte first.result,second.result,scalar,items[0],span[0],savedItems[1],savedSpan[1]
.endmodule
"#
    )
}

// Conflict diagnostics prove complete-value comparison; availability errors
// prove only that the importing module cannot evaluate an expression at site.
const STRUCT_REJECTIONS: &[(&str, &str, &str)] = &[
    ("struct-value-conflict", ".use dep as first with (VALUE=Point{x:2,y:4})\n.use dep as second with (VALUE=Point{x:2,y:5})\n", "conflicting .use parameters"),
    ("struct-type-conflict", ".use dep as first with (VALUE=Point)\n.use dep as second with (VALUE=Other)\n", "conflicting .use parameters"),
    ("instance-type-conflict", ".use dep as first with (VALUE=Point{x:2,y:4})\n.use dep as second with (VALUE=Other{x:2,y:4})\n", "conflicting .use parameters"),
    ("instance-scalar-type-conflict", ".use dep as first with (VALUE=Point{x:2,y:4})\n.use dep as second with (VALUE=2)\n", "conflicting .use parameters"),
    ("assembly-address-member", "here:\n.use dep with (VALUE=Point{x:here,y:4})\n", "Label not found: here"),
    ("uncertain-schema-choice", ".if later\nChoice .struct\nx .byte ?\n.endstruct\n.else\nChoice .struct\nx .long ?\n.endstruct\n.endif\n.use dep with (VALUE=Choice)\nlater=1\n", "Label not found: Choice"),
    ("unavailable-schema-extent", "Choice .struct\nx .res later\n.endstruct\n.use dep with (VALUE=Choice)\nlater=3\n", "Label not found: Choice"),
    ("uncertain-schema-body", "Choice .struct\n.if later\nx .byte ?\n.else\nx .long ?\n.endif\n.endstruct\n.use dep with (VALUE=Choice)\nlater=1\n", "Label not found: Choice"),
    ("uncertain-instance-write", "p .var Point{x:2,y:4}\n.if later\np .set Point{x:8,y:9}\n.endif\n.use dep with (VALUE=p)\nlater=1\n", "Label not found: p"),
];

const BAD: &[(&str, &str, &str)] = &[
    (
        "scalar-value-conflict",
        ".use dep as first with (VALUE=2)\n.use dep as second with (VALUE=3)\n",
        "conflicting .use parameters",
    ),
    (
        "list-value-conflict",
        ".use dep as first with (VALUE={2,4})\n.use dep as second with (VALUE={2,5})\n",
        "conflicting .use parameters",
    ),
    (
        "range-value-conflict",
        ".use dep as first with (VALUE=2..6:2)\n.use dep as second with (VALUE=2..8:2)\n",
        "conflicting .use parameters",
    ),
    (
        "list-range-type-conflict",
        ".use dep as first with (VALUE={2,4})\n.use dep as second with (VALUE=2..6:2)\n",
        "conflicting .use parameters",
    ),
    (
        "forward-type",
        ".use dep with (VALUE=Later)\nLater .struct\nx .byte ?\n.endstruct\n",
        "Label not found: Later",
    ),
    (
        "forward-instance",
        ".use dep with (VALUE=p)\np=Point{x:2,y:4}\n",
        "Label not found: p",
    ),
    (
        "hidden-instance",
        "hidden .block\np=Point{x:2,y:4}\n.bend\n.use dep with (VALUE=p)\n",
        "Label not found: p",
    ),
];

fn negative_source(cpu: &str, body: &str) -> String {
    source(cpu, ".pub\nresult=1\n", &format!("{OTHER}{body}"))
}

fn binding_rejections(cpu: &str) -> Vec<(&'static str, String, &'static str)> {
    let import =
        ".use dep as imported with (TYPE=Point, VALUE=Point{x:2,y:4})\n.byte imported.result\n";
    vec![
        (
            "readonly-instance-parameter",
            source(cpu, "VALUE .set TYPE{x:8,y:9}\n.pub\nresult=VALUE.x\n", import),
            "symbol has already been defined",
        ),
        (
            "conflicting-parameter-declaration",
            source(cpu, "VALUE=TYPE{x:8,y:9}\n.pub\nresult=VALUE.x\n", import),
            "symbol has already been defined",
        ),
        (
            "private-instance-parameter",
            source(cpu, ".pub\nresult=VALUE.x\n", ".use dep as imported with (TYPE=Point, VALUE=Point{x:2,y:4})\n.byte imported.VALUE.x\n"),
            "Symbol is private",
        ),
    ]
}

#[test]
fn native_struct_parameters_rust_oracles() {
    let dir = scratch();
    for cpu in ["68020", "6502"] {
        assert_eq!(
            oracle(&dir.0, &compound_control(cpu)).unwrap(),
            [16, 16, 99, 20, 40, 5, 8],
            "{cpu}/compound-control"
        );
        for (name, body, diagnostic) in BAD {
            let error = oracle(&dir.0, &negative_source(cpu, body)).expect_err(name);
            assert!(error.contains(diagnostic), "{cpu}/{name}: {error}");
        }
    }
}

#[test]
fn native_struct_parameters_struct_rust_oracles() {
    let dir = scratch();
    let mut failures = Vec::new();
    for cpu in ["68020", "6502"] {
        let (input, mut expected) = positive_matrix(cpu);
        // Three preceding one-byte roots move the referenced block to $0003.
        let entry = if cpu == "68020" { [0, 3] } else { [3, 0] };
        expected[6..8].copy_from_slice(&entry);
        assert_eq!(oracle(&dir.0, &input).unwrap(), expected, "{cpu}/matrix");
        for (name, input, expected) in positive_cases(cpu) {
            let actual = oracle(&dir.0, &input);
            if actual.as_deref() != Ok(expected) {
                failures.push(format!(
                    "{cpu}/{name}: expected {expected:?}, got {actual:?}"
                ));
            }
        }
        for (name, input, diagnostic) in STRUCT_REJECTIONS
            .iter()
            .map(|(name, body, diagnostic)| (*name, negative_source(cpu, body), *diagnostic))
            .chain(binding_rejections(cpu))
        {
            match oracle(&dir.0, &input) {
                Err(error) if error.contains(diagnostic) => {}
                actual => failures.push(format!(
                    "{cpu}/{name}: expected diagnostic {diagnostic:?}, got {actual:?}"
                )),
            }
        }
    }
    assert!(
        failures.is_empty(),
        "Rust struct parameter gaps:\n{}",
        failures.join("\n")
    );
}

#[test]
#[ignore = "requires configured FS-UAE; struct configuration, snapshots and forwarding"]
fn native_struct_parameters_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_STRUCT_PARAMETER_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    if let Some(path) = std::env::var_os("OPFORGE_STRUCT_PARAMETER_SAVE_IMAGE") {
        fs::write(path, &image).unwrap();
    }
    let filter = std::env::var("OPFORGE_STRUCT_PARAMETER_FILTER").unwrap_or_default();
    let selected =
        |name: &str| filter.is_empty() || filter.split(',').any(|item| name.contains(item.trim()));
    let cpus =
        std::env::var("OPFORGE_STRUCT_PARAMETER_CPUS").unwrap_or_else(|_| "68020,6502".into());
    let mut failures = Vec::new();
    let mut attempted = 0;
    for cpu in cpus.split(',').map(str::trim) {
        assert!(matches!(cpu, "68020" | "6502"));
        let cases = positive_cases(cpu)
            .into_iter()
            .map(|(name, input, _)| (name, input, None))
            .chain(std::iter::once((
                "combined-positive-matrix",
                positive_matrix(cpu).0,
                None,
            )))
            .chain(std::iter::once((
                "compound-control",
                compound_control(cpu),
                None,
            )))
            .chain(
                BAD.iter()
                    .chain(STRUCT_REJECTIONS)
                    .map(|(name, body, diagnostic)| {
                        (*name, negative_source(cpu, body), Some(*diagnostic))
                    }),
            )
            .chain(
                binding_rejections(cpu)
                    .into_iter()
                    .map(|(name, input, diagnostic)| (name, input, Some(diagnostic))),
            );
        for (name, input, diagnostic) in cases {
            if !selected(name) {
                continue;
            }
            attempted += 1;
            if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
                if let Some(diagnostic) = diagnostic {
                    let error = oracle(&dir.0, &input).expect_err(name);
                    assert!(error.contains(diagnostic), "{cpu}/{name}: {error}");
                }
                run_case(
                    &root,
                    &dir.0,
                    &image,
                    name,
                    cpu,
                    &input,
                    diagnostic.is_some(),
                );
            }))
            .is_err()
            {
                failures.push(format!("{cpu}/{name}"));
            }
        }
    }
    assert!(attempted > 0, "no struct parameter cases selected");
    assert!(
        failures.is_empty(),
        "struct parameter failures: {failures:?}; all selected cases attempted"
    );
}
