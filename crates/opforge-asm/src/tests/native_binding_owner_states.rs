//! Binding owner selection must account for forward values and mutable kinds.
use super::native_active_includes::{live_oracle, selected_names};
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::native_source_cases::{self, Case};
use super::*;

const SIGNED_GUARD_DECLARATIONS: &str = "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if helper == -2\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nhelper .const n/7\nn .const -17\n.byte 22\n";

fn cases(cpu: &str) -> Vec<Case> {
    let entries: &[(&str, &str, &str, Option<&[u8]>)] = &[
        (
            "forward-field-value",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:later}\nlater .const 66\n",
            Some(&[66]),
        ),
        (
            "forward-compound-base",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const later\nlater .const Info {value:66}\n",
            Some(&[66]),
        ),
        (
            "mutable-compound-before-scalar",
            ".byte provider.routine.saved1.value\nprovider.routine.saved1 .set 0\n.byte 7\n",
            "saved1 .var Info {value:66}\n",
            Some(&[66, 7]),
        ),
        (
            "mutable-compound-kind-restored",
            ".byte provider.routine.saved1.value\nprovider.routine.saved1 .set 0\nprovider.routine.saved1 .set provider.routine.Info {value:67}\n.byte provider.routine.saved1.value\n",
            "saved1 .var Info {value:66}\n",
            Some(&[66, 67]),
        ),
        // The longest declaration is physically inside other, although its
        // spelling begins with routine. Keep other (22), prune routine (11).
        (
            "unknown-longer-prefix-different-owner",
            ".byte provider.routine.saved1.value.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\nprovider.routine.saved1.value .const provider.routine.Info {value:later}\nlater .const 66\n.byte 22\n",
            Some(&[22, 66]),
        ),
        // A retained schema descriptor alone must not expose an inactive type.
        // The forward field forces runtime construction rather than a known value.
        (
            "inactive-schema-not-visible",
            ".byte provider.other.saved.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if 0\nHidden .struct\nvalue .byte ?\n.endstruct\n.endif\nsaved .const Hidden {value:later}\nlater .const 66\n.byte 22\n",
            None,
        ),
        (
            "deferred-inactive-schema-not-visible",
            ".byte provider.other.saved.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nHidden .struct\nvalue .byte ?\n.endstruct\n.endif\nenabled .const 0\nsaved .const Hidden {value:later}\nlater .const 66\n.byte 22\n",
            None,
        ),
        (
            "inactive-longer-declaration",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if 0\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\n.byte 22\n",
            Some(&[11, 7]),
        ),
        (
            "forward-enabled-longer-owner",
            ".byte provider.routine.saved1.value.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 66]),
        ),
        (
            "forward-enabled-longer-owner-alias",
            "saved .const provider.routine.saved1.value\n.byte saved.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 66]),
        ),
        (
            "forward-enabled-longer-owner-arithmetic",
            ".byte provider.routine.saved1.value.value+1\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 67]),
        ),
        (
            "forward-disabled-longer-owner",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const 0\n.byte 22\n",
            Some(&[11, 7]),
        ),
        (
            "forward-enabled-whole-signed-scalar",
            "saved .const provider.routine.saved1.value\n.byte saved\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const -17\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 239]),
        ),
        (
            "forward-enabled-whole-address-alias",
            "saved .const provider.routine.saved1.value\n.byte saved-provider.other.start\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\nstart:\n.if enabled\nprovider.routine.saved1.value .const start\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 0]),
        ),
        (
            "forward-enabled-transitive-immutable-guard",
            ".byte provider.routine.saved1.value.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.endif\nenabled .const helper\nhelper .const 1\n.byte 22\n",
            Some(&[22, 66]),
        ),
        (
            "exclusive-longer-declaration-same-id",
            ".byte provider.routine.saved1.value.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const provider.routine.Info {value:66}\n.else\nprovider.routine.saved1.value .const provider.routine.Info {value:77}\n.endif\nenabled .const 1\n.byte 22\n",
            Some(&[22, 66]),
        ),
        (
            "forward-transitive-signed-guard",
            ".byte provider.routine.saved1.value.value\n",
            SIGNED_GUARD_DECLARATIONS,
            Some(&[22, 66]),
        ),
        (
            "same-block-longest-scalar-rejects-member-fallback",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\nprovider.routine.saved1.value .const 66\n.byte 11\n",
            None,
        ),
        // Neither a mutable dependency closure nor a PC initializer may certify
        // this conditional declaration from a preparation-time value snapshot.
        (
            "transitive-mutable-guard-scalar-rejects-fallback",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\nflag .var 1\nhelper .const flag\nenabled .const helper\nflag .set 0\n.if enabled\nprovider.routine.saved1.value .const 66\n.endif\n.byte 11\n",
            None,
        ),
        (
            "transitive-pc-guard-scalar-rejects-fallback",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.byte 11\norigin .const *\nenabled .const origin\n.if enabled\nprovider.routine.saved1.value .const 66\n.endif\n",
            None,
        ),
        (
            "unmaterialized-loop-inactive-longer-keeps-shorter",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {0}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.endfor\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unmaterialized-loop-active-longer-rejects-declaration",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {1}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.endfor\n.byte 11\n",
            None,
        ),
        (
            "unmaterialized-loop-inactive-repeated-longer-keeps-shorter",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {0}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.if flag\nprovider.routine.saved1.value .const 67\n.endif\n.endfor\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unmaterialized-loop-active-repeated-longer-rejects-declaration",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {1}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.if flag\nprovider.routine.saved1.value .const 67\n.endif\n.endfor\n.byte 11\n",
            None,
        ),
        (
            "unmaterialized-loop-zero-unguarded-longer-keeps-shorter",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for 0\nprovider.routine.saved1.value .const missing\n.endfor\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unmaterialized-loop-zero-repeated-unguarded-keeps-shorter",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for 0\nprovider.routine.saved1.value .const missing\nprovider.routine.saved1.value .const missing+1\n.endfor\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unmaterialized-loop-active-unguarded-scalar-rejects-fallback",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for 1\nprovider.routine.saved1.value .const 66\n.endfor\n.byte 11\n",
            None,
        ),
        (
            "unmaterialized-loop-active-whole-constant-rejects-label",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.for 1\nprovider.routine.saved1.value .const 66\n.endfor\n.byte 11\n",
            None,
        ),
        (
            "unmaterialized-loop-mixed-whole-assignment-rejects-label",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.for flag in {0,1}\n.if flag\nprovider.routine.saved1.value = 66\n.endif\n.endfor\n.byte 11\n",
            None,
        ),
        (
            "unmaterialized-loop-zero-assignment-keeps-shorter",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.for 0\nprovider.routine.saved1.value = missing\n.endfor\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unmaterialized-loop-inactive-active-rejects-executed-label",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {0,1}\n.if flag\nprovider.routine.saved1.value .const Info {value:66}\n.endif\n.endfor\n.byte 11\n",
            None,
        ),
        (
            "unmaterialized-loop-active-inactive-rejects-executed-label",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {1,0}\n.if flag\nprovider.routine.saved1.value .const Info {value:66}\n.endif\n.endfor\n.byte 11\n",
            None,
        ),
        (
            "unmaterialized-loop-two-active-iterations-rejects-duplicate",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {1,1}\n.if flag\nprovider.routine.saved1.value .const Info {value:66}\n.endif\n.endfor\n.byte 11\n",
            None,
        ),
        (
            "selected-branch-unentered-loop-keeps-shorter",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.if 1\n.for 0\nprovider.routine.saved1.value .const missing\n.endfor\n.endif\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unmaterialized-loop-exclusive-longer-same-id",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {1}\n.if flag\nprovider.routine.saved1.value .const Info {value:66}\n.else\nprovider.routine.saved1.value .const missing\n.endif\n.endfor\n.byte 11\n",
            None,
        ),
        (
            "unmaterialized-loop-member-alias",
            "saved .const provider.routine.saved1.value\n.byte saved.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {0}\n.if flag\nprovider.routine.saved1.value .const Info {value:66}\n.endif\n.endfor\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unmaterialized-loop-inactive-member-arithmetic",
            ".byte provider.routine.saved1.value.value+1\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {0}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.endfor\n.byte 11\n",
            Some(&[11, 8]),
        ),
        (
            "unmaterialized-loop-forward-member-list",
            ".byte .len({provider.routine.saved1.value.value,1})\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {0}\n.if flag\nprovider.routine.saved1.value .const Info {value:66}\n.endif\n.endfor\n.byte 11\n",
            Some(&[11, 2]),
        ),
        (
            "unmaterialized-loop-forward-member-constructor",
            "picked .const provider.routine.Info {value:provider.routine.saved1.value.value}\n.byte picked.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {0}\n.if flag\nprovider.routine.saved1.value .const Info {value:66}\n.endif\n.endfor\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unmaterialized-loop-inactive-member-range",
            ".byte .len(1..=provider.routine.saved1.value.value)\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.for flag in {0}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.endfor\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unresolved-outer-guard-unentered-loop-keeps-shorter",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.byte 11\n.if enabled\n.for 1\nprovider.routine.saved1.value .const missing\n.endfor\n.endif\nenabled .const start-start\nstart:\n.byte 0\nfinish:\n",
            Some(&[11, 0, 7]),
        ),
        (
            "observed-active-global-guard-loop-label-rejects",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.if enabled\n.for 1\nprovider.routine.saved1.value .const 66\n.endfor\n.endif\nenabled .const start-start+1\nstart:\n.byte 0\n",
            None,
        ),
        (
            "observed-unresolved-global-guard-rejects",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.if enabled\n.for 1\nprovider.routine.saved1.value .const 66\n.endfor\n.endif\nenabled .const start-start\n.if 0\nstart:\n.endif\n.byte 0\n",
            None,
        ),
        (
            "observed-forward-label-value-changes",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.byte 11\n.if enabled\n.byte 22\n.endif\n.for 0\nprovider.routine.saved1.value .const missing\n.endfor\nenabled .const start>0\nstart:\n.byte 0\n",
            Some(&[11, 22, 0, 7]),
        ),
        (
            "observed-scalar-label-becomes-inactive",
            ".byte provider.routine.saved1.value.value,provider.routine.stale\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.byte 11\n.if !enabled\nstale:\n.byte 22\n.endif\n.for 0\nprovider.routine.saved1.value .const missing\n.endfor\nenabled .const start>0\nstart:\n.byte 0\n",
            None,
        ),
        (
            "observed-forward-while-still-unresolved-rejects",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.while enabled\nprovider.routine.saved1.value .const missing\n.endwhile\nenabled .const start-start\n.if 0\nstart:\n.endif\n.byte 0\n",
            None,
        ),
        (
            "forward-immutable-zero-loop-declaration-keeps-shorter",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.for count\nprovider.routine.saved1.value .const missing\n.endfor\ncount .const base-1\nbase .const 1\n.byte 11\n",
            Some(&[11, 7]),
        ),
        (
            "unresolved-while-guard-keeps-shorter",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.byte 11\n.while enabled\nprovider.routine.saved1.value .const missing\n.endwhile\nenabled .const start-start\nstart:\n.byte 0\n",
            Some(&[11, 0, 7]),
        ),
        (
            "unresolved-while-guard-without-binding-query",
            ".byte provider.routine\n",
            "saved1 .const Info {value:7}\n.byte 11\n.while enabled\nprovider.routine.saved1.value .const missing\n.endwhile\nenabled .const start-start\nstart:\n.byte 0\n",
            Some(&[11, 0, 0]),
        ),
        (
            "active-while-whole-constant-rejects-label",
            ".byte provider.routine.saved1.value\n",
            "saved1 .const Info {value:7}\n.while 1\nprovider.routine.saved1.value .const 66\n.endwhile\n.byte 11\n",
            None,
        ),
        (
            "mapped-early-region-baseline",
            ".section code,kind=code\n.byte provider.routine.saved1.value\n.endsection\n.section data,kind=data\n.byte 9\n.endsection\n.place code in ram\n.place data in ram\n.output \"out.bin\",format=bin,sections=code,data\n",
            "saved1 .const Info {value:7}\n.byte 11\n",
            Some(&[7, 9, 11]),
        ),
        (
            "mapped-early-region-zero-loop-declaration-keeps-shorter",
            ".section code,kind=code\n.byte provider.routine.saved1.value\n.endsection\n.section data,kind=data\n.byte 9\n.endsection\n.place code in ram\n.place data in ram\n.output \"out.bin\",format=bin,sections=code,data\n",
            "saved1 .const Info {value:7}\n.for 0\nprovider.routine.saved1.value .const missing\n.endfor\n.byte 11\n",
            Some(&[7, 9, 11]),
        ),
        (
            "mapped-early-region-inactive-loop-declaration-keeps-shorter",
            ".section code,kind=code\n.byte provider.routine.saved1.value\n.endsection\n.section data,kind=data\n.byte 9\n.endsection\n.place code in ram\n.place data in ram\n.output \"out.bin\",format=bin,sections=code,data\n",
            "saved1 .const Info {value:7}\n.for flag in {0}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.endfor\n.byte 11\n",
            Some(&[7, 9, 11]),
        ),
        (
            "mapped-zero-loop-declaration-keeps-shorter",
            ".section code,kind=code\n.byte provider.routine.saved1.value\n.endsection\n.section data,kind=data\n.byte 9\n.endsection\n.region ram,0,$ffff\n.place code in ram\n.place data in ram\n.output \"out.bin\",format=bin,sections=code,data\n",
            "saved1 .const Info {value:7}\n.for 0\nprovider.routine.saved1.value .const missing\n.endfor\n.byte 11\n",
            Some(&[7, 9, 11]),
        ),
        (
            "mapped-inactive-loop-declaration-keeps-shorter",
            ".section code,kind=code\n.byte provider.routine.saved1.value\n.endsection\n.section data,kind=data\n.byte 9\n.endsection\n.region ram,0,$ffff\n.place code in ram\n.place data in ram\n.output \"out.bin\",format=bin,sections=code,data\n",
            "saved1 .const Info {value:7}\n.for flag in {0}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.endfor\n.byte 11\n",
            Some(&[7, 9, 11]),
        ),
        // A shorter address record has a valid nested member path. Selecting
        // the longer scalar must reject rather than fall back to that record.
        (
            "executed-longest-scalar-rejects-member-fallback",
            ".byte provider.routine.saved1.value.value\n",
            "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n.byte 11\n.bend\nother .block\n.if enabled\nprovider.routine.saved1.value .const 66\n.endif\nenabled .const 1\n.byte 22\n",
            None,
        ),
    ];
    let names = entries.iter().map(|entry| entry.0).collect::<Vec<_>>();
    let selected = selected_names("OPFORGE_BINDING_OWNER_CASES", &names);
    entries
        .iter()
        .filter(|entry| selected.contains(&entry.0))
        .map(|(name, body, declarations, expected)| {
            let mapped = name.starts_with("mapped-");
            // Compare preparation order independently of the ownership body.
            // The original late-region cases remain valid Rust positives.
            let early_region = if name.starts_with("mapped-early-region-") { ".region ram,0,$ffff\n" } else { "" };
            let import = if mapped { ".use provider map { logical -> data }" } else { ".use provider" };
            let section = if mapped { ".section logical,kind=data,logical\n" } else { "" };
            let end_section = if mapped { ".endsection\n" } else { "" };
            Case {
            name: format!("{name}-{cpu}"),
            cpu: cpu.into(),
            files: vec![
                ("entry.asm", format!(".module app\n.cpu {cpu}\n{early_region}{import}\n{body}.endmodule\n")),
                ("provider.asm", format!(".module provider\n.cpu {cpu}\n.pub\n{section}routine .block\nInfo .struct\nvalue .byte ?\n.endstruct\n{declarations}.bend\n{end_section}.endmodule\n")),
            ],
            expected: expected.map(|bytes| bytes.to_vec()),
        }})
        .collect()
}

fn control_source() -> String {
    // The app is the physical entry root. The inline imported provider retains
    // exactly the same alternative owners as the file-tree signed guard case.
    format!(
        ".module app\n.cpu 68020\n.use provider\n.byte provider.routine.saved1.value.value\n.endmodule\n.module provider\n.cpu 68020\n.pub\nroutine .block\nInfo .struct\nvalue .byte ?\n.endstruct\n{SIGNED_GUARD_DECLARATIONS}.bend\n.endmodule\n"
    )
}

fn activity_control_source() -> &'static str {
    concat!(
        ".module app\n.cpu 68020\n.use provider\n.byte provider.routine.saved1.value.value\n.endmodule\n",
        ".module provider\n.cpu 68020\n.pub\nroutine .block\nInfo .struct\nvalue .byte ?\n.endstruct\n",
        "items .bfor 1\nvalue .const Info {value:7}\n.endfor\nsaved1 .const items[0]\n",
        ".for flag in {0}\n.if flag\nprovider.routine.saved1.value .const 66\n.endif\n.endfor\n.byte 11\n.bend\n.endmodule\n",
    )
}

#[test]
fn native_binding_owner_activity_control_rust_oracle() {
    let dir = scratch();
    assert_eq!(oracle(&dir.0, activity_control_source()).unwrap(), [11, 7]);
}

#[test]
#[ignore = "requires configured FS-UAE; MEMD control allocating declaration activity maps"]
fn native_binding_owner_activity_control_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    run_case(
        &root,
        &dir.0,
        &image,
        "declaration-activity-control",
        "68020",
        activity_control_source(),
        false,
    );
}

#[test]
fn native_binding_owner_control_rust_oracle() {
    let dir = scratch();
    assert_eq!(oracle(&dir.0, &control_source()).unwrap(), [22, 66]);
}

#[test]
#[ignore = "requires configured FS-UAE; timed and MEMD conditional owner control"]
fn native_binding_owner_control_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    run_case(
        &root,
        &dir.0,
        &image,
        "conditional-owner-signed-control",
        "68020",
        &control_source(),
        false,
    );
}

#[test]
fn native_binding_owner_states_rust_oracles() {
    for cpu in selected_names("OPFORGE_INCLUDE_CPUS", &["68020", "6502"]) {
        for case in cases(cpu) {
            let dir = scratch();
            match case.expected {
                Some(expected) => {
                    assert_eq!(live_oracle(&dir.0, &case.files), expected, "{}", case.name);
                }
                None => {
                    for (path, source) in &case.files {
                        fs::write(dir.0.join(path), source).unwrap();
                    }
                    let result = oracle(&dir.0, &case.files[0].1);
                    assert!(
                        result.is_err(),
                        "Rust accepted expected rejection {}: {result:?}",
                        case.name
                    );
                }
            }
        }
    }
}

#[test]
#[ignore = "requires configured FS-UAE; forward and mutable member owner states"]
fn native_binding_owner_states_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_TEMPLATE_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let all = selected_names("OPFORGE_INCLUDE_CPUS", &["68020", "6502"])
        .into_iter()
        .flat_map(cases)
        .collect::<Vec<_>>();
    native_source_cases::run(&root, &image, &all);
}
