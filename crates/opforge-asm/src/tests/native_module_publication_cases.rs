//! Interface-only modules publish objects without requiring executable payload.
use super::{private_oracle, private_pipeline_native_cases, PrivateCase, PRIVATE_OUTPUTS};

fn value_case() -> PrivateCase {
    PrivateCase {
        name: "private-publication-values-only",
        accepted_modules: 1,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use provider as abi\n.section code,kind=code\n.word $1234\n.endsection\n.section data,kind=data\n.word abi.VALUE,abi.Frame.X\n.byte .len(abi.ITEMS)\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/provider.asm", ".module provider\n.pub\nVALUE .const 5\nITEMS .const {3,9}\nFrame .struct\nPadding .long ?\nX .word ?\n.endstruct\n.endmodule\n".into()),
        ],
    }
}

// Defaults and renamed macro/segment facets follow the existing import syntax.
// Existing macro/segment semantics bind bare body/default names in the caller:
// provider.hidden=7 must not replace consumer.hidden=90 or root inner.hidden=80.
// Qualified PUBLIC provider.VALUE retains its ordinary module identity; no
// definition-site privilege grants access to a provider's private symbols.
// The same canonical EMIT name also owns an independently usable scalar value.
fn templates_case() -> PrivateCase {
    PrivateCase {
        name: "private-publication-templates-only",
        accepted_modules: 2,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use consumer\n.use provider (VALUE,EMIT as root_emit,INLINE as root_inline)\n.section code,kind=code\n.long consumer.entry\n.namespace inner\nhidden .const 80\n.root_emit 10\n.root_inline\n.endnamespace\n.endsection\n.section data,kind=data\n.word consumer.RESULT,root_emit\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/consumer.asm", ".module consumer\n.cpu 68020\n.use provider (VALUE,EMIT as renamed,INLINE as renamed_segment)\n.priv\nhidden .const 90\n.pub\nRESULT .const provider.VALUE+1\n.section code,kind=code\nentry .block\n.renamed\n.renamed_segment 4\n.word consumer.RESULT\n rts\n.bend\n.endsection\n.endmodule\n".into()),
            ("library/provider.asm", ".module provider\n.priv\nhidden .const 7\n.pub\nVALUE .const 5\nEMIT .const $11\nEMIT .macro offset=hidden\nlocal .byte .offset,hidden\n.endmacro\nINLINE .segment datum=hidden\n.byte .datum,hidden,provider.VALUE\n.ends\n.endmodule\n".into()),
        ],
    }
}

// Separate basic publication from the composite case's dependent constants,
// defaults, aliases and caller binding so failures retain a useful boundary.
fn reduced_templates() -> Vec<PrivateCase> {
    [
        ("private-publication-reduced-macro", "EMIT", "macro", "endmacro"),
        ("private-publication-reduced-segment", "INLINE", "segment", "ends"),
    ]
    .into_iter()
    .map(|(name, template, kind, end)| PrivateCase {
        name,
        accepted_modules: 2,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use consumer\n.use provider ({template})\n.section code,kind=code\n.long consumer.entry\n.{template} 8\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/consumer.asm", format!(".module consumer\n.cpu 68020\n.use provider ({template})\n.pub\n.section code,kind=code\nentry .block\n.{template} 7\n rts\n.bend\n.endsection\n.endmodule\n")),
            ("library/provider.asm", format!(".module provider\n.pub\n{template} .{kind} datum\n.word .datum\n.{end}\n.endmodule\n")),
        ],
    })
    .collect()
}

// Defaults intentionally retain caller binding even after selected alias import.
// No body labels or same-name values participate in this isolated control.
fn reduced_default_alias() -> PrivateCase {
    PrivateCase {
        name: "private-publication-reduced-default-alias",
        accepted_modules: 2,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use consumer\n.use provider (EMIT as root_emit)\n.section code,kind=code\n.long consumer.entry\nhidden .const 80\n.root_emit\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/consumer.asm", ".module consumer\n.cpu 68020\n.use provider (EMIT as renamed)\n.priv\nhidden .const 90\n.pub\n.section code,kind=code\nentry .block\n.renamed\n rts\n.bend\n.endsection\n.endmodule\n".into()),
            ("library/provider.asm", ".module provider\n.priv\nhidden .const 7\n.pub\nEMIT .macro datum=hidden\n.word .datum\n.endmacro\n.endmodule\n".into()),
        ],
    }
}

// Scalar and template facets coexist at one canonical name. Explicit arguments
// and unchanged import names keep default and alias behavior out of this case.
fn reduced_same_value() -> PrivateCase {
    PrivateCase {
        name: "private-publication-reduced-same-value",
        accepted_modules: 2,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use consumer\n.use provider (EMIT)\n.section code,kind=code\n.long consumer.entry\n.EMIT 8\n.endsection\n.section data,kind=data\n.word $1122,EMIT\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/consumer.asm", ".module consumer\n.cpu 68020\n.use provider (EMIT)\n.pub\n.section code,kind=code\nentry .block\n.EMIT 7\n rts\n.bend\n.endsection\n.endmodule\n".into()),
            ("library/provider.asm", ".module provider\n.pub\nEMIT .const $11\nEMIT .macro datum\n.word .datum\n.endmacro\n.endmodule\n".into()),
        ],
    }
}

// A body-local label is the only extra feature beyond the simple macro case.
fn reduced_body_label() -> PrivateCase {
    PrivateCase {
        name: "private-publication-reduced-body-label",
        accepted_modules: 2,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use consumer\n.use provider (EMIT)\n.section code,kind=code\n.long consumer.entry\n.EMIT 8\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/consumer.asm", ".module consumer\n.cpu 68020\n.use provider (EMIT)\n.pub\n.section code,kind=code\nentry .block\n.EMIT 7\n rts\n.bend\n.endsection\n.endmodule\n".into()),
            ("library/provider.asm", ".module provider\n.pub\nEMIT .macro datum\nlocal .word .datum\n.endmacro\n.endmodule\n".into()),
        ],
    }
}

// A public dependent value must survive private compilation alongside an
// imported template; both private and ordinary consumers read its final value.
fn reduced_dependent_value() -> PrivateCase {
    PrivateCase {
        name: "private-publication-reduced-dependent-value",
        accepted_modules: 2,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use consumer\n.use provider (EMIT)\n.section code,kind=code\n.long consumer.entry\n.EMIT 8\n.endsection\n.section data,kind=data\n.word $1122,consumer.RESULT\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/consumer.asm", ".module consumer\n.cpu 68020\n.use provider (VALUE,EMIT)\n.pub\nRESULT .const provider.VALUE+1\n.section code,kind=code\nentry .block\n.EMIT 7\n.word consumer.RESULT\n rts\n.bend\n.endsection\n.endmodule\n".into()),
            ("library/provider.asm", ".module provider\n.pub\nVALUE .const 5\nEMIT .macro datum\n.word .datum\n.endmacro\n.endmodule\n".into()),
        ],
    }
}

// Remove template publication entirely while retaining the same dependent
// public expression and both qualified RESULT uses from the failing control.
fn reduced_dependent_value_only() -> PrivateCase {
    PrivateCase {
        name: "private-publication-reduced-dependent-value-only",
        accepted_modules: 2,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use consumer\n.section code,kind=code\n.long consumer.entry\n.endsection\n.section data,kind=data\n.word $1122,consumer.RESULT\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/consumer.asm", ".module consumer\n.cpu 68020\n.use provider (VALUE)\n.pub\nRESULT .const provider.VALUE+1\n.section code,kind=code\nentry .block\n.word consumer.RESULT\n rts\n.bend\n.endsection\n.endmodule\n".into()),
            ("library/provider.asm", ".module provider\n.pub\nVALUE .const 5\n.endmodule\n".into()),
        ],
    }
}

// Consecutive canonical keywords have odd lengths (PUT=3, EMITS=5), and PUT
// retains two overload rows. Imported aliases differ, so copying a spelling pool
// must not leave the following serialized row or plan at an odd address.
fn statements_case() -> PrivateCase {
    PrivateCase {
        name: "private-publication-odd-statement-keywords",
        accepted_modules: 2,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use consumer\n.use provider (PUT as ROOTPUT,EMITS as ROOTEMIT)\n.section code,kind=code\n.long consumer.entry\n.namespace inner\n ROOTPUT 10\n ROOTEMIT 11\n.endnamespace\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/consumer.asm", ".module consumer\n.cpu 68020\n.use provider (PUT as STORE,EMITS as BUMP)\n.pub\n.section code,kind=code\nentry .block\n STORE 7\n STORE \"300\"\n BUMP 8\n rts\n.bend\n.endsection\n.endmodule\n".into()),
            ("library/provider.asm", ".module provider\n.pub\n.statement PUT byte:v\n.byte .v\n.endstatement\n.statement PUT str:v\n.word .v\n.endstatement\n.statement EMITS byte:v\n.byte .v+1\n.endstatement\n.endmodule\n".into()),
        ],
    }
}

// A neutral origin still belongs to ordinary execution; an interface-only
// publisher must explicitly reject the unowned effect rather than dropping it.
fn effect_case() -> PrivateCase {
    PrivateCase {
        name: "private-publication-unowned-origin-fallback",
        accepted_modules: 0,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.word $1234\n.endsection\n.section data,kind=data\n.word provider.VALUE\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/provider.asm", ".module provider\n.cpu 68020\n.org 0\n.pub\nVALUE .const 5\n.endmodule\n".into()),
        ],
    }
}

// Shared text definitions affect later ordinary source and must force explicit
// fallback even when the module otherwise contains only public interface data.
fn text_effect_case() -> PrivateCase {
    PrivateCase {
        name: "private-publication-unowned-text-fallback",
        accepted_modules: 0,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.word $1234\n.endsection\n.section data,kind=data\n.word provider.VALUE\n.enc shared\n.byte \"A\"\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/provider.asm", ".module provider\n.encode shared\n.cdef \"A\",\"A\",9\n.endencode\n.pub\nVALUE .const 5\n.endmodule\n".into()),
        ],
    }
}

pub(super) fn cases() -> Vec<PrivateCase> {
    let mut cases = vec![
        value_case(),
        templates_case(),
        statements_case(),
        text_effect_case(),
        reduced_default_alias(),
        reduced_same_value(),
        reduced_body_label(),
        reduced_dependent_value(),
        reduced_dependent_value_only(),
    ];
    cases.extend(reduced_templates());
    cases
}

// A published producer must retain its declared import order. Reversing two
// dependency-only edges changes emitted bytes even when all values are correct.
fn dependency_order_case() -> PrivateCase {
    PrivateCase {
        name: "private-publication-dependency-order",
        accepted_modules: 3,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use consumer\n.section code,kind=code\n.long consumer.entry\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/consumer.asm", ".module consumer\n.cpu 68020\n.use left\n.use right\n.pub\n.section code,kind=code\nentry .block\n.byte left.VALUE,right.VALUE\n rts\n.bend\n.endsection\n.endmodule\n".into()),
            ("library/left.asm", ".module left\n.cpu 68020\n.pub\nVALUE .const $31\n.section code,kind=code\n.byte $11\n.endsection\n.endmodule\n".into()),
            ("library/right.asm", ".module right\n.cpu 68020\n.pub\nVALUE .const $42\n.section code,kind=code\n.byte $22\n.endsection\n.endmodule\n".into()),
        ],
    }
}

fn dependency_order_cases() -> Vec<PrivateCase> {
    let ordinary = dependency_order_case();
    let mut repeated = dependency_order_case();
    repeated.name = "private-publication-repeated-dependency-order";
    repeated.files[1].1 = repeated.files[1]
        .1
        .replace(".use right\n", ".use right\n.use left as repeated\n");
    vec![ordinary, repeated]
}

#[test]
fn module_publication_dependency_order_live_rust_oracle() {
    for case in dependency_order_cases() {
        private_oracle(&case);
    }
}

#[test]
#[ignore = "requires instrumented compact CLI; retained dependency source order"]
fn native_module_publication_dependency_order_disk_progress() {
    private_pipeline_native_cases(true, true, dependency_order_cases());
}

#[test]
#[ignore = "requires instrumented compact CLI; live configuration dependency order"]
fn native_module_publication_dependency_order_resident_progress() {
    private_pipeline_native_cases(true, false, vec![dependency_order_case()]);
}

// (objects without initialized block bytes, objects with initialized bytes).
// These cases select every payload object; zero-payload objects must never read.
pub(super) fn object_counts(name: &str) -> Option<(usize, usize)> {
    match name {
        "private-publication-dependency-order"
        | "private-publication-repeated-dependency-order" => Some((0, 3)),
        "private-publication-values-only" => Some((1, 0)),
        "private-publication-templates-only" | "private-publication-odd-statement-keywords" => {
            Some((1, 1))
        }
        "private-publication-unowned-origin-fallback"
        | "private-publication-unowned-text-fallback" => Some((0, 0)),
        name if name.starts_with("private-publication-reduced-") => Some((1, 1)),
        _ => None,
    }
}

pub(super) fn check(name: &str, binary: &[u8]) {
    match name {
        "private-publication-dependency-order"
        | "private-publication-repeated-dependency-order" => assert_eq!(
            binary,
            [0x11, 0x22, 0x31, 0x42, 0x4e, 0x75, 0, 0, 0, 2, 0x11, 0x22]
        ),
        "private-publication-values-only" => assert_eq!(binary, [0x12, 0x34, 0, 5, 0, 4, 2]),
        "private-publication-templates-only" => {
            assert!(
                binary
                    .windows(9)
                    .any(|bytes| bytes == [90, 90, 4, 90, 5, 0, 6, 0x4e, 0x75]),
                "private consumer must use its own bare/default bindings"
            );
            assert!(
                binary.windows(5).any(|bytes| bytes == [10, 80, 80, 80, 5]),
                "ordinary root namespace must retain caller bindings"
            );
            assert!(binary.ends_with(&[0, 6, 0, 0x11]));
        }
        "private-publication-odd-statement-keywords" => {
            assert!(
                binary
                    .windows(6)
                    .any(|bytes| bytes == [7, 1, 0x2c, 9, 0x4e, 0x75]),
                "private consumer must retain both PUT overloads and the next odd keyword"
            );
            assert!(
                binary.windows(2).any(|bytes| bytes == [10, 12]),
                "ordinary root namespace must retain imported statement aliases"
            );
            assert!(binary.ends_with(&[0x11, 0x22]));
        }
        "private-publication-reduced-default-alias" => {
            assert!(binary.windows(4).any(|bytes| bytes == [0, 90, 0x4e, 0x75]));
            assert!(binary.windows(2).any(|bytes| bytes == [0, 80]));
            assert!(binary.ends_with(&[0x11, 0x22]));
        }
        "private-publication-reduced-same-value" => {
            assert!(binary.windows(4).any(|bytes| bytes == [0, 7, 0x4e, 0x75]));
            assert!(binary.windows(2).any(|bytes| bytes == [0, 8]));
            assert!(binary.ends_with(&[0x11, 0x22, 0, 0x11]));
        }
        "private-publication-reduced-dependent-value-only" => {
            assert!(binary.windows(4).any(|bytes| bytes == [0, 6, 0x4e, 0x75]));
            assert!(binary.ends_with(&[0x11, 0x22, 0, 6]));
        }
        "private-publication-reduced-dependent-value" => {
            assert!(binary
                .windows(6)
                .any(|bytes| bytes == [0, 7, 0, 6, 0x4e, 0x75]));
            assert!(binary.windows(2).any(|bytes| bytes == [0, 8]));
            assert!(binary.ends_with(&[0x11, 0x22, 0, 6]));
        }
        name if name.starts_with("private-publication-reduced-") => {
            assert!(
                binary.windows(4).any(|bytes| bytes == [0, 7, 0x4e, 0x75]),
                "the private consumer must execute the selected template"
            );
            assert!(
                binary.windows(2).any(|bytes| bytes == [0, 8]),
                "the ordinary root must execute its template independently"
            );
            assert!(binary.ends_with(&[0x11, 0x22]));
        }
        "private-publication-unowned-text-fallback" => assert_eq!(binary, [0x12, 0x34, 0, 5, 9]),
        "private-publication-unowned-origin-fallback" => assert_eq!(binary, [0x12, 0x34, 0, 5]),
        _ => unreachable!(),
    }
}

#[test]
fn module_publication_live_rust_oracles() {
    for case in cases().into_iter().chain([effect_case()]) {
        private_oracle(&case);
    }
}

#[test]
#[ignore = "requires instrumented compact CLI; interface-only values and public templates"]
fn native_module_publication_disk_progress() {
    private_pipeline_native_cases(true, true, cases());
}

#[test]
#[ignore = "requires instrumented compact CLI; public macro/segment execution in both policies"]
fn native_module_publication_templates_progress() {
    private_pipeline_native_cases(true, true, vec![templates_case()]);
}

// The exact .org fixture fails both native policies at provider .org 0, before
// publication. Keep this shared section/origin gap visible outside passing cases.
#[test]
#[ignore = "known shared native .org/section gap at provider .org 0; requires FS-UAE"]
fn native_module_publication_unowned_origin_gap() {
    private_pipeline_native_cases(true, true, vec![effect_case()]);
}

#[test]
fn module_public_template_actual_keeps_caller_privacy() {
    use super::super::native_iterable_loops::{oracle, scratch};
    let dir = scratch();
    for (kind, end) in [("macro", "endmacro"), ("segment", "ends")] {
        let source = format!(
            ".module provider\n.priv\nSECRET .const 7\n.pub\nidentity .{kind} value\n.byte .value\n.{end}\n.endmodule\n.module main\n.cpu 68020\n.use provider (identity)\n.identity provider.SECRET\n.endmodule\n"
        );
        let error = oracle(&dir.0, &source)
            .expect_err("actual argument must not acquire private definition authority");
        assert!(
            error.contains("Symbol is private") && error.contains("SECRET"),
            "{kind}: {error}"
        );
    }
}
