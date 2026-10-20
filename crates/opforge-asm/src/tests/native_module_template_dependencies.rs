//! Public value and template facets remain usable across privately compiled modules.
//! Both provider and consumer must publish, including template selection and shadowing.
use super::{private_pipeline_native_cases, PrivateCase, PRIVATE_OUTPUTS};

const ROOT: &str = r#".module main
.cpu 68020
.use consumer
.section code,kind=code
.long consumer.entry
.endsection
.section data,kind=data
.word consumer.RESULT
.endsection
"#;

const PROVIDER: &str = r#".module provider
.cpu 68020
.pub
SECRET .const $60
VALUE .const 5
EMIT .const $11
ITEMS .const {3,9}
Frame .struct
Padding .long ?
X .word ?
.endstruct
SEED .const Frame{Padding:0,X:7}
EMIT .macro offset
.byte provider.SECRET+.offset
.endmacro
.statement PUT byte:v
.byte provider.SECRET+.v
.endstatement
@@EXTRA@@.section code,kind=code
service .block
 rts
.bend
.endsection
.endmodule
"#;

pub(super) fn cases() -> Vec<PrivateCase> {
    [
        ("private-template-dependency-values", "", 2),
        ("private-template-dependency-macro", ".EMIT 10\n", 2),
        ("private-template-dependency-statement", " PUT 11\n", 2),
        ("private-template-dependency-directive-shadow", ".byte 12\n", 2),
        ("private-template-dependency-opcode-shadow", " nop\n", 2),
        ("private-template-dependency-local-statement", " PUT 300\n", 2),
        (
            "private-template-dependency-local-shadow",
            ".namespace inner\n.priv\nEMIT .macro offset\n.byte consumer.CALLER+.offset\n.endmacro\n.EMIT 14\n.endnamespace\n",
            2,
        ),
    ]
    .into_iter()
    .map(|(name, invocation, accepted)| case(name, invocation, accepted))
    .collect()
}

fn case(name: &'static str, invocation: &str, accepted_modules: u32) -> PrivateCase {
    let local = if name == "private-template-dependency-local-statement" {
        ".statement PUT word:s\n.byte $2f\n.endstatement\n"
    } else {
        ""
    };
    let extra = match name {
        "private-template-dependency-directive-shadow" => {
            "byte .macro offset\n.word provider.SECRET+.offset\n.endmacro\n"
        }
        "private-template-dependency-opcode-shadow" => {
            ".statement nop\n.byte provider.SECRET+13\n.endstatement\n"
        }
        _ => "",
    };
    let consumer = format!(
        r#".module consumer
.cpu 68020
.use provider (*)
.priv
CALLER .const $20
{local}.pub
RESULT .const provider.VALUE+1
.section code,kind=code
entry .block
{invocation}.word consumer.RESULT,provider.EMIT
.long provider.service
 rts
.bend
.endsection
.endmodule
"#
    );
    PrivateCase {
        name,
        accepted_modules,
        files: vec![
            ("entry.asm", format!("{ROOT}{PRIVATE_OUTPUTS}")),
            ("library/consumer.asm", consumer),
            ("library/provider.asm", PROVIDER.replace("@@EXTRA@@", extra)),
        ],
    }
}

// An unrelated core-shadowing failure remains visible in both native policies.
pub(super) fn core_root_case() -> PrivateCase {
    let mut case = cases()
        .into_iter()
        .find(|case| case.name == "private-template-dependency-directive-shadow")
        .unwrap();
    case.name = "private-template-dependency-core-root-gap";
    case.files[0].1 = case.files[0]
        .1
        .replace(".word consumer.RESULT", ".byte consumer.RESULT");
    case
}

// Shared native binding gap: an unqualified enclosing constant is captured as
// an import proxy when the module has a wildcard import. Keep this exact source
// separate from the object-lifetime qualification, which uses a qualified name.
pub(super) fn wildcard_parent_case() -> PrivateCase {
    let mut case = cases().remove(0);
    case.name = "private-template-dependency-wildcard-parent-gap";
    case.files[1].1 = case.files[1].1.replace(
        ".word consumer.RESULT,provider.EMIT",
        ".word RESULT,provider.EMIT",
    );
    case
}

pub(super) fn check(name: &str, binary: &[u8]) {
    assert!(
        binary.contains(&6),
        "the imported constant must remain usable"
    );
    let marker = match name {
        "private-template-dependency-values"
        | "private-template-dependency-wildcard-parent-gap" => None,
        "private-template-dependency-macro" => Some(0x6a),
        "private-template-dependency-statement" => Some(0x6b),
        "private-template-dependency-directive-shadow"
        | "private-template-dependency-core-root-gap" => Some(0x6c),
        "private-template-dependency-opcode-shadow" => Some(0x6d),
        "private-template-dependency-local-shadow" => Some(0x2e),
        "private-template-dependency-local-statement" => Some(0x2f),
        _ => unreachable!(),
    };
    if let Some(marker) = marker {
        assert_eq!(
            binary.iter().filter(|byte| **byte == marker).count(),
            1,
            "{name}: {binary:02x?}"
        );
        assert!(
            name.starts_with("private-template-dependency-local-")
                || !binary.contains(&(marker - 0x40)),
            "use declaration context"
        );
    }
}

#[test]
#[ignore = "requires instrumented compact CLI; public value/template facet publication and selection"]
fn native_module_private_template_facets_progress() {
    private_pipeline_native_cases(true, true, cases());
}

#[test]
#[ignore = "known reference binding gap: wildcard import captures enclosing RESULT as consumer.entry.result; requires FS-UAE"]
fn native_module_private_wildcard_parent_binding_gap() {
    private_pipeline_native_cases(true, true, vec![wildcard_parent_case()]);
}

#[test]
#[ignore = "known reference gap: public byte macro breaks root data emission; requires FS-UAE"]
fn native_module_private_core_template_root_gap() {
    private_pipeline_native_cases(true, true, vec![core_root_case()]);
}
