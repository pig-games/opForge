//! Current native modules exercised through the ordinary private CLI pipeline.
use super::{private_oracle, private_pipeline_native_cases, PrivateCase, PRIVATE_OUTPUTS};

const MEMORY: &str =
    include_str!("../../../../native/motorola68000/amigaos/experimental/binary_memory.asm");
const INCLUDES: [(&str, &str); 7] = [
    (
        "library/memory_telemetry.i",
        include_str!("../../../../native/motorola68000/amigaos/debug/memory_telemetry.i"),
    ),
    (
        "library/memory_telemetry_noop.i",
        include_str!("../../../../native/motorola68000/amigaos/debug/memory_telemetry_noop.i"),
    ),
    (
        "library/capture_telemetry.i",
        include_str!("../../../../native/motorola68000/amigaos/debug/capture_telemetry.i"),
    ),
    (
        "library/capture_telemetry_noop.i",
        include_str!("../../../../native/motorola68000/amigaos/debug/capture_telemetry_noop.i"),
    ),
    (
        "library/capture_lifetime.i",
        include_str!("../../../../native/motorola68000/amigaos/debug/capture_lifetime.i"),
    ),
    (
        "library/hotspot_telemetry.i",
        include_str!("../../../../native/motorola68000/amigaos/debug/hotspot_telemetry.i"),
    ),
    (
        "library/hotspot_telemetry_noop.i",
        include_str!("../../../../native/motorola68000/amigaos/debug/hotspot_telemetry_noop.i"),
    ),
];

fn memory_case() -> PrivateCase {
    let mut files = vec![
        (
            "entry.asm",
            format!(
                ".module main\n.cpu 68020\n.use experimental.amigaos.binary_memory as memory\n.section code,kind=code\n.long memory.reserve,memory.release\n.endsection\n.section data,kind=data\n.word memory.Block.Used\n.endsection\n{PRIVATE_OUTPUTS}"
            ),
        ),
        ("library/binary_memory.asm", MEMORY.into()),
    ];
    files.extend(INCLUDES.map(|(path, source)| (path, source.into())));
    PrivateCase {
        name: "private-real-memory",
        files,
        accepted_modules: 1,
    }
}

const BINDING_LAYOUT: &str =
    include_str!("../../../../native/motorola68000/amigaos/experimental/binary_binding_layout.asm");

// Byte-exact current ABI source: its CPU declaration does not create code/data.
fn layout_case() -> PrivateCase {
    PrivateCase {
        name: "private-real-binding-layout",
        accepted_modules: 2,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use worker\n.section code,kind=code\n.long worker.entry\n.endsection\n.section data,kind=data\n.word worker.VALUE\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/worker.asm", ".module worker\n.cpu 68020\n.use experimental.amigaos.binary_binding_layout as layout\n.pub\nVALUE .const layout.Plan.Bytes\n.section code,kind=code\nentry .block\n.word layout.Plan.Bytes,layout.Candidate.MemberCount\n rts\n.bend\n.endsection\n.endmodule\n".into()),
            ("library/binary_binding_layout.asm", BINDING_LAYOUT.into()),
        ],
    }
}

pub(super) fn object_counts(name: &str) -> Option<(usize, usize)> {
    (name == "private-real-binding-layout").then_some((1, 1))
}

// Unlabelled calls introduce lexical hygiene scopes, not exported addresses.
// A labelled invocation still owns a real address and must remain importable.
fn macro_scope_case() -> PrivateCase {
    PrivateCase {
        name: "private-lexical-macro-scopes",
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.long provider.entry,provider.labelled\n.endsection\n.section data,kind=data\n.word provider.FLAG\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/provider.asm", ".module provider\n.cpu 68020\n.priv\nnoop .macro\n.endmacro\nemit .macro value\nlocal=.value\n.byte local\n.endmacro\n.pub\nFLAG=11\n.section code,kind=code\nentry .block\n.noop\n.emit 7\n rts\n.bend\nlabelled .emit 9\n.endsection\n.endmodule\n".into()),
        ],
        accepted_modules: 1,
    }
}

#[test]
fn module_private_real_memory_live_rust_oracle() {
    private_oracle(&memory_case());
    private_oracle(&macro_scope_case());
    private_oracle(&layout_case());
}

#[test]
#[ignore = "requires instrumented compact CLI; current binary_memory source and exact includes"]
fn native_module_private_real_memory_progress() {
    private_pipeline_native_cases(true, true, vec![memory_case()]);
}

#[test]
#[ignore = "requires instrumented compact CLI; lexical-only and labelled macro scopes"]
fn native_module_private_lexical_macro_progress() {
    private_pipeline_native_cases(true, true, vec![macro_scope_case()]);
}

#[test]
#[ignore = "requires instrumented compact CLI; current binding ABI has no payload"]
fn native_module_private_real_binding_layout_progress() {
    private_pipeline_native_cases(true, true, vec![layout_case()]);
}
