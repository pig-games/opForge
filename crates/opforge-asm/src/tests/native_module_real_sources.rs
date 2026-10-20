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
