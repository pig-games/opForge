//! Actual modules behind the remaining private-compilation audit fallbacks.
use super::{private_oracle, private_pipeline_native_cases, PrivateCase, PRIVATE_OUTPUTS};

pub(super) fn cases() -> Vec<PrivateCase> {
    [
        (
            "private-binding-char-predicates",
            "tkvm.amigaos.char_predicates",
            "tkvmIsWhitespace",
            include_str!("../../../../native/motorola68000/amigaos/tkvm/tkvm_char_predicates.asm"),
        ),
        (
            "private-binding-text-names",
            "experimental.amigaos.binary_text_names",
            "normalize",
            include_str!("../../../../native/motorola68000/amigaos/experimental/binary_text_names.asm"),
        ),
    ]
    .into_iter()
    .map(|(name, module, entry, source)| PrivateCase {
        name,
        accepted_modules: 1,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use {module} as worker\n.section code,kind=code\n.long worker.{entry}\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/provider.asm", source.into()),
        ],
    })
    .collect()
}

fn forward_private_case() -> PrivateCase {
    PrivateCase {
        name: "private-binding-forward-private",
        accepted_modules: 1,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.long provider.entry\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/provider.asm", ".module provider\n.cpu 68020\n.pub\n.section code,kind=code\nentry .block\n cmpi.b #9,d0\n beq helper\n moveq #0,d0\n rts\n.bend\n.priv\nhelper\n moveq #1,d0\n rts\n.endsection\n.endmodule\n".into()),
        ],
    }
}

pub(super) fn check(binary: &[u8]) {
    assert!(binary.len() > 6, "actual module payload and root pointer");
    assert!(binary.ends_with(&[0x11, 0x22]));
}

#[test]
fn module_binding_fallback_live_rust_oracles() {
    for case in cases() {
        private_oracle(&case);
    }
}

#[test]
#[ignore = "requires instrumented compact CLI; actual modules from private-compilation fallback audit"]
fn native_module_binding_fallback_disk_progress() {
    private_pipeline_native_cases(true, true, cases());
}

#[test]
fn module_binding_forward_private_live_rust_oracle() {
    private_oracle(&forward_private_case());
}

#[test]
#[ignore = "requires instrumented compact CLI; reduced forward module-private helper"]
fn native_module_binding_forward_private_disk_progress() {
    private_pipeline_native_cases(true, true, vec![forward_private_case()]);
}

// Explicit width distinguishes a forward-name lookup failure from the existing
// rule that automatic branch widths cannot be frozen across movable fragments.
fn forward_private_word_case() -> PrivateCase {
    let mut case = forward_private_case();
    case.name = "private-binding-forward-private-word";
    case.files[1].1 = case.files[1].1.replace("beq helper", "beq.w helper");
    case
}

#[test]
fn module_binding_forward_private_word_live_rust_oracle() {
    private_oracle(&forward_private_word_case());
}

#[test]
#[ignore = "requires instrumented compact CLI; explicit-width forward module-private helper"]
fn native_module_binding_forward_private_word_disk_progress() {
    private_pipeline_native_cases(true, true, vec![forward_private_word_case()]);
}

// Dependency scaffolds retain only the real ABI declarations reached by EXVM.
// This qualifies the byte-exact producer, not the complete runtime dependencies.
fn declaration(source: &str, name: &str) -> String {
    source
        .lines()
        .find(|line| {
            line.strip_prefix(name)
                .is_some_and(|tail| tail.starts_with(char::is_whitespace))
        })
        .unwrap_or_else(|| panic!("missing current ABI declaration {name}"))
        .to_owned()
}

fn record(source: &str, name: &str) -> String {
    let start = source.find(&format!("{name}\t.struct")).unwrap();
    let end = start + source[start..].find(".endstruct").unwrap() + ".endstruct".len();
    source[start..end].to_owned()
}

fn exvm_case() -> PrivateCase {
    const SOURCE: &str =
        include_str!("../../../../native/motorola68000/amigaos/experimental/binary_source.asm");
    const ABI: &str = include_str!("../../../../native/motorola68000/amigaos/prvm/prvm_abi.asm");
    const TOKENS: &str =
        include_str!("../../../../native/motorola68000/amigaos/tkvm/tkvm_runtime.asm");
    let mut source_abi = String::from(".module experimental.amigaos.binary_source\n.pub\n");
    for name in [
        "PRVM_TOKEN_KIND_WIDE_NUMBER",
        "PRVM_NUMBER_BYTES",
        "PRVM_WIDE_NUMBER_BYTES",
        "PRVM_TOKEN_KIND_REGISTER",
    ] {
        source_abi.push_str(&declaration(ABI, name));
        source_abi.push('\n');
    }
    for name in [
        "TOKEN_WIDE_NUMBER",
        "NUMBER_BYTES",
        "WIDE_NUMBER_BYTES",
        "TOKEN_NAME_MAX",
        "FIELD_KEY_QUALIFIER",
        "SYNTAX_MEMBER_QUALIFIER",
    ] {
        source_abi.push_str(&declaration(SOURCE, name).replace("abi.", ""));
        source_abi.push('\n');
    }
    for name in ["NameToken", "NumberToken", "WideNumberToken"] {
        source_abi.push_str(&record(SOURCE, name));
        source_abi.push('\n');
    }
    source_abi.push_str(&declaration(SOURCE, "NAME_TOKEN_BYTES"));
    source_abi.push('\n');
    source_abi.push_str(".endmodule\n");
    let mut tokens_abi = String::from(".module tkvm.amigaos.runtime\n.pub\n");
    for line in TOKENS.lines().filter(|line| line.starts_with("TK_KIND_")) {
        tokens_abi.push_str(line);
        tokens_abi.push('\n');
    }
    tokens_abi.push_str(".endmodule\n");
    PrivateCase {
        name: "private-binding-exvm-producer",
        accepted_modules: 3,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use experimental.amigaos.binary_exvm as worker\n.section code,kind=code\n.long worker.compile\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/provider.asm", include_str!("../../../../native/motorola68000/amigaos/experimental/binary_exvm.asm").into()),
            ("library/source.asm", source_abi),
            ("library/tokens.asm", tokens_abi),
            ("library/lazy_work.i", include_str!("../../../../native/motorola68000/amigaos/debug/lazy_work.i").into()),
            ("library/telemetry_macros.i", include_str!("../../../../native/motorola68000/amigaos/debug/telemetry_macros.i").into()),
            ("library/telemetry_macros_noop.i", include_str!("../../../../native/motorola68000/amigaos/debug/telemetry_macros_noop.i").into()),
        ],
    }
}

#[test]
fn module_binding_exvm_live_rust_oracle() {
    private_oracle(&exvm_case());
}

#[test]
#[ignore = "requires instrumented compact CLI; exact EXVM producer with current ABI projections"]
fn native_module_binding_exvm_disk_progress() {
    private_pipeline_native_cases(true, true, vec![exvm_case()]);
}

// Keep the producer byte-exact and use its actual interface-only ABI module.
// This isolates the descriptor producer from the complete application's graph.
fn macro_descriptors_case() -> PrivateCase {
    PrivateCase {
        name: "private-binding-macro-descriptors",
        accepted_modules: 2,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use prvm.amigaos.macro_descriptors as worker\n.section code,kind=code\n.long worker.run\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/provider.asm", include_str!("../../../../native/motorola68000/amigaos/prvm/prvm_macro_descriptors.asm").into()),
            ("library/abi.asm", include_str!("../../../../native/motorola68000/amigaos/prvm/prvm_abi.asm").into()),
            ("library/lazy_work.i", include_str!("../../../../native/motorola68000/amigaos/debug/lazy_work.i").into()),
            ("library/telemetry_macros.i", include_str!("../../../../native/motorola68000/amigaos/debug/telemetry_macros.i").into()),
            ("library/telemetry_macros_noop.i", include_str!("../../../../native/motorola68000/amigaos/debug/telemetry_macros_noop.i").into()),
        ],
    }
}

fn trailing_names_case() -> PrivateCase {
    let outputs = PRIVATE_OUTPUTS
        .replace(
            ".place data in ram",
            ".place data in ram\n.place bss in ram",
        )
        .replace(
            "format=hunk,sections=code,data",
            "format=hunk,sections=code,data,bss",
        );
    PrivateCase {
        name: "private-binding-trailing-names-bss",
        accepted_modules: 1,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.long provider.entry\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{outputs}")),
            ("library/provider.asm", ".module provider\n.cpu 68020\n.pub\nFrame .struct\nValue .long ?\n.endstruct\nFRAME_BYTES = Frame.Value+4\n.section code,kind=code\nentry .block\n lea Name,a0\n lea Scratch,a1\n move.l #FRAME_BYTES,d0\n rts\n.bend\nName .byte \"logical\"\n.align 2\n.endsection\n.section bss,kind=bss\nScratch .res byte,FRAME_BYTES\n.endsection\n.endmodule\n".into()),
        ],
    }
}

#[test]
fn module_binding_remaining_audit_live_rust_oracles() {
    for case in [macro_descriptors_case(), trailing_names_case()] {
        private_oracle(&case);
    }
}

#[test]
#[ignore = "requires instrumented compact CLI; remaining actual descriptor producer and trailing-label control"]
fn native_module_binding_remaining_audit_disk_progress() {
    private_pipeline_native_cases(
        true,
        true,
        vec![macro_descriptors_case(), trailing_names_case()],
    );
}

fn pc_relative_strings_cases() -> Vec<PrivateCase> {
    [
        (
            "private-binding-pc-relative-trailing-string",
            "entry .block\n lea Name(PC),a0\n rts\n.bend\n.priv\nName .byte \"X\"\n.align 2\n",
        ),
        (
            "private-binding-pc-relative-prunable-gap",
            "entry .block\n lea Name(PC),a0\n rts\n.bend\n.priv\nunused .block\n.word $1122,$3344\n.bend\nName .byte \"X\"\n.align 2\n",
        ),
        (
            "private-binding-pc-relative-local-string",
            "entry .block\n lea Name(PC),a0\n rts\nName .byte \"X\"\n.align 2\n.bend\n",
        ),
        (
            "private-binding-absolute-trailing-string",
            "entry .block\n lea Name.l,a0\n rts\n.bend\n.priv\nName .byte \"X\"\n.align 2\n",
        ),
    ]
    .into_iter()
    .map(|(name, body)| PrivateCase {
        name,
        accepted_modules: 1,
        files: vec![
            ("entry.asm", format!(".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.long provider.entry\n.endsection\n.section data,kind=data\n.word $1122\n.endsection\n{PRIVATE_OUTPUTS}")),
            ("library/provider.asm", format!(".module provider\n.cpu 68020\n.pub\n.section code,kind=code\n{body}.endsection\n.endmodule\n")),
        ],
    })
    .collect()
}

#[test]
fn module_binding_pc_relative_strings_live_rust_oracles() {
    for case in pc_relative_strings_cases() {
        private_oracle(&case);
    }
}

#[test]
#[ignore = "requires instrumented compact CLI; exact PC-relative operand target across module fragments"]
fn native_module_binding_pc_relative_strings_disk_progress() {
    private_pipeline_native_cases(true, true, pc_relative_strings_cases());
}
