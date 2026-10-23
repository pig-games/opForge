//! Fresh source-tree proof for interleaved configuration and module publication.
use super::native_iterable_loops::{build_image, oracle_with_module_roots, scratch};
use super::native_source_cases::Case;
use super::*;

fn cases() -> Vec<Case> {
    vec![
        // aaa and pbq collide under every supported scope bucket mask. Parent
        // lookup must preserve its query while inspecting a different candidate.
        Case {
            name: "module-pipeline-colliding-parent-spellings".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\naaa=11\npbq=22\nroutine .block\n.byte aaa,pbq\n.bend\n.byte routine\n.endmodule\n".into()),
                ("library/unused.asm", ".module unused\n.cpu 68020\n.endmodule\n".into()),
            ],
            expected: Some(vec![11, 22, 0]),
        },
        // Qualified displacement names must not hide the consuming base-register
        // role when a macro actual has no scalar declaration.
        Case {
            name: "module-pipeline-macro-qualified-register-base".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use provider as layout\nemit .macro ownerArg\n lea layout.OFF(.ownerArg),a0\n.endmacro\nrun .block\n .emit a6\n.bend\n.endmodule\n".into()),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.pub\nOFF = 4\n.endmodule\n".into()),
            ],
            expected: Some(vec![0x41, 0xee, 0, 4]),
        },
        Case {
            name: "module-pipeline-macro-qualified-struct-register-base".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use provider as layout\nemit .macro ownerArg\n move.l layout.Frame.Value(.ownerArg),d1\n.endmacro\nrun .block\n .emit a6\n.bend\n.endmodule\n".into()),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.pub\nFrame .struct\nPadding .long ?\nValue .long ?\n.endstruct\n.endmodule\n".into()),
            ],
            expected: Some(vec![0x22, 0x2e, 0, 4]),
        },
        // The provider is closed before its public template runs in main.
        // Default arguments and ordinary names retain their existing invocation context.
        Case {
            name: "module-pipeline-late-template-invocation-context".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use provider (emit)\n.use sibling\nhidden=90\n.emit\n.emit 2\n.endmodule\n".into()),
                ("library/provider.asm", ".module provider\n.cpu 68020\nhidden=7\n.pub\nemit .macro value=hidden\n.byte .value,hidden\n.endmacro\n.endmodule\n".into()),
                ("library/sibling.asm", ".module sibling\n.cpu 68020\n.byte 3\n.endmodule\n".into()),
            ],
            expected: Some(vec![3, 90, 90, 2, 90]),
        },
        Case {
            name: "module-pipeline-typed-scalar-chain".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\nInfo .struct\nvalue .byte ?\n.endstruct\n.use middle with (VALUE=Info{value:7},COUNT=5)\n.byte middle.RESULT\n.endmodule\n".into()),
                ("library/middle.asm", ".module middle\n.cpu 68020\n.use leaf with (VALUE=VALUE,COUNT=COUNT)\n.pub\nRESULT .const leaf.RESULT\n.byte leaf.RESULT\n.endmodule\n".into()),
                ("library/leaf.asm", ".module leaf\n.cpu 68020\n.pub\nRESULT .const VALUE.value+COUNT\n.endmodule\n".into()),
            ],
            expected: Some(vec![12, 12]),
        },
        Case {
            name: "module-pipeline-template-include-before-sibling".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use provider (LOAD)\n.use sibling\n.LOAD \"parts/payload.inc\"\n.byte 10\n.endmodule\n".into()),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.pub\nLOAD .macro file\n.include .file\n.endmacro\n.LOAD \"parts/payload.inc\"\n.byte 8\n.endmodule\n".into()),
                ("library/parts/payload.inc", ".byte 7\n.if 0\n.include \"missing.inc\"\n.endif\n".into()),
                // Wrong invocation-relative resolution must change the artifact.
                ("parts/payload.inc", ".byte 99\n".into()),
                ("library/sibling.asm", ".module sibling\n.cpu 68020\n.byte 9\n.endmodule\n".into()),
            ],
            expected: Some(vec![7, 8, 9, 7, 10]),
        },
        Case {
            name: "module-pipeline-diamond-multiple-modules".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use left\n.use right\n.byte left.VALUE,right.VALUE\n.endmodule\n".into()),
                ("library/branches.asm", ".module left\n.cpu 68020\n.use leaf\n.pub\nVALUE .const leaf.VALUE+1\n.byte 2\n.endmodule\n.module right\n.cpu 68020\n.use leaf\n.pub\nVALUE .const leaf.VALUE+2\n.byte 3\n.endmodule\n".into()),
                ("library/leaf.asm", ".module leaf\n.cpu 68020\n.pub\nVALUE .const 4\n.byte 1\n.endmodule\n".into()),
            ],
            expected: Some(vec![1, 2, 3, 5, 6]),
        },
        Case {
            name: "module-pipeline-map-consumption-forward-layout".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use provider map { logical -> code }\n.use sibling\n.region ram,0,$ffff\n.section data,kind=data\n.byte 9,provider.anchor,sibling.VALUE\n.endsection\n.section code,kind=code\n.byte 1\n.endsection\n.place data in ram\n.place code in ram\n.endmodule\n".into()),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.pub\n.section logical,kind=code,logical\nanchor .block\n.if enabled\n.byte VALUE\n.else\n.byte 99\n.endif\n.word finish-anchor\nfinish\n.bend\n.endsection\nVALUE .const 2\nenabled .const 1\n.endmodule\n".into()),
                ("library/sibling.asm", ".module sibling\n.cpu 68020\n.pub\nVALUE .const 8\n.endmodule\n".into()),
            ],
            expected: Some(vec![9, 4, 8, 1, 2, 0, 3]),
        },
        Case {
            name: "module-pipeline-late-map-for-published-shared-leaf".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use left\n.use right\n.endmodule\n".into()),
                ("library/left.asm", ".module left\n.cpu 68020\n.use shared\n.endmodule\n".into()),
                ("library/right.asm", ".module right\n.cpu 68020\n.use shared map { logical -> code }\n.region ram,0,$ffff\n.section code,kind=code\n.byte 9\n.word shared.entry\n.endsection\n.place code in ram\n.endmodule\n".into()),
                ("library/shared.asm", ".module shared\n.cpu 68020\n.pub\n.section logical,kind=code,logical\nentry .block\n.byte 7\n.bend\n.endsection\n.endmodule\n".into()),
            ],
            expected: Some(vec![9, 0, 3, 7]),
        },
        Case {
            name: "module-pipeline-late-conflicting-parameters".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use left\n.use right\n.endmodule\n".into()),
                ("library/left.asm", ".module left\n.cpu 68020\n.use shared\n.endmodule\n".into()),
                ("library/right.asm", ".module right\n.cpu 68020\n.use shared with (PARAM=7)\n.endmodule\n".into()),
                ("library/shared.asm", ".module shared\n.cpu 68020\n.ifdef PARAM\n.byte PARAM\n.else\n.byte 1\n.endif\n.endmodule\n".into()),
            ],
            expected: None,
        },
        Case {
            name: "module-pipeline-repeated-compatible-parameters".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use left\n.use right\n.endmodule\n".into()),
                ("library/left.asm", ".module left\n.cpu 68020\n.use shared with (PARAM=7)\n.endmodule\n".into()),
                ("library/right.asm", ".module right\n.cpu 68020\n.use shared with (PARAM=7)\n.endmodule\n".into()),
                ("library/shared.asm", ".module shared\n.cpu 68020\n.byte PARAM\n.endmodule\n".into()),
            ],
            expected: Some(vec![7]),
        },
        Case {
            name: "module-pipeline-reordered-parameter-keys".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use shared as first with (A=7,B=8)\n.use shared as second with (B=8,A=7)\n.byte first.VALUE\n.endmodule\n".into()),
                ("library/shared.asm", ".module shared\n.cpu 68020\n.pub\nVALUE .const A+B\n.endmodule\n".into()),
            ],
            expected: Some(vec![15]),
        },
        Case {
            name: "module-pipeline-missing-parameter-key".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use shared as first with (A=7,B=8)\n.use shared as second with (A=7)\n.byte first.VALUE\n.endmodule\n".into()),
                ("library/shared.asm", ".module shared\n.cpu 68020\n.pub\nVALUE .const A+B\n.endmodule\n".into()),
            ],
            expected: None,
        },
        Case {
            name: "module-pipeline-duplicate-parameter-key".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use shared as first with (A=7,B=8)\n.use shared as second with (A=7,A=7)\n.byte first.VALUE\n.endmodule\n".into()),
                ("library/shared.asm", ".module shared\n.cpu 68020\n.pub\nVALUE .const A+B\n.endmodule\n".into()),
            ],
            expected: None,
        },
        Case {
            name: "module-pipeline-different-parameter-keys".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use shared as first with (A=7,B=8)\n.use shared as second with (A=7,C=8)\n.byte first.VALUE\n.endmodule\n".into()),
                ("library/shared.asm", ".module shared\n.cpu 68020\n.pub\nVALUE .const A+B\n.endmodule\n".into()),
            ],
            expected: None,
        },
        Case {
            name: "module-pipeline-header-before-map".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.region ram,0,$ffff\n.section header,kind=data\n.byte 9\n.endsection\n.use provider map { logical -> code }\n.section code,kind=code\n.word provider.entry\n.endsection\n.place header in ram\n.place code in ram\n.endmodule\n".into()),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.pub\n.section logical,kind=code,logical\nentry .block\n.byte 7\n.bend\n.endsection\n.endmodule\n".into()),
            ],
            expected: Some(vec![9, 0, 3, 7]),
        },
        Case {
            name: "module-pipeline-reversed-two-map-order".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use left\n.use right\n.endmodule\n".into()),
                ("library/left.asm", ".module left\n.cpu 68020\n.use low\n.endmodule\n".into()),
                ("library/right.asm", ".module right\n.cpu 68020\n.use high map { upper -> high_code }\n.use low map { lower -> low_code }\n.region ram,0,$ffff\n.section low_code,kind=code\n.byte 1\n.endsection\n.section header,kind=data\n.word low.entry,high.entry\n.endsection\n.section high_code,kind=code\n.byte 2\n.endsection\n.place low_code in ram\n.place header in ram\n.place high_code in ram\n.endmodule\n".into()),
                ("library/low.asm", ".module low\n.cpu 68020\n.pub\n.section lower,kind=code,logical\nentry .block\n.byte $11\n.bend\n.endsection\n.endmodule\n".into()),
                ("library/high.asm", ".module high\n.cpu 68020\n.pub\n.section upper,kind=code,logical\nentry .block\n.byte $44\n.bend\n.endsection\n.endmodule\n".into()),
            ],
            expected: Some(vec![1, 17, 0, 1, 0, 7, 2, 68]),
        },
        Case {
            name: "module-pipeline-unmapped-logical-geometry".into(),
            cpu: "68020".into(),
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.region ram,0,$ffff\n.section code,kind=code,logical\n.byte 7\n.endsection\n.section code,kind=code\n.byte 9\n.endsection\n.place code in ram\n.endmodule\n".into()),
                ("library/unused.asm", ".module unused\n.cpu 68020\n.endmodule\n".into()),
            ],
            expected: Some(vec![7, 9]),
        },
    ]
}

#[test]
fn module_pipeline_source_trees_have_live_rust_oracles() {
    for case in cases() {
        let dir = scratch();
        for (relative, contents) in &case.files {
            let path = dir.0.join(relative);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, contents).unwrap();
        }
        let actual = oracle_with_module_roots(&dir.0, &case.files[0].1, &["library"]);
        match case.expected {
            Some(expected) => assert_eq!(
                actual.unwrap_or_else(|error| panic!("{}: {error:?}", case.name)),
                expected,
                "{}",
                case.name
            ),
            None => assert!(actual.is_err(), "{} unexpectedly accepted", case.name),
        }
    }
}

#[test]
#[ignore = "requires fresh FS-UAE native completion and current compact CLI image"]
fn native_module_pipeline_discovery_exact_artifacts() {
    native_discovery(false);
}

#[test]
#[ignore = "requires fresh FS-UAE native completion and current compact CLI image"]
fn native_module_pipeline_disk_discovery_exact_artifacts() {
    native_discovery(true);
}

fn native_discovery(disk: bool) {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_MODULE_PIPELINE_IMAGE")
        .map(|path| fs::read(path).expect("current module-pipeline compact CLI image"))
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let command = if disk {
        "Work:entry.asm --bin Work:out.bin -M Work:library --capture-dir Work:"
    } else {
        "Work:entry.asm --bin Work:out.bin -M Work:library"
    };
    super::native_source_cases::run_discovery_batch_command(&root, &image, &cases(), 4, command);
}

struct PrivateCase {
    name: &'static str,
    files: Vec<(&'static str, String)>,
    accepted_modules: u32,
}

const PRIVATE_OUTPUTS: &str = ".region ram,0,$ffff\n.place code in ram\n.place data in ram\n.output \"build/module.bin\",format=bin,sections=code,data\n.output \"build/module.hunk\",format=hunk,sections=code,data\n.endmodule\n";

#[path = "native_module_binding_fallback.rs"]
mod binding_fallback;
#[path = "native_module_cancellation.rs"]
mod cancellation;
#[path = "native_module_counted_loops.rs"]
mod counted_loops;
#[path = "native_module_data_deltas.rs"]
mod data_deltas;
#[path = "native_module_move_geometry.rs"]
mod move_geometry;
#[path = "native_module_publication_cases.rs"]
mod publication_cases;
#[path = "native_module_real_sources.rs"]
mod real_sources;
#[path = "native_module_rigid_span.rs"]
mod rigid_span;
#[path = "native_module_template_dependencies.rs"]
mod template_dependencies;

fn private_cases() -> Vec<PrivateCase> {
    let outputs = PRIVATE_OUTPUTS;
    let mut cases = vec![
        prepared_page_private_case(),
        private_wide_interface_case(false),
        PrivateCase {
            name: "private-real-text-names",
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use opforge.assembler.language.text_names as names\n.section code,kind=code\n.long names.normalize\n.endsection\n.region ram,0,$ffff\n.place code in ram\n.output \"build/module.bin\",format=bin,sections=code\n.output \"build/module.hunk\",format=hunk,sections=code\n.endmodule\n".into()),
                ("library/text_names.asm", fs::read_to_string(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../native/motorola68000/amigaos/assembler/language/text_names.asm")).unwrap()),
            ],
            accepted_modules: 1,
        },
        PrivateCase {
            name: "private-package-state-reference-fallback",
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use consumer\n.section code,kind=code\n.long consumer.entry\n.endsection\n.region ram,0,$ffff\n.place code in ram\n.output \"build/module.bin\",format=bin,sections=code\n.output \"build/module.hunk\",format=hunk,sections=code\n.endmodule\n".into()),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.fpu 68881\n.section code,kind=code\n rts\n.endsection\n.endmodule\n".into()),
                ("library/consumer.asm", ".module consumer\n.use provider\n.pub\n.section code,kind=code\nentry .block\n fmove fp0,fp1\n rts\n.bend\n.endsection\n.endmodule\n".into()),
            ],
            accepted_modules: 0,
        },
        PrivateCase {
            name: "private-text-state-reference-fallback",
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use consumer\n.section code,kind=code\n.long consumer.entry\n.endsection\n.region ram,0,$ffff\n.place code in ram\n.output \"build/module.bin\",format=bin,sections=code\n.output \"build/module.hunk\",format=hunk,sections=code\n.endmodule\n".into()),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.encode shared\n.cdef \"A\",\"A\",9\n.endencode\n.section code,kind=code\n rts\n.endsection\n.endmodule\n".into()),
                ("library/consumer.asm", ".module consumer\n.cpu 68020\n.use provider\n.pub\n.section code,kind=code\nentry .block\n.enc shared\n.byte \"A\"\n.bend\n.endsection\n.endmodule\n".into()),
            ],
            accepted_modules: 0,
        },
        PrivateCase {
            name: "three-section-output-selection",
            files: vec![(
                "entry.asm",
                ".module main\n.cpu 68020\n.section alpha,kind=code\n.byte 1\n.endsection\n.section beta,kind=data\n.byte 2\n.endsection\n.section gamma,kind=data\n.byte 3\n.endsection\n.region ram,0,$ffff\n.place alpha in ram\n.place beta in ram\n.place gamma in ram\n.output \"build/module.bin\",format=bin,sections=alpha,beta,gamma\n.output \"build/module.hunk\",format=hunk,sections=alpha,beta,gamma\n.endmodule\n".into(),
            )],
            accepted_modules: 0,
        },
        PrivateCase {
            name: "private-scalar-code-block",
            files: vec![
                ("entry.asm", format!(".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.long provider.entry\n.endsection\n.section data,kind=data\n.byte provider.VALUE,provider.ITEMS[1],.len(provider.ITEMS),provider.Info.X,provider.SEED.X\n.endsection\n{outputs}")),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.pub\nVALUE .const 5\nITEMS .const {3,9}\nInfo .struct\nPadding .long ?\nX .long ?\n.endstruct\nSEED .const Info{Padding:0,X:7}\n.section code,kind=code\nentry .block\n.word $1111\n rts\n.bend\n.endsection\n.endmodule\n".into()),
            ],
            accepted_modules: 1,
        },
        PrivateCase {
            name: "private-schema-code-data-bss",
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use provider\nALIAS = provider.entry\n.section code,kind=code\n.long ALIAS\n.endsection\n.section data,kind=data\n.long provider.payload,provider.scratch\n.byte provider.Frame.Value,provider.COUNT,provider.Seed.Value\n.endsection\n.section bss,kind=bss\n.endsection\n.region ram,0,$ffff\n.place code in ram\n.place data in ram\n.place bss in ram\n.output \"build/module.bin\",format=bin,sections=code,data\n.output \"build/module.hunk\",format=hunk,sections=code,data,bss\n.endmodule\n".into()),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.pub\nFrame .struct\nPadding .long ?\nValue .long ?\n.endstruct\nCOUNT .const 5\nSeed .const Frame{Padding:0,Value:7}\n.section code,kind=code\nentry .block\n.byte $11\ninside .block\n.byte $22\n.bend\n.byte $33,$44\n rts\n.bend\n.priv\nunused .block\n.long $deadbeef\n.bend\n.endsection\n.pub\n.section data,kind=data\npayload .block\n.byte $45,$67\n.bend\n.endsection\n.section bss,kind=bss\nscratch .block\n.res byte,7\n.bend\n.endsection\n.endmodule\n".into()),
            ],
            accepted_modules: 1,
        },
        PrivateCase {
            name: "private-cross-module-absolute-addresses",
            files: vec![
                ("entry.asm", format!(".module main\n.cpu 68020\n.use middle\n.section code,kind=code\n.long middle.entry\n.endsection\n.section data,kind=data\n.byte middle.VALUE\n.endsection\n{outputs}")),
                ("library/middle.asm", ".module middle\n.cpu 68020\n.use leaf\n.pub\nVALUE .const leaf.FLAG+1\n.section code,kind=code\nentry .block\n lea leaf.payload.l,a0\n.long leaf.payload\n.word leaf.FLAG\n rts\n.bend\n.endsection\n.endmodule\n".into()),
                ("library/leaf.asm", ".module leaf\n.cpu 68020\n.pub\nFLAG .const 6\n.section data,kind=data\npayload .block\n.long $12345678\n.bend\n.endsection\n.endmodule\n".into()),
            ],
            accepted_modules: 2,
        },
        PrivateCase {
            name: "private-placement-reference-fallback",
            files: vec![
                ("entry.asm", ".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.long provider.entry\n.endsection\n.section data,kind=data\n.byte $55\n.endsection\n.output \"build/module.bin\",format=bin,sections=code,data\n.output \"build/module.hunk\",format=hunk,sections=code,data\n.endmodule\n".into()),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.pub\n.region ram,0,$ffff\n.section code,kind=code\nentry .block\n.byte $a5,$5a\n.bend\n.endsection\n.section data,kind=data\n.byte $33\n.endsection\n.place code in ram\n.place data in ram\n.endmodule\n".into()),
            ],
            accepted_modules: 0,
        },
        PrivateCase {
            name: "private-warning-reference-fallback-once",
            files: vec![
                ("entry.asm", format!(".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.long provider.entry\n.endsection\n.section data,kind=data\n.byte $55\n.endsection\n{outputs}")),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.pub\n.section code,kind=code\nentry .block\n.byte 256,$6b\n.bend\n.endsection\n.endmodule\n".into()),
            ],
            accepted_modules: 0,
        },
        PrivateCase {
            name: "private-active-include-reference-fallback-once",
            files: vec![
                ("entry.asm", format!(".module main\n.cpu 68020\n.use provider\n.section code,kind=code\n.long provider.entry\n.endsection\n.section data,kind=data\n.byte $55\n.endsection\n{outputs}")),
                ("library/provider.asm", ".module provider\n.cpu 68020\n.priv\nLOAD .macro file\n.include .file\n.endmacro\n.pub\n.section code,kind=code\nentry .block\n.LOAD \"parts/payload.inc\"\n.byte $6b\n.bend\n.endsection\n.endmodule\n".into()),
                ("library/parts/payload.inc", ".byte $7a\n".into()),
                // Invocation-relative resolution must not use the root copy.
                ("parts/payload.inc", ".byte $99\n".into()),
            ],
            accepted_modules: 0,
        },
    ];
    cases.extend(template_dependencies::cases());
    cases.extend(publication_cases::cases());
    cases.extend(cancellation::cases());
    cases.extend(binding_fallback::cases());
    cases.extend(move_geometry::cases());
    cases.extend(rigid_span::cases());
    cases.extend(counted_loops::cases());
    cases.extend(data_deltas::cases());
    cases
}

fn private_wide_interface_case(aliased_projection: bool) -> PrivateCase {
    let outputs = PRIVATE_OUTPUTS;
    let mut provider = String::from(".module provider\n.cpu 68020\n.pub\n");
    for value in 0..139 {
        provider.push_str(&format!("V{value} .const {value}\n"));
    }
    provider.push_str(
        "Pair .struct\nX .word ?\nY .word ?\n.endstruct\nSeed .const Pair{X:3,Y:7}\nItems .const {2,4,6}\n.section code,kind=code\nentry .block\n rts\n.bend\n.endsection\n.endmodule\n",
    );

    let mut values = String::new();
    for chunk in (0..139).collect::<Vec<_>>().chunks(8) {
        if !values.is_empty() {
            values.push('\n');
        }
        values.push_str(".word ");
        values.push_str(
            &chunk
                .iter()
                .map(|value| format!("provider.V{value}"))
                .collect::<Vec<_>>()
                .join(","),
        );
    }

    PrivateCase {
        name: if aliased_projection {
            "private-wide-aliased-schema-projection"
        } else {
            "private-wide-interface-import"
        },
        files: vec![
            (
                "entry.asm",
                format!(
                    ".module main\n.cpu 68020\n.use middle\n.use provider\n.section code,kind=code\n.long middle.helper\n.endsection\n.section data,kind=data\n.word middle.RESULT\n{values}\n.endsection\n{outputs}"
                ),
            ),
            (
                "library/middle.asm",
                format!(".module middle\n.cpu 68020\n.use provider as p\n.pub\nRESULT .const {}\n.section code,kind=code\nhelper .block\n.word RESULT\n.long p.entry\n rts\n.bend\n.endsection\n.endmodule\n", if aliased_projection {
                    "p.V138+p.Pair.Y+p.Seed.Y+p.Items[2]"
                } else {
                    "p.V138+p.V15"
                }),
            ),
            ("library/provider.asm", provider),
        ],
        accepted_modules: 2,
    }
}

// Each include forces ordinary replay exactly once. Eligible suppliers between
// those inclusions make admission read the appended prepared suffix repeatedly,
// crossing the four-page payload cache before another ordinary append.
fn prepared_page_private_case() -> PrivateCase {
    const FILLERS: [&str; 5] = [
        "library/filler0.asm",
        "library/filler1.asm",
        "library/filler2.asm",
        "library/filler3.asm",
        "library/filler4.asm",
    ];
    const SUPPLIERS: [&str; 5] = [
        "library/private0.asm",
        "library/private1.asm",
        "library/private2.asm",
        "library/private3.asm",
        "library/private4.asm",
    ];
    const PAYLOADS: [&str; 5] = [
        "library/parts/payload0.inc",
        "library/parts/payload1.inc",
        "library/parts/payload2.inc",
        "library/parts/payload3.inc",
        "library/parts/payload4.inc",
    ];
    let mut files = vec![(
        "entry.asm",
        ".module main\n.cpu 68020\n.use private4\n.section code,kind=code\n.byte private4.VALUE\n.endsection\n.region ram,0,$3ffff\n.place code in ram\n.output \"build/module.bin\",format=bin,sections=code\n.output \"build/module.hunk\",format=hunk,sections=code\n.endmodule\n".into(),
    )];
    for index in 0..5 {
        let dependency = if index == 0 {
            String::new()
        } else {
            format!(".use private{}\n", index - 1)
        };
        files.push((FILLERS[index], format!(
            ".module filler{index}\n.cpu 68020\n{dependency}.priv\nLOAD .macro file\n.include .file\n.endmacro\n.section code,kind=code\n.LOAD \"parts/payload{index}.inc\"\n.endsection\n.endmodule\n"
        )));
        files.push((SUPPLIERS[index], format!(
            ".module private{index}\n.cpu 68020\n.use filler{index}\n.pub\nVALUE .const {index}\n.section code,kind=code\n.long {index}\n.endsection\n.endmodule\n"
        )));
        let values = vec![(0x40 + index).to_string(); 16].join(",");
        files.push((PAYLOADS[index], format!(".byte {values}\n").repeat(2048)));
    }
    PrivateCase {
        name: "private-prepared-page-read-append-eviction",
        files,
        accepted_modules: 5,
    }
}

// Validate actual serialized block ranges before classifying initialized payload.
// Interface-only objects may carry semantic bytes; file length is not code size.
fn object_has_payload(bytes: &[u8]) -> bool {
    assert!(bytes.len() >= 36, "complete module header");
    let word =
        |offset: usize| u32::from_be_bytes(bytes[offset..offset + 4].try_into().unwrap()) as usize;
    let count = word(8);
    let blocks = word(12);
    let table_end = blocks.checked_add(count.checked_mul(32).unwrap()).unwrap();
    assert!(
        blocks >= 36 && table_end <= bytes.len(),
        "complete block directory"
    );
    let mut initialized = false;
    for index in 0..count {
        let row = blocks + index * 32;
        let offset = word(row + 4);
        let length = word(row + 8);
        assert!(
            offset >= word(24) && offset.checked_add(length).unwrap() <= bytes.len(),
            "initialized block must refer to actual object bytes"
        );
        initialized |= length != 0;
    }
    initialized
}

fn private_oracle(case: &PrivateCase) -> (Vec<u8>, Vec<u8>) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};

    let dir = scratch();
    fs::create_dir_all(dir.0.join("build")).unwrap();
    fs::create_dir_all(dir.0.join("library")).unwrap();
    for (relative, contents) in &case.files {
        let path = dir.0.join(relative);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }
    let cli = Cli::parse_from([
        "opforge".into(),
        dir.0.join("entry.asm").to_string_lossy().into_owned(),
        "-M".into(),
        dir.0.join("library").to_string_lossy().into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(dir.0.clone());
    run_with_validated_cli_with_context(&cli, &config)
        .unwrap_or_else(|error| panic!("{}: {error:?}", case.name));
    let binary = fs::read(dir.0.join("build/module.bin")).unwrap();
    let hunk = fs::read(dir.0.join("build/module.hunk")).unwrap();
    let segments = hunk::segments(&hunk).unwrap();
    match case.name {
        "private-prepared-page-read-append-eviction" => {
            let mut expected = Vec::new();
            for index in 0u32..5 {
                expected.extend(std::iter::repeat_n(0x40 + index as u8, 64 * 512));
                expected.extend(index.to_be_bytes());
            }
            expected.push(4);
            assert_eq!(
                binary, expected,
                "include payload must execute exactly once"
            );
            assert!(binary.len() > 4 * 32768);
            assert_eq!(segments.len(), 1);
        }
        "private-real-text-names" => {
            assert!(binary.len() > 4, "root pointer and reachable module code");
            assert_eq!(segments.len(), 1);
            assert_eq!(segments[0].relocations.len(), 1);
        }
        "private-real-binding-layout" => {
            assert!(binary.windows(4).any(|bytes| bytes == [0, 4, 0, 2]));
            assert!(binary.ends_with(&[0, 4]));
            assert_eq!(
                segments
                    .iter()
                    .map(|segment| segment.relocations.len())
                    .sum::<usize>(),
                1
            );
        }
        name if name.starts_with("private-pc-cancel-") => cancellation::check(case.name, &binary),
        name if name.starts_with("private-binding-") => binding_fallback::check(&binary),
        name if name.starts_with("private-rigid-span-") => rigid_span::check(case.name, &binary),
        name if name.starts_with("private-data-delta-") => data_deltas::check(case.name, &binary),
        name if name.starts_with("private-counted-loop-") => {
            counted_loops::check(case.name, &binary)
        }
        name if name.starts_with("private-move-") => {
            move_geometry::check(case.name, &binary);
            assert_eq!(
                segments.iter().map(|s| s.relocations.len()).sum::<usize>(),
                move_geometry::relocation_count(case.name),
                "each independent address field retains its Hunk relocation"
            );
        }
        name if name.starts_with("private-publication-reduced-") => {
            publication_cases::check(case.name, &binary)
        }
        "private-publication-values-only"
        | "private-publication-dependency-order"
        | "private-publication-repeated-dependency-order"
        | "private-publication-templates-only"
        | "private-publication-odd-statement-keywords"
        | "private-publication-unowned-origin-fallback"
        | "private-publication-unowned-text-fallback" => {
            publication_cases::check(case.name, &binary)
        }
        "private-real-memory" => {
            assert!(binary.len() > 8, "root pointers and reachable module code");
            assert_eq!(segments.len(), 2);
            assert!(segments[0].relocations.len() >= 2);
            assert_eq!(&segments[1].payload[..2], &[0, 8]);
        }
        "private-lexical-macro-scopes" => {
            assert_eq!(binary, [7, 0x4e, 0x75, 9, 0, 0, 0, 0, 0, 0, 0, 3, 0, 11]);
            assert_eq!(segments.len(), 2);
            assert_eq!(segments[0].relocations.len(), 2);
        }
        "private-package-state-reference-fallback" => {
            assert!(binary.len() >= 10);
            assert!(binary.windows(2).any(|bytes| bytes == [0x4e, 0x75]));
        }
        "private-text-state-reference-fallback" => {
            assert_eq!(binary.iter().filter(|byte| **byte == 9).count(), 1);
            assert!(!binary.contains(&65));
        }
        "three-section-output-selection" => {
            assert_eq!(binary, [1, 2, 3]);
            assert_eq!(segments.len(), 3);
        }
        "private-scalar-code-block" => {
            assert!(binary.windows(2).any(|bytes| bytes == [0x11, 0x11]));
            assert!(binary.windows(5).any(|bytes| bytes == [5, 9, 2, 4, 7]));
            assert!(segments
                .iter()
                .any(|segment| !segment.relocations.is_empty()));
        }
        "private-schema-code-data-bss" => {
            assert!(binary.windows(3).any(|bytes| bytes == [0x11, 0x22, 0x33]));
            assert!(!binary
                .windows(4)
                .any(|bytes| bytes == [0xde, 0xad, 0xbe, 0xef]));
            assert!(binary.windows(3).any(|bytes| bytes == [4, 5, 7]));
            assert!(hunk::allocation(&hunk).unwrap().bss >= 7);
            assert!(
                segments
                    .iter()
                    .map(|segment| segment.relocations.len())
                    .sum::<usize>()
                    >= 3
            );
        }
        "private-cross-module-absolute-addresses" => {
            assert!(binary
                .windows(4)
                .any(|bytes| bytes == [0x12, 0x34, 0x56, 0x78]));
            assert!(
                segments
                    .iter()
                    .map(|segment| segment.relocations.len())
                    .sum::<usize>()
                    >= 3
            );
        }
        "private-placement-reference-fallback" => {
            assert!(binary.windows(2).any(|bytes| bytes == [0xa5, 0x5a]));
        }
        "private-active-include-reference-fallback-once" => {
            assert_eq!(binary.iter().filter(|byte| **byte == 0x7a).count(), 1);
            assert_eq!(binary.iter().filter(|byte| **byte == 0x6b).count(), 1);
            assert!(!binary.contains(&0x99));
        }
        "private-warning-reference-fallback-once" => {
            assert!(binary.windows(2).any(|bytes| bytes == [0, 0x6b]));
        }
        "private-wide-interface-import" | "private-wide-aliased-schema-projection" => {
            let mut expected_data = 153u16.to_be_bytes().to_vec();
            for value in 0u16..139 {
                expected_data.extend(value.to_be_bytes());
            }
            assert!(
                binary.ends_with(&expected_data),
                "wide public interface data must include RESULT and every exported scalar"
            );
            assert!(
                segments
                    .iter()
                    .map(|segment| segment.relocations.len())
                    .sum::<usize>()
                    >= 2
            );
        }
        "private-template-dependency-values"
        | "private-template-dependency-macro"
        | "private-template-dependency-statement"
        | "private-template-dependency-directive-shadow"
        | "private-template-dependency-opcode-shadow"
        | "private-template-dependency-local-shadow"
        | "private-template-dependency-local-statement"
        | "private-template-dependency-wildcard-parent-gap"
        | "private-template-dependency-core-root-gap" => {
            template_dependencies::check(case.name, &binary);
            assert!(segments.iter().map(|s| s.relocations.len()).sum::<usize>() >= 2);
        }
        _ => unreachable!(),
    }
    (binary, hunk)
}

#[test]
fn module_private_pipeline_live_rust_oracles() {
    for case in private_cases() {
        private_oracle(&case);
    }
    private_oracle(&template_dependencies::wildcard_parent_case());
    private_oracle(&template_dependencies::core_root_case());
    // Keep the broader shared-binding failure explicit rather than dropping it
    // from coverage when qualifying the separate wide-interface storage case.
    private_oracle(&private_wide_interface_case(true));
    let files = reset_state_files();
    let dir = scratch();
    fs::create_dir_all(dir.0.join("build")).unwrap();
    for (relative, contents) in &files {
        let path = dir.0.join(relative);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(path, contents).unwrap();
    }
    let error = oracle_with_module_roots(&dir.0, &files[0].1, &["library"]).unwrap_err();
    assert!(
        error.contains("FNOP requires an active .fpu target"),
        "expected reset-disabled FPU: {error}"
    );
}

fn reset_state_files() -> Vec<(&'static str, String)> {
    vec![
        ("entry.asm", ".module main\n.cpu 68020\n.use consumer\n.section code,kind=code\n.word $1234\n.endsection\n.output \"build/module.hunk\",format=hunk,sections=code\n.endmodule\n".into()),
        ("library/prior.asm", ".module prior\n.cpu 68020\n.fpu 68881\n.section code,kind=code\n rts\n.endsection\n.endmodule\n".into()),
        ("library/resetter.asm", ".module resetter\n.cpu 68020\n.use prior\n.pub\n.section code,kind=code\nentry .block\n.word $1111\n.bend\n.endsection\n.endmodule\n".into()),
        ("library/consumer.asm", ".module consumer\n.use resetter\n.section code,kind=code\n.long resetter.entry\n fnop\n.endsection\n.endmodule\n".into()),
    ]
}

#[test]
#[ignore = "requires instrumented compact CLI; compiled module retains CPU reset"]
fn native_module_private_pipeline_reset_state() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let image =
        fs::read(std::env::var_os("OPFORGE_MODULE_PIPELINE_PROGRESS_IMAGE").unwrap()).unwrap();
    let source = reset_state_files();
    let files: Vec<_> = source
        .iter()
        .map(|(path, contents)| OpforgeNativeCliGuestFile {
            relative_path: path,
            bytes: contents.as_bytes(),
        })
        .chain([OpforgeNativeCliGuestFile {
            relative_path: "build/.keep",
            bytes: b"",
        }])
        .collect();
    for reference in [false, true] {
        let parity = OpforgeNativeCliParityCase {
            name: "private-outgoing-cpu-reset",
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(source[0].1.as_bytes()),
            command_template: Some(if reference {
                "Work:entry.asm -M Work:library --reference-modules"
            } else {
                "Work:entry.asm -M Work:library"
            }),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files,
            proof: OpforgeNativeCliProof::ExpectedFailureWithDiagnostic,
        };
        let outcome = run_prebuilt_compact_cli_case_from_env(&root, &parity, &image)
            .expect("fresh retained CPU reset failure proof");
        let FsUaeSmokeOutcome::Completed { runs } = outcome else {
            panic!("real native completion required")
        };
        assert_eq!(runs.len(), 1);
        let run = &runs[0];
        assert!(run.protocol_completed);
        assert_eq!(run.exit_code, Some(20));
        assert!(run.stdout.contains("source: Work:library/consumer.asm"));
        assert_eq!(private_counts(&run.stdout), [u32::from(!reference)]);
    }
}

#[test]
#[ignore = "requires prebuilt current compact CLI and fresh FS-UAE completion"]
fn native_module_private_pipeline_exact_artifacts() {
    private_pipeline_native(false, false);
}

#[test]
#[ignore = "requires instrumented current compact CLI and fresh FS-UAE completion"]
fn native_module_private_pipeline_progress() {
    private_pipeline_native(true, false);
}

#[test]
#[ignore = "requires current compact CLI; disk-backed proto modules and reference"]
fn native_module_private_disk_pipeline_artifacts() {
    private_pipeline_native(false, true);
}

#[test]
#[ignore = "requires instrumented compact CLI; prove disk proto module admission"]
fn native_module_private_disk_pipeline_progress() {
    private_pipeline_native(true, true);
}

#[test]
#[ignore = "requires prebuilt compact CLI; module warning fallback preserves --Werror"]
fn native_module_private_pipeline_warning_error() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let image = fs::read(std::env::var_os("OPFORGE_MODULE_PIPELINE_IMAGE").unwrap()).unwrap();
    let case = private_cases()
        .into_iter()
        .find(|case| case.name == "private-warning-reference-fallback-once")
        .unwrap();
    let (binary, hunk) = private_oracle(&case);
    let files: Vec<_> = case
        .files
        .iter()
        .map(|(path, contents)| OpforgeNativeCliGuestFile {
            relative_path: path,
            bytes: contents.as_bytes(),
        })
        .chain([OpforgeNativeCliGuestFile {
            relative_path: "build/.keep",
            bytes: b"",
        }])
        .collect();
    for reference in [false, true] {
        let parity = OpforgeNativeCliParityCase {
            name: case.name,
            cpu_override: "68020",
            extra_assembly_defines: &[],
            source_override: Some(case.files[0].1.as_bytes()),
            command_template: Some(if reference {
                "Work:entry.asm -M Work:library --Werror --reference-modules"
            } else {
                "Work:entry.asm -M Work:library --Werror"
            }),
            package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
            extra_guest_files: &files,
            proof: OpforgeNativeCliProof::ExpectedFailureContaining(
                "--Werror: warnings treated as errors",
            ),
        };
        let outcome = run_prebuilt_compact_cli_case_from_env(&root, &parity, &image)
            .expect("fresh module --Werror completion");
        let FsUaeSmokeOutcome::Completed { runs } = outcome else {
            panic!("real native completion required")
        };
        assert_eq!(runs.len(), 1);
        let run = &runs[0];
        assert!(run.protocol_completed);
        assert_eq!(run.exit_code, Some(20));
        assert_private_warnings(&case, &run.stdout);
        for (path, expected) in [
            ("Work/build/module.bin", &binary),
            ("Work/build/module.hunk", &hunk),
        ] {
            assert_eq!(run.captured_artifacts[&PathBuf::from(path)], *expected);
        }
    }
}

fn private_pipeline_native(progress: bool, disk: bool) {
    private_pipeline_native_cases(progress, disk, private_cases());
}

#[test]
#[ignore = "known reference binding gap: aliased p.Pair.Y fails in frontend.complete before emission; requires FS-UAE"]
fn native_module_private_aliased_schema_projection_gap() {
    private_pipeline_native_cases(true, true, vec![private_wide_interface_case(true)]);
}

fn private_pipeline_native_cases(progress: bool, disk: bool, mut cases: Vec<PrivateCase>) {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let variable = if progress {
        "OPFORGE_MODULE_PIPELINE_PROGRESS_IMAGE"
    } else {
        "OPFORGE_MODULE_PIPELINE_IMAGE"
    };
    let image =
        fs::read(std::env::var_os(variable).unwrap_or_else(|| panic!("set {variable}"))).unwrap();
    if let Ok(filter) = std::env::var("OPFORGE_MODULE_PRIVATE_CASE") {
        cases.retain(|case| case.name == filter);
        assert!(!cases.is_empty(), "unknown private module case {filter}");
    }
    let oracles: Vec<_> = cases.iter().map(private_oracle).collect();
    let artifacts: Vec<_> = oracles
        .iter()
        .map(|(binary, hunk)| {
            [
                OpforgeNativeCliExpectedArtifact {
                    relative_path: "Work/build/module.bin",
                    rust_oracle: binary,
                },
                OpforgeNativeCliExpectedArtifact {
                    relative_path: "Work/build/module.hunk",
                    rust_oracle: hunk,
                },
            ]
        })
        .collect();
    let files: Vec<Vec<_>> = cases
        .iter()
        .map(|case| {
            case.files
                .iter()
                .map(|(path, contents)| OpforgeNativeCliGuestFile {
                    relative_path: path,
                    bytes: contents.as_bytes(),
                })
                .chain([
                    OpforgeNativeCliGuestFile {
                        relative_path: "build/.keep",
                        bytes: b"",
                    },
                    OpforgeNativeCliGuestFile {
                        relative_path: "library/.keep",
                        bytes: b"",
                    },
                ])
                .collect()
        })
        .collect();
    let mut failures = Vec::new();
    for reference in [false, true] {
        let command = format!(
            "Work:entry.asm -M Work:library{}{}",
            if disk { " --capture-dir Work:" } else { "" },
            if reference {
                " --reference-modules"
            } else {
                ""
            }
        );
        let parity: Vec<_> = cases
            .iter()
            .zip(&artifacts)
            .zip(&files)
            .map(|((case, artifacts), files)| OpforgeNativeCliParityCase {
                name: case.name,
                cpu_override: "68020",
                extra_assembly_defines: &[],
                source_override: Some(case.files[0].1.as_bytes()),
                command_template: Some(&command),
                package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
                extra_guest_files: files,
                proof: OpforgeNativeCliProof::ExactArtifacts(artifacts),
            })
            .collect();
        if progress || disk {
            for (case, parity) in cases.iter().zip(&parity) {
                let outcome = match run_prebuilt_compact_cli_case_from_env(&root, parity, &image) {
                    Ok(outcome) => outcome,
                    Err(error) => {
                        failures.push(format!("{} reference={reference}: {error}", case.name));
                        continue;
                    }
                };
                let FsUaeSmokeOutcome::Completed { runs } = outcome else {
                    panic!("real native completion required")
                };
                assert_eq!(runs.len(), 1);
                let run = &runs[0];
                assert!(
                    run.protocol_completed && run.success && run.exit_code == Some(0),
                    "{} reference={reference}: {} {}",
                    case.name,
                    run.stdout,
                    run.stderr
                );
                let counts = private_counts(&run.stdout);
                let expected = [if reference { 0 } else { case.accepted_modules }];
                if progress && counts != expected {
                    failures.push(format!(
                        "{} reference={reference}: publication count {counts:?}, expected {expected:?}; stdout: {}",
                        case.name, run.stdout
                    ));
                    continue;
                }
                if disk && progress {
                    let io = store_io(&run.stdout).expect("module-store I/O receipt");
                    assert_eq!(
                        io[1], expected[0],
                        "one persisted image per accepted module"
                    );
                    assert!(
                        io[0] <= io[1],
                        "layout must not read objects; final emission reads each at most once"
                    );
                    eprintln!(
                        "MODULE_STORE_IO reference={reference} reads={} writes={} cache_hits={}",
                        io[0], io[1], io[2]
                    );
                }
                if disk {
                    let objects = run
                        .captured_artifacts
                        .iter()
                        .filter(|(path, _)| path.extension().is_some_and(|ext| ext == "ofm"))
                        .collect::<Vec<_>>();
                    assert_eq!(
                        objects.len(),
                        expected[0] as usize,
                        "completed .ofm files survive cleanup"
                    );
                    for (path, bytes) in &objects {
                        assert_eq!(bytes.get(..4), Some(b"OFMI".as_slice()), "{path:?}");
                        let extent = u32::from_be_bytes(bytes[4..8].try_into().unwrap());
                        assert_eq!(extent as usize, bytes.len(), "exact .ofm extent: {path:?}");
                    }
                    if let Some((semantic, payload)) = publication_cases::object_counts(case.name)
                        .or_else(|| real_sources::object_counts(case.name))
                        .or_else(|| cancellation::object_counts(case.name))
                        .or_else(|| move_geometry::object_counts(case.name))
                        .or_else(|| rigid_span::object_counts(case.name))
                        .or_else(|| counted_loops::object_counts(case.name))
                        .or_else(|| data_deltas::object_counts(case.name))
                    {
                        let expected_objects = if reference {
                            (0, 0)
                        } else {
                            (semantic, payload)
                        };
                        let actual_payload = objects
                            .iter()
                            .filter(|(_, bytes)| object_has_payload(bytes))
                            .count();
                        assert_eq!(
                            (objects.len() - actual_payload, actual_payload),
                            expected_objects,
                            "semantic objects must not disguise missing executable payload"
                        );
                        if progress {
                            let io = store_io(&run.stdout).expect("module-store I/O receipt");
                            assert_eq!(io[0] as usize, expected_objects.1,
                                "selected initialized payload reads once; semantic-only objects read zero times");
                            let memory = run
                                .captured_artifacts
                                .get(&PathBuf::from("Work/memory.bin"))
                                .expect("instrumented ownership receipt");
                            let memory = super::native_package_loading_performance::memory(memory);
                            assert_eq!(
                                memory["live_owned_bytes"].as_u64(),
                                Some(0),
                                "all transient owners must retire after publication/output"
                            );
                        }
                    }
                    if !reference {
                        if let Some(destination) = std::env::var_os("OPFORGE_MODULE_OBJECT_EXPORT")
                        {
                            let destination = PathBuf::from(destination);
                            assert!(destination.is_absolute());
                            fs::create_dir_all(&destination).unwrap();
                            let destination = destination.join(case.name);
                            fs::create_dir(&destination).expect("new object export directory");
                            for (path, bytes) in &objects {
                                fs::write(destination.join(path.file_name().unwrap()), bytes)
                                    .unwrap();
                            }
                            eprintln!("MODULE_OBJECT_EXPORT {}", destination.display());
                        }
                    }
                    assert!(
                        run.captured_artifacts
                            .keys()
                            .all(|path| path.extension().is_none_or(|ext| ext != "tmp")),
                        "unfinished object file survived cleanup"
                    );
                }
                assert_private_warnings(case, &run.stdout);
                if !progress {
                    assert!(
                        !run.captured_artifacts
                            .contains_key(&PathBuf::from("Work/memory.bin")),
                        "release timing requires telemetry to be disabled"
                    );
                }
                eprintln!(
                    "PRIVATE_MODULE_CASE name={} reference={reference} disk={disk} instrumented={progress} accepted={:?} exact=bin+hunk seconds={:?} memory={}",
                    case.name, counts.first(), run.start_to_done_host_seconds,
                    run.captured_artifacts.get(&PathBuf::from("Work/memory.bin"))
                        .map(|bytes| super::native_package_loading_performance::memory(bytes))
                        .unwrap_or(serde_json::Value::Null)
                );
                assert!(
                    run.captured_artifacts
                        .keys()
                        .all(|path| !path.components().any(|part| part
                            .as_os_str()
                            .to_str()
                            .is_some_and(|name| name.starts_with("opfg")))),
                    "owned disk storage leaked: {:?}",
                    run.captured_artifacts.keys()
                );
            }
        } else {
            let results = run_prebuilt_compact_cli_breadth_from_env(&root, &parity, &image, 3)
                .expect("fresh private pipeline artifact batch");
            assert_eq!(results.len(), cases.len());
            for (case, result) in cases.iter().zip(&results) {
                let Some(run) = &result.run else {
                    failures.push(format!(
                        "{} reference={reference}: guest missing: {:?}",
                        case.name, result.error
                    ));
                    continue;
                };
                if !run.protocol_completed
                    || !run.success
                    || run.exit_code != Some(0)
                    || result.error.is_some()
                {
                    failures.push(format!(
                        "{} reference={reference}: {:?} {} {}",
                        case.name, result.error, run.stdout, run.stderr
                    ));
                    continue;
                }
                eprintln!(
                    "PRIVATE_MODULE_CASE name={} reference={reference} exact=bin+hunk",
                    case.name
                );
                assert_private_warnings(case, &run.stdout);
            }
        }
    }
    assert!(
        failures.is_empty(),
        "private pipeline artifact failures: {failures:?}"
    );
}

fn assert_private_warnings(case: &PrivateCase, stdout: &str) {
    if case.name == "private-warning-reference-fallback-once" {
        assert_eq!(
            stdout.matches("warning: Value truncated to byte").count(),
            1,
            "{}: warning must survive reference fallback exactly once: {stdout}",
            case.name
        );
    }
}

fn private_counts(stdout: &str) -> Vec<u32> {
    stdout
        .lines()
        .filter_map(|line| {
            let fields: std::collections::BTreeMap<_, _> = line
                .strip_prefix("progress ")?
                .split_whitespace()
                .filter_map(|field| field.split_once('='))
                .collect();
            (u32::from_str_radix(fields.get("p")?, 16).ok()? == 300)
                .then(|| u32::from_str_radix(fields["r"], 16).expect("accepted module count"))
        })
        .collect()
}

fn store_io(stdout: &str) -> Option<[u32; 3]> {
    stdout.lines().find_map(|line| {
        let fields = line
            .strip_prefix("progress ")?
            .split_whitespace()
            .filter_map(|field| field.split_once('='))
            .collect::<std::collections::BTreeMap<_, _>>();
        if u32::from_str_radix(fields.get("p")?, 16).ok()? != 451 {
            return None;
        }
        Some([
            u32::from_str_radix(fields.get("f")?, 16).ok()?,
            u32::from_str_radix(fields.get("l")?, 16).ok()?,
            u32::from_str_radix(fields.get("r")?, 16).ok()?,
        ])
    })
}
