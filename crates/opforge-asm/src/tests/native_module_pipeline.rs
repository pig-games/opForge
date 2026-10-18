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
