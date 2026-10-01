//! Shared DATA directives preserve imported section identity in both runtimes.
use super::*;

fn source(data: &str) -> String {
    format!(
        ".module data_reloc_probe\n.cpu m68020\n.use data_dep as dep\nAbsolute .const 3\n.section code,kind=code\n{data}\n.endsection\n.section bss,kind=bss\n.res byte,12\n.endsection\n.section data,kind=data\n.byte $77\n.endsection\n.output \"build/sections.hunk\",format=hunk,sections=code,bss,data\n.endmodule\n.module data_dep\n.cpu m68020\n.pub\n.section data,kind=data\npayload .byte $11,$22,$33,$44\n.endsection\n.endmodule\n"
    )
}

#[test]
fn shared_hunk_data_imported_long_relocations() {
    let oracle = hunk_sections::rust_hunk_source(&source(
        ".long dep.payload,data_dep.payload+3,$1234,Absolute",
    ));
    let sections = hunk::segments(&oracle).expect("valid imported DATA Hunk");
    assert_eq!(
        sections[0].payload,
        [0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0x12, 0x34, 0, 0, 0, 3]
    );
    assert_eq!(sections[0].relocations, [(0, 2), (4, 2)]);
    assert_eq!(sections[2].payload, [0x11, 0x22, 0x33, 0x44, 0x77, 0, 0, 0]);
}

#[test]
fn shared_hunk_data_unsupported_symbolic_values_reject() {
    for data in [
        ".byte dep.payload",
        ".word dep.payload",
        ".long dep.payload*2",
        ".long dep.payload+data_dep.payload",
    ] {
        let source = source(data);
        let assembler = run_passes(&source.lines().collect::<Vec<_>>());
        let output = &assembler.root_metadata.linker_outputs[0];
        let error = build_linker_output_payload(output, assembler.sections())
            .expect_err("unsupported symbolic DATA requires an explicit Hunk failure");
        assert!(
            error.message().contains("does not support this symbolic"),
            "{data}: {error:?}"
        );
    }
}
