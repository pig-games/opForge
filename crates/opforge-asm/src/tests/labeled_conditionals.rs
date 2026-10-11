//! Control labels publish at their parent activity, before guard evaluation.
use super::*;

#[test]
fn labeled_conditionals_publish_parent_active_controls_in_source_order() {
    for cpu in ["6502", "68020"] {
        for colon in [":", ""] {
            let source = format!(".cpu {cpu}\n.org 16\nopening{colon} .if opening == $\n.byte 1\nalternative{colon} .elseif 1\n.byte 99\nterminal{colon} .else\n.byte 98\nclosing{colon} .endif\nfalse_open{colon} .if 0\n.byte 97\nfalse_arm{colon} .elseif false_arm != $\n.byte 96\ntrue_else{colon} .else\n.byte 2\nfalse_end{colon} .endif\n.byte opening,alternative,terminal,closing,false_open,false_arm,true_else,false_end\n");
            let assembler = run_passes(&source.lines().collect::<Vec<_>>());
            let expected = vec![1, 2, 16, 17, 17, 17, 17, 17, 17, 18];
            assert_eq!(
                assembler.image().entries().unwrap(),
                expected
                    .into_iter()
                    .enumerate()
                    .map(|(i, b)| (16 + i as u32, b))
                    .collect::<Vec<_>>(),
                "{cpu} {colon:?}"
            );
        }
    }
}

#[test]
fn labeled_conditionals_suppress_controls_with_inactive_parent() {
    for colon in [":", ""] {
        let source = format!(".cpu 68020\nouter{colon} .if 0\ninner{colon} .if 1\ninner_arm{colon} .elseif 1\ninner_else{colon} .else\ninner_end{colon} .endif\nouter_end{colon} .endif\n.byte outer,outer_end\n");
        let assembler = run_passes(&source.lines().collect::<Vec<_>>());
        for name in ["inner", "inner_arm", "inner_else", "inner_end"] {
            assert_eq!(assembler.symbols.lookup(name), None, "{name} {colon:?}");
        }
        assert_eq!(assembler.image().entries().unwrap(), vec![(0, 0), (1, 0)]);
    }
}

#[test]
fn labeled_conditionals_use_scoped_iterator_guards() {
    let source = ".cpu 68020\nitems .bfor i in {0,1}\nopening .if i == 0\n.byte 3\nalternative .elseif i == 1\n.byte 4\nterminal .else\n.byte 99\nclosing .endif\n.endfor\n.byte items[0].opening,items[0].closing,items[1].opening,items[1].closing";
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    assert_eq!(
        assembler.image().entries().unwrap(),
        vec![(0, 3), (1, 4), (2, 0), (3, 1), (4, 1), (5, 2)]
    );
}

#[test]
fn labeled_conditionals_preserve_readonly_label_ownership() {
    for colon in ["", ":"] {
        let source = format!(".cpu 68020\ncontrol .const 7\ncontrol{colon} .if 1\n.endif\n");
        let lines = source.lines().map(str::to_string).collect::<Vec<_>>();
        let mut assembler = Assembler::new();
        assert!(assembler.pass1(&lines).errors > 0);
        assert_eq!(assembler.symbols.lookup("control"), Some(7));
        assert!(assembler
            .diagnostics
            .iter()
            .any(|d| d.message().contains("defined more than once")));
    }
}

pub(crate) fn mapped_source(cpu: &str, colon: &str) -> String {
    format!(".module main\n.cpu {cpu}\n.use provider map {{ logical -> code }}\n.region ram,0,$ffff\n.section data,kind=data\n.byte 9,provider.opening,provider.closing\n.endsection\n.section code,kind=code\n.byte 1\n.endsection\n.place data in ram\n.place code in ram\n.endmodule\n.module provider\n.cpu {cpu}\n.pub\n.section logical,kind=code,logical\nopening{colon} .if opening == $\n.byte 2\nalternative{colon} .elseif 1\n.byte 99\nterminal{colon} .else\n.byte 98\nclosing{colon} .endif\n.byte opening,alternative,terminal,closing\n.endsection\n.endmodule\n")
}

#[test]
fn labeled_conditionals_mapped_self_reference_preserves_section_pc() {
    for cpu in ["6502", "68020"] {
        for colon in ["", ":"] {
            let source = mapped_source(cpu, colon);
            let assembler = run_passes(&source.lines().collect::<Vec<_>>());
            let expected = vec![9, 4, 5, 1, 2, 4, 5, 5, 5];
            assert_eq!(
                assembler.image().entries().unwrap(),
                expected
                    .into_iter()
                    .enumerate()
                    .map(|(i, b)| (i as u32, b))
                    .collect::<Vec<_>>(),
                "{cpu} {colon:?}"
            );
        }
    }
}
