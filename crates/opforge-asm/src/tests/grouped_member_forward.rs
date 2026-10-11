//! Grouping preserves value ownership while unknown member bases defer layout.
use super::*;

#[test]
fn grouped_member_forward_record_alias_resolves_unknown_base() {
    for cpu in ["6502", "68020"] {
        for base in ["(saved)", "((saved))"] {
            let source = format!(".cpu {cpu}\n.byte saved.value,{base}.value\nitems .bfor item in {{2,4}}\nvalue .const item+10\n.byte item\n.endfor\nsaved .const items[0]\n");
            let assembler = run_passes(&source.lines().collect::<Vec<_>>());
            assert_eq!(
                assembler.image().entries().unwrap(),
                vec![(0, 12), (1, 12), (2, 2), (3, 4)],
                "{cpu} {base}"
            );
        }
    }
}

#[test]
fn grouped_member_provisional_base_must_resolve_to_a_typed_owner() {
    for source in [
        ".cpu 6502\nscalar .const 5\n.byte (scalar).value\n",
        ".cpu 6502\n.byte (scalar).value\nscalar .const 5\n",
        ".cpu 6502\n.byte (missing).value\n",
    ] {
        let lines = source.lines().map(str::to_string).collect::<Vec<_>>();
        let mut assembler = Assembler::new();
        let mut errors = assembler.pass1(&lines).errors;
        if errors == 0 {
            let mut output = Vec::new();
            let mut listing = ListingWriter::new(&mut output, false);
            errors += assembler.pass2(&lines, &mut listing).unwrap().errors;
        }
        assert!(
            errors > 0,
            "accepted scalar or unresolved member base: {source}"
        );
    }
}
