//! Production reference regressions for immutable absolute dependency resolution.
use super::*;

fn bytes(source: &str) -> Vec<u8> {
    let assembler = run_passes(&source.lines().collect::<Vec<_>>());
    assembler
        .image()
        .entries()
        .unwrap()
        .into_iter()
        .map(|(_, byte)| byte)
        .collect()
}

#[test]
fn constant_dependencies_mixed_definition_forms() {
    assert_eq!(
        bytes("first = second+1\nsecond .const third+1\nthird = 7\n.byte first,second,third"),
        [9, 8, 7]
    );
}

#[test]
fn constant_dependencies_refresh_instruction_width_and_labels() {
    assert_eq!(bytes(".cpu m6502\n.org $1000\naddress = base+offset\nbase = page<<8\npage = 2\noffset = 4\nstart:\n lda address\nfinish:\n.word finish,start,finish-start"),
        [0xad,4,2,3,0x10,0,0x10,3,0]);
}

#[test]
fn constant_dependencies_keep_block_bindings_separate() {
    assert_eq!(bytes("left .block\nfirst = later+1\nlater = seed+1\nseed = 2\n.byte first\n.endblock\nright .block\nfirst .const later+2\nlater = seed+1\nseed = 6\n.byte first\n.endblock"), [4,9]);
}

#[test]
fn constant_dependencies_resolve_import_alias_in_definition_scope() {
    assert_eq!(bytes(".module library\n.pub\nvalue = seed+1\nseed = root+1\nroot = 5\n.endmodule\n.module consumer\n.use library as lib\nresult = lib.value+1\n.byte result\n.endmodule"), [8]);
}

#[test]
fn constant_dependencies_ignore_inactive_definitions() {
    assert_eq!(
        bytes(".if 0\nfirst = absent+1\n.endif\nfirst = second+1\nsecond = 4\n.byte first"),
        [5]
    );
}

#[test]
fn constant_dependencies_preserve_mutable_snapshots_and_layout_values() {
    assert_eq!(bytes(".org $1000\ntrigger = next+1\nnext = last+1\nlast = 1\nvariable := 3\nsaved = variable+1\nvariable := 8\n.byte saved,variable\nposition = $\n.word position\nvals .const {10,20,30}\n.byte vals[2]"), [4,8,2,0x10,30]);
}

#[test]
fn constant_dependencies_reject_cycles() {
    for body in [
        "left = right+1\nright = left+1",
        "left = $+right\nright = left",
        "self = self+1",
    ] {
        let assembler = run_pass1(&body.lines().collect::<Vec<_>>());
        assert!(
            assembler
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.severity == Severity::Error),
            "cycle accepted: {body}"
        );
    }
}

#[test]
fn constant_dependencies_forward_scalar_loop_counts() {
    for cpu in ["m6502", "m68000", "m68020"] {
        assert_eq!(
            bytes(&format!(
                ".cpu {cpu}\n.for count\n.byte 7\n.endfor\ncount = 2"
            )),
            [7, 7]
        );
        assert_eq!(
            bytes(&format!(
                ".cpu {cpu}\n.bfor count\n.byte 9\n.endfor\ncount = base+1\nbase = 1"
            )),
            [9, 9]
        );
    }
}

#[test]
fn constant_dependencies_forward_count_nested_loops() {
    assert_eq!(
        bytes(".for outer\n.for inner\n.byte 3\n.endfor\n.endfor\nouter = 2\ninner = 2"),
        [3, 3, 3, 3]
    );
    assert_eq!(
        bytes(".for 2\n.for inner\n.byte 4\n.endfor\n.endfor\ninner = 2"),
        [4, 4, 4, 4]
    );
}

#[test]
fn constant_dependencies_forward_count_scope() {
    assert_eq!(bytes("left .block\n.for count\n.byte 1\n.endfor\ncount = 2\n.endblock\nright .block\n.for count\n.byte 2\n.endfor\ncount = 3\n.endblock"), [1,1,2,2,2]);
}

#[test]
fn constant_dependencies_forward_count_mutable_snapshot() {
    assert_eq!(bytes("variable := 2\nsaved = variable\nvariable := 4\n.for saved\n.byte 1\n.endfor\ntrigger = next+1\nnext = 1"), [1,1]);
}

#[test]
fn constant_dependencies_forward_count_inactive_definition_stays_missing() {
    let lines = [
        ".for count",
        ".byte 1",
        ".endfor",
        ".if 0",
        "count = 2",
        ".endif",
    ]
    .map(str::to_string);
    let mut assembler = Assembler::new();
    assert_eq!(assembler.pass1(&lines).errors, 0);
    let mut out = Vec::new();
    let mut listing = ListingWriter::new(&mut out, false);
    assert_eq!(assembler.pass2(&lines, &mut listing).unwrap().errors, 0);
    assert!(assembler.symbols.entry("count").is_none());
    assert!(assembler.image().entries().unwrap().is_empty());
}

#[test]
fn constant_dependencies_forward_count_new_graph_fails_closed() {
    let assembler = run_pass1(&[
        ".bfor count",
        "new = later+1",
        ".byte new",
        ".endfor",
        "count = 2",
        "later = 3",
    ]);
    assert!(assembler.diagnostics.iter().any(|d| d
        .error
        .message()
        .contains("activated a new constant definition")));
}

#[test]
fn constant_dependencies_forward_count_limit_checked_before_allocation() {
    let mut assembler = Assembler::new();
    assembler.max_loop_iterations = 1;
    let lines = [".for count", ".byte 1", ".endfor", "count = 2147483647"].map(str::to_string);
    assert!(assembler.pass1(&lines).errors > 0);
    assert!(assembler
        .diagnostics
        .iter()
        .any(|d| d.error.message().contains("maximum iteration limit")));
}

#[test]
fn constant_dependencies_forward_count_references() {
    let root = workspace_root();
    let reference = root.join("examples/reference/opcore");
    let output = root
        .join("target")
        .join(format!("forward-count-references-{}", process::id()));
    fs::create_dir_all(&output).unwrap();
    let update = std::env::var("opForge_UPDATE_REFERENCE").is_ok();
    assemble_example_with_base(
        &root.join("examples/opcore/loop_forward_constant.asm"),
        &output,
        "loop_forward_constant",
        false,
    )
    .unwrap();
    for extension in ["hex", "lst"] {
        let name = format!("loop_forward_constant.{extension}");
        let data = fs::read(output.join(&name)).unwrap();
        let path = reference.join(name);
        if update {
            fs::write(&path, &data).unwrap();
        }
        assert_eq!(
            fs::read(&path).unwrap(),
            data,
            "reference {}",
            path.display()
        );
    }
    let error =
        assemble_example_error(&root.join("examples/opcore/loop_pass_instability_error.asm"))
            .unwrap();
    let path = reference.join("loop_pass_instability_error.err");
    if update {
        fs::write(&path, format!("{error}\n")).unwrap();
    }
    assert_eq!(
        fs::read_to_string(path).unwrap().trim_end(),
        error.trim_end()
    );
}

#[test]
fn constant_dependencies_forward_count_repeated_observation_scale() {
    let start = std::time::Instant::now();
    assert_eq!(
        bytes(".for outer\n.for inner\n.byte 1\n.endfor\n.endfor\nouter = 10000\ninner = 1").len(),
        10000
    );
    eprintln!("10000 counted-loop occurrences: {:?}", start.elapsed());
}

#[test]
fn constant_dependencies_emission_rejects_unexpected_zero_count_loop() {
    let lines = [".byte 0", ".for 0", ".byte 1", ".endfor"].map(str::to_string);
    let mut assembler = Assembler::new();
    assert_eq!(assembler.pass1(&lines).errors, 0);
    assembler.loop_iteration_trace_pass1.clear();
    let mut out = Vec::new();
    let mut listing = ListingWriter::new(&mut out, false);
    assert!(assembler.pass2(&lines, &mut listing).unwrap().errors > 0);
    assert!(assembler.diagnostics.iter().any(|d| d
        .error
        .message()
        .contains("unexpected loop during emission")));
}

#[test]
fn constant_dependencies_emission_rejects_unconsumed_loop_trace() {
    let lines = [".byte 1"].map(str::to_string);
    let mut assembler = Assembler::new();
    assert_eq!(assembler.pass1(&lines).errors, 0);
    assembler.loop_iteration_trace_pass1.push((1, 0));
    let mut out = Vec::new();
    let mut listing = ListingWriter::new(&mut out, false);
    assert!(assembler.pass2(&lines, &mut listing).unwrap().errors > 0);
    assert!(assembler
        .diagnostics
        .iter()
        .any(|d| d.error.message().contains("loop traversal changed")));
}

#[test]
fn constant_dependencies_forward_count_rejects_new_loop_in_retained_iteration() {
    let assembler = run_pass1(&[
        ".for target+1",
        ".byte 0",
        ".if later",
        ".for 1",
        ".byte 1",
        ".endfor",
        ".endif",
        ".endfor",
        "later:",
        "target = 1",
    ]);
    assert!(assembler.diagnostics.iter().any(|diagnostic| diagnostic
        .error
        .message()
        .contains("loop iteration count changed between passes")));
}

#[test]
fn constant_dependencies_count_keeps_iterator_shadowing() {
    assert_eq!(bytes(".for index in {0,1}\n.for index\n.byte 7\n.endfor\n.endfor\nindex = 2\ntrigger = tail+1\ntail = 1"), [7]);
}

#[test]
fn constant_dependencies_scoped_constants_keep_iterator_snapshots() {
    assert_eq!(bytes(".bfor index in {0,1}\nsaved .const index\n.byte saved\n.for saved\n.byte 7\n.endfor\n.endfor\nindex = 2\ntrigger = tail+1\ntail = 1"), [0,1,7]);
}
