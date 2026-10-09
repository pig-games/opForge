//! Raw struct cells are compared with the live Rust typed VM on identical bytes.
use crate::fs_uae_smoke::FsUaeSmokeOutcome;
use opcore::expr_vm::{
    eval_portable_expr_typed_program, ExprVmBinary as B, ExprVmOpcode as O,
    PortableExprEvalContext, PortableExprResultMode, PortableExprStructTypeFieldValue,
    PortableExprStructTypeValue, PortableExprTypedProgram, PortableExprValue as V,
    EXPR_VM_OPCODE_VERSION_V1,
};
use std::path::{Path, PathBuf};
use types::asm_value::ScalarProvenance;

const SYMBOLS: [&str; 7] = ["Point", "x", "y", "missing", "Alias", "Scalar", "Undefined"];
const ROW_BYTES: usize = 48;
const HARNESS: &str =
    "native/motorola68000/amigaos/test-harnesses/experimental/binary_struct_vm_harness.asm";

struct Context;
impl PortableExprEvalContext for Context {
    fn lookup_symbol(&self, name: &str) -> Option<i64> {
        name.eq_ignore_ascii_case("Scalar").then_some(7)
    }
    fn lookup_symbol_value(&self, name: &str) -> Option<V> {
        if name.eq_ignore_ascii_case("Point") || name.eq_ignore_ascii_case("Alias") {
            Some(V::StructType(PortableExprStructTypeValue {
                type_name: "Point".into(),
                fields: [("x", 0, 1), ("y", 1, 2)]
                    .into_iter()
                    .map(|(name, offset, size)| PortableExprStructTypeFieldValue {
                        field_name: name.into(),
                        offset,
                        size,
                    })
                    .collect(),
                size: 3,
            }))
        } else {
            self.lookup_symbol(name).map(V::Int)
        }
    }
    fn current_address(&self) -> Option<i64> {
        Some(80)
    }
    fn current_address_value(&self) -> Option<V> {
        Some(V::RelocatableScalar {
            value: 80,
            provenance: ScalarProvenance::Section("code".into()),
        })
    }
    fn pass(&self) -> u8 {
        2
    }
    fn symbol_is_finalized(&self, name: &str) -> Option<bool> {
        self.lookup_symbol_value(name).map(|_| true)
    }
}

struct Case {
    name: &'static str,
    code: Vec<u8>,
    succeeds: bool,
}

fn literal(code: &mut Vec<u8>, value: i64) {
    code.push(O::PushLiteral as u8);
    code.extend(value.to_le_bytes());
}

fn build(code: &mut Vec<u8>, type_id: u16, members: &[u16]) {
    code.push(O::BuildStructLiteral as u8);
    code.extend(type_id.to_le_bytes());
    code.extend((members.len() as u16).to_le_bytes());
    for member in members {
        code.extend(member.to_le_bytes());
    }
}

fn cases() -> Vec<Case> {
    let mut cases = Vec::new();
    let mut add = |name, mut code: Vec<u8>, succeeds| {
        code.push(O::End as u8);
        cases.push(Case {
            name,
            code,
            succeeds,
        });
    };
    let mut reversed = Vec::new();
    literal(&mut reversed, -7);
    literal(&mut reversed, 1i64 << 33);
    build(&mut reversed, 0, &[2, 1]);
    add(
        "literal preserves source order and signed64 cells",
        reversed,
        true,
    );
    let mut reloc = vec![O::PushCurrentAddress as u8];
    literal(&mut reloc, 9);
    build(&mut reloc, 0, &[1, 2]);
    add("raw literal retains field provenance", reloc.clone(), true);
    reloc.extend([O::GetMember as u8, 1, 0]);
    add("temporary member retains field provenance", reloc, true);
    add(
        "type member gives absolute schema offset",
        vec![O::PushSymbol as u8, 0, 0, O::GetMember as u8, 2, 0],
        true,
    );
    add(
        "raw type retains schema geometry",
        vec![O::PushSymbol as u8, 0, 0],
        true,
    );
    let mut alias = Vec::new();
    literal(&mut alias, 3);
    literal(&mut alias, 4);
    build(&mut alias, 4, &[1, 2]);
    add(
        "copied type alias retains nominal identity",
        alias.clone(),
        true,
    );
    for (name, type_id, members) in [
        ("missing required field", 0, vec![1]),
        ("duplicate field", 0, vec![1, 1]),
        ("unknown field", 0, vec![1, 3]),
        ("scalar is not a struct schema", 5, vec![1, 2]),
        ("undefined schema is rejected", 6, vec![1, 2]),
    ] {
        let mut code = Vec::new();
        for _ in &members {
            literal(&mut code, 7);
        }
        build(&mut code, type_id, &members);
        add(name, code, false);
    }
    let mut project = alias.clone();
    project.push(O::RequireScalar as u8);
    add("literal rejects scalar projection", project, false);
    let mut length = alias.clone();
    length.extend([O::CallBuiltin as u8, 1, 1, 0]);
    add("literal rejects len", length, false);
    for (name, operator, left, right, succeeds) in [
        ("canonical multiply operand", B::Multiply as u8, 2, 3, true),
        ("canonical equality operand", B::Eq as u8, 7, 7, true),
        ("canonical bit-and operand", B::BitAnd as u8, 3, 7, true),
        ("canonical left-shift operand", B::Shl as u8, 3, 2, true),
        ("canonical power operand", B::Power as u8, 2, 3, true),
        (
            "canonical subtraction keeps operand order",
            B::Subtract as u8,
            3,
            7,
            true,
        ),
        ("canonical binary operand is bounded", 20, 3, 7, false),
    ] {
        let mut code = Vec::new();
        literal(&mut code, left);
        literal(&mut code, right);
        code.extend([O::ApplyBinary as u8, operator]);
        add(name, code, succeeds);
    }
    let mut out_of_bounds = alias;
    let last = out_of_bounds.len() - 2;
    out_of_bounds[last] = SYMBOLS.len() as u8;
    add("member symbol ID is bounded", out_of_bounds, false);
    let mut truncated = cases[0].code.clone();
    truncated.truncate(truncated.len() - 2);
    cases.push(Case {
        name: "truncated member ID",
        code: truncated,
        succeeds: false,
    });
    cases
}

fn id(name: &str) -> u32 {
    SYMBOLS
        .iter()
        .position(|candidate| candidate.eq_ignore_ascii_case(name))
        .expect("oracle name has native numeric identity") as u32
}

fn scalar(value: V) -> [u32; 3] {
    let (value, provenance) = match value {
        V::Int(value) => (value, 0),
        V::RelocatableScalar { value, provenance } => (
            value,
            match provenance {
                ScalarProvenance::Absolute => 0,
                ScalarProvenance::Section(section) => {
                    assert_eq!(section, "code");
                    2
                }
                ScalarProvenance::Unsupported => u32::MAX,
            },
        ),
        value => panic!("unexpected scalar field {value:?}"),
    };
    [value as u32, (value >> 32) as u32, provenance]
}

fn oracle(cases: &[Case]) -> Vec<u8> {
    let mut expected = Vec::new();
    for case in cases {
        let program = PortableExprTypedProgram {
            opcode_version: EXPR_VM_OPCODE_VERSION_V1,
            code: case.code.clone(),
            symbols: SYMBOLS.iter().map(|name| (*name).into()).collect(),
            declared_stack_depth: 2,
            result_mode: PortableExprResultMode::ShapePreserving,
        };
        let result = eval_portable_expr_typed_program(&program, &Context, Default::default());
        assert_eq!(result.is_ok(), case.succeeds, "{}: {result:?}", case.name);
        let mut row = [0u32; ROW_BYTES / 4];
        match result.map(|result| result.value) {
            Err(_) => row[0] = 1,
            Ok(V::StructType(def)) => {
                assert_eq!(def.fields.len(), 2);
                row[1..4].copy_from_slice(&[5, id(&def.type_name), 2]);
                for (field, cell) in def.fields.into_iter().zip(row[4..].chunks_mut(4)) {
                    cell.copy_from_slice(&[id(&field.field_name), field.offset, field.size, 0]);
                }
            }
            Ok(V::StructLiteral(instance)) => {
                assert_eq!(instance.fields.len(), 2);
                row[1..4].copy_from_slice(&[6, id(&instance.type_name), 2]);
                for (field, cell) in instance.fields.into_iter().zip(row[4..].chunks_mut(4)) {
                    cell[0] = id(&field.field_name);
                    cell[1..].copy_from_slice(&scalar(field.value));
                }
            }
            Ok(value) => row[5..8].copy_from_slice(&scalar(value)),
        }
        expected.extend(row.into_iter().flat_map(u32::to_be_bytes));
    }
    expected
}

struct Scratch(PathBuf);
impl Drop for Scratch {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.0);
    }
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn assemble(root: &Path, cases: &[Case]) -> (Scratch, Vec<u8>, String) {
    use clap::Parser;
    use cli_core::{run_with_validated_cli_with_context, validate_cli, Cli};
    use std::fmt::Write;
    let path = std::env::temp_dir().join(format!(
        "opforge-struct-vm-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_nanos()
    ));
    std::fs::create_dir(&path).unwrap();
    let scratch = Scratch(path);
    let mut data = format!(
        "CASE_COUNT = {}\nOUTPUT_BYTES = CASE_COUNT*RESULT_BYTES\nCases\n",
        cases.len()
    );
    for index in 0..cases.len() {
        writeln!(data, "\t.long Case{index}, Case{index}End").unwrap();
    }
    for (index, case) in cases.iter().enumerate() {
        writeln!(data, "; {}\nCase{index}", case.name).unwrap();
        for bytes in case.code.chunks(16) {
            let encoded = bytes
                .iter()
                .map(|byte| format!("${byte:02x}"))
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(data, "\t.byte {encoded}").unwrap();
        }
        writeln!(data, "Case{index}End").unwrap();
    }
    let template = std::fs::read_to_string(root.join(HARNESS)).unwrap();
    assert_eq!(template.matches("; @CASE_DATA@").count(), 1);
    let source = template.replace("; @CASE_DATA@", &data);
    let entry = scratch.0.join("entry.asm");
    std::fs::write(&entry, &source).unwrap();
    let cli = Cli::parse_from([
        "opForge".to_owned(),
        entry.to_string_lossy().into_owned(),
        "--cpu".into(),
        "68020".into(),
        "-M".into(),
        root.join("native/motorola68000")
            .to_string_lossy()
            .into_owned(),
        "-I".into(),
        root.join("native/motorola68000/amigaos/debug")
            .to_string_lossy()
            .into_owned(),
    ]);
    let mut config = validate_cli(&cli).unwrap();
    config.out_dir = Some(scratch.0.clone());
    run_with_validated_cli_with_context(&cli, &config).unwrap_or_else(|error| match error {
        cli_core::CliRunError::Assembler { error, .. } => panic!(
            "native struct VM: {error}; diagnostics: {:?}",
            error.diagnostics()
        ),
        cli_core::CliRunError::Workflow { error, .. } => panic!("native struct VM: {error}"),
        _ => panic!("native struct VM warnings"),
    });
    let image = std::fs::read(scratch.0.join("struct-vm.hunk")).unwrap();
    (scratch, image, source)
}

#[test]
fn native_struct_vm_live_oracle() {
    let cases = cases();
    assert_eq!(oracle(&cases).len(), cases.len() * ROW_BYTES);
}

#[test]
fn native_struct_vm_host_assembles() {
    let cases = cases();
    let _ = oracle(&cases);
    assert!(!assemble(&workspace_root(), &cases).1.is_empty());
}

#[test]
#[ignore = "requires configured FS-UAE; fresh raw typed struct VM contract proof"]
fn native_struct_vm_fs_uae() {
    use crate::fs_uae_smoke::{
        run_prebuilt_compact_cli_case_from_env, OpforgeNativeCliPackageMode,
        OpforgeNativeCliParityCase, OpforgeNativeCliProof,
    };
    let root = workspace_root();
    let cases = cases();
    let expected = oracle(&cases);
    let (_scratch, image, source) = assemble(&root, &cases);
    let case = OpforgeNativeCliParityCase {
        name: "struct-vm",
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(source.as_bytes()),
        command_template: Some(""),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &[],
        proof: OpforgeNativeCliProof::ExactArtifact {
            relative_path: "Work/struct-vm.bin",
            rust_oracle: &expected,
        },
    };
    let result = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh raw struct VM completion");
    let FsUaeSmokeOutcome::Completed { runs } = result else {
        panic!("native execution required")
    };
    assert_eq!(runs.len(), 1);
    assert!(runs[0].success && runs[0].protocol_completed && runs[0].exit_code == Some(0));
}
