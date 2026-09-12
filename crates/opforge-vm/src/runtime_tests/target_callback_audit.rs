use super::*;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
};
use types::target_callbacks::{self as audit, Mode};

#[derive(Debug)]
struct DecliningResolver(Arc<AtomicUsize>);

impl FamilyExprResolver for DecliningResolver {
    fn family_id(&self) -> &str {
        "mos6502"
    }
    fn resolve_candidates(
        &self,
        _: &HierarchyExecutionModel,
        _: &ResolvedHierarchy,
        _: &str,
        _: &[Expr],
        _: &dyn AssemblerContext,
    ) -> Result<Option<Vec<registry::registry::VmEncodeCandidate>>, RuntimeBridgeError> {
        self.0.fetch_add(1, Ordering::Relaxed);
        Ok(None)
    }
}

#[test]
fn target_callback_refusal_prevents_invocation_and_package_fallback() {
    let mut model = HierarchyExecutionModel::from_registry(&mos6502_family_registry()).unwrap();
    let calls = Arc::new(AtomicUsize::new(0));
    model.register_family_expr_resolver(Box::new(DecliningResolver(Arc::clone(&calls))));
    let ctx = TestAssemblerContext::new();
    let encode = || model.encode_instruction_from_exprs("m6502", None, "NOP", &[], &ctx);
    let baseline = encode().unwrap();
    assert_eq!(baseline, Some(vec![0xea]));
    {
        let _session = audit::install(Mode::Report);
        assert_eq!(encode().unwrap(), baseline);
        assert_eq!(audit::snapshot().unwrap()["attempts"][0]["count"], 1);
    }
    let before = calls.load(Ordering::Relaxed);
    {
        let _session = audit::install(Mode::Refuse);
        assert!(encode()
            .unwrap_err()
            .to_string()
            .contains("family_candidate_resolver"));
        assert_eq!(calls.load(Ordering::Relaxed), before);
        assert!(audit::check().is_err());
    }
    assert_eq!(encode().unwrap(), baseline);
}

#[test]
fn target_callback_audit_distinguishes_surface_parsing_from_shared_emission() {
    let model =
        HierarchyExecutionModel::from_registry(&mos6502_and_motorola68000_registry()).unwrap();
    let line = " move.l (a0)+,d0";
    let baseline = model
        .parse_portable_line_for_assembler("m68000", None, line, 1)
        .unwrap();
    {
        let _session = audit::install(Mode::Report);
        let parsed = model
            .parse_portable_line_for_assembler("m68000", None, line, 1)
            .unwrap();
        assert_eq!(parsed, baseline);
        assert!(audit::snapshot().unwrap()["attempts"]
            .as_array()
            .unwrap()
            .iter()
            .any(|row| row["boundary"] == "family_operand_surface"));
    }
    {
        let _session = audit::install(Mode::Refuse);
        let _ = model.parse_portable_line_for_assembler("m68000", None, line, 1);
        assert!(audit::check()
            .unwrap_err()
            .contains("family_operand_surface"));
    }
    // Already-parsed input uses package selector/semantic execution without a
    // family callback. Strict mode must permit these shared implementations.
    let span = Span::default();
    let operands = [
        Expr::Immediate(Box::new(Expr::Number("7".into(), span)), span),
        Expr::Register("d0".into(), span),
    ];
    let _session = audit::install(Mode::Refuse);
    let bytes = model
        .encode_instruction_from_exprs(
            "m68000",
            None,
            "MOVEQ",
            &operands,
            &TestAssemblerContext::new(),
        )
        .unwrap();
    assert_eq!(bytes, Some(vec![0x70, 7]));
    assert!(audit::check().is_ok());
    assert!(audit::snapshot().unwrap()["attempts"]
        .as_array()
        .unwrap()
        .is_empty());
}

#[test]
fn target_callback_audit_covers_inline_family_postfix_route() {
    let model = HierarchyExecutionModel::from_registry(&parity_registry()).unwrap();
    let _session = audit::install(Mode::Refuse);
    let _ = model.parse_portable_line_for_assembler("m6809", None, " lda ,x+", 1);
    assert!(audit::check()
        .unwrap_err()
        .contains("family_indexed_postfix"));
}
