use super::*;

impl HierarchyExecutionModel {
    pub(crate) fn ensure_expr_parser_contract_compatible_for_assembler(
        &self,
        contract: &RuntimeExprParserContract,
    ) -> Result<(), RuntimeBridgeError> {
        self.core
            .ensure_expr_parser_contract_compatible_for_assembler(contract)
    }
}
