use super::*;

impl HierarchyExecutionModel {
    pub(super) fn interned_id(&self, value_lower: &str) -> Option<u32> {
        self.core.interned_ids.get(value_lower).copied()
    }

    pub(super) fn scoped_owner_lookup_order(
        &self,
        resolved: &ResolvedHierarchy,
    ) -> [(u8, Option<u32>); 3] {
        let dialect_id = resolved.dialect_id.to_ascii_lowercase();
        let cpu_id = resolved.cpu_id.to_ascii_lowercase();
        let family_id = resolved.family_id.to_ascii_lowercase();
        [
            (2u8, self.interned_id(&dialect_id)),
            (1u8, self.interned_id(&cpu_id)),
            (0u8, self.interned_id(&family_id)),
        ]
    }

    pub(super) fn encode_candidates(
        &self,
        resolved: &ResolvedHierarchy,
        mnemonic: &str,
        candidates: &[VmEncodeCandidate],
    ) -> Result<Option<Vec<u8>>, RuntimeBridgeError> {
        self.core.encode_candidates(resolved, mnemonic, candidates)
    }

    pub(super) fn enforce_candidate_budget(
        &self,
        candidates: &[VmEncodeCandidate],
    ) -> Result<(), RuntimeBridgeError> {
        self.core.enforce_candidate_budget(candidates)
    }

    pub(super) fn budget_error(name: &str, limit: usize, observed: usize) -> RuntimeBridgeError {
        RuntimeModelCore::budget_error(name, limit, observed)
    }
}
