// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

// Transitional re-exports while capability snapshot ownership lives in engine.

pub use libopforge::registry::{CapabilitySnapshot, CpuCapabilityView};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn capability_snapshot_contains_runtime_directives() {
        let registry = crate::build_default_asm_registry();
        let snapshot = CapabilitySnapshot::from_registry(&registry);
        assert!(snapshot.directive_keywords.iter().any(|d| d == ".struct"));
        assert!(snapshot.directive_keywords.iter().any(|d| d == ".for"));
        assert!(snapshot.directive_keywords.iter().any(|d| d == ".while"));
        let cpu = registry
            .resolve_cpu_name("65816")
            .expect("65816 cpu must resolve");
        let view = snapshot
            .view_for_cpu(cpu)
            .expect("snapshot should contain 65816 view");
        assert!(view
            .runtime_directives
            .iter()
            .any(|name| name.eq_ignore_ascii_case(".assume")));
    }
}
