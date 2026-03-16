// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

// Transitional re-exports while LSP CPU context helpers live in engine.

pub use libopforge::registry::{
    parse_cpu_directive_name, resolve_cpu_for_line, scan_cpu_transitions,
};

#[cfg(test)]
mod tests {
    use super::*;
    use families::{mos6502, z80};

    #[test]
    fn resolve_cpu_context_prefers_nearest_prior_directive() {
        let registry = crate::build_default_asm_registry();
        let lines = vec![
            ".cpu 6502".to_string(),
            "lda #$01".to_string(),
            ".cpu z80".to_string(),
            "ld a,1".to_string(),
        ];
        let transitions = scan_cpu_transitions(&lines, &registry);
        assert_eq!(
            resolve_cpu_for_line(2, &transitions, None),
            mos6502::module::CPU_ID
        );
        assert_eq!(
            resolve_cpu_for_line(4, &transitions, None),
            z80::module::CPU_ID
        );
    }
}
