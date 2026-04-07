// SPDX-License-Identifier: GPL-3.0-or-later

//! Family and CPU module implementations for libopforge.

pub mod hd6309;
pub mod i8085;
pub mod intel8080;
pub mod m45gs02;
pub mod m65816;
pub mod m65c02;
pub mod m6800;
pub mod m68000;
pub mod m68010;
pub mod m68020;
pub mod m68030;
pub mod m68040;
pub mod m68080;
pub mod m6809;
pub mod m68k;
pub mod mos6502;
pub mod z80;

use registry::registry::AsmRegistry;

// Transitional compatibility namespace so moved implementations can keep the
// existing crate::families::... paths while extraction is in progress.
pub mod families {
    pub mod intel8080 {
        pub use crate::intel8080::*;
    }

    pub mod m68k {
        pub use crate::m68k::*;
    }

    pub mod m6800 {
        pub use crate::m6800::*;
    }

    pub mod mos6502 {
        pub use crate::mos6502::*;
    }
}

pub fn register_intel8080_family_stack(registry: &mut AsmRegistry) {
    registry.register_family(Box::new(intel8080::module::Intel8080FamilyModule));
    registry.register_cpu(Box::new(i8085::module::I8085CpuModule));
    registry.register_cpu(Box::new(z80::module::Z80CpuModule));
}

pub fn register_motorola6800_family_stack(registry: &mut AsmRegistry) {
    registry.register_family(Box::new(m6800::module::Motorola6800FamilyModule));
    registry.register_cpu(Box::new(m6809::module::M6809CpuModule));
    registry.register_cpu(Box::new(hd6309::module::HD6309CpuModule));
}

pub fn register_motorola68000_family_stack(registry: &mut AsmRegistry) {
    registry.register_family(Box::new(m68k::module::Motorola68000FamilyModule));
    registry.register_cpu(Box::new(m68000::module::M68000CpuModule));
    registry.register_cpu(Box::new(m68010::module::M68010CpuModule));
    registry.register_cpu(Box::new(m68020::module::M68020CpuModule));
    registry.register_cpu(Box::new(m68030::module::M68030CpuModule));
    registry.register_cpu(Box::new(m68040::module::M68040CpuModule));
    registry.register_cpu(Box::new(m68080::module::M68080CpuModule));
}

pub fn register_mos6502_family_stack(registry: &mut AsmRegistry) {
    registry.register_family(Box::new(mos6502::module::MOS6502FamilyModule));
    registry.register_cpu(Box::new(mos6502::module::M6502CpuModule));
    registry.register_cpu(Box::new(m65c02::module::M65C02CpuModule));
    registry.register_cpu(Box::new(m65816::module::M65816CpuModule));
    registry.register_cpu(Box::new(m45gs02::module::M45GS02CpuModule));
}

#[cfg(test)]
mod tests {
    use super::{
        register_intel8080_family_stack, register_mos6502_family_stack,
        register_motorola68000_family_stack, register_motorola6800_family_stack,
    };
    use registry::cpu::CpuType;
    use registry::registry::AsmRegistry;

    #[test]
    fn intel8080_stack_registration_exposes_expected_aliases() {
        let mut registry = AsmRegistry::new();
        register_intel8080_family_stack(&mut registry);

        assert_eq!(
            registry.resolve_cpu_name("8080"),
            Some(CpuType::new("8085"))
        );
        assert_eq!(registry.resolve_cpu_name("z80"), Some(CpuType::new("z80")));
        assert!(registry
            .family_ids()
            .into_iter()
            .any(|family| family.as_str() == "intel8080"));
    }

    #[test]
    fn motorola6800_stack_registration_exposes_expected_aliases() {
        let mut registry = AsmRegistry::new();
        register_motorola6800_family_stack(&mut registry);

        assert_eq!(
            registry.resolve_cpu_name("6809"),
            Some(CpuType::new("m6809"))
        );
        assert_eq!(
            registry.resolve_cpu_name("6309"),
            Some(CpuType::new("hd6309"))
        );
        assert!(registry
            .family_ids()
            .into_iter()
            .any(|family| family.as_str() == "motorola6800"));
    }

    #[test]
    fn motorola68000_stack_registration_exposes_expected_aliases() {
        let mut registry = AsmRegistry::new();
        register_motorola68000_family_stack(&mut registry);

        assert_eq!(
            registry.resolve_cpu_name("68000"),
            Some(CpuType::new("m68000"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68000"),
            Some(CpuType::new("m68000"))
        );
        assert_eq!(
            registry.resolve_cpu_name("68010"),
            Some(CpuType::new("m68010"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68010"),
            Some(CpuType::new("m68010"))
        );
        assert_eq!(
            registry.resolve_cpu_name("68020"),
            Some(CpuType::new("m68020"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68020"),
            Some(CpuType::new("m68020"))
        );
        assert_eq!(
            registry.resolve_cpu_name("68030"),
            Some(CpuType::new("m68030"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68030"),
            Some(CpuType::new("m68030"))
        );
        assert_eq!(
            registry.resolve_cpu_name("68040"),
            Some(CpuType::new("m68040"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68040"),
            Some(CpuType::new("m68040"))
        );
        assert_eq!(
            registry.resolve_cpu_name("68080"),
            Some(CpuType::new("m68080"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mc68080"),
            Some(CpuType::new("m68080"))
        );
        assert!(registry
            .family_ids()
            .into_iter()
            .any(|family| family.as_str() == "motorola68000"));
    }

    #[test]
    fn mos6502_stack_registration_exposes_expected_aliases() {
        let mut registry = AsmRegistry::new();
        register_mos6502_family_stack(&mut registry);

        assert_eq!(
            registry.resolve_cpu_name("6502"),
            Some(CpuType::new("m6502"))
        );
        assert_eq!(
            registry.resolve_cpu_name("65c816"),
            Some(CpuType::new("65816"))
        );
        assert_eq!(
            registry.resolve_cpu_name("mega65"),
            Some(CpuType::new("45gs02"))
        );
        assert!(registry
            .family_ids()
            .into_iter()
            .any(|family| family.as_str() == "mos6502"));
    }
}
