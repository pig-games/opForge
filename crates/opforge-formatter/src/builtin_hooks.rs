// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

use crate::{CpuFormatterHook, DialectFormatterHook, FamilyFormatterHook};
use registry::cpu::{CpuFamily, CpuType};

pub struct Intel8080DialectFormatterHook;

impl DialectFormatterHook for Intel8080DialectFormatterHook {
    fn family_id(&self) -> CpuFamily {
        families::families::intel8080::module::FAMILY_ID
    }

    fn dialect_id(&self) -> &'static str {
        families::families::intel8080::module::DIALECT_INTEL8080
    }
}

pub struct ZilogDialectFormatterHook;

impl DialectFormatterHook for ZilogDialectFormatterHook {
    fn family_id(&self) -> CpuFamily {
        families::families::intel8080::module::FAMILY_ID
    }

    fn dialect_id(&self) -> &'static str {
        families::families::intel8080::module::DIALECT_ZILOG
    }
}

pub struct Intel8080FamilyFormatterHook;

impl FamilyFormatterHook for Intel8080FamilyFormatterHook {
    fn family_id(&self) -> CpuFamily {
        families::families::intel8080::module::FAMILY_ID
    }
}

pub struct Motorola680xDialectFormatterHook;

impl DialectFormatterHook for Motorola680xDialectFormatterHook {
    fn family_id(&self) -> CpuFamily {
        families::families::m6800::module::FAMILY_ID
    }

    fn dialect_id(&self) -> &'static str {
        families::families::m6800::module::DIALECT_MOTOROLA680X
    }
}

pub struct Motorola6800FamilyFormatterHook;

impl FamilyFormatterHook for Motorola6800FamilyFormatterHook {
    fn family_id(&self) -> CpuFamily {
        families::families::m6800::module::FAMILY_ID
    }
}

pub struct Motorola68KDialectFormatterHook;

impl DialectFormatterHook for Motorola68KDialectFormatterHook {
    fn family_id(&self) -> CpuFamily {
        families::families::m68k::module::FAMILY_ID
    }

    fn dialect_id(&self) -> &'static str {
        families::families::m68k::module::DIALECT_MOTOROLA68K
    }
}

pub struct Motorola68KFamilyFormatterHook;

impl FamilyFormatterHook for Motorola68KFamilyFormatterHook {
    fn family_id(&self) -> CpuFamily {
        families::families::m68k::module::FAMILY_ID
    }
}

pub struct TransparentDialectFormatterHook;

impl DialectFormatterHook for TransparentDialectFormatterHook {
    fn family_id(&self) -> CpuFamily {
        families::families::mos6502::module::FAMILY_ID
    }

    fn dialect_id(&self) -> &'static str {
        families::families::mos6502::module::DIALECT_TRANSPARENT
    }
}

pub struct Mos6502FamilyFormatterHook;

impl FamilyFormatterHook for Mos6502FamilyFormatterHook {
    fn family_id(&self) -> CpuFamily {
        families::families::mos6502::module::FAMILY_ID
    }
}

pub struct M6502FormatterHook;

impl CpuFormatterHook for M6502FormatterHook {
    fn cpu_id(&self) -> CpuType {
        families::families::mos6502::module::CPU_ID
    }
}

pub struct I8085FormatterHook;

impl CpuFormatterHook for I8085FormatterHook {
    fn cpu_id(&self) -> CpuType {
        families::i8085::module::CPU_ID
    }
}

pub struct Z80FormatterHook;

impl CpuFormatterHook for Z80FormatterHook {
    fn cpu_id(&self) -> CpuType {
        families::z80::module::CPU_ID
    }
}

pub struct M65C02FormatterHook;

impl CpuFormatterHook for M65C02FormatterHook {
    fn cpu_id(&self) -> CpuType {
        families::m65c02::module::CPU_ID
    }
}

pub struct M65816FormatterHook;

impl CpuFormatterHook for M65816FormatterHook {
    fn cpu_id(&self) -> CpuType {
        families::m65816::module::CPU_ID
    }
}

pub struct M45GS02FormatterHook;

impl CpuFormatterHook for M45GS02FormatterHook {
    fn cpu_id(&self) -> CpuType {
        families::m45gs02::module::CPU_ID
    }
}

pub struct M6809FormatterHook;

impl CpuFormatterHook for M6809FormatterHook {
    fn cpu_id(&self) -> CpuType {
        families::m6809::module::CPU_ID
    }
}

pub struct HD6309FormatterHook;

impl CpuFormatterHook for HD6309FormatterHook {
    fn cpu_id(&self) -> CpuType {
        families::hd6309::module::CPU_ID
    }
}

pub struct M68000FormatterHook;

impl CpuFormatterHook for M68000FormatterHook {
    fn cpu_id(&self) -> CpuType {
        families::m68000::module::CPU_ID
    }
}
