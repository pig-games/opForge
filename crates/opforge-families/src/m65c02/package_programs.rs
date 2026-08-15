// SPDX-License-Identifier: GPL-3.0-or-later

//! CPU-owned selector aliases for the WDC 65C02 extension.

use package::{
    compile_selector_map_program, OpcpuCodecError, SelectorProgramDescriptor,
    SELECTOR_VM_OPCODE_VERSION_V1,
};
use types::hierarchy::ScopedOwner;

pub fn selector_programs() -> Result<Vec<SelectorProgramDescriptor>, OpcpuCodecError> {
    let owner = ScopedOwner::Cpu("65c02".to_string());
    Ok(vec![SelectorProgramDescriptor {
        owner,
        id: "aliases.canonical".to_string(),
        opcode_version: SELECTOR_VM_OPCODE_VERSION_V1,
        priority: 0,
        cpu_allow_list: None,
        program: compile_selector_map_program(&[
            ("DEA".to_string(), "DEC".to_string()),
            ("INA".to_string(), "INC".to_string()),
        ])?,
    }])
}
