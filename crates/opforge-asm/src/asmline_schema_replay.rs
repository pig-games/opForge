// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Preserve executed schema metadata needed after its code block is pruned.
use super::*;
use types::asm_value::StructDef;

#[derive(Clone)]
pub(crate) struct SchemaReplaySnapshot {
    pub(crate) definition: StructDef,
    pub(crate) visibility: SymbolVisibility,
    pub(crate) module: Option<String>,
}

pub(crate) struct SchemaReplayUse {
    pub(crate) owner: Option<String>,
    pub(crate) snapshot: SchemaReplaySnapshot,
}

impl AsmLine<'_> {
    pub(super) fn record_schema_replay_use(&mut self, name: &str, span: Span) {
        if matches!(self.profile_phase, AsmProfilePhase::Pass2) {
            return;
        }
        let Ok(definition) = self.resolve_struct_def_for_literal(name, span) else {
            return;
        };
        let Some(entry) = self.symbols.entry(&definition.name) else {
            return;
        };
        let snapshot = SchemaReplaySnapshot {
            definition,
            visibility: entry.visibility,
            module: entry.module_id.clone(),
        };
        let owner = self.block_unit_stack.iter().rev().find_map(|(index, _)| {
            index
                .and_then(|index| self.reachable_blocks.get(index))
                .map(|block| block.symbol.clone())
        });
        self.schema_replay_uses
            .push(SchemaReplayUse { owner, snapshot });
    }

    pub(crate) fn seed_schema_replay(
        &mut self,
        schemas: &[SchemaReplaySnapshot],
    ) -> Result<(), AsmError> {
        for snapshot in schemas {
            let definition = &snapshot.definition;
            // Metadata keeps its original visibility and module. It creates no
            // executable block or source reference to the declaration's code.
            if self.symbols.entry(&definition.name).is_none() {
                let result = self.symbols.add(
                    &definition.name,
                    definition.size,
                    false,
                    snapshot.visibility,
                    snapshot.module.as_deref(),
                );
                if result != SymbolTableResult::Ok {
                    return Err(AsmError::new(
                        AsmErrorKind::Symbol,
                        "cannot restore pruned schema symbol",
                        Some(&definition.name),
                    ));
                }
            }
            if let Some(entry) = self.symbols.entry_mut(&definition.name) {
                entry.val = definition.size;
                entry.updated = true;
            }
            self.layout
                .absolute_constant_symbols
                .insert(definition.name.clone());
            self.layout.symbol_relocations.insert(
                definition.name.clone(),
                crate::state::SymbolRelocation::Absolute,
            );
            self.seeded_schema_names
                .insert(definition.name.to_ascii_uppercase());
            self.set_value_symbol(&definition.name, AsmValue::Struct(definition.clone()));
            self.struct_table
                .register(definition.clone())
                .map_err(|_| {
                    AsmError::new(
                        AsmErrorKind::Directive,
                        "duplicate pruned schema metadata",
                        Some(&definition.name),
                    )
                })?;
        }
        Ok(())
    }
}
