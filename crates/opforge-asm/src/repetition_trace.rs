// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Counted-loop replay proof and dynamic traversal validation.
use super::*;
use crate::state::AsmSymbolScopeState;
use opcore::parser::Expr;

#[derive(Clone)]
pub(crate) struct LoopObservation {
    pub line: u32,
    pub count: u32,
    pub original_count: u32,
    pub path: Vec<(u32, u32)>,
    pub operands: Vec<Expr>,
    pub scope: AsmSymbolScopeState,
    pub cpu: CpuType,
    pub counted: bool,
    pub immutable: bool,
    pub loop_variable_dependent: bool,
}

#[derive(Default)]
pub(crate) struct LoopReplayState {
    pub loop_observations: Vec<LoopObservation>,
    pub immutable_count_replay: bool,
    pub executed_constant_names: HashSet<(String, u32)>,
    pub immutable_scalar_constants: HashSet<String>,
}

impl LoopReplayState {
    pub(crate) fn resolve_loop_observations(
        &mut self,
        asm_line: &mut AsmLine<'_>,
        pass_num: u8,
        max_loop_iterations: u32,
        constant_layout_changed: &mut bool,
        diagnostics: &mut Vec<Diagnostic>,
        counts: &mut PassCounts,
    ) {
        self.immutable_scalar_constants = asm_line.immutable_scalar_constants.clone();
        let mut observations = std::mem::take(&mut asm_line.loop_observations);
        if pass_num > 1
            && self.immutable_count_replay
            && !asm_line
                .executed_constant_names()
                .is_subset(&self.executed_constant_names)
        {
            diagnostics.push(Diagnostic::new(
                1,
                Severity::Error,
                AsmError::new(
                    AsmErrorKind::Directive,
                    "immutable-count replay activated a new constant definition",
                    None,
                ),
            ));
            counts.errors += 1;
        }
        for observation in &mut observations {
            match asm_line.immutable_loop_count(observation, max_loop_iterations) {
                Ok(Some(count)) => {
                    observation.immutable = true;
                    if pass_num == 1 && observation.count != count {
                        self.immutable_count_replay = true;
                        *constant_layout_changed = true;
                        observation.count = count;
                    }
                }
                Ok(None) => {}
                Err((line, error)) => {
                    diagnostics.push(Diagnostic::new(line, Severity::Error, error));
                    counts.errors += 1;
                }
            }
        }
        if pass_num > 1 {
            let allow_new = self.immutable_count_replay;
            let previous_by_key: HashMap<_, _> = self
                .loop_observations
                .iter()
                .map(|observation| ((observation.line, observation.path.as_slice()), observation))
                .collect();
            let incoming_keys: HashSet<_> = observations
                .iter()
                .map(|observation| (observation.line, observation.path.as_slice()))
                .collect();
            for observation in &observations {
                let previous =
                    previous_by_key.get(&(observation.line, observation.path.as_slice()));
                let valid = previous.is_some_and(|previous| previous.count == observation.count)
                    || (previous.is_none()
                        && allow_new
                        && observation.immutable
                        && observation.path.iter().enumerate().any(
                            |(depth, (line, iteration))| {
                                previous_by_key
                                    .get(&(*line, &observation.path[..depth]))
                                    .is_some_and(|parent| {
                                        parent.immutable
                                            && *iteration >= parent.original_count
                                            && *iteration < parent.count
                                    })
                            },
                        ));
                if !valid {
                    diagnostics.push(Diagnostic::new(
                        observation.line,
                        Severity::Error,
                        AsmError::new(
                            AsmErrorKind::Directive,
                            "loop iteration count changed between passes",
                            None,
                        ),
                    ));
                    counts.errors += 1;
                }
            }
            for previous in &self.loop_observations {
                let present = incoming_keys.contains(&(previous.line, previous.path.as_slice()));
                let removed_iteration = allow_new
                    && previous
                        .path
                        .iter()
                        .enumerate()
                        .any(|(depth, (line, iteration))| {
                            previous_by_key
                                .get(&(*line, &previous.path[..depth]))
                                .is_some_and(|parent| {
                                    parent.immutable
                                        && parent.original_count != parent.count
                                        && *iteration >= parent.count
                                })
                        });
                if !present && !removed_iteration {
                    diagnostics.push(Diagnostic::new(
                        previous.line,
                        Severity::Error,
                        AsmError::new(
                            AsmErrorKind::Directive,
                            "loop traversal changed between passes",
                            None,
                        ),
                    ));
                    counts.errors += 1;
                }
            }
        }
        self.loop_observations = observations;
    }
}
