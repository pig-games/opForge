// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Counted-loop replay proof and dynamic traversal validation.
use super::*;
use crate::state::AsmSymbolScopeState;
use opcore::parser::Expr;

pub(crate) type GuardKey = (u32, Vec<(u32, u32)>);

#[derive(Clone)]
pub(crate) struct GuardObservation {
    pub key: GuardKey,
    pub tests: Vec<Expr>,
    pub last_test_positive: bool,
    pub active: bool,
    pub unresolved_at_observation: bool,
    pub scope: AsmSymbolScopeState,
    pub cpu: CpuType,
    pub loop_variable_dependent: bool,
}

#[derive(Clone)]
pub(crate) struct LoopObservation {
    pub line: u32,
    pub guards: Vec<GuardKey>,
    pub count: u32,
    pub original_count: u32,
    pub path: Vec<(u32, u32)>,
    pub operands: Vec<Expr>,
    pub scope: AsmSymbolScopeState,
    pub cpu: CpuType,
    pub counted: bool,
    pub immutable: bool,
    pub value_owner_replay: bool,
    pub loop_variable_dependent: bool,
}

#[derive(Default)]
pub(crate) struct LoopReplayState {
    pub loop_observations: Vec<LoopObservation>,
    pub activated_guards: HashSet<GuardKey>,
    pub deactivated_guards: HashSet<GuardKey>,
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
        if pass_num == 1 {
            self.activated_guards.clear();
            self.deactivated_guards.clear();
            for guard in std::mem::take(&mut asm_line.guard_observations) {
                if let Some(active) = asm_line.immutable_guard_activity(&guard) {
                    if active != guard.active {
                        *constant_layout_changed = true;
                        if active {
                            self.activated_guards.insert(guard.key);
                        } else {
                            self.deactivated_guards.insert(guard.key);
                        }
                    }
                }
            }
        }
        let mut observations = std::mem::take(&mut asm_line.loop_observations);
        if pass_num > 1 {
            // A certified typed owner authorizes the parent traversal extent for
            // this retry. Keep its original extent for membership checks, and
            // apply the same fail-closed definition checks as scalar replay.
            for observation in &observations {
                if !observation.value_owner_replay {
                    continue;
                }
                if let Some(previous) = self.loop_observations.iter_mut().find(|previous| {
                    previous.line == observation.line && previous.path == observation.path
                }) {
                    if previous.count != observation.count {
                        self.immutable_count_replay = true;
                        previous.immutable = true;
                        previous.count = observation.count;
                    }
                }
            }
            let (any_new, certified) = asm_line.certified_new_constant_definitions(
                &self.executed_constant_names,
                &self.activated_guards,
            );
            if any_new
                && !certified
                && (self.immutable_count_replay
                    || self
                        .loop_observations
                        .iter()
                        .any(|previous| previous.original_count != previous.count))
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
            } else if any_new && certified {
                // Admit one graph batch under predicates certified by the initial
                // traversal. Subsequent count-expanded definitions still fail
                // closed after those guard certificates have been consumed.
                self.executed_constant_names = asm_line.executed_constant_names();
                match asm_line.resolve_absolute_constants() {
                    Ok(changed) => *constant_layout_changed |= changed,
                    Err((line, error)) => {
                        diagnostics.push(Diagnostic::new(line, Severity::Error, error));
                        counts.errors += 1;
                    }
                }
                self.immutable_scalar_constants = asm_line.immutable_scalar_constants.clone();
            }
        }
        for observation in &mut observations {
            match asm_line.immutable_loop_count(observation, max_loop_iterations) {
                Ok(Some(count)) => {
                    observation.immutable = true;
                    let activated = observation
                        .guards
                        .iter()
                        .any(|key| self.activated_guards.contains(key));
                    if (pass_num == 1 || activated) && observation.count != count {
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
            let allow_new = self.immutable_count_replay
                || self
                    .loop_observations
                    .iter()
                    .any(|previous| previous.original_count != previous.count);
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
                let valid = previous.is_some_and(|previous| {
                    previous.count == observation.count || observation.value_owner_replay
                }) || (previous.is_none()
                    && ((observation
                        .guards
                        .iter()
                        .any(|key| self.activated_guards.contains(key)))
                        || (allow_new
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
                            ))));
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
                let removed_branch = previous
                    .guards
                    .iter()
                    .any(|key| self.deactivated_guards.contains(key));
                if !present && !removed_iteration && !removed_branch {
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
        if pass_num > 1 {
            self.activated_guards.clear();
            self.deactivated_guards.clear();
        }
    }
}
