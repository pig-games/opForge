// SPDX-License-Identifier: GPL-3.0-or-later

//! Opt-in, thread-local aggregate interpreter work. No runtime operand/state snapshots; program bytes may contain literals.
//! Program identity is exact (engine, version, bytes), not a hash or memory address.

use std::cell::RefCell;
use std::collections::{BTreeMap, HashSet};
use std::rc::Rc;

const MAX_PROGRAM_BYTES: usize = 4 * 1024 * 1024;
const MAX_POSITIONS: usize = 200_000;

#[derive(Default)]
struct State {
    phase: &'static str,
    programs: Vec<(&'static str, u16, Vec<u8>)>,
    rows: BTreeMap<(&'static str, usize), Rc<RefCell<Row>>>,
    events: BTreeMap<(&'static str, &'static str), u64>,
    program_bytes: usize,
    positions: usize,
    overflow: bool,
}

#[derive(Default)]
struct Row {
    calls: u64,
    steps: u64,
    repeated_within_call: u64,
    positions: BTreeMap<(usize, u8), u64>,
}

thread_local! {
    static ACTIVE: RefCell<Option<Rc<RefCell<State>>>> = const { RefCell::new(None) };
}

pub struct Session(Option<Rc<RefCell<State>>>);

/// Starts an isolated measurement on this thread; dropping restores its predecessor.
/// The caller owns enablement, lifetime, and emitting the report.
pub fn install() -> Session {
    Session(ACTIVE.with(|slot| {
        slot.replace(Some(Rc::new(RefCell::new(State {
            phase: "global",
            ..State::default()
        }))))
    }))
}

impl Drop for Session {
    fn drop(&mut self) {
        ACTIVE.with(|slot| *slot.borrow_mut() = self.0.take());
    }
}

pub struct PhaseGuard(Option<(Rc<RefCell<State>>, &'static str)>);

impl PhaseGuard {
    pub fn disabled() -> Self {
        Self(None)
    }
}

pub fn phase(name: &'static str) -> PhaseGuard {
    PhaseGuard(ACTIVE.with(|slot| {
        slot.borrow().as_ref().map(|state| {
            let previous = std::mem::replace(&mut state.borrow_mut().phase, name);
            (Rc::clone(state), previous)
        })
    }))
}

impl Drop for PhaseGuard {
    fn drop(&mut self) {
        if let Some((state, previous)) = &self.0 {
            state.borrow_mut().phase = previous;
        }
    }
}

struct ActiveRun {
    state: Rc<RefCell<State>>,
    row: Rc<RefCell<Row>>,
}

pub struct ProgramRun {
    active: Option<ActiveRun>,
    visited: RefCell<HashSet<(usize, u8)>>,
}

impl ProgramRun {
    pub fn new(engine: &'static str, version: u16, program: &[u8]) -> Self {
        let active = ACTIVE.with(|slot| {
            let state = slot.borrow().as_ref().cloned()?;
            let mut data = state.borrow_mut();
            let id = match data
                .programs
                .iter()
                .position(|(e, v, bytes)| *e == engine && *v == version && bytes == program)
            {
                Some(id) => id,
                None => {
                    if data.program_bytes + program.len() > MAX_PROGRAM_BYTES
                        || data.programs.len() >= 4096
                    {
                        data.overflow = true;
                        return None;
                    }
                    let id = data.programs.len();
                    data.program_bytes += program.len();
                    data.programs.push((engine, version, program.to_vec()));
                    id
                }
            };
            let phase = data.phase;
            let row = Rc::clone(data.rows.entry((phase, id)).or_default());
            row.borrow_mut().calls += 1;
            drop(data);
            Some(ActiveRun { state, row })
        });
        Self {
            active,
            visited: RefCell::new(HashSet::new()),
        }
    }

    /// Count a dispatched operation, including one that subsequently returns an error.
    /// Positions are byte offsets unless the engine is explicitly labelled `.steps`.
    pub fn step(&self, position: usize, opcode: u8) {
        let Some(ActiveRun { state, row }) = &self.active else {
            return;
        };
        let mut row = row.borrow_mut();
        let key = (position, opcode);
        if !row.positions.contains_key(&key) {
            let mut state = state.borrow_mut();
            if state.positions >= MAX_POSITIONS {
                state.overflow = true;
                return;
            }
            state.positions += 1;
        }
        row.steps += 1;
        *row.positions.entry(key).or_default() += 1;
        if !self.visited.borrow_mut().insert(key) {
            row.repeated_within_call += 1;
        }
    }

    pub fn event(&self, label: &'static str, amount: u64) {
        if self.active.is_some() {
            event(label, amount);
        }
    }
}

/// Count actual helper work separately from interpreter dispatches.
pub fn event(label: &'static str, amount: u64) {
    ACTIVE.with(|slot| {
        if let Some(state) = slot.borrow().as_ref() {
            let mut state = state.borrow_mut();
            let phase = state.phase;
            *state.events.entry((phase, label)).or_default() += amount;
        }
    });
}

/// One JSON object; overflow makes the measurement explicitly incomplete.
pub fn snapshot() -> Option<serde_json::Value> {
    ACTIVE.with(|slot| {
        let slot = slot.borrow();
        let state = slot.as_ref()?.borrow();
        let programs: Vec<_> = state
            .programs
            .iter()
            .enumerate()
            .map(|(id, (engine, version, bytes))| {
                serde_json::json!({"id": id, "engine": engine, "version": version,
                "bytes_hex": bytes.iter().map(|b| format!("{b:02x}")).collect::<String>()})
            })
            .collect();
        let rows: Vec<_> = state
            .rows
            .iter()
            .map(|((phase, id), row)| {
                let row = row.borrow();
                let positions: Vec<_> = row.positions.iter().map(|((pc, opcode), count)|
                serde_json::json!({"position": pc, "opcode": opcode, "count": count})).collect();
                serde_json::json!({"phase": phase, "program": id, "calls": row.calls,
                "steps": row.steps, "repeated_within_call": row.repeated_within_call,
                "positions": positions})
            })
            .collect();
        let events: Vec<_> = state.events.iter().map(|((phase,label),count)|
            serde_json::json!({"phase":phase,"label":label,"count":count})).collect();
        Some(serde_json::json!({"schema":1,"overflow":state.overflow,
            "programs":programs,"rows":rows,"events":events}))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn counts_loops_and_reinvocation_separately_with_exact_identity() {
        let _session = install();
        let _phase = phase("pass1");
        let run = ProgramRun::new("test", 1, &[3, 4]);
        run.step(0, 3);
        run.step(0, 3);
        ProgramRun::new("test", 1, &[3, 4]).step(0, 3);
        ProgramRun::new("test", 2, &[3, 4]).step(0, 3);
        let report = snapshot().unwrap();
        assert_eq!(report["programs"].as_array().unwrap().len(), 2);
        assert_eq!(report["rows"][0]["calls"], 2);
        assert_eq!(report["rows"][0]["steps"], 3);
        assert_eq!(report["rows"][0]["repeated_within_call"], 1);
    }

    #[test]
    fn nested_sessions_and_phases_restore_and_disabled_calls_do_nothing() {
        assert!(snapshot().is_none());
        ProgramRun::new("off", 1, &[0]).step(0, 0);
        let session = install();
        {
            let _phase = phase("pass2");
            event("attempt", 1);
        }
        {
            let _nested = install();
            assert_eq!(snapshot().unwrap()["events"], serde_json::json!([]));
        }
        event("attempt", 2);
        let report = snapshot().unwrap();
        assert_eq!(report["events"][0]["phase"], "global");
        assert_eq!(report["events"][1]["phase"], "pass2");
        drop(session);
        assert!(snapshot().is_none());
    }

    #[test]
    fn oversized_program_marks_report_incomplete() {
        let _session = install();
        ProgramRun::new("large", 1, &vec![0; MAX_PROGRAM_BYTES + 1]);
        assert_eq!(snapshot().unwrap()["overflow"], true);
    }
}
