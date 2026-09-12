// SPDX-License-Identifier: GPL-3.0-or-later

//! Scoped diagnostics for explicitly instrumented target-semantic host boundaries.
//! This is not a proof that uninstrumented code is package-controlled.

use std::cell::RefCell;
use std::collections::BTreeMap;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Mode {
    Report,
    Refuse,
}

impl Mode {
    pub fn parse(value: &str) -> Result<Self, String> {
        match value {
            "report" => Ok(Self::Report),
            "refuse" => Ok(Self::Refuse),
            _ => Err("OPFORGE_TARGET_CALLBACKS must be report or refuse".into()),
        }
    }
}

struct State {
    mode: Mode,
    attempts: BTreeMap<(String, String, String, String), u64>,
    first_refusal: Option<String>,
    overflow: bool,
}

thread_local! {
    static ACTIVE: RefCell<Option<State>> = const { RefCell::new(None) };
}

pub struct Session {
    previous: Option<State>,
    pub emit_on_drop: bool,
}

/// An isolated assembly/test scope. Dropping restores any enclosing audit.
pub fn install(mode: Mode) -> Session {
    Session {
        previous: ACTIVE.with(|slot| {
            slot.replace(Some(State {
                mode,
                attempts: BTreeMap::new(),
                first_refusal: None,
                overflow: false,
            }))
        }),
        emit_on_drop: false,
    }
}

impl Drop for Session {
    fn drop(&mut self) {
        if self.emit_on_drop {
            if let Some(report) = snapshot() {
                eprintln!("[opforge target callbacks] {report}");
            }
        }
        ACTIVE.with(|slot| *slot.borrow_mut() = self.previous.take());
    }
}

/// Call immediately before invoking a target-semantic host operation, even if it
/// may decline the input. Counts attempts, not successful handling or bytecodes.
pub fn attempt(boundary: &str, family: &str, cpu: &str, detail: &str) -> Result<(), String> {
    ACTIVE.with(|slot| {
        let mut slot = slot.borrow_mut();
        let Some(state) = slot.as_mut() else {
            return Ok(());
        };
        if let Some(message) = &state.first_refusal {
            return Err(message.clone());
        }
        crate::vm_work::event("target_callback.attempts", 1);
        let key = (boundary.into(), family.into(), cpu.into(), detail.into());
        // Bound diagnostic cardinality independently of source/program size.
        if state.attempts.len() < 256 || state.attempts.contains_key(&key) {
            *state.attempts.entry(key).or_default() += 1;
        } else {
            state.overflow = true;
        }
        if state.mode == Mode::Refuse {
            crate::vm_work::event("target_callback.refusals", 1);
            let message = format!(
                "target callback refused: {boundary} (family={family}, cpu={cpu}, detail={detail})"
            );
            state.first_refusal = Some(message.clone());
            return Err(message);
        }
        Ok(())
    })
}

/// Refusal is sticky: callers must check before accepting fallback results or
/// publishing output, including when a candidate/parser swallowed its error.
pub fn check() -> Result<(), String> {
    ACTIVE.with(
        |slot| match slot.borrow().as_ref().and_then(|s| s.first_refusal.clone()) {
            Some(message) => Err(message),
            None => Ok(()),
        },
    )
}

pub fn snapshot() -> Option<serde_json::Value> {
    ACTIVE.with(|slot| {
        let slot = slot.borrow();
        let state = slot.as_ref()?;
        let attempts: Vec<_> = state
            .attempts
            .iter()
            .map(|((boundary, family, cpu, detail), count)| {
                serde_json::json!({"boundary": boundary, "family": family, "cpu": cpu,
                "detail": detail, "count": count})
            })
            .collect();
        Some(serde_json::json!({"schema": 1,
            "mode": if state.mode == Mode::Report { "report" } else { "refuse" },
            "attempts": attempts, "first_refusal": state.first_refusal,
            "overflow": state.overflow}))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn refusal_survives_ignored_error_and_nested_sessions_restore_state() {
        let session = install(Mode::Refuse);
        let first = attempt("resolver", "family", "cpu", "op").unwrap_err();
        // Deliberately discard the immediate error, as candidate recovery can do.
        assert_eq!(check(), Err(first.clone()));
        {
            let _inner = install(Mode::Report);
            assert!(attempt("resolver", "family", "cpu", "other").is_ok());
            assert!(check().is_ok());
        }
        assert_eq!(check(), Err(first.clone()));
        assert_eq!(attempt("other", "family", "cpu", "later"), Err(first));
        assert_eq!(snapshot().unwrap()["attempts"].as_array().unwrap().len(), 1);
        drop(session);
        assert!(snapshot().is_none());
        assert!(check().is_ok());
    }

    #[test]
    fn report_counts_attempts_and_marks_cardinality_overflow() {
        let _session = install(Mode::Report);
        for i in 0..300 {
            attempt("resolver", "family", "cpu", &i.to_string()).unwrap();
        }
        attempt("resolver", "family", "cpu", "0").unwrap();
        let report = snapshot().unwrap();
        assert_eq!(report["overflow"], true);
        assert_eq!(report["attempts"].as_array().unwrap().len(), 256);
        assert_eq!(report["attempts"][0]["count"], 2);
        assert!(report["first_refusal"].is_null());
        assert!(Mode::parse("off").is_err());
    }
}
