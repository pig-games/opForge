// SPDX-License-Identifier: GPL-3.0-or-later

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::OnceLock;
use std::time::{Duration, Instant};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(usize)]
pub enum PhaseBucket {
    AssemblyTotal = 0,
    PrepareSourceModuleLoading,
    PrepareParseLineAst,
    PrepareMacroSegmentStatementExpand,
    PrepareModuleUseImport,
    Pass1Total,
    Pass1InitialLayoutPass,
    Pass1LayoutStabilizationRetries,
    Pass1ParseLineAst,
    Pass1LineRoute,
    Pass1ExprEval,
    Pass1ModuleUseImport,
    Pass1SymbolLookup,
    Pass1SymbolUpdate,
    Pass1LayoutSectionRegion,
    Pass1RepetitionLoopExecution,
    Pass1DiagnosticsGeneration,
    Pass1DiagnosticsDedup,
    Pass2Total,
    Pass2ParseLineAst,
    Pass2LineRoute,
    Pass2ExprEval,
    Pass2ModuleUseImport,
    Pass2SymbolLookup,
    Pass2SymbolUpdate,
    Pass2LayoutSectionRegion,
    Pass2RepetitionLoopExecution,
    Pass2DiagnosticsGeneration,
    Pass2DiagnosticsDedup,
    Pass2ListingGeneration,
    Pass2OutputImageEmission,
    PostOutputEmission,
}

const ALL_BUCKETS: [PhaseBucket; 32] = [
    PhaseBucket::AssemblyTotal,
    PhaseBucket::PrepareSourceModuleLoading,
    PhaseBucket::PrepareParseLineAst,
    PhaseBucket::PrepareMacroSegmentStatementExpand,
    PhaseBucket::PrepareModuleUseImport,
    PhaseBucket::Pass1Total,
    PhaseBucket::Pass1InitialLayoutPass,
    PhaseBucket::Pass1LayoutStabilizationRetries,
    PhaseBucket::Pass1ParseLineAst,
    PhaseBucket::Pass1LineRoute,
    PhaseBucket::Pass1ExprEval,
    PhaseBucket::Pass1ModuleUseImport,
    PhaseBucket::Pass1SymbolLookup,
    PhaseBucket::Pass1SymbolUpdate,
    PhaseBucket::Pass1LayoutSectionRegion,
    PhaseBucket::Pass1RepetitionLoopExecution,
    PhaseBucket::Pass1DiagnosticsGeneration,
    PhaseBucket::Pass1DiagnosticsDedup,
    PhaseBucket::Pass2Total,
    PhaseBucket::Pass2ParseLineAst,
    PhaseBucket::Pass2LineRoute,
    PhaseBucket::Pass2ExprEval,
    PhaseBucket::Pass2ModuleUseImport,
    PhaseBucket::Pass2SymbolLookup,
    PhaseBucket::Pass2SymbolUpdate,
    PhaseBucket::Pass2LayoutSectionRegion,
    PhaseBucket::Pass2RepetitionLoopExecution,
    PhaseBucket::Pass2DiagnosticsGeneration,
    PhaseBucket::Pass2DiagnosticsDedup,
    PhaseBucket::Pass2ListingGeneration,
    PhaseBucket::Pass2OutputImageEmission,
    PhaseBucket::PostOutputEmission,
];

#[derive(Debug, Clone, Copy, Default)]
struct PhaseStat {
    duration: Duration,
    count: usize,
}

#[derive(Debug)]
struct PhaseProfileState {
    stats: [PhaseStat; ALL_BUCKETS.len()],
}

impl Default for PhaseProfileState {
    fn default() -> Self {
        Self {
            stats: std::array::from_fn(|_| PhaseStat::default()),
        }
    }
}

#[derive(Clone, Debug)]
struct PhaseProfileHandle(Rc<RefCell<PhaseProfileState>>);

impl PhaseProfileHandle {
    fn new() -> Self {
        Self(Rc::new(RefCell::new(PhaseProfileState::default())))
    }

    fn record(&self, bucket: PhaseBucket, duration: Duration) {
        let mut state = self.0.borrow_mut();
        let stat = &mut state.stats[bucket as usize];
        stat.duration += duration;
        stat.count += 1;
    }

    fn snapshot(&self) -> [PhaseStat; ALL_BUCKETS.len()] {
        self.0.borrow().stats
    }
}

#[derive(Debug)]
struct ActiveScope {
    bucket: PhaseBucket,
    started_at: Instant,
    child_time: Duration,
}

thread_local! {
    static CURRENT_PROFILE: RefCell<Option<PhaseProfileHandle>> = const { RefCell::new(None) };
    static ACTIVE_SCOPES: RefCell<Vec<ActiveScope>> = const { RefCell::new(Vec::new()) };
}

thread_local! {
    static CURRENT_PATH_PROFILE: RefCell<Option<PathProfileHandle>> = const { RefCell::new(None) };
}

fn profile_enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("OPFORGE_PROFILE_PHASES").is_some())
}

pub fn path_profile_is_enabled() -> bool {
    static ENABLED: OnceLock<bool> = OnceLock::new();
    *ENABLED.get_or_init(|| std::env::var_os("OPFORGE_PROFILE_EXECUTION_PATHS").is_some())
}

fn with_current_profile<R>(f: impl FnOnce(&PhaseProfileHandle) -> R) -> Option<R> {
    if !profile_enabled() {
        return None;
    }
    CURRENT_PROFILE.with(|slot| slot.borrow().as_ref().map(f))
}

pub struct InstalledPhaseProfile {
    previous: Option<PhaseProfileHandle>,
}

impl Drop for InstalledPhaseProfile {
    fn drop(&mut self) {
        CURRENT_PROFILE.with(|slot| {
            *slot.borrow_mut() = self.previous.take();
        });
        ACTIVE_SCOPES.with(|stack| stack.borrow_mut().clear());
    }
}

pub fn install_for_current_thread_if_enabled() -> Option<InstalledPhaseProfile> {
    if !profile_enabled() {
        return None;
    }
    let handle = PhaseProfileHandle::new();
    let previous = CURRENT_PROFILE.with(|slot| slot.replace(Some(handle)));
    ACTIVE_SCOPES.with(|stack| stack.borrow_mut().clear());
    // also install optional per-path profile when requested
    if path_profile_is_enabled() {
        let path_handle = PathProfileHandle::new();
        let _prev = CURRENT_PATH_PROFILE.with(|slot| slot.replace(Some(path_handle)));
    }
    Some(InstalledPhaseProfile { previous })
}

pub fn record_direct(bucket: PhaseBucket, duration: Duration) {
    let _ = with_current_profile(|profile| profile.record(bucket, duration));
}

pub fn record_execution_path(bucket: Option<PhaseBucket>, label: &str, duration: Duration) {
    if !path_profile_is_enabled() {
        return;
    }
    CURRENT_PATH_PROFILE.with(|slot| {
        if let Some(handle) = slot.borrow().as_ref() {
            handle.record(bucket, label, duration);
        }
    });
}

pub struct PhaseScopeGuard {
    enabled: bool,
}

impl PhaseScopeGuard {
    pub fn disabled() -> Self {
        Self { enabled: false }
    }
}

impl Drop for PhaseScopeGuard {
    fn drop(&mut self) {
        if !self.enabled {
            return;
        }
        let scope = ACTIVE_SCOPES.with(|stack| stack.borrow_mut().pop());
        let Some(scope) = scope else {
            return;
        };
        let elapsed = scope.started_at.elapsed();
        let exclusive = elapsed.saturating_sub(scope.child_time);
        let _ = with_current_profile(|profile| profile.record(scope.bucket, exclusive));
        ACTIVE_SCOPES.with(|stack| {
            if let Some(parent) = stack.borrow_mut().last_mut() {
                parent.child_time += elapsed;
            }
        });
    }
}

pub fn scope(bucket: PhaseBucket) -> PhaseScopeGuard {
    if with_current_profile(|_| ()).is_none() {
        return PhaseScopeGuard::disabled();
    }
    ACTIVE_SCOPES.with(|stack| {
        stack.borrow_mut().push(ActiveScope {
            bucket,
            started_at: Instant::now(),
            child_time: Duration::default(),
        });
    });
    PhaseScopeGuard { enabled: true }
}

fn phase_name(bucket: PhaseBucket) -> &'static str {
    match bucket {
        PhaseBucket::AssemblyTotal => "assembly_total",
        PhaseBucket::PrepareSourceModuleLoading => "prepare.source_module_loading",
        PhaseBucket::PrepareParseLineAst => "prepare.parse_line_ast",
        PhaseBucket::PrepareMacroSegmentStatementExpand => "prepare.macro_segment_statement_expand",
        PhaseBucket::PrepareModuleUseImport => "prepare.module_use_import",
        PhaseBucket::Pass1Total => "pass1_total",
        PhaseBucket::Pass1InitialLayoutPass => "pass1.initial_layout_pass",
        PhaseBucket::Pass1LayoutStabilizationRetries => "pass1.layout_stabilization_retries",
        PhaseBucket::Pass1ParseLineAst => "pass1.parse_line_ast",
        PhaseBucket::Pass1LineRoute => "pass1.line_route",
        PhaseBucket::Pass1ExprEval => "pass1.expr_eval",
        PhaseBucket::Pass1ModuleUseImport => "pass1.module_use_import",
        PhaseBucket::Pass1SymbolLookup => "pass1.symbol_lookup",
        PhaseBucket::Pass1SymbolUpdate => "pass1.symbol_update",
        PhaseBucket::Pass1LayoutSectionRegion => "pass1.layout_section_region",
        PhaseBucket::Pass1RepetitionLoopExecution => "pass1.repetition_loop_execution",
        PhaseBucket::Pass1DiagnosticsGeneration => "pass1.diagnostics_generation",
        PhaseBucket::Pass1DiagnosticsDedup => "pass1.diagnostics_dedup",
        PhaseBucket::Pass2Total => "pass2_total",
        PhaseBucket::Pass2ParseLineAst => "pass2.parse_line_ast",
        PhaseBucket::Pass2LineRoute => "pass2.line_route",
        PhaseBucket::Pass2ExprEval => "pass2.expr_eval",
        PhaseBucket::Pass2ModuleUseImport => "pass2.module_use_import",
        PhaseBucket::Pass2SymbolLookup => "pass2.symbol_lookup",
        PhaseBucket::Pass2SymbolUpdate => "pass2.symbol_update",
        PhaseBucket::Pass2LayoutSectionRegion => "pass2.layout_section_region",
        PhaseBucket::Pass2RepetitionLoopExecution => "pass2.repetition_loop_execution",
        PhaseBucket::Pass2DiagnosticsGeneration => "pass2.diagnostics_generation",
        PhaseBucket::Pass2DiagnosticsDedup => "pass2.diagnostics_dedup",
        PhaseBucket::Pass2ListingGeneration => "pass2.listing_generation",
        PhaseBucket::Pass2OutputImageEmission => "pass2.output_image_emission",
        PhaseBucket::PostOutputEmission => "post.output_emission",
    }
}

fn phase_is_detail(bucket: PhaseBucket) -> bool {
    !matches!(
        bucket,
        PhaseBucket::AssemblyTotal | PhaseBucket::Pass1Total | PhaseBucket::Pass2Total
    )
}

fn phase_is_child(bucket: PhaseBucket) -> bool {
    matches!(
        bucket,
        PhaseBucket::Pass1InitialLayoutPass
            | PhaseBucket::Pass1LayoutStabilizationRetries
            | PhaseBucket::Pass1ParseLineAst
            | PhaseBucket::Pass1LineRoute
            | PhaseBucket::Pass1ExprEval
            | PhaseBucket::Pass1ModuleUseImport
            | PhaseBucket::Pass1SymbolLookup
            | PhaseBucket::Pass1SymbolUpdate
            | PhaseBucket::Pass1LayoutSectionRegion
            | PhaseBucket::Pass1RepetitionLoopExecution
            | PhaseBucket::Pass1DiagnosticsGeneration
            | PhaseBucket::Pass1DiagnosticsDedup
            | PhaseBucket::Pass2ParseLineAst
            | PhaseBucket::Pass2LineRoute
            | PhaseBucket::Pass2ExprEval
            | PhaseBucket::Pass2ModuleUseImport
            | PhaseBucket::Pass2SymbolLookup
            | PhaseBucket::Pass2SymbolUpdate
            | PhaseBucket::Pass2LayoutSectionRegion
            | PhaseBucket::Pass2RepetitionLoopExecution
            | PhaseBucket::Pass2DiagnosticsGeneration
            | PhaseBucket::Pass2DiagnosticsDedup
            | PhaseBucket::Pass2ListingGeneration
            | PhaseBucket::Pass2OutputImageEmission
    )
}

fn print_stat(bucket: PhaseBucket, stat: PhaseStat, assembly_total: Duration) {
    let indent = if phase_is_child(bucket) { "  " } else { "" };
    let pct = if assembly_total.is_zero() {
        0.0
    } else {
        (stat.duration.as_secs_f64() / assembly_total.as_secs_f64()) * 100.0
    };
    let count_suffix = if phase_is_detail(bucket) && stat.count > 0 {
        format!("  ({}x)", stat.count)
    } else {
        String::new()
    };
    eprintln!(
        "{indent}{:<30} {:>10.3} ms {:>7.2}%{}",
        phase_name(bucket),
        stat.duration.as_secs_f64() * 1000.0,
        pct,
        count_suffix,
    );
}

pub fn emit_summary_if_active() {
    let snapshot = with_current_profile(|profile| profile.snapshot());
    let Some(stats) = snapshot else {
        return;
    };
    let assembly_total = stats[PhaseBucket::AssemblyTotal as usize].duration;
    eprintln!("[opforge phase profile]");
    for bucket in ALL_BUCKETS {
        let stat = stats[bucket as usize];
        if stat.duration.is_zero() && stat.count == 0 {
            continue;
        }
        print_stat(bucket, stat, assembly_total);
    }
    // emit per-path profile if active
    if path_profile_is_enabled() {
        CURRENT_PATH_PROFILE.with(|slot| {
            if let Some(handle) = slot.borrow().as_ref() {
                handle.emit_summary(assembly_total);
            }
        });
    }
}

#[derive(Debug, Clone, Default)]
struct PathStat {
    duration: Duration,
    count: usize,
}

#[derive(Debug)]
struct PathProfileState {
    // map: bucket_index (usize, with usize::MAX for global) -> label -> PathStat
    stats: HashMap<usize, HashMap<String, PathStat>>,
}

impl PathProfileState {
    fn new() -> Self {
        Self {
            stats: HashMap::new(),
        }
    }
}

#[derive(Clone, Debug)]
struct PathProfileHandle(Rc<RefCell<PathProfileState>>);

impl PathProfileHandle {
    fn new() -> Self {
        Self(Rc::new(RefCell::new(PathProfileState::new())))
    }

    fn record(&self, bucket: Option<PhaseBucket>, label: &str, duration: Duration) {
        let mut state = self.0.borrow_mut();
        let key = bucket.map(|b| b as usize).unwrap_or(usize::MAX);
        let entry = state.stats.entry(key).or_default();
        let stat = entry.entry(label.to_string()).or_default();
        stat.duration += duration;
        stat.count += 1;
    }

    fn emit_summary(&self, assembly_total: Duration) {
        eprintln!("[opforge execution profile]");
        let state = self.0.borrow();
        // sort keys for stability: global last
        let mut keys: Vec<usize> = state.stats.keys().copied().collect();
        keys.sort_unstable();
        for key in keys {
            let label_map = &state.stats[&key];
            if key == usize::MAX {
                eprintln!("global:");
            } else {
                // try map key back to PhaseBucket name
                let bucket = unsafe { std::mem::transmute::<usize, PhaseBucket>(key) };
                eprintln!("{}", phase_name(bucket));
            }
            // sort labels
            let mut labels: Vec<_> = label_map.iter().collect();
            labels.sort_by(|a, b| a.0.cmp(b.0));
            for (lbl, stat) in labels {
                let pct = if assembly_total.is_zero() {
                    0.0
                } else {
                    (stat.duration.as_secs_f64() / assembly_total.as_secs_f64()) * 100.0
                };
                eprintln!(
                    "  {lbl:25} {:10.3} ms {:7.2}%  ({}x)",
                    stat.duration.as_secs_f64() * 1000.0,
                    pct,
                    stat.count
                );
            }
        }
    }
}
