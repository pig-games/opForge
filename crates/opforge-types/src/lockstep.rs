// SPDX-License-Identifier: GPL-3.0-or-later

use crate::processing::ProcessingRequestKind;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionMode {
    Rust,
    Vm,
    Lockstep { continuation_head: ContinuationHead },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContinuationHead {
    Rust,
    Vm,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LockstepStage {
    OpcoreExpr,
    OpasmStatementParse,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LockstepComparisonCategory {
    Ast,
    Diagnostics,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LockstepCheckpoint {
    CoreExprAst { normalized: String },
    PortableLineAst { normalized: String },
    Diagnostic { normalized: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LockstepMatch {
    pub stage: LockstepStage,
    pub request: ProcessingRequestKind,
    pub category: LockstepComparisonCategory,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LockstepDivergence {
    pub stage: LockstepStage,
    pub processor_domain: String,
    pub request: ProcessingRequestKind,
    pub continuation_head: ContinuationHead,
    pub source_line: Option<u32>,
    pub active_cpu: Option<String>,
    pub active_dialect: Option<String>,
    pub left: LockstepCheckpoint,
    pub right: LockstepCheckpoint,
    pub category: LockstepComparisonCategory,
    pub reason_code: String,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LockstepReport {
    matches: Vec<LockstepMatch>,
    divergences: Vec<LockstepDivergence>,
}

impl LockstepReport {
    pub fn matches(&self) -> &[LockstepMatch] {
        &self.matches
    }

    pub fn divergences(&self) -> &[LockstepDivergence] {
        &self.divergences
    }

    pub fn has_divergences(&self) -> bool {
        !self.divergences.is_empty()
    }

    pub fn push_match(&mut self, entry: LockstepMatch) {
        self.matches.push(entry);
    }

    pub fn push_divergence(&mut self, entry: LockstepDivergence) {
        self.divergences.push(entry);
    }

    pub fn extend(&mut self, other: LockstepReport) {
        self.matches.extend(other.matches);
        self.divergences.extend(other.divergences);
    }
}
