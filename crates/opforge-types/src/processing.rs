// SPDX-License-Identifier: GPL-3.0-or-later

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OpcoreRequestKind {
    Expr,
    Statement,
    ModuleItem,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProcessingRequestKind {
    Opcore(OpcoreRequestKind),
    Processor { processor: String, kind: String },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProcessingReturn {
    Request { request: ProcessingRequestKind },
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ProcessingOutcome<T, E> {
    Done(T),
    Return(ProcessingReturn),
    Error(E),
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LineProcessingTrace {
    requests: Vec<ProcessingRequestKind>,
}

impl LineProcessingTrace {
    pub fn requests(&self) -> &[ProcessingRequestKind] {
        &self.requests
    }

    pub fn push(&mut self, request: ProcessingRequestKind) {
        self.requests.push(request);
    }
}
