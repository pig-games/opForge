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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ProcessorErrorKind {
    InvalidRequest,
    Io,
    ProcessorDiagnostic,
    Internal,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessorFailureDetail {
    code: String,
    summary: String,
    field: Option<String>,
}

impl ProcessorFailureDetail {
    pub fn new(
        code: impl Into<String>,
        summary: impl Into<String>,
        field: Option<impl Into<String>>,
    ) -> Self {
        Self {
            code: code.into(),
            summary: summary.into(),
            field: field.map(Into::into),
        }
    }

    pub fn code(&self) -> &str {
        &self.code
    }

    pub fn summary(&self) -> &str {
        &self.summary
    }

    pub fn field(&self) -> Option<&str> {
        self.field.as_deref()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProcessorError {
    processor_id: String,
    kind: ProcessorErrorKind,
    code: String,
    summary: String,
    details: Vec<ProcessorFailureDetail>,
}

impl ProcessorError {
    pub fn new(
        processor_id: impl Into<String>,
        kind: ProcessorErrorKind,
        code: impl Into<String>,
        summary: impl Into<String>,
        details: Vec<ProcessorFailureDetail>,
    ) -> Self {
        Self {
            processor_id: processor_id.into(),
            kind,
            code: code.into(),
            summary: summary.into(),
            details,
        }
    }

    pub fn processor_id(&self) -> &str {
        &self.processor_id
    }

    pub fn kind(&self) -> ProcessorErrorKind {
        self.kind
    }

    pub fn code(&self) -> &str {
        &self.code
    }

    pub fn summary(&self) -> &str {
        &self.summary
    }

    pub fn details(&self) -> &[ProcessorFailureDetail] {
        &self.details
    }
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
