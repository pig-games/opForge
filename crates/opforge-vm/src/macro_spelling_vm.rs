// SPDX-License-Identifier: GPL-3.0-or-later
//! VM-owned transient fragment lexing for spelling descriptors. These lexical
//! tokens describe spelling only, and must never replace packed execution tokens.
use crate::execution_model::HierarchyExecutionModel;
use crate::macro_descriptor_vm::{self, Descriptor, DescriptorError, TokenSpan};
use crate::runtime_model_types::RuntimeTokenizerVmProgram;
use crate::runtime_portable_types::{PortableTokenizeRequest, PortableTokenizerByteStream};
/// The supplied list is literal content, so leading parentheses belong to an
/// argument. The selected macro program must use envelope flags 8 (comments).
pub fn execute(
    model: &HierarchyExecutionModel,
    tokenizer: &RuntimeTokenizerVmProgram,
    request: &PortableTokenizeRequest<'_>,
    macro_program: &[u8],
    capacity: usize,
    steps: usize,
) -> Result<Vec<Descriptor>, DescriptorError> {
    if request.source_line.len() > 253 {
        return Err(DescriptorError {
            status: 4,
            offset: 0,
            message: "Fragment extent exceeds service bound",
        });
    }
    let source = format!(".x {}", request.source_line);
    let temporary = PortableTokenizeRequest {
        source_line: &source,
        source_stream: PortableTokenizerByteStream::from_source_line(&source),
        ..request.clone()
    };
    let tokens = model
        .tokenize_with_vm_core(&temporary, tokenizer)
        .map_err(|_| DescriptorError {
            status: 5,
            offset: 0,
            message: "Fragment tokenization failed",
        })?;
    if tokens.len() > 64 {
        return Err(DescriptorError {
            status: 7,
            offset: 0,
            message: "Fragment lexical capacity exceeded",
        });
    }
    let spans = tokens
        .iter()
        .map(|t| TokenSpan {
            start: (t.span.col_start - 1) as u32,
            end: (t.span.col_end - 1) as u32,
        })
        .collect::<Vec<_>>();
    let mut records =
        macro_descriptor_vm::execute(2, 2, macro_program, &source, &spans, capacity, steps)
            .map_err(|mut e| {
                e.offset = e.offset.saturating_sub(3);
                e
            })?;
    for record in &mut records {
        record.source_start -= 3;
        record.source_end -= 3;
    }
    Ok(records)
}
