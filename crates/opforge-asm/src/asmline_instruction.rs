// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Instruction processing for `AsmLine`.
//!
//! Handles instruction encoding, VM runtime dispatch, dialect‐fixit
//! hints, and "instruction not found" diagnostics.

use super::*;

#[cfg(not(feature = "vm-runtime-only"))]
#[derive(Debug, Clone)]
struct InstructionOutputFixupPlan {
    offset: u32,
    target_section: String,
    encoded_value: u32,
}

impl<'a> AsmLine<'a> {
    #[cfg(not(feature = "vm-runtime-only"))]
    const M68K_V03_EXPLICIT_LONG_NOTATION_ERROR: &'static str =
        "symbolic absolute instruction operands require explicit .L notation outside the supported v0.3 bare-symbol subset";

    pub fn process_instruction_ast(&mut self, mnemonic: &str, operands: &[Expr]) -> LineStatus {
        #[cfg(feature = "vm-runtime-only")]
        {
            if self.in_section() {
                self.mark_current_section_not_relocation_free();
            }
            self.try_encode_instruction_vm_only(mnemonic, operands)
                .unwrap_or_else(|| {
                    self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Instruction,
                        &format!(
                            "VM runtime model unavailable for CPU '{}'",
                            self.cpu.as_str()
                        ),
                        None,
                    )
                })
        }

        #[cfg(not(feature = "vm-runtime-only"))]
        {
            let pipeline = match Self::resolve_pipeline_for_cpu(self.registry, self.cpu) {
                Ok(pipeline) => pipeline,
                Err(message) => {
                    return self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Instruction,
                        &message,
                        None,
                    );
                }
            };

            let mut rewritten_operands = None;
            let family_operands = match pipeline.family.parse_operands(mnemonic, operands) {
                Ok(ops) => ops,
                Err(err) => {
                    if pipeline.family_id == M68K_FAMILY_ID {
                        rewritten_operands = self
                            .m68k_canonicalize_supported_bare_symbol_operands(mnemonic, operands);
                        if let Some(rewritten) = rewritten_operands.as_ref() {
                            match pipeline
                                .family
                                .parse_operands(mnemonic, rewritten.as_slice())
                            {
                                Ok(ops) => ops,
                                Err(retry_err) => {
                                    if let Some(span) = self
                                        .m68k_raw_explicit_long_notation_required(
                                            mnemonic, operands,
                                        )
                                    {
                                        return self.failure_at_span(
                                            LineStatus::Error,
                                            AsmErrorKind::Instruction,
                                            Self::M68K_V03_EXPLICIT_LONG_NOTATION_ERROR,
                                            None,
                                            span,
                                        );
                                    }
                                    return self.failure_at_span(
                                        LineStatus::Error,
                                        AsmErrorKind::Instruction,
                                        &retry_err.message,
                                        None,
                                        retry_err.span,
                                    );
                                }
                            }
                        } else if let Some(span) =
                            self.m68k_raw_explicit_long_notation_required(mnemonic, operands)
                        {
                            return self.failure_at_span(
                                LineStatus::Error,
                                AsmErrorKind::Instruction,
                                Self::M68K_V03_EXPLICIT_LONG_NOTATION_ERROR,
                                None,
                                span,
                            );
                        } else {
                            return self.failure_at_span(
                                LineStatus::Error,
                                AsmErrorKind::Instruction,
                                &err.message,
                                None,
                                err.span,
                            );
                        }
                    } else {
                        return self.failure_at_span(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            &err.message,
                            None,
                            err.span,
                        );
                    }
                }
            };

            let effective_operands = rewritten_operands.as_deref().unwrap_or(operands);

            let (mapped_mnemonic, mapped_operands) = pipeline
                .dialect
                .map_mnemonic(mnemonic, family_operands.as_ref())
                .unwrap_or_else(|| (mnemonic.to_string(), family_operands.clone()));

            if pipeline.family_id == M68K_FAMILY_ID {
                if let Some(m68k_mapped_operands) = mapped_operands
                    .as_any()
                    .downcast_ref::<M68KFamilyOperands>()
                {
                    if let Some(span) = self.m68k_explicit_long_notation_required(
                        &mapped_mnemonic,
                        m68k_mapped_operands.0.as_slice(),
                    ) {
                        return self.failure_at_span(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            Self::M68K_V03_EXPLICIT_LONG_NOTATION_ERROR,
                            None,
                            span,
                        );
                    }
                }
            }

            if let Some(status) = self.try_encode_instruction_via_runtime_expr(
                &pipeline,
                mnemonic,
                effective_operands,
                family_operands.as_ref(),
                &mapped_mnemonic,
                mapped_operands.as_ref(),
            ) {
                return status;
            }

            match pipeline.family.encode_family_operands(
                &mapped_mnemonic,
                mnemonic,
                mapped_operands.as_ref(),
                self,
            ) {
                registry::family::FamilyEncodeResult::Ok(bytes) => {
                    if let Err(err) = self.validate_instruction_emit_span(
                        &mapped_mnemonic,
                        effective_operands,
                        bytes.len(),
                    ) {
                        return self.failure_at_span(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            err.error.message(),
                            None,
                            err.span,
                        );
                    }
                    if self.in_section()
                        && !self.family_operands_keep_current_section_relocation_free(
                            pipeline.family_id,
                            mapped_operands.as_ref(),
                        )
                    {
                        self.mark_current_section_not_relocation_free();
                    }
                    self.bytes.extend_from_slice(&bytes);
                    return LineStatus::Ok;
                }
                registry::family::FamilyEncodeResult::Error {
                    bytes,
                    message,
                    span,
                    param,
                } => {
                    self.bytes.extend_from_slice(&bytes);
                    if let Some(span) = span {
                        return self.failure_at_span(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            &message,
                            param.as_deref(),
                            span,
                        );
                    }
                    return self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Instruction,
                        &message,
                        param.as_deref(),
                    );
                }
                registry::family::FamilyEncodeResult::NotFound => {}
            }

            let resolved_operands =
                match pipeline
                    .cpu
                    .resolve_operands(mnemonic, mapped_operands.as_ref(), self)
                {
                    Ok(ops) => ops,
                    Err(err) => {
                        return self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            &err,
                            None,
                        )
                    }
                };

            if let Some(validator) = pipeline.validator.as_ref() {
                if let Err(err) = validator.validate_instruction(
                    &mapped_mnemonic,
                    resolved_operands.as_ref(),
                    self,
                ) {
                    return self.failure(LineStatus::Error, AsmErrorKind::Instruction, &err, None);
                }
            }

            if let Some(status) = self.try_encode_instruction_via_runtime_operands(
                &pipeline,
                &mapped_mnemonic,
                effective_operands,
                resolved_operands.as_ref(),
            ) {
                return status;
            }

            match pipeline
                .family
                .encode_instruction(&mapped_mnemonic, resolved_operands.as_ref(), self)
                .into_outcome()
            {
                Ok(Some(mut bytes)) => {
                    if let Err(err) = self.validate_instruction_emit_span(
                        &mapped_mnemonic,
                        effective_operands,
                        bytes.len(),
                    ) {
                        return self.failure_at_span(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            err.error.message(),
                            None,
                            err.span,
                        );
                    }
                    let supported_output_fixups = match self.queue_instruction_output_fixups(
                        pipeline.family_id,
                        resolved_operands.as_ref(),
                        &mut bytes,
                    ) {
                        Ok(supported) => supported,
                        Err(status) => return status,
                    };
                    if self.in_section()
                        && !supported_output_fixups
                        && !self.family_operands_keep_current_section_relocation_free(
                            pipeline.family_id,
                            mapped_operands.as_ref(),
                        )
                    {
                        self.mark_current_section_not_relocation_free();
                    }
                    self.bytes.extend_from_slice(&bytes);
                    self.apply_cpu_runtime_state_after_encode(
                        pipeline.cpu.as_ref(),
                        &mapped_mnemonic,
                        resolved_operands.as_ref(),
                    );
                    LineStatus::Ok
                }
                Err(err) => {
                    if let Some(span) = err.span {
                        self.failure_at_span(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            &err.message,
                            None,
                            span,
                        )
                    } else {
                        self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            &err.message,
                            None,
                        )
                    }
                }
                Ok(None) => match pipeline
                    .cpu
                    .encode_instruction(&mapped_mnemonic, resolved_operands.as_ref(), self)
                    .into_outcome()
                {
                    Ok(Some(mut bytes)) => {
                        if let Err(err) = self.validate_instruction_emit_span(
                            &mapped_mnemonic,
                            operands,
                            bytes.len(),
                        ) {
                            return self.failure_at_span(
                                LineStatus::Error,
                                AsmErrorKind::Instruction,
                                err.error.message(),
                                None,
                                err.span,
                            );
                        }
                        let supported_output_fixups = match self.queue_instruction_output_fixups(
                            pipeline.family_id,
                            resolved_operands.as_ref(),
                            &mut bytes,
                        ) {
                            Ok(supported) => supported,
                            Err(status) => return status,
                        };
                        if self.in_section()
                            && !supported_output_fixups
                            && !self.family_operands_keep_current_section_relocation_free(
                                pipeline.family_id,
                                mapped_operands.as_ref(),
                            )
                        {
                            self.mark_current_section_not_relocation_free();
                        }
                        self.bytes.extend_from_slice(&bytes);
                        self.apply_cpu_runtime_state_after_encode(
                            pipeline.cpu.as_ref(),
                            &mapped_mnemonic,
                            resolved_operands.as_ref(),
                        );
                        LineStatus::Ok
                    }
                    Err(err) => {
                        if let Some(span) = err.span {
                            self.failure_at_span(
                                LineStatus::Error,
                                AsmErrorKind::Instruction,
                                &err.message,
                                None,
                                span,
                            )
                        } else {
                            self.failure(
                                LineStatus::Error,
                                AsmErrorKind::Instruction,
                                &err.message,
                                None,
                            )
                        }
                    }
                    Ok(None) => self.failure_instruction_not_found(
                        LineStatus::Error,
                        &pipeline,
                        mnemonic,
                        family_operands.as_ref(),
                    ),
                },
            }
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_raw_explicit_long_notation_required(
        &self,
        mnemonic: &str,
        operands: &[Expr],
    ) -> Option<Span> {
        let upper = mnemonic.to_ascii_uppercase();
        match upper.as_str() {
            "LEA" | "PEA" | "JMP" | "JSR" => operands
                .first()
                .filter(|expr| Self::expr_is_bare_symbol_candidate(expr))
                .map(expr_span),
            "MOVEA.L" => operands
                .first()
                .filter(|expr| Self::expr_is_bare_symbol_candidate(expr))
                .map(expr_span),
            "MOVE.L" => operands
                .iter()
                .find(|expr| Self::expr_is_bare_symbol_candidate(expr))
                .map(expr_span),
            _ => None,
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_canonicalize_supported_bare_symbol_operands(
        &self,
        mnemonic: &str,
        operands: &[Expr],
    ) -> Option<Vec<Expr>> {
        let upper = mnemonic.to_ascii_uppercase();
        match upper.as_str() {
            "LEA" => match operands {
                [src, dst]
                    if Self::expr_is_bare_symbol_candidate(src)
                        && Self::expr_is_address_register_candidate(dst) =>
                {
                    Some(vec![
                        Self::wrap_expr_with_absolute_long_suffix(src),
                        dst.clone(),
                    ])
                }
                _ => None,
            },
            "PEA" | "JMP" | "JSR" => match operands {
                [expr] if Self::expr_is_bare_symbol_candidate(expr) => {
                    Some(vec![Self::wrap_expr_with_absolute_long_suffix(expr)])
                }
                _ => None,
            },
            "MOVE.L" => match operands {
                [src, dst]
                    if Self::expr_is_bare_symbol_candidate(src)
                        && Self::expr_is_data_register_candidate(dst) =>
                {
                    Some(vec![
                        Self::wrap_expr_with_absolute_long_suffix(src),
                        dst.clone(),
                    ])
                }
                [src, dst]
                    if Self::expr_is_data_register_candidate(src)
                        && Self::expr_is_bare_symbol_candidate(dst) =>
                {
                    Some(vec![
                        src.clone(),
                        Self::wrap_expr_with_absolute_long_suffix(dst),
                    ])
                }
                _ => None,
            },
            "MOVEA.L" => match operands {
                [src, dst]
                    if Self::expr_is_bare_symbol_candidate(src)
                        && Self::expr_is_address_register_candidate(dst) =>
                {
                    Some(vec![
                        Self::wrap_expr_with_absolute_long_suffix(src),
                        dst.clone(),
                    ])
                }
                _ => None,
            },
            _ => None,
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn expr_is_bare_symbol_candidate(expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(_, _) => true,
            Expr::Unary {
                op: UnaryOp::Plus,
                expr: inner,
                ..
            } => Self::expr_is_bare_symbol_candidate(inner),
            _ => false,
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn wrap_expr_with_absolute_long_suffix(expr: &Expr) -> Expr {
        Expr::Member {
            base: Box::new(expr.clone()),
            field: "L".to_string(),
            span: expr_span(expr),
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn expr_is_data_register_candidate(expr: &Expr) -> bool {
        matches!(
            Self::raw_register_name(expr),
            Some(name)
                if name.len() == 2
                    && name.starts_with('D')
                    && matches!(name.as_bytes()[1], b'0'..=b'7')
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn expr_is_address_register_candidate(expr: &Expr) -> bool {
        matches!(
            Self::raw_register_name(expr),
            Some(name)
                if name == "SP"
                    || (name.len() == 2
                        && name.starts_with('A')
                        && matches!(name.as_bytes()[1], b'0'..=b'7'))
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn raw_register_name(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(name, _) | Expr::Register(name, _) => Some(name.to_ascii_uppercase()),
            _ => None,
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn queue_instruction_output_fixups(
        &mut self,
        family_id: CpuFamily,
        operands: &dyn OperandSet,
        bytes: &mut [u8],
    ) -> Result<bool, LineStatus> {
        if !self.in_section() || family_id != M68K_FAMILY_ID {
            return Ok(false);
        }
        let Some(m68k_operands) = operands.as_any().downcast_ref::<M68KOperands>() else {
            return Ok(false);
        };
        let fixups = match self.m68k_instruction_output_fixups(m68k_operands.0.as_slice(), bytes) {
            Ok(fixups) => fixups,
            Err((kind, message, span)) => {
                return Err(self.failure_at_span(LineStatus::Error, kind, &message, None, span))
            }
        };
        if fixups.is_empty() {
            if let Some(message) =
                self.m68k_instruction_hunk_fixup_error(m68k_operands.0.as_slice())
            {
                self.mark_current_section_hunk_fixup_error(&message);
            }
            return Ok(false);
        }
        let base_offset = match u32::try_from(self.bytes.len()) {
            Ok(offset) => offset,
            Err(_) => {
                return Err(self.failure(
                    LineStatus::Error,
                    AsmErrorKind::Instruction,
                    "instruction relocation base exceeds supported range",
                    None,
                ))
            }
        };
        self.mark_current_section_hunk_relocatable();
        for fixup in fixups {
            let Some(end) = usize::try_from(fixup.offset)
                .ok()
                .and_then(|start| start.checked_add(4))
            else {
                return Err(self.failure(
                    LineStatus::Error,
                    AsmErrorKind::Instruction,
                    "instruction relocation offset exceeds supported range",
                    None,
                ));
            };
            bytes[end - 4..end].copy_from_slice(&fixup.encoded_value.to_be_bytes());
            let Some(offset) = base_offset.checked_add(fixup.offset) else {
                return Err(self.failure(
                    LineStatus::Error,
                    AsmErrorKind::Instruction,
                    "instruction relocation offset exceeds supported range",
                    None,
                ));
            };
            let Some(output_fixup) =
                self.hunk_abs32_output_fixup(offset, fixup.encoded_value, fixup.target_section)
            else {
                return Err(self.failure(
                    LineStatus::Error,
                    AsmErrorKind::Instruction,
                    "instruction relocation requires an active section",
                    None,
                ));
            };
            self.pending_output_fixups.push(output_fixup);
        }
        Ok(true)
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_explicit_long_notation_required(
        &self,
        mnemonic: &str,
        operands: &[M68KFamilyOperand],
    ) -> Option<Span> {
        let upper = mnemonic.to_ascii_uppercase();
        match upper.as_str() {
            "LEA" | "PEA" | "JMP" | "JSR" => match operands.first() {
                Some(M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }) if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                _ => None,
            },
            "MOVEA.L" => match operands {
                [M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }, M68KFamilyOperand::AddressRegister { .. }]
                    if self.hunk_abs32_target_section_for_expr(expr).is_some() =>
                {
                    Some(*span)
                }
                _ => None,
            },
            "MOVE.L" => match operands {
                [M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }, M68KFamilyOperand::DataRegister { .. }]
                    if self.hunk_abs32_target_section_for_expr(expr).is_some() =>
                {
                    Some(*span)
                }
                [M68KFamilyOperand::DataRegister { .. }, M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }] if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                _ => None,
            },
            _ => None,
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_instruction_hunk_fixup_error(&self, operands: &[M68KFamilyOperand]) -> Option<String> {
        let mut saw_symbolic_fixup_candidate = false;
        for operand in operands {
            let (expr, explicit_long_required) = match operand {
                M68KFamilyOperand::Absolute { expr, size, .. } => (
                    expr,
                    matches!(size, families::m68k::operand::AbsoluteSize::Word),
                ),
                M68KFamilyOperand::Immediate { expr, .. } => (expr, false),
                _ => continue,
            };
            if self.hunk_abs32_target_section_for_expr(expr).is_some() {
                saw_symbolic_fixup_candidate = true;
                if explicit_long_required {
                    return Some(
                        "format=hunk requires explicit .L notation for this symbolic instruction form in v0.3"
                            .to_string(),
                    );
                }
                continue;
            }
            if !self.expr_is_relocation_free_symbolic_value(expr, false) {
                saw_symbolic_fixup_candidate = true;
            }
        }

        if saw_symbolic_fixup_candidate {
            Some("format=hunk does not support this symbolic instruction form in v0.3".to_string())
        } else {
            None
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_instruction_output_fixups(
        &self,
        operands: &[M68KFamilyOperand],
        bytes: &[u8],
    ) -> Result<Vec<InstructionOutputFixupPlan>, (AsmErrorKind, String, Span)> {
        let mut relocatable_operands = Vec::new();
        for (index, operand) in operands.iter().enumerate() {
            let (expr, span) = match operand {
                M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Long,
                    span,
                } => (expr, *span),
                M68KFamilyOperand::Immediate { expr, span } => (expr, *span),
                _ => continue,
            };
            let relocation = self.eval_hunk_abs32_relocation_value(expr).map_err(|err| {
                (
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message().to_string(),
                    err.span,
                )
            })?;
            if relocation.is_some() {
                relocatable_operands.push((index, expr, span));
            }
        }

        let [(index, expr, span)] = relocatable_operands.as_slice() else {
            return Ok(Vec::new());
        };

        let Some(offset) = (match index {
            0 => Some(2),
            1 => match operands.get(1) {
                Some(M68KFamilyOperand::Absolute {
                    size: families::m68k::operand::AbsoluteSize::Long,
                    ..
                }) => u32::try_from(bytes.len())
                    .ok()
                    .and_then(|length| length.checked_sub(4)),
                _ => None,
            },
            _ => None,
        }) else {
            return Ok(Vec::new());
        };

        self.single_instruction_abs32_fixup(expr, *span, offset, bytes)
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn single_instruction_abs32_fixup(
        &self,
        expr: &Expr,
        span: Span,
        offset: u32,
        bytes: &[u8],
    ) -> Result<Vec<InstructionOutputFixupPlan>, (AsmErrorKind, String, Span)> {
        let Some((encoded_value, target_section)) =
            self.eval_hunk_abs32_relocation_value(expr).map_err(|err| {
                (
                    ast_eval_error_kind_to_asm(err.error.kind()),
                    err.error.message().to_string(),
                    err.span,
                )
            })?
        else {
            return Ok(Vec::new());
        };
        let Some(end) = usize::try_from(offset)
            .ok()
            .and_then(|start| start.checked_add(4))
        else {
            return Err((
                AsmErrorKind::Instruction,
                "instruction relocation offset exceeds supported range".to_string(),
                span,
            ));
        };
        if end > bytes.len() {
            return Err((
                AsmErrorKind::Instruction,
                "instruction bytes are too short for a 32-bit relocation".to_string(),
                span,
            ));
        }
        Ok(vec![InstructionOutputFixupPlan {
            offset,
            target_section,
            encoded_value,
        }])
    }

    #[cfg(feature = "vm-runtime-only")]
    fn try_encode_instruction_vm_only(
        &mut self,
        mnemonic: &str,
        operands: &[Expr],
    ) -> Option<LineStatus> {
        let model = self.opthread_execution_model.as_ref()?;
        match vm::vm_opasm::encode_instruction_from_exprs(
            model,
            self.cpu.as_str(),
            None,
            mnemonic,
            operands,
            self,
        ) {
            Ok(Some(bytes)) => {
                if bytes.is_empty() {
                    return Some(self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Instruction,
                        &format!(
                            "VM runtime emitted no bytes for {}",
                            mnemonic.to_ascii_uppercase()
                        ),
                        None,
                    ));
                }
                if let Err(err) =
                    self.validate_instruction_emit_span(mnemonic, operands, bytes.len())
                {
                    return Some(self.failure_at_span(
                        LineStatus::Error,
                        AsmErrorKind::Instruction,
                        err.error.message(),
                        None,
                        err.span,
                    ));
                }
                self.bytes.extend_from_slice(&bytes);
                Some(LineStatus::Ok)
            }
            Ok(None) => Some(self.failure(
                LineStatus::Error,
                AsmErrorKind::Instruction,
                &format!(
                    "instruction not found for CPU '{}' in VM runtime: {}",
                    self.cpu.as_str(),
                    mnemonic
                ),
                None,
            )),
            Err(err) => Some(self.failure(
                LineStatus::Error,
                AsmErrorKind::Instruction,
                &err.to_string(),
                None,
            )),
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn try_encode_instruction_via_runtime_expr(
        &mut self,
        pipeline: &ResolvedPipeline<'_>,
        mnemonic: &str,
        operands: &[Expr],
        family_operands: &dyn FamilyOperandSet,
        mapped_mnemonic: &str,
        mapped_operands: &dyn FamilyOperandSet,
    ) -> Option<LineStatus> {
        let vm_instruction_runtime_supported_for_cpu =
            !self.cpu.as_str().eq_ignore_ascii_case("45gs02");
        let family_runtime_authoritative =
            vm::rollout::package_runtime_default_enabled_for_family(pipeline.family_id.as_str())
                && vm_instruction_runtime_supported_for_cpu;

        let allow = match self.opthread_form_allows_mnemonic(pipeline, mapped_mnemonic) {
            Ok(allow) => allow,
            Err(message) => {
                return Some(self.failure(
                    LineStatus::Error,
                    AsmErrorKind::Instruction,
                    &message,
                    None,
                ))
            }
        };
        if !allow {
            return Some(self.failure_instruction_not_found(
                LineStatus::Error,
                pipeline,
                mnemonic,
                family_operands,
            ));
        }

        if self.opthread_execution_model.is_none() && family_runtime_authoritative {
            return Some(self.failure(
                LineStatus::Error,
                AsmErrorKind::Instruction,
                &format!(
                    "VM runtime model unavailable for authoritative family '{}'",
                    pipeline.family_id.as_str()
                ),
                None,
            ));
        }

        let model = self.opthread_execution_model.as_ref()?;

        let runtime_expr_force_host =
            self.portable_expr_runtime_force_host_for_family(pipeline.family_id.as_str());
        let strict_runtime_parse_resolve =
            vm::vm_opasm::expr_resolution_is_strict_for_family(model, pipeline.family_id.as_str())
                && vm_instruction_runtime_supported_for_cpu;
        let runtime_expr_bytes_authoritative = (strict_runtime_parse_resolve
            || family_runtime_authoritative)
            && !runtime_expr_force_host;
        let runtime_expr_vm_path_enabled = runtime_expr_bytes_authoritative;
        let runtime_expr_selector_gate_only = runtime_expr_vm_path_enabled
            && vm::vm_opasm::selector_gate_only_expr_runtime_for_cpu(model, self.cpu.as_str());
        if !runtime_expr_vm_path_enabled {
            return None;
        }

        let runtime_expr_operands_storage =
            Self::opthread_runtime_expr_operands_from_mapped(mapped_operands);
        let runtime_expr_operands = runtime_expr_operands_storage.as_deref().unwrap_or(operands);
        match vm::vm_opasm::encode_instruction_from_exprs(
            model,
            self.cpu.as_str(),
            None,
            mapped_mnemonic,
            runtime_expr_operands,
            self,
        ) {
            Ok(Some(bytes)) => {
                if runtime_expr_selector_gate_only {
                    return None;
                }
                if bytes.is_empty() {
                    if family_runtime_authoritative {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            &format!(
                                "VM program emitted no bytes for {}",
                                mapped_mnemonic.to_ascii_uppercase()
                            ),
                            None,
                        ));
                    }
                    return None;
                }
                if !runtime_expr_bytes_authoritative {
                    return None;
                }

                if let Some(status) =
                    self.emit_instruction_bytes_checked(mapped_mnemonic, operands, bytes.as_slice())
                {
                    return Some(status);
                }
                if let Ok(resolved_operands) =
                    pipeline
                        .cpu
                        .resolve_operands(mnemonic, mapped_operands, self)
                {
                    self.apply_cpu_runtime_state_after_encode(
                        pipeline.cpu.as_ref(),
                        mapped_mnemonic,
                        resolved_operands.as_ref(),
                    );
                }
                Some(LineStatus::Ok)
            }
            Ok(None) => {
                let defer_to_native_diagnostics =
                    vm::vm_opasm::defer_native_diagnostics_on_expr_none(
                        model,
                        pipeline.family_id.as_str(),
                    );
                if strict_runtime_parse_resolve && !defer_to_native_diagnostics {
                    Some(self.failure_instruction_not_found(
                        LineStatus::Error,
                        pipeline,
                        mapped_mnemonic,
                        mapped_operands,
                    ))
                } else {
                    None
                }
            }
            Err(err) => {
                if runtime_expr_selector_gate_only {
                    None
                } else {
                    Some(self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Instruction,
                        &err.to_string(),
                        None,
                    ))
                }
            }
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn try_encode_instruction_via_runtime_operands(
        &mut self,
        pipeline: &ResolvedPipeline<'_>,
        mapped_mnemonic: &str,
        operands: &[Expr],
        resolved_operands: &dyn OperandSet,
    ) -> Option<LineStatus> {
        let model = self.opthread_execution_model.as_ref()?;

        let vm_instruction_runtime_supported_for_cpu =
            !self.cpu.as_str().eq_ignore_ascii_case("45gs02");
        let family_runtime_authoritative =
            vm::rollout::package_runtime_default_enabled_for_family(pipeline.family_id.as_str())
                && vm_instruction_runtime_supported_for_cpu;

        let strict_runtime_vm_programs = family_runtime_authoritative
            || (vm::vm_opasm::expr_resolution_is_strict_for_family(
                model,
                pipeline.family_id.as_str(),
            ) && vm_instruction_runtime_supported_for_cpu);
        match vm::vm_opasm::encode_instruction(
            model,
            self.cpu.as_str(),
            None,
            mapped_mnemonic,
            resolved_operands,
        ) {
            Ok(Some(bytes)) => {
                if bytes.is_empty() {
                    if family_runtime_authoritative {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            &format!(
                                "VM program emitted no bytes for {}",
                                mapped_mnemonic.to_ascii_uppercase()
                            ),
                            None,
                        ));
                    }
                    return None;
                }

                if let Some(status) =
                    self.emit_instruction_bytes_checked(mapped_mnemonic, operands, bytes.as_slice())
                {
                    return Some(status);
                }
                self.apply_cpu_runtime_state_after_encode(
                    pipeline.cpu.as_ref(),
                    mapped_mnemonic,
                    resolved_operands,
                );
                Some(LineStatus::Ok)
            }
            Ok(None) => {
                if strict_runtime_vm_programs {
                    Some(self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Instruction,
                        &format!(
                            "missing VM program for {}",
                            mapped_mnemonic.to_ascii_uppercase()
                        ),
                        None,
                    ))
                } else {
                    None
                }
            }
            Err(err) => Some(self.failure(
                LineStatus::Error,
                AsmErrorKind::Instruction,
                &err.to_string(),
                None,
            )),
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn emit_instruction_bytes_checked(
        &mut self,
        mapped_mnemonic: &str,
        operands: &[Expr],
        bytes: &[u8],
    ) -> Option<LineStatus> {
        if let Err(err) =
            self.validate_instruction_emit_span(mapped_mnemonic, operands, bytes.len())
        {
            return Some(self.failure_at_span(
                LineStatus::Error,
                AsmErrorKind::Instruction,
                err.error.message(),
                None,
                err.span,
            ));
        }
        self.bytes.extend_from_slice(bytes);
        None
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn failure_instruction_not_found(
        &mut self,
        status: LineStatus,
        pipeline: &ResolvedPipeline<'_>,
        mnemonic: &str,
        operands: &dyn FamilyOperandSet,
    ) -> LineStatus {
        let message = format!("No instruction found for {}", mnemonic.to_ascii_uppercase());
        if let Some((help, fixit)) =
            self.dialect_fixit_for_instruction_not_found(pipeline, mnemonic, operands)
        {
            let column = self
                .mnemonic_span_in_current_line(mnemonic)
                .map(|(start, _)| start)
                .or(self.line_end_span.map(|span| span.col_start));
            let status =
                self.set_failure_core(status, AsmErrorKind::Instruction, &message, None, column);
            self.diagnostics.last_error_help = Some(help);
            self.diagnostics.last_error_fixits = vec![fixit];
            return status;
        }

        self.failure(status, AsmErrorKind::Instruction, &message, None)
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn dialect_fixit_for_instruction_not_found(
        &self,
        pipeline: &ResolvedPipeline<'_>,
        mnemonic: &str,
        operands: &dyn FamilyOperandSet,
    ) -> Option<(String, Fixit)> {
        if !pipeline
            .family_id
            .as_str()
            .eq_ignore_ascii_case(INTEL8080_FAMILY_ID.as_str())
        {
            return None;
        }

        if pipeline.dialect_id.eq_ignore_ascii_case("zilog") {
            return None;
        }

        let intel_operands = operands
            .as_any()
            .downcast_ref::<Intel8080FamilyOperands>()?;
        let (canonical_mnemonic, _mapped_operands) =
            map_zilog_to_canonical(mnemonic, intel_operands.0.as_slice())?;

        if canonical_mnemonic.eq_ignore_ascii_case(mnemonic) {
            return None;
        }

        let (col_start, col_end) = self.mnemonic_span_in_current_line(mnemonic)?;
        let replacement = canonical_mnemonic.to_ascii_uppercase();
        let help = format!(
            "{} appears to use Z80 dialect under {} CPU mode; replace with Intel8080-family form '{}', or switch CPU/dialect",
            mnemonic.to_ascii_uppercase(),
            self.cpu.as_str(),
            replacement
        );

        Some((
            help,
            Fixit {
                file: None,
                line: self.current_line_num,
                col_start: Some(col_start),
                col_end: Some(col_end),
                replacement,
                applicability: "machine-applicable".to_string(),
            },
        ))
    }

    fn mnemonic_span_in_current_line(&self, mnemonic: &str) -> Option<(usize, usize)> {
        let source = self.current_source_line.as_ref()?;
        if mnemonic.is_empty() {
            return None;
        }

        let source_lower = source.to_ascii_lowercase();
        let needle = mnemonic.to_ascii_lowercase();
        let mut search_from = 0usize;

        while let Some(relative) = source_lower.get(search_from..)?.find(&needle) {
            let start = search_from + relative;
            let end = start + needle.len();

            let prev = source[..start].chars().next_back();
            let next = source[end..].chars().next();

            let left_ok = prev.is_none_or(|ch| !is_identifierish(ch));
            let right_ok = next.is_none_or(|ch| !is_identifierish(ch));
            if left_ok && right_ok {
                return Some((start + 1, end + 1));
            }

            search_from = end;
        }

        None
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    pub fn attach_dialect_fixit_hint_from_source_line(&mut self) {
        let Ok(pipeline) = Self::resolve_pipeline_for_cpu(self.registry, self.cpu) else {
            return;
        };

        if !pipeline
            .family_id
            .as_str()
            .eq_ignore_ascii_case(INTEL8080_FAMILY_ID.as_str())
            || pipeline.dialect_id.eq_ignore_ascii_case("zilog")
        {
            return;
        }

        let Some((mnemonic, col_start, col_end)) = self.statement_mnemonic_from_source_line()
        else {
            return;
        };
        let Some(suggestion) = canonical_suggestion_for_zilog_mnemonic(mnemonic.as_str()) else {
            return;
        };
        if suggestion.eq_ignore_ascii_case(mnemonic.as_str()) {
            return;
        }

        self.diagnostics.last_error_help = Some(format!(
            "{} appears to use Z80 dialect under {} CPU mode; consider Intel8080-family '{}' syntax, or switch CPU/dialect",
            mnemonic.to_ascii_uppercase(),
            self.cpu.as_str(),
            suggestion.to_ascii_uppercase()
        ));
        self.diagnostics.last_error_fixits.push(Fixit {
            file: None,
            line: self.current_line_num,
            col_start: Some(col_start),
            col_end: Some(col_end),
            replacement: suggestion.to_ascii_uppercase(),
            applicability: "maybe-incorrect".to_string(),
        });
    }

    fn statement_mnemonic_from_source_line(&self) -> Option<(String, usize, usize)> {
        let source = self.current_source_line.as_ref()?;
        let without_comment = source.split(';').next().unwrap_or("");
        let bytes = without_comment.as_bytes();
        let mut idx = 0usize;

        while idx < bytes.len() && bytes[idx].is_ascii_whitespace() {
            idx += 1;
        }
        if idx >= bytes.len() {
            return None;
        }

        let mut probe = idx;
        while probe < bytes.len() && is_identifierish(bytes[probe] as char) {
            probe += 1;
        }
        if probe < bytes.len() && bytes[probe] == b':' {
            idx = probe + 1;
            while idx < bytes.len() && bytes[idx].is_ascii_whitespace() {
                idx += 1;
            }
        }

        let start = idx;
        while idx < bytes.len() && is_identifierish(bytes[idx] as char) {
            idx += 1;
        }
        if idx == start {
            return None;
        }

        let mnemonic = without_comment[start..idx].trim();
        if mnemonic.is_empty() {
            return None;
        }

        Some((mnemonic.to_string(), start + 1, idx + 1))
    }

    pub fn failure_for_unknown_directive_with_fixit(
        &mut self,
        mnemonic: &str,
    ) -> Option<LineStatus> {
        let suggestion = match mnemonic.to_ascii_uppercase().as_str() {
            ".EDIF" | ".ENDFI" | ".ENIDF" => ".ENDIF",
            ".ESLEIF" | ".ELSIEF" | ".ELSIF" | ".ELIF" | ".ELSFI" | ".ELSEFI" => ".ELSEIF",
            ".ENDMOD" | ".ENDMODUL" | ".ENDMODLE" | ".ENDMODUEL" => ".ENDMODULE",
            ".ENDSECT" | ".ENDSECTON" | ".ENDSEC" | ".ENDSECTIO" => ".ENDSECTION",
            ".ENDMACH" | ".ENDMTACH" | ".ENDMATC" => ".ENDMATCH",
            _ => return None,
        };

        let (col_start, col_end) = self
            .mnemonic_span_in_current_line(mnemonic)
            .or_else(|| {
                self.statement_mnemonic_from_source_line()
                    .map(|(_, start, end)| (start, end))
            })
            .unwrap_or((1, 1));

        let status = self.set_failure_core(
            LineStatus::Error,
            AsmErrorKind::Directive,
            &format!("Unknown directive {}", mnemonic.to_ascii_uppercase()),
            None,
            Some(col_start),
        );
        self.diagnostics.last_error_help =
            Some(format!("did you mean {}?", suggestion.to_ascii_lowercase()));
        self.diagnostics.last_error_fixits = vec![Fixit {
            file: None,
            line: self.current_line_num,
            col_start: Some(col_start),
            col_end: Some(col_end),
            replacement: suggestion.to_string(),
            applicability: "machine-applicable".to_string(),
        }];

        Some(status)
    }
}
