// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Instruction processing for `AsmLine`.
//!
//! Handles instruction encoding, VM runtime dispatch, dialect‐fixit
//! hints, and "instruction not found" diagnostics.

use super::*;

use crate::prepared_line::PreparedLine;

#[cfg(not(feature = "vm-runtime-only"))]
use crate::prepared_line::PreparedInstructionRoute;

#[cfg(not(feature = "vm-runtime-only"))]
struct BoundInstructionRoute<'a, 'pipeline> {
    pipeline: &'a ResolvedPipeline<'pipeline>,
    mnemonic: &'a str,
    operands: &'a [Expr],
    effective_operands: &'a [Expr],
    family_operands: &'a dyn FamilyOperandSet,
    mapped_mnemonic: &'a str,
    mapped_operands: &'a dyn FamilyOperandSet,
}

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

    pub(crate) fn process_instruction_ast(
        &mut self,
        mnemonic: &str,
        operands: &[Expr],
        #[cfg_attr(feature = "vm-runtime-only", allow(unused_variables))] prepared_line: Option<
            &PreparedLine,
        >,
    ) -> LineStatus {
        #[cfg(feature = "vm-runtime-only")]
        {
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
            if let Some(status) =
                self.try_encode_instruction_via_package_before_family_callbacks(mnemonic, operands)
            {
                return status;
            }

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

            if let Some(status) = self.try_encode_mos6502_instruction_via_runtime_expr_first(
                &pipeline, mnemonic, operands,
            ) {
                return status;
            }

            if RuntimeParseCache::enabled() {
                if let Some(prepared_line) = prepared_line {
                    let route_ref = prepared_line.instruction_route();
                    if let Some(route) = route_ref.as_ref() {
                        if route.cpu_id == self.cpu.as_str()
                            && route.mnemonic.eq_ignore_ascii_case(mnemonic)
                        {
                            let effective_operands =
                                route.rewritten_operands.as_deref().unwrap_or(operands);
                            crate::phase_profile::record_execution_path(
                                Some(self.line_route_bucket()),
                                "prepared.instruction_route_cache_hit",
                                std::time::Duration::ZERO,
                            );
                            return self.process_bound_instruction_route(BoundInstructionRoute {
                                pipeline: &pipeline,
                                mnemonic,
                                operands,
                                effective_operands,
                                family_operands: route.family_operands.as_ref(),
                                mapped_mnemonic: &route.mapped_mnemonic,
                                mapped_operands: route.mapped_operands.as_ref(),
                            });
                        }
                    }
                }
            }

            let mut rewritten_operands = None;
            let family_operands = match pipeline.family.parse_operands(mnemonic, operands) {
                Ok(ops) => {
                    if pipeline.family_id == M68K_FAMILY_ID {
                        if let Some(rewritten) = self
                            .m68k_canonicalize_supported_bare_symbol_operands(mnemonic, operands)
                        {
                            match pipeline
                                .family
                                .parse_operands(mnemonic, rewritten.as_slice())
                            {
                                Ok(rewritten_ops) => {
                                    rewritten_operands = Some(rewritten);
                                    rewritten_ops
                                }
                                Err(_) => ops,
                            }
                        } else {
                            ops
                        }
                    } else {
                        ops
                    }
                }
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

            if RuntimeParseCache::enabled() {
                if let Some(prepared_line) = prepared_line {
                    prepared_line.store_instruction_route(PreparedInstructionRoute {
                        cpu_id: self.cpu.as_str().to_string(),
                        mnemonic: mnemonic.to_string(),
                        rewritten_operands,
                        family_operands,
                        mapped_mnemonic,
                        mapped_operands,
                    });
                    let route_ref = prepared_line.instruction_route();
                    let route = route_ref
                        .as_ref()
                        .expect("prepared instruction route was just stored");
                    let effective_operands =
                        route.rewritten_operands.as_deref().unwrap_or(operands);
                    return self.process_bound_instruction_route(BoundInstructionRoute {
                        pipeline: &pipeline,
                        mnemonic,
                        operands,
                        effective_operands,
                        family_operands: route.family_operands.as_ref(),
                        mapped_mnemonic: &route.mapped_mnemonic,
                        mapped_operands: route.mapped_operands.as_ref(),
                    });
                }
            }

            self.process_bound_instruction_route(BoundInstructionRoute {
                pipeline: &pipeline,
                mnemonic,
                operands,
                effective_operands,
                family_operands: family_operands.as_ref(),
                mapped_mnemonic: &mapped_mnemonic,
                mapped_operands: mapped_operands.as_ref(),
            })
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn try_encode_instruction_via_package_before_family_callbacks(
        &mut self,
        mnemonic: &str,
        operands: &[Expr],
    ) -> Option<LineStatus> {
        let model = self.opthread_execution_model.as_ref()?;
        let resolved = model.resolve_pipeline(self.cpu.as_str(), None).ok()?;
        if !vm::rollout::package_runtime_pre_callback_enabled_for_family(&resolved.family_id) {
            return None;
        }

        match vm::vm_opasm::encode_instruction_from_exprs_with_effects(
            model,
            self.cpu.as_str(),
            None,
            mnemonic,
            operands,
            self,
        ) {
            Ok(Some((bytes, effects))) => {
                if bytes.is_empty() {
                    return Some(self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Instruction,
                        &format!(
                            "VM program emitted no bytes for {}",
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
                let instruction_offset = match u32::try_from(self.bytes.len()) {
                    Ok(offset) => offset,
                    Err(_) => {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "instruction relocation base exceeds supported range",
                            None,
                        ))
                    }
                };
                let has_output_fixups = !effects.output_fixups.is_empty();
                for fixup in effects.output_fixups {
                    if fixup.width != 4
                        || fixup.kind != vm::fixup_vm::PortableOutputFixupKind::Absolute
                    {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "VM runtime emitted an unsupported output fixup",
                            None,
                        ));
                    }
                    let Some(offset) = instruction_offset.checked_add(fixup.offset) else {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "instruction relocation offset exceeds supported range",
                            None,
                        ));
                    };
                    let Some(output_fixup) =
                        self.hunk_abs32_output_fixup(offset, fixup.encoded_addend, fixup.target)
                    else {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "instruction relocation requires an active section",
                            None,
                        ));
                    };
                    self.mark_current_section_hunk_relocatable();
                    self.pending_output_fixups.push(output_fixup);
                }
                self.bytes.extend_from_slice(&bytes);
                if self.in_section()
                    && !effects.relocation_free
                    && !has_output_fixups
                    && operands
                        .iter()
                        .any(|expr| self.instruction_expr_references_target(expr))
                {
                    self.mark_current_section_hunk_fixup_error(&format!(
                        "format=hunk does not support this symbolic instruction form in v0.3: instruction {mnemonic} with operands {operands:?} references a relocatable symbol but its package encoding emitted no output fixup"
                    ));
                }
                Some(LineStatus::Ok)
            }
            Ok(None) => None,
            Err(err) => Some(if let Some(operand) = operands.last() {
                self.failure_at_span(
                    LineStatus::Error,
                    AsmErrorKind::Instruction,
                    &err.to_string(),
                    None,
                    expr_span(operand),
                )
            } else {
                self.failure(
                    LineStatus::Error,
                    AsmErrorKind::Instruction,
                    &err.to_string(),
                    None,
                )
            }),
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn process_bound_instruction_route(
        &mut self,
        route: BoundInstructionRoute<'_, '_>,
    ) -> LineStatus {
        let BoundInstructionRoute {
            pipeline,
            mnemonic,
            operands,
            effective_operands,
            family_operands,
            mapped_mnemonic,
            mapped_operands,
        } = route;

        if let Some(status) = self.try_encode_instruction_via_runtime_expr(
            pipeline,
            mnemonic,
            effective_operands,
            family_operands,
            mapped_mnemonic,
            mapped_operands,
        ) {
            return status;
        }
        let rust_encode_started = std::time::Instant::now();
        match pipeline.family.encode_family_operands(
            mapped_mnemonic,
            mnemonic,
            mapped_operands,
            self,
        ) {
            registry::family::FamilyEncodeResult::Ok(bytes) => {
                let rust_encode_elapsed = rust_encode_started.elapsed();
                let bucket = self.line_route_bucket();
                crate::phase_profile::record_execution_path(
                    Some(bucket),
                    "rust.encode",
                    rust_encode_elapsed,
                );
                if let Err(err) = self.validate_instruction_emit_span(
                    mapped_mnemonic,
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
                        mapped_operands,
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
                let rust_encode_elapsed = rust_encode_started.elapsed();
                let bucket = self.line_route_bucket();
                crate::phase_profile::record_execution_path(
                    Some(bucket),
                    "rust.encode",
                    rust_encode_elapsed,
                );
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
            registry::family::FamilyEncodeResult::NotFound => {
                let rust_encode_elapsed = rust_encode_started.elapsed();
                let bucket = self.line_route_bucket();
                crate::phase_profile::record_execution_path(
                    Some(bucket),
                    "rust.encode.notfound",
                    rust_encode_elapsed,
                );
            }
        }

        let resolved_operands = match pipeline
            .cpu
            .resolve_operands(mnemonic, mapped_operands, self)
        {
            Ok(ops) => ops,
            Err(err) => {
                return self.failure(LineStatus::Error, AsmErrorKind::Instruction, &err, None)
            }
        };

        if let Some(validator) = pipeline.validator.as_ref() {
            if let Err(err) =
                validator.validate_instruction(mapped_mnemonic, resolved_operands.as_ref(), self)
            {
                return self.failure(LineStatus::Error, AsmErrorKind::Instruction, &err, None);
            }
        }

        if let Some(status) = self.try_encode_instruction_via_runtime_operands(
            pipeline,
            mapped_mnemonic,
            effective_operands,
            resolved_operands.as_ref(),
        ) {
            return status;
        }

        match pipeline
            .family
            .encode_instruction(mapped_mnemonic, resolved_operands.as_ref(), self)
            .into_outcome()
        {
            Ok(Some(mut bytes)) => {
                if let Err(err) = self.validate_instruction_emit_span(
                    mapped_mnemonic,
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
                        mapped_operands,
                    )
                {
                    self.mark_current_section_not_relocation_free();
                }
                self.bytes.extend_from_slice(&bytes);
                self.apply_cpu_runtime_state_after_encode(
                    pipeline.cpu.as_ref(),
                    mapped_mnemonic,
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
                .encode_instruction(mapped_mnemonic, resolved_operands.as_ref(), self)
                .into_outcome()
            {
                Ok(Some(mut bytes)) => {
                    if let Err(err) =
                        self.validate_instruction_emit_span(mapped_mnemonic, operands, bytes.len())
                    {
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
                            mapped_operands,
                        )
                    {
                        self.mark_current_section_not_relocation_free();
                    }
                    self.bytes.extend_from_slice(&bytes);
                    self.apply_cpu_runtime_state_after_encode(
                        pipeline.cpu.as_ref(),
                        mapped_mnemonic,
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
                    pipeline,
                    mnemonic,
                    family_operands,
                ),
            },
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn try_encode_mos6502_instruction_via_runtime_expr_first(
        &mut self,
        pipeline: &ResolvedPipeline<'_>,
        mnemonic: &str,
        operands: &[Expr],
    ) -> Option<LineStatus> {
        if pipeline.family_id != MOS6502_FAMILY_ID
            || self.cpu.as_str().eq_ignore_ascii_case("45gs02")
            || self.cpu.as_str().eq_ignore_ascii_case("65816")
            || !vm::rollout::package_runtime_default_enabled_for_family(pipeline.family_id.as_str())
            || self.portable_expr_runtime_force_host_for_family(pipeline.family_id.as_str())
        {
            return None;
        }

        let allow = match self.opthread_form_allows_mnemonic(pipeline, mnemonic) {
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
            return Some(self.failure(
                LineStatus::Error,
                AsmErrorKind::Instruction,
                &format!("No instruction found for {}", mnemonic.to_ascii_uppercase()),
                None,
            ));
        }

        let Some(model) = self.opthread_execution_model.as_ref() else {
            return Some(self.failure(
                LineStatus::Error,
                AsmErrorKind::Instruction,
                &format!(
                    "VM runtime model unavailable for authoritative family '{}'",
                    pipeline.family_id.as_str()
                ),
                None,
            ));
        };

        let vm_start = std::time::Instant::now();
        let vm_res = vm::vm_opasm::encode_instruction_from_exprs(
            model,
            self.cpu.as_str(),
            None,
            mnemonic,
            operands,
            self,
        );
        let vm_elapsed = vm_start.elapsed();
        let bucket = self.line_route_bucket();
        crate::phase_profile::record_execution_path(Some(bucket), "vm.encode", vm_elapsed);

        match vm_res {
            Ok(Some(bytes)) => {
                if bytes.is_empty() {
                    return Some(self.failure(
                        LineStatus::Error,
                        AsmErrorKind::Instruction,
                        &format!(
                            "VM program emitted no bytes for {}",
                            mnemonic.to_ascii_uppercase()
                        ),
                        None,
                    ));
                }
                if let Some(status) =
                    self.emit_instruction_bytes_checked(mnemonic, operands, bytes.as_slice())
                {
                    return Some(status);
                }
                Some(LineStatus::Ok)
            }
            Ok(None) => Some(self.failure(
                LineStatus::Error,
                AsmErrorKind::Instruction,
                &format!(
                    "VM runtime selector missing for {}",
                    mnemonic.to_ascii_uppercase()
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
            _ if Self::m68k_is_supported_special_move_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [src, dst]
                        if Self::expr_is_sr_or_ccr_candidate(src)
                            && Self::expr_is_bare_symbol_candidate(dst) =>
                    {
                        Some(expr_span(dst))
                    }
                    [src, dst]
                        if Self::expr_is_bare_symbol_candidate(src)
                            && Self::expr_is_sr_or_ccr_candidate(dst) =>
                    {
                        Some(expr_span(src))
                    }
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_chk2_cmp2_bare_symbol_mnemonic(&upper) => match operands {
                [src, dst]
                    if Self::expr_is_bare_symbol_candidate(src)
                        && Self::expr_is_general_register_candidate(dst) =>
                {
                    Some(expr_span(src))
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_cas_bare_symbol_mnemonic(&upper) => match operands {
                [compare, update, destination]
                    if Self::expr_is_data_register_candidate(compare)
                        && Self::expr_is_data_register_candidate(update)
                        && Self::expr_is_bare_symbol_candidate(destination) =>
                {
                    Some(expr_span(destination))
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_callm_bare_symbol_mnemonic(&upper) => match operands {
                [count, destination]
                    if Self::expr_is_immediate_candidate(count)
                        && Self::expr_is_bare_symbol_candidate(destination) =>
                {
                    Some(expr_span(destination))
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_bitfield_bare_symbol_mnemonic(&upper) => match operands {
                [operand] if Self::expr_is_bitfield_bare_symbol_candidate(operand) => {
                    Some(expr_span(operand))
                }
                [src, dst]
                    if Self::expr_is_bitfield_bare_symbol_candidate(src)
                        && Self::expr_is_data_register_candidate(dst) =>
                {
                    Some(expr_span(src))
                }
                [src, dst]
                    if Self::expr_is_data_register_candidate(src)
                        && Self::expr_is_bitfield_bare_symbol_candidate(dst) =>
                {
                    Some(expr_span(dst))
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_moves_bare_symbol_mnemonic(&upper) => match operands {
                [src, dst]
                    if Self::expr_is_general_register_candidate(src)
                        && Self::expr_is_bare_symbol_candidate(dst) =>
                {
                    Some(expr_span(dst))
                }
                [src, dst]
                    if Self::expr_is_bare_symbol_candidate(src)
                        && Self::expr_is_general_register_candidate(dst) =>
                {
                    Some(expr_span(src))
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_movem_bare_symbol_mnemonic(&upper) => match operands {
                [src, dst]
                    if Self::expr_is_integer_register_list_candidate(src)
                        && Self::expr_is_bare_symbol_candidate(dst) =>
                {
                    Some(expr_span(dst))
                }
                [src, dst]
                    if Self::expr_is_bare_symbol_candidate(src)
                        && Self::expr_is_integer_register_list_candidate(dst) =>
                {
                    Some(expr_span(src))
                }
                _ => None,
            },
            "MOVEA.L" => operands
                .first()
                .filter(|expr| Self::expr_is_bare_symbol_candidate(expr))
                .map(expr_span),
            _ if Self::m68k_is_supported_wordmath_src_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [src, dst]
                        if Self::expr_is_bare_symbol_candidate(src)
                            && Self::expr_is_data_register_candidate(dst) =>
                    {
                        Some(expr_span(src))
                    }
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_addrreg_binary_src_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [src, dst]
                        if Self::expr_is_bare_symbol_candidate(src)
                            && Self::expr_is_address_register_candidate(dst) =>
                    {
                        Some(expr_span(src))
                    }
                    _ => None,
                }
            }
            "MOVE.B" | "MOVE.W" | "MOVE.L" => operands
                .iter()
                .find(|expr| Self::expr_is_bare_symbol_candidate(expr))
                .map(expr_span),
            _ if Self::m68k_is_supported_datareg_binary_src_bare_symbol_mnemonic(&upper)
                || Self::m68k_is_supported_datareg_binary_dst_bare_symbol_mnemonic(&upper) =>
            {
                match operands {
                    [src, dst]
                        if Self::expr_is_bare_symbol_candidate(src)
                            && Self::expr_is_data_register_candidate(dst) =>
                    {
                        Some(expr_span(src))
                    }
                    [src, dst]
                        if Self::expr_is_data_register_candidate(src)
                            && Self::expr_is_bare_symbol_candidate(dst) =>
                    {
                        Some(expr_span(dst))
                    }
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_immediate_binary_dst_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [_, dst] if Self::expr_is_bare_symbol_candidate(dst) => Some(expr_span(dst)),
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_bitop_dst_bare_symbol_mnemonic(&upper) => match operands {
                [bitnum, dst]
                    if (Self::expr_is_immediate_candidate(bitnum)
                        || Self::expr_is_data_register_candidate(bitnum))
                        && Self::expr_is_bare_symbol_candidate(dst) =>
                {
                    Some(expr_span(dst))
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_scc_bare_symbol_mnemonic(&upper) => operands
                .first()
                .filter(|expr| Self::expr_is_bare_symbol_candidate(expr))
                .map(expr_span),
            _ if Self::m68k_is_supported_memory_shift_bare_symbol_mnemonic(&upper) => operands
                .first()
                .filter(|expr| Self::expr_is_bare_symbol_candidate(expr))
                .map(expr_span),
            _ if Self::m68k_is_supported_unary_bare_symbol_mnemonic(&upper) => operands
                .first()
                .filter(|expr| Self::expr_is_bare_symbol_candidate(expr))
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
            _ if Self::m68k_is_supported_special_move_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [src, dst]
                        if Self::expr_is_sr_or_ccr_candidate(src)
                            && Self::expr_is_bare_symbol_candidate(dst) =>
                    {
                        Some(vec![
                            src.clone(),
                            Self::wrap_expr_with_absolute_long_suffix(dst),
                        ])
                    }
                    [src, dst]
                        if Self::expr_is_bare_symbol_candidate(src)
                            && Self::expr_is_sr_or_ccr_candidate(dst) =>
                    {
                        Some(vec![
                            Self::wrap_expr_with_absolute_long_suffix(src),
                            dst.clone(),
                        ])
                    }
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_chk2_cmp2_bare_symbol_mnemonic(&upper) => match operands {
                [src, dst]
                    if Self::expr_is_bare_symbol_candidate(src)
                        && Self::expr_is_general_register_candidate(dst) =>
                {
                    Some(vec![
                        Self::wrap_expr_with_absolute_long_suffix(src),
                        dst.clone(),
                    ])
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_cas_bare_symbol_mnemonic(&upper) => match operands {
                [compare, update, destination]
                    if Self::expr_is_data_register_candidate(compare)
                        && Self::expr_is_data_register_candidate(update)
                        && Self::expr_is_bare_symbol_candidate(destination) =>
                {
                    Some(vec![
                        compare.clone(),
                        update.clone(),
                        Self::wrap_expr_with_absolute_long_suffix(destination),
                    ])
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_callm_bare_symbol_mnemonic(&upper) => match operands {
                [count, destination]
                    if Self::expr_is_immediate_candidate(count)
                        && Self::expr_is_bare_symbol_candidate(destination) =>
                {
                    Some(vec![
                        count.clone(),
                        Self::wrap_expr_with_absolute_long_suffix(destination),
                    ])
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_bitfield_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [operand] if Self::expr_is_bitfield_bare_symbol_candidate(operand) => Some(
                        vec![Self::wrap_bitfield_base_with_absolute_long_suffix(operand)],
                    ),
                    [src, dst]
                        if Self::expr_is_bitfield_bare_symbol_candidate(src)
                            && Self::expr_is_data_register_candidate(dst) =>
                    {
                        Some(vec![
                            Self::wrap_bitfield_base_with_absolute_long_suffix(src),
                            dst.clone(),
                        ])
                    }
                    [src, dst]
                        if Self::expr_is_data_register_candidate(src)
                            && Self::expr_is_bitfield_bare_symbol_candidate(dst) =>
                    {
                        Some(vec![
                            src.clone(),
                            Self::wrap_bitfield_base_with_absolute_long_suffix(dst),
                        ])
                    }
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_moves_bare_symbol_mnemonic(&upper) => match operands {
                [src, dst]
                    if Self::expr_is_general_register_candidate(src)
                        && Self::expr_is_bare_symbol_candidate(dst) =>
                {
                    Some(vec![
                        src.clone(),
                        Self::wrap_expr_with_absolute_long_suffix(dst),
                    ])
                }
                [src, dst]
                    if Self::expr_is_bare_symbol_candidate(src)
                        && Self::expr_is_general_register_candidate(dst) =>
                {
                    Some(vec![
                        Self::wrap_expr_with_absolute_long_suffix(src),
                        dst.clone(),
                    ])
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_movem_bare_symbol_mnemonic(&upper) => match operands {
                [src, dst]
                    if Self::expr_is_integer_register_list_candidate(src)
                        && Self::expr_is_bare_symbol_candidate(dst) =>
                {
                    Some(vec![
                        src.clone(),
                        Self::wrap_expr_with_absolute_long_suffix(dst),
                    ])
                }
                [src, dst]
                    if Self::expr_is_bare_symbol_candidate(src)
                        && Self::expr_is_integer_register_list_candidate(dst) =>
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
            "MOVE.B" | "MOVE.W" | "MOVE.L" => match operands {
                [src, dst]
                    if Self::expr_is_bare_symbol_candidate(src)
                        && !Self::expr_is_bare_symbol_candidate(dst) =>
                {
                    Some(vec![
                        Self::wrap_expr_with_absolute_long_suffix(src),
                        dst.clone(),
                    ])
                }
                [src, dst]
                    if !Self::expr_is_bare_symbol_candidate(src)
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
            _ if Self::m68k_is_supported_wordmath_src_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [src, dst]
                        if Self::expr_is_bare_symbol_candidate(src)
                            && Self::expr_is_data_register_candidate(dst) =>
                    {
                        Some(vec![
                            Self::wrap_expr_with_absolute_long_suffix(src),
                            dst.clone(),
                        ])
                    }
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_addrreg_binary_src_bare_symbol_mnemonic(&upper) => {
                match operands {
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
                }
            }
            _ if Self::m68k_is_supported_datareg_binary_src_bare_symbol_mnemonic(&upper)
                || Self::m68k_is_supported_datareg_binary_dst_bare_symbol_mnemonic(&upper) =>
            {
                match operands {
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
                }
            }
            _ if Self::m68k_is_supported_immediate_binary_dst_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [src, dst] if Self::expr_is_bare_symbol_candidate(dst) => Some(vec![
                        src.clone(),
                        Self::wrap_expr_with_absolute_long_suffix(dst),
                    ]),
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_bitop_dst_bare_symbol_mnemonic(&upper) => match operands {
                [bitnum, dst]
                    if (Self::expr_is_immediate_candidate(bitnum)
                        || Self::expr_is_data_register_candidate(bitnum))
                        && Self::expr_is_bare_symbol_candidate(dst) =>
                {
                    Some(vec![
                        bitnum.clone(),
                        Self::wrap_expr_with_absolute_long_suffix(dst),
                    ])
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_scc_bare_symbol_mnemonic(&upper) => match operands {
                [expr] if Self::expr_is_bare_symbol_candidate(expr) => {
                    Some(vec![Self::wrap_expr_with_absolute_long_suffix(expr)])
                }
                _ => None,
            },
            _ if Self::m68k_is_supported_memory_shift_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [expr] if Self::expr_is_bare_symbol_candidate(expr) => {
                        Some(vec![Self::wrap_expr_with_absolute_long_suffix(expr)])
                    }
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_unary_bare_symbol_mnemonic(&upper) => match operands {
                [expr] if Self::expr_is_bare_symbol_candidate(expr) => {
                    Some(vec![Self::wrap_expr_with_absolute_long_suffix(expr)])
                }
                _ => None,
            },
            _ => None,
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_unary_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(upper, "NBCD" | "TAS")
            || matches!(
                upper.split_once('.'),
                Some((base, size))
                    if matches!(base, "CLR" | "NEGX" | "NEG" | "NOT" | "TST")
                        && matches!(size, "B" | "W" | "L")
            )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_special_move_bare_symbol_mnemonic(upper: &str) -> bool {
        upper == "MOVE"
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_chk2_cmp2_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(
            upper.split_once('.'),
            Some((base, size))
                if (base == "CHK2" && matches!(size, "W" | "L"))
                    || (base == "CMP2" && matches!(size, "B" | "W" | "L"))
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_cas_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(
            upper.split_once('.'),
            Some((base, size)) if base == "CAS" && matches!(size, "B" | "W" | "L")
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_callm_bare_symbol_mnemonic(upper: &str) -> bool {
        upper == "CALLM"
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_bitfield_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(
            upper,
            "BFTST" | "BFEXTU" | "BFEXTS" | "BFFFO" | "BFCHG" | "BFCLR" | "BFSET" | "BFINS"
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_movem_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(upper, "MOVEM.W" | "MOVEM.L")
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_moves_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(
            upper.split_once('.'),
            Some((base, size)) if base == "MOVES" && matches!(size, "B" | "W" | "L")
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_datareg_binary_src_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(
            upper.split_once('.'),
            Some((base, size))
                if matches!(base, "ADD" | "SUB" | "AND" | "OR" | "CMP")
                    && matches!(size, "B" | "W" | "L")
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_wordmath_src_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(upper, "CHK" | "MULU" | "MULS" | "DIVU" | "DIVS")
            || matches!(
                upper.split_once('.'),
                Some((base, size))
                    if matches!(base, "CHK" | "MULU" | "MULS" | "DIVU" | "DIVS")
                        && matches!(size, "W" | "L")
            )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_addrreg_binary_src_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(
            upper.split_once('.'),
            Some((base, size))
                if matches!(base, "ADDA" | "SUBA" | "CMPA") && matches!(size, "W" | "L")
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_datareg_binary_dst_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(
            upper.split_once('.'),
            Some((base, size))
                if matches!(base, "ADD" | "SUB" | "AND" | "OR" | "EOR")
                    && matches!(size, "B" | "W" | "L")
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_immediate_binary_dst_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(
            upper.split_once('.'),
            Some((base, size))
                if matches!(base, "ORI" | "ANDI" | "SUBI" | "ADDI" | "EORI" | "CMPI")
                    && matches!(size, "B" | "W" | "L")
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_bitop_dst_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(upper, "BTST" | "BCHG" | "BCLR" | "BSET")
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_scc_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(
            upper,
            "ST" | "SF"
                | "SHI"
                | "SLS"
                | "SCC"
                | "SCS"
                | "SNE"
                | "SEQ"
                | "SVC"
                | "SVS"
                | "SPL"
                | "SMI"
                | "SGE"
                | "SLT"
                | "SGT"
                | "SLE"
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn m68k_is_supported_memory_shift_bare_symbol_mnemonic(upper: &str) -> bool {
        matches!(
            upper,
            "ASL" | "ASR" | "LSL" | "LSR" | "ROL" | "ROR" | "ROXL" | "ROXR"
        ) || matches!(
            upper.split_once('.'),
            Some((base, size))
                if matches!(base, "ASL" | "ASR" | "LSL" | "LSR" | "ROL" | "ROR" | "ROXL" | "ROXR")
                    && size == "W"
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn expr_is_bare_symbol_candidate(expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(name, _) => !name.contains('.'),
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
        match expr {
            Expr::Identifier(name, span) | Expr::Register(name, span) => {
                Expr::Identifier(format!("{name}.L"), *span)
            }
            _ => Expr::Member {
                base: Box::new(expr.clone()),
                field: "L".to_string(),
                span: expr_span(expr),
            },
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn expr_is_bitfield_bare_symbol_candidate(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Call { name, args, .. }
                if name == ".bitfield"
                    && args.len() == 3
                    && Self::expr_is_bare_symbol_candidate(&args[0])
        )
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn wrap_bitfield_base_with_absolute_long_suffix(expr: &Expr) -> Expr {
        match expr {
            Expr::Call { name, args, span } if name == ".bitfield" && args.len() == 3 => {
                Expr::Call {
                    name: name.clone(),
                    args: vec![
                        Self::wrap_expr_with_absolute_long_suffix(&args[0]),
                        args[1].clone(),
                        args[2].clone(),
                    ],
                    span: *span,
                }
            }
            _ => expr.clone(),
        }
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
    fn expr_is_general_register_candidate(expr: &Expr) -> bool {
        Self::expr_is_data_register_candidate(expr)
            || Self::expr_is_address_register_candidate(expr)
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn raw_register_name(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(name, _) | Expr::Register(name, _) => Some(name.to_ascii_uppercase()),
            _ => None,
        }
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn expr_is_immediate_candidate(expr: &Expr) -> bool {
        matches!(expr, Expr::Immediate(_, _))
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn expr_is_sr_or_ccr_candidate(expr: &Expr) -> bool {
        matches!(Self::raw_register_name(expr).as_deref(), Some("SR" | "CCR"))
    }

    #[cfg(not(feature = "vm-runtime-only"))]
    fn expr_is_integer_register_list_candidate(expr: &Expr) -> bool {
        if Self::expr_is_data_register_candidate(expr)
            || Self::expr_is_address_register_candidate(expr)
        {
            return true;
        }

        matches!(
            expr,
            Expr::Binary {
                op: BinaryOp::Divide | BinaryOp::Subtract,
                left,
                right,
                ..
            } if Self::expr_is_integer_register_list_candidate(left)
                && Self::expr_is_integer_register_list_candidate(right)
        )
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
            _ if Self::m68k_is_supported_special_move_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [M68KFamilyOperand::SpecialRegister {
                        register:
                            families::m68k::operand::SpecialRegisterKind::Sr
                            | families::m68k::operand::SpecialRegisterKind::Ccr,
                        ..
                    }, M68KFamilyOperand::Absolute {
                        expr,
                        size: families::m68k::operand::AbsoluteSize::Word,
                        span,
                    }] if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                    [M68KFamilyOperand::Absolute {
                        expr,
                        size: families::m68k::operand::AbsoluteSize::Word,
                        span,
                    }, M68KFamilyOperand::SpecialRegister {
                        register:
                            families::m68k::operand::SpecialRegisterKind::Sr
                            | families::m68k::operand::SpecialRegisterKind::Ccr,
                        ..
                    }] if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_chk2_cmp2_bare_symbol_mnemonic(&upper) => match operands {
                [M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }, M68KFamilyOperand::DataRegister { .. }]
                | [M68KFamilyOperand::Absolute {
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
            _ if Self::m68k_is_supported_cas_bare_symbol_mnemonic(&upper) => match operands {
                [M68KFamilyOperand::DataRegister { .. }, M68KFamilyOperand::DataRegister { .. }, M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }] if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                _ => None,
            },
            _ if Self::m68k_is_supported_callm_bare_symbol_mnemonic(&upper) => match operands {
                [M68KFamilyOperand::Immediate { .. }, M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }] if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                _ => None,
            },
            _ if Self::m68k_is_supported_bitfield_bare_symbol_mnemonic(&upper) => {
                match operands.iter().find_map(|operand| match operand {
                    M68KFamilyOperand::BitField { base, span, .. } => match base.as_ref() {
                        M68KFamilyOperand::Absolute {
                            expr,
                            size: families::m68k::operand::AbsoluteSize::Word,
                            ..
                        } if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                        _ => None,
                    },
                    _ => None,
                }) {
                    some_span @ Some(_) => some_span,
                    None => None,
                }
            }
            _ if Self::m68k_is_supported_moves_bare_symbol_mnemonic(&upper) => match operands {
                [M68KFamilyOperand::DataRegister { .. }, M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }]
                | [M68KFamilyOperand::AddressRegister { .. }, M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }]
                | [M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }, M68KFamilyOperand::DataRegister { .. }]
                | [M68KFamilyOperand::Absolute {
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
            _ if Self::m68k_is_supported_movem_bare_symbol_mnemonic(&upper) => match operands {
                [M68KFamilyOperand::RegisterList { .. }, M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }]
                | [M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }, M68KFamilyOperand::RegisterList { .. }]
                    if self.hunk_abs32_target_section_for_expr(expr).is_some() =>
                {
                    Some(*span)
                }
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
            _ if Self::m68k_is_supported_wordmath_src_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [M68KFamilyOperand::Absolute {
                        expr,
                        size: families::m68k::operand::AbsoluteSize::Word,
                        span,
                    }, M68KFamilyOperand::DataRegister { .. }]
                        if self.hunk_abs32_target_section_for_expr(expr).is_some() =>
                    {
                        Some(*span)
                    }
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_addrreg_binary_src_bare_symbol_mnemonic(&upper) => {
                match operands {
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
                }
            }
            "MOVE.B" | "MOVE.W" | "MOVE.L" => match operands {
                [M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }, _]
                    if self.hunk_abs32_target_section_for_expr(expr).is_some() =>
                {
                    Some(*span)
                }
                [_, M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }] if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                _ => None,
            },
            _ if Self::m68k_is_supported_datareg_binary_src_bare_symbol_mnemonic(&upper)
                || Self::m68k_is_supported_datareg_binary_dst_bare_symbol_mnemonic(&upper) =>
            {
                match operands {
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
                }
            }
            _ if Self::m68k_is_supported_immediate_binary_dst_bare_symbol_mnemonic(&upper) => {
                match operands {
                    [M68KFamilyOperand::Immediate { .. }, M68KFamilyOperand::Absolute {
                        expr,
                        size: families::m68k::operand::AbsoluteSize::Word,
                        span,
                    }] if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_bitop_dst_bare_symbol_mnemonic(&upper) => match operands {
                [M68KFamilyOperand::Immediate { .. }, M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }]
                | [M68KFamilyOperand::DataRegister { .. }, M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Word,
                    span,
                }] if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                _ => None,
            },
            _ if Self::m68k_is_supported_scc_bare_symbol_mnemonic(&upper) => {
                match operands.first() {
                    Some(M68KFamilyOperand::Absolute {
                        expr,
                        size: families::m68k::operand::AbsoluteSize::Word,
                        span,
                    }) if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_memory_shift_bare_symbol_mnemonic(&upper) => {
                match operands.first() {
                    Some(M68KFamilyOperand::Absolute {
                        expr,
                        size: families::m68k::operand::AbsoluteSize::Word,
                        span,
                    }) if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                    _ => None,
                }
            }
            _ if Self::m68k_is_supported_unary_bare_symbol_mnemonic(&upper) => {
                match operands.first() {
                    Some(M68KFamilyOperand::Absolute {
                        expr,
                        size: families::m68k::operand::AbsoluteSize::Word,
                        span,
                    }) if self.hunk_abs32_target_section_for_expr(expr).is_some() => Some(*span),
                    _ => None,
                }
            }
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
                M68KFamilyOperand::BitField { base, .. } => match base.as_ref() {
                    M68KFamilyOperand::Absolute { expr, size, .. } => (
                        expr,
                        matches!(size, families::m68k::operand::AbsoluteSize::Word),
                    ),
                    _ => continue,
                },
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
        enum RelocOperandKind {
            Plain,
            BitFieldBase,
        }

        let mut relocatable_operands = Vec::new();
        for (index, operand) in operands.iter().enumerate() {
            match operand {
                M68KFamilyOperand::Absolute {
                    expr,
                    size: families::m68k::operand::AbsoluteSize::Long,
                    span,
                } => {
                    let relocation =
                        self.eval_hunk_abs32_relocation_value(expr).map_err(|err| {
                            (
                                ast_eval_error_kind_to_asm(err.error.kind()),
                                err.error.message().to_string(),
                                err.span,
                            )
                        })?;
                    if relocation.is_some() {
                        relocatable_operands.push((index, RelocOperandKind::Plain, expr, *span));
                    }
                    continue;
                }
                M68KFamilyOperand::Immediate { expr, span } => {
                    let relocation =
                        self.eval_hunk_abs32_relocation_value(expr).map_err(|err| {
                            (
                                ast_eval_error_kind_to_asm(err.error.kind()),
                                err.error.message().to_string(),
                                err.span,
                            )
                        })?;
                    if relocation.is_some() {
                        relocatable_operands.push((index, RelocOperandKind::Plain, expr, *span));
                    }
                    continue;
                }
                M68KFamilyOperand::BitField { base, span, .. } => match base.as_ref() {
                    M68KFamilyOperand::Absolute {
                        expr,
                        size: families::m68k::operand::AbsoluteSize::Long,
                        ..
                    } => {
                        let relocation =
                            self.eval_hunk_abs32_relocation_value(expr).map_err(|err| {
                                (
                                    ast_eval_error_kind_to_asm(err.error.kind()),
                                    err.error.message().to_string(),
                                    err.span,
                                )
                            })?;
                        if relocation.is_some() {
                            relocatable_operands.push((
                                index,
                                RelocOperandKind::BitFieldBase,
                                expr,
                                *span,
                            ));
                        }
                        continue;
                    }
                    _ => continue,
                },
                _ => continue,
            }
        }

        let [(index, kind, expr, span)] = relocatable_operands.as_slice() else {
            return Ok(Vec::new());
        };

        let Some(offset) = (match kind {
            RelocOperandKind::BitFieldBase => u32::try_from(bytes.len())
                .ok()
                .and_then(|length| length.checked_sub(4)),
            RelocOperandKind::Plain => match index {
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
                2 => match operands {
                    [M68KFamilyOperand::DataRegister { .. }, M68KFamilyOperand::DataRegister { .. }, M68KFamilyOperand::Absolute {
                        size: families::m68k::operand::AbsoluteSize::Long,
                        ..
                    }] => u32::try_from(bytes.len())
                        .ok()
                        .and_then(|length| length.checked_sub(4)),
                    _ => None,
                },
                _ => None,
            },
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
        match vm::vm_opasm::encode_instruction_from_exprs_with_effects(
            model,
            self.cpu.as_str(),
            None,
            mnemonic,
            operands,
            self,
        ) {
            Ok(Some((bytes, effects))) => {
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
                let instruction_offset = match u32::try_from(self.bytes.len()) {
                    Ok(offset) => offset,
                    Err(_) => {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "instruction relocation base exceeds supported range",
                            None,
                        ))
                    }
                };
                let has_output_fixups = !effects.output_fixups.is_empty();
                for fixup in effects.output_fixups {
                    if fixup.width != 4
                        || fixup.kind != vm::fixup_vm::PortableOutputFixupKind::Absolute
                    {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "VM runtime emitted an unsupported output fixup",
                            None,
                        ));
                    }
                    let Some(offset) = instruction_offset.checked_add(fixup.offset) else {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "instruction relocation offset exceeds supported range",
                            None,
                        ));
                    };
                    let Some(output_fixup) =
                        self.hunk_abs32_output_fixup(offset, fixup.encoded_addend, fixup.target)
                    else {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "instruction relocation requires an active section",
                            None,
                        ));
                    };
                    self.mark_current_section_hunk_relocatable();
                    self.pending_output_fixups.push(output_fixup);
                }
                self.bytes.extend_from_slice(&bytes);
                if self.in_section()
                    && !effects.relocation_free
                    && !has_output_fixups
                    && operands
                        .iter()
                        .any(|expr| self.instruction_expr_references_target(expr))
                {
                    self.mark_current_section_hunk_fixup_error(&format!(
                        "format=hunk does not support this symbolic instruction form in v0.3: instruction {mnemonic} with operands {operands:?} references a relocatable symbol but its package encoding emitted no output fixup"
                    ));
                }
                Some(LineStatus::Ok)
            }
            Ok(None) => Some(self.failure(
                LineStatus::Error,
                AsmErrorKind::Instruction,
                &format!("No instruction found for {}", mnemonic.to_ascii_uppercase()),
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

    fn instruction_expr_references_target(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(name, _) | Expr::Register(name, _) => {
                self.symbol_is_target_reference(name)
            }
            Expr::List(items, _) | Expr::Tuple(items, _) => items
                .iter()
                .any(|item| self.instruction_expr_references_target(item)),
            Expr::Index { base, index, .. } => {
                self.instruction_expr_references_target(base)
                    || self.instruction_expr_references_target(index)
            }
            Expr::Member { base, .. }
            | Expr::Indirect(base, _)
            | Expr::Immediate(base, _)
            | Expr::IndirectLong(base, _)
            | Expr::Unary { expr: base, .. } => self.instruction_expr_references_target(base),
            Expr::Binary { left, right, .. } => {
                self.instruction_expr_references_target(left)
                    || self.instruction_expr_references_target(right)
            }
            Expr::Range {
                start, end, step, ..
            } => {
                self.instruction_expr_references_target(start)
                    || self.instruction_expr_references_target(end)
                    || step
                        .as_deref()
                        .is_some_and(|step| self.instruction_expr_references_target(step))
            }
            _ => false,
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
        let vm_start = std::time::Instant::now();
        let vm_res = vm::vm_opasm::encode_instruction_from_exprs_with_effects(
            model,
            self.cpu.as_str(),
            None,
            mapped_mnemonic,
            runtime_expr_operands,
            self,
        );
        let vm_elapsed = vm_start.elapsed();
        let bucket = self.line_route_bucket();
        crate::phase_profile::record_execution_path(Some(bucket), "vm.encode", vm_elapsed);

        match vm_res {
            Ok(Some((bytes, effects))) => {
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
                let instruction_offset = match u32::try_from(self.bytes.len()) {
                    Ok(offset) => offset,
                    Err(_) => {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "instruction relocation base exceeds supported range",
                            None,
                        ))
                    }
                };
                let has_output_fixups = !effects.output_fixups.is_empty();
                for fixup in effects.output_fixups {
                    if fixup.width != 4
                        || fixup.kind != vm::fixup_vm::PortableOutputFixupKind::Absolute
                    {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "VM runtime emitted an unsupported output fixup",
                            None,
                        ));
                    }
                    let Some(offset) = instruction_offset.checked_add(fixup.offset) else {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "instruction relocation offset exceeds supported range",
                            None,
                        ));
                    };
                    let Some(output_fixup) =
                        self.hunk_abs32_output_fixup(offset, fixup.encoded_addend, fixup.target)
                    else {
                        return Some(self.failure(
                            LineStatus::Error,
                            AsmErrorKind::Instruction,
                            "instruction relocation requires an active section",
                            None,
                        ));
                    };
                    self.mark_current_section_hunk_relocatable();
                    self.pending_output_fixups.push(output_fixup);
                }
                self.bytes.extend_from_slice(&bytes);
                if self.in_section()
                    && !effects.relocation_free
                    && !has_output_fixups
                    && runtime_expr_operands
                        .iter()
                        .any(|expr| self.instruction_expr_references_target(expr))
                {
                    self.mark_current_section_hunk_fixup_error(&format!(
                        "format=hunk does not support this symbolic instruction form in v0.3: instruction {mnemonic} with operands {runtime_expr_operands:?} references a relocatable symbol but its package encoding emitted no output fixup"
                    ));
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
                if family_runtime_authoritative
                    || (strict_runtime_parse_resolve && !defer_to_native_diagnostics)
                {
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
        let vm_start = std::time::Instant::now();
        let vm_res = vm::vm_opasm::encode_instruction(
            model,
            self.cpu.as_str(),
            None,
            mapped_mnemonic,
            resolved_operands,
        );
        let vm_elapsed = vm_start.elapsed();
        let bucket = self.line_route_bucket();
        crate::phase_profile::record_execution_path(Some(bucket), "vm.encode", vm_elapsed);

        match vm_res {
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

#[cfg(all(test, not(feature = "vm-runtime-only")))]
mod tests {
    use super::*;
    use opcore::parser::BinaryOp;

    fn span() -> Span {
        Span {
            line: 1,
            col_start: 1,
            col_end: 1,
        }
    }

    #[test]
    fn m68k_canonicalize_rewrites_datareg_binary_destination_symbol() {
        let operands = vec![
            Expr::Identifier("D0".to_string(), span()),
            Expr::Identifier("target".to_string(), span()),
        ];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("ADD.W", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Identifier(name, _), Expr::Identifier(target, _)]
                if name == "D0" && target == "target.L"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_immediate_binary_destination_symbol() {
        let operands = vec![
            Expr::Immediate(Box::new(Expr::Number("1".to_string(), span())), span()),
            Expr::Identifier("target".to_string(), span()),
        ];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("ADDI.W", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Immediate { .. }, Expr::Identifier(target, _)] if target == "target.L"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_bitop_destination_symbol() {
        let operands = vec![
            Expr::Immediate(Box::new(Expr::Number("1".to_string(), span())), span()),
            Expr::Identifier("target".to_string(), span()),
        ];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("BTST", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Immediate(_, _), Expr::Identifier(target, _)] if target == "target.L"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_addrreg_binary_source_symbol() {
        let operands = vec![
            Expr::Identifier("target".to_string(), span()),
            Expr::Identifier("A0".to_string(), span()),
        ];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("ADDA.L", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Identifier(target, _), Expr::Identifier(name, _)]
                if target == "target.L" && name == "A0"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_scc_destination_symbol() {
        let operands = vec![Expr::Identifier("target".to_string(), span())];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("SNE", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Identifier(target, _)] if target == "target.L"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_wordmath_source_symbol() {
        let operands = vec![
            Expr::Identifier("target".to_string(), span()),
            Expr::Identifier("D1".to_string(), span()),
        ];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("MULU", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Identifier(target, _), Expr::Identifier(name, _)]
                if target == "target.L" && name == "D1"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_memory_shift_destination_symbol() {
        let operands = vec![Expr::Identifier("target".to_string(), span())];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("ASL", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Identifier(target, _)] if target == "target.L"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_special_move_symbol_operand() {
        let operands = vec![
            Expr::Identifier("target".to_string(), span()),
            Expr::Identifier("SR".to_string(), span()),
        ];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("MOVE", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Identifier(target, _), Expr::Identifier(name, _)]
                if target == "target.L" && name == "SR"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_movem_symbol_operand() {
        let operands = vec![
            Expr::Binary {
                op: BinaryOp::Divide,
                left: Box::new(Expr::Identifier("D0".to_string(), span())),
                right: Box::new(Expr::Identifier("D1".to_string(), span())),
                span: span(),
            },
            Expr::Identifier("target".to_string(), span()),
        ];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("MOVEM.W", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Binary { .. }, Expr::Identifier(target, _)] if target == "target.L"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_moves_symbol_operand() {
        let operands = vec![
            Expr::Identifier("target".to_string(), span()),
            Expr::Identifier("A2".to_string(), span()),
        ];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("MOVES.L", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Identifier(target, _), Expr::Identifier(name, _)]
                if target == "target.L" && name == "A2"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_chk2_cmp2_symbol_operand() {
        let operands = vec![
            Expr::Identifier("target".to_string(), span()),
            Expr::Identifier("D0".to_string(), span()),
        ];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("CHK2.W", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Identifier(target, _), Expr::Identifier(name, _)]
                if target == "target.L" && name == "D0"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_cas_symbol_operand() {
        let operands = vec![
            Expr::Identifier("D0".to_string(), span()),
            Expr::Identifier("D1".to_string(), span()),
            Expr::Identifier("target".to_string(), span()),
        ];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("CAS.W", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Identifier(compare, _), Expr::Identifier(update, _), Expr::Identifier(target, _)]
                if compare == "D0" && update == "D1" && target == "target.L"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_callm_symbol_operand() {
        let operands = vec![
            Expr::Immediate(Box::new(Expr::Number("5".to_string(), span())), span()),
            Expr::Identifier("target".to_string(), span()),
        ];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("CALLM", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Immediate(_, _), Expr::Identifier(target, _)] if target == "target.L"
        ));
    }

    #[test]
    fn m68k_canonicalize_rewrites_bitfield_symbol_operand() {
        let operands = vec![Expr::Call {
            name: ".bitfield".to_string(),
            args: vec![
                Expr::Identifier("target".to_string(), span()),
                Expr::Number("3".to_string(), span()),
                Expr::Number("5".to_string(), span()),
            ],
            span: span(),
        }];
        let mut symbols = SymbolTable::new();
        let registry = ModuleRegistry::new();
        let asm = AsmLine::new(&mut symbols, &registry);
        let rewritten = asm
            .m68k_canonicalize_supported_bare_symbol_operands("BFTST", operands.as_slice())
            .expect("expected rewrite");
        assert!(matches!(
            rewritten.as_slice(),
            [Expr::Call { name, args, .. }]
                if name == ".bitfield"
                    && matches!(&args[0], Expr::Identifier(target, _) if target == "target.L")
        ));
    }
}
