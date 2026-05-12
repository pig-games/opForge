// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! 6502-native host harness envelope for VM runtime integration.

use crate::execution_model::HierarchyExecutionModel;
use crate::native6502_abi::{
    NATIVE_6502_ABI_MAGIC_V1, NATIVE_6502_ABI_VERSION_V1, NATIVE_6502_CAPABILITY_ENUM_TABLES_V1,
    NATIVE_6502_CAPABILITY_EXT_TLV_V1, NATIVE_6502_CAPABILITY_STRUCT_LAYOUTS_V1,
    NATIVE_6502_CB_ABI_VERSION_OFFSET, NATIVE_6502_CB_CAPABILITY_FLAGS_OFFSET,
    NATIVE_6502_CB_INPUT_LEN_OFFSET, NATIVE_6502_CB_LAST_ERROR_LEN_OFFSET,
    NATIVE_6502_CB_MAGIC_OFFSET, NATIVE_6502_CB_OUTPUT_LEN_OFFSET,
    NATIVE_6502_CB_REQUEST_ID_OFFSET, NATIVE_6502_CB_STATUS_CODE_OFFSET,
    NATIVE_6502_CB_STRUCT_SIZE_OFFSET, NATIVE_6502_CONTROL_BLOCK_SIZE_V1,
    NATIVE_6502_ENTRYPOINT_COUNT_V1, NATIVE_6502_ENTRYPOINT_ENCODE_INSTRUCTION_V1,
    NATIVE_6502_ENTRYPOINT_ENCODE_SELECTED_INSTRUCTION_V1,
    NATIVE_6502_ENTRYPOINT_EVALUATE_EXPRESSION_V1, NATIVE_6502_ENTRYPOINT_INIT_V1,
    NATIVE_6502_ENTRYPOINT_LAST_ERROR_V1, NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
    NATIVE_6502_ENTRYPOINT_PARSE_LINE_V1, NATIVE_6502_ENTRYPOINT_SELECT_INSTRUCTION_V1,
    NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1, NATIVE_6502_ENTRYPOINT_TOKENIZE_LINE_V1,
};
use crate::native_prvm::{
    NativePrvmExprRequest, NativePrvmHostExpressionBridge, NativePrvmHostExpressionEvaluation,
    NATIVE_PRVM_EXPR_RESULT_SLOT_SIZE,
};
use crate::portable_contract::{PortableLineAst, PortableToken};
use crate::runtime_error::RuntimeBridgeError;
use crate::runtime_portable_types::PortableInstructionRequest;
use crate::vm_opasm::{
    parse_operand_expr_range, split_top_level_comma_ranges, OperandExprBoundary,
    OperandExprParseHints,
};
use crate::vm_opasm_parse::{tokenize_parser_tokens_with_model, VmExprParseContext};
use opcore::parser::Expr;
use opcore::tokenizer::Span;
use registry::family::AssemblerContext;
use registry::registry::VmEncodeCandidate;
use registry::syntax::register_checker_none;

pub const NATIVE_6502_STATUS_OK_V1: u16 = 0;
pub const NATIVE_6502_STATUS_BAD_CONTROL_BLOCK_V1: u16 = 1;
pub const NATIVE_6502_STATUS_BAD_REQUEST_V1: u16 = 2;
pub const NATIVE_6502_STATUS_RUNTIME_ERROR_V1: u16 = 3;
pub const NATIVE_6502_SESSION_IMAGE_CAPACITY: usize = 4096;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Native6502ControlBlockV1 {
    bytes: [u8; NATIVE_6502_CONTROL_BLOCK_SIZE_V1 as usize],
}

impl Default for Native6502ControlBlockV1 {
    fn default() -> Self {
        Self::new_v1()
    }
}

impl Native6502ControlBlockV1 {
    pub fn new_v1() -> Self {
        let mut out = Self {
            bytes: [0u8; NATIVE_6502_CONTROL_BLOCK_SIZE_V1 as usize],
        };
        out.reset_v1_header();
        out
    }

    pub fn reset_v1_header(&mut self) {
        self.bytes.fill(0);
        self.bytes[NATIVE_6502_CB_MAGIC_OFFSET..NATIVE_6502_CB_MAGIC_OFFSET + 4]
            .copy_from_slice(&NATIVE_6502_ABI_MAGIC_V1);
        self.write_u16_at(
            NATIVE_6502_CB_ABI_VERSION_OFFSET,
            NATIVE_6502_ABI_VERSION_V1,
        );
        self.write_u16_at(
            NATIVE_6502_CB_STRUCT_SIZE_OFFSET,
            NATIVE_6502_CONTROL_BLOCK_SIZE_V1,
        );
        self.write_u16_at(
            NATIVE_6502_CB_CAPABILITY_FLAGS_OFFSET,
            NATIVE_6502_CAPABILITY_EXT_TLV_V1
                | NATIVE_6502_CAPABILITY_STRUCT_LAYOUTS_V1
                | NATIVE_6502_CAPABILITY_ENUM_TABLES_V1,
        );
        self.write_u16_at(NATIVE_6502_CB_STATUS_CODE_OFFSET, NATIVE_6502_STATUS_OK_V1);
    }

    pub fn bytes(&self) -> &[u8; NATIVE_6502_CONTROL_BLOCK_SIZE_V1 as usize] {
        &self.bytes
    }

    pub fn validate_v1_header(&self) -> Result<(), String> {
        if self.bytes[NATIVE_6502_CB_MAGIC_OFFSET..NATIVE_6502_CB_MAGIC_OFFSET + 4]
            != NATIVE_6502_ABI_MAGIC_V1
        {
            return Err("invalid native 6502 control-block magic".to_string());
        }
        if self.abi_version() != NATIVE_6502_ABI_VERSION_V1 {
            return Err(format!(
                "unsupported native 6502 control-block version {}",
                self.abi_version()
            ));
        }
        if self.struct_size() != NATIVE_6502_CONTROL_BLOCK_SIZE_V1 {
            return Err(format!(
                "unsupported native 6502 control-block size {}",
                self.struct_size()
            ));
        }
        Ok(())
    }

    pub fn abi_version(&self) -> u16 {
        self.read_u16_at(NATIVE_6502_CB_ABI_VERSION_OFFSET)
    }

    pub fn struct_size(&self) -> u16 {
        self.read_u16_at(NATIVE_6502_CB_STRUCT_SIZE_OFFSET)
    }

    pub fn capability_flags(&self) -> u16 {
        self.read_u16_at(NATIVE_6502_CB_CAPABILITY_FLAGS_OFFSET)
    }

    pub fn status_code(&self) -> u16 {
        self.read_u16_at(NATIVE_6502_CB_STATUS_CODE_OFFSET)
    }

    pub fn request_id(&self) -> u16 {
        self.read_u16_at(NATIVE_6502_CB_REQUEST_ID_OFFSET)
    }

    pub fn input_len(&self) -> u16 {
        self.read_u16_at(NATIVE_6502_CB_INPUT_LEN_OFFSET)
    }

    pub fn output_len(&self) -> u16 {
        self.read_u16_at(NATIVE_6502_CB_OUTPUT_LEN_OFFSET)
    }

    pub fn last_error_len(&self) -> u16 {
        self.read_u16_at(NATIVE_6502_CB_LAST_ERROR_LEN_OFFSET)
    }

    fn read_u16_at(&self, offset: usize) -> u16 {
        u16::from_le_bytes([self.bytes[offset], self.bytes[offset + 1]])
    }

    fn write_u16_at(&mut self, offset: usize, value: u16) {
        self.bytes[offset..offset + 2].copy_from_slice(&value.to_le_bytes());
    }

    fn set_status_code(&mut self, value: u16) {
        self.write_u16_at(NATIVE_6502_CB_STATUS_CODE_OFFSET, value);
    }

    fn set_request_id(&mut self, value: u16) {
        self.write_u16_at(NATIVE_6502_CB_REQUEST_ID_OFFSET, value);
    }

    fn set_input_len(&mut self, value: u16) {
        self.write_u16_at(NATIVE_6502_CB_INPUT_LEN_OFFSET, value);
    }

    fn set_output_len(&mut self, value: u16) {
        self.write_u16_at(NATIVE_6502_CB_OUTPUT_LEN_OFFSET, value);
    }

    fn set_last_error_len(&mut self, value: u16) {
        self.write_u16_at(NATIVE_6502_CB_LAST_ERROR_LEN_OFFSET, value);
    }
}

pub enum Native6502HarnessRequest<'a> {
    Init,
    LoadPackage {
        package_bytes: &'a [u8],
    },
    SetPipeline {
        cpu_id: &'a str,
        dialect_override: Option<&'a str>,
    },
    TokenizeLine {
        source_line: &'a str,
        line_num: u32,
    },
    ParseLine {
        source_line: &'a str,
        line_num: u32,
    },
    EncodeInstruction {
        mnemonic: &'a str,
        candidates: &'a [VmEncodeCandidate],
    },
    EvaluateExpression {
        source_line: &'a str,
        line_num: u32,
        operand_start_col: u16,
        operand_end_col: u16,
        mnemonic: Option<&'a str>,
        assembler_ctx: &'a dyn AssemblerContext,
    },
    SelectInstruction {
        mnemonic: &'a str,
        source_line: &'a str,
        line_num: u32,
        operand_span: Option<(u16, u16)>,
        assembler_ctx: &'a dyn AssemblerContext,
    },
    EncodeSelectedInstruction {
        mnemonic: &'a str,
        source_line: &'a str,
        line_num: u32,
        address: u32,
        operand_span: Option<(u16, u16)>,
        assembler_ctx: &'a dyn AssemblerContext,
    },
    LastError,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Native6502HarnessOutput {
    None,
    Tokens(Vec<PortableToken>),
    LineAst(PortableLineAst),
    EncodedBytes(Option<Vec<u8>>),
    ExpressionEvaluation(NativePrvmHostExpressionEvaluation),
    InstructionCandidates(Vec<VmEncodeCandidate>),
    ErrorMessage(String),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Native6502HarnessResponse {
    pub entrypoint: u8,
    pub status_code: u16,
    pub request_id: u16,
    pub output: Native6502HarnessOutput,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Native6502WireResponse {
    pub entrypoint: u8,
    pub status_code: u16,
    pub request_id: u16,
    pub output_payload: Vec<u8>,
}

enum Native6502HarnessRequestOwned {
    Init,
    LoadPackage {
        package_bytes: Vec<u8>,
    },
    SetPipeline {
        cpu_id: String,
        dialect_override: Option<String>,
    },
    TokenizeLine {
        source_line: String,
        line_num: u32,
    },
    ParseLine {
        source_line: String,
        line_num: u32,
    },
    EncodeInstruction {
        mnemonic: String,
        candidates: Vec<VmEncodeCandidate>,
    },
    LastError,
}

impl Native6502HarnessRequestOwned {
    fn as_borrowed(&self) -> Native6502HarnessRequest<'_> {
        match self {
            Self::Init => Native6502HarnessRequest::Init,
            Self::LoadPackage { package_bytes } => Native6502HarnessRequest::LoadPackage {
                package_bytes: package_bytes.as_slice(),
            },
            Self::SetPipeline {
                cpu_id,
                dialect_override,
            } => Native6502HarnessRequest::SetPipeline {
                cpu_id: cpu_id.as_str(),
                dialect_override: dialect_override.as_deref(),
            },
            Self::TokenizeLine {
                source_line,
                line_num,
            } => Native6502HarnessRequest::TokenizeLine {
                source_line: source_line.as_str(),
                line_num: *line_num,
            },
            Self::ParseLine {
                source_line,
                line_num,
            } => Native6502HarnessRequest::ParseLine {
                source_line: source_line.as_str(),
                line_num: *line_num,
            },
            Self::EncodeInstruction {
                mnemonic,
                candidates,
            } => Native6502HarnessRequest::EncodeInstruction {
                mnemonic: mnemonic.as_str(),
                candidates: candidates.as_slice(),
            },
            Self::LastError => Native6502HarnessRequest::LastError,
        }
    }
}

#[derive(Debug)]
pub struct Native6502Harness {
    model: Option<HierarchyExecutionModel>,
    active_cpu: Option<String>,
    dialect_override: Option<String>,
    session_image_origin: Option<u32>,
    session_image_bytes: Vec<u8>,
    last_error: String,
    next_request_id: u16,
}

impl Default for Native6502Harness {
    fn default() -> Self {
        Self::new()
    }
}

impl Native6502Harness {
    pub fn new() -> Self {
        Self {
            model: None,
            active_cpu: None,
            dialect_override: None,
            session_image_origin: None,
            session_image_bytes: Vec::new(),
            last_error: String::new(),
            next_request_id: 1,
        }
    }

    pub fn invoke_v1(
        &mut self,
        control_block: &mut Native6502ControlBlockV1,
        entrypoint: u8,
        request: Native6502HarnessRequest<'_>,
    ) -> Native6502HarnessResponse {
        let request_id = self.next_request_id;
        self.next_request_id = self.next_request_id.wrapping_add(1);
        control_block.set_request_id(request_id);

        if entrypoint == NATIVE_6502_ENTRYPOINT_INIT_V1 {
            control_block.reset_v1_header();
            control_block.set_request_id(request_id);
            self.model = None;
            self.active_cpu = None;
            self.dialect_override = None;
            self.session_image_origin = None;
            self.session_image_bytes.clear();
            self.last_error.clear();
            return self.finish_ok(
                control_block,
                entrypoint,
                request_id,
                0,
                Native6502HarnessOutput::None,
            );
        }

        if let Err(message) = control_block.validate_v1_header() {
            return self.finish_error(
                control_block,
                entrypoint,
                request_id,
                NATIVE_6502_STATUS_BAD_CONTROL_BLOCK_V1,
                message,
            );
        }

        let input_len = match request_input_len(&request) {
            Ok(value) => value,
            Err(message) => {
                return self.finish_error(
                    control_block,
                    entrypoint,
                    request_id,
                    NATIVE_6502_STATUS_BAD_REQUEST_V1,
                    message,
                );
            }
        };
        control_block.set_input_len(input_len);
        control_block.set_output_len(0);

        if entrypoint >= NATIVE_6502_ENTRYPOINT_COUNT_V1 {
            return self.finish_error(
                control_block,
                entrypoint,
                request_id,
                NATIVE_6502_STATUS_BAD_REQUEST_V1,
                format!("unsupported native entrypoint ordinal {}", entrypoint),
            );
        }

        let call_result: Result<(usize, Native6502HarnessOutput), String> =
            match (entrypoint, request) {
                (
                    NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
                    Native6502HarnessRequest::LoadPackage { package_bytes },
                ) => self
                    .load_package(package_bytes)
                    .map(|_| (0usize, Native6502HarnessOutput::None)),
                (
                    NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1,
                    Native6502HarnessRequest::SetPipeline {
                        cpu_id,
                        dialect_override,
                    },
                ) => self
                    .set_pipeline(cpu_id, dialect_override)
                    .map(|_| (0usize, Native6502HarnessOutput::None)),
                (
                    NATIVE_6502_ENTRYPOINT_TOKENIZE_LINE_V1,
                    Native6502HarnessRequest::TokenizeLine {
                        source_line,
                        line_num,
                    },
                ) => self
                    .tokenize_line(source_line, line_num)
                    .map(|tokens| (tokens.len(), Native6502HarnessOutput::Tokens(tokens))),
                (
                    NATIVE_6502_ENTRYPOINT_PARSE_LINE_V1,
                    Native6502HarnessRequest::ParseLine {
                        source_line,
                        line_num,
                    },
                ) => self
                    .parse_line(source_line, line_num)
                    .map(|ast| (1usize, Native6502HarnessOutput::LineAst(ast))),
                (
                    NATIVE_6502_ENTRYPOINT_ENCODE_INSTRUCTION_V1,
                    Native6502HarnessRequest::EncodeInstruction {
                        mnemonic,
                        candidates,
                    },
                ) => self.encode_instruction(mnemonic, candidates).map(|bytes| {
                    let output_len = bytes.as_ref().map(|value| value.len()).unwrap_or(0usize);
                    (output_len, Native6502HarnessOutput::EncodedBytes(bytes))
                }),
                (
                    NATIVE_6502_ENTRYPOINT_EVALUATE_EXPRESSION_V1,
                    Native6502HarnessRequest::EvaluateExpression {
                        source_line,
                        line_num,
                        operand_start_col,
                        operand_end_col,
                        mnemonic,
                        assembler_ctx,
                    },
                ) => self
                    .evaluate_expression(
                        source_line,
                        line_num,
                        operand_start_col,
                        operand_end_col,
                        mnemonic,
                        assembler_ctx,
                    )
                    .map(|evaluation| {
                        let output_len = encode_expression_evaluation_output(&evaluation).len();
                        (
                            output_len,
                            Native6502HarnessOutput::ExpressionEvaluation(evaluation),
                        )
                    }),
                (
                    NATIVE_6502_ENTRYPOINT_SELECT_INSTRUCTION_V1,
                    Native6502HarnessRequest::SelectInstruction {
                        mnemonic,
                        source_line,
                        line_num,
                        operand_span,
                        assembler_ctx,
                    },
                ) => self
                    .select_instruction_candidates(
                        mnemonic,
                        source_line,
                        line_num,
                        operand_span,
                        assembler_ctx,
                    )
                    .map(|candidates| {
                        (
                            candidates.len(),
                            Native6502HarnessOutput::InstructionCandidates(candidates),
                        )
                    }),
                (
                    NATIVE_6502_ENTRYPOINT_ENCODE_SELECTED_INSTRUCTION_V1,
                    Native6502HarnessRequest::EncodeSelectedInstruction {
                        mnemonic,
                        source_line,
                        line_num,
                        address,
                        operand_span,
                        assembler_ctx,
                    },
                ) => self
                    .encode_selected_instruction(
                        mnemonic,
                        source_line,
                        line_num,
                        address,
                        operand_span,
                        assembler_ctx,
                    )
                    .map(|bytes| {
                        let output_len = bytes.len();
                        (
                            output_len,
                            Native6502HarnessOutput::EncodedBytes(Some(bytes)),
                        )
                    }),
                (NATIVE_6502_ENTRYPOINT_LAST_ERROR_V1, Native6502HarnessRequest::LastError) => {
                    Ok((
                        self.last_error.len(),
                        Native6502HarnessOutput::ErrorMessage(self.last_error.clone()),
                    ))
                }
                _ => Err(format!(
                    "native entrypoint {} received mismatched request payload",
                    entrypoint
                )),
            };

        match call_result {
            Ok((output_len, output)) => {
                self.finish_ok(control_block, entrypoint, request_id, output_len, output)
            }
            Err(message) => self.finish_error(
                control_block,
                entrypoint,
                request_id,
                NATIVE_6502_STATUS_RUNTIME_ERROR_V1,
                message,
            ),
        }
    }

    pub fn invoke_wire_v1(
        &mut self,
        control_block: &mut Native6502ControlBlockV1,
        entrypoint: u8,
        input_payload: &[u8],
    ) -> Native6502WireResponse {
        let owned_request = match decode_wire_request(entrypoint, input_payload) {
            Ok(request) => request,
            Err(message) => {
                let request_id = self.next_request_id;
                self.next_request_id = self.next_request_id.wrapping_add(1);
                control_block.set_request_id(request_id);
                control_block.set_input_len(u16::try_from(input_payload.len()).unwrap_or(u16::MAX));
                let response = self.finish_error(
                    control_block,
                    entrypoint,
                    request_id,
                    NATIVE_6502_STATUS_BAD_REQUEST_V1,
                    message,
                );
                return Native6502WireResponse {
                    entrypoint,
                    status_code: response.status_code,
                    request_id: response.request_id,
                    output_payload: encode_wire_output_payload(&response.output),
                };
            }
        };
        let response = self.invoke_v1(control_block, entrypoint, owned_request.as_borrowed());
        let output_payload = encode_wire_output_payload(&response.output);
        if response.status_code == NATIVE_6502_STATUS_OK_V1 {
            control_block.set_output_len(u16::try_from(output_payload.len()).unwrap_or(u16::MAX));
        }
        Native6502WireResponse {
            entrypoint,
            status_code: response.status_code,
            request_id: response.request_id,
            output_payload,
        }
    }

    pub fn session_image_origin(&self) -> Option<u32> {
        self.session_image_origin
    }

    pub fn session_image_bytes(&self) -> &[u8] {
        self.session_image_bytes.as_slice()
    }

    fn load_package(&mut self, package_bytes: &[u8]) -> Result<(), String> {
        let model = crate::vm_opasm::load_model_from_package_bytes(package_bytes)
            .map_err(|err| err.to_string())?;
        self.model = Some(model);
        self.active_cpu = None;
        self.dialect_override = None;
        Ok(())
    }

    fn set_pipeline(&mut self, cpu_id: &str, dialect_override: Option<&str>) -> Result<(), String> {
        let model = self.model.as_ref().ok_or_else(|| {
            "OTR001: native set_pipeline requires a loaded VM package".to_string()
        })?;
        model
            .resolve_pipeline(cpu_id, dialect_override)
            .map_err(runtime_error_to_string)?;
        self.active_cpu = Some(cpu_id.to_string());
        self.dialect_override = dialect_override.map(ToString::to_string);
        Ok(())
    }

    fn tokenize_line(
        &self,
        source_line: &str,
        line_num: u32,
    ) -> Result<Vec<PortableToken>, String> {
        let (model, active_cpu, dialect_override) = self.require_active_model()?;
        model
            .tokenize_portable_statement_for_assembler(
                active_cpu,
                dialect_override,
                source_line,
                line_num,
            )
            .map_err(runtime_error_to_string)
    }

    fn parse_line(&self, source_line: &str, line_num: u32) -> Result<PortableLineAst, String> {
        let (model, active_cpu, dialect_override) = self.require_active_model()?;
        crate::vm_opasm::parse_portable_line_for_assembler(
            model,
            active_cpu,
            dialect_override,
            source_line,
            line_num,
        )
        .map_err(|err| err.message)
    }

    fn encode_instruction(
        &self,
        mnemonic: &str,
        candidates: &[VmEncodeCandidate],
    ) -> Result<Option<Vec<u8>>, String> {
        let (model, active_cpu, dialect_override) = self.require_active_model()?;
        let request = PortableInstructionRequest {
            cpu_id: active_cpu.to_string(),
            dialect_override: dialect_override.map(ToString::to_string),
            mnemonic: mnemonic.to_string(),
            candidates: candidates.to_vec(),
        };
        model
            .encode_portable_instruction(&request)
            .map_err(runtime_error_to_string)
    }

    fn encode_selected_instruction(
        &mut self,
        mnemonic: &str,
        source_line: &str,
        line_num: u32,
        address: u32,
        operand_span: Option<(u16, u16)>,
        assembler_ctx: &dyn AssemblerContext,
    ) -> Result<Vec<u8>, String> {
        let candidates = self.select_instruction_candidates(
            mnemonic,
            source_line,
            line_num,
            operand_span,
            assembler_ctx,
        )?;
        let bytes = self
            .encode_instruction(mnemonic, candidates.as_slice())?
            .ok_or_else(|| format!("native encode emitted no bytes for {mnemonic}"))?;
        self.store_session_image_bytes(address, bytes.as_slice())?;
        Ok(bytes)
    }

    fn store_session_image_bytes(&mut self, address: u32, bytes: &[u8]) -> Result<(), String> {
        if bytes.is_empty() {
            return Ok(());
        }
        let origin = match self.session_image_origin {
            Some(origin) => origin,
            None => {
                self.session_image_origin = Some(address);
                address
            }
        };
        if address < origin {
            return Err(format!(
                "native session image write address ${address:04X} precedes origin ${origin:04X}"
            ));
        }
        let offset = usize::try_from(address - origin)
            .map_err(|_| "native session image offset exceeds host usize".to_string())?;
        let end = offset
            .checked_add(bytes.len())
            .ok_or_else(|| "native session image write overflows host usize".to_string())?;
        if end > NATIVE_6502_SESSION_IMAGE_CAPACITY {
            return Err(format!(
                "native session image write exceeds capacity {}",
                NATIVE_6502_SESSION_IMAGE_CAPACITY
            ));
        }
        if self.session_image_bytes.len() < end {
            self.session_image_bytes.resize(end, 0);
        }
        self.session_image_bytes[offset..end].copy_from_slice(bytes);
        Ok(())
    }

    fn evaluate_expression(
        &self,
        source_line: &str,
        line_num: u32,
        operand_start_col: u16,
        operand_end_col: u16,
        mnemonic: Option<&str>,
        assembler_ctx: &dyn AssemblerContext,
    ) -> Result<NativePrvmHostExpressionEvaluation, String> {
        if operand_start_col == 0 || operand_end_col < operand_start_col {
            return Err("native expression request has invalid operand span".to_string());
        }
        let (model, active_cpu, dialect_override) = self.require_active_model()?;
        let register_checker = register_checker_none();
        let (tokens, _, _) = tokenize_parser_tokens_with_model(
            model,
            active_cpu,
            dialect_override,
            source_line,
            line_num,
            &register_checker,
        )
        .map_err(|err| err.message)?;
        let operand_start_col = operand_start_col as usize;
        let operand_end_col = operand_end_col as usize;
        let start_token = tokens
            .iter()
            .position(|token| token.span.col_start >= operand_start_col)
            .ok_or_else(|| "native expression request operand start token not found".to_string())?;
        let end_token = tokens
            .iter()
            .position(|token| token.span.col_start >= operand_end_col)
            .unwrap_or(tokens.len());
        let mut bridge = NativePrvmHostExpressionBridge::from_source_line(
            model,
            active_cpu,
            dialect_override,
            source_line,
            line_num,
            &register_checker,
            mnemonic,
        )
        .map_err(|err| err.message)?;
        let request = NativePrvmExprRequest {
            operand_index: 0,
            expr_slot_index: 0,
            start_token: start_token as u32,
            end_token: end_token as u32,
            boundary_span: Span {
                line: line_num,
                col_start: operand_start_col,
                col_end: operand_end_col,
            },
        };
        let mut result_slot = [0u8; NATIVE_PRVM_EXPR_RESULT_SLOT_SIZE];
        bridge
            .handle_and_evaluate_expression_request(request, &mut result_slot, assembler_ctx)
            .map(|result| result.evaluation)
            .map_err(|err| format!("native expression evaluation failed: {err:?}"))
    }

    fn select_instruction_candidates(
        &self,
        mnemonic: &str,
        source_line: &str,
        line_num: u32,
        operand_span: Option<(u16, u16)>,
        assembler_ctx: &dyn AssemblerContext,
    ) -> Result<Vec<VmEncodeCandidate>, String> {
        if mnemonic.is_empty() {
            return Err("native selector request requires non-empty mnemonic".to_string());
        }
        let (model, active_cpu, dialect_override) = self.require_active_model()?;
        let resolved = model
            .resolve_pipeline(active_cpu, dialect_override)
            .map_err(runtime_error_to_string)?;
        if !resolved.family_id.eq_ignore_ascii_case("mos6502") {
            return Err(format!(
                "native selector request does not support family {}",
                resolved.family_id
            ));
        }
        let operands = match operand_span {
            Some((operand_start_col, operand_end_col)) => self.parse_operand_expressions(
                source_line,
                line_num,
                operand_start_col,
                operand_end_col,
                Some(mnemonic),
            )?,
            None => Vec::new(),
        };
        let candidates = model
            .select_candidates_from_exprs_mos6502(
                &resolved,
                mnemonic,
                operands.as_slice(),
                assembler_ctx,
            )
            .map_err(runtime_error_to_string)?
            .ok_or_else(|| format!("native selector found no candidates for {mnemonic}"))?;
        Ok(candidates)
    }

    fn parse_operand_expressions(
        &self,
        source_line: &str,
        line_num: u32,
        operand_start_col: u16,
        operand_end_col: u16,
        mnemonic: Option<&str>,
    ) -> Result<Vec<Expr>, String> {
        if operand_start_col == 0 || operand_end_col < operand_start_col {
            return Err("native selector request has invalid operand span".to_string());
        }
        let (model, active_cpu, dialect_override) = self.require_active_model()?;
        let register_checker = register_checker_none();
        let (tokens, _, _) = tokenize_parser_tokens_with_model(
            model,
            active_cpu,
            dialect_override,
            source_line,
            line_num,
            &register_checker,
        )
        .map_err(|err| err.message)?;
        let operand_start_col = operand_start_col as usize;
        let operand_end_col = operand_end_col as usize;
        let start_token = tokens
            .iter()
            .position(|token| token.span.col_start >= operand_start_col)
            .ok_or_else(|| "native selector operand start token not found".to_string())?;
        let end_token = tokens
            .iter()
            .position(|token| token.span.col_start >= operand_end_col)
            .unwrap_or(tokens.len());
        let boundary_span = Span {
            line: line_num,
            col_start: operand_start_col,
            col_end: operand_end_col,
        };
        let mut operands = Vec::new();
        let expr_parse_ctx = VmExprParseContext {
            model,
            cpu_id: active_cpu,
            dialect_override,
            expr_parser_opt_in_families: &[],
            expr_parser_force_host_families: &[],
            expr_handler: None,
        };
        for (operand_index, (start, end)) in
            split_top_level_comma_ranges(tokens.as_slice(), start_token, end_token)
                .into_iter()
                .enumerate()
        {
            parse_operand_expr_range(
                tokens.as_slice(),
                start,
                end,
                OperandExprBoundary {
                    end_span: boundary_span,
                    end_token_text: None,
                },
                OperandExprParseHints {
                    mnemonic,
                    operand_index,
                },
                &expr_parse_ctx,
                &mut operands,
            )
            .map_err(|err| format!("native selector expression parse failed: {err:?}"))?;
        }
        Ok(operands)
    }

    fn require_active_model(
        &self,
    ) -> Result<(&HierarchyExecutionModel, &str, Option<&str>), String> {
        let model = self
            .model
            .as_ref()
            .ok_or_else(|| "OTR001: native entrypoint requires a loaded VM package".to_string())?;
        let active_cpu = self
            .active_cpu
            .as_deref()
            .ok_or_else(|| "OTR001: native entrypoint requires set_pipeline first".to_string())?;
        Ok((model, active_cpu, self.dialect_override.as_deref()))
    }

    fn finish_ok(
        &mut self,
        control_block: &mut Native6502ControlBlockV1,
        entrypoint: u8,
        request_id: u16,
        output_len: usize,
        output: Native6502HarnessOutput,
    ) -> Native6502HarnessResponse {
        let output_len_u16 = u16::try_from(output_len).unwrap_or(u16::MAX);
        control_block.set_status_code(NATIVE_6502_STATUS_OK_V1);
        control_block.set_output_len(output_len_u16);
        control_block.set_last_error_len(0);
        self.last_error.clear();
        Native6502HarnessResponse {
            entrypoint,
            status_code: NATIVE_6502_STATUS_OK_V1,
            request_id,
            output,
        }
    }

    fn finish_error(
        &mut self,
        control_block: &mut Native6502ControlBlockV1,
        entrypoint: u8,
        request_id: u16,
        status_code: u16,
        message: String,
    ) -> Native6502HarnessResponse {
        let message_len = u16::try_from(message.len()).unwrap_or(u16::MAX);
        self.last_error = message.clone();
        control_block.set_status_code(status_code);
        control_block.set_output_len(0);
        control_block.set_last_error_len(message_len);
        Native6502HarnessResponse {
            entrypoint,
            status_code,
            request_id,
            output: Native6502HarnessOutput::ErrorMessage(message),
        }
    }
}

fn request_input_len(request: &Native6502HarnessRequest<'_>) -> Result<u16, String> {
    let len = match request {
        Native6502HarnessRequest::Init => 0usize,
        Native6502HarnessRequest::LoadPackage { package_bytes } => package_bytes.len(),
        Native6502HarnessRequest::SetPipeline {
            cpu_id,
            dialect_override,
        } => {
            cpu_id.len()
                + dialect_override
                    .map(|value| value.len())
                    .unwrap_or_default()
        }
        Native6502HarnessRequest::TokenizeLine { source_line, .. }
        | Native6502HarnessRequest::ParseLine { source_line, .. } => source_line.len(),
        Native6502HarnessRequest::EncodeInstruction {
            mnemonic,
            candidates,
        } => mnemonic.len() + candidates.len(),
        Native6502HarnessRequest::EvaluateExpression {
            source_line,
            mnemonic,
            ..
        } => source_line.len() + mnemonic.map(|value| value.len()).unwrap_or_default(),
        Native6502HarnessRequest::SelectInstruction {
            mnemonic,
            source_line,
            ..
        } => mnemonic.len() + source_line.len(),
        Native6502HarnessRequest::EncodeSelectedInstruction {
            mnemonic,
            source_line,
            ..
        } => mnemonic.len() + source_line.len(),
        Native6502HarnessRequest::LastError => 0usize,
    };
    u16::try_from(len)
        .map_err(|_| format!("native request payload length {} exceeds u16 envelope", len))
}

fn runtime_error_to_string(err: RuntimeBridgeError) -> String {
    err.to_string()
}

pub fn encode_wire_set_pipeline_payload(
    cpu_id: &str,
    dialect_override: Option<&str>,
) -> Result<Vec<u8>, String> {
    if cpu_id.is_empty() {
        return Err("native set_pipeline payload requires non-empty cpu_id".to_string());
    }
    if cpu_id.as_bytes().contains(&0) {
        return Err("native set_pipeline cpu_id cannot contain NUL".to_string());
    }
    if let Some(dialect) = dialect_override {
        if dialect.as_bytes().contains(&0) {
            return Err("native set_pipeline dialect_override cannot contain NUL".to_string());
        }
    }
    let mut payload = Vec::with_capacity(
        cpu_id.len() + 1 + dialect_override.map(|value| value.len()).unwrap_or(0),
    );
    payload.extend_from_slice(cpu_id.as_bytes());
    payload.push(0);
    if let Some(dialect) = dialect_override {
        payload.extend_from_slice(dialect.as_bytes());
    }
    Ok(payload)
}

pub fn encode_wire_line_payload(line_num: u32, source_line: &str) -> Vec<u8> {
    let mut payload = Vec::with_capacity(4 + source_line.len());
    payload.extend_from_slice(&line_num.to_le_bytes());
    payload.extend_from_slice(source_line.as_bytes());
    payload
}

pub fn encode_wire_encode_instruction_payload(
    mnemonic: &str,
    candidates: &[VmEncodeCandidate],
) -> Result<Vec<u8>, String> {
    if mnemonic.is_empty() {
        return Err("native encode payload requires non-empty mnemonic".to_string());
    }
    let mut payload = Vec::new();
    push_u8_len(&mut payload, mnemonic.len(), "mnemonic length")?;
    payload.extend_from_slice(mnemonic.as_bytes());
    push_u8_len(&mut payload, candidates.len(), "candidate count")?;
    for candidate in candidates {
        push_u8_len(&mut payload, candidate.mode_key.len(), "mode key length")?;
        payload.extend_from_slice(candidate.mode_key.as_bytes());
        push_u8_len(
            &mut payload,
            candidate.operand_bytes.len(),
            "operand count per candidate",
        )?;
        for operand in &candidate.operand_bytes {
            push_u8_len(&mut payload, operand.len(), "operand byte length")?;
            payload.extend_from_slice(operand.as_slice());
        }
    }
    Ok(payload)
}

fn push_u8_len(payload: &mut Vec<u8>, len: usize, context: &str) -> Result<(), String> {
    let value =
        u8::try_from(len).map_err(|_| format!("native payload {} exceeds u8 range", context))?;
    payload.push(value);
    Ok(())
}

fn decode_wire_request(
    entrypoint: u8,
    payload: &[u8],
) -> Result<Native6502HarnessRequestOwned, String> {
    match entrypoint {
        NATIVE_6502_ENTRYPOINT_INIT_V1 => {
            if !payload.is_empty() {
                return Err("native init payload must be empty".to_string());
            }
            Ok(Native6502HarnessRequestOwned::Init)
        }
        NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1 => Ok(Native6502HarnessRequestOwned::LoadPackage {
            package_bytes: payload.to_vec(),
        }),
        NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1 => {
            let (cpu_id, dialect_override) = decode_wire_set_pipeline_payload(payload)?;
            Ok(Native6502HarnessRequestOwned::SetPipeline {
                cpu_id,
                dialect_override,
            })
        }
        NATIVE_6502_ENTRYPOINT_TOKENIZE_LINE_V1 => {
            let (line_num, source_line) = decode_wire_line_payload(payload)?;
            Ok(Native6502HarnessRequestOwned::TokenizeLine {
                source_line,
                line_num,
            })
        }
        NATIVE_6502_ENTRYPOINT_PARSE_LINE_V1 => {
            let (line_num, source_line) = decode_wire_line_payload(payload)?;
            Ok(Native6502HarnessRequestOwned::ParseLine {
                source_line,
                line_num,
            })
        }
        NATIVE_6502_ENTRYPOINT_ENCODE_INSTRUCTION_V1 => {
            let (mnemonic, candidates) = decode_wire_encode_instruction_payload(payload)?;
            Ok(Native6502HarnessRequestOwned::EncodeInstruction {
                mnemonic,
                candidates,
            })
        }
        NATIVE_6502_ENTRYPOINT_EVALUATE_EXPRESSION_V1 => {
            Err("native evaluate_expression wire payload requires assembler context".to_string())
        }
        NATIVE_6502_ENTRYPOINT_SELECT_INSTRUCTION_V1 => {
            Err("native select_instruction wire payload requires assembler context".to_string())
        }
        NATIVE_6502_ENTRYPOINT_ENCODE_SELECTED_INSTRUCTION_V1 => Err(
            "native encode_selected_instruction wire payload requires assembler context"
                .to_string(),
        ),
        NATIVE_6502_ENTRYPOINT_LAST_ERROR_V1 => {
            if !payload.is_empty() {
                return Err("native last_error payload must be empty".to_string());
            }
            Ok(Native6502HarnessRequestOwned::LastError)
        }
        _ => Err(format!(
            "unsupported native entrypoint ordinal {}",
            entrypoint
        )),
    }
}

fn decode_wire_set_pipeline_payload(payload: &[u8]) -> Result<(String, Option<String>), String> {
    let Some(cpu_sep) = payload.iter().position(|value| *value == 0u8) else {
        return Err("native set_pipeline payload is missing cpu separator".to_string());
    };
    let cpu_id = std::str::from_utf8(&payload[..cpu_sep])
        .map_err(|_| "native set_pipeline cpu_id is not UTF-8".to_string())?
        .trim()
        .to_string();
    if cpu_id.is_empty() {
        return Err("native set_pipeline payload has empty cpu_id".to_string());
    }
    let dialect_bytes = &payload[cpu_sep + 1..];
    let dialect_override = if dialect_bytes.is_empty() {
        None
    } else {
        let dialect = std::str::from_utf8(dialect_bytes)
            .map_err(|_| "native set_pipeline dialect_override is not UTF-8".to_string())?
            .trim()
            .to_string();
        if dialect.is_empty() {
            None
        } else {
            Some(dialect)
        }
    };
    Ok((cpu_id, dialect_override))
}

fn decode_wire_line_payload(payload: &[u8]) -> Result<(u32, String), String> {
    if payload.len() < 4 {
        return Err("native line payload must include 4-byte line number prefix".to_string());
    }
    let line_num = u32::from_le_bytes([payload[0], payload[1], payload[2], payload[3]]);
    let source_line = std::str::from_utf8(&payload[4..])
        .map_err(|_| "native line payload source is not UTF-8".to_string())?
        .to_string();
    Ok((line_num, source_line))
}

fn decode_wire_encode_instruction_payload(
    payload: &[u8],
) -> Result<(String, Vec<VmEncodeCandidate>), String> {
    let mut cursor = 0usize;
    let mnemonic_len = read_u8_len(payload, &mut cursor, "mnemonic length")?;
    let mnemonic = read_utf8(payload, &mut cursor, mnemonic_len, "mnemonic")?;
    if mnemonic.is_empty() {
        return Err("native encode payload mnemonic cannot be empty".to_string());
    }
    let candidate_count = read_u8_len(payload, &mut cursor, "candidate count")?;
    let mut candidates = Vec::with_capacity(candidate_count);
    for _ in 0..candidate_count {
        let mode_len = read_u8_len(payload, &mut cursor, "mode key length")?;
        let mode_key = read_utf8(payload, &mut cursor, mode_len, "mode key")?;
        let operand_count = read_u8_len(payload, &mut cursor, "operand count")?;
        let mut operand_bytes = Vec::with_capacity(operand_count);
        for _ in 0..operand_count {
            let operand_len = read_u8_len(payload, &mut cursor, "operand byte length")?;
            let operand = read_bytes(payload, &mut cursor, operand_len, "operand bytes")?;
            operand_bytes.push(operand);
        }
        candidates.push(VmEncodeCandidate {
            mode_key,
            operand_bytes,
        });
    }
    if cursor != payload.len() {
        return Err("native encode payload has trailing bytes".to_string());
    }
    Ok((mnemonic, candidates))
}

fn read_u8_len(payload: &[u8], cursor: &mut usize, context: &str) -> Result<usize, String> {
    if *cursor >= payload.len() {
        return Err(format!("native payload missing {}", context));
    }
    let value = payload[*cursor] as usize;
    *cursor = cursor.saturating_add(1);
    Ok(value)
}

fn read_bytes(
    payload: &[u8],
    cursor: &mut usize,
    len: usize,
    context: &str,
) -> Result<Vec<u8>, String> {
    if payload.len().saturating_sub(*cursor) < len {
        return Err(format!(
            "native payload truncated while reading {}",
            context
        ));
    }
    let start = *cursor;
    let end = start + len;
    *cursor = end;
    Ok(payload[start..end].to_vec())
}

fn read_utf8(
    payload: &[u8],
    cursor: &mut usize,
    len: usize,
    context: &str,
) -> Result<String, String> {
    let bytes = read_bytes(payload, cursor, len, context)?;
    String::from_utf8(bytes).map_err(|_| format!("native payload field '{}' is not UTF-8", context))
}

fn encode_wire_output_payload(output: &Native6502HarnessOutput) -> Vec<u8> {
    match output {
        Native6502HarnessOutput::None => Vec::new(),
        Native6502HarnessOutput::Tokens(tokens) => {
            let mut out = String::new();
            for token in tokens {
                out.push_str(
                    format!(
                        "{:?}@{}:{}-{}\n",
                        token.kind, token.span.line, token.span.col_start, token.span.col_end
                    )
                    .as_str(),
                );
            }
            out.into_bytes()
        }
        Native6502HarnessOutput::LineAst(ast) => format!("{ast:?}").into_bytes(),
        Native6502HarnessOutput::EncodedBytes(Some(bytes)) => bytes.clone(),
        Native6502HarnessOutput::EncodedBytes(None) => Vec::new(),
        Native6502HarnessOutput::ExpressionEvaluation(evaluation) => {
            encode_expression_evaluation_output(evaluation)
        }
        Native6502HarnessOutput::InstructionCandidates(candidates) => {
            encode_instruction_candidates_output(candidates)
        }
        Native6502HarnessOutput::ErrorMessage(message) => message.as_bytes().to_vec(),
    }
}

fn encode_expression_evaluation_output(evaluation: &NativePrvmHostExpressionEvaluation) -> Vec<u8> {
    match evaluation {
        NativePrvmHostExpressionEvaluation::Concrete { value } => {
            format!("VALUE {value}").into_bytes()
        }
        NativePrvmHostExpressionEvaluation::DeferredUnresolved { message } => {
            format!("DEFERRED {message}").into_bytes()
        }
    }
}

fn encode_instruction_candidates_output(candidates: &[VmEncodeCandidate]) -> Vec<u8> {
    let mut out = String::new();
    for candidate in candidates {
        out.push_str(candidate.mode_key.as_str());
        for operand in &candidate.operand_bytes {
            out.push(' ');
            for byte in operand {
                out.push_str(format!("{byte:02X}").as_str());
            }
        }
        out.push('\n');
    }
    out.into_bytes()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::builder::{
        build_hierarchy_chunks_from_registry, build_hierarchy_package_from_registry,
    };
    use crate::hierarchy::ScopedOwner;
    use families::mos6502::module::{M6502CpuModule, MOS6502FamilyModule};
    use package::encode_hierarchy_chunks_from_chunks;
    use registry::registry::ModuleRegistry;
    use std::collections::HashMap;
    use std::fs;
    use std::path::PathBuf;

    fn harness_fixture_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("..")
            .join("..")
            .join("examples")
            .join("vm")
            .join("harness-fixtures")
    }

    fn load_fixture(name: &str) -> String {
        let path = harness_fixture_dir().join(name);
        fs::read_to_string(path)
            .expect("fixture read")
            .trim()
            .to_string()
    }

    fn load_expected_failure_prefixes() -> HashMap<String, String> {
        let path = harness_fixture_dir().join("failure_expected_prefixes.tsv");
        let mut out = HashMap::new();
        let text = fs::read_to_string(path).expect("failure prefixes fixture");
        for line in text.lines() {
            let trimmed = line.trim();
            if trimmed.is_empty() || trimmed.starts_with('#') {
                continue;
            }
            let mut parts = trimmed.split_whitespace();
            let key = parts.next().unwrap_or_default().trim();
            let value = parts.next().unwrap_or_default().trim();
            if !key.is_empty() && !value.is_empty() {
                out.insert(key.to_string(), value.to_string());
            }
        }
        out
    }

    fn m6502_registry() -> ModuleRegistry {
        let mut registry = ModuleRegistry::new();
        registry.register_family(Box::new(MOS6502FamilyModule));
        registry.register_cpu(Box::new(M6502CpuModule));
        registry
    }

    fn constrained_tokenizer_package_bytes() -> Vec<u8> {
        let registry = m6502_registry();
        let mut chunks =
            build_hierarchy_chunks_from_registry(&registry).expect("hierarchy chunks build");
        for program in &mut chunks.tokenizer_vm_programs {
            if matches!(program.owner, ScopedOwner::Family(ref family) if family.eq_ignore_ascii_case("mos6502"))
            {
                program.limits.max_tokens_per_line = 1;
            }
        }
        encode_hierarchy_chunks_from_chunks(&chunks).expect("encode constrained chunks")
    }

    fn wire_pipeline_payload(cpu_id: &str, dialect_override: Option<&str>) -> Vec<u8> {
        encode_wire_set_pipeline_payload(cpu_id, dialect_override).expect("pipeline payload")
    }

    fn wire_line_payload(line_num: u32, source_line: &str) -> Vec<u8> {
        encode_wire_line_payload(line_num, source_line)
    }

    #[test]
    fn encode_wire_set_pipeline_payload_rejects_nul_bytes() {
        let cpu_nul = encode_wire_set_pipeline_payload("m65\0c02", None)
            .expect_err("cpu_id containing NUL should fail");
        assert!(cpu_nul.contains("cpu_id cannot contain NUL"));

        let dialect_nul = encode_wire_set_pipeline_payload("m6502", Some("zi\0log"))
            .expect_err("dialect containing NUL should fail");
        assert!(dialect_nul.contains("dialect_override cannot contain NUL"));
    }

    #[test]
    fn native6502_harness_smoke_executes_load_set_tokenize_parse_encode_flow() {
        let package_bytes =
            build_hierarchy_package_from_registry(&m6502_registry()).expect("package bytes build");
        let tokenize_line = load_fixture("smoke_success_line.txt");
        let parse_line = load_fixture("smoke_parse_line.txt");

        let mut harness = Native6502Harness::new();
        let mut cb = Native6502ControlBlockV1::new_v1();

        let init = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_INIT_V1,
            Native6502HarnessRequest::Init,
        );
        assert_eq!(init.status_code, NATIVE_6502_STATUS_OK_V1);
        assert_eq!(cb.status_code(), NATIVE_6502_STATUS_OK_V1);
        assert_eq!(
            cb.capability_flags(),
            NATIVE_6502_CAPABILITY_EXT_TLV_V1
                | NATIVE_6502_CAPABILITY_STRUCT_LAYOUTS_V1
                | NATIVE_6502_CAPABILITY_ENUM_TABLES_V1
        );

        let load = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
            Native6502HarnessRequest::LoadPackage {
                package_bytes: package_bytes.as_slice(),
            },
        );
        assert_eq!(load.status_code, NATIVE_6502_STATUS_OK_V1);
        assert_eq!(
            cb.input_len(),
            u16::try_from(package_bytes.len()).unwrap_or(u16::MAX)
        );

        let set_pipeline = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1,
            Native6502HarnessRequest::SetPipeline {
                cpu_id: "m6502",
                dialect_override: None,
            },
        );
        assert_eq!(set_pipeline.status_code, NATIVE_6502_STATUS_OK_V1);

        let tokenize = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_TOKENIZE_LINE_V1,
            Native6502HarnessRequest::TokenizeLine {
                source_line: tokenize_line.as_str(),
                line_num: 1,
            },
        );
        let tokens = match tokenize.output {
            Native6502HarnessOutput::Tokens(tokens) => tokens,
            other => panic!("unexpected tokenize output: {other:?}"),
        };
        assert!(tokens.len() >= 2, "expected non-empty token stream");

        let parse = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_PARSE_LINE_V1,
            Native6502HarnessRequest::ParseLine {
                source_line: parse_line.as_str(),
                line_num: 1,
            },
        );
        assert_eq!(
            parse.status_code, NATIVE_6502_STATUS_OK_V1,
            "parse output: {:?}",
            parse.output
        );
        let parsed = match parse.output {
            Native6502HarnessOutput::LineAst(ast) => ast,
            other => panic!("unexpected parse output: {other:?}"),
        };
        assert!(
            !matches!(parsed, PortableLineAst::Empty),
            "expected non-empty parsed AST"
        );

        let candidates = vec![VmEncodeCandidate {
            mode_key: "immediate".to_string(),
            operand_bytes: vec![vec![0x42]],
        }];
        let encode = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_ENCODE_INSTRUCTION_V1,
            Native6502HarnessRequest::EncodeInstruction {
                mnemonic: "LDA",
                candidates: candidates.as_slice(),
            },
        );
        let encoded = match encode.output {
            Native6502HarnessOutput::EncodedBytes(bytes) => bytes,
            other => panic!("unexpected encode output: {other:?}"),
        };
        assert_eq!(encoded, Some(vec![0xA9, 0x42]));
        assert_eq!(cb.output_len(), 2);
    }

    #[test]
    fn native6502_harness_wire_smoke_executes_process_boundary_payload_flow() {
        let package_bytes =
            build_hierarchy_package_from_registry(&m6502_registry()).expect("package bytes build");
        let tokenize_line = load_fixture("smoke_success_line.txt");
        let parse_line = load_fixture("smoke_parse_line.txt");
        let encode_candidates = vec![VmEncodeCandidate {
            mode_key: "immediate".to_string(),
            operand_bytes: vec![vec![0x2A]],
        }];

        let mut harness = Native6502Harness::new();
        let mut cb = Native6502ControlBlockV1::new_v1();

        let init = harness.invoke_wire_v1(&mut cb, NATIVE_6502_ENTRYPOINT_INIT_V1, &[]);
        assert_eq!(init.status_code, NATIVE_6502_STATUS_OK_V1);
        assert!(init.output_payload.is_empty());

        let load = harness.invoke_wire_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
            package_bytes.as_slice(),
        );
        assert_eq!(load.status_code, NATIVE_6502_STATUS_OK_V1);

        let set_pipeline = harness.invoke_wire_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1,
            wire_pipeline_payload("m6502", None).as_slice(),
        );
        assert_eq!(set_pipeline.status_code, NATIVE_6502_STATUS_OK_V1);

        let tokenize = harness.invoke_wire_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_TOKENIZE_LINE_V1,
            wire_line_payload(1, tokenize_line.as_str()).as_slice(),
        );
        assert_eq!(tokenize.status_code, NATIVE_6502_STATUS_OK_V1);
        assert!(!tokenize.output_payload.is_empty());

        let parse = harness.invoke_wire_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_PARSE_LINE_V1,
            wire_line_payload(1, parse_line.as_str()).as_slice(),
        );
        assert_eq!(parse.status_code, NATIVE_6502_STATUS_OK_V1);
        assert!(!parse.output_payload.is_empty());

        let encode_payload =
            encode_wire_encode_instruction_payload("LDA", encode_candidates.as_slice())
                .expect("encode payload");
        let encode = harness.invoke_wire_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_ENCODE_INSTRUCTION_V1,
            encode_payload.as_slice(),
        );
        assert_eq!(encode.status_code, NATIVE_6502_STATUS_OK_V1);
        assert_eq!(encode.output_payload, vec![0xA9, 0x2A]);
        assert_eq!(cb.output_len(), 2);
    }

    #[test]
    fn native6502_harness_wire_reports_bad_request_for_malformed_payload() {
        let mut harness = Native6502Harness::new();
        let mut cb = Native6502ControlBlockV1::new_v1();
        let response = harness.invoke_wire_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_PARSE_LINE_V1,
            &[0x01, 0x00, 0x00],
        );
        assert_eq!(response.status_code, NATIVE_6502_STATUS_BAD_REQUEST_V1);
        let message = String::from_utf8(response.output_payload).expect("utf8 error message");
        assert!(message.contains("line number prefix"));
        assert_eq!(cb.status_code(), NATIVE_6502_STATUS_BAD_REQUEST_V1);
        assert!(cb.last_error_len() > 0);
    }

    #[test]
    fn native6502_harness_wire_rejects_encode_payload_trailing_bytes() {
        let mut harness = Native6502Harness::new();
        let mut cb = Native6502ControlBlockV1::new_v1();

        let mut payload = encode_wire_encode_instruction_payload(
            "LDA",
            &[VmEncodeCandidate {
                mode_key: "immediate".to_string(),
                operand_bytes: vec![vec![0x42]],
            }],
        )
        .expect("encode payload");
        payload.push(0xFF);

        let response = harness.invoke_wire_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_ENCODE_INSTRUCTION_V1,
            payload.as_slice(),
        );
        assert_eq!(response.status_code, NATIVE_6502_STATUS_BAD_REQUEST_V1);
        let message = String::from_utf8(response.output_payload).expect("utf8 error message");
        assert!(message.contains("trailing bytes"));
    }

    #[test]
    fn native6502_harness_wire_last_error_round_trips_runtime_failure_message() {
        let package_bytes =
            build_hierarchy_package_from_registry(&m6502_registry()).expect("package bytes build");

        let mut harness = Native6502Harness::new();
        let mut cb = Native6502ControlBlockV1::new_v1();
        let _ = harness.invoke_wire_v1(&mut cb, NATIVE_6502_ENTRYPOINT_INIT_V1, &[]);
        let _ = harness.invoke_wire_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
            package_bytes.as_slice(),
        );
        let encode_payload = encode_wire_encode_instruction_payload(
            "LDA",
            &[VmEncodeCandidate {
                mode_key: "immediate".to_string(),
                operand_bytes: vec![vec![0x42]],
            }],
        )
        .expect("encode payload");
        let encode = harness.invoke_wire_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_ENCODE_INSTRUCTION_V1,
            encode_payload.as_slice(),
        );
        assert_eq!(encode.status_code, NATIVE_6502_STATUS_RUNTIME_ERROR_V1);
        assert!(String::from_utf8_lossy(encode.output_payload.as_slice()).contains("OTR001"));

        let last_error = harness.invoke_wire_v1(&mut cb, NATIVE_6502_ENTRYPOINT_LAST_ERROR_V1, &[]);
        assert_eq!(last_error.status_code, NATIVE_6502_STATUS_OK_V1);
        let message = String::from_utf8(last_error.output_payload).expect("utf8 last_error");
        assert!(message.contains("OTR001"));
    }

    #[test]
    fn native6502_harness_fixture_shakeout_covers_opc_otr_ott_otp_failures() {
        let expected = load_expected_failure_prefixes();
        let tokenizer_line = load_fixture("tokenizer_failure_line.txt");
        let parser_line = load_fixture("parser_failure_line.txt");

        let mut harness = Native6502Harness::new();
        let mut cb = Native6502ControlBlockV1::new_v1();
        let _ = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_INIT_V1,
            Native6502HarnessRequest::Init,
        );

        let opc = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
            Native6502HarnessRequest::LoadPackage {
                package_bytes: b"not-an-opasm",
            },
        );
        let opc_error = match opc.output {
            Native6502HarnessOutput::ErrorMessage(message) => message,
            other => panic!("expected OPC failure payload, got {other:?}"),
        };
        assert!(opc_error.contains(expected.get("opc").expect("opc prefix").as_str()));

        let package_bytes =
            build_hierarchy_package_from_registry(&m6502_registry()).expect("package bytes build");
        let _ = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
            Native6502HarnessRequest::LoadPackage {
                package_bytes: package_bytes.as_slice(),
            },
        );

        let otr = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_ENCODE_INSTRUCTION_V1,
            Native6502HarnessRequest::EncodeInstruction {
                mnemonic: "LDA",
                candidates: &[VmEncodeCandidate {
                    mode_key: "immediate".to_string(),
                    operand_bytes: vec![vec![0x42]],
                }],
            },
        );
        let otr_error = match otr.output {
            Native6502HarnessOutput::ErrorMessage(message) => message,
            other => panic!("expected OTR failure payload, got {other:?}"),
        };
        assert!(otr_error.contains(expected.get("otr").expect("otr prefix").as_str()));

        let constrained_bytes = constrained_tokenizer_package_bytes();
        let _ = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
            Native6502HarnessRequest::LoadPackage {
                package_bytes: constrained_bytes.as_slice(),
            },
        );
        let _ = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1,
            Native6502HarnessRequest::SetPipeline {
                cpu_id: "m6502",
                dialect_override: None,
            },
        );
        let ott = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_TOKENIZE_LINE_V1,
            Native6502HarnessRequest::TokenizeLine {
                source_line: tokenizer_line.as_str(),
                line_num: 1,
            },
        );
        let ott_error = match ott.output {
            Native6502HarnessOutput::ErrorMessage(message) => message,
            other => panic!("expected ott failure payload, got {other:?}"),
        };
        assert!(ott_error
            .to_ascii_lowercase()
            .contains(expected.get("ott").expect("ott prefix").as_str()));

        let _ = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
            Native6502HarnessRequest::LoadPackage {
                package_bytes: package_bytes.as_slice(),
            },
        );
        let _ = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1,
            Native6502HarnessRequest::SetPipeline {
                cpu_id: "m6502",
                dialect_override: None,
            },
        );
        let otp = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_PARSE_LINE_V1,
            Native6502HarnessRequest::ParseLine {
                source_line: parser_line.as_str(),
                line_num: 1,
            },
        );
        let otp_error = match otp.output {
            Native6502HarnessOutput::ErrorMessage(message) => message,
            other => panic!("expected otp failure payload, got {other:?}"),
        };
        assert!(otp_error
            .to_ascii_lowercase()
            .contains(expected.get("otp").expect("otp prefix").as_str()));
    }

    #[test]
    fn native6502_harness_rejects_mismatched_entrypoint_payload_pairs() {
        let mut harness = Native6502Harness::new();
        let mut cb = Native6502ControlBlockV1::new_v1();
        let response = harness.invoke_v1(
            &mut cb,
            NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1,
            Native6502HarnessRequest::LastError,
        );
        assert_eq!(response.status_code, NATIVE_6502_STATUS_RUNTIME_ERROR_V1);
        let message = match response.output {
            Native6502HarnessOutput::ErrorMessage(message) => message,
            other => panic!("unexpected error payload: {other:?}"),
        };
        assert!(message.contains("mismatched request payload"));
    }
}
