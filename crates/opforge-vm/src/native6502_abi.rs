// SPDX-License-Identifier: GPL-3.0-or-later

//! Stable host/runtime ABI constants for 6502-native integrations.

/// VM v1 native host ABI marker for 6502-class integrations.
pub const NATIVE_6502_ABI_MAGIC_V1: [u8; 4] = *b"OT65";
pub const NATIVE_6502_ABI_VERSION_V1: u16 = 0x0001;

/// Fixed control-block size for the 6502-native host ABI v1 envelope.
pub const NATIVE_6502_CONTROL_BLOCK_SIZE_V1: u16 = 32;

pub const NATIVE_6502_CB_MAGIC_OFFSET: usize = 0;
pub const NATIVE_6502_CB_ABI_VERSION_OFFSET: usize = 4;
pub const NATIVE_6502_CB_STRUCT_SIZE_OFFSET: usize = 6;
pub const NATIVE_6502_CB_CAPABILITY_FLAGS_OFFSET: usize = 8;
pub const NATIVE_6502_CB_STATUS_CODE_OFFSET: usize = 10;
pub const NATIVE_6502_CB_REQUEST_ID_OFFSET: usize = 12;
pub const NATIVE_6502_CB_RESERVED0_OFFSET: usize = 14;
pub const NATIVE_6502_CB_INPUT_PTR_OFFSET: usize = 16;
pub const NATIVE_6502_CB_INPUT_LEN_OFFSET: usize = 18;
pub const NATIVE_6502_CB_OUTPUT_PTR_OFFSET: usize = 20;
pub const NATIVE_6502_CB_OUTPUT_LEN_OFFSET: usize = 22;
pub const NATIVE_6502_CB_EXTENSION_PTR_OFFSET: usize = 24;
pub const NATIVE_6502_CB_EXTENSION_LEN_OFFSET: usize = 26;
pub const NATIVE_6502_CB_LAST_ERROR_PTR_OFFSET: usize = 28;
pub const NATIVE_6502_CB_LAST_ERROR_LEN_OFFSET: usize = 30;

/// Capability bits for forward-compatible native ABI growth.
pub const NATIVE_6502_CAPABILITY_EXT_TLV_V1: u16 = 1 << 0;
pub const NATIVE_6502_CAPABILITY_STRUCT_LAYOUTS_V1: u16 = 1 << 1;
pub const NATIVE_6502_CAPABILITY_ENUM_TABLES_V1: u16 = 1 << 2;

/// Stable jump-table ordinals for 6502-native host runtimes.
pub const NATIVE_6502_ENTRYPOINT_INIT_V1: u8 = 0;
pub const NATIVE_6502_ENTRYPOINT_LOAD_PACKAGE_V1: u8 = 1;
pub const NATIVE_6502_ENTRYPOINT_SET_PIPELINE_V1: u8 = 2;
pub const NATIVE_6502_ENTRYPOINT_TOKENIZE_LINE_V1: u8 = 3;
pub const NATIVE_6502_ENTRYPOINT_PARSE_LINE_V1: u8 = 4;
pub const NATIVE_6502_ENTRYPOINT_ENCODE_INSTRUCTION_V1: u8 = 5;
pub const NATIVE_6502_ENTRYPOINT_LAST_ERROR_V1: u8 = 6;
pub const NATIVE_6502_ENTRYPOINT_EVALUATE_EXPRESSION_V1: u8 = 7;
pub const NATIVE_6502_ENTRYPOINT_SELECT_INSTRUCTION_V1: u8 = 8;
pub const NATIVE_6502_ENTRYPOINT_COUNT_V1: u8 = 9;
