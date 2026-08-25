; Shared native VM service ABI constants for the AmigaOS tkpkg scaffold.

	.module tkpkg.amigaos.abi
	.cpu 68020
	.pub

NATIVE_ABI_MAGIC_0                   = 'O'
NATIVE_ABI_MAGIC_1                   = 'T'
NATIVE_ABI_MAGIC_2                   = '6'
NATIVE_ABI_MAGIC_3                   = '5'
NATIVE_ABI_VERSION_V1                = 1
NATIVE_CONTROL_BLOCK_SIZE_V1         = 32

CB_MAGIC                             = 0
CB_ABI_VERSION                       = 4
CB_STRUCT_SIZE                       = 6
CB_CAPABILITY_FLAGS                  = 8
CB_STATUS_CODE                       = 10
CB_REQUEST_ID                        = 12
CB_RESERVED0                         = 14
CB_INPUT_PTR                         = 16
CB_INPUT_LEN                         = 18
CB_OUTPUT_PTR                        = 20
CB_OUTPUT_LEN                        = 22
CB_EXTENSION_PTR                     = 24
CB_EXTENSION_LEN                     = 26
CB_LAST_ERROR_PTR                    = 28
CB_LAST_ERROR_LEN                    = 30

CAPABILITY_EXT_TLV_V1                = 1
CAPABILITY_STRUCT_LAYOUTS_V1         = 2
CAPABILITY_ENUM_TABLES_V1            = 4
CAPABILITY_FLAGS_V1                  = 7

ENTRY_ORD_INIT                       = 0
ENTRY_ORD_LOAD_PACKAGE               = 1
ENTRY_ORD_SET_PIPELINE               = 2
ENTRY_ORD_TOKENIZE_LINE              = 3
ENTRY_ORD_PARSE_LINE                 = 4
ENTRY_ORD_ENCODE_INSTRUCTION         = 5
ENTRY_ORD_LAST_ERROR                 = 6
ENTRY_ORD_EVALUATE_EXPRESSION        = 7
ENTRY_ORD_SELECT_INSTRUCTION         = 8
ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION = 9
ENTRY_ORD_EXECUTE_OPERAND_RECORD     = 10
ENTRY_ORD_COUNT_V1                   = 11

OPERAND_RECORD_REQUEST_VERSION_V1    = 1
OPERAND_RECORD_RESULT_VERSION_V1     = 1
OPERAND_RECORD_RESULT_SIZE_V1        = 24
OPERAND_RECORD_RESULT_VERSION        = 0
OPERAND_RECORD_RESULT_KIND           = 1
OPERAND_RECORD_RESULT_VARIANT        = 2
OPERAND_RECORD_RESULT_PRIMARY_CLASS  = 4
OPERAND_RECORD_RESULT_PRIMARY_INDEX  = 6
OPERAND_RECORD_RESULT_SECONDARY_CLASS = 8
OPERAND_RECORD_RESULT_SECONDARY_INDEX = 10
OPERAND_RECORD_RESULT_VALUE          = 12
OPERAND_RECORD_RESULT_WIDTH          = 20
OPERAND_RECORD_RESULT_SCALE          = 21

STATUS_OK_V1                         = 0
STATUS_BAD_CONTROL_BLOCK_V1          = 1
STATUS_BAD_REQUEST_V1                = 2
STATUS_RUNTIME_ERROR_V1              = 3

SET_PIPELINE_PAYLOAD_SEPARATOR       = 0
LINE_PAYLOAD_LINE_NUM_SIZE           = 4
LAST_ERROR_REQUEST_LEN               = 0
SET_PIPELINE_SAMPLE_CPU_LEN          = 5
SET_PIPELINE_SAMPLE_DIALECT_LEN      = 7
TOKENIZE_LINE_SAMPLE_LINE_NUM        = 42
PARSE_LINE_SAMPLE_LINE_NUM           = 5
EVALUATE_EXPRESSION_SAMPLE_LINE_NUM  = 7
EVALUATE_EXPRESSION_SAMPLE_START_COL = 5
EVALUATE_EXPRESSION_SAMPLE_END_COL   = 11
EVALUATE_EXPRESSION_SAMPLE_MNEM_LEN  = 3
ENCODE_INSTRUCTION_SAMPLE_MNEM_LEN   = 3
ENCODE_INSTRUCTION_SAMPLE_CAND_COUNT = 1
ENCODE_INSTRUCTION_SAMPLE_MODE_LEN   = 9
ENCODE_INSTRUCTION_SAMPLE_OPER_COUNT = 1
ENCODE_INSTRUCTION_SAMPLE_OPER_LEN   = 1

	.section data, kind=data

AbiMarker
	.byte "OPFORGE-TKPKG-ABI-V1", 0

WireContractMarker
	.byte "TKPKG-WIRE-CONTRACT-V1", 0

WireSetPipelineExample
	.byte "68020", 0, "amigaos"

WireTokenizeLineExample
	.byte 42, 0, 0, 0
	.byte "move.b d0,d1"

WireParseLineExample
	.byte 5, 0, 0, 0
	.byte "start:  lda #$42"

WireEvaluateExpressionExample
	.byte 7, 0, 0, 0
	.byte 5, 0
	.byte 11, 0
	.byte 3, "LDA"
	.byte "LDA #$2A+1"

WireEncodeInstructionExample
	.byte 3, "LDA"
	.byte 1
	.byte 9, "immediate"
	.byte 1
	.byte 1, $42

	.endsection
	.endmodule
