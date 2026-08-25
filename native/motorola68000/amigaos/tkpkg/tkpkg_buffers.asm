; Owned runtime buffers for the first tkpkg native ABI slice.

	.module tkpkg.amigaos.buffers
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi

LAST_ERROR_BUFFER_CAPACITY           = 4096
OPERAND_RECORD_RESULT_CAPACITY       = abi.OPERAND_RECORD_RESULT_SIZE_V1
LAST_ERROR_BUFFER_PTR_V1             = abi.NATIVE_CONTROL_BLOCK_SIZE_V1
PACKAGE_STORAGE_CAPACITY             = 393216
PIPELINE_ID_BUFFER_CAPACITY          = 32
TOKEN_RECORD_SIZE                    = 20
TOKEN_BUFFER_CAPACITY                = 64
TOKEN_SCRATCH_CAPACITY               = 256
TOKENIZER_VM_STATE_TABLE_CAPACITY    = 32
TOKENIZER_VM_DIAG_CODE_CAPACITY      = 32
COMPACT_SELECTOR_TEXT_CAPACITY       = 64
COMPACT_STRING_SCRATCH_CAPACITY      = LAST_ERROR_BUFFER_CAPACITY
PACKAGE_STATE_LOADED                 = 1
PACKAGE_STATE_PIPELINE_ACTIVE        = 2
PACKAGE_CHUNK_FAMS                   = 1
PACKAGE_CHUNK_CPUS                   = 2
PACKAGE_CHUNK_DIAL                   = 4
PACKAGE_CHUNK_TOKS                   = 8
PACKAGE_CHUNK_TKVM                   = 16
PACKAGE_CHUNK_TABL                   = 32
PACKAGE_CHUNK_EXPR                   = 64
PACKAGE_CHUNK_EXVM                   = 128
PACKAGE_CHUNK_MSEL                   = 1
PACKAGE_CHUNK_PRVM                   = 2
PACKAGE_CHUNK_CTBL                   = 4
PACKAGE_CHUNK_CSEM                   = 8
PACKAGE_CHUNK_CMSE                   = 16
PACKAGE_CHUNK_RENC                   = 32
PACKAGE_CHUNK_VALP                   = 64
PACKAGE_CHUNK_CPRD                   = 128
PACKAGE_REQUIRED_CHUNK_FLAGS         = 31
SCOPED_OWNER_FAMILY                  = 0
SCOPED_OWNER_CPU                     = 1
SCOPED_OWNER_DIALECT                 = 2
BAD_REQUEST_TEXT_LEN                 = 19
CONTROL_BLOCK_ERROR_TEXT_LEN         = 20
RUNTIME_ERROR_TEXT_LEN               = 21
LAST_ERROR_KIND_NONE                 = 0
LAST_ERROR_KIND_BAD_REQUEST          = 1
LAST_ERROR_KIND_BAD_CONTROL          = 2
LAST_ERROR_KIND_RUNTIME              = 3

	.section data, kind=data

RuntimeStageMarker
	.byte "TKPKG-SLICE-1", 0

BadRequestText
	.byte "OTR002: bad request", 0

ControlBlockErrorText
	.byte "OTR003: bad control", 0

RuntimeErrorText
	.byte "OTR901: unimplemented", 0

	.endsection

	.section bss, kind=bss

ControlBlockV1
	.res byte, abi.NATIVE_CONTROL_BLOCK_SIZE_V1

LastErrorBuffer
	.res byte, LAST_ERROR_BUFFER_CAPACITY

OperandRecordResultBuffer
	.res byte, OPERAND_RECORD_RESULT_CAPACITY

OPERAND_RECORD_RESULT_BUFFER_PTR_V1 = OperandRecordResultBuffer - ControlBlockV1

TokenRecordBuffer
	.res byte, TOKEN_RECORD_SIZE * TOKEN_BUFFER_CAPACITY

TokenScratchBuffer
	.res byte, TOKEN_SCRATCH_CAPACITY

CompactStringScratchBuffer
	.res byte, COMPACT_STRING_SCRATCH_CAPACITY

LastTokenCount
	.res word, 1

LastLexemeLen
	.res word, 1

	.align 2
PackageStateClearStart
PackageStateFlags
	.res byte, 1

PackageChunkFlags
	.res byte, 1

PackageChunkFlagsHi
	.res byte, 1

PackageChunkFlagsExtra
	.res byte, 1

PackageStorageLen
	.res byte, 1

PackageStorageLenMidLo
	.res byte, 1

PackageStorageLenMidHi
	.res byte, 1

PackageStorageLenHi
	.res byte, 1

FamsChunkOffsetLo
	.res byte, 1

FamsChunkOffsetMidLo
	.res byte, 1

FamsChunkOffsetMidHi
	.res byte, 1

FamsChunkOffsetHi
	.res byte, 1

FamsChunkLenLo
	.res byte, 1

FamsChunkLenMidLo
	.res byte, 1

FamsChunkLenMidHi
	.res byte, 1

FamsChunkLenHi
	.res byte, 1

CpusChunkOffsetLo
	.res byte, 1

CpusChunkOffsetMidLo
	.res byte, 1

CpusChunkOffsetMidHi
	.res byte, 1

CpusChunkOffsetHi
	.res byte, 1

CpusChunkLenLo
	.res byte, 1

CpusChunkLenMidLo
	.res byte, 1

CpusChunkLenMidHi
	.res byte, 1

CpusChunkLenHi
	.res byte, 1

CalsChunkOffsetLo
	.res byte, 1

CalsChunkOffsetMidLo
	.res byte, 1

CalsChunkOffsetMidHi
	.res byte, 1

CalsChunkOffsetHi
	.res byte, 1

CalsChunkLenLo
	.res byte, 1

CalsChunkLenMidLo
	.res byte, 1

CalsChunkLenMidHi
	.res byte, 1

CalsChunkLenHi
	.res byte, 1

DialChunkOffsetLo
	.res byte, 1

DialChunkOffsetMidLo
	.res byte, 1

DialChunkOffsetMidHi
	.res byte, 1

DialChunkOffsetHi
	.res byte, 1

DialChunkLenLo
	.res byte, 1

DialChunkLenMidLo
	.res byte, 1

DialChunkLenMidHi
	.res byte, 1

DialChunkLenHi
	.res byte, 1

ToksChunkOffsetLo
	.res byte, 1

ToksChunkOffsetMidLo
	.res byte, 1

ToksChunkOffsetMidHi
	.res byte, 1

ToksChunkOffsetHi
	.res byte, 1

ToksChunkLenLo
	.res byte, 1

ToksChunkLenMidLo
	.res byte, 1

ToksChunkLenMidHi
	.res byte, 1

ToksChunkLenHi
	.res byte, 1

TkvmChunkOffsetLo
	.res byte, 1

TkvmChunkOffsetMidLo
	.res byte, 1

TkvmChunkOffsetMidHi
	.res byte, 1

TkvmChunkOffsetHi
	.res byte, 1

TkvmChunkLenLo
	.res byte, 1

TkvmChunkLenMidLo
	.res byte, 1

TkvmChunkLenMidHi
	.res byte, 1

TkvmChunkLenHi
	.res byte, 1

TablChunkOffsetLo
	.res byte, 1

TablChunkOffsetMidLo
	.res byte, 1

TablChunkOffsetMidHi
	.res byte, 1

TablChunkOffsetHi
	.res byte, 1

TablChunkLenLo
	.res byte, 1

TablChunkLenMidLo
	.res byte, 1

TablChunkLenMidHi
	.res byte, 1

TablChunkLenHi
	.res byte, 1

MselChunkOffsetLo
	.res byte, 1

MselChunkOffsetMidLo
	.res byte, 1

MselChunkOffsetMidHi
	.res byte, 1

MselChunkOffsetHi
	.res byte, 1

MselChunkLenLo
	.res byte, 1

MselChunkLenMidLo
	.res byte, 1

MselChunkLenMidHi
	.res byte, 1

MselChunkLenHi
	.res byte, 1

ExprChunkOffsetLo
	.res byte, 1

ExprChunkOffsetMidLo
	.res byte, 1

ExprChunkOffsetMidHi
	.res byte, 1

ExprChunkOffsetHi
	.res byte, 1

ExprChunkLenLo
	.res byte, 1

ExprChunkLenMidLo
	.res byte, 1

ExprChunkLenMidHi
	.res byte, 1

ExprChunkLenHi
	.res byte, 1

ExvmChunkOffsetLo
	.res byte, 1

ExvmChunkOffsetMidLo
	.res byte, 1

ExvmChunkOffsetMidHi
	.res byte, 1

ExvmChunkOffsetHi
	.res byte, 1

ExvmChunkLenLo
	.res byte, 1

ExvmChunkLenMidLo
	.res byte, 1

ExvmChunkLenMidHi
	.res byte, 1

ExvmChunkLenHi
	.res byte, 1

PrvmChunkOffsetLo
	.res byte, 1

PrvmChunkOffsetMidLo
	.res byte, 1

PrvmChunkOffsetMidHi
	.res byte, 1

PrvmChunkOffsetHi
	.res byte, 1

PrvmChunkLenLo
	.res byte, 1

PrvmChunkLenMidLo
	.res byte, 1

PrvmChunkLenMidHi
	.res byte, 1

PrvmChunkLenHi
	.res byte, 1

CtblChunkOffsetLo
	.res byte, 1

CtblChunkOffsetMidLo
	.res byte, 1

CtblChunkOffsetMidHi
	.res byte, 1

CtblChunkOffsetHi
	.res byte, 1

CtblChunkLenLo
	.res byte, 1

CtblChunkLenMidLo
	.res byte, 1

CtblChunkLenMidHi
	.res byte, 1

CtblChunkLenHi
	.res byte, 1

CsemChunkOffsetLo
	.res byte, 1

CsemChunkOffsetMidLo
	.res byte, 1

CsemChunkOffsetMidHi
	.res byte, 1

CsemChunkOffsetHi
	.res byte, 1

CsemChunkLenLo
	.res byte, 1

CsemChunkLenMidLo
	.res byte, 1

CsemChunkLenMidHi
	.res byte, 1

CsemChunkLenHi
	.res byte, 1

CmseChunkOffsetLo
	.res byte, 1

CmseChunkOffsetMidLo
	.res byte, 1

CmseChunkOffsetMidHi
	.res byte, 1

CmseChunkOffsetHi
	.res byte, 1

CmseChunkLenLo
	.res byte, 1

CmseChunkLenMidLo
	.res byte, 1

CmseChunkLenMidHi
	.res byte, 1

CmseChunkLenHi
	.res byte, 1

RencChunkOffsetLo
	.res byte, 1

RencChunkOffsetMidLo
	.res byte, 1

RencChunkOffsetMidHi
	.res byte, 1

RencChunkOffsetHi
	.res byte, 1

RencChunkLenLo
	.res byte, 1

RencChunkLenMidLo
	.res byte, 1

RencChunkLenMidHi
	.res byte, 1

RencChunkLenHi
	.res byte, 1

ValpChunkOffsetLo
	.res byte, 1

ValpChunkOffsetMidLo
	.res byte, 1

ValpChunkOffsetMidHi
	.res byte, 1

ValpChunkOffsetHi
	.res byte, 1

ValpChunkLenLo
	.res byte, 1

ValpChunkLenMidLo
	.res byte, 1

ValpChunkLenMidHi
	.res byte, 1

ValpChunkLenHi
	.res byte, 1

CprdChunkOffsetLo
	.res byte, 1

CprdChunkOffsetMidLo
	.res byte, 1

CprdChunkOffsetMidHi
	.res byte, 1

CprdChunkOffsetHi
	.res byte, 1

CprdChunkLenLo
	.res byte, 1

CprdChunkLenMidLo
	.res byte, 1

CprdChunkLenMidHi
	.res byte, 1

CprdChunkLenHi
	.res byte, 1

	.align 2
CompactSelectorStringsPtr
	.res long, 1

CompactSelectorChunkEndPtr
	.res long, 1

CompactSelectorStringCount
	.res word, 1

CompactSelectorMnemonicText
	.res byte, COMPACT_SELECTOR_TEXT_CAPACITY

CompactSelectorShapeText
	.res byte, COMPACT_SELECTOR_TEXT_CAPACITY

CompactSelectorModeText
	.res byte, COMPACT_SELECTOR_TEXT_CAPACITY

CompactSelectorPlanText
	.res byte, COMPACT_SELECTOR_TEXT_CAPACITY

CompactSelectorInputText
	.res byte, COMPACT_SELECTOR_TEXT_CAPACITY

	.align 2
SemanticInputRecordPtr
	.res long, 1
SemanticInputRecordCount
	.res word, 1
SemanticFirstInputLen
	.res word, 1

ActiveCpuBuffer
	.res byte, PIPELINE_ID_BUFFER_CAPACITY

ActiveDialectBuffer
	.res byte, PIPELINE_ID_BUFFER_CAPACITY

ActiveFamilyBuffer
	.res byte, PIPELINE_ID_BUFFER_CAPACITY

	.align 2
ActiveTokenPolicyOffsetLo
	.res byte, 1

ActiveTokenPolicyOffsetMidLo
	.res byte, 1

ActiveTokenPolicyOffsetMidHi
	.res byte, 1

ActiveTokenPolicyOffsetHi
	.res byte, 1

ActiveTokenPolicyLenLo
	.res byte, 1

ActiveTokenPolicyLenMidLo
	.res byte, 1

ActiveTokenPolicyLenMidHi
	.res byte, 1

ActiveTokenPolicyLenHi
	.res byte, 1

ActiveTokenPolicyOwnerTag
	.res byte, 1

	.align 2
ActiveTokenizerVmOffsetLo
	.res byte, 1

ActiveTokenizerVmOffsetMidLo
	.res byte, 1

ActiveTokenizerVmOffsetMidHi
	.res byte, 1

ActiveTokenizerVmOffsetHi
	.res byte, 1

ActiveTokenizerVmLenLo
	.res byte, 1

ActiveTokenizerVmLenMidLo
	.res byte, 1

ActiveTokenizerVmLenMidHi
	.res byte, 1

ActiveTokenizerVmLenHi
	.res byte, 1

ActiveTokenizerVmOwnerTag
	.res byte, 1

	.align 2
ActiveParserVmOffsetLo
	.res byte, 1

ActiveParserVmOffsetMidLo
	.res byte, 1

ActiveParserVmOffsetMidHi
	.res byte, 1

ActiveParserVmOffsetHi
	.res byte, 1

ActiveParserVmLenLo
	.res byte, 1

ActiveParserVmLenMidLo
	.res byte, 1

ActiveParserVmLenMidHi
	.res byte, 1

ActiveParserVmLenHi
	.res byte, 1

ActiveParserVmOwnerTag
	.res byte, 1

ActiveTokenizerVmStartStateLo
	.res byte, 1

ActiveTokenizerVmStartStateHi
	.res byte, 1

ActiveTokenizerVmStateCountLo
	.res byte, 1

ActiveTokenizerVmStateCountHi
	.res byte, 1

ActiveTokenizerVmStateTable
	.res byte, TOKENIZER_VM_STATE_TABLE_CAPACITY * 4

ActiveTokenizerVmMaxErrorsPerLine
	.res byte, 4

ActiveTokenizerVmInvalidCharDiagLen
	.res byte, 1

ActiveTokenizerVmInvalidCharDiagCode
	.res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

ActiveTokenizerVmUnterminatedStringDiagLen
	.res byte, 1

ActiveTokenizerVmUnterminatedStringDiagCode
	.res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

ActiveTokenizerVmStepLimitDiagLen
	.res byte, 1

ActiveTokenizerVmStepLimitDiagCode
	.res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

ActiveTokenizerVmTokenLimitDiagLen
	.res byte, 1

ActiveTokenizerVmTokenLimitDiagCode
	.res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

ActiveTokenizerVmLexemeLimitDiagLen
	.res byte, 1

ActiveTokenizerVmLexemeLimitDiagCode
	.res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

ActiveTokenizerVmErrorLimitDiagLen
	.res byte, 1

ActiveTokenizerVmErrorLimitDiagCode
	.res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

	.align 2
PendingFamilyOffsetLo
	.res byte, 1

PendingFamilyOffsetMidLo
	.res byte, 1

PendingFamilyOffsetMidHi
	.res byte, 1

PendingFamilyOffsetHi
	.res byte, 1

PendingFamilyLenLo
	.res byte, 1

PendingFamilyLenMidLo
	.res byte, 1

PendingFamilyLenMidHi
	.res byte, 1

PendingFamilyLenHi
	.res byte, 1

PendingCpuOffsetLo
	.res byte, 1

PendingCpuOffsetMidLo
	.res byte, 1

PendingCpuOffsetMidHi
	.res byte, 1

PendingCpuOffsetHi
	.res byte, 1

PendingCpuLenLo
	.res byte, 1

PendingCpuLenMidLo
	.res byte, 1

PendingCpuLenMidHi
	.res byte, 1

PendingCpuLenHi
	.res byte, 1

PendingDialectOffsetLo
	.res byte, 1

PendingDialectOffsetMidLo
	.res byte, 1

PendingDialectOffsetMidHi
	.res byte, 1

PendingDialectOffsetHi
	.res byte, 1

PendingDialectLenLo
	.res byte, 1

PendingDialectLenMidLo
	.res byte, 1

PendingDialectLenMidHi
	.res byte, 1

PendingDialectLenHi
	.res byte, 1

PendingDefaultDialectOffsetLo
	.res byte, 1

PendingDefaultDialectOffsetMidLo
	.res byte, 1

PendingDefaultDialectOffsetMidHi
	.res byte, 1

PendingDefaultDialectOffsetHi
	.res byte, 1

PendingDefaultDialectLenLo
	.res byte, 1

PendingDefaultDialectLenMidLo
	.res byte, 1

PendingDefaultDialectLenMidHi
	.res byte, 1

PendingDefaultDialectLenHi
	.res byte, 1

PendingCanonicalDialectOffsetLo
	.res byte, 1

PendingCanonicalDialectOffsetMidLo
	.res byte, 1

PendingCanonicalDialectOffsetMidHi
	.res byte, 1

PendingCanonicalDialectOffsetHi
	.res byte, 1

PendingCanonicalDialectLenLo
	.res byte, 1

PendingCanonicalDialectLenMidLo
	.res byte, 1

PendingCanonicalDialectLenMidHi
	.res byte, 1

PendingCanonicalDialectLenHi
	.res byte, 1

PendingTokenPolicyOffsetLo
	.res byte, 1

PendingTokenPolicyOffsetMidLo
	.res byte, 1

PendingTokenPolicyOffsetMidHi
	.res byte, 1

PendingTokenPolicyOffsetHi
	.res byte, 1

PendingTokenPolicyLenLo
	.res byte, 1

PendingTokenPolicyLenMidLo
	.res byte, 1

PendingTokenPolicyLenMidHi
	.res byte, 1

PendingTokenPolicyLenHi
	.res byte, 1

PendingTokenPolicyOwnerTag
	.res byte, 1

	.align 2
PendingTokenizerVmOffsetLo
	.res byte, 1

PendingTokenizerVmOffsetMidLo
	.res byte, 1

PendingTokenizerVmOffsetMidHi
	.res byte, 1

PendingTokenizerVmOffsetHi
	.res byte, 1

PendingTokenizerVmLenLo
	.res byte, 1

PendingTokenizerVmLenMidLo
	.res byte, 1

PendingTokenizerVmLenMidHi
	.res byte, 1

PendingTokenizerVmLenHi
	.res byte, 1

PendingTokenizerVmOwnerTag
	.res byte, 1

	.align 2
PendingParserVmOffsetLo
	.res byte, 1

PendingParserVmOffsetMidLo
	.res byte, 1

PendingParserVmOffsetMidHi
	.res byte, 1

PendingParserVmOffsetHi
	.res byte, 1

PendingParserVmLenLo
	.res byte, 1

PendingParserVmLenMidLo
	.res byte, 1

PendingParserVmLenMidHi
	.res byte, 1

PendingParserVmLenHi
	.res byte, 1

PendingParserVmOwnerTag
	.res byte, 1

StoredLastErrorLen
	.res byte, 1

StoredLastErrorLenHi
	.res byte, 1

StoredLastErrorKind
	.res byte, 1

NextRequestIdLo
	.res byte, 1

NextRequestIdHi
	.res byte, 1

PACKAGE_STATE_CLEAR_END

PACKAGE_STATE_CLEAR_BYTE_COUNT = PACKAGE_STATE_CLEAR_END - PackageStateClearStart

; The public v1 service ABI carries 16-bit control-block-relative pointers.
; Keep every request/result buffer before this large allocation, while keeping
; PackageStorage's starting address inside that window. The package can then
; extend beyond 64 KiB without pushing later ABI buffers out of range.
PackageStorage
	.res byte, PACKAGE_STORAGE_CAPACITY

PACKAGE_STORAGE_PTR_V1 = PackageStorage - ControlBlockV1

	.endsection
	.endmodule
