; Owned runtime buffers for the first tkpkg native ABI slice.

	.module tkpkg.amigaos.buffers
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi (NATIVE_CONTROL_BLOCK_SIZE_V1)

LAST_ERROR_BUFFER_CAPACITY           = 4096
LAST_ERROR_BUFFER_PTR_V1             = NATIVE_CONTROL_BLOCK_SIZE_V1
PACKAGE_STORAGE_CAPACITY             = 32768
PIPELINE_ID_BUFFER_CAPACITY          = 32
TOKEN_RECORD_SIZE                    = 20
TOKEN_BUFFER_CAPACITY                = 64
TOKEN_SCRATCH_CAPACITY               = 256
TOKENIZER_VM_STATE_TABLE_CAPACITY    = 32
TOKENIZER_VM_DIAG_CODE_CAPACITY      = 32
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
PACKAGE_REQUIRED_CHUNK_FLAGS         = 31
PACKAGE_STATE_CLEAR_LONGWORD_COUNT   = 40
SCOPED_OWNER_FAMILY                  = 0
SCOPED_OWNER_CPU                     = 1
SCOPED_OWNER_DIALECT                 = 2
BAD_REQUEST_TEXT_LEN                 = 19
CONTROL_BLOCK_ERROR_TEXT_LEN         = 20
RUNTIME_ERROR_TEXT_LEN               = 20
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
	.res byte, NATIVE_CONTROL_BLOCK_SIZE_V1

LastErrorBuffer
	.res byte, LAST_ERROR_BUFFER_CAPACITY

PackageStorage
	.res byte, PACKAGE_STORAGE_CAPACITY

TokenRecordBuffer
	.res byte, TOKEN_RECORD_SIZE * TOKEN_BUFFER_CAPACITY

TokenScratchBuffer
	.res byte, TOKEN_SCRATCH_CAPACITY

LastTokenCount
	.res word, 1

LastLexemeLen
	.res word, 1

	.align 2
PackageStateFlags
	.res byte, 1

PackageChunkFlags
	.res byte, 1

PackageStorageLen
	.res byte, 1

PackageStorageLenHi
	.res byte, 1

FamsChunkOffsetLo
	.res byte, 1

FamsChunkOffsetHi
	.res byte, 1

FamsChunkLenLo
	.res byte, 1

FamsChunkLenHi
	.res byte, 1

CpusChunkOffsetLo
	.res byte, 1

CpusChunkOffsetHi
	.res byte, 1

CpusChunkLenLo
	.res byte, 1

CpusChunkLenHi
	.res byte, 1

DialChunkOffsetLo
	.res byte, 1

DialChunkOffsetHi
	.res byte, 1

DialChunkLenLo
	.res byte, 1

DialChunkLenHi
	.res byte, 1

ToksChunkOffsetLo
	.res byte, 1

ToksChunkOffsetHi
	.res byte, 1

ToksChunkLenLo
	.res byte, 1

ToksChunkLenHi
	.res byte, 1

TkvmChunkOffsetLo
	.res byte, 1

TkvmChunkOffsetHi
	.res byte, 1

TkvmChunkLenLo
	.res byte, 1

TkvmChunkLenHi
	.res byte, 1

TablChunkOffsetLo
	.res byte, 1

TablChunkOffsetHi
	.res byte, 1

TablChunkLenLo
	.res byte, 1

TablChunkLenHi
	.res byte, 1

ExprChunkOffsetLo
	.res byte, 1

ExprChunkOffsetHi
	.res byte, 1

ExprChunkLenLo
	.res byte, 1

ExprChunkLenHi
	.res byte, 1

ExvmChunkOffsetLo
	.res byte, 1

ExvmChunkOffsetHi
	.res byte, 1

ExvmChunkLenLo
	.res byte, 1

ExvmChunkLenHi
	.res byte, 1

ActiveCpuBuffer
	.res byte, PIPELINE_ID_BUFFER_CAPACITY

ActiveDialectBuffer
	.res byte, PIPELINE_ID_BUFFER_CAPACITY

ActiveFamilyBuffer
	.res byte, PIPELINE_ID_BUFFER_CAPACITY

	.align 2
ActiveTokenPolicyOffsetLo
	.res byte, 1

ActiveTokenPolicyOffsetHi
	.res byte, 1

ActiveTokenPolicyLenLo
	.res byte, 1

ActiveTokenPolicyLenHi
	.res byte, 1

ActiveTokenPolicyOwnerTag
	.res byte, 1

	.align 2
ActiveTokenizerVmOffsetLo
	.res byte, 1

ActiveTokenizerVmOffsetHi
	.res byte, 1

ActiveTokenizerVmLenLo
	.res byte, 1

ActiveTokenizerVmLenHi
	.res byte, 1

ActiveTokenizerVmOwnerTag
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

PendingFamilyOffsetHi
	.res byte, 1

PendingFamilyLenLo
	.res byte, 1

PendingFamilyLenHi
	.res byte, 1

PendingCpuOffsetLo
	.res byte, 1

PendingCpuOffsetHi
	.res byte, 1

PendingCpuLenLo
	.res byte, 1

PendingCpuLenHi
	.res byte, 1

PendingDialectOffsetLo
	.res byte, 1

PendingDialectOffsetHi
	.res byte, 1

PendingDialectLenLo
	.res byte, 1

PendingDialectLenHi
	.res byte, 1

PendingDefaultDialectOffsetLo
	.res byte, 1

PendingDefaultDialectOffsetHi
	.res byte, 1

PendingDefaultDialectLenLo
	.res byte, 1

PendingDefaultDialectLenHi
	.res byte, 1

PendingCanonicalDialectOffsetLo
	.res byte, 1

PendingCanonicalDialectOffsetHi
	.res byte, 1

PendingCanonicalDialectLenLo
	.res byte, 1

PendingCanonicalDialectLenHi
	.res byte, 1

PendingTokenPolicyOffsetLo
	.res byte, 1

PendingTokenPolicyOffsetHi
	.res byte, 1

PendingTokenPolicyLenLo
	.res byte, 1

PendingTokenPolicyLenHi
	.res byte, 1

PendingTokenPolicyOwnerTag
	.res byte, 1

	.align 2
PendingTokenizerVmOffsetLo
	.res byte, 1

PendingTokenizerVmOffsetHi
	.res byte, 1

PendingTokenizerVmLenLo
	.res byte, 1

PendingTokenizerVmLenHi
	.res byte, 1

PendingTokenizerVmOwnerTag
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

	.endsection
	.endmodule
