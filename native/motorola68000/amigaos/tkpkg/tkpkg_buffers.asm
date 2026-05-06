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

runtimeStageMarker:
        .byte "TKPKG-SLICE-1", 0

badRequestText:
        .byte "OTR002: bad request", 0

controlBlockErrorText:
        .byte "OTR003: bad control", 0

runtimeErrorText:
        .byte "OTR901: unimplemented", 0

        .endsection

        .section bss, kind=bss

controlBlockV1:
        .res byte, NATIVE_CONTROL_BLOCK_SIZE_V1

lastErrorBuffer:
        .res byte, LAST_ERROR_BUFFER_CAPACITY

packageStorage:
        .res byte, PACKAGE_STORAGE_CAPACITY

tokenRecordBuffer:
        .res byte, TOKEN_RECORD_SIZE * TOKEN_BUFFER_CAPACITY

tokenScratchBuffer:
        .res byte, TOKEN_SCRATCH_CAPACITY

lastTokenCount:
        .res word, 1

lastLexemeLen:
        .res word, 1

        .align 2
packageStateFlags:
        .res byte, 1

packageChunkFlags:
        .res byte, 1

packageStorageLen:
        .res byte, 1

packageStorageLenHi:
        .res byte, 1

famsChunkOffsetLo:
        .res byte, 1

famsChunkOffsetHi:
        .res byte, 1

famsChunkLenLo:
        .res byte, 1

famsChunkLenHi:
        .res byte, 1

cpusChunkOffsetLo:
        .res byte, 1

cpusChunkOffsetHi:
        .res byte, 1

cpusChunkLenLo:
        .res byte, 1

cpusChunkLenHi:
        .res byte, 1

dialChunkOffsetLo:
        .res byte, 1

dialChunkOffsetHi:
        .res byte, 1

dialChunkLenLo:
        .res byte, 1

dialChunkLenHi:
        .res byte, 1

toksChunkOffsetLo:
        .res byte, 1

toksChunkOffsetHi:
        .res byte, 1

toksChunkLenLo:
        .res byte, 1

toksChunkLenHi:
        .res byte, 1

tkvmChunkOffsetLo:
        .res byte, 1

tkvmChunkOffsetHi:
        .res byte, 1

tkvmChunkLenLo:
        .res byte, 1

tkvmChunkLenHi:
        .res byte, 1

tablChunkOffsetLo:
        .res byte, 1

tablChunkOffsetHi:
        .res byte, 1

tablChunkLenLo:
        .res byte, 1

tablChunkLenHi:
        .res byte, 1

activeCpuBuffer:
        .res byte, PIPELINE_ID_BUFFER_CAPACITY

activeDialectBuffer:
        .res byte, PIPELINE_ID_BUFFER_CAPACITY

activeFamilyBuffer:
        .res byte, PIPELINE_ID_BUFFER_CAPACITY

        .align 2
activeTokenPolicyOffsetLo:
        .res byte, 1

activeTokenPolicyOffsetHi:
        .res byte, 1

activeTokenPolicyLenLo:
        .res byte, 1

activeTokenPolicyLenHi:
        .res byte, 1

activeTokenPolicyOwnerTag:
        .res byte, 1

        .align 2
activeTokenizerVmOffsetLo:
        .res byte, 1

activeTokenizerVmOffsetHi:
        .res byte, 1

activeTokenizerVmLenLo:
        .res byte, 1

activeTokenizerVmLenHi:
        .res byte, 1

activeTokenizerVmOwnerTag:
        .res byte, 1

activeTokenizerVmStartStateLo:
        .res byte, 1

activeTokenizerVmStartStateHi:
        .res byte, 1

activeTokenizerVmStateCountLo:
        .res byte, 1

activeTokenizerVmStateCountHi:
        .res byte, 1

activeTokenizerVmStateTable:
        .res byte, TOKENIZER_VM_STATE_TABLE_CAPACITY * 4

activeTokenizerVmMaxErrorsPerLine:
        .res byte, 4

activeTokenizerVmInvalidCharDiagLen:
        .res byte, 1

activeTokenizerVmInvalidCharDiagCode:
        .res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

activeTokenizerVmUnterminatedStringDiagLen:
        .res byte, 1

activeTokenizerVmUnterminatedStringDiagCode:
        .res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

activeTokenizerVmStepLimitDiagLen:
        .res byte, 1

activeTokenizerVmStepLimitDiagCode:
        .res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

activeTokenizerVmTokenLimitDiagLen:
        .res byte, 1

activeTokenizerVmTokenLimitDiagCode:
        .res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

activeTokenizerVmLexemeLimitDiagLen:
        .res byte, 1

activeTokenizerVmLexemeLimitDiagCode:
        .res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

activeTokenizerVmErrorLimitDiagLen:
        .res byte, 1

activeTokenizerVmErrorLimitDiagCode:
        .res byte, TOKENIZER_VM_DIAG_CODE_CAPACITY

        .align 2
pendingFamilyOffsetLo:
        .res byte, 1

pendingFamilyOffsetHi:
        .res byte, 1

pendingFamilyLenLo:
        .res byte, 1

pendingFamilyLenHi:
        .res byte, 1

pendingCpuOffsetLo:
        .res byte, 1

pendingCpuOffsetHi:
        .res byte, 1

pendingCpuLenLo:
        .res byte, 1

pendingCpuLenHi:
        .res byte, 1

pendingDialectOffsetLo:
        .res byte, 1

pendingDialectOffsetHi:
        .res byte, 1

pendingDialectLenLo:
        .res byte, 1

pendingDialectLenHi:
        .res byte, 1

pendingDefaultDialectOffsetLo:
        .res byte, 1

pendingDefaultDialectOffsetHi:
        .res byte, 1

pendingDefaultDialectLenLo:
        .res byte, 1

pendingDefaultDialectLenHi:
        .res byte, 1

pendingCanonicalDialectOffsetLo:
        .res byte, 1

pendingCanonicalDialectOffsetHi:
        .res byte, 1

pendingCanonicalDialectLenLo:
        .res byte, 1

pendingCanonicalDialectLenHi:
        .res byte, 1

pendingTokenPolicyOffsetLo:
        .res byte, 1

pendingTokenPolicyOffsetHi:
        .res byte, 1

pendingTokenPolicyLenLo:
        .res byte, 1

pendingTokenPolicyLenHi:
        .res byte, 1

pendingTokenPolicyOwnerTag:
        .res byte, 1

        .align 2
pendingTokenizerVmOffsetLo:
        .res byte, 1

pendingTokenizerVmOffsetHi:
        .res byte, 1

pendingTokenizerVmLenLo:
        .res byte, 1

pendingTokenizerVmLenHi:
        .res byte, 1

pendingTokenizerVmOwnerTag:
        .res byte, 1

storedLastErrorLen:
        .res byte, 1

storedLastErrorLenHi:
        .res byte, 1

storedLastErrorKind:
        .res byte, 1

nextRequestIdLo:
        .res byte, 1

nextRequestIdHi:
        .res byte, 1

        .endsection
        .endmodule
