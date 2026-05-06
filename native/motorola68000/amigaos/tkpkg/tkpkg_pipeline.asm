; Package-backed pipeline-resolution module for the tkpkg native runtime.

        .module tkpkg.amigaos.pipeline
        .cpu 68020
        .pub
        .use tkpkg.amigaos.abi (CB_INPUT_PTR, CB_INPUT_LEN, STATUS_BAD_REQUEST_V1)
        .use tkpkg.amigaos.abi (STATUS_RUNTIME_ERROR_V1)
        .use tkpkg.amigaos.buffers (PACKAGE_STATE_LOADED, PACKAGE_STATE_PIPELINE_ACTIVE)
        .use tkpkg.amigaos.buffers (PIPELINE_ID_BUFFER_CAPACITY, SCOPED_OWNER_DIALECT)
        .use tkpkg.amigaos.buffers (SCOPED_OWNER_CPU, SCOPED_OWNER_FAMILY)
        .use tkpkg.amigaos.buffers (packageStateFlags, packageStorage, famsChunkOffsetLo)
        .use tkpkg.amigaos.buffers (cpusChunkOffsetLo, dialChunkOffsetLo)
        .use tkpkg.amigaos.buffers (tkvmChunkOffsetLo, activeCpuBuffer)
        .use tkpkg.amigaos.buffers (activeDialectBuffer, activeFamilyBuffer)
        .use tkpkg.amigaos.buffers (activeTokenPolicyOffsetLo, activeTokenPolicyOwnerTag)
        .use tkpkg.amigaos.buffers (activeTokenizerVmOffsetLo, activeTokenizerVmOwnerTag)
        .use tkpkg.amigaos.buffers (pendingFamilyOffsetLo, pendingCpuOffsetLo)
        .use tkpkg.amigaos.buffers (pendingDialectOffsetLo, pendingDefaultDialectOffsetLo)
        .use tkpkg.amigaos.buffers (pendingCanonicalDialectOffsetLo)
        .use tkpkg.amigaos.buffers (pendingTokenPolicyOffsetLo)
        .use tkpkg.amigaos.buffers (pendingTokenPolicyOwnerTag)
        .use tkpkg.amigaos.buffers (pendingTokenizerVmOffsetLo)
        .use tkpkg.amigaos.buffers (pendingTokenizerVmOwnerTag)
        .use tkpkg.amigaos.token_policy (tkpkg_token_policy_resolve_locator_v1)

NO_PACKAGE_TEXT_LEN                  = 41
UNRESOLVED_CPU_TEXT_LEN              = 33
UNRESOLVED_FAMILY_TEXT_LEN           = 33
UNRESOLVED_DIALECT_TEXT_LEN          = 34
MISSING_PROGRAM_TEXT_LEN             = 36
IDENTIFIER_TOO_LONG_TEXT_LEN         = 35
TOKENIZER_VM_ENTRY_PREFIX_SIZE      = 4
TOKENIZER_VM_ENTRY_FIXED_TAIL_SIZE  = 19

        .section data, kind=data

noPackageText:
        .byte "OTR001: set_pipeline requires load_package", 0

unresolvedCpuText:
        .byte "OTR004: unresolved package cpu id", 0

unresolvedFamilyText:
        .byte "OTR004: unresolved package family", 0

unresolvedDialectText:
        .byte "OTR004: unresolved package dialect", 0

missingProgramText:
        .byte "OTR001: missing tokenizer VM program", 0

identifierTooLongText:
        .byte "OTR004: package identifier too long", 0

        .endsection

        .section code, kind=code

tkpkg_pipeline_set_active_v1:
        BTST #0, packageStateFlags
        BNE.S tkpkgPipelineParseRequest
        LEA noPackageText, A1
        MOVEQ #NO_PACKAGE_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkgPipelineParseRequest:
        BSR.W tkpkg_pipeline_parse_request_v1
        TST.B D0
        BNE.W tkpkgPipelineDone
        BSR.W tkpkg_pipeline_resolve_hierarchy_v1
        TST.B D0
        BNE.W tkpkgPipelineDone
        BSR.W tkpkg_token_policy_resolve_locator_v1
        TST.B D0
        BNE.W tkpkgPipelineDone
        BSR.W tkpkg_pipeline_resolve_tokenizer_vm_locator_v1
        TST.B D0
        BNE.W tkpkgPipelineDone
        BSR.W tkpkg_pipeline_commit_active_selection_v1
        TST.B D0
        BNE.W tkpkgPipelineDone
        MOVEQ #0, D0

tkpkgPipelineDone:
        RTS

tkpkg_pipeline_parse_request_v1:
        LEA pendingFamilyOffsetLo, A3
        MOVEQ #29, D0

tkpkgPipelineClearPendingLoop:
        CLR.B (A3)+
        DBF D0, tkpkgPipelineClearPendingLoop
        MOVEQ #0, D0
        MOVE.B CB_INPUT_LEN(A0), D0
        MOVEQ #0, D1
        MOVE.B 19(A0), D1
        LSL.W #8, D1
        OR.W D1, D0
        CMPI.W #2, D0
        BLO.W tkpkgPipelineBadRequest
        MOVE.W D0, D6
        MOVEQ #0, D1
        MOVE.B CB_INPUT_PTR(A0), D1
        MOVEQ #0, D2
        MOVE.B 17(A0), D2
        LSL.W #8, D2
        OR.W D2, D1
        TST.W D1
        BEQ.W tkpkgPipelineBadRequest
        LEA 0(A0, D1.W), A1
        MOVEQ #0, D3
        MOVE.W D6, D4

tkpkgPipelineSeparatorLoop:
        TST.W D4
        BEQ.W tkpkgPipelineBadRequest
        TST.B 0(A1, D3.W)
        BEQ.S tkpkgPipelineSeparatorFound
        ADDQ.W #1, D3
        SUBQ.W #1, D4
        BRA.W tkpkgPipelineSeparatorLoop

tkpkgPipelineSeparatorFound:
        TST.W D3
        BEQ.W tkpkgPipelineBadRequest
        MOVE.W D1, D4
        MOVE.W D3, D5
        LEA pendingCpuOffsetLo, A3
        MOVE.B D4, (A3)+
        LSR.W #8, D4
        MOVE.B D4, (A3)+
        MOVE.B D5, (A3)+
        LSR.W #8, D5
        MOVE.B D5, (A3)+
        MOVE.W D6, D0
        MOVE.W D3, D1
        SUB.W D1, D0
        SUBQ.W #1, D0
        BEQ.S tkpkgPipelineNoDialect
        LEA pendingDialectOffsetLo, A3
        MOVE.W D4, D2
        LSL.W #8, D2
        MOVEQ #0, D5
        MOVE.B pendingCpuOffsetLo, D5
        OR.W D5, D2
        ADD.W D1, D2
        ADDQ.W #1, D2
        MOVE.B D2, (A3)+
        LSR.W #8, D2
        MOVE.B D2, (A3)+
        MOVE.B D0, (A3)+
        LSR.W #8, D0
        MOVE.B D0, (A3)+
        MOVEQ #0, D0
        RTS

tkpkgPipelineNoDialect:
        LEA pendingDialectOffsetLo, A3
        CLR.L (A3)
        MOVEQ #0, D0
        RTS

tkpkgPipelineBadRequest:
        MOVEQ #STATUS_BAD_REQUEST_V1, D0
        RTS

tkpkg_pipeline_resolve_hierarchy_v1:
        BSR.W tkpkg_pipeline_find_cpu_entry_v1
        TST.B D0
        BNE.W tkpkgPipelineCpuUnresolved
        BSR.W tkpkg_pipeline_find_family_entry_v1
        TST.B D0
        BNE.W tkpkgPipelineFamilyUnresolved
        BSR.W tkpkg_pipeline_resolve_selected_dialect_v1
        TST.B D0
        BNE.W tkpkgPipelineDialectUnresolved
        MOVEQ #0, D0
        RTS

tkpkgPipelineCpuUnresolved:
        LEA unresolvedCpuText, A1
        MOVEQ #UNRESOLVED_CPU_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkgPipelineFamilyUnresolved:
        LEA unresolvedFamilyText, A1
        MOVEQ #UNRESOLVED_FAMILY_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkgPipelineDialectUnresolved:
        LEA unresolvedDialectText, A1
        MOVEQ #UNRESOLVED_DIALECT_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkg_pipeline_find_cpu_entry_v1:
        LEA pendingCpuOffsetLo, A3
        BSR.W tkpkg_pipeline_read_request_locator_ptr_len_v1
        MOVE.W D3, D5
        MOVEA.L A1, A5
        LEA cpusChunkOffsetLo, A3
        BSR.W tkpkg_pipeline_chunk_ptr_from_locator_v1
        BSR.W tkpkg_pipeline_read_u32_le_low16_v1
        TST.B D1
        BNE.W tkpkgPipelineCpuMissing
        TST.W D0
        BEQ.W tkpkgPipelineCpuMissing
        MOVE.W D0, D7
        SUBQ.W #1, D7
        LEA 4(A2), A2

tkpkgPipelineCpuLoop:
        BSR.W tkpkg_pipeline_locate_string_v1
        TST.B D1
        BNE.W tkpkgPipelineCpuMissing
        MOVE.W D0, D6
        MOVEA.L A1, A4
        MOVE.L A2, -(SP)
        MOVE.W D6, D0
        MOVE.W D5, D1
        MOVEA.L A4, A1
        MOVEA.L A5, A2
        BSR.W tkpkg_pipeline_string_eq_ascii_casefold_v1
        MOVEA.L (SP)+, A2
        TST.B D0
        BEQ.W tkpkgPipelineSkipCpuEntry
        LEA pendingCpuOffsetLo, A3
        MOVEA.L A4, A1
        MOVE.W D6, D0
        BSR.W tkpkg_pipeline_store_package_string_locator_v1
        BSR.W tkpkg_pipeline_locate_string_v1
        TST.B D1
        BNE.W tkpkgPipelineCpuMissing
        LEA pendingFamilyOffsetLo, A3
        BSR.W tkpkg_pipeline_store_package_string_locator_v1
        BSR.W tkpkg_pipeline_locate_optional_string_v1
        TST.B D1
        BNE.W tkpkgPipelineCpuMissing
        LEA pendingDefaultDialectOffsetLo, A3
        BSR.W tkpkg_pipeline_store_optional_package_string_locator_v1
        MOVEQ #0, D0
        RTS

tkpkgPipelineSkipCpuEntry:
        BSR.W tkpkg_pipeline_skip_string_v1
        TST.B D1
        BNE.W tkpkgPipelineCpuMissing
        BSR.W tkpkg_pipeline_skip_optional_string_v1
        TST.B D1
        BNE.W tkpkgPipelineCpuMissing
        DBF D7, tkpkgPipelineCpuLoop

tkpkgPipelineCpuMissing:
        MOVEQ #1, D0
        RTS

tkpkg_pipeline_find_family_entry_v1:
        LEA packageStorage, A6
        LEA pendingFamilyOffsetLo, A3
        BSR.W tkpkg_pipeline_read_locator_ptr_len_v1
        MOVE.W D3, D5
        MOVEA.L A1, A5
        LEA famsChunkOffsetLo, A3
        BSR.W tkpkg_pipeline_chunk_ptr_from_locator_v1
        BSR.W tkpkg_pipeline_read_u32_le_low16_v1
        TST.B D1
        BNE.W tkpkgPipelineFamilyMissing
        TST.W D0
        BEQ.W tkpkgPipelineFamilyMissing
        MOVE.W D0, D7
        SUBQ.W #1, D7
        LEA 4(A2), A2

tkpkgPipelineFamilyLoop:
        BSR.W tkpkg_pipeline_locate_string_v1
        TST.B D1
        BNE.W tkpkgPipelineFamilyMissing
        MOVE.W D0, D6
        MOVEA.L A1, A4
        MOVE.L A2, -(SP)
        MOVE.W D6, D0
        MOVE.W D5, D1
        MOVEA.L A4, A1
        MOVEA.L A5, A2
        BSR.W tkpkg_pipeline_string_eq_ascii_casefold_v1
        MOVEA.L (SP)+, A2
        TST.B D0
        BEQ.W tkpkgPipelineSkipFamilyEntry
        BSR.W tkpkg_pipeline_locate_string_v1
        TST.B D1
        BNE.W tkpkgPipelineFamilyMissing
        LEA pendingCanonicalDialectOffsetLo, A3
        BSR.W tkpkg_pipeline_store_package_string_locator_v1
        MOVEQ #0, D0
        RTS

tkpkgPipelineSkipFamilyEntry:
        BSR.W tkpkg_pipeline_skip_string_v1
        TST.B D1
        BNE.W tkpkgPipelineFamilyMissing
        DBF D7, tkpkgPipelineFamilyLoop

tkpkgPipelineFamilyMissing:
        MOVEQ #1, D0
        RTS

tkpkg_pipeline_resolve_selected_dialect_v1:
        LEA pendingDialectOffsetLo, A3
        BSR.W tkpkg_pipeline_read_locator_ptr_len_v1
        TST.W D3
        BEQ.S tkpkgPipelineDefaultDialect
        LEA pendingDialectOffsetLo, A3
        BSR.W tkpkg_pipeline_find_requested_dialect_entry_v1
        TST.B D0
        BEQ.S tkpkgPipelineDialectDone
        MOVEQ #1, D0
        RTS

tkpkgPipelineDefaultDialect:
        LEA pendingDefaultDialectOffsetLo, A3
        BSR.W tkpkg_pipeline_read_locator_ptr_len_v1
        TST.W D3
        BEQ.S tkpkgPipelineCanonicalDialect
        LEA pendingDefaultDialectOffsetLo, A3
        BSR.W tkpkg_pipeline_find_dialect_entry_v1
        TST.B D0
        BEQ.S tkpkgPipelineDialectDone

tkpkgPipelineCanonicalDialect:
        LEA pendingCanonicalDialectOffsetLo, A3
        BSR.W tkpkg_pipeline_find_dialect_entry_v1
        TST.B D0
        BNE.S tkpkgPipelineDialectMissing

tkpkgPipelineDialectDone:
        MOVEQ #0, D0
        RTS

tkpkgPipelineDialectMissing:
        MOVEQ #1, D0
        RTS

tkpkg_pipeline_find_requested_dialect_entry_v1:
        BSR.W tkpkg_pipeline_read_request_locator_ptr_len_v1
        BRA.S tkpkgPipelineFindDialectEntryLoaded

tkpkg_pipeline_find_dialect_entry_v1:
        BSR.W tkpkg_pipeline_read_locator_ptr_len_v1

tkpkgPipelineFindDialectEntryLoaded:
        MOVE.W D3, D5
        MOVEA.L A1, A5
        LEA pendingFamilyOffsetLo, A3
        BSR.W tkpkg_pipeline_read_locator_ptr_len_v1
        MOVE.W D3, D6
        MOVEA.L A1, A4
        LEA dialChunkOffsetLo, A3
        BSR.W tkpkg_pipeline_chunk_ptr_from_locator_v1
        BSR.W tkpkg_pipeline_read_u32_le_low16_v1
        TST.B D1
        BNE.W tkpkgPipelineDialectNotFound
        TST.W D0
        BEQ.W tkpkgPipelineDialectNotFound
        MOVE.W D0, D7
        SUBQ.W #1, D7
        LEA 4(A2), A2

tkpkgPipelineDialectLoop:
        BSR.W tkpkg_pipeline_locate_string_v1
        TST.B D1
        BNE.W tkpkgPipelineDialectNotFound
        MOVE.W D0, -(SP)
        MOVEA.L A1, A0
        MOVE.L A2, -(SP)
        MOVE.W 4(SP), D0
        MOVE.W D5, D1
        MOVEA.L A0, A1
        MOVEA.L A5, A2
        BSR.W tkpkg_pipeline_string_eq_ascii_casefold_v1
        MOVEA.L (SP)+, A2
        TST.B D0
        BEQ.W tkpkgPipelineSkipDialectEntry
        BSR.W tkpkg_pipeline_locate_string_v1
        TST.B D1
        BEQ.S tkpkgPipelineDialectFamilyLoaded
        ADDQ.W #2, SP
        BRA.W tkpkgPipelineDialectNotFound

tkpkgPipelineDialectFamilyLoaded:
        MOVE.W D0, D2
        MOVE.L A2, -(SP)
        MOVE.W D2, D0
        MOVE.W D6, D1
        MOVEA.L A4, A2
        BSR.W tkpkg_pipeline_string_eq_ascii_casefold_v1
        MOVEA.L (SP)+, A2
        TST.B D0
        BEQ.W tkpkgPipelineSkipDialectAllowList
        MOVE.W D2, -(SP)
        BSR.W tkpkg_pipeline_dialect_allows_cpu_v1
        MOVE.W (SP)+, D2
        TST.B D0
        BEQ.W tkpkgPipelineDialectAccept
        ADDQ.W #2, SP
        BRA.W tkpkgPipelineDialectNext

tkpkgPipelineSkipDialectEntry:
        ADDQ.W #2, SP
        BSR.W tkpkg_pipeline_skip_string_v1
        BRA.S tkpkgPipelineSkipDialectAllowListPayload

tkpkgPipelineSkipDialectAllowList:
        ADDQ.W #2, SP

tkpkgPipelineSkipDialectAllowListPayload:
        BSR.W tkpkg_pipeline_skip_optional_string_list_v1
        TST.B D1
        BNE.W tkpkgPipelineDialectNotFound

tkpkgPipelineDialectNext:
        DBF D7, tkpkgPipelineDialectLoop

tkpkgPipelineDialectNotFound:
        MOVEQ #1, D0
        RTS

tkpkgPipelineDialectAccept:
        LEA pendingDialectOffsetLo, A3
        MOVEA.L A0, A1
        MOVE.W (SP)+, D0
        BSR.W tkpkg_pipeline_store_package_string_locator_v1
        MOVEQ #0, D0
        RTS

tkpkg_pipeline_dialect_allows_cpu_v1:
        MOVE.W D7, -(SP)
        MOVEQ #1, D0
        BSR.W tkpkg_pipeline_require_bytes_v1
        TST.B D1
        BNE.S tkpkgPipelineDialectRejected
        MOVE.B (A2)+, D0
        BEQ.S tkpkgPipelineDialectAllowed
        BSR.W tkpkg_pipeline_read_u32_le_low16_v1
        TST.B D1
        BNE.S tkpkgPipelineDialectRejected
        MOVE.W D0, D7
        LEA 4(A2), A2
        TST.W D7
        BEQ.S tkpkgPipelineDialectRejected
        MOVE.L A6, -(SP)
        LEA pendingCpuOffsetLo, A3
        BSR.W tkpkg_pipeline_read_locator_ptr_len_v1
        MOVEA.L (SP)+, A6
        MOVE.W D3, D5
        MOVEA.L A1, A5
        SUBQ.W #1, D7

tkpkgPipelineAllowLoop:
        BSR.W tkpkg_pipeline_locate_string_v1
        TST.B D1
        BNE.S tkpkgPipelineDialectRejected
        MOVE.W D0, D6
        MOVEA.L A1, A4
        MOVE.L A2, -(SP)
        MOVE.W D6, D0
        MOVE.W D5, D1
        MOVEA.L A4, A1
        MOVEA.L A5, A2
        BSR.W tkpkg_pipeline_string_eq_ascii_casefold_v1
        MOVEA.L (SP)+, A2
        TST.B D0
        BNE.S tkpkgPipelineDialectAllowed
        DBF D7, tkpkgPipelineAllowLoop

tkpkgPipelineDialectRejected:
        MOVE.W (SP)+, D7
        MOVEQ #1, D0
        RTS

tkpkgPipelineDialectAllowed:
        MOVE.W (SP)+, D7
        MOVEQ #0, D0
        RTS

tkpkg_pipeline_resolve_tokenizer_vm_locator_v1:
        MOVEQ #SCOPED_OWNER_DIALECT, D0
        LEA pendingDialectOffsetLo, A3
        BSR.W tkpkg_pipeline_find_tokenizer_vm_owner_v1
        TST.B D0
        BEQ.S tkpkgPipelineVmResolved
        MOVEQ #SCOPED_OWNER_CPU, D0
        LEA pendingCpuOffsetLo, A3
        BSR.W tkpkg_pipeline_find_tokenizer_vm_owner_v1
        TST.B D0
        BEQ.S tkpkgPipelineVmResolved
        MOVEQ #SCOPED_OWNER_FAMILY, D0
        LEA pendingFamilyOffsetLo, A3
        BSR.W tkpkg_pipeline_find_tokenizer_vm_owner_v1
        TST.B D0
        BEQ.S tkpkgPipelineVmResolved
        LEA missingProgramText, A1
        MOVEQ #MISSING_PROGRAM_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkgPipelineVmResolved:
        MOVEQ #0, D0
        RTS

tkpkg_pipeline_find_tokenizer_vm_owner_v1:
        MOVE.B D0, D6
        LEA packageStorage, A6
        BSR.W tkpkg_pipeline_read_locator_ptr_len_v1
        MOVE.W D3, D5
        MOVEA.L A1, A5
        LEA tkvmChunkOffsetLo, A3
        BSR.W tkpkg_pipeline_chunk_ptr_from_locator_v1
        BSR.W tkpkg_pipeline_read_u32_le_low16_v1
        TST.B D1
        BNE.W tkpkgPipelineVmOwnerMissing
        TST.W D0
        BEQ.W tkpkgPipelineVmOwnerMissing
        MOVE.W D0, D7
        SUBQ.W #1, D7
        LEA 4(A2), A2

tkpkgPipelineVmLoop:
        MOVEA.L A2, A4
        MOVEQ #1, D0
        BSR.W tkpkg_pipeline_require_bytes_v1
        TST.B D1
        BNE.W tkpkgPipelineVmOwnerMissing
        MOVE.B (A2)+, D4
        BSR.W tkpkg_pipeline_locate_string_v1
        TST.B D1
        BNE.W tkpkgPipelineVmOwnerMissing
        CMP.B D6, D4
        BNE.W tkpkgPipelineVmSkipEntry
        MOVE.W D0, D2
        MOVE.L A2, -(SP)
        MOVE.W D2, D0
        MOVE.W D5, D1
        MOVEA.L A5, A2
        BSR.W tkpkg_pipeline_string_eq_ascii_casefold_v1
        MOVEA.L (SP)+, A2
        TST.B D0
        BNE.W tkpkgPipelineVmFound

tkpkgPipelineVmSkipEntry:
        BSR.W tkpkg_pipeline_skip_tokenizer_vm_entry_v1
        TST.B D1
        BNE.W tkpkgPipelineVmOwnerMissing
        DBF D7, tkpkgPipelineVmLoop

tkpkgPipelineVmOwnerMissing:
        MOVEQ #1, D0
        RTS

tkpkgPipelineVmFound:
        BSR.W tkpkg_pipeline_skip_tokenizer_vm_entry_v1
        TST.B D1
        BNE.W tkpkgPipelineVmOwnerMissing
        LEA pendingTokenizerVmOffsetLo, A3
        MOVEA.L A4, A1
        MOVE.L A2, D0
        SUB.L A4, D0
        BSR.W tkpkg_pipeline_store_record_locator_v1
        MOVE.B D6, pendingTokenizerVmOwnerTag
        MOVEQ #0, D0
        RTS

tkpkg_pipeline_skip_tokenizer_vm_entry_v1:
        MOVE.W D7, -(SP)
        MOVEQ #TOKENIZER_VM_ENTRY_PREFIX_SIZE, D0
        BSR.W tkpkg_pipeline_require_bytes_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        LEA TOKENIZER_VM_ENTRY_PREFIX_SIZE(A2), A2
        BSR.W tkpkg_pipeline_read_u32_le_low16_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        MOVE.W D0, D7
        LEA 4(A2), A2
        MOVEQ #0, D0
        MOVE.W D7, D0
        LSL.L #2, D0
        BSR.W tkpkg_pipeline_require_bytes_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        TST.W D7
        BEQ.S tkpkgPipelineVmAfterOffsets
        SUBQ.W #1, D7

tkpkgPipelineVmOffsetLoop:
        ADDQ.W #4, A2
        DBF D7, tkpkgPipelineVmOffsetLoop

tkpkgPipelineVmAfterOffsets:
        MOVEQ #TOKENIZER_VM_ENTRY_FIXED_TAIL_SIZE, D0
        BSR.W tkpkg_pipeline_require_bytes_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        LEA TOKENIZER_VM_ENTRY_FIXED_TAIL_SIZE(A2), A2
        BSR.W tkpkg_pipeline_skip_string_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        BSR.W tkpkg_pipeline_skip_string_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        BSR.W tkpkg_pipeline_skip_string_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        BSR.W tkpkg_pipeline_skip_string_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        BSR.W tkpkg_pipeline_skip_string_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        BSR.W tkpkg_pipeline_skip_string_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        BSR.W tkpkg_pipeline_read_u32_le_low16_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        MOVE.L D0, D2
        MOVE.L D0, D3
        ADDQ.L #4, D3
        MOVE.L D3, D0
        BSR.W tkpkg_pipeline_require_bytes_v1
        TST.B D1
        BNE.W tkpkgPipelineVmSkipBoundsFail
        LEA 4(A2), A2
        ADDA.L D2, A2
        MOVE.W (SP)+, D7
        MOVEQ #0, D1
        RTS

tkpkgPipelineVmSkipBoundsFail:
        MOVE.W (SP)+, D7
        MOVEQ #1, D1
        RTS

tkpkg_pipeline_commit_active_selection_v1:
        LEA pendingCpuOffsetLo, A3
        LEA activeCpuBuffer.L, A2
        BSR.W tkpkg_pipeline_copy_locator_to_buffer_v1
        TST.B D0
        BNE.S tkpkgPipelineCommitDone
        LEA pendingDialectOffsetLo, A3
        LEA activeDialectBuffer.L, A2
        BSR.W tkpkg_pipeline_copy_locator_to_buffer_v1
        TST.B D0
        BNE.S tkpkgPipelineCommitDone
        LEA pendingFamilyOffsetLo, A3
        LEA activeFamilyBuffer.L, A2
        BSR.W tkpkg_pipeline_copy_locator_to_buffer_v1
        TST.B D0
        BNE.S tkpkgPipelineCommitDone
        LEA pendingTokenPolicyOffsetLo, A3
        LEA activeTokenPolicyOffsetLo.L, A2
        BSR.W tkpkg_pipeline_copy_record_locator_v1
        MOVE.B pendingTokenPolicyOwnerTag, D0
        MOVE.B D0, activeTokenPolicyOwnerTag
        LEA pendingTokenizerVmOffsetLo, A3
        LEA activeTokenizerVmOffsetLo.L, A2
        BSR.W tkpkg_pipeline_copy_record_locator_v1
        MOVE.B pendingTokenizerVmOwnerTag, D0
        MOVE.B D0, activeTokenizerVmOwnerTag
        ORI.B #PACKAGE_STATE_PIPELINE_ACTIVE, packageStateFlags
        MOVEQ #0, D0

tkpkgPipelineCommitDone:
        RTS

tkpkg_pipeline_copy_locator_to_buffer_v1:
        BSR.W tkpkg_pipeline_read_locator_ptr_len_v1
        CMPI.W #PIPELINE_ID_BUFFER_CAPACITY, D3
        BHS.S tkpkgPipelineCopyBufferTooLong
        MOVE.W D3, D2
        TST.W D2
        BEQ.S tkpkgPipelineCopyBufferDone
        SUBQ.W #1, D2

tkpkgPipelineCopyBufferLoop:
        MOVE.B (A1)+, (A2)+
        DBF D2, tkpkgPipelineCopyBufferLoop

tkpkgPipelineCopyBufferDone:
        CLR.B (A2)
        MOVEQ #0, D0
        RTS

tkpkgPipelineCopyBufferTooLong:
        LEA identifierTooLongText, A1
        MOVEQ #IDENTIFIER_TOO_LONG_TEXT_LEN, D1
        MOVEQ #STATUS_RUNTIME_ERROR_V1, D0
        RTS

tkpkg_pipeline_copy_record_locator_v1:
        MOVE.L (A3), (A2)
        RTS

tkpkg_pipeline_store_package_string_locator_v1:
        MOVE.L A6, -(SP)
        MOVE.L A1, D2
        LEA packageStorage, A6
        SUB.L A6, D2
        MOVE.B D2, (A3)+
        LSR.W #8, D2
        MOVE.B D2, (A3)+
        MOVE.B D0, (A3)+
        LSR.W #8, D0
        MOVE.B D0, (A3)+
        MOVEA.L (SP)+, A6
        RTS

tkpkg_pipeline_store_optional_package_string_locator_v1:
        TST.W D0
        BEQ.S tkpkgPipelineClearOptionalLocator
        BSR.W tkpkg_pipeline_store_package_string_locator_v1
        RTS

tkpkgPipelineClearOptionalLocator:
        CLR.L (A3)
        RTS

tkpkg_pipeline_store_record_locator_v1:
        MOVE.L A6, -(SP)
        MOVE.L A1, D2
        LEA packageStorage, A6
        SUB.L A6, D2
        MOVE.B D2, (A3)+
        LSR.W #8, D2
        MOVE.B D2, (A3)+
        MOVE.B D0, (A3)+
        LSR.W #8, D0
        MOVE.B D0, (A3)+
        MOVEA.L (SP)+, A6
        RTS

tkpkg_pipeline_read_locator_ptr_len_v1:
        MOVEQ #0, D2
        MOVE.B (A3)+, D2
        MOVEQ #0, D1
        MOVE.B (A3)+, D1
        LSL.W #8, D1
        OR.W D1, D2
        MOVEQ #0, D3
        MOVE.B (A3)+, D3
        MOVEQ #0, D1
        MOVE.B (A3)+, D1
        LSL.W #8, D1
        OR.W D1, D3
        LEA packageStorage, A6
        LEA 0(A6, D2.W), A1
        RTS

tkpkg_pipeline_read_request_locator_ptr_len_v1:
        MOVEQ #0, D2
        MOVE.B (A3)+, D2
        MOVEQ #0, D1
        MOVE.B (A3)+, D1
        LSL.W #8, D1
        OR.W D1, D2
        MOVEQ #0, D3
        MOVE.B (A3)+, D3
        MOVEQ #0, D1
        MOVE.B (A3)+, D1
        LSL.W #8, D1
        OR.W D1, D3
        LEA 0(A0, D2.W), A1
        RTS

tkpkg_pipeline_chunk_ptr_from_locator_v1:
        MOVEQ #0, D0
        MOVE.B (A3)+, D0
        MOVEQ #0, D1
        MOVE.B (A3)+, D1
        LSL.W #8, D1
        OR.W D1, D0
        MOVEQ #0, D7
        MOVE.B (A3)+, D7
        MOVEQ #0, D1
        MOVE.B (A3)+, D1
        LSL.W #8, D1
        OR.W D1, D7
        LEA packageStorage, A6
        LEA 0(A6, D0.W), A2
        LEA 0(A2, D7.W), A6
        RTS

tkpkg_pipeline_locate_string_v1:
        BSR.W tkpkg_pipeline_read_u32_le_low16_v1
        TST.B D1
        BNE.S tkpkgPipelineLocateStringBoundsFail
        MOVE.L D0, D2
        MOVE.L D0, D3
        ADDQ.L #4, D3
        MOVE.L D3, D0
        BSR.W tkpkg_pipeline_require_bytes_v1
        TST.B D1
        BNE.S tkpkgPipelineLocateStringBoundsFail
        MOVE.L D2, D0
        LEA 4(A2), A1
        LEA 4(A2), A2
        ADDA.L D0, A2
        MOVEQ #0, D1
        RTS

tkpkgPipelineLocateStringBoundsFail:
        MOVEQ #0, D0
        MOVEA.L D0, A1
        MOVEQ #1, D1
        RTS

tkpkg_pipeline_skip_string_v1:
        BSR.W tkpkg_pipeline_locate_string_v1
        RTS

tkpkg_pipeline_locate_optional_string_v1:
        MOVEQ #1, D0
        BSR.W tkpkg_pipeline_require_bytes_v1
        TST.B D1
        BNE.S tkpkgPipelineOptionalBoundsFail
        MOVE.B (A2)+, D1
        BEQ.S tkpkgPipelineOptionalNone
        BSR.W tkpkg_pipeline_locate_string_v1
        RTS

tkpkgPipelineOptionalNone:
        MOVEQ #0, D0
        MOVEA.L D0, A1
        MOVEQ #0, D1
        RTS

tkpkgPipelineOptionalBoundsFail:
        MOVEQ #0, D0
        MOVEA.L D0, A1
        MOVEQ #1, D1
        RTS

tkpkg_pipeline_skip_optional_string_v1:
        BSR.W tkpkg_pipeline_locate_optional_string_v1
        RTS

tkpkg_pipeline_skip_optional_string_list_v1:
        MOVE.W D7, -(SP)
        MOVEQ #1, D0
        BSR.W tkpkg_pipeline_require_bytes_v1
        TST.B D1
        BNE.S tkpkgPipelineSkipListBoundsFail
        MOVE.B (A2)+, D1
        BEQ.S tkpkgPipelineSkipListDone
        BSR.W tkpkg_pipeline_read_u32_le_low16_v1
        TST.B D1
        BNE.S tkpkgPipelineSkipListBoundsFail
        MOVE.W D0, D7
        LEA 4(A2), A2
        TST.W D7
        BEQ.S tkpkgPipelineSkipListDone
        SUBQ.W #1, D7

tkpkgPipelineSkipListLoop:
        BSR.W tkpkg_pipeline_skip_string_v1
        TST.B D1
        BNE.S tkpkgPipelineSkipListBoundsFail
        DBF D7, tkpkgPipelineSkipListLoop

tkpkgPipelineSkipListDone:
        MOVE.W (SP)+, D7
        MOVEQ #0, D1
        RTS

tkpkgPipelineSkipListBoundsFail:
        MOVE.W (SP)+, D7
        MOVEQ #1, D1
        RTS

tkpkg_pipeline_read_u32_le_low16_v1:
        MOVEQ #4, D0
        BSR.W tkpkg_pipeline_require_bytes_v1
        TST.B D1
        BNE.S tkpkgPipelineReadU32BoundsFail
        MOVEQ #0, D0
        MOVE.B (A2), D0
        MOVEQ #0, D1
        MOVE.B 1(A2), D1
        LSL.W #8, D1
        OR.W D1, D0
        MOVEQ #0, D1
        RTS

tkpkgPipelineReadU32BoundsFail:
        MOVEQ #0, D0
        MOVEQ #1, D1
        RTS

tkpkg_pipeline_require_bytes_v1:
        MOVEA.L A2, A1
        ADDA.L D0, A1
        CMPA.L A6, A1
        BHI.S tkpkgPipelineRequireBytesFail
        MOVEQ #0, D1
        RTS

tkpkgPipelineRequireBytesFail:
        MOVEQ #1, D1
        RTS

tkpkg_pipeline_string_eq_ascii_casefold_v1:
        CMP.W D1, D0
        BNE.S tkpkgPipelineStringNoMatch
        MOVE.W D0, D4
        TST.W D4
        BEQ.S tkpkgPipelineStringMatch
        SUBQ.W #1, D4

tkpkgPipelineStringLoop:
        MOVEQ #0, D2
        MOVE.B (A1)+, D2
        MOVEQ #0, D3
        MOVE.B (A2)+, D3
        MOVE.B D2, D0
        BSR.W tkpkg_pipeline_fold_ascii_lower_v1
        MOVE.B D0, D2
        MOVE.B D3, D0
        BSR.W tkpkg_pipeline_fold_ascii_lower_v1
        CMP.B D0, D2
        BNE.S tkpkgPipelineStringNoMatch
        DBF D4, tkpkgPipelineStringLoop

tkpkgPipelineStringMatch:
        MOVEQ #1, D0
        RTS

tkpkgPipelineStringNoMatch:
        MOVEQ #0, D0
        RTS

tkpkg_pipeline_fold_ascii_lower_v1:
        CMPI.B #'A', D0
        BLO.S tkpkgPipelineFoldDone
        CMPI.B #'Z', D0
        BHI.S tkpkgPipelineFoldDone
        ORI.B #$20, D0

tkpkgPipelineFoldDone:
        RTS

tkpkg_pipeline_placeholder:
        RTS

        .endsection
        .endmodule
