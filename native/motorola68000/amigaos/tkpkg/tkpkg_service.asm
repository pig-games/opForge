; Request dispatch and lifecycle scaffolding for the first tkpkg native slice.

        .module tkpkg.amigaos.service
        .cpu 68020
        .pub
        .use tkpkg.amigaos.abi (NATIVE_ABI_MAGIC_0, NATIVE_ABI_MAGIC_1)
        .use tkpkg.amigaos.abi (NATIVE_ABI_MAGIC_2, NATIVE_ABI_MAGIC_3)
        .use tkpkg.amigaos.abi (NATIVE_ABI_VERSION_V1, NATIVE_CONTROL_BLOCK_SIZE_V1)
        .use tkpkg.amigaos.abi (CAPABILITY_FLAGS_V1, STATUS_BAD_CONTROL_BLOCK_V1)
        .use tkpkg.amigaos.abi (STATUS_BAD_REQUEST_V1)
        .use tkpkg.amigaos.abi (STATUS_OK_V1, STATUS_RUNTIME_ERROR_V1)
        .use tkpkg.amigaos.abi (ENTRY_ORD_INIT, ENTRY_ORD_LOAD_PACKAGE)
        .use tkpkg.amigaos.abi (ENTRY_ORD_SET_PIPELINE, ENTRY_ORD_TOKENIZE_LINE)
        .use tkpkg.amigaos.abi (ENTRY_ORD_PARSE_LINE, ENTRY_ORD_ENCODE_INSTRUCTION)
        .use tkpkg.amigaos.abi (ENTRY_ORD_LAST_ERROR)
        .use tkpkg.amigaos.abi (CB_MAGIC, CB_ABI_VERSION, CB_STRUCT_SIZE)
        .use tkpkg.amigaos.abi (CB_CAPABILITY_FLAGS, CB_STATUS_CODE, CB_REQUEST_ID)
        .use tkpkg.amigaos.abi (CB_RESERVED0, CB_INPUT_PTR, CB_INPUT_LEN)
        .use tkpkg.amigaos.abi (CB_OUTPUT_PTR, CB_OUTPUT_LEN, CB_EXTENSION_PTR)
        .use tkpkg.amigaos.abi (CB_EXTENSION_LEN, CB_LAST_ERROR_PTR, CB_LAST_ERROR_LEN)
        .use tkpkg.amigaos.buffers (BAD_REQUEST_TEXT_LEN, CONTROL_BLOCK_ERROR_TEXT_LEN)
        .use tkpkg.amigaos.buffers (LAST_ERROR_BUFFER_PTR_V1)
        .use tkpkg.amigaos.buffers (RUNTIME_ERROR_TEXT_LEN)
        .use tkpkg.amigaos.buffers (LAST_ERROR_KIND_NONE, LAST_ERROR_KIND_BAD_REQUEST)
        .use tkpkg.amigaos.buffers (LAST_ERROR_KIND_BAD_CONTROL, LAST_ERROR_KIND_RUNTIME)
        .use tkpkg.amigaos.buffers (badRequestText, controlBlockErrorText, runtimeErrorText)
        .use tkpkg.amigaos.buffers (controlBlockV1, nextRequestIdHi, nextRequestIdLo)
        .use tkpkg.amigaos.buffers (storedLastErrorKind, storedLastErrorLen)
        .use tkpkg.amigaos.buffers (storedLastErrorLenHi)
        .use tkpkg.amigaos.buffers (lastErrorBuffer, packageStorage)
        .use tkpkg.amigaos.buffers (tablChunkOffsetLo, tablChunkOffsetHi)
        .use tkpkg.amigaos.package_loader (tkpkg_package_loader_load_v1)
        .use tkpkg.amigaos.pipeline (tkpkg_pipeline_set_active_v1)
        .use tkpkg.amigaos.tokenizer_vm (tkpkg_tokenizer_vm_tokenize_line_v1)
        .use prvm.amigaos.line_router (prvm_route_line_68000)

TKPKG_PARSE_ROUTE_FRAME_SIZE         = 116

        .section code, kind=code

; ---------------------------------------------------------------------------
; Initialize the shared tkpkg service control block.
;
; This is the local bootstrap convenience entry used by native AmigaOS callers
; that link the service directly. It routes through the same dispatch surface as
; external calls so the initialized control block matches the public ABI.
;
; Inputs:
; - none; uses the shared controlBlockV1 buffer.
;
; Outputs:
; - controlBlockV1 contains ABI magic/version/capability fields.
; - D0/D1 follow tkpkg_service_dispatch_v1 for ENTRY_ORD_INIT.
; ---------------------------------------------------------------------------

tkpkg_service_bootstrap_v1:
        LEA controlBlockV1, A0          ; shared in-module CB used by the direct native bootstrap
        MOVEQ #ENTRY_ORD_INIT, D0       ; exercise the public init ordinal, not a private initializer
        BSR.W tkpkg_service_dispatch_v1 ; keep bootstrap behavior identical to an external init call
        RTS

; ---------------------------------------------------------------------------
; Public tkpkg service dispatcher.
;
; This is the stable native runtime boundary for package-backed VM services. It
; validates the v1 control block for every non-init request, then dispatches by
; ENTRY_ORD_* without exposing package internals to the CLI.
;
; Inputs:
; - A0: NATIVE_CONTROL_BLOCK_V1 pointer.
; - D0: ENTRY_ORD_* request ordinal.
;
; Outputs:
; - D0/D1 are request-specific immediate results.
; - CB_STATUS_CODE reports STATUS_*_V1 for the caller-visible service result.
; - CB_OUTPUT_PTR/CB_OUTPUT_LEN identify any payload written in the control
;   block output window.
; - Last-error fields are updated for bad-control, bad-request, and runtime
;   error paths.
; ---------------------------------------------------------------------------

tkpkg_service_dispatch_v1:
        CMPI.B #ENTRY_ORD_INIT, D0
        BEQ.S tkpkgServiceHandleInitEntry
        BSR.W tkpkg_service_prepare_request_v1 ; assign a request id before validation/status reporting
        BSR.W tkpkg_service_validate_header_v1 ; reject stale or foreign control blocks early
        TST.B D1
        BNE.S tkpkgServiceDispatchDone
        CMPI.B #ENTRY_ORD_LAST_ERROR, D0
        BEQ.S tkpkgServiceHandleLastError
        CMPI.B #ENTRY_ORD_LOAD_PACKAGE, D0
        BEQ.W tkpkgServiceHandleLoadPackage
        CMPI.B #ENTRY_ORD_SET_PIPELINE, D0
        BEQ.W tkpkgServiceHandleSetPipeline
        CMPI.B #ENTRY_ORD_TOKENIZE_LINE, D0
        BEQ.W tkpkgServiceHandleTokenizeLine
        CMPI.B #ENTRY_ORD_PARSE_LINE, D0
        BEQ.W tkpkgServiceHandleParseLine
        CMPI.B #ENTRY_ORD_ENCODE_INSTRUCTION, D0
        BEQ.W tkpkgServiceHandleEncodeInstruction
        BSR.W tkpkg_service_set_bad_request_v1
        RTS

tkpkgServiceHandleInitEntry:
        BSR.W tkpkg_service_prepare_request_v1
        BSR.W tkpkg_service_write_header_v1
        BSR.W tkpkg_service_write_clear_input_fields_v1
        BRA.S tkpkgServiceHandleInit

tkpkgServiceDispatchDone:
        RTS

tkpkgServiceHandleInit:
        BSR.W tkpkg_service_clear_stored_last_error_v1
        BSR.W tkpkg_service_write_clear_last_error_fields_v1
        BSR.W tkpkg_service_set_status_ok_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        RTS

tkpkgServiceHandleLastError:
        TST.B CB_INPUT_LEN(A0)
        BNE.S tkpkgServiceLastErrorBadRequest
        TST.B 19(A0)
        BNE.S tkpkgServiceLastErrorBadRequest
        BSR.W tkpkg_service_write_clear_input_fields_v1
        BSR.W tkpkg_service_set_status_ok_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        MOVE.B storedLastErrorLen, CB_OUTPUT_LEN(A0)
        MOVE.B storedLastErrorLenHi, 23(A0)
        TST.B storedLastErrorLen
        BEQ.S tkpkgServiceLastErrorDone
        BSR.W tkpkg_service_write_output_buffer_offset_v1

tkpkgServiceLastErrorDone:
        BSR.W tkpkg_service_clear_stored_last_error_v1
        BSR.W tkpkg_service_write_clear_last_error_fields_v1
        RTS

tkpkgServiceLastErrorBadRequest:
        BSR.W tkpkg_service_set_bad_request_v1
        RTS

tkpkgServiceHandleLoadPackage:
        MOVE.L A0, -(SP)
        BSR.W tkpkg_package_loader_load_v1
        MOVEA.L (SP)+, A0
        TST.B D0
        BNE.S tkpkgServiceLoadPackageError
        BSR.W tkpkg_service_write_clear_input_fields_v1
        BSR.W tkpkg_service_clear_stored_last_error_v1
        BSR.W tkpkg_service_write_clear_last_error_fields_v1
        BSR.W tkpkg_service_set_status_ok_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        RTS

tkpkgServiceLoadPackageError:
        BSR.W tkpkg_service_set_runtime_error_message_v1
        RTS

tkpkgServiceHandleSetPipeline:
        MOVE.L A0, -(SP)
        BSR.W tkpkg_pipeline_set_active_v1
        MOVEA.L (SP)+, A0
        TST.B D0
        BEQ.S tkpkgServiceSetPipelineOk
        CMPI.B #STATUS_BAD_REQUEST_V1, D0
        BEQ.S tkpkgServiceSetPipelineBadRequest
        BSR.W tkpkg_service_set_runtime_error_message_v1
        RTS

tkpkgServiceSetPipelineBadRequest:
        BSR.W tkpkg_service_set_bad_request_v1
        RTS

tkpkgServiceSetPipelineOk:
        BSR.W tkpkg_service_write_clear_input_fields_v1
        BSR.W tkpkg_service_clear_stored_last_error_v1
        BSR.W tkpkg_service_write_clear_last_error_fields_v1
        BSR.W tkpkg_service_set_status_ok_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        RTS

tkpkgServiceHandleTokenizeLine:
        MOVE.L A0, -(SP)
        BSR.W tkpkg_tokenizer_vm_tokenize_line_v1
        MOVEA.L (SP)+, A0
        TST.B D0
        BEQ.S tkpkgServiceTokenizeLineOk
        CMPI.B #STATUS_BAD_REQUEST_V1, D0
        BEQ.S tkpkgServiceTokenizeLineBadRequest
        BSR.W tkpkg_service_set_runtime_error_message_v1
        RTS

tkpkgServiceTokenizeLineBadRequest:
        BSR.W tkpkg_service_set_bad_request_v1
        RTS

tkpkgServiceTokenizeLineOk:
        BSR.W tkpkg_service_write_clear_input_fields_v1
        BSR.W tkpkg_service_clear_stored_last_error_v1
        BSR.W tkpkg_service_write_clear_last_error_fields_v1
        BSR.W tkpkg_service_set_status_ok_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        TST.W D1
        BEQ.S tkpkgServiceTokenizeLineDone
        BSR.W tkpkg_service_write_output_buffer_offset_v1
        MOVE.B D1, CB_OUTPUT_LEN(A0)
        LSR.W #8, D1
        MOVE.B D1, 23(A0)

tkpkgServiceTokenizeLineDone:
        RTS

tkpkgServiceHandleParseLine:
        MOVE.L A0, -(SP)
        BSR.W tkpkg_service_parse_line_v1
        MOVEA.L (SP)+, A0
        TST.B D2
        BEQ.S tkpkgServiceParseLineOk
        BSR.W tkpkg_service_set_bad_request_v1
        RTS

tkpkgServiceParseLineOk:
        MOVEM.L D0-D1, -(SP)
        BSR.W tkpkg_service_write_clear_input_fields_v1
        BSR.W tkpkg_service_clear_stored_last_error_v1
        BSR.W tkpkg_service_write_clear_last_error_fields_v1
        BSR.W tkpkg_service_set_status_ok_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        MOVEM.L (SP)+, D0-D1
        RTS

tkpkgServiceHandleEncodeInstruction:
        MOVE.L A0, -(SP)
        BSR.W tkpkg_service_encode_instruction_v1
        MOVEA.L (SP)+, A0
        TST.B D0
        BEQ.S tkpkgServiceEncodeInstructionOk
        BSR.W tkpkg_service_set_runtime_error_message_v1
        RTS

tkpkgServiceEncodeInstructionOk:
        BSR.W tkpkg_service_write_clear_input_fields_v1
        BSR.W tkpkg_service_clear_stored_last_error_v1
        BSR.W tkpkg_service_write_clear_last_error_fields_v1
        BSR.W tkpkg_service_set_status_ok_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        TST.W D1
        BEQ.S tkpkgServiceEncodeInstructionDone
        BSR.W tkpkg_service_write_output_buffer_offset_v1
        MOVE.B D1, CB_OUTPUT_LEN(A0)
        LSR.W #8, D1
        MOVE.B D1, 23(A0)

tkpkgServiceEncodeInstructionDone:
        RTS

; ---------------------------------------------------------------------------
; Route one parser request frame through PRVM.
;
; Current implementation note: this entry accepts only the fixed
; TKPKG_PARSE_ROUTE_FRAME_SIZE route frame built by the native CLI. It is the
; intended service boundary for parse-line behavior even while the CLI still
; owns some transitional parser/assembler state.
;
; Inputs:
; - A0: validated control block whose input window points at a PRVM route frame.
;
; Outputs:
; - D0/D1: PRVM status/result-count values.
; - D2: 0 on accepted request, 1 on malformed service payload.
; ---------------------------------------------------------------------------

tkpkg_service_parse_line_v1:
        MOVEQ #0, D0
        MOVE.B CB_INPUT_PTR(A0), D0     ; low byte of CB-relative route-frame offset
        MOVEQ #0, D1
        MOVE.B 17(A0), D1               ; high byte of CB_INPUT_PTR; direct offset avoids a temp struct
        LSL.W #8, D1
        OR.W D1, D0
        LEA 0(A0,D0.W), A1              ; A1 now points at the caller-supplied PRVM route frame
        MOVEQ #0, D0
        MOVE.B CB_INPUT_LEN(A0), D0     ; low byte of route-frame byte length
        MOVEQ #0, D1
        MOVE.B 19(A0), D1               ; high byte of CB_INPUT_LEN
        LSL.W #8, D1
        OR.W D1, D0
        CMPI.W #TKPKG_PARSE_ROUTE_FRAME_SIZE, D0
        BNE.S tkpkgParseLineBadRequest
        MOVEA.L A1, A0                  ; PRVM router ABI expects its route frame in A0
        JSR prvm_route_line_68000       ; D0/D1 become the parser service's immediate return pair
        MOVEQ #0, D2
        RTS

tkpkgParseLineBadRequest:
        MOVEQ #1, D2
        MOVEQ #0, D0
        MOVEQ #0, D1
        RTS

; ---------------------------------------------------------------------------
; Encode one package-backed instruction request.
;
; The request payload is the compact selector/encoder envelope currently built
; by native opasm staging code:
; - mnemonic length + mnemonic bytes
; - candidate count
; - per-candidate addressing-mode and operand bytes
;
; Current implementation note: this entry still decodes only the small native
; 6502 smoke envelope used by the first CLI slice. The architectural contract is
; still correct: the CLI asks the package service to encode instead of writing
; opcodes directly.
;
; Inputs:
; - A0: validated control block whose input window points at the encode request.
;
; Outputs:
; - D0: 0 on success, nonzero on runtime error.
; - D1: encoded byte count on success.
; - output bytes are written in the service output window.
; ---------------------------------------------------------------------------

tkpkg_service_encode_instruction_v1:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVEQ #0, D0
        MOVE.B CB_INPUT_PTR(A0), D0     ; low byte of CB-relative encode-request offset
        MOVEQ #0, D1
        MOVE.B 17(A0), D1               ; high byte of CB_INPUT_PTR
        LSL.W #8, D1
        OR.W D1, D0
        LEA 0(A0,D0.W), A4              ; A4 walks the request envelope in-place
        MOVEQ #0, D7
        MOVE.B CB_INPUT_LEN(A0), D7     ; D7 tracks remaining request bytes as fields are consumed
        MOVEQ #0, D0
        MOVE.B 19(A0), D0
        LSL.W #8, D0
        OR.W D0, D7
        CMPI.W #4, D7
        BCS.W tkpkgEncodeInstructionFail
        MOVEQ #0, D2
        MOVE.B (A4)+, D2
        SUBQ.W #1, D7
        TST.W D2
        BEQ.W tkpkgEncodeInstructionFail
        CMP.W D7, D2
        BHI.W tkpkgEncodeInstructionFail
        MOVEA.L A4, A5
        ADDA.W D2, A4
        SUB.W D2, D7
        TST.W D7
        BEQ.W tkpkgEncodeInstructionFail
        MOVEQ #0, D3
        MOVE.B (A4)+, D3
        SUBQ.W #1, D7
        TST.W D3
        BEQ.W tkpkgEncodeInstructionNoMatch
        TST.W D7
        BEQ.W tkpkgEncodeInstructionFail
        MOVEQ #0, D4
        MOVE.B (A4)+, D4
        SUBQ.W #1, D7
        TST.W D4
        BEQ.W tkpkgEncodeInstructionFail
        CMP.W D7, D4
        BHI.W tkpkgEncodeInstructionFail
        MOVEA.L A4, A6
        ADDA.W D4, A4
        SUB.W D4, D7
        TST.W D7
        BEQ.W tkpkgEncodeInstructionFail
        MOVEQ #0, D5
        MOVE.B (A4)+, D5
        SUBQ.W #1, D7
        TST.W D5
        BEQ.W tkpkgEncodeInstructionFail
        TST.W D7
        BEQ.W tkpkgEncodeInstructionFail
        MOVEQ #0, D6
        MOVE.B (A4)+, D6
        SUBQ.W #1, D7
        CMP.W D7, D6
        BHI.W tkpkgEncodeInstructionFail
        MOVEA.L A4, A3
        BSR.W tkpkgEncodeFindAndExecuteTableProgram
        BRA.S tkpkgEncodeInstructionReturn

tkpkgEncodeInstructionNoMatch:
        MOVEQ #0, D1
        MOVEQ #0, D0
        BRA.S tkpkgEncodeInstructionReturn

tkpkgEncodeInstructionFail:
        LEA runtimeErrorText, A1
        MOVEQ #RUNTIME_ERROR_TEXT_LEN, D1
        MOVEQ #1, D0

tkpkgEncodeInstructionReturn:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tkpkgEncodeFindAndExecuteTableProgram:
        MOVEM.L D2-D7/A0-A6, -(SP)
        MOVEQ #0, D0
        MOVE.B tablChunkOffsetLo, D0
        MOVEQ #0, D1
        MOVE.B tablChunkOffsetHi, D1
        LSL.W #8, D1
        OR.W D1, D0
        BEQ.W tkpkgEncodeFindTableFail
        LEA packageStorage, A0
        ADDA.W D0, A0
        BSR.W tkpkgEncodeReadU32Low16
        TST.W D0
        BEQ.W tkpkgEncodeFindTableNoMatch
        MOVE.W D0, D7
        SUBQ.W #1, D7

tkpkgEncodeFindTableLoop:
        MOVE.B (A0)+, D0
        BSR.W tkpkgEncodeSkipString
        MOVEA.L A0, A1
        BSR.W tkpkgEncodeReadU32Low16
        MOVE.W D0, D1
        MOVEA.L A0, A2
        ADDA.W D1, A0
        MOVEA.L A5, A1
        MOVE.W D2, D0
        BSR.W tkpkgEncodeStringEqIgnoreCase
        TST.B D0
        BEQ.S tkpkgEncodeFindTableSkipModeCheck
        MOVEA.L A0, A1
        BSR.W tkpkgEncodeReadU32Low16
        MOVE.W D0, D1
        MOVEA.L A0, A2
        ADDA.W D1, A0
        MOVEA.L A6, A1
        MOVE.W D4, D0
        BSR.W tkpkgEncodeStringEqIgnoreCase
        TST.B D0
        BEQ.S tkpkgEncodeFindTableSkipProgram
        BSR.W tkpkgEncodeReadU32Low16
        MOVE.W D0, D1
        MOVEA.L A0, A1
        BSR.W tkpkgEncodeExecuteProgram
        BRA.S tkpkgEncodeFindTableReturn

tkpkgEncodeFindTableSkipModeCheck:
        BSR.W tkpkgEncodeSkipString

tkpkgEncodeFindTableSkipProgram:
        BSR.W tkpkgEncodeSkipBytes
        DBRA D7, tkpkgEncodeFindTableLoop

tkpkgEncodeFindTableNoMatch:
        MOVEQ #0, D1
        MOVEQ #0, D0
        BRA.S tkpkgEncodeFindTableReturn

tkpkgEncodeFindTableFail:
        LEA runtimeErrorText, A1
        MOVEQ #RUNTIME_ERROR_TEXT_LEN, D1
        MOVEQ #1, D0

tkpkgEncodeFindTableReturn:
        MOVEM.L (SP)+, D2-D7/A0-A6
        RTS

tkpkgEncodeExecuteProgram:
        MOVEM.L D2-D7/A0-A4, -(SP)
        MOVEA.L A1, A0
        MOVE.W D1, D7
        LEA lastErrorBuffer, A2
        CLR.W D1

tkpkgEncodeExecuteProgramLoop:
        TST.W D7
        BEQ.S tkpkgEncodeExecuteProgramFail
        MOVE.B (A0)+, D0
        SUBQ.W #1, D7
        CMPI.B #$FF, D0
        BEQ.S tkpkgEncodeExecuteProgramOk
        CMPI.B #$01, D0
        BEQ.S tkpkgEncodeExecuteProgramEmitU8
        CMPI.B #$02, D0
        BEQ.S tkpkgEncodeExecuteProgramEmitOperand
        BRA.W tkpkgEncodeExecuteProgramFail

tkpkgEncodeExecuteProgramEmitU8:
        TST.W D7
        BEQ.S tkpkgEncodeExecuteProgramFail
        MOVE.B (A0)+, (A2)+
        SUBQ.W #1, D7
        ADDQ.W #1, D1
        BRA.S tkpkgEncodeExecuteProgramLoop

tkpkgEncodeExecuteProgramEmitOperand:
        TST.W D7
        BEQ.S tkpkgEncodeExecuteProgramFail
        MOVE.B (A0)+, D0
        SUBQ.W #1, D7
        TST.B D0
        BNE.S tkpkgEncodeExecuteProgramFail
        MOVE.W D6, D0
        BEQ.S tkpkgEncodeExecuteProgramLoop
        MOVEA.L A3, A4

tkpkgEncodeExecuteProgramOperandLoop:
        MOVE.B (A4)+, (A2)+
        ADDQ.W #1, D1
        SUBQ.W #1, D0
        BNE.S tkpkgEncodeExecuteProgramOperandLoop
        BRA.S tkpkgEncodeExecuteProgramLoop

tkpkgEncodeExecuteProgramOk:
        MOVEQ #0, D0
        BRA.S tkpkgEncodeExecuteProgramReturn

tkpkgEncodeExecuteProgramFail:
        LEA runtimeErrorText, A1
        MOVEQ #RUNTIME_ERROR_TEXT_LEN, D1
        MOVEQ #1, D0

tkpkgEncodeExecuteProgramReturn:
        MOVEM.L (SP)+, D2-D7/A0-A4
        RTS

tkpkgEncodeReadU32Low16:
        MOVEQ #0, D0
        MOVE.B (A0)+, D0
        MOVEQ #0, D1
        MOVE.B (A0)+, D1
        LSL.W #8, D1
        OR.W D1, D0
        ADDQ.L #2, A0
        RTS

tkpkgEncodeSkipString:
        BSR.W tkpkgEncodeReadU32Low16
        ADDA.W D0, A0
        RTS

tkpkgEncodeSkipBytes:
        BSR.W tkpkgEncodeReadU32Low16
        ADDA.W D0, A0
        RTS

tkpkgEncodeStringEqIgnoreCase:
        MOVEM.L D1-D4/A1-A2, -(SP)
        CMP.W D1, D0
        BNE.S tkpkgEncodeStringEqNo
        TST.W D0
        BEQ.S tkpkgEncodeStringEqYes
        MOVE.W D0, D4
        SUBQ.W #1, D4

tkpkgEncodeStringEqLoop:
        MOVE.B (A1)+, D2
        MOVE.B (A2)+, D3
        CMPI.B #'A', D2
        BCS.S tkpkgEncodeStringEqLeftOk
        CMPI.B #'Z', D2
        BHI.S tkpkgEncodeStringEqLeftOk
        ADDI.B #32, D2

tkpkgEncodeStringEqLeftOk:
        CMPI.B #'A', D3
        BCS.S tkpkgEncodeStringEqCompare
        CMPI.B #'Z', D3
        BHI.S tkpkgEncodeStringEqCompare
        ADDI.B #32, D3

tkpkgEncodeStringEqCompare:
        CMP.B D3, D2
        BNE.S tkpkgEncodeStringEqNo
        DBRA D4, tkpkgEncodeStringEqLoop

tkpkgEncodeStringEqYes:
        MOVEQ #1, D0
        BRA.S tkpkgEncodeStringEqReturn

tkpkgEncodeStringEqNo:
        MOVEQ #0, D0

tkpkgEncodeStringEqReturn:
        MOVEM.L (SP)+, D1-D4/A1-A2
        RTS

tkpkg_service_prepare_request_v1:
        BSR.W tkpkg_service_increment_request_id_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        BSR.W tkpkg_service_write_clear_extension_fields_v1
        RTS

tkpkg_service_validate_header_v1:
        MOVEQ #0, D1
        CMPI.B #$4f, (A0)
        BNE.S tkpkgServiceBadControlBlock
        CMPI.B #$54, 1(A0)
        BNE.S tkpkgServiceBadControlBlock
        CMPI.B #$36, 2(A0)
        BNE.S tkpkgServiceBadControlBlock
        CMPI.B #$35, 3(A0)
        BNE.S tkpkgServiceBadControlBlock
        CMPI.B #$01, CB_ABI_VERSION(A0)
        BNE.S tkpkgServiceBadControlBlock
        TST.B 5(A0)
        BNE.S tkpkgServiceBadControlBlock
        CMPI.B #NATIVE_CONTROL_BLOCK_SIZE_V1, CB_STRUCT_SIZE(A0)
        BNE.S tkpkgServiceBadControlBlock
        TST.B 7(A0)
        BNE.S tkpkgServiceBadControlBlock
        RTS

tkpkgServiceBadControlBlock:
        BSR.W tkpkg_service_set_bad_control_block_v1
        MOVEQ #1, D1
        RTS

tkpkg_service_write_header_v1:
        MOVE.B #$4f, (A0)
        MOVE.B #$54, 1(A0)
        MOVE.B #$36, 2(A0)
        MOVE.B #$35, 3(A0)
        MOVE.B #$01, CB_ABI_VERSION(A0)
        CLR.B 5(A0)
        MOVE.B #NATIVE_CONTROL_BLOCK_SIZE_V1, CB_STRUCT_SIZE(A0)
        CLR.B 7(A0)
        MOVE.B #CAPABILITY_FLAGS_V1, CB_CAPABILITY_FLAGS(A0)
        CLR.B 9(A0)
        CLR.B CB_RESERVED0(A0)
        CLR.B 15(A0)
        BSR.W tkpkg_service_set_status_ok_v1
        RTS

tkpkg_service_increment_request_id_v1:
        MOVE.B nextRequestIdLo, D1
        ADDQ.B #1, D1
        MOVE.B D1, nextRequestIdLo
        BNE.S tkpkgServiceRequestIdDone
        MOVE.B nextRequestIdHi, D2
        ADDQ.B #1, D2
        MOVE.B D2, nextRequestIdHi

tkpkgServiceRequestIdDone:
        MOVE.B nextRequestIdLo, CB_REQUEST_ID(A0)
        MOVE.B nextRequestIdHi, 13(A0)
        RTS

tkpkg_service_set_bad_request_v1:
        BSR.W tkpkg_service_set_status_bad_request_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        LEA badRequestText, A1
        MOVEQ #BAD_REQUEST_TEXT_LEN, D1
        BSR.W tkpkg_service_copy_last_error_message_v1
        BSR.W tkpkg_service_write_last_error_buffer_offset_v1
        MOVE.B #BAD_REQUEST_TEXT_LEN, CB_LAST_ERROR_LEN(A0)
        CLR.B 31(A0)
        MOVE.B #BAD_REQUEST_TEXT_LEN, storedLastErrorLen
        CLR.B storedLastErrorLenHi
        MOVE.B #LAST_ERROR_KIND_BAD_REQUEST, storedLastErrorKind
        RTS

tkpkg_service_set_bad_control_block_v1:
        BSR.W tkpkg_service_set_status_bad_control_block_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        LEA controlBlockErrorText, A1
        MOVEQ #CONTROL_BLOCK_ERROR_TEXT_LEN, D1
        BSR.W tkpkg_service_copy_last_error_message_v1
        BSR.W tkpkg_service_write_last_error_buffer_offset_v1
        MOVE.B #CONTROL_BLOCK_ERROR_TEXT_LEN, CB_LAST_ERROR_LEN(A0)
        CLR.B 31(A0)
        MOVE.B #CONTROL_BLOCK_ERROR_TEXT_LEN, storedLastErrorLen
        CLR.B storedLastErrorLenHi
        MOVE.B #LAST_ERROR_KIND_BAD_CONTROL, storedLastErrorKind
        RTS

tkpkg_service_set_runtime_error_v1:
        BSR.W tkpkg_service_set_status_runtime_error_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        LEA runtimeErrorText, A1
        MOVEQ #RUNTIME_ERROR_TEXT_LEN, D1
        BSR.W tkpkg_service_set_runtime_error_message_v1
        MOVE.B #LAST_ERROR_KIND_RUNTIME, storedLastErrorKind
        MOVE.B #RUNTIME_ERROR_TEXT_LEN, storedLastErrorLen
        CLR.B storedLastErrorLenHi
        RTS

tkpkg_service_set_runtime_error_message_v1:
        BSR.W tkpkg_service_set_status_runtime_error_v1
        BSR.W tkpkg_service_write_clear_output_fields_v1
        BSR.W tkpkg_service_copy_last_error_message_v1
        BSR.W tkpkg_service_write_last_error_buffer_offset_v1
        MOVE.B D1, CB_LAST_ERROR_LEN(A0)
        CLR.B 31(A0)
        MOVE.B D1, storedLastErrorLen
        CLR.B storedLastErrorLenHi
        MOVE.B #LAST_ERROR_KIND_RUNTIME, storedLastErrorKind
        RTS

tkpkg_service_clear_stored_last_error_v1:
        CLR.B storedLastErrorLen
        CLR.B storedLastErrorLenHi
        MOVE.B #LAST_ERROR_KIND_NONE, storedLastErrorKind
        RTS

tkpkg_service_write_clear_output_fields_v1:
        CLR.B CB_OUTPUT_PTR(A0)
        CLR.B 21(A0)
        CLR.B CB_OUTPUT_LEN(A0)
        CLR.B 23(A0)
        RTS

tkpkg_service_write_clear_extension_fields_v1:
        CLR.B CB_EXTENSION_PTR(A0)
        CLR.B 25(A0)
        CLR.B CB_EXTENSION_LEN(A0)
        CLR.B 27(A0)
        RTS

tkpkg_service_write_clear_input_fields_v1:
        CLR.B CB_INPUT_PTR(A0)
        CLR.B 17(A0)
        CLR.B CB_INPUT_LEN(A0)
        CLR.B 19(A0)
        RTS

tkpkg_service_write_clear_last_error_fields_v1:
        CLR.B CB_LAST_ERROR_PTR(A0)
        CLR.B 29(A0)
        CLR.B CB_LAST_ERROR_LEN(A0)
        CLR.B 31(A0)
        RTS

tkpkg_service_write_last_error_buffer_offset_v1:
        MOVE.B #LAST_ERROR_BUFFER_PTR_V1, CB_LAST_ERROR_PTR(A0)
        CLR.B 29(A0)
        RTS

tkpkg_service_write_output_buffer_offset_v1:
        MOVE.B #LAST_ERROR_BUFFER_PTR_V1, CB_OUTPUT_PTR(A0)
        CLR.B 21(A0)
        RTS

tkpkg_service_copy_last_error_message_v1:
        LEA lastErrorBuffer, A2
        MOVE.W D1, D2
        TST.W D2
        BEQ.S tkpkgServiceCopyLastErrorDone

tkpkgServiceCopyLastErrorLoop:
        MOVE.B (A1)+, (A2)+
        SUBQ.W #1, D2
        BNE.S tkpkgServiceCopyLastErrorLoop

tkpkgServiceCopyLastErrorDone:
        CLR.B (A2)
        RTS

tkpkg_service_set_status_ok_v1:
        CLR.B CB_STATUS_CODE(A0)
        CLR.B 11(A0)
        RTS

tkpkg_service_set_status_bad_control_block_v1:
        MOVE.B #STATUS_BAD_CONTROL_BLOCK_V1, CB_STATUS_CODE(A0)
        CLR.B 11(A0)
        RTS

tkpkg_service_set_status_bad_request_v1:
        MOVE.B #STATUS_BAD_REQUEST_V1, CB_STATUS_CODE(A0)
        CLR.B 11(A0)
        RTS

tkpkg_service_set_status_runtime_error_v1:
        MOVE.B #STATUS_RUNTIME_ERROR_V1, CB_STATUS_CODE(A0)
        CLR.B 11(A0)
        RTS

        .endsection
        .endmodule
