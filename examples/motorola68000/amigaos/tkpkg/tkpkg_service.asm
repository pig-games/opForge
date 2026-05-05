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
        .use tkpkg.amigaos.buffers (lastErrorBuffer)
        .use tkpkg.amigaos.package_loader (tkpkg_package_loader_load_v1)
        .use tkpkg.amigaos.pipeline (tkpkg_pipeline_set_active_v1)
        .use tkpkg.amigaos.tokenizer_vm (tkpkg_tokenizer_vm_tokenize_line_v1)

        .section code, kind=code

tkpkg_service_bootstrap_v1:
        LEA controlBlockV1, A0
        MOVEQ #ENTRY_ORD_INIT, D0
        BSR.W tkpkg_service_dispatch_v1
        RTS

tkpkg_service_dispatch_v1:
        CMPI.B #ENTRY_ORD_INIT, D0
        BEQ.S tkpkgServiceHandleInitEntry
        BSR.W tkpkg_service_prepare_request_v1
        BSR.W tkpkg_service_validate_header_v1
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
        BEQ.S tkpkgServiceDeferredRuntime
        CMPI.B #ENTRY_ORD_ENCODE_INSTRUCTION, D0
        BEQ.S tkpkgServiceDeferredRuntime
        BSR.W tkpkg_service_set_bad_request_v1
        RTS

tkpkgServiceDeferredRuntime:
        BSR.W tkpkg_service_set_runtime_error_v1
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