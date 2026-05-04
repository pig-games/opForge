; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

        .module main
        .cpu 68020
        .use tkpkg.amigaos.abi (ENTRY_ORD_INIT, ENTRY_ORD_LOAD_PACKAGE)
        .use tkpkg.amigaos.abi (ENTRY_ORD_SET_PIPELINE, ENTRY_ORD_TOKENIZE_LINE)
        .use tkpkg.amigaos.abi (CB_INPUT_PTR, CB_INPUT_LEN, CB_OUTPUT_LEN, CB_STATUS_CODE)
        .use tkpkg.amigaos.buffers (controlBlockV1, lastErrorBuffer, packageStorage)
        .use tkpkg.amigaos.buffers (tokenRecordBuffer, tokenScratchBuffer)
        .use tkpkg.amigaos.buffers (lastTokenCount, lastLexemeLen, TOKEN_RECORD_SIZE)
        .use tkpkg.amigaos.buffers (PACKAGE_STORAGE_CAPACITY)
        .use tkpkg.amigaos.buffers (LAST_ERROR_BUFFER_PTR_V1, LAST_ERROR_BUFFER_CAPACITY)
        .use tkpkg.amigaos.service (tkpkg_service_dispatch_v1)
        .use prvm.amigaos.line_router (prvm_route_line_68000)

SysBase                         = 4

pr_CLI                          = 172
pr_MsgPort                      = 92

OpenLibrary                     = -552
CloseLibrary                    = -414
FindTask                        = -294
WaitPort                        = -384
GetMsg                          = -372
ReplyMsg                        = -378
Forbid                          = -132

Open                            = -30
Close                           = -36
Read                            = -42
PutStr                          = -948
GetArgStr                       = -534

MODE_OLDFILE                    = 1005

RETURN_OK                       = 0
RETURN_USAGE                    = 20
RETURN_FILE_FAILURE             = 21
RETURN_RUNTIME_FAILURE          = 22
RETURN_NOT_IMPLEMENTED          = 30
RETURN_WORKBENCH_UNSUPPORTED    = 31

PATH_BUFFER_CAPACITY            = 256
TOKEN_BUFFER_CAPACITY           = 64
SOURCE_LINE_BUFFER_CAPACITY     = 512
NATIVE_MODULE_TABLE_CAPACITY    = 16
NATIVE_IMPORT_TABLE_CAPACITY    = 32
NATIVE_MODULE_PATH_CAPACITY     = 8
NATIVE_IMPORT_SELECT_CAPACITY   = 64
NATIVE_MODULE_USE_STATE_BYTES   = (7 * 2) + (NATIVE_MODULE_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_MODULE_TABLE_CAPACITY * 2) + (NATIVE_MODULE_TABLE_CAPACITY * 4) + (NATIVE_MODULE_TABLE_CAPACITY * 2) + (NATIVE_IMPORT_TABLE_CAPACITY * 2) + (NATIVE_IMPORT_TABLE_CAPACITY * 2) + (NATIVE_IMPORT_TABLE_CAPACITY * 2) + (NATIVE_IMPORT_TABLE_CAPACITY * 4) + (NATIVE_IMPORT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_IMPORT_SELECT_CAPACITY * 2) + (NATIVE_IMPORT_SELECT_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_IMPORT_SELECT_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_IMPORT_SELECT_CAPACITY * 2) + (NATIVE_MODULE_PATH_CAPACITY * PATH_BUFFER_CAPACITY)
PACKAGE_INPUT_PTR_V1            = LAST_ERROR_BUFFER_PTR_V1 + LAST_ERROR_BUFFER_CAPACITY
NATIVE_INCLUDE_DEPTH_LIMIT      = 1
PRVM_ROUTE_MAGIC_OPLR           = $4F504C52
PRVM_ROUTE_FRAME_SIZE           = 116
PRVM_ROUTE_ABI_VERSION_V1       = 1
PRVM_PARSER_CONTRACT_VERSION_V2 = 2
PRVM_ROUTE_RESULT_CAPACITY      = 128
PRVM_ROUTE_DIAG_CAPACITY        = 32
PRVM_ROUTE_RESUME_CAPACITY      = 40
PRVM_ROUTE_EXPR_REQUEST_SIZE    = 32
PRVM_ROUTE_EXPR_RESULT_COUNT    = 0
PRVM_ROUTE_STEP_BUDGET          = 256
PRVM_STATUS_OK                  = 0
PRVM_RESULT_MNEMONIC_TEXT       = 3
OPFORGE_NATIVE_CLI_PRVM_PROGRAM_LEN = 13
NATIVE_TOKEN_RECORD_SIZE        = 20
TK_KIND_IDENTIFIER              = 0
NCLI_PARSER_DIRECTIVE_NONE      = 0
NCLI_PARSER_DIRECTIVE_MODULE    = 1
NCLI_PARSER_DIRECTIVE_ENDMODULE = 2
NCLI_PARSER_DIRECTIVE_USE       = 3

        .section entry, kind=code

start:
        MOVEM.L D2-D7/A2-A6, -(SP)
        CLR.L D2

        SUBA.L A1, A1
        MOVEA.L SysBase.W, A6
        JSR FindTask(A6)

        MOVEA.L D0, A2
        TST.L pr_CLI(A2)
        BNE.W opforgeStartCli

        LEA pr_MsgPort(A2), A0
        JSR WaitPort(A6)
        LEA pr_MsgPort(A2), A0
        JSR GetMsg(A6)
        MOVE.L D0, D2
        MOVEQ #RETURN_WORKBENCH_UNSUPPORTED, D7
        BRA.W opforgeStartReply

opforgeStartCli:
        BSR.W opforge_native_cli_run
        MOVE.L D0, D7

opforgeStartReply:
        TST.L D2
        BEQ.W opforgeStartDone
        JSR Forbid(A6)
        MOVEA.L D2, A1
        JSR ReplyMsg(A6)

opforgeStartDone:
        MOVE.L D7, D0
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

opforge_native_cli_run:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVE.L #RETURN_USAGE, nativeCliReturnCode

        LEA dosName, A1
        MOVEQ #36, D0
        MOVEA.L SysBase.W, A6
        JSR OpenLibrary(A6)
        TST.L D0
        BNE.S opforgeNativeCliHaveDos

        LEA dosName, A1
        MOVEQ #0, D0
        MOVEA.L SysBase.W, A6
        JSR OpenLibrary(A6)
        TST.L D0
        BEQ.W opforgeNativeCliDone

opforgeNativeCliHaveDos:
        MOVE.L D0, nativeCliDosBase
        BSR.W opforge_native_cli_init_module_use_state
        MOVEA.L D0, A6
        JSR GetArgStr(A6)
.ifdef OPFORGE_FS_UAE_SMOKE
        LEA defaultFsUaeArgTail, A0
.else
        MOVEA.L D0, A0
.endif
        BSR.W opforge_native_cli_parse_args

        CMPI.W #NCLI_PARSE_HELP, D0
        BEQ.W opforgeNativeCliHelp
        CMPI.W #NCLI_PARSE_VERSION, D0
        BEQ.W opforgeNativeCliVersion
        TST.W D0
        BEQ.W opforgeNativeCliParsed

        BSR.W opforge_native_cli_report_parse_error
        MOVE.L #RETURN_USAGE, nativeCliReturnCode
        BRA.W opforgeNativeCliCloseDos

opforgeNativeCliHelp:
        MOVE.L #helpText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #RETURN_OK, nativeCliReturnCode
        BRA.W opforgeNativeCliCloseDos

opforgeNativeCliVersion:
        MOVE.L #versionText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #RETURN_OK, nativeCliReturnCode
        BRA.W opforgeNativeCliCloseDos

opforgeNativeCliParsed:
        LEA nativeCliInputPath, A0
        BSR.W opforge_native_cli_open_input
        TST.L D0
        BNE.S opforgeNativeCliInputOpened
        MOVE.L #inputOpenErrorText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliInputPath, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #RETURN_FILE_FAILURE, nativeCliReturnCode
        BRA.W opforgeNativeCliCloseDos

opforgeNativeCliInputOpened:
        MOVE.L D0, D1
        BSR.W opforge_native_cli_close
        MOVE.L #stubHeaderText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #inputLabelText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliInputPath, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #hunkLabelText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliHunkPath, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        BSR.W opforgeNativeCliEmitModulePathRecords
        BSR.W opforge_native_cli_tokenize_frontend
        TST.L D0
        BEQ.S opforgeNativeCliTokenizerOk
        MOVE.L #tokenizerFailureText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #RETURN_RUNTIME_FAILURE, nativeCliReturnCode
        BRA.W opforgeNativeCliCloseDos

opforgeNativeCliTokenizerOk:
        MOVE.L #parserOkText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #emitterStubText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #RETURN_NOT_IMPLEMENTED, nativeCliReturnCode

opforgeNativeCliCloseDos:
        MOVE.L nativeCliDosBase, D0
        BEQ.S opforgeNativeCliDone
        MOVEA.L SysBase.W, A6
        MOVEA.L D0, A1
        JSR CloseLibrary(A6)
        CLR.L nativeCliDosBase

opforgeNativeCliDone:
        MOVE.L nativeCliReturnCode, D0
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

opforge_native_cli_put_str:
        MOVEA.L nativeCliDosBase, A6
        JSR PutStr(A6)
        RTS

opforge_native_cli_open_input:
        MOVE.L A0, D1
        MOVE.L #MODE_OLDFILE, D2
        MOVEA.L nativeCliDosBase, A6
        JSR Open(A6)
        RTS

opforge_native_cli_close:
        MOVEA.L nativeCliDosBase, A6
        JSR Close(A6)
        RTS

opforge_native_cli_read_input:
        MOVE.L A0, D2
        MOVE.L D0, D3
        MOVEA.L nativeCliDosBase, A6
        JSR Read(A6)
        RTS

opforge_native_cli_copy_bytes:
        MOVE.W D0, D2
        TST.W D2
        BEQ.S opforgeNativeCliCopyBytesDone

opforgeNativeCliCopyBytesLoop:
        MOVE.B (A1)+, (A2)+
        SUBQ.W #1, D2
        BNE.S opforgeNativeCliCopyBytesLoop

opforgeNativeCliCopyBytesDone:
        RTS

opforge_native_cli_copy_c_string:
        MOVEQ #0, D0

opforgeNativeCliCopyCStringLoop:
        MOVE.B (A0)+, D1
        MOVE.B D1, (A1)+
        ADDQ.W #1, D0
        TST.B D1
        BNE.S opforgeNativeCliCopyCStringLoop
        RTS

opforge_native_cli_copy_fixed_string:
        MOVE.W D0, D2
        TST.W D2
        BEQ.S opforgeNativeCliCopyFixedStringDone

opforgeNativeCliCopyFixedStringLoop:
        MOVE.B (A0)+, (A1)+
        SUBQ.W #1, D2
        BNE.S opforgeNativeCliCopyFixedStringLoop

opforgeNativeCliCopyFixedStringDone:
        RTS

opforge_native_cli_write_input_window:
        MOVE.B D0, CB_INPUT_PTR(A0)
        LSR.W #8, D0
        MOVE.B D0, 17(A0)
        MOVE.B D1, CB_INPUT_LEN(A0)
        LSR.W #8, D1
        MOVE.B D1, 19(A0)
        RTS

opforge_native_cli_read_status:
        MOVEQ #0, D0
        MOVE.B CB_STATUS_CODE(A0), D0
        RTS

opforge_native_cli_read_output_len:
        MOVEQ #0, D0
        MOVE.B CB_OUTPUT_LEN(A0), D0
        MOVEQ #0, D1
        MOVE.B 23(A0), D1
        LSL.W #8, D1
        OR.W D1, D0
        RTS

opforge_native_cli_tokenize_frontend:
        MOVEM.L D2-D7/A2-A6, -(SP)
        BSR.W opforge_native_cli_init_package_pipeline
        TST.L D0
        BNE.W opforgeNativeCliTokenizeReturn
        MOVE.L #tokenizerOkText, D1
        BSR.W opforge_native_cli_put_str
        BSR.W opforge_native_cli_tokenize_file
        TST.L D0
        BNE.W opforgeNativeCliTokenizeReturn

opforgeNativeCliTokenizeSuccess:
        MOVEQ #0, D0

opforgeNativeCliTokenizeReturn:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

opforge_native_cli_tokenize_file:
        LEA nativeCliInputPath, A0
        LEA nativeCliCurrentPath, A1
        BSR.W opforgeNativeCliCopyPathBuffer
        TST.L D0
        BNE.S opforgeNativeCliTokenizeFilePathFail
        LEA nativeCliInputPath, A0
        BSR.W opforge_native_cli_tokenize_file_at_path
        RTS

opforgeNativeCliTokenizeFilePathFail:
        MOVEQ #1, D0
        RTS

opforge_native_cli_tokenize_file_at_path:
        BSR.W opforge_native_cli_open_input
        TST.L D0
        BNE.S opforgeNativeCliTokenizeFileOpenOk
        MOVEQ #1, D0
        RTS

opforgeNativeCliTokenizeFileOpenOk:
        MOVE.L D0, D5
        MOVE.L #1, nativeCliSourceLineNum
        CLR.W nativeCliSourceLineLen
        CLR.W nativeCliSawCr

opforgeNativeCliTokenizeFileReadLoop:
        LEA nativeCliInputChar, A0
        MOVEQ #1, D0
        MOVE.L D5, D1
        BSR.W opforge_native_cli_read_input
        CMP.L #-1, D0
        BEQ.W opforgeNativeCliTokenizeFileFailClose
        TST.L D0
        BEQ.W opforgeNativeCliTokenizeFileEof

        MOVE.B nativeCliInputChar, D0
        TST.W nativeCliSawCr
        BEQ.S opforgeNativeCliTokenizeFileCheckBreak
        CLR.W nativeCliSawCr
        CMPI.B #10, D0
        BEQ.W opforgeNativeCliTokenizeFileReadLoop

opforgeNativeCliTokenizeFileCheckBreak:
        CMPI.B #10, D0
        BEQ.S opforgeNativeCliTokenizeFileLineDone
        CMPI.B #13, D0
        BEQ.S opforgeNativeCliTokenizeFileCrDone

        MOVE.W nativeCliSourceLineLen, D1
        CMPI.W #SOURCE_LINE_BUFFER_CAPACITY, D1
        BHS.W opforgeNativeCliTokenizeFileFailClose
        LEA nativeCliSourceLine, A1
        MOVE.B D0, 0(A1,D1.W)
        ADDQ.W #1, D1
        MOVE.W D1, nativeCliSourceLineLen
        BRA.W opforgeNativeCliTokenizeFileReadLoop

opforgeNativeCliTokenizeFileCrDone:
        MOVE.W #1, nativeCliSawCr

opforgeNativeCliTokenizeFileLineDone:
        BSR.W opforge_native_cli_tokenize_current_line
        TST.L D0
        BNE.S opforgeNativeCliTokenizeFileFailClose
        MOVE.L nativeCliSourceLineNum, D0
        ADDQ.L #1, D0
        MOVE.L D0, nativeCliSourceLineNum
        CLR.W nativeCliSourceLineLen
        BRA.W opforgeNativeCliTokenizeFileReadLoop

opforgeNativeCliTokenizeFileEof:
        TST.W nativeCliSourceLineLen
        BEQ.S opforgeNativeCliTokenizeFileCheckModuleDepth
        BSR.W opforge_native_cli_tokenize_current_line
        TST.L D0
        BNE.S opforgeNativeCliTokenizeFileFailClose

opforgeNativeCliTokenizeFileCheckModuleDepth:
        TST.W nativeCliIncludeDepth
        BNE.S opforgeNativeCliTokenizeFileSuccessClose
        TST.W nativeCliModuleResolveDepth
        BNE.S opforgeNativeCliTokenizeFileSuccessClose
        TST.W nativeCliModuleDepth
        BEQ.S opforgeNativeCliTokenizeFileSuccessClose
        MOVE.L #moduleDepthFailureText, D1
        BSR.W opforge_native_cli_put_str
        BRA.S opforgeNativeCliTokenizeFileFailClose

opforgeNativeCliTokenizeFileSuccessClose:
        MOVE.L D5, D1
        BSR.W opforge_native_cli_close
        MOVEQ #0, D0
        RTS

opforgeNativeCliTokenizeFileFailClose:
        MOVE.L D5, D1
        BSR.W opforge_native_cli_close
        MOVEQ #1, D0
        RTS

opforge_native_cli_init_package_pipeline:
        LEA controlBlockV1, A0
        MOVEQ #ENTRY_ORD_INIT, D0
        JSR tkpkg_service_dispatch_v1
        BSR.W opforge_native_cli_read_status
        TST.B D0
        BNE.W opforgeNativeCliInitPipelineFail

        BSR.W opforge_native_cli_stage_package
        TST.L D0
        BNE.W opforgeNativeCliInitPipelineFail

        LEA controlBlockV1, A0
        MOVE.W #PACKAGE_INPUT_PTR_V1, D0
        MOVE.W nativeCliPackageLenActive, D1
        BSR.W opforge_native_cli_write_input_window
        MOVEQ #ENTRY_ORD_LOAD_PACKAGE, D0
        JSR tkpkg_service_dispatch_v1
        BSR.W opforge_native_cli_read_status
        TST.B D0
        BNE.S opforgeNativeCliInitPipelineFail

        BSR.W opforge_native_cli_prepare_pipeline_request
        TST.L D0
        BNE.S opforgeNativeCliInitPipelineFail

        LEA controlBlockV1, A0
        MOVE.W #LAST_ERROR_BUFFER_PTR_V1, D0
        MOVE.W nativeCliPipelineRequestLen, D1
        BSR.W opforge_native_cli_write_input_window
        MOVEQ #ENTRY_ORD_SET_PIPELINE, D0
        JSR tkpkg_service_dispatch_v1
        BSR.W opforge_native_cli_read_status
        TST.B D0
        BNE.S opforgeNativeCliInitPipelineFail
        MOVEQ #0, D0
        RTS

opforgeNativeCliInitPipelineFail:
        MOVEQ #1, D0
        RTS

opforge_native_cli_stage_package:
        TST.B nativeCliPackagePath
        BNE.S opforgeNativeCliStageExternalPackage

        LEA opforgeNativeCliPackageData, A1
        LEA packageStorage, A2
        MOVE.W opforgeNativeCliPackageLen, D0
        MOVE.W D0, nativeCliPackageLenActive
        BSR.W opforge_native_cli_copy_bytes
        MOVEQ #0, D0
        RTS

opforgeNativeCliStageExternalPackage:
        LEA nativeCliPackagePath, A0
        BSR.W opforge_native_cli_open_input
        TST.L D0
        BNE.S opforgeNativeCliStageExternalOpenOk
        MOVEQ #1, D0
        RTS

opforgeNativeCliStageExternalOpenOk:
        MOVE.L D0, D5
        LEA packageStorage, A0
        MOVE.L #PACKAGE_STORAGE_CAPACITY, D0
        MOVE.L D5, D1
        BSR.W opforge_native_cli_read_input
        MOVE.L D0, D6
        MOVE.L D5, D1
        BSR.W opforge_native_cli_close
        CMP.L #-1, D6
        BNE.S opforgeNativeCliStageExternalReadOk
        MOVEQ #1, D0
        RTS

opforgeNativeCliStageExternalReadOk:
        MOVE.W D6, nativeCliPackageLenActive
        MOVEQ #0, D0
        RTS

opforge_native_cli_prepare_pipeline_request:
        LEA nativeCliCpuName, A0
        TST.B (A0)
        BNE.S opforgeNativeCliPreparePipelineHaveCpu
        LEA defaultCpuName, A0

opforgeNativeCliPreparePipelineHaveCpu:
        LEA lastErrorBuffer, A1
        BSR.W opforge_native_cli_copy_c_string
        MOVE.W D0, D7
        LEA defaultFamilyName, A0
        MOVEQ #DEFAULT_FAMILY_NAME_LEN, D0
        BSR.W opforge_native_cli_copy_fixed_string
        ADD.W #DEFAULT_FAMILY_NAME_LEN, D7
        MOVE.W D7, nativeCliPipelineRequestLen
        MOVEQ #0, D0
        RTS

opforge_native_cli_tokenize_current_line:
        TST.W nativeCliIncludeDepth
        BEQ.S opforgeNativeCliTokenizeCurrentLineNoIncludeRecord
        BSR.W opforge_native_cli_emit_include_line_record

opforgeNativeCliTokenizeCurrentLineNoIncludeRecord:
        LEA lastErrorBuffer, A2
        MOVE.L nativeCliSourceLineNum, D2
        MOVE.B D2, (A2)+
        LSR.L #8, D2
        MOVE.B D2, (A2)+
        LSR.L #8, D2
        MOVE.B D2, (A2)+
        LSR.L #8, D2
        MOVE.B D2, (A2)+
        LEA nativeCliSourceLine, A1
        MOVE.W nativeCliSourceLineLen, D0
        BSR.W opforge_native_cli_copy_bytes

        LEA controlBlockV1, A0
        MOVE.W #LAST_ERROR_BUFFER_PTR_V1, D0
        MOVE.W nativeCliSourceLineLen, D1
        ADDQ.W #4, D1
        BSR.W opforge_native_cli_write_input_window
        MOVEQ #ENTRY_ORD_TOKENIZE_LINE, D0
        JSR tkpkg_service_dispatch_v1
        BSR.W opforge_native_cli_read_status
        TST.B D0
        BNE.S opforgeNativeCliTokenizeCurrentLineFail
        LEA controlBlockV1, A0
        BSR.W opforge_native_cli_read_output_len
        TST.W D0
        BEQ.S opforgeNativeCliTokenizeCurrentLineOk
        LEA lastErrorBuffer, A1
        CLR.B 0(A1,D0.W)
        MOVE.L #lastErrorBuffer, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str

opforgeNativeCliTokenizeCurrentLineOk:
        BSR.W opforge_native_cli_parse_current_line
        TST.L D0
        BNE.S opforgeNativeCliTokenizeCurrentLineFail
        MOVEQ #0, D0
        RTS

opforgeNativeCliTokenizeCurrentLineFail:
        MOVEQ #1, D0
        RTS

opforge_native_cli_emit_include_line_record:
        MOVEM.L D0-D1,-(SP)
        MOVE.L #includeLineText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.W nativeCliIncludeDepth, D0
        BSR.W opforge_native_cli_put_dec_u16
        MOVE.L #spaceText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L nativeCliSourceLineNum, D0
        BSR.W opforge_native_cli_put_dec_u16
        MOVE.L #spaceText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliCurrentPath, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVEM.L (SP)+,D0-D1
        RTS

opforge_native_cli_parse_current_line:
        MOVEM.L D2-D7/A2-A4, -(SP)
        LEA nativeCliSourceLine, A0
        MOVEQ #0, D0
        MOVE.W nativeCliSourceLineLen, D0
        BSR.W opforgeNativeCliSkipLineWhitespace
        TST.L D0
        BEQ.W opforgeNativeCliParseLineDone
        MOVEA.L A0, A4
        MOVE.L D0, D7

        LEA ifdefDirectiveText, A1
        MOVEQ #6, D1
        BSR.W opforgeNativeCliLineStartsWith
        TST.L D0
        BNE.W opforgeNativeCliParseConditionalLine

        MOVEA.L A4, A0
        MOVE.L D7, D0
        LEA ifndefDirectiveText, A1
        MOVEQ #7, D1
        BSR.W opforgeNativeCliLineStartsWith
        TST.L D0
        BNE.W opforgeNativeCliParseConditionalLine

        MOVEA.L A4, A0
        MOVE.L D7, D0
        LEA elseifDirectiveText, A1
        MOVEQ #7, D1
        BSR.W opforgeNativeCliLineStartsWith
        TST.L D0
        BNE.W opforgeNativeCliParseConditionalLine

        MOVEA.L A4, A0
        MOVE.L D7, D0
        LEA elseDirectiveText, A1
        MOVEQ #5, D1
        BSR.W opforgeNativeCliLineStartsWith
        TST.L D0
        BNE.W opforgeNativeCliParseConditionalLine

        MOVEA.L A4, A0
        MOVE.L D7, D0
        LEA endifDirectiveText, A1
        MOVEQ #6, D1
        BSR.W opforgeNativeCliLineStartsWith
        TST.L D0
        BNE.W opforgeNativeCliParseConditionalLine

        MOVEA.L A4, A0
        MOVE.L D7, D0
        LEA ifDirectiveText, A1
        MOVEQ #3, D1
        BSR.W opforgeNativeCliLineStartsWith
        TST.L D0
        BNE.W opforgeNativeCliParseConditionalLine

        MOVEA.L A4, A0
        MOVE.L D7, D0
        LEA includeDirectiveText, A1
        MOVEQ #8, D1
        BSR.W opforgeNativeCliLineStartsWith
        TST.L D0
        BNE.W opforgeNativeCliParseIncludeLine

        BSR.W opforgeNativeCliRouteParserModuleUseLine
        CMPI.W #NCLI_PARSER_DIRECTIVE_MODULE, D0
        BEQ.W opforgeNativeCliParseModuleLine
        CMPI.W #NCLI_PARSER_DIRECTIVE_ENDMODULE, D0
        BEQ.W opforgeNativeCliParseEndmoduleLine
        CMPI.W #NCLI_PARSER_DIRECTIVE_USE, D0
        BEQ.W opforgeNativeCliParseUseLine

opforgeNativeCliParseLineDone:
        MOVEQ #0, D0
        BRA.W opforgeNativeCliParseLineReturn

opforgeNativeCliParseConditionalLine:
        MOVE.L #conditionalFailureText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #1, D0
        BRA.W opforgeNativeCliParseLineReturn

opforgeNativeCliRouteParserModuleUseLine:
        MOVEM.L D1-D7/A0-A3, -(SP)
        BSR.W opforgeNativeCliBuildPrvmRouteFrame
        LEA opforgeNativeCliPrvmRouteFrame, A0
        MOVE.L #PRVM_ROUTE_FRAME_SIZE, D0
        JSR prvm_route_line_68000
        BSR.W opforgeNativeCliParserDirectiveKind
        MOVEM.L (SP)+, D1-D7/A0-A3
        RTS

opforgeNativeCliBuildPrvmRouteFrame:
        LEA opforgeNativeCliPrvmRouteFrame, A0
        MOVE.L #PRVM_ROUTE_MAGIC_OPLR, 0(A0)
        MOVE.W #PRVM_ROUTE_ABI_VERSION_V1, 4(A0)
        MOVE.W #PRVM_ROUTE_FRAME_SIZE, 6(A0)
        LEA processorAsmText, A1
        MOVE.L A1, 8(A0)
        MOVE.L #3, 12(A0)
        LEA kindStatementText, A1
        MOVE.L A1, 16(A0)
        MOVE.L #9, 20(A0)
        MOVE.L nativeCliSourceLineNum, 24(A0)
        LEA nativeCliSourceLine, A1
        MOVE.L A1, 28(A0)
        CLR.L D0
        MOVE.W nativeCliSourceLineLen, D0
        MOVE.L D0, 32(A0)
        LEA tokenRecordBuffer, A1
        MOVE.L A1, 36(A0)
        CLR.L D0
        MOVE.W lastTokenCount, D0
        MOVE.L D0, 40(A0)
        MOVE.W #TOKEN_RECORD_SIZE, 44(A0)
        CLR.W 46(A0)
        LEA tokenScratchBuffer, A1
        MOVE.L A1, 48(A0)
        CLR.L D0
        MOVE.W lastLexemeLen, D0
        MOVE.L D0, 52(A0)
        LEA opforgeNativeCliPrvmParserProgram, A1
        MOVE.L A1, 56(A0)
        MOVE.L #OPFORGE_NATIVE_CLI_PRVM_PROGRAM_LEN, 60(A0)
        LEA opforgeNativeCliPrvmResultBuffer, A1
        CLR.W (A1)
        CLR.W 32(A1)
        MOVE.L A1, 64(A0)
        MOVE.L #PRVM_ROUTE_RESULT_CAPACITY, 68(A0)
        LEA opforgeNativeCliPrvmDiagBuffer, A1
        MOVE.L A1, 72(A0)
        MOVE.L #PRVM_ROUTE_DIAG_CAPACITY, 76(A0)
        LEA opforgeNativeCliPrvmResumeBuffer, A1
        MOVE.L A1, 80(A0)
        MOVE.L #PRVM_ROUTE_RESUME_CAPACITY, 84(A0)
        LEA opforgeNativeCliPrvmExprRequest, A1
        MOVE.L A1, 88(A0)
        MOVE.L #PRVM_ROUTE_EXPR_REQUEST_SIZE, 92(A0)
        CLR.L 96(A0)
        MOVE.L #PRVM_ROUTE_EXPR_RESULT_COUNT, 100(A0)
        MOVE.L #PRVM_PARSER_CONTRACT_VERSION_V2, 104(A0)
        MOVE.L #PRVM_ROUTE_STEP_BUDGET, 108(A0)
        CLR.L 112(A0)
        RTS

opforgeNativeCliParserDirectiveKind:
        LEA opforgeNativeCliPrvmResultBuffer, A2
        CMPI.W #PRVM_RESULT_MNEMONIC_TEXT, 32(A2)
        BNE.W opforgeNativeCliParserDirectiveKindFallback
        MOVE.L 48(A2), D0
        LEA tokenScratchBuffer, A0
        ADDA.L D0, A0
        MOVE.L 52(A2), D0
        LEA moduleMnemonicText, A1
        MOVEQ #6, D1
        BSR.W opforgeNativeCliParserMnemonicEquals
        TST.L D0
        BNE.W opforgeNativeCliParserDirectiveModule
        MOVE.L 48(A2), D0
        LEA tokenScratchBuffer, A0
        ADDA.L D0, A0
        MOVE.L 52(A2), D0
        LEA endmoduleMnemonicText, A1
        MOVEQ #9, D1
        BSR.W opforgeNativeCliParserMnemonicEquals
        TST.L D0
        BNE.W opforgeNativeCliParserDirectiveEndmodule
        MOVE.L 48(A2), D0
        LEA tokenScratchBuffer, A0
        ADDA.L D0, A0
        MOVE.L 52(A2), D0
        LEA useMnemonicText, A1
        MOVEQ #3, D1
        BSR.W opforgeNativeCliParserMnemonicEquals
        TST.L D0
        BNE.W opforgeNativeCliParserDirectiveUse

opforgeNativeCliParserDirectiveKindFallback:
        LEA nativeCliSourceLine, A0
        MOVEQ #0, D0
        MOVE.W nativeCliSourceLineLen, D0
        BSR.W opforgeNativeCliSkipLineWhitespace
        LEA moduleDirectiveText, A1
        MOVEQ #7, D1
        BSR.W opforgeNativeCliParserMnemonicEquals
        TST.L D0
        BNE.S opforgeNativeCliParserDirectiveModule
        LEA nativeCliSourceLine, A0
        MOVEQ #0, D0
        MOVE.W nativeCliSourceLineLen, D0
        BSR.W opforgeNativeCliSkipLineWhitespace
        LEA endmoduleDirectiveText, A1
        MOVEQ #10, D1
        BSR.W opforgeNativeCliParserMnemonicEquals
        TST.L D0
        BNE.S opforgeNativeCliParserDirectiveEndmodule
        LEA nativeCliSourceLine, A0
        MOVEQ #0, D0
        MOVE.W nativeCliSourceLineLen, D0
        BSR.W opforgeNativeCliSkipLineWhitespace
        LEA useDirectiveText, A1
        MOVEQ #4, D1
        BSR.W opforgeNativeCliParserMnemonicEquals
        TST.L D0
        BNE.S opforgeNativeCliParserDirectiveUse
        MOVEQ #NCLI_PARSER_DIRECTIVE_NONE, D0
        RTS

opforgeNativeCliParserDirectiveModule:
        MOVEQ #NCLI_PARSER_DIRECTIVE_MODULE, D0
        RTS

opforgeNativeCliParserDirectiveEndmodule:
        MOVEQ #NCLI_PARSER_DIRECTIVE_ENDMODULE, D0
        RTS

opforgeNativeCliParserDirectiveUse:
        MOVEQ #NCLI_PARSER_DIRECTIVE_USE, D0
        RTS

opforgeNativeCliParserMnemonicEquals:
        BSR.W opforgeNativeCliLineStartsWith
        RTS

opforgeNativeCliBuildParserTailBuffer:
        MOVEM.L D1-D7/A0-A3, -(SP)
        LEA opforgeNativeCliPrvmResultBuffer, A0
        MOVE.L 44(A0), D6
        LEA tokenRecordBuffer, A2
        MOVEQ #0, D7
        MOVE.W lastTokenCount, D7

opforgeNativeCliBuildParserTailFindLoop:
        TST.W D7
        BEQ.S opforgeNativeCliBuildParserTailStart
        MOVE.L 4(A2), D0
        CMP.L D6, D0
        BHI.S opforgeNativeCliBuildParserTailStart
        LEA NATIVE_TOKEN_RECORD_SIZE(A2), A2
        SUBQ.W #1, D7
        BRA.S opforgeNativeCliBuildParserTailFindLoop

opforgeNativeCliBuildParserTailStart:
        LEA nativeCliParserTailBuffer, A1
        CLR.W nativeCliParserTailLen
        MOVEQ #0, D5
        MOVEQ #-1, D4

opforgeNativeCliBuildParserTailCopyLoop:
        TST.W D7
        BEQ.W opforgeNativeCliBuildParserTailDone
        MOVE.W 0(A2), D3
        CMPI.W #TK_KIND_IDENTIFIER, D4
        BNE.S opforgeNativeCliBuildParserTailCopyLexeme
        CMPI.W #TK_KIND_IDENTIFIER, D3
        BNE.S opforgeNativeCliBuildParserTailCopyLexeme
        MOVE.L D5, D0
        ADDQ.L #1, D0
        CMPI.L #SOURCE_LINE_BUFFER_CAPACITY - 1, D0
        BHI.W opforgeNativeCliBuildParserTailFail
        MOVE.B #' ', (A1)+
        ADDQ.L #1, D5

opforgeNativeCliBuildParserTailCopyLexeme:
        MOVE.L 16(A2), D2
        MOVE.L D5, D0
        ADD.L D2, D0
        CMPI.L #SOURCE_LINE_BUFFER_CAPACITY - 1, D0
        BHI.W opforgeNativeCliBuildParserTailFail
        MOVE.L 12(A2), D0
        LEA tokenScratchBuffer, A0
        ADDA.L D0, A0
        TST.L D2
        BEQ.S opforgeNativeCliBuildParserTailAdvance
        SUBQ.L #1, D2

opforgeNativeCliBuildParserTailByteLoop:
        MOVE.B (A0)+, (A1)+
        ADDQ.L #1, D5
        DBRA D2, opforgeNativeCliBuildParserTailByteLoop

opforgeNativeCliBuildParserTailAdvance:
        MOVE.W D3, D4
        LEA NATIVE_TOKEN_RECORD_SIZE(A2), A2
        SUBQ.W #1, D7
        BRA.W opforgeNativeCliBuildParserTailCopyLoop

opforgeNativeCliBuildParserTailDone:
        CLR.B (A1)
        MOVE.W D5, nativeCliParserTailLen
        MOVEQ #0, D0
        BRA.S opforgeNativeCliBuildParserTailReturn

opforgeNativeCliBuildParserTailFail:
        CLR.B nativeCliParserTailBuffer
        CLR.W nativeCliParserTailLen
        MOVEQ #1, D0

opforgeNativeCliBuildParserTailReturn:
        MOVEM.L (SP)+, D1-D7/A0-A3
        RTS

opforgeNativeCliParserTailPtr:
        BSR.W opforgeNativeCliBuildParserTailBuffer
        MOVE.L D0, D1
        TST.L D1
        BNE.S opforgeNativeCliParserTailPtrReturn
        LEA nativeCliParserTailBuffer, A0
        MOVEQ #0, D0
        MOVE.W nativeCliParserTailLen, D0
        MOVEQ #0, D1

opforgeNativeCliParserTailPtrReturn:
        RTS

opforgeNativeCliParseModuleLine:
        BSR.W opforgeNativeCliParserTailPtr
        TST.L D1
        BNE.W opforgeNativeCliParseLineFail
        BSR.W opforgeNativeCliSkipLineWhitespace
        LEA nativeCliArgToken, A1
        BSR.W opforgeNativeCliCopyLineWord
        TST.L D0
        BNE.W opforgeNativeCliParseLineFail
        TST.B nativeCliArgToken
        BEQ.W opforgeNativeCliParseLineFail
        BSR.W opforgeNativeCliSkipLineWhitespace
        TST.L D0
        BEQ.S opforgeNativeCliParseModuleLineRecord
        CMPI.B #';', (A0)
        BNE.W opforgeNativeCliParseLineFail

opforgeNativeCliParseModuleLineRecord:
        BSR.W opforgeNativeCliRecordModule
        TST.L D0
        BNE.W opforgeNativeCliParseLineFail
        MOVEQ #0, D0
        MOVE.W nativeCliCurrentModuleId, D0
        BSR.W opforgeNativeCliEmitModuleRecord
        MOVEQ #0, D0
        MOVE.W nativeCliCurrentModuleId, D0
        BSR.W opforgeNativeCliEmitModuleCompatibility
        BRA.W opforgeNativeCliParseLineDone

opforgeNativeCliParseEndmoduleLine:
        BSR.W opforgeNativeCliParserTailPtr
        TST.L D1
        BNE.W opforgeNativeCliParseLineFail
        BSR.W opforgeNativeCliSkipLineWhitespace
        TST.L D0
        BEQ.S opforgeNativeCliParseEndmoduleLineClose
        CMPI.B #';', (A0)
        BNE.W opforgeNativeCliParseLineFail

opforgeNativeCliParseEndmoduleLineClose:
        BSR.W opforgeNativeCliCloseModule
        TST.L D0
        BNE.W opforgeNativeCliParseModuleDepthFail
        BRA.W opforgeNativeCliParseLineDone

opforgeNativeCliParseUseLine:
        BSR.W opforgeNativeCliParserTailPtr
        TST.L D1
        BNE.W opforgeNativeCliParseLineFail
        BSR.W opforgeNativeCliSkipLineWhitespace
        LEA nativeCliArgToken, A1
        BSR.W opforgeNativeCliCopyUseToken
        TST.L D1
        BNE.W opforgeNativeCliParseLineFail
        TST.B nativeCliArgToken
        BEQ.W opforgeNativeCliParseLineFail
        CLR.B nativeCliIncludeTarget
        BSR.W opforgeNativeCliSkipLineWhitespace
        BSR.W opforgeNativeCliParseUseOptionalAlias
        TST.L D1
        BNE.W opforgeNativeCliParseLineFail
        MOVE.L D0, D5
        BSR.W opforgeNativeCliRecordImport
        TST.L D0
        BNE.W opforgeNativeCliParseLineFail
        MOVE.L D5, D0
        BSR.W opforgeNativeCliSkipLineWhitespace
        TST.L D0
        BEQ.W opforgeNativeCliParseUseBare
        CMPI.B #';', (A0)
        BEQ.W opforgeNativeCliParseUseBare
        BSR.W opforgeNativeCliEmitImportRecord
        CMPI.B #'(', (A0)
        BNE.W opforgeNativeCliParseLineFail
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BSR.W opforgeNativeCliParseUseItems
        TST.L D1
        BNE.W opforgeNativeCliParseLineFail
        BSR.W opforgeNativeCliSkipLineWhitespace
        TST.L D0
        BEQ.W opforgeNativeCliParseLineDone
        CMPI.B #';', (A0)
        BNE.W opforgeNativeCliParseLineFail
        BRA.W opforgeNativeCliParseLineDone

opforgeNativeCliParseUseBare:
        TST.B nativeCliIncludeTarget
        BNE.S opforgeNativeCliParseUseBareEmit
        TST.W nativeCliModuleResolveDepth
        BNE.S opforgeNativeCliParseUseBareEmit
        BSR.W opforgeNativeCliResolveBareUseModule
        TST.L D1
        BNE.W opforgeNativeCliParseUseResolveFail
        MOVEQ #0, D2
        MOVE.W D4, D2
        ADD.W D2, D2
        LEA nativeCliImportModuleTable, A1
        MOVE.W D0, 0(A1,D2.L)

opforgeNativeCliParseUseBareEmit:
        BSR.W opforgeNativeCliEmitImportRecord
        BRA.W opforgeNativeCliParseLineDone

opforgeNativeCliParseIncludeLine:
        LEA nativeCliSourceLine, A0
        MOVEQ #0, D0
        MOVE.W nativeCliSourceLineLen, D0
        BSR.W opforgeNativeCliSkipLineWhitespace
        ADDQ.L #8, A0
        SUBQ.L #8, D0
        BSR.W opforgeNativeCliSkipLineWhitespace
        LEA nativeCliIncludeTarget, A1
        BSR.W opforgeNativeCliCopyIncludeTarget
        TST.L D0
        BNE.W opforgeNativeCliParseIncludeFail
        TST.B nativeCliIncludeTarget
        BEQ.W opforgeNativeCliParseIncludeFail
        BSR.W opforge_native_cli_expand_include_target
        TST.L D0
        BNE.W opforgeNativeCliParseLineReturn
        BRA.W opforgeNativeCliParseLineDone

opforgeNativeCliParseIncludeFail:
        MOVE.L #includeFailureText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #1, D0
        BRA.W opforgeNativeCliParseLineReturn

opforgeNativeCliParseModuleDepthFail:
        MOVE.L #moduleDepthFailureText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #1, D0
        BRA.W opforgeNativeCliParseLineReturn

opforgeNativeCliParseUseResolveFail:
        MOVE.L #moduleResolveFailureText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliArgToken, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #1, D0
        BRA.W opforgeNativeCliParseLineReturn

opforgeNativeCliParseLineFail:
        MOVE.L #parserFailureText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #1, D0

opforgeNativeCliParseLineReturn:
        MOVEM.L (SP)+, D2-D7/A2-A4
        RTS

opforgeNativeCliSkipLineWhitespace:
        TST.L D0
        BEQ.S opforgeNativeCliSkipLineWhitespaceDone
        CMPI.B #' ', (A0)
        BEQ.S opforgeNativeCliSkipLineWhitespaceOne
        CMPI.B #9, (A0)
        BNE.S opforgeNativeCliSkipLineWhitespaceDone

opforgeNativeCliSkipLineWhitespaceOne:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BRA.S opforgeNativeCliSkipLineWhitespace

opforgeNativeCliSkipLineWhitespaceDone:
        RTS

opforgeNativeCliLineStartsWith:
        CMP.L D1, D0
        BCS.S opforgeNativeCliLineStartsNo
        MOVEA.L A0, A2
        MOVEA.L A1, A3
        MOVE.L D1, D2
        BEQ.S opforgeNativeCliLineStartsBoundary
        SUBQ.L #1, D2

opforgeNativeCliLineStartsLoop:
        MOVE.B (A2)+, D3
        MOVE.B (A3)+, D4
        CMPI.B #'A', D3
        BCS.S opforgeNativeCliLineStartsCompare
        CMPI.B #'Z', D3
        BHI.S opforgeNativeCliLineStartsCompare
        ADDI.B #32, D3

opforgeNativeCliLineStartsCompare:
        CMP.B D4, D3
        BNE.S opforgeNativeCliLineStartsNo
        DBRA D2, opforgeNativeCliLineStartsLoop

opforgeNativeCliLineStartsBoundary:
        CMP.L D1, D0
        BEQ.S opforgeNativeCliLineStartsYes
        MOVE.B 0(A0,D1.L), D3
        CMPI.B #' ', D3
        BEQ.S opforgeNativeCliLineStartsYes
        CMPI.B #9, D3
        BEQ.S opforgeNativeCliLineStartsYes
        CMPI.B #';', D3
        BEQ.S opforgeNativeCliLineStartsYes
        MOVEQ #0, D0
        RTS

opforgeNativeCliLineStartsYes:
        MOVEQ #1, D0
        RTS

opforgeNativeCliLineStartsNo:
        MOVEQ #0, D0
        RTS

opforgeNativeCliCopyLineWord:
        MOVE.L #TOKEN_BUFFER_CAPACITY - 1, D6
        CLR.L D5

opforgeNativeCliCopyLineWordLoop:
        TST.L D0
        BEQ.S opforgeNativeCliCopyLineWordDone
        MOVEQ #0, D2
        MOVE.B (A0), D2
        CMPI.B #' ', D2
        BEQ.S opforgeNativeCliCopyLineWordDone
        CMPI.B #9, D2
        BEQ.S opforgeNativeCliCopyLineWordDone
        CMPI.B #';', D2
        BEQ.S opforgeNativeCliCopyLineWordDone
        CMPI.B #'(', D2
        BEQ.S opforgeNativeCliCopyLineWordDone
        CMPI.B #',', D2
        BEQ.S opforgeNativeCliCopyLineWordDone
        TST.L D6
        BEQ.S opforgeNativeCliCopyLineWordFail
        MOVE.B D2, (A1)+
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        ADDQ.L #1, D5
        SUBQ.L #1, D6
        BRA.S opforgeNativeCliCopyLineWordLoop

opforgeNativeCliCopyLineWordDone:
        CLR.B (A1)
        MOVEQ #0, D0
        RTS

opforgeNativeCliCopyLineWordFail:
        MOVEQ #1, D0
        RTS

opforgeNativeCliCopyUseToken:
        MOVE.L #TOKEN_BUFFER_CAPACITY - 1, D6

opforgeNativeCliCopyUseTokenLoop:
        TST.L D0
        BEQ.S opforgeNativeCliCopyUseTokenDone
        MOVEQ #0, D2
        MOVE.B (A0), D2
        CMPI.B #' ', D2
        BEQ.S opforgeNativeCliCopyUseTokenDone
        CMPI.B #9, D2
        BEQ.S opforgeNativeCliCopyUseTokenDone
        CMPI.B #';', D2
        BEQ.S opforgeNativeCliCopyUseTokenDone
        CMPI.B #'(', D2
        BEQ.S opforgeNativeCliCopyUseTokenDone
        CMPI.B #')', D2
        BEQ.S opforgeNativeCliCopyUseTokenDone
        CMPI.B #',', D2
        BEQ.S opforgeNativeCliCopyUseTokenDone
        TST.L D6
        BEQ.S opforgeNativeCliCopyUseTokenFail
        MOVE.B D2, (A1)+
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        SUBQ.L #1, D6
        BRA.S opforgeNativeCliCopyUseTokenLoop

opforgeNativeCliCopyUseTokenDone:
        CLR.B (A1)
        MOVEQ #0, D1
        RTS

opforgeNativeCliCopyUseTokenFail:
        CLR.B (A1)
        MOVEQ #1, D1
        RTS

opforgeNativeCliParseUseOptionalAlias:
        MOVEM.L D0/D6/A1, -(SP)
        MOVE.L D0, D6
        LEA asKeywordText, A1
        MOVEQ #2, D1
        BSR.W opforgeNativeCliLineStartsWith
        TST.L D0
        BEQ.S opforgeNativeCliParseUseAliasNone
        MOVE.L D6, D0
        ADDQ.L #2, A0
        SUBQ.L #2, D0
        BSR.W opforgeNativeCliSkipLineWhitespace
        LEA nativeCliIncludeTarget, A1
        BSR.W opforgeNativeCliCopyUseToken
        TST.L D1
        BNE.S opforgeNativeCliParseUseAliasFail
        TST.B nativeCliIncludeTarget
        BEQ.S opforgeNativeCliParseUseAliasFail
        MOVEQ #0, D1
        BRA.S opforgeNativeCliParseUseAliasReturn

opforgeNativeCliParseUseAliasNone:
        MOVE.L D6, D0
        MOVEQ #0, D1
        BRA.S opforgeNativeCliParseUseAliasReturn

opforgeNativeCliParseUseAliasFail:
        MOVEQ #1, D1

opforgeNativeCliParseUseAliasReturn:
        MOVEM.L (SP)+, D6/A1
        ADDQ.L #4, SP
        RTS

opforgeNativeCliParseUseItems:
        MOVE.W D4, D5
        CLR.W D7
        BSR.W opforgeNativeCliSkipLineWhitespace
        TST.L D0
        BEQ.W opforgeNativeCliParseUseItemsFail
        CMPI.B #')', (A0)
        BEQ.W opforgeNativeCliParseUseItemsFail
        CMPI.B #'*', (A0)
        BEQ.W opforgeNativeCliParseUseWildcard

opforgeNativeCliParseUseItemLoop:
        LEA nativeCliArgToken, A1
        BSR.W opforgeNativeCliCopyUseToken
        TST.L D1
        BNE.W opforgeNativeCliParseUseItemsFail
        TST.B nativeCliArgToken
        BEQ.W opforgeNativeCliParseUseItemsFail
        CMPI.B #'*', nativeCliArgToken
        BNE.S opforgeNativeCliParseUseItemNameOk
        LEA nativeCliArgToken, A1
        TST.B 1(A1)
        BEQ.W opforgeNativeCliParseUseItemsFail

opforgeNativeCliParseUseItemNameOk:
        CLR.B nativeCliIncludeTarget
        BSR.W opforgeNativeCliSkipLineWhitespace
        BSR.W opforgeNativeCliParseUseOptionalAlias
        TST.L D1
        BNE.W opforgeNativeCliParseUseItemsFail
        MOVEQ #0, D3
        TST.B nativeCliIncludeTarget
        BEQ.S opforgeNativeCliParseUseItemNoAliasFlag
        MOVEQ #1, D3

opforgeNativeCliParseUseItemNoAliasFlag:
        MOVE.L D0, -(SP)
        MOVE.W D5, D4
        BSR.W opforgeNativeCliRecordImportSelect
        TST.L D0
        BNE.W opforgeNativeCliParseUseItemsFailPop
        BSR.W opforgeNativeCliEmitImportSelectRecord
        MOVE.L (SP)+, D0
        ADDQ.W #1, D7
        BSR.W opforgeNativeCliSkipLineWhitespace
        TST.L D0
        BEQ.W opforgeNativeCliParseUseItemsFail
        CMPI.B #')', (A0)
        BEQ.S opforgeNativeCliParseUseItemsClose
        CMPI.B #',', (A0)
        BNE.W opforgeNativeCliParseUseItemsFail
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BSR.W opforgeNativeCliSkipLineWhitespace
        BRA.W opforgeNativeCliParseUseItemLoop

opforgeNativeCliParseUseItemsClose:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        MOVEQ #0, D1
        RTS

opforgeNativeCliParseUseWildcard:
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        BSR.W opforgeNativeCliSkipLineWhitespace
        MOVE.L D0, D6
        LEA asKeywordText, A1
        MOVEQ #2, D1
        BSR.W opforgeNativeCliLineStartsWith
        TST.L D0
        BNE.S opforgeNativeCliParseUseItemsFail
        MOVE.L D6, D0
        TST.L D0
        BEQ.S opforgeNativeCliParseUseItemsFail
        CMPI.B #')', (A0)
        BNE.S opforgeNativeCliParseUseItemsFail
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        MOVEQ #0, D3
        MOVE.W D5, D4
        BSR.W opforgeNativeCliEmitImportWildcardRecord
        MOVEQ #0, D1
        RTS

opforgeNativeCliParseUseItemsFailPop:
        ADDQ.L #4, SP

opforgeNativeCliParseUseItemsFail:
        MOVEQ #1, D1
        RTS

opforgeNativeCliRecordModule:
        MOVEM.L D1-D3/A0-A1, -(SP)
        MOVEQ #0, D0
        MOVE.W nativeCliModuleCount, D0
        CMPI.W #NATIVE_MODULE_TABLE_CAPACITY, D0
        BHS.W opforgeNativeCliRecordModuleFail
        MOVE.W D0, D3
        LEA nativeCliArgToken, A0
        LEA nativeCliModuleNameTable, A1
        MOVEQ #0, D1
        MOVE.W D3, D1
        LSL.L #6, D1
        ADDA.L D1, A1
        BSR.W opforgeNativeCliCopyTokenBuffer

        MOVEQ #0, D1
        MOVE.W D3, D1
        ADD.W D1, D1
        LEA nativeCliModuleFileIdTable, A1
        MOVE.W #1, 0(A1,D1.L)
        LEA nativeCliModuleDepthTable, A1
        MOVE.W nativeCliModuleDepth, 0(A1,D1.L)

        MOVEQ #0, D1
        MOVE.W D3, D1
        LSL.L #2, D1
        LEA nativeCliModuleLineTable, A1
        MOVE.L nativeCliSourceLineNum, 0(A1,D1.L)

        TST.W nativeCliModuleCount
        BNE.S opforgeNativeCliRecordModuleHaveRoot
        MOVE.W D3, nativeCliRootModuleId

opforgeNativeCliRecordModuleHaveRoot:
        MOVE.W D3, nativeCliCurrentModuleId
        MOVE.W nativeCliModuleCount, D0
        ADDQ.W #1, D0
        MOVE.W D0, nativeCliModuleCount
        MOVE.W nativeCliModuleDepth, D0
        ADDQ.W #1, D0
        MOVE.W D0, nativeCliModuleDepth
        MOVEQ #0, D0
        BRA.S opforgeNativeCliRecordModuleReturn

opforgeNativeCliRecordModuleFail:
        MOVEQ #1, D0

opforgeNativeCliRecordModuleReturn:
        MOVEM.L (SP)+, D1-D3/A0-A1
        RTS

opforgeNativeCliRecordImport:
        MOVEM.L D1-D3/A0-A1, -(SP)
        MOVEQ #0, D0
        MOVE.W nativeCliImportCount, D0
        CMPI.W #NATIVE_IMPORT_TABLE_CAPACITY, D0
        BHS.W opforgeNativeCliRecordImportFail
        MOVE.W D0, D4
        MOVEQ #0, D1
        MOVE.W D4, D1
        ADD.W D1, D1
        LEA nativeCliImportOwnerModuleTable, A1
        MOVE.W nativeCliCurrentModuleId, 0(A1,D1.L)
        LEA nativeCliImportModuleTable, A1
        CLR.W 0(A1,D1.L)
        LEA nativeCliImportFileIdTable, A1
        MOVE.W #1, 0(A1,D1.L)

        MOVEQ #0, D1
        MOVE.W D4, D1
        LSL.L #2, D1
        LEA nativeCliImportLineTable, A1
        MOVE.L nativeCliSourceLineNum, 0(A1,D1.L)

        MOVEQ #0, D1
        MOVE.W D4, D1
        LSL.L #6, D1
        LEA nativeCliImportAliasTable, A1
        ADDA.L D1, A1
        LEA nativeCliIncludeTarget, A0
        BSR.W opforgeNativeCliCopyTokenBuffer

        MOVE.W nativeCliImportCount, D0
        ADDQ.W #1, D0
        MOVE.W D0, nativeCliImportCount
        MOVEQ #0, D0
        BRA.S opforgeNativeCliRecordImportReturn

opforgeNativeCliRecordImportFail:
        MOVEQ #1, D0

opforgeNativeCliRecordImportReturn:
        MOVEM.L (SP)+, D1-D3/A0-A1
        RTS

opforgeNativeCliRecordImportSelect:
        MOVEM.L D1-D3/A0-A1, -(SP)
        MOVEQ #0, D0
        MOVE.W nativeCliImportSelectCount, D0
        CMPI.W #NATIVE_IMPORT_SELECT_CAPACITY, D0
        BHS.W opforgeNativeCliRecordImportSelectFail
        MOVE.W D0, D6
        MOVEQ #0, D1
        MOVE.W D6, D1
        ADD.W D1, D1
        LEA nativeCliImportSelectImportTable, A1
        MOVE.W D4, 0(A1,D1.L)
        LEA nativeCliImportSelectFlagsTable, A1
        MOVE.W D3, 0(A1,D1.L)

        MOVEQ #0, D1
        MOVE.W D6, D1
        LSL.L #6, D1
        LEA nativeCliImportSelectNameTable, A1
        ADDA.L D1, A1
        LEA nativeCliArgToken, A0
        BSR.W opforgeNativeCliCopyTokenBuffer

        MOVEQ #0, D1
        MOVE.W D6, D1
        LSL.L #6, D1
        LEA nativeCliImportSelectAliasTable, A1
        ADDA.L D1, A1
        LEA nativeCliIncludeTarget, A0
        BSR.W opforgeNativeCliCopyTokenBuffer

        MOVE.W nativeCliImportSelectCount, D0
        ADDQ.W #1, D0
        MOVE.W D0, nativeCliImportSelectCount
        MOVEQ #0, D0
        BRA.S opforgeNativeCliRecordImportSelectReturn

opforgeNativeCliRecordImportSelectFail:
        MOVEQ #1, D0

opforgeNativeCliRecordImportSelectReturn:
        MOVEM.L (SP)+, D1-D3/A0-A1
        RTS

opforgeNativeCliEmitImportRecord:
        MOVEM.L D0-D4/A0-A1, -(SP)
        MOVE.L #useImportText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #0, D0
        MOVE.W D4, D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace

        MOVEQ #0, D0
        MOVE.W D4, D0
        ADD.W D0, D0
        LEA nativeCliImportOwnerModuleTable, A0
        MOVE.W 0(A0,D0.L), D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace

        MOVEQ #0, D0
        MOVE.W D4, D0
        ADD.W D0, D0
        LEA nativeCliImportModuleTable, A0
        MOVE.W 0(A0,D0.L), D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace

        MOVEQ #0, D0
        MOVE.W D4, D0
        ADD.W D0, D0
        LEA nativeCliImportFileIdTable, A0
        MOVE.W 0(A0,D0.L), D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace

        MOVEQ #0, D0
        MOVE.W D4, D0
        LSL.L #2, D0
        LEA nativeCliImportLineTable, A0
        MOVE.L 0(A0,D0.L), D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace

        BSR.W opforgeNativeCliImportAliasPtr
        BSR.W opforgeNativeCliTokenLen
        MOVE.W D0, D3
        BSR.W opforge_native_cli_put_dec_u16
        TST.W D3
        BEQ.S opforgeNativeCliEmitImportRecordNewline
        BSR.W opforgeNativeCliPutSpace
        BSR.W opforgeNativeCliImportAliasPtr
        MOVE.L A0, D1
        BSR.W opforge_native_cli_put_str

opforgeNativeCliEmitImportRecordNewline:
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVEM.L (SP)+, D0-D4/A0-A1
        RTS

opforgeNativeCliEmitImportSelectRecord:
        MOVEM.L D0-D4/D6-D7/A0-A1, -(SP)
        MOVE.L #useSelectText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #0, D0
        MOVE.W D4, D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace
        MOVEQ #0, D0
        MOVE.W D7, D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace

        BSR.W opforgeNativeCliImportSelectNamePtr
        BSR.W opforgeNativeCliTokenLen
        MOVE.W D0, D3
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace
        BSR.W opforgeNativeCliImportSelectNamePtr
        MOVE.L A0, D1
        BSR.W opforge_native_cli_put_str
        BSR.W opforgeNativeCliPutSpace

        BSR.W opforgeNativeCliImportSelectAliasPtr
        BSR.W opforgeNativeCliTokenLen
        MOVE.W D0, D3
        BSR.W opforge_native_cli_put_dec_u16
        TST.W D3
        BEQ.S opforgeNativeCliEmitImportSelectFlags
        BSR.W opforgeNativeCliPutSpace
        BSR.W opforgeNativeCliImportSelectAliasPtr
        MOVE.L A0, D1
        BSR.W opforge_native_cli_put_str

opforgeNativeCliEmitImportSelectFlags:
        BSR.W opforgeNativeCliPutSpace
        MOVEQ #0, D0
        MOVE.W D6, D0
        ADD.W D0, D0
        LEA nativeCliImportSelectFlagsTable, A0
        MOVE.W 0(A0,D0.L), D0
        BSR.W opforge_native_cli_put_dec_u16
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVEM.L (SP)+, D0-D4/D6-D7/A0-A1
        RTS

opforgeNativeCliEmitImportWildcardRecord:
        MOVEM.L D0-D4, -(SP)
        MOVE.L #useWildcardText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #0, D0
        MOVE.W D4, D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace
        MOVEQ #0, D0
        BSR.W opforge_native_cli_put_dec_u16
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVEM.L (SP)+, D0-D4
        RTS

opforgeNativeCliResolveBareUseModule:
        MOVEM.L D2-D7/A0-A1, -(SP)
        CLR.W D7

opforgeNativeCliResolveBareUseLoop:
        MOVE.W nativeCliModulePathCount, D0
        CMP.W D0, D7
        BHS.W opforgeNativeCliResolveBareUseFail
        MOVEQ #0, D0
        MOVE.W D7, D0
        LSL.L #8, D0
        LEA nativeCliModulePathTable, A0
        ADDA.L D0, A0
        LEA nativeCliIncludePath, A1
        BSR.W opforgeNativeCliCopyPathBuffer
        TST.L D0
        BNE.W opforgeNativeCliResolveBareUseFail
        LEA nativeCliArgToken, A0
        LEA nativeCliIncludePath, A1
        BSR.W opforgeNativeCliAppendPathBuffer
        TST.L D0
        BNE.W opforgeNativeCliResolveBareUseFail
        LEA moduleSourceExtensionText, A0
        LEA nativeCliIncludePath, A1
        BSR.W opforgeNativeCliAppendPathBuffer
        TST.L D0
        BNE.W opforgeNativeCliResolveBareUseFail
        LEA nativeCliIncludePath, A0
        BSR.W opforge_native_cli_open_input
        TST.L D0
        BNE.S opforgeNativeCliResolveBareUseFound
        ADDQ.W #1, D7
        BRA.W opforgeNativeCliResolveBareUseLoop

opforgeNativeCliResolveBareUseFound:
        MOVE.L D0, D1
        BSR.W opforge_native_cli_close
        MOVE.W nativeCliModuleCount, D6
        MOVE.W D6, nativeCliResolvedModuleId
        MOVE.W nativeCliSourceLineLen, D0
        MOVE.W D0, nativeCliModuleSavedLineLen
        MOVE.W nativeCliSawCr, D0
        MOVE.W D0, nativeCliModuleSavedSawCr
        MOVE.L nativeCliSourceLineNum, D0
        MOVE.L D0, nativeCliModuleSavedLineNum
        LEA nativeCliCurrentPath, A0
        LEA nativeCliModuleSavedPath, A1
        BSR.W opforgeNativeCliCopyPathBuffer
        TST.L D0
        BNE.W opforgeNativeCliResolveBareUseFail
        LEA nativeCliIncludePath, A0
        LEA nativeCliCurrentPath, A1
        BSR.W opforgeNativeCliCopyPathBuffer
        TST.L D0
        BNE.W opforgeNativeCliResolveBareUseFail
        LEA nativeCliIncludePath, A0
        MOVE.W #1, nativeCliModuleResolveDepth
        BSR.W opforge_native_cli_tokenize_file_at_path
        CLR.W nativeCliModuleResolveDepth
        TST.L D0
        BNE.S opforgeNativeCliResolveBareUseRestoreFail
        MOVEQ #0, D1
        BRA.S opforgeNativeCliResolveBareUseRestore

opforgeNativeCliResolveBareUseRestoreFail:
        MOVEQ #1, D1

opforgeNativeCliResolveBareUseRestore:
        MOVE.W nativeCliModuleSavedLineLen, D2
        MOVE.W D2, nativeCliSourceLineLen
        MOVE.W nativeCliModuleSavedSawCr, D2
        MOVE.W D2, nativeCliSawCr
        MOVE.L nativeCliModuleSavedLineNum, D2
        MOVE.L D2, nativeCliSourceLineNum
        LEA nativeCliModuleSavedPath, A0
        LEA nativeCliCurrentPath, A1
        BSR.W opforgeNativeCliCopyPathBuffer
        TST.L D0
        BNE.S opforgeNativeCliResolveBareUseRestoreCopyFail
        TST.L D1
        BNE.S opforgeNativeCliResolveBareUseReturn
        MOVEQ #0, D0
        MOVE.W nativeCliResolvedModuleId, D0
        BRA.S opforgeNativeCliResolveBareUseReturn

opforgeNativeCliResolveBareUseRestoreCopyFail:
        MOVEQ #1, D1

opforgeNativeCliResolveBareUseFail:
        MOVEQ #1, D1

opforgeNativeCliResolveBareUseReturn:
        MOVEM.L (SP)+, D2-D7/A0-A1
        RTS

opforgeNativeCliEmitModuleRecord:
        MOVEM.L D0-D4/A0-A1, -(SP)
        MOVE.W D0, D4
        CMP.W nativeCliRootModuleId, D4
        BNE.S opforgeNativeCliEmitModuleRecordDef
        MOVE.L #modRootText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #0, D0
        MOVE.W D4, D0
        BSR.W opforge_native_cli_put_dec_u16
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str

opforgeNativeCliEmitModuleRecordDef:
        MOVE.L #modDefText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #0, D0
        MOVE.W D4, D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace

        MOVEQ #0, D0
        MOVE.W D4, D0
        ADD.W D0, D0
        LEA nativeCliModuleFileIdTable, A0
        MOVE.W 0(A0,D0.L), D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace

        MOVEQ #0, D0
        MOVE.W D4, D0
        LSL.L #2, D0
        LEA nativeCliModuleLineTable, A0
        MOVE.L 0(A0,D0.L), D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace

        MOVEQ #0, D0
        MOVE.W D4, D0
        ADD.W D0, D0
        LEA nativeCliModuleDepthTable, A0
        MOVE.W 0(A0,D0.L), D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace

        BSR.W opforgeNativeCliModuleNamePtr
        BSR.W opforgeNativeCliTokenLen
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace
        BSR.W opforgeNativeCliModuleNamePtr
        MOVE.L A0, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVEM.L (SP)+, D0-D4/A0-A1
        RTS

opforgeNativeCliEmitModuleCompatibility:
        MOVEM.L D0/D4/A0, -(SP)
        MOVE.W D0, D4
        MOVE.L #moduleFoundText, D1
        BSR.W opforge_native_cli_put_str
        BSR.W opforgeNativeCliModuleNamePtr
        MOVE.L A0, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVEM.L (SP)+, D0/D4/A0
        RTS

opforgeNativeCliCloseModule:
        MOVEM.L D1-D4/A0-A1, -(SP)
        TST.W nativeCliModuleDepth
        BEQ.S opforgeNativeCliCloseModuleFail
        MOVEQ #0, D0
        MOVE.W nativeCliModuleDepth, D0
        SUBQ.W #1, D0
        MOVE.W D0, nativeCliModuleDepth
        MOVEQ #0, D0
        MOVE.W nativeCliCurrentModuleId, D0
        BSR.W opforgeNativeCliEmitModuleEndRecord
        BSR.W opforgeNativeCliRestoreParentModule
        MOVEQ #0, D0
        BRA.S opforgeNativeCliCloseModuleReturn

opforgeNativeCliCloseModuleFail:
        MOVEQ #1, D0

opforgeNativeCliCloseModuleReturn:
        MOVEM.L (SP)+, D1-D4/A0-A1
        RTS

opforgeNativeCliEmitModuleEndRecord:
        MOVEM.L D0-D4/A0-A1, -(SP)
        MOVE.W D0, D4
        MOVE.L #modEndText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #0, D0
        MOVE.W D4, D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace
        MOVEQ #1, D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace
        MOVE.L nativeCliSourceLineNum, D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace
        MOVEQ #0, D0
        MOVE.W nativeCliModuleDepth, D0
        BSR.W opforge_native_cli_put_dec_u16
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVEM.L (SP)+, D0-D4/A0-A1
        RTS

opforgeNativeCliRestoreParentModule:
        MOVEM.L D1-D3/A0, -(SP)
        TST.W nativeCliModuleDepth
        BNE.S opforgeNativeCliRestoreParentModuleFind
        CLR.W nativeCliCurrentModuleId
        BRA.S opforgeNativeCliRestoreParentModuleReturn

opforgeNativeCliRestoreParentModuleFind:
        MOVE.W nativeCliModuleDepth, D0
        SUBQ.W #1, D0
        MOVE.W nativeCliModuleCount, D1
        BEQ.S opforgeNativeCliRestoreParentModuleClear
        SUBQ.W #1, D1

opforgeNativeCliRestoreParentModuleLoop:
        MOVEQ #0, D2
        MOVE.W D1, D2
        ADD.W D2, D2
        LEA nativeCliModuleDepthTable, A0
        MOVE.W 0(A0,D2.L), D3
        CMP.W D0, D3
        BEQ.S opforgeNativeCliRestoreParentModuleFound
        DBRA D1, opforgeNativeCliRestoreParentModuleLoop

opforgeNativeCliRestoreParentModuleClear:
        CLR.W nativeCliCurrentModuleId
        BRA.S opforgeNativeCliRestoreParentModuleReturn

opforgeNativeCliRestoreParentModuleFound:
        MOVE.W D1, nativeCliCurrentModuleId

opforgeNativeCliRestoreParentModuleReturn:
        MOVEM.L (SP)+, D1-D3/A0
        RTS

opforgeNativeCliPutSpace:
        MOVE.L #spaceText, D1
        BSR.W opforge_native_cli_put_str
        RTS

opforgeNativeCliModuleNamePtr:
        MOVEQ #0, D0
        MOVE.W D4, D0
        LSL.L #6, D0
        LEA nativeCliModuleNameTable, A0
        ADDA.L D0, A0
        RTS

opforgeNativeCliImportAliasPtr:
        MOVEQ #0, D0
        MOVE.W D4, D0
        LSL.L #6, D0
        LEA nativeCliImportAliasTable, A0
        ADDA.L D0, A0
        RTS

opforgeNativeCliImportSelectNamePtr:
        MOVEQ #0, D0
        MOVE.W D6, D0
        LSL.L #6, D0
        LEA nativeCliImportSelectNameTable, A0
        ADDA.L D0, A0
        RTS

opforgeNativeCliImportSelectAliasPtr:
        MOVEQ #0, D0
        MOVE.W D6, D0
        LSL.L #6, D0
        LEA nativeCliImportSelectAliasTable, A0
        ADDA.L D0, A0
        RTS

opforgeNativeCliTokenLen:
        MOVEM.L D1/A0, -(SP)
        MOVEQ #0, D0
        MOVE.L #TOKEN_BUFFER_CAPACITY - 1, D1

opforgeNativeCliTokenLenLoop:
        TST.B (A0)+
        BEQ.S opforgeNativeCliTokenLenDone
        ADDQ.W #1, D0
        DBRA D1, opforgeNativeCliTokenLenLoop

opforgeNativeCliTokenLenDone:
        MOVEM.L (SP)+, D1/A0
        RTS

opforgeNativeCliCopyIncludeTarget:
        TST.L D0
        BEQ.W opforgeNativeCliCopyIncludeTargetFail
        MOVEQ #0, D2
        MOVE.B (A0), D2
        CMPI.B #'"', D2
        BEQ.S opforgeNativeCliCopyIncludeTargetQuoted
        CMPI.B #39, D2
        BEQ.S opforgeNativeCliCopyIncludeTargetQuoted
        MOVE.L #PATH_BUFFER_CAPACITY - 1, D6
        CLR.L D5

opforgeNativeCliCopyIncludeTargetBareLoop:
        TST.L D0
        BEQ.S opforgeNativeCliCopyIncludeTargetDone
        MOVEQ #0, D2
        MOVE.B (A0), D2
        CMPI.B #' ', D2
        BEQ.S opforgeNativeCliCopyIncludeTargetDone
        CMPI.B #9, D2
        BEQ.S opforgeNativeCliCopyIncludeTargetDone
        CMPI.B #';', D2
        BEQ.S opforgeNativeCliCopyIncludeTargetDone
        TST.L D6
        BEQ.S opforgeNativeCliCopyIncludeTargetFail
        MOVE.B D2, (A1)+
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        ADDQ.L #1, D5
        SUBQ.L #1, D6
        BRA.S opforgeNativeCliCopyIncludeTargetBareLoop

opforgeNativeCliCopyIncludeTargetQuoted:
        MOVE.B D2, D4
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        MOVE.L #PATH_BUFFER_CAPACITY - 1, D6
        CLR.L D5

opforgeNativeCliCopyIncludeTargetQuotedLoop:
        TST.L D0
        BEQ.S opforgeNativeCliCopyIncludeTargetFail
        MOVEQ #0, D2
        MOVE.B (A0), D2
        CMP.B D4, D2
        BEQ.S opforgeNativeCliCopyIncludeTargetDone
        TST.L D6
        BEQ.S opforgeNativeCliCopyIncludeTargetFail
        MOVE.B D2, (A1)+
        ADDQ.L #1, A0
        SUBQ.L #1, D0
        ADDQ.L #1, D5
        SUBQ.L #1, D6
        BRA.S opforgeNativeCliCopyIncludeTargetQuotedLoop

opforgeNativeCliCopyIncludeTargetDone:
        TST.L D5
        BEQ.S opforgeNativeCliCopyIncludeTargetFail
        CLR.B (A1)
        MOVEQ #0, D0
        RTS

opforgeNativeCliCopyIncludeTargetFail:
        MOVEQ #1, D0
        RTS

opforge_native_cli_expand_include_target:
        TST.W nativeCliIncludeDepth
        BNE.W opforgeNativeCliExpandIncludeFail
        BSR.W opforgeNativeCliResolveIncludePath
        TST.L D0
        BNE.W opforgeNativeCliExpandIncludeFail

        MOVE.W nativeCliSourceLineLen, D0
        MOVE.W D0, nativeCliSavedLineLen
        MOVE.W nativeCliSawCr, D0
        MOVE.W D0, nativeCliSavedSawCr
        MOVE.L nativeCliSourceLineNum, D0
        MOVE.L D0, nativeCliSavedLineNum
        LEA nativeCliCurrentPath, A0
        LEA nativeCliSavedPath, A1
        BSR.W opforgeNativeCliCopyPathBuffer
        TST.L D0
        BNE.W opforgeNativeCliExpandIncludeFail

        MOVE.L #includeStageText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #includeRootText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliIncludeRootPath, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #includeFileText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliIncludePath, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #includeEnterText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliCurrentPath, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #spaceText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliIncludePath, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str

        MOVE.W #NATIVE_INCLUDE_DEPTH_LIMIT, nativeCliIncludeDepth
        LEA nativeCliIncludePath, A0
        LEA nativeCliCurrentPath, A1
        BSR.W opforgeNativeCliCopyPathBuffer
        TST.L D0
        BNE.S opforgeNativeCliExpandIncludeRestoreFail
        LEA nativeCliIncludePath, A0
        BSR.W opforge_native_cli_tokenize_file_at_path
        TST.L D0
        BNE.S opforgeNativeCliExpandIncludeRestoreFail

        MOVE.L #includeLeaveText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #includeOkText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #0, D0
        BRA.S opforgeNativeCliExpandIncludeRestore

opforgeNativeCliExpandIncludeRestoreFail:
        MOVE.L #includeFailureText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #1, D0

opforgeNativeCliExpandIncludeRestore:
        MOVE.W nativeCliSavedLineLen, D1
        MOVE.W D1, nativeCliSourceLineLen
        MOVE.W nativeCliSavedSawCr, D1
        MOVE.W D1, nativeCliSawCr
        MOVE.L nativeCliSavedLineNum, D1
        MOVE.L D1, nativeCliSourceLineNum
        LEA nativeCliSavedPath, A0
        LEA nativeCliCurrentPath, A1
        BSR.W opforgeNativeCliCopyPathBuffer
        CLR.W nativeCliIncludeDepth
        RTS

opforgeNativeCliExpandIncludeFail:
        MOVE.L #includeFailureText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #1, D0
        RTS

opforgeNativeCliResolveIncludePath:
        LEA nativeCliCurrentPath, A0
        LEA nativeCliIncludeRootPath, A1
        BSR.W opforgeNativeCliCopyPathRoot
        TST.L D0
        BNE.W opforgeNativeCliResolveIncludeFail
        LEA nativeCliIncludeTarget, A0
        BSR.W opforgeNativeCliPathIsAbsolute
        TST.L D0
        BEQ.S opforgeNativeCliResolveIncludeRelative
        LEA nativeCliIncludeTarget, A0
        LEA nativeCliIncludePath, A1
        BSR.W opforgeNativeCliCopyPathBuffer
        RTS

opforgeNativeCliResolveIncludeRelative:
        LEA nativeCliIncludeRootPath, A0
        LEA nativeCliIncludePath, A1
        BSR.W opforgeNativeCliCopyPathBuffer
        TST.L D0
        BNE.S opforgeNativeCliResolveIncludeFail
        LEA nativeCliIncludeTarget, A0
        LEA nativeCliIncludePath, A1
        BSR.W opforgeNativeCliAppendPathBuffer
        RTS

opforgeNativeCliResolveIncludeFail:
        MOVEQ #1, D0
        RTS

opforgeNativeCliPathIsAbsolute:
        MOVEQ #0, D0

opforgeNativeCliPathIsAbsoluteLoop:
        MOVE.B (A0)+, D1
        BEQ.S opforgeNativeCliPathIsAbsoluteNo
        CMPI.B #':', D1
        BEQ.S opforgeNativeCliPathIsAbsoluteYes
        BRA.S opforgeNativeCliPathIsAbsoluteLoop

opforgeNativeCliPathIsAbsoluteYes:
        MOVEQ #1, D0
        RTS

opforgeNativeCliPathIsAbsoluteNo:
        MOVEQ #0, D0
        RTS

opforgeNativeCliCopyPathRoot:
        MOVEM.L D2-D6/A2, -(SP)
        MOVEA.L A0, A2
        CLR.L D5
        CLR.L D6

opforgeNativeCliCopyPathRootScan:
        MOVE.B (A2)+, D2
        BEQ.S opforgeNativeCliCopyPathRootCopy
        ADDQ.L #1, D5
        CMPI.B #':', D2
        BEQ.S opforgeNativeCliCopyPathRootMark
        CMPI.B #'/', D2
        BNE.S opforgeNativeCliCopyPathRootScan

opforgeNativeCliCopyPathRootMark:
        MOVE.L D5, D6
        BRA.S opforgeNativeCliCopyPathRootScan

opforgeNativeCliCopyPathRootCopy:
        MOVEA.L A0, A2
        MOVE.L #PATH_BUFFER_CAPACITY - 1, D4
        TST.L D6
        BEQ.S opforgeNativeCliCopyPathRootDone

opforgeNativeCliCopyPathRootCopyLoop:
        TST.L D4
        BEQ.S opforgeNativeCliCopyPathRootFail
        MOVE.B (A2)+, D3
        MOVE.B D3, (A1)+
        SUBQ.L #1, D6
        SUBQ.L #1, D4
        TST.L D6
        BNE.S opforgeNativeCliCopyPathRootCopyLoop

opforgeNativeCliCopyPathRootDone:
        CLR.B (A1)
        MOVEQ #0, D0
        BRA.S opforgeNativeCliCopyPathRootReturn

opforgeNativeCliCopyPathRootFail:
        MOVEQ #1, D0

opforgeNativeCliCopyPathRootReturn:
        MOVEM.L (SP)+, D2-D6/A2
        RTS

opforgeNativeCliCopyPathBuffer:
        MOVE.L #PATH_BUFFER_CAPACITY - 1, D6

opforgeNativeCliCopyPathBufferLoop:
        MOVE.B (A0)+, D2
        MOVE.B D2, (A1)+
        BEQ.S opforgeNativeCliCopyPathBufferOk
        SUBQ.L #1, D6
        BNE.S opforgeNativeCliCopyPathBufferLoop
        CLR.B -(A1)
        MOVEQ #1, D0
        RTS

opforgeNativeCliCopyPathBufferOk:
        MOVEQ #0, D0
        RTS

opforgeNativeCliAppendPathBuffer:
        MOVE.L #PATH_BUFFER_CAPACITY - 1, D6

opforgeNativeCliAppendPathBufferFindEnd:
        TST.B (A1)
        BEQ.S opforgeNativeCliAppendPathBufferCopy
        ADDQ.L #1, A1
        SUBQ.L #1, D6
        BEQ.S opforgeNativeCliAppendPathBufferFail
        BRA.S opforgeNativeCliAppendPathBufferFindEnd

opforgeNativeCliAppendPathBufferCopy:
        MOVE.B (A0)+, D2
        MOVE.B D2, (A1)+
        BEQ.S opforgeNativeCliAppendPathBufferOk
        SUBQ.L #1, D6
        BNE.S opforgeNativeCliAppendPathBufferCopy

opforgeNativeCliAppendPathBufferFail:
        CLR.B -(A1)
        MOVEQ #1, D0
        RTS

opforgeNativeCliAppendPathBufferOk:
        MOVEQ #0, D0
        RTS

opforge_native_cli_put_dec_u16:
        MOVEM.L D1-D6/A0-A1, -(SP)
        ANDI.L #$0000FFFF, D0
        LEA decimalPowers, A0
        MOVEQ #4, D6
        CLR.W D5

opforgeNativeCliPutDecPowerLoop:
        MOVEQ #0, D3
        MOVE.W (A0)+, D2

opforgeNativeCliPutDecDigitLoop:
        CMP.W D2, D0
        BCS.S opforgeNativeCliPutDecMaybeEmit
        SUB.W D2, D0
        ADDQ.W #1, D3
        BRA.S opforgeNativeCliPutDecDigitLoop

opforgeNativeCliPutDecMaybeEmit:
        TST.W D3
        BNE.S opforgeNativeCliPutDecEmit
        TST.W D5
        BNE.S opforgeNativeCliPutDecEmit
        CMPI.W #1, D2
        BNE.S opforgeNativeCliPutDecNext

opforgeNativeCliPutDecEmit:
        MOVE.W #1, D5
        ADDI.B #'0', D3
        LEA nativeCliDecimalChar, A1
        MOVE.B D3, (A1)
        CLR.B 1(A1)
        MOVE.L #nativeCliDecimalChar, D1
        BSR.W opforge_native_cli_put_str

opforgeNativeCliPutDecNext:
        DBRA D6, opforgeNativeCliPutDecPowerLoop
        MOVEM.L (SP)+, D1-D6/A0-A1
        RTS

opforge_native_cli_init_module_use_state:
        MOVEM.L D0-D1/A0, -(SP)
        LEA nativeCliModuleUseStateStart, A0
        MOVE.L #NATIVE_MODULE_USE_STATE_BYTES, D0
        BSR.W opforge_native_cli_clear_bytes
        MOVEM.L (SP)+, D0-D1/A0
        RTS

opforge_native_cli_clear_bytes:
        TST.L D0
        BEQ.S opforgeNativeCliClearBytesDone
        MOVEQ #0, D1

opforgeNativeCliClearBytesLoop:
        MOVE.B D1, (A0)+
        SUBQ.L #1, D0
        BNE.S opforgeNativeCliClearBytesLoop

opforgeNativeCliClearBytesDone:
        RTS

NCLI_PARSE_OK                   = 0
NCLI_PARSE_HELP                 = 1
NCLI_PARSE_VERSION              = 2
NCLI_PARSE_USAGE                = -1
NCLI_PARSE_QUOTED               = -2
NCLI_PARSE_UNSUPPORTED          = -3
NCLI_PARSE_UNKNOWN_FLAG         = -4
NCLI_PARSE_MISSING_VALUE        = -5
NCLI_PARSE_NO_INPUT             = -6
NCLI_PARSE_HUNK_REQUIRED        = -7
NCLI_PARSE_MIXED_INPUT          = -8
NCLI_PARSE_MULTIPLE_POSITIONAL  = -9
NCLI_PARSE_MODULE_PATH_CAPACITY = -10

opforge_native_cli_parse_args:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVEA.L A0, A3
        CLR.W nativeCliInputStyle
        CLR.W nativeCliHunkRequested
        CLR.W nativeCliParseStatus
        CLR.B nativeCliInputPath
        CLR.B nativeCliHunkPath
        CLR.B nativeCliOutfileBase
        CLR.B nativeCliCpuName
        CLR.B nativeCliPackagePath
        MOVE.W #1, nativeCliModulePathCount

opforgeNativeCliParseLoop:
        BSR.W opforgeNativeCliSkipWhitespace
        TST.B (A3)
        BEQ.W opforgeNativeCliParseDone
        CMPI.B #'"', (A3)
        BEQ.W opforgeNativeCliQuoted
        LEA nativeCliArgToken, A1
        BSR.W opforgeNativeCliCopyToken
        TST.L D0
        BNE.W opforgeNativeCliUsage

        LEA nativeCliArgToken, A0
        LEA flagHelpLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliParseHelp
        LEA nativeCliArgToken, A0
        LEA flagHelpShort, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliParseHelp
        LEA nativeCliArgToken, A0
        LEA flagVersionLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliParseVersion
        LEA nativeCliArgToken, A0
        LEA flagVersionShort, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliParseVersion
        LEA nativeCliArgToken, A0
        LEA flagInfileShort, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliInfile
        LEA nativeCliArgToken, A0
        LEA flagInfileLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliInfile
        LEA nativeCliArgToken, A0
        LEA flagHunkLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliHunk
        LEA nativeCliArgToken, A0
        LEA flagOutfileShort, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliOutfile
        LEA nativeCliArgToken, A0
        LEA flagOutfileLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliOutfile
        LEA nativeCliArgToken, A0
        LEA flagCpuLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliCpu
        LEA nativeCliArgToken, A0
        LEA flagPackageLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliPackage
        LEA nativeCliArgToken, A0
        LEA flagModuleShort, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliModulePath
        LEA nativeCliArgToken, A0
        LEA flagModuleLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliModulePath
        BSR.W opforgeNativeCliIsUnsupportedFlag
        TST.L D0
        BNE.W opforgeNativeCliUnsupported
        LEA nativeCliArgToken, A0
        CMPI.B #'-', (A0)
        BEQ.W opforgeNativeCliUnknownFlag
        BRA.W opforgeNativeCliPositionalInput

opforgeNativeCliInfile:
        TST.W nativeCliInputStyle
        BEQ.S opforgeNativeCliInfileFirst
        CMPI.W #1, nativeCliInputStyle
        BEQ.W opforgeNativeCliMixedInput
        BRA.W opforgeNativeCliUsage

opforgeNativeCliInfileFirst:
        MOVE.W #2, nativeCliInputStyle
        LEA nativeCliInputPath, A1
        BSR.W opforgeNativeCliCopyRequiredValue
        TST.L D0
        BNE.W opforgeNativeCliMissingValue
        BRA.W opforgeNativeCliParseLoop

opforgeNativeCliHunk:
        MOVE.W #1, nativeCliHunkRequested
        LEA nativeCliHunkPath, A1
        BSR.W opforgeNativeCliCopyOptionalValue
        TST.L D0
        BMI.W opforgeNativeCliQuoted
        BRA.W opforgeNativeCliParseLoop

opforgeNativeCliOutfile:
        LEA nativeCliOutfileBase, A1
        BSR.W opforgeNativeCliCopyRequiredValue
        TST.L D0
        BNE.W opforgeNativeCliMissingValue
        BRA.W opforgeNativeCliParseLoop

opforgeNativeCliCpu:
        LEA nativeCliCpuName, A1
        BSR.W opforgeNativeCliCopyRequiredValue
        TST.L D0
        BNE.W opforgeNativeCliMissingValue
        BRA.W opforgeNativeCliParseLoop

opforgeNativeCliPackage:
        LEA nativeCliPackagePath, A1
        BSR.W opforgeNativeCliCopyRequiredValue
        TST.L D0
        BNE.W opforgeNativeCliMissingValue
        BRA.W opforgeNativeCliParseLoop

opforgeNativeCliModulePath:
        LEA nativeCliIncludeTarget, A1
        BSR.W opforgeNativeCliCopyRequiredPathValue
        CMPI.L #1, D0
        BEQ.W opforgeNativeCliMissingValue
        TST.L D0
        BNE.W opforgeNativeCliModulePathCapacity
        BSR.W opforgeNativeCliRecordModulePathValue
        TST.L D0
        BNE.W opforgeNativeCliModulePathCapacity
        BRA.W opforgeNativeCliParseLoop

opforgeNativeCliPositionalInput:
        TST.W nativeCliInputStyle
        BEQ.S opforgeNativeCliPositionalInputFirst
        CMPI.W #2, nativeCliInputStyle
        BEQ.W opforgeNativeCliMixedInput
        BRA.W opforgeNativeCliMultiplePositional

opforgeNativeCliPositionalInputFirst:
        MOVE.W #1, nativeCliInputStyle
        LEA nativeCliArgToken, A0
        LEA nativeCliInputPath, A1
        BSR.W opforgeNativeCliCopyTokenBuffer
        BRA.W opforgeNativeCliParseLoop

opforgeNativeCliParseDone:
        TST.W nativeCliInputStyle
        BEQ.W opforgeNativeCliNoInput
        TST.W nativeCliHunkRequested
        BEQ.W opforgeNativeCliHunkRequired
        TST.B nativeCliHunkPath
        BNE.S opforgeNativeCliParseOk
        TST.B nativeCliOutfileBase
        BEQ.S opforgeNativeCliParseOk
        LEA nativeCliOutfileBase, A0
        LEA nativeCliHunkPath, A1
        BSR.W opforgeNativeCliCopyTokenBuffer

opforgeNativeCliParseOk:
        BSR.W opforgeNativeCliRecordImplicitModulePathRoot
        TST.L D0
        BNE.W opforgeNativeCliModulePathCapacity
        MOVE.W #NCLI_PARSE_OK, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliParseHelp:
        MOVE.W #NCLI_PARSE_HELP, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliParseVersion:
        MOVE.W #NCLI_PARSE_VERSION, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliUsage:
        MOVE.W #NCLI_PARSE_USAGE, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliQuoted:
        MOVE.W #NCLI_PARSE_QUOTED, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliUnsupported:
        MOVE.W #NCLI_PARSE_UNSUPPORTED, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliUnknownFlag:
        MOVE.W #NCLI_PARSE_UNKNOWN_FLAG, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliMissingValue:
        MOVE.W #NCLI_PARSE_MISSING_VALUE, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliNoInput:
        MOVE.W #NCLI_PARSE_NO_INPUT, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliHunkRequired:
        MOVE.W #NCLI_PARSE_HUNK_REQUIRED, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliMixedInput:
        MOVE.W #NCLI_PARSE_MIXED_INPUT, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliMultiplePositional:
        MOVE.W #NCLI_PARSE_MULTIPLE_POSITIONAL, nativeCliParseStatus
        BRA.W opforgeNativeCliParseReturn

opforgeNativeCliModulePathCapacity:
        MOVE.W #NCLI_PARSE_MODULE_PATH_CAPACITY, nativeCliParseStatus

opforgeNativeCliParseReturn:
        MOVE.W nativeCliParseStatus, D0
        EXT.L D0
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

opforgeNativeCliSkipWhitespace:
        CMPI.B #' ', (A3)
        BEQ.S opforgeNativeCliSkipOne
        CMPI.B #9, (A3)
        BEQ.S opforgeNativeCliSkipOne
        CMPI.B #10, (A3)
        BEQ.S opforgeNativeCliSkipOne
        CMPI.B #13, (A3)
        BNE.S opforgeNativeCliSkipDone

opforgeNativeCliSkipOne:
        ADDQ.L #1, A3
        BRA.S opforgeNativeCliSkipWhitespace

opforgeNativeCliSkipDone:
        RTS

opforgeNativeCliCopyToken:
        MOVE.L #TOKEN_BUFFER_CAPACITY - 1, D6

opforgeNativeCliCopyTokenLoop:
        MOVEQ #0, D0
        MOVE.B (A3), D0
        BEQ.S opforgeNativeCliCopyTokenDone
        CMPI.B #' ', D0
        BEQ.S opforgeNativeCliCopyTokenDone
        CMPI.B #9, D0
        BEQ.S opforgeNativeCliCopyTokenDone
        CMPI.B #10, D0
        BEQ.S opforgeNativeCliCopyTokenDone
        CMPI.B #13, D0
        BEQ.S opforgeNativeCliCopyTokenDone
        CMPI.B #'"', D0
        BEQ.S opforgeNativeCliCopyTokenFail
        TST.L D6
        BEQ.S opforgeNativeCliCopyTokenFail
        MOVE.B D0, (A1)+
        ADDQ.L #1, A3
        SUBQ.L #1, D6
        BRA.S opforgeNativeCliCopyTokenLoop

opforgeNativeCliCopyTokenDone:
        CLR.B (A1)
        MOVEQ #0, D0
        RTS

opforgeNativeCliCopyTokenFail:
        MOVEQ #1, D0
        RTS

opforgeNativeCliCopyRequiredValue:
        BSR.W opforgeNativeCliSkipWhitespace
        TST.B (A3)
        BEQ.S opforgeNativeCliRequiredMissing
        CMPI.B #'"', (A3)
        BEQ.S opforgeNativeCliRequiredMissing
        BSR.W opforgeNativeCliCopyToken
        RTS

opforgeNativeCliRequiredMissing:
        MOVEQ #1, D0
        RTS

opforgeNativeCliCopyRequiredPathValue:
        BSR.W opforgeNativeCliSkipWhitespace
        TST.B (A3)
        BEQ.S opforgeNativeCliRequiredPathMissing
        CMPI.B #'"', (A3)
        BEQ.S opforgeNativeCliRequiredPathMissing
        MOVE.L #PATH_BUFFER_CAPACITY - 1, D6

opforgeNativeCliCopyRequiredPathLoop:
        MOVEQ #0, D0
        MOVE.B (A3), D0
        BEQ.S opforgeNativeCliCopyRequiredPathDone
        CMPI.B #' ', D0
        BEQ.S opforgeNativeCliCopyRequiredPathDone
        CMPI.B #9, D0
        BEQ.S opforgeNativeCliCopyRequiredPathDone
        CMPI.B #10, D0
        BEQ.S opforgeNativeCliCopyRequiredPathDone
        CMPI.B #13, D0
        BEQ.S opforgeNativeCliCopyRequiredPathDone
        CMPI.B #'"', D0
        BEQ.S opforgeNativeCliCopyRequiredPathCapacity
        TST.L D6
        BEQ.S opforgeNativeCliCopyRequiredPathCapacity
        MOVE.B D0, (A1)+
        ADDQ.L #1, A3
        SUBQ.L #1, D6
        BRA.S opforgeNativeCliCopyRequiredPathLoop

opforgeNativeCliCopyRequiredPathDone:
        CLR.B (A1)
        MOVEQ #0, D0
        RTS

opforgeNativeCliRequiredPathMissing:
        MOVEQ #1, D0
        RTS

opforgeNativeCliCopyRequiredPathCapacity:
        MOVEQ #2, D0
        RTS

opforgeNativeCliCopyOptionalValue:
        BSR.W opforgeNativeCliSkipWhitespace
        TST.B (A3)
        BEQ.S opforgeNativeCliOptionalNone
        CMPI.B #'"', (A3)
        BEQ.S opforgeNativeCliOptionalQuoted
        CMPI.B #'-', (A3)
        BEQ.S opforgeNativeCliOptionalNone
        BSR.W opforgeNativeCliCopyToken
        RTS

opforgeNativeCliOptionalNone:
        CLR.B (A1)
        MOVEQ #0, D0
        RTS

opforgeNativeCliOptionalQuoted:
        MOVEQ #-1, D0
        RTS

opforgeNativeCliCopyTokenBuffer:
        MOVEQ #0, D0
        MOVE.B (A0)+, D0
        MOVE.B D0, (A1)+
        BNE.S opforgeNativeCliCopyTokenBuffer
        RTS

opforgeNativeCliTokenEquals:
        MOVEQ #0, D2

opforgeNativeCliTokenEqualsLoop:
        MOVE.B (A0)+, D0
        MOVE.B (A1)+, D1
        CMP.B D1, D0
        BNE.S opforgeNativeCliTokenNotEqual
        TST.B D0
        BNE.S opforgeNativeCliTokenEqualsLoop
        MOVEQ #1, D0
        RTS

opforgeNativeCliTokenNotEqual:
        MOVEQ #0, D0
        RTS

opforgeNativeCliIsUnsupportedFlag:
        LEA nativeCliArgToken, A0
        LEA flagListShort, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        LEA nativeCliArgToken, A0
        LEA flagListLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        LEA nativeCliArgToken, A0
        LEA flagHexShort, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        LEA nativeCliArgToken, A0
        LEA flagHexLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        LEA nativeCliArgToken, A0
        LEA flagSrecShort, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        LEA nativeCliArgToken, A0
        LEA flagSrecLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        LEA nativeCliArgToken, A0
        LEA flagBinShort, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        LEA nativeCliArgToken, A0
        LEA flagBinLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        LEA nativeCliArgToken, A0
        LEA flagDefineShort, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        LEA nativeCliArgToken, A0
        LEA flagDefineLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        LEA nativeCliArgToken, A0
        LEA flagIncludeShort, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        LEA nativeCliArgToken, A0
        LEA flagIncludeLong, A1
        BSR.W opforgeNativeCliTokenEquals
        TST.L D0
        BNE.W opforgeNativeCliUnsupportedYes
        MOVEQ #0, D0
        RTS

opforgeNativeCliUnsupportedYes:
        MOVEQ #1, D0
        RTS

opforge_native_cli_report_parse_error:
        MOVE.W nativeCliParseStatus, D0
        CMPI.W #NCLI_PARSE_QUOTED, D0
        BEQ.S opforgeNativeCliReportQuoted
        CMPI.W #NCLI_PARSE_UNSUPPORTED, D0
        BEQ.S opforgeNativeCliReportUnsupported
        CMPI.W #NCLI_PARSE_UNKNOWN_FLAG, D0
        BEQ.S opforgeNativeCliReportUnknown
        CMPI.W #NCLI_PARSE_MISSING_VALUE, D0
        BEQ.S opforgeNativeCliReportMissing
        CMPI.W #NCLI_PARSE_NO_INPUT, D0
        BEQ.W opforgeNativeCliReportNoInput
        CMPI.W #NCLI_PARSE_HUNK_REQUIRED, D0
        BEQ.W opforgeNativeCliReportHunkRequired
        CMPI.W #NCLI_PARSE_MIXED_INPUT, D0
        BEQ.W opforgeNativeCliReportMixedInput
        CMPI.W #NCLI_PARSE_MULTIPLE_POSITIONAL, D0
        BEQ.W opforgeNativeCliReportMultiplePositional
        CMPI.W #NCLI_PARSE_MODULE_PATH_CAPACITY, D0
        BEQ.W opforgeNativeCliReportModulePathCapacity
        MOVE.L #usageText, D1
        BRA.W opforgeNativeCliReportText

opforgeNativeCliReportQuoted:
        MOVE.L #quotedText, D1
        BRA.W opforgeNativeCliReportText

opforgeNativeCliReportUnsupported:
        MOVE.L #unsupportedText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliArgToken, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeSubsetHelpText, D1
        BRA.S opforgeNativeCliReportText

opforgeNativeCliReportUnknown:
        MOVE.L #unknownFlagText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliArgToken, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BRA.S opforgeNativeCliReportText

opforgeNativeCliReportMissing:
        MOVE.L #missingValueText, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #nativeCliArgToken, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BRA.S opforgeNativeCliReportText

opforgeNativeCliReportNoInput:
        MOVE.L #noInputText, D1
        BRA.S opforgeNativeCliReportText

opforgeNativeCliReportHunkRequired:
        MOVE.L #hunkRequiredText, D1
        BRA.S opforgeNativeCliReportText

opforgeNativeCliReportMixedInput:
        MOVE.L #mixedInputText, D1
        BRA.S opforgeNativeCliReportText

opforgeNativeCliReportMultiplePositional:
        MOVE.L #multiplePositionalText, D1
        BRA.S opforgeNativeCliReportText

opforgeNativeCliReportModulePathCapacity:
        MOVE.L #modulePathCapacityText, D1

opforgeNativeCliReportText:
        BSR.W opforge_native_cli_put_str
        RTS

opforgeNativeCliRecordImplicitModulePathRoot:
        LEA nativeCliInputPath, A0
        LEA nativeCliModulePathTable, A1
        BSR.W opforgeNativeCliCopyPathRoot
        RTS

opforgeNativeCliRecordModulePathValue:
        MOVEM.L D1/A0-A1, -(SP)
        MOVEQ #0, D0
        MOVE.W nativeCliModulePathCount, D0
        CMPI.W #NATIVE_MODULE_PATH_CAPACITY, D0
        BHS.S opforgeNativeCliRecordModulePathFail
        MOVE.L D0, D1
        LSL.L #8, D1
        LEA nativeCliModulePathTable, A1
        ADDA.L D1, A1
        LEA nativeCliIncludeTarget, A0
        BSR.W opforgeNativeCliCopyPathBuffer
        TST.L D0
        BNE.S opforgeNativeCliRecordModulePathFail
        MOVE.W nativeCliModulePathCount, D0
        ADDQ.W #1, D0
        MOVE.W D0, nativeCliModulePathCount
        MOVEQ #0, D0
        BRA.S opforgeNativeCliRecordModulePathReturn

opforgeNativeCliRecordModulePathFail:
        MOVEQ #1, D0

opforgeNativeCliRecordModulePathReturn:
        MOVEM.L (SP)+, D1/A0-A1
        RTS

opforgeNativeCliEmitModulePathRecords:
        MOVEM.L D0-D4/A0, -(SP)
        CLR.W D4

opforgeNativeCliEmitModulePathLoop:
        MOVE.W nativeCliModulePathCount, D0
        CMP.W D0, D4
        BHS.S opforgeNativeCliEmitModulePathDone
        MOVE.L #modPathText, D1
        BSR.W opforge_native_cli_put_str
        MOVEQ #0, D0
        MOVE.W D4, D0
        BSR.W opforge_native_cli_put_dec_u16
        BSR.W opforgeNativeCliPutSpace
        MOVEQ #0, D0
        MOVE.W D4, D0
        LSL.L #8, D0
        LEA nativeCliModulePathTable, A0
        ADDA.L D0, A0
        MOVE.L A0, D1
        BSR.W opforge_native_cli_put_str
        MOVE.L #newlineText, D1
        BSR.W opforge_native_cli_put_str
        ADDQ.W #1, D4
        BRA.S opforgeNativeCliEmitModulePathLoop

opforgeNativeCliEmitModulePathDone:
        MOVEM.L (SP)+, D0-D4/A0
        RTS

        .endsection

        .section data, kind=data

dosName:
        .byte "dos.library",0
newlineText:
        .byte 10,0
versionText:
        .byte "opForge native AmigaOS CLI 0.1",10,0
helpText:
        .byte "Usage: opForge [OPTIONS] [INPUT]",10
        .byte "Native subset: INPUT, -i/--infile, --hunk [FILE], -o/--outfile, --cpu, --opasm-package, -M/--module-path",10,0
usageText:
        .byte "OPC-NCLI001: Usage: opForge [OPTIONS] [INPUT]",10,0
quotedText:
        .byte "OPC-NCLI002: quoted arguments are not supported by the native CLI subset",10,0
unsupportedText:
        .byte "OPC-NCLI003: recognized Rust CLI option is not implemented by native AmigaOS CLI yet: ",0
nativeSubsetHelpText:
        .byte 10,"Native subset supports INPUT, -i/--infile, --hunk [FILE], -o/--outfile, --cpu, --opasm-package, and -M/--module-path.",10,0
unknownFlagText:
        .byte "OPC-NCLI004: unknown CLI flag: ",0
missingValueText:
        .byte "OPC-NCLI005: option requires a value: ",0
noInputText:
        .byte "OPC-NCLI006: No input files specified. Use -i/--infile",10,0
hunkRequiredText:
        .byte "OPC-NCLI007: No outputs selected. Native AmigaOS CLI currently supports --hunk only",10,0
mixedInputText:
        .byte "OPC-NCLI011: Do not mix positional input with -i/--infile; use one style",10,0
multiplePositionalText:
        .byte "OPC-NCLI012: Multiple positional inputs are not supported; use repeatable -i/--infile",10,0
modulePathCapacityText:
        .byte "OPC-NCLI017: native module path capacity exceeded",10,0
inputOpenErrorText:
        .byte "OPC-NCLI008: Input source file not found: ",0
stubHeaderText:
        .byte "OPFORGE-NATIVE 1",10
        .byte "STATUS emitter-not-implemented",10,0
inputLabelText:
        .byte "INPUT ",0
hunkLabelText:
        .byte "HUNK ",0
tokenizerOkText:
        .byte "STATUS tokenizer-ok",10,0
tokenizerFailureText:
        .byte "ERROR OPC-NCLI010: native tokenizer stage failed",10,0
parserOkText:
        .byte "STAGE parser",10
        .byte "STATUS parser-module-use-ok",10,0
emitterStubText:
        .byte "STAGE emitter",10
        .byte "ERROR OPC-NCLI009: native emitter VM not implemented",10,0
parserFailureText:
        .byte "ERROR OPC-NCLI013: native module/use parser stage failed",10,0
moduleDepthFailureText:
        .byte "ERROR OPC-NCLI016: native module depth mismatch",10,0
includeStageText:
        .byte "STAGE include",10,0
includeOkText:
        .byte "STATUS include-ok",10,0
includeFailureText:
        .byte "ERROR OPC-NCLI014: native include expansion failed",10,0
conditionalFailureText:
        .byte "ERROR OPC-NCLI015: native conditional preprocessing not implemented",10,0
moduleResolveFailureText:
        .byte "ERROR OPC-NCLI018: native module resolution failed: ",0
includeRootText:
        .byte "INCLUDE-ROOT 1 ",0
includeFileText:
        .byte "INCLUDE-FILE 1 ",0
includeEnterText:
        .byte "INCLUDE-ENTER 1 ",0
includeLineText:
        .byte "INCLUDE-LINE ",0
includeLeaveText:
        .byte "INCLUDE-LEAVE 1",10,0
modRootText:
        .byte "MOD-ROOT ",0
modDefText:
        .byte "MOD-DEF ",0
modEndText:
        .byte "MOD-END ",0
modPathText:
        .byte "MOD-PATH ",0
useImportText:
        .byte "USE-IMPORT ",0
useSelectText:
        .byte "USE-SELECT ",0
useWildcardText:
        .byte "USE-WILDCARD ",0
moduleFoundText:
        .byte "MODULE ",0
spaceText:
        .byte " ",0
asKeywordText:
        .byte "as"
moduleSourceExtensionText:
        .byte ".asm",0
processorAsmText:
        .byte "asm"
kindStatementText:
        .byte "statement"
opforgeNativeCliPrvmParserProgram:
        .byte $60,$10,$07,$03,$0B,$00,$20,$30,$65,$64,$00,$64,$00
moduleMnemonicText:
        .byte "module"
endmoduleMnemonicText:
        .byte "endmodule"
useMnemonicText:
        .byte "use"
moduleDirectiveText:
        .byte ".module"
endmoduleDirectiveText:
        .byte ".endmodule"
useDirectiveText:
        .byte ".use"
includeDirectiveText:
        .byte ".include"
ifDirectiveText:
        .byte ".if"
ifdefDirectiveText:
        .byte ".ifdef"
ifndefDirectiveText:
        .byte ".ifndef"
elseDirectiveText:
        .byte ".else"
elseifDirectiveText:
        .byte ".elseif"
endifDirectiveText:
        .byte ".endif"
.ifdef OPFORGE_FS_UAE_SMOKE
defaultFsUaeArgTail:
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_INPUT
        .byte "Work:opforge_missing_input.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_cli_package.opasm",0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_HUNK
        .byte "Work:opforge_fsuae_smoke_input.asm --cpu m68020 --opasm-package Work:opforge_cli_package.opasm",0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MIXED_INPUT
        .byte "Work:opforge_fsuae_smoke_input.asm --infile Work:opforge_fsuae_smoke_input.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_cli_package.opasm",0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_BAD_PACKAGE
        .byte "Work:opforge_fsuae_smoke_input.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_missing_package.opasm",0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_UNMATCHED_ENDMODULE
        .byte "Work:opforge_fsuae_unmatched_endmodule.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_cli_package.opasm",0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_UNTERMINATED_MODULE
        .byte "Work:opforge_fsuae_unterminated_module.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_cli_package.opasm",0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_BAD_USE
        .byte "Work:opforge_fsuae_bad_use.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_cli_package.opasm",0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE
        .byte "Work:opforge_fsuae_missing_module.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_cli_package.opasm",0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE_PATH
        .byte "Work:opforge_fsuae_smoke_input.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_cli_package.opasm -M",0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MODULE_PATH_OVERFLOW
        .byte "Work:opforge_fsuae_smoke_input.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_cli_package.opasm -M Work:mod1 -M Work:mod2 -M Work:mod3 -M Work:mod4 -M Work:mod5 -M Work:mod6 -M Work:mod7 -M Work:mod8",0
.else
        .byte "Work:opforge_fsuae_smoke_input.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_cli_package.opasm -M Work:opforge_module_a --module-path Work:opforge_module_b",0
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif

flagHelpLong:
        .byte "--help",0
flagHelpShort:
        .byte "-h",0
flagVersionLong:
        .byte "--version",0
flagVersionShort:
        .byte "-V",0
flagInfileShort:
        .byte "-i",0
flagInfileLong:
        .byte "--infile",0
flagHunkLong:
        .byte "--hunk",0
flagOutfileShort:
        .byte "-o",0
flagOutfileLong:
        .byte "--outfile",0
flagCpuLong:
        .byte "--cpu",0
flagPackageLong:
        .byte "--opasm-package",0
flagListShort:
        .byte "-l",0
flagListLong:
        .byte "--list",0
flagHexShort:
        .byte "-x",0
flagHexLong:
        .byte "--hex",0
flagSrecShort:
        .byte "-s",0
flagSrecLong:
        .byte "--srec",0
flagBinShort:
        .byte "-b",0
flagBinLong:
        .byte "--bin",0
flagDefineShort:
        .byte "-D",0
flagDefineLong:
        .byte "--define",0
flagIncludeShort:
        .byte "-I",0
flagIncludeLong:
        .byte "--include-path",0
flagModuleShort:
        .byte "-M",0
flagModuleLong:
        .byte "--module-path",0

        .align 2

decimalPowers:
        .word 10000,1000,100,10,1

        .align 2

opforgeNativeCliPackageLen:
        .word OPFORGE_NATIVE_CLI_PACKAGE_LEN

defaultCpuName:
        .byte "m68020",0
defaultFamilyName:
        .byte "motorola68k"
defaultFamilyNameEnd:

        .align 2
opforgeNativeCliPackageData:
        .incbin "opforge_cli_package.opasm"
opforgeNativeCliPackageDataEnd:

DEFAULT_FAMILY_NAME_LEN = defaultFamilyNameEnd - defaultFamilyName
OPFORGE_NATIVE_CLI_PACKAGE_LEN = opforgeNativeCliPackageDataEnd - opforgeNativeCliPackageData

        .endsection

        .section bss, kind=bss
        .align 4

nativeCliDosBase:
        .res long,1
nativeCliReturnCode:
        .res long,1
nativeCliInputStyle:
        .res word,1
nativeCliHunkRequested:
        .res word,1
nativeCliParseStatus:
        .res word,1

nativeCliArgToken:
        .res byte,TOKEN_BUFFER_CAPACITY
nativeCliInputPath:
        .res byte,PATH_BUFFER_CAPACITY
nativeCliHunkPath:
        .res byte,PATH_BUFFER_CAPACITY
nativeCliOutfileBase:
        .res byte,PATH_BUFFER_CAPACITY
nativeCliCpuName:
        .res byte,TOKEN_BUFFER_CAPACITY
nativeCliPackagePath:
        .res byte,PATH_BUFFER_CAPACITY
nativeCliSourceLineLen:
        .res word,1
nativeCliParserTailLen:
        .res word,1
nativeCliPackageLenActive:
        .res word,1
nativeCliPipelineRequestLen:
        .res word,1
nativeCliSourceLineNum:
        .res long,1
nativeCliSawCr:
        .res word,1
nativeCliIncludeDepth:
        .res word,1
nativeCliModuleResolveDepth:
        .res word,1
nativeCliResolvedModuleId:
        .res word,1
nativeCliSavedLineLen:
        .res word,1
nativeCliSavedSawCr:
        .res word,1
nativeCliSavedLineNum:
        .res long,1
nativeCliModuleSavedLineLen:
        .res word,1
nativeCliModuleSavedSawCr:
        .res word,1
nativeCliModuleSavedLineNum:
        .res long,1
nativeCliInputChar:
        .res byte,1
nativeCliDecimalChar:
        .res byte,2
nativeCliSourceLine:
        .res byte,SOURCE_LINE_BUFFER_CAPACITY
nativeCliParserTailBuffer:
        .res byte,SOURCE_LINE_BUFFER_CAPACITY
nativeCliCurrentPath:
        .res byte,PATH_BUFFER_CAPACITY
nativeCliSavedPath:
        .res byte,PATH_BUFFER_CAPACITY
nativeCliModuleSavedPath:
        .res byte,PATH_BUFFER_CAPACITY
nativeCliIncludeTarget:
        .res byte,PATH_BUFFER_CAPACITY
nativeCliIncludePath:
        .res byte,PATH_BUFFER_CAPACITY
nativeCliIncludeRootPath:
        .res byte,PATH_BUFFER_CAPACITY
opforgeNativeCliPrvmRouteFrame:
        .res byte,PRVM_ROUTE_FRAME_SIZE
opforgeNativeCliPrvmResultBuffer:
        .res byte,PRVM_ROUTE_RESULT_CAPACITY
opforgeNativeCliPrvmDiagBuffer:
        .res byte,PRVM_ROUTE_DIAG_CAPACITY
opforgeNativeCliPrvmResumeBuffer:
        .res byte,PRVM_ROUTE_RESUME_CAPACITY
opforgeNativeCliPrvmExprRequest:
        .res byte,PRVM_ROUTE_EXPR_REQUEST_SIZE

nativeCliModuleUseStateStart:
nativeCliModuleCount:
        .res word,1
nativeCliImportCount:
        .res word,1
nativeCliModulePathCount:
        .res word,1
nativeCliImportSelectCount:
        .res word,1
nativeCliRootModuleId:
        .res word,1
nativeCliCurrentModuleId:
        .res word,1
nativeCliModuleDepth:
        .res word,1
nativeCliModuleNameTable:
        .res byte,NATIVE_MODULE_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
nativeCliModuleFileIdTable:
        .res word,NATIVE_MODULE_TABLE_CAPACITY
nativeCliModuleLineTable:
        .res long,NATIVE_MODULE_TABLE_CAPACITY
nativeCliModuleDepthTable:
        .res word,NATIVE_MODULE_TABLE_CAPACITY
nativeCliImportOwnerModuleTable:
        .res word,NATIVE_IMPORT_TABLE_CAPACITY
nativeCliImportModuleTable:
        .res word,NATIVE_IMPORT_TABLE_CAPACITY
nativeCliImportFileIdTable:
        .res word,NATIVE_IMPORT_TABLE_CAPACITY
nativeCliImportLineTable:
        .res long,NATIVE_IMPORT_TABLE_CAPACITY
nativeCliImportAliasTable:
        .res byte,NATIVE_IMPORT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
nativeCliImportSelectImportTable:
        .res word,NATIVE_IMPORT_SELECT_CAPACITY
nativeCliImportSelectNameTable:
        .res byte,NATIVE_IMPORT_SELECT_CAPACITY * TOKEN_BUFFER_CAPACITY
nativeCliImportSelectAliasTable:
        .res byte,NATIVE_IMPORT_SELECT_CAPACITY * TOKEN_BUFFER_CAPACITY
nativeCliImportSelectFlagsTable:
        .res word,NATIVE_IMPORT_SELECT_CAPACITY
nativeCliModulePathTable:
        .res byte,NATIVE_MODULE_PATH_CAPACITY * PATH_BUFFER_CAPACITY
nativeCliModuleUseStateEnd:

        .endsection

        .output "build/opforge_cli", format=hunk, sections=entry, code, data, bss
        .endmodule
