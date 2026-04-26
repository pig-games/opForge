; Thin AmigaOS CLI smoke wrapper for the package-backed tkpkg runtime.
;
; This slice exercises init, load_package, set_pipeline, tokenize_line, and
; last_error without folding CLI/report concerns into the core runtime modules.

        .module main
        .cpu 68020
        .use tkpkg.amigaos.abi (ENTRY_ORD_INIT, ENTRY_ORD_LOAD_PACKAGE)
        .use tkpkg.amigaos.abi (ENTRY_ORD_SET_PIPELINE, ENTRY_ORD_TOKENIZE_LINE)
        .use tkpkg.amigaos.abi (ENTRY_ORD_LAST_ERROR, TOKENIZE_LINE_SAMPLE_LINE_NUM)
        .use tkpkg.amigaos.abi (CB_INPUT_PTR, CB_INPUT_LEN, CB_OUTPUT_LEN, CB_STATUS_CODE)
        .use tkpkg.amigaos.buffers (controlBlockV1, lastErrorBuffer, packageStorage)
        .use tkpkg.amigaos.buffers (LAST_ERROR_BUFFER_PTR_V1, LAST_ERROR_BUFFER_CAPACITY)
        .use tkpkg.amigaos.service (tkpkg_service_dispatch_v1)

SysBase                         = 4
RETURN_OK                       = 0
RETURN_FAIL                     = 20

OpenLibrary                     = -552
CloseLibrary                    = -414
PutStr                          = -948
GetArgStr                       = -534
Open                            = -30
Close                           = -36
Read                            = -42

MODE_OLDFILE                    = 1005
PATH_BUFFER_CAPACITY            = 256
DEBUG_CLI_SOURCE_BUFFER_CAPACITY = 16384
DEBUG_CLI_MANIFEST_BUFFER_CAPACITY = 8192
DEBUG_CLI_MAX_LINE_BYTES        = LAST_ERROR_BUFFER_CAPACITY - 4
DEBUG_CLI_FILE_MODE_SINGLE      = 1
DEBUG_CLI_FILE_MODE_MANIFEST    = 2

PACKAGE_INPUT_PTR_V1           = LAST_ERROR_BUFFER_PTR_V1 + LAST_ERROR_BUFFER_CAPACITY

        .section entry, kind=code

start:
        MOVE.L #RETURN_FAIL, debugCliReturnCode

        LEA dosName, A1
        MOVEQ #36, D0
        MOVEA.L SysBase.W, A6
        JSR OpenLibrary(A6)

        TST.L D0
        BNE.W tkpkgDebugCliHaveDos

        LEA dosName, A1
        MOVEQ #0, D0
        MOVEA.L SysBase.W, A6
        JSR OpenLibrary(A6)

        TST.L D0
        BEQ.W tkpkgDebugCliDone

        MOVE.L D0, debugCliDosBase
        MOVE.L #dosVersionFailureText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        BRA.W tkpkgDebugCliCloseDos

tkpkgDebugCliHaveDos:
        MOVE.L D0, debugCliDosBase
        MOVE.L #startedText, D1
        BSR.W tkpkg_debug_cli_put_str_v1

        LEA controlBlockV1, A0
        MOVEQ #ENTRY_ORD_INIT, D0
        BSR.W tkpkg_debug_cli_dispatch_service_v1
        BSR.W tkpkg_debug_cli_read_status_v1
        TST.B D0
        BNE.W tkpkgDebugCliReportFailure

        LEA tkpkgDebugCliPackageData, A1
        LEA packageStorage, A2
        MOVE.W tkpkgDebugCliPackageLen, D0
        BSR.W tkpkg_debug_cli_copy_bytes_v1

        LEA controlBlockV1, A0
        MOVE.W #PACKAGE_INPUT_PTR_V1, D0
        MOVE.W tkpkgDebugCliPackageLen, D1
        BSR.W tkpkg_debug_cli_write_input_window_v1
        MOVEQ #ENTRY_ORD_LOAD_PACKAGE, D0
        BSR.W tkpkg_debug_cli_dispatch_service_v1
        BSR.W tkpkg_debug_cli_read_status_v1
        TST.B D0
        BNE.W tkpkgDebugCliReportFailure
        MOVE.L #packageLoadedText, D1
        BSR.W tkpkg_debug_cli_put_str_v1

.ifdef OPFORGE_FS_UAE_TKPKG_MANIFEST
        LEA defaultSmokeManifestPath, A0
        LEA debugCliInputPathBuffer, A1
tkpkgDebugCliCopyDefaultManifestPathLoop:
        MOVE.B (A0)+, (A1)+
        BNE.S tkpkgDebugCliCopyDefaultManifestPathLoop
        MOVEQ #DEBUG_CLI_FILE_MODE_MANIFEST, D0
.else
.ifdef OPFORGE_FS_UAE_SMOKE
        LEA defaultSmokeInputPath, A0
        LEA debugCliInputPathBuffer, A1
tkpkgDebugCliCopyDefaultSmokePathLoop:
        MOVE.B (A0)+, (A1)+
        BNE.S tkpkgDebugCliCopyDefaultSmokePathLoop
        MOVEQ #DEBUG_CLI_FILE_MODE_SINGLE, D0
.else
        BSR.W tkpkg_debug_cli_parse_optional_input_path_v1
        TST.L D0
        BMI.W tkpkgDebugCliCloseDos
.endif
.endif
        MOVE.L D0, debugCliFileModeEnabled

        LEA setPipelineRequest, A1
        LEA lastErrorBuffer, A2
        MOVEQ #SET_PIPELINE_REQUEST_LEN, D0
        BSR.W tkpkg_debug_cli_copy_bytes_v1

        LEA controlBlockV1, A0
        MOVE.W #LAST_ERROR_BUFFER_PTR_V1, D0
        MOVEQ #SET_PIPELINE_REQUEST_LEN, D1
        BSR.W tkpkg_debug_cli_write_input_window_v1
        MOVEQ #ENTRY_ORD_SET_PIPELINE, D0
        BSR.W tkpkg_debug_cli_dispatch_service_v1
        BSR.W tkpkg_debug_cli_read_status_v1
        TST.B D0
        BNE.W tkpkgDebugCliReportFailure

        MOVE.L debugCliFileModeEnabled, D0
        CMPI.L #DEBUG_CLI_FILE_MODE_MANIFEST, D0
        BEQ.W tkpkgDebugCliPipelineManifestMode
        TST.L D0
        BNE.W tkpkgDebugCliPipelineFileMode

        MOVE.L #pipelineSuccessText, D1
        BSR.W tkpkg_debug_cli_put_str_v1

        LEA tokenizeLineRequest, A1
        LEA lastErrorBuffer, A2
        MOVEQ #TOKENIZE_LINE_REQUEST_LEN, D0
        BSR.W tkpkg_debug_cli_copy_bytes_v1

        LEA controlBlockV1, A0
        MOVE.W #LAST_ERROR_BUFFER_PTR_V1, D0
        MOVEQ #TOKENIZE_LINE_REQUEST_LEN, D1
        BSR.W tkpkg_debug_cli_write_input_window_v1
        MOVEQ #ENTRY_ORD_TOKENIZE_LINE, D0
        BSR.W tkpkg_debug_cli_dispatch_service_v1
        BSR.W tkpkg_debug_cli_read_status_v1
        TST.B D0
        BNE.W tkpkgDebugCliReportFailure
        BSR.W tkpkg_debug_cli_read_output_len_v1
        TST.W D0
        BEQ.W tkpkgDebugCliReportEmptyTokenizeOutput
        MOVE.L #tokenizeSuccessText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L #lastErrorBuffer, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L #newlineText, D1
        BSR.W tkpkg_debug_cli_put_str_v1

        BRA.W tkpkgDebugCliCheckLastErrorClear

tkpkgDebugCliPipelineManifestMode:
        MOVE.L #pipelineSuccessText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        BSR.W tkpkg_debug_cli_tokenize_manifest_v1
        TST.L D0
        BMI.W tkpkgDebugCliCloseDos
        BNE.W tkpkgDebugCliReportFailure
        BRA.W tkpkgDebugCliCheckLastErrorClear

tkpkgDebugCliPipelineFileMode:
        MOVE.L #pipelineSuccessText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        BSR.W tkpkg_debug_cli_tokenize_file_v1
        TST.L D0
        BMI.W tkpkgDebugCliCloseDos
        BNE.W tkpkgDebugCliReportFailure

tkpkgDebugCliCheckLastErrorClear:

        LEA controlBlockV1, A0
        BSR.W tkpkg_debug_cli_run_last_error_v1
        TST.B D0
        BNE.W tkpkgDebugCliCloseDos
        BSR.W tkpkg_debug_cli_read_output_len_v1
        TST.W D0
        BNE.W tkpkgDebugCliReportLastErrorBuffer

        MOVE.L #lastErrorClearText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L #RETURN_OK, debugCliReturnCode
        BRA.W tkpkgDebugCliCloseDos

tkpkgDebugCliReportFailure:
        LEA controlBlockV1, A0
        BSR.W tkpkg_debug_cli_run_last_error_v1
        TST.B D0
        BNE.W tkpkgDebugCliCloseDos

tkpkgDebugCliReportLastErrorBuffer:
        MOVE.L #failurePrefixText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L #lastErrorBuffer, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L #newlineText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        BRA.W tkpkgDebugCliCloseDos

tkpkgDebugCliReportEmptyTokenizeOutput:
        MOVE.L #emptyTokenizeOutputText, D1
        BSR.W tkpkg_debug_cli_put_str_v1

tkpkgDebugCliCloseDos:
        BSR.W tkpkg_debug_cli_close_dos_v1

tkpkgDebugCliDone:
        MOVE.L debugCliReturnCode, D0
        RTS

tkpkg_debug_cli_dispatch_service_v1:
        MOVE.L D7, -(SP)
        MOVE.L A5, -(SP)
        MOVE.L A6, -(SP)
        JSR tkpkg_service_dispatch_v1
        MOVEA.L (SP)+, A6
        MOVEA.L (SP)+, A5
        MOVE.L (SP)+, D7
        RTS

tkpkg_debug_cli_put_str_v1:
        MOVEA.L debugCliDosBase, A6
        JSR PutStr(A6)
        RTS

tkpkg_debug_cli_parse_optional_input_path_v1:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVEA.L debugCliDosBase, A6
        JSR GetArgStr(A6)
        MOVEA.L D0, A3
        BSR.W tkpkgDebugCliSkipWhitespace
        TST.B (A3)
        BEQ.S tkpkgDebugCliNoInputPath
        CMPI.B #'"', (A3)
        BEQ.S tkpkgDebugCliQuotedPath
        LEA debugCliInputPathBuffer, A1
        BSR.W tkpkgDebugCliCopyPathToken
        TST.L D0
        BNE.S tkpkgDebugCliUsagePath
        BSR.W tkpkgDebugCliSkipWhitespace
        TST.B (A3)
        BNE.S tkpkgDebugCliUsagePath
        MOVEQ #1, D0
        BRA.S tkpkgDebugCliParseArgsDone

tkpkgDebugCliNoInputPath:
        MOVEQ #0, D0
        BRA.S tkpkgDebugCliParseArgsDone

tkpkgDebugCliQuotedPath:
        MOVE.L #quotedPathFailureText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.S tkpkgDebugCliParseArgsDone

tkpkgDebugCliUsagePath:
        MOVE.L #usageText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0

tkpkgDebugCliParseArgsDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tkpkgDebugCliSkipWhitespace:
        CMPI.B #' ', (A3)
        BEQ.S tkpkgDebugCliSkipOne
        CMPI.B #9, (A3)
        BEQ.S tkpkgDebugCliSkipOne
        CMPI.B #10, (A3)
        BEQ.S tkpkgDebugCliSkipOne
        CMPI.B #13, (A3)
        BNE.S tkpkgDebugCliSkipDone

tkpkgDebugCliSkipOne:
        ADDQ.L #1, A3
        BRA.S tkpkgDebugCliSkipWhitespace

tkpkgDebugCliSkipDone:
        RTS

tkpkgDebugCliCopyPathToken:
        MOVE.L #PATH_BUFFER_CAPACITY - 1, D6

tkpkgDebugCliCopyPathLoop:
        MOVEQ #0, D0
        MOVE.B (A3), D0
        BEQ.S tkpkgDebugCliCopyPathDone
        CMPI.B #' ', D0
        BEQ.S tkpkgDebugCliCopyPathDone
        CMPI.B #9, D0
        BEQ.S tkpkgDebugCliCopyPathDone
        CMPI.B #10, D0
        BEQ.S tkpkgDebugCliCopyPathDone
        CMPI.B #13, D0
        BEQ.S tkpkgDebugCliCopyPathDone
        CMPI.B #'"', D0
        BEQ.S tkpkgDebugCliCopyPathFail
        TST.L D6
        BEQ.S tkpkgDebugCliCopyPathFail
        MOVE.B D0, (A1)+
        ADDQ.L #1, A3
        SUBQ.L #1, D6
        BRA.S tkpkgDebugCliCopyPathLoop

tkpkgDebugCliCopyPathDone:
        CLR.B (A1)
        MOVEQ #0, D0
        RTS

tkpkgDebugCliCopyPathFail:
        MOVEQ #1, D0
        RTS

tkpkg_debug_cli_tokenize_manifest_v1:
        MOVEM.L D2-D7/A2-A6, -(SP)
        LEA debugCliInputPathBuffer, A0
        BSR.W tkpkg_debug_cli_open_input_v1
        TST.L D0
        BNE.S tkpkgDebugCliManifestOpenOk
        MOVE.L #manifestOpenFailureText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliTokenizeManifestDone

tkpkgDebugCliManifestOpenOk:
        MOVE.L D0, D5
        MOVE.L #manifestOpenOkText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        LEA debugCliManifestBuffer, A0
        MOVE.L #DEBUG_CLI_MANIFEST_BUFFER_CAPACITY, D0
        MOVE.L D5, D1
        BSR.W tkpkg_debug_cli_read_input_v1
        CMP.L #-1, D0
        BNE.S tkpkgDebugCliManifestReadOk
        MOVE.L D5, D1
        BSR.W tkpkg_debug_cli_close_input_v1
        MOVE.L #inputReadFailureText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliTokenizeManifestDone

tkpkgDebugCliManifestReadOk:
        MOVE.L D0, D6
        LEA debugCliSourceFileProbeByte, A0
        MOVEQ #1, D0
        MOVE.L D5, D1
        BSR.W tkpkg_debug_cli_read_input_v1
        MOVE.L D0, D7
        MOVE.L D5, D1
        BSR.W tkpkg_debug_cli_close_input_v1
        CMP.L #-1, D7
        BNE.S tkpkgDebugCliManifestProbeOk
        MOVE.L #inputReadFailureText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliTokenizeManifestDone

tkpkgDebugCliManifestProbeOk:
        TST.L D7
        BEQ.S tkpkgDebugCliManifestFitsBuffer
        MOVE.L #manifestTooLargeText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliTokenizeManifestDone

tkpkgDebugCliManifestFitsBuffer:
        MOVE.L #manifestReadOkText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L D6, D0
        BSR.W tkpkg_debug_cli_tokenize_manifest_buffer_v1

tkpkgDebugCliTokenizeManifestDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tkpkg_debug_cli_tokenize_manifest_buffer_v1:
        MOVEM.L D2-D7/A2-A6, -(SP)
        LEA debugCliManifestBuffer, A3
        MOVE.L D0, D7

tkpkgDebugCliManifestLineLoop:
        TST.L D7
        BEQ.W tkpkgDebugCliManifestDone
        MOVEA.L A3, A1
        MOVEQ #0, D0

tkpkgDebugCliFindManifestLineEnd:
        TST.L D7
        BEQ.S tkpkgDebugCliManifestLineReady
        CMPI.B #10, (A3)
        BEQ.S tkpkgDebugCliConsumeManifestLf
        ADDQ.L #1, A3
        ADDQ.L #1, D0
        SUBQ.L #1, D7
        BRA.S tkpkgDebugCliFindManifestLineEnd

tkpkgDebugCliConsumeManifestLf:
        ADDQ.L #1, A3
        SUBQ.L #1, D7

tkpkgDebugCliManifestLineReady:
        TST.L D0
        BEQ.S tkpkgDebugCliManifestSkipLine
        LEA 0(A1, D0.L), A2
        SUBQ.L #1, A2
        CMPI.B #13, (A2)
        BNE.S tkpkgDebugCliManifestLineTrimmed
        SUBQ.L #1, D0
        BEQ.S tkpkgDebugCliManifestSkipLine

tkpkgDebugCliManifestLineTrimmed:
        CMPI.B #'#', (A1)
        BEQ.S tkpkgDebugCliManifestSkipLine
        CMPI.B #';', (A1)
        BEQ.S tkpkgDebugCliManifestSkipLine
        BSR.W tkpkg_debug_cli_prepare_manifest_entry_v1
        TST.L D0
        BMI.W tkpkgDebugCliManifestReturn
        BEQ.S tkpkgDebugCliManifestEntryOk
        BRA.W tkpkgDebugCliManifestReturn

tkpkgDebugCliManifestEntryOk:
        MOVE.L #manifestFileText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L #debugCliInputPathBuffer, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L #newlineText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L #manifestTokenizeBeginText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        BSR.W tkpkg_debug_cli_tokenize_file_v1
        TST.L D0
        BNE.S tkpkgDebugCliManifestReturn
        MOVE.L #manifestTokenizeOkText, D1
        BSR.W tkpkg_debug_cli_put_str_v1

tkpkgDebugCliManifestSkipLine:
        BRA.W tkpkgDebugCliManifestLineLoop

tkpkgDebugCliManifestDone:
        MOVEQ #0, D0

tkpkgDebugCliManifestReturn:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tkpkg_debug_cli_prepare_manifest_entry_v1:
        MOVEM.L D2-D7/A2-A6, -(SP)
        MOVEA.L A1, A4
        MOVE.L D0, D7
        MOVEA.L A1, A3
        MOVE.L D0, D6
        MOVEQ #0, D5

tkpkgDebugCliManifestFindTab:
        TST.L D6
        BEQ.W tkpkgDebugCliManifestNoPipeline
        CMPI.B #9, (A3)
        BEQ.S tkpkgDebugCliManifestHasPipeline
        ADDQ.L #1, A3
        ADDQ.L #1, D5
        SUBQ.L #1, D6
        BRA.S tkpkgDebugCliManifestFindTab

tkpkgDebugCliManifestHasPipeline:
        TST.L D5
        BNE.S tkpkgDebugCliManifestCpuPresent
        MOVE.L #manifestFormatFailureText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestCpuPresent:
        MOVE.L D7, D0
        SUB.L D5, D0
        SUBQ.L #1, D0
        TST.L D0
        BNE.S tkpkgDebugCliManifestPathPresent
        MOVE.L #manifestFormatFailureText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestPathPresent:
        CMP.L #LAST_ERROR_BUFFER_CAPACITY - 1, D5
        BLS.S tkpkgDebugCliManifestCpuFits
        MOVE.L #manifestPipelineTooLongText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestCpuFits:
        LEA lastErrorBuffer, A2
        MOVEA.L A4, A1
        MOVE.L D5, D0
        BSR.W tkpkg_debug_cli_copy_bytes_v1
        CLR.B (A2)
        MOVEM.L D5-D7/A3-A4, -(SP)
        MOVE.L #manifestPipelineBeginText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L #lastErrorBuffer, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L #newlineText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEM.L (SP)+, D5-D7/A3-A4
        MOVE.L D5, D0
        ADDQ.L #1, D0
        MOVEM.L D5-D7/A3-A4, -(SP)
        BSR.W tkpkg_debug_cli_set_pipeline_from_last_error_v1
        MOVE.L D0, D1
        MOVEM.L (SP)+, D5-D7/A3-A4
        MOVE.L D1, D0
        TST.B D0
        BEQ.S tkpkgDebugCliManifestPipelineOk
        MOVEQ #1, D0
        BRA.W tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestPipelineOk:
        MOVEM.L D5-D7/A3-A4, -(SP)
        MOVE.L #manifestPipelineOkText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEM.L (SP)+, D5-D7/A3-A4
        LEA 1(A3), A1
        MOVE.L D7, D0
        SUB.L D5, D0
        SUBQ.L #1, D0
        BSR.W tkpkg_debug_cli_copy_manifest_path_v1
        TST.L D0
        BEQ.S tkpkgDebugCliManifestPrepareOk
        MOVE.L #manifestPathTooLongText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.S tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestNoPipeline:
        MOVEA.L A4, A1
        MOVE.L D7, D0
        BSR.W tkpkg_debug_cli_copy_manifest_path_v1
        TST.L D0
        BEQ.S tkpkgDebugCliManifestPrepareOk
        MOVE.L #manifestPathTooLongText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.S tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestPrepareOk:
        MOVEQ #0, D0

tkpkgDebugCliManifestPrepareDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tkpkg_debug_cli_copy_manifest_path_v1:
        MOVEM.L D1-D3/A0-A2, -(SP)
        CMP.L #PATH_BUFFER_CAPACITY - 1, D0
        BLS.S tkpkgDebugCliManifestCopyFits
        MOVEQ #-1, D0
        BRA.S tkpkgDebugCliManifestCopyDone

tkpkgDebugCliManifestCopyFits:
        LEA debugCliInputPathBuffer, A0
        MOVEA.L A1, A2
        MOVE.L D0, D2

tkpkgDebugCliManifestCopyLoop:
        TST.L D2
        BEQ.S tkpkgDebugCliManifestCopyTerminator
        MOVE.B (A2)+, (A0)+
        SUBQ.L #1, D2
        BRA.S tkpkgDebugCliManifestCopyLoop

tkpkgDebugCliManifestCopyTerminator:
        CLR.B (A0)
        MOVEQ #0, D0

tkpkgDebugCliManifestCopyDone:
        MOVEM.L (SP)+, D1-D3/A0-A2
        RTS

tkpkg_debug_cli_set_pipeline_from_last_error_v1:
        MOVEM.L D1/A0, -(SP)
        LEA controlBlockV1, A0
        MOVE.W D0, D1
        MOVE.W #LAST_ERROR_BUFFER_PTR_V1, D0
        BSR.W tkpkg_debug_cli_write_input_window_v1
        MOVEQ #ENTRY_ORD_SET_PIPELINE, D0
        BSR.W tkpkg_debug_cli_dispatch_service_v1
        BSR.W tkpkg_debug_cli_read_status_v1
        MOVEM.L (SP)+, D1/A0
        RTS

tkpkg_debug_cli_tokenize_file_v1:
        MOVEM.L D2-D7/A2-A6, -(SP)
        LEA debugCliInputPathBuffer, A0
        BSR.W tkpkg_debug_cli_open_input_v1
        TST.L D0
        BNE.S tkpkgDebugCliFileOpenOk
        MOVE.L #inputOpenFailureText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliTokenizeFileDone

tkpkgDebugCliFileOpenOk:
        MOVE.L D0, D5
        LEA debugCliSourceFileBuffer, A0
        MOVE.L #DEBUG_CLI_SOURCE_BUFFER_CAPACITY, D0
        MOVE.L D5, D1
        BSR.W tkpkg_debug_cli_read_input_v1
        CMP.L #-1, D0
        BNE.S tkpkgDebugCliFileReadOk
        MOVE.L D5, D1
        BSR.W tkpkg_debug_cli_close_input_v1
        MOVE.L #inputReadFailureText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliTokenizeFileDone

tkpkgDebugCliFileReadOk:
        MOVE.L D0, D6
        LEA debugCliSourceFileProbeByte, A0
        MOVEQ #1, D0
        MOVE.L D5, D1
        BSR.W tkpkg_debug_cli_read_input_v1
        MOVE.L D0, D7
        MOVE.L D5, D1
        BSR.W tkpkg_debug_cli_close_input_v1
        CMP.L #-1, D7
        BNE.S tkpkgDebugCliFileProbeOk
        MOVE.L #inputReadFailureText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliTokenizeFileDone

tkpkgDebugCliFileProbeOk:
        TST.L D7
        BEQ.S tkpkgDebugCliFileFitsBuffer
        MOVE.L #fileTooLargeText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliTokenizeFileDone

tkpkgDebugCliFileFitsBuffer:
        MOVE.L D6, D0
        BSR.W tkpkg_debug_cli_tokenize_source_buffer_v1

tkpkgDebugCliTokenizeFileDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tkpkg_debug_cli_tokenize_source_buffer_v1:
        MOVEM.L D2-D7/A2-A6, -(SP)
        LEA debugCliSourceFileBuffer, A3
        MOVE.L D0, D7
        MOVEQ #1, D6

tkpkgDebugCliSourceLineLoop:
        TST.L D7
        BEQ.S tkpkgDebugCliSourceDone
        MOVEA.L A3, A1
        MOVEQ #0, D0

tkpkgDebugCliFindLineEnd:
        TST.L D7
        BEQ.S tkpkgDebugCliLineReady
        CMPI.B #10, (A3)
        BEQ.S tkpkgDebugCliConsumeLf
        ADDQ.L #1, A3
        ADDQ.L #1, D0
        SUBQ.L #1, D7
        BRA.S tkpkgDebugCliFindLineEnd

tkpkgDebugCliConsumeLf:
        ADDQ.L #1, A3
        SUBQ.L #1, D7

tkpkgDebugCliLineReady:
        TST.L D0
        BEQ.S tkpkgDebugCliLineDispatch
        LEA 0(A1, D0.L), A2
        SUBQ.L #1, A2
        CMPI.B #13, (A2)
        BNE.S tkpkgDebugCliLineDispatch
        SUBQ.L #1, D0

tkpkgDebugCliLineDispatch:
        MOVE.L D6, D1
        BSR.W tkpkg_debug_cli_tokenize_line_slice_v1
        TST.L D0
        BNE.W tkpkgDebugCliSourceReturn
        ADDQ.L #1, D6
        BRA.W tkpkgDebugCliSourceLineLoop

tkpkgDebugCliSourceDone:
        MOVEQ #0, D0

tkpkgDebugCliSourceReturn:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tkpkg_debug_cli_tokenize_line_slice_v1:
        MOVEM.L D2-D7/A2-A6, -(SP)
        TST.L D0
        BEQ.W tkpkgDebugCliSliceOk
        CMPI.L #DEBUG_CLI_MAX_LINE_BYTES, D0
        BLS.W tkpkgDebugCliSliceFits
        MOVE.L #lineTooLongText, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVEQ #-1, D0
        BRA.W tkpkgDebugCliSliceDone

tkpkgDebugCliSliceFits:
        MOVEA.L A1, A3
        MOVE.L D0, D7
        LEA lastErrorBuffer, A2
        MOVE.L D1, D2
        MOVE.B D2, (A2)+
        LSR.L #8, D2
        MOVE.B D2, (A2)+
        LSR.L #8, D2
        MOVE.B D2, (A2)+
        LSR.L #8, D2
        MOVE.B D2, (A2)+
        MOVEA.L A3, A1
        MOVE.L D7, D0
        BSR.W tkpkg_debug_cli_copy_bytes_v1

        LEA controlBlockV1, A0
        MOVE.W #LAST_ERROR_BUFFER_PTR_V1, D0
        MOVE.W D7, D1
        ADDQ.W #4, D1
        BSR.W tkpkg_debug_cli_write_input_window_v1
        MOVEQ #ENTRY_ORD_TOKENIZE_LINE, D0
        BSR.W tkpkg_debug_cli_dispatch_service_v1
        BSR.W tkpkg_debug_cli_read_status_v1
        TST.B D0
        BNE.W tkpkgDebugCliSliceRuntimeFailure
        BSR.W tkpkg_debug_cli_read_output_len_v1
        TST.W D0
        BEQ.W tkpkgDebugCliSliceOk
        LEA lastErrorBuffer, A1
        CLR.B 0(A1,D0.L)
        MOVE.L #lastErrorBuffer, D1
        BSR.W tkpkg_debug_cli_put_str_v1
        MOVE.L #newlineText, D1
        BSR.W tkpkg_debug_cli_put_str_v1

tkpkgDebugCliSliceOk:
        MOVEQ #0, D0
        BRA.W tkpkgDebugCliSliceDone

tkpkgDebugCliSliceRuntimeFailure:
        MOVEQ #1, D0

tkpkgDebugCliSliceDone:
        MOVEM.L (SP)+, D2-D7/A2-A6
        RTS

tkpkg_debug_cli_open_input_v1:
        MOVE.L A0, D1
        MOVE.L #MODE_OLDFILE, D2
        MOVEA.L debugCliDosBase, A6
        JSR Open(A6)
        RTS

tkpkg_debug_cli_read_input_v1:
        MOVE.L A0, D2
        MOVE.L D0, D3
        MOVEA.L debugCliDosBase, A6
        JSR Read(A6)
        RTS

tkpkg_debug_cli_close_input_v1:
        MOVEA.L debugCliDosBase, A6
        JSR Close(A6)
        RTS

tkpkg_debug_cli_close_dos_v1:
        MOVEA.L debugCliDosBase, A1
        MOVEA.L SysBase.W, A6
        JSR CloseLibrary(A6)
        RTS

tkpkg_debug_cli_copy_bytes_v1:
        MOVE.W D0, D2
        TST.W D2
        BEQ.S tkpkgDebugCliCopyDone

tkpkgDebugCliCopyLoop:
        MOVE.B (A1)+, (A2)+
        SUBQ.W #1, D2
        BNE.S tkpkgDebugCliCopyLoop

tkpkgDebugCliCopyDone:
        RTS

tkpkg_debug_cli_write_input_window_v1:
        MOVE.B D0, CB_INPUT_PTR(A0)
        LSR.W #8, D0
        MOVE.B D0, 17(A0)
        MOVE.B D1, CB_INPUT_LEN(A0)
        LSR.W #8, D1
        MOVE.B D1, 19(A0)
        RTS

tkpkg_debug_cli_clear_input_window_v1:
        CLR.B CB_INPUT_PTR(A0)
        CLR.B 17(A0)
        CLR.B CB_INPUT_LEN(A0)
        CLR.B 19(A0)
        RTS

tkpkg_debug_cli_read_status_v1:
        MOVEQ #0, D0
        MOVE.B CB_STATUS_CODE(A0), D0
        RTS

tkpkg_debug_cli_read_output_len_v1:
        MOVEQ #0, D0
        MOVE.B CB_OUTPUT_LEN(A0), D0
        MOVEQ #0, D1
        MOVE.B 23(A0), D1
        LSL.W #8, D1
        OR.W D1, D0
        RTS

tkpkg_debug_cli_run_last_error_v1:
        BSR.W tkpkg_debug_cli_clear_input_window_v1
        MOVEQ #ENTRY_ORD_LAST_ERROR, D0
        JSR tkpkg_service_dispatch_v1
        BSR.W tkpkg_debug_cli_read_status_v1
        RTS

        .endsection

        .section data, kind=data

dosName:
        .byte "dos.library", 0

debugCliMarker:
        .byte "TKPKG-DEBUG-CLI-V1", 0

failurePrefixText:
        .byte "tkpkg failure: ", 0

dosVersionFailureText:
        .byte "tkpkg_debug_cli requires dos.library v36+", 10, 0

startedText:
        .byte "tkpkg_debug_cli started", 10, 0

.ifdef OPFORGE_FS_UAE_SMOKE
defaultSmokeInputPath:
        .byte "Work:opforge_fsuae_smoke_input.asm", 0
.endif

.ifdef OPFORGE_FS_UAE_TKPKG_MANIFEST
defaultSmokeManifestPath:
        .byte "Work:opforge_fsuae_tkpkg_manifest.txt", 0
.endif

usageText:
        .byte "Usage: tkpkg_debug_cli [input-path]", 10, 0

quotedPathFailureText:
        .byte "tkpkg_debug_cli: quoted paths are not supported", 10, 0

packageLoadedText:
        .byte "tkpkg package loaded", 10, 0

pipelineSuccessText:
        .byte "TKPKG load_package/set_pipeline OK", 10, 0

tokenizeSuccessText:
        .byte "TKPKG tokenize_line OK", 10, 0

lastErrorClearText:
        .byte "TKPKG last_error clear OK", 10, 0

emptyTokenizeOutputText:
        .byte "tkpkg failure: tokenize_line returned empty output", 10, 0

inputOpenFailureText:
        .byte "tkpkg failure: failed to open input file", 10, 0

manifestOpenFailureText:
        .byte "tkpkg failure: failed to open manifest file", 10, 0

inputReadFailureText:
        .byte "tkpkg failure: failed to read input file", 10, 0

fileTooLargeText:
        .byte "tkpkg failure: input file exceeds debug-cli buffer", 10, 0

manifestTooLargeText:
        .byte "tkpkg failure: manifest file exceeds debug-cli buffer", 10, 0

manifestPathTooLongText:
        .byte "tkpkg failure: manifest path exceeds debug-cli path buffer", 10, 0

manifestPipelineTooLongText:
        .byte "tkpkg failure: manifest pipeline exceeds debug-cli buffer", 10, 0

manifestFormatFailureText:
        .byte "tkpkg failure: manifest entry requires cpu, tab, and path", 10, 0

manifestFileText:
        .byte "TKPKG manifest file ", 0

manifestOpenOkText:
        .byte "TKPKG manifest open OK", 10, 0

manifestReadOkText:
        .byte "TKPKG manifest read OK", 10, 0

manifestPipelineBeginText:
        .byte "TKPKG manifest set_pipeline begin ", 0

manifestPipelineOkText:
        .byte "TKPKG manifest set_pipeline OK", 10, 0

manifestTokenizeBeginText:
        .byte "TKPKG manifest tokenize_file begin", 10, 0

manifestTokenizeOkText:
        .byte "TKPKG manifest tokenize_file OK", 10, 0

lineTooLongText:
        .byte "tkpkg failure: input line exceeds tokenize_line payload budget", 10, 0

newlineText:
        .byte 10, 0

        .align 2

debugCliDosBase:
        .long 0

debugCliReturnCode:
        .long RETURN_FAIL

debugCliFileModeEnabled:
        .long 0

tkpkgDebugCliPackageLen:
        .word TKPKG_DEBUG_CLI_PACKAGE_LEN

setPipelineRequest:
.ifdef TKPKG_DEBUG_PIPELINE_M6502
        .byte "m6502", 0
.else
.ifdef TKPKG_DEBUG_PIPELINE_65C02
        .byte "65c02", 0
.else
.ifdef TKPKG_DEBUG_PIPELINE_65816
        .byte "65816", 0
.else
.ifdef TKPKG_DEBUG_PIPELINE_45GS02
        .byte "45gs02", 0
.else
.ifdef TKPKG_DEBUG_PIPELINE_8085
        .byte "8085", 0
.else
.ifdef TKPKG_DEBUG_PIPELINE_Z80
        .byte "z80", 0, "zilog"
.else
.ifdef TKPKG_DEBUG_PIPELINE_M6809
        .byte "m6809", 0
.else
.ifdef TKPKG_DEBUG_PIPELINE_HD6309
        .byte "hd6309", 0
.else
.ifdef TKPKG_DEBUG_PIPELINE_M68000
        .byte "m68000", 0, "motorola68k"
.else
.ifdef TKPKG_DEBUG_PIPELINE_M68010
        .byte "m68010", 0, "motorola68k"
.else
.ifdef TKPKG_DEBUG_PIPELINE_M68030
        .byte "m68030", 0, "motorola68k"
.else
.ifdef TKPKG_DEBUG_PIPELINE_M68040
        .byte "m68040", 0, "motorola68k"
.else
.ifdef TKPKG_DEBUG_PIPELINE_M68080
        .byte "m68080", 0, "motorola68k"
.else
        .byte "m68020", 0, "motorola68k"
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
.endif
setPipelineRequestEnd:

tokenizeLineRequest:
        .byte TOKENIZE_LINE_SAMPLE_LINE_NUM, 0, 0, 0
        .byte "move.b d0,d1"
tokenizeLineRequestEnd:

        .align 2
tkpkgDebugCliPackageData:
        .incbin "tkpkg_debug_cli_package.opasm"
tkpkgDebugCliPackageDataEnd:

SET_PIPELINE_REQUEST_LEN = setPipelineRequestEnd - setPipelineRequest
TOKENIZE_LINE_REQUEST_LEN = tokenizeLineRequestEnd - tokenizeLineRequest
TKPKG_DEBUG_CLI_PACKAGE_LEN = tkpkgDebugCliPackageDataEnd - tkpkgDebugCliPackageData

        .endsection
        .section bss, kind=bss

debugCliInputPathBuffer:
        .res byte, PATH_BUFFER_CAPACITY

debugCliSourceFileBuffer:
        .res byte, DEBUG_CLI_SOURCE_BUFFER_CAPACITY

debugCliManifestBuffer:
        .res byte, DEBUG_CLI_MANIFEST_BUFFER_CAPACITY

debugCliSourceFileProbeByte:
        .res byte, 1

        .endsection
        .output "build/tkpkg_debug_cli.hunk", format=hunk, sections=entry,code,data,bss
        .endmodule
