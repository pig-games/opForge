; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module main
	.cpu 68020
	.use tkpkg.amigaos.abi (ENTRY_ORD_INIT, ENTRY_ORD_LOAD_PACKAGE)
	.use tkpkg.amigaos.abi (ENTRY_ORD_SET_PIPELINE, ENTRY_ORD_TOKENIZE_LINE)
	.use tkpkg.amigaos.abi (ENTRY_ORD_PARSE_LINE, ENTRY_ORD_ENCODE_INSTRUCTION)
	.use tkpkg.amigaos.abi (ENTRY_ORD_EVALUATE_EXPRESSION, ENTRY_ORD_SELECT_INSTRUCTION)
	.use tkpkg.amigaos.abi (ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION)
	.use tkpkg.amigaos.abi (CB_INPUT_PTR, CB_INPUT_LEN, CB_OUTPUT_LEN, CB_STATUS_CODE)
	.use tkpkg.amigaos.abi (CB_LAST_ERROR_LEN)
	.use tkpkg.amigaos.abi (CB_EXTENSION_PTR, CB_EXTENSION_LEN)
	.use tkpkg.amigaos.buffers (controlBlockV1, lastErrorBuffer, packageStorage)
	.use tkpkg.amigaos.buffers (tokenRecordBuffer, tokenScratchBuffer)
	.use tkpkg.amigaos.buffers (lastTokenCount, lastLexemeLen, TOKEN_RECORD_SIZE)
	.use tkpkg.amigaos.buffers (ActiveParserVmOffsetLo)
	.use tkpkg.amigaos.buffers (PACKAGE_STORAGE_CAPACITY)
	.use tkpkg.amigaos.buffers (LAST_ERROR_BUFFER_PTR_V1, LAST_ERROR_BUFFER_CAPACITY)
	.use tkpkg.amigaos.service (tkpkgServiceDispatchV1)
	.use opasm.amigaos.engine (opasmEngineRunTwoPassV1)
	.use opasm.amigaos.engine (opasmEngineAssemblySessionStart, opasmEngineStmtCount)
	.use opasm.amigaos.engine (opasmEngineSessionPass, opasmEngineSourceRecordCount)
	.use opasm.amigaos.engine (opasmEngineLabelCount, opasmEngineImageByteCount)
	.use opasm.amigaos.engine (opasmEngineSessionCpuName, opasmEngineSessionOrigin)
	.use opasm.amigaos.engine (opasmEngineSessionCurrentPc, opasmEngineSourceLineNumTable)
	.use opasm.amigaos.engine (opasmEngineSourceLineLenTable, opasmEngineStmtLineTable)
	.use opasm.amigaos.engine (opasmEngineStmtSourceLineLenTable, opasmEngineStmtSourceLineTextTable)
	.use opasm.amigaos.engine (opasmEngineStmtLabelLenTable, opasmEngineStmtMnemLenTable)
	.use opasm.amigaos.engine (opasmEngineStmtOperandLenTable)
	.use opasm.amigaos.engine (opasmEngineStmtDirectiveKindTable, opasmEngineStmtMnemOffTable)
	.use opasm.amigaos.engine (opasmEngineStmtLabelNameTable, opasmEngineStmtMnemNameTable)
	.use opasm.amigaos.engine (opasmEngineStmtOperandNameTable, opasmEngineLabelValueTable)
	.use opasm.amigaos.engine (opasmEngineStmtExprFlagsTable, opasmEngineStmtExprOperandIndexTable)
	.use opasm.amigaos.engine (opasmEngineStmtExprSlotIndexTable, opasmEngineStmtExprStartTokenTable)
	.use opasm.amigaos.engine (opasmEngineStmtExprEndTokenTable, opasmEngineStmtExprSpanLineTable)
	.use opasm.amigaos.engine (opasmEngineStmtExprSpanStartTable, opasmEngineStmtExprSpanEndTable)
	.use opasm.amigaos.engine (opasmEngineLabelNameTable, opasmEngineLabelFinalizedTable)
	.use opasm.amigaos.engine (opasmEngineImageBuffer)

SYS_BASE                        = 4

PR_CLI                          = 172
PR_MSG_PORT                     = 92

OPEN_LIBRARY                    = -552
CLOSE_LIBRARY                   = -414
FIND_TASK                       = -294
WAIT_PORT                       = -384
GET_MSG                         = -372
REPLY_MSG                       = -378
FORBID                          = -132

OPEN                            = -30
CLOSE                           = -36
READ                            = -42
WRITE                           = -48
PUT_STR                         = -948
GET_ARG_STR                     = -534

MODE_OLDFILE                    = 1005
MODE_NEWFILE                    = 1006

RETURN_OK                       = 0
RETURN_USAGE                    = 20
RETURN_FILE_FAILURE             = 21
RETURN_RUNTIME_FAILURE          = 22
RETURN_NOT_IMPLEMENTED          = 30
RETURN_WORKBENCH_UNSUPPORTED    = 31

PATH_BUFFER_CAPACITY            = 256
TOKEN_BUFFER_CAPACITY           = 64
SOURCE_LINE_BUFFER_CAPACITY     = 512
NATIVE_SOURCE_RECORD_CAPACITY   = 512
NATIVE_MODULE_TABLE_CAPACITY    = 16
NATIVE_IMPORT_TABLE_CAPACITY    = 32
NATIVE_MODULE_PATH_CAPACITY     = 8
NATIVE_IMPORT_SELECT_CAPACITY   = 64
NATIVE_STATEMENT_TABLE_CAPACITY = 160
NATIVE_LABEL_TABLE_CAPACITY     = 16
NATIVE_IMAGE_BUFFER_CAPACITY    = 4096
NATIVE_OPASM_ENGINE_CONTEXT_LONGS = 10
NATIVE_MODULE_USE_STATE_BYTES   = (7 * 2) + (NATIVE_MODULE_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_MODULE_TABLE_CAPACITY * 2) + (NATIVE_MODULE_TABLE_CAPACITY * 4) + (NATIVE_MODULE_TABLE_CAPACITY * 2) + (NATIVE_IMPORT_TABLE_CAPACITY * 2) + (NATIVE_IMPORT_TABLE_CAPACITY * 2) + (NATIVE_IMPORT_TABLE_CAPACITY * 2) + (NATIVE_IMPORT_TABLE_CAPACITY * 4) + (NATIVE_IMPORT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_IMPORT_SELECT_CAPACITY * 2) + (NATIVE_IMPORT_SELECT_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_IMPORT_SELECT_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_IMPORT_SELECT_CAPACITY * 2) + (NATIVE_MODULE_PATH_CAPACITY * PATH_BUFFER_CAPACITY)
NATIVE_ASSEMBLY_SESSION_BYTES   = (5 * 2) + TOKEN_BUFFER_CAPACITY + (2 * 4) + (NATIVE_SOURCE_RECORD_CAPACITY * 4) + (NATIVE_SOURCE_RECORD_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * SOURCE_LINE_BUFFER_CAPACITY) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_STATEMENT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + (NATIVE_STATEMENT_TABLE_CAPACITY * 2) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_STATEMENT_TABLE_CAPACITY * 4) + (NATIVE_LABEL_TABLE_CAPACITY * 4) + (NATIVE_LABEL_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY) + NATIVE_IMAGE_BUFFER_CAPACITY
PACKAGE_INPUT_PTR_V1            = LAST_ERROR_BUFFER_PTR_V1 + LAST_ERROR_BUFFER_CAPACITY
NATIVE_INCLUDE_DEPTH_LIMIT      = 1
PRVM_ROUTE_MAGIC_OPLR           = $4F504C52
PRVM_ROUTE_FRAME_SIZE           = 116
PRVM_ROUTE_ABI_VERSION_V1       = 1
PRVM_PARSER_CONTRACT_VERSION_V2 = 2
PRVM_ROUTE_RESULT_CAPACITY      = 256
PRVM_ROUTE_DIAG_CAPACITY        = 32
PRVM_ROUTE_RESUME_CAPACITY      = 40
PRVM_ROUTE_EXPR_REQUEST_SIZE    = 32
PRVM_ROUTE_EXPR_RESULT_SIZE     = 32
PRVM_ROUTE_EXPR_RESULT_CAPACITY = 4
PRVM_ROUTE_EXPR_RESULT_COUNT    = 0
PRVM_ROUTE_STEP_BUDGET          = 256
PRVM_STATUS_OK                  = 0
PRVM_STATUS_EXPR_REQUEST        = 1
PRVM_EXPR_SLOT_READY            = 1
PRVM_RESULT_RECORD_SIZE         = 32
PRVM_RESULT_RECORD_COUNT        = PRVM_ROUTE_RESULT_CAPACITY / PRVM_RESULT_RECORD_SIZE
PRVM_RESULT_LABEL_TEXT          = 2
PRVM_RESULT_MNEMONIC_TEXT       = 3
PRVM_RESULT_OPERAND_EXPR_SLOT   = 4
PRVM_RESULT_DIRECTIVE_TEXT      = 6
PRVM_RESULT_OPERAND_TEXT        = 7
NATIVE_TOKEN_RECORD_SIZE        = 20
NATIVE_EVAL_EXPR_EXTENSION_BYTES = 24
NATIVE_EVAL_EXPR_EXTENSION_PTR_V1 = LAST_ERROR_BUFFER_PTR_V1 + LAST_ERROR_BUFFER_CAPACITY - NATIVE_EVAL_EXPR_EXTENSION_BYTES
TK_KIND_IDENTIFIER              = 0
NCLI_PARSER_DIRECTIVE_NONE      = 0
NCLI_PARSER_DIRECTIVE_MODULE    = 1
NCLI_PARSER_DIRECTIVE_ENDMODULE = 2
NCLI_PARSER_DIRECTIVE_USE       = 3
NCLI_PARSER_DIRECTIVE_GENERIC   = 4

	.section entry, kind=code

; ---------------------------------------------------------------------------
; AmigaOS process entry for the native opForge CLI.
;
; Workbench launches are rejected for this deliverable slice because the native
; host contract is currently Shell/file based. Shell launches hand off to
; opforge_native_cli_run, which mirrors the Rust CLI orchestration surface for
; the supported native subset.
;
; Inputs:
; - AmigaOS process context; no explicit arguments.
;
; Outputs:
; - D0: AmigaDOS return code.
; ---------------------------------------------------------------------------

	.pub
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	clr.l d2  ; no Workbench startup message is pending until GetMsg succeeds

	suba.l a1, a1  ; Exec FindTask(NULL) => current process
	movea.l SYS_BASE.W, a6  ; Exec base for process and Workbench-message calls
	jsr FIND_TASK(a6)

	movea.l d0, a2
	tst.l PR_CLI(a2)  ; nonzero means Shell launch; zero means Workbench activation
	bne.w opforgeStartCli

	lea PR_MSG_PORT(a2), a0
	jsr WAIT_PORT(a6)
	lea PR_MSG_PORT(a2), a0
	jsr GET_MSG(a6)
	move.l d0, d2  ; preserve startup message so ReplyMsg can be sent before exit
	moveq #RETURN_WORKBENCH_UNSUPPORTED, d7
	bra.w opforgeStartReply

opforgeStartCli
	bsr.w opforgeNativeCliRun  ; run the Shell-native CLI host path
	move.l d0, d7  ; keep return code stable across optional Workbench reply path

opforgeStartReply
	tst.l d2
	beq.w opforgeStartDone
	jsr FORBID(a6)
	movea.l d2, a1
	jsr REPLY_MSG(a6)

opforgeStartDone
	move.l d7, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts

; ---------------------------------------------------------------------------
; Run one native opForge CLI invocation.
;
; This host entry owns AmigaDOS setup, argument parsing, package staging, source
; processing, report emission, and output-file writes. It should not grow new
; assembler semantics; future implementation work should move semantic ownership
; into native opasm/tkpkg/opcore stages.
;
; Inputs:
; - Shell argument tail is read from dos.library/GetArgStr, unless an FS-UAE
;   smoke define supplies a deterministic test argument tail.
;
; Outputs:
; - D0: AmigaDOS return code.
; - textual OPFORGE-NATIVE report is written to stdout.
; - flat `.bin` output is written when selected and image bytes exist.
; ---------------------------------------------------------------------------

opforgeNativeCliRun
	movem.l d2-d7/a2-a6, -(sp)
	move.l #RETURN_USAGE, NativeCliReturnCode

	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.W, a6  ; first try the AmigaOS 2.x+ dos.library version expected by tests
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	bne.s opforgeNativeCliHaveDos

	lea DosName, a1
	moveq #0, d0
	movea.l SYS_BASE.W, a6  ; fallback keeps older emulator images usable for smoke runs
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w opforgeNativeCliDone

opforgeNativeCliHaveDos
	move.l d0, NativeCliDosBase  ; all file/console calls below dispatch through dos.library base
	bsr.w opforgeNativeCliInitModuleUseState
	movea.l d0, a6
	jsr GET_ARG_STR(a6)
.ifdef OPFORGE_FS_UAE_SMOKE
	lea defaultFsUaeArgTail, a0
.else
	movea.l d0, a0
.endif
	bsr.w opforgeNativeCliParseArgs

	cmpi.w #NCLI_PARSE_HELP, d0
	beq.w opforgeNativeCliHelp
	cmpi.w #NCLI_PARSE_VERSION, d0
	beq.w opforgeNativeCliVersion
	tst.w d0
	beq.w opforgeNativeCliParsed

	bsr.w opforgeNativeCliReportParseError
	move.l #RETURN_USAGE, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliHelp
	move.l #HelpText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #RETURN_OK, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliVersion
	move.l #VersionText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #RETURN_OK, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliParsed
	lea NativeCliInputPath, a0
	bsr.w opforgeNativeCliOpenInput
	tst.l d0
	bne.s opforgeNativeCliInputOpened
	move.l #InputOpenErrorText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliInputPath, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #RETURN_FILE_FAILURE, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliInputOpened
	move.l d0, d1
	bsr.w opforgeNativeCliClose
	cmpi.w #NATIVE_OUTPUT_FORMAT_HUNK, NativeCliOutputFormat
	bne.s opforgeNativeCliOutputFormatReady
	move.l #NativeHunkNotImplementedText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #RETURN_NOT_IMPLEMENTED, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliOutputFormatReady
	move.l #StubHeaderText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #InputLabelText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliInputPath, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #BinLabelText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliBinPath, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	bsr.w opforgeNativeCliInitAssemblySession
	bsr.w opforgeNativeCliEmitModulePathRecords
	bsr.w opforgeNativeCliTokenizeFrontend
	tst.l d0
	beq.s opforgeNativeCliTokenizerOk
	move.l #TokenizerFailureText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #RETURN_RUNTIME_FAILURE, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliTokenizerOk
	move.l #ParserOkText, d1
	bsr.w opforgeNativeCliPutStr
	bsr.w opforgeNativeCliRunTwoPassEngine
	tst.l d0
	beq.s opforgeNativeCliPassesOk
	move.l #NativePassFailureText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #RETURN_RUNTIME_FAILURE, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliPassesOk
	bsr.w opforgeNativeCliEmitAssemblySessionSummary
	tst.w opasmEngineImageByteCount.l
	beq.s opforgeNativeCliEmitStub
	bsr.w opforgeNativeCliWriteFlatOutput
	tst.l d0
	beq.s opforgeNativeCliOutputOk
	move.l #NativeOutputFailureText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #RETURN_FILE_FAILURE, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliOutputOk
	move.l #NativeOutputOkText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #RETURN_OK, NativeCliReturnCode
	bra.w opforgeNativeCliCloseDos

opforgeNativeCliEmitStub
	move.l #EmitterStubText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #RETURN_NOT_IMPLEMENTED, NativeCliReturnCode

opforgeNativeCliCloseDos
	move.l NativeCliDosBase, d0
	beq.s opforgeNativeCliDone
	movea.l SYS_BASE.W, a6
	movea.l d0, a1
	jsr CLOSE_LIBRARY(a6)
	clr.l NativeCliDosBase

opforgeNativeCliDone
	move.l NativeCliReturnCode, d0
	movem.l (sp)+, d2-d7/a2-a6
	rts

; Write a zero-terminated string through dos.library/PutStr.
opforgeNativeCliPutStr
	movea.l NativeCliDosBase, a6
	jsr PUT_STR(a6)
	rts

; Open an existing AmigaDOS input file.
opforgeNativeCliOpenInput
	move.l a0, d1
	move.l #MODE_OLDFILE, d2
	movea.l NativeCliDosBase, a6
	jsr OPEN(a6)
	rts

; Close an AmigaDOS file handle in D1.
opforgeNativeCliClose
	movea.l NativeCliDosBase, a6
	jsr CLOSE(a6)
	rts

; Read D0 bytes from file handle D1 into buffer A0.
opforgeNativeCliReadInput
	move.l a0, d2
	move.l d0, d3
	movea.l NativeCliDosBase, a6
	jsr READ(a6)
	rts

; Open or create an AmigaDOS output file.
opforgeNativeCliOpenOutput
	move.l a0, d1
	move.l #MODE_NEWFILE, d2
	movea.l NativeCliDosBase, a6
	jsr OPEN(a6)
	rts

; Write D0 bytes from buffer A0 to file handle D1.
opforgeNativeCliWriteOutput
	move.l a0, d2
	move.l d0, d3
	movea.l NativeCliDosBase, a6
	jsr WRITE(a6)
	rts

; Copy D0 bytes from A1 to A2.
opforgeNativeCliCopyBytes
	move.w d0, d2
	tst.w d2
	beq.s opforgeNativeCliCopyBytesDone

opforgeNativeCliCopyBytesLoop
	move.b (a1)+, (a2)+
	subq.w #1, d2
	bne.s opforgeNativeCliCopyBytesLoop

opforgeNativeCliCopyBytesDone
	rts

; Copy a C string from A0 to A1 and return copied byte count, including NUL.
opforgeNativeCliCopyCString
	moveq #0, d0

opforgeNativeCliCopyCStringLoop
	move.b (a0)+, d1
	move.b d1, (a1)+
	addq.w #1, d0
	tst.b d1
	bne.s opforgeNativeCliCopyCStringLoop
	rts

; Copy exactly D0 bytes from A0 to A1.
opforgeNativeCliCopyFixedString
	move.w d0, d2
	tst.w d2
	beq.s opforgeNativeCliCopyFixedStringDone

opforgeNativeCliCopyFixedStringLoop
	move.b (a0)+, (a1)+
	subq.w #1, d2
	bne.s opforgeNativeCliCopyFixedStringLoop

opforgeNativeCliCopyFixedStringDone
	rts

; Write a CB-relative input window offset/length pair into control block A0.
opforgeNativeCliWriteInputWindow
	move.b d0, CB_INPUT_PTR(a0)
	lsr.w #8, d0
	move.b d0, 17(a0)
	move.b d1, CB_INPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 19(a0)
	rts

; Write a CB-relative extension window offset/length pair into control block A0.
opforgeNativeCliWriteExtensionWindow
	move.b d0, CB_EXTENSION_PTR(a0)
	lsr.w #8, d0
	move.b d0, 25(a0)
	move.b d1, CB_EXTENSION_LEN(a0)
	lsr.w #8, d1
	move.b d1, 27(a0)
	rts

; Read the tkpkg service status byte from control block A0.
opforgeNativeCliReadStatus
	moveq #0, d0
	move.b CB_STATUS_CODE(a0), d0
	rts

; Read the tkpkg service output length from control block A0.
opforgeNativeCliReadOutputLen
	moveq #0, d0
	move.b CB_OUTPUT_LEN(a0), d0
	moveq #0, d1
	move.b 23(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	rts

; Read the tkpkg service last-error length from control block A0.
opforgeNativeCliReadLastErrorLen
	moveq #0, d0
	move.b CB_LAST_ERROR_LEN(a0), d0
	moveq #0, d1
	move.b 31(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	rts

; Initialize package state, tokenize every source line, and run parser routing.
opforgeNativeCliTokenizeFrontend
	movem.l d2-d7/a2-a6, -(sp)
	bsr.w opforgeNativeCliInitPackagePipeline
	tst.l d0
	bne.w opforgeNativeCliTokenizeReturn
	move.l #TokenizerOkText, d1
	bsr.w opforgeNativeCliPutStr
	bsr.w opforgeNativeCliTokenizeFile
	tst.l d0
	bne.w opforgeNativeCliTokenizeReturn

opforgeNativeCliTokenizeSuccess
	moveq #0, d0

opforgeNativeCliTokenizeReturn
	movem.l (sp)+, d2-d7/a2-a6
	rts

; Tokenize the primary input file path recorded by argument parsing.
opforgeNativeCliTokenizeFile
	lea NativeCliInputPath, a0
	lea NativeCliCurrentPath, a1
	bsr.w opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s opforgeNativeCliTokenizeFilePathFail
	lea NativeCliInputPath, a0
	bsr.w opforgeNativeCliTokenizeFileAtPath
	rts

opforgeNativeCliTokenizeFilePathFail
	moveq #1, d0
	rts

; Read and tokenize one AmigaDOS text file at A0, preserving logical line state.
opforgeNativeCliTokenizeFileAtPath
	bsr.w opforgeNativeCliOpenInput
	tst.l d0
	bne.s opforgeNativeCliTokenizeFileOpenOk
	moveq #1, d0
	rts

opforgeNativeCliTokenizeFileOpenOk
	move.l d0, d5
	move.l #1, NativeCliSourceLineNum
	clr.w NativeCliSourceLineLen
	clr.w NativeCliSawCr

opforgeNativeCliTokenizeFileReadLoop
	lea NativeCliInputChar, a0
	moveq #1, d0
	move.l d5, d1
	bsr.w opforgeNativeCliReadInput
	cmp.l #-1, d0
	beq.w opforgeNativeCliTokenizeFileFailClose
	tst.l d0
	beq.w opforgeNativeCliTokenizeFileEof

	move.b NativeCliInputChar, d0
	tst.w NativeCliSawCr
	beq.s opforgeNativeCliTokenizeFileCheckBreak
	clr.w NativeCliSawCr
	cmpi.b #10, d0
	beq.w opforgeNativeCliTokenizeFileReadLoop

opforgeNativeCliTokenizeFileCheckBreak
	cmpi.b #10, d0
	beq.s opforgeNativeCliTokenizeFileLineDone
	cmpi.b #13, d0
	beq.s opforgeNativeCliTokenizeFileCrDone

	move.w NativeCliSourceLineLen, d1
	cmpi.w #SOURCE_LINE_BUFFER_CAPACITY, d1
	bhs.w opforgeNativeCliTokenizeFileFailClose
	lea NativeCliSourceLine, a1
	move.b d0, 0(a1, d1.W)
	addq.w #1, d1
	move.w d1, NativeCliSourceLineLen
	bra.w opforgeNativeCliTokenizeFileReadLoop

opforgeNativeCliTokenizeFileCrDone
	move.w #1, NativeCliSawCr

opforgeNativeCliTokenizeFileLineDone
	bsr.w opforgeNativeCliTokenizeCurrentLine
	tst.l d0
	bne.s opforgeNativeCliTokenizeFileFailClose
	move.l NativeCliSourceLineNum, d0
	addq.l #1, d0
	move.l d0, NativeCliSourceLineNum
	clr.w NativeCliSourceLineLen
	bra.w opforgeNativeCliTokenizeFileReadLoop

opforgeNativeCliTokenizeFileEof
	tst.w NativeCliSourceLineLen
	beq.s opforgeNativeCliTokenizeFileCheckModuleDepth
	bsr.w opforgeNativeCliTokenizeCurrentLine
	tst.l d0
	bne.s opforgeNativeCliTokenizeFileFailClose

opforgeNativeCliTokenizeFileCheckModuleDepth
	tst.w NativeCliIncludeDepth
	bne.s opforgeNativeCliTokenizeFileSuccessClose
	tst.w NativeCliModuleResolveDepth
	bne.s opforgeNativeCliTokenizeFileSuccessClose
	tst.w NativeCliModuleDepth
	beq.s opforgeNativeCliTokenizeFileSuccessClose
	move.l #ModuleDepthFailureText, d1
	bsr.w opforgeNativeCliPutStr
	bra.s opforgeNativeCliTokenizeFileFailClose

opforgeNativeCliTokenizeFileSuccessClose
	move.l d5, d1
	bsr.w opforgeNativeCliClose
	moveq #0, d0
	rts

opforgeNativeCliTokenizeFileFailClose
	move.l d5, d1
	bsr.w opforgeNativeCliClose
	moveq #1, d0
	rts

; Initialize tkpkg, stage/load package bytes, and select the requested pipeline.
opforgeNativeCliInitPackagePipeline
	lea controlBlockV1, a0
	moveq #ENTRY_ORD_INIT, d0
	jsr tkpkgServiceDispatchV1
	bsr.w opforgeNativeCliReadStatus
	tst.b d0
	bne.w opforgeNativeCliInitPipelineFail

	bsr.w opforgeNativeCliStagePackage
	tst.l d0
	bne.w opforgeNativeCliInitPipelineFail

	lea controlBlockV1, a0
	move.w #PACKAGE_INPUT_PTR_V1, d0
	move.w NativeCliPackageLenActive, d1
	bsr.w opforgeNativeCliWriteInputWindow
	moveq #ENTRY_ORD_LOAD_PACKAGE, d0
	jsr tkpkgServiceDispatchV1
	bsr.w opforgeNativeCliReadStatus
	tst.b d0
	bne.s opforgeNativeCliInitPipelineFail

	bsr.w opforgeNativeCliPreparePipelineRequest
	tst.l d0
	bne.s opforgeNativeCliInitPipelineFail

	lea controlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliPipelineRequestLen, d1
	bsr.w opforgeNativeCliWriteInputWindow
	moveq #ENTRY_ORD_SET_PIPELINE, d0
	jsr tkpkgServiceDispatchV1
	bsr.w opforgeNativeCliReadStatus
	tst.b d0
	bne.s opforgeNativeCliInitPipelineFail
	moveq #0, d0
	rts

opforgeNativeCliInitPipelineFail
	moveq #1, d0
	rts

; Stage either the embedded package or an external --opasm-package file.
opforgeNativeCliStagePackage
	tst.b NativeCliPackagePath
	bne.s opforgeNativeCliStageExternalPackage

	lea opforgeNativeCliPackageData, a1
	lea packageStorage, a2
	move.w OpforgeNativeCliPackageLen, d0
	move.w d0, NativeCliPackageLenActive
	bsr.w opforgeNativeCliCopyBytes
	moveq #0, d0
	rts

opforgeNativeCliStageExternalPackage
	lea NativeCliPackagePath, a0
	bsr.w opforgeNativeCliOpenInput
	tst.l d0
	bne.s opforgeNativeCliStageExternalOpenOk
	moveq #1, d0
	rts

opforgeNativeCliStageExternalOpenOk
	move.l d0, d5
	lea packageStorage, a0
	move.l #PACKAGE_STORAGE_CAPACITY, d0
	move.l d5, d1
	bsr.w opforgeNativeCliReadInput
	move.l d0, d6
	cmp.l #-1, d6
	beq.w opforgeNativeCliStageExternalReadFail
	cmpi.l #PACKAGE_STORAGE_CAPACITY, d6
	bne.s opforgeNativeCliStageExternalReadOk
	lea NativeCliInputChar, a0
	moveq #1, d0
	move.l d5, d1
	bsr.w opforgeNativeCliReadInput
	move.l d0, d7
	cmp.l #-1, d7
	beq.w opforgeNativeCliStageExternalReadFail
	tst.l d7
	beq.s opforgeNativeCliStageExternalReadOk
	move.l d5, d1
	bsr.w opforgeNativeCliClose
	move.l #PackageTooLargeText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	rts

opforgeNativeCliStageExternalReadOk
	move.l d5, d1
	bsr.w opforgeNativeCliClose
	move.w d6, NativeCliPackageLenActive
	moveq #0, d0
	rts

opforgeNativeCliStageExternalReadFail
	move.l d5, d1
	bsr.w opforgeNativeCliClose
	moveq #1, d0
	rts

; Build the tkpkg set-pipeline request payload from --cpu or the default CPU.
opforgeNativeCliPreparePipelineRequest
	lea NativeCliCpuName, a0
	tst.b (a0)
	bne.s opforgeNativeCliPreparePipelineHaveCpu
	lea DefaultCpuName, a0

opforgeNativeCliPreparePipelineHaveCpu
	lea lastErrorBuffer, a1
	bsr.w opforgeNativeCliCopyCString
	move.w d0, NativeCliPipelineRequestLen
	moveq #0, d0
	rts

; Build the tokenizer request payload: u32 line number plus source bytes.
opforgeNativeCliPrepareLineServiceRequest
	lea lastErrorBuffer, a2
	move.l NativeCliSourceLineNum, d2  ; line number is little-endian to match package fixtures
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lea NativeCliSourceLine, a1
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliCopyBytes
	move.w NativeCliSourceLineLen, d1
	addq.w #4, d1
	move.w d1, NativeCliLineRequestLen
	moveq #0, d0
	rts

; Dispatch the current line through tkpkg ENTRY_ORD_PARSE_LINE.
opforgeNativeCliDispatchParseLineEnvelope
	bsr.w opforgeNativeCliPrepareParseLineServiceRequest
	tst.l d0
	bne.s opforgeNativeCliDispatchParseLineDone

opforgeNativeCliDispatchPreparedParseLineEnvelope
	bsr.w opforgeNativeCliWritePrvmRouteFrameInput
	tst.l d0
	bne.s opforgeNativeCliDispatchParseLineDone
	lea controlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliLineRequestLen, d1
	bsr.w opforgeNativeCliWriteInputWindow
	moveq #ENTRY_ORD_PARSE_LINE, d0
	jsr tkpkgServiceDispatchV1
	move.l d0, NativeCliPrvmRouteStatus
	move.w d1, NativeCliPrvmResultCount
	lea controlBlockV1, a0
	bsr.w opforgeNativeCliReadStatus

opforgeNativeCliDispatchParseLineDone
	rts

opforgeNativeCliDispatchParseLineUntilReady
	bsr.w opforgeNativeCliPrepareParseLineServiceRequest
	tst.l d0
	bne.s opforgeNativeCliDispatchParseLineUntilReadyDone

opforgeNativeCliDispatchParseLineUntilReadyLoop
	bsr.w opforgeNativeCliDispatchPreparedParseLineEnvelope
	tst.l d0
	bne.s opforgeNativeCliDispatchParseLineUntilReadyDone
	cmpi.l #PRVM_STATUS_EXPR_REQUEST, NativeCliPrvmRouteStatus
	bne.s opforgeNativeCliDispatchParseLineUntilReadyDone
	bsr.w opforgeNativeCliServicePrvmExpressionRequest
	tst.l d0
	bne.s opforgeNativeCliDispatchParseLineUntilReadyDone
	bra.s opforgeNativeCliDispatchParseLineUntilReadyLoop

opforgeNativeCliDispatchParseLineUntilReadyDone
	rts

; Copy the PRVM route frame into the tkpkg control-block input window.
opforgeNativeCliPrepareParseLineServiceRequest
	bsr.w opforgeNativeCliBuildPrvmRouteFrame
	tst.l d0
	bne.s opforgeNativeCliPrepareParseLineServiceRequestDone

opforgeNativeCliWritePrvmRouteFrameInput
	lea OpforgeNativeCliPrvmRouteFrame, a1
	lea lastErrorBuffer, a2
	move.w #PRVM_ROUTE_FRAME_SIZE, d0
	bsr.w opforgeNativeCliCopyBytes
	move.w #PRVM_ROUTE_FRAME_SIZE, NativeCliLineRequestLen
	moveq #0, d0

opforgeNativeCliPrepareParseLineServiceRequestDone
	rts

; Build the minimal encode request envelope used by the early tkpkg encoder.
opforgeNativeCliPrepareEncodeInstructionRequest
	lea lastErrorBuffer, a2
	move.l NativeCliStmtMnemLen, d0
	cmpi.l #255, d0
	bhi.s opforgeNativeCliPrepareEncodeFail
	move.b d0, (a2)+
	tst.l d0
	beq.s opforgeNativeCliPrepareEncodeCandidateCount
	movea.l NativeCliStmtMnemStart, a1
	bsr.w opforgeNativeCliCopyBytes

opforgeNativeCliPrepareEncodeCandidateCount
	clr.b (a2)+
	addq.w #2, d0
	move.w d0, NativeCliEncodeRequestLen
	moveq #0, d0
	rts

opforgeNativeCliPrepareEncodeFail
	moveq #1, d0
	rts

; Build the shared line/span request envelope for tkpkg encode_selected_instruction.
;
; Inputs:
; - D6: statement index.
;
; Outputs:
; - D0: 0 on success, 1 on malformed local request state.
; - nativeCliEvalRequestLen: request byte length.
opforgeNativeCliPrepareEncodeSelectedRequestForStatement
	movem.l d1-d7/a0-a2, -(sp)
	move.w d6, d7
	moveq #0, d0
	move.w d7, d0
	lsl.l #6, d0
	lea opasmEngineStmtMnemNameTable.l, a0
	adda.l d0, a0
	movea.l a0, a2
	moveq #0, d0
	move.w d7, d0
	add.w d0, d0
	lea opasmEngineStmtMnemLenTable.l, a1
	moveq #0, d6
	move.w 0(a1, d0.l), d6
	bne.w opforgeNativeCliPrepareEncodeSelectedHaveMnemLen
	movea.l a2, a0
	bsr.w opforgeNativeCliTokenLen
	move.w d0, d6

opforgeNativeCliPrepareEncodeSelectedHaveMnemLen
	tst.w d6
	beq.w opforgeNativeCliPrepareEncodeSelectedFail
	move.l a2, NativeCliStmtMnemStart
	move.l d6, NativeCliStmtMnemLen
	moveq #0, d0
	move.w d7, d0
	add.w d0, d0
	lea opasmEngineStmtOperandLenTable.l, a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	moveq #0, d0
	move.w d7, d0
	lsl.l #6, d0
	lea opasmEngineStmtOperandNameTable.l, a0
	adda.l d0, a0
	move.l d1, d0

opforgeNativeCliPrepareEncodeSelectedBuildRequest
	move.l a0, d3
	move.l d1, d4
	bsr.w opforgeNativeCliLoadStatementExprMetadata
	tst.w NativeCliStmtExprFound
	bne.w opforgeNativeCliPrepareEncodeSelectedMaybeSourceLineRequest

opforgeNativeCliPrepareEncodeSelectedSyntheticRequest
	bsr.w opforgeNativeCliClearStatementExprSpanForSyntheticRequest
	movea.l d3, a0
	move.l d4, d0
	bsr.w opforgeNativeCliPrepareEvaluateExpressionRequest
	bra.w opforgeNativeCliPrepareEncodeSelectedReturn

opforgeNativeCliPrepareEncodeSelectedMaybeSourceLineRequest
	tst.l d4
	bne.w opforgeNativeCliPrepareEncodeSelectedSyntheticRequest
	move.l NativeCliStmtExprSpanStart, d2
	move.l NativeCliStmtExprSpanEnd, d3
	cmp.l d2, d3
	bls.w opforgeNativeCliPrepareEncodeSelectedSyntheticRequest

opforgeNativeCliPrepareEncodeSelectedSourceLineRequest
	bsr.w opforgeNativeCliLoadStatementSourceLineText
	tst.l d0
	beq.w opforgeNativeCliPrepareEncodeSelectedSyntheticRequest
	move.l d0, d1
	move.l d2, d0
	subq.l #1, d0
	cmp.l d1, d0
	bhs.w opforgeNativeCliPrepareEncodeSelectedSyntheticRequest
	move.l d3, d0
	subq.l #1, d0
	cmp.l d1, d0
	bhi.w opforgeNativeCliPrepareEncodeSelectedSyntheticRequest
	move.l d1, d0
	bsr.w opforgeNativeCliPrepareEvaluateExpressionRequest
	bra.w opforgeNativeCliPrepareEncodeSelectedReturn

opforgeNativeCliPrepareEncodeSelectedFail
	moveq #1, d0

opforgeNativeCliPrepareEncodeSelectedReturn
	movem.l (sp)+, d1-d7/a0-a2
	rts

; Build the tkpkg evaluate-expression envelope from the active statement text.
;
; Inputs:
; - A0/D0: source-line text pointer and byte length, or a fallback operand slice
;   when no persisted expression span is available.
; - D7: statement index.
;
; Outputs:
; - D0: 0 on success, 1 on malformed local request state.
; - nativeCliEvalRequestLen: request byte length.
opforgeNativeCliPrepareEvaluateExpressionRequest
	movem.l d1-d7/a1-a2, -(sp)
	movea.l a0, a2
	move.l d0, d6
	lea lastErrorBuffer, a1
	move.l NativeCliStmtExprSpanLine, d2
	tst.l d2
	bne.s opforgeNativeCliPrepareEvalHaveLineNum
	moveq #0, d0
	move.w d7, d0
	lsl.l #2, d0
	lea opasmEngineStmtLineTable.l, a0
	move.l 0(a0, d0.l), d2

opforgeNativeCliPrepareEvalHaveLineNum
	move.l d2, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	lsr.l #8, d3
	move.b d3, (a1)+
	tst.w NativeCliStmtExprFound
	beq.s opforgeNativeCliPrepareEvalSyntheticSpan
	move.l NativeCliStmtExprSpanStart, d2
	move.l NativeCliStmtExprSpanEnd, d3
	bra.s opforgeNativeCliPrepareEvalWriteSpan

opforgeNativeCliPrepareEvalSyntheticSpan
	tst.l d6
	bne.s opforgeNativeCliPrepareEvalSyntheticNonEmptySpan
	clr.l d2
	clr.l d3
	bra.s opforgeNativeCliPrepareEvalWriteSpan

opforgeNativeCliPrepareEvalSyntheticNonEmptySpan
	moveq #1, d2
	move.l d6, d3
	addq.l #1, d3

opforgeNativeCliPrepareEvalWriteSpan
	move.w d2, d4
	move.b d4, (a1)+
	lsr.w #8, d4
	move.b d4, (a1)+
	move.w d3, d4
	move.b d4, (a1)+
	lsr.w #8, d4
	move.b d4, (a1)+
	move.l NativeCliStmtMnemLen, d5
	cmpi.l #255, d5
	bhi.w opforgeNativeCliPrepareEvalFail
	move.b d5, (a1)+
	tst.l d5
	beq.s opforgeNativeCliPrepareEvalCopyOperand
	movea.l NativeCliStmtMnemStart, a0
	move.w d5, d0
	bsr.w opforgeNativeCliCopyFixedString

opforgeNativeCliPrepareEvalCopyOperand
	movea.l a2, a0
	move.w d6, d0
	bsr.w opforgeNativeCliCopyFixedString
	move.w d6, d0
	add.w d5, d0
	addi.w #9, d0
	move.w d0, NativeCliEvalRequestLen
	moveq #0, d0
	bra.s opforgeNativeCliPrepareEvalReturn

opforgeNativeCliPrepareEvalFail
	moveq #1, d0

opforgeNativeCliPrepareEvalReturn
	movem.l (sp)+, d1-d7/a1-a2
	rts

; Write optional label/PC context for tkpkg evaluate-expression requests.
opforgeNativeCliPrepareEvaluateExpressionExtension
	movem.l d1-d7/a0-a2, -(sp)
	lea ControlBlockV1, a1
	adda.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, a1
	move.l #opasmEngineLabelNameTable, (a1)+
	move.l #opasmEngineLabelValueTable, (a1)+
	moveq #0, d0
	move.w opasmEngineLabelCount.l, d0
	move.l d0, (a1)+
	move.l opasmEngineSessionCurrentPc.l, (a1)+
	clr.l (a1)
	clr.l 4(a1)
	bsr.w opforgeNativeCliInferSelectedShapeForEvalRequest
	tst.w d0
	beq.s opforgeNativeCliPrepareEvaluateExpressionExtensionDone
	move.l a0, (a1)
	move.l d0, 4(a1)

opforgeNativeCliPrepareEvaluateExpressionExtensionDone
	moveq #0, d0
	movem.l (sp)+, d1-d7/a0-a2
	rts

opforgeNativeCliInferSelectedShapeForEvalRequest
	movem.l d1-d7/a1-a2, -(sp)
	lea lastErrorBuffer, a0
	moveq #0, d0
	move.b 8(a0), d0
	movea.l a0, a2
	bsr.w opforgeNativeCliInferSelectedShapeBranchMnemonic
	tst.l d0
	bne.w opforgeNativeCliInferSelectedShapeDirect
	movea.l a2, a0
	moveq #0, d0
	move.b 8(a0), d0
	moveq #0, d2
	move.w NativeCliEvalRequestLen.l, d2
	subi.w #9, d2
	bcs.w opforgeNativeCliInferSelectedShapeNone
	sub.w d0, d2
	bcs.w opforgeNativeCliInferSelectedShapeNone
	lea 9(a0, d0.w), a0

opforgeNativeCliInferSelectedShapeTrimLeading
	tst.w d2
	beq.w opforgeNativeCliInferSelectedShapeNone
	move.b (a0), d3
	cmpi.b #' ', d3
	beq.s opforgeNativeCliInferSelectedShapeTrimLeadingOne
	cmpi.b #9, d3
	bne.s opforgeNativeCliInferSelectedShapeTrimTrailing

opforgeNativeCliInferSelectedShapeTrimLeadingOne
	addq.l #1, a0
	subq.w #1, d2
	bra.s opforgeNativeCliInferSelectedShapeTrimLeading

opforgeNativeCliInferSelectedShapeTrimTrailing
	tst.w d2
	beq.w opforgeNativeCliInferSelectedShapeNone
	move.w d2, d4
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #' ', d3
	beq.s opforgeNativeCliInferSelectedShapeTrimTrailingOne
	cmpi.b #9, d3
	bne.s opforgeNativeCliInferSelectedShapeReady

opforgeNativeCliInferSelectedShapeTrimTrailingOne
	subq.w #1, d2
	bra.s opforgeNativeCliInferSelectedShapeTrimTrailing

opforgeNativeCliInferSelectedShapeReady
	cmpi.w #1, d2
	bne.s opforgeNativeCliInferSelectedShapeCheckPrefix
	move.b (a0), d3
	ori.b #$20, d3
	cmpi.b #'a', d3
	beq.w opforgeNativeCliInferSelectedShapeAccumulator

opforgeNativeCliInferSelectedShapeCheckPrefix
	move.b (a0), d3
	cmpi.b #'#', d3
	beq.w opforgeNativeCliInferSelectedShapeImmediate
	cmpi.b #'(', d3
	beq.w opforgeNativeCliInferSelectedShapeParen
	bsr.w opforgeNativeCliInferSelectedShapeSuffix
	cmpi.b #'x', d0
	beq.w opforgeNativeCliInferSelectedShapeDirectX
	cmpi.b #'y', d0
	beq.w opforgeNativeCliInferSelectedShapeDirectY
	bra.w opforgeNativeCliInferSelectedShapeDirect

opforgeNativeCliInferSelectedShapeParen
	bsr.w opforgeNativeCliInferSelectedShapeSuffix
	cmpi.b #'y', d0
	beq.w opforgeNativeCliInferSelectedShapeIndirectIndexedY
	move.w d2, d4
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #')', d3
	bne.w opforgeNativeCliInferSelectedShapeIndirect
	cmpi.w #4, d2
	bcs.w opforgeNativeCliInferSelectedShapeIndirect
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	ori.b #$20, d3
	cmpi.b #'x', d3
	bne.w opforgeNativeCliInferSelectedShapeIndirect
	tst.w d4
	beq.w opforgeNativeCliInferSelectedShapeIndirect
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #',', d3
	beq.w opforgeNativeCliInferSelectedShapeIndexedIndirectX
	bra.w opforgeNativeCliInferSelectedShapeIndirect

opforgeNativeCliInferSelectedShapeSuffix
	moveq #0, d0
	cmpi.w #3, d2
	bcs.s opforgeNativeCliInferSelectedShapeSuffixReturn
	move.w d2, d4
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	ori.b #$20, d3
	cmpi.b #'x', d3
	beq.s opforgeNativeCliInferSelectedShapeSuffixMaybe
	cmpi.b #'y', d3
	bne.s opforgeNativeCliInferSelectedShapeSuffixReturn

opforgeNativeCliInferSelectedShapeSuffixMaybe
	move.b d3, d0
	subq.w #1, d4
	move.b 0(a0, d4.w), d3
	cmpi.b #',', d3
	beq.s opforgeNativeCliInferSelectedShapeSuffixReturn
	moveq #0, d0

opforgeNativeCliInferSelectedShapeSuffixReturn
	rts

opforgeNativeCliInferSelectedShapeBranchMnemonic
	cmpi.w #3, d0
	beq.s opforgeNativeCliInferSelectedShapeBranchLenOk
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchLenOk
	lea 9(a2), a1
	move.b (a1)+, d1
	ori.b #$20, d1
	cmpi.b #'b', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchHaveB
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchHaveB
	move.b (a1)+, d1
	move.b (a1), d2
	ori.b #$20, d1
	ori.b #$20, d2
	cmpi.b #'c', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckC
	cmpi.b #'e', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckEq
	cmpi.b #'n', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckNe
	cmpi.b #'m', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckMi
	cmpi.b #'p', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckPl
	cmpi.b #'v', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckV
	cmpi.b #'r', d1
	beq.s opforgeNativeCliInferSelectedShapeBranchCheckRa
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckC
	cmpi.b #'c', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	cmpi.b #'s', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckEq
	cmpi.b #'q', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckNe
	cmpi.b #'e', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckMi
	cmpi.b #'i', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckPl
	cmpi.b #'l', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckV
	cmpi.b #'c', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	cmpi.b #'s', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchCheckRa
	cmpi.b #'a', d2
	beq.s opforgeNativeCliInferSelectedShapeBranchYes
	moveq #0, d0
	rts

opforgeNativeCliInferSelectedShapeBranchYes
	moveq #1, d0
	rts

opforgeNativeCliInferSelectedShapeAccumulator
	lea NativeCliSelectedShapeAccumulatorText, a0
	moveq #11, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeImmediate
	lea NativeCliSelectedShapeImmediateText, a0
	moveq #9, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeDirect
	lea NativeCliSelectedShapeDirectText, a0
	moveq #6, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeDirectX
	lea NativeCliSelectedShapeDirectXText, a0
	moveq #8, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeDirectY
	lea NativeCliSelectedShapeDirectYText, a0
	moveq #8, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeIndirect
	lea NativeCliSelectedShapeIndirectText, a0
	moveq #8, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeIndexedIndirectX
	lea NativeCliSelectedShapeIndexedIndirectXText, a0
	moveq #18, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeIndirectIndexedY
	lea NativeCliSelectedShapeIndirectIndexedYText, a0
	moveq #18, d0
	bra.s opforgeNativeCliInferSelectedShapeReturn

opforgeNativeCliInferSelectedShapeNone
	moveq #0, d0

opforgeNativeCliInferSelectedShapeReturn
	movem.l (sp)+, d1-d7/a1-a2
	rts

; Read the signed 32-bit result written by tkpkg evaluate-expression.
opforgeNativeCliReadEvaluateExpressionValue
	lea ControlBlockV1, a0
	adda.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, a0
	move.l 16(a0), d3
	rts

opforgeNativeCliClearStatementExprSpanForSyntheticRequest
	clr.w NativeCliStmtExprFound
	clr.l NativeCliStmtExprSpanLine
	clr.l NativeCliStmtExprSpanStart
	clr.l NativeCliStmtExprSpanEnd
	rts

; Dispatch the current encode envelope through tkpkg ENTRY_ORD_ENCODE_INSTRUCTION.
opforgeNativeCliDispatchEncodeInstructionEnvelope
	bsr.w opforgeNativeCliPrepareEncodeInstructionRequest
	tst.l d0
	bne.s opforgeNativeCliDispatchEncodeDone
	lea controlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliEncodeRequestLen, d1
	bsr.w opforgeNativeCliWriteInputWindow
	moveq #ENTRY_ORD_ENCODE_INSTRUCTION, d0
	jsr tkpkgServiceDispatchV1
	bsr.w opforgeNativeCliReadStatus

opforgeNativeCliDispatchEncodeDone
	rts

opforgeNativeCliTokenizeCurrentLine
	tst.w NativeCliIncludeDepth
	beq.s opforgeNativeCliTokenizeCurrentLineNoIncludeRecord
	bsr.w opforgeNativeCliEmitIncludeLineRecord

opforgeNativeCliTokenizeCurrentLineNoIncludeRecord
	bsr.w opforgeNativeCliRecordSourceLine
	bsr.w opforgeNativeCliPrepareLineServiceRequest
	tst.l d0
	bne.s opforgeNativeCliTokenizeCurrentLineFail

	lea controlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliLineRequestLen, d1
	bsr.w opforgeNativeCliWriteInputWindow
	moveq #ENTRY_ORD_TOKENIZE_LINE, d0
	jsr tkpkgServiceDispatchV1
	bsr.w opforgeNativeCliReadStatus
	tst.b d0
	bne.s opforgeNativeCliTokenizeCurrentLineFail
	lea controlBlockV1, a0
	bsr.w opforgeNativeCliReadOutputLen
	tst.w d0
	beq.s opforgeNativeCliTokenizeCurrentLineOk
	lea lastErrorBuffer, a1
	clr.b 0(a1, d0.w)
	move.l #lastErrorBuffer, d1
	bsr.w opforgeNativeCliPutStr

opforgeNativeCliTokenizeCurrentLineOk
	bsr.w opforgeNativeCliParseCurrentLine
	tst.l d0
	bne.s opforgeNativeCliTokenizeCurrentLineFail
	moveq #0, d0
	rts

opforgeNativeCliTokenizeCurrentLineFail
	lea controlBlockV1, a0
	bsr.w opforgeNativeCliReadOutputLen
	tst.w d0
	beq.s opforgeNativeCliTokenizeCurrentLineFailReturn
	lea lastErrorBuffer, a1
	clr.b 0(a1, d0.W)
	move.l #lastErrorBuffer, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr

opforgeNativeCliTokenizeCurrentLineFailReturn
	moveq #1, d0
	rts

opforgeNativeCliEmitIncludeLineRecord
	movem.l d0-d1, -(sp)
	move.l #IncludeLineText, d1
	bsr.w opforgeNativeCliPutStr
	move.w NativeCliIncludeDepth, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #SpaceText, d1
	bsr.w opforgeNativeCliPutStr
	move.l NativeCliSourceLineNum, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #SpaceText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliCurrentPath, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d0-d1
	rts

opforgeNativeCliParseCurrentLine
	movem.l d2-d7/a2-a4, -(sp)
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w opforgeNativeCliParseLineDone
	movea.l a0, a4
	move.l d0, d7

	lea IfdefDirectiveText, a1
	moveq #6, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliParseConditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea IfndefDirectiveText, a1
	moveq #7, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliParseConditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea ElseifDirectiveText, a1
	moveq #7, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliParseConditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea ElseDirectiveText, a1
	moveq #5, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliParseConditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea EndifDirectiveText, a1
	moveq #6, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliParseConditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea IfDirectiveText, a1
	moveq #3, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliParseConditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea IncludeDirectiveText, a1
	moveq #8, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliParseIncludeLine

	movea.l a4, a0
	move.l d7, d0
	lea OrgMnemonicText, a1
	moveq #4, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliParseBadOrgLine

	bsr.w opforgeNativeCliRouteParserModuleUseLine
	cmpi.w #NCLI_PARSER_DIRECTIVE_MODULE, d0
	beq.w opforgeNativeCliParseModuleLine
	cmpi.w #NCLI_PARSER_DIRECTIVE_ENDMODULE, d0
	beq.w opforgeNativeCliParseEndmoduleLine
	cmpi.w #NCLI_PARSER_DIRECTIVE_USE, d0
	beq.w opforgeNativeCliParseUseLine
	bsr.w opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w opforgeNativeCliParseLineFail

opforgeNativeCliParseLineDone
	moveq #0, d0
	bra.w opforgeNativeCliParseLineReturn

opforgeNativeCliParseConditionalLine
	move.l #ConditionalFailureText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	bra.w opforgeNativeCliParseLineReturn

opforgeNativeCliParseBadOrgLine
	move.l #NativeBadOrgText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	bra.w opforgeNativeCliParseLineReturn

opforgeNativeCliRouteParserModuleUseLine
	movem.l d1-d7/a0-a3, -(sp)
	clr.l NativeCliPrvmRouteStatus
	clr.w NativeCliPrvmResultCount
	bsr.w opforgeNativeCliDispatchParseLineUntilReady
	bsr.w opforgeNativeCliParserDirectiveKind
	movem.l (sp)+, d1-d7/a0-a3
	rts

opforgeNativeCliServicePrvmExpressionRequest
	movem.l d1-d4/a0-a2, -(sp)
	lea OpforgeNativeCliPrvmExprRequest, a0
	lea OpforgeNativeCliPrvmExprResultSlot, a1
	move.w 0(a0), d0
	cmpi.w #1, d0
	bne.s opforgeNativeCliServicePrvmExpressionRequestFail
	tst.w 2(a0)
	bne.s opforgeNativeCliServicePrvmExpressionRequestFail
	clr.l d0
	move.l 8(a0), d0
	cmpi.l #PRVM_ROUTE_EXPR_RESULT_CAPACITY, d0
	bhs.s opforgeNativeCliServicePrvmExpressionRequestFail
	move.l d0, d3
	lsl.l #5, d3
	lea OpforgeNativeCliPrvmExprResultSlot, a1
	adda.l d3, a1
	move.w #PRVM_EXPR_SLOT_READY, 0(a1)
	clr.w 2(a1)
	move.l d0, 4(a1)
	move.l 20(a0), 8(a1)
	move.l 24(a0), 12(a1)
	move.l 28(a0), 16(a1)
	move.l d0, 20(a1)
	move.l #$FFFFFFFF, 24(a1)
	clr.l 28(a1)
	lea OpforgeNativeCliPrvmRouteFrame, a2
	move.l #OpforgeNativeCliPrvmExprResultSlot, 96(a2)
	addq.l #1, d0
	move.l d0, 100(a2)
	moveq #0, d0
	bra.s opforgeNativeCliServicePrvmExpressionRequestReturn

opforgeNativeCliServicePrvmExpressionRequestFail
	moveq #1, d0

opforgeNativeCliServicePrvmExpressionRequestReturn
	movem.l (sp)+, d1-d4/a0-a2
	rts

opforgeNativeCliBuildPrvmRouteFrame
	lea OpforgeNativeCliPrvmRouteFrame, a0
	move.l #PRVM_ROUTE_MAGIC_OPLR, 0(a0)
	move.w #PRVM_ROUTE_ABI_VERSION_V1, 4(a0)
	move.w #PRVM_ROUTE_FRAME_SIZE, 6(a0)
	lea ProcessorAsmText, a1
	move.l a1, 8(a0)
	move.l #3, 12(a0)
	lea KindStatementText, a1
	move.l a1, 16(a0)
	move.l #9, 20(a0)
	move.l NativeCliSourceLineNum, 24(a0)
	lea NativeCliSourceLine, a1
	move.l a1, 28(a0)
	clr.l d0
	move.w NativeCliSourceLineLen, d0
	move.l d0, 32(a0)
	lea tokenRecordBuffer, a1
	move.l a1, 36(a0)
	clr.l d0
	move.w lastTokenCount, d0
	move.l d0, 40(a0)
	move.w #TOKEN_RECORD_SIZE, 44(a0)
	clr.w 46(a0)
	lea tokenScratchBuffer, a1
	move.l a1, 48(a0)
	clr.l d0
	move.w lastLexemeLen, d0
	move.l d0, 52(a0)
	bsr.w opforgeNativeCliLoadActivePrvmProgram
	tst.l d0
	bne.w opforgeNativeCliBuildPrvmRouteFrameDone
	lea OpforgeNativeCliPrvmResultBuffer, a1
	movea.l a1, a0
	move.l #PRVM_ROUTE_RESULT_CAPACITY, d0
	bsr.w opforgeNativeCliClearBytes
	lea OpforgeNativeCliPrvmRouteFrame, a0
	lea OpforgeNativeCliPrvmResultBuffer, a1
	move.l a1, 64(a0)
	move.l #PRVM_ROUTE_RESULT_CAPACITY, 68(a0)
	lea OpforgeNativeCliPrvmDiagBuffer, a1
	move.l a1, 72(a0)
	move.l #PRVM_ROUTE_DIAG_CAPACITY, 76(a0)
	lea OpforgeNativeCliPrvmResumeBuffer, a1
	move.l a1, 80(a0)
	move.l #PRVM_ROUTE_RESUME_CAPACITY, 84(a0)
	lea OpforgeNativeCliPrvmExprRequest, a1
	move.l a1, 88(a0)
	move.l #PRVM_ROUTE_EXPR_REQUEST_SIZE, 92(a0)
	clr.l 96(a0)
	move.l #PRVM_ROUTE_EXPR_RESULT_COUNT, 100(a0)
	move.l #PRVM_PARSER_CONTRACT_VERSION_V2, 104(a0)
	move.l #PRVM_ROUTE_STEP_BUDGET, 108(a0)
	clr.l 112(a0)
	moveq #0, d0

opforgeNativeCliBuildPrvmRouteFrameDone
	rts

opforgeNativeCliLoadActivePrvmProgram
	movem.l d1-d4/a1-a4, -(sp)
	movea.l a0, a4
	lea ActiveParserVmOffsetLo, a1
	moveq #0, d0
	move.b (a1)+, d0
	moveq #0, d1
	move.b (a1)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d2
	move.b (a1)+, d2
	moveq #0, d1
	move.b (a1)+, d1
	lsl.w #8, d1
	or.w d1, d2
	tst.w d2
	beq.w opforgeNativeCliLoadActivePrvmProgramFail
	lea packageStorage, a2
	lea 0(a2, d0.W), a2
	movea.l a2, a3
	adda.l d2, a3
	moveq #1, d0
	bsr.w opforgeNativeCliActivePrvmRequireBytes
	tst.l d0
	bne.s opforgeNativeCliLoadActivePrvmProgramFail
	addq.w #1, a2
	bsr.w opforgeNativeCliActivePrvmReadU32
	tst.l d1
	bne.s opforgeNativeCliLoadActivePrvmProgramFail
	move.l d0, d3
	bsr.w opforgeNativeCliActivePrvmRequireBytes
	tst.l d0
	bne.s opforgeNativeCliLoadActivePrvmProgramFail
	adda.l d3, a2
	moveq #2, d0
	bsr.w opforgeNativeCliActivePrvmRequireBytes
	tst.l d0
	bne.s opforgeNativeCliLoadActivePrvmProgramFail
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.w #8, d1
	or.w d1, d0
	cmpi.w #PRVM_PARSER_CONTRACT_VERSION_V2, d0
	bne.s opforgeNativeCliLoadActivePrvmProgramFail
	bsr.w opforgeNativeCliActivePrvmReadU32
	tst.l d1
	bne.s opforgeNativeCliLoadActivePrvmProgramFail
	tst.l d0
	beq.s opforgeNativeCliLoadActivePrvmProgramFail
	move.l d0, d3
	bsr.w opforgeNativeCliActivePrvmRequireBytes
	tst.l d0
	bne.s opforgeNativeCliLoadActivePrvmProgramFail
	move.l a2, 56(a4)
	move.l d3, 60(a4)
	moveq #0, d0
	bra.s opforgeNativeCliLoadActivePrvmProgramReturn

opforgeNativeCliLoadActivePrvmProgramFail
	moveq #1, d0

opforgeNativeCliLoadActivePrvmProgramReturn
	movem.l (sp)+, d1-d4/a1-a4
	rts

opforgeNativeCliActivePrvmReadU32
	moveq #4, d0
	bsr.w opforgeNativeCliActivePrvmRequireBytes
	tst.l d0
	bne.s opforgeNativeCliActivePrvmReadU32Fail
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	rts

opforgeNativeCliActivePrvmReadU32Fail
	moveq #1, d1
	rts

opforgeNativeCliActivePrvmRequireBytes
	movea.l a2, a1
	adda.l d0, a1
	cmpa.l a3, a1
	bhi.s opforgeNativeCliActivePrvmRequireBytesFail
	moveq #0, d0
	rts

opforgeNativeCliActivePrvmRequireBytesFail
	moveq #1, d0
	rts

opforgeNativeCliParserDirectiveKind
	lea OpforgeNativeCliPrvmResultBuffer, a2
	cmpi.w #PRVM_RESULT_MNEMONIC_TEXT, 32(a2)
	beq.s opforgeNativeCliParserDirectiveKindHaveText
	cmpi.w #PRVM_RESULT_DIRECTIVE_TEXT, 32(a2)
	bne.w opforgeNativeCliParserDirectiveKindFallback
opforgeNativeCliParserDirectiveKindHaveText
	move.l 48(a2), d0
	lea tokenScratchBuffer, a0
	adda.l d0, a0
	move.l 52(a2), d0
	lea ModuleMnemonicText, a1
	moveq #6, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.w opforgeNativeCliParserDirectiveModule
	move.l 48(a2), d0
	lea tokenScratchBuffer, a0
	adda.l d0, a0
	move.l 52(a2), d0
	lea EndmoduleMnemonicText, a1
	moveq #9, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.w opforgeNativeCliParserDirectiveEndmodule
	move.l 48(a2), d0
	lea tokenScratchBuffer, a0
	adda.l d0, a0
	move.l 52(a2), d0
	lea UseMnemonicText, a1
	moveq #3, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.w opforgeNativeCliParserDirectiveUse

opforgeNativeCliParserDirectiveKindFallback
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea ModuleDirectiveText, a1
	moveq #7, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.s opforgeNativeCliParserDirectiveModule
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea EndmoduleDirectiveText, a1
	moveq #10, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.s opforgeNativeCliParserDirectiveEndmodule
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea UseDirectiveText, a1
	moveq #4, d1
	bsr.w opforgeNativeCliParserMnemonicEquals
	tst.l d0
	bne.s opforgeNativeCliParserDirectiveUse
	moveq #NCLI_PARSER_DIRECTIVE_NONE, d0
	rts

opforgeNativeCliParserDirectiveModule
	moveq #NCLI_PARSER_DIRECTIVE_MODULE, d0
	rts

opforgeNativeCliParserDirectiveEndmodule
	moveq #NCLI_PARSER_DIRECTIVE_ENDMODULE, d0
	rts

opforgeNativeCliParserDirectiveUse
	moveq #NCLI_PARSER_DIRECTIVE_USE, d0
	rts

opforgeNativeCliParserMnemonicEquals
	bsr.w opforgeNativeCliLineStartsWith
	rts

opforgeNativeCliRecordPrvmStatementLine
	movem.l d1-d7/a0-a2, -(sp)
	tst.l NativeCliPrvmRouteStatus
	beq.s opforgeNativeCliRecordPrvmStatementRouteOk
	cmpi.l #PRVM_STATUS_EXPR_REQUEST, NativeCliPrvmRouteStatus
	bne.w opforgeNativeCliRecordPrvmStatementSourceOnly

opforgeNativeCliRecordPrvmStatementRouteOk
	clr.l NativeCliStmtLabelStart
	clr.l NativeCliStmtLabelEnd
	clr.l NativeCliStmtLabelOff
	clr.l NativeCliStmtLabelLen
	clr.l NativeCliStmtMnemStart
	clr.l NativeCliStmtMnemEnd
	clr.l NativeCliStmtMnemOff
	clr.l NativeCliStmtMnemLen
	clr.l NativeCliStmtOperandStart
	clr.l NativeCliStmtOperandEnd
	clr.l NativeCliStmtExprOperandIndex
	clr.l NativeCliStmtExprSlotIndex
	clr.l NativeCliStmtExprStartToken
	clr.l NativeCliStmtExprEndToken
	clr.l NativeCliStmtExprSpanLine
	clr.l NativeCliStmtExprSpanStart
	clr.l NativeCliStmtExprSpanEnd
	clr.w NativeCliStmtMnemFound
	clr.w NativeCliStmtExprFound
	moveq #NCLI_PARSER_DIRECTIVE_NONE, d0
	move.w d0, NativeCliStmtDirectiveKind
	move.w NativeCliPrvmResultCount, d7
	cmpi.l #PRVM_STATUS_EXPR_REQUEST, NativeCliPrvmRouteStatus
	bne.s opforgeNativeCliRecordPrvmStatementHaveCount
	move.w #PRVM_RESULT_RECORD_COUNT, d7

opforgeNativeCliRecordPrvmStatementHaveCount
	beq.w opforgeNativeCliRecordPrvmStatementDone
	subq.w #1, d7
	lea OpforgeNativeCliPrvmResultBuffer, a2

opforgeNativeCliRecordPrvmStatementScan
	tst.w 0(a2)
	beq.w opforgeNativeCliRecordPrvmStatementFinalize
	cmpi.w #PRVM_RESULT_LABEL_TEXT, 0(a2)
	beq.w opforgeNativeCliRecordPrvmStatementHaveLabel
	cmpi.w #PRVM_RESULT_MNEMONIC_TEXT, 0(a2)
	beq.w opforgeNativeCliRecordPrvmStatementHaveMnemonic
	cmpi.w #PRVM_RESULT_DIRECTIVE_TEXT, 0(a2)
	beq.w opforgeNativeCliRecordPrvmStatementHaveDirective
	cmpi.w #PRVM_RESULT_OPERAND_TEXT, 0(a2)
	beq.w opforgeNativeCliRecordPrvmStatementHaveOperandText
	cmpi.w #PRVM_RESULT_OPERAND_EXPR_SLOT, 0(a2)
	beq.w opforgeNativeCliRecordPrvmStatementHaveOperandExpr

opforgeNativeCliRecordPrvmStatementNext
	adda.l #PRVM_RESULT_RECORD_SIZE, a2
	dbra d7, opforgeNativeCliRecordPrvmStatementScan

opforgeNativeCliRecordPrvmStatementFinalize
	cmpi.l #PRVM_STATUS_EXPR_REQUEST, NativeCliPrvmRouteStatus
	bne.s opforgeNativeCliRecordPrvmStatementCheckMnemonic
	bsr.w opforgeNativeCliRecordPrvmExpressionRequest

opforgeNativeCliRecordPrvmStatementCheckMnemonic
	tst.l NativeCliStmtLabelLen
	beq.s opforgeNativeCliRecordPrvmStatementCheckMnemonicFound
	tst.w NativeCliStmtMnemFound
	beq.w opforgeNativeCliRecordPrvmStatementMaybeLabelOnly
	tst.l NativeCliStmtOperandStart
	bne.s opforgeNativeCliRecordPrvmStatementCheckMnemonicFound
	move.l NativeCliStmtLabelLen, d0
	cmp.l NativeCliStmtMnemLen, d0
	beq.s opforgeNativeCliRecordPrvmStatementClearBareMnem
	move.l NativeCliStmtLabelStart, d0
	cmp.l NativeCliStmtMnemStart, d0
	bne.s opforgeNativeCliRecordPrvmStatementCheckMnemonicFound
	move.l NativeCliStmtLabelEnd, d0
	cmp.l NativeCliStmtMnemEnd, d0
	bne.s opforgeNativeCliRecordPrvmStatementCheckMnemonicFound

opforgeNativeCliRecordPrvmStatementClearBareMnem
	clr.l NativeCliStmtMnemStart
	clr.l NativeCliStmtMnemEnd
	clr.l NativeCliStmtMnemOff
	clr.l NativeCliStmtMnemLen
	clr.w NativeCliStmtMnemFound
	bra.s opforgeNativeCliRecordPrvmStatementMaybeLabelOnly

opforgeNativeCliRecordPrvmStatementCheckMnemonicFound
	tst.w NativeCliStmtMnemFound
	beq.s opforgeNativeCliRecordPrvmStatementMaybeLabelOnly
	tst.l NativeCliStmtOperandStart
	bne.s opforgeNativeCliRecordPrvmStatementCheckStore
	tst.l NativeCliStmtLabelLen
	bne.s opforgeNativeCliRecordPrvmStatementCheckStore
	clr.l NativeCliStmtMnemStart
	clr.l NativeCliStmtMnemEnd
	clr.l NativeCliStmtMnemOff
	clr.l NativeCliStmtMnemLen
	clr.w NativeCliStmtMnemFound
	bra.w opforgeNativeCliRecordPrvmStatementTrySourceFallback

opforgeNativeCliRecordPrvmStatementMaybeLabelOnly
	tst.l NativeCliStmtLabelLen
	bne.s opforgeNativeCliRecordPrvmStatementCheckStore
opforgeNativeCliRecordPrvmStatementTrySourceFallback
	bsr.w opforgeNativeCliRecordSourceStatementFallback
	tst.w NativeCliStmtMnemFound
	bne.s opforgeNativeCliRecordPrvmStatementCheckStore
	tst.l NativeCliStmtLabelLen
	beq.w opforgeNativeCliRecordPrvmStatementDone
opforgeNativeCliRecordPrvmStatementCheckStore
	move.l NativeCliStmtMnemLen, d0
	cmp.l #TOKEN_BUFFER_CAPACITY - 1, d0
	bhi.w opforgeNativeCliRecordPrvmStatementFail
	move.w opasmEngineStmtCount.l, d0
	cmpi.w #NATIVE_STATEMENT_TABLE_CAPACITY, d0
	bhs.w opforgeNativeCliRecordPrvmStatementFail
	bsr.w opforgeNativeCliStoreStatementRecord
	tst.w opasmEngineStmtCount.l
	bpl.s opforgeNativeCliRecordPrvmStatementSkipEmit
	bsr.w opforgeNativeCliEmitStatementRecord

opforgeNativeCliRecordPrvmStatementSkipEmit
	addq.w #1, opasmEngineStmtCount.l

opforgeNativeCliRecordPrvmStatementDone
	moveq #0, d0
	bra.w opforgeNativeCliRecordPrvmStatementReturn

opforgeNativeCliRecordPrvmStatementSourceOnly
	clr.l NativeCliStmtLabelStart
	clr.l NativeCliStmtLabelEnd
	clr.l NativeCliStmtLabelOff
	clr.l NativeCliStmtLabelLen
	clr.l NativeCliStmtMnemStart
	clr.l NativeCliStmtMnemEnd
	clr.l NativeCliStmtMnemOff
	clr.l NativeCliStmtMnemLen
	clr.l NativeCliStmtOperandStart
	clr.l NativeCliStmtOperandEnd
	clr.l NativeCliStmtExprOperandIndex
	clr.l NativeCliStmtExprSlotIndex
	clr.l NativeCliStmtExprStartToken
	clr.l NativeCliStmtExprEndToken
	clr.l NativeCliStmtExprSpanLine
	clr.l NativeCliStmtExprSpanStart
	clr.l NativeCliStmtExprSpanEnd
	clr.w NativeCliStmtMnemFound
	clr.w NativeCliStmtExprFound
	moveq #NCLI_PARSER_DIRECTIVE_NONE, d0
	move.w d0, NativeCliStmtDirectiveKind
	bra.w opforgeNativeCliRecordPrvmStatementTrySourceFallback

opforgeNativeCliRecordPrvmStatementHaveLabel
	move.l 8(a2), NativeCliStmtLabelStart
	move.l 12(a2), NativeCliStmtLabelEnd
	move.l 16(a2), NativeCliStmtLabelOff
	move.l 20(a2), NativeCliStmtLabelLen
	bra.w opforgeNativeCliRecordPrvmStatementNext

opforgeNativeCliRecordPrvmStatementHaveMnemonic
	move.l 8(a2), NativeCliStmtMnemStart
	move.l 12(a2), NativeCliStmtMnemEnd
	move.l 16(a2), NativeCliStmtMnemOff
	move.l 20(a2), NativeCliStmtMnemLen
	move.w #1, NativeCliStmtMnemFound
	bra.w opforgeNativeCliRecordPrvmStatementNext

opforgeNativeCliRecordPrvmStatementHaveDirective
	move.w #NCLI_PARSER_DIRECTIVE_GENERIC, NativeCliStmtDirectiveKind
	bra.s opforgeNativeCliRecordPrvmStatementHaveMnemonic

opforgeNativeCliRecordPrvmStatementHaveOperandText
	move.l 8(a2), NativeCliStmtOperandStart
	move.l 12(a2), NativeCliStmtOperandEnd
	bra.w opforgeNativeCliRecordPrvmStatementNext

opforgeNativeCliRecordPrvmStatementHaveOperandExpr
	move.l 4(a2), NativeCliStmtExprSpanLine
	move.l 8(a2), NativeCliStmtExprSpanStart
	move.l 12(a2), NativeCliStmtExprSpanEnd
	move.l 16(a2), NativeCliStmtExprOperandIndex
	move.l 20(a2), NativeCliStmtExprSlotIndex
	move.l 24(a2), NativeCliStmtExprStartToken
	move.l 28(a2), NativeCliStmtExprEndToken
	move.w #1, NativeCliStmtExprFound
	bra.w opforgeNativeCliRecordPrvmStatementNext

opforgeNativeCliRecordPrvmExpressionRequest
	lea OpforgeNativeCliPrvmExprRequest, a2
	cmpi.w #1, 0(a2)
	bne.s opforgeNativeCliRecordPrvmExpressionRequestDone
	move.l 4(a2), NativeCliStmtExprOperandIndex
	move.l 8(a2), NativeCliStmtExprSlotIndex
	move.l 12(a2), NativeCliStmtExprStartToken
	move.l 16(a2), NativeCliStmtExprEndToken
	move.l 20(a2), NativeCliStmtExprSpanLine
	move.l 24(a2), NativeCliStmtExprSpanStart
	move.l 28(a2), NativeCliStmtExprSpanEnd
	move.w #1, NativeCliStmtExprFound

opforgeNativeCliRecordPrvmExpressionRequestDone
	rts

opforgeNativeCliRecordSourceStatementFallback
	movem.l d0-d7/a0-a3, -(sp)
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w opforgeNativeCliRecordSourceStatementFallbackReturn
	movea.l a0, a2
	move.l d0, d2
	bsr.w opforgeNativeCliFallbackTokenLen
	tst.w d0
	beq.w opforgeNativeCliRecordSourceStatementFallbackReturn
	move.w d0, d3
	moveq #0, d4
	move.w NativeCliSourceLineLen, d4
	sub.w d2, d4
	addq.w #1, d4
	movea.l a2, a3
	adda.w d3, a3
	move.l d2, d5
	sub.w d3, d5
	tst.l d5
	beq.s opforgeNativeCliRecordSourceStatementFirstToken
	cmpi.b #':', (a3)
	beq.s opforgeNativeCliRecordSourceStatementLabelToken

opforgeNativeCliRecordSourceStatementFirstToken
	cmpi.l #1, d4
	bne.s opforgeNativeCliRecordSourceStatementFirstTokenMnemonic
	tst.l d5
	beq.s opforgeNativeCliRecordSourceStatementBareLabel
	movea.l a3, a0
	move.l d5, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.s opforgeNativeCliRecordSourceStatementBareLabel
	tst.b (a0)
	beq.s opforgeNativeCliRecordSourceStatementBareLabel
	cmpi.b #10, (a0)
	beq.s opforgeNativeCliRecordSourceStatementBareLabel
	cmpi.b #13, (a0)
	beq.s opforgeNativeCliRecordSourceStatementBareLabel
	cmpi.b #';', (a0)
	beq.s opforgeNativeCliRecordSourceStatementBareLabel

opforgeNativeCliRecordSourceStatementFirstTokenMnemonic
	bsr.w opforgeNativeCliRecordSourceStatementMnemonic
	bra.w opforgeNativeCliRecordSourceStatementFallbackReturn

opforgeNativeCliRecordSourceStatementBareLabel
	move.l d4, NativeCliStmtLabelStart
	move.l d4, d0
	add.w d3, d0
	move.l d0, NativeCliStmtLabelEnd
	move.l d3, NativeCliStmtLabelLen
	clr.l NativeCliStmtLabelOff
	bra.w opforgeNativeCliRecordSourceStatementFallbackReturn

opforgeNativeCliRecordSourceStatementLabelToken
	move.l d4, NativeCliStmtLabelStart
	move.l d4, d0
	add.w d3, d0
	move.l d0, NativeCliStmtLabelEnd
	move.l d3, NativeCliStmtLabelLen
	clr.l NativeCliStmtLabelOff
	addq.l #1, a3
	subq.l #1, d5
	movea.l a3, a0
	move.l d5, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w opforgeNativeCliRecordSourceStatementFallbackReturn
	movea.l a0, a2
	move.l d0, d2
	bsr.w opforgeNativeCliFallbackTokenLen
	tst.w d0
	beq.w opforgeNativeCliRecordSourceStatementFallbackReturn
	move.w d0, d3
	moveq #0, d4
	move.w NativeCliSourceLineLen, d4
	sub.w d2, d4
	addq.w #1, d4
	bsr.w opforgeNativeCliRecordSourceStatementMnemonic
	bra.w opforgeNativeCliRecordSourceStatementFallbackReturn

opforgeNativeCliRecordSourceStatementMnemonic
	move.l d4, NativeCliStmtMnemStart
	move.l d4, d0
	add.w d3, d0
	move.l d0, NativeCliStmtMnemEnd
	clr.l NativeCliStmtMnemOff
	move.l d3, NativeCliStmtMnemLen
	lea tokenScratchBuffer, a1
	movea.l a2, a0
	move.w d3, d0
	bsr.w opforgeNativeCliCopyFixedString
	clr.b (a1)
	move.w #1, NativeCliStmtMnemFound
	movea.l a2, a0
	adda.w d3, a0
	move.l d2, d0
	sub.w d3, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.s opforgeNativeCliRecordSourceStatementMnemonicDone
	tst.b (a0)
	beq.s opforgeNativeCliRecordSourceStatementMnemonicDone
	cmpi.b #10, (a0)
	beq.s opforgeNativeCliRecordSourceStatementMnemonicDone
	cmpi.b #13, (a0)
	beq.s opforgeNativeCliRecordSourceStatementMnemonicDone
	cmpi.b #';', (a0)
	beq.s opforgeNativeCliRecordSourceStatementMnemonicDone
	moveq #0, d5
	move.w NativeCliSourceLineLen, d5
	sub.w d0, d5
	addq.w #1, d5
	move.l d5, NativeCliStmtOperandStart
	bsr.w opforgeNativeCliFallbackOperandLen
	tst.w d0
	beq.s opforgeNativeCliRecordSourceStatementMnemonicDone
	add.w d0, d5
	move.l d5, NativeCliStmtOperandEnd

opforgeNativeCliRecordSourceStatementMnemonicDone
	rts

opforgeNativeCliFallbackTokenLen
	movem.l d1-d2/a0, -(sp)
	moveq #0, d1

opforgeNativeCliFallbackTokenLenLoop
	tst.l d0
	beq.s opforgeNativeCliFallbackTokenLenDone
	move.b (a0), d2
	tst.b d2
	beq.s opforgeNativeCliFallbackTokenLenDone
	cmpi.b #10, d2
	beq.s opforgeNativeCliFallbackTokenLenDone
	cmpi.b #13, d2
	beq.s opforgeNativeCliFallbackTokenLenDone
	cmpi.b #' ', d2
	beq.s opforgeNativeCliFallbackTokenLenDone
	cmpi.b #9, d2
	beq.s opforgeNativeCliFallbackTokenLenDone
	cmpi.b #':', d2
	beq.s opforgeNativeCliFallbackTokenLenDone
	cmpi.b #';', d2
	beq.s opforgeNativeCliFallbackTokenLenDone
	addq.l #1, a0
	subq.l #1, d0
	addq.w #1, d1
	bra.s opforgeNativeCliFallbackTokenLenLoop

opforgeNativeCliFallbackTokenLenDone
	move.w d1, d0
	movem.l (sp)+, d1-d2/a0
	rts

opforgeNativeCliFallbackOperandLen
	movem.l d1-d3/a0, -(sp)
	moveq #0, d1
	moveq #0, d2

opforgeNativeCliFallbackOperandLenLoop
	tst.l d0
	beq.s opforgeNativeCliFallbackOperandLenDone
	move.b (a0)+, d3
	tst.b d3
	beq.s opforgeNativeCliFallbackOperandLenDone
	cmpi.b #10, d3
	beq.s opforgeNativeCliFallbackOperandLenDone
	cmpi.b #13, d3
	beq.s opforgeNativeCliFallbackOperandLenDone
	cmpi.b #';', d3
	beq.s opforgeNativeCliFallbackOperandLenDone
	addq.w #1, d1
	cmpi.b #' ', d3
	beq.s opforgeNativeCliFallbackOperandLenNext
	cmpi.b #9, d3
	beq.s opforgeNativeCliFallbackOperandLenNext
	move.w d1, d2

opforgeNativeCliFallbackOperandLenNext
	subq.l #1, d0
	bra.s opforgeNativeCliFallbackOperandLenLoop

opforgeNativeCliFallbackOperandLenDone
	move.w d2, d0
	movem.l (sp)+, d1-d3/a0
	rts

opforgeNativeCliRecordSourceStatementFallbackReturn
	movem.l (sp)+, d0-d7/a0-a3
	rts

opforgeNativeCliRecordPrvmStatementFail
	moveq #1, d0

opforgeNativeCliRecordPrvmStatementReturn
	movem.l (sp)+, d1-d7/a0-a2
	rts

opforgeNativeCliStoreStatementRecord
	movem.l d1-d4/a0-a1, -(sp)
	moveq #0, d1
	move.w opasmEngineStmtCount.l, d1
	lsl.l #2, d1
	lea opasmEngineStmtLineTable.l, a0
	move.l NativeCliSourceLineNum, 0(a0, d1.l)
	lea opasmEngineStmtMnemOffTable.l, a0
	move.l NativeCliStmtMnemOff, 0(a0, d1.l)
	moveq #0, d2
	move.w opasmEngineStmtCount.l, d2
	add.w d2, d2
	lea opasmEngineStmtSourceLineLenTable.l, a0
	clr.w 0(a0, d2.l)
	lea opasmEngineStmtLabelLenTable.l, a0
	move.w NativeCliStmtLabelLen, 0(a0, d2.l)
	lea opasmEngineStmtMnemLenTable.l, a0
	move.w NativeCliStmtMnemLen, 0(a0, d2.l)
	lea opasmEngineStmtOperandLenTable.l, a0
	clr.w 0(a0, d2.l)
	lea opasmEngineStmtDirectiveKindTable.l, a0
	move.w NativeCliStmtDirectiveKind, 0(a0, d2.l)
	lea opasmEngineStmtExprFlagsTable.l, a0
	clr.w 0(a0, d2.l)
	lea opasmEngineStmtExprOperandIndexTable.l, a0
	clr.l 0(a0, d1.l)
	lea opasmEngineStmtExprSlotIndexTable.l, a0
	clr.l 0(a0, d1.l)
	lea opasmEngineStmtExprStartTokenTable.l, a0
	clr.l 0(a0, d1.l)
	lea opasmEngineStmtExprEndTokenTable.l, a0
	clr.l 0(a0, d1.l)
	lea opasmEngineStmtExprSpanLineTable.l, a0
	clr.l 0(a0, d1.l)
	lea opasmEngineStmtExprSpanStartTable.l, a0
	clr.l 0(a0, d1.l)
	lea opasmEngineStmtExprSpanEndTable.l, a0
	clr.l 0(a0, d1.l)
	moveq #0, d3
	move.w opasmEngineStmtCount.l, d3
	lsl.l #6, d3
	moveq #0, d4
	move.w opasmEngineStmtCount.l, d4
	lsl.l #8, d4
	add.l d4, d4
	lea opasmEngineStmtSourceLineTextTable.l, a1
	adda.l d4, a1
	clr.b (a1)
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	beq.s opforgeNativeCliStoreStatementSourceLineDone
	cmp.l #SOURCE_LINE_BUFFER_CAPACITY - 1, d0
	bls.s opforgeNativeCliStoreStatementSourceLineLenOk
	move.l #SOURCE_LINE_BUFFER_CAPACITY - 1, d0

opforgeNativeCliStoreStatementSourceLineLenOk
	lea NativeCliSourceLine, a0
	move.l d2, -(sp)
	bsr.w opforgeNativeCliCopyFixedString
	move.l (sp)+, d2
	clr.b (a1)
	lea opasmEngineStmtSourceLineLenTable.l, a0
	move.w d0, 0(a0, d2.l)

opforgeNativeCliStoreStatementSourceLineDone
	lea opasmEngineStmtLabelNameTable.l, a1
	adda.l d3, a1
	clr.b (a1)
	move.l NativeCliStmtLabelLen, d0
	beq.s opforgeNativeCliStoreStatementMnemText
	move.l NativeCliStmtLabelStart, d1
	beq.s opforgeNativeCliStoreStatementMnemText
	subq.l #1, d1
	lea NativeCliSourceLine, a0
	adda.l d1, a0
	bsr.w opforgeNativeCliCopyFixedString
	clr.b (a1)

opforgeNativeCliStoreStatementMnemText
	lea opasmEngineStmtMnemNameTable.l, a1
	adda.l d3, a1
	clr.b (a1)
	move.l NativeCliStmtMnemLen, d0
	beq.w opforgeNativeCliStoreStatementDone
	lea tokenScratchBuffer, a0
	adda.l NativeCliStmtMnemOff, a0
	bsr.w opforgeNativeCliCopyFixedString
	clr.b (a1)

opforgeNativeCliStoreStatementOperandText
	lea opasmEngineStmtOperandNameTable.l, a1
	adda.l d3, a1
	clr.b (a1)
	move.l NativeCliStmtMnemStart, d0
	bne.s opforgeNativeCliStoreStatementOperandFallback
	move.l NativeCliStmtOperandStart, d0
	beq.s opforgeNativeCliStoreStatementOperandFallback
	move.l NativeCliStmtOperandEnd, d1
	cmp.l d0, d1
	bls.s opforgeNativeCliStoreStatementOperandFallback
	move.l d0, d2
	subq.l #1, d2
	sub.l d0, d1
	lea NativeCliSourceLine, a0
	adda.l d2, a0
	move.l d1, d0
	bsr.w opforgeNativeCliCopyOperandText
	moveq #0, d0
	move.w opasmEngineStmtCount.l, d0
	add.w d0, d0
	lea opasmEngineStmtOperandLenTable.l, a0
	move.w d5, 0(a0, d0.l)
	bra.s opforgeNativeCliStoreStatementExprMetadata

opforgeNativeCliStoreStatementOperandFallback
	move.l NativeCliStmtMnemStart, d0
	beq.w opforgeNativeCliStoreStatementExprMetadata
	move.l NativeCliStmtMnemLen, d2
	beq.w opforgeNativeCliStoreStatementExprMetadata
	add.l d2, d0
	beq.w opforgeNativeCliStoreStatementExprMetadata
	moveq #0, d1
	move.w NativeCliSourceLineLen, d1
	cmp.l d1, d0
	bhs.w opforgeNativeCliStoreStatementExprMetadata
	lea NativeCliSourceLine, a0
	adda.l d0, a0
	sub.l d0, d1
	move.l d1, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	bsr.w opforgeNativeCliCopyOperandText
	moveq #0, d0
	move.w opasmEngineStmtCount.l, d0
	add.w d0, d0
	lea opasmEngineStmtOperandLenTable.l, a0
	move.w d5, 0(a0, d0.l)

opforgeNativeCliStoreStatementExprMetadata
	tst.w NativeCliStmtExprFound
	beq.w opforgeNativeCliStoreStatementDone
	moveq #0, d1
	move.w opasmEngineStmtCount.l, d1
	lsl.l #2, d1
	moveq #0, d2
	move.w opasmEngineStmtCount.l, d2
	add.w d2, d2
	lea opasmEngineStmtExprFlagsTable.l, a0
	move.w #1, 0(a0, d2.l)
	lea opasmEngineStmtExprOperandIndexTable.l, a0
	move.l NativeCliStmtExprOperandIndex, 0(a0, d1.l)
	lea opasmEngineStmtExprSlotIndexTable.l, a0
	move.l NativeCliStmtExprSlotIndex, 0(a0, d1.l)
	lea opasmEngineStmtExprStartTokenTable.l, a0
	move.l NativeCliStmtExprStartToken, 0(a0, d1.l)
	lea opasmEngineStmtExprEndTokenTable.l, a0
	move.l NativeCliStmtExprEndToken, 0(a0, d1.l)
	lea opasmEngineStmtExprSpanLineTable.l, a0
	move.l NativeCliStmtExprSpanLine, 0(a0, d1.l)
	lea opasmEngineStmtExprSpanStartTable.l, a0
	move.l NativeCliStmtExprSpanStart, 0(a0, d1.l)
	lea opasmEngineStmtExprSpanEndTable.l, a0
	move.l NativeCliStmtExprSpanEnd, 0(a0, d1.l)

opforgeNativeCliStoreStatementDone
	movem.l (sp)+, d1-d4/a0-a1
	rts

opforgeNativeCliCopyOperandText
	movem.l d0-d4/a0-a1, -(sp)
	clr.w d5
	move.l #TOKEN_BUFFER_CAPACITY - 1, d4

opforgeNativeCliCopyOperandTextLoop
	tst.l d0
	beq.s opforgeNativeCliCopyOperandTextDone
	moveq #0, d2
	move.b (a0), d2
	beq.s opforgeNativeCliCopyOperandTextDone
	cmpi.b #';', d2
	beq.s opforgeNativeCliCopyOperandTextDone
	cmpi.b #10, d2
	beq.s opforgeNativeCliCopyOperandTextDone
	cmpi.b #13, d2
	beq.s opforgeNativeCliCopyOperandTextDone
	tst.l d4
	beq.s opforgeNativeCliCopyOperandTextDone
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.w #1, d5
	subq.l #1, d4
	bra.s opforgeNativeCliCopyOperandTextLoop

opforgeNativeCliCopyOperandTextDone
	bsr.w opforgeNativeCliTrimCopiedOperandText
	clr.b (a1)
	movem.l (sp)+, d0-d4/a0-a1
	rts

opforgeNativeCliTrimCopiedOperandText
	tst.w d5
	beq.s opforgeNativeCliTrimCopiedOperandTextDone
	movea.l a1, a0

opforgeNativeCliTrimCopiedOperandTextLoop
	tst.w d5
	beq.s opforgeNativeCliTrimCopiedOperandTextSetEnd
	subq.l #1, a0
	move.b (a0), d0
	cmpi.b #' ', d0
	beq.s opforgeNativeCliTrimCopiedOperandTextOne
	cmpi.b #9, d0
	beq.s opforgeNativeCliTrimCopiedOperandTextOne
	bra.s opforgeNativeCliTrimCopiedOperandTextSetEnd

opforgeNativeCliTrimCopiedOperandTextOne
	subq.w #1, d5
	bra.s opforgeNativeCliTrimCopiedOperandTextLoop

opforgeNativeCliTrimCopiedOperandTextSetEnd
	movea.l a0, a1
	addq.l #1, a1

opforgeNativeCliTrimCopiedOperandTextDone
	rts

opforgeNativeCliEmitStatementRecord
	movem.l d0-d7/a0-a1, -(sp)
	move.l #StatementText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineStmtCount.l, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliSourceLineNum, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.w NativeCliStmtDirectiveKind, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtLabelStart, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtLabelEnd, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtMnemStart, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtMnemEnd, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtLabelLen, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtMnemLen, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	lea tokenScratchBuffer, a0
	move.l NativeCliStmtMnemOff, d0
	adda.l d0, a0
	lea NativeCliArgToken, a1
	move.l NativeCliStmtMnemLen, d0
	bsr.w opforgeNativeCliCopyFixedString
	clr.b (a1)
	move.l #NativeCliArgToken, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	tst.w NativeCliStmtExprFound
	beq.s opforgeNativeCliEmitStatementRecordDone
	bsr.w opforgeNativeCliEmitStatementExprRequest

opforgeNativeCliEmitStatementRecordDone
	movem.l (sp)+, d0-d7/a0-a1
	rts

opforgeNativeCliEmitStatementExprRequest
	move.l #StatementExprText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineStmtCount.l, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprOperandIndex, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSlotIndex, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprStartToken, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprEndToken, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSpanLine, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSpanStart, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliStmtExprSpanEnd, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	rts

opforgeNativeCliBuildParserTailBuffer
	movem.l d1-d7/a0-a3, -(sp)
	bsr.w opforgeNativeCliParserTailFallbackEnd

opforgeNativeCliBuildParserTailHaveEnd
	lea NativeCliParserTailBuffer, a1
	clr.w NativeCliParserTailLen
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	cmp.l d0, d6
	bhi.w opforgeNativeCliBuildParserTailFail

opforgeNativeCliBuildParserTailEndOk
	lea NativeCliSourceLine, a0
	adda.l d6, a0
	sub.l d6, d0
	moveq #0, d5

opforgeNativeCliBuildParserTailCopyLoop
	tst.l d0
	beq.w opforgeNativeCliBuildParserTailDone
	cmpi.l #SOURCE_LINE_BUFFER_CAPACITY - 1, d5
	bhs.w opforgeNativeCliBuildParserTailFail
	move.b (a0)+, (a1)+
	addq.l #1, d5
	subq.l #1, d0
	bra.w opforgeNativeCliBuildParserTailCopyLoop

opforgeNativeCliBuildParserTailDone
	clr.b (a1)
	move.w d5, NativeCliParserTailLen
	moveq #0, d0
	bra.s opforgeNativeCliBuildParserTailReturn

opforgeNativeCliBuildParserTailFail
	clr.b NativeCliParserTailBuffer
	clr.w NativeCliParserTailLen
	moveq #1, d0

opforgeNativeCliBuildParserTailReturn
	movem.l (sp)+, d1-d7/a0-a3
	rts

opforgeNativeCliParserTailFallbackEnd
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	moveq #0, d5
	move.w NativeCliSourceLineLen, d5
	sub.l d0, d5
	lea ModuleDirectiveText, a1
	moveq #7, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.s opforgeNativeCliParserTailFallbackModule
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea EndmoduleDirectiveText, a1
	moveq #10, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.s opforgeNativeCliParserTailFallbackEndmodule
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea UseDirectiveText, a1
	moveq #4, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.s opforgeNativeCliParserTailFallbackUse
	moveq #0, d6
	rts

opforgeNativeCliParserTailFallbackModule
	move.l d5, d6
	addq.l #7, d6
	rts

opforgeNativeCliParserTailFallbackEndmodule
	move.l d5, d6
	addi.l #10, d6
	rts

opforgeNativeCliParserTailFallbackUse
	move.l d5, d6
	addq.l #4, d6
	rts

opforgeNativeCliParserTailPtr
	bsr.w opforgeNativeCliBuildParserTailBuffer
	move.l d0, d1
	tst.l d1
	bne.s opforgeNativeCliParserTailPtrReturn
	lea NativeCliParserTailBuffer, a0
	moveq #0, d0
	move.w NativeCliParserTailLen, d0
	moveq #0, d1

opforgeNativeCliParserTailPtrReturn
	rts

opforgeNativeCliParseModuleLine
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w opforgeNativeCliParseLineFail
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea NativeCliArgToken, a1
	bsr.w opforgeNativeCliCopyLineWord
	tst.l d0
	bne.w opforgeNativeCliParseLineFail
	tst.b NativeCliArgToken
	beq.w opforgeNativeCliParseLineFail
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.s opforgeNativeCliParseModuleLineRecord
	cmpi.b #';', (a0)
	bne.w opforgeNativeCliParseLineFail

opforgeNativeCliParseModuleLineRecord
	bsr.w opforgeNativeCliRecordModule
	tst.l d0
	bne.w opforgeNativeCliParseLineFail
	moveq #0, d0
	move.w NativeCliCurrentModuleId, d0
	bsr.w opforgeNativeCliEmitModuleRecord
	moveq #0, d0
	move.w NativeCliCurrentModuleId, d0
	bsr.w opforgeNativeCliEmitModuleCompatibility
	bra.w opforgeNativeCliParseLineDone

opforgeNativeCliParseEndmoduleLine
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w opforgeNativeCliParseLineFail
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.s opforgeNativeCliParseEndmoduleLineClose
	cmpi.b #';', (a0)
	bne.w opforgeNativeCliParseLineFail

opforgeNativeCliParseEndmoduleLineClose
	bsr.w opforgeNativeCliCloseModule
	tst.l d0
	bne.w opforgeNativeCliParseModuleDepthFail
	bra.w opforgeNativeCliParseLineDone

opforgeNativeCliParseUseLine
	bsr.w opforgeNativeCliParserTailPtr
	tst.l d1
	bne.w opforgeNativeCliParseLineFail
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea NativeCliArgToken, a1
	bsr.w opforgeNativeCliCopyUseToken
	tst.l d1
	bne.w opforgeNativeCliParseLineFail
	tst.b NativeCliArgToken
	beq.w opforgeNativeCliParseLineFail
	clr.b NativeCliIncludeTarget
	bsr.w opforgeNativeCliSkipLineWhitespace
	bsr.w opforgeNativeCliParseUseOptionalAlias
	tst.l d1
	bne.w opforgeNativeCliParseLineFail
	move.l d0, d5
	bsr.w opforgeNativeCliRecordImport
	tst.l d0
	bne.w opforgeNativeCliParseLineFail
	move.l d5, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w opforgeNativeCliParseUseBare
	cmpi.b #';', (a0)
	beq.w opforgeNativeCliParseUseBare
	bsr.w opforgeNativeCliEmitImportRecord
	cmpi.b #'(', (a0)
	bne.w opforgeNativeCliParseLineFail
	addq.l #1, a0
	subq.l #1, d0
	bsr.w opforgeNativeCliParseUseItems
	tst.l d1
	bne.w opforgeNativeCliParseLineFail
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w opforgeNativeCliParseLineDone
	cmpi.b #';', (a0)
	bne.w opforgeNativeCliParseLineFail
	bra.w opforgeNativeCliParseLineDone

opforgeNativeCliParseUseBare
	tst.b NativeCliIncludeTarget
	bne.s opforgeNativeCliParseUseBareEmit
	tst.w NativeCliModuleResolveDepth
	bne.s opforgeNativeCliParseUseBareEmit
	bsr.w opforgeNativeCliResolveBareUseModule
	tst.l d1
	bne.w opforgeNativeCliParseUseResolveFail
	moveq #0, d2
	move.w d4, d2
	add.w d2, d2
	lea NativeCliImportModuleTable, a1
	move.w d0, 0(a1, d2.l)

opforgeNativeCliParseUseBareEmit
	bsr.w opforgeNativeCliEmitImportRecord
	bra.w opforgeNativeCliParseLineDone

opforgeNativeCliParseIncludeLine
	lea NativeCliSourceLine, a0
	moveq #0, d0
	move.w NativeCliSourceLineLen, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	addq.l #8, a0
	subq.l #8, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea NativeCliIncludeTarget, a1
	bsr.w opforgeNativeCliCopyIncludeTarget
	tst.l d0
	bne.w opforgeNativeCliParseIncludeFail
	tst.b NativeCliIncludeTarget
	beq.w opforgeNativeCliParseIncludeFail
	bsr.w opforgeNativeCliExpandIncludeTarget
	tst.l d0
	bne.w opforgeNativeCliParseLineReturn
	bra.w opforgeNativeCliParseLineDone

opforgeNativeCliParseIncludeFail
	move.l #IncludeFailureText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	bra.w opforgeNativeCliParseLineReturn

opforgeNativeCliParseModuleDepthFail
	move.l #ModuleDepthFailureText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	bra.w opforgeNativeCliParseLineReturn

opforgeNativeCliParseUseResolveFail
	move.l #ModuleResolveFailureText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	bra.w opforgeNativeCliParseLineReturn

opforgeNativeCliParseLineFail
	move.l #ParserFailureText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0

opforgeNativeCliParseLineReturn
	movem.l (sp)+, d2-d7/a2-a4
	rts

opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.s opforgeNativeCliSkipLineWhitespaceDone
	cmpi.b #' ', (a0)
	beq.s opforgeNativeCliSkipLineWhitespaceOne
	cmpi.b #9, (a0)
	bne.s opforgeNativeCliSkipLineWhitespaceDone

opforgeNativeCliSkipLineWhitespaceOne
	addq.l #1, a0
	subq.l #1, d0
	bra.s opforgeNativeCliSkipLineWhitespace

opforgeNativeCliSkipLineWhitespaceDone
	rts

opforgeNativeCliLineStartsWith
	cmp.l d1, d0
	bcs.s opforgeNativeCliLineStartsNo
	movea.l a0, a2
	movea.l a1, a3
	move.l d1, d2
	beq.s opforgeNativeCliLineStartsBoundary
	subq.l #1, d2

opforgeNativeCliLineStartsLoop
	move.b (a2)+, d3
	move.b (a3)+, d4
	cmpi.b #'A', d3
	bcs.s opforgeNativeCliLineStartsCompare
	cmpi.b #'Z', d3
	bhi.s opforgeNativeCliLineStartsCompare
	addi.b #32, d3

opforgeNativeCliLineStartsCompare
	cmp.b d4, d3
	bne.s opforgeNativeCliLineStartsNo
	dbra d2, opforgeNativeCliLineStartsLoop

opforgeNativeCliLineStartsBoundary
	cmp.l d1, d0
	beq.s opforgeNativeCliLineStartsYes
	move.b 0(a0, d1.l), d3
	cmpi.b #' ', d3
	beq.s opforgeNativeCliLineStartsYes
	cmpi.b #9, d3
	beq.s opforgeNativeCliLineStartsYes
	cmpi.b #';', d3
	beq.s opforgeNativeCliLineStartsYes
	moveq #0, d0
	rts

opforgeNativeCliLineStartsYes
	moveq #1, d0
	rts

opforgeNativeCliLineStartsNo
	moveq #0, d0
	rts

opforgeNativeCliCopyLineWord
	move.l #TOKEN_BUFFER_CAPACITY - 1, d6
	clr.l d5

opforgeNativeCliCopyLineWordLoop
	tst.l d0
	beq.s opforgeNativeCliCopyLineWordDone
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #' ', d2
	beq.s opforgeNativeCliCopyLineWordDone
	cmpi.b #9, d2
	beq.s opforgeNativeCliCopyLineWordDone
	cmpi.b #';', d2
	beq.s opforgeNativeCliCopyLineWordDone
	cmpi.b #'(', d2
	beq.s opforgeNativeCliCopyLineWordDone
	cmpi.b #',', d2
	beq.s opforgeNativeCliCopyLineWordDone
	tst.l d6
	beq.s opforgeNativeCliCopyLineWordFail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d5
	subq.l #1, d6
	bra.s opforgeNativeCliCopyLineWordLoop

opforgeNativeCliCopyLineWordDone
	clr.b (a1)
	moveq #0, d0
	rts

opforgeNativeCliCopyLineWordFail
	moveq #1, d0
	rts

opforgeNativeCliCopyUseToken
	move.l #TOKEN_BUFFER_CAPACITY - 1, d6

opforgeNativeCliCopyUseTokenLoop
	tst.l d0
	beq.s opforgeNativeCliCopyUseTokenDone
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #' ', d2
	beq.s opforgeNativeCliCopyUseTokenDone
	cmpi.b #9, d2
	beq.s opforgeNativeCliCopyUseTokenDone
	cmpi.b #';', d2
	beq.s opforgeNativeCliCopyUseTokenDone
	cmpi.b #'(', d2
	beq.s opforgeNativeCliCopyUseTokenDone
	cmpi.b #')', d2
	beq.s opforgeNativeCliCopyUseTokenDone
	cmpi.b #',', d2
	beq.s opforgeNativeCliCopyUseTokenDone
	tst.l d6
	beq.s opforgeNativeCliCopyUseTokenFail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	subq.l #1, d6
	bra.s opforgeNativeCliCopyUseTokenLoop

opforgeNativeCliCopyUseTokenDone
	clr.b (a1)
	moveq #0, d1
	rts

opforgeNativeCliCopyUseTokenFail
	clr.b (a1)
	moveq #1, d1
	rts

opforgeNativeCliParseUseOptionalAlias
	movem.l d0/d6/a1, -(sp)
	move.l d0, d6
	lea AsKeywordText, a1
	moveq #2, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	beq.s opforgeNativeCliParseUseAliasNone
	move.l d6, d0
	addq.l #2, a0
	subq.l #2, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	lea NativeCliIncludeTarget, a1
	bsr.w opforgeNativeCliCopyUseToken
	tst.l d1
	bne.s opforgeNativeCliParseUseAliasFail
	tst.b NativeCliIncludeTarget
	beq.s opforgeNativeCliParseUseAliasFail
	moveq #0, d1
	bra.s opforgeNativeCliParseUseAliasReturn

opforgeNativeCliParseUseAliasNone
	move.l d6, d0
	moveq #0, d1
	bra.s opforgeNativeCliParseUseAliasReturn

opforgeNativeCliParseUseAliasFail
	moveq #1, d1

opforgeNativeCliParseUseAliasReturn
	movem.l (sp)+, d6/a1
	addq.l #4, sp
	rts

opforgeNativeCliParseUseItems
	move.w d4, d5
	clr.w d7
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w opforgeNativeCliParseUseItemsFail
	cmpi.b #')', (a0)
	beq.w opforgeNativeCliParseUseItemsFail
	cmpi.b #'*', (a0)
	beq.w opforgeNativeCliParseUseWildcard

opforgeNativeCliParseUseItemLoop
	lea NativeCliArgToken, a1
	bsr.w opforgeNativeCliCopyUseToken
	tst.l d1
	bne.w opforgeNativeCliParseUseItemsFail
	tst.b NativeCliArgToken
	beq.w opforgeNativeCliParseUseItemsFail
	cmpi.b #'*', NativeCliArgToken
	bne.s opforgeNativeCliParseUseItemNameOk
	lea NativeCliArgToken, a1
	tst.b 1(a1)
	beq.w opforgeNativeCliParseUseItemsFail

opforgeNativeCliParseUseItemNameOk
	clr.b NativeCliIncludeTarget
	bsr.w opforgeNativeCliSkipLineWhitespace
	bsr.w opforgeNativeCliParseUseOptionalAlias
	tst.l d1
	bne.w opforgeNativeCliParseUseItemsFail
	moveq #0, d3
	tst.b NativeCliIncludeTarget
	beq.s opforgeNativeCliParseUseItemNoAliasFlag
	moveq #1, d3

opforgeNativeCliParseUseItemNoAliasFlag
	move.l d0, -(sp)
	move.w d5, d4
	bsr.w opforgeNativeCliRecordImportSelect
	tst.l d0
	bne.w opforgeNativeCliParseUseItemsFailPop
	bsr.w opforgeNativeCliEmitImportSelectRecord
	move.l (sp)+, d0
	addq.w #1, d7
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	beq.w opforgeNativeCliParseUseItemsFail
	cmpi.b #')', (a0)
	beq.s opforgeNativeCliParseUseItemsClose
	cmpi.b #',', (a0)
	bne.w opforgeNativeCliParseUseItemsFail
	addq.l #1, a0
	subq.l #1, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	bra.w opforgeNativeCliParseUseItemLoop

opforgeNativeCliParseUseItemsClose
	addq.l #1, a0
	subq.l #1, d0
	moveq #0, d1
	rts

opforgeNativeCliParseUseWildcard
	addq.l #1, a0
	subq.l #1, d0
	bsr.w opforgeNativeCliSkipLineWhitespace
	move.l d0, d6
	lea AsKeywordText, a1
	moveq #2, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.s opforgeNativeCliParseUseItemsFail
	move.l d6, d0
	tst.l d0
	beq.s opforgeNativeCliParseUseItemsFail
	cmpi.b #')', (a0)
	bne.s opforgeNativeCliParseUseItemsFail
	addq.l #1, a0
	subq.l #1, d0
	moveq #0, d3
	move.w d5, d4
	bsr.w opforgeNativeCliEmitImportWildcardRecord
	moveq #0, d1
	rts

opforgeNativeCliParseUseItemsFailPop
	addq.l #4, sp

opforgeNativeCliParseUseItemsFail
	moveq #1, d1
	rts

opforgeNativeCliRecordModule
	movem.l d1-d3/a0-a1, -(sp)
	moveq #0, d0
	move.w NativeCliModuleCount, d0
	cmpi.w #NATIVE_MODULE_TABLE_CAPACITY, d0
	bhs.w opforgeNativeCliRecordModuleFail
	move.w d0, d3
	lea NativeCliArgToken, a0
	lea NativeCliModuleNameTable, a1
	moveq #0, d1
	move.w d3, d1
	lsl.l #6, d1
	adda.l d1, a1
	bsr.w opforgeNativeCliCopyTokenBuffer

	moveq #0, d1
	move.w d3, d1
	add.w d1, d1
	lea NativeCliModuleFileIdTable, a1
	move.w #1, 0(a1, d1.l)
	lea NativeCliModuleDepthTable, a1
	move.w NativeCliModuleDepth, 0(a1, d1.l)

	moveq #0, d1
	move.w d3, d1
	lsl.l #2, d1
	lea NativeCliModuleLineTable, a1
	move.l NativeCliSourceLineNum, 0(a1, d1.l)

	tst.w NativeCliModuleCount
	bne.s opforgeNativeCliRecordModuleHaveRoot
	move.w d3, NativeCliRootModuleId

opforgeNativeCliRecordModuleHaveRoot
	move.w d3, NativeCliCurrentModuleId
	move.w NativeCliModuleCount, d0
	addq.w #1, d0
	move.w d0, NativeCliModuleCount
	move.w NativeCliModuleDepth, d0
	addq.w #1, d0
	move.w d0, NativeCliModuleDepth
	moveq #0, d0
	bra.s opforgeNativeCliRecordModuleReturn

opforgeNativeCliRecordModuleFail
	moveq #1, d0

opforgeNativeCliRecordModuleReturn
	movem.l (sp)+, d1-d3/a0-a1
	rts

opforgeNativeCliRecordImport
	movem.l d1-d3/a0-a1, -(sp)
	moveq #0, d0
	move.w NativeCliImportCount, d0
	cmpi.w #NATIVE_IMPORT_TABLE_CAPACITY, d0
	bhs.w opforgeNativeCliRecordImportFail
	move.w d0, d4
	moveq #0, d1
	move.w d4, d1
	add.w d1, d1
	lea NativeCliImportOwnerModuleTable, a1
	move.w NativeCliCurrentModuleId, 0(a1, d1.l)
	lea NativeCliImportModuleTable, a1
	clr.w 0(a1, d1.l)
	lea NativeCliImportFileIdTable, a1
	move.w #1, 0(a1, d1.l)

	moveq #0, d1
	move.w d4, d1
	lsl.l #2, d1
	lea NativeCliImportLineTable, a1
	move.l NativeCliSourceLineNum, 0(a1, d1.l)

	moveq #0, d1
	move.w d4, d1
	lsl.l #6, d1
	lea NativeCliImportAliasTable, a1
	adda.l d1, a1
	lea NativeCliIncludeTarget, a0
	bsr.w opforgeNativeCliCopyTokenBuffer

	move.w NativeCliImportCount, d0
	addq.w #1, d0
	move.w d0, NativeCliImportCount
	moveq #0, d0
	bra.s opforgeNativeCliRecordImportReturn

opforgeNativeCliRecordImportFail
	moveq #1, d0

opforgeNativeCliRecordImportReturn
	movem.l (sp)+, d1-d3/a0-a1
	rts

opforgeNativeCliRecordImportSelect
	movem.l d1-d3/a0-a1, -(sp)
	moveq #0, d0
	move.w NativeCliImportSelectCount, d0
	cmpi.w #NATIVE_IMPORT_SELECT_CAPACITY, d0
	bhs.w opforgeNativeCliRecordImportSelectFail
	move.w d0, d6
	moveq #0, d1
	move.w d6, d1
	add.w d1, d1
	lea NativeCliImportSelectImportTable, a1
	move.w d4, 0(a1, d1.l)
	lea NativeCliImportSelectFlagsTable, a1
	move.w d3, 0(a1, d1.l)

	moveq #0, d1
	move.w d6, d1
	lsl.l #6, d1
	lea NativeCliImportSelectNameTable, a1
	adda.l d1, a1
	lea NativeCliArgToken, a0
	bsr.w opforgeNativeCliCopyTokenBuffer

	moveq #0, d1
	move.w d6, d1
	lsl.l #6, d1
	lea NativeCliImportSelectAliasTable, a1
	adda.l d1, a1
	lea NativeCliIncludeTarget, a0
	bsr.w opforgeNativeCliCopyTokenBuffer

	move.w NativeCliImportSelectCount, d0
	addq.w #1, d0
	move.w d0, NativeCliImportSelectCount
	moveq #0, d0
	bra.s opforgeNativeCliRecordImportSelectReturn

opforgeNativeCliRecordImportSelectFail
	moveq #1, d0

opforgeNativeCliRecordImportSelectReturn
	movem.l (sp)+, d1-d3/a0-a1
	rts

opforgeNativeCliEmitImportRecord
	movem.l d0-d4/a0-a1, -(sp)
	move.l #UseImportText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea NativeCliImportOwnerModuleTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea NativeCliImportModuleTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea NativeCliImportFileIdTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	lsl.l #2, d0
	lea NativeCliImportLineTable, a0
	move.l 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportAliasPtr
	bsr.w opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w opforgeNativeCliPutDecU16
	tst.w d3
	beq.s opforgeNativeCliEmitImportRecordNewline
	bsr.w opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliImportAliasPtr
	move.l a0, d1
	bsr.w opforgeNativeCliPutStr

opforgeNativeCliEmitImportRecordNewline
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4/a0-a1
	rts

opforgeNativeCliEmitImportSelectRecord
	movem.l d0-d4/d6-d7/a0-a1, -(sp)
	move.l #UseSelectText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	moveq #0, d0
	move.w d7, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportSelectNamePtr
	bsr.w opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliImportSelectNamePtr
	move.l a0, d1
	bsr.w opforgeNativeCliPutStr
	bsr.w opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliImportSelectAliasPtr
	bsr.w opforgeNativeCliTokenLen
	move.w d0, d3
	bsr.w opforgeNativeCliPutDecU16
	tst.w d3
	beq.s opforgeNativeCliEmitImportSelectFlags
	bsr.w opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliImportSelectAliasPtr
	move.l a0, d1
	bsr.w opforgeNativeCliPutStr

opforgeNativeCliEmitImportSelectFlags
	bsr.w opforgeNativeCliPutSpace
	moveq #0, d0
	move.w d6, d0
	add.w d0, d0
	lea NativeCliImportSelectFlagsTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4/d6-d7/a0-a1
	rts

opforgeNativeCliEmitImportWildcardRecord
	movem.l d0-d4, -(sp)
	move.l #UseWildcardText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	moveq #0, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4
	rts

opforgeNativeCliResolveBareUseModule
	movem.l d2-d7/a0-a1, -(sp)
	clr.w d7

opforgeNativeCliResolveBareUseLoop
	move.w NativeCliModulePathCount, d0
	cmp.w d0, d7
	bhs.w opforgeNativeCliResolveBareUseFail
	moveq #0, d0
	move.w d7, d0
	lsl.l #8, d0
	lea NativeCliModulePathTable, a0
	adda.l d0, a0
	lea NativeCliIncludePath, a1
	bsr.w opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.w opforgeNativeCliResolveBareUseFail
	lea NativeCliArgToken, a0
	lea NativeCliIncludePath, a1
	bsr.w opforgeNativeCliAppendPathBuffer
	tst.l d0
	bne.w opforgeNativeCliResolveBareUseFail
	lea ModuleSourceExtensionText, a0
	lea NativeCliIncludePath, a1
	bsr.w opforgeNativeCliAppendPathBuffer
	tst.l d0
	bne.w opforgeNativeCliResolveBareUseFail
	lea NativeCliIncludePath, a0
	bsr.w opforgeNativeCliOpenInput
	tst.l d0
	bne.s opforgeNativeCliResolveBareUseFound
	addq.w #1, d7
	bra.w opforgeNativeCliResolveBareUseLoop

opforgeNativeCliResolveBareUseFound
	move.l d0, d1
	bsr.w opforgeNativeCliClose
	move.w NativeCliModuleCount, d6
	move.w d6, NativeCliResolvedModuleId
	move.w NativeCliSourceLineLen, d0
	move.w d0, NativeCliModuleSavedLineLen
	move.w NativeCliSawCr, d0
	move.w d0, NativeCliModuleSavedSawCr
	move.l NativeCliSourceLineNum, d0
	move.l d0, NativeCliModuleSavedLineNum
	lea NativeCliCurrentPath, a0
	lea NativeCliModuleSavedPath, a1
	bsr.w opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.w opforgeNativeCliResolveBareUseFail
	lea NativeCliIncludePath, a0
	lea NativeCliCurrentPath, a1
	bsr.w opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.w opforgeNativeCliResolveBareUseFail
	lea NativeCliIncludePath, a0
	move.w #1, NativeCliModuleResolveDepth
	bsr.w opforgeNativeCliTokenizeFileAtPath
	clr.w NativeCliModuleResolveDepth
	tst.l d0
	bne.s opforgeNativeCliResolveBareUseRestoreFail
	moveq #0, d1
	bra.s opforgeNativeCliResolveBareUseRestore

opforgeNativeCliResolveBareUseRestoreFail
	moveq #1, d1

opforgeNativeCliResolveBareUseRestore
	move.w NativeCliModuleSavedLineLen, d2
	move.w d2, NativeCliSourceLineLen
	move.w NativeCliModuleSavedSawCr, d2
	move.w d2, NativeCliSawCr
	move.l NativeCliModuleSavedLineNum, d2
	move.l d2, NativeCliSourceLineNum
	lea NativeCliModuleSavedPath, a0
	lea NativeCliCurrentPath, a1
	bsr.w opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s opforgeNativeCliResolveBareUseRestoreCopyFail
	tst.l d1
	bne.s opforgeNativeCliResolveBareUseReturn
	moveq #0, d0
	move.w NativeCliResolvedModuleId, d0
	bra.s opforgeNativeCliResolveBareUseReturn

opforgeNativeCliResolveBareUseRestoreCopyFail
	moveq #1, d1

opforgeNativeCliResolveBareUseFail
	moveq #1, d1

opforgeNativeCliResolveBareUseReturn
	movem.l (sp)+, d2-d7/a0-a1
	rts

opforgeNativeCliEmitModuleRecord
	movem.l d0-d4/a0-a1, -(sp)
	move.w d0, d4
	cmp.w NativeCliRootModuleId, d4
	bne.s opforgeNativeCliEmitModuleRecordDef
	move.l #ModRootText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr

opforgeNativeCliEmitModuleRecordDef
	move.l #ModDefText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea NativeCliModuleFileIdTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	lsl.l #2, d0
	lea NativeCliModuleLineTable, a0
	move.l 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	moveq #0, d0
	move.w d4, d0
	add.w d0, d0
	lea NativeCliModuleDepthTable, a0
	move.w 0(a0, d0.l), d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace

	bsr.w opforgeNativeCliModuleNamePtr
	bsr.w opforgeNativeCliTokenLen
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	bsr.w opforgeNativeCliModuleNamePtr
	move.l a0, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4/a0-a1
	rts

opforgeNativeCliEmitModuleCompatibility
	movem.l d0/d4/a0, -(sp)
	move.w d0, d4
	move.l #ModuleFoundText, d1
	bsr.w opforgeNativeCliPutStr
	bsr.w opforgeNativeCliModuleNamePtr
	move.l a0, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d0/d4/a0
	rts

opforgeNativeCliCloseModule
	movem.l d1-d4/a0-a1, -(sp)
	tst.w NativeCliModuleDepth
	beq.s opforgeNativeCliCloseModuleFail
	moveq #0, d0
	move.w NativeCliModuleDepth, d0
	subq.w #1, d0
	move.w d0, NativeCliModuleDepth
	moveq #0, d0
	move.w NativeCliCurrentModuleId, d0
	bsr.w opforgeNativeCliEmitModuleEndRecord
	bsr.w opforgeNativeCliRestoreParentModule
	moveq #0, d0
	bra.s opforgeNativeCliCloseModuleReturn

opforgeNativeCliCloseModuleFail
	moveq #1, d0

opforgeNativeCliCloseModuleReturn
	movem.l (sp)+, d1-d4/a0-a1
	rts

opforgeNativeCliEmitModuleEndRecord
	movem.l d0-d4/a0-a1, -(sp)
	move.w d0, d4
	move.l #ModEndText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	moveq #1, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	move.l NativeCliSourceLineNum, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	moveq #0, d0
	move.w NativeCliModuleDepth, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4/a0-a1
	rts

opforgeNativeCliRestoreParentModule
	movem.l d1-d3/a0, -(sp)
	tst.w NativeCliModuleDepth
	bne.s opforgeNativeCliRestoreParentModuleFind
	clr.w NativeCliCurrentModuleId
	bra.s opforgeNativeCliRestoreParentModuleReturn

opforgeNativeCliRestoreParentModuleFind
	move.w NativeCliModuleDepth, d0
	subq.w #1, d0
	move.w NativeCliModuleCount, d1
	beq.s opforgeNativeCliRestoreParentModuleClear
	subq.w #1, d1

opforgeNativeCliRestoreParentModuleLoop
	moveq #0, d2
	move.w d1, d2
	add.w d2, d2
	lea NativeCliModuleDepthTable, a0
	move.w 0(a0, d2.l), d3
	cmp.w d0, d3
	beq.s opforgeNativeCliRestoreParentModuleFound
	dbra d1, opforgeNativeCliRestoreParentModuleLoop

opforgeNativeCliRestoreParentModuleClear
	clr.w NativeCliCurrentModuleId
	bra.s opforgeNativeCliRestoreParentModuleReturn

opforgeNativeCliRestoreParentModuleFound
	move.w d1, NativeCliCurrentModuleId

opforgeNativeCliRestoreParentModuleReturn
	movem.l (sp)+, d1-d3/a0
	rts

opforgeNativeCliPutSpace
	move.l #SpaceText, d1
	bsr.w opforgeNativeCliPutStr
	rts

opforgeNativeCliModuleNamePtr
	moveq #0, d0
	move.w d4, d0
	lsl.l #6, d0
	lea NativeCliModuleNameTable, a0
	adda.l d0, a0
	rts

opforgeNativeCliImportAliasPtr
	moveq #0, d0
	move.w d4, d0
	lsl.l #6, d0
	lea NativeCliImportAliasTable, a0
	adda.l d0, a0
	rts

opforgeNativeCliImportSelectNamePtr
	moveq #0, d0
	move.w d6, d0
	lsl.l #6, d0
	lea NativeCliImportSelectNameTable, a0
	adda.l d0, a0
	rts

opforgeNativeCliImportSelectAliasPtr
	moveq #0, d0
	move.w d6, d0
	lsl.l #6, d0
	lea NativeCliImportSelectAliasTable, a0
	adda.l d0, a0
	rts

opforgeNativeCliTokenLen
	movem.l d1/a0, -(sp)
	moveq #0, d0
	move.l #TOKEN_BUFFER_CAPACITY - 1, d1

opforgeNativeCliTokenLenLoop
	tst.b (a0)+
	beq.s opforgeNativeCliTokenLenDone
	addq.w #1, d0
	dbra d1, opforgeNativeCliTokenLenLoop

opforgeNativeCliTokenLenDone
	movem.l (sp)+, d1/a0
	rts

opforgeNativeCliCopyIncludeTarget
	tst.l d0
	beq.w opforgeNativeCliCopyIncludeTargetFail
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #'"', d2
	beq.s opforgeNativeCliCopyIncludeTargetQuoted
	cmpi.b #39, d2
	beq.s opforgeNativeCliCopyIncludeTargetQuoted
	move.l #PATH_BUFFER_CAPACITY - 1, d6
	clr.l d5

opforgeNativeCliCopyIncludeTargetBareLoop
	tst.l d0
	beq.s opforgeNativeCliCopyIncludeTargetDone
	moveq #0, d2
	move.b (a0), d2
	cmpi.b #' ', d2
	beq.s opforgeNativeCliCopyIncludeTargetDone
	cmpi.b #9, d2
	beq.s opforgeNativeCliCopyIncludeTargetDone
	cmpi.b #';', d2
	beq.s opforgeNativeCliCopyIncludeTargetDone
	tst.l d6
	beq.s opforgeNativeCliCopyIncludeTargetFail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d5
	subq.l #1, d6
	bra.s opforgeNativeCliCopyIncludeTargetBareLoop

opforgeNativeCliCopyIncludeTargetQuoted
	move.b d2, d4
	addq.l #1, a0
	subq.l #1, d0
	move.l #PATH_BUFFER_CAPACITY - 1, d6
	clr.l d5

opforgeNativeCliCopyIncludeTargetQuotedLoop
	tst.l d0
	beq.s opforgeNativeCliCopyIncludeTargetFail
	moveq #0, d2
	move.b (a0), d2
	cmp.b d4, d2
	beq.s opforgeNativeCliCopyIncludeTargetDone
	tst.l d6
	beq.s opforgeNativeCliCopyIncludeTargetFail
	move.b d2, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.l #1, d5
	subq.l #1, d6
	bra.s opforgeNativeCliCopyIncludeTargetQuotedLoop

opforgeNativeCliCopyIncludeTargetDone
	tst.l d5
	beq.s opforgeNativeCliCopyIncludeTargetFail
	clr.b (a1)
	moveq #0, d0
	rts

opforgeNativeCliCopyIncludeTargetFail
	moveq #1, d0
	rts

; Expand the one-level native `.include` target and emit include report records.
opforgeNativeCliExpandIncludeTarget
	tst.w NativeCliIncludeDepth
	bne.w opforgeNativeCliExpandIncludeFail
	bsr.w opforgeNativeCliResolveIncludePath
	tst.l d0
	bne.w opforgeNativeCliExpandIncludeFail

	move.w NativeCliSourceLineLen, d0
	move.w d0, NativeCliSavedLineLen
	move.w NativeCliSawCr, d0
	move.w d0, NativeCliSavedSawCr
	move.l NativeCliSourceLineNum, d0
	move.l d0, NativeCliSavedLineNum
	lea NativeCliCurrentPath, a0
	lea NativeCliSavedPath, a1
	bsr.w opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.w opforgeNativeCliExpandIncludeFail

	move.l #IncludeStageText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #IncludeRootText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliIncludeRootPath, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #IncludeFileText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliIncludePath, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #IncludeEnterText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliCurrentPath, d1
	bsr.w opforgeNativeCliPutStr
	move.l #SpaceText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliIncludePath, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr

	move.w #NATIVE_INCLUDE_DEPTH_LIMIT, NativeCliIncludeDepth
	lea NativeCliIncludePath, a0
	lea NativeCliCurrentPath, a1
	bsr.w opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s opforgeNativeCliExpandIncludeRestoreFail
	lea NativeCliIncludePath, a0
	bsr.w opforgeNativeCliTokenizeFileAtPath
	tst.l d0
	bne.s opforgeNativeCliExpandIncludeRestoreFail

	move.l #IncludeLeaveText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #IncludeOkText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	bra.s opforgeNativeCliExpandIncludeRestore

opforgeNativeCliExpandIncludeRestoreFail
	move.l #IncludeFailureText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0

opforgeNativeCliExpandIncludeRestore
	move.w NativeCliSavedLineLen, d1
	move.w d1, NativeCliSourceLineLen
	move.w NativeCliSavedSawCr, d1
	move.w d1, NativeCliSawCr
	move.l NativeCliSavedLineNum, d1
	move.l d1, NativeCliSourceLineNum
	lea NativeCliSavedPath, a0
	lea NativeCliCurrentPath, a1
	bsr.w opforgeNativeCliCopyPathBuffer
	clr.w NativeCliIncludeDepth
	rts

opforgeNativeCliExpandIncludeFail
	move.l #IncludeFailureText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	rts

opforgeNativeCliResolveIncludePath
	lea NativeCliCurrentPath, a0
	lea NativeCliIncludeRootPath, a1
	bsr.w opforgeNativeCliCopyPathRoot
	tst.l d0
	bne.w opforgeNativeCliResolveIncludeFail
	lea NativeCliIncludeTarget, a0
	bsr.w opforgeNativeCliPathIsAbsolute
	tst.l d0
	beq.s opforgeNativeCliResolveIncludeRelative
	lea NativeCliIncludeTarget, a0
	lea NativeCliIncludePath, a1
	bsr.w opforgeNativeCliCopyPathBuffer
	rts

opforgeNativeCliResolveIncludeRelative
	lea NativeCliIncludeRootPath, a0
	lea NativeCliIncludePath, a1
	bsr.w opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s opforgeNativeCliResolveIncludeFail
	lea NativeCliIncludeTarget, a0
	lea NativeCliIncludePath, a1
	bsr.w opforgeNativeCliAppendPathBuffer
	rts

opforgeNativeCliResolveIncludeFail
	moveq #1, d0
	rts

opforgeNativeCliPathIsAbsolute
	moveq #0, d0

opforgeNativeCliPathIsAbsoluteLoop
	move.b (a0)+, d1
	beq.s opforgeNativeCliPathIsAbsoluteNo
	cmpi.b #':', d1
	beq.s opforgeNativeCliPathIsAbsoluteYes
	bra.s opforgeNativeCliPathIsAbsoluteLoop

opforgeNativeCliPathIsAbsoluteYes
	moveq #1, d0
	rts

opforgeNativeCliPathIsAbsoluteNo
	moveq #0, d0
	rts

opforgeNativeCliCopyPathRoot
	movem.l d2-d6/a2, -(sp)
	movea.l a0, a2
	clr.l d5
	clr.l d6

opforgeNativeCliCopyPathRootScan
	move.b (a2)+, d2
	beq.s opforgeNativeCliCopyPathRootCopy
	addq.l #1, d5
	cmpi.b #':', d2
	beq.s opforgeNativeCliCopyPathRootMark
	cmpi.b #'/', d2
	bne.s opforgeNativeCliCopyPathRootScan

opforgeNativeCliCopyPathRootMark
	move.l d5, d6
	bra.s opforgeNativeCliCopyPathRootScan

opforgeNativeCliCopyPathRootCopy
	movea.l a0, a2
	move.l #PATH_BUFFER_CAPACITY - 1, d4
	tst.l d6
	beq.s opforgeNativeCliCopyPathRootDone

opforgeNativeCliCopyPathRootCopyLoop
	tst.l d4
	beq.s opforgeNativeCliCopyPathRootFail
	move.b (a2)+, d3
	move.b d3, (a1)+
	subq.l #1, d6
	subq.l #1, d4
	tst.l d6
	bne.s opforgeNativeCliCopyPathRootCopyLoop

opforgeNativeCliCopyPathRootDone
	clr.b (a1)
	moveq #0, d0
	bra.s opforgeNativeCliCopyPathRootReturn

opforgeNativeCliCopyPathRootFail
	moveq #1, d0

opforgeNativeCliCopyPathRootReturn
	movem.l (sp)+, d2-d6/a2
	rts

opforgeNativeCliCopyPathBuffer
	move.l #PATH_BUFFER_CAPACITY - 1, d6

opforgeNativeCliCopyPathBufferLoop
	move.b (a0)+, d2
	move.b d2, (a1)+
	beq.s opforgeNativeCliCopyPathBufferOk
	subq.l #1, d6
	bne.s opforgeNativeCliCopyPathBufferLoop
	clr.b -(a1)
	moveq #1, d0
	rts

opforgeNativeCliCopyPathBufferOk
	moveq #0, d0
	rts

opforgeNativeCliAppendPathBuffer
	move.l #PATH_BUFFER_CAPACITY - 1, d6

opforgeNativeCliAppendPathBufferFindEnd
	tst.b (a1)
	beq.s opforgeNativeCliAppendPathBufferCopy
	addq.l #1, a1
	subq.l #1, d6
	beq.s opforgeNativeCliAppendPathBufferFail
	bra.s opforgeNativeCliAppendPathBufferFindEnd

opforgeNativeCliAppendPathBufferCopy
	move.b (a0)+, d2
	move.b d2, (a1)+
	beq.s opforgeNativeCliAppendPathBufferOk
	subq.l #1, d6
	bne.s opforgeNativeCliAppendPathBufferCopy

opforgeNativeCliAppendPathBufferFail
	clr.b -(a1)
	moveq #1, d0
	rts

opforgeNativeCliAppendPathBufferOk
	moveq #0, d0
	rts

; Print unsigned 16-bit D0 as decimal through the CLI stdout path.
opforgeNativeCliPutDecU16
	movem.l d1-d6/a0-a1, -(sp)
	andi.l #$0000FFFF, d0
	lea DecimalPowers, a0
	moveq #4, d6
	clr.w d5

opforgeNativeCliPutDecPowerLoop
	moveq #0, d3
	move.w (a0)+, d2

opforgeNativeCliPutDecDigitLoop
	cmp.w d2, d0
	bcs.s opforgeNativeCliPutDecMaybeEmit
	sub.w d2, d0
	addq.w #1, d3
	bra.s opforgeNativeCliPutDecDigitLoop

opforgeNativeCliPutDecMaybeEmit
	tst.w d3
	bne.s opforgeNativeCliPutDecEmit
	tst.w d5
	bne.s opforgeNativeCliPutDecEmit
	cmpi.w #1, d2
	bne.s opforgeNativeCliPutDecNext

opforgeNativeCliPutDecEmit
	move.w #1, d5
	addi.b #'0', d3
	lea NativeCliDecimalChar, a1
	move.b d3, (a1)
	clr.b 1(a1)
	move.l #NativeCliDecimalChar, d1
	bsr.w opforgeNativeCliPutStr

opforgeNativeCliPutDecNext
	dbra d6, opforgeNativeCliPutDecPowerLoop
	movem.l (sp)+, d1-d6/a0-a1
	rts

; Print unsigned 32-bit D0 as `$XXXXXXXX`.
opforgeNativeCliPutHexU32
	movem.l d0-d4/a0-a2, -(sp)
	move.l d0, -(sp)
	lea NativeCliHexBuffer, a1
	move.b #'$', (a1)+
	lea HexDigitsText, a0
	movea.l sp, a2
	moveq #3, d4

opforgeNativeCliPutHexLoop
	moveq #0, d1
	move.b (a2)+, d1
	move.l d1, d2
	lsr.b #4, d2
	move.b 0(a0, d2.l), (a1)+
	andi.b #$0F, d1
	move.b 0(a0, d1.l), (a1)+
	dbra d4, opforgeNativeCliPutHexLoop
	clr.b (a1)
	addq.l #4, sp
	move.l #NativeCliHexBuffer, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4/a0-a2
	rts

; Initialize transitional native assembly-session state for the current CLI run.
opforgeNativeCliInitAssemblySession
	movem.l d0-d1/a0-a1, -(sp)
	lea opasmEngineAssemblySessionStart.l, a0
	move.l #NATIVE_ASSEMBLY_SESSION_BYTES, d0
	bsr.w opforgeNativeCliClearBytes
	lea NativeCliCpuName, a0
	tst.b (a0)
	bne.s opforgeNativeCliInitAssemblySessionHaveCpu
	lea DefaultCpuName, a0

opforgeNativeCliInitAssemblySessionHaveCpu
	lea opasmEngineSessionCpuName, a1
	bsr.w opforgeNativeCliCopySessionCpuName
	movem.l (sp)+, d0-d1/a0-a1
	moveq #0, d0
	rts

opforgeNativeCliCopySessionCpuName
	move.l #TOKEN_BUFFER_CAPACITY - 1, d0

opforgeNativeCliCopySessionCpuNameLoop
	move.b (a0)+, d1
	move.b d1, (a1)+
	beq.s opforgeNativeCliCopySessionCpuNameDone
	subq.l #1, d0
	bne.s opforgeNativeCliCopySessionCpuNameLoop
	clr.b -(a1)

opforgeNativeCliCopySessionCpuNameDone
	rts

; Record the current logical source line in the session tables.
opforgeNativeCliRecordSourceLine
	movem.l d0/a0, -(sp)
	moveq #0, d0
	move.w opasmEngineSourceRecordCount.l, d0
	cmpi.w #NATIVE_SOURCE_RECORD_CAPACITY, d0
	bhs.s opforgeNativeCliRecordSourceLineDone
	lsl.l #2, d0
	lea opasmEngineSourceLineNumTable.l, a0
	move.l NativeCliSourceLineNum, 0(a0, d0.l)
	moveq #0, d0
	move.w opasmEngineSourceRecordCount.l, d0
	add.w d0, d0
	lea opasmEngineSourceLineLenTable.l, a0
	move.w NativeCliSourceLineLen, 0(a0, d0.l)
	addq.w #1, opasmEngineSourceRecordCount.l

opforgeNativeCliRecordSourceLineDone
	movem.l (sp)+, d0/a0
	rts

; Emit the current session summary records into the OPFORGE-NATIVE report.
opforgeNativeCliEmitAssemblySessionSummary
	movem.l d0-d2/a0-a1, -(sp)
	move.l #SessionStageText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #SessionCpuText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #opasmEngineSessionCpuName, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #SessionPassText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineSessionPass.l, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #SessionOriginText, d1
	bsr.w opforgeNativeCliPutStr
	move.l opasmEngineSessionOrigin.l, d0
	bsr.w opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #SessionPcText, d1
	bsr.w opforgeNativeCliPutStr
	move.l opasmEngineSessionCurrentPc.l, d0
	bsr.w opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #SessionSourceCountText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineSourceRecordCount.l, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #SessionStmtCountText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineStmtCount.l, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #SessionLabelCountText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineLabelCount.l, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #SessionImageBytesText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineImageByteCount.l, d0
	bsr.w opforgeNativeCliPutDecU16
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #SessionReadyText, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d0-d2/a0-a1
	rts

; Run the current transitional two-pass native assembler path through opasm.
opforgeNativeCliRunTwoPassEngine
	bsr.w opforgeNativeCliBuildOpasmEngineContext
	jsr opasmEngineRunTwoPassV1
	rts

opforgeNativeCliBuildOpasmEngineContext
	lea NativeCliOpasmEngineContext.l, a4
	move.l #opasmEngineSessionPass, (a4)+
	move.l #opasmEngineStmtCount, (a4)+
	move.l #NativeCliBinRequested, (a4)+
	move.l #opforgeNativeCliOpasmPassOneBegin, (a4)+
	move.l #opforgeNativeCliOpasmPassTwoBegin, (a4)+
	move.l #opforgeNativeCliOpasmPassOneOk, (a4)+
	move.l #opforgeNativeCliOpasmPassTwoOk, (a4)+
	move.l #opforgeNativeCliPassOneRecordLabel, (a4)+
	move.l #opforgeNativeCliPassAdvancePc, (a4)+
	move.l #opforgeNativeCliPassTwoEmitImageBytes, (a4)+
	lea NativeCliOpasmEngineContext.l, a4
	rts

opforgeNativeCliOpasmPassOneBegin
	movem.l d0-d1, -(sp)
	move.l #NativePassOneText, d1
	bsr.w opforgeNativeCliPutStr
	clr.w opasmEngineLabelCount.l
	lea opasmEngineLabelFinalizedTable.l, a0
	moveq #NATIVE_LABEL_TABLE_CAPACITY - 1, d0

opforgeNativeCliPassOneClearLabelFinalizedLoop
	clr.b (a0)+
	dbf d0, opforgeNativeCliPassOneClearLabelFinalizedLoop
	clr.w opasmEngineImageByteCount.l
	move.l #$00000800, opasmEngineSessionOrigin.l
	move.l opasmEngineSessionOrigin.l, d0
	move.l d0, opasmEngineSessionCurrentPc.l
	movem.l (sp)+, d0-d1
	moveq #0, d0
	rts

opforgeNativeCliOpasmPassOneOk
	movem.l d1, -(sp)
	move.l #NativePassOneOkText, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d1
	moveq #0, d0
	rts

opforgeNativeCliOpasmPassTwoBegin
	movem.l d0-d1, -(sp)
	move.l #NativePassTwoText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineLabelCount.l, d0
	subq.w #1, d0
	bmi.s opforgeNativeCliPassTwoFinalizeLabelsDone
	lea opasmEngineLabelFinalizedTable.l, a0

opforgeNativeCliPassTwoFinalizeLabelLoop
	move.b #1, (a0)+
	dbf d0, opforgeNativeCliPassTwoFinalizeLabelLoop

opforgeNativeCliPassTwoFinalizeLabelsDone
	clr.w opasmEngineImageByteCount.l
	move.l opasmEngineSessionOrigin.l, d0
	move.l d0, opasmEngineSessionCurrentPc.l
	movem.l (sp)+, d0-d1
	moveq #0, d0
	rts

opforgeNativeCliOpasmPassTwoOk
	movem.l d1, -(sp)
	move.l #NativePassTwoOkText, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d1
	moveq #0, d0
	rts

; Record a statement label at the current PC, rejecting duplicates.
opforgeNativeCliPassOneRecordLabel
	movem.l d1-d7/a0-a2, -(sp)
	move.l d0, d7
	lsl.l #6, d7
	lea opasmEngineStmtLabelNameTable.l, a1
	adda.l d7, a1
	tst.b (a1)
	beq.w opforgeNativeCliPassOneRecordLabelOk
	moveq #0, d0
	move.w opasmEngineLabelCount.l, d0
	cmpi.w #NATIVE_LABEL_TABLE_CAPACITY, d0
	bhs.w opforgeNativeCliPassOneRecordLabelFail
	moveq #0, d6

opforgeNativeCliPassOneDuplicateLoop
	move.w opasmEngineLabelCount.l, d0
	cmp.w d0, d6
	bhs.s opforgeNativeCliPassOneStoreLabel
	moveq #0, d5
	move.w d6, d5
	lsl.l #6, d5
	lea opasmEngineLabelNameTable.l, a0
	adda.l d5, a0
	moveq #0, d0
	move.l d7, d5
	lsr.l #6, d5
	add.w d5, d5
	lea opasmEngineStmtLabelLenTable.l, a2
	move.w 0(a2, d5.l), d0
	bne.s opforgeNativeCliPassOneDuplicateHaveLabelLen
	move.l a0, d3
	movea.l a1, a0
	bsr.w opforgeNativeCliTokenLen
	movea.l d3, a0

opforgeNativeCliPassOneDuplicateHaveLabelLen
	bsr.w opforgeNativeCliLabelEquals
	tst.l d0
	bne.w opforgeNativeCliPassOneDuplicate
	addq.w #1, d6
	bra.s opforgeNativeCliPassOneDuplicateLoop

opforgeNativeCliPassOneStoreLabel
	moveq #0, d6
	move.w opasmEngineLabelCount.l, d6
	move.l d6, d5
	lsl.l #2, d5
	lea opasmEngineLabelValueTable.l, a0
	move.l opasmEngineSessionCurrentPc.l, 0(a0, d5.l)
	lea opasmEngineLabelFinalizedTable.l, a0
	clr.b 0(a0, d6.l)
	move.l d6, d5
	lsl.l #6, d5
	lea opasmEngineLabelNameTable.l, a0
	adda.l d5, a0
	move.l a0, d2
	move.l a0, d4
	move.l a1, d3
	movea.l a1, a2
	movea.l a0, a1
	movea.l a2, a0
	moveq #0, d0
	move.l d7, d5
	lsr.l #6, d5
	add.w d5, d5
	lea opasmEngineStmtLabelLenTable.l, a2
	move.w 0(a2, d5.l), d0
	bne.s opforgeNativeCliPassOneStoreHaveLabelLen
	movea.l d3, a0
	bsr.w opforgeNativeCliTokenLen

opforgeNativeCliPassOneStoreHaveLabelLen
	bsr.w opforgeNativeCliCopyFixedString
	clr.b (a1)
	addq.w #1, opasmEngineLabelCount.l
	move.l #NativeLabelText, d1
	bsr.w opforgeNativeCliPutStr
	move.l d4, d1
	bsr.w opforgeNativeCliPutStr
	bsr.w opforgeNativeCliPutSpace
	move.l opasmEngineSessionCurrentPc.l, d0
	bsr.w opforgeNativeCliPutHexU32
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	bra.s opforgeNativeCliPassOneRecordLabelOk

opforgeNativeCliPassOneDuplicate
	move.l #NativeDuplicateLabelText, d1
	bsr.w opforgeNativeCliPutStr
	move.l a1, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr

opforgeNativeCliPassOneRecordLabelFail
	moveq #1, d0
	bra.s opforgeNativeCliPassOneRecordLabelReturn

opforgeNativeCliPassOneRecordLabelOk
	moveq #0, d0

opforgeNativeCliPassOneRecordLabelReturn
	movem.l (sp)+, d1-d7/a0-a2
	rts

; Encode one statement through tkpkg and append resulting bytes to image buffer.
opforgeNativeCliPassTwoEmitImageBytes
	movem.l d1-d6/a0-a4, -(sp)
	move.w d0, d6
	moveq #0, d0
	move.w d6, d0
	lsl.l #6, d0
	lea opasmEngineStmtMnemNameTable.l, a0
	adda.l d0, a0
	move.l a0, d5
	moveq #0, d3
	move.w d6, d3
	add.w d3, d3
	lea opasmEngineStmtMnemLenTable.l, a1
	moveq #0, d0
	move.w 0(a1, d3.l), d0
	bne.s opforgeNativeCliPassTwoEmitHaveMlen
	movea.l d5, a0
	bsr.w opforgeNativeCliTokenLen

opforgeNativeCliPassTwoEmitHaveMlen
	move.w d0, d4
	beq.w opforgeNativeCliPassTwoEmitOk
	move.w d6, d0
	move.w d4, d1
	movea.l d5, a0
	bsr.w opforgeNativeCliStatementMnemDuplicatesLabel
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitOk
	movea.l d5, a0
	move.w d4, d0
	lea OrgMnemonicText, a1
	moveq #4, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitOk
	movea.l d5, a0
	move.w d4, d0
	lea CpuMnemonicText, a1
	moveq #4, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitOk
	movea.l d5, a0
	move.w d4, d0
	lea EndMnemonicText, a1
	moveq #4, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitOk
	bsr.w opforgeNativeCliPrepareEncodeSelectedRequestForStatement
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitReturn
	tst.w NativeCliEvalRequestLen.l
	beq.w opforgeNativeCliPassTwoEmitOk
	bsr.w opforgeNativeCliPrepareEvaluateExpressionExtension
	lea controlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliEvalRequestLen.l, d1
	bsr.w opforgeNativeCliWriteInputWindow
	move.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, d0
	move.w #NATIVE_EVAL_EXPR_EXTENSION_BYTES, d1
	bsr.w opforgeNativeCliWriteExtensionWindow
	moveq #ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION, d0
	jsr tkpkgServiceDispatchV1
	lea controlBlockV1, a0
	bsr.w opforgeNativeCliReadStatus
	tst.b d0
	bne.w opforgeNativeCliPassTwoEmitServiceFail
	lea controlBlockV1, a0
	bsr.w opforgeNativeCliReadOutputLen
	tst.w d0
	beq.w opforgeNativeCliPassTwoEmitOk
	move.w d0, d6
	move.l #NativeSelectorStatusOkText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w opasmEngineImageByteCount.l, d0
	add.w d6, d0
	cmpi.w #NATIVE_IMAGE_BUFFER_CAPACITY, d0
	bhi.w opforgeNativeCliPassTwoEmitFail
	moveq #0, d0
	move.w opasmEngineImageByteCount.l, d0
	lea opasmEngineImageBuffer.l, a0
	adda.l d0, a0
	lea lastErrorBuffer, a1
	move.w d6, d1

opforgeNativeCliPassTwoCopyEncodedLoop
	move.b (a1)+, (a0)+
	subq.w #1, d1
	bne.s opforgeNativeCliPassTwoCopyEncodedLoop
	add.w d6, opasmEngineImageByteCount.l

opforgeNativeCliPassTwoEmitOk
	moveq #0, d0
	bra.s opforgeNativeCliPassTwoEmitReturn

opforgeNativeCliPassTwoEmitFail
	move.l #NativeImageCapacityText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	bra.s opforgeNativeCliPassTwoEmitReturn

opforgeNativeCliPassTwoEmitServiceFail
	move.w d6, d0
	bsr.w opforgeNativeCliStatementLooksBareColumnOne
	tst.l d0
	bne.w opforgeNativeCliPassTwoEmitOk
	lea controlBlockV1, a0
	bsr.w opforgeNativeCliReadLastErrorLen
	tst.w d0
	beq.s opforgeNativeCliPassTwoEmitServiceFailReturn
	lea lastErrorBuffer, a1
	clr.b 0(a1, d0.W)
	bsr.w opforgeNativeCliPassTwoEmitSelectorDiagnostic
	tst.l d0
	bne.s opforgeNativeCliPassTwoEmitServiceFailReturn
	move.l #lastErrorBuffer, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr

opforgeNativeCliPassTwoEmitServiceFailReturn
	moveq #1, d0

opforgeNativeCliPassTwoEmitReturn
	movem.l (sp)+, d1-d6/a0-a4
	rts

opforgeNativeCliPassTwoEmitSelectorDiagnostic
	lea lastErrorBuffer, a0
	lea NativeSelectorUnknownRawText, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.s opforgeNativeCliPassTwoEmitUnknownMnemonic
	lea lastErrorBuffer, a0
	lea NativeSelectorUnsupportedRawText, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.s opforgeNativeCliPassTwoEmitUnsupportedAddressing
	lea lastErrorBuffer, a0
	lea NativeSelectorOperandRawText, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.s opforgeNativeCliPassTwoEmitOperandError
	lea lastErrorBuffer, a0
	lea NativeSelectedOperandCompileRawText, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.s opforgeNativeCliPassTwoEmitOperandError
	moveq #0, d0
	rts

opforgeNativeCliPassTwoEmitUnknownMnemonic
	move.l #NativeUnknownMnemonicText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	rts

opforgeNativeCliPassTwoEmitUnsupportedAddressing
	move.l #NativeUnsupportedAddressingText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	rts

opforgeNativeCliPassTwoEmitOperandError
	move.l #NativeUnresolvedLabelText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	rts

; Resolve one statement operand through tkpkg evaluate_expression.
opforgeNativeCliReadOperandValueForStatement
	movem.l d1-d2/d4-d7/a0-a2, -(sp)
	bsr.w opforgeNativeCliLoadStatementExprMetadata
	tst.w NativeCliStmtExprFound
	bne.s opforgeNativeCliReadOperandLoadSourceLine
	bra.w opforgeNativeCliReadOperandStoredText

opforgeNativeCliReadOperandLoadSourceLine
	bsr.w opforgeNativeCliLoadStatementSourceLineText
	tst.l d0
	bne.s opforgeNativeCliReadOperandHaveText
	bra.w opforgeNativeCliReadOperandFail

opforgeNativeCliReadOperandStoredText
	clr.l d3
	moveq #0, d0
	move.w d7, d0
	add.w d0, d0
	lea opasmEngineStmtOperandLenTable.l, a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	bne.s opforgeNativeCliReadOperandStoredTextReady
	bra.w opforgeNativeCliReadOperandFail

opforgeNativeCliReadOperandStoredTextReady
	moveq #0, d0
	move.w d7, d0
	lsl.l #6, d0
	lea opasmEngineStmtOperandNameTable.l, a0
	adda.l d0, a0
	move.l d1, d0

opforgeNativeCliReadOperandHaveText
	tst.w NativeCliStmtExprFound
	bne.s opforgeNativeCliReadOperandPrepareRequest
	bsr.w opforgeNativeCliSkipLineWhitespace
	tst.l d0
	bne.s opforgeNativeCliReadOperandPrepareRequest
	bra.w opforgeNativeCliReadOperandFail

opforgeNativeCliReadOperandPrepareRequest
	bsr.w opforgeNativeCliPrepareEvaluateExpressionRequest
	tst.l d0
	beq.s opforgeNativeCliReadOperandPrepareExtension
	bra.w opforgeNativeCliReadOperandFail

opforgeNativeCliReadOperandPrepareExtension
	bsr.w opforgeNativeCliPrepareEvaluateExpressionExtension
	lea controlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliEvalRequestLen, d1
	bsr.w opforgeNativeCliWriteInputWindow
	move.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, d0
	move.w #NATIVE_EVAL_EXPR_EXTENSION_BYTES, d1
	bsr.w opforgeNativeCliWriteExtensionWindow
	moveq #ENTRY_ORD_EVALUATE_EXPRESSION, d0
	jsr tkpkgServiceDispatchV1
	bsr.w opforgeNativeCliReadStatus
	beq.s opforgeNativeCliReadOperandReadValue
	bra.w opforgeNativeCliReadOperandFail

opforgeNativeCliReadOperandReadValue
	bsr.w opforgeNativeCliReadEvaluateExpressionValue
	cmpi.b #1, d5
	bne.s opforgeNativeCliReadOperandOk
	cmpi.l #$000000FF, d3
	bls.s opforgeNativeCliReadOperandOk

opforgeNativeCliReadOperandFail
	move.l #NativeUnresolvedLabelText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #1, d0
	bra.s opforgeNativeCliReadOperandReturn

opforgeNativeCliReadOperandOk
	moveq #0, d0

opforgeNativeCliReadOperandReturn
	movem.l (sp)+, d1-d2/d4-d7/a0-a2
	rts

opforgeNativeCliLoadStatementSourceLineText
	moveq #0, d0
	move.w d7, d0
	add.w d0, d0
	lea opasmEngineStmtSourceLineLenTable.l, a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	beq.s opforgeNativeCliLoadStatementSourceLineTextFail
	moveq #0, d0
	move.w d7, d0
	lsl.l #8, d0
	add.l d0, d0
	lea opasmEngineStmtSourceLineTextTable.l, a0
	adda.l d0, a0
	move.l d1, d0
	rts

opforgeNativeCliLoadStatementSourceLineTextFail
	clr.l d0
	rts

opforgeNativeCliLoadStatementExprText
	moveq #0, d0
	move.w d7, d0
	add.w d0, d0
	lea opasmEngineStmtSourceLineLenTable.l, a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	beq.s opforgeNativeCliLoadStatementExprTextFail
	moveq #0, d0
	move.w d7, d0
	lsl.l #8, d0
	add.l d0, d0
	lea opasmEngineStmtSourceLineTextTable.l, a0
	adda.l d0, a0
	move.l NativeCliStmtExprSpanStart, d2
	beq.s opforgeNativeCliLoadStatementExprTextFail
	move.l NativeCliStmtExprSpanEnd, d0
	cmp.l d2, d0
	bls.s opforgeNativeCliLoadStatementExprTextFail
	subq.l #1, d2
	cmp.l d1, d2
	bhs.s opforgeNativeCliLoadStatementExprTextFail
	adda.l d2, a0
	sub.l d2, d1
	move.l NativeCliStmtExprSpanEnd, d0
	sub.l NativeCliStmtExprSpanStart, d0
	cmp.l d1, d0
	bls.s opforgeNativeCliLoadStatementExprTextDone
	move.l d1, d0

opforgeNativeCliLoadStatementExprTextDone
	rts

opforgeNativeCliLoadStatementExprTextFail
	clr.l d0
	rts

opforgeNativeCliLoadStatementExprMetadata
	moveq #0, d0
	move.w d7, d0
	add.w d0, d0
	lea opasmEngineStmtExprFlagsTable.l, a0
	tst.w 0(a0, d0.l)
	beq.s opforgeNativeCliLoadStatementExprMetadataEmpty
	lsr.w #1, d0
	lsl.l #2, d0
	lea opasmEngineStmtExprOperandIndexTable.l, a0
	move.l 0(a0, d0.l), NativeCliStmtExprOperandIndex
	lea opasmEngineStmtExprSlotIndexTable.l, a0
	move.l 0(a0, d0.l), NativeCliStmtExprSlotIndex
	lea opasmEngineStmtExprStartTokenTable.l, a0
	move.l 0(a0, d0.l), NativeCliStmtExprStartToken
	lea opasmEngineStmtExprEndTokenTable.l, a0
	move.l 0(a0, d0.l), NativeCliStmtExprEndToken
	lea opasmEngineStmtExprSpanLineTable.l, a0
	move.l 0(a0, d0.l), NativeCliStmtExprSpanLine
	lea opasmEngineStmtExprSpanStartTable.l, a0
	move.l 0(a0, d0.l), NativeCliStmtExprSpanStart
	lea opasmEngineStmtExprSpanEndTable.l, a0
	move.l 0(a0, d0.l), NativeCliStmtExprSpanEnd
	move.w #1, NativeCliStmtExprFound
	rts

opforgeNativeCliLoadStatementExprMetadataEmpty
	clr.l NativeCliStmtExprOperandIndex
	clr.l NativeCliStmtExprSlotIndex
	clr.l NativeCliStmtExprStartToken
	clr.l NativeCliStmtExprEndToken
	clr.l NativeCliStmtExprSpanLine
	clr.l NativeCliStmtExprSpanStart
	clr.l NativeCliStmtExprSpanEnd
	clr.w NativeCliStmtExprFound
	rts

; Advance current PC for statement index D0, handling `.org` specially.
opforgeNativeCliPassAdvancePc
	movem.l d0-d7/a0-a3, -(sp)
	move.l d0, d7
	lsl.l #6, d0
	lea opasmEngineStmtMnemNameTable.l, a0
	adda.l d0, a0
	move.l a0, d5
	move.l d0, d4
	lsr.l #6, d4
	add.w d4, d4
	lea opasmEngineStmtMnemLenTable.l, a1
	moveq #0, d0
	move.w 0(a1, d4.l), d0
	bne.s opforgeNativeCliPassAdvanceHaveMlen
	movea.l d5, a0
	bsr.w opforgeNativeCliTokenLen

opforgeNativeCliPassAdvanceHaveMlen
	move.w d0, d6
	beq.w opforgeNativeCliPassAdvanceDone
	move.w d7, d0
	move.w d6, d1
	movea.l d5, a0
	bsr.w opforgeNativeCliStatementMnemDuplicatesLabel
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceDone
	lea OrgMnemonicText, a1
	moveq #4, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceOrg
	movea.l d5, a0
	move.w d6, d0
	lea CpuMnemonicText, a1
	moveq #4, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceDone
	movea.l d5, a0
	move.w d6, d0
	lea EndMnemonicText, a1
	moveq #4, d1
	bsr.w opforgeNativeCliLineStartsWith
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceDone
	moveq #0, d0
	move.w d7, d0
	bsr.w opforgeNativeCliTrySelectedEncodeSizeForStatement
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceFail
	cmpi.w #1, d1
	beq.w opforgeNativeCliPassAdvanceOne
	cmpi.w #2, d1
	beq.w opforgeNativeCliPassAdvanceTwo
	cmpi.w #3, d1
	beq.w opforgeNativeCliPassAdvanceThree
	bra.w opforgeNativeCliPassAdvanceDone

opforgeNativeCliPassAdvanceOrg
	move.w d4, d7
	moveq #2, d5
	bsr.w opforgeNativeCliReadOperandValueForStatement
	tst.l d0
	beq.s opforgeNativeCliPassAdvanceOrgOk
	move.l #NativeBadOrgText, d1
	bsr.w opforgeNativeCliPutStr
	movem.l (sp)+, d0-d7/a0-a3
	moveq #1, d0
	rts

opforgeNativeCliPassAdvanceOrgOk
	move.l d3, opasmEngineSessionOrigin.l
	move.l opasmEngineSessionOrigin.l, d0
	move.l d0, opasmEngineSessionCurrentPc.l
	bra.w opforgeNativeCliPassAdvanceDone

opforgeNativeCliPassAdvanceOne
	addq.l #1, opasmEngineSessionCurrentPc.l
	bra.w opforgeNativeCliPassAdvanceDone

opforgeNativeCliPassAdvanceTwo
	addq.l #2, opasmEngineSessionCurrentPc.l
	bra.w opforgeNativeCliPassAdvanceDone

opforgeNativeCliPassAdvanceThree
	addq.l #3, opasmEngineSessionCurrentPc.l
	bra.w opforgeNativeCliPassAdvanceDone

opforgeNativeCliPassAdvanceFail
	move.w d7, d0
	bsr.w opforgeNativeCliStatementLooksBareColumnOne
	tst.l d0
	bne.w opforgeNativeCliPassAdvanceDone
	movem.l (sp)+, d0-d7/a0-a3
	moveq #1, d0
	rts

opforgeNativeCliPassAdvanceDone
	movem.l (sp)+, d0-d7/a0-a3
	moveq #0, d0
	rts

opforgeNativeCliTrySelectedEncodeSizeForStatement
	movem.l d2-d7/a0-a2, -(sp)
	move.w d0, d6
	bsr.w opforgeNativeCliPrepareEncodeSelectedRequestForStatement
	tst.l d0
	bne.w opforgeNativeCliTrySelectedEncodeSizePrepareFail
	tst.w NativeCliEvalRequestLen.l
	beq.w opforgeNativeCliTrySelectedEncodeSizeEmpty
	bsr.w opforgeNativeCliPrepareEvaluateExpressionExtension
	lea controlBlockV1, a0
	move.w #LAST_ERROR_BUFFER_PTR_V1, d0
	move.w NativeCliEvalRequestLen.l, d1
	bsr.w opforgeNativeCliWriteInputWindow
	move.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, d0
	move.w #NATIVE_EVAL_EXPR_EXTENSION_BYTES, d1
	bsr.w opforgeNativeCliWriteExtensionWindow
	moveq #ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION, d0
	jsr tkpkgServiceDispatchV1
	lea controlBlockV1, a0
	bsr.w opforgeNativeCliReadStatus
	tst.b d0
	bne.w opforgeNativeCliTrySelectedEncodeSizeFail
	lea controlBlockV1, a0
	bsr.w opforgeNativeCliReadOutputLen
	move.w d0, d1
	moveq #0, d0
	bra.s opforgeNativeCliTrySelectedEncodeSizeReturn

opforgeNativeCliTrySelectedEncodeSizeEmpty
	moveq #0, d1
	moveq #0, d0
	bra.s opforgeNativeCliTrySelectedEncodeSizeReturn

opforgeNativeCliTrySelectedEncodeSizePrepareFail

opforgeNativeCliTrySelectedEncodeSizeFail
	lea controlBlockV1, a0
	bsr.w opforgeNativeCliReadLastErrorLen
	tst.w d0
	beq.s opforgeNativeCliTrySelectedEncodeSizeFailReturn
	lea lastErrorBuffer, a1
	clr.b 0(a1, d0.W)
	bsr.w opforgeNativeCliPassTwoEmitSelectorDiagnostic
	tst.l d0
	bne.s opforgeNativeCliTrySelectedEncodeSizeFailReturn
	move.l #lastErrorBuffer, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr

opforgeNativeCliTrySelectedEncodeSizeFailReturn
	moveq #1, d0

opforgeNativeCliTrySelectedEncodeSizeReturn
	movem.l (sp)+, d2-d7/a0-a2
	rts

; D0: statement index, D1: mnemonic length, A0: mnemonic text.
; Returns D0=1 when the statement mnemonic duplicates its label text.
opforgeNativeCliStatementMnemDuplicatesLabel	.block
	movem.l d1-d4/a0-a2, -(sp)
	move.l d0, d2
	add.w d2, d2
	lea opasmEngineStmtLabelLenTable.l, a2
	moveq #0, d3
	move.w 0(a2, d2.l), d3
	beq.s no
	cmp.w d1, d3
	bne.s no
	move.l d0, d4
	lsl.l #6, d4
	lea opasmEngineStmtLabelNameTable.l, a1
	adda.l d4, a1
	move.l d1, d0
	bsr.w opforgeNativeCliLabelEquals
	bra.s return

no
	moveq #0, d0

return
	movem.l (sp)+, d1-d4/a0-a2
	rts
	.bend  ; opforgeNativeCliStatementMnemDuplicatesLabel

; D0: statement index. Returns D0=1 for no-operand column-one bare symbols.
opforgeNativeCliStatementLooksBareColumnOne	.block
	movem.l d1-d4/a0, -(sp)
	move.l d0, d1
	add.w d1, d1
	lea opasmEngineStmtOperandLenTable.l, a0
	tst.w 0(a0, d1.l)
	bne.w no
	lea opasmEngineStmtSourceLineLenTable.l, a0
	moveq #0, d4
	move.w 0(a0, d1.l), d4
	beq.w no
	move.l d0, d2
	lsl.l #8, d2
	add.l d2, d2
	lea opasmEngineStmtSourceLineTextTable.l, a0
	adda.l d2, a0
	move.b (a0), d3
	tst.b d3
	beq.w no
	cmpi.b #10, d3
	beq.w no
	cmpi.b #13, d3
	beq.w no
	cmpi.b #' ', d3
	beq.w no
	cmpi.b #9, d3
	beq.w no
	cmpi.b #'.', d3
	beq.w no
	cmpi.b #';', d3
	beq.w no

tokenLoop
	tst.l d4
	beq.w yes
	move.b (a0), d3
	tst.b d3
	beq.w yes
	cmpi.b #10, d3
	beq.w yes
	cmpi.b #13, d3
	beq.w yes
	cmpi.b #';', d3
	beq.w yes
	cmpi.b #' ', d3
	beq.w trailingLoop
	cmpi.b #9, d3
	beq.w trailingLoop
	addq.l #1, a0
	subq.l #1, d4
	bra.w tokenLoop

trailingLoop
	tst.l d4
	beq.w yes
	move.b (a0), d3
	tst.b d3
	beq.w yes
	cmpi.b #10, d3
	beq.w yes
	cmpi.b #13, d3
	beq.w yes
	cmpi.b #';', d3
	beq.w yes
	cmpi.b #' ', d3
	beq.w trailingOne
	cmpi.b #9, d3
	beq.w trailingOne
	bra.w no

trailingOne
	addq.l #1, a0
	subq.l #1, d4
	bra.w trailingLoop

yes
	moveq #1, d0
	bra.w return

no
	moveq #0, d0

return
	movem.l (sp)+, d1-d4/a0
	rts
	.bend  ; opforgeNativeCliStatementLooksBareColumnOne

; Compare a fixed-length statement label to a stored zero-terminated label.
opforgeNativeCliLabelEquals
	movem.l d1-d3/a0-a1, -(sp)
	move.l d0, d3
	beq.s opforgeNativeCliLabelEqualsNo

opforgeNativeCliLabelEqualsLoop
	move.b (a0)+, d1
	move.b (a1)+, d2
	cmp.b d2, d1
	bne.s opforgeNativeCliLabelEqualsNo
	subq.l #1, d3
	bne.s opforgeNativeCliLabelEqualsLoop
	tst.b (a0)
	bne.s opforgeNativeCliLabelEqualsNo
	moveq #1, d0
	bra.s opforgeNativeCliLabelEqualsReturn

opforgeNativeCliLabelEqualsNo
	moveq #0, d0

opforgeNativeCliLabelEqualsReturn
	movem.l (sp)+, d1-d3/a0-a1
	rts

; Write the current native image buffer as flat `.bin` output.
opforgeNativeCliWriteFlatOutput
	movem.l d1-d4/a0-a1, -(sp)
	lea NativeCliBinPath, a0
	bsr.w opforgeNativeCliOpenOutput
	tst.l d0
	beq.s opforgeNativeCliWriteFlatFail
	move.l d0, d4
	lea opasmEngineImageBuffer.l, a0
	moveq #0, d0
	move.w opasmEngineImageByteCount.l, d0
	move.l d0, d3
	move.l d4, d1
	bsr.w opforgeNativeCliWriteOutput
	cmp.l d3, d0
	bne.s opforgeNativeCliWriteFlatCloseFail
	move.l d4, d1
	bsr.w opforgeNativeCliClose
	moveq #0, d0
	bra.s opforgeNativeCliWriteFlatReturn

opforgeNativeCliWriteFlatCloseFail
	move.l d4, d1
	bsr.w opforgeNativeCliClose

opforgeNativeCliWriteFlatFail
	moveq #1, d0

opforgeNativeCliWriteFlatReturn
	movem.l (sp)+, d1-d4/a0-a1
	rts

; Clear module/use and statement collection state before parsing input.
opforgeNativeCliInitModuleUseState
	movem.l d0-d1/a0, -(sp)
	lea NativeCliModuleUseStateStart, a0
	move.l #NATIVE_MODULE_USE_STATE_BYTES, d0
	bsr.w opforgeNativeCliClearBytes
	clr.w opasmEngineStmtCount.l
	movem.l (sp)+, d0-d1/a0
	rts

; Clear D0 bytes at A0.
opforgeNativeCliClearBytes
	tst.l d0
	beq.s opforgeNativeCliClearBytesDone
	moveq #0, d1

opforgeNativeCliClearBytesLoop
	move.b d1, (a0)+
	subq.l #1, d0
	bne.s opforgeNativeCliClearBytesLoop

opforgeNativeCliClearBytesDone
	rts

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

NATIVE_OUTPUT_FORMAT_NONE       = 0
NATIVE_OUTPUT_FORMAT_BIN        = 1
NATIVE_OUTPUT_FORMAT_HUNK       = 2

; Parse the native CLI argument tail into fixed buffers and request flags.
opforgeNativeCliParseArgs
	movem.l d2-d7/a2-a6, -(sp)
	movea.l a0, a3  ; A3 walks the AmigaDOS argument tail in-place
	clr.w NativeCliInputStyle
	clr.w NativeCliHunkRequested
	clr.w NativeCliBinRequested
	clr.w NativeCliOutputFormat
	clr.w NativeCliParseStatus
	clr.b NativeCliInputPath
	clr.b NativeCliHunkPath
	clr.b NativeCliBinPath
	clr.b NativeCliOutfileBase
	clr.b NativeCliCpuName
	clr.b NativeCliPackagePath
	move.w #1, NativeCliModulePathCount

opforgeNativeCliParseLoop
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.w opforgeNativeCliParseDone
	cmpi.b #'"', (a3)
	beq.w opforgeNativeCliQuoted
	lea NativeCliArgToken, a1
	bsr.w opforgeNativeCliCopyToken
	tst.l d0
	bne.w opforgeNativeCliUsage

	lea NativeCliArgToken, a0
	lea FlagHelpLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliParseHelp
	lea NativeCliArgToken, a0
	lea FlagHelpShort, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliParseHelp
	lea NativeCliArgToken, a0
	lea FlagVersionLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliParseVersion
	lea NativeCliArgToken, a0
	lea FlagVersionShort, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliParseVersion
	lea NativeCliArgToken, a0
	lea FlagInfileShort, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliInfile
	lea NativeCliArgToken, a0
	lea FlagInfileLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliInfile
	lea NativeCliArgToken, a0
	lea FlagHunkLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliHunk
	lea NativeCliArgToken, a0
	lea FlagBinShort, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliBin
	lea NativeCliArgToken, a0
	lea FlagBinLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliBin
	lea NativeCliArgToken, a0
	lea FlagOutfileShort, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliOutfile
	lea NativeCliArgToken, a0
	lea FlagOutfileLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliOutfile
	lea NativeCliArgToken, a0
	lea FlagCpuLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliCpu
	lea NativeCliArgToken, a0
	lea FlagPackageLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliPackage
	lea NativeCliArgToken, a0
	lea FlagModuleShort, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliModulePath
	lea NativeCliArgToken, a0
	lea FlagModuleLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliModulePath
	bsr.w opforgeNativeCliIsUnsupportedFlag
	tst.l d0
	bne.w opforgeNativeCliUnsupported
	lea NativeCliArgToken, a0
	cmpi.b #'-', (a0)
	beq.w opforgeNativeCliUnknownFlag
	bra.w opforgeNativeCliPositionalInput

opforgeNativeCliInfile
	tst.w NativeCliInputStyle
	beq.s opforgeNativeCliInfileFirst
	cmpi.w #1, NativeCliInputStyle
	beq.w opforgeNativeCliMixedInput
	bra.w opforgeNativeCliUsage

opforgeNativeCliInfileFirst
	move.w #2, NativeCliInputStyle
	lea NativeCliInputPath, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w opforgeNativeCliMissingValue
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliHunk
	move.w #1, NativeCliHunkRequested
	move.w #NATIVE_OUTPUT_FORMAT_HUNK, NativeCliOutputFormat
	lea NativeCliHunkPath, a1
	bsr.w opforgeNativeCliCopyOptionalValue
	tst.l d0
	bmi.w opforgeNativeCliQuoted
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliBin
	move.w #1, NativeCliBinRequested
	move.w #NATIVE_OUTPUT_FORMAT_BIN, NativeCliOutputFormat
	lea NativeCliBinPath, a1
	bsr.w opforgeNativeCliCopyOptionalValue
	tst.l d0
	bmi.w opforgeNativeCliQuoted
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliOutfile
	lea NativeCliOutfileBase, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w opforgeNativeCliMissingValue
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliCpu
	lea NativeCliCpuName, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w opforgeNativeCliMissingValue
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliPackage
	lea NativeCliPackagePath, a1
	bsr.w opforgeNativeCliCopyRequiredValue
	tst.l d0
	bne.w opforgeNativeCliMissingValue
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliModulePath
	lea NativeCliIncludeTarget, a1
	bsr.w opforgeNativeCliCopyRequiredPathValue
	cmpi.l #1, d0
	beq.w opforgeNativeCliMissingValue
	tst.l d0
	bne.w opforgeNativeCliModulePathCapacity
	bsr.w opforgeNativeCliRecordModulePathValue
	tst.l d0
	bne.w opforgeNativeCliModulePathCapacity
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliPositionalInput
	tst.w NativeCliInputStyle
	beq.s opforgeNativeCliPositionalInputFirst
	cmpi.w #2, NativeCliInputStyle
	beq.w opforgeNativeCliMixedInput
	bra.w opforgeNativeCliMultiplePositional

opforgeNativeCliPositionalInputFirst
	move.w #1, NativeCliInputStyle
	lea NativeCliArgToken, a0
	lea NativeCliInputPath, a1
	bsr.w opforgeNativeCliCopyTokenBuffer
	bra.w opforgeNativeCliParseLoop

opforgeNativeCliParseDone
	tst.w NativeCliInputStyle
	beq.w opforgeNativeCliNoInput
	tst.w NativeCliOutputFormat
	beq.w opforgeNativeCliHunkRequired
	cmpi.w #NATIVE_OUTPUT_FORMAT_BIN, NativeCliOutputFormat
	beq.s opforgeNativeCliDefaultBinPath
	cmpi.w #NATIVE_OUTPUT_FORMAT_HUNK, NativeCliOutputFormat
	beq.s opforgeNativeCliDefaultHunkPath
	bra.w opforgeNativeCliUsage

opforgeNativeCliDefaultBinPath
	tst.b NativeCliBinPath
	bne.s opforgeNativeCliParseOk
	tst.b NativeCliOutfileBase
	beq.s opforgeNativeCliParseOk
	lea NativeCliOutfileBase, a0
	lea NativeCliBinPath, a1
	bsr.w opforgeNativeCliCopyTokenBuffer
	bra.s opforgeNativeCliParseOk

opforgeNativeCliDefaultHunkPath
	tst.b NativeCliHunkPath
	bne.s opforgeNativeCliParseOk
	tst.b NativeCliOutfileBase
	beq.s opforgeNativeCliParseOk
	lea NativeCliOutfileBase, a0
	lea NativeCliHunkPath, a1
	bsr.w opforgeNativeCliCopyTokenBuffer

opforgeNativeCliParseOk
	bsr.w opforgeNativeCliRecordImplicitModulePathRoot
	tst.l d0
	bne.w opforgeNativeCliModulePathCapacity
	move.w #NCLI_PARSE_OK, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliParseHelp
	move.w #NCLI_PARSE_HELP, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliParseVersion
	move.w #NCLI_PARSE_VERSION, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliUsage
	move.w #NCLI_PARSE_USAGE, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliQuoted
	move.w #NCLI_PARSE_QUOTED, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliUnsupported
	move.w #NCLI_PARSE_UNSUPPORTED, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliUnknownFlag
	move.w #NCLI_PARSE_UNKNOWN_FLAG, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliMissingValue
	move.w #NCLI_PARSE_MISSING_VALUE, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliNoInput
	move.w #NCLI_PARSE_NO_INPUT, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliHunkRequired
	move.w #NCLI_PARSE_HUNK_REQUIRED, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliMixedInput
	move.w #NCLI_PARSE_MIXED_INPUT, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliMultiplePositional
	move.w #NCLI_PARSE_MULTIPLE_POSITIONAL, NativeCliParseStatus
	bra.w opforgeNativeCliParseReturn

opforgeNativeCliModulePathCapacity
	move.w #NCLI_PARSE_MODULE_PATH_CAPACITY, NativeCliParseStatus

opforgeNativeCliParseReturn
	move.w NativeCliParseStatus, d0
	ext.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts

opforgeNativeCliSkipWhitespace
	cmpi.b #' ', (a3)
	beq.s opforgeNativeCliSkipOne
	cmpi.b #9, (a3)
	beq.s opforgeNativeCliSkipOne
	cmpi.b #10, (a3)
	beq.s opforgeNativeCliSkipOne
	cmpi.b #13, (a3)
	bne.s opforgeNativeCliSkipDone

opforgeNativeCliSkipOne
	addq.l #1, a3
	bra.s opforgeNativeCliSkipWhitespace

opforgeNativeCliSkipDone
	rts

opforgeNativeCliCopyToken
	move.l #TOKEN_BUFFER_CAPACITY - 1, d6

opforgeNativeCliCopyTokenLoop
	moveq #0, d0
	move.b (a3), d0
	beq.s opforgeNativeCliCopyTokenDone
	cmpi.b #' ', d0
	beq.s opforgeNativeCliCopyTokenDone
	cmpi.b #9, d0
	beq.s opforgeNativeCliCopyTokenDone
	cmpi.b #10, d0
	beq.s opforgeNativeCliCopyTokenDone
	cmpi.b #13, d0
	beq.s opforgeNativeCliCopyTokenDone
	cmpi.b #'"', d0
	beq.s opforgeNativeCliCopyTokenFail
	tst.l d6
	beq.s opforgeNativeCliCopyTokenFail
	move.b d0, (a1)+
	addq.l #1, a3
	subq.l #1, d6
	bra.s opforgeNativeCliCopyTokenLoop

opforgeNativeCliCopyTokenDone
	clr.b (a1)
	moveq #0, d0
	rts

opforgeNativeCliCopyTokenFail
	moveq #1, d0
	rts

opforgeNativeCliCopyRequiredValue
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.s opforgeNativeCliRequiredMissing
	cmpi.b #'"', (a3)
	beq.s opforgeNativeCliRequiredMissing
	bsr.w opforgeNativeCliCopyToken
	rts

opforgeNativeCliRequiredMissing
	moveq #1, d0
	rts

opforgeNativeCliCopyRequiredPathValue
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.s opforgeNativeCliRequiredPathMissing
	cmpi.b #'"', (a3)
	beq.s opforgeNativeCliRequiredPathMissing
	move.l #PATH_BUFFER_CAPACITY - 1, d6

opforgeNativeCliCopyRequiredPathLoop
	moveq #0, d0
	move.b (a3), d0
	beq.s opforgeNativeCliCopyRequiredPathDone
	cmpi.b #' ', d0
	beq.s opforgeNativeCliCopyRequiredPathDone
	cmpi.b #9, d0
	beq.s opforgeNativeCliCopyRequiredPathDone
	cmpi.b #10, d0
	beq.s opforgeNativeCliCopyRequiredPathDone
	cmpi.b #13, d0
	beq.s opforgeNativeCliCopyRequiredPathDone
	cmpi.b #'"', d0
	beq.s opforgeNativeCliCopyRequiredPathCapacity
	tst.l d6
	beq.s opforgeNativeCliCopyRequiredPathCapacity
	move.b d0, (a1)+
	addq.l #1, a3
	subq.l #1, d6
	bra.s opforgeNativeCliCopyRequiredPathLoop

opforgeNativeCliCopyRequiredPathDone
	clr.b (a1)
	moveq #0, d0
	rts

opforgeNativeCliRequiredPathMissing
	moveq #1, d0
	rts

opforgeNativeCliCopyRequiredPathCapacity
	moveq #2, d0
	rts

opforgeNativeCliCopyOptionalValue
	bsr.w opforgeNativeCliSkipWhitespace
	tst.b (a3)
	beq.s opforgeNativeCliOptionalNone
	cmpi.b #'"', (a3)
	beq.s opforgeNativeCliOptionalQuoted
	cmpi.b #'-', (a3)
	beq.s opforgeNativeCliOptionalNone
	bsr.w opforgeNativeCliCopyToken
	rts

opforgeNativeCliOptionalNone
	clr.b (a1)
	moveq #0, d0
	rts

opforgeNativeCliOptionalQuoted
	moveq #-1, d0
	rts

opforgeNativeCliCopyTokenBuffer
	moveq #0, d0
	move.b (a0)+, d0
	move.b d0, (a1)+
	bne.s opforgeNativeCliCopyTokenBuffer
	rts

opforgeNativeCliTokenEquals
	moveq #0, d2

opforgeNativeCliTokenEqualsLoop
	move.b (a0)+, d0
	move.b (a1)+, d1
	cmp.b d1, d0
	bne.s opforgeNativeCliTokenNotEqual
	tst.b d0
	bne.s opforgeNativeCliTokenEqualsLoop
	moveq #1, d0
	rts

opforgeNativeCliTokenNotEqual
	moveq #0, d0
	rts

opforgeNativeCliIsUnsupportedFlag
	lea NativeCliArgToken, a0
	lea FlagListShort, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagListLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagHexShort, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagHexLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagSrecShort, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagSrecLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagDefineShort, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagDefineLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagIncludeShort, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	lea NativeCliArgToken, a0
	lea FlagIncludeLong, a1
	bsr.w opforgeNativeCliTokenEquals
	tst.l d0
	bne.w opforgeNativeCliUnsupportedYes
	moveq #0, d0
	rts

opforgeNativeCliUnsupportedYes
	moveq #1, d0
	rts

; Print the deterministic diagnostic for the current argument-parse status.
opforgeNativeCliReportParseError
	move.w NativeCliParseStatus, d0
	cmpi.w #NCLI_PARSE_QUOTED, d0
	beq.s opforgeNativeCliReportQuoted
	cmpi.w #NCLI_PARSE_UNSUPPORTED, d0
	beq.s opforgeNativeCliReportUnsupported
	cmpi.w #NCLI_PARSE_UNKNOWN_FLAG, d0
	beq.s opforgeNativeCliReportUnknown
	cmpi.w #NCLI_PARSE_MISSING_VALUE, d0
	beq.s opforgeNativeCliReportMissing
	cmpi.w #NCLI_PARSE_NO_INPUT, d0
	beq.w opforgeNativeCliReportNoInput
	cmpi.w #NCLI_PARSE_HUNK_REQUIRED, d0
	beq.w opforgeNativeCliReportHunkRequired
	cmpi.w #NCLI_PARSE_MIXED_INPUT, d0
	beq.w opforgeNativeCliReportMixedInput
	cmpi.w #NCLI_PARSE_MULTIPLE_POSITIONAL, d0
	beq.w opforgeNativeCliReportMultiplePositional
	cmpi.w #NCLI_PARSE_MODULE_PATH_CAPACITY, d0
	beq.w opforgeNativeCliReportModulePathCapacity
	move.l #UsageText, d1
	bra.w opforgeNativeCliReportText

opforgeNativeCliReportQuoted
	move.l #QuotedText, d1
	bra.w opforgeNativeCliReportText

opforgeNativeCliReportUnsupported
	move.l #UnsupportedText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeSubsetHelpText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportUnknown
	move.l #UnknownFlagText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportMissing
	move.l #MissingValueText, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NativeCliArgToken, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportNoInput
	move.l #NoInputText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportHunkRequired
	move.l #HunkRequiredText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportMixedInput
	move.l #MixedInputText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportMultiplePositional
	move.l #MultiplePositionalText, d1
	bra.s opforgeNativeCliReportText

opforgeNativeCliReportModulePathCapacity
	move.l #ModulePathCapacityText, d1

opforgeNativeCliReportText
	bsr.w opforgeNativeCliPutStr
	rts

opforgeNativeCliRecordImplicitModulePathRoot
	lea NativeCliInputPath, a0
	lea NativeCliModulePathTable, a1
	bsr.w opforgeNativeCliCopyPathRoot
	rts

opforgeNativeCliRecordModulePathValue
	movem.l d1/a0-a1, -(sp)
	moveq #0, d0
	move.w NativeCliModulePathCount, d0
	cmpi.w #NATIVE_MODULE_PATH_CAPACITY, d0
	bhs.s opforgeNativeCliRecordModulePathFail
	move.l d0, d1
	lsl.l #8, d1
	lea NativeCliModulePathTable, a1
	adda.l d1, a1
	lea NativeCliIncludeTarget, a0
	bsr.w opforgeNativeCliCopyPathBuffer
	tst.l d0
	bne.s opforgeNativeCliRecordModulePathFail
	move.w NativeCliModulePathCount, d0
	addq.w #1, d0
	move.w d0, NativeCliModulePathCount
	moveq #0, d0
	bra.s opforgeNativeCliRecordModulePathReturn

opforgeNativeCliRecordModulePathFail
	moveq #1, d0

opforgeNativeCliRecordModulePathReturn
	movem.l (sp)+, d1/a0-a1
	rts

opforgeNativeCliEmitModulePathRecords
	movem.l d0-d4/a0, -(sp)
	clr.w d4

opforgeNativeCliEmitModulePathLoop
	move.w NativeCliModulePathCount, d0
	cmp.w d0, d4
	bhs.s opforgeNativeCliEmitModulePathDone
	move.l #ModPathText, d1
	bsr.w opforgeNativeCliPutStr
	moveq #0, d0
	move.w d4, d0
	bsr.w opforgeNativeCliPutDecU16
	bsr.w opforgeNativeCliPutSpace
	moveq #0, d0
	move.w d4, d0
	lsl.l #8, d0
	lea NativeCliModulePathTable, a0
	adda.l d0, a0
	move.l a0, d1
	bsr.w opforgeNativeCliPutStr
	move.l #NewlineText, d1
	bsr.w opforgeNativeCliPutStr
	addq.w #1, d4
	bra.s opforgeNativeCliEmitModulePathLoop

opforgeNativeCliEmitModulePathDone
	movem.l (sp)+, d0-d4/a0
	rts
	.bend  ; start
	.priv

	.endsection

	.section data, kind=data

DosName
	.byte "dos.library", 0
NewlineText
	.byte 10, 0
VersionText
	.byte "opForge native AmigaOS CLI 0.1", 10, 0
HelpText
	.byte "Usage: opForge [OPTIONS] [INPUT]", 10
	.byte "Native subset: INPUT, -i/--infile, --bin [FILE], --hunk [FILE], -o/--outfile, --cpu, --opasm-package, -M/--module-path", 10, 0
UsageText
	.byte "OPC-NCLI001: Usage: opForge [OPTIONS] [INPUT]", 10, 0
QuotedText
	.byte "OPC-NCLI002: quoted arguments are not supported by the native CLI subset", 10, 0
UnsupportedText
	.byte "OPC-NCLI003: recognized Rust CLI option is not implemented by native AmigaOS CLI yet: ", 0
NativeSubsetHelpText
	.byte 10, "Native subset supports INPUT, -i/--infile, --bin [FILE], --hunk [FILE], -o/--outfile, --cpu, --opasm-package, and -M/--module-path; --hunk is not implemented yet.", 10, 0
UnknownFlagText
	.byte "OPC-NCLI004: unknown CLI flag: ", 0
MissingValueText
	.byte "OPC-NCLI005: option requires a value: ", 0
NoInputText
	.byte "OPC-NCLI006: No input files specified. Use -i/--infile", 10, 0
HunkRequiredText
	.byte "OPC-NCLI007: No outputs selected. Native AmigaOS CLI currently requires --bin", 10, 0
MixedInputText
	.byte "OPC-NCLI011: Do not mix positional input with -i/--infile; use one style", 10, 0
MultiplePositionalText
	.byte "OPC-NCLI012: Multiple positional inputs are not supported; use repeatable -i/--infile", 10, 0
ModulePathCapacityText
	.byte "OPC-NCLI017: native module path capacity exceeded", 10, 0
PackageTooLargeText
	.byte "ERROR OPC-NCLI019: opasm package exceeds native package storage capacity", 10, 0
InputOpenErrorText
	.byte "OPC-NCLI008: Input source file not found: ", 0
StubHeaderText
	.byte "OPFORGE-NATIVE 1", 10
	.byte "STATUS emitter-not-implemented", 10, 0
InputLabelText
	.byte "INPUT ", 0
HunkLabelText
	.byte "HUNK ", 0
BinLabelText
	.byte "BIN ", 0
TokenizerOkText
	.byte "STATUS tokenizer-ok", 10, 0
TokenizerFailureText
	.byte "ERROR OPC-NCLI010: native tokenizer stage failed", 10, 0
ParserOkText
	.byte "STAGE parser", 10
	.byte "STATUS parser-module-use-ok", 10, 0
SessionStageText
	.byte "STAGE session", 10, 0
SessionCpuText
	.byte "SESSION-CPU ", 0
SessionPassText
	.byte "SESSION-PASS ", 0
SessionOriginText
	.byte "SESSION-ORIGIN ", 0
SessionPcText
	.byte "SESSION-PC ", 0
SessionSourceCountText
	.byte "SESSION-SOURCE-COUNT ", 0
SessionStmtCountText
	.byte "SESSION-STMT-COUNT ", 0
SessionLabelCountText
	.byte "SESSION-LABEL-COUNT ", 0
SessionImageBytesText
	.byte "SESSION-IMAGE-BYTES ", 0
SessionReadyText
	.byte "STATUS session-ready", 10, 0
NativePassOneText
	.byte "STAGE pass1", 10, 0
NativePassOneOkText
	.byte "STATUS pass1-ok", 10, 0
NativePassTwoText
	.byte "STAGE pass2", 10, 0
NativePassTwoOkText
	.byte "STATUS pass2-ok", 10, 0
NativeSelectorStatusOkText
	.byte "STATUS selector-status-ok", 10, 0
NativePassFailureText
	.byte "ERROR OPC-NCLI020: native pass engine failed", 10, 0
NativeDuplicateLabelText
	.byte "ERROR OPC-NCLI021: duplicate native label: ", 0
NativeUnresolvedLabelText
	.byte "ERROR OPC-NCLI022: unresolved native label", 10, 0
NativeOutputOkText
	.byte "STATUS output-ok", 10, 0
NativeOutputFailureText
	.byte "ERROR OPC-NCLI023: native flat output write failed", 10, 0
NativeImageCapacityText
	.byte "ERROR OPC-NCLI024: native image buffer capacity exceeded", 10, 0
NativeUnknownMnemonicText
	.byte "ERROR OPC-NCLI025: unknown native mnemonic", 10, 0
NativeUnsupportedAddressingText
	.byte "ERROR OPC-NCLI026: unsupported native addressing mode", 10, 0
NativeBadOrgText
	.byte "ERROR OPC-NCLI027: invalid native .org expression", 10, 0
NativeHunkNotImplementedText
	.byte "ERROR OPC-NCLI028: native Hunk output is not implemented; use --bin for flat output", 10, 0
NativeSelectorUnknownRawText
	.byte "OTR901: selector unknown mnemonic", 0
NativeSelectorUnsupportedRawText
	.byte "OTR901: selector unsupported address", 0
NativeSelectorOperandRawText
	.byte "OTR901: selector operand error", 0
NativeSelectedOperandCompileRawText
	.byte "OTR901: selected operand compile failed", 0
NativeLabelText
	.byte "LABEL ", 0
EmitterStubText
	.byte "STAGE emitter", 10
	.byte "ERROR OPC-NCLI009: native emitter VM not implemented", 10, 0
ParserFailureText
	.byte "ERROR OPC-NCLI013: native module/use parser stage failed", 10, 0
ModuleDepthFailureText
	.byte "ERROR OPC-NCLI016: native module depth mismatch", 10, 0
IncludeStageText
	.byte "STAGE include", 10, 0
IncludeOkText
	.byte "STATUS include-ok", 10, 0
IncludeFailureText
	.byte "ERROR OPC-NCLI014: native include expansion failed", 10, 0
ConditionalFailureText
	.byte "ERROR OPC-NCLI015: native conditional preprocessing not implemented", 10, 0
ModuleResolveFailureText
	.byte "ERROR OPC-NCLI018: native module resolution failed: ", 0
IncludeRootText
	.byte "INCLUDE-ROOT 1 ", 0
IncludeFileText
	.byte "INCLUDE-FILE 1 ", 0
IncludeEnterText
	.byte "INCLUDE-ENTER 1 ", 0
IncludeLineText
	.byte "INCLUDE-LINE ", 0
IncludeLeaveText
	.byte "INCLUDE-LEAVE 1", 10, 0
ModRootText
	.byte "MOD-ROOT ", 0
ModDefText
	.byte "MOD-DEF ", 0
ModEndText
	.byte "MOD-END ", 0
ModPathText
	.byte "MOD-PATH ", 0
UseImportText
	.byte "USE-IMPORT ", 0
UseSelectText
	.byte "USE-SELECT ", 0
UseWildcardText
	.byte "USE-WILDCARD ", 0
StatementText
	.byte "STMT ", 0
StatementExprText
	.byte "STMT-EXPR ", 0
ModuleFoundText
	.byte "MODULE ", 0
SpaceText
	.byte " ", 0
HexDigitsText
	.byte "0123456789ABCDEF"
AsKeywordText
	.byte "as"
ModuleSourceExtensionText
	.byte ".asm", 0
ProcessorAsmText
	.byte "asm"
KindStatementText
	.byte "statement"
ModuleMnemonicText
	.byte "module"
EndmoduleMnemonicText
	.byte "endmodule"
UseMnemonicText
	.byte "use"
OrgMnemonicText
	.byte ".org"
CpuMnemonicText
	.byte ".cpu"
EndMnemonicText
	.byte ".end"
NativeCliSelectedShapeAccumulatorText
	.byte "accumulator", 0
NativeCliSelectedShapeImmediateText
	.byte "immediate", 0
NativeCliSelectedShapeDirectText
	.byte "direct", 0
NativeCliSelectedShapeDirectXText
	.byte "direct_x", 0
NativeCliSelectedShapeDirectYText
	.byte "direct_y", 0
NativeCliSelectedShapeIndirectText
	.byte "indirect", 0
NativeCliSelectedShapeIndexedIndirectXText
	.byte "indexed_indirect_x", 0
NativeCliSelectedShapeIndirectIndexedYText
	.byte "indirect_indexed_y", 0
LdaMnemonicText
	.byte "lda"
StaMnemonicText
	.byte "sta"
JmpMnemonicText
	.byte "jmp"
NopMnemonicText
	.byte "nop"
ImmediateModeText
	.byte "immediate"
AbsoluteModeText
	.byte "absolute"
ModuleDirectiveText
	.byte ".module"
EndmoduleDirectiveText
	.byte ".endmodule"
UseDirectiveText
	.byte ".use"
IncludeDirectiveText
	.byte ".include"
IfDirectiveText
	.byte ".if"
IfdefDirectiveText
	.byte ".ifdef"
IfndefDirectiveText
	.byte ".ifndef"
ElseDirectiveText
	.byte ".else"
ElseifDirectiveText
	.byte ".elseif"
EndifDirectiveText
	.byte ".endif"
.ifdef OPFORGE_FS_UAE_SMOKE
defaultFsUaeArgTail
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_6502_OUTPUT
	.byte "Work:opforge_6502_native_cli_smoke.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_65C02_OUTPUT
	.byte "Work:opforge_6502_native_cli_smoke.asm --bin Work:opforge_native_out.bin --cpu 65c02 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC
	.byte "Work:opforge_6502_unknown_mnemonic.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING
	.byte "Work:opforge_6502_unsupported_addressing.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_6502_UNRESOLVED_LABEL
	.byte "Work:opforge_6502_unresolved_label.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_6502_BAD_ORG
	.byte "Work:opforge_6502_bad_org.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_UNSUPPORTED_OUTPUT
	.byte "Work:opforge_6502_native_cli_smoke.asm --srec Work:opforge_native_out.srec --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_INPUT
	.byte "Work:opforge_missing_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_HUNK
	.byte "Work:opforge_fsuae_smoke_input.asm --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_HUNK_OUTPUT
	.byte "Work:opforge_fsuae_smoke_input.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MIXED_INPUT
	.byte "Work:opforge_fsuae_smoke_input.asm --infile Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_BAD_PACKAGE
	.byte "Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_missing_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_PACKAGE_TOO_LARGE
	.byte "Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package_oversized.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_UNMATCHED_ENDMODULE
	.byte "Work:opforge_fsuae_unmatched_endmodule.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_UNTERMINATED_MODULE
	.byte "Work:opforge_fsuae_unterminated_module.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_BAD_USE
	.byte "Work:opforge_fsuae_bad_use.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE
	.byte "Work:opforge_fsuae_missing_module.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE_PATH
	.byte "Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm -M", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MODULE_PATH_OVERFLOW
	.byte "Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm -M Work:mod1 -M Work:mod2 -M Work:mod3 -M Work:mod4 -M Work:mod5 -M Work:mod6 -M Work:mod7 -M Work:mod8", 0
.else
	.byte "Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm -M Work:opforge_module_a --module-path Work:opforge_module_b", 0
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
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif

FlagHelpLong
	.byte "--help", 0
FlagHelpShort
	.byte "-h", 0
FlagVersionLong
	.byte "--version", 0
FlagVersionShort
	.byte "-V", 0
FlagInfileShort
	.byte "-i", 0
FlagInfileLong
	.byte "--infile", 0
FlagHunkLong
	.byte "--hunk", 0
FlagOutfileShort
	.byte "-o", 0
FlagOutfileLong
	.byte "--outfile", 0
FlagCpuLong
	.byte "--cpu", 0
FlagPackageLong
	.byte "--opasm-package", 0
FlagListShort
	.byte "-l", 0
FlagListLong
	.byte "--list", 0
FlagHexShort
	.byte "-x", 0
FlagHexLong
	.byte "--hex", 0
FlagSrecShort
	.byte "-s", 0
FlagSrecLong
	.byte "--srec", 0
FlagBinShort
	.byte "-b", 0
FlagBinLong
	.byte "--bin", 0
FlagDefineShort
	.byte "-D", 0
FlagDefineLong
	.byte "--define", 0
FlagIncludeShort
	.byte "-I", 0
FlagIncludeLong
	.byte "--include-path", 0
FlagModuleShort
	.byte "-M", 0
FlagModuleLong
	.byte "--module-path", 0

	.align 2

DecimalPowers
	.word 10000, 1000, 100, 10, 1

	.align 2

OpforgeNativeCliPackageLen
	.word OPFORGE_NATIVE_CLI_PACKAGE_LEN

DefaultCpuName
	.byte "m68020", 0
DefaultFamilyName
	.byte "motorola68k"
DefaultFamilyNameEnd
M6502CpuNameText
	.byte "m6502", 0
Mos6502FamilyName
	.byte "mos6502"
mos6502FamilyNameEnd

	.align 2
opforgeNativeCliPackageData
	.incbin "opforge_cli_package.opasm"
OPFORGE_NATIVE_CLI_PACKAGE_DATA_END

DEFAULT_FAMILY_NAME_LEN = DefaultFamilyNameEnd - DefaultFamilyName
MOS6502_FAMILY_NAME_LEN = mos6502FamilyNameEnd - Mos6502FamilyName
OPFORGE_NATIVE_CLI_PACKAGE_LEN = OPFORGE_NATIVE_CLI_PACKAGE_DATA_END - opforgeNativeCliPackageData

	.endsection

	.section bss, kind=bss
	.align 4

NativeCliDosBase
	.res long, 1
NativeCliReturnCode
	.res long, 1
NativeCliInputStyle
	.res word, 1
NativeCliHunkRequested
	.res word, 1
NativeCliBinRequested
	.res word, 1
NativeCliOutputFormat
	.res word, 1
NativeCliParseStatus
	.res word, 1

NativeCliArgToken
	.res byte, TOKEN_BUFFER_CAPACITY
NativeCliInputPath
	.res byte, PATH_BUFFER_CAPACITY
NativeCliHunkPath
	.res byte, PATH_BUFFER_CAPACITY
NativeCliBinPath
	.res byte, PATH_BUFFER_CAPACITY
NativeCliOutfileBase
	.res byte, PATH_BUFFER_CAPACITY
NativeCliCpuName
	.res byte, TOKEN_BUFFER_CAPACITY
NativeCliPackagePath
	.res byte, PATH_BUFFER_CAPACITY
NativeCliSourceLineLen
	.res word, 1
NativeCliParserTailLen
	.res word, 1
NativeCliPackageLenActive
	.res word, 1
NativeCliPipelineRequestLen
	.res word, 1
NativeCliLineRequestLen
	.res word, 1
NativeCliEvalRequestLen
	.res word, 1
NativeCliEncodeRequestLen
	.res word, 1
	.align 4
NativeCliOpasmEngineContext
	.res long, NATIVE_OPASM_ENGINE_CONTEXT_LONGS
NativeCliSourceLineNum
	.res long, 1
NativeCliSawCr
	.res word, 1
NativeCliIncludeDepth
	.res word, 1
NativeCliModuleResolveDepth
	.res word, 1
NativeCliResolvedModuleId
	.res word, 1
NativeCliSavedLineLen
	.res word, 1
NativeCliSavedSawCr
	.res word, 1
NativeCliSavedLineNum
	.res long, 1
NativeCliModuleSavedLineLen
	.res word, 1
NativeCliModuleSavedSawCr
	.res word, 1
NativeCliModuleSavedLineNum
	.res long, 1
NativeCliStmtMnemFound
	.res word, 1
NativeCliStmtExprFound
	.res word, 1
NativeCliStmtDirectiveKind
	.res word, 1
NativeCliInputChar
	.res byte, 1
NativeCliDecimalChar
	.res byte, 2
NativeCliHexBuffer
	.res byte, 10
NativeCliStmtLabelStart
	.res long, 1
NativeCliStmtLabelEnd
	.res long, 1
NativeCliStmtLabelOff
	.res long, 1
NativeCliStmtLabelLen
	.res long, 1
NativeCliStmtMnemStart
	.res long, 1
NativeCliStmtMnemEnd
	.res long, 1
NativeCliStmtMnemOff
	.res long, 1
NativeCliStmtMnemLen
	.res long, 1
NativeCliStmtOperandStart
	.res long, 1
NativeCliStmtOperandEnd
	.res long, 1
NativeCliStmtExprOperandIndex
	.res long, 1
NativeCliStmtExprSlotIndex
	.res long, 1
NativeCliStmtExprStartToken
	.res long, 1
NativeCliStmtExprEndToken
	.res long, 1
NativeCliStmtExprSpanLine
	.res long, 1
NativeCliStmtExprSpanStart
	.res long, 1
NativeCliStmtExprSpanEnd
	.res long, 1
NativeCliSourceLine
	.res byte, SOURCE_LINE_BUFFER_CAPACITY
NativeCliParserTailBuffer
	.res byte, SOURCE_LINE_BUFFER_CAPACITY
NativeCliCurrentPath
	.res byte, PATH_BUFFER_CAPACITY
NativeCliSavedPath
	.res byte, PATH_BUFFER_CAPACITY
NativeCliModuleSavedPath
	.res byte, PATH_BUFFER_CAPACITY
NativeCliIncludeTarget
	.res byte, PATH_BUFFER_CAPACITY
NativeCliIncludePath
	.res byte, PATH_BUFFER_CAPACITY
NativeCliIncludeRootPath
	.res byte, PATH_BUFFER_CAPACITY
OpforgeNativeCliPrvmRouteFrame
	.res byte, PRVM_ROUTE_FRAME_SIZE
NativeCliPrvmRouteStatus
	.res long, 1
NativeCliPrvmResultCount
	.res word, 1
OpforgeNativeCliPrvmResultBuffer
	.res byte, PRVM_ROUTE_RESULT_CAPACITY
OpforgeNativeCliPrvmDiagBuffer
	.res byte, PRVM_ROUTE_DIAG_CAPACITY
OpforgeNativeCliPrvmResumeBuffer
	.res byte, PRVM_ROUTE_RESUME_CAPACITY
OpforgeNativeCliPrvmExprRequest
	.res byte, PRVM_ROUTE_EXPR_REQUEST_SIZE
OpforgeNativeCliPrvmExprResultSlot
	.res byte, PRVM_ROUTE_EXPR_RESULT_SIZE * PRVM_ROUTE_EXPR_RESULT_CAPACITY

NativeCliModuleUseStateStart
NativeCliModuleCount
	.res word, 1
NativeCliImportCount
	.res word, 1
NativeCliModulePathCount
	.res word, 1
NativeCliImportSelectCount
	.res word, 1
NativeCliRootModuleId
	.res word, 1
NativeCliCurrentModuleId
	.res word, 1
NativeCliModuleDepth
	.res word, 1
NativeCliModuleNameTable
	.res byte, NATIVE_MODULE_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
NativeCliModuleFileIdTable
	.res word, NATIVE_MODULE_TABLE_CAPACITY
NativeCliModuleLineTable
	.res long, NATIVE_MODULE_TABLE_CAPACITY
NativeCliModuleDepthTable
	.res word, NATIVE_MODULE_TABLE_CAPACITY
NativeCliImportOwnerModuleTable
	.res word, NATIVE_IMPORT_TABLE_CAPACITY
NativeCliImportModuleTable
	.res word, NATIVE_IMPORT_TABLE_CAPACITY
NativeCliImportFileIdTable
	.res word, NATIVE_IMPORT_TABLE_CAPACITY
NativeCliImportLineTable
	.res long, NATIVE_IMPORT_TABLE_CAPACITY
NativeCliImportAliasTable
	.res byte, NATIVE_IMPORT_TABLE_CAPACITY * TOKEN_BUFFER_CAPACITY
NativeCliImportSelectImportTable
	.res word, NATIVE_IMPORT_SELECT_CAPACITY
NativeCliImportSelectNameTable
	.res byte, NATIVE_IMPORT_SELECT_CAPACITY * TOKEN_BUFFER_CAPACITY
NativeCliImportSelectAliasTable
	.res byte, NATIVE_IMPORT_SELECT_CAPACITY * TOKEN_BUFFER_CAPACITY
NativeCliImportSelectFlagsTable
	.res word, NATIVE_IMPORT_SELECT_CAPACITY
NativeCliModulePathTable
	.res byte, NATIVE_MODULE_PATH_CAPACITY * PATH_BUFFER_CAPACITY
nativeCliModuleUseStateEnd

	.endsection

	.output "build/opforge_cli", format=hunk, sections=entry, code, data, bss
	.endmodule
