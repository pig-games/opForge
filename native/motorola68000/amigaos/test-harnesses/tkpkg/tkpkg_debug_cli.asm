; Thin AmigaOS CLI smoke wrapper for the package-backed tkpkg runtime.
;
; This slice exercises init, load_package, set_pipeline, tokenize_line, and
; last_error without folding CLI/report concerns into the core runtime modules.

	.module main
	.cpu 68020
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.compact_table as compact
	.use tkpkg.amigaos.package_loader
	.use tkpkg.amigaos.service

SYS_BASE                        = 4
RETURN_OK                       = 0
RETURN_FAIL                     = 20

OPEN_LIBRARY                    = -552
CLOSE_LIBRARY                   = -414
PUT_STR                         = -948
GET_ARG_STR                     = -534
OPEN                            = -30
CLOSE                           = -36
READ                            = -42

MODE_OLDFILE                    = 1005
PATH_BUFFER_CAPACITY            = 256
DEBUG_CLI_SOURCE_BUFFER_CAPACITY = 16384
DEBUG_CLI_MANIFEST_BUFFER_CAPACITY = 8192
DEBUG_CLI_MAX_LINE_BYTES        = buffers.LAST_ERROR_BUFFER_CAPACITY - 4
DEBUG_CLI_FILE_MODE_SINGLE      = 1
DEBUG_CLI_FILE_MODE_MANIFEST    = 2

PACKAGE_INPUT_PTR_V1           = buffers.PACKAGE_STORAGE_PTR_V1

	.section entry, kind=code
	.pub

start	.block
	move.l #RETURN_FAIL, DebugCliReturnCode

	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.W, a6
	jsr OPEN_LIBRARY(a6)

	tst.l d0
	bne.w tkpkgDebugCliHaveDos

	lea DosName, a1
	moveq #0, d0
	movea.l SYS_BASE.W, a6
	jsr OPEN_LIBRARY(a6)

	tst.l d0
	beq.w tkpkgDebugCliDone

	move.l d0, DebugCliDosBase
	move.l #DosVersionFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bra.w tkpkgDebugCliCloseDos

tkpkgDebugCliHaveDos
	move.l d0, DebugCliDosBase
	move.l #StartedText, d1
	bsr.w tkpkgDebugCliPutStrV1

	lea buffers.ControlBlockV1, a0
	moveq #abi.ENTRY_ORD_INIT, d0
	bsr.w tkpkgDebugCliDispatchServiceV1
	bsr.w tkpkgDebugCliReadStatusV1
	bne.w tkpkgDebugCliReportFailure

	lea tkpkgDebugCliPackageData, a1
	lea buffers.packageStorage, a2
	move.l TkpkgDebugCliPackageLen, d0
	bsr.w tkpkgDebugCliCopyBytesV1
	move.l TkpkgDebugCliPackageLen, d0
	move.l d0, d1
	swap d1
	tst.w d1
	bne.s tkpkgDebugCliLoadStagedPackage

	lea buffers.ControlBlockV1, a0
	move.w #PACKAGE_INPUT_PTR_V1, d0
	move.l TkpkgDebugCliPackageLen, d1
	bsr.w tkpkgDebugCliWriteInputWindowV1
	moveq #abi.ENTRY_ORD_LOAD_PACKAGE, d0
	bsr.w tkpkgDebugCliDispatchServiceV1
	bsr.w tkpkgDebugCliReadStatusV1
	bne.w tkpkgDebugCliReportFailure
	bra.s tkpkgDebugCliPackageLoaded

tkpkgDebugCliLoadStagedPackage
	move.l TkpkgDebugCliPackageLen, d0
	jsr package_loader.tkpkgPackageLoaderLoadStagedV1
	tst.l d0
	beq.s tkpkgDebugCliPackageLoaded
	movea.l a1, a3
	move.l #FailurePrefixText, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l a3, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #NewlineText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bra.w tkpkgDebugCliCloseDos

tkpkgDebugCliPackageLoaded
	move.l #PackageLoadedText, d1
	bsr.w tkpkgDebugCliPutStrV1

.ifdef OPFORGE_FS_UAE_TKPKG_MANIFEST
	lea DefaultSmokeManifestPath, a0
	lea DebugCliInputPathBuffer, a1
tkpkgDebugCliCopyDefaultManifestPathLoop
	move.b (a0)+, (a1)+
	bne.s tkpkgDebugCliCopyDefaultManifestPathLoop
	moveq #DEBUG_CLI_FILE_MODE_MANIFEST, d0
.else
.ifdef OPFORGE_FS_UAE_SMOKE
	lea DefaultSmokeInputPath, a0
	lea DebugCliInputPathBuffer, a1
tkpkgDebugCliCopyDefaultSmokePathLoop
	move.b (a0)+, (a1)+
	bne.s tkpkgDebugCliCopyDefaultSmokePathLoop
	moveq #DEBUG_CLI_FILE_MODE_SINGLE, d0
.else
	bsr.w tkpkgDebugCliParseOptionalInputPathV1
	bmi.w tkpkgDebugCliCloseDos
.endif
.endif
	move.l d0, DebugCliFileModeEnabled

	lea setPipelineRequest, a1
	lea buffers.lastErrorBuffer, a2
	moveq #SET_PIPELINE_REQUEST_LEN, d0
	bsr.w tkpkgDebugCliCopyBytesV1

	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	moveq #SET_PIPELINE_REQUEST_LEN, d1
	bsr.w tkpkgDebugCliWriteInputWindowV1
	moveq #abi.ENTRY_ORD_SET_PIPELINE, d0
	bsr.w tkpkgDebugCliDispatchServiceV1
	bsr.w tkpkgDebugCliReadStatusV1
	bne.w tkpkgDebugCliReportFailure

.ifdef OPFORGE_FS_UAE_TKPKG_OPERAND_RECORD
	bsr.w tkpkgDebugCliRunOperandRecordBatchV1
	tst.l d0
	bmi.w tkpkgDebugCliCloseDos
	bne.w tkpkgDebugCliReportFailure
	bra.w tkpkgDebugCliCheckLastErrorClear
.endif

	move.l DebugCliFileModeEnabled, d0

.ifdef OPFORGE_FS_UAE_TKPKG_FIXED_OPCODE
	lea FixedNopRequest, a1
	lea buffers.lastErrorBuffer, a2
	moveq #FIXED_NOP_REQUEST_LEN, d0
	bsr.w tkpkgDebugCliCopyBytesV1

	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	moveq #FIXED_NOP_REQUEST_LEN, d1
	bsr.w tkpkgDebugCliWriteInputWindowV1
	jsr compact.findFixedProgramFromRequestV1
	bne.w tkpkgDebugCliReportFixedTableMalformed
	tst.w d1
	beq.w tkpkgDebugCliReportFixedTableNoMatch
	cmpi.w #5, d1
	bne.w tkpkgDebugCliReportFixedTableMismatch
	cmpi.b #$01, (a1)+
	bne.w tkpkgDebugCliReportFixedTableMismatch
	cmpi.b #$4e, (a1)+
	bne.w tkpkgDebugCliReportFixedTableMismatch
	cmpi.b #$01, (a1)+
	bne.w tkpkgDebugCliReportFixedTableMismatch
	cmpi.b #$71, (a1)+
	bne.w tkpkgDebugCliReportFixedTableMismatch
	cmpi.b #$ff, (a1)
	bne.w tkpkgDebugCliReportFixedTableMismatch
	move.l #FixedTableSuccessText, d1
	bsr.w tkpkgDebugCliPutStrV1
	lea buffers.ControlBlockV1, a0
	moveq #abi.ENTRY_ORD_ENCODE_SELECTED_INSTRUCTION, d0
	bsr.w tkpkgDebugCliDispatchServiceV1
	bsr.w tkpkgDebugCliReadStatusV1
	bne.w tkpkgDebugCliReportFailure
	bsr.w tkpkgDebugCliReadOutputLenV1
	cmpi.w #2, d0
	bne.w tkpkgDebugCliReportFixedOpcodeMismatch
	moveq #0, d0
	move.b abi.CB_OUTPUT_PTR(a0), d0
	moveq #0, d1
	move.b 21(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.w), a1
	cmpi.b #$4e, (a1)+
	bne.w tkpkgDebugCliReportFixedOpcodeMismatch
	cmpi.b #$71, (a1)
	bne.w tkpkgDebugCliReportFixedOpcodeMismatch
	move.l #FixedOpcodeSuccessText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bra.w tkpkgDebugCliCheckLastErrorClear
.endif

	cmpi.l #DEBUG_CLI_FILE_MODE_MANIFEST, d0
	beq.w tkpkgDebugCliPipelineManifestMode
	tst.l d0
	bne.w tkpkgDebugCliPipelineFileMode

	move.l #PipelineSuccessText, d1
	bsr.w tkpkgDebugCliPutStrV1

	lea TokenizeLineRequest, a1
	lea buffers.lastErrorBuffer, a2
	moveq #TOKENIZE_LINE_REQUEST_LEN, d0
	bsr.w tkpkgDebugCliCopyBytesV1

	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	moveq #TOKENIZE_LINE_REQUEST_LEN, d1
	bsr.w tkpkgDebugCliWriteInputWindowV1
	moveq #abi.ENTRY_ORD_TOKENIZE_LINE, d0
	bsr.w tkpkgDebugCliDispatchServiceV1
	bsr.w tkpkgDebugCliReadStatusV1
	bne.w tkpkgDebugCliReportFailure
	bsr.w tkpkgDebugCliReadOutputLenV1
	beq.w tkpkgDebugCliReportEmptyTokenizeOutput
	move.l #TokenizeSuccessText, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #buffers.lastErrorBuffer, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #NewlineText, d1
	bsr.w tkpkgDebugCliPutStrV1

	bra.w tkpkgDebugCliCheckLastErrorClear

tkpkgDebugCliPipelineManifestMode
	move.l #PipelineSuccessText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bsr.w tkpkgDebugCliTokenizeManifestV1
	bmi.w tkpkgDebugCliCloseDos
	bne.w tkpkgDebugCliReportFailure
	bra.w tkpkgDebugCliCheckLastErrorClear

tkpkgDebugCliPipelineFileMode
	move.l #PipelineSuccessText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bsr.w tkpkgDebugCliTokenizeFileV1
	bmi.w tkpkgDebugCliCloseDos
	bne.w tkpkgDebugCliReportFailure

tkpkgDebugCliCheckLastErrorClear

	lea buffers.ControlBlockV1, a0
	bsr.w tkpkgDebugCliRunLastErrorV1
	bne.w tkpkgDebugCliCloseDos
	bsr.w tkpkgDebugCliReadOutputLenV1
	bne.w tkpkgDebugCliReportLastErrorBuffer

	move.l #LastErrorClearText, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #RETURN_OK, DebugCliReturnCode
	bra.w tkpkgDebugCliCloseDos

tkpkgDebugCliReportFailure
	lea buffers.ControlBlockV1, a0
	bsr.w tkpkgDebugCliRunLastErrorV1
	bne.w tkpkgDebugCliCloseDos

tkpkgDebugCliReportLastErrorBuffer
	move.l #FailurePrefixText, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #buffers.lastErrorBuffer, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #NewlineText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bra.w tkpkgDebugCliCloseDos

tkpkgDebugCliReportEmptyTokenizeOutput
	move.l #EmptyTokenizeOutputText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bra.w tkpkgDebugCliCloseDos

tkpkgDebugCliReportFixedOpcodeMismatch
	move.l #FixedOpcodeMismatchText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bra.w tkpkgDebugCliCloseDos

tkpkgDebugCliReportFixedTableMalformed
	move.l #FixedTableMalformedText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bra.w tkpkgDebugCliCloseDos

tkpkgDebugCliReportFixedTableNoMatch
	move.l #FixedTableNoMatchText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bra.w tkpkgDebugCliCloseDos

tkpkgDebugCliReportFixedTableMismatch
	move.l #FixedTableMismatchText, d1
	bsr.w tkpkgDebugCliPutStrV1

tkpkgDebugCliCloseDos
	bsr.w tkpkgDebugCliCloseDosV1

tkpkgDebugCliDone
	move.l DebugCliReturnCode, d0
	rts

tkpkgDebugCliDispatchServiceV1
	move.l d7, -(sp)
	move.l a5, -(sp)
	move.l a6, -(sp)
	jsr service.dispatchV1
	movea.l (sp)+, a6
	movea.l (sp)+, a5
	move.l (sp)+, d7
	rts

tkpkgDebugCliPutStrV1
	movea.l DebugCliDosBase, a6
	jsr PUT_STR(a6)
	rts

; Parse the optional single-file input path from DOS args.
; Inputs: DebugCliDosBase = open dos.library base.
; Outputs: D0.L = file mode (0 none, 1 single-file, -1 usage/failure); DebugCliInputPathBuffer filled on success.
; Clobbers: D0-D1/A1/A3/A6/CCR.
; CCR: reflects D0.L on return.
tkpkgDebugCliParseOptionalInputPathV1
	movem.l d2-d7/a2-a6, -(sp)
	movea.l DebugCliDosBase, a6
	jsr GET_ARG_STR(a6)
	movea.l d0, a3
	bsr.w tkpkgDebugCliSkipWhitespace
	tst.b (a3)
	beq.s tkpkgDebugCliNoInputPath
	cmpi.b #'"', (a3)
	beq.s tkpkgDebugCliQuotedPath
	lea DebugCliInputPathBuffer, a1
	bsr.w tkpkgDebugCliCopyPathToken
	bne.s tkpkgDebugCliUsagePath
	bsr.w tkpkgDebugCliSkipWhitespace
	tst.b (a3)
	bne.s tkpkgDebugCliUsagePath
	moveq #1, d0
	bra.s tkpkgDebugCliParseArgsDone

tkpkgDebugCliNoInputPath
	moveq #0, d0
	bra.s tkpkgDebugCliParseArgsDone

tkpkgDebugCliQuotedPath
	move.l #QuotedPathFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.s tkpkgDebugCliParseArgsDone

tkpkgDebugCliUsagePath
	move.l #UsageText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0

tkpkgDebugCliParseArgsDone
	movem.l (sp)+, d2-d7/a2-a6
	rts

tkpkgDebugCliSkipWhitespace
	cmpi.b #' ', (a3)
	beq.s tkpkgDebugCliSkipOne
	cmpi.b #9, (a3)
	beq.s tkpkgDebugCliSkipOne
	cmpi.b #10, (a3)
	beq.s tkpkgDebugCliSkipOne
	cmpi.b #13, (a3)
	bne.s tkpkgDebugCliSkipDone

tkpkgDebugCliSkipOne
	addq.l #1, a3
	bra.s tkpkgDebugCliSkipWhitespace

tkpkgDebugCliSkipDone
	rts

; Copy one unquoted whitespace-delimited path token from A3 into A1.
; Inputs: A3 = current DOS arg cursor; A1 = destination buffer.
; Outputs: D0.L = 0 on success, 1 on invalid/too-long token; A3/A1 advanced on copied bytes.
; Clobbers: D0/D6/CCR.
; CCR: reflects D0.L on return.
tkpkgDebugCliCopyPathToken
	move.l #PATH_BUFFER_CAPACITY - 1, d6

tkpkgDebugCliCopyPathLoop
	moveq #0, d0
	move.b (a3), d0
	beq.s tkpkgDebugCliCopyPathDone
	cmpi.b #' ', d0
	beq.s tkpkgDebugCliCopyPathDone
	cmpi.b #9, d0
	beq.s tkpkgDebugCliCopyPathDone
	cmpi.b #10, d0
	beq.s tkpkgDebugCliCopyPathDone
	cmpi.b #13, d0
	beq.s tkpkgDebugCliCopyPathDone
	cmpi.b #'"', d0
	beq.s tkpkgDebugCliCopyPathFail
	tst.l d6
	beq.s tkpkgDebugCliCopyPathFail
	move.b d0, (a1)+
	addq.l #1, a3
	subq.l #1, d6
	bra.s tkpkgDebugCliCopyPathLoop

tkpkgDebugCliCopyPathDone
	clr.b (a1)
	moveq #0, d0
	rts

tkpkgDebugCliCopyPathFail
	moveq #1, d0
	rts

; Tokenize each file listed in the manifest path currently in DebugCliInputPathBuffer.
; Inputs: DebugCliInputPathBuffer = manifest path; DebugCliDosBase = open dos.library base.
; Outputs: D0.L = 0 on success, 1 on tokenizer/manifest validation failure, -1 on DOS/file I/O failure.
; Clobbers: D0-D7/A0-A6/CCR.
; CCR: reflects D0.L on return.
tkpkgDebugCliTokenizeManifestV1
	movem.l d2-d7/a2-a6, -(sp)
	lea DebugCliInputPathBuffer, a0
	bsr.w tkpkgDebugCliOpenInputV1
	tst.l d0
	bne.s tkpkgDebugCliManifestOpenOk
	move.l #ManifestOpenFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliTokenizeManifestDone

tkpkgDebugCliManifestOpenOk
	move.l d0, d5
	move.l #ManifestOpenOkText, d1
	bsr.w tkpkgDebugCliPutStrV1
	lea DebugCliManifestBuffer, a0
	move.l #DEBUG_CLI_MANIFEST_BUFFER_CAPACITY, d0
	move.l d5, d1
	bsr.w tkpkgDebugCliReadInputV1
	cmp.l #-1, d0
	bne.s tkpkgDebugCliManifestReadOk
	move.l d5, d1
	bsr.w tkpkgDebugCliCloseInputV1
	move.l #InputReadFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliTokenizeManifestDone

tkpkgDebugCliManifestReadOk
	move.l d0, d6
	lea DebugCliSourceFileProbeByte, a0
	moveq #1, d0
	move.l d5, d1
	bsr.w tkpkgDebugCliReadInputV1
	move.l d0, d7
	move.l d5, d1
	bsr.w tkpkgDebugCliCloseInputV1
	cmp.l #-1, d7
	bne.s tkpkgDebugCliManifestProbeOk
	move.l #InputReadFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliTokenizeManifestDone

tkpkgDebugCliManifestProbeOk
	tst.l d7
	beq.s tkpkgDebugCliManifestFitsBuffer
	move.l #ManifestTooLargeText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliTokenizeManifestDone

tkpkgDebugCliManifestFitsBuffer
	move.l #ManifestReadOkText, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l d6, d0
	bsr.w tkpkgDebugCliTokenizeManifestBufferV1

tkpkgDebugCliTokenizeManifestDone
	movem.l (sp)+, d2-d7/a2-a6
	rts

tkpkgDebugCliTokenizeManifestBufferV1
	movem.l d2-d7/a2-a6, -(sp)
	lea DebugCliManifestBuffer, a3
	move.l d0, d7

tkpkgDebugCliManifestLineLoop
	tst.l d7
	beq.w tkpkgDebugCliManifestDone
	movea.l a3, a1
	moveq #0, d0

tkpkgDebugCliFindManifestLineEnd
	tst.l d7
	beq.s tkpkgDebugCliManifestLineReady
	cmpi.b #10, (a3)
	beq.s tkpkgDebugCliConsumeManifestLf
	addq.l #1, a3
	addq.l #1, d0
	subq.l #1, d7
	bra.s tkpkgDebugCliFindManifestLineEnd

tkpkgDebugCliConsumeManifestLf
	addq.l #1, a3
	subq.l #1, d7

tkpkgDebugCliManifestLineReady
	tst.l d0
	beq.s tkpkgDebugCliManifestSkipLine
	lea 0(a1, d0.l), a2
	subq.l #1, a2
	cmpi.b #13, (a2)
	bne.s tkpkgDebugCliManifestLineTrimmed
	subq.l #1, d0
	beq.s tkpkgDebugCliManifestSkipLine

tkpkgDebugCliManifestLineTrimmed
	cmpi.b #'#', (a1)
	beq.s tkpkgDebugCliManifestSkipLine
	cmpi.b #';', (a1)
	beq.s tkpkgDebugCliManifestSkipLine
	bsr.w tkpkgDebugCliPrepareManifestEntryV1
	bmi.w tkpkgDebugCliManifestReturn
	beq.s tkpkgDebugCliManifestEntryOk
	bra.w tkpkgDebugCliManifestReturn

tkpkgDebugCliManifestEntryOk
	move.l #ManifestFileText, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #DebugCliInputPathBuffer, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #NewlineText, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #ManifestTokenizeBeginText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bsr.w tkpkgDebugCliTokenizeFileV1
	bne.s tkpkgDebugCliManifestReturn
	move.l #ManifestTokenizeOkText, d1
	bsr.w tkpkgDebugCliPutStrV1

tkpkgDebugCliManifestSkipLine
	bra.w tkpkgDebugCliManifestLineLoop

tkpkgDebugCliManifestDone
	moveq #0, d0

tkpkgDebugCliManifestReturn
	movem.l (sp)+, d2-d7/a2-a6
	rts

; Prepare one manifest entry by optionally selecting a pipeline and copying the path.
; Inputs: A1 = manifest line start; D0.L = trimmed line length.
; Outputs: D0.L = 0 on success, 1 on pipeline-set failure, -1 on invalid manifest entry; DebugCliInputPathBuffer updated on success.
; Clobbers: D0-D1/D5-D7/A1-A4/CCR.
; CCR: reflects D0.L on return.
tkpkgDebugCliPrepareManifestEntryV1
	movem.l d2-d7/a2-a6, -(sp)
	movea.l a1, a4
	move.l d0, d7
	movea.l a1, a3
	move.l d0, d6
	moveq #0, d5

tkpkgDebugCliManifestFindTab
	tst.l d6
	beq.w tkpkgDebugCliManifestNoPipeline
	cmpi.b #9, (a3)
	beq.s tkpkgDebugCliManifestHasPipeline
	addq.l #1, a3
	addq.l #1, d5
	subq.l #1, d6
	bra.s tkpkgDebugCliManifestFindTab

tkpkgDebugCliManifestHasPipeline
	tst.l d5
	bne.s tkpkgDebugCliManifestCpuPresent
	move.l #ManifestFormatFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestCpuPresent
	move.l d7, d0
	sub.l d5, d0
	subq.l #1, d0
	tst.l d0
	bne.s tkpkgDebugCliManifestPathPresent
	move.l #ManifestFormatFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestPathPresent
	cmp.l #buffers.LAST_ERROR_BUFFER_CAPACITY - 1, d5
	bls.s tkpkgDebugCliManifestCpuFits
	move.l #ManifestPipelineTooLongText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestCpuFits
	lea buffers.lastErrorBuffer, a2
	movea.l a4, a1
	move.l d5, d0
	bsr.w tkpkgDebugCliCopyBytesV1
	clr.b (a2)
	movem.l d5-d7/a3-a4, -(sp)
	move.l #ManifestPipelineBeginText, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #buffers.lastErrorBuffer, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #NewlineText, d1
	bsr.w tkpkgDebugCliPutStrV1
	movem.l (sp)+, d5-d7/a3-a4
	move.l d5, d0
	addq.l #1, d0
	movem.l d5-d7/a3-a4, -(sp)
	bsr.w tkpkgDebugCliSetPipelineFromLastErrorV1
	move.l d0, d1
	movem.l (sp)+, d5-d7/a3-a4
	move.l d1, d0
	tst.b d0
	beq.s tkpkgDebugCliManifestPipelineOk
	moveq #1, d0
	bra.w tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestPipelineOk
	movem.l d5-d7/a3-a4, -(sp)
	move.l #ManifestPipelineOkText, d1
	bsr.w tkpkgDebugCliPutStrV1
	movem.l (sp)+, d5-d7/a3-a4
	lea 1(a3), a1
	move.l d7, d0
	sub.l d5, d0
	subq.l #1, d0
	bsr.w tkpkgDebugCliCopyManifestPathV1
	beq.s tkpkgDebugCliManifestPrepareOk
	move.l #ManifestPathTooLongText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.s tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestNoPipeline
	movea.l a4, a1
	move.l d7, d0
	bsr.w tkpkgDebugCliCopyManifestPathV1
	beq.s tkpkgDebugCliManifestPrepareOk
	move.l #ManifestPathTooLongText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.s tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestPrepareOk
	moveq #0, d0

tkpkgDebugCliManifestPrepareDone
	movem.l (sp)+, d2-d7/a2-a6
	rts

; Copy one manifest-selected path into DebugCliInputPathBuffer.
; Inputs: A1 = source path bytes; D0.L = path length.
; Outputs: D0.L = 0 on success, -1 when the path does not fit; DebugCliInputPathBuffer written on success.
; Clobbers: D0/D2/A0/A2/CCR.
; CCR: reflects D0.L on return.
tkpkgDebugCliCopyManifestPathV1
	movem.l d1-d3/a0-a2, -(sp)
	cmp.l #PATH_BUFFER_CAPACITY - 1, d0
	bls.s tkpkgDebugCliManifestCopyFits
	moveq #-1, d0
	bra.s tkpkgDebugCliManifestCopyDone

tkpkgDebugCliManifestCopyFits
	lea DebugCliInputPathBuffer, a0
	movea.l a1, a2
	move.l d0, d2

tkpkgDebugCliManifestCopyLoop
	tst.l d2
	beq.s tkpkgDebugCliManifestCopyTerminator
	move.b (a2)+, (a0)+
	subq.l #1, d2
	bra.s tkpkgDebugCliManifestCopyLoop

tkpkgDebugCliManifestCopyTerminator
	clr.b (a0)
	moveq #0, d0

tkpkgDebugCliManifestCopyDone
	movem.l (sp)+, d1-d3/a0-a2
	rts

tkpkgDebugCliSetPipelineFromLastErrorV1
	movem.l d1/a0, -(sp)
	lea buffers.ControlBlockV1, a0
	move.w d0, d1
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	bsr.w tkpkgDebugCliWriteInputWindowV1
	moveq #abi.ENTRY_ORD_SET_PIPELINE, d0
	bsr.w tkpkgDebugCliDispatchServiceV1
	bsr.w tkpkgDebugCliReadStatusV1
	movem.l (sp)+, d1/a0
	rts

; Tokenize the single source file currently in DebugCliInputPathBuffer.
; Inputs: DebugCliInputPathBuffer = source path; DebugCliDosBase = open dos.library base.
; Outputs: D0.L = 0 on success, 1 on tokenizer/runtime failure, -1 on DOS/file I/O failure.
; Clobbers: D0-D7/A0-A6/CCR.
; CCR: reflects D0.L on return.
tkpkgDebugCliTokenizeFileV1
	movem.l d2-d7/a2-a6, -(sp)
	lea DebugCliInputPathBuffer, a0
	bsr.w tkpkgDebugCliOpenInputV1
	tst.l d0
	bne.s tkpkgDebugCliFileOpenOk
	move.l #InputOpenFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliTokenizeFileDone

tkpkgDebugCliFileOpenOk
	move.l d0, d5
	lea DebugCliSourceFileBuffer, a0
	move.l #DEBUG_CLI_SOURCE_BUFFER_CAPACITY, d0
	move.l d5, d1
	bsr.w tkpkgDebugCliReadInputV1
	cmp.l #-1, d0
	bne.s tkpkgDebugCliFileReadOk
	move.l d5, d1
	bsr.w tkpkgDebugCliCloseInputV1
	move.l #InputReadFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliTokenizeFileDone

tkpkgDebugCliFileReadOk
	move.l d0, d6
	lea DebugCliSourceFileProbeByte, a0
	moveq #1, d0
	move.l d5, d1
	bsr.w tkpkgDebugCliReadInputV1
	move.l d0, d7
	move.l d5, d1
	bsr.w tkpkgDebugCliCloseInputV1
	cmp.l #-1, d7
	bne.s tkpkgDebugCliFileProbeOk
	move.l #InputReadFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliTokenizeFileDone

tkpkgDebugCliFileProbeOk
	tst.l d7
	beq.s tkpkgDebugCliFileFitsBuffer
	move.l #FileTooLargeText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliTokenizeFileDone

tkpkgDebugCliFileFitsBuffer
	move.l d6, d0
	bsr.w tkpkgDebugCliTokenizeSourceBufferV1

tkpkgDebugCliTokenizeFileDone
	movem.l (sp)+, d2-d7/a2-a6
	rts

tkpkgDebugCliTokenizeSourceBufferV1
	movem.l d2-d7/a2-a6, -(sp)
	lea DebugCliSourceFileBuffer, a3
	move.l d0, d7
	moveq #1, d6

tkpkgDebugCliSourceLineLoop
	tst.l d7
	beq.s tkpkgDebugCliSourceDone
	movea.l a3, a1
	moveq #0, d0

tkpkgDebugCliFindLineEnd
	tst.l d7
	beq.s tkpkgDebugCliLineReady
	cmpi.b #10, (a3)
	beq.s tkpkgDebugCliConsumeLf
	addq.l #1, a3
	addq.l #1, d0
	subq.l #1, d7
	bra.s tkpkgDebugCliFindLineEnd

tkpkgDebugCliConsumeLf
	addq.l #1, a3
	subq.l #1, d7

tkpkgDebugCliLineReady
	tst.l d0
	beq.s tkpkgDebugCliLineDispatch
	lea 0(a1, d0.l), a2
	subq.l #1, a2
	cmpi.b #13, (a2)
	bne.s tkpkgDebugCliLineDispatch
	subq.l #1, d0

tkpkgDebugCliLineDispatch
	move.l d6, d1
	bsr.w tkpkgDebugCliTokenizeLineSliceV1
	bne.w tkpkgDebugCliSourceReturn
	addq.l #1, d6
	bra.w tkpkgDebugCliSourceLineLoop

tkpkgDebugCliSourceDone
	moveq #0, d0

tkpkgDebugCliSourceReturn
	movem.l (sp)+, d2-d7/a2-a6
	rts

tkpkgDebugCliTokenizeLineSliceV1
	movem.l d2-d7/a2-a6, -(sp)
	tst.l d0
	beq.w tkpkgDebugCliSliceOk
	cmpi.l #DEBUG_CLI_MAX_LINE_BYTES, d0
	bls.w tkpkgDebugCliSliceFits
	move.l #LineTooLongText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w tkpkgDebugCliSliceDone

tkpkgDebugCliSliceFits
	movea.l a1, a3
	move.l d0, d7
	lea buffers.lastErrorBuffer, a2
	move.l d1, d2
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	movea.l a3, a1
	move.l d7, d0
	bsr.w tkpkgDebugCliCopyBytesV1

	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	move.w d7, d1
	addq.w #4, d1
	bsr.w tkpkgDebugCliWriteInputWindowV1
	moveq #abi.ENTRY_ORD_TOKENIZE_LINE, d0
	bsr.w tkpkgDebugCliDispatchServiceV1
	bsr.w tkpkgDebugCliReadStatusV1
	bne.w tkpkgDebugCliSliceRuntimeFailure
	bsr.w tkpkgDebugCliReadOutputLenV1
	beq.w tkpkgDebugCliSliceOk
	lea buffers.lastErrorBuffer, a1
	clr.b 0(a1, d0.l)
	move.l #buffers.lastErrorBuffer, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #NewlineText, d1
	bsr.w tkpkgDebugCliPutStrV1

tkpkgDebugCliSliceOk
	moveq #0, d0
	bra.w tkpkgDebugCliSliceDone

tkpkgDebugCliSliceRuntimeFailure
	moveq #1, d0

tkpkgDebugCliSliceDone
	movem.l (sp)+, d2-d7/a2-a6
	rts

tkpkgDebugCliOpenInputV1
	move.l a0, d1
	move.l #MODE_OLDFILE, d2
	movea.l DebugCliDosBase, a6
	jsr OPEN(a6)
	rts

tkpkgDebugCliReadInputV1
	move.l a0, d2
	move.l d0, d3
	movea.l DebugCliDosBase, a6
	jsr READ(a6)
	rts

tkpkgDebugCliCloseInputV1
	movea.l DebugCliDosBase, a6
	jsr CLOSE(a6)
	rts

tkpkgDebugCliCloseDosV1
	movea.l DebugCliDosBase, a1
	movea.l SYS_BASE.W, a6
	jsr CLOSE_LIBRARY(a6)
	rts

; Execute a host-staged batch of exact operand-record service requests.
; Batch bytes are `u16 count`, then repeated `u16 request_len, request_bytes`,
; all little-endian. Length bit 15 asks the harness to place that request in the
; result buffer so the service's overlap rejection can be exercised. Each
; successful fixed-size result is rendered as one
; `TKPKG OPRD <hex>` row for exact host comparison.
; Inputs: DebugCliInputPathBuffer names the staged batch file.
; Outputs: D0.L = 0 success, 1 service failure, -1 harness/file failure.
; Clobbers: D0-D7/A0-A6/CCR. CCR: reflects D0.L on return.
tkpkgDebugCliRunOperandRecordBatchV1
	movem.l d2-d7/a2-a6, -(sp)
	lea DebugCliInputPathBuffer, a0
	bsr.w tkpkgDebugCliOpenInputV1
	tst.l d0
	bne.s opened
	move.l #InputOpenFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.w return
opened
	move.l d0, d5
	lea DebugCliSourceFileBuffer, a0
	move.l #DEBUG_CLI_SOURCE_BUFFER_CAPACITY, d0
	move.l d5, d1
	bsr.w tkpkgDebugCliReadInputV1
	cmpi.l #-1, d0
	beq.w readFail
	move.l d0, d7
	lea DebugCliSourceFileProbeByte, a0
	moveq #1, d0
	move.l d5, d1
	bsr.w tkpkgDebugCliReadInputV1
	move.l d0, d6
	move.l d5, d1
	bsr.w tkpkgDebugCliCloseInputV1
	cmpi.l #-1, d6
	beq.w reportReadFail
	tst.l d6
	bne.w malformedBatch
	cmpi.l #2, d7
	bcs.w malformedBatch
	lea DebugCliSourceFileBuffer, a3
	moveq #0, d6
	move.b (a3)+, d6
	moveq #0, d0
	move.b (a3)+, d0
	lsl.w #8, d0
	or.w d0, d6
	subq.l #2, d7
	tst.w d6
	beq.w malformedBatch

caseLoop
	cmpi.l #2, d7
	bcs.w malformedBatch
	moveq #0, d5
	move.b (a3)+, d5
	moveq #0, d0
	move.b (a3)+, d0
	lsl.w #8, d0
	or.w d0, d5
	subq.l #2, d7
	clr.b OperandRecordOverlapRequestFlag
	btst #15, d5
	beq.s requestLengthReady
	bclr #15, d5
	st OperandRecordOverlapRequestFlag
requestLengthReady
	tst.w d5
	beq.w malformedBatch
	cmpi.w #buffers.LAST_ERROR_BUFFER_CAPACITY, d5
	bhi.w malformedBatch
	moveq #0, d0
	move.w d5, d0
	cmp.l d7, d0
	bhi.w malformedBatch
	movea.l a3, a1
	lea buffers.LastErrorBuffer, a2
	tst.b OperandRecordOverlapRequestFlag
	beq.s requestDestinationReady
	lea buffers.OperandRecordResultBuffer, a2
requestDestinationReady
	bsr.w tkpkgDebugCliCopyBytesV1
	adda.w d5, a3
	sub.l d0, d7
	movem.l d5-d7/a3, -(sp)
	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	tst.b OperandRecordOverlapRequestFlag
	beq.s requestPointerReady
	move.w #buffers.OPERAND_RECORD_RESULT_BUFFER_PTR_V1, d0
requestPointerReady
	move.w d5, d1
	bsr.w tkpkgDebugCliWriteInputWindowV1
	moveq #abi.ENTRY_ORD_EXECUTE_OPERAND_RECORD, d0
	bsr.w tkpkgDebugCliDispatchServiceV1
	bsr.w tkpkgDebugCliReadStatusV1
	move.l d0, d4
	movem.l (sp)+, d5-d7/a3
	tst.l d4
	bne.s serviceFail
	lea buffers.ControlBlockV1, a0
	bsr.w tkpkgDebugCliReadOutputLenV1
	cmpi.w #abi.OPERAND_RECORD_RESULT_SIZE_V1, d0
	bne.s malformedResult
	moveq #0, d0
	move.b abi.CB_OUTPUT_PTR(a0), d0
	moveq #0, d1
	move.b 21(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.w), a1
	movem.l d5-d7/a3, -(sp)
	bsr.w tkpkgDebugCliRenderOperandRecordV1
	movem.l (sp)+, d5-d7/a3
	subq.w #1, d6
	bne.w caseLoop
	tst.l d7
	bne.s malformedBatch
	moveq #0, d0
	bra.s return

readFail
	move.l d5, d1
	bsr.w tkpkgDebugCliCloseInputV1
reportReadFail
	move.l #InputReadFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.s return
malformedBatch
	move.l #OperandRecordBatchFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.s return
malformedResult
	move.l #OperandRecordResultFailureText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.s return
serviceFail
	moveq #1, d0
return
	movem.l (sp)+, d2-d7/a2-a6
	rts

; Render the 24-byte neutral result at A1 as one exact uppercase-hex row.
; Outputs: row written to stdout. Clobbers: D0-D5/A0-A2/A6/CCR.
tkpkgDebugCliRenderOperandRecordV1
	movem.l d6-d7/a3-a5, -(sp)
	movea.l a1, a3
	move.l #OperandRecordRowPrefixText, d1
	bsr.w tkpkgDebugCliPutStrV1
	lea OperandRecordHexBuffer, a2
	lea OperandRecordHexDigits, a4
	moveq #abi.OPERAND_RECORD_RESULT_SIZE_V1 - 1, d5
byteLoop
	moveq #0, d0
	move.b (a3)+, d0
	move.l d0, d1
	lsr.b #4, d1
	move.b 0(a4, d1.w), (a2)+
	andi.b #$0F, d0
	move.b 0(a4, d0.w), (a2)+
	dbf d5, byteLoop
	move.b #10, (a2)+
	clr.b (a2)
	move.l #OperandRecordHexBuffer, d1
	bsr.w tkpkgDebugCliPutStrV1
	movem.l (sp)+, d6-d7/a3-a5
	rts

tkpkgDebugCliCopyBytesV1
	move.l d0, d2
	beq.s tkpkgDebugCliCopyDone

tkpkgDebugCliCopyLoop
	move.b (a1)+, (a2)+
	subq.l #1, d2
	bne.s tkpkgDebugCliCopyLoop

tkpkgDebugCliCopyDone
	rts

tkpkgDebugCliWriteInputWindowV1
	move.b d0, abi.CB_INPUT_PTR(a0)
	lsr.w #8, d0
	move.b d0, 17(a0)
	move.b d1, abi.CB_INPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 19(a0)
	rts

tkpkgDebugCliClearInputWindowV1
	clr.b abi.CB_INPUT_PTR(a0)
	clr.b 17(a0)
	clr.b abi.CB_INPUT_LEN(a0)
	clr.b 19(a0)
	rts

tkpkgDebugCliReadStatusV1
	moveq #0, d0
	move.b abi.CB_STATUS_CODE(a0), d0
	rts

tkpkgDebugCliReadOutputLenV1
	moveq #0, d0
	move.b abi.CB_OUTPUT_LEN(a0), d0
	moveq #0, d1
	move.b 23(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	rts

tkpkgDebugCliRunLastErrorV1
	bsr.w tkpkgDebugCliClearInputWindowV1
	moveq #abi.ENTRY_ORD_LAST_ERROR, d0
	jsr service.dispatchV1
	bsr.w tkpkgDebugCliReadStatusV1
	rts
	.bend ; start
	.priv

	.endsection

	.section data, kind=data

DosName
	.byte "dos.library", 0

DebugCliMarker
	.byte "TKPKG-DEBUG-CLI-V1", 0

FailurePrefixText
	.byte "tkpkg failure: ", 0

DosVersionFailureText
	.byte "tkpkg_debug_cli requires dos.library v36+", 10, 0

StartedText
	.byte "tkpkg_debug_cli started", 10, 0

.ifdef OPFORGE_FS_UAE_SMOKE
DefaultSmokeInputPath
.ifdef OPFORGE_FS_UAE_TKPKG_OPERAND_RECORD
	.byte "Work:opforge_fsuae_operand_records.bin", 0
.else
	.byte "Work:opforge_fsuae_smoke_input.asm", 0
.endif
.endif

.ifdef OPFORGE_FS_UAE_TKPKG_MANIFEST
DefaultSmokeManifestPath
	.byte "Work:opforge_fsuae_tkpkg_manifest.txt", 0
.endif

UsageText
	.byte "Usage: tkpkg_debug_cli [input-path]", 10, 0

QuotedPathFailureText
	.byte "tkpkg_debug_cli: quoted paths are not supported", 10, 0

PackageLoadedText
	.byte "tkpkg package loaded", 10, 0

PipelineSuccessText
	.byte "TKPKG load_package/set_pipeline OK", 10, 0

TokenizeSuccessText
	.byte "TKPKG tokenize_line OK", 10, 0

LastErrorClearText
	.byte "TKPKG last_error clear OK", 10, 0

FixedOpcodeSuccessText
	.byte "TKPKG fixed opcode 4E71 OK", 10, 0

FixedOpcodeMismatchText
	.byte "tkpkg failure: fixed opcode output was not 4E71", 10, 0

FixedTableSuccessText
	.byte "TKPKG fixed table program 014E0171FF OK", 10, 0

FixedTableMalformedText
	.byte "tkpkg failure: compact table lookup malformed", 10, 0

FixedTableNoMatchText
	.byte "tkpkg failure: compact table lookup found no NOP", 10, 0

FixedTableNoOwnerText
	.byte "tkpkg failure: compact table found no active family owner", 10, 0

FixedTableNoMnemonicText
	.byte "tkpkg failure: compact table found no NOP string", 10, 0

FixedTableNoProgramText
	.byte "tkpkg failure: compact table found no NOP program row", 10, 0

FixedTableMismatchText
	.byte "tkpkg failure: compact table NOP program mismatch", 10, 0

EmptyTokenizeOutputText
	.byte "tkpkg failure: tokenize_line returned empty output", 10, 0

InputOpenFailureText
	.byte "tkpkg failure: failed to open input file", 10, 0

ManifestOpenFailureText
	.byte "tkpkg failure: failed to open manifest file", 10, 0

InputReadFailureText
	.byte "tkpkg failure: failed to read input file", 10, 0

FileTooLargeText
	.byte "tkpkg failure: input file exceeds debug-cli buffer", 10, 0

ManifestTooLargeText
	.byte "tkpkg failure: manifest file exceeds debug-cli buffer", 10, 0

ManifestPathTooLongText
	.byte "tkpkg failure: manifest path exceeds debug-cli path buffer", 10, 0

ManifestPipelineTooLongText
	.byte "tkpkg failure: manifest pipeline exceeds debug-cli buffer", 10, 0

ManifestFormatFailureText
	.byte "tkpkg failure: manifest entry requires cpu, tab, and path", 10, 0

ManifestFileText
	.byte "TKPKG manifest file ", 0

ManifestOpenOkText
	.byte "TKPKG manifest open OK", 10, 0

ManifestReadOkText
	.byte "TKPKG manifest read OK", 10, 0

ManifestPipelineBeginText
	.byte "TKPKG manifest set_pipeline begin ", 0

ManifestPipelineOkText
	.byte "TKPKG manifest set_pipeline OK", 10, 0

ManifestTokenizeBeginText
	.byte "TKPKG manifest tokenize_file begin", 10, 0

ManifestTokenizeOkText
	.byte "TKPKG manifest tokenize_file OK", 10, 0

LineTooLongText
	.byte "tkpkg failure: input line exceeds tokenize_line payload budget", 10, 0

OperandRecordBatchFailureText
	.byte "tkpkg failure: operand-record batch malformed", 10, 0

OperandRecordResultFailureText
	.byte "tkpkg failure: operand-record result malformed", 10, 0

OperandRecordRowPrefixText
	.byte "TKPKG OPRD ", 0

OperandRecordHexDigits
	.byte "0123456789ABCDEF"

NewlineText
	.byte 10, 0

	.align 2

DebugCliDosBase
	.long 0

DebugCliReturnCode
	.long RETURN_FAIL

DebugCliFileModeEnabled
	.long 0

TkpkgDebugCliPackageLen
	.long TKPKG_DEBUG_CLI_PACKAGE_LEN

setPipelineRequest
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
SetPipelineRequestEnd

TokenizeLineRequest
	.byte abi.TOKENIZE_LINE_SAMPLE_LINE_NUM, 0, 0, 0
	.byte "move.b d0,d1"
tokenizeLineRequestEnd

FixedNopRequest
	.byte 1, 0, 0, 0
	.byte 0, 0
	.byte 0, 0
	.byte 3, "nop"
FixedNopRequestEnd

	.align 2
tkpkgDebugCliPackageData
	.incbin "../../tkpkg/tkpkg_debug_cli_package.opasm"
TKPKG_DEBUG_CLI_PACKAGE_DATA_END

SET_PIPELINE_REQUEST_LEN = SetPipelineRequestEnd - setPipelineRequest
TOKENIZE_LINE_REQUEST_LEN = tokenizeLineRequestEnd - TokenizeLineRequest
FIXED_NOP_REQUEST_LEN = FixedNopRequestEnd - FixedNopRequest
TKPKG_DEBUG_CLI_PACKAGE_LEN = TKPKG_DEBUG_CLI_PACKAGE_DATA_END - tkpkgDebugCliPackageData

	.endsection
	.section bss, kind=bss

DebugCliInputPathBuffer
	.res byte, PATH_BUFFER_CAPACITY

DebugCliSourceFileBuffer
	.res byte, DEBUG_CLI_SOURCE_BUFFER_CAPACITY

DebugCliManifestBuffer
	.res byte, DEBUG_CLI_MANIFEST_BUFFER_CAPACITY

DebugCliSourceFileProbeByte
	.res byte, 1

OperandRecordHexBuffer
	.res byte, abi.OPERAND_RECORD_RESULT_SIZE_V1 * 2 + 2

OperandRecordOverlapRequestFlag
	.res byte, 1

	.endsection
	.output "build/tkpkg_debug_cli.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
