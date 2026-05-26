; Thin AmigaOS CLI smoke wrapper for the package-backed tkpkg runtime.
;
; This slice exercises init, load_package, set_pipeline, tokenize_line, and
; last_error without folding CLI/report concerns into the core runtime modules.

	.module main
	.cpu 68020
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
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

PACKAGE_INPUT_PTR_V1           = buffers.LAST_ERROR_BUFFER_PTR_V1 + buffers.LAST_ERROR_BUFFER_CAPACITY

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
	tst.b d0
	bne.w tkpkgDebugCliReportFailure

	lea tkpkgDebugCliPackageData, a1
	lea buffers.packageStorage, a2
	move.w TkpkgDebugCliPackageLen, d0
	bsr.w tkpkgDebugCliCopyBytesV1

	lea buffers.ControlBlockV1, a0
	move.w #PACKAGE_INPUT_PTR_V1, d0
	move.w TkpkgDebugCliPackageLen, d1
	bsr.w tkpkgDebugCliWriteInputWindowV1
	moveq #abi.ENTRY_ORD_LOAD_PACKAGE, d0
	bsr.w tkpkgDebugCliDispatchServiceV1
	bsr.w tkpkgDebugCliReadStatusV1
	tst.b d0
	bne.w tkpkgDebugCliReportFailure
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
	tst.l d0
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
	tst.b d0
	bne.w tkpkgDebugCliReportFailure

	move.l DebugCliFileModeEnabled, d0
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
	tst.b d0
	bne.w tkpkgDebugCliReportFailure
	bsr.w tkpkgDebugCliReadOutputLenV1
	tst.w d0
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
	tst.l d0
	bmi.w tkpkgDebugCliCloseDos
	bne.w tkpkgDebugCliReportFailure
	bra.w tkpkgDebugCliCheckLastErrorClear

tkpkgDebugCliPipelineFileMode
	move.l #PipelineSuccessText, d1
	bsr.w tkpkgDebugCliPutStrV1
	bsr.w tkpkgDebugCliTokenizeFileV1
	tst.l d0
	bmi.w tkpkgDebugCliCloseDos
	bne.w tkpkgDebugCliReportFailure

tkpkgDebugCliCheckLastErrorClear

	lea buffers.ControlBlockV1, a0
	bsr.w tkpkgDebugCliRunLastErrorV1
	tst.b d0
	bne.w tkpkgDebugCliCloseDos
	bsr.w tkpkgDebugCliReadOutputLenV1
	tst.w d0
	bne.w tkpkgDebugCliReportLastErrorBuffer

	move.l #LastErrorClearText, d1
	bsr.w tkpkgDebugCliPutStrV1
	move.l #RETURN_OK, DebugCliReturnCode
	bra.w tkpkgDebugCliCloseDos

tkpkgDebugCliReportFailure
	lea buffers.ControlBlockV1, a0
	bsr.w tkpkgDebugCliRunLastErrorV1
	tst.b d0
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
	tst.l d0
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
	tst.l d0
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
	tst.l d0
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
	tst.l d0
	beq.s tkpkgDebugCliManifestPrepareOk
	move.l #ManifestPathTooLongText, d1
	bsr.w tkpkgDebugCliPutStrV1
	moveq #-1, d0
	bra.s tkpkgDebugCliManifestPrepareDone

tkpkgDebugCliManifestNoPipeline
	movea.l a4, a1
	move.l d7, d0
	bsr.w tkpkgDebugCliCopyManifestPathV1
	tst.l d0
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
	tst.l d0
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
	tst.b d0
	bne.w tkpkgDebugCliSliceRuntimeFailure
	bsr.w tkpkgDebugCliReadOutputLenV1
	tst.w d0
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

tkpkgDebugCliCopyBytesV1
	move.w d0, d2
	tst.w d2
	beq.s tkpkgDebugCliCopyDone

tkpkgDebugCliCopyLoop
	move.b (a1)+, (a2)+
	subq.w #1, d2
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
	.byte "Work:opforge_fsuae_smoke_input.asm", 0
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
	.word TKPKG_DEBUG_CLI_PACKAGE_LEN

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

	.align 2
tkpkgDebugCliPackageData
	.incbin "../../tkpkg/tkpkg_debug_cli_package.opasm"
TKPKG_DEBUG_CLI_PACKAGE_DATA_END

SET_PIPELINE_REQUEST_LEN = SetPipelineRequestEnd - setPipelineRequest
TOKENIZE_LINE_REQUEST_LEN = tokenizeLineRequestEnd - TokenizeLineRequest
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

	.endsection
	.output "build/tkpkg_debug_cli.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
