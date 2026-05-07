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
	.use tkpkg.amigaos.buffers (BadRequestText, ControlBlockErrorText, RuntimeErrorText)
	.use tkpkg.amigaos.buffers (ControlBlockV1, NextRequestIdHi, NextRequestIdLo)
	.use tkpkg.amigaos.buffers (StoredLastErrorKind, StoredLastErrorLen)
	.use tkpkg.amigaos.buffers (StoredLastErrorLenHi)
	.use tkpkg.amigaos.buffers (LastErrorBuffer, PackageStorage)
	.use tkpkg.amigaos.buffers (TablChunkOffsetLo, TablChunkOffsetHi)
	.use tkpkg.amigaos.package_loader (tkpkgPackageLoaderLoadV1)
	.use tkpkg.amigaos.pipeline (tkpkgPipelineSetActiveV1)
	.use tkpkg.amigaos.tokenizer_vm (tkpkgTokenizerVmTokenizeLineV1)
	.use prvm.amigaos.line_router (prvmRouteLine68000)

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

tkpkgServiceBootstrapV1
	lea ControlBlockV1, a0  ; shared in-module CB used by the direct native bootstrap
	moveq #ENTRY_ORD_INIT, d0  ; exercise the public init ordinal, not a private initializer
	bsr.w tkpkgServiceDispatchV1  ; keep bootstrap behavior identical to an external init call
	rts

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

tkpkgServiceDispatchV1
	cmpi.b #ENTRY_ORD_INIT, d0
	beq.s tkpkgServiceHandleInitEntry
	bsr.w tkpkgServicePrepareRequestV1  ; assign a request id before validation/status reporting
	bsr.w tkpkgServiceValidateHeaderV1  ; reject stale or foreign control blocks early
	tst.b d1
	bne.s tkpkgServiceDispatchDone
	cmpi.b #ENTRY_ORD_LAST_ERROR, d0
	beq.s tkpkgServiceHandleLastError
	cmpi.b #ENTRY_ORD_LOAD_PACKAGE, d0
	beq.w tkpkgServiceHandleLoadPackage
	cmpi.b #ENTRY_ORD_SET_PIPELINE, d0
	beq.w tkpkgServiceHandleSetPipeline
	cmpi.b #ENTRY_ORD_TOKENIZE_LINE, d0
	beq.w tkpkgServiceHandleTokenizeLine
	cmpi.b #ENTRY_ORD_PARSE_LINE, d0
	beq.w tkpkgServiceHandleParseLine
	cmpi.b #ENTRY_ORD_ENCODE_INSTRUCTION, d0
	beq.w tkpkgServiceHandleEncodeInstruction
	bsr.w tkpkgServiceSetBadRequestV1
	rts

tkpkgServiceHandleInitEntry
	bsr.w tkpkgServicePrepareRequestV1
	bsr.w tkpkgServiceWriteHeaderV1
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bra.s tkpkgServiceHandleInit

tkpkgServiceDispatchDone
	rts

tkpkgServiceHandleInit
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	rts

tkpkgServiceHandleLastError
	tst.b CB_INPUT_LEN(a0)
	bne.s tkpkgServiceLastErrorBadRequest
	tst.b 19(a0)
	bne.s tkpkgServiceLastErrorBadRequest
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	move.b StoredLastErrorLen, CB_OUTPUT_LEN(a0)
	move.b StoredLastErrorLenHi, 23(a0)
	tst.b StoredLastErrorLen
	beq.s tkpkgServiceLastErrorDone
	bsr.w tkpkgServiceWriteOutputBufferOffsetV1

tkpkgServiceLastErrorDone
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	rts

tkpkgServiceLastErrorBadRequest
	bsr.w tkpkgServiceSetBadRequestV1
	rts

tkpkgServiceHandleLoadPackage
	move.l a0, -(sp)
	bsr.w tkpkgPackageLoaderLoadV1
	movea.l (sp)+, a0
	tst.b d0
	bne.s tkpkgServiceLoadPackageError
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	rts

tkpkgServiceLoadPackageError
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

tkpkgServiceHandleSetPipeline
	move.l a0, -(sp)
	bsr.w tkpkgPipelineSetActiveV1
	movea.l (sp)+, a0
	tst.b d0
	beq.s tkpkgServiceSetPipelineOk
	cmpi.b #STATUS_BAD_REQUEST_V1, d0
	beq.s tkpkgServiceSetPipelineBadRequest
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

tkpkgServiceSetPipelineBadRequest
	bsr.w tkpkgServiceSetBadRequestV1
	rts

tkpkgServiceSetPipelineOk
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	rts

tkpkgServiceHandleTokenizeLine
	move.l a0, -(sp)
	bsr.w tkpkgTokenizerVmTokenizeLineV1
	movea.l (sp)+, a0
	tst.b d0
	beq.s tkpkgServiceTokenizeLineOk
	cmpi.b #STATUS_BAD_REQUEST_V1, d0
	beq.s tkpkgServiceTokenizeLineBadRequest
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

tkpkgServiceTokenizeLineBadRequest
	bsr.w tkpkgServiceSetBadRequestV1
	rts

tkpkgServiceTokenizeLineOk
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	tst.w d1
	beq.s tkpkgServiceTokenizeLineDone
	bsr.w tkpkgServiceWriteOutputBufferOffsetV1
	move.b d1, CB_OUTPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 23(a0)

tkpkgServiceTokenizeLineDone
	rts

tkpkgServiceHandleParseLine
	move.l a0, -(sp)
	bsr.w tkpkgServiceParseLineV1
	movea.l (sp)+, a0
	tst.b d2
	beq.s tkpkgServiceParseLineOk
	bsr.w tkpkgServiceSetBadRequestV1
	rts

tkpkgServiceParseLineOk
	movem.l d0-d1, -(sp)
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	movem.l (sp)+, d0-d1
	rts

tkpkgServiceHandleEncodeInstruction
	move.l a0, -(sp)
	bsr.w tkpkgServiceEncodeInstructionV1
	movea.l (sp)+, a0
	tst.b d0
	beq.s tkpkgServiceEncodeInstructionOk
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	rts

tkpkgServiceEncodeInstructionOk
	bsr.w tkpkgServiceWriteClearInputFieldsV1
	bsr.w tkpkgServiceClearStoredLastErrorV1
	bsr.w tkpkgServiceWriteClearLastErrorFieldsV1
	bsr.w tkpkgServiceSetStatusOkV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	tst.w d1
	beq.s tkpkgServiceEncodeInstructionDone
	bsr.w tkpkgServiceWriteOutputBufferOffsetV1
	move.b d1, CB_OUTPUT_LEN(a0)
	lsr.w #8, d1
	move.b d1, 23(a0)

tkpkgServiceEncodeInstructionDone
	rts

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

tkpkgServiceParseLineV1
	moveq #0, d0
	move.b CB_INPUT_PTR(a0), d0  ; low byte of CB-relative route-frame offset
	moveq #0, d1
	move.b 17(a0), d1  ; high byte of CB_INPUT_PTR; direct offset avoids a temp struct
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.W), a1  ; A1 now points at the caller-supplied PRVM route frame
	moveq #0, d0
	move.b CB_INPUT_LEN(a0), d0  ; low byte of route-frame byte length
	moveq #0, d1
	move.b 19(a0), d1  ; high byte of CB_INPUT_LEN
	lsl.w #8, d1
	or.w d1, d0
	cmpi.w #TKPKG_PARSE_ROUTE_FRAME_SIZE, d0
	bne.s tkpkgParseLineBadRequest
	movea.l a1, a0  ; PRVM router ABI expects its route frame in A0
	jsr prvmRouteLine68000  ; D0/D1 become the parser service's immediate return pair
	moveq #0, d2
	rts

tkpkgParseLineBadRequest
	moveq #1, d2
	moveq #0, d0
	moveq #0, d1
	rts

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

tkpkgServiceEncodeInstructionV1
	movem.l d2-d7/a2-a6, -(sp)
	moveq #0, d0
	move.b CB_INPUT_PTR(a0), d0  ; low byte of CB-relative encode-request offset
	moveq #0, d1
	move.b 17(a0), d1  ; high byte of CB_INPUT_PTR
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.W), a4  ; A4 walks the request envelope in-place
	moveq #0, d7
	move.b CB_INPUT_LEN(a0), d7  ; D7 tracks remaining request bytes as fields are consumed
	moveq #0, d0
	move.b 19(a0), d0
	lsl.w #8, d0
	or.w d0, d7
	cmpi.w #4, d7
	bcs.w tkpkgEncodeInstructionFail
	moveq #0, d2
	move.b (a4)+, d2
	subq.w #1, d7
	tst.w d2
	beq.w tkpkgEncodeInstructionFail
	cmp.w d7, d2
	bhi.w tkpkgEncodeInstructionFail
	movea.l a4, a5
	adda.w d2, a4
	sub.w d2, d7
	tst.w d7
	beq.w tkpkgEncodeInstructionFail
	moveq #0, d3
	move.b (a4)+, d3
	subq.w #1, d7
	tst.w d3
	beq.w tkpkgEncodeInstructionNoMatch
	tst.w d7
	beq.w tkpkgEncodeInstructionFail
	moveq #0, d4
	move.b (a4)+, d4
	subq.w #1, d7
	tst.w d4
	beq.w tkpkgEncodeInstructionFail
	cmp.w d7, d4
	bhi.w tkpkgEncodeInstructionFail
	movea.l a4, a6
	adda.w d4, a4
	sub.w d4, d7
	tst.w d7
	beq.w tkpkgEncodeInstructionFail
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d7
	tst.w d5
	beq.w tkpkgEncodeInstructionFail
	tst.w d7
	beq.w tkpkgEncodeInstructionFail
	moveq #0, d6
	move.b (a4)+, d6
	subq.w #1, d7
	cmp.w d7, d6
	bhi.w tkpkgEncodeInstructionFail
	movea.l a4, a3
	bsr.w tkpkgEncodeFindAndExecuteTableProgram
	bra.s tkpkgEncodeInstructionReturn

tkpkgEncodeInstructionNoMatch
	moveq #0, d1
	moveq #0, d0
	bra.s tkpkgEncodeInstructionReturn

tkpkgEncodeInstructionFail
	lea RuntimeErrorText, a1
	moveq #RUNTIME_ERROR_TEXT_LEN, d1
	moveq #1, d0

tkpkgEncodeInstructionReturn
	movem.l (sp)+, d2-d7/a2-a6
	rts

tkpkgEncodeFindAndExecuteTableProgram
	movem.l d2-d7/a0-a6, -(sp)
	moveq #0, d0
	move.b TablChunkOffsetLo, d0
	moveq #0, d1
	move.b TablChunkOffsetHi, d1
	lsl.w #8, d1
	or.w d1, d0
	beq.w tkpkgEncodeFindTableFail
	lea PackageStorage, a0
	adda.w d0, a0
	bsr.w tkpkgEncodeReadU32Low16
	tst.w d0
	beq.w tkpkgEncodeFindTableNoMatch
	move.w d0, d7
	subq.w #1, d7

tkpkgEncodeFindTableLoop
	move.b (a0)+, d0
	bsr.w tkpkgEncodeSkipString
	movea.l a0, a1
	bsr.w tkpkgEncodeReadU32Low16
	move.w d0, d1
	movea.l a0, a2
	adda.w d1, a0
	movea.l a5, a1
	move.w d2, d0
	bsr.w tkpkgEncodeStringEqIgnoreCase
	tst.b d0
	beq.s tkpkgEncodeFindTableSkipModeCheck
	movea.l a0, a1
	bsr.w tkpkgEncodeReadU32Low16
	move.w d0, d1
	movea.l a0, a2
	adda.w d1, a0
	movea.l a6, a1
	move.w d4, d0
	bsr.w tkpkgEncodeStringEqIgnoreCase
	tst.b d0
	beq.s tkpkgEncodeFindTableSkipProgram
	bsr.w tkpkgEncodeReadU32Low16
	move.w d0, d1
	movea.l a0, a1
	bsr.w tkpkgEncodeExecuteProgram
	bra.s tkpkgEncodeFindTableReturn

tkpkgEncodeFindTableSkipModeCheck
	bsr.w tkpkgEncodeSkipString

tkpkgEncodeFindTableSkipProgram
	bsr.w tkpkgEncodeSkipBytes
	dbra d7, tkpkgEncodeFindTableLoop

tkpkgEncodeFindTableNoMatch
	moveq #0, d1
	moveq #0, d0
	bra.s tkpkgEncodeFindTableReturn

tkpkgEncodeFindTableFail
	lea RuntimeErrorText, a1
	moveq #RUNTIME_ERROR_TEXT_LEN, d1
	moveq #1, d0

tkpkgEncodeFindTableReturn
	movem.l (sp)+, d2-d7/a0-a6
	rts

tkpkgEncodeExecuteProgram
	movem.l d2-d7/a0-a4, -(sp)
	movea.l a1, a0
	move.w d1, d7
	lea LastErrorBuffer, a2
	clr.w d1

tkpkgEncodeExecuteProgramLoop
	tst.w d7
	beq.s tkpkgEncodeExecuteProgramFail
	move.b (a0)+, d0
	subq.w #1, d7
	cmpi.b #$FF, d0
	beq.s tkpkgEncodeExecuteProgramOk
	cmpi.b #$01, d0
	beq.s tkpkgEncodeExecuteProgramEmitU8
	cmpi.b #$02, d0
	beq.s tkpkgEncodeExecuteProgramEmitOperand
	bra.w tkpkgEncodeExecuteProgramFail

tkpkgEncodeExecuteProgramEmitU8
	tst.w d7
	beq.s tkpkgEncodeExecuteProgramFail
	move.b (a0)+, (a2)+
	subq.w #1, d7
	addq.w #1, d1
	bra.s tkpkgEncodeExecuteProgramLoop

tkpkgEncodeExecuteProgramEmitOperand
	tst.w d7
	beq.s tkpkgEncodeExecuteProgramFail
	move.b (a0)+, d0
	subq.w #1, d7
	tst.b d0
	bne.s tkpkgEncodeExecuteProgramFail
	move.w d6, d0
	beq.s tkpkgEncodeExecuteProgramLoop
	movea.l a3, a4

tkpkgEncodeExecuteProgramOperandLoop
	move.b (a4)+, (a2)+
	addq.w #1, d1
	subq.w #1, d0
	bne.s tkpkgEncodeExecuteProgramOperandLoop
	bra.s tkpkgEncodeExecuteProgramLoop

tkpkgEncodeExecuteProgramOk
	moveq #0, d0
	bra.s tkpkgEncodeExecuteProgramReturn

tkpkgEncodeExecuteProgramFail
	lea RuntimeErrorText, a1
	moveq #RUNTIME_ERROR_TEXT_LEN, d1
	moveq #1, d0

tkpkgEncodeExecuteProgramReturn
	movem.l (sp)+, d2-d7/a0-a4
	rts

tkpkgEncodeReadU32Low16
	moveq #0, d0
	move.b (a0)+, d0
	moveq #0, d1
	move.b (a0)+, d1
	lsl.w #8, d1
	or.w d1, d0
	addq.l #2, a0
	rts

tkpkgEncodeSkipString
	bsr.w tkpkgEncodeReadU32Low16
	adda.w d0, a0
	rts

tkpkgEncodeSkipBytes
	bsr.w tkpkgEncodeReadU32Low16
	adda.w d0, a0
	rts

tkpkgEncodeStringEqIgnoreCase
	movem.l d1-d4/a1-a2, -(sp)
	cmp.w d1, d0
	bne.s tkpkgEncodeStringEqNo
	tst.w d0
	beq.s tkpkgEncodeStringEqYes
	move.w d0, d4
	subq.w #1, d4

tkpkgEncodeStringEqLoop
	move.b (a1)+, d2
	move.b (a2)+, d3
	cmpi.b #'A', d2
	bcs.s tkpkgEncodeStringEqLeftOk
	cmpi.b #'Z', d2
	bhi.s tkpkgEncodeStringEqLeftOk
	addi.b #32, d2

tkpkgEncodeStringEqLeftOk
	cmpi.b #'A', d3
	bcs.s tkpkgEncodeStringEqCompare
	cmpi.b #'Z', d3
	bhi.s tkpkgEncodeStringEqCompare
	addi.b #32, d3

tkpkgEncodeStringEqCompare
	cmp.b d3, d2
	bne.s tkpkgEncodeStringEqNo
	dbra d4, tkpkgEncodeStringEqLoop

tkpkgEncodeStringEqYes
	moveq #1, d0
	bra.s tkpkgEncodeStringEqReturn

tkpkgEncodeStringEqNo
	moveq #0, d0

tkpkgEncodeStringEqReturn
	movem.l (sp)+, d1-d4/a1-a2
	rts

tkpkgServicePrepareRequestV1
	bsr.w tkpkgServiceIncrementRequestIdV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	bsr.w tkpkgServiceWriteClearExtensionFieldsV1
	rts

tkpkgServiceValidateHeaderV1
	moveq #0, d1
	cmpi.b #$4f, (a0)
	bne.s tkpkgServiceBadControlBlock
	cmpi.b #$54, 1(a0)
	bne.s tkpkgServiceBadControlBlock
	cmpi.b #$36, 2(a0)
	bne.s tkpkgServiceBadControlBlock
	cmpi.b #$35, 3(a0)
	bne.s tkpkgServiceBadControlBlock
	cmpi.b #$01, CB_ABI_VERSION(a0)
	bne.s tkpkgServiceBadControlBlock
	tst.b 5(a0)
	bne.s tkpkgServiceBadControlBlock
	cmpi.b #NATIVE_CONTROL_BLOCK_SIZE_V1, CB_STRUCT_SIZE(a0)
	bne.s tkpkgServiceBadControlBlock
	tst.b 7(a0)
	bne.s tkpkgServiceBadControlBlock
	rts

tkpkgServiceBadControlBlock
	bsr.w tkpkgServiceSetBadControlBlockV1
	moveq #1, d1
	rts

tkpkgServiceWriteHeaderV1
	move.b #$4f, (a0)
	move.b #$54, 1(a0)
	move.b #$36, 2(a0)
	move.b #$35, 3(a0)
	move.b #$01, CB_ABI_VERSION(a0)
	clr.b 5(a0)
	move.b #NATIVE_CONTROL_BLOCK_SIZE_V1, CB_STRUCT_SIZE(a0)
	clr.b 7(a0)
	move.b #CAPABILITY_FLAGS_V1, CB_CAPABILITY_FLAGS(a0)
	clr.b 9(a0)
	clr.b CB_RESERVED0(a0)
	clr.b 15(a0)
	bsr.w tkpkgServiceSetStatusOkV1
	rts

tkpkgServiceIncrementRequestIdV1
	move.b NextRequestIdLo, d1
	addq.b #1, d1
	move.b d1, NextRequestIdLo
	bne.s tkpkgServiceRequestIdDone
	move.b NextRequestIdHi, d2
	addq.b #1, d2
	move.b d2, NextRequestIdHi

tkpkgServiceRequestIdDone
	move.b NextRequestIdLo, CB_REQUEST_ID(a0)
	move.b NextRequestIdHi, 13(a0)
	rts

tkpkgServiceSetBadRequestV1
	bsr.w tkpkgServiceSetStatusBadRequestV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	lea BadRequestText, a1
	moveq #BAD_REQUEST_TEXT_LEN, d1
	bsr.w tkpkgServiceCopyLastErrorMessageV1
	bsr.w tkpkgServiceWriteLastErrorBufferOffsetV1
	move.b #BAD_REQUEST_TEXT_LEN, CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	move.b #BAD_REQUEST_TEXT_LEN, StoredLastErrorLen
	clr.b StoredLastErrorLenHi
	move.b #LAST_ERROR_KIND_BAD_REQUEST, StoredLastErrorKind
	rts

tkpkgServiceSetBadControlBlockV1
	bsr.w tkpkgServiceSetStatusBadControlBlockV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	lea ControlBlockErrorText, a1
	moveq #CONTROL_BLOCK_ERROR_TEXT_LEN, d1
	bsr.w tkpkgServiceCopyLastErrorMessageV1
	bsr.w tkpkgServiceWriteLastErrorBufferOffsetV1
	move.b #CONTROL_BLOCK_ERROR_TEXT_LEN, CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	move.b #CONTROL_BLOCK_ERROR_TEXT_LEN, StoredLastErrorLen
	clr.b StoredLastErrorLenHi
	move.b #LAST_ERROR_KIND_BAD_CONTROL, StoredLastErrorKind
	rts

tkpkgServiceSetRuntimeErrorV1
	bsr.w tkpkgServiceSetStatusRuntimeErrorV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	lea RuntimeErrorText, a1
	moveq #RUNTIME_ERROR_TEXT_LEN, d1
	bsr.w tkpkgServiceSetRuntimeErrorMessageV1
	move.b #LAST_ERROR_KIND_RUNTIME, StoredLastErrorKind
	move.b #RUNTIME_ERROR_TEXT_LEN, StoredLastErrorLen
	clr.b StoredLastErrorLenHi
	rts

tkpkgServiceSetRuntimeErrorMessageV1
	bsr.w tkpkgServiceSetStatusRuntimeErrorV1
	bsr.w tkpkgServiceWriteClearOutputFieldsV1
	bsr.w tkpkgServiceCopyLastErrorMessageV1
	bsr.w tkpkgServiceWriteLastErrorBufferOffsetV1
	move.b d1, CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	move.b d1, StoredLastErrorLen
	clr.b StoredLastErrorLenHi
	move.b #LAST_ERROR_KIND_RUNTIME, StoredLastErrorKind
	rts

tkpkgServiceClearStoredLastErrorV1
	clr.b StoredLastErrorLen
	clr.b StoredLastErrorLenHi
	move.b #LAST_ERROR_KIND_NONE, StoredLastErrorKind
	rts

tkpkgServiceWriteClearOutputFieldsV1
	clr.b CB_OUTPUT_PTR(a0)
	clr.b 21(a0)
	clr.b CB_OUTPUT_LEN(a0)
	clr.b 23(a0)
	rts

tkpkgServiceWriteClearExtensionFieldsV1
	clr.b CB_EXTENSION_PTR(a0)
	clr.b 25(a0)
	clr.b CB_EXTENSION_LEN(a0)
	clr.b 27(a0)
	rts

tkpkgServiceWriteClearInputFieldsV1
	clr.b CB_INPUT_PTR(a0)
	clr.b 17(a0)
	clr.b CB_INPUT_LEN(a0)
	clr.b 19(a0)
	rts

tkpkgServiceWriteClearLastErrorFieldsV1
	clr.b CB_LAST_ERROR_PTR(a0)
	clr.b 29(a0)
	clr.b CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	rts

tkpkgServiceWriteLastErrorBufferOffsetV1
	move.b #LAST_ERROR_BUFFER_PTR_V1, CB_LAST_ERROR_PTR(a0)
	clr.b 29(a0)
	rts

tkpkgServiceWriteOutputBufferOffsetV1
	move.b #LAST_ERROR_BUFFER_PTR_V1, CB_OUTPUT_PTR(a0)
	clr.b 21(a0)
	rts

tkpkgServiceCopyLastErrorMessageV1
	lea LastErrorBuffer, a2
	move.w d1, d2
	tst.w d2
	beq.s tkpkgServiceCopyLastErrorDone

tkpkgServiceCopyLastErrorLoop
	move.b (a1)+, (a2)+
	subq.w #1, d2
	bne.s tkpkgServiceCopyLastErrorLoop

tkpkgServiceCopyLastErrorDone
	clr.b (a2)
	rts

tkpkgServiceSetStatusOkV1
	clr.b CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts

tkpkgServiceSetStatusBadControlBlockV1
	move.b #STATUS_BAD_CONTROL_BLOCK_V1, CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts

tkpkgServiceSetStatusBadRequestV1
	move.b #STATUS_BAD_REQUEST_V1, CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts

tkpkgServiceSetStatusRuntimeErrorV1
	move.b #STATUS_RUNTIME_ERROR_V1, CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts

	.endsection
	.endmodule
