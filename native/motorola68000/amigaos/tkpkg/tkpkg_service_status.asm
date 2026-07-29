; ABI-facing status, output-window, and last-error projection for tkpkg.

	.module tkpkg.amigaos.service_status
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers

	.section code, kind=code

; Set a bad-request result and retain its caller-visible last-error payload.
; Inputs: A0 = native control block.
; Outputs: status/output/last-error fields describe OTR002.
; Clobbers: D1-D2/A1-A2/CCR.
; CCR: unspecified on return.
setBadRequestV1	.block
	bsr.w setStatusBadRequestV1
	bsr.w writeClearOutputFieldsV1
	lea buffers.BadRequestText, a1
	moveq #buffers.BAD_REQUEST_TEXT_LEN, d1
	bsr.w copyLastErrorMessageV1
	bsr.w writeLastErrorBufferOffsetV1
	move.b #buffers.BAD_REQUEST_TEXT_LEN, abi.CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	move.b #buffers.BAD_REQUEST_TEXT_LEN, buffers.StoredLastErrorLen
	clr.b buffers.StoredLastErrorLenHi
	move.b #buffers.LAST_ERROR_KIND_BAD_REQUEST, buffers.StoredLastErrorKind
	rts
	.bend  ; setBadRequestV1

; Set a bad-control result and retain its caller-visible last-error payload.
; Inputs: A0 = native control block.
; Outputs: status/output/last-error fields describe OTR003.
; Clobbers: D1-D2/A1-A2/CCR.
; CCR: unspecified on return.
setBadControlBlockV1	.block
	bsr.w setStatusBadControlBlockV1
	bsr.w writeClearOutputFieldsV1
	lea buffers.ControlBlockErrorText, a1
	moveq #buffers.CONTROL_BLOCK_ERROR_TEXT_LEN, d1
	bsr.w copyLastErrorMessageV1
	bsr.w writeLastErrorBufferOffsetV1
	move.b #buffers.CONTROL_BLOCK_ERROR_TEXT_LEN, abi.CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	move.b #buffers.CONTROL_BLOCK_ERROR_TEXT_LEN, buffers.StoredLastErrorLen
	clr.b buffers.StoredLastErrorLenHi
	move.b #buffers.LAST_ERROR_KIND_BAD_CONTROL, buffers.StoredLastErrorKind
	rts
	.bend  ; setBadControlBlockV1

; Set the default runtime-error result and retain its payload.
; Inputs: A0 = native control block.
; Outputs: status/output/last-error fields describe OTR901.
; Clobbers: D1-D2/A1-A2/CCR.
; CCR: unspecified on return.
setRuntimeErrorV1	.block
	bsr.w setStatusRuntimeErrorV1
	bsr.w writeClearOutputFieldsV1
	lea buffers.RuntimeErrorText, a1
	moveq #buffers.RUNTIME_ERROR_TEXT_LEN, d1
	bsr.w setRuntimeErrorMessageV1
	move.b #buffers.LAST_ERROR_KIND_RUNTIME, buffers.StoredLastErrorKind
	move.b #buffers.RUNTIME_ERROR_TEXT_LEN, buffers.StoredLastErrorLen
	clr.b buffers.StoredLastErrorLenHi
	rts
	.bend  ; setRuntimeErrorV1

; Project a supplied runtime-error message through the ABI and stored error.
; Inputs: A0 = native control block; A1/D1 = message pointer/length.
; Outputs: runtime status and stored last-error payload.
; Clobbers: D1-D2/A1-A2/CCR.
; CCR: unspecified on return.
setRuntimeErrorMessageV1	.block
	bsr.w setStatusRuntimeErrorV1
	bsr.w writeClearOutputFieldsV1
	bsr.w copyLastErrorMessageV1
	bsr.w writeLastErrorBufferOffsetV1
	move.b d1, abi.CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	move.b d1, buffers.StoredLastErrorLen
	clr.b buffers.StoredLastErrorLenHi
	move.b #buffers.LAST_ERROR_KIND_RUNTIME, buffers.StoredLastErrorKind
	rts
	.bend  ; setRuntimeErrorMessageV1

; Clear the retained last-error identity between successful requests.
; Inputs: none.
; Outputs: stored last-error kind and lengths are clear.
; Clobbers: CCR.
; CCR: reflects the final stored-kind write.
clearStoredLastErrorV1	.block
	clr.b buffers.StoredLastErrorLen
	clr.b buffers.StoredLastErrorLenHi
	move.b #buffers.LAST_ERROR_KIND_NONE, buffers.StoredLastErrorKind
	rts
	.bend  ; clearStoredLastErrorV1

; Clear ABI output fields.
; Inputs: A0 = native control block.
; Outputs: output pointer and length are zero.
; Clobbers: CCR.
; CCR: reflects the final clear.
writeClearOutputFieldsV1	.block
	clr.b abi.CB_OUTPUT_PTR(a0)
	clr.b 21(a0)
	clr.b abi.CB_OUTPUT_LEN(a0)
	clr.b 23(a0)
	rts
	.bend  ; writeClearOutputFieldsV1

; Clear ABI last-error fields.
; Inputs: A0 = native control block.
; Outputs: last-error pointer and length are zero.
; Clobbers: CCR.
; CCR: reflects the final clear.
writeClearLastErrorFieldsV1	.block
	clr.b abi.CB_LAST_ERROR_PTR(a0)
	clr.b 29(a0)
	clr.b abi.CB_LAST_ERROR_LEN(a0)
	clr.b 31(a0)
	rts
	.bend  ; writeClearLastErrorFieldsV1

; Point ABI last-error fields at the shared stored-error buffer.
; Inputs: A0 = native control block.
; Outputs: last-error pointer offset targets LastErrorBuffer.
; Clobbers: CCR.
; CCR: reflects the final clear.
writeLastErrorBufferOffsetV1	.block
	move.b #buffers.LAST_ERROR_BUFFER_PTR_V1, abi.CB_LAST_ERROR_PTR(a0)
	clr.b 29(a0)
	rts
	.bend  ; writeLastErrorBufferOffsetV1

; Point ABI output fields at the shared output buffer.
; Inputs: A0 = native control block.
; Outputs: output pointer offset targets LastErrorBuffer.
; Clobbers: CCR.
; CCR: reflects the final clear.
writeOutputBufferOffsetV1	.block
	move.b #buffers.LAST_ERROR_BUFFER_PTR_V1, abi.CB_OUTPUT_PTR(a0)
	clr.b 21(a0)
	rts
	.bend  ; writeOutputBufferOffsetV1

; Copy a caller-owned diagnostic into the shared last-error buffer.
; Inputs: A1 = source bytes; D1 = byte count.
; Outputs: LastErrorBuffer contains the bytes followed by NUL.
; Clobbers: D2/A1-A2/CCR.
; CCR: reflects the final clear.
copyLastErrorMessageV1	.block
	lea buffers.LastErrorBuffer, a2
	move.w d1, d2
	beq.s done

loop
	move.b (a1)+, (a2)+
	subq.w #1, d2
	bne.s loop

done
	clr.b (a2)
	rts
	.bend  ; copyLastErrorMessageV1

; Set an ABI success status.
; Inputs: A0 = native control block.
; Outputs: status code is STATUS_OK_V1.
; Clobbers: CCR.
; CCR: reflects the final clear.
setStatusOkV1	.block
	clr.b abi.CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts
	.bend  ; setStatusOkV1

; Set an ABI bad-control status.
; Inputs: A0 = native control block.
; Outputs: status code is STATUS_BAD_CONTROL_BLOCK_V1.
; Clobbers: CCR.
; CCR: reflects the final clear.
setStatusBadControlBlockV1	.block
	move.b #abi.STATUS_BAD_CONTROL_BLOCK_V1, abi.CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts
	.bend  ; setStatusBadControlBlockV1

; Set an ABI bad-request status.
; Inputs: A0 = native control block.
; Outputs: status code is STATUS_BAD_REQUEST_V1.
; Clobbers: CCR.
; CCR: reflects the final clear.
setStatusBadRequestV1	.block
	move.b #abi.STATUS_BAD_REQUEST_V1, abi.CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts
	.bend  ; setStatusBadRequestV1

; Set an ABI runtime-error status.
; Inputs: A0 = native control block.
; Outputs: status code is STATUS_RUNTIME_ERROR_V1.
; Clobbers: CCR.
; CCR: reflects the final clear.
setStatusRuntimeErrorV1	.block
	move.b #abi.STATUS_RUNTIME_ERROR_V1, abi.CB_STATUS_CODE(a0)
	clr.b 11(a0)
	rts
	.bend  ; setStatusRuntimeErrorV1

	.endsection
	.endmodule
