; ABI-facing control-block bootstrap, validation, and request bookkeeping.

	.module tkpkg.amigaos.service_request
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.service_status as status

	.section code, kind=code

; Initialize the per-request control-block state.
; Inputs: A0 = native control block.
; Outputs: request id advances and output fields are clear.
; Clobbers: D1-D2/CCR.
; CCR: unspecified on return.
prepareRequestV1	.block
	bsr.w incrementRequestIdV1
	bsr.w status.writeClearOutputFieldsV1
	rts
	.bend  ; prepareRequestV1

; Validate the native service control block header.
; Inputs: A0 = candidate control block.
; Outputs: D1 = 0 when valid, 1 when rejected; rejected blocks have bad-control status.
; Clobbers: D1/CCR.
; CCR: reflects D1 on return.
validateHeaderV1	.block
	moveq #0, d1
	cmpi.b #$4f, (a0)
	bne.s badControlBlock
	cmpi.b #$54, 1(a0)
	bne.s badControlBlock
	cmpi.b #$36, 2(a0)
	bne.s badControlBlock
	cmpi.b #$35, 3(a0)
	bne.s badControlBlock
	cmpi.b #$01, abi.CB_ABI_VERSION(a0)
	bne.s badControlBlock
	tst.b 5(a0)
	bne.s badControlBlock
	cmpi.b #abi.NATIVE_CONTROL_BLOCK_SIZE_V1, abi.CB_STRUCT_SIZE(a0)
	bne.s badControlBlock
	tst.b 7(a0)
	bne.s badControlBlock
	moveq #0, d1
	rts

badControlBlock
	bsr.w status.setBadControlBlockV1
	moveq #1, d1
	rts
	.bend  ; validateHeaderV1

; Initialize the ABI header and capabilities for an init request.
; Inputs: A0 = native control block.
; Outputs: ABI header fields and success status are initialized.
; Clobbers: CCR.
; CCR: unspecified on return.
writeHeaderV1	.block
	move.b #$4f, (a0)
	move.b #$54, 1(a0)
	move.b #$36, 2(a0)
	move.b #$35, 3(a0)
	move.b #$01, abi.CB_ABI_VERSION(a0)
	clr.b 5(a0)
	move.b #abi.NATIVE_CONTROL_BLOCK_SIZE_V1, abi.CB_STRUCT_SIZE(a0)
	clr.b 7(a0)
	move.b #abi.CAPABILITY_FLAGS_V1, abi.CB_CAPABILITY_FLAGS(a0)
	clr.b 9(a0)
	clr.b abi.CB_RESERVED0(a0)
	clr.b 15(a0)
	bsr.w status.setStatusOkV1
	rts
	.bend  ; writeHeaderV1

; Advance and project the monotonic native request identifier.
; Inputs: A0 = native control block.
; Outputs: control block receives the next request id.
; Clobbers: D1-D2/CCR.
; CCR: reflects the final high-byte write.
incrementRequestIdV1	.block
	move.b buffers.NextRequestIdLo, d1
	addq.b #1, d1
	move.b d1, buffers.NextRequestIdLo
	bne.s done
	move.b buffers.NextRequestIdHi, d2
	addq.b #1, d2
	move.b d2, buffers.NextRequestIdHi

done
	move.b buffers.NextRequestIdLo, abi.CB_REQUEST_ID(a0)
	move.b buffers.NextRequestIdHi, 13(a0)
	rts
	.bend  ; incrementRequestIdV1

; Clear ABI extension fields after an init request.
; Inputs: A0 = native control block.
; Outputs: extension pointer and length are zero.
; Clobbers: CCR.
; CCR: reflects the final clear.
writeClearExtensionFieldsV1	.block
	clr.b abi.CB_EXTENSION_PTR(a0)
	clr.b 25(a0)
	clr.b abi.CB_EXTENSION_LEN(a0)
	clr.b 27(a0)
	rts
	.bend  ; writeClearExtensionFieldsV1

; Clear ABI input fields after request consumption.
; Inputs: A0 = native control block.
; Outputs: input pointer and length are zero.
; Clobbers: CCR.
; CCR: reflects the final clear.
writeClearInputFieldsV1	.block
	clr.b abi.CB_INPUT_PTR(a0)
	clr.b 17(a0)
	clr.b abi.CB_INPUT_LEN(a0)
	clr.b 19(a0)
	rts
	.bend  ; writeClearInputFieldsV1

	.endsection
	.endmodule
