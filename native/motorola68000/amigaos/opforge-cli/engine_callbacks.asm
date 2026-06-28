; Native AmigaOS opForge CLI opasm assembly-session handoff.

	.module opforge.cli.engine_callbacks
	.cpu 68020

	.use tkpkg.amigaos.buffers

	.use opasm.amigaos.callback_abi as abi
	.use opasm.amigaos.assembly_driver as driver

	.use opforge.cli.constants
	.use opforge.cli.state
	.use opforge.cli.opasm_event_report

NATIVE_CLI_OPASM_EVENT_CAPACITY = 192

	.section code, kind=code
	.pub

opforgeNativeCliRunTwoPassEngine	.block
	movem.l d1-d7/a0-a2, -(sp)
	suba.l #abi.OPASM_ASSEMBLE_REQ_BYTES + abi.OPASM_SERVICE_BYTES, sp
	movea.l sp, a0
	move.l #state.NativeCliBinRequested, abi.OPASM_ASSEMBLE_REQ_BIN_REQUESTED_PTR(a0)
	move.l #NativeCliOpasmEventBuffer, abi.OPASM_ASSEMBLE_REQ_EVENT_BUFFER_PTR(a0)
	move.w #NATIVE_CLI_OPASM_EVENT_CAPACITY, abi.OPASM_ASSEMBLE_REQ_EVENT_CAPACITY(a0)
	move.l #NativeCliOpasmEventCount, abi.OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a0)
	lea abi.OPASM_ASSEMBLE_REQ_BYTES(a0), a2
	move.l a2, abi.OPASM_ASSEMBLE_REQ_SERVICE_FRAME_PTR(a0)
	clr.l abi.OPASM_ASSEMBLE_REQ_DIAG_BUFFER_PTR(a0)
	clr.w abi.OPASM_ASSEMBLE_REQ_DIAG_BUFFER_CAPACITY(a0)
	clr.w abi.OPASM_ASSEMBLE_REQ_FLAGS(a0)
	movea.l a2, a1
	move.l #buffers.ControlBlockV1, abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a1)
	move.l #buffers.lastErrorBuffer, abi.OPASM_SERVICE_IO_BUFFER_PTR(a1)
	move.w #buffers.LAST_ERROR_BUFFER_CAPACITY, abi.OPASM_SERVICE_IO_BUFFER_CAPACITY(a1)
	lea buffers.ControlBlockV1, a2
	adda.w #constants.NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, a2
	move.l a2, abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a1)
	move.w #constants.NATIVE_EVAL_EXPR_EXTENSION_BYTES, abi.OPASM_SERVICE_EVAL_EXTENSION_BYTES(a1)
	clr.w NativeCliOpasmEventCount
	jsr driver.assembleSessionV1
	move.l d0, d7
	bne.s renderEvents
	tst.w state.NativeCliDebugEnabled
	beq.s returnStatus

renderEvents
	lea NativeCliOpasmEventBuffer, a0
	move.w NativeCliOpasmEventCount, d0
	jsr opasm_event_report.opforgeNativeCliRenderOpasmEventsV1

returnStatus
	move.l d7, d0
	adda.l #abi.OPASM_ASSEMBLE_REQ_BYTES + abi.OPASM_SERVICE_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a2
	rts
	.bend  ; opforgeNativeCliRunTwoPassEngine

	.endsection

	.section bss, kind=bss

NativeCliOpasmEventCount
	.res word, 1

NativeCliOpasmEventBuffer
	.res byte, NATIVE_CLI_OPASM_EVENT_CAPACITY * abi.OPASM_EVENT_BYTES

	.endsection
	.endmodule
