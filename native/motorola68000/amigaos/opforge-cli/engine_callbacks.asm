; Native AmigaOS opForge CLI opasm assembly-session handoff.

	.module opforge.cli.engine_callbacks
	.cpu 68020

	.use tkpkg.amigaos.buffers (ControlBlockV1, lastErrorBuffer, LAST_ERROR_BUFFER_CAPACITY)

	.use opasm.amigaos.callback_abi (OPASM_ASSEMBLE_REQ_BIN_REQUESTED_PTR)
	.use opasm.amigaos.callback_abi (OPASM_ASSEMBLE_REQ_EVENT_BUFFER_PTR, OPASM_ASSEMBLE_REQ_EVENT_CAPACITY)
	.use opasm.amigaos.callback_abi (OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR, OPASM_ASSEMBLE_REQ_SERVICE_FRAME_PTR)
	.use opasm.amigaos.callback_abi (OPASM_ASSEMBLE_REQ_DIAG_BUFFER_PTR, OPASM_ASSEMBLE_REQ_DIAG_BUFFER_CAPACITY)
	.use opasm.amigaos.callback_abi (OPASM_ASSEMBLE_REQ_FLAGS, OPASM_ASSEMBLE_REQ_BYTES)
	.use opasm.amigaos.callback_abi (OPASM_EVENT_BYTES)
	.use opasm.amigaos.callback_abi (OPASM_SERVICE_CONTROL_BLOCK_PTR, OPASM_SERVICE_IO_BUFFER_PTR)
	.use opasm.amigaos.callback_abi (OPASM_SERVICE_IO_BUFFER_CAPACITY, OPASM_SERVICE_EVAL_EXTENSION_PTR)
	.use opasm.amigaos.callback_abi (OPASM_SERVICE_EVAL_EXTENSION_BYTES, OPASM_SERVICE_BYTES)
	.use opasm.amigaos.assembly_driver (opasmNativeAssembleSessionV1)

	.use opforge.cli.constants (NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, NATIVE_EVAL_EXPR_EXTENSION_BYTES)
	.use opforge.cli.state (NativeCliBinRequested)
	.use opforge.cli.opasm_event_report (opforgeNativeCliRenderOpasmEventsV1)

NATIVE_CLI_OPASM_EVENT_CAPACITY = 192

	.section code, kind=code
	.pub

opforgeNativeCliRunTwoPassEngine	.block
	movem.l d1-d7/a0-a2, -(sp)
	suba.l #OPASM_ASSEMBLE_REQ_BYTES + OPASM_SERVICE_BYTES, sp
	movea.l sp, a0
	move.l #NativeCliBinRequested, OPASM_ASSEMBLE_REQ_BIN_REQUESTED_PTR(a0)
	move.l #NativeCliOpasmEventBuffer, OPASM_ASSEMBLE_REQ_EVENT_BUFFER_PTR(a0)
	move.w #NATIVE_CLI_OPASM_EVENT_CAPACITY, OPASM_ASSEMBLE_REQ_EVENT_CAPACITY(a0)
	move.l #NativeCliOpasmEventCount, OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a0)
	lea OPASM_ASSEMBLE_REQ_BYTES(a0), a2
	move.l a2, OPASM_ASSEMBLE_REQ_SERVICE_FRAME_PTR(a0)
	clr.l OPASM_ASSEMBLE_REQ_DIAG_BUFFER_PTR(a0)
	clr.w OPASM_ASSEMBLE_REQ_DIAG_BUFFER_CAPACITY(a0)
	clr.w OPASM_ASSEMBLE_REQ_FLAGS(a0)
	movea.l a2, a1
	move.l #ControlBlockV1, OPASM_SERVICE_CONTROL_BLOCK_PTR(a1)
	move.l #lastErrorBuffer, OPASM_SERVICE_IO_BUFFER_PTR(a1)
	move.w #LAST_ERROR_BUFFER_CAPACITY, OPASM_SERVICE_IO_BUFFER_CAPACITY(a1)
	lea ControlBlockV1, a2
	adda.w #NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, a2
	move.l a2, OPASM_SERVICE_EVAL_EXTENSION_PTR(a1)
	move.w #NATIVE_EVAL_EXPR_EXTENSION_BYTES, OPASM_SERVICE_EVAL_EXTENSION_BYTES(a1)
	clr.w NativeCliOpasmEventCount
	jsr opasmNativeAssembleSessionV1
	move.l d0, d7
	lea NativeCliOpasmEventBuffer, a0
	move.w NativeCliOpasmEventCount, d0
	jsr opforgeNativeCliRenderOpasmEventsV1
	move.l d7, d0
	adda.l #OPASM_ASSEMBLE_REQ_BYTES + OPASM_SERVICE_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a2
	rts
	.bend  ; opforgeNativeCliRunTwoPassEngine

	.endsection

	.section bss, kind=bss

NativeCliOpasmEventCount
	.res word, 1

NativeCliOpasmEventBuffer
	.res byte, NATIVE_CLI_OPASM_EVENT_CAPACITY * OPASM_EVENT_BYTES

	.endsection
	.endmodule
