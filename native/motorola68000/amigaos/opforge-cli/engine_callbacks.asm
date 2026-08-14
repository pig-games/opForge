; Native AmigaOS opForge CLI opasm assembly-session handoff.

	.module opforge.cli.engine_callbacks
	.cpu 68020

	.use tkpkg.amigaos.buffers

	.use opasm.amigaos.callback_abi as abi
	.use opasm.amigaos.assembly_driver as driver
	.use opasm.amigaos.layout as layout

	.use opforge.cli.constants
	.use opforge.cli.state
	.use opforge.cli.module_use
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
	move.l #seedRetainedSectionMapsV1, abi.OPASM_ASSEMBLE_REQ_LAYOUT_INIT_CB(a0)
	movea.l a2, a1
	move.l #buffers.ControlBlockV1, abi.OPASM_SERVICE_CONTROL_BLOCK_PTR(a1)
	move.l #buffers.lastErrorBuffer, abi.OPASM_SERVICE_IO_BUFFER_PTR(a1)
	move.w #buffers.LAST_ERROR_BUFFER_CAPACITY, abi.OPASM_SERVICE_IO_BUFFER_CAPACITY(a1)
	lea buffers.ControlBlockV1, a2
	adda.w #constants.NATIVE_EVAL_EXPR_EXTENSION_PTR_V1, a2
	move.l a2, abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a1)
	move.w #constants.NATIVE_EVAL_EXPR_EXTENSION_BYTES, abi.OPASM_SERVICE_EVAL_EXTENSION_BYTES(a1)
	move.l #module_use.opforgeNativeCliResolveImportedOrdinaryNameV1, abi.OPASM_SERVICE_IMPORT_NAME_RESOLVER_PTR(a1)
	movea.l sp, a0
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

; Seed the generic layout engine from the CLI preprocessor's authoritative,
; structurally parsed import-map records.
; Outputs: D0.L = 0 on success, 1 on invalid retained metadata.
seedRetainedSectionMapsV1	.block
	movem.l d1-d7/a0-a5, -(sp)
	moveq #0, d7
mapLoop
	cmp.w state.NativeCliImportSectionMapCount, d7
	bhs.w success
	moveq #0, d4
	move.w d7, d4
	add.w d4, d4
	lea state.NativeCliImportSectionMapImportTable, a0
	move.w 0(a0, d4.w), d4
	cmp.w state.NativeCliImportCount, d4
	bhs.w fail
	move.w d4, d1
	add.w d1, d1
	lea state.NativeCliImportModuleTable, a0
	moveq #0, d0
	move.w 0(a0, d1.w), d0
	bsr.w moduleNameForIdV1
	beq.w fail
	movea.l a0, a4
	move.l d0, d3
	lea state.NativeCliImportOwnerModuleTable, a0
	moveq #0, d0
	move.w 0(a0, d1.w), d0
	bsr.w moduleNameForIdV1
	beq.w fail
	movea.l a0, a5
	move.l d0, d2
	movea.l a4, a0
	move.l d3, d0
	movea.l a5, a1
	move.l d2, d1
	jsr layout.setScratchMapOwnersV1
	bne.w fail
	move.l d7, d6
	lsl.l #6, d6
	lea state.NativeCliImportSectionMapLogicalTable, a0
	adda.l d6, a0
	movea.l a0, a2
	bsr.w mapNameLengthV1
	move.l d0, d5
	lea state.NativeCliImportSectionMapConcreteTable, a0
	adda.l d6, a0
	movea.l a0, a3
	bsr.w mapNameLengthV1
	move.l d0, d1
	movea.l a2, a0
	movea.l a3, a1
	move.l d5, d0
	jsr layout.recordSectionMapV1
	bne.w fail
	addq.w #1, d7
	bra.w mapLoop
success
	moveq #0, d0
	bra.w return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d7/a0-a5
	rts
	.bend  ; seedRetainedSectionMapsV1

; Return one retained module name after validating its structural table id.
; Inputs: D0.W = module id. Outputs: A0/D0 = name and bounded length.
; CCR: D0 is zero for invalid/empty names.
moduleNameForIdV1	.block
	cmp.w state.NativeCliModuleCount, d0
	bhs.s invalid
	lsl.l #6, d0
	lea state.NativeCliModuleNameTable, a0
	adda.l d0, a0
	bsr.s mapNameLengthV1
	tst.l d0
	rts
invalid
	moveq #0, d0
	rts
	.bend  ; moduleNameForIdV1

; Return the bounded length of one retained fixed-capacity map name.
; Inputs: A0 = name. Outputs: D0.L = length, or zero for empty/unterminated.
mapNameLengthV1	.block
	movea.l a0, a1
	moveq #0, d0
lengthLoop
	cmpi.w #constants.TOKEN_BUFFER_CAPACITY, d0
	bhs.s invalid
	tst.b (a1)+
	beq.s done
	addq.w #1, d0
	bra.s lengthLoop
invalid
	moveq #0, d0
done
	rts
	.bend  ; mapNameLengthV1

	.endsection

	.section bss, kind=bss

NativeCliOpasmEventCount
	.res word, 1

NativeCliOpasmEventBuffer
	.res byte, NATIVE_CLI_OPASM_EVENT_CAPACITY * abi.OPASM_EVENT_BYTES

	.endsection
	.endmodule
