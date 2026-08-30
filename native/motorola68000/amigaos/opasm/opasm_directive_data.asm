; Numeric directive sizing and emission owner.

	.module opasm.amigaos.directive_data
	.cpu 68020

	.use opasm.amigaos.engine as eng
.ifdef OPFORGE_DEBUG_CONTRACTS
	.use opforge.debug.contracts as debug_contracts
	.use opforge.debug.events as debug_events
	.include "debug_macros.i"
.endif

	.section code, kind=code
	.pub

; Return the emitted size of a numeric directive.
; Inputs: D5.W = unit bytes; A0 = callback returning D0 status and D3 part count.
; Outputs: D0.L = status; D3.L = total byte size.
; Clobbers: D0/D3/CCR.
; CCR: reflects D0 on return.
sizeNumericDirectiveV1	.block
	movem.l d1-d2/a0, -(sp)
	movea.l a0, a1
	jsr (a1)
	bne.s fail
	moveq #0, d2
	move.w d5, d2
	mulu.l d2, d3
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d2/a0
	rts
	.bend  ; sizeNumericDirectiveV1

; Emit numeric directive values in first-run MOS little-endian order.
; Inputs: D7.L = statement; D5.W = unit bytes; A0 = count callback;
;         A1 = resolver callback. The resolver returns D3.L.
; Outputs: D0.L = 0 on success, 1 on malformed data or image overflow.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0 on return.
emitNumericDirectiveV1	.block
	movem.l d1-d7/a0-a3, -(sp)
	move.l a0, DataCountCallback
	move.l a1, DataResolveCallback
	move.w d5, d4
	move.w d5, DataUnitBytes
	movea.l DataCountCallback, a0
	jsr (a0)
	bne.w fail
	move.w d3, d2
	moveq #1, d6
loop
	cmp.w d2, d6
	bhi.w ok
	move.w d4, d5
	movea.l DataResolveCallback, a0
	jsr (a0)
	bne.w fail

.ifdef OPFORGE_DEBUG_CONTRACTS
	; Instrumentation point: numeric directive operand resolved before packing.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D7/A0-A3 by the balanced save/restore below.
	; SR/CCR preserved: no branch consumes flags across this diagnostic block.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: the result status was already
	; checked, and the next operation repacks the resolved value independently.
	; Removal/stabilization plan: retain as the stabilized numeric-data boundary
	; event; remove only if a dedicated directive-data event supersedes it.
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a3, -(sp)
	moveq #0, d1
	moveq #1, d2
	moveq #0, d4
	move.w DataUnitBytes, d4
	move.l d7, d5
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_DIRECTIVE_DATA
	movem.l (sp)+, d0-d7/a0-a3
	move.w (sp)+, ccr
.endif

	cmpi.w #1, d4
	bne.s pack
	cmpi.l #$000000ff, d3
	bhi.w fail
pack
	lea DataScratch, a0
	move.b d3, (a0)
	cmpi.w #1, d4
	beq.s append
	move.l d3, d0
	lsr.l #8, d0
	move.b d0, 1(a0)
	cmpi.w #2, d4
	beq.s append
	cmpi.w #4, d4
	bne.w fail
	move.l d3, d0
	lsr.l #8, d0
	lsr.l #8, d0
	move.b d0, 2(a0)
	move.l d3, d0
	lsr.l #8, d0
	lsr.l #8, d0
	lsr.l #8, d0
	move.b d0, 3(a0)
append
	move.w d4, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.w fail

.ifdef OPFORGE_DEBUG_CONTRACTS
	; Instrumentation point: numeric directive bytes appended to the session image.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D7/A0-A3 by the balanced save/restore below.
	; SR/CCR preserved: no branch consumes flags across this diagnostic block.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: image append already succeeded and
	; the following loop increment establishes fresh flags.
	; Removal/stabilization plan: retain as the stabilized image-emission event.
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a3, -(sp)
	moveq #0, d1
	moveq #2, d2
	moveq #0, d4
	move.w DataUnitBytes, d4
	move.l d7, d5
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_DIRECTIVE_DATA
	movem.l (sp)+, d0-d7/a0-a3
	move.w (sp)+, ccr
.endif

	addq.w #1, d6
	bra.w loop
ok
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; emitNumericDirectiveV1

	.endsection

	.section bss, kind=bss
DataCountCallback
	.res long, 1
DataResolveCallback
	.res long, 1
DataUnitBytes
	.res word, 1
DataScratch
	.res byte, 4

	.endsection
	.endmodule
