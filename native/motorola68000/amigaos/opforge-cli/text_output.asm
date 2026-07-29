; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.text_output
	.cpu 68020

	.use opforge.cli.state
	.use opforge.cli.strings
	.use opforge.cli.dos

	.section code, kind=code
	.pub

; Print unsigned 16-bit D0 as decimal through the CLI stdout path.
opforgeNativeCliPutU16Decimal	.block
	movem.l d1-d6/a0-a1, -(sp)
	andi.l #$0000FFFF, d0
	lea DecimalPowers, a0
	moveq #4, d6
	clr.w d5

powerLoop
	moveq #0, d3
	move.w (a0)+, d2

digitLoop
	cmp.w d2, d0
	bcs.s maybeEmit
	sub.w d2, d0
	addq.w #1, d3
	bra.s digitLoop

maybeEmit
	tst.w d3
	bne.s emit
	tst.w d5
	bne.s emit
	cmpi.w #1, d2
	bne.s next

emit
	move.w #1, d5
	addi.b #'0', d3
	lea state.NativeCliDecimalChar, a1
	move.b d3, (a1)
	clr.b 1(a1)
	move.l #state.NativeCliDecimalChar, d1
	jsr dos.putStr

next
	dbra d6, powerLoop
	movem.l (sp)+, d1-d6/a0-a1
	rts
	.bend  ; opforgeNativeCliPutU16Decimal

; Print unsigned 16-bit D0 as decimal through the CLI ErrorOutput path.
; Inputs: D0.w = unsigned value.
; Outputs: decimal text is written to ErrorOutput; D0 is consumed.
; Clobbers: D0/CCR; D1-D6/A0-A1 are preserved.
; CCR: unspecified on return.
opforgeNativeCliPutErrU16Decimal	.block
	movem.l d1-d6/a0-a1, -(sp)
	andi.l #$0000FFFF, d0
	lea DecimalPowers, a0
	moveq #4, d6
	clr.w d5

powerLoop
	moveq #0, d3
	move.w (a0)+, d2

digitLoop
	cmp.w d2, d0
	bcs.s maybeEmit
	sub.w d2, d0
	addq.w #1, d3
	bra.s digitLoop

maybeEmit
	tst.w d3
	bne.s emit
	tst.w d5
	bne.s emit
	cmpi.w #1, d2
	bne.s next

emit
	move.w #1, d5
	addi.b #'0', d3
	lea state.NativeCliDecimalChar, a1
	move.b d3, (a1)
	clr.b 1(a1)
	move.l #state.NativeCliDecimalChar, d1
	jsr dos.putErrStr

next
	dbra d6, powerLoop
	movem.l (sp)+, d1-d6/a0-a1
	rts
	.bend  ; opforgeNativeCliPutErrU16Decimal

; Print unsigned 32-bit D0 as `$XXXXXXXX`.
opforgeNativeCliPutHexU32	.block
	movem.l d0-d4/a0-a2, -(sp)
	move.l d0, -(sp)
	lea state.NativeCliHexBuffer, a1
	move.b #'$', (a1)+
	lea strings.HexDigitsText, a0
	movea.l sp, a2
	moveq #3, d4

loop
	moveq #0, d1
	move.b (a2)+, d1
	move.l d1, d2
	lsr.b #4, d2
	move.b 0(a0, d2.l), (a1)+
	andi.b #$0F, d1
	move.b 0(a0, d1.l), (a1)+
	dbra d4, loop
	clr.b (a1)
	addq.l #4, sp
	move.l #state.NativeCliHexBuffer, d1
	jsr dos.putStr
	movem.l (sp)+, d0-d4/a0-a2
	rts
	.bend  ; opforgeNativeCliPutHexU32

opforgeNativeCliPutSpace	.block
	move.l #strings.SpaceText, d1
	jsr dos.putStr
	rts
	.bend  ; opforgeNativeCliPutSpace

; Write one space through the CLI ErrorOutput path.
; Inputs: none.
; Outputs: one space is written to ErrorOutput.
; Clobbers: D0-D2/A0-A1/A6/CCR.
; CCR: unspecified on return.
opforgeNativeCliPutErrSpace	.block
	move.l #strings.SpaceText, d1
	jsr dos.putErrStr
	rts
	.bend  ; opforgeNativeCliPutErrSpace

	.endsection

	.section data, kind=data

	.align 2

DecimalPowers
	.word 10000, 1000, 100, 10, 1

	.endsection

	.endmodule
