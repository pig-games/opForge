; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.text_output
	.cpu 68020

	.use opforge.cli.state (NativeCliDecimalChar, NativeCliHexBuffer)
	.use opforge.cli.strings (HexDigitsText, SpaceText)
	.use opforge.cli.dos (opforgeNativeCliPutStr)

	.section code, kind=code
	.pub

; Print unsigned 16-bit D0 as decimal through the CLI stdout path.
opforgeNativeCliPutDecU16
	movem.l d1-d6/a0-a1, -(sp)
	andi.l #$0000FFFF, d0
	lea DecimalPowers, a0
	moveq #4, d6
	clr.w d5

opforgeNativeCliPutDecPowerLoop
	moveq #0, d3
	move.w (a0)+, d2

opforgeNativeCliPutDecDigitLoop
	cmp.w d2, d0
	bcs.s opforgeNativeCliPutDecMaybeEmit
	sub.w d2, d0
	addq.w #1, d3
	bra.s opforgeNativeCliPutDecDigitLoop

opforgeNativeCliPutDecMaybeEmit
	tst.w d3
	bne.s opforgeNativeCliPutDecEmit
	tst.w d5
	bne.s opforgeNativeCliPutDecEmit
	cmpi.w #1, d2
	bne.s opforgeNativeCliPutDecNext

opforgeNativeCliPutDecEmit
	move.w #1, d5
	addi.b #'0', d3
	lea NativeCliDecimalChar, a1
	move.b d3, (a1)
	clr.b 1(a1)
	move.l #NativeCliDecimalChar, d1
	jsr opforgeNativeCliPutStr

opforgeNativeCliPutDecNext
	dbra d6, opforgeNativeCliPutDecPowerLoop
	movem.l (sp)+, d1-d6/a0-a1
	rts

; Print unsigned 32-bit D0 as `$XXXXXXXX`.
opforgeNativeCliPutHexU32
	movem.l d0-d4/a0-a2, -(sp)
	move.l d0, -(sp)
	lea NativeCliHexBuffer, a1
	move.b #'$', (a1)+
	lea HexDigitsText, a0
	movea.l sp, a2
	moveq #3, d4

opforgeNativeCliPutHexLoop
	moveq #0, d1
	move.b (a2)+, d1
	move.l d1, d2
	lsr.b #4, d2
	move.b 0(a0, d2.l), (a1)+
	andi.b #$0F, d1
	move.b 0(a0, d1.l), (a1)+
	dbra d4, opforgeNativeCliPutHexLoop
	clr.b (a1)
	addq.l #4, sp
	move.l #NativeCliHexBuffer, d1
	jsr opforgeNativeCliPutStr
	movem.l (sp)+, d0-d4/a0-a2
	rts

opforgeNativeCliPutSpace
	move.l #SpaceText, d1
	jsr opforgeNativeCliPutStr
	rts

	.endsection

	.section data, kind=data

	.align 2

DecimalPowers
	.word 10000, 1000, 100, 10, 1

	.endsection

	.endmodule
