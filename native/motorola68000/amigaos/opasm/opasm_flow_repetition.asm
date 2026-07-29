; Native opasm repetition directive routing.

	.module opasm.amigaos.flow_repetition
	.cpu 68020

	.section code, kind=code
	.pub

; Classify `.for`, `.endfor`, `.while`, and `.endwhile` directives.
; Inputs: A0/D0 = mnemonic text.
; Outputs: D0 = 0; D3.W = 0 unhandled, 1 for, 2 endfor, 3 while,
;          4 endwhile, or 5 bfor.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
routeDirectiveV1	.block
	movea.l a0, a2
	move.l d0, d6
	movea.l a2, a0
	move.l d6, d0
	lea RepetitionBforMnemonicText, a1
	moveq #4, d1
	bsr.w repetitionLineStartsWith
	bne.w repetitionBfor
	movea.l a2, a0
	move.l d6, d0
	lea RepetitionForMnemonicText, a1
	moveq #3, d1
	bsr.w repetitionLineStartsWith
	bne.w repetitionFor
	movea.l a2, a0
	move.l d6, d0
	lea RepetitionEndforMnemonicText, a1
	moveq #6, d1
	bsr.w repetitionLineStartsWith
	bne.w repetitionEndfor
	movea.l a2, a0
	move.l d6, d0
	lea RepetitionWhileMnemonicText, a1
	moveq #5, d1
	bsr.w repetitionLineStartsWith
	bne.w repetitionWhile
	movea.l a2, a0
	move.l d6, d0
	lea RepetitionEndwhileMnemonicText, a1
	moveq #8, d1
	bsr.w repetitionLineStartsWith
	bne.w repetitionEndwhile
	clr.w d3
	moveq #0, d0
	rts
repetitionFor
	moveq #1, d3
	bra.w repetitionDone
repetitionEndfor
	moveq #2, d3
	bra.w repetitionDone
repetitionWhile
	moveq #3, d3
	bra.w repetitionDone
repetitionEndwhile
	moveq #4, d3
	bra.s repetitionDone
repetitionBfor
	moveq #5, d3
repetitionDone
	moveq #0, d0
	rts
	.bend  ; routeDirectiveV1

repetitionLineStartsWith	.block
	movem.l d2-d4/a0-a3, -(sp)
	tst.l d0
	beq.w repetitionNo
	cmpi.b #'.', (a0)
	bne.w repetitionCompareStart
	addq.l #1, a0
	subq.l #1, d0
repetitionCompareStart
	cmp.l d1, d0
	bcs.w repetitionNo
	movea.l a0, a2
	movea.l a1, a3
	move.l d1, d2
	beq.w repetitionBoundary
	subq.l #1, d2
repetitionLoop
	move.b (a2)+, d3
	move.b (a3)+, d4
	cmpi.b #'A', d3
	bcs.w repetitionCompare
	cmpi.b #'Z', d3
	bhi.w repetitionCompare
	addi.b #32, d3
repetitionCompare
	cmp.b d4, d3
	bne.w repetitionNo
	dbra d2, repetitionLoop
repetitionBoundary
	cmp.l d1, d0
	beq.w repetitionYes
	move.b 0(a0, d1.l), d3
	cmpi.b #' ', d3
	beq.w repetitionYes
	cmpi.b #9, d3
	beq.w repetitionYes
	cmpi.b #';', d3
	beq.w repetitionYes
repetitionNo
	movem.l (sp)+, d2-d4/a0-a3
	moveq #0, d0
	rts
repetitionYes
	movem.l (sp)+, d2-d4/a0-a3
	moveq #1, d0
	rts
	.bend  ; repetitionLineStartsWith

	.endsection
	.section data, kind=data
RepetitionForMnemonicText
	.byte "for", 0
RepetitionBforMnemonicText
	.byte "bfor", 0
RepetitionEndforMnemonicText
	.byte "endfor", 0
RepetitionWhileMnemonicText
	.byte "while", 0
RepetitionEndwhileMnemonicText
	.byte "endwhile", 0
	.endsection
	.endmodule
