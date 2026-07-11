; Native opasm conditional and match directive routing.

	.module opasm.amigaos.flow_conditionals
	.cpu 68020

	.section code, kind=code
	.pub

; Classify one conditional or match directive for the driver's existing flow paths.
; Inputs: A0/D0 = mnemonic text.
; Outputs: D0 = 0; D3.W = 0 unhandled, 1 match, 2 case/default, 3 endmatch, 4 if, 5 elseif, 6 else, 7 endif.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
routeDirectiveV1	.block
	movea.l a0, a2
	move.l d0, d6

	movea.l a2, a0
	move.l d6, d0
	lea ConditionalMatchMnemonicText, a1
	moveq #5, d1
	bsr.w conditionalLineStartsWith
	bne.w conditionalMatchedMatch
	movea.l a2, a0
	move.l d6, d0
	lea ConditionalCaseMnemonicText, a1
	moveq #4, d1
	bsr.w conditionalLineStartsWith
	bne.w conditionalMatchedCase
	movea.l a2, a0
	move.l d6, d0
	lea ConditionalDefaultMnemonicText, a1
	moveq #7, d1
	bsr.w conditionalLineStartsWith
	bne.w conditionalMatchedCase
	movea.l a2, a0
	move.l d6, d0
	lea ConditionalEndmatchMnemonicText, a1
	moveq #8, d1
	bsr.w conditionalLineStartsWith
	bne.w conditionalMatchedEndmatch
	movea.l a2, a0
	move.l d6, d0
	lea ConditionalIfMnemonicText, a1
	moveq #2, d1
	bsr.w conditionalLineStartsWith
	bne.w conditionalMatchedIf
	movea.l a2, a0
	move.l d6, d0
	lea ConditionalElseifMnemonicText, a1
	moveq #6, d1
	bsr.w conditionalLineStartsWith
	bne.w conditionalMatchedElseif
	movea.l a2, a0
	move.l d6, d0
	lea ConditionalElseMnemonicText, a1
	moveq #4, d1
	bsr.w conditionalLineStartsWith
	bne.w conditionalMatchedElse
	movea.l a2, a0
	move.l d6, d0
	lea ConditionalEndifMnemonicText, a1
	moveq #5, d1
	bsr.w conditionalLineStartsWith
	bne.w conditionalMatchedEndif
	clr.w d3
	moveq #0, d0
	rts

conditionalMatchedMatch
	moveq #1, d3
	bra.w conditionalHandled
conditionalMatchedCase
	moveq #2, d3
	bra.w conditionalHandled
conditionalMatchedEndmatch
	moveq #3, d3
	bra.w conditionalHandled
conditionalMatchedIf
	moveq #4, d3
	bra.w conditionalHandled
conditionalMatchedElseif
	moveq #5, d3
	bra.w conditionalHandled
conditionalMatchedElse
	moveq #6, d3
	bra.w conditionalHandled
conditionalMatchedEndif
	moveq #7, d3
conditionalHandled
	moveq #0, d0
	rts
	.bend  ; routeDirectiveV1

; Match a directive mnemonic with optional leading dot and a token boundary.
; Inputs: A0/D0 = candidate text; A1/D1 = lowercase mnemonic text/length.
; Outputs: D0 = 1 on match, 0 otherwise.
; Clobbers: D0-D4/A0-A3/CCR.
; CCR: reflects D0 on return.
conditionalLineStartsWith	.block
	movem.l d2-d4/a0-a3, -(sp)
	tst.l d0
	beq.w conditionalNo
	cmpi.b #'.', (a0)
	bne.w conditionalCompareStart
	addq.l #1, a0
	subq.l #1, d0
conditionalCompareStart
	cmp.l d1, d0
	bcs.w conditionalNo
	movea.l a0, a2
	movea.l a1, a3
	move.l d1, d2
	beq.w conditionalBoundary
	subq.l #1, d2
conditionalLoop
	move.b (a2)+, d3
	move.b (a3)+, d4
	cmpi.b #'A', d3
	bcs.w conditionalCompare
	cmpi.b #'Z', d3
	bhi.w conditionalCompare
	addi.b #32, d3
conditionalCompare
	cmp.b d4, d3
	bne.w conditionalNo
	dbra d2, conditionalLoop
conditionalBoundary
	cmp.l d1, d0
	beq.w conditionalYes
	move.b 0(a0, d1.l), d3
	cmpi.b #' ', d3
	beq.w conditionalYes
	cmpi.b #9, d3
	beq.w conditionalYes
	cmpi.b #';', d3
	beq.w conditionalYes
conditionalNo
	movem.l (sp)+, d2-d4/a0-a3
	moveq #0, d0
	rts
conditionalYes
	movem.l (sp)+, d2-d4/a0-a3
	moveq #1, d0
	rts
	.bend  ; conditionalLineStartsWith

	.endsection

	.section data, kind=data
ConditionalMatchMnemonicText
	.byte "match", 0
ConditionalCaseMnemonicText
	.byte "case", 0
ConditionalDefaultMnemonicText
	.byte "default", 0
ConditionalEndmatchMnemonicText
	.byte "endmatch", 0
ConditionalIfMnemonicText
	.byte "if", 0
ConditionalElseifMnemonicText
	.byte "elseif", 0
ConditionalElseMnemonicText
	.byte "else", 0
ConditionalEndifMnemonicText
	.byte "endif", 0

	.endsection
	.endmodule
