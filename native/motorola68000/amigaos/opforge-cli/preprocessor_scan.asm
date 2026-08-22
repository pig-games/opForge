; Bounded macro directive scanning for the native CLI preprocessor.

	.module opforge.cli.preprocessor_scan
	.cpu 68020

	.use opforge.cli.line_text

	.section code, kind=code
	.pub

lineStartsWithDirective	.block
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	jsr line_text.opforgeNativeCliLineStartsWith
	rts
	.bend  ; lineStartsWithDirective

; Match `.endmacro` as the first non-whitespace directive.
lineStartsWithEndmacroDirective	.block
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	cmpi.l #9, d0
	bcs.w no
	cmpi.b #'.', (a0)
	bne.w no
	move.b 1(a0), d1
	ori.b #32, d1
	cmpi.b #'e', d1
	bne.w no
	move.b 2(a0), d1
	ori.b #32, d1
	cmpi.b #'n', d1
	bne.w no
	move.b 3(a0), d1
	ori.b #32, d1
	cmpi.b #'d', d1
	bne.w no
	move.b 4(a0), d1
	ori.b #32, d1
	cmpi.b #'m', d1
	bne.w no
	move.b 5(a0), d1
	ori.b #32, d1
	cmpi.b #'a', d1
	bne.w no
	move.b 6(a0), d1
	ori.b #32, d1
	cmpi.b #'c', d1
	bne.w no
	move.b 7(a0), d1
	ori.b #32, d1
	cmpi.b #'r', d1
	bne.w no
	move.b 8(a0), d1
	ori.b #32, d1
	cmpi.b #'o', d1
	bne.s no
	cmpi.l #9, d0
	beq.s yes
	move.b 9(a0), d1
	cmpi.b #' ', d1
	beq.s yes
	cmpi.b #9, d1
	beq.s yes
	cmpi.b #';', d1
	bne.s no
yes
	moveq #1, d0
	rts
no
	moveq #0, d0
	rts
	.bend  ; lineStartsWithEndmacroDirective

; Accept either a name-first header or the exact expected directive-first
; header.  A stray dot-prefixed token must not be captured merely because a
; macro/segment directive occurs later in the line.
; Inputs: A0/D0 = line slice; A1/D1 = expected directive bytes/length.
; Outputs: D0 = 1 when a name or the expected directive begins the header.
; Clobbers: D0-D4/A0/A2-A3/CCR.
; CCR: reflects D0 on return.
macroHeaderHasName	.block
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s no
	cmpi.b #'.', (a0)
	beq.s directive
	cmpi.b #';', (a0)
	beq.s no
yes
	moveq #1, d0
	rts

directive
	jsr line_text.opforgeNativeCliLineStartsWith
	rts

no
	moveq #0, d0
	rts
	.bend  ; macroHeaderHasName

; Require a directive-first `.statement` header with a keyword token.
; Inputs: A0 = line bytes; D0 = line length.
; Outputs: D0 = 1 when a keyword follows `.statement`, else 0.
; Clobbers: D0-D4/A0-A3/CCR.
statementHeaderHasKeyword	.block
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	move.l a0, -(sp)
	move.l d0, -(sp)
	lea StatementText.l, a1
	moveq #10, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s noPop
	move.l (sp)+, d0
	movea.l (sp)+, a0
	adda.l #10, a0
	subi.l #10, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s no
	move.b (a0), d1
	cmpi.b #';', d1
	beq.s no
	cmpi.b #'_', d1
	beq.s yes
	cmpi.b #'A', d1
	bcs.s no
	cmpi.b #'Z', d1
	bls.s yes
	cmpi.b #'a', d1
	bcs.s no
	cmpi.b #'z', d1
	bhi.s no
yes
	moveq #1, d0
	rts
noPop
	addq.l #8, sp
no
	moveq #0, d0
	rts
	.bend  ; statementHeaderHasKeyword

; Match a standalone `.macro` directive without relying on a data-section
; pointer. Inputs are the source line in A0/D0; D0 is 1 on match, else 0.
lineContainsMacroDirective	.block
	movem.l d1-d4/a0, -(sp)
	cmpi.l #6, d0
	bcs.w no
	move.l d0, d3
	subi.l #6, d3
	clr.l d2
	clr.l d4
scan
	cmp.l d3, d2
	bhi.w no
	move.b 0(a0, d2.l), d1
	tst.b d4
	beq.s outsideQuote
	cmp.b d4, d1
	bne.w next
	clr.l d4
	bra.w next
outsideQuote
	cmpi.b #';', d1
	beq.w no
	cmpi.b #'\'', d1
	beq.s enterQuote
	cmpi.b #'"', d1
	bne.s candidateStart
enterQuote
	move.b d1, d4
	bra.w next
candidateStart
	tst.l d2
	beq.s candidate
	move.b -1(a0, d2.l), d1
	cmpi.b #' ', d1
	beq.s candidate
	cmpi.b #9, d1
	bne.w next
candidate
	cmpi.b #'.', 0(a0, d2.l)
	bne.w next
	move.b 1(a0, d2.l), d1
	ori.b #32, d1
	cmpi.b #'m', d1
	bne.w next
	move.b 2(a0, d2.l), d1
	ori.b #32, d1
	cmpi.b #'a', d1
	bne.w next
	move.b 3(a0, d2.l), d1
	ori.b #32, d1
	cmpi.b #'c', d1
	bne.w next
	move.b 4(a0, d2.l), d1
	ori.b #32, d1
	cmpi.b #'r', d1
	bne.w next
	move.b 5(a0, d2.l), d1
	ori.b #32, d1
	cmpi.b #'o', d1
	bne.w next
	cmp.l d3, d2
	beq.s yes
	move.b 6(a0, d2.l), d1
	cmpi.b #' ', d1
	beq.s yes
	cmpi.b #9, d1
	beq.s yes
	cmpi.b #';', d1
	beq.s yes
next
	addq.l #1, d2
	bra.w scan
yes
	movem.l (sp)+, d1-d4/a0
	moveq #1, d0
	rts
no
	movem.l (sp)+, d1-d4/a0
	moveq #0, d0
	rts
	.bend  ; lineContainsMacroDirective

lineContainsDirective	.block
	movem.l d5-d6/a3, -(sp)
	movea.l a0, a2
	movea.l a1, a3
	move.l d1, d3
	cmp.l d3, d0
	bcc.s containsLongEnough
	bra.w no
containsLongEnough
	sub.l d3, d0
	move.l d0, d5
	clr.l d4
	clr.l d6

scan
	cmp.l d5, d4
	bhi.w no
	move.b 0(a2, d4.l), d0
	tst.l d6
	beq.s outsideQuote
	cmp.b d6, d0
	bne.w next
	clr.l d6
	bra.w next
outsideQuote
	cmpi.b #';', d0
	beq.w no
	cmpi.b #'\'', d0
	beq.s enterQuote
	cmpi.b #'"', d0
	bne.s candidate
enterQuote
	move.b d0, d6
	bra.w next
candidate
	tst.l d4
	beq.s compare
	move.b -1(a2, d4.l), d0
	cmpi.b #' ', d0
	beq.s compare
	cmpi.b #9, d0
	bne.w next

compare
	moveq #0, d0

compareLoop
	cmp.l d3, d0
	beq.s boundary
	move.b 0(a2, d4.l), d1
	add.l d0, d4
	move.b 0(a2, d4.l), d1
	sub.l d0, d4
	move.b 0(a3, d0.l), d2
	cmpi.b #'A', d1
	bcs.s compareByte
	cmpi.b #'Z', d1
	bhi.s compareByte
	addi.b #32, d1

compareByte
	cmp.b d2, d1
	bne.w next
	addq.l #1, d0
	bra.s compareLoop

boundary
	cmp.l d5, d4
	beq.s yes
	add.l d3, d4
	move.b 0(a2, d4.l), d0
	sub.l d3, d4
	cmpi.b #' ', d0
	beq.s yes
	cmpi.b #9, d0
	beq.s yes
	cmpi.b #';', d0
	beq.s yes

next
	addq.l #1, d4
	bra.w scan

yes
	movem.l (sp)+, d5-d6/a3
	moveq #1, d0
	rts

no
	movem.l (sp)+, d5-d6/a3
	moveq #0, d0
	rts
	.bend  ; lineContainsDirective

	.endsection
	.section data, kind=data
StatementText
	.byte ".statement", 0
	.endsection
	.endmodule
