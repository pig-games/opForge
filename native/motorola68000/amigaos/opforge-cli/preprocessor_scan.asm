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
	bcs.s no
	cmpi.b #'.', (a0)
	bne.s no
	move.b 1(a0), d1
	ori.b #32, d1
	cmpi.b #'e', d1
	bne.s no
	move.b 2(a0), d1
	ori.b #32, d1
	cmpi.b #'n', d1
	bne.s no
	move.b 3(a0), d1
	ori.b #32, d1
	cmpi.b #'d', d1
	bne.s no
	move.b 4(a0), d1
	ori.b #32, d1
	cmpi.b #'m', d1
	bne.s no
	move.b 5(a0), d1
	ori.b #32, d1
	cmpi.b #'a', d1
	bne.s no
	move.b 6(a0), d1
	ori.b #32, d1
	cmpi.b #'c', d1
	bne.s no
	move.b 7(a0), d1
	ori.b #32, d1
	cmpi.b #'r', d1
	bne.s no
	move.b 8(a0), d1
	ori.b #32, d1
	cmpi.b #'o', d1
	bne.s no
	moveq #1, d0
	rts
no
	moveq #0, d0
	rts
	.bend  ; lineStartsWithEndmacroDirective

; Require the first non-whitespace token of a macro header to be a name.
; Inputs: A0 = line bytes; D0 = line length.
; Outputs: D0 = 1 when a non-directive name token begins the header, else 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
macroHeaderHasName	.block
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s no
	cmpi.b #'.', (a0)
	beq.s no
	cmpi.b #';', (a0)
	beq.s no
	moveq #1, d0
	rts

no
	moveq #0, d0
	rts
	.bend  ; macroHeaderHasName

; Match a standalone `.macro` directive without relying on a data-section
; pointer. Inputs are the source line in A0/D0; D0 is 1 on match, else 0.
lineContainsMacroDirective	.block
	movem.l d1-d4/a0, -(sp)
	cmpi.l #6, d0
	bcs.w no
	move.l d0, d3
	subi.l #6, d3
	clr.l d2
scan
	cmp.l d3, d2
	bhi.w no
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
	movem.l d5/a3, -(sp)
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

scan
	cmp.l d5, d4
	bhi.s no
	move.b 0(a2, d4.l), d0
	cmpi.b #';', d0
	beq.s no
	tst.l d4
	beq.s compare
	move.b -1(a2, d4.l), d0
	cmpi.b #' ', d0
	beq.s compare
	cmpi.b #9, d0
	bne.s next

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
	bne.s next
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
	bra.s scan

yes
	movem.l (sp)+, d5/a3
	moveq #1, d0
	rts

no
	movem.l (sp)+, d5/a3
	moveq #0, d0
	rts
	.bend  ; lineContainsDirective

	.endsection
	.endmodule
