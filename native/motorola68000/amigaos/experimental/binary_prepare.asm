; Compile scalar expressions while keeping package operand syntax outside them.
; @opforge-owner: experimental.amigaos.binary_prepare

	.module experimental.amigaos.binary_prepare
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use opasm.amigaos.binary_expression as expression
	.pub
	.section code, kind=code

; A0=valid writer record, A1=distinct 256-byte output, A2=validated package.
; Returns D0/CCR status, D1=record length (zero on failure). Other registers
; preserved. Uses 256 bytes temporary stack storage, released on all paths.
; Label/statement names and register/member wrappers remain numeric tokens.
line	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #256, sp
	movea.l sp, a6
	movea.l a1, a5
	movea.l a1, a3
	lea 256(a1), a4
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	cmpi.w #4, d0
	blo.w bad
	movea.l a0, a1
	adda.w d0, a1
	btst #4, 1(a0)
	bne.w layoutControl
	moveq #4, d6
	bsr.w copy
	bne.w bad
	; Only the statement prefix may define a label.
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #5, d0
	blo.w statement
	cmpi.b #1, (a0)
	bhi.w statement
	cmpi.b #34, 4(a0)
	beq.w constant
	cmpi.b #5, 4(a0)
	bne.w statement
	moveq #5, d6
	bsr.w copy
	bne.w bad
statement
	cmpa.l a1, a0
	beq.w complete
	cmpi.b #7, (a0)
	beq.w directive
	bsr.w name
	bne.w bad
	bra.w operands
layoutControl
	move.l a1, d6
	sub.l a0, d6
	bsr.w copy
	bne.w bad
	bra.w complete
constant
	; Assignment is a shared statement, independent of the package operand grammar.
	moveq #5, d6
	bsr.w copy
	bne.w bad
	bsr.w compile
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	bra.w complete
directive
	moveq #1, d6
	bsr.w copy
	bne.w bad
	bsr.w name
	bne.w bad
	move.w d7, d4
	cmp.w package.Header.CpuDirective(a2), d7
	beq.w copyRest
	cmp.w package.Header.EndDirective(a2), d7
	beq.w copyRest
	cmp.w package.Header.OrgDirective(a2), d7
	beq.w scalar
	cmp.w package.Header.ByteDirective(a2), d7
	beq.w dataScalar
	cmp.w package.Header.WordDirective(a2), d7
	beq.w dataScalar
	cmp.w package.Header.LongDirective(a2), d7
	bne.w bad
dataScalar
	cmpa.l a1, a0
	bhs.w bad
	cmpi.b #3, (a0)
	bne.w scalar
	bsr.w string
	bne.w bad
	bra.w scalarTail
scalar
	; Shared directive arguments are always expressions, never register names.
	bsr.w compile
	bne.w bad
scalarTail
	cmpa.l a1, a0
	beq.w complete
	cmpi.b #4, (a0)
	bne.w bad
	moveq #1, d6
	bsr.w copy
	bne.w bad
	cmp.w package.Header.OrgDirective(a2), d4
	beq.w scalar
	bra.w dataScalar
operands
	cmpa.l a1, a0
	beq.w complete
	cmpi.b #8, (a0)
	bne.w operand
	moveq #1, d6
	bsr.w copy
	bne.w bad
	; Immediate operands are scalar even when a reserved name follows.
	bra.w expressionOperand
operand
	cmpi.b #14, (a0)
	beq.w parenthesizedRegister
	cmpi.b #19, (a0)  ; preserve unary/indirect token structure
	beq.w prefixedName
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w expressionOperand
	cmpi.b #1, (a0)
	bhi.w expressionOperand
	cmpi.l #5, d0
	blo.w bareName
	cmpi.b #19, 4(a0)
	beq.w nameSequence
	cmpi.b #22, 4(a0)
	beq.w nameSequence
bareName
	moveq #0, d7
	move.b 1(a0), d7
	lsl.w #8, d7
	move.b 2(a0), d7
	cmp.w package.Header.NameCount(a2), d7
	bhs.w expressionOperand
	; Keep package-defined register IDs intact; the package validates the class.
	bsr.w name
	bne.w bad
	bra.w operandDone
parenthesizedRegister
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #6, d0
	blo.w expressionOperand
	cmpi.b #1, 1(a0)
	bhi.w expressionOperand
	cmpi.b #15, 5(a0)
	bne.w expressionOperand
	move.l a0, -(sp)
	lea 1(a0), a0
	bsr.w packageRegister
	movea.l (sp)+, a0
	cmpi.l #2, d0
	beq.w bad
	tst.l d0
	bne.w expressionOperand
	moveq #6, d6
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #7, d0
	blo.w parenthesizedReady
	cmpi.b #18, 6(a0)  ; retain the package-matched postfix wrapper
	bne.w parenthesizedReady
	moveq #7, d6
parenthesizedReady
	bsr.w copy
	bne.w bad
	bra.w operandDone
nameSequence
	bsr.w validateNameSequence
	cmpi.l #2, d0
	beq.w bad
	tst.l d0
	bne.w expressionOperand
	bsr.w copy
	bne.w bad
	bra.w operandDone
prefixedName
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #7, d0
	blo.w expressionOperand
	cmpi.b #14, 1(a0)
	bne.w expressionOperand
	cmpi.b #1, 2(a0)
	bhi.w expressionOperand
	cmpi.b #15, 6(a0)
	bne.w expressionOperand
	move.l a0, -(sp)
	lea 2(a0), a0
	bsr.w packageRegister
	movea.l (sp)+, a0
	cmpi.l #2, d0
	beq.w bad
	tst.l d0
	bne.w expressionOperand
	moveq #7, d6
	bsr.w copy
	bne.w bad
	bra.w operandDone
expressionOperand
	cmpa.l a1, a0
	bhs.w bad
	cmpi.b #3, (a0)
	bne.w compiledOperand
	; Decoded single-byte strings are shared scalar values in instruction
	; operands too. Longer strings remain data-only and fail closed here.
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w bad
	cmpi.b #1, 1(a0)
	bne.w bad
	bsr.w string
	bne.w bad
	bra.w operandDone
compiledOperand
	bsr.w compile
	bne.w bad
operandDone
	cmpa.l a1, a0
	beq.w complete
	cmpi.b #14, (a0)
	bne.w operandDelimiter
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #6, d0
	blo.w bad
	cmpi.b #1, 1(a0)
	bhi.w bad
	cmpi.b #15, 5(a0)
	bne.w bad
	move.l a0, -(sp)
	lea 1(a0), a0
	bsr.w packageRegister
	movea.l (sp)+, a0
	tst.l d0
	bne.w bad
	moveq #6, d6
	bsr.w copy
	bne.w bad
	cmpa.l a1, a0
	beq.w complete
operandDelimiter
	cmpi.b #4, (a0)
	bne.w bad
	moveq #1, d6
	bsr.w copy
	bne.w bad
	cmpa.l a1, a0
	beq.w bad
	bra.w operands
copyRest
	move.l a1, d6
	sub.l a0, d6
	bsr.w copy
	bne.w bad
complete
	move.l a3, d1
	sub.l a5, d1
	move.l d1, d0
	subq.l #1, d0
	move.b d0, (a5)
	moveq #0, d0
	bra.w done
bad
	clr.b (a5)
	moveq #0, d1
	moveq #1, d0
done
	adda.w #256, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line
	.priv

; Copy D6 bounded input bytes; advances A0/A3, clobbers D0/D6. CCR=status.
copy	.block
	move.l a1, d0
	sub.l a0, d0
	cmp.l d6, d0
	blo.w bad
	move.l a4, d0
	sub.l a3, d0
	cmp.l d6, d0
	blo.w bad
loop
	tst.l d6
	beq.w ok
	move.b (a0)+, (a3)+
	subq.l #1, d6
	bra.w loop
ok
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; copy

; Copy one numeric name, return its ID in D7. Other clobbers as copy.
name	.block
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w bad
	cmpi.b #1, (a0)
	bhi.w bad
	moveq #0, d7
	move.b 1(a0), d7
	lsl.w #8, d7
	move.b 2(a0), d7
	moveq #4, d6
	bra.w copy
bad
	moveq #1, d0
	rts
	.bend  ; name

; Recognize a package-register name/range/list without modifying the source
; or output. Non-register subtraction stays on the scalar expression path.
; D0=0 and D6=byte count for a list, 1 for another expression, 2 bad package.
validateNameSequence	.block
	movem.l d1-d5/a0/a3, -(sp)
	move.l a0, d5
	movea.l a0, a3
nextName
	movea.l a3, a0
	bsr.w packageRegister
	tst.l d0
	bne.w done
	adda.w #4, a3
	cmpa.l a1, a3
	beq.w valid
	cmpi.b #4, (a3)
	beq.w valid
	cmpi.b #19, (a3)
	beq.w separator
	cmpi.b #22, (a3)
	bne.w other
separator
	addq.l #1, a3
	bra.w nextName
valid
	move.l a3, d6
	sub.l d5, d6
	moveq #0, d0
	bra.w done
other
	moveq #1, d0
done
	movem.l (sp)+, d1-d5/a0/a3
	rts
	.bend  ; validateNameSequence

; A0=name token,A1=end,A2=BSP3 package. D0=0 known register, 1 other,
; 2 malformed package; all other registers preserved.
packageRegister	.block
	movem.l d1-d4/a3, -(sp)
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w other
	cmpi.b #1, (a0)
	bhi.w other
	tst.b 3(a0)
	bne.w other
	moveq #0, d1
	move.w 1(a0), d1
	move.l package.Header.RegisterRows(a2), d0
	move.l package.Header.RegisterCount(a2), d3
	cmpi.l #$ffff, d3
	bhi.w malformed
	move.l d3, d4
	mulu.w #6, d4
	add.l d0, d4
	bcs.w malformed
	cmp.l package.Header.Bytes(a2), d4
	bhi.w malformed
	movea.l a2, a3
	adda.l d0, a3
scan
	tst.l d3
	beq.w other
	cmp.w (a3), d1
	beq.w known
	addq.l #6, a3
	subq.l #1, d3
	bra.w scan
known
	moveq #0, d0
	bra.w done
other
	moveq #1, d0
	bra.w done
malformed
	moveq #2, d0
done
	movem.l (sp)+, d1-d4/a3
	tst.l d0
	rts
	.bend  ; packageRegister

; Keep a decoded string of two or more bytes as a data operand. A one-byte
; string is a scalar, so compile its byte as a numeric literal. No source text
; is consulted. A0 advances over the complete bounded string token.
string	.block
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w bad
	moveq #0, d6
	move.b 1(a0), d6
	move.l d6, d1
	addq.l #2, d1
	cmp.l d1, d0
	blo.w bad
	tst.w d6
	beq.w bad
	cmpi.w #1, d6
	beq.w scalarByte
	move.l d1, d6
	bra.w copy
scalarByte
	; expression.compile accepts numeric kind 2, never raw string kind 3.
	; This temporary literal is independent of input and output record storage.
	movem.l a0-a1, -(sp)
	suba.w #8, sp
	move.b #2, (sp)
	clr.b 1(sp)
	clr.b 2(sp)
	clr.b 3(sp)
	move.b 2(a0), 4(sp)
	movea.l sp, a0
	lea 5(sp), a1
	bsr.w compile
	adda.w #8, sp
	movem.l (sp)+, a0-a1
	tst.l d0
	bne.w bad
	addq.l #3, a0
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; string

; Compile through bounded scratch, then preserve a member wrapper when a
; parenthesized scalar or numeric literal is followed by .<package name>.
; Normalize both spellings to the same packed wrapper without consulting text.
; A0 advances; A3 receives compiled bytes. D0/status, D5-D7 scratch.
compile	.block
	cmpa.l a1, a0
	bhs.w bad
	moveq #0, d5
	move.b (a0), d5
	movem.l a3-a4, -(sp)
	movea.l a6, a3
	lea 256(a6), a4
	jsr expression.compile
	move.l a3, d6
	sub.l a6, d6
	movem.l (sp)+, a3-a4
	tst.l d0
	bne.w bad
	moveq #0, d7
	cmpa.l a1, a0
	bhs.w ready
	cmpi.b #7, (a0)
	bne.w ready
	cmpi.b #14, d5
	beq.w memberRoot
	cmpi.b #2, d5
	bne.w bad
memberRoot
	moveq #1, d7
ready
	move.l a4, d0
	sub.l a3, d0
	sub.l d6, d0
	bcs.w bad
	tst.l d7
	beq.w payload
	cmpi.l #7, d0
	blo.w bad
	move.b #14, (a3)+
payload
	movea.l a6, a4
loop
	move.b (a4)+, (a3)+
	subq.l #1, d6
	bne.w loop
	lea 256(a5), a4
	tst.l d7
	beq.w ok
	move.b #15, (a3)+
	moveq #1, d6
	bsr.w copy
	bne.w bad
	bsr.w name
	bne.w bad
ok
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; compile
	.endsection
	.endmodule
