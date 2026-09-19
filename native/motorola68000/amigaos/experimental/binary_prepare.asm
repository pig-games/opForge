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
	cmp.w package.Header.CpuDirective(a2), d7
	beq.w copyRest
	cmp.w package.Header.EndDirective(a2), d7
	beq.w copyRest
	cmp.w package.Header.OrgDirective(a2), d7
	beq.w scalar
	cmp.w package.Header.ByteDirective(a2), d7
	beq.w scalar
	cmp.w package.Header.WordDirective(a2), d7
	beq.w scalar
	cmp.w package.Header.LongDirective(a2), d7
	bne.w bad
scalar
	; Shared directive arguments are always expressions, never register names.
	bsr.w compile
	bne.w bad
	cmpa.l a1, a0
	beq.w complete
	cmpi.b #4, (a0)
	bne.w bad
	moveq #1, d6
	bsr.w copy
	bne.w bad
	bra.w scalar
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
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w expressionOperand
	cmpi.b #1, (a0)
	bhi.w expressionOperand
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
expressionOperand
	bsr.w compile
	bne.w bad
operandDone
	cmpa.l a1, a0
	beq.w complete
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

; Compile through bounded scratch, then preserve a parenthesized member wrapper
; if the scalar is followed by .<package name>. No target spelling is inspected.
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
	bne.w bad
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
