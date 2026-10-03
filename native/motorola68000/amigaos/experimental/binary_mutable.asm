; Statement-time signed scalar storage for read-write declarations.
; Values persist between assembly passes, matching forward/self initializers.
	.module experimental.amigaos.binary_mutable
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use opasm.amigaos.binary_expression as expression
	.use exprvm.amigaos.runtime as runtime
	.pub
DEFINED = 3
PENDING = 7; dependency indexing only; never an executable availability state
	.section code, kind=code
; A0=name+mutable marker+compiled scalar,A1=end,A2=Context.
; D0/CCR=status; other registers preserved. Stores both signed64 words only
; after full expression/ownership validation. Pass1 may retain unresolved
; placeholders; pass2 requires a resolved value. No immutable graph mutation.
execute	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.b 3(a0)
	bne.w bad
	moveq #0, d4
	move.w 1(a0), d4
	movea.l package.Context.Package(a2), a3
	cmp.w package.Header.NameCount(a3), d4
	blo.w bad
	cmp.l package.Context.Count(a2), d4
	bhs.w bad
	movea.l package.Context.Defined(a2), a3
	tst.b 0(a3, d4.l)
	beq.w evaluate
	cmpi.b #DEFINED, 0(a3, d4.l)
	bne.w bad
evaluate
	addq.l #5, a0
	movea.l a2, a6
	jsr expression.evaluate
	movea.l a6, a2
	tst.l d0
	bne.w bad
	tst.l d2
	beq.w resolved
	cmpi.w #1, package.Context.Pass(a2)
	bne.w bad
resolved
	cmpa.l a1, a0
	bne.w bad
	movea.l package.Context.Values(a2), a3
	move.l d4, d5
	lsl.l #3, d5
	move.l d1, runtime.Value.Low(a3, d5.l)
	move.l package.Context.High(a2), runtime.Value.High(a3, d5.l)
	movea.l package.Context.Defined(a2), a3
	move.b #DEFINED, 0(a3, d4.l)
	movea.l package.Context.SectionIds(a2), a3
	clr.b 0(a3, d4.l)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; execute
	.endsection
	.endmodule
