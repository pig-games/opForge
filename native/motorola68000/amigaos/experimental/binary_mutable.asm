; Statement-time signed scalar storage for read-write declarations.
; Values persist between assembly passes, matching forward/self initializers.
; @opforge-owner: experimental.amigaos.binary_mutable
	.module experimental.amigaos.binary_mutable
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use opasm.amigaos.binary_expression as expression
	.use exprvm.amigaos.runtime as runtime
	.use experimental.amigaos.binary_source as source
	.pub
DEFINED = 3
; Readonly statement-time scalar proof; unlike dependency ABSOLUTE, recompute.
SNAPSHOT_ABSOLUTE = 4
PENDING = 7; dependency indexing only; never an executable availability state
INVALID_RECORDS = 1
UNSUPPORTED_LAYOUT = 2
	.section code, kind=code
; Reject scalar mutations before layouts that execute filtered record sweeps.
; A0=packed records, D0=record bytes, D2=layout mode. D0/CCR=status;
; D1=offending record offset, or -1 when none. Other registers preserved.
; Only modes 2/4 need inspection; inactive/template/control records are ignored.
validateLayout	.block
	movem.l d2-d4/a0-a2, -(sp)
	moveq #-1, d1
	cmpi.w #2, d2
	beq.w scan
	cmpi.w #4, d2
	beq.w scan
	bra.w good
scan
	movea.l a0, a2
	add.l a0, d0
	bcs.w bad
	movea.l d0, a1
record
	cmpa.l a1, a0
	beq.w good
	bhi.w bad
	move.l a0, d1
	sub.l a2, d1
	moveq #0, d3
	move.b (a0), d3
	addq.w #1, d3
	cmpi.w #4, d3
	blo.w bad
	move.l a1, d4
	sub.l a0, d4
	cmp.l d4, d3
	bhi.w bad
	moveq #0, d4
	move.b 1(a0), d4
	cmpi.b #source.FLAG_ALLOWED, d4
	bhi.w bad
	andi.w #source.FLAG_OMIT+source.FLAG_LAYOUT+source.FLAG_PLAN, d4
	bne.w next
	cmpi.w #9, d3
	blo.w next
	cmpi.b #1, 4(a0)
	bhi.w next
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, 8(a0)
	beq.w unsupported
next
	adda.l d3, a0
	bra.w record
unsupported
	moveq #UNSUPPORTED_LAYOUT, d0
	bra.w done
bad
	moveq #INVALID_RECORDS, d0
	bra.w done
good
	moveq #-1, d1
	moveq #0, d0
done
	movem.l (sp)+, d2-d4/a0-a2
	tst.l d0
	rts
	.bend  ; validateLayout

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
