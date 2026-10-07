; Load preparation snapshots into the assembly session's immutable value owner.
; @opforge-owner: experimental.amigaos.binary_parameters
	.module experimental.amigaos.binary_parameters
	.cpu 68020
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_dependencies as dependencies
	.use exprvm.amigaos.runtime as runtime
	.pub
	.section code, kind=code

; A0=Context with cleared symbol cells and a reset value owner.
; Parameters is a bounded table followed by immutable payload; compound offsets
; are relative to that payload. Copy into the session owner, publish absolute
; symbols and retain its prefix for layout replay. D0/CCR=status; others kept.
load	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	clr.l pkg.Context.RetainedValues(a6)
	move.l pkg.Context.ParameterCount(a6), d7
	cmpi.l #pkg.PARAMETER_LIMIT, d7
	bhi.w bad
	move.l pkg.Context.ParameterBytes(a6), d6
	cmpi.l #memory.LIMIT, d6
	bhi.w bad
	move.l d7, d0
	mulu.w #pkg.PARAMETER_BYTES, d0
	cmp.l d0, d6
	blo.w bad
	sub.l d0, d6
	movea.l pkg.Context.Parameters(a6), a3
	move.l a3, d1
	tst.l d7
	beq.w empty
	tst.l d1
	beq.w bad
	btst #0, d1
	bne.w bad
	add.l pkg.Context.ParameterBytes(a6), d1
	bcs.w bad
	movea.l a3, a4
	adda.l d0, a4
next
	moveq #0, d4
	move.w pkg.Parameter.Id(a3), d4
	movea.l pkg.Context.Package(a6), a0
	cmp.w pkg.Header.NameCount(a0), d4
	blo.w bad
	cmp.l pkg.Context.Count(a6), d4
	bhs.w bad
	movea.l pkg.Context.Defined(a6), a1
	tst.b 0(a1, d4.l)
	bne.w bad
	moveq #0, d5
	move.w pkg.Parameter.Kind(a3), d5
	cmpi.l #values.RANGE, d5
	bhi.w bad
	move.l pkg.Parameter.Low(a3), d1
	move.l pkg.Parameter.High(a3), d2
	tst.l d5
	beq.w publish
	tst.l d2
	bne.w bad
	movea.l pkg.Context.Owner(a6), a0
	move.l a0, d0
	beq.w bad
	movea.l a4, a1
	move.l d6, d0
	move.l d5, d3
	jsr values.copyRecord
	bne.w bad
	move.l d2, d1
	moveq #0, d2
publish
	movem.l d1-d2, -(sp)
	movea.l pkg.Context.Owner(a6), a0
	move.l a0, d0
	beq.w noOwner
	move.l d4, d1
	move.l d5, d2
	jsr values.setKind
	bra.w kindReady
noOwner
	move.l d5, d0
kindReady
	movem.l (sp)+, d1-d2
	tst.l d0
	bne.w bad
	movea.l pkg.Context.Defined(a6), a1
	move.b #dependencies.ABSOLUTE, 0(a1, d4.l)
	lsl.l #3, d4
	movea.l pkg.Context.Values(a6), a0
	move.l d1, runtime.Value.Low(a0, d4.l)
	move.l d2, runtime.Value.High(a0, d4.l)
	adda.w #pkg.PARAMETER_BYTES, a3
	subq.l #1, d7
	bne.w next
	movea.l pkg.Context.Owner(a6), a0
	move.l a0, d0
	beq.w good
	move.l values.Owner.Arena+memory.Block.Used(a0), pkg.Context.RetainedValues(a6)
good
	moveq #0, d0
	bra.w done
empty
	tst.l d6
	beq.w good
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; load
	.endsection
	.endmodule
