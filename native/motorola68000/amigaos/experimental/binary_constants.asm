; Capture readonly statement values and their relocation proof together.
; @opforge-owner: experimental.amigaos.binary_constants

	.module experimental.amigaos.binary_constants
	.cpu 68020
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_dependencies as dependencies
	.use experimental.amigaos.binary_mutable as mutable
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_hunk_references as references
	.use opasm.amigaos.binary_expression as expr
	.use exprvm.amigaos.runtime as runtime
	.use experimental.amigaos.binary_values as values
	.priv
LAYOUT = 1
	.section code, kind=code
	.pub

; A0=name+constant marker+compiled scalar,A1=end,A2=Context,D0=record flags.
; D0/CCR=status; other registers preserved. Captures value and source section
; without retaining the expression or reevaluating aliases at their use sites.
; Pass-one unresolved values remain unavailable; pass two must resolve them.
; Dependency preparation already checked declaration ownership. Unsupported
; address algebra retains no section proof, so a later Hunk use fails closed.
execute	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l d0, d6
	move.l a1, d0
	sub.l a0, d0
	bcs.w bad
	cmpi.l #8, d0
	blo.w bad
	cmpi.b #1, (a0)
	bhi.w bad
	tst.b 3(a0)
	bne.w bad
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, 4(a0)
	bne.w bad
	moveq #0, d4
	move.w 1(a0), d4
	movea.l pkg.Context.Package(a2), a3
	cmp.w pkg.Header.NameCount(a3), d4
	blo.w bad
	cmp.l pkg.Context.Count(a2), d4
	bhs.w bad
	movea.l pkg.Context.Defined(a2), a4
	cmpi.b #dependencies.ABSOLUTE, 0(a4, d4.l)
	beq.w good  ; preparation already validated and evaluated this definition
	addq.l #5, a0
	move.l a0, -(sp)
	jsr expr.evaluateValue
	movea.l (sp)+, a3  ; bounded definition wrapper for provenance proof
	tst.l d0
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	moveq #LAYOUT, d7
	moveq #0, d3
	tst.l d2
	beq.w resolved
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w bad
	moveq #0, d7  ; placeholders must not appear resolved to their consumers
	bra.w store
resolved
	tst.l pkg.Context.Kind(a2)
	beq.w scalarProof
	tst.w pkg.Context.Relocatable(a2)
	bne.w bad  ; compound address provenance is not qualified yet
	bra.w store
scalarProof
	tst.w pkg.Context.Relocatable(a2)
	beq.w store
	movem.l d1-d2/a0-a1, -(sp)
	movea.l a3, a0
	jsr references.affineTarget
	cmpi.l #references.STATUS_CLEAR, d0
	beq.w scalar
	cmpi.l #references.STATUS_SECTION, d0
	bne.w proofReady
	jsr references.baseSection
	bne.w proofReady
	move.l d1, d3
	bra.w proofReady
scalar
	moveq #mutable.SNAPSHOT_ABSOLUTE, d7
proofReady
	movem.l (sp)+, d1-d2/a0-a1
store
	movea.l pkg.Context.Defined(a2), a4
	movea.l pkg.Context.Values(a2), a5
	move.l d4, d5
	lsl.l #3, d5
	cmpi.w #1, pkg.Context.Pass(a2)
	bne.w existing
	tst.b 0(a4, d4.l)
	bne.w bad
	bra.w write
existing
	tst.b 0(a4, d4.l)
	beq.w write  ; first resolved value of a pass-one forward placeholder
	btst #6, d6  ; source.FLAG_MUTABLE_SNAPSHOT refreshes at its declaration
	bne.w write
	tst.l pkg.Context.Kind(a2)
	beq.w compareScalar
	tst.b 0(a4, d4.l)
	bpl.w bad
	movem.l d1-d2/a0, -(sp)
	move.l runtime.Value.Low(a5, d5.l), d2
	movea.l pkg.Context.Owner(a2), a0
	jsr values.equalLists
	movem.l (sp)+, d1-d2/a0
	tst.l d0
	bne.w bad
	bra.w write
compareScalar
	tst.b 0(a4, d4.l)
	bmi.w bad
	move.l pkg.Context.High(a2), d0
	cmp.l runtime.Value.High(a5, d5.l), d0
	bne.w bad
	cmp.l runtime.Value.Low(a5, d5.l), d1
	bne.w bad  ; another layout iteration is outside this two-pass engine
write
	movem.l d1-d2/a0, -(sp)
	movea.l pkg.Context.Owner(a2), a0
	move.l a0, d0
	beq.w noOwner
	move.l d4, d1
	move.l pkg.Context.Kind(a2), d2
	jsr values.setKind
	bra.w kindReady
noOwner
	tst.l pkg.Context.Kind(a2)
	beq.w kindReady
	moveq #1, d0
kindReady
	movem.l (sp)+, d1-d2/a0
	tst.l d0
	bne.w bad
	tst.l pkg.Context.Kind(a2)
	beq.w writeCell
	tst.l d7
	beq.w writeCell  ; unavailable values must keep Defined=0
	bset #expr.COMPOUND_BIT, d7
writeCell
	move.l d1, runtime.Value.Low(a5, d5.l)
	move.l pkg.Context.High(a2), runtime.Value.High(a5, d5.l)
	move.b d7, 0(a4, d4.l)
	movea.l pkg.Context.SectionIds(a2), a5
	move.b d3, 0(a5, d4.l)
good
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
