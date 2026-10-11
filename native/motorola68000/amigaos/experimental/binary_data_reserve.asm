; @opforge-owner: experimental.amigaos.binary_assembly
; Shared byte reservations. Included inside the assembly output owner so flat
; gaps, initialized section padding and BSS extents use its existing lifecycle.
; D0=validated u32 byte count,A2=Context. D0/CCR=status; others preserved.
reserveDataBytes	.block
	movem.l d1-d2/a0-a1, -(sp)
	move.l pkg.Context.Pc(a2), d1
	add.l d0, d1
	bcs.w bad
	movea.l pkg.Context.Package(a2), a0
	cmp.l pkg.Header.MaxAddress(a0), d1
	bhi.w bad  ; the resulting next PC must remain in the address domain
	lea SectionState, a0
	tst.w sections.State.Mode(a0)
	beq.w flat
	cmpi.w #sections.KIND_BSS, sections.State.ActiveKind(a0)
	bne.w initialized
	movea.l a2, a1
	jsr sections.reserve
	bra.w done
initialized
	bsr.w emitGap
	bra.w done
flat
	; Materialize internal holes only when a subsequent initialized emission
	; arrives. Leading and trailing reservations remain outside the artifact.
	movea.l Session+session.State.Frame, a0
	tst.l Frame.Used(a0)
	beq.w flatPc
	add.l d0, PendingFlatGap
	bcs.w bad
flatPc
	move.l d1, pkg.Context.Pc(a2)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; reserveDataBytes
