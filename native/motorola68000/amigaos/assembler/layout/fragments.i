; Optional artifact consumers observe semantic reservations/alignment. The shared
; assembler still owns validation, sizing and original section progression.

; D0=validated BSS extent,A2=Context. D0/CCR=status; others preserved.
; Observe only selected final-pass extents, before changing their original PC.
reserveSectionBytes	.block
	movem.l d1-d3/a0-a1, -(sp)
	move.l d0, d3
	cmpi.w #2, pkg.Context.Pass(a2)
	bne.w reserve
	tst.w SectionState+sections.State.HunkSelected
	beq.w reserve
	movea.l Session+session.State.Frame, a0
	movea.l Frame.Reserved(a0), a1
	cmpa.w #0, a1
	beq.w reserve
	move.l pkg.Context.Pc(a2), d1
	move.l Frame.Used(a0), d2
	jsr (a1)
	bne.w done
reserve
	move.l d3, d0
	lea SectionState, a0
	movea.l a2, a1
	jsr sections.reserve
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; reserveSectionBytes

; D0=padding,D1=alignment,A2=Context. D0/CCR=status; others preserved.
; Alignment consumers receive the constraint even when the original PC already
; satisfies it. Its original padding advances assembly but is not payload/extent
; reported a second time to that consumer. Normal output remains unchanged.
emitAlignment	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l d0, d7
	move.l d1, d6
	movea.l Session+session.State.Frame, a5
	move.l Frame.Emitted(a5), d4
	move.l Frame.Reserved(a5), d5
	movea.l Frame.Aligned(a5), a1
	cmpa.w #0, a1
	beq.w padding
	cmpi.w #2, pkg.Context.Pass(a2)
	bne.w suppress
	tst.w SectionState+sections.State.HunkSelected
	beq.w suppress
	movea.l a5, a0
	move.l pkg.Context.Pc(a2), d1
	move.l d6, d2
	move.l d7, d3
	jsr (a1)
	bne.w done
suppress
	clr.l Frame.Emitted(a5)
	clr.l Frame.Reserved(a5)
padding
	move.l d7, d0
	lea SectionState, a0
	cmpi.w #sections.KIND_BSS, sections.State.ActiveKind(a0)
	bne.w initialized
	movea.l a2, a1
	jsr sections.reserve
	bra.w restore
initialized
	bsr.w emitGap
restore
	move.l d4, Frame.Emitted(a5)
	move.l d5, Frame.Reserved(a5)
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; emitAlignment

; Internal immutable-fragment carrier. The callback owns only image lookup,
; placement and VM patching; the shared engine owns section/PC/output behavior.
; A0=record,D0=exact bytes,A2=Context. D0/CCR=status; others preserved.
; Callback A0=Frame,A1=record,D0=operation (0 describe,1 place,2 link).
; Success D1=alignment,D2=initialized bytes,D3=allocation extent,A1=payload.
; Place/link use the current Context.Pc and Frame.Sections. No allocation may
; change the frozen record or symbol arrays. BSS has Bytes=0 and Extent>0.
emitCompiledFragment	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a4
	jsr moduleRecords.fragmentView
	bne.w bad
	movea.l Session+session.State.Frame, a5
	movea.l Frame.Fragment(a5), a3
	cmpa.w #0, a3
	beq.w bad
	cmpi.w #sections.HUNK_MODE, SectionState+sections.State.Mode
	bne.w bad
	tst.w SectionState+sections.State.HunkSelected
	beq.w good
	movea.l a5, a0
	movea.l a4, a1
	moveq #0, d0
	jsr (a3)
	bne.w bad
	move.l d1, d6
	beq.w bad
	subq.l #1, d6
	move.l d6, d0
	and.l d1, d0
	bne.w bad
	move.l pkg.Context.Pc(a2), d0
	add.l d6, d0
	bcs.w bad
	not.l d6
	and.l d6, d0
	sub.l pkg.Context.Pc(a2), d0
	bsr.w emitAlignment
	bne.w bad
	movea.l a5, a0
	movea.l a4, a1
	moveq #1, d0
	cmpi.w #2, pkg.Context.Pass(a2)
	bne.w execute
	moveq #2, d0
execute
	jsr (a3)
	bne.w bad
	cmpi.w #sections.KIND_BSS, SectionState+sections.State.ActiveKind
	beq.w bss
	cmp.l d3, d2
	bne.w bad
	move.l d2, d0
	movea.l a1, a0
	bsr.w emitBytes
	bra.w done
bss
	tst.l d2
	bne.w bad
	move.l d3, d0
	bsr.w reserveSectionBytes
	bra.w done
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; emitCompiledFragment
