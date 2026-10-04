; Concrete-prefix origins and final extents for mapped numeric Hunk fragments.
; @opforge-owner: experimental.amigaos.binary_hunk_mapping
	.module experimental.amigaos.binary_hunk_mapping
	.cpu 68020
	.use experimental.amigaos.binary_sections as sections
	.use experimental.amigaos.binary_package as pkg
	.pub
	.section code, kind=code

; A0=section state. Require distinct logical sources/concrete targets, matching
; kinds and declared concrete destinations. Output names select concrete slots.
; D0/CCR=status; other registers preserved. No names or source strings enter.
validate	.block
	movem.l d1-d5/a0-a3, -(sp)
	movea.l a0, a3
	lea sections.HUNK_SLOTS(a3), a1
	moveq #0, d4
	moveq #7, d5
slot
	moveq #0, d1
	move.w sections.HunkSlot.Target(a1), d1
	beq.w next
	subq.w #1, d1
	btst d1, d4
	bne.w bad  ; Rust also forbids two logical sections mapping to one target
	bset d1, d4
	move.l d1, d2
	mulu.w #sections.HUNK_SLOT_BYTES, d2
	lea sections.HUNK_SLOTS(a3), a2
	adda.l d2, a2
	tst.w sections.HunkSlot.Seen(a2)
	beq.w bad
	tst.w sections.HunkSlot.Target(a2)
	bne.w bad  ; no chained mappings
	move.w sections.HunkSlot.Kind(a1), d2
	cmp.w sections.HunkSlot.Kind(a2), d2
	bne.w bad
	moveq #7, d2
	sub.w d5, d2
	lea sections.ORDER(a3), a0
	move.w sections.State.OrderCount(a3), d3
selected
	cmp.b (a0)+, d2
	beq.w bad  ; mapped source is not an independent output section
	subq.w #1, d3
	bne.w selected
next
	adda.w #sections.HUNK_SLOT_BYTES, a1
	dbra d5, slot
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d5/a0-a3
	tst.l d0
	rts
	.bend  ; validate

; A0=section state,A1=Context. Freeze measured concrete PC/payload prefixes.
; The assembly coordinator resets provisional symbols before replay.
; D0/CCR=status; other registers preserved.
freeze	.block
	movem.l d1-d3/a0-a4, -(sp)
	lea sections.HUNK_SLOTS(a0), a2
	moveq #7, d3
slot
	moveq #0, d1
	move.w sections.HunkSlot.Target(a2), d1
	beq.w next
	subq.w #1, d1
	mulu.w #sections.HUNK_SLOT_BYTES, d1
	lea sections.HUNK_SLOTS(a0), a3
	adda.l d1, a3
	move.l sections.HunkSlot.Allocation(a3), sections.HunkSlot.Bias(a2)
	clr.l sections.HunkSlot.PayloadBias(a2)
	cmpi.w #3, sections.HunkSlot.Kind(a3)
	beq.w next
	move.l sections.HunkSlot.Allocation(a3), sections.HunkSlot.PayloadBias(a2)
next
	adda.w #sections.HUNK_SLOT_BYTES, a2
	dbra d3, slot
	move.w #1, sections.State.MapReady(a0)
	moveq #0, d0
	movem.l (sp)+, d1-d3/a0-a4
	rts
	.bend  ; freeze

; A0=section state. Compare frozen prefixes with current allocation extents.
; The coordinator may replay pass one; pass-two changes fail. D0/CCR=status,
; other registers preserved.
check	.block
	movem.l d1-d3/a0-a3, -(sp)
	lea sections.HUNK_SLOTS(a0), a1
	moveq #7, d3
slot
	moveq #0, d1
	move.w sections.HunkSlot.Target(a1), d1
	beq.w next
	subq.w #1, d1
	mulu.w #sections.HUNK_SLOT_BYTES, d1
	lea sections.HUNK_SLOTS(a0), a2
	adda.l d1, a2
	move.l sections.HunkSlot.Allocation(a2), d2
	cmp.l sections.HunkSlot.Bias(a1), d2
	bne.w bad
	moveq #0, d2
	cmpi.w #3, sections.HunkSlot.Kind(a2)
	beq.w payloadChecked
	move.l sections.HunkSlot.Allocation(a2), d2
payloadChecked
	cmp.l sections.HunkSlot.PayloadBias(a1), d2
	bne.w bad
next
	adda.w #sections.HUNK_SLOT_BYTES, a1
	dbra d3, slot
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; check

; After final emission only, expose combined extents to the shared Hunk writer.
; A0=section state. D0/CCR=status; other registers preserved.
merge	.block
	movem.l d1-d3/a0-a2, -(sp)
	lea sections.HUNK_SLOTS(a0), a1
	moveq #7, d3
slot
	moveq #0, d1
	move.w sections.HunkSlot.Target(a1), d1
	beq.w next
	subq.w #1, d1
	mulu.w #sections.HUNK_SLOT_BYTES, d1
	lea sections.HUNK_SLOTS(a0), a2
	adda.l d1, a2
	move.l sections.HunkSlot.Size(a1), d2
	add.l d2, sections.HunkSlot.Size(a2)
	bcs.w bad
	move.l sections.HunkSlot.Allocation(a1), d2
	add.l d2, sections.HunkSlot.Allocation(a2)
	bcs.w bad
	move.l sections.HunkSlot.PayloadBias(a1), d2
	add.l sections.HunkSlot.Used(a1), d2
	bcs.w bad
	cmp.l sections.HunkSlot.Used(a2), d2
	bls.w next
	move.l d2, sections.HunkSlot.Used(a2)
next
	adda.w #sections.HUNK_SLOT_BYTES, a1
	dbra d3, slot
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; merge
	.endsection
	.endmodule
