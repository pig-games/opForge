; Preparation-only lexical iterator identities; runtime records retain IDs only.
; @opforge-owner: experimental.amigaos.binary_iterator_bindings
	.module experimental.amigaos.binary_iterator_bindings
	.cpu 68020
	.use experimental.amigaos.binary_lexical_store as lexical
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_name_store as nameStore
	.pub
LIMIT = 16
COUNTED = $ffff
FOR = 0
WHILE = 3
SCOPED_FOR = 4
SCOPED_WHILE = 5
Slot	.struct
Identity	.word ?
Kind	.word ?
	.endstruct
SLOT_BYTES = Slot.Kind+2
SLOTS_BYTES = LIMIT*SLOT_BYTES
State	.struct
Depth	.word ?
Slots	.res SLOTS_BYTES
	.endstruct
STATE_BYTES = State.Slots+SLOTS_BYTES
	.section code, kind=code

; A0=state. Reset the lexical overlay; no clobbers.
begin	.block
	clr.w State.Depth(a0)
	rts
	.bend  ; begin

; A0=state,D0=temporary source ID or COUNTED,D1=FOR/WHILE kind. D0/CCR=status; others kept.
open	.block
	movem.l d1-d3/a1, -(sp)
	move.l d1, d3
	moveq #0, d1
	move.w State.Depth(a0), d1
	cmpi.w #LIMIT, d1
	bhs.w bad
	cmpi.l #COUNTED, d0
	bhi.w bad
	move.l d1, d2
	mulu.w #SLOT_BYTES, d2
	lea State.Slots(a0), a1
	move.w d0, Slot.Identity(a1, d2.w)
	move.w d3, Slot.Kind(a1, d2.w)
	addq.w #1, State.Depth(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a1
	tst.l d0
	rts
	.bend  ; open

; A0=state,D1=expected FOR/WHILE kind. Pop one matching lexical loop. D0/CCR=status; others kept.
close	.block
	movem.l d2/a1, -(sp)
	move.w State.Depth(a0), d2
	beq.w bad
	subq.w #1, d2
	mulu.w #SLOT_BYTES, d2
	lea State.Slots(a0), a1
	cmp.w Slot.Kind(a1, d2.w), d1
	beq.w matched
	cmpi.w #WHILE, d1
	beq.w closeScopedWhile
	cmpi.w #FOR, d1
	bne.w bad
	cmpi.w #SCOPED_FOR, Slot.Kind(a1, d2.w)
	bne.w bad
	bra.w matched
closeScopedWhile
	cmpi.w #SCOPED_WHILE, Slot.Kind(a1, d2.w)
	bne.w bad
matched
	subq.w #1, State.Depth(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2/a1
	tst.l d0
	rts
	.bend  ; close

; A0/D0=unqualified lexical name/bytes,A1=state,A2=lexical.State,A3=unused,
; D1=source ID base,D2=current lexical owner (entry index+1).
; D0/CCR=0 matched,1 absent; D1=temporary ID,D2=zero qualifier on match.
; Other registers preserved. Lookup happens only during source preparation.
probe	.block
	movem.l d3-d7/a0-a6, -(sp)
	movea.l a1, a6
	movea.l a0, a5
	move.l d0, d6
	move.l d1, d4
	move.l d2, d5
	tst.l d6
	beq.w absent
	moveq #0, d7
	move.w State.Depth(a6), d7
	beq.w absent
	; Qualified spellings always retain their declaration/import binding.
	move.l d6, d0
qualified
	cmpi.b #'.', (a0)+
	beq.w absent
	subq.l #1, d0
	bne.w qualified
slot
	subq.w #1, d7
	bmi.w absent
	move.l d7, d0
	mulu.w #SLOT_BYTES, d0
	lea State.Slots(a6), a0
	moveq #0, d3
	move.w Slot.Identity(a0, d0.w), d3
	cmpi.w #COUNTED, d3
	beq.w slot
	move.l d3, d0
	sub.w d4, d0
	movea.l a2, a0
	lea LexicalEntry, a1
	jsr lexical.readEntry
	bne.w absent
	lea LexicalEntry, a4
	move.l d5, d2
ancestor
	cmp.w records.Entry.Owner(a4), d2
	beq.w name
	tst.w d2
	beq.w slot
	move.l d2, d0
	subq.w #1, d0
	movea.l a2, a0
	lea LexicalAncestor, a1
	jsr lexical.readEntry
	bne.w absent
	lea LexicalAncestor, a0
	moveq #0, d2
	move.w records.Entry.Owner(a0), d2
	bra.w ancestor
name
	moveq #0, d0
	move.w records.Entry.Length(a4), d0
	sub.w records.Entry.Leaf(a4), d0
	cmp.l d6, d0
	bne.w slot
	movea.l a2, a0
	lea LexicalName, a1
	move.l records.Entry.Name(a4), d0
	moveq #0, d1
	move.w records.Entry.Leaf(a4), d1
	moveq #0, d1
	move.w records.Entry.Length(a4), d1
	jsr lexical.copyName
	bne.w absent
	lea LexicalName, a0
	moveq #0, d1
	move.w records.Entry.Leaf(a4), d1
	adda.l d1, a0
	movea.l a5, a1
	move.l d6, d2
compare
	moveq #0, d0
	move.b (a0)+, d0
	bsr.w fold
	move.l d0, d1
	move.b (a1)+, d0
	bsr.w fold
	cmp.b d1, d0
	bne.w slot
	subq.l #1, d2
	bne.w compare
	move.l d3, d1
	moveq #0, d2
	moveq #0, d0
	bra.w done
absent
	moveq #1, d0
done
	movem.l (sp)+, d3-d7/a0-a6
	tst.l d0
	rts
	.bend  ; probe
	.priv
; D0=ASCII byte. Return its folded byte; other registers preserved.
fold	.block
	cmpi.b #'A', d0
	blo.w done
	cmpi.b #'Z', d0
	bhi.w done
	addi.b #'a'-'A', d0
done
	rts
	.bend  ; fold
	.pub
; A0=preparation iterator state. D0/CCR=zero only for innermost scoped repetition.
; Other registers preserved; declarations in ordinary loops remain forbidden.
scoped	.block
	movem.l d1/a1, -(sp)
	moveq #0, d1
	move.w State.Depth(a0), d1
	beq.w absent
	subq.w #1, d1
	mulu.w #SLOT_BYTES, d1
	lea State.Slots(a0), a1
	cmpi.w #SCOPED_FOR, Slot.Kind(a1, d1.w)
	beq.w found
	cmpi.w #SCOPED_WHILE, Slot.Kind(a1, d1.w)
	bne.w absent
found
	moveq #0, d0
	bra.w done
absent
	moveq #1, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; scoped
	.endsection
	.section bss, kind=bss
; Accessor scratch is consumed before any callback; no Entry view escapes.
LexicalEntry	.res byte, records.ENTRY_BYTES
LexicalAncestor	.res byte, records.ENTRY_BYTES
LexicalName	.res byte, records.NAME_BYTES
	.endsection
	.endmodule
