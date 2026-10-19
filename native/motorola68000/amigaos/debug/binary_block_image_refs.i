; Compiled payloads keep their outgoing identities in an internal layout record.
; A0=record,D6=exact bytes. D0/CCR=status; other registers preserved.
; Use the same numeric owner binder as ordinary names and expression bytecode.
compiledReferences	.block
	movem.l d1-d3/a0-a2, -(sp)
	move.l d6, d0
	jsr compiled.referencesView
	bne.w bad
	movea.l a1, a0
next
	tst.l d1
	beq.w good
	moveq #0, d0
	move.w (a0)+, d0
	bsr.w mark
	bne.w bad
	subq.l #1, d1
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; compiledReferences
