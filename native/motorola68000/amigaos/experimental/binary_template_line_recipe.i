; Private preparation trailers and per-record lexical publication. These tokens
; never reach canonical records: optional recipes are consumed or discarded.

; A0=record,D0=encoded record bytes. D0/CCR=status; all other registers kept.
; Exactly one trailer may follow the packed body. Bit6 belongs to this owner's
; line recipes, not the canonical writer mutable-declaration flag. Recipe handles
; are opaque/nonzero here; the plans/FragmentLine owners validate their storage.
validateBodyPlan	.block
	movem.l d1/a1, -(sp)
	cmpi.l #source.RECORD_HEADER_BYTES, d0
	blo.w bad
	cmpi.l #source.MAX_LINE, d0
	bhi.w bad
	moveq #0, d1
	move.b source.RecordHeader.Flags(a0), d1
	andi.w #source.FLAG_PLAN+LINE_PLAN_FLAG, d1
	beq.w ready
	cmpi.l #source.RECORD_HEADER_BYTES+source.PLAN_TOKEN_BYTES, d0
	blo.w bad
	movea.l a0, a1
	adda.l d0, a1
	suba.w #source.PLAN_TOKEN_BYTES, a1
	cmpi.b #source.PLAN_TOKEN_BYTES, source.PlanToken.Bytes(a1)
	bne.w bad
	tst.l source.PlanToken.Handle(a1)
	beq.w bad
	cmpi.w #source.FLAG_PLAN, d1
	beq.w callPlan
	cmpi.w #LINE_PLAN_FLAG, d1
	bne.w bad
	cmpi.b #TOKEN_LINE_PLAN, source.PlanToken.Kind(a1)
	beq.w ready
	cmpi.b #TOKEN_OPTIONAL_LINE_PLAN, source.PlanToken.Kind(a1)
	bne.w bad
	bra.w ready
callPlan
	cmpi.b #TOKEN_PLAN, source.PlanToken.Kind(a1)
	bne.w bad
ready
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; validateBodyPlan

; A0=State. Preserve all registers; CCR unspecified. NeedLexical stays owned by
; the caller. Clear before a cursor attempt and after discarded blank recipes.
clearLexicalRecord	.block
	clr.w State.LexicalReady(a0)
	clr.w State.InheritedLabelPresent(a0)
	clr.l State.InheritedLabel(a0)
	rts
	.bend  ; clearLexicalRecord

; A0=State,A6=CallFrame. Preserve all registers; CCR unspecified. Publish only
; after the numeric caller label has been appended to this output record.
publishInheritedLabel	.block
	move.w #1, State.InheritedLabelPresent(a0)
	move.l CallFrame.CallLabel(a6), State.InheritedLabel(a0)
	rts
	.bend  ; publishInheritedLabel
