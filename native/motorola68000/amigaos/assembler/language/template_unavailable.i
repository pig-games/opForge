; Metadata-only dependency carriers retain ordinary selection and visibility.
; No missing body may become an empty expansion or an ordinary statement.
	.pub
; A0=template State,A1=scope,D0=canonical destination name ID,D1=KIND_*.
; D0/CCR=STATUS_OK/STATUS_BAD,D1=one-based carrier or zero; others kept.
; Caller first publishes the canonical public template/statement facet and its
; module ownership through scopes. Internal kinds require TEMPLATE_* facets;
; EXTERNAL requires STATEMENT_* facets and remains outside the macro index.
; Seeds are closed and own no body/default/formal/spelling bytes. Install only
; at an idle session boundary; release the private session after any failure.
; Internal and external handles address their independent normal owner pools.
seedUnavailable	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #records.ENTRY_BYTES, sp
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d5
	move.l d1, d2
	cmpi.l #65535, d5
	bhi.w notSeeded
	cmpi.l #KIND_EXTERNAL, d2
	bhi.w notSeeded
	tst.w State.Open(a6)
	bne.w notSeeded
	tst.w State.ExternalOpen(a6)
	bne.w notSeeded
	tst.w State.Skipping(a6)
	bne.w notSeeded
	tst.w State.Depth(a6)
	bne.w notSeeded
	move.l d5, d0
	sub.w layout.State.Base(a5), d0
	bcs.w notSeeded
	cmp.w layout.State.Count(a5), d0
	bhs.w notSeeded
	andi.l #$ffff, d0
	lea layout.LEXICAL_STATE(a5), a0
	movea.l sp, a1
	jsr lexical.readEntry
	bne.w notSeeded
	move.w records.Entry.Flags(sp), d0
	andi.w #records.IMPORT_PROXY|records.TEMPLATE_PROXY|records.BINDING_PROXY, d0
	bne.w notSeeded
	moveq #records.TEMPLATE_DECLARED|records.TEMPLATE_PUBLIC, d3
	cmpi.l #KIND_EXTERNAL, d2
	bne.w facet
	moveq #records.STATEMENT_DECLARED|records.STATEMENT_PUBLIC, d3
facet
	move.w records.Entry.TemplateFlags(sp), d0
	and.w d3, d0
	cmp.w d3, d0
	bne.w notSeeded
	cmpi.l #KIND_EXTERNAL, d2
	beq.w external
	move.l d5, d0
	movea.l a6, a0
	adda.l #CANDIDATES, a0
	jsr index.find
	tst.w d1
	bne.w notSeeded
	moveq #0, d4
	move.w State.Count(a6), d4
	bsr.w reserveDefinition
	bne.w notSeeded
	movea.l a0, a3
	move.l d5, d0
	movea.l a5, a0
	jsr scopes.templateKey
	bne.w notSeeded
	move.l d5, d0
	movea.l a6, a0
	adda.l #CANDIDATES, a0
	jsr index.add
	bne.w notSeeded
	addq.w #1, State.Count(a6)
	moveq #0, d1
	move.w State.Count(a6), d1
	bra.w descriptor
external
	movea.l a6, a0
	bsr.w externalReserve
	bne.w notSeeded
	moveq #0, d4
	move.w State.ExternalCount(a6), d4
	lea EXTERNALS(a6), a0
	bsr.w reserveRow
	bne.w notSeeded
	movea.l a0, a3
	addq.w #1, State.ExternalCount(a6)
	moveq #0, d1
	move.w State.ExternalCount(a6), d1
descriptor
	move.w d5, Def.Name(a3)
	move.w d2, Def.Kind(a3)
	move.l State.Used(a6), Def.First(a3)
	move.l State.Used(a6), Def.Last(a3)
	move.l #BODY_UNAVAILABLE, Def.Flags(a3)
	moveq #STATUS_OK, d0
	bra.w seedDone
notSeeded
	moveq #0, d1
	moveq #STATUS_BAD, d0
seedDone
	adda.w #records.ENTRY_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; seedUnavailable

; A0=template State,D0=external one-based carrier selected by statement PRVM.
; D0/CCR=STATUS_OK only for a present, available external body; others kept.
; Call immediately after selection, before argument preparation. Internal
; template calls apply the same availability rule after their normal selection.
requireBody	.block
	movem.l d1/a0-a1, -(sp)
	tst.l d0
	beq.w notAvailable
	cmpi.l #65535, d0
	bhi.w notAvailable
	cmp.w State.ExternalCount(a0), d0
	bhi.w notAvailable
	subq.l #1, d0
	mulu.w #DEF_BYTES, d0
	movea.l EXTERNALS+memory.Block.Pointer(a0), a1
	adda.l d0, a1
	cmpi.w #KIND_EXTERNAL, Def.Kind(a1)
	bne.w notAvailable
	tst.l Def.Flags(a1)
	bne.w notAvailable
	moveq #STATUS_OK, d0
	bra.w bodyDone
notAvailable
	moveq #STATUS_BAD, d0
bodyDone
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; requireBody
