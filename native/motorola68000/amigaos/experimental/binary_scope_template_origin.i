; Metadata origins preserve a captured spelling's hygiene boundary without
; changing the canonical destination declaration or adding a value facet.
; KIND_TEMPLATE_ORIGIN owns OriginalId=canonical spelling/template identity,
; Target=final numeric value identity. Neither link is a dotted binding plan.
	.priv
; A0=checked Entry. CCR Z only for an unindexed template spelling origin;
; all registers preserved. Proxy ScopeKind stores an origin index, not a kind.
templateOriginEntry	.block
	move.l d0, -(sp)
	move.w records.Entry.Flags(a0), d0
	andi.w #records.IMPORT_PROXY+records.BINDING_PROXY, d0
	bne.w done
	cmpi.w #KIND_TEMPLATE_ORIGIN, records.Entry.ScopeKind(a0)
done
	movem.l (sp)+, d0
	rts
	.bend  ; templateOriginEntry
	.pub
; A0=scope,A1=borrowed complete name,D0=bytes,D1=source leaf offset,
; D2=packed register facets. D0/CCR=status,D1=unindexed origin ID,
; D2=canonical value ID. Other registers preserved. At most one origin is
; appended; failures may leave unused names but never declaration/public claims.
bindTemplateOrigin	.block
	movem.l d3-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d4
	move.l d1, d5
	move.l d2, d6
	move.w layout.State.Current(a6), -(sp)
	move.w layout.State.BindingNamespace(a6), -(sp)
	move.w layout.State.FirstBound(a6), -(sp)
	move.w layout.State.FirstExplicit(a6), -(sp)
	.VIEW_ENTER a6
	tst.l d4
	beq.w bad
	cmpi.l #layout.NAME_BYTES-1, d4
	bhi.w bad
	cmp.l d4, d5
	bhs.w bad
	tst.b d6
	bne.w bad
	move.l d6, d0
	swap d0
	andi.l #$ffff, d0
	cmp.w layout.State.Base(a6), d0
	bhi.w bad
	tst.w d0
	bne.w copyName
	tst.l d6
	bne.w bad
copyName
	movea.l VIEWS(a6), a0
	lea ViewFrame.Name0(a0), a0
	move.l d4, d0
copy
	move.b (a5)+, (a0)+
	beq.w bad
	subq.l #1, d0
	bne.w copy
	clr.b (a0)
	clr.w layout.State.Current(a6)
	clr.w layout.State.BindingNamespace(a6)
	movea.l VIEWS(a6), a0
	lea ViewFrame.Name0(a0), a0
	move.l d4, d0
	movea.l a6, a1
	bsr.w bindOrigin
	bne.w bad
	move.l d1, d7
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	.VIEW_READ a6, d0, a2, ViewFrame.Entry2, a6, bad
	moveq #0, d0
	move.w records.Entry.Target(a2), d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	moveq #0, d0
	move.w layout.State.Count(a6), d0
	addq.l #1, d0
	movea.l a6, a0
	bsr.w reserveIdentities
	bne.w bad
	movea.l VIEWS(a6), a1
	lea ViewFrame.Name0(a1), a1
	lea layout.LEXICAL_STATE(a6), a0
	move.l d4, d0
	moveq #0, d1
	move.w layout.State.Count(a6), d1
	jsr lexical.appendName
	bne.w bad
	move.l d1, d3
	movea.l VIEWS(a6), a3
	lea ViewFrame.Entry3+View.Entry(a3), a3
	movea.l a3, a0
	moveq #records.ENTRY_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	move.l d3, records.Entry.Name(a3)
	move.w d4, records.Entry.Length(a3)
	move.w d5, records.Entry.Leaf(a3)
	move.w records.Entry.Target(a2), records.Entry.Target(a3)
	move.w d7, records.Entry.OriginalId(a3)
	move.w #KIND_TEMPLATE_ORIGIN, records.Entry.ScopeKind(a3)
	move.l d6, records.Entry.RegisterName(a3)
	move.w #$ffff, records.Entry.MemberId(a3)
	movea.l a3, a1
	lea layout.LEXICAL_STATE(a6), a0
	moveq #0, d0
	move.w layout.State.Count(a6), d0
	jsr lexical.appendEntry
	bne.w bad
	moveq #0, d1
	move.w layout.State.Count(a6), d1
	add.w layout.State.Base(a6), d1
	addq.w #1, layout.State.Count(a6)
	add.l d4, layout.State.ArenaUsed(a6)
	move.l d7, d2
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
	moveq #0, d2
done
	.VIEW_LEAVE a6
	move.w (sp)+, layout.State.FirstExplicit(a6)
	move.w (sp)+, layout.State.FirstBound(a6)
	move.w (sp)+, layout.State.BindingNamespace(a6)
	move.w (sp)+, layout.State.Current(a6)
	movem.l (sp)+, d3-d7/a0-a6
	tst.l d0
	rts
	.bend  ; bindTemplateOrigin
	.priv
; A0=scope,D0=source origin. D0/CCR=status,D1=canonical template ID.
; Only our explicit metadata kind follows Target in the template namespace;
; ordinary numeric aliases never select another name's independent facet.
; The caller still performs the normal template visibility/import checks.
templateOriginTarget	.block
	movem.l d2/a0-a3/a6, -(sp)
	movea.l a0, a6
	.VIEW_ENTER a6
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	.VIEW_READ a6, d0, a3, ViewFrame.Entry3, a6, bad
	movea.l a3, a0
	bsr.w templateOriginEntry
	bne.w bad
	moveq #0, d1
	move.w records.Entry.OriginalId(a3), d1
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	bcs.w bad
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	.VIEW_READ a6, d0, a3, ViewFrame.Entry3, a6, bad
	movea.l a3, a0
	bsr.w templateOriginEntry
	beq.w bad
	btst #0, records.Entry.TemplateFlags+1(a3)
	beq.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	.VIEW_LEAVE a6
	movem.l (sp)+, d2/a0-a3/a6
	tst.l d0
	rts
	.bend  ; templateOriginTarget
	.pub
