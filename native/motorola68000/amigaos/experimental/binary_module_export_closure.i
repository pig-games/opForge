; Retain immutable value roots referenced by stored binary template closures.
; The public spelling index is unchanged: new rows have no public name.
	.pub
; A0=prepared exports.State,A1=completed frontend,A2=OFTM,D0=exact bytes.
; D0/CCR=status; other registers kept. Called before producer lexical retirement.
; Appends only missing referenced value roots. Assembly supplies their final
; typed contents and ordinary publication closes nominal/member dependencies.
; Partial failure leaves the unpublished State owned by its normal cleanup.
prepareTemplateClosure	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #records.ENTRY_BYTES, sp
	movea.l a0, a5
	movea.l a1, a6
	movea.l a2, a4
	cmpi.l #1, State.Ready(a5)
	bne.w bad
	tst.l State.Consumed(a5)
	bne.w bad
	movea.l a2, a0
	jsr moduleTemplates.validate
	bne.w bad
	movea.l frontend.Frame.Scratch(a6), a6
	adda.l #frontend.SCOPE_STATE, a6
	move.l moduleTemplates.Header.BindingCount(a4), d7
	adda.l moduleTemplates.Header.Bindings(a4), a4
nextRoot
	tst.l d7
	beq.w good
	move.l templateNames.Row.Facets(a4), d0
	andi.l #templateNames.VALUE_FACET, d0
	beq.w advance
	tst.l templateNames.Row.ValueDependency(a4)
	bne.w advance  ; dependency directory owns this foreign entity
	move.l templateNames.Row.ValueEntity(a4), d6
	cmp.l State.Base(a5), d6
	blo.w advance  ; package identities retain package-owned meaning
	move.l d6, d0
	sub.l State.Base(a5), d0
	cmp.l State.Count(a5), d0
	bhs.w bad
	move.l State.ExportCount(a5), d5
	movea.l State.Exports+memory.Block.Pointer(a5), a3
findRoot
	tst.l d5
	beq.w appendRoot
	cmp.l interface.Export.Id(a3), d6
	beq.w advance
	adda.w #interface.EXPORT_BYTES, a3
	subq.l #1, d5
	bra.w findRoot
appendRoot
	move.l d6, d0
	sub.l State.Base(a5), d0
	lea layout.LEXICAL_STATE(a6), a0
	movea.l sp, a1
	jsr lexical.readEntry
	bne.w bad
	move.w records.Entry.Flags(a1), d0
	andi.w #scopes.DECLARED, d0
	beq.w bad
	move.w records.Entry.Flags(a1), d0
	andi.w #records.MUTABLE+records.CONDITIONAL_VALUE+records.IMPORT_PROXY+records.BINDING_PROXY, d0
	bne.w bad
	moveq #0, d0
	move.w records.Entry.Target(a1), d0
	cmp.l d6, d0
	bne.w bad
	lea State.Exports(a5), a0
	move.l memory.Block.Used(a0), d0
	addi.l #interface.EXPORT_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l d6, interface.Export.Id(a1)
	clr.l interface.Export.Name(a1)
	clr.l interface.Export.NameBytes(a1)
	move.l d6, interface.Export.Kind(a1)  ; runtime target until publication
	clr.l interface.Export.Low(a1)
	clr.l interface.Export.High(a1)
	clr.l interface.Export.Provenance(a1)
	addi.l #interface.EXPORT_BYTES, memory.Block.Used(a0)
	addq.l #1, State.ExportCount(a5)
advance
	adda.w #templateNames.ROW_BYTES, a4
	subq.l #1, d7
	bra.w nextRoot
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	adda.w #records.ENTRY_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prepareTemplateClosure
	.priv
