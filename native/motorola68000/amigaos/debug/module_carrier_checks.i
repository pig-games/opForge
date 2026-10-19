; Focused native numeric carrier/whole-block liveness component.
; Included by the isolated geometry probe; it owns no production observers.
carrierChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	clr.l CarrierBuild+image.Frame.Output
	lea CarrierBuild, a0
	jsr image.build
	bne.w bad
	move.l CarrierBuild+image.Frame.Used, d0
	lea CarrierImage, a0
	jsr memory.reserveExact
	bne.w bad
	move.l CarrierBuild+image.Frame.Used, CarrierImage+memory.Block.Used
	move.l CarrierImage+memory.Block.Pointer, CarrierBuild+image.Frame.Output
	move.l CarrierImage+memory.Block.Capacity, CarrierBuild+image.Frame.Capacity
	lea CarrierBuild, a0
	jsr image.build
	bne.w bad
	move.l CarrierImage+memory.Block.Pointer, CarrierRequest+carrier.Frame.Image
	move.l CarrierImage+memory.Block.Used, CarrierRequest+carrier.Frame.Bytes
	lea CarrierRequest, a0
	jsr carrier.build
	bne.w bad
	lea CarrierExpectedCounts, a1
	bsr.w carrierFraming
	bne.w bad
	movea.l CarrierStream+memory.Block.Pointer, a0
	move.l CarrierStream+memory.Block.Used, d0
	lea CarrierIndex, a1
	jsr blockIndex.index
	bne.w bad
	cmpi.l #2, d1
	bne.w bad
	movea.l CarrierIndex+memory.Block.Pointer, a1
	lea CarrierScopeState, a2
	move.w #$3000, carrierScope.State.Base(a2)
	move.w #32, carrierScope.State.Count(a2)
	move.l #32, carrierScope.LEXICAL_STATE+carrierLexical.State.Count(a2)
	move.l #CarrierModuleOwners, carrierScope.MODULE_STATE+carrierModules.OWNERS_POINTER(a2)
	move.l #CarrierKnownDefined, carrierScope.IMPORT_STATE+carrierImports.KNOWN_DEFINED_POINTER(a2)
	move.l #32, carrierScope.IMPORT_STATE+carrierImports.KNOWN_DEFINED+memory.Block.Capacity(a2)
	move.l #32, carrierScope.IMPORT_STATE+carrierImports.KNOWN_DEFINED+memory.Block.Used(a2)
	move.l #$3020, carrierScope.IMPORT_STATE+carrierImports.KNOWN_OWNER+carrierValues.Owner.Count(a2)
	move.w #2, CarrierModuleOwners
	move.w #2, CarrierModuleOwners+16*2
	move.w #1, CarrierGraphState+carrierGraph.GraphState.Count
	move.w #2, CarrierGraphState+carrierGraph.NODES+carrierGraph.Node.Binding
	move.w #2, CarrierGraphState+carrierGraph.NODES+carrierGraph.Node.File
	move.w #1, CarrierGraphState+carrierGraph.HASH+2*2
	lea CarrierGraphState, a3
	suba.l a4, a4  ; no binding plans/guards require package grammar
	movea.l CarrierStream+memory.Block.Pointer, a0
	move.l CarrierStream+memory.Block.Used, d0
	moveq #2, d1
	jsr blockIndex.select
	bne.w bad
	movea.l CarrierIndex+memory.Block.Pointer, a1
	tst.w blockIndex.HEADER_BYTES+blockIndex.Span.Live(a1)
	beq.w bad
	tst.w blockIndex.HEADER_BYTES+blockIndex.SPAN_BYTES+blockIndex.Span.Live(a1)
	beq.w bad
	; Fresh zero Live decisions omit both hidden wrappers and their fragments.
	movea.l CarrierStream+memory.Block.Pointer, a0
	move.l CarrierStream+memory.Block.Used, d0
	lea CarrierIndex, a1
	jsr blockIndex.index
	bne.w bad
	movea.l CarrierStream+memory.Block.Pointer, a0
	move.l CarrierStream+memory.Block.Used, d0
	movea.l CarrierIndex+memory.Block.Pointer, a1
	moveq #2, d1
	jsr blockIndex.applySelection
	bne.w bad
	bsr.w carrierDeadFlags
	bne.w bad
	; Reuse the builder after successful retirement, with a changed owner run.
	; Owner1 now spans code and data; the old two-wrapper stream must disappear.
	movea.l CarrierImage+memory.Block.Pointer, a1
	adda.l image.Header.Blocks(a1), a1
	move.l #1, 3*image.BLOCK_BYTES+image.Block.Owner(a1)
	clr.l CarrierStream+memory.Block.Used
	lea CarrierRequest, a0
	jsr carrier.build
	bne.w bad
	lea CarrierRepeatCounts, a1
	bsr.w carrierFraming
	bne.w bad
	; A recurrence would split owner1 around owner2. No append may escape.
	movea.l CarrierImage+memory.Block.Pointer, a1
	adda.l image.Header.Blocks(a1), a1
	move.l #2, 3*image.BLOCK_BYTES+image.Block.Owner(a1)
	move.l #1, 4*image.BLOCK_BYTES+image.Block.Owner(a1)
	move.l CarrierStream+memory.Block.Used, d7
	lea CarrierRequest, a0
	jsr carrier.build
	tst.l d0
	beq.w bad
	cmp.l CarrierStream+memory.Block.Used, d7
	bne.w bad
	moveq #0, d7
	bra.w cleanup
bad
	moveq #1, d7
cleanup
	lea CarrierIndex, a0
	jsr memory.release
	lea CarrierStream, a0
	jsr memory.release
	lea CarrierImage, a0
	jsr memory.release
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; carrierChecks

carrierFraming	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l a1, d7  ; expected counts for this complete stream
	lea CarrierCounters, a0
	moveq #6, d0
clear
	clr.l (a0)+
	dbra d0, clear
	movea.l CarrierStream+memory.Block.Pointer, a4
	movea.l a4, a6
	adda.l CarrierStream+memory.Block.Used, a6
next
	cmpa.l a6, a4
	beq.w complete
	bhi.w bad
	moveq #0, d6
	move.b source.RecordHeader.Length(a4), d6
	addq.l #1, d6
	move.l a6, d0
	sub.l a4, d0
	cmp.l d0, d6
	bhi.w bad
	move.b source.RecordHeader.Flags(a4), d0
	btst #source.FLAG_BLOCK_OPEN_BIT, d0
	beq.w closing
	addq.l #1, CarrierCounters
closing
	btst #source.FLAG_BLOCK_CLOSE_BIT, d0
	beq.w layout
	addq.l #1, CarrierCounters+4
layout
	btst #source.FLAG_LAYOUT_BIT, d0
	beq.w advance
	moveq #0, d0
	move.b source.LayoutRecord.Opcode(a4), d0
	cmpi.b #sections.CONTROL_EXTRA_SECTION, d0
	beq.w sectionOpen
	cmpi.b #sections.CONTROL_END_SECTION, d0
	beq.w sectionClose
	cmpi.b #records.OP_FRAGMENT, d0
	beq.w fragment
	cmpi.b #records.OP_REFERENCES, d0
	beq.w references
	cmpi.b #records.OP_OWNERS, d0
	bne.w bad
	addq.l #1, CarrierCounters+24
	movea.l a4, a0
	move.l d6, d0
	jsr records.ownersView
	bne.w bad
	bra.w advance
sectionOpen
	addq.l #1, CarrierCounters+8
	bra.w advance
sectionClose
	addq.l #1, CarrierCounters+12
	bra.w advance
fragment
	addq.l #1, CarrierCounters+16
	movea.l a4, a0
	move.l d6, d0
	jsr records.fragmentView
	bne.w bad
	cmpi.l #7, d1
	bne.w bad
	bra.w advance
references
	addq.l #1, CarrierCounters+20
	movea.l a4, a0
	move.l d6, d0
	jsr records.referencesView
	bne.w bad
advance
	adda.l d6, a4
	bra.w next
complete
	lea CarrierCounters, a0
	movea.l d7, a1
	moveq #6, d0
compare
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.w bad
	dbra d0, compare
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; carrierFraming

carrierDeadFlags	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l CarrierStream+memory.Block.Pointer, a4
	movea.l a4, a6
	adda.l CarrierStream+memory.Block.Used, a6
next
	cmpa.l a6, a4
	beq.w good
	moveq #0, d6
	move.b source.RecordHeader.Length(a4), d6
	addq.l #1, d6
	cmpi.b #records.OP_FRAGMENT, source.LayoutRecord.Opcode(a4)
	bne.w advance
	moveq #0, d1
	move.w records.Fragment.Id(a4), d1
	move.b source.RecordHeader.Flags(a4), d0
	andi.b #source.FLAG_OMIT, d0
	cmpi.l #1, d1
	beq.w mandatory
	cmpi.l #5, d1
	beq.w mandatory
	tst.b d0
	beq.w bad
	bra.w advance
mandatory
	tst.b d0
	bne.w bad
advance
	adda.l d6, a4
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; carrierDeadFlags
	.endsection
	.section data, kind=data
	.priv
	.align 4
CarrierInputs
	.long 1, CarrierPayload, 1, 0, 0, image.KIND_CODE, 1, 1
	.long 2, CarrierPayload+1, 2, 1, 0, image.KIND_CODE, 4, 2
	.long 3, CarrierPayload+3, 1, 1, 0, image.KIND_CODE, 1, 1
	.long 4, CarrierPayload+4, 1, 2, 1, image.KIND_DATA, 1, 1
	.long 5, 0, 0, 0, 2, image.KIND_BSS, 8, 7
CarrierBuild
	.long CarrierInputs, 5, 0, 0, 0, 0, 0, 0, 0
CarrierRows
	.long 0, 0, CarrierRootRefs, 1, 0, 0
	.long 1, $3000, CarrierFirstRefs, 1, CarrierFirstPublic, 2
	.long 2, $3010, 0, 0, CarrierSecondPublic, 1
CarrierRootRefs	.long $3001
CarrierFirstRefs	.long $3011
CarrierFirstPublic	.long $3001, $3002
CarrierSecondPublic	.long $3011
CarrierSections
	.long 2, 4, 4, 2, 6, 8, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1
CarrierRequest
	.long 0, 0, 7, CarrierRows, 3, CarrierSections, CarrierStream, 1
CarrierExpectedCounts	.long 2, 2, 3, 3, 5, 3, 2
CarrierRepeatCounts	.long 1, 1, 3, 3, 5, 3, 1
CarrierPayload	.byte $11, $22, $23, $24, $33
	.endsection
	.section bss, kind=bss
	.priv
	.align 4
CarrierImage	.res byte, memory.Block.Used+4
CarrierStream	.res byte, memory.Block.Used+4
CarrierIndex	.res byte, memory.Block.Used+4
CarrierCounters	.res long, 7
CarrierScopeState	.res byte, carrierScopes.SCRATCH_BYTES
CarrierGraphState	.res byte, carrierGraph.SCRATCH_BYTES
CarrierModuleOwners	.res word, 32
CarrierKnownDefined	.res byte, 32
	.endsection
	.section code, kind=code
	.priv
