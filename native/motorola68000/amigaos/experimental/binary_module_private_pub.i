; Capture numeric inventory while the producer still owns lexical names.
privateInventory	.block
	movem.l d1-d7/a0-a6, -(sp)
	bsr.w privateResetRecord
	bne.w bad
	movea.l frontend.Frame.Scratch(a0), a6
	adda.l #frontend.SCOPE_STATE, a6
	movea.l a3, a5
	movea.l memory.Block.Pointer(a1), a0
	move.l blocks.Scratch.Count(a0), d7
	addq.l #1, d7
	move.l d7, PrivateOwnerCount
	move.l d7, d0
	mulu.l #moduleCarrier.OWNER_BYTES, d0
	lea PrivateOwners, a0
	jsr memory.reserveExact
	bne.w bad
	move.l d7, d0
	mulu.l #moduleCarrier.OWNER_BYTES, d0
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	bsr.w privateClear
	move.l d7, d0
	mulu.l #PRIVATE_LINK_BYTES, d0
	lea PrivateLinksBlock, a0
	jsr memory.reserveExact
	bne.w bad
	move.l d7, d0
	mulu.l #PRIVATE_LINK_BYTES, d0
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	bsr.w privateClear
	movea.l PrivateOwners+memory.Block.Pointer, a0
	moveq #0, d0
owners
	move.l d0, moduleCarrier.Owner.Local(a0)
	adda.w #moduleCarrier.OWNER_BYTES, a0
	addq.l #1, d0
	cmp.l d7, d0
	blo.w owners
	lea PrivateSectionMeta, a1
	move.l #8*PRIVATE_SECTION_BYTES, d0
	bsr.w privateClear
	lea scopes.SECTION_STATE(a6), a4
	moveq #0, d7
	move.w sectionPrepare.State.SlotCount(a4), d7
	cmpi.l #8, d7
	bhi.w bad
	moveq #0, d6
section
	cmp.l d7, d6
	bhs.w recordsBegin
	move.l d6, d0
	add.l d0, d0
	moveq #0, d1
	move.w sectionPrepare.SLOT_NAMES(a4, d0.l), d1
	move.l d1, d0
	sub.w layout.State.Base(a6), d0
	andi.l #$ffff, d0
	lea layout.LEXICAL_STATE(a6), a0
	lea PrivateEntry, a1
	jsr lexical.readEntry
	bne.w bad
	move.l d6, d5
	mulu.l #PRIVATE_SECTION_BYTES, d5
	lea PrivateSectionMeta, a2
	adda.l d5, a2
	moveq #0, d1
	move.w records.Entry.Length(a1), d1
	moveq #0, d3
	move.w records.Entry.Leaf(a1), d3
	move.l d1, d0
	sub.l d3, d0
	bls.w bad
	move.l d0, PrivateSection.Bytes(a2)
	move.l #1, PrivateSection.Align(a2)
	move.l records.Entry.Name(a1), d0
	movea.l a2, a1
	jsr lexical.copyName
	bne.w bad
	; Bind the declared spelling globally, without its private module prefix.
	; Explicit dotted section names have Leaf0 and retain their full spelling.
	movea.l a2, a0
	adda.l d3, a0
	move.l PrivateSection.Bytes(a2), d0
sectionName
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w sectionName
	clr.b (a1)
	addq.l #1, d6
	bra.w section
recordsBegin
	movea.l memory.Block.Pointer(a5), a3
	move.l memory.Block.Used(a5), d7
record
	tst.l d7
	beq.w good
	cmpi.l #source.RECORD_HEADER_BYTES, d7
	blo.w bad
	moveq #0, d6
	move.b source.RecordHeader.Length(a3), d6
	addq.l #1, d6
	cmpi.l #source.RECORD_HEADER_BYTES, d6
	blo.w bad
	cmp.l d7, d6
	bhi.w bad
	btst #source.FLAG_LAYOUT_BIT, source.RecordHeader.Flags(a3)
	beq.w advance
	cmpi.l #source.LayoutRecord.Opcode+1, d6
	blo.w bad
	cmpi.b #sections.CONTROL_FIRST_SECTION, source.LayoutRecord.Opcode(a3)
	beq.w sectionControl
	cmpi.b #sections.CONTROL_SECOND_SECTION, source.LayoutRecord.Opcode(a3)
	beq.w sectionControl
	cmpi.b #sections.CONTROL_EXTRA_SECTION, source.LayoutRecord.Opcode(a3)
	bne.w advance
sectionControl
	cmpi.l #source.CONCRETE_SECTION_BYTES, d6
	bne.w bad
	moveq #0, d0
	move.b source.ConcreteSection.Slot(a3), d0
	cmpi.l #8, d0
	bhs.w bad
	mulu.l #PRIVATE_SECTION_BYTES, d0
	lea PrivateSectionMeta, a2
	adda.l d0, a2
	moveq #0, d1
	move.b source.ConcreteSection.Kind(a3), d1
	cmpi.l #moduleImage.KIND_CODE, d1
	blo.w bad
	cmpi.l #moduleImage.KIND_BSS, d1
	bhi.w bad
	move.l source.ConcreteSection.Alignment(a3), d2
	beq.w bad
	move.l d2, d0
	subq.l #1, d0
	and.l d2, d0
	bne.w bad
	tst.l PrivateSection.Kind(a2)
	beq.w firstSection
	cmp.l PrivateSection.Kind(a2), d1
	bne.w bad
	cmp.l PrivateSection.Align(a2), d2
	bne.w bad
	bra.w advance
firstSection
	move.l d1, PrivateSection.Kind(a2)
	move.l d2, PrivateSection.Align(a2)
advance
	adda.l d6, a3
	sub.l d6, d7
	bra.w record
good
	moveq #0, d0
	bra.w done
bad
	moveq #moduleCompile.REPLAY_UNSUPPORTED, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateInventory

; Admission proves an unconditional reset before the first state-dependent
; statement. Preserve that exact numeric record at the publication boundary.
; Inputs are the Inventory callback arguments; D0/CCR=status, others kept.
privateResetRecord	.block
	movem.l d1-d3/a0-a3, -(sp)
	movea.l moduleCompile.Context.Package(a2), a1
	moveq #0, d1
	move.w frontend.Frame.AsciiName(a0), d1
	moveq #0, d2
	move.w frontend.Frame.PetsciiName(a0), d2
	movea.l memory.Block.Pointer(a3), a0
	move.l memory.Block.Used(a3), d0
	jsr moduleAdmission.prepare
	bne.w done
	movea.l a1, a2
	move.l d1, d3
	lea PrivateReset, a0
	move.l d3, d0
	jsr memory.reserveExact
	bne.w done
	move.l d3, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	movea.l a2, a0
	move.l d3, d0
	bsr.w copy
	moveq #0, d0
done
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; privateResetRecord

; Image accepted. All subsequent failure is a real publication/storage error;
; no reference replay may run against partially installed module ownership.
privatePublish	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l PrivateImage+memory.Block.Pointer, a0
	move.l PrivateImage+memory.Block.Used, d0
	jsr moduleImage.semanticView
	bne.w bad
	move.l PrivateGlobalScope, PrivateInstall+moduleScope.Frame.Scope
	move.l a1, PrivateInstall+moduleScope.Frame.Image
	move.l d1, PrivateInstall+moduleScope.Frame.Bytes
	move.l #PrivateModuleName, PrivateInstall+moduleScope.Frame.ModuleName
	move.l PrivateModuleBytes, PrivateInstall+moduleScope.Frame.ModuleNameBytes
	move.l #privateOutgoingAddress, PrivateInstall+moduleScope.Frame.Address
	move.l #privateOutgoingReuse, PrivateInstall+moduleScope.Frame.Reuse
	lea PrivateInstall, a0
	jsr moduleScope.install
	bne.w bad
	move.l PrivateInstall+moduleScope.Frame.Module, PrivateInstalledModule
	bsr.w privateMapSections
	bne.w bad
	movea.l PrivateOwners+memory.Block.Pointer, a4
	move.l PrivateOwnerCount, d7
	moveq #0, d6
owner
	tst.l d7
	beq.w dependencies
	tst.l d6
	beq.w ownerNext
	movea.l PrivateGlobalScope, a0
	move.l PrivateInstalledModule, d0
	jsr moduleScope.allocateOwner
	bne.w bad
	move.l d1, moduleCarrier.Owner.Global(a4)
ownerNext
	adda.w #moduleCarrier.OWNER_BYTES, a4
	addq.l #1, d6
	subq.l #1, d7
	bra.w owner
dependencies
	movea.l PrivateDependencies+memory.Block.Pointer, a4
	move.l PrivateDependencies+memory.Block.Used, d7
nextDependency
	tst.l d7
	beq.w lists
	move.l (a4)+, d1
	subq.l #1, d1
	move.l PrivateInstalledModule, d0
	movea.l PrivateGlobalScope, a0
	jsr imports.seedDependency
	bne.w bad
	subq.l #4, d7
	bra.w nextDependency
lists
	bsr.w privateBuildLists
	bne.w bad
	bsr.w privateRememberValues
	bne.w bad
	lea PrivateImage, a0
	jsr moduleImage.retireSemantics
	bne.w bad
	lea PrivateRegistry, a0
	lea PrivateImage, a1
	jsr moduleRegistry.add
	bne.w bad
	move.l d1, PrivateModuleID
	moveq #1, d2
	jsr moduleRegistry.locate
	bne.w bad
	move.l a1, PrivateCarrier+moduleCarrier.Frame.Image
	move.l moduleImage.Header.Bytes(a1), PrivateCarrier+moduleCarrier.Frame.Bytes
	move.l PrivateModuleID, PrivateCarrier+moduleCarrier.Frame.ModuleID
	move.l PrivateOwners+memory.Block.Pointer, PrivateCarrier+moduleCarrier.Frame.Owners
	move.l PrivateOwnerCount, PrivateCarrier+moduleCarrier.Frame.OwnerCount
	move.l #PrivateSections, PrivateCarrier+moduleCarrier.Frame.Sections
	move.l #PrivateCarrierRecords, PrivateCarrier+moduleCarrier.Frame.Destination
	move.l SourceLine, PrivateCarrier+moduleCarrier.Frame.Line
	lea PrivateReset, a0
	lea PrivateCarrierRecords, a1
	bsr.w privateAppendBlock
	bne.w bad
	lea PrivateCarrier, a0
	jsr moduleCarrier.build
	bne.w bad
	movea.l PrivateAddresses+memory.Block.Pointer, a4
	move.l PrivateAddresses+memory.Block.Used, d7
address
	tst.l d7
	beq.w append
	lea PrivateRegistry, a0
	move.l PrivateModuleID, d1
	move.l PrivateAddress.Fragment(a4), d2
	move.l PrivateAddress.Offset(a4), d3
	move.l PrivateAddress.Global(a4), d4
	jsr moduleRegistry.bindAddress
	bne.w bad
	adda.w #PRIVATE_ADDRESS_BYTES, a4
	subi.l #PRIVATE_ADDRESS_BYTES, d7
	bra.w address
append
	move.l Records+memory.Block.Used, PrivateRecordStart
	bsr.w privateAppendCarrier
	bne.w bad
	lea FileSpans, a0
	move.l memory.Block.Used(a0), d0
	addi.l #SPAN_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l PrivateRecordStart, Span.Start(a1)
	move.l Records+memory.Block.Used, Span.End(a1)
	move.l PublishedCapture+Span.File, Span.File(a1)
	addi.l #SPAN_BYTES, memory.Block.Used(a0)
	addq.l #1, SpanCount
	move.l PrivateRecordStart, LineOffset
	move.l PublishedCapture+Span.File, OriginId
	move.l PublishedCapture+Span.File, SourceOrdinal
	bsr.w appendOrigin
	bne.w bad
	movea.l Front+frontend.Frame.Graph, a6
	move.l PrivateRecordStart, graph.GraphState.Cursor(a6)
	move.l PublishedCapture+Span.File, graph.GraphState.SourceIndex(a6)
	movea.l a6, a0
	moveq #0, d0
	moveq #0, d1
	move.l PrivateInstalledModule, d2
	jsr graph.line
	bne.w bad
	movea.l a6, a0
	move.l PrivateCarrierRecords+memory.Block.Used, d0
	move.l PrivateInstalledModule, d1
	move.l d1, d2
	jsr graph.line
	bne.w bad
	movea.l a6, a0
	moveq #0, d0
	move.l PrivateInstalledModule, d1
	moveq #0, d2
	jsr graph.line
	bne.w bad
	movea.l PrivateGlobalScope, a0
	jsr scopes.count
	move.l d0, Front+frontend.Frame.NameCount
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privatePublish

privateOutgoingAddress	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l d0, d5
	move.l d1, d6
	move.l d2, d7
	move.l d3, d4
	lea PrivateAddresses, a0
	move.l memory.Block.Used(a0), d0
	addi.l #PRIVATE_ADDRESS_BYTES, d0
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l d5, PrivateAddress.Local(a1)
	move.l d6, PrivateAddress.Global(a1)
	move.l d4, PrivateAddress.Fragment(a1)
	move.l d7, PrivateAddress.Offset(a1)
	addi.l #PRIVATE_ADDRESS_BYTES, memory.Block.Used(a0)
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateOutgoingAddress
privateOutgoingReuse	.block
	movem.l d2-d4/a0-a2, -(sp)
	move.l PrivateIncoming+memory.Block.Used, d2
	movea.l PrivateIncoming+memory.Block.Pointer, a1
next
	tst.l d2
	beq.w missing
	cmpi.l #moduleScope.VALUE, moduleScope.MapRow.Kind(a1)
	bne.w advance
	cmp.l moduleScope.MapRow.Global(a1), d1
	bne.w advance
	move.l moduleScope.MapRow.Local(a1), d1
	moveq #0, d0
	bra.w done
advance
	adda.w #moduleScope.MAP_BYTES, a1
	subi.l #moduleScope.MAP_BYTES, d2
	bra.w next
missing
	moveq #moduleScope.NOT_FOUND, d0
done
	movem.l (sp)+, d2-d4/a0-a2
	tst.l d0
	rts
	.bend  ; privateOutgoingReuse

privateMapSections	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l PrivateGlobalScope, a6
	lea PrivateSections, a4
	lea PrivateSectionMeta, a5
	moveq #7, d7
next
	clr.l moduleCarrier.Section.Slot(a4)
	move.l #1, moduleCarrier.Section.Alignment(a4)
	tst.l PrivateSection.Kind(a5)
	beq.w advance
	clr.w layout.State.Current(a6)
	movea.l a5, a0
	move.l PrivateSection.Bytes(a5), d0
	movea.l a6, a1
	jsr scopes.bindOrigin
	bne.w bad
	movea.l a6, a0
	lea scopes.SECTION_STATE(a6), a1
	jsr sectionPrepare.bindCompiledSlot
	bne.w bad
	move.l d1, moduleCarrier.Section.Slot(a4)
	move.l PrivateSection.Align(a5), moduleCarrier.Section.Alignment(a4)
advance
	adda.w #moduleCarrier.SECTION_BYTES, a4
	adda.w #PRIVATE_SECTION_BYTES, a5
	dbra d7, next
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateMapSections

; Link/public lists belong to whole owners, including unconditional owner0.
privateBuildLists	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l PrivateImage+memory.Block.Pointer, a6
	move.l moduleImage.Header.RelocCount(a6), d7
	movea.l a6, a4
	adda.l moduleImage.Header.Relocs(a6), a4
reloc
	tst.l d7
	beq.w publicBegin
	movea.l a6, a0
	move.l moduleImage.Reloc.Block(a4), d1
	jsr moduleImage.descriptorValidated
	bne.w bad
	move.l moduleImage.Block.Owner(a1), d6
	move.l moduleImage.Reloc.Target(a4), d1
	bmi.w imported
	movea.l a6, a0
	jsr moduleImage.descriptorValidated
	bne.w bad
	move.l moduleImage.Block.Owner(a1), d0
	beq.w relocNext
	mulu.l #moduleCarrier.OWNER_BYTES, d0
	movea.l PrivateOwners+memory.Block.Pointer, a0
	move.l moduleCarrier.Owner.Global(a0, d0.l), d1
	bra.w dependency
imported
	andi.l #$7fffffff, d1
dependency
	move.l d6, d0
	mulu.l #PRIVATE_LINK_BYTES, d0
	movea.l PrivateLinksBlock+memory.Block.Pointer, a0
	adda.l d0, a0
	lea PrivateLinks.Refs(a0), a0
	bsr.w privateAppendId
	bne.w bad
relocNext
	adda.w #moduleImage.RELOC_BYTES, a4
	subq.l #1, d7
	bra.w reloc
publicBegin
	movea.l PrivateAddresses+memory.Block.Pointer, a4
	move.l PrivateAddresses+memory.Block.Used, d7
public
	tst.l d7
	beq.w pointers
	movea.l a6, a0
	move.l PrivateAddress.Fragment(a4), d1
	jsr moduleImage.descriptorValidated
	bne.w bad
	move.l moduleImage.Block.Owner(a1), d0
	mulu.l #PRIVATE_LINK_BYTES, d0
	movea.l PrivateLinksBlock+memory.Block.Pointer, a0
	adda.l d0, a0
	lea PrivateLinks.Public(a0), a0
	move.l PrivateAddress.Global(a4), d1
	bsr.w privateAppendId
	bne.w bad
	adda.w #PRIVATE_ADDRESS_BYTES, a4
	subi.l #PRIVATE_ADDRESS_BYTES, d7
	bra.w public
pointers
	move.l PrivateOwnerCount, d7
	movea.l PrivateLinksBlock+memory.Block.Pointer, a0
	movea.l PrivateOwners+memory.Block.Pointer, a1
pointer
	tst.l d7
	beq.w good
	move.l PrivateLinks.Refs+memory.Block.Pointer(a0), moduleCarrier.Owner.Refs(a1)
	move.l PrivateLinks.Refs+memory.Block.Used(a0), d0
	lsr.l #2, d0
	move.l d0, moduleCarrier.Owner.RefCount(a1)
	move.l PrivateLinks.Public+memory.Block.Pointer(a0), moduleCarrier.Owner.PublicIDs(a1)
	move.l PrivateLinks.Public+memory.Block.Used(a0), d0
	lsr.l #2, d0
	move.l d0, moduleCarrier.Owner.PublicCount(a1)
	adda.w #PRIVATE_LINK_BYTES, a0
	adda.w #moduleCarrier.OWNER_BYTES, a1
	subq.l #1, d7
	bra.w pointer
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateBuildLists
privateAppendId	.block
	movem.l d1-d4/a0-a2, -(sp)
	move.l memory.Block.Used(a0), d3
	movea.l memory.Block.Pointer(a0), a1
	move.l d3, d2
next
	tst.l d2
	beq.w append
	cmp.l (a1)+, d1
	beq.w good
	subq.l #4, d2
	bra.w next
append
	move.l d3, d0
	addq.l #4, d0
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a1
	move.l d1, 0(a1, d3.l)
	addq.l #4, memory.Block.Used(a0)
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d4/a0-a2
	tst.l d0
	rts
	.bend  ; privateAppendId
privateClear	.block
	movem.l d0/a1, -(sp)
	tst.l d0
	beq.w done
next
	clr.b (a1)+
	subq.l #1, d0
	bne.w next
done
	movem.l (sp)+, d0/a1
	rts
	.bend  ; privateClear

privateReleaseAttempt	.block
	movem.l d0-d3/a0-a2, -(sp)
	movea.l PrivateLinksBlock+memory.Block.Pointer, a2
	move.l PrivateLinksBlock+memory.Block.Used, d3
links
	tst.l d3
	beq.w blocks
	lea PrivateLinks.Refs(a2), a0
	jsr memory.release
	lea PrivateLinks.Public(a2), a0
	jsr memory.release
	adda.w #PRIVATE_LINK_BYTES, a2
	subi.l #PRIVATE_LINK_BYTES, d3
	bra.w links
blocks
	lea PrivateImage, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateSnapshot, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateInstall+moduleScope.Frame.Map, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateSeen, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateIncoming, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateDependencies, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateOwners, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateLinksBlock, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateAddresses, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateCarrierRecords, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateReset, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateFileSpans, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateOrigins, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movem.l (sp)+, d0-d3/a0-a2
	rts
	.bend  ; privateReleaseAttempt

; Retain new value IDs only. Reused dependency schemas already belong to their
; earlier canonical source/module and must not be defined twice at execution.
privateRememberValues	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l PrivateInstall+moduleScope.Frame.Map+memory.Block.Pointer, a4
	move.l PrivateInstall+moduleScope.Frame.Map+memory.Block.Used, d7
next
	tst.l d7
	beq.w good
	cmpi.l #moduleScope.VALUE, moduleScope.MapRow.Kind(a4)
	bne.w advance
	move.l moduleScope.MapRow.Local(a4), d1
	bsr.w privateOutgoingReuse
	tst.l d0
	beq.w advance
	lea PrivatePublishedValues, a0
	move.l memory.Block.Used(a0), d6
	move.l d6, d0
	addi.l #moduleScope.MAP_BYTES, d0
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a1
	adda.l d6, a1
	move.l moduleScope.MapRow.Local(a4), moduleScope.MapRow.Local(a1)
	move.l moduleScope.MapRow.Global(a4), moduleScope.MapRow.Global(a1)
	move.l #moduleScope.VALUE, moduleScope.MapRow.Kind(a1)
	addi.l #moduleScope.MAP_BYTES, memory.Block.Used(a0)
advance
	adda.w #moduleScope.MAP_BYTES, a4
	subi.l #moduleScope.MAP_BYTES, d7
	bra.w next
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateRememberValues

; Publish accepted carrier records into the selected global prepared owner.
; Disk append consumes whole packed records and keeps logical numeric offsets;
; tentative replay never reaches this routine. D0/CCR=status; others preserved.
privateAppendCarrier	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l CaptureDirectory
	bne.w stored
	lea PrivateCarrierRecords, a0
	lea Records, a1
	bsr.w privateAppendBlock
	bra.w done
stored
	movea.l PrivateCarrierRecords+memory.Block.Pointer, a4
	move.l PrivateCarrierRecords+memory.Block.Used, d7
next
	tst.l d7
	beq.w good
	cmpi.l #source.RECORD_HEADER_BYTES, d7
	blo.w bad
	moveq #0, d6
	move.b source.RecordHeader.Length(a4), d6
	addq.l #1, d6
	cmpi.l #source.RECORD_HEADER_BYTES, d6
	blo.w bad
	cmp.l d7, d6
	bhi.w bad
	lea PreparedStore, a0
	movea.l a4, a1
	move.l d6, d0
	jsr prepared_store.append
	bne.w bad
	move.l prepared_store.State.LogicalUsed(a0), Records+memory.Block.Used
	adda.l d6, a4
	sub.l d6, d7
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
	.bend  ; privateAppendCarrier
