; Cross-run publication context. Object-local IDs stay in the immutable OFMI;
; these records retain only spellings and package-dependent reset bytes.
IncrementalPubHeader	.struct
Magic	.long ?
Bytes	.long ?
OwnerCount	.long ?
Sections	.long ?
SectionBytes	.long ?
Reset	.long ?
ResetBytes	.long ?
Dependencies	.long ?
DependencyCount	.long ?
.endstruct
INCREMENTAL_PUB_HEADER_BYTES = IncrementalPubHeader.DependencyCount+4
INCREMENTAL_PUB_MAGIC = $4f464950; OFIP, integrity tag
IncrementalPubDependency	.struct
NameBytes	.long ?
Name	.res PATH_BYTES
.endstruct
INCREMENTAL_PUB_DEP_BYTES = IncrementalPubDependency.Name+PATH_BYTES

; No arguments. D0/CCR=status; other registers preserved. Empty output on
; failure. Fixed-size dependency rows contain length followed by owned spelling.
incrementalSavePublication	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea IncrementalPublication, a0
	clr.l memory.Block.Used(a0)
	move.l PrivateDependencies+memory.Block.Used, d7
	move.l d7, d0
	andi.l #3, d0
	bne.w bad
	lsr.l #2, d7
	cmpi.l #memory.LIMIT/INCREMENTAL_PUB_DEP_BYTES, d7
	bhi.w bad
	move.l d7, d6
	mulu.l #INCREMENTAL_PUB_DEP_BYTES, d6
	addi.l #INCREMENTAL_PUB_HEADER_BYTES+8*PRIVATE_SECTION_BYTES, d6
	bcs.w bad
	move.l PrivateReset+memory.Block.Used, d5
	add.l d5, d6
	bcs.w bad
	addq.l #3, d6
	bcs.w bad
	andi.l #$fffffffc, d6
	cmpi.l #memory.LIMIT, d6
	bhi.w bad
	move.l d6, d0
	jsr memory.reserveExact
	bne.w bad
	movea.l memory.Block.Pointer(a0), a4
	movea.l a4, a1
	move.l d6, d0
	bsr.w privateClear
	move.l #INCREMENTAL_PUB_MAGIC, IncrementalPubHeader.Magic(a4)
	move.l d6, IncrementalPubHeader.Bytes(a4)
	move.l PrivateOwnerCount, IncrementalPubHeader.OwnerCount(a4)
	move.l #INCREMENTAL_PUB_HEADER_BYTES, IncrementalPubHeader.Sections(a4)
	move.l #8*PRIVATE_SECTION_BYTES, IncrementalPubHeader.SectionBytes(a4)
	move.l #INCREMENTAL_PUB_HEADER_BYTES+8*PRIVATE_SECTION_BYTES, IncrementalPubHeader.Reset(a4)
	move.l d5, IncrementalPubHeader.ResetBytes(a4)
	move.l #INCREMENTAL_PUB_HEADER_BYTES+8*PRIVATE_SECTION_BYTES, d0
	add.l d5, d0
	addq.l #3, d0
	andi.l #$fffffffc, d0
	move.l d0, IncrementalPubHeader.Dependencies(a4)
	move.l d7, IncrementalPubHeader.DependencyCount(a4)
	lea PrivateSectionMeta, a0
	lea INCREMENTAL_PUB_HEADER_BYTES(a4), a1
	move.l #8*PRIVATE_SECTION_BYTES, d0
	bsr.w copy
	movea.l PrivateReset+memory.Block.Pointer, a0
	movea.l a4, a1
	adda.l IncrementalPubHeader.Reset(a4), a1
	move.l d5, d0
	bsr.w copy
	movea.l PrivateDependencies+memory.Block.Pointer, a5
	movea.l a4, a3
	adda.l IncrementalPubHeader.Dependencies(a4), a3
	movea.l PrivateGlobalScope, a6
next
	tst.l d7
	beq.w good
	move.l (a5)+, d0
	subq.l #1, d0
	lea layout.LEXICAL_STATE(a6), a0
	lea PrivateEntry, a1
	jsr lexical.readEntry
	bne.w bad
	moveq #0, d5
	move.w records.Entry.Length(a1), d5
	beq.w bad
	cmpi.l #PATH_BYTES, d5
	bhs.w bad
	move.l d5, IncrementalPubDependency.NameBytes(a3)
	move.l d5, d1
	move.l records.Entry.Name(a1), d0
	lea IncrementalPubDependency.Name(a3), a1
	jsr lexical.copyName
	bne.w bad
	adda.l #INCREMENTAL_PUB_DEP_BYTES, a3
	subq.l #1, d7
	bra.w next
good
	move.l d6, IncrementalPublication+memory.Block.Used
	moveq #0, d0
	bra.w done
bad
	clr.l IncrementalPublication+memory.Block.Used
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; incrementalSavePublication

; A0/D0=borrowed blob/extent. PrivateImage has passed image.validate.
; Validate all persisted structure before root/provider publication. Private
; Returns 2 for validation/stale misses before allocation, 1 for failures
; after allocation starts, and 0 for success. Private attempt allocations
; remain releasable by privateReleaseAttempt on failure.
incrementalRestorePublication	.block
	movem.l d1-d7/a0-a6, -(sp)
RESTORE_STARTED = 0
	clr.l -(sp)
	movea.l a0, a6
	move.l a0, d1
	beq.w bad
	btst #0, d1
	bne.w bad
	add.l d0, d1
	bcs.w bad
	cmpi.l #INCREMENTAL_PUB_HEADER_BYTES, d0
	blo.w bad
	cmpi.l #memory.LIMIT, d0
	bhi.w bad
	cmpi.l #INCREMENTAL_PUB_MAGIC, IncrementalPubHeader.Magic(a6)
	bne.w bad
	cmp.l IncrementalPubHeader.Bytes(a6), d0
	bne.w bad
	cmpi.l #INCREMENTAL_PUB_HEADER_BYTES, IncrementalPubHeader.Sections(a6)
	bne.w bad
	cmpi.l #8*PRIVATE_SECTION_BYTES, IncrementalPubHeader.SectionBytes(a6)
	bne.w bad
	cmpi.l #INCREMENTAL_PUB_HEADER_BYTES+8*PRIVATE_SECTION_BYTES, IncrementalPubHeader.Reset(a6)
	bne.w bad
	move.l IncrementalPubHeader.ResetBytes(a6), d1
	addi.l #INCREMENTAL_PUB_HEADER_BYTES+8*PRIVATE_SECTION_BYTES, d1
	bcs.w bad
	addq.l #3, d1
	bcs.w bad
	andi.l #$fffffffc, d1
	cmp.l IncrementalPubHeader.Dependencies(a6), d1
	bne.w bad
	move.l IncrementalPubHeader.DependencyCount(a6), d7
	cmpi.l #memory.LIMIT/INCREMENTAL_PUB_DEP_BYTES, d7
	bhi.w bad
	move.l d7, d2
	mulu.l #INCREMENTAL_PUB_DEP_BYTES, d2
	add.l d2, d1
	bcs.w bad
	cmp.l d0, d1
	bne.w bad
	move.l IncrementalPubHeader.OwnerCount(a6), d6
	beq.w bad
	cmpi.l #memory.LIMIT/PRIVATE_LINK_BYTES, d6
	bhi.w bad
	; All fragment owners must fit the restored wrapper inventory.
	movea.l PrivateImage+memory.Block.Pointer, a4
	move.l moduleImage.Header.BlockCount(a4), d7
	movea.l a4, a3
	adda.l moduleImage.Header.Blocks(a4), a3
blocks
	tst.l d7
	beq.w sections
	cmp.l moduleImage.Block.Owner(a3), d6
	bls.w bad
	move.l moduleImage.Block.Section(a3), d1
	cmpi.l #8, d1
	bhs.w bad
	mulu.l #PRIVATE_SECTION_BYTES, d1
	lea INCREMENTAL_PUB_HEADER_BYTES(a6), a0
	adda.l d1, a0
	move.l moduleImage.Block.Kind(a3), d1
	cmp.l PrivateSection.Kind(a0), d1
	bne.w bad
	adda.w #moduleImage.BLOCK_BYTES, a3
	subq.l #1, d7
	bra.w blocks
sections
	lea INCREMENTAL_PUB_HEADER_BYTES(a6), a3
	moveq #7, d7
section
	move.l PrivateSection.Kind(a3), d0
	beq.w sectionNext
	cmpi.l #moduleImage.KIND_CODE, d0
	blo.w bad
	cmpi.l #moduleImage.KIND_BSS, d0
	bhi.w bad
	move.l PrivateSection.Bytes(a3), d0
	beq.w bad
	cmpi.l #PATH_BYTES, d0
	bhs.w bad
	tst.b 0(a3, d0.l)
	bne.w bad
	move.l PrivateSection.Align(a3), d0
	beq.w bad
	move.l d0, d1
	subq.l #1, d1
	and.l d0, d1
	bne.w bad
sectionNext
	adda.l #PRIVATE_SECTION_BYTES, a3
	dbra d7, section
	movea.l a6, a3
	adda.l IncrementalPubHeader.Dependencies(a6), a3
	move.l IncrementalPubHeader.DependencyCount(a6), d7
validateNames
	tst.l d7
	beq.w validateInterface
	move.l IncrementalPubDependency.NameBytes(a3), d0
	beq.w bad
	cmpi.l #PATH_BYTES, d0
	bhs.w bad
	lea IncrementalPubDependency.Name(a3), a0
	move.l d0, d1
nameByte
	tst.b (a0)+
	beq.w bad
	subq.l #1, d1
	bne.w nameByte
	tst.b (a0)
	bne.w bad
	adda.l #INCREMENTAL_PUB_DEP_BYTES, a3
	subq.l #1, d7
	bra.w validateNames
validateInterface
	movea.l PrivateImage+memory.Block.Pointer, a4
	move.l moduleImage.Header.SemanticsBytes(a4), d0
	beq.w bad
	adda.l moduleImage.Header.Semantics(a4), a4
	movea.l a4, a0
	jsr assemblyInterface.validate
	bne.w bad
	; Require the complete stored dependency directory before materializing any
	; root binding; missing/reordered objects must not leave partial bridges.
	move.l assemblyInterface.Header.DependencyCount(a4), d7
	movea.l a4, a3
	adda.l assemblyInterface.Header.Dependencies(a4), a3
validateProviders
	tst.l d7
	beq.w validateReset
	move.l assemblyInterface.Dependency.NameBytes(a3), d2
	movea.l a4, a1
	adda.l assemblyInterface.Dependency.Name(a3), a1
	lea PrivateRegistry, a0
	jsr moduleRegistry.findObject
	bne.w bad
	adda.w #assemblyInterface.DEPENDENCY_BYTES, a3
	subq.l #1, d7
	bra.w validateProviders
validateReset
	; Reset records are package-dependent: the outer cache identity covers the
	; package/tool/settings; admission checks the bounded record grammar again.
	movea.l a6, a0
	adda.l IncrementalPubHeader.Reset(a6), a0
	move.l IncrementalPubHeader.ResetBytes(a6), d0
	tst.l d0
	beq.w allocate
	; A reset is exactly one CPU directive with an unqualified package CPU name.
	cmpi.l #source.RECORD_HEADER_BYTES+parser_abi.PRVM_PACKED_DOT_BYTES+parser_abi.PRVM_PACKED_NAME_BYTES, d0
	bne.w bad
	moveq #0, d1
	move.b source.RecordHeader.Length(a0), d1
	addq.l #1, d1
	cmp.l d0, d1
	bne.w bad
	moveq #0, d1
	move.b source.RecordHeader.Flags(a0), d1
	andi.l #$ff-source.FLAG_INDENT, d1
	bne.w bad
	adda.w #source.RECORD_HEADER_BYTES, a0
	cmpi.b #parser_abi.PRVM_TOKEN_KIND_DOT, parser_abi.PackedDot.Kind(a0)
	bne.w bad
	cmpi.b #parser_abi.PRVM_TOKEN_KIND_REGISTER, parser_abi.PackedDot.Name+parser_abi.PackedName.Kind(a0)
	bhi.w bad
	tst.b parser_abi.PackedDot.Name+parser_abi.PackedName.Qualifier(a0)
	bne.w bad
	movea.l Front+frontend.Frame.Package, a1
	move.w parser_abi.PackedDot.Name+parser_abi.PackedName.Id(a0), d1
	cmp.w package.Header.CpuDirective(a1), d1
	bne.w bad
	adda.w #parser_abi.PRVM_PACKED_DOT_BYTES, a0
	cmpi.b #parser_abi.PRVM_TOKEN_KIND_REGISTER, parser_abi.PackedName.Kind(a0)
	bhi.w bad
	tst.b parser_abi.PackedName.Qualifier(a0)
	bne.w bad
	move.w parser_abi.PackedName.Id(a0), d1
	cmp.w package.Header.CpuName(a1), d1
	bne.w bad
allocate
	move.l #1, RESTORE_STARTED(sp)
	lea PrivateOwners, a0
	move.l d6, d0
	mulu.l #moduleCarrier.OWNER_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	move.l d6, d0
	mulu.l #moduleCarrier.OWNER_BYTES, d0
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	bsr.w privateClear
	lea PrivateLinksBlock, a0
	move.l d6, d0
	mulu.l #PRIVATE_LINK_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	move.l d6, d0
	mulu.l #PRIVATE_LINK_BYTES, d0
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	bsr.w privateClear
	movea.l PrivateOwners+memory.Block.Pointer, a0
	moveq #0, d1
owners
	move.l d1, moduleCarrier.Owner.Local(a0)
	adda.w #moduleCarrier.OWNER_BYTES, a0
	addq.l #1, d1
	cmp.l d6, d1
	blo.w owners
	move.l d6, PrivateOwnerCount
	lea PrivateReset, a0
	move.l IncrementalPubHeader.ResetBytes(a6), d0
	jsr memory.reserveExact
	bne.w bad
	move.l IncrementalPubHeader.ResetBytes(a6), d0
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	movea.l a6, a0
	adda.l IncrementalPubHeader.Reset(a6), a0
	bsr.w copy
	lea INCREMENTAL_PUB_HEADER_BYTES(a6), a0
	lea PrivateSectionMeta, a1
	move.l #8*PRIVATE_SECTION_BYTES, d0
	bsr.w copy
	lea PrivateDependencies, a0
	move.l IncrementalPubHeader.DependencyCount(a6), d0
	lsl.l #2, d0
	jsr memory.reserveExact
	bne.w bad
	clr.l memory.Block.Used(a0)
	movea.l a6, a3
	adda.l IncrementalPubHeader.Dependencies(a6), a3
	move.l IncrementalPubHeader.DependencyCount(a6), d7
dependencies
	tst.l d7
	beq.w imports
	move.l IncrementalPubDependency.NameBytes(a3), d0
	move.l d0, PrivateDependencyBytes
	lea IncrementalPubDependency.Name(a3), a0
	lea PrivateDependencyName, a1
	bsr.w copy
	bsr.w privateFindGlobalModule
	bne.w bad
	move.l d1, d5
	bsr.w storedInterfaceModuleId
	bne.w bad
	movea.l PrivateDependencies+memory.Block.Pointer, a0
	move.l PrivateDependencies+memory.Block.Used, d0
	move.l d5, 0(a0, d0.l)
	addq.l #4, PrivateDependencies+memory.Block.Used
	adda.l #INCREMENTAL_PUB_DEP_BYTES, a3
	subq.l #1, d7
	bra.w dependencies
imports
	lea PrivateObjectImports, a0
	move.l assemblyInterface.Header.ImportCount(a4), d0
	mulu.l #assemblyInterface.IMPORT_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	clr.l memory.Block.Used(a0)
	lea PrivateImportGlobals, a0
	move.l assemblyInterface.Header.ImportCount(a4), d0
	lsl.l #2, d0
	jsr memory.reserveExact
	bne.w bad
	clr.l memory.Block.Used(a0)
	movea.l a4, a3
	adda.l assemblyInterface.Header.Imports(a4), a3
	move.l assemblyInterface.Header.ImportCount(a4), d7
importNext
	tst.l d7
	beq.w good
	move.l assemblyInterface.Import.Entity(a3), d5
	move.l assemblyInterface.Import.Dependency(a3), d0
	mulu.l #assemblyInterface.DEPENDENCY_BYTES, d0
	add.l assemblyInterface.Header.Dependencies(a4), d0
	movea.l a4, a2
	adda.l d0, a2
	move.l assemblyInterface.Dependency.NameBytes(a2), d2
	movea.l a4, a1
	adda.l assemblyInterface.Dependency.Name(a2), a1
	lea PrivateRegistry, a0
	jsr moduleRegistry.findObject
	bne.w bad
	move.l d5, d2
	bsr.w storedInterfaceRootValue
	bne.w bad
	movea.l PrivateImportGlobals+memory.Block.Pointer, a0
	move.l PrivateImportGlobals+memory.Block.Used, d0
	move.l d1, 0(a0, d0.l)
	addq.l #4, PrivateImportGlobals+memory.Block.Used
	movea.l PrivateObjectImports+memory.Block.Pointer, a0
	adda.l PrivateObjectImports+memory.Block.Used, a0
	move.l assemblyInterface.Import.Dependency(a3), (a0)+
	move.l assemblyInterface.Import.Entity(a3), (a0)
	addq.l #assemblyInterface.IMPORT_BYTES, PrivateObjectImports+memory.Block.Used
	adda.w #assemblyInterface.IMPORT_BYTES, a3
	subq.l #1, d7
	bra.w importNext
good
	moveq #0, d0
	bra.w done
bad
	moveq #2, d0
	tst.l RESTORE_STARTED(sp)
	beq.w done
	moveq #1, d0
done
	addq.l #4, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; incrementalRestorePublication

	.endsection
	.section bss, kind=bss
IncrementalPublication	.res byte, memory.Block.Used+4
	.endsection
	.section code, kind=code
