; Object-local imports and the temporary identities required by the old linker.
StoredImportLink	.struct
First	.long ?  ; byte offset into StoredImportGlobals
Count	.long ?
	.endstruct
STORED_IMPORT_LINK_BYTES = StoredImportLink.Count+4

; D1=private consumer ID,D2=root linker ID -> D0/status,D1=import ordinal.
; Other registers kept. Persist provider/entity identity, never the root ID.
privateInterfaceImport	.block
	movem.l d2-d7/a0-a3, -(sp)
	move.l d2, d7
	moveq #moduleProvider.VALUE, d2
	bsr.w privateInterfaceBinding
	bne.w bad
	move.l moduleProvider.Binding.Entity(a1), d6
	move.l moduleProvider.Binding.Provider(a1), d1
	bsr.w privateInterfaceDependency
	bne.w bad
	move.l d1, d5
	moveq #0, d4
	move.l PrivateObjectImports+memory.Block.Used, d3
	movea.l PrivateObjectImports+memory.Block.Pointer, a1
find
	cmp.l d3, d4
	bhs.w append
	cmp.l assemblyInterface.Import.Dependency(a1), d5
	bne.w next
	cmp.l assemblyInterface.Import.Entity(a1), d6
	beq.w found
next
	adda.w #assemblyInterface.IMPORT_BYTES, a1
	addi.l #assemblyInterface.IMPORT_BYTES, d4
	bra.w find
append
	lea PrivateObjectImports, a0
	move.l d3, d0
	addi.l #assemblyInterface.IMPORT_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d3, a1
	move.l d5, assemblyInterface.Import.Dependency(a1)
	move.l d6, assemblyInterface.Import.Entity(a1)
	addi.l #assemblyInterface.IMPORT_BYTES, memory.Block.Used(a0)
	lea PrivateImportGlobals, a0
	move.l memory.Block.Used(a0), d0
	addq.l #4, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l d7, (a1)
	addq.l #4, memory.Block.Used(a0)
found
	move.l d4, d1
	lsr.l #3, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a3
	tst.l d0
	rts
	.bend  ; privateInterfaceImport

; A0=context,D1=module,D2=importordinal -> D0/status,D1=root source identity.
; Other registers preserved. This transient callback does no object I/O.
storedInterfaceImportSource	.block
	movem.l d2-d4/a0-a1, -(sp)
	move.l d1, d0
	mulu.l #STORED_IMPORT_LINK_BYTES, d0
	move.l d0, d3
	addi.l #STORED_IMPORT_LINK_BYTES, d0
	cmp.l StoredImportLinks+memory.Block.Used, d0
	bhi.w bad
	movea.l StoredImportLinks+memory.Block.Pointer, a0
	adda.l d3, a0
	cmp.l StoredImportLink.Count(a0), d2
	bhs.w bad
	move.l d2, d4
	lsl.l #2, d4
	add.l StoredImportLink.First(a0), d4
	move.l d4, d0
	addq.l #4, d0
	cmp.l StoredImportGlobals+memory.Block.Used, d0
	bhi.w bad
	movea.l StoredImportGlobals+memory.Block.Pointer, a1
	move.l 0(a1, d4.l), d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d4/a0-a1
	tst.l d0
	rts
	.bend  ; storedInterfaceImportSource

; Publish one compact source-identity span indexed by registry module ID.
; D0/CCR=status; other registers kept. The map owns no semantic value payloads.
privatePublishImportLinks	.block
	movem.l d1-d6/a0-a2, -(sp)
	move.l PrivateModuleID, d0
	mulu.l #STORED_IMPORT_LINK_BYTES, d0
	cmp.l StoredImportLinks+memory.Block.Used, d0
	bne.w bad
	move.l PrivateObjectImports+memory.Block.Used, d6
	lsr.l #3, d6
	move.l d6, d0
	lsl.l #2, d0
	cmp.l PrivateImportGlobals+memory.Block.Used, d0
	bne.w bad
	move.l StoredImportGlobals+memory.Block.Used, d5
	lea PrivateImportGlobals, a0
	lea StoredImportGlobals, a1
	bsr.w privateAppendBlock
	bne.w bad
	lea StoredImportLinks, a0
	move.l memory.Block.Used(a0), d0
	addi.l #STORED_IMPORT_LINK_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l d5, StoredImportLink.First(a1)
	move.l d6, StoredImportLink.Count(a1)
	addi.l #STORED_IMPORT_LINK_BYTES, memory.Block.Used(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d6/a0-a2
	tst.l d0
	rts
	.bend  ; privatePublishImportLinks

; D1=current private import ordinal -> D0/status,D1=root identity; others kept.
; Carrier generation precedes publishing the registry-indexed bridge.
privateInterfaceImportSource	.block
	movem.l d2/a0, -(sp)
	move.l d1, d2
	lsl.l #2, d2
	move.l d2, d0
	addq.l #4, d0
	cmp.l PrivateImportGlobals+memory.Block.Used, d0
	bhi.w bad
	movea.l PrivateImportGlobals+memory.Block.Pointer, a0
	move.l 0(a0, d2.l), d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2/a0
	tst.l d0
	rts
	.bend  ; privateInterfaceImportSource

; Compiler lifetime callback: detach borrowed lexical state before it is freed.
; D0/CCR=status; other registers preserved. Seed retains numeric references.
privateSealInterfaces	.block
	movem.l a0, -(sp)
	lea StoredPrivate, a0
	jsr moduleProvider.seal
	movem.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; privateSealInterfaces
