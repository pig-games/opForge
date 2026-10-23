; Sparse consumer address identities bridge stored entities to the current
; linker's numeric cells. They do not publish whole provider symbol tables.

; Transfer the interface window between idle root/private readers. Neither
; caller retains a lease across this operation. D0/CCR=status; others kept.
storedInterfaceSwapCache	.block
	movem.l a0-a1, -(sp)
	lea StoredRoot+moduleProvider.State.Reader+moduleInterfaceStore.State.Window, a0
	lea StoredPrivate+moduleProvider.State.Reader+moduleInterfaceStore.State.Window, a1
	bsr.w swapModuleGraphs
	clr.l memory.Block.Used(a0)
	clr.l memory.Block.Used(a1)
	moveq #0, d0
	movem.l (sp)+, a0-a1
	rts
	.bend  ; storedInterfaceSwapCache

; D1=stored provider,D2=entity -> D0/status,D1=canonical root binding.
; First use binds just this public spelling or one hidden closure identity.
; Caller owns the shared reader window; other registers preserved.
storedInterfaceRootValue	.block
	movem.l d2-d7/a0-a6, -(sp)
	move.l d1, d6
	move.l d2, d7
	lea StoredRoot, a0
	moveq #moduleProvider.VALUE, d3
	jsr moduleProvider.lookup
	tst.l d0
	beq.w done
	lea StoredRoot+moduleProvider.State.Reader, a0
	lea PrivateRegistry, a1
	move.l d6, d1
	jsr moduleInterfaceStore.select
	bne.w bad
	move.l d7, d1
	jsr moduleInterfaceStore.valueRow
	bne.w bad
	move.l moduleInterface.Export.NameBytes(a1), d3
	beq.w hidden
	move.l moduleInterface.Export.Name(a1), d2
	jsr moduleInterfaceStore.valueRead
	bne.w bad
	movea.l a1, a0
	move.l d3, d0
	movea.l StoredRoot+moduleProvider.State.Scope, a1
	move.w layout.State.Current(a1), -(sp)
	clr.w layout.State.Current(a1)
	jsr scopes.bindOrigin
	movea.l StoredRoot+moduleProvider.State.Scope, a1
	move.w (sp)+, layout.State.Current(a1)
	tst.l d0
	bne.w bad
	bra.w demand
hidden
	move.l StoredRoot+moduleProvider.State.Modules+memory.Block.Used, d3
	movea.l StoredRoot+moduleProvider.State.Modules+memory.Block.Pointer, a1
findModule
	tst.l d3
	beq.w bad
	cmp.l moduleProvider.Module.Stored(a1), d6
	beq.w allocate
	adda.w #moduleProvider.MODULE_BYTES, a1
	subi.l #moduleProvider.MODULE_BYTES, d3
	bra.w findModule
allocate
	move.l moduleProvider.Module.Local(a1), d0
	movea.l StoredRoot+moduleProvider.State.Scope, a0
	jsr moduleScope.allocateEntity
	bne.w bad
demand
	move.l d1, d3
	move.l d6, d1
	move.l d7, d2
	lea StoredRoot, a0
	jsr moduleProvider.acquire
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; storedInterfaceRootValue

; A0=StoredRoot/StoredPrivate,D0=provider,D1=entity,D2=consumer,D3=fragment,
; D4=offset -> D0/status,D5=consumer whole-block bridge. Other registers kept.
; Root registers a real linker cell; private consumers retain an external recipe
; and must never interpret a root wrapper as their own block span.
storedInterfaceAddress	.block
	movem.l d1-d4/d6-d7/a0-a6, -(sp)
	movea.l a0, a5
	move.l d0, d6
	move.l d2, d7
	cmpa.l #StoredRoot, a5
	bne.w private
	lea PrivateRegistry, a0
	move.l d6, d1
	move.l d3, d2
	move.l d4, d3
	move.l d7, d4
	jsr moduleRegistry.bindAddress
	bne.w bad
	jsr moduleRegistry.owner
	bne.w bad
	move.l d1, d5
	moveq #0, d0
	bra.w done
private
	move.l d1, d2
	move.l d6, d1
	bsr.w storedInterfaceSwapCache
	bsr.w storedInterfaceRootValue
	move.l d0, -(sp)
	bsr.w storedInterfaceSwapCache
	move.l (sp)+, d0
	tst.l d0
	bne.w bad
	move.l d1, d6
	move.l PrivateIncoming+memory.Block.Used, d3
	movea.l PrivateIncoming+memory.Block.Pointer, a1
find
	tst.l d3
	beq.w append
	cmp.l moduleScope.MapRow.Global(a1), d7
	bne.w next
	cmp.l moduleScope.MapRow.Local(a1), d6
	bne.w bad
	bra.w privateDone
next
	adda.w #moduleScope.MAP_BYTES, a1
	subi.l #moduleScope.MAP_BYTES, d3
	bra.w find
append
	lea PrivateIncoming, a0
	move.l memory.Block.Used(a0), d0
	addi.l #moduleScope.MAP_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l d6, moduleScope.MapRow.Local(a1)
	move.l d7, moduleScope.MapRow.Global(a1)
	move.l #moduleScope.VALUE, moduleScope.MapRow.Kind(a1)
	addi.l #moduleScope.MAP_BYTES, memory.Block.Used(a0)
privateDone
	moveq #0, d5
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/d6-d7/a0-a6
	tst.l d0
	rts
	.bend  ; storedInterfaceAddress
