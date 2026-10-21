; Incoming values are resolved through the retained private interface.
privateSeed	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea StoredPrivate, a0
	jsr moduleProvider.seed
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; privateSeed

; Seal the accepted root interface before frontend.finish releases the session.
privateRetainKnown	.block
	movem.l a0, -(sp)
	lea StoredRoot, a0
	jsr moduleProvider.seal
	movem.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; privateRetainKnown

privateGlobalSeed	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea StoredRoot, a0
	jsr moduleProvider.seed
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; privateGlobalSeed

; Imported scalar labels retain an object-local import ordinal plus addend.
; Root symbol identities remain only in the transient Hunk/reachability bridge.
privateExternal	.block
	movem.l d3-d7/a0-a6, -(sp)
	move.l d1, d6
	tst.l d3
	bmi.w bad
	move.l PrivateIncoming+memory.Block.Used, d7
	movea.l PrivateIncoming+memory.Block.Pointer, a0
next
	tst.l d7
	beq.w bad
	cmpi.l #moduleScope.VALUE, moduleScope.MapRow.Kind(a0)
	bne.w advance
	cmp.l moduleScope.MapRow.Global(a0), d1
	bne.w advance
	move.l moduleScope.MapRow.Local(a0), d2
	move.l d6, d1
	bsr.w privateInterfaceImport
	bne.w bad
	ori.l #$80000000, d1
	move.l d3, d2
	moveq #0, d0
	bra.w done
advance
	adda.w #moduleScope.MAP_BYTES, a0
	subi.l #moduleScope.MAP_BYTES, d7
	bra.w next
bad
	moveq #1, d0
done
	movem.l (sp)+, d3-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateExternal
