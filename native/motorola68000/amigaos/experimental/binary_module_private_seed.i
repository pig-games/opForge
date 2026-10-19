; Incoming values keep one canonical semantic owner until numeric retirement.
privateSeed	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l PrivateGlobalScope, a2
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER(a2), a3
	move.l a3, PrivateSeedSourceOwner
	move.l layout.IMPORT_STATE+imports.KNOWN_VALUES_POINTER(a2), PrivateSeedSourceValues
	move.l layout.IMPORT_STATE+imports.KNOWN_DEFINED_POINTER(a2), PrivateSeedSourceDefined
	moveq #0, d0
	move.w layout.State.Base(a2), d0
	move.l d0, PrivateSeedSourceBase
	move.l #privateSeedResolve, PrivateResolver+moduleRemap.Frame.Resolve
	clr.l PrivateSeedGlobal
	lea PrivateIncoming, a1
	bsr.w privateSeedMapped
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateSeed

; Transfer, never clone, canonical snapshots before frontend.finish releases
; the lexical session. Only privately published values seed global execution.
privateRetainKnown	.block
	movem.l d0-d3/a0-a2, -(sp)
	tst.l PrivatePublishedCount
	beq.w done
	movea.l Front+frontend.Frame.Scratch, a2
	adda.l #frontend.SCOPE_STATE, a2
	moveq #0, d0
	move.w layout.State.Base(a2), d0
	move.l d0, PrivateRetainedBase
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER(a2), a0
	lea PrivateRetainedOwner, a1
	move.l #values.OWNER_BYTES, d0
	bsr.w privateSwap
	lea layout.IMPORT_STATE+imports.KNOWN_VALUES(a2), a0
	lea PrivateRetainedValues, a1
	bsr.w swapModuleGraphs
	lea layout.IMPORT_STATE+imports.KNOWN_DEFINED(a2), a0
	lea PrivateRetainedDefined, a1
	bsr.w swapModuleGraphs
done
	movem.l (sp)+, d0-d3/a0-a2
	rts
	.bend  ; privateRetainKnown
privateGlobalSeed	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #PrivateRetainedOwner, PrivateSeedSourceOwner
	move.l PrivateRetainedValues+memory.Block.Pointer, PrivateSeedSourceValues
	move.l PrivateRetainedDefined+memory.Block.Pointer, PrivateSeedSourceDefined
	move.l PrivateRetainedBase, PrivateSeedSourceBase
	move.l #privateIdentity, PrivateResolver+moduleRemap.Frame.Resolve
	move.l #1, PrivateSeedGlobal
	lea PrivatePublishedValues, a1
	bsr.w privateSeedMapped
	move.l d0, d7
	bsr.w privateReleaseRetained
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; privateGlobalSeed

; A0=assembly.Frame,A1=Map Block. Private map Local=global,Global=private;
; retained global map uses Global for both source and destination. No names.
privateSeedMapped	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l assembly.Frame.Context(a0), a6
	move.l memory.Block.Used(a1), d7
	movea.l memory.Block.Pointer(a1), a4
next
	tst.l d7
	beq.w good
	cmpi.l #moduleScope.VALUE, moduleScope.MapRow.Kind(a4)
	bne.w advance
	move.l moduleScope.MapRow.Local(a4), d5
	tst.l PrivateSeedGlobal
	beq.w sourceReady
	move.l moduleScope.MapRow.Global(a4), d5
sourceReady
	move.l moduleScope.MapRow.Global(a4), d6
	move.l d5, d0
	sub.l PrivateSeedSourceBase, d0
	bcs.w bad
	movea.l PrivateSeedSourceDefined, a0
	tst.b 0(a0, d0.l)
	beq.w external
	lsl.l #3, d0
	movea.l PrivateSeedSourceValues, a0
	move.l runtime.Value.Low(a0, d0.l), d2
	move.l runtime.Value.High(a0, d0.l), d3
	movem.l d2-d3, -(sp)
	movea.l PrivateSeedSourceOwner, a0
	move.l d5, d1
	jsr values.getKind
	move.l d2, d4
	movem.l (sp)+, d2-d3
	tst.l d0
	bne.w bad
	tst.l d4
	beq.w absolute
	move.l d2, d1
	move.l d4, d3
	movea.l PrivateSeedSourceOwner, a0
	move.l values.Owner.Arena+memory.Block.Used(a0), d0
	movea.l values.Owner.Arena+memory.Block.Pointer(a0), a1
	movea.l package.Context.Owner(a6), a0
	jsr values.copyRecord
	bne.w bad
	lea PrivateResolver, a1
	jsr moduleRemap.record
	bne.w bad
	moveq #0, d3
absolute
	moveq #0, d5
	bra.w store
external
	tst.l PrivateSeedGlobal
	bne.w advance
	moveq #0, d2
	moveq #0, d3
	moveq #values.SCALAR, d4
	moveq #1, d5
store
	movea.l package.Context.Values(a6), a0
	move.l d6, d0
	lsl.l #3, d0
	move.l d2, runtime.Value.Low(a0, d0.l)
	move.l d3, runtime.Value.High(a0, d0.l)
	movea.l package.Context.Defined(a6), a0
	; Immutable incoming cells survive eager normalization and layout rewinds.
	; Address availability stays distinct from absolute constant classification.
	moveq #dependencies.ABSOLUTE, d0
	tst.l d5
	beq.w defined
	moveq #dependencies.RETAINED_ADDRESS, d0
defined
	move.b d0, 0(a0, d6.l)
	movea.l package.Context.SectionIds(a6), a0
	move.b d5, 0(a0, d6.l)
	movea.l package.Context.Owner(a6), a0
	move.l d6, d1
	move.l d4, d2
	jsr values.setKind
	bne.w bad
advance
	adda.w #moduleScope.MAP_BYTES, a4
	subi.l #moduleScope.MAP_BYTES, d7
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
	.bend  ; privateSeedMapped
privateSeedResolve	.block
	cmpi.l #moduleRemap.NOMINAL, d0
	beq.w value
	cmpi.l #moduleRemap.MEMBER, d0
	beq.w member
	moveq #1, d0
	rts
value
	moveq #moduleScope.VALUE, d0
	bra.w lookup
member
	moveq #moduleScope.MEMBER, d0
lookup
	bra.w privateLookupValue
	.bend  ; privateSeedResolve
privateLookupValue	.block
	movem.l a0-a1, -(sp)
	lea PrivateLookup, a0
	move.l PrivateIncoming+memory.Block.Pointer, moduleScope.Frame.Map+memory.Block.Pointer(a0)
	move.l PrivateIncoming+memory.Block.Capacity, moduleScope.Frame.Map+memory.Block.Capacity(a0)
	move.l PrivateIncoming+memory.Block.Used, moduleScope.Frame.Map+memory.Block.Used(a0)
	jsr moduleScope.find
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; privateLookupValue
privateIdentity	.block
	cmpi.l #moduleRemap.SECTION, d0
	beq.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; privateIdentity

; Imported private scalar labels use base0/section1. Their affine observation
; retains the exact global identity plus the observed offset as an import recipe.
privateExternal	.block
	movem.l d3-d7/a0-a6, -(sp)
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
	move.l moduleScope.MapRow.Local(a0), d1
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
privateReleaseRetained	.block
	movem.l d0/a0, -(sp)
	lea PrivateRetainedOwner, a0
	jsr values.release
	lea PrivateRetainedValues, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateRetainedDefined, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movem.l (sp)+, d0/a0
	rts
	.bend  ; privateReleaseRetained
