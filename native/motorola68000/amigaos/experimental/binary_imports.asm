; Preparation-only module aliases and origin-specific qualified reference IDs.
; @opforge-owner: experimental.amigaos.binary_imports
	.module experimental.amigaos.binary_imports
	.cpu 68020
	.include "telemetry_macros.i"
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_section_prepare as sections
	.pub
Item	.struct
Target	.word ?
Qualifier	.word ?
Selected	.word ?
Unqualified	.word ?
Next	.word ?
.endstruct
Selection	.struct
Name	.word ?
Next	.word ?
.endstruct
ITEM_BYTES = Item.Next+2
SELECTION_BYTES = Selection.Next+2
COUNT = 0
HEADS = 2
ITEMS = HEADS+layout.LIMIT*2
SELECTED_COUNT = ITEMS+layout.LIMIT*ITEM_BYTES
SELECTIONS = SELECTED_COUNT+2
PROXIES = SELECTIONS+layout.LIMIT*SELECTION_BYTES
SCRATCH_BYTES = PROXIES+256*2
PROXY = 8
	.section code, kind=code

; A0=import scratch. D0/CCR=status; other registers preserved.
begin	.block
	movem.l d1/a0, -(sp)
	move.w #SCRATCH_BYTES/2-1, d1
clear
	clr.w (a0)+
	dbra d1, clear
	movem.l (sp)+, d1/a0
	moveq #0, d0
	rts
	.bend  ; begin

; A0=.use token,A1=scope state,A2=binder callback,A3=section state,
; A4=record end.
; One or more selected names and an optional module alias; references stay separate.
; D0/CCR=status; other registers preserved.
line	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l a3, -(sp)
	movea.l a1, a6
	movea.l a2, a5
	moveq #0, d6
	move.w layout.MODULE_STATE+modules.State.Active(a6), d6
	beq.w bad
	cmp.w layout.State.Current(a6), d6
	bne.w bad
	addq.l #5, a0
	movea.l a0, a3
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #4, d0
	blo.w bad
	bsr.w tokenName
	bne.w bad
	bsr.w globalBind
	bne.w bad
	move.l d1, d7
	sub.w layout.State.Base(a6), d7
	addq.l #4, a3
	movea.l a3, a0
	movea.l a6, a1
	movea.l (sp), a2
	move.l d7, d0
	jsr sections.importMap
	bne.w bad
	moveq #0, d4  ; selected-name list head, zero means no selection
	moveq #0, d3  ; direct unqualified access
	cmpa.l a4, a3
	beq.w defaultQualifier
	cmpi.b #14, (a3)
	bne.w afterSelection
	addq.l #1, a3
selectedName
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #4, d0
	blo.w bad
	tst.b 3(a3)
	bne.w bad  ; this bounded form selects unqualified names
	movea.l a3, a0
	bsr.w tokenName
	bne.w bad
	movea.l a0, a2
	move.l d0, d3
	move.l d7, d0
	bsr.w entryName
	add.l d0, d3
	addq.l #1, d3
	cmpi.l #63, d3
	bhi.w bad
	lea layout.BUFFER(a6), a1
	move.l d0, d1
copyTarget
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bne.w copyTarget
	move.b #'.', (a1)+
	movea.l a2, a0
	move.l d3, d1
	sub.l d0, d1
	subq.l #1, d1
copySelected
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bne.w copySelected
	lea layout.BUFFER(a6), a0
	move.l d3, d0
	bsr.w globalBind
	bne.w bad
	sub.w layout.State.Base(a6), d1
	addq.w #1, d1
	lea layout.IMPORT_STATE(a6), a2
	move.l d4, d2
seenName
	tst.w d2
	beq.w appendName
	move.l d2, d0
	subq.w #1, d0
	lsl.l #2, d0
	lea SELECTIONS(a2), a0
	adda.l d0, a0
	cmp.w Selection.Name(a0), d1
	beq.w nextNameToken
	moveq #0, d2
	move.w Selection.Next(a0), d2
	bra.w seenName
appendName
	moveq #0, d2
	move.w SELECTED_COUNT(a2), d2
	cmpi.w #layout.LIMIT, d2
	bhs.w bad
	move.l d2, d0
	lsl.l #2, d0
	lea SELECTIONS(a2), a0
	adda.l d0, a0
	move.w d1, Selection.Name(a0)
	move.w d4, Selection.Next(a0)
	move.l d2, d4
	addq.w #1, d4
	move.w d4, SELECTED_COUNT(a2)
nextNameToken
	addq.l #4, a3
	cmpa.l a4, a3
	bhs.w bad
	cmpi.b #4, (a3)
	beq.w anotherName
	cmpi.b #15, (a3)
	bne.w bad
	addq.l #1, a3
	moveq #1, d3
	bra.w afterSelection
anotherName
	addq.l #1, a3
	bra.w selectedName
afterSelection
	cmpa.l a4, a3
	beq.w defaultQualifier
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #8, d0
	bne.w bad
	tst.b 3(a3)
	bne.w bad
	movea.l a3, a0
	bsr.w tokenName
	bne.w bad
	cmpi.l #2, d0
	bne.w bad
	move.w (a0), d0
	ori.w #$2020, d0
	cmpi.w #$6173, d0  ; as
	bne.w bad
	lea 4(a3), a0
	tst.b 3(a0)
	bne.w bad
	bsr.w tokenName
	bne.w bad
	moveq #0, d3
	bra.w qualifier
defaultQualifier
	tst.w d4
	bne.w noQualifier
	move.l d7, d0
	bsr.w entryLeaf
	bra.w qualifier
noQualifier
	moveq #0, d5
	bra.w collisionStart
qualifier
	movea.l a6, a1
	jsr (a5)
	tst.l d0
	bne.w bad
	sub.w layout.State.Base(a6), d1
	move.l d1, d5
collisionStart
	lea layout.IMPORT_STATE(a6), a4
	move.l d6, d0
	subq.w #1, d0
	add.w d0, d0
	lea HEADS(a4), a3
	adda.w d0, a3
	moveq #0, d2
	move.w (a3), d2
collision
	tst.w d2
	beq.w append
	subq.w #1, d2
	mulu.w #ITEM_BYTES, d2
	lea ITEMS(a4), a0
	adda.l d2, a0
	tst.w d5
	beq.w nextCollision
	cmp.w Item.Qualifier(a0), d5
	beq.w bad
nextCollision
	moveq #0, d2
	move.w Item.Next(a0), d2
	bra.w collision
append
	moveq #0, d0
	move.w COUNT(a4), d0
	cmpi.w #layout.LIMIT, d0
	bhs.w bad
	move.l d0, d1
	mulu.w #ITEM_BYTES, d1
	lea ITEMS(a4), a0
	adda.l d1, a0
	move.w d7, Item.Target(a0)
	move.w d5, Item.Qualifier(a0)
	move.w d4, Item.Selected(a0)
	move.w d3, Item.Unqualified(a0)
	move.w (a3), Item.Next(a0)
	addq.w #1, d0
	move.w d0, (a3)
	move.w d0, COUNT(a4)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	addq.l #4, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line

; A0=name token,A1=scope state. Imported references get a proxy keyed by
; (module, original ID), sharing name bytes. D0/CCR=status; others kept.
reference	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a6
	movea.l a0, a5
	moveq #0, d4
	moveq #0, d7
	move.w layout.MODULE_STATE+modules.State.Active(a6), d7
	beq.w ok
	tst.b 3(a0)
	bne.w referenceProxy
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w ok  ; package-owned names have no import selection
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	bsr.w selectedTarget
	bne.w bad
	tst.w d4
	beq.w ok
referenceProxy
	moveq #0, d6
	move.w 1(a0), d6
	sub.w layout.State.Base(a6), d6
	bcs.w ok
	cmp.w layout.State.Count(a6), d6
	bhs.w bad
	move.l d6, d0
	eor.w d7, d0
	andi.w #255, d0
	add.w d0, d0
	lea layout.IMPORT_STATE(a6), a4
	lea PROXIES(a4), a4
	adda.w d0, a4
	moveq #0, d2
	move.w (a4), d2
find
	tst.w d2
	beq.w allocate
	subq.w #1, d2
	move.l d2, d0
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a3
	adda.l d0, a3
	cmp.w records.Entry.Owner(a3), d7
	bne.w next
	move.w records.Entry.ScopeKind(a3), d0
	subq.w #1, d0
	cmp.w d6, d0
	beq.w found
next
	moveq #0, d2
	move.w records.Entry.Next(a3), d2
	bra.w find
allocate
	moveq #0, d2
	move.w layout.State.Count(a6), d2
	cmpi.w #layout.LIMIT, d2
	bhs.w bad
	move.l d2, d0
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a3
	adda.l d0, a3
	move.l d6, d0
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a0
	adda.l d0, a0
	move.w records.Entry.Name(a0), records.Entry.Name(a3)
	move.w records.Entry.Length(a0), records.Entry.Length(a3)
	move.w d7, records.Entry.Owner(a3)
	move.w d4, records.Entry.Leaf(a3)  ; selected target index+1, or zero
	move.w #PROXY, records.Entry.Flags(a3)
	move.w d6, records.Entry.ScopeKind(a3)
	addq.w #1, records.Entry.ScopeKind(a3)
	move.w (a4), records.Entry.Next(a3)
	move.w d2, d0
	addq.w #1, d0
	move.w d0, (a4)
	move.w d0, layout.State.Count(a6)
	move.w layout.State.Base(a6), d0
	add.w d2, d0
	move.w d0, records.Entry.Target(a3)
found
	move.w records.Entry.Target(a3), 1(a5)
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; reference

; A5=unqualified name token,A6=scope state,D7=active module. D4=selected
; target index+1 (zero when not imported), D0/CCR=status. Other registers kept.
selectedTarget	.block
	movem.l d1-d3/d5-d7/a0-a4, -(sp)
	lea layout.IMPORT_STATE(a6), a4
	move.l d7, d0
	subq.w #1, d0
	add.w d0, d0
	lea HEADS(a4), a0
	moveq #0, d7
	move.w 0(a0, d0.w), d7
	beq.w ok
	movea.l a5, a0
	bsr.w tokenName
	bne.w bad
	movea.l a0, a2
	move.l d0, d5
scan
	tst.w d7
	beq.w ok
	move.l d7, d0
	subq.w #1, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a3
	adda.l d0, a3
	tst.w Item.Unqualified(a3)
	beq.w next
	moveq #0, d6
	move.w Item.Selected(a3), d6
selected
	tst.w d6
	beq.w next
	move.l d6, d0
	subq.w #1, d0
	lsl.l #2, d0
	lea SELECTIONS(a4), a1
	adda.l d0, a1
	moveq #0, d2
	move.w Selection.Next(a1), d2
	moveq #0, d3
	move.w Selection.Name(a1), d3
	move.l d3, d0
	subq.w #1, d0
	bsr.w entryLeaf
	cmp.w d5, d0
	bne.w nextSelected
	bsr.w prefixEqual
	bne.w nextSelected
	tst.w d4
	bne.w bad  ; two direct imports claim the same name
	move.w d3, d4
nextSelected
	move.l d2, d6
	bra.w selected
next
	moveq #0, d7
	move.w Item.Next(a3), d7
	bra.w scan
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/d5-d7/a0-a4
	tst.l d0
	rts
	.bend  ; selectedTarget

; A0=scope state,A1=binder callback. Validate forward modules and resolve every
; proxy after all imports are known. D0/CCR=status; other registers preserved.
finish	.block
	movem.l d1-d7/a0-a6, -(sp)
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_STATE
	movea.l a0, a6
	movea.l a1, a5
	lea layout.IMPORT_STATE(a6), a4
	tst.w layout.MODULE_STATE+modules.State.Selection(a6)
	bne.w selectedModules
	moveq #0, d7
modulesLoop
	cmp.w COUNT(a4), d7
	bhs.w proxies
	move.l d7, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a0
	adda.l d0, a0
	moveq #0, d0
	move.w Item.Target(a0), d0
	add.w d0, d0
	lea layout.MODULE_STATE+modules.FLAGS(a6), a0
	btst #3, 1(a0, d0.w)
	beq.w bad
	move.l d7, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a3
	adda.l d0, a3
	bsr.w validateSelected
	bne.w bad
	addq.w #1, d7
	bra.w modulesLoop
selectedModules
	moveq #0, d6
selectedModule
	cmp.w layout.State.Count(a6), d6
	bhs.w proxies
	move.l d6, d0
	add.w d0, d0
	lea layout.MODULE_STATE+modules.FLAGS(a6), a0
	btst #4, 1(a0, d0.w)
	beq.w nextSelectedModule
	lea HEADS(a4), a0
	moveq #0, d7
	move.w 0(a0, d0.w), d7
selectedItem
	tst.w d7
	beq.w nextSelectedModule
	move.l d7, d0
	subq.w #1, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a3
	adda.l d0, a3
	moveq #0, d0
	move.w Item.Target(a3), d0
	add.w d0, d0
	lea layout.MODULE_STATE+modules.FLAGS(a6), a0
	btst #3, 1(a0, d0.w)
	beq.w bad
	bsr.w validateSelected
	bne.w bad
	moveq #0, d7
	move.w Item.Next(a3), d7
	bra.w selectedItem
nextSelectedModule
	addq.w #1, d6
	bra.w selectedModule
proxies
	moveq #0, d7
loop
	cmp.w layout.State.Count(a6), d7
	bhs.w ok
	move.l d7, d0
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a3
	adda.l d0, a3
	btst #3, records.Entry.Flags+1(a3)
	beq.w next
	tst.w layout.MODULE_STATE+modules.State.Selection(a6)
	beq.w resolveProxy
	moveq #0, d0
	move.w records.Entry.Owner(a3), d0
	beq.w next
	subq.w #1, d0
	add.w d0, d0
	lea layout.MODULE_STATE+modules.FLAGS(a6), a0
	btst #4, 1(a0, d0.w)
	beq.w next
resolveProxy
	bsr.w resolve
	bne.w bad
	move.w d1, records.Entry.Target(a3)
	ori.w #1, records.Entry.Flags(a3)  ; validated declaration target, not a new value
	move.w #1, layout.State.Changed(a6)
next
	addq.w #1, d7
	bra.w loop
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; finish
	.priv

; A3=import item,A6=scope state. Selected names must be declared and public
; even when no reached code references them. D0/CCR=status; others preserved.
validateSelected	.block
	movem.l d1-d2/a0-a2, -(sp)
	moveq #0, d2
	move.w Item.Selected(a3), d2
selected
	tst.w d2
	beq.w ok
	lea layout.IMPORT_STATE(a6), a2
	move.l d2, d0
	subq.w #1, d0
	cmp.w SELECTED_COUNT(a2), d0
	bhs.w bad
	lsl.l #2, d0
	lea SELECTIONS(a2), a1
	adda.l d0, a1
	move.w Selection.Next(a1), d2
	moveq #0, d0
	move.w Selection.Name(a1), d0
	subq.w #1, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	move.l d0, d1
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a0
	adda.l d0, a0
	btst #0, records.Entry.Flags+1(a0)
	beq.w bad
	add.w d1, d1
	lea layout.MODULE_STATE+modules.FLAGS(a6), a1
	btst #0, 1(a1, d1.w)
	beq.w bad
	bra.w selected
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; validateSelected

; A3=proxy,A4=import state,A5=binder,A6=scope state. D1=canonical numeric ID,
; D0/CCR=status; other registers preserved. Alias precedence precedes exact names.
resolve	.block
	movem.l d2-d7/a0-a4, -(sp)
	tst.w records.Entry.Leaf(a3)
	bne.w resolveSelected
	moveq #0, d0
	move.w records.Entry.Name(a3), d0
	lea layout.ARENA(a6), a2
	adda.l d0, a2
	moveq #0, d6
	move.w records.Entry.Length(a3), d6
	moveq #0, d5
prefix
	cmp.w d6, d5
	bhs.w bad
	cmpi.b #'.', 0(a2, d5.w)
	beq.w imports
	addq.w #1, d5
	bra.w prefix
imports
	moveq #0, d0
	move.w records.Entry.Owner(a3), d0
	subq.w #1, d0
	add.w d0, d0
	lea HEADS(a4), a0
	moveq #0, d7
	move.w 0(a0, d0.w), d7
	move.l d7, d4
aliasLoop
	tst.w d7
	beq.w fullPaths
	move.l d7, d0
	subq.w #1, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a1
	adda.l d0, a1
	moveq #0, d0
	move.w Item.Qualifier(a1), d0
	move.l a1, -(sp)
	bsr.w entryLeaf
	movea.l (sp)+, a1
	cmp.w d5, d0
	bne.w aliasNext
	bsr.w prefixEqual
	bne.w aliasNext
	moveq #0, d0
	move.w Item.Target(a1), d0
	bsr.w entryName
	move.l d6, d1
	sub.w d5, d1
	add.w d0, d1
	cmpi.w #63, d1
	bhi.w bad
	lea layout.BUFFER(a6), a1
	move.l d0, d2
copyModule
	move.b (a0)+, (a1)+
	subq.w #1, d2
	bne.w copyModule
	movea.l a2, a0
	adda.w d5, a0
	move.l d6, d2
	sub.w d5, d2
copySuffix
	move.b (a0)+, (a1)+
	subq.w #1, d2
	bne.w copySuffix
	lea layout.BUFFER(a6), a0
	move.l d1, d0
	bra.w bind
aliasNext
	moveq #0, d7
	move.w Item.Next(a1), d7
	bra.w aliasLoop
fullPaths
	move.l d4, d7
	moveq #0, d3
fullLoop
	tst.w d7
	beq.w exact
	move.l d7, d0
	subq.w #1, d0
	mulu.w #ITEM_BYTES, d0
	lea ITEMS(a4), a1
	adda.l d0, a1
	moveq #0, d0
	move.w Item.Target(a1), d0
	move.l a1, -(sp)
	bsr.w entryName
	movea.l (sp)+, a1
	cmp.w d6, d0
	bhs.w fullNext
	cmpi.b #'.', 0(a2, d0.w)
	bne.w fullNext
	bsr.w prefixEqual
	bne.w fullNext
	addq.w #1, d3
	cmpi.w #1, d3
	bhi.w bad
fullNext
	moveq #0, d7
	move.w Item.Next(a1), d7
	bra.w fullLoop
exact
	movea.l a2, a0
	move.l d6, d0
bind
	bsr.w globalBind
	bne.w bad
	moveq #0, d0
	move.w d1, d0
	sub.w layout.State.Base(a6), d0
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a0
	adda.l d0, a0
	btst #0, records.Entry.Flags+1(a0)
	beq.w bad
	moveq #0, d0
	bra.w done
resolveSelected
	moveq #0, d0
	move.w records.Entry.ScopeKind(a3), d0
	subq.w #1, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a0
	adda.l d0, a0
	btst #0, records.Entry.Flags+1(a0)
	bne.w selectedBound
	moveq #0, d0
	move.w records.Entry.Leaf(a3), d0
	subq.w #1, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w bad
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a0
	adda.l d0, a0
	btst #0, records.Entry.Flags+1(a0)
	beq.w bad
selectedBound
	move.w records.Entry.Target(a0), d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a4
	tst.l d0
	rts
	.bend  ; resolve

; A0/D0=bytes to compare with A2 prefix. D0/CCR=status, others preserved.
prefixEqual	.block
	movem.l d1-d3/a0-a1, -(sp)
	movea.l a2, a1
	move.l d0, d3
loop
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w fold
	move.w d1, d2
	move.b (a1)+, d1
	bsr.w fold
	cmp.b d1, d2
	bne.w bad
	subq.w #1, d3
	bne.w loop
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; prefixEqual

; A0=raw source-name token. Return A0/D0 lexical bytes (drop scope prefix for
; unqualified spellings). D0 is length on success, CCR Z explicitly signals success.
; D1/A1 scratch; A6=scope state.
tokenName	.block
	cmpi.b #1, (a0)
	bhi.w bad
	moveq #0, d1
	move.b 3(a0), d1
	cmpi.w #1, d1
	bhi.w bad
	move.l d1, -(sp)
	moveq #0, d0
	move.w 1(a0), d0
	sub.w layout.State.Base(a6), d0
	bcs.w popBad
	cmp.w layout.State.Count(a6), d0
	bhs.w popBad
	tst.l (sp)+
	beq.w leaf
	bsr.w entryName
	bra.w ok
leaf
	bsr.w entryLeaf
ok
	cmp.l d0, d0
	rts
popBad
	addq.l #4, sp
bad
	moveq #1, d0
	rts
	.bend  ; tokenName

; D0=entry index, A6=scope state. A0/D0=full bytes; D1/A1 scratch.
entryName	.block
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a1
	adda.l d0, a1
	lea layout.ARENA(a6), a0
	moveq #0, d0
	move.w records.Entry.Name(a1), d0
	adda.l d0, a0
	moveq #0, d0
	move.w records.Entry.Length(a1), d0
	rts
	.bend  ; entryName

; Same as entryName, but derive the final component from the stored full name.
; Entry.Leaf describes first-intern context, which may have been a qualified
; declaration before this bare alias token. D1/A1 scratch.
entryLeaf	.block
	bsr.w entryName
	movea.l a0, a1
	move.l d0, d1
scan
	cmpi.b #'.', (a1)+
	bne.w next
	movea.l a1, a0
	move.l d1, d0
	subq.w #1, d0
next
	subq.w #1, d1
	bne.w scan
	rts
	.bend  ; entryLeaf

; A0/D0=qualified name,A5=binder,A6=scope state. D0/status,D1/ID,D2scratch;
; A0/A1 scratch. No process pointer is stored in preparation metadata.
globalBind	.block
	move.w layout.State.Current(a6), -(sp)
	clr.w layout.State.Current(a6)
	movea.l a6, a1
	jsr (a5)
	move.w (sp)+, layout.State.Current(a6)
	tst.l d0
	rts
	.bend  ; globalBind

; D1=ASCII byte; other registers preserved.
fold	.block
	cmpi.b #'A', d1
	blo.w done
	cmpi.b #'Z', d1
	bhi.w done
	addi.b #32, d1
done
	rts
	.bend  ; fold
	.endsection
	.endmodule
