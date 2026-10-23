; Import-owned caller-visible template keys. Only structured import metadata and
; retained definition spelling are read; invocation grammar remains shared PRVM.
; Included by binary_imports, reusing its canonical name and import ownership.
.pub
TemplateExposureFrame	.struct
Scope	.long ?
DefinitionId	.long ?
OriginalKey	.long ?
OriginalKeyBytes	.long ?
Cursor	.long ?
KeyArena	.long ?
KeyCapacity	.long ?
KeyUsed	.long ?
RowOutput	.long ?
RowCapacity	.long ?
Visibility	.long ?  ; this overload's 0/1 visibility, not aggregated key existence
.endstruct
TEMPLATE_EXPOSURE_FRAME_BYTES = TemplateExposureFrame.Visibility+4
ExposureRow	.struct
KeyOffset	.long ?
KeyBytes	.long ?
DefinitionId	.long ?
ImportOrder	.long ?
.endstruct
EXPOSURE_ROW_BYTES = ExposureRow.ImportOrder+4
.priv
ExposureCursor	.struct
Phase	.long ?
Item	.long ?
Selection	.long ?
Variant	.long ?
Visited	.long ?
SelectedVisited	.long ?
.endstruct
.pub
EXPOSURE_CURSOR_BYTES = ExposureCursor.SelectedVisited+4
.priv
EXPOSURE_LOCAL = 0
EXPOSURE_IMPORTS = 1
EXPOSURE_DONE = 2
EXPOSURE_INVALID = 4
EXPOSURE_CAPACITY = 7
ExposureBound	.struct
Start	.long ?
End	.long ?
Mutable	.long ?
.endstruct
EXPOSURE_BOUND_BYTES = ExposureBound.Mutable+4
EXPOSURE_BOUND_COUNT = 5
ExposureLocal	.struct
Cursor	.res EXPOSURE_CURSOR_BYTES
Prefix	.long ?
PrefixBytes	.long ?
Key	.long ?
KeyBytes	.long ?
Bytes	.long ?
ImportOrder	.long ?
Definition	.long ?
Item	.long ?
Bounds	.res EXPOSURE_BOUND_COUNT*EXPOSURE_BOUND_BYTES
Entry	.res records.ENTRY_BYTES
PrefixName	.res records.NAME_BYTES
KeyName	.res records.NAME_BYTES
.endstruct
EXPOSURE_LOCAL_BYTES = ExposureLocal.KeyName+records.NAME_BYTES
.pub
; A0=TemplateExposureFrame. D0/status,D1=found boolean; CCR reflects D0.
; Other registers preserved. Success publishes one caller-owned ExposureRow.
; Zero the caller's EXPOSURE_CURSOR_BYTES before the first call per definition.
; One success appends one complete key and publishes ExposureRow. Exhaustion
; advances the cursor only. Capacity/invalid failure changes no publication,
; cursor or KeyUsed. Readable owner storage must remain live throughout a call.
; Returned offsets address the caller arena; no borrowed pointer is retained.
; Keys authorize one retained overload for the active module. ImportOrder0
; denotes locals; higher nonzero .use ordinals replace earlier imported vectors.
; The caller retains all winning import overloads and then appends all local rows.
templateExposure	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #EXPOSURE_LOCAL_BYTES, sp
	movea.l a0, a5
	movea.l sp, a4
	move.l a5, d0
	beq.w invalid
	andi.l #3, d0
	bne.w invalid
	move.l a5, d0
	addi.l #TEMPLATE_EXPOSURE_FRAME_BYTES, d0
	bcs.w invalid
	movea.l TemplateExposureFrame.Scope(a5), a6
	bsr.w exposureRequest
	bne.w failed
	movea.l TemplateExposureFrame.Cursor(a5), a0
	lea ExposureLocal.Cursor(a4), a1
	moveq #EXPOSURE_CURSOR_BYTES/4-1, d0
cursorCopy
	move.l (a0)+, (a1)+
	dbra d0, cursorCopy
	bsr.w exposureCursorCheck
	bne.w failed
	move.l ExposureLocal.Cursor+ExposureCursor.Phase(a4), d0
	cmpi.l #EXPOSURE_DONE, d0
	bhi.w invalid
	beq.w exhausted
	tst.l d0
	bne.w imports
	move.l #EXPOSURE_IMPORTS, ExposureLocal.Cursor+ExposureCursor.Phase(a4)
	bsr.w exposureImportHead
	bne.w failed
	bsr.w exposureLocalModule
	bne.w imports
	clr.l ExposureLocal.PrefixBytes(a4)
	move.l TemplateExposureFrame.OriginalKey(a5), ExposureLocal.Key(a4)
	move.l TemplateExposureFrame.OriginalKeyBytes(a5), ExposureLocal.KeyBytes(a4)
	clr.l ExposureLocal.ImportOrder(a4)
	bra.w publish
imports
	move.l ExposureLocal.Cursor+ExposureCursor.Item(a4), d0
	beq.w exhausted
	lea layout.IMPORT_STATE(a6), a2
	moveq #0, d1
	move.w COUNT(a2), d1
	cmp.l d1, d0
	bhi.w invalid
	subq.l #1, d0
	mulu.w #ITEM_BYTES, d0
	movea.l ITEMS_POINTER(a2), a3
	adda.l d0, a3
	move.l a3, ExposureLocal.Item(a4)
	move.w Item.Flags(a3), d0
	andi.w #$ffff-EXPLICIT_QUALIFIER, d0
	bne.w invalid
	moveq #0, d0
	move.w Item.Target(a3), d0
	addq.l #1, d0
	movea.l ExposureLocal.Definition(a4), a0
	cmp.w records.Entry.StatementModule(a0), d0
	bne.w nextItem
	tst.l TemplateExposureFrame.Visibility(a5)
	beq.w nextItem
	move.l ExposureLocal.Cursor+ExposureCursor.Item(a4), ExposureLocal.ImportOrder(a4)
	tst.w Item.Selected(a3)
	bne.w selected
	cmpi.w #2, Item.Unqualified(a3)
	beq.w wildcard
	tst.w Item.Unqualified(a3)
	bne.w invalid
	tst.l ExposureLocal.Cursor+ExposureCursor.Variant(a4)
	bne.w qualifiedAlias
	move.l #1, ExposureLocal.Cursor+ExposureCursor.Variant(a4)
	moveq #0, d0
	move.w Item.Target(a3), d0
	lea ExposureLocal.PrefixName(a4), a1
	bsr.w entryName
	bne.w failed
	bra.w qualified
qualifiedAlias
	btst #EXPLICIT_QUALIFIER_BIT, Item.Flags+1(a3)
	beq.w nextItem
	moveq #0, d0
	move.w Item.Qualifier(a3), d0
	lea ExposureLocal.PrefixName(a4), a1
	bsr.w entryLeaf
	bne.w failed
	move.l a0, ExposureLocal.Prefix(a4)
	move.l d0, ExposureLocal.PrefixBytes(a4)
	bsr.w exposureAdvanceItem
	bne.w failed
	bra.w originalKey
qualified
	move.l a0, ExposureLocal.Prefix(a4)
	move.l d0, ExposureLocal.PrefixBytes(a4)
	bra.w originalKey
wildcard
	clr.l ExposureLocal.PrefixBytes(a4)
	bsr.w exposureAdvanceItem
	bne.w failed
	bra.w originalKey
selected
	tst.l ExposureLocal.Cursor+ExposureCursor.Variant(a4)
	bne.w nextSelection
	move.l #1, ExposureLocal.Cursor+ExposureCursor.Variant(a4)
	moveq #0, d0
	move.w Item.Selected(a3), d0
	move.l d0, ExposureLocal.Cursor+ExposureCursor.Selection(a4)
	clr.l ExposureLocal.Cursor+ExposureCursor.SelectedVisited(a4)
nextSelection
	move.l ExposureLocal.Cursor+ExposureCursor.Selection(a4), d0
	beq.w nextItem
	moveq #0, d1
	move.w SELECTED_COUNT(a2), d1
	cmp.l d1, d0
	bhi.w invalid
	subq.l #1, d0
	mulu.w #SELECTION_BYTES, d0
	movea.l SELECTIONS_POINTER(a2), a1
	adda.l d0, a1
	moveq #0, d0
	move.w Selection.Next(a1), d0
	move.l d0, ExposureLocal.Cursor+ExposureCursor.Selection(a4)
	addq.l #1, ExposureLocal.Cursor+ExposureCursor.SelectedVisited(a4)
	moveq #0, d0
	move.w SELECTED_COUNT(a2), d0
	cmp.l ExposureLocal.Cursor+ExposureCursor.SelectedVisited(a4), d0
	blo.w invalid
	move.l TemplateExposureFrame.DefinitionId(a5), d0
	sub.w layout.State.Base(a6), d0
	addq.l #1, d0
	cmp.w Selection.Name(a1), d0
	bne.w nextSelection
	clr.l ExposureLocal.PrefixBytes(a4)
	tst.w Item.Unqualified(a3)
	bne.w selectedKey
	moveq #0, d0
	move.w Item.Qualifier(a3), d0
	move.l a1, -(sp)
	lea ExposureLocal.PrefixName(a4), a1
	bsr.w entryLeaf
	movea.l (sp)+, a1
	bne.w failed
	move.l a0, ExposureLocal.Prefix(a4)
	move.l d0, ExposureLocal.PrefixBytes(a4)
selectedKey
	moveq #0, d0
	move.w Selection.Alias(a1), d0
	beq.w originalKey
	subq.l #1, d0
	lea ExposureLocal.KeyName(a4), a1
	bsr.w entryLeaf
	bne.w failed
	move.l a0, ExposureLocal.Key(a4)
	move.l d0, ExposureLocal.KeyBytes(a4)
	bra.w publish
originalKey
	move.l TemplateExposureFrame.OriginalKey(a5), ExposureLocal.Key(a4)
	move.l TemplateExposureFrame.OriginalKeyBytes(a5), ExposureLocal.KeyBytes(a4)
	bra.w publish
nextItem
	bsr.w exposureAdvanceItem
	bne.w failed
	bra.w imports
exhausted
	move.l #EXPOSURE_DONE, ExposureLocal.Cursor+ExposureCursor.Phase(a4)
	bsr.w exposureCommitCursor
	moveq #0, d1
	moveq #0, d0
	bra.w done
publish
	bsr.w exposurePublish
	bne.w failed
	moveq #1, d1
	bra.w done
invalid
	moveq #EXPOSURE_INVALID, d0
failed
	moveq #0, d1
done
	adda.w #EXPOSURE_LOCAL_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; templateExposure
.priv
; A5=request,A6=scope,A4=locals. Check owned IDs and explicit caller extents.

exposureImportHead	.block
	clr.l ExposureLocal.Cursor+ExposureCursor.Item(a4)
	clr.l ExposureLocal.Cursor+ExposureCursor.Selection(a4)
	clr.l ExposureLocal.Cursor+ExposureCursor.Variant(a4)
	clr.l ExposureLocal.Cursor+ExposureCursor.Visited(a4)
	clr.l ExposureLocal.Cursor+ExposureCursor.SelectedVisited(a4)
	moveq #0, d0
	move.w layout.MODULE_STATE+modules.State.Active(a6), d0
	beq.w done
	subq.l #1, d0
	cmp.w layout.State.Count(a6), d0
	bhs.w invalid
	add.l d0, d0
	lea layout.IMPORT_STATE(a6), a0
	cmp.l HEADS+memory.Block.Used(a0), d0
	bhs.w invalid
	movea.l HEADS_POINTER(a0), a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	move.l d1, ExposureLocal.Cursor+ExposureCursor.Item(a4)
done
	moveq #0, d0
	rts
invalid
	moveq #EXPOSURE_INVALID, d0
	rts
	.bend  ; exposureImportHead
exposureLocalModule	.block
	movea.l ExposureLocal.Definition(a4), a0
	move.w records.Entry.StatementModule(a0), d0
	beq.w owners
	cmp.w layout.MODULE_STATE+modules.State.Active(a6), d0
	bne.w missing
owners
	moveq #0, d1
	moveq #0, d0
	rts
missing
	moveq #1, d0
	rts
	.bend  ; exposureLocalModule
exposureAdvanceItem	.block
	movea.l ExposureLocal.Item(a4), a3
	moveq #0, d0
	move.w Item.Next(a3), d0
	move.l d0, ExposureLocal.Cursor+ExposureCursor.Item(a4)
	clr.l ExposureLocal.Cursor+ExposureCursor.Selection(a4)
	clr.l ExposureLocal.Cursor+ExposureCursor.Variant(a4)
	clr.l ExposureLocal.Cursor+ExposureCursor.SelectedVisited(a4)
	addq.l #1, ExposureLocal.Cursor+ExposureCursor.Visited(a4)
	lea layout.IMPORT_STATE(a6), a0
	moveq #0, d0
	move.w COUNT(a0), d0
	cmp.l ExposureLocal.Cursor+ExposureCursor.Visited(a4), d0
	blo.w invalid
	moveq #0, d0
	rts
invalid
	moveq #EXPOSURE_INVALID, d0
	rts
	.bend  ; exposureAdvanceItem

; Preflight both destinations before copying. No fallible operation follows copy.

; Cursor is numeric owner state; reject corrupt counters before dereferencing.

; Explicit mutable spans cannot overlap one another or retained source spelling.
; Owner scope allocations are trusted live inputs under the scope owner's ABI.

; A0/D0=span,A1=next Bound,D1=mutable. Clobbers D0/D2/A1 only.

	.include "template_keys.i"
