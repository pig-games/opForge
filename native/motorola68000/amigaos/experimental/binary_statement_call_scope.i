; Private scope construction: import replacement, then declaration-order locals.
; A4=State,A5=PrepareFrame,A6=Local unless stated otherwise. No source grammar.
reset	.block
	movem.l d0-d1/a0, -(sp)
	movea.l a4, a0
	moveq #4, d0
blocks
	clr.l memory.Block.Used(a0)
	adda.w #BLOCK_BYTES, a0
	dbra d0, blocks
	lea State.RowCount(a4), a0
	moveq #(STATE_BYTES-State.RowCount)/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	movem.l (sp)+, d0-d1/a0
	rts
	.bend  ; reset
; D0/status; other registers may change. Plans index closed rows, not name IDs.
build	.block
	movea.l State.Definitions(a4), a0
	jsr store.count
	move.l d0, Local.ClosedCount(a6)
	move.l d0, State.PlanCount(a4)
	lsl.l #3, d0  ; abi.PLAN_REF_BYTES
	lea State.Plans(a4), a0
	jsr memory.reserve
	bne.w storage
	move.l Local.ClosedCount(a6), d0
	lsl.l #3, d0
	move.l d0, State.Plans+memory.Block.Used(a4)
	; Exposure requires a nonnull key arena even before its first append.
	; Reserve only when a closed declaration can produce an eligible row.
	tst.l Local.ClosedCount(a6)
	beq.w keysReady
	moveq #1, d0
	lea State.Keys(a4), a0
	jsr memory.reserve
	bne.w storage
keysReady
	clr.l Local.Phase(a6)
phase
	move.l #1, Local.Handle(a6)
row
	move.l Local.Handle(a6), d1
	cmp.l Local.ClosedCount(a6), d1
	bhi.w nextPhase
	bsr.w tick
	bne.w done
	movea.l State.Definitions(a4), a0
	jsr store.view
	bne.w done
	move.l a1, Local.Entry(a6)
	move.l a2, Local.Plan(a6)
	move.l d2, Local.PlanBytes(a6)
	move.l a3, Local.Keyword(a6)
	move.l d3, Local.KeywordBytes(a6)
	moveq #0, d0
	move.w store.Row.Name(a1), d0
	move.l d0, Local.Name(a6)
	moveq #0, d0
	move.w store.Row.Template(a1), d0
	move.l d0, Local.Template(a6)
	moveq #0, d0
	move.w store.Row.Visibility(a1), d0
	move.l d0, Local.Visibility(a6)
	cmpi.l #1, d0
	bhi.w invalid
	tst.l Local.Phase(a6)
	bne.w cursor
	move.l Local.Handle(a6), d0
	subq.l #1, d0
	lsl.l #3, d0
	movea.l State.Plans+memory.Block.Pointer(a4), a0
	adda.l d0, a0
	move.l Local.Plan(a6), (a0)  ; PlanRef.Ptr (zero offset)
	move.l Local.PlanBytes(a6), abi.PlanRef.Bytes(a0)
cursor
	lea Local.Cursor(a6), a0
	moveq #imports.EXPOSURE_CURSOR_BYTES/4-1, d0
clearCursor
	clr.l (a0)+
	dbra d0, clearCursor
expose
	bsr.w tick
	bne.w done
	move.l State.Keys+memory.Block.Used(a4), Local.BeforeKeyUsed(a6)
	bsr.w exposureFrame
	lea Local.Exposure(a6), a0
	jsr scopes.templateExposure
	cmpi.l #abi.OUTPUT_OVERFLOW, d0
	beq.w growKeys
	tst.l d0
	bne.w done
	tst.l d1
	beq.w advance
	.TELEMETRY_CANDIDATE runtime_profile.OPFORGE_RUNTIME_CANDIDATE_SELECTION
	move.l Local.Exposure+imports.TemplateExposureFrame.KeyUsed(a6), State.Keys+memory.Block.Used(a4)
	move.l Local.Exposed+imports.ExposureRow.ImportOrder(a6), d0
	tst.l Local.Phase(a6)
	beq.w imported
	tst.l d0
	bne.w discardLocal
	bsr.w append
	bne.w done
	bra.w advance  ; only the single local exposure belongs to this phase
imported
	tst.l d0
	beq.w discard
	bsr.w group
	bne.w done
	tst.l d1
	beq.w discard
	bsr.w append
	bne.w done
	bra.w expose
discardLocal
	move.l Local.BeforeKeyUsed(a6), State.Keys+memory.Block.Used(a4)
	bra.w advance
discard
	move.l Local.BeforeKeyUsed(a6), State.Keys+memory.Block.Used(a4)
	bra.w expose
growKeys
	move.l State.Keys+memory.Block.Capacity(a4), d0
	cmpi.l #memory.LIMIT, d0
	bhs.w overflow
	addq.l #1, d0
	lea State.Keys(a4), a0
	jsr memory.reserve
	bne.w storage
	bra.w expose
advance
	addq.l #1, Local.Handle(a6)
	bra.w row
nextPhase
	tst.l Local.Phase(a6)
	bne.w valid
	move.l #PHASE_LOCALS, Local.Phase(a6)
	bra.w phase
valid
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
	bra.w done
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
	bra.w done
storage
	moveq #store.STORAGE_FAILURE, d0
done
	tst.l d0
	rts
	.bend  ; build
; Bind owner-controlled spans into the current import exposure frame.
exposureFrame	.block
	lea Local.Exposure(a6), a0
	move.l State.CallerScope(a4), (a0)  ; TemplateExposureFrame.Scope (zero offset)
	move.l Local.Name(a6), imports.TemplateExposureFrame.DefinitionId(a0)
	move.l Local.Keyword(a6), imports.TemplateExposureFrame.OriginalKey(a0)
	move.l Local.KeywordBytes(a6), imports.TemplateExposureFrame.OriginalKeyBytes(a0)
	lea Local.Cursor(a6), a1
	move.l a1, imports.TemplateExposureFrame.Cursor(a0)
	move.l State.Keys+memory.Block.Pointer(a4), imports.TemplateExposureFrame.KeyArena(a0)
	move.l State.Keys+memory.Block.Capacity(a4), imports.TemplateExposureFrame.KeyCapacity(a0)
	move.l State.Keys+memory.Block.Used(a4), imports.TemplateExposureFrame.KeyUsed(a0)
	lea Local.Exposed(a6), a1
	move.l a1, imports.TemplateExposureFrame.RowOutput(a0)
	move.l #imports.EXPOSURE_ROW_BYTES, imports.TemplateExposureFrame.RowCapacity(a0)
	move.l Local.Visibility(a6), imports.TemplateExposureFrame.Visibility(a0)
	rts
	.bend  ; exposureFrame
; Append a retained overload and its order. Allocation failure publishes no row.
append	.block
	move.l State.RowCount(a4), d0
	cmpi.l #memory.LIMIT/abi.SCOPE_ROW_BYTES, d0
	bhs.w overflow
	addq.l #1, d0
	lsl.l #4, d0  ; abi.SCOPE_ROW_BYTES
	lea State.Rows(a4), a0
	jsr memory.reserve
	bne.w storage
	move.l State.RowCount(a4), d0
	addq.l #1, d0
	lsl.l #2, d0  ; IMPORT_ORDER_BYTES
	lea State.ImportOrders(a4), a0
	jsr memory.reserve
	bne.w storage
	move.l State.RowCount(a4), d0
	lsl.l #2, d0
	movea.l State.ImportOrders+memory.Block.Pointer(a4), a1
	adda.l d0, a1
	move.l Local.Exposed+imports.ExposureRow.ImportOrder(a6), (a1)
	move.l State.RowCount(a4), d0
	lsl.l #4, d0
	movea.l State.Rows+memory.Block.Pointer(a4), a0
	adda.l d0, a0
	move.l Local.Exposed+imports.ExposureRow.KeyOffset(a6), (a0)  ; ScopeRow.KeywordOffset (zero offset)
	move.l Local.Exposed+imports.ExposureRow.KeyBytes(a6), abi.ScopeRow.KeywordBytes(a0)
	move.l Local.Handle(a6), d0
	subq.l #1, d0
	move.l d0, abi.ScopeRow.PlanId(a0)
	move.l Local.Template(a6), abi.ScopeRow.TemplateId(a0)
	addq.l #1, State.RowCount(a4)
	bsr.w rowExtents
	moveq #abi.OK, d0
	rts
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
	rts
storage
	moveq #store.STORAGE_FAILURE, d0
	rts
	.bend  ; append
; D0/status,D1=append boolean. Same-key imported rows retain only maximum order.
; One plan exposed twice under the same key/order is a spelling duplicate.
group	.block
	moveq #0, d7
next
	cmp.l State.RowCount(a4), d7
	bhs.w append
	bsr.w tick
	bne.w done
	move.l d7, d0
	lsl.l #4, d0
	movea.l State.Rows+memory.Block.Pointer(a4), a3
	adda.l d0, a3
	move.l abi.ScopeRow.KeywordBytes(a3), d0
	cmp.l Local.Exposed+imports.ExposureRow.KeyBytes(a6), d0
	bne.w advance
	movea.l State.Keys+memory.Block.Pointer(a4), a0
	movea.l a0, a1
	adda.l (a3), a0  ; ScopeRow.KeywordOffset (zero offset)
	adda.l Local.Exposed+imports.ExposureRow.KeyOffset(a6), a1
	movea.l (a6), a2  ; Local.Policy (zero offset)
	lea State.RemainingBudget(a4), a3
	jsr text.equal
	bne.w done
	tst.l d1
	beq.w advance
	move.l d7, d0
	lsl.l #2, d0
	movea.l State.ImportOrders+memory.Block.Pointer(a4), a0
	move.l 0(a0,d0.l), d0
	cmp.l Local.Exposed+imports.ExposureRow.ImportOrder(a6), d0
	bhi.w skip
	blo.w remove
	move.l d7, d0
	lsl.l #4, d0
	movea.l State.Rows+memory.Block.Pointer(a4), a0
	adda.l d0, a0
	move.l Local.Handle(a6), d0
	subq.l #1, d0
	cmp.l abi.ScopeRow.PlanId(a0), d0
	beq.w skip
advance
	addq.l #1, d7
	bra.w next
remove
	bsr.w removeRow
	bne.w done
	bra.w next
append
	moveq #1, d1
	moveq #abi.OK, d0
	bra.w done
skip
	moveq #0, d1
	moveq #abi.OK, d0
done
	tst.l d0
	rts
	.bend  ; group
; D7=index. Compact parallel rows/orders; failed staging is never Prepared.
removeRow	.block
	movem.l d1-d6/a0-a3, -(sp)
	move.l d7, d6
	addq.l #1, d6
next
	cmp.l State.RowCount(a4), d6
	bhs.w removed
	bsr.w tick
	bne.w done
	move.l d6, d0
	lsl.l #4, d0
	movea.l State.Rows+memory.Block.Pointer(a4), a0
	adda.l d0, a0
	lea -abi.SCOPE_ROW_BYTES(a0), a1
	moveq #abi.SCOPE_ROW_BYTES/4-1, d0
copy
	move.l (a0)+, (a1)+
	dbra d0, copy
	move.l d6, d0
	lsl.l #2, d0
	movea.l State.ImportOrders+memory.Block.Pointer(a4), a0
	adda.l d0, a0
	move.l (a0), -IMPORT_ORDER_BYTES(a0)
	addq.l #1, d6
	bra.w next
removed
	subq.l #1, State.RowCount(a4)
	bsr.w rowExtents
	moveq #abi.OK, d0
done
	tst.l d0
	movem.l (sp)+, d1-d6/a0-a3
	rts
	.bend  ; removeRow
rowExtents	.block
	move.l State.RowCount(a4), d0
	lsl.l #4, d0
	move.l d0, State.Rows+memory.Block.Used(a4)
	move.l State.RowCount(a4), d0
	lsl.l #2, d0
	move.l d0, State.ImportOrders+memory.Block.Used(a4)
	rts
	.bend  ; rowExtents
; Charge one bounded owner operation; shared PRVM calls use this SAME cell.
tick	.block
	move.l a0, -(sp)
	lea State.RemainingBudget(a4), a0
	jsr inputs.tick
	movea.l (sp)+, a0
	rts
	.bend  ; tick
