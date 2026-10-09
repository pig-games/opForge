; Invocation ownership and iteration overlays keyed by stable trace-row offsets.
; @opforge-owner: experimental.amigaos.binary_repeat_frames
	.module experimental.amigaos.binary_repeat_frames
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_repeat_inventory as inventory
	.use experimental.amigaos.binary_repeat_cells as cells
	.use experimental.amigaos.binary_repetition_trace as trace
	.use experimental.amigaos.binary_dependency_layout as graph
	.use experimental.amigaos.binary_dependencies as dependencies
	.pub
Group	.struct
Owner	.word ?
Declarations	.word ?
Iterations	.long ?
Cells	.long ?  ; first iteration cell offset
Saved	.long ?  ; enclosing lexical values
	.endstruct
GROUP_BYTES = Group.Saved+4
DYNAMIC_ITERATIONS = -1
DYNAMIC_CELLS = -1
Cursor	.struct
First	.long ?  ; first forward-linked dynamic row, -1 when empty
Current	.long ?  ; replay cursor row offset
Index	.long ?  ; replay cursor iteration
Reserved	.long ?
	.endstruct
CURSOR_BYTES = Cursor.Reserved+4
Row	.struct
Next	.long ?
Cells	.long ?
	.endstruct
ROW_BYTES = Row.Cells+4
State	.struct
Inventory	.long ?
Context	.long ?
Records	.long ?
RecordBytes	.long ?
Pass	.long ?
Groups	.res memory.Block.Used+4
Cursors	.res memory.Block.Used+4
Rows	.res memory.Block.Used+4
Cells	.res cells.STATE_BYTES
Graph	.res graph.FRAME_BYTES
	.endstruct
STATE_BYTES = State.Graph+graph.FRAME_BYTES
	.section code, kind=code
; A0=state,A1=prepared inventory,A2=context,A3=records,D0=bytes.
; Registers preserved; CCR unspecified. Existing storage must be released first.
prepare	.block
	move.l a1, State.Inventory(a0)
	move.l a2, State.Context(a0)
	move.l a3, State.Records(a0)
	move.l d0, State.RecordBytes(a0)
	rts
	.bend  ; prepare

; A0=state,D0=pass1/2. D0/CCR=status; other registers preserved.
; Retry discards provisional ownership/cells alongside the observation trace.
beginPass	.block
	cmpi.l #1, d0
	beq.w capture
	cmpi.l #2, d0
	bne.w bad
	move.l d0, State.Pass(a0)
	moveq #0, d0
	rts
capture
	move.l d0, State.Pass(a0)
	clr.l State.Groups+memory.Block.Used(a0)
	clr.l State.Cursors+memory.Block.Used(a0)
	clr.l State.Rows+memory.Block.Used(a0)
	move.l a0, -(sp)
	lea State.Cells(a0), a0
	jsr cells.reset
	movea.l (sp)+, a0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; beginPass

; A0=state,D0=trace row offset,D1=header offset,D2=iteration count or DYNAMIC_ITERATIONS.
; D0/CCR=status,D3=group byte offset. Other registers preserved.
; Groups map directly from trace offsets; dynamic counts retain forward-linked rows.
open	.block
	movem.l d1-d2/d4-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d0, d4
	move.l d2, d7
	andi.l #trace.RECORD_BYTES-1, d0
	bne.w bad
	cmpi.l #memory.LIMIT-GROUP_BYTES, d4
	bhi.w bad
	move.l d1, d0
	movea.l State.Inventory(a6), a0
	jsr inventory.find
	bne.w done
	cmpi.l #65535, d2
	bhi.w bad
	move.l d1, d5
	move.l d2, d6
	cmpi.l #1, State.Pass(a6)
	bne.w compare
	lea State.Groups(a6), a0
	move.l memory.Block.Used(a0), d2
	move.l d4, d0
	addi.l #GROUP_BYTES, d0
	move.l d0, d1
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a1
	adda.l d2, a1
	move.l d1, d0
	sub.l d2, d0
	bcs.w bad
	move.l d1, memory.Block.Used(a0)
	tst.l d0
	beq.w allocated
clear
	clr.l (a1)+
	subq.l #4, d0
	bne.w clear
allocated
	movea.l memory.Block.Pointer(a0), a5
	adda.l d4, a5
	move.w d5, Group.Owner(a5)
	move.w d6, Group.Declarations(a5)
	move.l d7, Group.Iterations(a5)
	clr.l Group.Cells(a5)
	cmpi.l #DYNAMIC_ITERATIONS, d7
	bne.w fixedAllocation
	move.l #DYNAMIC_CELLS, Group.Cells(a5)
	clr.l Group.Iterations(a5)
	bsr.w createCursor
	bne.w done
	moveq #0, d7
fixedAllocation
	lea State.Cells(a6), a0
	move.l d7, d1
	move.l d6, d2
	jsr cells.append
	bne.w done
	move.l d3, Group.Saved(a5)
	tst.l Group.Cells(a5)
	bmi.w save
	move.l d6, d0
	lsl.l #cells.CELL_SHIFT, d0
	add.l d3, d0
	move.l d0, Group.Cells(a5)
	bra.w save
compare
	move.l d4, d0
	addi.l #GROUP_BYTES, d0
	cmp.l State.Groups+memory.Block.Used(a6), d0
	bhi.w bad
	movea.l State.Groups+memory.Block.Pointer(a6), a5
	adda.l d4, a5
	cmp.w Group.Owner(a5), d5
	bne.w bad
	cmp.w Group.Declarations(a5), d6
	bne.w bad
	cmpi.l #DYNAMIC_ITERATIONS, d7
	beq.w compareDynamic
	tst.l Group.Cells(a5)
	bmi.w bad
	cmp.l Group.Iterations(a5), d7
	bne.w bad
	bra.w save
compareDynamic
	tst.l Group.Cells(a5)
	bpl.w bad
save
	tst.l Group.Cells(a5)
	bpl.w saveCells
	bsr.w locateCursor
	bne.w done
	moveq #-1, d0
	move.l d0, Cursor.Current(a3)
	move.l d0, Cursor.Index(a3)
saveCells
	move.l Group.Saved(a5), d0
	moveq #0, d2
	bsr.w transfer
	bne.w done
	move.l d4, d3
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/d4-d7/a0-a6
	tst.l d0
	rts
	.bend  ; open

; A0=state,D0=group offset,D1=iteration. Restore retained iteration cells,
; then solve directly-owned immutable roots. D0/CCR=status; others preserved.
enter	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d0, d7
	bsr.w locate
	bne.w done
	tst.l Group.Cells(a5)
	bpl.w enterFixed
	bsr.w locateCursor
	bne.w done
	bsr.w advance
	bne.w done
enterFixed
	move.l d7, d0
	bsr.w iteration
	bne.w done
	moveq #1, d2
	bsr.w transfer
	bne.w restoreFailure
	bsr.w graphFrame
	lea State.Graph(a6), a0
	jsr dependencies.resolveLocal
	beq.w done
restoreFailure
	move.l d0, d7
	move.l Group.Saved(a5), d0
	moveq #1, d2
	bsr.w transfer
	move.l d7, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; enter

; A0=state,D0=group offset,D1=selected branch record offset.
; D0/CCR=status; others preserved. Live iteration cells and private graph states
; were initialized by enter; activation must not reset other branch roots.
activate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d1, d7
	bsr.w locate
	bne.w done
	bsr.w graphFrame
	lea State.Graph(a6), a0
	move.l d7, graph.Frame.Branch(a0)
	jsr dependencies.activateLocal
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; activate

; A0=state,D0=group offset,D1=iteration. Retain complete iteration values.
; D0/CCR=status; other registers preserved.
capture	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	bsr.w iteration
	bne.w done
	moveq #0, d2
	bsr.w transfer
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; capture

; A0=state,D0=group offset. Restore enclosing values at close/failed replay.
; D0/CCR=status; other registers preserved.
close	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	bsr.w locate
	bne.w done
	move.l Group.Saved(a5), d0
	moveq #1, d2
	bsr.w transfer
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; close

; A0=state after all active overlays were closed. Preserve caller registers;
; CCR reflects caller D0. No partial group or retained cell survives cleanup.
release	.block
	move.l a0, -(sp)
	lea State.Groups(a0), a0
	jsr memory.release
	movea.l (sp), a0
	lea State.Cursors(a0), a0
	jsr memory.release
	movea.l (sp), a0
	lea State.Rows(a0), a0
	jsr memory.release
	movea.l (sp), a0
	lea State.Cells(a0), a0
	jsr cells.release
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; release
	.priv
; A6=state,D0=group offset. D0/CCR=status,A5=validated group.
locate	.block
	move.l d0, d3
	andi.l #GROUP_BYTES-1, d3
	bne.w bad
	move.l d0, d3
	addi.l #GROUP_BYTES, d3
	bcs.w bad
	cmp.l State.Groups+memory.Block.Used(a6), d3
	bhi.w bad
	movea.l State.Groups+memory.Block.Pointer(a6), a5
	adda.l d0, a5
	tst.w Group.Owner(a5)
	beq.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; locate

; A6=state,D0=group offset,D1=iteration. D0=cell offset or status on error.
; CCR zero on success. A5=group,D3/D4 scratch.
iteration	.block
	bsr.w locate
	bne.w bad
	cmp.l Group.Iterations(a5), d1
	bhs.w bad
	tst.l Group.Cells(a5)
	bpl.w fixed
	bsr.w locateCursor
	bne.w bad
	cmp.l Cursor.Index(a3), d1
	bne.w bad
	move.l Cursor.Current(a3), d3
	bsr.w locateRow
	bne.w bad
	move.l Row.Cells(a4), d0
	moveq #0, d3
	tst.l d3
	rts
fixed
	moveq #0, d3
	move.w Group.Declarations(a5), d3
	move.l d1, d4
	mulu.l d3, d4  ; bounded by open's checked cell allocation
	lsl.l #cells.CELL_SHIFT, d4
	move.l Group.Cells(a5), d0
	add.l d4, d0
	moveq #0, d3
	tst.l d3
	rts
bad
	moveq #1, d0
	rts
	.bend  ; iteration

; A6=state,A5=dynamic group,D1=next iteration. Append zeroed capture cells
; in pass one; replay the retained forward link in pass two. D0/CCR=status.
; A3=cursor sidecar. Clobbers D2-D4/A0/A4; duplicate entry is invalid.
advance	.block
	move.l Cursor.Index(a3), d3
	addq.l #1, d3
	cmp.l d3, d1
	bne.w bad
	cmpi.l #trace.MODE_CAPTURE, State.Pass(a6)
	bne.w compare
	cmp.l Group.Iterations(a5), d1
	bne.w bad
	moveq #0, d2
	move.w Group.Declarations(a5), d2
	move.l d1, -(sp)
	moveq #0, d1
	lea State.Cells(a6), a0
	jsr cells.append
	move.l (sp)+, d1
	tst.l d0
	bne.w done
	move.l d3, d4
	lea State.Rows(a6), a0
	move.l memory.Block.Used(a0), d2
	move.l d2, d0
	addi.l #ROW_BYTES, d0
	jsr memory.reserve
	bne.w done
	move.l d2, d0
	addi.l #ROW_BYTES, d0
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a4
	adda.l d2, a4
	move.l #-1, Row.Next(a4)
	move.l d4, Row.Cells(a4)
	move.l Cursor.Current(a3), d3
	bmi.w first
	bsr.w locateRow
	bne.w done
	move.l d2, Row.Next(a4)
	bra.w appended
first
	move.l d2, Cursor.First(a3)
appended
	addq.l #1, Group.Iterations(a5)
	bra.w ready
compare
	cmp.l Group.Iterations(a5), d1
	bhs.w bad
	move.l Cursor.First(a3), d2
	move.l Cursor.Current(a3), d3
	bmi.w ready
	bsr.w locateRow
	bne.w done
	move.l Row.Next(a4), d2
ready
	move.l d2, d3
	bsr.w locateRow
	bne.w done
	move.l d2, Cursor.Current(a3)
	move.l d1, Cursor.Index(a3)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; advance

; A6=state,A5=group,D4=trace/group offset. Allocate a dynamic-only
; cursor at the same offset; fixed groups retain their original capacity.
; D0/CCR=status; clobbers D1-D3/A0/A3. Group storage cannot relocate here.
createCursor	.block
	lea State.Cursors(a6), a0
	move.l memory.Block.Used(a0), d2
	move.l d4, d0
	addi.l #CURSOR_BYTES, d0
	move.l d0, d1
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a3
	adda.l d2, a3
	move.l d1, d3
	sub.l d2, d3
	bcs.w bad
	move.l d1, memory.Block.Used(a0)
clear
	tst.l d3
	beq.w allocated
	clr.l (a3)+
	subq.l #4, d3
	bra.w clear
allocated
	movea.l memory.Block.Pointer(a0), a3
	adda.l d4, a3
	move.l #-1, Cursor.First(a3)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; createCursor

; A6=state,A5=group. D0/CCR=status,A3=cursor; clobbers D3.
; Both tables use trace offsets and 16-byte rows, preserving their full bound.
locateCursor	.block
	move.l a5, d3
	sub.l State.Groups+memory.Block.Pointer(a6), d3
	move.l d3, d0
	addi.l #CURSOR_BYTES, d0
	bcs.w bad
	cmp.l State.Cursors+memory.Block.Used(a6), d0
	bhi.w bad
	movea.l State.Cursors+memory.Block.Pointer(a6), a3
	adda.l d3, a3
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; locateCursor

; A6=state,D3=row offset. D0/CCR=status,A4=row; other registers preserved.
locateRow	.block
	move.l d3, d0
	andi.l #ROW_BYTES-1, d0
	bne.w bad
	move.l d3, d0
	addi.l #ROW_BYTES, d0
	bcs.w bad
	cmp.l State.Rows+memory.Block.Used(a6), d0
	bhi.w bad
	movea.l State.Rows+memory.Block.Pointer(a6), a4
	adda.l d3, a4
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; locateRow

; A6=state,A5=group,D0=cell offset,D2=direction. D0/CCR=status.
; D1/A0-A2 scratch; other registers preserved.
transfer	.block
	movea.l State.Inventory(a6), a0
	moveq #0, d1
	move.w Group.Owner(a5), d1
	subq.l #1, d1
	lsl.l #inventory.OWNER_SHIFT, d1
	movea.l inventory.State.Owners+memory.Block.Pointer(a0), a1
	adda.l d1, a1
	move.l inventory.Owner.First(a1), d1
	movea.l inventory.State.Ids+memory.Block.Pointer(a0), a1
	adda.l d1, a1
	moveq #0, d1
	move.w Group.Declarations(a5), d1
	movea.l State.Context(a6), a2
	lea State.Cells(a6), a0
	jsr cells.transfer
	rts
	.bend  ; transfer

; A6=state,A5=group. Populate the transient shared-DFS call frame.
; Clobbers D0/A0-A2; no retained pointers are persisted into groups/cells.
graphFrame	.block
	lea State.Graph(a6), a2
	move.l State.Context(a6), graph.Frame.Context(a2)
	move.l State.Records(a6), graph.Frame.Records(a2)
	move.l State.RecordBytes(a6), graph.Frame.RecordBytes(a2)
	movea.l State.Inventory(a6), a0
	move.l inventory.State.Declarations+memory.Block.Pointer(a0), graph.Frame.Declarations(a2)
	moveq #0, d0
	move.w Group.Owner(a5), d0
	move.l d0, graph.Frame.Owner(a2)
	subq.l #1, d0
	lsl.l #inventory.OWNER_SHIFT, d0
	movea.l inventory.State.Owners+memory.Block.Pointer(a0), a1
	adda.l d0, a1
	move.l inventory.Owner.First(a1), d0
	movea.l inventory.State.Ids+memory.Block.Pointer(a0), a1
	adda.l d0, a1
	move.l a1, graph.Frame.Ids(a2)
	moveq #0, d0
	move.w Group.Declarations(a5), d0
	move.l d0, graph.Frame.Count(a2)
	rts
	.bend  ; graphFrame
	.endsection
	.endmodule
