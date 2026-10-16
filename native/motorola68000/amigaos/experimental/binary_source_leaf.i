; Optional prepared declaration cache; included only by binary_app.
; Acquisition stays eager. A hit replaces one module body's semantic replay;
; ordinary module open/close, graph order and finalization remain authoritative.
; Cold candidates retain offsets only until global binding is finalized.
LeafPendingRow	.struct
Module	.long ?
Records	.long ?
RecordBytes	.long ?
CaptureBytes	.long ?
Origin	.long ?
.endstruct
LEAF_PENDING_BYTES = LeafPendingRow.Origin+4

; Line collection outputs D0=status,D1=bytes,D2=consumed physical bytes.
; Preserve all registers. Extent framing distinguishes final LF/EOF and paths.
leafHashLine	.block
	tst.l LeafDirectory
	beq.w done
	movem.l d0-d2/a0-a1, -(sp)
	move.l d2, -(sp)
	lea LeafKey, a0
	movea.l sp, a1
	moveq #4, d0
	jsr leaf_key.update
	addq.l #4, sp
	move.l d1, d0
	movea.l LineBuffer, a1
	jsr leaf_key.update
	movem.l (sp)+, d0-d2/a0-a1
done
	rts
	.bend  ; leafHashLine

; Preserve all registers; input paths are part of the conservative fingerprint.
leafHashPath	.block
	tst.l LeafDirectory
	beq.w done
	movem.l d0/a0-a1, -(sp)
	lea SourcePath, a1
	moveq #0, d0
length
	tst.b 0(a1, d0.l)
	beq.w mix
	addq.l #1, d0
	bra.w length
mix
	lea LeafKey, a0
	jsr leaf_key.update
	movem.l (sp)+, d0/a0-a1
done
	rts
	.bend  ; leafHashPath

leafSpanBegin	.block
	tst.l LeafDirectory
	beq.w done
	clr.l LeafBodyModule
	clr.l LeafWarm
	clr.l LeafStarted
	lea LeafObject, a0
	jsr leaf_object.release
done
	rts
	.bend  ; leafSpanBegin

; Snapshot only an enabled experiment's ordinary replay boundary.
; Registers preserved, CCR unspecified.
leafBeforeRecord	.block
	tst.l LeafDirectory
	beq.w done
	movem.l d0-d2/a0-a2, -(sp)
	clr.l LeafCpu
	lea Front, a0
	movea.l frontend.Frame.Scratch(a0), a0
	moveq #0, d0
	move.w frontend.SCOPE_STATE+layout.State.Current(a0), d0
	move.l d0, LeafBeforeModule
	move.l Records+memory.Block.Used, LeafBeforeRecords
	tst.l d0
	beq.w restore
	tst.l LeafStarted
	bne.w restore
	; Package-owned numeric head identity recognizes the ordinary CPU prefix.
	; Preview uses the existing shared token binder, never a spelling parser.
	lea Front, a0
	movea.l a4, a1
	lea PeekRecord, a2
	jsr frontend.previewCapture
	bne.w restore
	cmpi.l #source.RECORD_HEADER_BYTES+5, d1
	blo.w restore
	cmpi.b #7, source.RECORD_HEADER_BYTES(a1)
	bne.w restore
	moveq #0, d0
	move.w source.RECORD_HEADER_BYTES+2(a1), d0
	movea.l Front+frontend.Frame.Package, a0
	cmp.w package.Header.CpuDirective(a0), d0
	bne.w restore
	move.l #1, LeafCpu
restore
	movem.l (sp)+, d0-d2/a0-a2
done
	rts
	.bend  ; leafBeforeRecord

; D0/CCR=status; other registers preserved. Cache failures before publication
; are misses. Failure after dictionary publication aborts assembly explicitly.
leafAfterRecord	.block
	tst.l LeafDirectory
	beq.w disabled
	movem.l d1-d7/a0-a6, -(sp)
	tst.l ScheduleDerived
	bne.w good
	lea Front, a0
	movea.l frontend.Frame.Scratch(a0), a4
	adda.l #frontend.SCOPE_STATE, a4
	moveq #0, d7
	move.w layout.State.Current(a4), d7
	tst.l LeafBeforeModule
	bne.w closing
	tst.l d7
	beq.w good
	; An ordinary .module header has established this run's module identity.
	move.l d7, LeafBodyModule
	clr.l LeafStarted
	bra.w good
prefix
	tst.l LeafCpu
	beq.w good
	tst.l LeafStarted
	bne.w good
	move.l #1, LeafStarted
	move.l OriginId, LeafBodyOrigin
	move.l Records+memory.Block.Used, LeafBodyRecords
	move.l SourceCursorData+SourceCursorState.Next, LeafBodyCapture
	clr.l LeafWarm
	.MEMORY_COUNTER_INC LeafRequests
	tst.l LeafDynamicSource
	bne.w good
	bsr.w leafIoArguments
	jsr leaf_io.load
	bne.w good
	lea LeafIo+leaf_io.State.Buffer, a0
	lea LeafObject+leaf_object.Frame.Object, a1
	move.l (a0), (a1)
	move.l memory.Block.Capacity(a0), memory.Block.Capacity(a1)
	move.l memory.Block.Used(a0), memory.Block.Used(a1)
	clr.l (a0)
	clr.l memory.Block.Capacity(a0)
	clr.l memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a1), a5
	movea.l a5, a0
	move.l memory.Block.Used(a1), d0
	jsr leaf_object.validate
	bne.w miss
	move.l leaf_object.Header.CaptureBytes(a5), d0
	add.l LeafBodyCapture, d0
	bcs.w miss
	cmp.l ScheduleEnd, d0
	bhs.w miss  ; footer still belongs to ordinary source execution
	move.l leaf_object.Header.CaptureBytes(a5), d0
	bsr.w leafBodyValidate
	bne.w miss
	cmp.l leaf_object.Header.Count(a5), d2
	bne.w miss
	move.l a4, LeafObject+leaf_object.Frame.Scope
	lea LeafObject, a0
	jsr leaf_object.beginImport
	cmpi.l #1, d0
	beq.w miss
	tst.l d0
	bne.w bad
	moveq #0, d6
warmRow
	cmp.l leaf_object.Header.Count(a5), d6
	bhs.w warmDone
	lea LeafObject, a0
	move.l d6, d1
	movea.l Front+frontend.Frame.Output, a1
	jsr leaf_object.record
	bne.w bad
	moveq #0, d0
	move.w source.RecordHeader.Line(a1), d0
	move.l d0, SourceLine
	move.l d1, d0
	lea Front, a0
	move.l LeafBodyModule, frontend.Frame.GraphBefore(a0)
	jsr frontend.data
	bne.w bad
	bsr.w appendPrepared
	bne.w bad
	addq.l #1, d6
	bra.w warmRow
warmDone
	move.l leaf_object.Header.CaptureBytes(a5), d0
	add.l LeafBodyCapture, d0
	move.l d0, SourceCursorData+SourceCursorState.Next
	move.l #1, LeafWarm
	.MEMORY_COUNTER_INC LeafHits
	.MEMORY_COUNTER_ADD LeafRows, d6
	bra.w dispose
closing
	tst.l d7
	bne.w prefix
	tst.l LeafBodyModule
	beq.w good
	tst.l LeafStarted
	beq.w dispose
	tst.l LeafWarm
	bne.w dispose
	; Do not cache modules with imports, configured parameters or templates.
	move.l LeafBodyModule, d0
	subq.l #1, d0
	add.l d0, d0
	movea.l layout.IMPORT_STATE+imports.HEADS_POINTER(a4), a0
	tst.w 0(a0, d0.l)
	bne.w dispose
	move.l SourceCursorData+SourceCursorState.Offset, d0
	sub.l LeafBodyCapture, d0
	bcs.w dispose
	move.l d0, d5
	bsr.w leafBodyValidate
	bne.w dispose
	; Do not snapshot values before scopes.finish has resolved every spelling.
	; Pending rows borrow no arena pointers and survive ordinary record growth.
	lea LeafPending, a0
	move.l memory.Block.Used(a0), d6
	move.l d6, d0
	addi.l #LEAF_PENDING_BYTES, d0
	jsr memory.reserve
	bne.w dispose  ; an optional cache allocation never breaks ordinary assembly
	movea.l memory.Block.Pointer(a0), a1
	adda.l d6, a1
	move.l LeafBodyModule, LeafPendingRow.Module(a1)
	move.l LeafBodyRecords, LeafPendingRow.Records(a1)
	move.l LeafBeforeRecords, d0
	sub.l LeafBodyRecords, d0
	move.l d0, LeafPendingRow.RecordBytes(a1)
	move.l d5, LeafPendingRow.CaptureBytes(a1)
	move.l LeafBodyOrigin, LeafPendingRow.Origin(a1)
	addi.l #LEAF_PENDING_BYTES, memory.Block.Used(a0)
dispose
	lea LeafObject, a0
	jsr leaf_object.release
	bra.w good
miss
	lea LeafObject, a0
	jsr leaf_object.release
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
disabled
	moveq #0, d0
	rts
	.bend  ; leafAfterRecord

; D0=proposed physical body extent. Verify a complete declaration-only stream
; through shared role selection, including the exact original module footer.
; D0/CCR=status,D2=physical rows. Other registers preserved. This deliberately
; rejects includes, templates, imports and control flow even when consumed.
leafBodyValidate	.block
	movem.l d1/d3-d7/a0-a6, -(sp)
	move.l LeafBodyCapture, d5
	move.l d5, d6
	add.l d0, d6
	bcs.w bad
	cmp.l ScheduleEnd, d6
	bhs.w bad
	moveq #0, d4  ; struct depth
	moveq #0, d7
next
	move.l d5, d1
	bsr.w locateCapture
	bne.w bad
	bsr.w captureView
	bne.w bad
	jsr capture.resolve
	bne.w bad
	movea.l a1, a5
	move.l capture.Record.Origin(a5), d0
	cmp.l LeafBodyOrigin, d0
	bne.w bad
	; Decline position-sensitive leaves before source preparation can fold a
	; field extent. '*' is ambiguous with multiplication, so this deliberately
	; conservative first boundary declines either spelling rather than parsing.
	moveq #0, d3
	move.b capture.Record.Count(a5), d3
	lea capture.Record.Tokens(a5), a0
position
	tst.l d3
	beq.w classified
	cmpi.w #tokenizer.TK_KIND_DOLLAR, source.Token.Kind(a0)
	beq.w bad
	cmpi.w #tokenizer.TK_KIND_OP_MULTIPLY, source.Token.Kind(a0)
	beq.w bad
	adda.w #source.TOKEN_BYTES, a0
	subq.l #1, d3
	bra.w position
classified
	lea ConfigScan, a0
	jsr configuration.captureRole
	bne.w bad
	cmp.l d6, d5
	beq.w footer
	bhi.w bad
	cmpi.l #configuration.ASSIGNMENT, d1
	beq.w advance
	cmpi.l #scopes.KEY_STRUCT, d1
	beq.w open
	cmpi.l #scopes.KEY_ENDSTRUCT, d1
	beq.w close
	cmpi.l #scopes.KEY_PUB, d1
	beq.w visibility
	cmpi.l #scopes.KEY_PRIV, d1
	beq.w visibility
	tst.l d4
	beq.w outside
	; Struct field directives retain package parsing/geometry on the cold path.
	tst.l d1
	beq.w advance
	cmpi.l #scopes.KEY_DB, d1
	beq.w advance
	cmpi.l #scopes.KEY_DW, d1
	beq.w advance
	bra.w bad
outside
	tst.l d1
	bne.w bad
	tst.b capture.Record.Count(a5)
	bne.w bad
	bra.w advance
open
	tst.l d4
	bne.w bad
	moveq #1, d4
	bra.w advance
close
	tst.l d4
	beq.w bad
	moveq #0, d4
	bra.w advance
visibility
	tst.l d4
	bne.w bad
advance
	add.l capture.Record.Bytes(a5), d5
	bcs.w bad
	addq.l #1, d7
	bra.w next
footer
	tst.l d4
	bne.w bad
	cmpi.l #scopes.KEY_ENDMODULE, d1
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l d7, d2
	movem.l (sp)+, d1/d3-d7/a0-a6
	tst.l d0
	rts
	.bend  ; leafBodyValidate

; Set the borrowed arguments for leaf_io.load/save. Clobbers D1-D3/A0-A2.
leafIoArguments	.block
	lea LeafIo, a0
	move.l DosBase, leaf_io.State.Dos(a0)
	move.l LeafDirectory, leaf_io.State.Directory(a0)
	lea SourcePath, a1
	move.l LeafKey+leaf_key.State.High, d1
	move.l LeafKey+leaf_key.State.Low, d2
	movea.l Front+frontend.Frame.Package, a2
	move.l package.Header.Bytes(a2), d3
	rts
	.bend  ; leafIoArguments

; Publish closed declaration candidates only after frontend.complete resolves
; dotted/member/import identities. No candidate borrows pointers during replay.
; All registers preserved; optional export/IO failures leave assembly unchanged.
leafPublish	.block
	tst.l LeafDirectory
	beq.w done
	movem.l d0-d7/a0-a6, -(sp)
	; A replay-time include changes the conservative source fingerprint. Such
	; builds stay eager until transitive acquisition has a stable cache key.
	tst.l LeafDynamicSource
	bne.w restore
	moveq #0, d7
next
	cmp.l LeafPending+memory.Block.Used, d7
	bhs.w restore
	movea.l LeafPending+memory.Block.Pointer, a5
	adda.l d7, a5
	movea.l Front+frontend.Frame.Scratch, a4
	adda.l #frontend.SCOPE_STATE, a4
	move.l a4, LeafObject+leaf_object.Frame.Scope
	move.l LeafPendingRow.Module(a5), LeafObject+leaf_object.Frame.Module
	move.l LeafPendingRow.Module(a5), LeafBodyModule
	movea.l Records+memory.Block.Pointer, a0
	adda.l LeafPendingRow.Records(a5), a0
	move.l a0, LeafObject+leaf_object.Frame.Records
	move.l LeafPendingRow.RecordBytes(a5), LeafObject+leaf_object.Frame.RecordBytes
	move.l LeafPendingRow.CaptureBytes(a5), LeafObject+leaf_object.Frame.CaptureBytes
	lea LeafObject, a0
	jsr leaf_object.export
	.MEMORY_PROGRESS DosBase, #342, LeafBodyModule, leaf_object.DiagnosticStage, #0
	bne.w dispose
	bsr.w leafIoArguments
	; Use the retained origin path without changing current diagnostics/context.
	move.l LeafPendingRow.Origin(a5), d0
	lsl.l #8, d0
	movea.l OriginPaths+memory.Block.Pointer, a1
	adda.l d0, a1
	movea.l LeafObject+leaf_object.Frame.Object+memory.Block.Pointer, a3
	move.l LeafObject+leaf_object.Frame.Object+memory.Block.Used, d4
	jsr leaf_io.save
	.MEMORY_SETUP_RESULT DosBase, #344
	bne.w dispose
	.MEMORY_COUNTER_INC LeafWrites
dispose
	lea LeafObject, a0
	jsr leaf_object.release
	addi.l #LEAF_PENDING_BYTES, d7
	bra.w next
restore
	movem.l (sp)+, d0-d7/a0-a6
done
	rts
	.bend  ; leafPublish
