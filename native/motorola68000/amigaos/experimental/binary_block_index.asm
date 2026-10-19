; Index named block spans in prepared binary records without source text.
; @opforge-owner: experimental.amigaos.binary_block_index
	.module experimental.amigaos.binary_block_index
	.cpu 68020
	.use experimental.amigaos.binary_lexical_store as lexical
	.include "memory_telemetry.i"
	.include "binding_telemetry.i"
	.include "lazy_audit.i"
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_graph as graph
	.use experimental.amigaos.binary_imports as imports
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_record_view as views
	.use experimental.amigaos.binary_payload_bounds as payloads
	.use prvm.amigaos.abi as abi
	.use experimental.amigaos.binary_materialized_scope as materialized
	.use opasm.amigaos.binary_expression as expr
	.use exprvm.amigaos.runtime as runtime
	.use experimental.amigaos.binary_owner_guards as guards
	.use experimental.amigaos.binary_binding_plans as plans
	.use experimental.amigaos.binary_module_records as compiled
	.pub
LIMIT = 65535; packed parent/queue links remain representable words
	.include "binary_block_span.i"
Scratch	.struct
Count	.long ?
WorkspaceOffset	.long ?  ; relative to the first span
Bytes	.long ?  ; logical stream bound retained with the numeric selection
	.endstruct
HEADER_BYTES = Scratch.Bytes+4
ROW_BYTES = SPAN_BYTES+2; nesting stack becomes the reachability queue
	.section bss, kind=bss
	.priv
View	.res long, 1
RecordPointer	.res long, 1
RecordOffset	.res long, 1
RecordLimit	.res long, 1
BufferView	.res byte, views.FRAME_BYTES
EndRecords	.res long, 1
Spans	.res long, 1
WorkspaceOffset	.res long, 1
Scope	.res long, 1
Graph	.res long, 1
Count	.res word, 1
Head	.res word, 1
Tail	.res word, 1
Owners	.res byte, memory.Block.Used+4
Guards	.res byte, guards.STATE_BYTES
GuardActive	.res word, 1
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.pub
QueueAdds	.res long, 1
Scanned	.res long, 1
Marks	.res long, 1
	.priv
.endif
.endif
	.endsection
	.section code, kind=code
	.pub

; A0=prepared records,D0=bytes,A1=caller-owned memory.Block descriptor.
; Buffer adapter for indexView; the owner retains release responsibility.
; D0/CCR=status,D1=span count; other registers preserved.
index	.block
	movem.l d2/a0-a2, -(sp)
	move.l d0, d2
	movea.l a1, a2
	movea.l a0, a1
	lea BufferView, a0
	jsr views.initBuffer
	bne.w done
	move.l d2, d0
	movea.l a2, a1
	bsr.w indexView
	move.l d0, -(sp)
	lea BufferView, a0
	jsr views.finish
	move.l (sp)+, d0
done
	movem.l (sp)+, d2/a0-a2
	tst.l d0
	rts
	.bend  ; index

; A0=record-view Frame,D0=logical bytes,A1=caller-owned memory.Block.
; Index every named block, failing closed on malformed/unbalanced markers.
; Spans retain logical offsets and numeric parents, never leased pointers.
; D0/CCR=status,D1=span count; other registers preserved. View stays borrowed.
indexView	.block
	movem.l d2-d7/a0-a6, -(sp)
	move.l a0, View
	move.l d0, EndRecords
	cmp.l views.Frame.Bytes(a0), d0
	bne.w bad
	movea.l a1, a4
	bsr.w measure
	bne.w bad
	move.l d1, d7
	mulu.w #ROW_BYTES, d1
	add.l #HEADER_BYTES, d1
	move.l d1, d5
	move.l d1, d0
	movea.l a4, a0
	jsr memory.reserveExact
	bne.w bad
	move.l d5, memory.Block.Used(a4)
	movea.l memory.Block.Pointer(a4), a4
	move.l d7, Scratch.Count(a4)
	move.l EndRecords, Scratch.Bytes(a4)
	mulu.w #SPAN_BYTES, d7
	move.l d7, Scratch.WorkspaceOffset(a4)
	move.l d7, WorkspaceOffset
	adda.l #HEADER_BYTES, a4
	moveq #0, d0
	movea.l d0, a3  ; logical cursor
	moveq #0, d1  ; span count
	moveq #0, d2  ; nesting depth
	moveq #0, d7  ; preceding qualifying label offset+1
line
	cmpa.l EndRecords, a3
	beq.w complete
	move.l a3, d0
	bsr.w record
	bne.w bad
	moveq #0, d4
	move.b source.RecordHeader.Flags(a0), d4
	move.l d4, d5
	andi.w #source.FLAG_BLOCK_OPEN, d5
	beq.w closing
	move.l d1, d5
	mulu.w #SPAN_BYTES, d5
	lea 0(a4, d5.l), a5
	move.l a3, d5
	tst.l d7
	beq.w startReady
	move.l d7, d5
	subq.l #1, d5
startReady
	move.l d5, Span.Start(a5)
	clr.l Span.End(a5)
	moveq #0, d5
	move.b source.DeclarationRecord.Name+source.NameToken.Id(a0), d5
	lsl.w #8, d5
	move.b source.DeclarationRecord.Name+source.NameToken.Id+1(a0), d5
	move.w d5, Span.Entry(a5)
	clr.w Span.Parent(a5)
	clr.w Span.Live(a5)
	tst.w d2
	beq.w push
	move.l d2, d5
	subq.l #1, d5
	add.l d5, d5
	movea.l a4, a1
	adda.l WorkspaceOffset, a1
	moveq #0, d3
	move.w 0(a1, d5.l), d3
	addq.w #1, d3
	move.w d3, Span.Parent(a5)
push
	move.l d2, d5
	add.l d5, d5
	movea.l a4, a1
	adda.l WorkspaceOffset, a1
	move.w d1, 0(a1, d5.l)
	addq.w #1, d2
	addq.w #1, d1
	bra.w next
closing
	move.l d4, d5
	andi.w #source.FLAG_BLOCK_CLOSE, d5
	beq.w next
	subq.w #1, d2
	move.l d2, d5
	add.l d5, d5
	movea.l a4, a1
	adda.l WorkspaceOffset, a1
	moveq #0, d3
	move.w 0(a1, d5.l), d3
	move.l d3, d5
	mulu.w #SPAN_BYTES, d5
	lea 0(a4, d5.l), a5
	move.l a3, d5
	add.l d6, d5
	move.l d5, Span.End(a5)
next
	; Cache only the preceding declaration's offset; the next seek may evict it.
	moveq #0, d7
	cmpi.w #source.DECLARATION_HEADER_BYTES, d6
	bne.w advance
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, source.DeclarationRecord.Name+source.NameToken.Kind(a0)
	bhi.w advance
	cmpi.b #source.TOKEN_LABEL_DECLARATION, source.DeclarationRecord.Marker(a0)
	bne.w advance
	move.l a3, d7
	addq.l #1, d7
advance
	adda.l d6, a3
	bra.w line
complete
	.LAZY_BLOCKS a4, d1
	moveq #0, d0
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; indexView

	.priv
; Validate packed framing and markers before allocation or span writes.
; View/EndRecords describe the stream. D0/CCR=status,D1=span count.
; Other registers preserved. Nesting depth validates balanced markers.
measure	.block
	movem.l d2-d7/a0-a3, -(sp)
	moveq #0, d0
	movea.l d0, a3
	moveq #0, d1
	moveq #0, d2
line
	cmpa.l EndRecords, a3
	beq.w complete
	move.l a3, d0
	bsr.w record
	bne.w bad
	moveq #0, d5
	move.b source.RecordHeader.Flags(a0), d5
	move.l d5, d4
	andi.w #$ff-source.FLAG_ALLOWED, d4
	bne.w bad
	move.l d5, d4
	andi.w #source.FLAG_BLOCK_OPEN, d4
	beq.w closing
	move.l d5, d4
	andi.w #source.FLAG_BLOCK_CLOSE, d4
	bne.w bad
	cmpi.l #source.DECLARATION_HEADER_BYTES, d6
	bne.w bad
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, source.DeclarationRecord.Name+source.NameToken.Kind(a0)
	bhi.w bad
	cmpi.b #source.TOKEN_LABEL_DECLARATION, source.DeclarationRecord.Marker(a0)
	bne.w bad
	cmpi.l #LIMIT, d1
	bhs.w bad
	addq.l #1, d1
	addq.l #1, d2
	bra.w next
closing
	move.l d5, d4
	andi.w #source.FLAG_BLOCK_CLOSE, d4
	beq.w next
	cmpi.l #source.RECORD_HEADER_BYTES, d6
	bne.w bad
	tst.l d2
	beq.w bad
	subq.l #1, d2
next
	adda.l d6, a3
	bra.w line
complete
	tst.l d2
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	tst.l RecordPointer
	beq.w unobserved
	.BINDING_RECORD_OFFSET_FAILURE #binding_diagnostic.BLOCK_INDEX, RecordPointer, RecordLimit, RecordPointer, RecordOffset
unobserved
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a3
	tst.l d0
	rts
	.bend  ; measure

; D0=logical offset. Return A0=leased record,D6=width,D0/CCR=status.
; Other registers preserved. The view owns the lease until its next seek/finish.
record	.block
	movem.l d1-d2/a1, -(sp)
	move.l d0, d1
	move.l d0, RecordOffset
	clr.l RecordPointer
	movea.l View, a0
	jsr views.seek
	bne.w bad
	move.l a1, RecordPointer
	move.l EndRecords, d6
	sub.l d1, d6
	cmp.l d6, d2
	bls.w available
	move.l d6, d2
available
	move.l a1, d6
	add.l d2, d6
	move.l d6, RecordLimit
	cmpi.l #source.RECORD_HEADER_BYTES, d2
	blo.w bad
	moveq #0, d6
	move.b source.RecordHeader.Length(a1), d6
	addq.l #1, d6
	cmpi.l #source.RECORD_HEADER_BYTES, d6
	blo.w bad
	cmp.l d2, d6
	bhi.w bad
	move.l d1, d2
	add.l d6, d2
	bcs.w bad
	cmp.l EndRecords, d2
	bhi.w bad
	move.l a1, RecordPointer
	movea.l a1, a0
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a1
	tst.l d0
	rts
	.bend  ; record
	.pub

; Select blocks from a contiguous buffer and apply omitted flags in place.
; A0=records,D0=bytes,A1=index,D1=span count,A2=scope,A3=graph,A4=package.
; D0/CCR=status; other registers preserved.
select	.block
	movem.l d2/a0-a1/a5, -(sp)
	move.l d0, d2
	movea.l a1, a5
	movea.l a0, a1
	lea BufferView, a0
	jsr views.initBuffer
	bne.w done
	move.l d2, d0
	movea.l a5, a1
	bsr.w selectView
	move.l d0, -(sp)
	lea BufferView, a0
	jsr views.finish
	move.l (sp)+, d0
	bne.w done
	movea.l 4(sp), a0
	movea.l a5, a1
	move.l d2, d0
	bsr.w applySelection
done
	movem.l (sp)+, d2/a0-a1/a5
	tst.l d0
	rts
	.bend  ; select

; Select reachable outer blocks through a dependency-ordered logical view.
; A0=view Frame,D0=bytes,A1=index,D1=count,A2=scope,A3=graph,A4=package.
; Retain numeric Live decisions in the index; applySelection mutates a later
; contiguous materialization. No leased pointer survives a seek.
; D0/CCR=status; other registers preserved. Caller finishes the borrowed view.
selectView	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l a0, View
	move.l d0, EndRecords
	cmp.l views.Frame.Bytes(a0), d0
	bne.w invalid
	cmp.l Scratch.Bytes(a1), d0
	bne.w invalid
	cmpi.l #LIMIT, d1
	bhi.w invalid
	cmp.l Scratch.Count(a1), d1
	bne.w invalid
	move.l d1, d2
	mulu.w #SPAN_BYTES, d2
	cmp.l Scratch.WorkspaceOffset(a1), d2
	bne.w invalid
	move.l d2, WorkspaceOffset
	adda.l #HEADER_BYTES, a1
	move.l a1, Spans
	move.l a2, Scope
	move.l a3, Graph
	move.w d1, Count
	clr.w Head
	clr.w Tail
	.MEMORY_COUNTER_CLEAR QueueAdds
	.MEMORY_COUNTER_CLEAR Scanned
	.MEMORY_COUNTER_CLEAR Marks
	clr.w GuardActive
	move.l a4, -(sp)  ; source-session package survives the predicate call
	movea.l Scope, a0
	lea layout.LEXICAL_STATE(a0), a1
	moveq #0, d0
	moveq #0, d1
	move.w layout.State.Base(a0), d0
	move.w layout.State.Count(a0), d1
	adda.l #scopes.BINDING_PLANS, a0
	jsr plans.needsGuards
	movea.l (sp)+, a3
	bne.w invalid
	tst.l d2
	beq.w guardsReady
	move.w #1, GuardActive
	lea Guards, a0
	movea.l View, a1
	move.l EndRecords, d0
	movea.l Scope, a2
	jsr guards.prepareView
	bne.w invalid
	movea.l Scope, a0
	adda.l #scopes.BINDING_PLANS, a0
	lea Guards, a1
	jsr plans.seal
	bne.w invalid
guardsReady
	bsr.w indexOwners
	bne.w invalid
	moveq #0, d7
root
	cmp.w Count, d7
	bhs.w outside
	move.l d7, d0
	mulu.w #SPAN_BYTES, d0
	movea.l Spans, a4
	adda.l d0, a4
	tst.w Span.Parent(a4)
	bne.w nextRoot
	move.l Span.Start(a4), d0
	lea Guards, a0
	jsr guards.activity
	cmpi.l #guards.INACTIVE, d0
	beq.w nextRoot
	moveq #0, d0
	move.w Span.Entry(a4), d0
	movea.l Scope, a5
	sub.w layout.State.Base(a5), d0
	bcs.w invalid
	cmp.w layout.State.Count(a5), d0
	bhs.w invalid
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.OWNERS_POINTER(a5), a0
	moveq #0, d1
	move.w 0(a0, d0.l), d1
	beq.w liveRoot
	move.w d1, d3
	subq.w #1, d3  ; imports retain the zero-based module binding index
	movea.l Graph, a0
	; Ownership uses source binding IDs; graph nodes are independently dense.
	jsr graph.nodeForBinding
	bne.w invalid
	cmpi.w #1, graph.Node.File(a0)
	bne.w nextRoot
	; An entry-file sibling may also be imported. Import availability alone
	; must not make its blocks roots; only references make them live.
	lea layout.IMPORT_STATE(a5), a0
	moveq #0, d2
	move.w imports.COUNT(a0), d2
	movea.l imports.ITEMS_POINTER(a0), a1
importedRoot
	tst.w d2
	beq.w liveRoot
	cmp.w imports.Item.Target(a1), d3
	beq.w nextRoot
	adda.w #imports.ITEM_BYTES, a1
	subq.w #1, d2
	bra.w importedRoot
liveRoot
	tst.w Span.Live(a4)
	bne.w nextRoot
	move.w #1, Span.Live(a4)
	move.l d7, d0
	bsr.w enqueue
	bne.w invalid
nextRoot
	addq.w #1, d7
	bra.w root
outside
	moveq #0, d0
	movea.l d0, a4
	movea.l EndRecords, a6
	moveq #0, d7  ; outer spans and record offsets both advance in source order
outsideLine
	cmpa.l a6, a4
	beq.w queued
	bhi.w invalid
	move.l a4, d4
owner
	cmp.w Count, d7
	bhs.w outsideScan
	move.l d7, d0
	mulu.w #SPAN_BYTES, d0
	movea.l Spans, a5
	adda.l d0, a5
	tst.w Span.Parent(a5)
	bne.w nextOwner
	cmp.l Span.Start(a5), d4
	blo.w outsideScan
	cmp.l Span.End(a5), d4
	bhs.w nextOwner
	movea.l Span.End(a5), a4
	bra.w outsideLine
nextOwner
	addq.w #1, d7
	bra.w owner
outsideScan
	bsr.w scanLine
	bne.w invalid
	bra.w outsideLine
queued
	moveq #0, d7
	move.w Head, d7
	cmp.w Tail, d7
	bhs.w queueDone
	addq.w #1, Head
	add.l d7, d7
	movea.l Spans, a5
	adda.l WorkspaceOffset, a5
	moveq #0, d0
	move.w 0(a5, d7.l), d0
	movea.l Spans, a5
	mulu.w #SPAN_BYTES, d0
	adda.l d0, a5
	movea.l Span.Start(a5), a4
	movea.l Span.End(a5), a6
blockLine
	cmpa.l a6, a4
	beq.w queued
	bhi.w invalid
	bsr.w scanLine
	bne.w invalid
	bra.w blockLine
queueDone
selected
	.LAZY_SELECTED Spans, Count
	moveq #0, d0
	bra.w doneSelect
invalid
	moveq #1, d0
doneSelect
	lea Guards, a0
	jsr guards.release
	lea Owners, a0
	jsr memory.release
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; selectView

; Apply a retained numeric selection after the logical stream is materialized.
; A0=contiguous records,D0=bytes,A1=index,D1=count. The index stays owned by
; its caller independently of lexical scratch. D0/CCR=status; others preserved.
applySelection	.block
	movem.l d1-d7/a0-a6, -(sp)
	cmp.l Scratch.Count(a1), d1
	bne.w invalid
	cmp.l Scratch.Bytes(a1), d0
	bne.w invalid
	cmpi.l #LIMIT, d1
	bhi.w invalid
	move.l d1, d2
	mulu.w #SPAN_BYTES, d2
	cmp.l Scratch.WorkspaceOffset(a1), d2
	bne.w invalid
	movea.l a0, a2
	add.l a0, d0
	bcs.w invalid
	movea.l d0, a3
	adda.l #HEADER_BYTES, a1
	moveq #0, d7
span
	cmp.l d1, d7
	bhs.w good
	move.l d7, d0
	mulu.w #SPAN_BYTES, d0
	lea 0(a1, d0.l), a5
	tst.w Span.Parent(a5)
	bne.w next
	tst.w Span.Live(a5)
	bne.w next
	movea.l a2, a4
	adda.l Span.Start(a5), a4
	movea.l a2, a6
	adda.l Span.End(a5), a6
	cmpa.l a2, a4
	blo.w invalid
	cmpa.l a3, a6
	bhi.w invalid
line
	cmpa.l a6, a4
	beq.w next
	bhi.w invalid
	move.l a6, d0
	sub.l a4, d0
	cmpi.l #source.RECORD_HEADER_BYTES, d0
	blo.w invalid
	moveq #0, d6
	move.b source.RecordHeader.Length(a4), d6
	addq.w #1, d6
	cmpi.l #source.RECORD_HEADER_BYTES, d6
	blo.w invalid
	cmp.l d0, d6
	bhi.w invalid
	ori.b #source.FLAG_OMIT, source.RecordHeader.Flags(a4)
	adda.w d6, a4
	bra.w line
next
	addq.l #1, d7
	bra.w span
good
	moveq #0, d0
	bra.w done
invalid
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; applySelection
	.priv

; Resolve each defining numeric identity to its first outer block once. The
; table is temporary and stores span indices+1, never process pointers. It
; includes the block name, internal declarations, and a preceding label.
indexOwners	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l Scope, a2
	moveq #0, d0
	move.w layout.State.Count(a2), d0
	add.l d0, d0
	lea Owners, a0
	jsr memory.reserveExact
	bne.w ownerBad
	lea Owners, a0
	movea.l memory.Block.Pointer(a0), a0
	moveq #0, d1
	move.w layout.State.Count(a2), d1
clearOwner
	tst.l d1
	beq.w ownerSpans
	clr.w (a0)+
	subq.l #1, d1
	bra.w clearOwner
ownerSpans
	moveq #0, d7
ownerSpan
	cmp.w Count, d7
	bhs.w ownerGood
	move.l d7, d1
	mulu.w #SPAN_BYTES, d1
	movea.l Spans, a3
	adda.l d1, a3
	tst.w Span.Parent(a3)
	bne.w ownerNext
	move.l Span.Start(a3), d0
	lea Guards, a0
	jsr guards.activity
	cmpi.l #guards.INACTIVE, d0
	beq.w ownerNext
	moveq #0, d0
	move.w Span.Entry(a3), d0
	bsr.w saveOwner
	bne.w ownerBad
	movea.l Span.Start(a3), a4
	movea.l Span.End(a3), a6
ownerLine
	cmpa.l a6, a4
	beq.w ownerNext
	bhi.w ownerBad
	move.l a4, d0
	bsr.w record
	bne.w ownerBad
	movea.l a0, a5
	move.l a6, d0
	sub.l a4, d0
	cmp.l d0, d6
	bhi.w ownerBad
	move.l a4, d0
	lea Guards, a0
	jsr guards.activity
	cmpi.l #guards.INACTIVE, d0
	beq.w ownerAdvance
	btst #source.FLAG_LAYOUT_BIT, source.RecordHeader.Flags(a5)
	beq.w ordinaryOwner
	cmpi.w #source.LayoutRecord.Opcode+1, d6
	blo.w ownerBad
	cmpi.b #compiled.OP_OWNERS, source.LayoutRecord.Opcode(a5)
	bne.w ownerAdvance
	movea.l a5, a0
	move.l d6, d0
	jsr compiled.ownersView
	bne.w ownerBad
	bsr.w saveCompiledOwners
	bne.w ownerBad
	bra.w ownerAdvance
ordinaryOwner
	cmpi.w #materialized.Record.Kind+1, d6
	blo.w ownerAdvance
	cmpi.b #materialized.TOKEN, materialized.Record.Kind(a5)
	beq.w materializedOwner
	cmpi.w #source.DECLARATION_HEADER_BYTES, d6
	blo.w ownerAdvance
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, source.DeclarationRecord.Name+source.NameToken.Kind(a5)
	bhi.w ownerAdvance
	cmpi.b #source.TOKEN_LABEL_DECLARATION, source.DeclarationRecord.Marker(a5)
	beq.w ownerDeclaration
	cmpi.b #source.TOKEN_MUTABLE_DECLARATION, source.DeclarationRecord.Marker(a5)
	beq.w ownerDeclaration
	cmpi.b #source.TOKEN_CONDITIONAL_DECLARATION, source.DeclarationRecord.Marker(a5)
	beq.w ownerDeclaration
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, source.DeclarationRecord.Marker(a5)
	beq.w ownerDeclaration
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, source.DeclarationRecord.Marker(a5)
	beq.w ownerDeclaration
	bra.w ownerAdvance
materializedOwner
	movea.l a5, a0
	movea.l a5, a1
	adda.l d6, a1
	jsr materialized.validate
	bne.w ownerBad
	cmpi.b #materialized.COLLECTION_OPEN, materialized.Record.Action(a5)
	bne.w ownerAdvance
	moveq #0, d0
	move.w materialized.Record.Label(a5), d0
	cmpi.l #materialized.ANONYMOUS, d0
	beq.w ownerAdvance
	bsr.w saveOwner
	bne.w ownerBad
	bra.w ownerAdvance

ownerDeclaration
	moveq #0, d0
	move.w source.DeclarationRecord.Name+source.NameToken.Id(a5), d0
	bsr.w saveOwner
	bne.w ownerBad
ownerAdvance
	adda.w d6, a4
	bra.w ownerLine
ownerNext
	addq.w #1, d7
	bra.w ownerSpan
ownerGood
	; Proxy occurrences have no declaration record. Resolve their selected
	; numeric prefix to this same table once; each later mark remains O(1).
	movea.l Scope, a0
	movea.l Owners+memory.Block.Pointer, a1
	moveq #0, d0
	move.w layout.State.Count(a0), d0
	moveq #0, d2
	tst.w GuardActive
	beq.w certificateReady
	move.l #Guards, d2
certificateReady
	jsr scopes.indexBindingOwners
	bra.w ownerDone
ownerBad
	moveq #1, d0
ownerDone
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; indexOwners

; A1=validated packed owner IDs,D1=count,D7=outer span. Owner metadata
; establishes reachability only; the fragment runtime supplies actual values.
; D0/CCR=status; all other registers preserved.
saveCompiledOwners	.block
	movem.l d1-d3/a0-a1, -(sp)
next
	tst.l d1
	beq.w good
	moveq #0, d0
	move.w (a1)+, d0
	bsr.w saveOwner
	bne.w done
	subq.l #1, d1
	bra.w next
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; saveCompiledOwners

; D0=bound numeric identity, D7=outer span index. Keep the first owner, as
; the former source-order search did. D0/CCR=status; other registers preserved.
saveOwner	.block
	movem.l d1-d2/a0-a1, -(sp)
	movea.l Scope, a1
	moveq #0, d1
	move.w layout.State.Base(a1), d1
	sub.l d1, d0
	bcs.w ownerSkip
	moveq #0, d1
	move.w layout.State.Count(a1), d1
	cmp.l d1, d0
	bhs.w ownerSkip
	add.l d0, d0
	lea Owners, a0
	movea.l memory.Block.Pointer(a0), a0
	tst.w 0(a0, d0.l)
	bne.w ownerSkip
	move.w d7, d2
	addq.w #1, d2
	; The binding-plan owner table reserves $ffff for ambiguity. Fail before
	; publishing a real owner that would collide with that existing contract.
	cmpi.w #scopes.AMBIGUOUS_BINDING_OWNER, d2
	beq.w ownerOverflow
	move.w d2, 0(a0, d0.l)
ownerSkip
	moveq #0, d0
	movem.l (sp)+, d1-d2/a0-a1
	rts
ownerOverflow
	moveq #1, d0
	movem.l (sp)+, d1-d2/a0-a1
	rts
	.bend  ; saveOwner

	.priv
; A4=current logical offset,A6=end offset. Advance A4 once.
; D0/CCR=status; clobbers D6/A0-A1. References may enqueue live blocks.
scanLine	.block
	move.l a4, d0
	bsr.w record
	bne.w scanBad
	move.l a6, d0
	sub.l a4, d0
	cmp.l d0, d6
	bhi.w scanBad
	.MEMORY_COUNTER_INC Scanned
	move.l a0, -(sp)
	move.l a4, d0
	lea Guards, a0
	jsr guards.activity
	movea.l (sp)+, a0
	cmpi.l #guards.INACTIVE, d0
	beq.w scanNext
	btst #source.FLAG_LAYOUT_BIT, source.RecordHeader.Flags(a0)
	bne.w scanLayout
	movea.l a0, a1
	adda.w d6, a1
	move.l d3, -(sp)
	moveq #0, d3
	move.b source.RecordHeader.Flags(a0), d3
	adda.w #source.RECORD_HEADER_BYTES, a0
	bsr.w references
	move.l (sp)+, d3
	tst.l d0
	bne.w scanBad
	bra.w scanNext
scanLayout
	cmpi.w #source.LayoutRecord.Opcode+1, d6
	blo.w scanBad
	cmpi.b #compiled.OP_REFERENCES, source.LayoutRecord.Opcode(a0)
	bne.w scanNext
	bsr.w compiledReferences
	bne.w scanBad
scanNext
	adda.w d6, a4
	moveq #0, d0
	rts
scanBad
	moveq #1, d0
	rts
	.bend  ; scanLine

	.include "binary_block_image_refs.i"

; D0=outer span index. Queue capacity equals the indexed span limit, and each
; outer span is enqueued only on its dead-to-live transition. D0/CCR=status;
; clobbers D1/A0.
enqueue	.block
	moveq #0, d1
	move.w Tail, d1
	cmp.w Count, d1
	bhs.w queueFull
	add.l d1, d1
	movea.l Spans, a0
	adda.l WorkspaceOffset, a0
	move.w d0, 0(a0, d1.l)
	addq.w #1, Tail
	.MEMORY_COUNTER_INC QueueAdds
	moveq #0, d0
	rts
queueFull
	moveq #1, d0
	rts
	.bend  ; enqueue

; A0..A1=packed token range. Follow only numeric symbol operands; other
; bytecode operands are skipped by their encoded width. D0/CCR=status.
references	.block
	movem.l d1-d5/a0-a2/a5, -(sp)
	move.l d3, -(sp)  ; record flags remain independent of operand scratch
token
	cmpa.l a1, a0
	beq.w good
	bhi.w bad
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #1, d1
	bls.w name
	cmpi.b #2, d1
	beq.w literal
	cmpi.b #source.TOKEN_WIDE_NUMBER, d1
	beq.w wideLiteral
	cmpi.b #source.TOKEN_BINARY_BYTES, d1
	beq.w binary
	cmpi.b #3, d1
	beq.w string
	cmpi.b #expr.COMPILED_TAG, d1
	beq.w compactExpression
	cmpi.b #expr.VALUE_TAG, d1
	beq.w typedExpression
	cmpi.b #materialized.TOKEN, d1
	beq.w materializedPayload
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, d1
	beq.w schema
	cmpi.b #source.TOKEN_ITERATOR_BINDING, d1
	beq.w iterator
	cmpi.b #source.MACRO_PLAN, d1
	beq.w plan
	cmpi.b #source.LINE_PLAN, d1
	beq.w plan
	cmpi.b #source.OPTIONAL_LINE_PLAN, d1
	beq.w plan
	cmpi.b #41, d1
	beq.w bad  ; expansion must replace composite recipes before indexing
	bra.w token
plan
	move.l d1, d0
	move.l (sp), d1
	jsr payloads.plan
	bne.w bad
	tst.l d1
	beq.w token
	andi.l #$ff-source.FLAG_PLAN, (sp)
	bra.w token
iterator
	; An iterator binding is not a reference to an emitted block.
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #2, d0
	blo.w bad
	addq.l #2, a0
	bra.w token
literal
	moveq #source.NUMBER_BYTES-1, d0
	bra.w literalPayload
wideLiteral
	moveq #source.WIDE_NUMBER_BYTES-1, d0
	bra.w literalPayload
schema
	moveq #4, d0  ; declaration seed offsets are not emitted-code references
literalPayload
	move.l a1, d1
	sub.l a0, d1
	cmp.l d0, d1
	blo.w bad
	adda.l d0, a0
	bra.w token
materializedPayload
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #materialized.RECORD_BYTES-materialized.Record.Action, d0
	bne.w bad
	movea.l a1, a0
	bra.w token

binary
	subq.l #1, a0
	jsr payloads.binary
	bne.w bad
	adda.l d2, a0
	bra.w token

string
	cmpa.l a1, a0
	bhs.w bad
	moveq #0, d0
	move.b (a0)+, d0
	move.l a1, d1
	sub.l a0, d1
	cmp.l d0, d1
	blo.w bad
	adda.l d0, a0
	bra.w token
name
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w bad
	cmpi.b #source.FIELD_KEY_QUALIFIER, 2(a0)
	beq.w bareField
	moveq #0, d0
	move.w (a0), d0
	bsr.w mark
	bne.w bad
bareField
	addq.l #3, a0
	bra.w token
compactExpression
	moveq #0, d5
	bra.w expression
typedExpression
	moveq #1, d5
expression
	cmpa.l a1, a0
	bhs.w bad
	moveq #0, d0
	move.b (a0)+, d0
	beq.w bad
	movea.l a0, a5
	adda.w d0, a5
	cmpa.l a1, a5
	bhi.w bad
opcode
	cmpa.l a5, a0
	bhs.w bad
	moveq #0, d1
	move.b (a0)+, d1
	beq.w endExpression
	cmpi.b #runtime.SELECT, d1
	beq.w opcode
	tst.l d5
	bne.w typedOpcode
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_SYMBOL, d1
	beq.w symbol
	cmpi.b #runtime.COMPACT_I8, d1
	beq.w one
	cmpi.b #runtime.EXPRVM_OPCODE_APPLY_UNARY, d1
	beq.w one
	cmpi.b #runtime.EXPRVM_OPCODE_APPLY_BINARY, d1
	beq.w one
	cmpi.b #runtime.COMPACT_I16, d1
	beq.w two
	cmpi.b #runtime.COMPACT_I32, d1
	beq.w four
	cmpi.b #runtime.COMPACT_U32, d1
	beq.w four
	cmpi.b #runtime.COMPACT_I64, d1
	beq.w eight
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_CURRENT_ADDR, d1
	beq.w opcode
	cmpi.b #runtime.COMPACT_NEGATE, d1
	beq.w opcode
	cmpi.b #runtime.COMPACT_ADD, d1
	beq.w opcode
	cmpi.b #runtime.COMPACT_SUBTRACT, d1
	beq.w opcode
	cmpi.b #runtime.COMPACT_MULTIPLY, d1
	beq.w opcode
	bra.w bad
typedOpcode
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_STRING_LITERAL, d1
	beq.w typedString
	cmpi.b #$10, d1
	beq.w eight
	cmpi.b #$11, d1
	beq.w opcode
	cmpi.b #$12, d1
	beq.w symbol
	cmpi.b #$20, d1
	beq.w one
	cmpi.b #$21, d1
	beq.w one
	cmpi.b #$51, d1
	beq.w two
	cmpi.b #$52, d1
	beq.w one
	cmpi.b #runtime.EXPRVM_OPCODE_BUILD_STRUCT, d1
	beq.w struct
	cmpi.b #runtime.EXPRVM_OPCODE_GET_MEMBER, d1
	beq.w two
	cmpi.b #$61, d1
	beq.w opcode
	cmpi.b #$62, d1
	beq.w three
	cmpi.b #$70, d1
	beq.w opcode
	bra.w bad
typedString
	move.l a1, -(sp)
	movea.l a5, a1
	jsr payloads.string
	movea.l (sp)+, a1
	bne.w bad
	bra.w opcode
struct
	move.l a5, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w bad
	moveq #0, d0
	move.b 1(a0), d0
	lsl.w #8, d0
	move.b (a0), d0
	; The nominal schema is immutable metadata, not a reference to its
	; declaration's emitted code. Keep numeric bounds validation independent.
	movea.l Scope, a2
	moveq #0, d1
	move.w layout.State.Base(a2), d1
	moveq #0, d2
	move.w layout.State.Count(a2), d2
	add.l d2, d1
	cmp.l d1, d0
	bhs.w bad
	moveq #0, d2
	move.b 3(a0), d2
	lsl.w #8, d2
	move.b 2(a0), d2
	add.l d2, d2
	addq.l #4, d2
	move.l a5, d0
	sub.l a0, d0
	cmp.l d2, d0
	blo.w bad
	adda.l d2, a0
	bra.w opcode
three
	addq.l #3, a0
	bra.w opcode

one
	addq.l #1, a0
	bra.w opcode
two
	addq.l #2, a0
	bra.w opcode
four
	addq.l #4, a0
	bra.w opcode
eight
	addq.l #8, a0
	bra.w opcode
symbol
	move.l a5, d0
	sub.l a0, d0
	cmpi.l #2, d0
	blo.w bad
	moveq #0, d0
	move.b 1(a0), d0
	lsl.w #8, d0
	move.b (a0), d0
	bsr.w mark
	bne.w bad
	addq.l #2, a0
	bra.w opcode
endExpression
	cmpa.l a5, a0
	bne.w bad
	bra.w token
good
	move.l (sp), d0
	andi.l #source.FLAG_PLAN, d0
	bne.w bad
	moveq #0, d0
	bra.w doneRefs
bad
	.BINDING_RECORD_OFFSET_FAILURE #binding_diagnostic.BLOCK_REFERENCES, a0, a1, RecordPointer, RecordOffset
	moveq #1, d0
doneRefs
	addq.l #4, sp
	movem.l (sp)+, d1-d5/a0-a2/a5
	tst.l d0
	rts
	.bend  ; references

; D0=numeric ID. The owner table names the first defining outer block, avoiding
; repeated packed-record scans and import proxy identities with no owned code.
; An internal label retains its enclosing block and all fallthrough bytes.
; Enqueue on first inclusion. Return D0/CCR=status; preserve all other registers.
mark	.block
	movem.l d1-d3/a0-a4, -(sp)
	.MEMORY_COUNTER_INC Marks
	movea.l Scope, a2
	moveq #0, d1
	move.w layout.State.Base(a2), d1
	sub.l d1, d0
	bcs.w doneMark  ; package tokens cannot name a source block
	moveq #0, d1
	move.w layout.State.Count(a2), d1
	cmp.l d1, d0
	bhs.w doneMark
	add.l d0, d0
	lea Owners, a2
	movea.l memory.Block.Pointer(a2), a2
	moveq #0, d2
	move.w 0(a2, d0.l), d2
	beq.w doneMark
	cmpi.w #scopes.AMBIGUOUS_BINDING_OWNER, d2
	beq.w ambiguousOwner
	subq.w #1, d2
	move.l d2, d1
	mulu.w #SPAN_BYTES, d1
	movea.l Spans, a3
	adda.l d1, a3
markRoot
	tst.w Span.Live(a3)
	bne.w doneMark
	move.w #1, Span.Live(a3)
	move.l d2, d0
	bsr.w enqueue
	bra.w markExit
ambiguousOwner
	moveq #1, d0
	bra.w markExit
doneMark
	moveq #0, d0
markExit
	movem.l (sp)+, d1-d3/a0-a4
	tst.l d0
	rts
	.bend  ; mark
	.endsection
	.endmodule
