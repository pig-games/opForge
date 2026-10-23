; Strict source selection before active acquisition and binding export.
; Package PRVMs classify records; EXVM evaluates preparation-known operands.
; @opforge-owner: opforge.assembler.source.control
	.module opforge.assembler.source.control
	.cpu 68020
	.use opforge.assembler.prepare.frontend as frontend
	.use opforge.packages.format as package
	.use opforge.assembler.symbols.scopes as scopes
	.use opforge.assembler.symbols.scope_layout as layout
	.use opforge.assembler.language.conditionals as conditionals
	.use opforge.assembler.symbols.imports as imports
	.use opforge.assembler.values.values as values
	.use opforge.assembler.source.records as source
	.use opforge.assembler.symbols.materialized_scope as materialized
	.use opforge.vm.prvm.packed_loop as loops
	.use opforge.vm.prvm.abi as abi
	.pub
LIMIT = 16
MAX_ITERATIONS = 65536
EFFECT_FILE = 1
EFFECT_CALL = 2
Slot	.struct
Body	.long ?
Close	.long ?
After	.long ?
StreamEnd	.long ?
StreamKey	.long ?
Remaining	.long ?
Index	.long ?
Variable	.long ?
Descriptor	.long ?
Kind	.long ?
Label	.long ?
Scoped	.word ?
Padding	.word ?
Header	.res source.MAX_LINE
.endstruct
SLOT_BYTES = Slot.Header+source.MAX_LINE
	.section code, kind=code

; A0=session. Reset caller-owned assembly preparation traversal. D0/CCR=0;
; other registers preserved. Compound descriptors remain in the scope owner.
begin	.block
	clr.w Depth
	clr.w BatchActive
	clr.w frontend.Frame.SourceIteration(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=session,A1=writer record. D0/CCR=status,D1=1 consumed / 0 ordinary.
; Other registers preserved. Owned physical or template loops with potential
; acquisition are selected before records and lexical identities are frozen.
line	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a4
	; Template invocation scope envelopes are private records, consumed by the
	; existing scope owner rather than the portable source-control grammar.
	btst #scopes.LEXICAL_BLOCK_BIT, 1(a4)
	bne.w ordinary
	jsr frontend.conditionalState
	tst.w conditionals.State.Active(a1)
	beq.w ordinary
	tst.w conditionals.State.Deferred(a1)
	bne.w ordinary
	movea.l a5, a0
	jsr frontend.scopeState
	movea.l a1, a6
	tst.w frontend.Frame.SourceOwned(a5)
	bne.w acquiredGraph
	movea.l a5, a0
	jsr frontend.expansionDepth
	cmp.w frontend.Frame.ExpansionFloor(a5), d0
	bls.w selected
acquiredGraph
	movea.l a4, a0
	movea.l a6, a1
	jsr scopes.statementRole
	cmpi.l #scopes.KEY_MODULE, d0
	beq.w bad
	cmpi.l #scopes.KEY_ENDMODULE, d0
	beq.w bad
	cmpi.l #scopes.KEY_USE, d0
	beq.w bad
selected
	movea.l a4, a0
	bsr.w classifyLoop
	cmpi.l #abi.PRVM_LOOP_MALFORMED, d0
	beq.w bad
	cmpi.l #abi.PRVM_LOOP_ENDFOR, d0
	beq.w closing
	cmpi.l #abi.PRVM_LOOP_ENDWHILE, d0
	beq.w closing
	cmpi.l #abi.PRVM_LOOP_ORDINARY, d0
	beq.w ordinary
	move.l d0, PendingKind
	move.l d1, PendingVariable
	move.l frontend.Frame.SourceCursor(a5), d0
	beq.w ordinary
	movea.l d0, a2
	movea.l a5, a0
	moveq #0, d0
	jsr (a2)
	bne.w bad
	cmp.l d1, d2
	bls.w ordinary  ; direct streaming root has no capture frontier
	move.l d3, PendingStreamKey
	move.l d2, PendingStreamEnd
	move.l d1, PendingHeader
	bsr.w scanBody
	bne.w bad
	tst.l Effects
	beq.w ordinary
	moveq #0, d0
	move.w Depth, d0
	cmpi.w #LIMIT, d0
	bhs.w bad
	mulu.w #SLOT_BYTES, d0
	lea Slots, a3
	adda.l d0, a3
	move.l PendingBody, Slot.Body(a3)
	move.l PendingClose, Slot.Close(a3)
	move.l PendingAfter, Slot.After(a3)
	move.l PendingStreamEnd, Slot.StreamEnd(a3)
	move.l PendingStreamKey, Slot.StreamKey(a3)
	move.l PendingKind, Slot.Kind(a3)
	move.l PendingVariable, Slot.Variable(a3)
	clr.l Slot.Descriptor(a3)
	clr.l Slot.Index(a3)
	moveq #0, d0
	move.b (a4), d0
	addq.l #1, d0
	movea.l a4, a0
	lea Slot.Header(a3), a1
copyHeader
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyHeader
	bsr.w evaluateHeader
	bne.w unavailableHeader
	move.l d1, -(sp)
	bsr.w openCollection
	move.l (sp)+, d1
	tst.l d0
	bne.w bad
	tst.l d1
	beq.w skipLoop
	move.l d1, Slot.Remaining(a3)
	addq.w #1, Depth
	move.w Depth, layout.State.MaterializedLoops(a6)
	move.w Depth, frontend.Frame.SourceIteration(a5)
	bsr.w openIteration
	bne.w bad
	bsr.w selectIterator
	bne.w bad
	move.l Slot.Body(a3), d1
	bsr.w seek
	bne.w bad
	bra.w consumed
skipLoop
	moveq #materialized.COLLECTION_CLOSE, d0
	bsr.w boundary
	bne.w bad
	move.l Slot.After(a3), d1
	bsr.w seek
	bne.w bad
	bra.w consumed
closing
	tst.w Depth
	beq.w ordinary
	moveq #0, d0
	move.w Depth, d0
	subq.w #1, d0
	mulu.w #SLOT_BYTES, d0
	lea Slots, a3
	adda.l d0, a3
	move.l frontend.Frame.SourceCursor(a5), d0
	movea.l d0, a2
	movea.l a5, a0
	moveq #0, d0
	jsr (a2)
	bne.w bad
	cmp.l Slot.StreamKey(a3), d3
	bne.w ordinary
	cmp.l Slot.StreamEnd(a3), d2
	bne.w ordinary
	cmp.l Slot.Close(a3), d1
	bne.w ordinary
	moveq #materialized.ITERATION_CLOSE, d0
	bsr.w boundary
	bne.w bad
	movea.l a4, a0
	movea.l a6, a1
	move.l #source.MAX_LINE, d0
	jsr scopes.line
	bne.w bad
	cmpi.l #abi.PRVM_LOOP_WHILE, Slot.Kind(a3)
	beq.w nextWhile
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a3)
	beq.w nextWhile
	subq.l #1, Slot.Remaining(a3)
	beq.w finished
	addq.l #1, Slot.Index(a3)
	bra.w repeat
nextWhile
	addq.l #1, Slot.Index(a3)
	cmpi.l #MAX_ITERATIONS, Slot.Index(a3)
	bhs.w bad
	bsr.w evaluateHeader
	bne.w bad
	tst.l d1
	beq.w finished
repeat
	bsr.w openIteration
	bne.w bad
	bsr.w selectIterator
	bne.w bad
	move.l Slot.Body(a3), d1
	bsr.w seek
	bne.w bad
	bra.w consumed
finished
	addq.l #1, Slot.Index(a3)
	; while already advanced its index before selecting the final guard.
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a3)
	bne.w finishMarker
	subq.l #1, Slot.Index(a3)
finishMarker
	moveq #materialized.COLLECTION_CLOSE, d0
	bsr.w boundary
	bne.w bad
	subq.w #1, Depth
	move.w Depth, layout.State.MaterializedLoops(a6)
	move.w Depth, frontend.Frame.SourceIteration(a5)
	bra.w consumed
unavailableHeader
	move.l Effects, d0
	andi.l #EFFECT_FILE, d0
	bne.w bad  ; direct file actions require strict known selection
	bra.w ordinary
ordinary
	moveq #0, d0
	moveq #0, d1
	bra.w done
consumed
	moveq #0, d0
	moveq #1, d1
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	bsr.w finishBatch
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line

	.priv
; A0=writer record. Return the shared package loop classifier result.
; A5=session. D0=role,D1=variable,D2=operand offset; preserves other registers.
classifyLoop	.block
	movem.l a1, -(sp)
	moveq #0, d0
	move.b (a0), d0
	addq.l #1, d0
	movea.l frontend.Frame.Package(a5), a1
	move.l package.Header.LoopPlanBytes(a1), d1
	adda.l package.Header.LoopPlan(a1), a1
	jsr loops.classify
	movea.l (sp)+, a1
	rts
	.bend  ; classifyLoop

; Scan owned numeric previews for matching controls and potential acquisition.
; Definitions are skipped by the normal shared scope directive role resolver.
scanBody	.block
	movem.l d3-d7/a0-a3, -(sp)
	clr.l Effects
	move.l PendingHeader, d1
	bsr.w peek
	bne.w bad
	move.l d1, PendingBody
	moveq #1, d7
	move.l PendingKind, ScanKinds
	moveq #0, d6
next
	cmp.l PendingStreamEnd, d1
	bhs.w bad
	move.l d1, d5
	bsr.w peek
	bne.w bad
	move.l d1, d4
	movea.l a1, a0
	movea.l a6, a1
	jsr scopes.statementRole
	cmpi.l #scopes.KEY_MACRO, d0
	beq.w definition
	cmpi.l #scopes.KEY_SEGMENT, d0
	beq.w definition
	cmpi.l #scopes.KEY_ENDMACRO, d0
	beq.w endDefinition
	cmpi.l #scopes.KEY_ENDSEGMENT, d0
	beq.w endDefinition
	tst.l d6
	bne.w advance
	move.l d5, d1
	bsr.w peek
	bne.w bad
	movea.l a1, a0
	bsr.w classifyLoop
	cmpi.l #abi.PRVM_LOOP_MALFORMED, d0
	beq.w bad
	cmpi.l #abi.PRVM_LOOP_ORDINARY, d0
	beq.w file
	cmpi.l #abi.PRVM_LOOP_ENDFOR, d0
	beq.w close
	cmpi.l #abi.PRVM_LOOP_ENDWHILE, d0
	beq.w close
	cmpi.l #LIMIT, d7
	bhs.w bad
	move.l d7, d3
	lsl.l #2, d3
	lea ScanKinds, a0
	move.l d0, 0(a0, d3.l)
	addq.l #1, d7
	bra.w advance
close
	move.l d0, d3
	move.l d7, d0
	subq.l #1, d0
	lsl.l #2, d0
	lea ScanKinds, a0
	move.l 0(a0, d0.l), d0
	cmpi.l #abi.PRVM_LOOP_WHILE, d0
	beq.w whileClose
	cmpi.l #abi.PRVM_LOOP_BWHILE, d0
	beq.w whileClose
	cmpi.l #abi.PRVM_LOOP_ENDFOR, d3
	bne.w bad
	bra.w matchedClose
whileClose
	cmpi.l #abi.PRVM_LOOP_ENDWHILE, d3
	bne.w bad
matchedClose
	subq.l #1, d7
	bne.w advance
	move.l d5, PendingClose
	move.l d4, PendingAfter
	moveq #0, d0
	bra.w done
file
	move.l d5, d1
	bsr.w peek
	bne.w bad
	move.l a1, -(sp)
	movea.l a5, a0
	jsr frontend.templateRole
	cmpi.l #1, d0
	movea.l (sp)+, a0
	beq.w potentialCall
	bsr.w fileAction
	bne.w bad
	tst.l d1
	beq.w advance
	ori.l #EFFECT_FILE, Effects
	bra.w advance
potentialCall
	ori.l #EFFECT_CALL, Effects
	bra.w advance
definition
	addq.l #1, d6
	bra.w advance
endDefinition
	tst.l d6
	beq.w bad
	subq.l #1, d6
advance
	move.l d4, d1
	bra.w next
bad
	moveq #1, d0
done
	movem.l (sp)+, d3-d7/a0-a3
	tst.l d0
	rts
	.bend  ; scanBody

; A3=slot,A6=scope. D0/status,D1=count or while activity. Strict EXVM.
evaluateHeader	.block
	movem.l d4-d7/a0-a2, -(sp)
	lea Slot.Header(a3), a0
	bsr.w classifyLoop
	cmpi.l #abi.PRVM_LOOP_MALFORMED, d0
	beq.w bad
	move.l d0, d7
	movea.l a0, a1
	moveq #0, d0
	move.b (a0), d0
	addq.l #1, d0
	adda.l d0, a1
	adda.l d2, a0
	movea.l a6, a2
	jsr imports.evaluateKnown
	bne.w bad
	cmpi.l #abi.PRVM_LOOP_WHILE, d7
	beq.w whileValue
	cmpi.l #abi.PRVM_LOOP_BWHILE, d7
	beq.w whileValue
	tst.l Slot.Variable(a3)
	bmi.w countValue
	cmpi.l #values.LIST, d3
	blo.w bad
	cmpi.l #values.TYPED_LIST, d3
	bhi.w bad
	move.l d1, Slot.Descriptor(a3)
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER(a6), a0
	jsr values.compoundLength
	bne.w bad
	bra.w countBounds
whileValue
	tst.l d3
	bne.w bad
	or.l d2, d1
	beq.w good
	moveq #1, d1
	bra.w good
countValue
	tst.l d3
	bne.w bad
countBounds
	tst.l d2
	bne.w bad
	cmpi.l #MAX_ITERATIONS, d1
	bhi.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d4-d7/a0-a2
	tst.l d0
	rts
	.bend  ; evaluateHeader

; A3=slot,A6=scope. Seed the fresh iterator scalar for normal preparation.
selectIterator	.block
	movem.l d1-d3/a0, -(sp)
	tst.l Slot.Variable(a3)
	bmi.w good
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER(a6), a0
	move.l Slot.Descriptor(a3), d1
	move.l Slot.Index(a3), d2
	moveq #0, d3
	jsr values.compoundGet
	bne.w done
	movea.l a6, a0
	move.l Slot.Variable(a3), d0
	moveq #1, d3
	jsr imports.setKnownScalar
	bne.w done
	bsr.w scalarDestination
	movea.l a5, a0
	move.l Slot.Variable(a3), d0
	jsr frontend.sourceScalar
	bne.w done
	bsr.w appendScalar
	bra.w done
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d3/a0
	tst.l d0
	rts
	.bend  ; selectIterator

; A5=session,D1=owned source offset. Numeric callbacks preserve our state.
peek	.block
	move.l a2, -(sp)
	movea.l frontend.Frame.SourcePeek(a5), a2
	move.l a2, d0
	beq.w bad
	movea.l a5, a0
	jsr (a2)
	bra.w done
bad
	moveq #1, d0
done
	movea.l (sp)+, a2
	tst.l d0
	rts
	.bend  ; peek

seek	.block
	move.l a2, -(sp)
	movea.l frontend.Frame.SourceCursor(a5), a2
	movea.l a5, a0
	moveq #1, d0
	jsr (a2)
	movea.l (sp)+, a2
	tst.l d0
	rts
	.bend  ; seek

; A0=raw record,A5=session. D0/status,D1=selected file action (0 absent).
fileAction	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	movea.l a5, a0
	jsr frontend.classifyFile
	cmpi.l #abi.PRVM_STATUS_INVALID_TOKEN, d0
	bne.w done
	moveq #1, d1  ; potentially selected file; validate only when active
	moveq #0, d0
done
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; fileAction

; Bind a fresh iterator identity for this selected iteration through scopes.
openIteration	.block
	movem.l d1-d3/a0-a2, -(sp)
	lea Slot.Header(a3), a0
	lea Opening, a1
	moveq #0, d0
	move.b (a0), d0
copy
	move.b (a0)+, (a1)+
	dbf d0, copy
	tst.w Slot.Scoped(a3)
	beq.w headerReady
	cmpi.l #materialized.ANONYMOUS, Slot.Label(a3)
	beq.w headerReady
	lea Opening+source.RECORD_HEADER_BYTES+source.NAME_TOKEN_BYTES+1, a0
	lea Opening+source.RECORD_HEADER_BYTES, a1
	moveq #0, d0
	move.b Opening+source.RecordHeader.Length, d0
	subi.l #source.RECORD_HEADER_BYTES+source.NAME_TOKEN_BYTES, d0
stripLabel
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w stripLabel
	subi.b #source.NAME_TOKEN_BYTES+1, Opening+source.RecordHeader.Length
headerReady
	lea Opening, a0
	movea.l a6, a1
	move.l #source.MAX_LINE, d0
	jsr scopes.line
	bne.w done
	lea Opening, a0
	bsr.w classifyLoop
	move.l d1, Slot.Variable(a3)
	moveq #materialized.ITERATION_OPEN, d0
	bsr.w boundary
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; openIteration

; A3=selected slot. Declare one scoped collection, then publish its open marker.
openCollection	.block
	clr.w Slot.Scoped(a3)
	move.l #materialized.ANONYMOUS, Slot.Label(a3)
	cmpi.l #abi.PRVM_LOOP_BFOR, Slot.Kind(a3)
	beq.w scoped
	cmpi.l #abi.PRVM_LOOP_BWHILE, Slot.Kind(a3)
	bne.w good
scoped
	move.w #1, Slot.Scoped(a3)
	lea Slot.Header(a3), a0
	movea.l a6, a1
	jsr scopes.materializedCollection
	bne.w done
	move.l d1, Slot.Label(a3)
	moveq #materialized.COLLECTION_OPEN, d0
	bsr.w boundary
	bra.w done
good
	moveq #0, d0
done
	tst.l d0
	rts
	.bend  ; openCollection

; Append a private boundary to one bounded publication batch.
boundary	.block
	movem.l d1-d3/a0, -(sp)
	tst.w Slot.Scoped(a3)
	beq.w good
	bsr.w beginBatch
	move.l BatchUsed, d1
	cmpi.l #source.MAX_LINE-materialized.RECORD_BYTES, d1
	bhi.w bad
	lea Batch, a0
	adda.l d1, a0
	move.l Slot.Label(a3), d1
	move.l Slot.Index(a3), d2
	moveq #0, d3
	move.w Slot.Header+source.RecordHeader.Line(a3), d3
	jsr materialized.emit
	bne.w done
	addi.l #materialized.RECORD_BYTES, BatchUsed
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0
	tst.l d0
	rts
	.bend  ; boundary

beginBatch	.block
	tst.w BatchActive
	bne.w done
	move.w #1, BatchActive
	clr.l BatchUsed
	move.l frontend.Frame.Output(a5), SavedOutput
	move.l frontend.Frame.Graph(a5), SavedGraph
	clr.l frontend.Frame.Graph(a5)
done
	rts
	.bend  ; beginBatch
scalarDestination	.block
	tst.w BatchActive
	beq.w done
	; Compile into the complete existing scratch record, never a batch tail.
	lea Opening, a0
	move.l a0, frontend.Frame.Output(a5)
done
	rts
	.bend  ; scalarDestination
; Copy the already bounded compiled initializer only after checking batch space.
appendScalar	.block
	movem.l d1-d2/a0-a1, -(sp)
	tst.w BatchActive
	beq.w good
	move.l frontend.Frame.Used(a5), d1
	cmpi.l #source.RECORD_HEADER_BYTES, d1
	blo.w bad
	move.l BatchUsed, d2
	add.l d1, d2
	bcs.w bad
	cmpi.l #source.MAX_LINE, d2
	bhi.w bad
	lea Batch, a1
	adda.l BatchUsed, a1
	lea Opening, a0
copy
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bne.w copy
	move.l d2, BatchUsed
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; appendScalar

; D0=status,D1=consumed. Restore borrowed frontend views on every exit,
; then publish the complete numeric batch through its graph accounting owner.
finishBatch	.block
	movem.l d1/a0-a1, -(sp)
	tst.w BatchActive
	beq.w done
	clr.w BatchActive
	move.l SavedOutput, frontend.Frame.Output(a5)
	move.l SavedGraph, frontend.Frame.Graph(a5)
	tst.l d0
	bne.w done
	move.l BatchUsed, d0
	cmpi.l #source.MAX_LINE, d0
	bhi.w bad
	movea.l a5, a0
	lea Batch, a1
	jsr frontend.data
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; finishBatch

	.endsection
	.section bss, kind=bss
BatchActive	.res word, 1
.align 4
BatchUsed	.res long, 1
SavedOutput	.res long, 1
SavedGraph	.res long, 1
Batch	.res byte, source.MAX_LINE
Depth	.res word, 1
.align 4
PendingKind	.res long, 1
PendingVariable	.res long, 1
PendingStreamEnd	.res long, 1
PendingStreamKey	.res long, 1
PendingHeader	.res long, 1
PendingBody	.res long, 1
PendingClose	.res long, 1
PendingAfter	.res long, 1
Effects	.res long, 1
ScanKinds	.res long, LIMIT
Slots	.res byte, LIMIT*SLOT_BYTES
Opening	.res byte, source.MAX_LINE
	.endsection
	.endmodule
