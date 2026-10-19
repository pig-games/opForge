; One publication frontier owns the original records until conversion commits.
; Global lexical/configuration owners stay live. A failed eligibility proof
; installs the same original bytes once; it never repeats source or file effects.
FRAGMENT_CANDIDATE = 1
FRAGMENT_PASSTHROUGH = 2
beginPreparedFragment	.block
	movem.l d1/a0, -(sp)
	tst.l FragmentActive
	bne.w bad
	tst.l FragmentInput+memory.Block.Pointer
	bne.w bad
	move.l Records+memory.Block.Used, FragmentBase
	clr.l FragmentRows
	move.l #FRAGMENT_CANDIDATE, FragmentActive
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend ; beginPreparedFragment

; Virtual global offsets remain valid for graph/origin recording while staging.
; Records.Pointer/Capacity still describe only the published prefix.
stagePreparedFragment	.block
	movem.l d1-d3/a0-a4, -(sp)
	move.l Records+memory.Block.Used, LineOffset
	move.l Front+frontend.Frame.Used, d3
	tst.l d3
	beq.w good
	cmpi.l #fragments.LIMIT, FragmentRows
	bhs.w passthrough
	move.l FragmentInput+memory.Block.Used, d0
	add.l d3, d0
	bcs.w bad
	cmpi.l #memory.LIMIT, d0
	bhi.w passthrough
	lea FragmentInput, a0
	move.l #memory.LIMIT, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	add.l d3, memory.Block.Used(a0)
	move.l d3, d0
	movea.l Front+frontend.Frame.Output, a0
	bsr.w copy
	add.l d3, Records+memory.Block.Used
	bcs.w bad
	addq.l #1, FragmentRows
	.LAZY_COUNT lazy_audit.MATERIALIZED, Front+frontend.Frame.Used
	.WORK_COUNT lazy_work.PREPARED_COPY, Front+frontend.Frame.Used
	.LAZY_PREPARED Front, LineOffset, SourceCursorState.Offset+SourceCursorData, OriginId, SourceLine
	bsr.w appendOrigin
	bra.w done
passthrough
	; Stop retaining a candidate as soon as conversion's own bound excludes it.
	; Its original virtual suffix is published once, with identical offsets.
	move.l FragmentBase, Records+memory.Block.Used
	lea FragmentInput, a4
	bsr.w appendFragmentBlock
	bne.w bad
	lea FragmentInput, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	move.l #FRAGMENT_PASSTHROUGH, FragmentActive
	bsr.w appendPrepared
	bra.w done
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a4
	tst.l d0
	rts
	.bend ; stagePreparedFragment

; Closed module only. Shared VM execution is borrowed synchronously; restore
; the frontend grammar before publication or another module can run.
publishPreparedFragment	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l FragmentActive
	beq.w bad
	cmpi.l #FRAGMENT_PASSTHROUGH, FragmentActive
	beq.w installed
	move.l Records+memory.Block.Used, FragmentEnd
	tst.l FragmentInput+memory.Block.Used
	beq.w unchanged
	lea FragmentFrame, a0
	move.l FragmentInput+memory.Block.Pointer, fragments.Frame.Input(a0)
	move.l FragmentInput+memory.Block.Used, fragments.Frame.Bytes(a0)
	move.l Front+frontend.Frame.Package, fragments.Frame.Package(a0)
	movea.l Front+frontend.Frame.Scratch, a1
	adda.l #frontend.SCOPE_STATE, a1
	move.l a1, fragments.Frame.Scope(a0)
	move.l #FragmentOutput, fragments.Frame.Output(a0)
	move.l #FragmentMap, fragments.Frame.Map(a0)
	move.l OriginSpans+memory.Block.Pointer, fragments.Frame.Origins(a0)
	move.l OriginCount, fragments.Frame.OriginCount(a0)
	move.l FragmentBase, fragments.Frame.Base(a0)
	jsr fragments.lower
	move.l d0, d7
	lea Front, a0
	jsr frontend.activate
	bne.w bad
	cmpi.l #fragments.INELIGIBLE, d7
	beq.w unchanged
	tst.l d7
	bne.w bad
	bsr.w remapFragmentMetadata
	bne.w bad
	.MEMORY_COUNTER_INC FragmentCounters+FragmentStats.Count
	.MEMORY_COUNTER_ADD FragmentCounters+FragmentStats.Before, FragmentInput+memory.Block.Used
	.MEMORY_COUNTER_ADD FragmentCounters+FragmentStats.After, FragmentOutput+memory.Block.Used
	lea FragmentOutput, a4
	bra.w install
unchanged
	lea FragmentInput, a4
install
	move.l FragmentBase, Records+memory.Block.Used
	bsr.w appendFragmentBlock
	bne.w bad
installed
	move.l Records+memory.Block.Used, LineOffset
	bsr.w releaseFragmentBuffers
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; publishPreparedFragment

; A4=owned complete record block. Append at the actual published frontier,
; without touching semantic/origin metadata or releasing the caller's owner.
; D0/CCR=status; other registers preserved. Partial I/O is a fatal build error.
appendFragmentBlock	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l memory.Block.Pointer(a4), a2
	move.l memory.Block.Used(a4), d4
	tst.l d4
	beq.w good
	tst.l CaptureDirectory
	bne.w stored
	lea Records, a0
	move.l memory.Block.Used(a0), d0
	add.l d4, d0
	bcs.w bad
	move.l #RECORD_LIMIT, d1
	jsr memory.reserveBounded
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l d4, d0
	movea.l a2, a0
	bsr.w copy
	add.l d4, Records+memory.Block.Used
	bra.w good
stored
	tst.l d4
	beq.w good
	moveq #0, d5
	move.b source.RecordHeader.Length(a2), d5
	addq.l #1, d5
	cmpi.l #source.RECORD_HEADER_BYTES, d5
	blo.w bad
	cmp.l d4, d5
	bhi.w bad
	move.l d5, d0
	movea.l a2, a1
	lea PreparedStore, a0
	jsr prepared_store.append
	bne.w bad
	cmp.l Records+memory.Block.Used, d1
	bne.w bad
	move.l prepared_store.State.LogicalUsed(a0), Records+memory.Block.Used
	adda.l d5, a2
	sub.l d5, d4
	bra.w stored
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; appendFragmentBlock

; Origin and semantic graph offsets refer to published records, not captures.
; Both are monotonic boundary maps. Capture/discovery offsets are untouched.
remapFragmentMetadata	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l OriginSpans+memory.Block.Pointer, a3
	move.l OriginCount, d6
origin
	tst.l d6
	beq.w graphReady
	move.l Span.Start(a3), d0
	bsr.w mapFragmentOffset
	bne.w bad
	move.l d1, Span.Start(a3)
	move.l Span.End(a3), d0
	bsr.w mapFragmentOffset
	bne.w bad
	move.l d1, Span.End(a3)
	adda.l #SPAN_BYTES, a3
	subq.l #1, d6
	bra.w origin
graphReady
	movea.l Front+frontend.Frame.Graph, a3
	move.l a3, d0
	beq.w good
	move.l graph.GraphState.Cursor(a3), d0
	bsr.w mapFragmentOffset
	bne.w bad
	move.l d1, graph.GraphState.Cursor(a3)
	moveq #0, d6
	move.w graph.GraphState.Count(a3), d6
	adda.l #graph.NODES, a3
node
	tst.l d6
	beq.w good
	move.l graph.Node.Start(a3), d0
	bsr.w mapFragmentOffset
	bne.w bad
	move.l d1, graph.Node.Start(a3)
	move.l graph.Node.End(a3), d0
	bsr.w mapFragmentOffset
	bne.w bad
	move.l d1, graph.Node.End(a3)
	adda.l #graph.NODE_BYTES, a3
	subq.l #1, d6
	bra.w node
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend ; remapFragmentMetadata

; D0=old global boundary. D1=new boundary,D0/CCR=status. Other registers kept.
; Only the current module changes; an interior nonboundary is a hard error.
mapFragmentOffset	.block
	movem.l d2-d4/a0, -(sp)
	move.l d0, d1
	cmp.l FragmentBase, d0
	blo.w good
	cmp.l FragmentEnd, d0
	bhi.w good
	sub.l FragmentBase, d0
	movea.l FragmentMap+memory.Block.Pointer, a0
	move.l FragmentMap+memory.Block.Used, d2
next
	cmpi.l #fragments.BOUNDARY_BYTES, d2
	blo.w bad
	cmp.l fragments.Boundary.Old(a0), d0
	beq.w found
	adda.l #fragments.BOUNDARY_BYTES, a0
	subi.l #fragments.BOUNDARY_BYTES, d2
	bra.w next
found
	move.l fragments.Boundary.New(a0), d1
	add.l FragmentBase, d1
	bcs.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d4/a0
	tst.l d0
	rts
	.bend ; mapFragmentOffset

; Idempotent cleanup on success and every failure path, including partial I/O.
releaseFragmentBuffers	.block
	movem.l d0/a0, -(sp)
	lea FragmentInput, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea FragmentOutput, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea FragmentMap, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l FragmentActive
	clr.l FragmentRows
	movem.l (sp)+, d0/a0
	tst.l d0
	rts
	.bend ; releaseFragmentBuffers
