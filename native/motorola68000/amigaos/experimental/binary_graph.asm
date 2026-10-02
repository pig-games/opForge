; Numeric dependency-first module spans for explicitly supplied source files.
; @opforge-owner: experimental.amigaos.binary_graph
	.module experimental.amigaos.binary_graph
	.cpu 68020
	.include "telemetry_macros.i"
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_imports as imports
	.pub
Span	.struct
Start	.long ?
End	.long ?
File	.long ?
.endstruct
SPAN_BYTES = Span.File+4
ENTRY_TARGET = 32; graph-private static incoming-use flag in module metadata
LIMIT = 512; independent module graph capacity
MAX_SPANS = LIMIT
Node	.struct
Start	.long ?
End	.long ?
File	.word ?
Color	.word ?
Binding	.word ?
Configured	.word ?
.endstruct
NODE_BYTES = Node.Configured+2
FrameEntry	.struct
Module	.word ?
Edge	.word ?
.endstruct
VISIT_BYTES = FrameEntry.Edge+2
GraphState	.struct
Cursor	.long ?
SourceIndex	.long ?
Count	.word ?
Invalid	.word ?
Ordered	.word ?
Reserved	.word ?
.endstruct
NODES = GraphState.Reserved+2
STACK = NODES+LIMIT*NODE_BYTES
NEXT = STACK+LIMIT*VISIT_BYTES
EDGE_HEADS = NEXT+imports.LIST_LIMIT*2
HASH = EDGE_HEADS+LIMIT*2
HASH_SLOTS = LIMIT*2; at most half full; entries hold dense index+1
HASH_MASK = HASH_SLOTS-1
SCRATCH_BYTES = HASH+HASH_SLOTS*2
	.section code, kind=code

; A0=caller-owned aligned graph scratch. D0/CCR=status; other registers kept.
begin	.block
	movem.l d1/a0, -(sp)
	move.w #SCRATCH_BYTES/2-1, d1
clear
	clr.w (a0)+
	dbra d1, clear
	movem.l (sp)+, d1/a0
	move.l #1, GraphState.SourceIndex(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=graph,D0=packed line bytes,D1=module before,D2=module after (index+1).
; Capture original record offsets; retain no process pointers. D0/CCR=status;
; other registers kept. Unsupported outside-module values invalidate graph mode.
line	.block
	movem.l d1-d5/a0-a2/a6, -(sp)
	movea.l a0, a6
	; Source binding identities are independent of the compact graph capacity.
	cmpi.l #65535, d1
	bhi.w bad
	cmpi.l #65535, d2
	bhi.w bad
	move.l GraphState.Cursor(a6), d4
	move.l d4, d5
	add.l d0, d5
	bcs.w bad
	tst.w d1
	bne.w inside
	tst.w d2
	beq.w outside
	move.l d2, d1
	bsr.w findBinding
	tst.w d2
	bne.w bad
	moveq #0, d3
	move.w GraphState.Count(a6), d3
	cmpi.w #LIMIT, d3
	bhs.w bad
	move.l d3, d0
	mulu.w #NODE_BYTES, d0
	lea NODES(a6), a0
	adda.l d0, a0
	addq.w #1, d3
	move.w d3, (a1)
	move.w d3, GraphState.Count(a6)
	move.w d1, Node.Binding(a0)
	move.w #1, Node.Configured(a0)  ; ordinary streaming nodes are already prepared
	move.l d4, Node.Start(a0)
	move.w GraphState.SourceIndex+2(a6), d0
	move.w d0, Node.File(a0)
	bra.w advance
inside
	tst.w d2
	beq.w close
	cmp.w d1, d2
	bne.w bad
	bra.w advance
close
	bsr.w findBinding
	tst.w d2
	beq.w bad
	move.l d5, Node.End(a0)
	bra.w advance
outside
	cmpi.l #4, d0
	beq.w advance
	move.w #1, GraphState.Invalid(a6)
advance
	move.l d5, GraphState.Cursor(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d5/a0-a2/a6
	tst.l d0
	rts
	.bend  ; line

; A0=graph. Advance the one-based provenance ordinal. D0/CCR=status, others kept.
endFile	.block
	cmpi.l #65535, GraphState.SourceIndex(a0)
	bhs.w bad
	addq.l #1, GraphState.SourceIndex(a0)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; endFile

; A0=graph,D0=owned capture extent. Advance an outside-module capture offset
; without interpreting its physical storage size as semantic body content.
; D0/CCR=status; other registers preserved.
advanceCapture	.block
	move.l d1, -(sp)
	move.l GraphState.Cursor(a0), d1
	add.l d0, d1
	bcs.w bad
	move.l d1, GraphState.Cursor(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; advanceCapture

; A0=graph,D1=source binding index+1. Mark a captured module for a separate
; configuration scan before its dependencies are traversed. D0/CCR=status;
; other registers preserved. Must precede successful ordering.
requireConfiguration	.block
	movem.l d1-d2/a0-a1/a6, -(sp)
	movea.l a0, a6
	tst.w GraphState.Ordered(a6)
	bne.w bad
	bsr.w findBinding
	tst.w d2
	beq.w bad
	clr.w Node.Configured(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1/a6
	tst.l d0
	rts
	.bend  ; requireConfiguration

; A0=graph,D1=source binding index+1. Publish a completed configuration scan.
; D0/CCR=status; other registers preserved. Retry order after this call.
configured	.block
	movem.l d1-d2/a0-a1/a6, -(sp)
	movea.l a0, a6
	tst.w GraphState.Ordered(a6)
	bne.w bad
	bsr.w findBinding
	tst.w d2
	beq.w bad
	move.w #1, Node.Configured(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1/a6
	tst.l d0
	rts
	.bend  ; configured

; A0=graph,A1=scope,D1=canonical module binding index+1. Mark a static
; active .use target, including forward entry declarations. No graph edge or
; configuration is created. D0/CCR=status; other registers preserved.
entryTarget	.block
	movem.l d1-d2/a0-a2, -(sp)
	tst.l d1
	beq.w bad
	cmp.w layout.State.Count(a1), d1
	bhi.w bad
	subq.l #1, d1
	add.l d1, d1
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a1), a2
	ori.w #ENTRY_TARGET, 0(a2, d1.l)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; entryTarget

; A0=graph,A1=scope state,A2=span buffer,D0=buffer bytes. D0=0,D1=span
; count on success; D0=2,D1=missing module index+1 for a discovery retry;
; D0=3,D1=present module index+1 needing configuration before traversal;
; D0=1,D1=0 for invalid graph. CCR reflects D0; other registers preserved.
; Visit entry-file modules in declaration order and imports in source order.
order	.block
	movem.l d2-d7/a0-a6, -(sp)
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_STATE
	movea.l a0, a6
	movea.l a1, a5
	movea.l a2, a4
	move.l d0, d5
	moveq #0, d4
	tst.w GraphState.Invalid(a6)
	bne.w bad
	tst.w GraphState.Ordered(a6)
	bne.w bad
	; A missing dependency can cause discovery to load another file and retry.
	; Recompute selection from the complete graph on every attempt.
	moveq #0, d7
reset
	cmp.w GraphState.Count(a6), d7
	bhs.w headsStart
	move.l d7, d1
	addq.w #1, d1
	bsr.w getNode
	clr.w Node.Color(a0)
	moveq #0, d0
	move.w Node.Binding(a0), d0
	subq.l #1, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a5), a0
	adda.l d0, a0
	andi.w #$ffff-modules.SELECTED, 0(a0)
	suba.l d0, a0
	addq.w #1, d7
	bra.w reset
headsStart
	; Reverse the preparation import lists into source-order numeric links.
	moveq #0, d7
heads
	cmp.w GraphState.Count(a6), d7
	bhs.w roots
	move.l d7, d1
	addq.w #1, d1
	bsr.w getNode
	moveq #0, d1
	move.w Node.Binding(a0), d1
	subq.l #1, d1
	add.l d1, d1
	movea.l layout.IMPORT_STATE+imports.HEADS_POINTER(a5), a0
	adda.l d1, a0
	moveq #0, d2
	move.w (a0), d2
	move.l d7, d1
	add.w d1, d1
	lea EDGE_HEADS(a6), a1
	adda.w d1, a1
	clr.w (a1)  ; rebuild this head on every discovery retry
reverse
	tst.w d2
	beq.w headNext
	move.l d2, d0
	subq.w #1, d0
	move.l d0, d1
	add.w d1, d1
	lea NEXT(a6), a0
	move.w (a1), d0
	move.w d0, 0(a0, d1.w)
	move.w d2, (a1)
	move.l d2, d0
	subq.w #1, d0
	mulu.w #imports.ITEM_BYTES, d0
	lea layout.IMPORT_STATE+imports.ITEMS(a5), a0
	adda.l d0, a0
	moveq #0, d2
	move.w imports.Item.Next(a0), d2
	bra.w reverse
headNext
	addq.w #1, d7
	bra.w heads
roots
	clr.w GraphState.Reserved(a6)  ; stable non-target pass, then target pass
	moveq #0, d7
root
	cmp.w GraphState.Count(a6), d7
	bhs.w nextRootPass
	move.l d7, d1
	addq.w #1, d1
	bsr.w getNode
	cmpi.w #1, Node.File(a0)
	bne.w rootNext
	cmpi.w #2, Node.Color(a0)
	beq.w rootNext
	moveq #0, d0
	move.w Node.Binding(a0), d0
	subq.w #1, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a5), a1
	move.w 0(a1, d0.l), d0
	andi.w #ENTRY_TARGET, d0
	beq.w nonTarget
	tst.w GraphState.Reserved(a6)
	beq.w rootNext
	bra.w visitRoot
nonTarget
	tst.w GraphState.Reserved(a6)
	bne.w rootNext
visitRoot
	moveq #0, d6
	bsr.w push
	cmpi.l #3, d0
	beq.w needsConfiguration
	tst.l d0
	bne.w bad
walk
	tst.w d6
	beq.w rootNext
	move.l d6, d0
	subq.w #1, d0
	lsl.l #2, d0
	lea STACK(a6), a3
	adda.l d0, a3
	moveq #0, d2
	move.w FrameEntry.Edge(a3), d2
	beq.w emit
	subq.w #1, d2
	move.l d2, d0
	add.w d0, d0
	lea NEXT(a6), a0
	move.w 0(a0, d0.w), d0
	move.w d0, FrameEntry.Edge(a3)
	mulu.w #imports.ITEM_BYTES, d2
	lea layout.IMPORT_STATE+imports.ITEMS(a5), a0
	adda.l d2, a0
	moveq #0, d1
	move.w imports.Item.Target(a0), d1
	addq.l #1, d1
	cmpi.l #65535, d1
	bhi.w bad
	bsr.w findBinding
	tst.w d2
	beq.w missing
	move.l d2, d1
	cmpi.w #1, Node.Color(a0)
	beq.w bad
	cmpi.w #2, Node.Color(a0)
	beq.w walk
	bsr.w push
	cmpi.l #3, d0
	beq.w needsConfiguration
	tst.l d0
	bne.w bad
	bra.w walk
emit
	moveq #0, d1
	move.w FrameEntry.Module(a3), d1
	bsr.w getNode
	cmpi.l #SPAN_BYTES, d5
	blo.w bad
	move.l Node.End(a0), d0
	cmp.l Node.Start(a0), d0
	blo.w bad
	move.l Node.Start(a0), (a4)+
	move.l d0, (a4)+
	moveq #0, d0
	move.w Node.File(a0), d0
	move.l d0, (a4)+
	subi.l #SPAN_BYTES, d5
	addq.w #1, d4
	move.w #2, Node.Color(a0)
	subq.w #1, d6
	bra.w walk
rootNext
	addq.w #1, d7
	bra.w root
nextRootPass
	tst.w GraphState.Reserved(a6)
	bne.w ok
	move.w #1, GraphState.Reserved(a6)
	moveq #0, d7
	bra.w root
ok
	move.w #1, GraphState.Ordered(a6)
	move.w #1, layout.MODULE_STATE+modules.State.Selection(a5)
	move.l d4, d1
	moveq #0, d0
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
	bra.w done
missing
	moveq #2, d0  ; D1 remains the requested module index+1
	bra.w done
needsConfiguration
	moveq #0, d1
	move.w Node.Binding(a0), d1
	moveq #3, d0
done
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; order

; A0=ordered graph,D1=source binding index+1. Resolve independent dense storage.
; D0/CCR=zero and A0=node when present; nonzero and A0 unchanged when absent.
; Other registers preserved. The graph owns the returned node until reset.
nodeForBinding	.block
	movem.l d2/a1/a6, -(sp)
	move.l a0, -(sp)
	movea.l a0, a6
	tst.l d1
	beq.w missing
	cmpi.l #65535, d1
	bhi.w missing
	bsr.w findBinding
	tst.w d2
	beq.w missing
	moveq #0, d0
	bra.w done
missing
	movea.l (sp), a0
	moveq #1, d0
done
	addq.l #4, sp
	movem.l (sp)+, d2/a1/a6
	tst.l d0
	rts
	.bend  ; nodeForBinding
	.priv

; D1=source binding index+1,A6=graph. D2=dense index+1 (zero if absent).
; A0=node when present; A1=matching/empty hash cell; D0 scratch, others kept.
; No deletions and at most LIMIT entries in HASH_SLOTS guarantee an empty cell.
findBinding	.block
	move.l d3, -(sp)
	move.l d1, d3
probe
	andi.l #HASH_MASK, d3
	move.l d3, d0
	add.w d0, d0
	lea HASH(a6), a1
	adda.w d0, a1
	moveq #0, d2
	move.w (a1), d2
	beq.w done
	move.l d2, d0
	subq.w #1, d0
	mulu.w #NODE_BYTES, d0
	lea NODES(a6), a0
	adda.l d0, a0
	cmp.w Node.Binding(a0), d1
	beq.w done
	addq.l #1, d3
	bra.w probe
done
	move.l (sp)+, d3
	rts
	.bend  ; findBinding

; D1=dense module index+1,A6=graph. A0=node; D0 scratch, others preserved.
getNode	.block
	move.l d1, d0
	subq.w #1, d0
	mulu.w #NODE_BYTES, d0
	lea NODES(a6), a0
	adda.l d0, a0
	rts
	.bend  ; getNode

; D1=unvisited dense module index+1,A0=node,A5=scope,A6=graph,D6=depth.
; Push one explicit DFS frame and mark exact module ownership as selected.
; D0/CCR=status; A1/D2 scratch, D6 incremented.
push	.block
	tst.w Node.Configured(a0)
	beq.w needsConfiguration
	cmpi.w #LIMIT, d6
	bhs.w bad
	move.w #1, Node.Color(a0)
	moveq #0, d2
	move.w Node.Binding(a0), d2
	subq.l #1, d2
	add.l d2, d2
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a5), a1
	adda.l d2, a1
	ori.w #modules.SELECTED, 0(a1)
	suba.l d2, a1
	move.l d6, d0
	lsl.l #2, d0
	lea STACK(a6), a1
	adda.l d0, a1
	move.w d1, FrameEntry.Module(a1)
	move.l d1, d2
	subq.w #1, d2
	add.w d2, d2
	lea EDGE_HEADS(a6), a0
	move.w 0(a0, d2.w), d0
	move.w d0, FrameEntry.Edge(a1)
	addq.w #1, d6
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
needsConfiguration
	moveq #3, d0
	rts
	.bend  ; push
	.endsection
	.endmodule
