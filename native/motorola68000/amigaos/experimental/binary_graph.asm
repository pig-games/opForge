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
SPAN_BYTES = 12
LIMIT = 512; independent module graph capacity
MAX_SPANS = LIMIT
Node	.struct
Start	.long ?
End	.long ?
File	.word ?
Color	.word ?
.endstruct
NODE_BYTES = 12
FrameEntry	.struct
Module	.word ?
Edge	.word ?
.endstruct
VISIT_BYTES = 4
GraphState	.struct
Cursor	.long ?
SourceIndex	.long ?
Count	.word ?
Invalid	.word ?
Ordered	.word ?
Reserved	.word ?
.endstruct
NODES = 16; Hunk treats struct-derived size expressions as relocatable
ROOTS = NODES+LIMIT*NODE_BYTES
STACK = ROOTS+LIMIT*2
NEXT = STACK+LIMIT*VISIT_BYTES
HEADS = NEXT+LIMIT*2
SCRATCH_BYTES = HEADS+LIMIT*2
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
	movem.l d1-d5/a0-a2, -(sp)
	; Graph arrays are keyed by binding index, not by module count alone.
	; Identity tables may grow beyond this independent bounded graph.
	cmpi.l #LIMIT, d1
	bhi.w bad
	cmpi.l #LIMIT, d2
	bhi.w bad
	move.l GraphState.Cursor(a0), d4
	move.l d4, d5
	add.l d0, d5
	bcs.w bad
	tst.w d1
	bne.w inside
	tst.w d2
	beq.w outside
	moveq #0, d3
	move.w GraphState.Count(a0), d3
	cmpi.w #LIMIT, d3
	bhs.w bad
	add.w d3, d3
	lea 6160(a0), a1
	move.w d2, 0(a1, d3.w)
	addq.w #1, GraphState.Count(a0)
	move.l d2, d3
	subq.w #1, d3
	mulu.w #NODE_BYTES, d3
	lea 16(a0), a1
	adda.l d3, a1
	tst.w Node.File(a1)
	bne.w bad
	move.l d4, Node.Start(a1)
	move.w 6(a0), d0
	move.w d0, Node.File(a1)
	bra.w advance
inside
	tst.w d2
	beq.w close
	cmp.w d1, d2
	bne.w bad
	bra.w advance
close
	move.l d1, d3
	subq.w #1, d3
	mulu.w #NODE_BYTES, d3
	lea 16(a0), a1
	adda.l d3, a1
	move.l d5, Node.End(a1)
	bra.w advance
outside
	cmpi.l #4, d0
	beq.w advance
	move.w #1, GraphState.Invalid(a0)
advance
	move.l d5, GraphState.Cursor(a0)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d5/a0-a2
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

; A0=graph,A1=scope state,A2=span buffer,D0=buffer bytes. D0=0,D1=span
; count on success; D0=2,D1=missing module index+1 for a discovery retry;
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
	move.l d7, d0
	add.w d0, d0
	lea 6160(a6), a0
	moveq #0, d1
	move.w 0(a0, d0.w), d1
	bsr.w getNode
	clr.w Node.Color(a0)
	move.l d1, d0
	subq.w #1, d0
	add.l d0, d0
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a5), a0
	adda.l d0, a0
	andi.w #$ffef, 0(a0)
	suba.l d0, a0
	addq.w #1, d7
	bra.w reset
headsStart
	; Reverse the preparation import lists into source-order numeric links.
	moveq #0, d7
heads
	cmp.w GraphState.Count(a6), d7
	bhs.w roots
	move.l d7, d0
	add.w d0, d0
	lea 6160(a6), a0
	moveq #0, d1
	move.w 0(a0, d0.w), d1
	subq.w #1, d1
	add.w d1, d1
	movea.l layout.IMPORT_STATE+imports.HEADS_POINTER(a5), a0
	moveq #0, d2
	move.w 0(a0, d1.w), d2
	lea 10256(a6), a1
	adda.w d1, a1
	clr.w (a1)  ; rebuild this head on every discovery retry
reverse
	tst.w d2
	beq.w headNext
	move.l d2, d0
	subq.w #1, d0
	move.l d0, d1
	add.w d1, d1
	lea 9232(a6), a0
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
	moveq #0, d7
root
	cmp.w GraphState.Count(a6), d7
	bhs.w ok
	move.l d7, d0
	add.w d0, d0
	lea 6160(a6), a0
	moveq #0, d1
	move.w 0(a0, d0.w), d1
	bsr.w getNode
	cmpi.w #1, Node.File(a0)
	bne.w rootNext
	cmpi.w #2, Node.Color(a0)
	beq.w rootNext
	moveq #0, d6
	bsr.w push
	bne.w bad
walk
	tst.w d6
	beq.w rootNext
	move.l d6, d0
	subq.w #1, d0
	lsl.l #2, d0
	lea 7184(a6), a3
	adda.l d0, a3
	moveq #0, d2
	move.w FrameEntry.Edge(a3), d2
	beq.w emit
	subq.w #1, d2
	move.l d2, d0
	add.w d0, d0
	lea 9232(a6), a0
	move.w 0(a0, d0.w), d0
	move.w d0, FrameEntry.Edge(a3)
	mulu.w #imports.ITEM_BYTES, d2
	lea layout.IMPORT_STATE+imports.ITEMS(a5), a0
	adda.l d2, a0
	moveq #0, d1
	move.w imports.Item.Target(a0), d1
	cmpi.l #LIMIT, d1
	bhs.w bad
	addq.w #1, d1
	bsr.w getNode
	tst.w Node.File(a0)
	beq.w missing
	cmpi.w #1, Node.Color(a0)
	beq.w bad
	cmpi.w #2, Node.Color(a0)
	beq.w walk
	bsr.w push
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
	bls.w bad
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
done
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; order
	.priv

; D1=module index+1,A6=graph. A0=node; D0 scratch, others preserved.
getNode	.block
	move.l d1, d0
	subq.w #1, d0
	mulu.w #NODE_BYTES, d0
	lea 16(a6), a0
	adda.l d0, a0
	rts
	.bend  ; getNode

; D1=unvisited module index+1,A0=node,A5=scope,A6=graph,D6=depth.
; Push one explicit DFS frame and mark exact module ownership as selected.
; D0/CCR=status; A1/D2 scratch, D6 incremented.
push	.block
	cmpi.w #LIMIT, d6
	bhs.w bad
	move.w #1, Node.Color(a0)
	move.l d1, d2
	subq.w #1, d2
	add.l d2, d2
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a5), a1
	adda.l d2, a1
	ori.w #modules.SELECTED, 0(a1)
	suba.l d2, a1
	move.l d6, d0
	lsl.l #2, d0
	lea 7184(a6), a1
	adda.l d0, a1
	move.w d1, FrameEntry.Module(a1)
	lea 10256(a6), a0
	move.w 0(a0, d2.w), d0
	move.w d0, FrameEntry.Edge(a1)
	addq.w #1, d6
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; push
	.endsection
	.endmodule
