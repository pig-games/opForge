; Incremental graph publication handshake and configuration ordering.
	.module opforge.tests.assembler.graph_publication_probe
	.cpu 68020
	.use opforge.assembler.modules.graph as graph
	.use opforge.assembler.symbols.scope_layout as layout
	.use opforge.assembler.symbols.imports as imports
	.use opforge.assembler.modules.bindings as modules
	.pub
	.section code, kind=code
; No inputs. D0/CCR=status, all other registers preserved; allocations released.
run	.block
	movem.l d1-d7/a0-a6, -(sp)
	moveq #88, d7
	bsr.w setup
	bne.w done
	lea Graph, a0
	moveq #2, d1
	jsr graph.requireConfiguration
	bne.w done
	bsr.w next
	cmpi.l #graph.READY, d0
	bne.w done
	cmpi.l #1, d1
	bne.w done
	tst.l Span+graph.Span.Start
	bne.w done
	cmpi.l #8, Span+graph.Span.End
	bne.w done
	; Yield cannot be overwritten, and configuration is still unpublished.
	bsr.w remaining
	cmpi.l #1, d0
	bne.w done
	bsr.w next
	cmpi.l #1, d0
	bne.w done
	lea Graph, a0
	jsr graph.acknowledge
	bne.w done
	jsr graph.acknowledge
	cmpi.l #1, d0
	bne.w done
	bsr.w remaining
	cmpi.l #3, d0
	bne.w done
	cmpi.l #2, d1
	bne.w done
	tst.w Graph+graph.GraphState.Ordered
	bne.w done
	bsr.w next
	cmpi.l #3, d0
	bne.w done
	cmpi.l #2, d1
	bne.w done
	lea Graph, a0
	jsr graph.configured
	bne.w done
	bsr.w next
	cmpi.l #graph.READY, d0
	bne.w done
	cmpi.l #8, Span+graph.Span.Start
	bne.w done
	lea Graph, a0
	jsr graph.acknowledge
	bne.w done
	bsr.w next
	tst.l d0
	bne.w done
	tst.l d1
	bne.w done
	; Reinitialization clears publication state; legacy order still emits all.
	lea Graph, a0
	jsr graph.release
	bsr.w setup
	bne.w done
	lea Graph, a0
	lea Scope, a1
	lea Span, a2
	moveq #2*graph.SPAN_BYTES, d0
	jsr graph.order
	bne.w done
	cmpi.l #2, d1
	bne.w done
	; A complete remaining order skips acknowledged spans and seals once.
	lea Graph, a0
	jsr graph.release
	bsr.w setup
	bne.w done
	bsr.w next
	cmpi.l #graph.READY, d0
	bne.w done
	lea Graph, a0
	jsr graph.acknowledge
	bne.w done
	bsr.w remaining
	bne.w done
	cmpi.l #1, d1
	bne.w done
	cmpi.l #8, Span+graph.Span.Start
	bne.w done
	cmpi.l #16, Span+graph.Span.End
	bne.w done
	tst.w Graph+graph.GraphState.Ordered
	beq.w done
	tst.w Graph+graph.GraphState.Pending
	bne.w done
	; A later missing dependency cannot hide an earlier complete root.
	lea Graph, a0
	jsr graph.release
	bsr.w setup
	bne.w done
	move.w #1, Scope+layout.IMPORT_STATE+imports.COUNT
	move.l #Item, Scope+layout.IMPORT_STATE+imports.ITEMS_POINTER
	move.w #1, Heads+2
	move.w #2, Item+imports.Item.Target
	clr.w Item+imports.Item.Next
	bsr.w next
	cmpi.l #graph.READY, d0
	bne.w done
	lea Graph, a0
	jsr graph.acknowledge
	bne.w done
	bsr.w next
	cmpi.l #2, d0
	bne.w done
	cmpi.l #3, d1
	bne.w done
	; Turning that unresolved edge into a self-cycle is still a hard error.
	move.w #1, Item+imports.Item.Target
	bsr.w next
	cmpi.l #1, d0
	bne.w done
	moveq #0, d7
done
	lea Graph, a0
	jsr graph.release
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; run
	.priv
next	.block
	lea Graph, a0
	lea Scope, a1
	lea Span, a2
	moveq #graph.SPAN_BYTES, d0
	jsr graph.orderNext
	rts
	.bend  ; next
remaining	.block
	lea Graph, a0
	lea Scope, a1
	lea Span, a2
	moveq #2*graph.SPAN_BYTES, d0
	jsr graph.orderRemaining
	rts
	.bend  ; remaining
setup	.block
	lea Graph, a0
	jsr graph.begin
	bne.w done
	move.w #2, Scope+layout.State.Count
	move.l #Flags, Scope+layout.MODULE_STATE+modules.FLAGS_POINTER
	move.l #Heads, Scope+layout.IMPORT_STATE+imports.HEADS_POINTER
	clr.w Scope+layout.IMPORT_STATE+imports.COUNT
	clr.w Flags
	clr.w Flags+2
	clr.w Heads
	clr.w Heads+2
	; Two complete root-file modules in declaration order.
	moveq #4, d0
	moveq #0, d1
	moveq #1, d2
	jsr graph.line
	bne.w done
	moveq #4, d0
	moveq #1, d1
	moveq #0, d2
	jsr graph.line
	bne.w done
	moveq #4, d0
	moveq #0, d1
	moveq #2, d2
	jsr graph.line
	bne.w done
	moveq #4, d0
	moveq #2, d1
	moveq #0, d2
	jsr graph.line
done
	rts
	.bend  ; setup
	.endsection
	.section bss, kind=bss
Graph	.res byte, graph.SCRATCH_BYTES
Scope	.res byte, layout.IMPORT_STATE+imports.SCRATCH_BYTES
Span	.res byte, 2*graph.SPAN_BYTES
Flags	.res word, 2
Heads	.res word, 2
Item	.res byte, imports.ITEM_BYTES
	.endsection
	.endmodule
