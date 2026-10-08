; @opforge-owner: experimental.amigaos.binary_repetition_trace
; Bounded sparse repetition observations. Parent offsets encode nesting paths.
	.module experimental.amigaos.binary_repetition_trace
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.pub
MODE_CAPTURE = 1
MODE_COMPARE = 2
Record	.struct
SourceOffset	.long ?
Parent	.long ?
ParentIteration	.long ?
Count	.long ?
	.endstruct
RECORD_BYTES = Record.Count+4
State	.struct
Mode	.word ?
Padding	.word ?
Cursor	.long ?
Block	.res memory.Block.Used+4
	.endstruct
STATE_BYTES = State.Block+memory.Block.Used+4
	.section code, kind=code
; A0=zero-initialized or released state,D0=assembly pass (1/2).
; Start capture afresh for provisional pass-one layout retries; emission compares.
; D0/CCR=status; others preserved. Allocation is retained until release.
beginPass	.block
	cmpi.l #MODE_CAPTURE, d0
	beq.w capture
	cmpi.l #MODE_COMPARE, d0
	bne.w bad
	move.w #MODE_COMPARE, State.Mode(a0)
	bra.w ready
capture
	move.w #MODE_CAPTURE, State.Mode(a0)
	clr.l State.Block+memory.Block.Used(a0)
ready
	clr.l State.Cursor(a0)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; beginPass

; A0=state. Emission must consume the complete captured traversal.
; D0/CCR=status; other registers preserved.
endPass	.block
	moveq #0, d0
	cmpi.w #MODE_COMPARE, State.Mode(a0)
	bne.w done
	move.l State.Cursor(a0), d0
	cmp.l State.Block+memory.Block.Used(a0), d0
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; endPass

; A0=state,D0=source record offset,D1=parent row offset/root sweep key,
; D2=parent iteration. D0/CCR=status,D1=stable row byte offset on success.
; Other registers preserved. No source paths or pointers enter observations.
open	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	move.l d0, d4
	move.l d1, d5
	move.l d2, d6
	move.l State.Cursor(a5), d7
	cmpi.w #MODE_CAPTURE, State.Mode(a5)
	beq.w capture
	cmpi.w #MODE_COMPARE, State.Mode(a5)
	bne.w bad
	move.l State.Block+memory.Block.Used(a5), d0
	sub.l d7, d0
	bcs.w bad
	cmpi.l #RECORD_BYTES, d0
	blo.w bad
	movea.l State.Block+memory.Block.Pointer(a5), a4
	adda.l d7, a4
	cmp.l Record.SourceOffset(a4), d4
	bne.w bad
	cmp.l Record.Parent(a4), d5
	bne.w bad
	cmp.l Record.ParentIteration(a4), d6
	bne.w bad
	bra.w ready
capture
	move.l State.Block+memory.Block.Used(a5), d7
	move.l d7, d0
	addi.l #RECORD_BYTES, d0
	bcs.w bad
	lea State.Block(a5), a0
	jsr memory.reserve
	bne.w bad
	movea.l State.Block+memory.Block.Pointer(a5), a4
	adda.l d7, a4
	move.l d4, Record.SourceOffset(a4)
	move.l d5, Record.Parent(a4)
	move.l d6, Record.ParentIteration(a4)
	clr.l Record.Count(a4)
	move.l d7, d0
	addi.l #RECORD_BYTES, d0
	move.l d0, State.Block+memory.Block.Used(a5)
ready
	move.l d7, d1
	addi.l #RECORD_BYTES, d7
	move.l d7, State.Cursor(a5)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; open

; A0=state,D0=completed iteration count,D1=observation byte offset.
; Capture counts only on completion; emission compares the same executed loop.
; D0/CCR=status; other registers preserved. Incomplete loops cannot qualify.
finish	.block
	movem.l d1-d3/a0-a1, -(sp)
	move.l d0, d3
	move.l State.Block+memory.Block.Used(a0), d2
	sub.l d1, d2
	bcs.w bad
	cmpi.l #RECORD_BYTES, d2
	blo.w bad
	movea.l State.Block+memory.Block.Pointer(a0), a1
	adda.l d1, a1
	cmpi.w #MODE_CAPTURE, State.Mode(a0)
	beq.w capture
	cmpi.w #MODE_COMPARE, State.Mode(a0)
	bne.w bad
	cmp.l Record.Count(a1), d3
	bne.w bad
	bra.w ready
capture
	move.l d3, Record.Count(a1)
ready
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; finish

; A0=state. Release every success/failure path's transient observation arena.
; All registers preserved; CCR unspecified. Safe before any allocation.
release	.block
	move.l a0, -(sp)
	clr.w State.Mode(a0)
	clr.l State.Cursor(a0)
	clr.l State.Block+memory.Block.Used(a0)
	lea State.Block(a0), a0
	jsr memory.release
	movea.l (sp)+, a0
	rts
	.bend  ; release
	.endsection
	.endmodule
