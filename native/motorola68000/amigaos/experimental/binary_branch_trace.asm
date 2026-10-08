; Bounded sizing observations. Decisions are numeric offsets and scalar truth.
; This owns convergence bookkeeping, never condition grammar or evaluation.
; @opforge-owner: experimental.amigaos.binary_branch_trace
	.module experimental.amigaos.binary_branch_trace
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.pub
Row	.struct
Offset	.long ?
Truth	.long ?
	.endstruct
ROW_BYTES = Row.Truth+4
State	.struct
Enabled	.long ?
PreviousBytes	.long ?
Cursor	.long ?
Changed	.long ?
Block	.res memory.Block.Used+4
	.endstruct
STATE_BYTES = State.Block+memory.Block.Used+4
	.section code, kind=code
; A0=state. Begin sizing/emission observation; keep the previous decision rows.
; Registers preserved; CCR unspecified. Disabled sources allocate nothing.
begin	.block
	move.l State.Block+memory.Block.Used(a0), State.PreviousBytes(a0)
	clr.l State.Cursor(a0)
	clr.l State.Changed(a0)
	rts
	.bend  ; begin

; A0=state,D0=source record offset,D1=truth (zero/nonzero).
; Compare and overwrite one previous row in traversal order. Newly active
; nested conditions may change row count; a subsequent sweep must stabilize it.
; D0/CCR=status; other registers preserved. Loop identity is checked separately
; by the repetition trace, so differing invocation paths cannot alias these rows.
observe	.block
	movem.l d1-d4/a0-a2, -(sp)
	movea.l a0, a2
	move.l d0, d3
	moveq #0, d4
	tst.l d1
	beq.w normalized
	moveq #1, d4
normalized
	move.l State.Cursor(a2), d2
	cmp.l State.PreviousBytes(a2), d2
	bhs.w changed
	movea.l State.Block+memory.Block.Pointer(a2), a1
	adda.l d2, a1
	cmp.l Row.Offset(a1), d3
	bne.w changed
	cmp.l Row.Truth(a1), d4
	beq.w append
changed
	move.l #1, State.Changed(a2)
append
	move.l d2, d0
	addi.l #ROW_BYTES, d0
	bcs.w bad
	lea State.Block(a2), a0
	jsr memory.reserve
	bne.w done
	movea.l memory.Block.Pointer(a0), a1
	adda.l d2, a1
	move.l d3, Row.Offset(a1)
	move.l d4, Row.Truth(a1)
	addi.l #ROW_BYTES, d2
	move.l d2, State.Cursor(a2)
	cmp.l State.Block+memory.Block.Used(a2), d2
	bls.w stored
	move.l d2, State.Block+memory.Block.Used(a2)
stored
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a2
	tst.l d0
	rts
	.bend  ; observe

; A0=state. Finish the complete source pass, including all section sweeps.
; D0/CCR=zero when the decision sequence is unchanged; others preserved.
end	.block
	move.l State.Cursor(a0), d0
	cmp.l State.PreviousBytes(a0), d0
	beq.w lengthReady
	move.l #1, State.Changed(a0)
lengthReady
	move.l d0, State.Block+memory.Block.Used(a0)
	move.l State.Changed(a0), d0
	tst.l d0
	rts
	.bend  ; end

; A0=state. Release observations on every assembly exit.
; Registers preserved; CCR reflects the caller's D0 after cleanup.
release	.block
	clr.l State.Enabled(a0)
	clr.l State.PreviousBytes(a0)
	clr.l State.Cursor(a0)
	clr.l State.Changed(a0)
	move.l a0, -(sp)
	lea State.Block(a0), a0
	jsr memory.release
	movea.l (sp)+, a0
	clr.l State.Block+memory.Block.Used(a0)
	tst.l d0
	rts
	.bend  ; release
	.endsection
	.endmodule
