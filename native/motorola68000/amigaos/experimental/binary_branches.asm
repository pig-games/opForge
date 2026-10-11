; Runtime conditional selection before loop/state/emission effects.
; @opforge-owner: experimental.amigaos.binary_branches
	.module experimental.amigaos.binary_branches
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use prvm.amigaos.packed_branch as parser
	.use opasm.amigaos.binary_expression as expression
	.use experimental.amigaos.binary_repetition as loops
	.use experimental.amigaos.binary_branch_trace as trace
	.include "memory_telemetry.i"
	.pub
LIMIT = 16
RESULT_ACTIVE = 0
RESULT_CONSUMED = 1
RESULT_INVALID = 2
RESULT_SELECTED = 3
Slot	.struct
Parent	.byte ?
Taken	.byte ?
ElseSeen	.byte ?
Kind	.byte ?
ValueLow	.long ?
Reserved	.long ?
Unavailable	.long ?
	.endstruct
SLOT_BYTES = Slot.Unavailable+4
SLOT_SHIFT = 4
State	.struct
Depth	.word ?
Active	.word ?
Strict	.word ?
Deferred	.word ?
Loops	.long ?
Records	.long ?
Proof	.long ?  ; optional preparation provenance veto; zero during normal execution
OperandOffset	.word ?  ; original record operand, supplied by packed PRVM
Trace	.res trace.STATE_BYTES
Slots	.res LIMIT*SLOT_BYTES
	.endstruct
STATE_BYTES = State.Slots+LIMIT*SLOT_BYTES
	.section code, kind=code
; A0=state,A1=loop state,A2=records,D0=strict guard evaluation (0/1).
; Begin a complete sizing/emission pass; per-sweep branch stacks reset separately.
; Registers preserved; CCR unspecified. Trace storage is enabled only after
; a sizing guard encounters an unavailable value with retained control ownership.
beginPass	.block
	move.w d0, State.Strict(a0)
	clr.w State.Deferred(a0)
	clr.l State.Proof(a0)
	move.l a1, State.Loops(a0)
	move.l a2, State.Records(a0)
	move.l a0, -(sp)
	lea State.Trace(a0), a0
	jsr trace.begin
	movea.l (sp)+, a0
	rts
	.bend  ; beginPass

; A0=state,A1=current records. Trace rows retain offsets, never source views.
; Other registers preserved; D0/CCR=status.
refresh	.block
	move.l a1, State.Records(a0)
	moveq #0, d0
	rts
	.bend  ; refresh

; A0=state. D0/CCR=nonzero if bounded sizing replay is still needed.
; Other registers preserved; final emission requires a stable decision trace.
endPass	.block
	move.l a0, -(sp)
	lea State.Trace(a0), a0
	jsr trace.end
	movea.l (sp)+, a0
	tst.w State.Deferred(a0)
	beq.w done
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; endPass

; A0=state. Release sizing observations on every exit; registers preserved.
; CCR reflects the caller's D0 after cleanup.
release	.block
	move.l a0, -(sp)
	lea State.Trace(a0), a0
	jsr trace.release
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; release

; A0=state. Start an independent source sweep. D0/CCR=status; others kept.
begin	.block
	clr.w State.Depth(a0)
	move.w #1, State.Active(a0)
	moveq #0, d0
	rts
	.bend  ; begin
; A0=state. Reject unterminated controls. D0/CCR=status; others kept.
end	.block
	moveq #0, d0
	tst.w State.Depth(a0)
	beq.w done
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; end
; A0=record,A1=stream end,A2=context,A3=branch state.
; D0/CCR=status,D1=parent-active numeric label prefix bytes; others preserved.
; Publish before step evaluates the guard so a control can reference its own PC.
; PRVM owns the bounded syntax; this owner consults only the enclosing activity.
prefix	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a3, a6
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	movea.l a0, a4
	adda.l d0, a4
	cmpa.l a1, a4
	bhi.w bad
	movea.l package.Context.Package(a2), a1
	adda.l package.Header.BranchPlan(a1), a1
	jsr parser.classifyDetailed
	bne.w bad
	tst.l d1
	beq.w absent
	cmpi.w #parser.MATCH, d1
	beq.w opening
	cmpi.w #parser.IF, d1
	beq.w opening
	moveq #0, d2
	move.w State.Depth(a6), d2
	beq.w bad
	subq.w #1, d2
	lsl.w #SLOT_SHIFT, d2
	tst.b State.Slots+Slot.Parent(a6, d2.w)
	beq.w absent
	bra.w present
opening
	tst.w State.Active(a6)
	beq.w absent
present
	move.l d3, d1
	moveq #0, d0
	bra.w done
absent
	moveq #0, d1
	moveq #0, d0
	bra.w done
bad
	moveq #0, d1
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; prefix

; A0=record,A1=stream end,A2=context,A3=branch state.
; D0=0 active ordinary,1 consumed/inactive,2 invalid,3 selected branch.
; Other registers kept. A selected branch may activate local dependency roots.
; Controls retain only numeric heads and EXVM expressions; no source text.
step	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a3, a6
	moveq #0, d4
	move.b (a5), d4  ; base points at parser.Record.Length
	addq.w #1, d4
	movea.l a5, a4
	adda.l d4, a4
	cmpa.l a1, a4
	bhi.w bad
	movea.l package.Context.Package(a2), a1
	adda.l package.Header.BranchPlan(a1), a1
	move.l d4, d0
	jsr parser.classifyDetailed
	bne.w bad
	move.w d2, State.OperandOffset(a6)
	move.l d1, d7
	tst.l d1
	beq.w ordinary
	moveq #0, d6
	move.w State.Depth(a6), d6
	cmpi.w #parser.MATCH, d7
	beq.w opening
	cmpi.w #parser.IF, d7
	beq.w opening
	tst.w d6
	beq.w bad
	subq.w #1, d6
	lsl.w #SLOT_SHIFT, d6
	lea State.Slots(a6), a3
	adda.w d6, a3
	cmpi.w #parser.ENDMATCH, d7
	beq.w matchClosing
	cmpi.w #parser.ENDIF, d7
	beq.w ifClosing
	cmpi.w #parser.CASE, d7
	beq.w matchBranch
	cmpi.w #parser.DEFAULT, d7
	beq.w matchBranch
	cmpi.b #parser.IF, Slot.Kind(a3)
	bne.w bad
	tst.b Slot.ElseSeen(a3)
	bne.w bad
	clr.w State.Active(a6)
	cmpi.w #parser.ELSE, d7
	beq.w terminal
	tst.b Slot.Parent(a3)
	beq.w consumed
	tst.b Slot.Taken(a3)
	bne.w consumed
	bsr.w evaluate
	bne.w bad
	tst.l d1
	beq.w consumed
	move.b #1, Slot.Taken(a3)
	move.w #1, State.Active(a6)
	bra.w selected
terminal
	move.b #1, Slot.ElseSeen(a3)
	moveq #0, d0
	move.b Slot.Taken(a3), d0
	eori.b #1, d0
	and.b Slot.Parent(a3), d0
	move.w d0, State.Active(a6)
	move.b #1, Slot.Taken(a3)
	tst.w State.Active(a6)
	bne.w selected
	bra.w consumed
opening
	cmpi.w #LIMIT, d6
	bhs.w bad
	lsl.w #SLOT_SHIFT, d6
	lea State.Slots(a6), a3
	adda.w d6, a3
	move.w State.Active(a6), d0
	move.b d0, Slot.Parent(a3)
	clr.b Slot.Taken(a3)
	clr.b Slot.ElseSeen(a3)
	move.b d7, Slot.Kind(a3)
	clr.l Slot.Unavailable(a3)
	addq.w #1, State.Depth(a6)
	clr.w State.Active(a6)
	tst.w d0
	beq.w consumed
	cmpi.w #parser.MATCH, d7
	beq.w matchValue
	bsr.w evaluate
	bne.w bad
	clr.w State.Active(a6)
	tst.l d1
	beq.w consumed
	move.b #1, Slot.Taken(a3)
	move.w #1, State.Active(a6)
	bra.w selected
matchBranch
	cmpi.b #parser.MATCH, Slot.Kind(a3)
	bne.w bad
	tst.b Slot.ElseSeen(a3)
	bne.w bad
	clr.w State.Active(a6)
	cmpi.w #parser.DEFAULT, d7
	beq.w terminal
	tst.b Slot.Parent(a3)
	beq.w consumed
	tst.b Slot.Taken(a3)
	bne.w consumed
	moveq #0, d0
	move.w State.OperandOffset(a6), d0
	lea 0(a5, d0.w), a0
	movea.l a4, a1
caseValue
	bsr.w evaluateScalar
	bne.w bad
	tst.l Slot.Unavailable(a3)
	bne.w nextCase
	tst.l d2
	bne.w nextCase
	cmp.l Slot.ValueLow(a3), d1
	bne.w nextCase
	move.b #1, Slot.Taken(a3)
	move.w #1, State.Active(a6)
	bra.w selected
nextCase
	cmpa.l a1, a0
	beq.w consumed
	cmpi.b #parser.COMMA_TOKEN, (a0)+  ; shared packed comma between scalar wrappers
	bne.w bad
	bra.w caseValue
matchValue
	moveq #0, d0
	move.w State.OperandOffset(a6), d0
	lea 0(a5, d0.w), a0
	movea.l a4, a1
	bsr.w evaluateScalar
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	move.l d1, Slot.ValueLow(a3)
	move.l d2, Slot.Unavailable(a3)
	bra.w consumed
matchClosing
	cmpi.b #parser.MATCH, Slot.Kind(a3)
	bne.w bad
	bra.w closing
ifClosing
	cmpi.b #parser.IF, Slot.Kind(a3)
	bne.w bad
closing
	subq.w #1, State.Depth(a6)
	moveq #0, d0
	move.b Slot.Parent(a3), d0
	move.w d0, State.Active(a6)
	bra.w consumed
ordinary
	moveq #0, d0
	tst.w State.Active(a6)
	bne.w done
	bra.w consumed
selected
	bsr.w observeMatch
	bne.w bad
	moveq #RESULT_SELECTED, d0
	bra.w done
consumed
	bsr.w observeMatch
	bne.w bad
	moveq #1, d0
	bra.w done
bad
	moveq #RESULT_INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; step
	.priv
; Record final match-arm activity in the existing convergence trace.
; A5=record,A6=state,D7=role. D0/CCR=status; other registers preserved.
observeMatch	.block
	movem.l d1/a0, -(sp)
	cmpi.w #parser.MATCH, d7
	blo.w done
	cmpi.w #parser.DEFAULT, d7
	bhi.w done
	tst.l State.Trace+trace.State.Enabled(a6)
	beq.w done
	moveq #0, d1
	move.w State.Active(a6), d1
	move.l a5, d0
	sub.l State.Records(a6), d0
	lea State.Trace(a6), a0
	jsr trace.observe
	bra.w restore
done
	moveq #0, d0
restore
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; observeMatch

; A0..A1=compiled scalar wrapper,A2=context,A6=state.
; D0/CCR=status,D1=low,D2=unavailable,A0=after wrapper; high in context.
; D3-D7/A1-A6 preserved. Certify exactly the consumed wrapper for owner replay.
evaluateScalar	.block
	movem.l d3/a3-a4, -(sp)
	.MEMORY_COUNTER_INC Evaluations
	movea.l a0, a4
	jsr expression.evaluate
	bne.w failed
	move.l State.Proof(a6), d0
	beq.w certified
	movea.l d0, a3
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	movea.l a4, a0
	jsr (a3)
	movem.l (sp)+, a0-a1
	tst.l d0
	bne.w failed
certified
	tst.l d2
	beq.w available
	tst.w State.Strict(a6)
	bne.w failed
	movem.l a0/d2, -(sp)
	movea.l State.Loops(a6), a0
	jsr loops.provisionalControls
	movem.l (sp)+, a0/d2
	tst.l d0
	bne.w failed
	move.w #1, State.Deferred(a6)
	move.l #1, State.Trace+trace.State.Enabled(a6)
	.MEMORY_COUNTER_INC DeferredEvaluations
available
	; The case truth observations determine selection. A changing unresolved
	; scalar also requests another sizing pass via Deferred above.
	moveq #0, d0
	bra.w done
failed
	moveq #1, d0
done
	movem.l (sp)+, d3/a3-a4
	tst.l d0
	rts
	.bend  ; evaluateScalar

; A5=record,A4=record end,A2=context. D0/CCR=status,D1=truth; D2 scratch.
evaluate	.block
	movem.l a0-a1, -(sp)
	.MEMORY_COUNTER_INC Evaluations
	; A proof callback receives the bounded operand before EXVM reads values.
	; D0/CCR=zero certified; all other registers preserved. Runtime leaves it zero.
	move.l State.Proof(a6), d0
	beq.w evaluateOperand
	move.l a3, -(sp)
	movea.l d0, a3
	moveq #0, d0
	move.w State.OperandOffset(a6), d0
	lea 0(a5, d0.w), a0
	movea.l a4, a1
	jsr (a3)
	movea.l (sp)+, a3
	tst.l d0
	bne.w failed
evaluateOperand
	moveq #0, d0
	move.w State.OperandOffset(a6), d0
	lea 0(a5, d0.w), a0
	movea.l a4, a1
	jsr expression.evaluate
	bne.w failed
	cmpa.l a1, a0
	bne.w failed
	tst.l d2
	beq.w available
	tst.w State.Strict(a6)
	bne.w failed
	movea.l State.Loops(a6), a0
	jsr loops.provisionalControls
	bne.w failed
	move.w #1, State.Deferred(a6)
	move.l #1, State.Trace+trace.State.Enabled(a6)
	.MEMORY_COUNTER_INC DeferredEvaluations
available
	or.l package.Context.High(a2), d1
	tst.l State.Trace+trace.State.Enabled(a6)
	beq.w evaluated
	move.l a5, d0
	sub.l State.Records(a6), d0
	lea State.Trace(a6), a0
	jsr trace.observe
	bne.w failed
evaluated
	moveq #0, d0
	bra.w done
failed
	moveq #1, d0
done
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; evaluate
	.endsection
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.pub
	.section bss, kind=bss
Evaluations	.res long, 1
DeferredEvaluations	.res long, 1
	.endsection
.endif
.endif
	.endmodule
