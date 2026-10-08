; Runtime conditional selection before loop/state/emission effects.
; @opforge-owner: experimental.amigaos.binary_branches
	.module experimental.amigaos.binary_branches
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use prvm.amigaos.packed_branch as parser
	.use opasm.amigaos.binary_expression as expression
	.include "memory_telemetry.i"
	.pub
LIMIT = 16
Slot	.struct
Parent	.byte ?
Taken	.byte ?
ElseSeen	.byte ?
Padding	.byte ?
	.endstruct
SLOT_BYTES = Slot.Padding+1
SLOT_SHIFT = 2
State	.struct
Depth	.word ?
Active	.word ?
Slots	.res LIMIT*SLOT_BYTES
	.endstruct
STATE_BYTES = State.Slots+LIMIT*SLOT_BYTES
	.section code, kind=code
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
; D0=0 active ordinary,1 consumed/inactive,2 invalid. Other registers kept.
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
	jsr parser.classify
	bne.w bad
	tst.l d1
	beq.w ordinary
	move.l d1, d7
	moveq #0, d6
	move.w State.Depth(a6), d6
	cmpi.w #parser.IF, d7
	beq.w opening
	tst.w d6
	beq.w bad
	subq.w #1, d6
	lsl.w #experimental.amigaos.binary_branches.SLOT_SHIFT, d6
	lea State.Slots(a6), a3
	adda.w d6, a3
	cmpi.w #parser.ENDIF, d7
	beq.w closing
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
	bra.w consumed
terminal
	move.b #1, Slot.ElseSeen(a3)
	moveq #0, d0
	move.b Slot.Taken(a3), d0
	eori.b #1, d0
	and.b Slot.Parent(a3), d0
	move.w d0, State.Active(a6)
	move.b #1, Slot.Taken(a3)
	bra.w consumed
opening
	cmpi.w #LIMIT, d6
	bhs.w bad
	lsl.w #experimental.amigaos.binary_branches.SLOT_SHIFT, d6
	lea State.Slots(a6), a3
	adda.w d6, a3
	move.w State.Active(a6), d0
	move.b d0, Slot.Parent(a3)
	clr.b Slot.Taken(a3)
	clr.b Slot.ElseSeen(a3)
	addq.w #1, State.Depth(a6)
	tst.w d0
	beq.w consumed
	bsr.w evaluate
	bne.w bad
	clr.w State.Active(a6)
	tst.l d1
	beq.w consumed
	move.b #1, Slot.Taken(a3)
	move.w #1, State.Active(a6)
	bra.w consumed
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
consumed
	moveq #1, d0
	bra.w done
bad
	moveq #2, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; step
	.priv
; A5=record,A4=record end,A2=context. D0/CCR=status,D1=truth; D2 scratch.
evaluate	.block
	movem.l a0-a1, -(sp)
	.MEMORY_COUNTER_INC Evaluations
	lea parser.Record.Operand(a5), a0
	movea.l a4, a1
	jsr expression.evaluate
	bne.w failed
	cmpa.l a1, a0
	bne.w failed
	tst.l d2
	bne.w failed
	or.l package.Context.High(a2), d1
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
	.endsection
.endif
.endif
	.endmodule
