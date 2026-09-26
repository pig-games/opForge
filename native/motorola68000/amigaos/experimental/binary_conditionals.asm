; Preparation-only conditional selection over numeric writer records.
; Filtered lines become empty records before binding and expression lowering.
; @opforge-owner: experimental.amigaos.binary_conditionals
	.module experimental.amigaos.binary_conditionals
	.cpu 68020
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_imports as imports
	.pub
; No preprocessor define ingress exists in the compact frontend: the namespace
; is empty. Assembler declarations and templates never populate it.
LIMIT = 16
EXPRESSION = 0
DEFINED = 1
NOT_DEFINED = 2
Slot	.struct
Parent	.byte ?
Taken	.byte ?
ElseSeen	.byte ?
Kind	.byte ?
	.endstruct
State	.struct
Depth	.word ?
Active	.word ?
	.endstruct
SLOTS = State.Active+2
SCRATCH_BYTES = SLOTS+LIMIT*4
	.section code, kind=code

; A0=state. Start a physical source with no open conditionals.
begin	.block
	clr.w State.Depth(a0)
	move.w #1, State.Active(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=state. D0/CCR=status; reject a conditional crossing a file boundary.
endFile	.block
	moveq #0, d0
	tst.w State.Depth(a0)
	beq.w done
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; endFile

; A0=writer record,A1=scope state,A2=conditional state.
; D0/CCR=status, D1=one when the normal scope/preparation path should run.
; Control lines and inactive body lines are consumed here; no text is retained.
line	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	movea.l a2, a4
	moveq #0, d1
	move.b (a5), d1
	addq.w #1, d1
	cmpi.w #9, d1
	blo.w ordinary
	moveq #EXPRESSION, d7
	lea 4(a5), a3
	cmpi.b #7, (a3)
	bne.w ordinary
	cmpi.b #1, 1(a3)
	bhi.w ordinary
	tst.b 4(a3)
	bne.w ordinary
	moveq #0, d0
	move.w 2(a3), d0
	sub.w layout.State.Base(a6), d0
	bcs.w ordinary
	cmp.w layout.State.Count(a6), d0
	bhs.w ordinary
	lsl.l #4, d0
	lea layout.ENTRIES(a6), a0
	adda.l d0, a0
	moveq #0, d2
	move.w records.Entry.Length(a0), d2
	sub.w records.Entry.Leaf(a0), d2
	movea.l layout.ARENA_POINTER(a6), a1
	moveq #0, d0
	move.w records.Entry.Name(a0), d0
	add.w records.Entry.Leaf(a0), d0
	adda.l d0, a1
	cmpi.w #2, d2
	bne.w checkElse
	move.w (a1), d0
	ori.w #$2020, d0
	cmpi.w #$6966, d0  ; if
	beq.w ifLine
checkElse
	cmpi.w #4, d2
	bne.w checkIfdef
	move.l (a1), d0
	ori.l #$20202020, d0
	cmpi.l #$656c7365, d0  ; else
	beq.w elseLine
checkIfdef
	cmpi.w #5, d2
	bne.w checkSix
	move.l (a1), d0
	ori.l #$20202020, d0
	cmpi.l #$69666465, d0  ; ifde
	bne.w checkEndif
	move.b 4(a1), d0
	ori.b #$20, d0
	cmpi.b #'f', d0
	bne.w checkEndif
	moveq #DEFINED, d7
	bra.w ifLine
checkSix
	cmpi.w #6, d2
	bne.w ordinary
	move.l (a1), d0
	ori.l #$20202020, d0
	cmpi.l #$69666e64, d0  ; ifnd
	bne.w checkElseif
	move.w 4(a1), d0
	ori.w #$2020, d0
	cmpi.w #$6566, d0  ; ef
	bne.w ordinary
	moveq #NOT_DEFINED, d7
	bra.w ifLine
checkElseif
	cmpi.l #$656c7365, d0  ; else
	bne.w ordinary
	move.w 4(a1), d0
	ori.w #$2020, d0
	cmpi.w #$6966, d0  ; if
	bne.w ordinary
	bra.w elseifLine
checkEndif
	cmpi.w #5, d2
	bne.w ordinary
	move.l (a1), d0
	ori.l #$20202020, d0
	cmpi.l #$656e6469, d0  ; endi
	bne.w ordinary
	moveq #0, d0
	move.b 4(a1), d0
	ori.b #$20, d0
	cmpi.b #'f', d0
	bne.w ordinary
	bra.w endifLine
ifLine
	tst.w d7
	beq.w ifExpression
	bsr.w definedOperand
	bne.w bad
ifExpression
	moveq #0, d2
	move.w State.Depth(a4), d2
	cmpi.w #LIMIT, d2
	bhs.w bad
	lsl.w #2, d2
	lea SLOTS(a4), a2
	adda.w d2, a2
	move.w State.Active(a4), d0
	move.b d0, Slot.Parent(a2)
	clr.b Slot.Taken(a2)
	clr.b Slot.ElseSeen(a2)
	move.b d7, Slot.Kind(a2)
	tst.w State.Active(a4)
	beq.w ifStored
	tst.w d7
	beq.w evaluateIf
	moveq #0, d1
	cmpi.w #NOT_DEFINED, d7
	bne.w ifValue
	moveq #1, d1
	bra.w ifValue
evaluateIf
	movea.l a5, a0
	moveq #0, d0
	move.b (a0), d0
	lea 1(a0, d0.w), a1
	lea 9(a0), a0
	movea.l a6, a2
	jsr imports.evaluateScoped
	bne.w bad
ifValue
	tst.l d1
	beq.w ifStored
	moveq #0, d2
	move.w State.Depth(a4), d2
	lsl.w #2, d2
	lea SLOTS(a4), a2
	move.b #1, Slot.Taken(a2, d2.w)
ifStored
	moveq #0, d2
	move.w State.Depth(a4), d2
	lsl.w #2, d2
	lea SLOTS(a4), a2
	adda.w d2, a2
	addq.w #1, State.Depth(a4)
	moveq #0, d0
	move.b Slot.Parent(a2), d0
	and.b Slot.Taken(a2), d0
	move.w d0, State.Active(a4)
	bra.w consumed
elseLine
	moveq #0, d7
	bra.w branchLine
elseifLine
	moveq #1, d7
branchLine
	moveq #0, d2
	move.w State.Depth(a4), d2
	beq.w bad
	subq.w #1, d2
	lsl.w #2, d2
	lea SLOTS(a4), a2
	adda.w d2, a2
	tst.b Slot.ElseSeen(a2)
	bne.w bad
	moveq #0, d0
	move.b (a5), d0
	cmpi.w #8, d0
	beq.w terminalBranch
	tst.b Slot.Kind(a2)
	beq.w expressionBranch
	bsr.w definedOperand
	bne.w bad
	clr.w State.Active(a4)  ; absent preprocessor name cannot select a branch
	bra.w consumed
terminalBranch
	tst.w d7
	beq.w takeElse
	tst.b Slot.Kind(a2)
	beq.w bad  ; expression .elseif requires an expression
	; Preprocessor .elseif with no name is the terminal .else form.
takeElse
	move.b #1, Slot.ElseSeen(a2)
	moveq #0, d0
	move.b Slot.Taken(a2), d0
	eori.b #1, d0
	and.b Slot.Parent(a2), d0
	move.w d0, State.Active(a4)
	move.b #1, Slot.Taken(a2)
	bra.w consumed
expressionBranch
	tst.w d7
	beq.w bad  ; expression .else has no operands
	clr.w State.Active(a4)
	tst.b Slot.Parent(a2)
	beq.w consumed
	tst.b Slot.Taken(a2)
	bne.w consumed
	move.l a2, -(sp)
	movea.l a5, a0
	moveq #0, d0
	move.b (a0), d0
	lea 1(a0, d0.w), a1
	lea 9(a0), a0
	movea.l a6, a2
	jsr imports.evaluateScoped
	movea.l (sp)+, a2
	bne.w bad
	tst.l d1
	beq.w consumed
	move.b #1, Slot.Taken(a2)
	move.w #1, State.Active(a4)
	bra.w consumed

endifLine
	moveq #0, d0
	move.b (a5), d0
	cmpi.w #8, d0
	bne.w bad  ; no operands
	moveq #0, d2
	move.w State.Depth(a4), d2
	beq.w bad
	subq.w #1, State.Depth(a4)
	subq.w #1, d2
	lsl.w #2, d2
	lea SLOTS(a4), a2
	moveq #0, d0
	move.b Slot.Parent(a2, d2.w), d0
	move.w d0, State.Active(a4)
	bra.w consumed
ordinary
	move.w State.Active(a4), d1
	moveq #0, d0
	bra.w done
consumed
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
	.bend  ; line
	.priv
; A5=control record. Validate one packed preprocessor name without consulting
; assembler definitions. D0/CCR=status; other registers kept.
definedOperand	.block
	cmpi.b #12, (a5)  ; directive plus exactly one four-byte name token
	bne.w invalid
	cmpi.b #1, 9(a5)
	bhi.w invalid
	cmpi.b #1, 12(a5)
	bhi.w invalid
	moveq #0, d0
	rts
invalid
	moveq #1, d0
	rts
	.bend  ; definedOperand
	.endsection
	.endmodule
