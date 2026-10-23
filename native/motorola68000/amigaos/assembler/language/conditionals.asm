; Conditional preparation selection over numeric writer records. Unavailable
; guards retain remaining branch envelopes for shared numeric runtime replay;
; selected preparation controls become empty records before expression lowering.
; @opforge-owner: opforge.assembler.language.conditionals
	.module opforge.assembler.language.conditionals
	.cpu 68020
	.use opforge.assembler.symbols.lexical_store as lexical
	.use opforge.assembler.symbols.name_store as nameStore
	.use opforge.assembler.symbols.scope_layout as layout
	.use opforge.assembler.symbols.binding_records as records
	.use opforge.assembler.symbols.imports as imports
	.use opforge.packages.format as package
	.use opforge.vm.prvm.packed_branch as branches
	.use opforge.vm.prvm.packed_loop as loop_parser
	.use opforge.vm.prvm.abi as loop_abi
	.use opforge.vm.prvm.abi as abi
	.use opforge.assembler.prepare.expression as expression
	.pub
; No preprocessor define ingress exists in the compact frontend: the namespace
; is empty. Assembler declarations and templates never populate it.
LIMIT = 16
EXPRESSION = 0
DEFINED = 1
NOT_DEFINED = 2
MATCH_KIND = 3
Slot	.struct
Parent	.byte ?
Taken	.byte ?
ElseSeen	.byte ?
Kind	.byte ?
ValueLow	.long ?
Reserved	.long ?
Unavailable	.long ?
	.endstruct
State	.struct
Depth	.word ?
Active	.word ?
Deferred	.word ?
Static	.word ?  ; configuration discovery can retain unavailable controls
Uncertain	.word ?  ; earliest unavailable conditional depth+1, zero when known
Materializing	.word ?  ; selected source iterations evaluate current known guards
OperandOffset	.word ?  ; shared PRVM offset from the original record
LabelBytes	.word ?  ; validated numeric label prefix, including optional colon
LabelParent	.word ?  ; enclosing branch activity before this control
Acquisition	.word ?  ; capture policy retains every expression guard
PublishBytes	.word ?  ; consumed control prefix for ordinary scope preparation
	.endstruct
SLOTS = State.PublishBytes+2
SLOT_BYTES = Slot.Unavailable+4
SLOT_SHIFT = 4
DEFERRED_SLOTS = SLOTS+LIMIT*SLOT_BYTES
DeferredSlot	.struct
LoopDepth	.word ?
PreparationDepth	.word ?
ElseSeen	.byte ?
Kind	.byte ?
Padding	.res 2
	.endstruct
DEFERRED_SLOT_BYTES = DeferredSlot.Padding+2
DEFERRED_SLOT_SHIFT = 3
SCRATCH_BYTES = DEFERRED_SLOTS+LIMIT*DEFERRED_SLOT_BYTES
	.section code, kind=code

; A0=state. Start a strict semantic source with no open conditionals.
begin	.block
	clr.w State.Depth(a0)
	clr.w State.Deferred(a0)
	move.w #1, State.Active(a0)
	clr.w State.Static(a0)
	clr.w State.Uncertain(a0)
	clr.w State.Materializing(a0)
	clr.w State.Acquisition(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=state. Start configuration discovery with no open conditionals. Unavailable
; scalar expressions select false, matching Rust's static source-graph scan;
; malformed conditional structure still fails. D0/CCR=status, others preserved.
beginStatic	.block
	bsr.w begin
	move.w #1, State.Static(a0)
	moveq #0, d0
	rts
	.bend  ; beginStatic

	; A0=state. Static source capture executes only preprocessor guards. Scalar
; expression controls remain envelopes, including known .if selectors.
beginAcquisition	.block
	bsr.w begin
	move.w #1, State.Acquisition(a0)
	moveq #0, d0
	rts
	.bend  ; beginAcquisition

; A0=state. D0/CCR=status; reject a conditional crossing a file boundary.
endFile	.block
	moveq #0, d0
	tst.w State.Deferred(a0)
	bne.w unbalanced
	tst.w State.Depth(a0)
	beq.w done
unbalanced
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
	move.w #branches.Record.Operand, State.OperandOffset(a4)
	clr.w State.LabelBytes(a4)
	clr.w State.LabelParent(a4)
	clr.w State.PublishBytes(a4)
	; An active loop retains expression conditionals for replay. Outer static
	; inactive regions still use normal preparation filtering.
	tst.w State.Active(a4)
	beq.w preparation
	tst.w State.Materializing(a4)
	bne.w sourceSelection
	tst.w layout.State.LoopDepth(a6)
	bne.w deferred
sourceSelection
	tst.w State.Deferred(a4)
	beq.w preparation
deferred
	; Preprocessor controls own branch operands while their preparation frame
	; is innermost; expression conditionals nested there are prepared normally.
	move.w State.Depth(a4), d2
	beq.w deferredPolicy
	subq.w #1, d2
	lsl.w #SLOT_SHIFT, d2
	cmpi.b #MATCH_KIND, SLOTS+Slot.Kind(a4, d2.w)
	beq.w deferredPolicy
	tst.b SLOTS+Slot.Kind(a4, d2.w)
	beq.w deferredPolicy
	move.w State.Deferred(a4), d2
	beq.w preprocessorHead
	subq.w #1, d2
	lsl.w #DEFERRED_SLOT_SHIFT, d2
	lea DEFERRED_SLOTS(a4), a2
	adda.w d2, a2
	move.w State.Depth(a4), d0
	cmp.w DeferredSlot.PreparationDepth(a2), d0
	beq.w deferredPolicy
preprocessorHead
	movea.l layout.State.Package(a6), a1
	move.l a1, d0
	beq.w preparation
	adda.l package.Header.BranchPlan(a1), a1
	moveq #0, d0
	move.b (a5), d0
	addq.w #1, d0
	movea.l a5, a0
	jsr branches.selectDetailed
	bne.w bad
	cmpi.w #branches.IF, d1
	bne.w preparation
deferredPolicy
	movea.l layout.State.Package(a6), a1
	move.l a1, d0
	beq.w legacyPreparation
	adda.l package.Header.BranchPlan(a1), a1
	moveq #0, d0
	move.b (a5), d0
	addq.w #1, d0
	movea.l a5, a0
	jsr branches.classifyDetailed
	bne.w bad
	; Static preprocessor controls retain the preparation stack. A closing
	; control belongs to it when that stack is deeper than the deferred opener.
	tst.w d1
	beq.w deferredOrdinary
	cmpi.w #branches.MATCH, d1
	beq.w deferIf
	cmpi.w #branches.IF, d1
	beq.w deferIf
	move.w State.Deferred(a4), d2
	beq.w preparation
	subq.w #1, d2
	lsl.w #DEFERRED_SLOT_SHIFT, d2
	lea DEFERRED_SLOTS(a4), a2
	adda.w d2, a2
	move.w State.Depth(a4), d0
	cmp.w DeferredSlot.PreparationDepth(a2), d0
	bne.w preparation
	move.b DeferredSlot.Kind(a2), d0
	cmpi.w #branches.ENDMATCH, d1
	beq.w deferredMatchEnd
	cmpi.w #branches.ENDIF, d1
	beq.w deferredIfEnd
	cmpi.b #branches.MATCH, d0
	beq.w deferredMatchArm
	cmpi.w #branches.ELSEIF, d1
	beq.w deferredArm
	cmpi.w #branches.ELSE, d1
	bne.w bad
	bra.w deferredArm
deferredMatchArm
	cmpi.w #branches.CASE, d1
	beq.w deferredArm
	cmpi.w #branches.DEFAULT, d1
	bne.w bad
deferredArm
	tst.b DeferredSlot.ElseSeen(a2)
	bne.w bad
	cmpi.w #branches.DEFAULT, d1
	beq.w deferredTerminal
	cmpi.w #branches.ELSE, d1
	bne.w deferredReady
deferredTerminal
	move.b #1, DeferredSlot.ElseSeen(a2)
	bra.w deferredReady
deferredMatchEnd
	cmpi.b #branches.MATCH, d0
	bne.w bad
	bra.w deferEndif
deferredIfEnd
	cmpi.b #branches.IF, d0
	bne.w bad
	bra.w deferEndif
deferIf
	moveq #0, d2
	move.w State.Deferred(a4), d2
	cmpi.w #LIMIT, d2
	bhs.w bad
	lsl.w #DEFERRED_SLOT_SHIFT, d2
	lea DEFERRED_SLOTS(a4), a2
	adda.w d2, a2
	clr.b DeferredSlot.ElseSeen(a2)
	move.b d1, DeferredSlot.Kind(a2)
	move.w layout.State.LoopDepth(a6), DeferredSlot.LoopDepth(a2)
	move.w State.Depth(a4), DeferredSlot.PreparationDepth(a2)
	addq.w #1, State.Deferred(a4)
	bra.w deferredReady
deferEndif
	tst.w State.Deferred(a4)
	beq.w bad
	move.w layout.State.LoopDepth(a6), d0
	cmp.w DeferredSlot.LoopDepth(a2), d0
	bne.w bad
	subq.w #1, State.Deferred(a4)
	bra.w deferredReady
deferredOrdinary
	; A conditional may surround nested loops, but may not cross its own
	; containing loop's boundary.
	tst.w State.Deferred(a4)
	beq.w preparation
	movea.l layout.State.Package(a6), a1
	cmpi.b #branches.Record.Operand-1, (a5)
	blo.w deferredReady
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, branches.Record.Dot(a5)
	bne.w deferredReady
	move.w branches.Record.Head(a5), d0
	cmp.w package.Header.EndforDirective(a1), d0
	bne.w preparation
	move.w State.Deferred(a4), d2
	subq.w #1, d2
	lsl.w #DEFERRED_SLOT_SHIFT, d2
	lea DEFERRED_SLOTS(a4), a2
	adda.w d2, a2
	move.w layout.State.LoopDepth(a6), d0
	cmp.w DeferredSlot.LoopDepth(a2), d0
	beq.w bad
deferredReady
	moveq #1, d1
	moveq #0, d0
	bra.w done
preparation
	; Shared expression heads are package numeric identities in all regions.
	; Preprocessor name controls retain their existing preparation-only path.
	movea.l layout.State.Package(a6), a1
	move.l a1, d0
	beq.w legacyPreparation
	adda.l package.Header.BranchPlan(a1), a1
	moveq #0, d0
	move.b (a5), d0
	addq.w #1, d0
	movea.l a5, a0
	move.w State.Depth(a4), d2
	beq.w preparationExpression
	subq.w #1, d2
	lsl.w #SLOT_SHIFT, d2
	cmpi.b #MATCH_KIND, SLOTS+Slot.Kind(a4, d2.w)
	beq.w preparationExpression
	tst.b SLOTS+Slot.Kind(a4, d2.w)
	beq.w preparationExpression
	jsr branches.selectDetailed
	bra.w preparationSelected
preparationExpression
	jsr branches.classifyDetailed
preparationSelected
	bne.w bad
	move.w d2, State.OperandOffset(a4)
	move.w d3, State.LabelBytes(a4)
	tst.l d1
	bne.w preparationRole
	move.w #branches.Record.Operand, State.OperandOffset(a4)
preparationRole
	moveq #EXPRESSION, d7
	cmpi.w #branches.MATCH, d1
	beq.w matchLine
	cmpi.w #branches.CASE, d1
	beq.w caseLine
	cmpi.w #branches.DEFAULT, d1
	beq.w defaultLine
	cmpi.w #branches.ENDMATCH, d1
	beq.w endmatchLine
	cmpi.w #branches.IF, d1
	beq.w ifLine
	cmpi.w #branches.ELSEIF, d1
	beq.w elseifLine
	cmpi.w #branches.ELSE, d1
	beq.w elseLine
	cmpi.w #branches.ENDIF, d1
	beq.w endifLine
legacyPreparation
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
	movem.l a0-a1, -(sp)
	lea layout.LEXICAL_STATE(a6), a0
	lea LexicalEntry, a1
	jsr lexical.readEntry
	movem.l (sp)+, a0-a1
	bne.w bad
	lea LexicalEntry, a0
	moveq #0, d2
	move.w records.Entry.Length(a0), d2
	sub.w records.Entry.Leaf(a0), d2
	moveq #0, d0
	move.l records.Entry.Name(a0), d0
	movem.l d1-d2, -(sp)
	moveq #0, d1
	move.w records.Entry.Length(a0), d1
	lea layout.LEXICAL_STATE(a6), a0
	lea LexicalName, a1
	jsr lexical.copyName
	movem.l (sp)+, d1-d2
	bne.w bad
	moveq #0, d0
	move.w LexicalEntry+records.Entry.Leaf, d0
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
matchLine
	; Keep the complete match envelope for runtime and ownership replay.
	; Unlike .elseif promotion, a later unavailable case needs its scalar opener.
	tst.w State.Active(a4)
	beq.w preparedMatch
	tst.w State.Static(a4)
	beq.w deferIf
preparedMatch
	moveq #MATCH_KIND, d7
	bra.w ifLine
caseLine
	moveq #1, d7
	bra.w matchArm
defaultLine
	moveq #0, d7
matchArm
	moveq #0, d2
	move.w State.Depth(a4), d2
	beq.w bad
	subq.w #1, d2
	lsl.w #SLOT_SHIFT, d2
	lea SLOTS(a4), a2
	adda.w d2, a2
	cmpi.b #MATCH_KIND, Slot.Kind(a2)
	bne.w bad
	tst.b Slot.ElseSeen(a2)
	bne.w bad
	moveq #0, d0
	move.b Slot.Parent(a2), d0
	move.w d0, State.LabelParent(a4)
	clr.w State.Active(a4)
	tst.w d7
	beq.w takeElse
	tst.b Slot.Parent(a2)
	beq.w consumed
	tst.b Slot.Taken(a2)
	bne.w consumed
	tst.l Slot.Unavailable(a2)
	bne.w consumed
	moveq #0, d0
	move.w State.OperandOffset(a4), d0
	lea 0(a5, d0.w), a0
	moveq #0, d0
	move.b (a5), d0
	lea 1(a5, d0.w), a1
staticCase
	bsr.w scopedScalar
	bne.w staticCaseUnavailable  ; unavailable static guards select false
	cmp.l Slot.ValueLow(a2), d1
	bne.w staticNextCase
	move.b #1, Slot.Taken(a2)
	move.w #1, State.Active(a4)
	bra.w consumed
staticNextCase
	cmpa.l a1, a0
	beq.w consumed
	cmpi.b #abi.PRVM_TOKEN_KIND_COMMA, (a0)+
	bne.w bad
	bra.w staticCase
staticCaseUnavailable
	tst.w State.Uncertain(a4)
	bne.w consumed
	move.w State.Depth(a4), State.Uncertain(a4)
	bra.w consumed
endmatchLine
	move.w State.Depth(a4), d2
	beq.w bad
	subq.w #1, d2
	lsl.w #SLOT_SHIFT, d2
	cmpi.b #MATCH_KIND, SLOTS+Slot.Kind(a4, d2.w)
	bne.w bad
	bra.w closeFrame
ifLine
	tst.w State.Acquisition(a4)
	beq.w ordinaryIf
	tst.w d7
	bne.w ordinaryIf
	tst.w State.Active(a4)
	beq.w ordinaryIf
	moveq #branches.IF, d1
	bra.w deferIf
ordinaryIf
	move.w State.Active(a4), State.LabelParent(a4)
	cmpi.w #MATCH_KIND, d7
	beq.w ifExpression
	tst.w d7
	beq.w ifExpression
	bsr.w definedOperand
	bne.w bad
ifExpression
	moveq #0, d2
	move.w State.Depth(a4), d2
	cmpi.w #LIMIT, d2
	bhs.w bad
	lsl.w #SLOT_SHIFT, d2
	lea SLOTS(a4), a2
	adda.w d2, a2
	move.w State.Active(a4), d0
	move.b d0, Slot.Parent(a2)
	clr.w Slot.Taken(a2)
	move.b d7, Slot.Kind(a2)
	clr.l Slot.Unavailable(a2)
	tst.w State.Active(a4)
	beq.w ifStored
	cmpi.w #MATCH_KIND, d7
	beq.w staticMatch
	tst.w d7
	beq.w evaluateIf
	moveq #0, d1
	cmpi.w #NOT_DEFINED, d7
	bne.w ifValue
	moveq #1, d1
	bra.w ifValue
staticMatch
	moveq #0, d0
	move.w State.OperandOffset(a4), d0
	lea 0(a5, d0.w), a0
	moveq #0, d0
	move.b (a5), d0
	lea 1(a5, d0.w), a1
	bsr.w scopedScalar
	bne.w staticUnavailable
	cmpa.l a1, a0
	bne.w bad
	move.l d1, Slot.ValueLow(a2)
	bra.w ifStored
staticUnavailable
	move.l #1, Slot.Unavailable(a2)
	tst.w State.Uncertain(a4)
	bne.w ifStored
	move.w State.Depth(a4), d2
	addq.w #1, d2
	move.w d2, State.Uncertain(a4)
	bra.w ifStored
evaluateIf
	bsr.w evaluateCondition
	beq.w ifValue
	; An unavailable opening guard cannot select preparation effects. Retain
	; its numeric envelope for the shared runtime expression/branch replay.
	; Static discovery keeps its separate uncertainty policy above.
	tst.w State.Static(a4)
	bne.w bad
	movea.l layout.State.Package(a6), a1
	move.l a1, d0
	beq.w bad
	moveq #branches.IF, d1
	bra.w deferIf
ifValue
	tst.l d1
	beq.w ifStored
	moveq #0, d2
	move.w State.Depth(a4), d2
	lsl.w #SLOT_SHIFT, d2
	lea SLOTS(a4), a2
	move.b #1, Slot.Taken(a2, d2.w)
ifStored
	moveq #0, d2
	move.w State.Depth(a4), d2
	lsl.w #SLOT_SHIFT, d2
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
	lsl.w #SLOT_SHIFT, d2
	lea SLOTS(a4), a2
	adda.w d2, a2
	tst.b Slot.ElseSeen(a2)
	bne.w bad
	moveq #0, d0
	move.b Slot.Parent(a2), d0
	move.w d0, State.LabelParent(a4)
	cmpi.b #MATCH_KIND, Slot.Kind(a2)
	beq.w bad
	move.b (a5), d0
	addq.w #1, d0
	cmp.w State.OperandOffset(a4), d0
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
	bsr.w evaluateCondition
	bne.w deferElseif
	tst.l d1
	beq.w consumed
	move.b #1, Slot.Taken(a2)
	move.w #1, State.Active(a4)
	bra.w consumed
deferElseif
	; Earlier false arms have been consumed. The unavailable arm now opens
	; the remaining runtime envelope using the package-owned IF identity.
	movea.l layout.State.Package(a6), a1
	move.l a1, d0
	beq.w bad
	adda.l package.Header.BranchPlan(a1), a1
	lea branches.Program.Rows(a1), a1
	moveq #branches.HEAD_COUNT-1, d3
ifHead
	cmpi.b #branches.IF, branches.Row.Role(a1)
	beq.w promoteElseif
	adda.w #branches.ROW_BYTES, a1
	dbra d3, ifHead
	bra.w bad
promoteElseif
	moveq #0, d0
	move.w State.OperandOffset(a4), d0
	subq.w #branches.Record.Operand-branches.Record.Head, d0
	move.w branches.Row.Head(a1), 0(a5, d0.w)
	subq.w #1, State.Depth(a4)
	moveq #0, d0
	move.b Slot.Parent(a2), d0
	move.w d0, State.Active(a4)
	moveq #branches.IF, d1
	bra.w deferIf

endifLine
	move.w State.Depth(a4), d2
	beq.w bad
	subq.w #1, d2
	lsl.w #SLOT_SHIFT, d2
	cmpi.b #MATCH_KIND, SLOTS+Slot.Kind(a4, d2.w)
	beq.w bad
closeFrame
	moveq #0, d0
	move.b (a5), d0
	addq.w #1, d0
	cmp.w State.OperandOffset(a4), d0
	bne.w bad  ; no operands
	moveq #0, d2
	move.w State.Depth(a4), d2
	beq.w bad
	subq.w #1, State.Depth(a4)
	move.w State.Depth(a4), d0
	cmp.w State.Uncertain(a4), d0
	bhs.w uncertaintyRetained
	clr.w State.Uncertain(a4)
uncertaintyRetained
	subq.w #1, d2
	lsl.w #SLOT_SHIFT, d2
	lea SLOTS(a4), a2
	moveq #0, d0
	move.b Slot.Parent(a2, d2.w), d0
	move.w d0, State.Active(a4)
	move.w d0, State.LabelParent(a4)
	bra.w consumed
ordinary
	move.w State.Active(a4), d1
	moveq #0, d0
	bra.w done
consumed
	tst.w State.LabelParent(a4)
	beq.w consumedReady
	move.w State.LabelBytes(a4), State.PublishBytes(a4)
consumedReady
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
; A0..A1=raw scalar sequence,A6=scope. Shared EXVM determines one expression
; extent; scoped evaluation owns bindings and i64 scalar semantics.
; D0/CCR=status,D1/D2=low/high,A0=after expression; other registers preserved.
scopedScalar	.block
	movem.l d3-d7/a2-a6, -(sp)
	lea -256(sp), sp
	movea.l a0, a5
	movea.l sp, a3
	lea 256(sp), a4
	jsr expression.compileUnfolded
	bne.w done
	movea.l a0, a4
	movem.l a0-a1, -(sp)
	movea.l a5, a0
	movea.l a4, a1
	movea.l a6, a2
	jsr imports.evaluateScoped
	movem.l (sp)+, a0-a1
done
	lea 256(sp), sp
	movem.l (sp)+, d3-d7/a2-a6
	tst.l d0
	rts
	.bend  ; scopedScalar

; A5=conditional writer record,A6=scope,A4=conditional state. Use the shared
; expression evaluator in both modes; only static discovery converts failure
; to a false condition. D0/CCR=status,D1=truth value,D2 scratch; others kept.
evaluateCondition	.block
	movem.l a0-a2, -(sp)
	movea.l a5, a0
	moveq #0, d0
	move.b (a0), d0
	lea 1(a0, d0.w), a1
	moveq #0, d0
	move.w State.OperandOffset(a4), d0
	adda.w d0, a0
	movea.l a6, a2
	jsr imports.evaluateScoped
	beq.w known
	tst.w State.Static(a4)
	beq.w failed
	tst.w State.Uncertain(a4)
	bne.w unavailable
	move.w State.Depth(a4), d2
	tst.w d7
	bne.w uncertaintyDepthReady
	addq.w #1, d2
uncertaintyDepthReady
	move.w d2, State.Uncertain(a4)
unavailable
	moveq #0, d1
	moveq #0, d0
	bra.w done
known
	or.l d2, d1
	moveq #0, d0
	bra.w done
failed
	moveq #1, d0
done
	movem.l (sp)+, a0-a2
	tst.l d0
	rts
	.bend  ; evaluateCondition

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
	.section bss, kind=bss
; Accessor scratch is consumed before any callback; no Entry view escapes.
LexicalEntry	.res byte, records.ENTRY_BYTES
LexicalName	.res byte, records.NAME_BYTES
	.endsection
	.endmodule
