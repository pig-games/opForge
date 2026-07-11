; Native opasm assembly-session driver.

	.module opasm.amigaos.assembly_driver
	.cpu 68020

	.use opasm.amigaos.callback_abi as abi
	.use opasm.amigaos.compile_values as compile_values
	.use opasm.amigaos.engine as eng
	.use opasm.amigaos.events
	.use opasm.amigaos.flow_conditionals as conditionals
	.use opasm.amigaos.flow_navigation as navigation
	.use opasm.amigaos.flow_repetition as repetition
	.use opasm.amigaos.flow_scopes as scopes
	.use opasm.amigaos.tkpkg_bridge as tkpkg

OPASM_LAYOUT_NAME_CAPACITY = 32
OPASM_LAYOUT_REGION_CAPACITY = 8
OPASM_LAYOUT_SECTION_CAPACITY = 16
OPASM_LAYOUT_INDEX_NONE = $ffff
OPASM_TEXT_SCRATCH_CAPACITY = 512
OPASM_REPEAT_STACK_CAPACITY = 8
OPASM_REPEAT_ITERATION_LIMIT = 1024
OPASM_REPEAT_KIND_FOR = 1
OPASM_REPEAT_KIND_WHILE = 2

	.section code, kind=code
	.pub

; Run one native opasm assembly session.
;
; Inputs:
; - A0: abi.OPASM_ASSEMBLE_REQ_* frame.
;
; Outputs:
; - D0: current opasm engine status.
; - A0: original request frame pointer.
assembleSessionV1	.block
	movem.l a1-a2/a4, -(sp)
	movea.l a0, a1
	move.l a1, OpasmActiveAssembleReqPtr
	tst.l abi.OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a1)
	beq.s buildContext
	movea.l abi.OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a1), a0
	clr.w (a0)

buildContext
	suba.l #eng.OPASM_ENGINE_CALLBACK_REQ_BYTES, sp
	movea.l sp, a0
	move.l abi.OPASM_ASSEMBLE_REQ_BIN_REQUESTED_PTR(a1), eng.OPASM_ENGINE_CALLBACK_REQ_BIN_REQUESTED_PTR(a0)
	move.l #opasmDriverPassOneBegin, eng.OPASM_ENGINE_CALLBACK_REQ_PASS1_BEGIN_CB(a0)
	move.l #opasmDriverPassTwoBegin, eng.OPASM_ENGINE_CALLBACK_REQ_PASS2_BEGIN_CB(a0)
	move.l #opasmDriverPassOneOk, eng.OPASM_ENGINE_CALLBACK_REQ_PASS1_OK_CB(a0)
	move.l #opasmDriverPassTwoOk, eng.OPASM_ENGINE_CALLBACK_REQ_PASS2_OK_CB(a0)
	move.l #opasmDriverRecordLabel, eng.OPASM_ENGINE_CALLBACK_REQ_RECORD_LABEL_CB(a0)
	move.l #opasmDriverAdvancePc, eng.OPASM_ENGINE_CALLBACK_REQ_ADVANCE_PC_CB(a0)
	move.l #opasmDriverApplyFlowControl, eng.OPASM_ENGINE_CALLBACK_REQ_FLOW_CONTROL_CB(a0)
	move.l #opasmDriverEmitImageBytes, eng.OPASM_ENGINE_CALLBACK_REQ_EMIT_IMAGE_CB(a0)
	jsr eng.opasmEngineBuildCallbackContextV1
	adda.l #eng.OPASM_ENGINE_CALLBACK_REQ_BYTES, sp
	jsr eng.opasmEngineRunTwoPassV1
	movea.l a1, a0
	movem.l (sp)+, a1-a2/a4
	rts
	.bend  ; assembleSessionV1

	.priv

opasmDriverPassOneBegin	.block
	moveq #abi.OPASM_EVENT_PASS_BEGIN, d0
	moveq #1, d1
	bsr.w appendPassEvent
	jsr eng.opasmEngineBeginPassOneV1
	bsr.w resetLayoutState
	clr.w OpasmRepeatDepth
	clr.w OpasmIfDepth
	jsr scopes.resetStateV1
	jsr compile_values.resetBindingsV1
	rts
	.bend  ; opasmDriverPassOneBegin

opasmDriverPassOneOk	.block
	moveq #abi.OPASM_EVENT_PASS_OK, d0
	moveq #1, d1
	bsr.w appendPassEvent
	moveq #0, d0
	rts
	.bend  ; opasmDriverPassOneOk

opasmDriverPassTwoBegin	.block
	moveq #abi.OPASM_EVENT_PASS_BEGIN, d0
	moveq #2, d1
	bsr.w appendPassEvent
	jsr eng.opasmEngineBeginPassTwoV1
	clr.w OpasmLayoutSectionActive
	clr.w OpasmRepeatDepth
	clr.w OpasmIfDepth
	jsr scopes.resetStateV1
	jsr compile_values.resetBindingsV1
	moveq #0, d0
	rts
	.bend  ; opasmDriverPassTwoBegin

; Apply counted-repetition control before one statement reaches pass logic.
; Inputs: D0.W = current statement index.
; Outputs: D0 = status; D1 = 0 to process or 1 to skip; D2.W = next index when skipped.
; Clobbers: D0-D2/CCR.
; CCR: reflects D0 on return.
opasmDriverApplyFlowControl	.block
	movem.l d3-d7/a0-a2, -(sp)
	move.w d0, d7
	move.w d7, d0
	jsr navigation.initializeStatementFlowV1
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w processStatement
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	cmpi.l #1, d4
	bne.s checkFor
	cmpi.b #'=', (a0)
	bne.s checkFor
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	bra.w success

checkFor
	cmpi.l #5, d4
	beq.s compareBlock
	cmpi.l #6, d4
	bne.s checkEndblock
compareBlock
	moveq #0, d0
	move.w d4, d0
	lea BlockMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	beq.s checkEndblock
	bsr.w scopes.beginBlockScopeV1
	bne.w fail
	bra.w success

checkEndblock
	cmpi.l #8, d4
	beq.s compareEndblock
	cmpi.l #9, d4
	bne.s checkBend
compareEndblock
	moveq #0, d0
	move.w d4, d0
	lea EndblockMnemonicText, a1
	moveq #8, d1
	bsr.w lineStartsWith
	beq.s checkBend
	bsr.w scopes.endScopeDirectiveV1
	bne.w fail
	bra.w success

checkBend
	cmpi.l #4, d4
	beq.s compareBend
	cmpi.l #5, d4
	bne.s checkNamespace
compareBend
	moveq #0, d0
	move.w d4, d0
	lea BendMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	beq.s checkNamespace
	bsr.w scopes.endScopeDirectiveV1
	bne.w fail
	bra.w success

checkNamespace
	cmpi.l #9, d4
	beq.s compareNamespace
	cmpi.l #10, d4
	bne.s checkEndnamespace
compareNamespace
	moveq #0, d0
	move.w d4, d0
	lea NamespaceMnemonicText, a1
	moveq #9, d1
	bsr.w lineStartsWith
	beq.s checkEndnamespace
	bsr.w scopes.beginNamespaceScopeV1
	bne.w fail
	bra.w success

checkEndnamespace
	cmpi.l #12, d4
	beq.s compareEndnamespace
	cmpi.l #13, d4
	bne.s checkEndn
compareEndnamespace
	moveq #0, d0
	move.w d4, d0
	lea EndnamespaceMnemonicText, a1
	moveq #12, d1
	bsr.w lineStartsWith
	beq.s checkEndn
	bsr.w scopes.endScopeDirectiveV1
	bne.w fail
	bra.w success

checkEndn
	cmpi.l #4, d4
	beq.s compareEndn
	cmpi.l #5, d4
	bne.s checkConditional
compareEndn
	moveq #0, d0
	move.w d4, d0
	lea EndnMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	beq.s checkConditional
	bsr.w scopes.endScopeDirectiveV1
	bne.w fail
	bra.w success

checkConditional
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	move.l d4, d0
	jsr conditionals.routeDirectiveV1
	bne.w fail
	tst.w d3
	beq.w checkForMnemonic
	cmpi.w #1, d3
	beq.w beginMatchBranch
	cmpi.w #2, d3
	beq.w skipSelectedMatchBranch
	cmpi.w #3, d3
	beq.w finishEndmatchBranch
	cmpi.w #4, d3
	beq.w beginIfBranch
	cmpi.w #5, d3
	beq.w handleElseifBranch
	cmpi.w #6, d3
	beq.w handleElseBranch
	cmpi.w #7, d3
	beq.w finishIfBranch
	bra.w fail

checkForMnemonic
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	move.l d4, d0
	jsr repetition.routeDirectiveV1
	bne.w fail
	tst.w d3
	beq.w processStatement
	cmpi.w #1, d3
	beq.w beginFor
	cmpi.w #2, d3
	beq.w compareEndfor
	cmpi.w #3, d3
	beq.w compareWhile
	cmpi.w #4, d3
	beq.w compareEndwhile
	bra.w fail

	; Legacy fall-through routing retained below as dead compatibility labels for
	; the existing repetition state transitions until their next focused move.
	cmpi.l #3, d4
	beq.s compareFor
	cmpi.l #4, d4
	bne.s checkEndfor

compareFor
	moveq #0, d0
	move.w d4, d0
	lea ForMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w beginFor

checkEndfor
	cmpi.l #6, d4
	beq.s compareEndfor
	cmpi.l #7, d4
	bne.w checkWhile

compareEndfor
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	moveq #0, d0
	move.w d4, d0
	lea EndforMnemonicText, a1
	moveq #6, d1
	bsr.w lineStartsWith
	beq.w checkWhile
	tst.w OpasmRepeatDepth
	beq.w fail
	moveq #0, d3
	move.w OpasmRepeatDepth, d3
	subq.w #1, d3
	lea OpasmRepeatKind, a0
	cmpi.b #OPASM_REPEAT_KIND_FOR, 0(a0, d3.l)
	bne.w fail
	move.l d3, d4
	lsl.l #2, d4
	lea OpasmRepeatRemaining, a0
	move.l 0(a0, d4.l), d5
	subq.l #1, d5
	move.l d5, 0(a0, d4.l)
	beq.s finishFor
	lea OpasmRepeatHasBinding, a0
	tst.b 0(a0, d3.l)
	beq.s repeatBody
	move.l d3, d4
	lsl.l #2, d4
	lea OpasmRepeatStep, a0
	move.l 0(a0, d4.l), d1
	bne.s advanceRange
	lea OpasmRepeatValuePtr, a0
	movea.l 0(a0, d4.l), a1
	addq.l #4, a1
	move.l a1, 0(a0, d4.l)
	move.l (a1), d1
	bra.s updateBinding
advanceRange
	lea OpasmRepeatCurrentValue, a0
	add.l 0(a0, d4.l), d1
updateBinding
	lea OpasmRepeatCurrentValue, a0
	move.l d1, 0(a0, d4.l)
	jsr compile_values.updateTopBindingV1
	bne.w fail
repeatBody
	move.l d3, d4
	add.w d4, d4
	lea OpasmRepeatBodyStart, a0
	move.w 0(a0, d4.l), d2
	moveq #1, d1
	bra.w success

finishFor
	move.w OpasmRepeatDepth, d3
	subq.w #1, d3
	lea OpasmRepeatHasBinding, a0
	tst.b 0(a0, d3.l)
	beq.s finishPop
	jsr compile_values.popBindingV1
	bne.w fail
finishPop
	move.w d3, OpasmRepeatDepth
	moveq #1, d1
	bra.w success

checkWhile
	cmpi.l #5, d4
	beq.w compareWhile
	cmpi.l #6, d4
	beq.w compareWhile
	cmpi.l #8, d4
	beq.s compareEndwhile
	cmpi.l #9, d4
	bne.w processStatement
compareEndwhile
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	moveq #0, d0
	move.w d4, d0
	lea EndwhileMnemonicText, a1
	moveq #8, d1
	bsr.w lineStartsWith
	beq.w processStatement
	tst.w OpasmRepeatDepth
	beq.w fail
	moveq #0, d3
	move.w OpasmRepeatDepth, d3
	subq.w #1, d3
	lea OpasmRepeatKind, a0
	cmpi.b #OPASM_REPEAT_KIND_WHILE, 0(a0, d3.l)
	bne.w fail
	move.l d3, d4
	add.w d4, d4
	lea OpasmRepeatOpening, a0
	moveq #0, d0
	move.w 0(a0, d4.l), d0
	move.w d7, d6
	move.w d0, d7
	moveq #4, d5
	move.w #1, OpasmDriverWhileReevaluation
	bsr.w readWhileConditionForStatement
	move.l d0, d5
	clr.w OpasmDriverWhileReevaluation
	move.w d6, d7
	tst.l d5
	bne.w fail
	tst.l d3
	beq.w finishWhile
	moveq #0, d3
	move.w OpasmRepeatDepth, d3
	subq.w #1, d3
	move.l d3, d4
	lsl.l #2, d4
	lea OpasmRepeatRemaining, a0
	move.l 0(a0, d4.l), d5
	addq.l #1, d5
	cmpi.l #OPASM_REPEAT_ITERATION_LIMIT, d5
	bhi.w fail
	move.l d5, 0(a0, d4.l)
	move.l d3, d4
	add.w d4, d4
	lea OpasmRepeatBodyStart, a0
	move.w 0(a0, d4.l), d2
	moveq #1, d1
	bra.w success
finishWhile
	move.w OpasmRepeatDepth, d3
	subq.w #1, d3
	move.w d3, OpasmRepeatDepth
	moveq #1, d1
	bra.w success

compareWhile
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	moveq #0, d0
	move.w d4, d0
	lea WhileMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	beq.w processStatement
	moveq #4, d5
	bsr.w readWhileConditionForStatement
	bne.w fail
	tst.l d3
	beq.w zeroWhile
	moveq #0, d4
	move.w OpasmRepeatDepth, d4
	cmpi.w #OPASM_REPEAT_STACK_CAPACITY, d4
	bhs.w fail
	move.l d4, d5
	add.w d5, d5
	lea OpasmRepeatBodyStart, a0
	move.w d7, d6
	addq.w #1, d6
	move.w d6, 0(a0, d5.l)
	lea OpasmRepeatOpening, a0
	move.w d7, 0(a0, d5.l)
	move.l d4, d5
	lsl.l #2, d5
	lea OpasmRepeatRemaining, a0
	moveq #1, d6
	move.l d6, 0(a0, d5.l)
	lea OpasmRepeatKind, a0
	move.b #OPASM_REPEAT_KIND_WHILE, 0(a0, d4.l)
	addq.w #1, d4
	move.w d4, OpasmRepeatDepth
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	bra.w success
zeroWhile
	bsr.w findMatchingEndwhile
	bne.w fail
	moveq #1, d1
	addq.w #1, d2
	bra.w success

beginIfBranch
	moveq #0, d4
	move.w OpasmIfDepth, d4
	cmpi.w #OPASM_REPEAT_STACK_CAPACITY, d4
	bhs.w fail
	lea OpasmIfMatched, a0
	clr.b 0(a0, d4.l)
	addq.w #1, d4
	move.w d4, OpasmIfDepth
	moveq #4, d5
	bsr.w readOperandValueForStatement
	bne.w fail
	tst.l d3
	beq.s ifFalse
	moveq #0, d4
	move.w OpasmIfDepth, d4
	subq.w #1, d4
	lea OpasmIfMatched, a0
	move.b #1, 0(a0, d4.l)
	bra.w selectIfBranch
ifFalse
	bsr.w findNextIfBranch
	bne.w fail
	cmpi.w #1, d1
	beq.w selectIfBranchAtD2
	cmpi.w #2, d1
	beq.w selectElseIfBranchAtD2
	addq.w #1, d2
	bra.w finishIfBranch

handleElseifBranch
	tst.w OpasmIfDepth
	beq.w fail
	moveq #0, d4
	move.w OpasmIfDepth, d4
	subq.w #1, d4
	lea OpasmIfMatched, a0
	tst.b 0(a0, d4.l)
	bne.w skipSelectedIfBranch
	moveq #4, d5
	bsr.w readOperandValueForStatement
	bne.w fail
	tst.l d3
	beq.s elseifFalse
	move.b #1, 0(a0, d4.l)
	bra.w selectIfBranch
elseifFalse
	bsr.w findNextIfBranch
	bne.w fail
	cmpi.w #1, d1
	beq.w selectIfBranchAtD2
	cmpi.w #2, d1
	beq.w selectElseIfBranchAtD2
	addq.w #1, d2
	bra.w finishIfBranch

handleElseBranch
	tst.w OpasmIfDepth
	beq.w fail
	moveq #0, d4
	move.w OpasmIfDepth, d4
	subq.w #1, d4
	lea OpasmIfMatched, a0
	tst.b 0(a0, d4.l)
	bne.w skipSelectedIfBranch
	move.b #1, 0(a0, d4.l)
	bra.w selectIfBranch

finishIfBranch
	tst.w OpasmIfDepth
	beq.w fail
	move.w OpasmIfDepth, d4
	subq.w #1, d4
	move.w d4, OpasmIfDepth
	moveq #1, d1
	bra.w success

beginMatchBranch
	moveq #4, d5
	bsr.w readOperandValueForStatement
	bne.w fail
	move.l d3, OpasmMatchValue
	bsr.w findSelectedMatchBranch
	bne.w fail
	addq.w #1, d2
	moveq #1, d1
	bra.w success

skipSelectedMatchBranch
	bsr.w findMatchingEndmatch
	bne.w fail
	addq.w #1, d2
	moveq #1, d1
	bra.w success

finishEndmatchBranch
	moveq #1, d1
	bra.w success

selectIfBranch
	move.w d7, d2
	addq.w #1, d2
selectIfBranchAtD2
	moveq #1, d1
	bra.w success

selectElseIfBranchAtD2
	moveq #0, d4
	move.w OpasmIfDepth, d4
	subq.w #1, d4
	lea OpasmIfMatched, a0
	move.b #1, 0(a0, d4.l)
	addq.w #1, d2
	moveq #1, d1
	bra.w success

skipSelectedIfBranch
	bsr.w findMatchingEndif
	bne.w fail
	addq.w #1, d2
	moveq #1, d1
	bra.w success

beginFor
	moveq #0, d5
	move.w OpasmRepeatDepth, d5
	cmpi.w #OPASM_REPEAT_STACK_CAPACITY, d5
	bhs.w fail
	move.l d5, d6
	add.w d6, d6
	lea OpasmRepeatBodyStart, a0
	move.w d7, d0
	addq.w #1, d0
	move.w d0, 0(a0, d6.l)
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	jsr compile_values.planListForOperandV1
	beq.w beginIterableFor
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	jsr compile_values.planRangeForOperandV1
	beq.w beginIterableFor
	moveq #4, d5
	bsr.w readOperandValueForStatement
	bne.w fail
	cmpi.l #OPASM_REPEAT_ITERATION_LIMIT, d3
	bhi.w fail
	tst.l d3
	beq.w zeroFor
	moveq #0, d4
	move.w OpasmRepeatDepth, d4
	move.l d4, d5
	lsl.l #2, d5
	lea OpasmRepeatRemaining, a0
	move.l d3, 0(a0, d5.l)
	lea OpasmRepeatHasBinding, a0
	clr.b 0(a0, d4.l)
	lea OpasmRepeatKind, a0
	move.b #OPASM_REPEAT_KIND_FOR, 0(a0, d4.l)
	move.w OpasmRepeatDepth, d4
	addq.w #1, d4
	move.w d4, OpasmRepeatDepth
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	bra.w success

beginIterableFor
	cmpi.l #OPASM_REPEAT_ITERATION_LIMIT, d1
	bhi.w fail
	tst.l d1
	beq.w zeroFor
	moveq #0, d5
	move.w OpasmRepeatDepth, d5
	move.l d5, d6
	lsl.l #2, d6
	lea OpasmRepeatRemaining, a2
	move.l d1, 0(a2, d6.l)
	lea OpasmRepeatValuePtr, a2
	move.l a1, 0(a2, d6.l)
	lea OpasmRepeatCurrentValue, a2
	move.l d2, 0(a2, d6.l)
	lea OpasmRepeatStep, a2
	move.l d3, 0(a2, d6.l)
	lea OpasmRepeatHasBinding, a2
	move.b #1, 0(a2, d5.l)
	lea OpasmRepeatKind, a2
	move.b #OPASM_REPEAT_KIND_FOR, 0(a2, d5.l)
	move.l d4, d0
	move.l d2, d1
	jsr compile_values.pushBindingV1
	bne.w fail
	move.w OpasmRepeatDepth, d4
	addq.w #1, d4
	move.w d4, OpasmRepeatDepth
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	bra.w success

zeroFor
	bsr.w findMatchingEndfor
	bne.w fail
	moveq #1, d1
	addq.w #1, d2
	bra.w success

processStatement
	clr.w d1

success
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d3-d7/a0-a2
	rts
	.bend  ; opasmDriverApplyFlowControl

; Find the next same-level `.elseif`, `.else`, or `.endif` after an `.if` branch.
; Inputs: D7.W = current conditional statement index.
; Outputs: D0 = status; D1 = 1 elseif, 2 else, 3 endif; D2.W = statement index.
; Clobbers: D0-D6/A0-A1/CCR.
; CCR: reflects D0 on return.
findNextIfBranch	.block
	movem.l d3-d6/a0-a1, -(sp)
	move.w d7, d2
	moveq #1, d6
scan
	addq.w #1, d2
	jsr eng.opasmEngineGetStatementCountV1
	cmp.w d0, d2
	bhs.w fail
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d2, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w next
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	cmpi.l #2, d4
	beq.s maybeIf
	cmpi.l #3, d4
	bne.s maybeEndif
maybeIf
	moveq #0, d0
	move.w d4, d0
	lea IfMnemonicText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	beq.s maybeEndif
	addq.w #1, d6
	bra.w next
maybeEndif
	cmpi.l #5, d4
	beq.s compareEndif
	cmpi.l #6, d4
	bne.s outerBranch
compareEndif
	moveq #0, d0
	move.w d4, d0
	lea EndifMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	beq.s outerBranch
	subq.w #1, d6
	beq.w foundEndif
	bra.w next
outerBranch
	cmpi.w #1, d6
	bne.w next
	cmpi.l #6, d4
	beq.s compareElseif
	cmpi.l #7, d4
	bne.s maybeElse
compareElseif
	moveq #0, d0
	move.w d4, d0
	lea ElseifMnemonicText, a1
	moveq #6, d1
	bsr.w lineStartsWith
	beq.s maybeElse
	moveq #1, d1
	bra.w found
maybeElse
	cmpi.l #4, d4
	beq.s compareElse
	cmpi.l #5, d4
	bne.w next
compareElse
	moveq #0, d0
	move.w d4, d0
	lea ElseMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	beq.w next
	moveq #2, d1
	bra.w found
foundEndif
	moveq #3, d1
found
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	bra.s return
next
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	bra.w scan
fail
	moveq #1, d0
return
	movem.l (sp)+, d3-d6/a0-a1
	rts
	.bend  ; findNextIfBranch

; Find the matching same-level `.endif` after one selected branch marker.
; Inputs: D7.W = current `.elseif` or `.else` statement index.
; Outputs: D0 = status; D2.W = matching endif index.
; Clobbers: D0-D6/A0-A1/CCR.
; CCR: reflects D0 on return.
findMatchingEndif	.block
	movem.l d1/d3-d6/a0-a1, -(sp)
	move.w d7, d2
	moveq #1, d6
scan
	addq.w #1, d2
	jsr eng.opasmEngineGetStatementCountV1
	cmp.w d0, d2
	bhs.w fail
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d2, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.s next
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	cmpi.l #2, d4
	beq.s maybeIf
	cmpi.l #3, d4
	bne.s maybeEndif
maybeIf
	moveq #0, d0
	move.w d4, d0
	lea IfMnemonicText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	beq.s maybeEndif
	addq.w #1, d6
	bra.s next
maybeEndif
	cmpi.l #5, d4
	beq.s compareEndif
	cmpi.l #6, d4
	bne.s next
compareEndif
	moveq #0, d0
	move.w d4, d0
	lea EndifMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	beq.s next
	subq.w #1, d6
	beq.s found
next
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	bra.w scan
found
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1/d3-d6/a0-a1
	rts
	.bend  ; findMatchingEndif

; Find the first same-level matching `.case`, otherwise `.default`, for `.match`.
; Inputs: D7.W = match statement index; OpasmMatchValue = scalar match value.
; Outputs: D0 = status; D2.W = selected case/default/endmatch statement index.
; Clobbers: D0-D6/A0-A1/CCR.
; CCR: reflects D0 on return.
findSelectedMatchBranch	.block
	movem.l d1/d3-d6/a0-a1, -(sp)
	move.w d7, d5
	move.w #$ffff, OpasmMatchDefaultIndex
	move.w d7, d2
	moveq #1, d6
scan
	addq.w #1, d2
	jsr eng.opasmEngineGetStatementCountV1
	cmp.w d0, d2
	bhs.w fail
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d2, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w next
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	cmpi.l #5, d4
	beq.s maybeMatch
	cmpi.l #6, d4
	bne.s maybeEndmatch
maybeMatch
	moveq #0, d0
	move.w d4, d0
	lea MatchMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	beq.s maybeEndmatch
	addq.w #1, d6
	bra.w next
maybeEndmatch
	cmpi.l #8, d4
	beq.s compareEndmatch
	cmpi.l #9, d4
	bne.s outerBranch
compareEndmatch
	moveq #0, d0
	move.w d4, d0
	lea EndmatchMnemonicText, a1
	moveq #8, d1
	bsr.w lineStartsWith
	beq.s outerBranch
	subq.w #1, d6
	beq.w chooseDefault
	bra.w next
outerBranch
	cmpi.w #1, d6
	bne.w next
	cmpi.l #4, d4
	beq.s compareCase
	cmpi.l #5, d4
	bne.s maybeDefault
compareCase
	moveq #0, d0
	move.w d4, d0
	lea CaseMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	beq.s maybeDefault
	move.w d2, d7
	bsr.w caseMatchesCurrentValue
	move.l d0, d4
	move.w d5, d7
	tst.l d4
	bne.w next
	bra.w found
maybeDefault
	cmpi.l #7, d4
	beq.s compareDefault
	cmpi.l #8, d4
	bne.w next
compareDefault
	moveq #0, d0
	move.w d4, d0
	lea DefaultMnemonicText, a1
	moveq #7, d1
	bsr.w lineStartsWith
	beq.w next
	move.w d2, OpasmMatchDefaultIndex
	bra.w next
chooseDefault
	move.w OpasmMatchDefaultIndex, d0
	cmpi.w #$ffff, d0
	beq.s found
	move.w d0, d2
found
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	bra.s return
next
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	bra.w scan
fail
	moveq #1, d0
return
	movem.l (sp)+, d1/d3-d6/a0-a1
	rts
	.bend  ; findSelectedMatchBranch

; Return whether any comma-separated `.case` value matches OpasmMatchValue.
; Inputs: D7.W = case statement index.
; Outputs: D0 = 0 on match, 1 otherwise/malformed.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
caseMatchesCurrentValue	.block
	movem.l d1-d6/a0-a2, -(sp)
	bsr.w countCommaPartsForStatement
	bne.w fail
	move.w d3, d5
	moveq #1, d6
loop
	cmp.w d5, d6
	bhi.s no
	bsr.w readCommaOperandValueForStatement
	bne.w fail
	cmp.l OpasmMatchValue, d3
	beq.s match
	addq.w #1, d6
	bra.s loop
match
	moveq #0, d0
	bra.s return
no
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d6/a0-a2
	rts
	.bend  ; caseMatchesCurrentValue

; Find a matching same-level `.endmatch` after a selected case/default.
; Inputs: D7.W = current case/default statement index.
; Outputs: D0 = status; D2.W = matching endmatch index.
; Clobbers: D0-D6/A0-A1/CCR.
; CCR: reflects D0 on return.
findMatchingEndmatch	.block
	movem.l d1/d3-d6/a0-a1, -(sp)
	move.w d7, d2
	moveq #1, d6
scan
	addq.w #1, d2
	jsr eng.opasmEngineGetStatementCountV1
	cmp.w d0, d2
	bhs.w fail
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d2, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.s next
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	cmpi.l #5, d4
	beq.s maybeMatch
	cmpi.l #6, d4
	bne.s maybeEndmatch
maybeMatch
	moveq #0, d0
	move.w d4, d0
	lea MatchMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	beq.s maybeEndmatch
	addq.w #1, d6
	bra.s next
maybeEndmatch
	cmpi.l #8, d4
	beq.s compareEndmatch
	cmpi.l #9, d4
	bne.s next
compareEndmatch
	moveq #0, d0
	move.w d4, d0
	lea EndmatchMnemonicText, a1
	moveq #8, d1
	bsr.w lineStartsWith
	beq.s next
	subq.w #1, d6
	beq.s found
next
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	bra.w scan
found
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1/d3-d6/a0-a1
	rts
	.bend  ; findMatchingEndmatch

; Find the matching `.endfor` for a zero-count `.for`.
; Inputs: D7.W = opening statement index.
; Outputs: D0 = status; D2.W = matching end statement on success.
; Clobbers: D0-D6/A0-A1/CCR.
; CCR: reflects D0 on return.
findMatchingEndfor	.block
	movem.l d1/d3-d6/a0-a1, -(sp)
	move.w d7, d2
	moveq #1, d6

scan
	addq.w #1, d2
	jsr eng.opasmEngineGetStatementCountV1
	cmp.w d0, d2
	bhs.w fail
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d2, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.s next
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	cmpi.l #3, d4
	beq.s compareNestedFor
	cmpi.l #4, d4
	bne.s maybeEnd

compareNestedFor
	moveq #0, d0
	move.w d4, d0
	lea ForMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	beq.s maybeEnd
	addq.w #1, d6
	bra.s next

maybeEnd
	cmpi.l #6, d4
	beq.s compareNestedEnd
	cmpi.l #7, d4
	bne.s next

compareNestedEnd
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	moveq #0, d0
	move.w d4, d0
	lea EndforMnemonicText, a1
	moveq #6, d1
	bsr.w lineStartsWith
	beq.s next
	subq.w #1, d6
	beq.s found

next
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	bra.w scan

found
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1/d3-d6/a0-a1
	rts
	.bend  ; findMatchingEndfor

; Find the matching `.endwhile` for a false-first `.while`.
; Inputs: D7.W = opening statement index.
; Outputs: D0 = status; D2.W = matching end statement on success.
; Clobbers: D0-D6/A0-A1/CCR.
; CCR: reflects D0 on return.
findMatchingEndwhile	.block
	movem.l d1/d3-d6/a0-a1, -(sp)
	move.w d7, d2
	moveq #1, d6
scan
	addq.w #1, d2
	jsr eng.opasmEngineGetStatementCountV1
	cmp.w d0, d2
	bhs.w fail
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d2, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.s next
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	cmpi.l #5, d4
	beq.s compareNestedWhile
	cmpi.l #6, d4
	bne.s maybeEnd
compareNestedWhile
	moveq #0, d0
	move.w d4, d0
	lea WhileMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	beq.s maybeEnd
	addq.w #1, d6
	bra.s next
maybeEnd
	cmpi.l #8, d4
	beq.s compareNestedEndwhile
	cmpi.l #9, d4
	bne.s next
compareNestedEndwhile
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	moveq #0, d0
	move.w d4, d0
	lea EndwhileMnemonicText, a1
	moveq #8, d1
	bsr.w lineStartsWith
	beq.s next
	subq.w #1, d6
	beq.s found
next
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	bra.w scan
found
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1/d3-d6/a0-a1
	rts
	.bend  ; findMatchingEndwhile

opasmDriverPassTwoOk	.block
	moveq #abi.OPASM_EVENT_PASS_OK, d0
	moveq #2, d1
	bsr.w appendPassEvent
	moveq #0, d0
	rts
	.bend  ; opasmDriverPassTwoOk

opasmDriverRecordLabel	.block
	movem.l d1-d7/a0-a1, -(sp)
	move.w d0, d7
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w recordPcLabel
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a0), a1
	move.l a1, d5
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(a0), d4
	beq.w recordPcLabel
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea ConstMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w recordConstSymbol
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea VarMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w recordMutableSymbol
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea SetMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w recordMutableSymbol

recordPcLabel
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineRecordStatementLabelV1
	bra.w handleLabelEvent

recordConstSymbol
	moveq #0, d6
	bra.s recordSymbolValue

recordMutableSymbol
	moveq #1, d6

recordSymbolValue
	moveq #2, d5
	move.l d7, -(sp)
	bsr.w readOperandValueForStatement
	move.l d0, d5
	move.l (sp)+, d7
	tst.l d5
	bne.w symbolValueFail
	move.l d3, -(sp)
	jsr scopes.qualifyStatementLabelIfScopedV1
	move.l d0, d5
	move.l (sp)+, d3
	tst.l d5
	bne.w symbolValueFail
	moveq #0, d0
	move.w d7, d0
	move.w d6, d4
	jsr eng.opasmEngineRecordStatementLabelValueV1

handleLabelEvent
	move.l a0, d4
	move.l d2, d5
	cmpi.w #eng.OPASM_ENGINE_LABEL_EVENT_STORED, d1
	beq.s stored
	cmpi.w #eng.OPASM_ENGINE_LABEL_EVENT_DUPLICATE, d1
	beq.s duplicate
	bra.s return

stored
	movea.l d4, a0
	bsr.w tokenLen
	move.w d0, d1
	movea.l d4, a0
	move.l d5, d2
	moveq #abi.OPASM_EVENT_LABEL_STORED, d0
	bsr.w appendTextValueEvent
	moveq #0, d0
	bra.s return

duplicate
	movea.l d4, a0
	bsr.w tokenLen
	move.w d0, d1
	movea.l d4, a0
	moveq #abi.OPASM_EVENT_LABEL_DUPLICATE, d0
	bsr.w appendTextEvent
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a1
	rts

symbolValueFail
	moveq #1, d0
	bra.s return
	.bend  ; opasmDriverRecordLabel

opasmDriverEmitImageBytes	.block
	movem.l d1-d6/a0-a4, -(sp)
	move.w d0, d6
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d6, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w ok
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a0), a1
	move.l a1, d5
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(a0), d4
	beq.w ok
	moveq #0, d0
	move.w d6, d0
	move.w d4, d1
	movea.l d5, a0
	jsr eng.opasmEngineStatementMnemonicDuplicatesLabelV1
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea OrgMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea CpuMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea ConstMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea VarMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea SetMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea EndMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea RegionMnemonicText, a1
	moveq #6, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea SectionMnemonicText, a1
	moveq #7, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea EndsectionMnemonicText, a1
	moveq #10, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea PlaceMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea AlignMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w emitAlign
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea DsMnemonicText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	bne.w emitDs
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea ResMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w ok
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea FillMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w emitFill
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea ByteMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w emitByte
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea DbMnemonicText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	bne.w emitByte
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea WordMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w emitWord
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea DwMnemonicText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	bne.w emitWord
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea LongMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w emitLong
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea TextMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w emitText
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea NullMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w emitNull
	movea.l d5, a0
	moveq #0, d0
	move.w d4, d0
	lea PtextMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w emitPtext
	bsr.w prepareEncodeSelectedRequestForStatement
	bne.w return
	tst.w OpasmDriverEvalRequestLen
	beq.w ok
	bsr.w prepareEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.dispatchEncodeSelectedV1
	move.w d2, d4
	tst.b d0
	bne.w serviceFail
	tst.w d1
	beq.w noOutput
	move.w d1, d6
	bsr.w serviceFramePtr
	jsr tkpkg.readOutputPtrV1
	moveq #0, d0
	move.w d6, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.w fail
	moveq #abi.OPASM_EVENT_SELECTOR_STATUS_OK, d0
	bsr.w appendKindEvent

ok
	moveq #0, d0
	bra.w return

fail
	moveq #abi.OPASM_EVENT_IMAGE_CAPACITY_EXCEEDED, d0
	suba.l a0, a0
	moveq #0, d1
	move.l d6, d2
	bsr.w appendTextValueEvent
	moveq #1, d0
	bra.w return

serviceFail
	moveq #0, d0
	move.w d6, d0
	jsr eng.opasmEngineStatementLooksBareColumnOneV1
	bne.w ok
	tst.w d4
	beq.s serviceFailReturn
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	clr.b 0(a0, d4.W)
	bsr.w emitSelectorFailureEvent
	bne.s serviceFailReturn
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	move.w d4, d1
	moveq #abi.OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w appendTextEvent

serviceFailReturn
	moveq #1, d0
	bra.w return

noOutput
	moveq #abi.OPASM_EVENT_UNSUPPORTED_ADDRESSING, d0
	bsr.w appendKindEvent
	moveq #1, d0
	bra.w return

emitAlign
	move.w d6, d7
	bsr.w readAlignPadForStatement
	bne.w emitLayoutFail
	move.l d3, d0
	moveq #0, d1
	bsr.w appendRepeatedByte
	bne.w emitLayoutFail
	moveq #0, d0
	bra.w return

emitDs
	move.w d6, d7
	moveq #2, d5
	bsr.w readOperandValueForStatement
	bne.w emitLayoutFail
	move.l d3, d0
	moveq #0, d1
	bsr.w appendRepeatedByte
	bne.w emitLayoutFail
	moveq #0, d0
	bra.w return

emitFill
	move.w d6, d7
	moveq #2, d6
	bsr.w readCommaOperandValueForStatement
	bne.w emitLayoutFail
	move.l d3, d5
	move.w d7, d6
	moveq #3, d6
	bsr.w readCommaOperandValueForStatement
	bne.w emitLayoutFail
	move.l d5, d0
	move.b d3, d1
	bsr.w appendRepeatedByte
	bne.w emitLayoutFail
	moveq #0, d0
	bra.w return

emitByte
	move.w d6, d7
	moveq #1, d5
	bsr.w emitDataDirectiveForStatement
	bne.w emitLayoutFail
	moveq #0, d0
	bra.w return

emitWord
	move.w d6, d7
	moveq #2, d5
	bsr.w emitDataDirectiveForStatement
	bne.w emitLayoutFail
	moveq #0, d0
	bra.w return

emitLong
	move.w d6, d7
	moveq #4, d5
	bsr.w emitDataDirectiveForStatement
	bne.w emitLayoutFail
	moveq #0, d0
	bra.w return

emitText
	move.w d6, d7
	moveq #0, d5
	bsr.w emitTextDirectiveForStatement
	bne.w emitLayoutFail
	moveq #0, d0
	bra.w return

emitNull
	move.w d6, d7
	moveq #1, d5
	bsr.w emitTextDirectiveForStatement
	bne.w emitLayoutFail
	moveq #0, d0
	bra.w return

emitPtext
	move.w d6, d7
	moveq #2, d5
	bsr.w emitTextDirectiveForStatement
	bne.w emitLayoutFail
	moveq #0, d0
	bra.w return

emitLayoutFail
	moveq #abi.OPASM_EVENT_UNRESOLVED_LABEL, d0
	bsr.w appendKindEvent
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d6/a0-a4
	rts
	.bend  ; opasmDriverEmitImageBytes

; Advance the current PC by the size inferred for the selected statement text.
; Inputs: D0 = statement index.
; Outputs: D0 = 0 on success, nonzero on selector or evaluation failure.
; Clobbers: D1-D7/A0-A3/CCR.
; CCR: reflects D0 on return.
opasmDriverAdvancePc	.block
	movem.l d0-d7/a0-a3, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	move.l d0, d7
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w done
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(a0), a1
	move.l a1, d5
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(a0), d6
	moveq #0, d4
	move.w d7, d4
	add.w d4, d4
	tst.w d6
	beq.w done
	moveq #0, d0
	move.w d7, d0
	move.w d6, d1
	movea.l d5, a0
	jsr eng.opasmEngineStatementMnemonicDuplicatesLabelV1
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea OrgMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w org
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea CpuMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea ConstMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea VarMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea SetMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea EndMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w done
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea RegionMnemonicText, a1
	moveq #6, d1
	bsr.w lineStartsWith
	bne.w region
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea SectionMnemonicText, a1
	moveq #7, d1
	bsr.w lineStartsWith
	bne.w section
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea EndsectionMnemonicText, a1
	moveq #10, d1
	bsr.w lineStartsWith
	bne.w endsection
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea PlaceMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w place
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea AlignMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w align
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea DsMnemonicText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	bne.w ds
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea ResMnemonicText, a1
	moveq #3, d1
	bsr.w lineStartsWith
	bne.w res
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea FillMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w fill
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea ByteMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w byte
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea DbMnemonicText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	bne.w byte
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea WordMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w word
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea DwMnemonicText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	bne.w word
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea LongMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w long
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea TextMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w text
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea NullMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	bne.w null
	movea.l d5, a0
	moveq #0, d0
	move.w d6, d0
	lea PtextMnemonicText, a1
	moveq #5, d1
	bsr.w lineStartsWith
	bne.w ptext
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #2, d0
	bne.s selectedSizeDispatch
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementOutputByteCountV1
	move.l d0, d1
	beq.s selectedSizeDispatch
	moveq #0, d0
	bra.s selectedSizeOk

selectedSizeDispatch
	moveq #0, d0
	move.w d7, d0
	bsr.w trySelectedEncodeSizeForStatement
	beq.s selectedSizeOk
	move.w d7, d0
	jsr eng.opasmEngineStatementLooksBareColumnOneV1
	bne.w done
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a3
	moveq #1, d0
	rts

selectedSizeOk
	cmpi.w #1, d1
	beq.w advanceOne
	cmpi.w #2, d1
	beq.w advanceTwo
	cmpi.w #3, d1
	beq.w advanceThree
	bra.w done

org
	moveq #2, d5
	bsr.w readOperandValueForStatement
	beq.s orgOk

orgBad
	moveq #abi.OPASM_EVENT_BAD_ORG, d0
	bsr.w appendKindEvent
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a3
	moveq #1, d0
	rts

orgOk
	move.l d3, d0
	jsr eng.opasmEngineSetOriginV1
	bra.w done

region
	bsr.w processRegionDirectiveForStatement
	beq.w done
	bra.s orgBad

section
	bsr.w processSectionDirectiveForStatement
	beq.w done
	bra.s orgBad

endsection
	bsr.w processEndsectionDirectiveForStatement
	beq.w done
	bra.s orgBad

place
	bsr.w processPlaceDirectiveForStatement
	beq.w done
	bra.s orgBad

align
	bsr.w readAlignPadForStatement
	beq.s advanceLayoutD3
	bra.s orgBad

ds
	moveq #2, d5
	bsr.w readOperandValueForStatement
	beq.s advanceLayoutD3
	bra.s orgBad

res
	moveq #2, d6
	bsr.w readCommaOperandValueForStatement
	beq.s advanceLayoutD3
	bra.s orgBad

fill
	moveq #2, d6
	bsr.w readCommaOperandValueForStatement
	bne.s orgBad

advanceLayoutD3
	move.l d3, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

byte
	moveq #1, d5
	bsr.w dataDirectiveSizeForStatement
	beq.s advanceLayoutD3
	bra.w orgBad

word
	moveq #2, d5
	bsr.w dataDirectiveSizeForStatement
	beq.s advanceLayoutD3
	bra.w orgBad

long
	moveq #4, d5
	bsr.w dataDirectiveSizeForStatement
	beq.s advanceLayoutD3
	bra.w orgBad

text
	moveq #0, d5
	bsr.w textDirectiveSizeForStatement
	beq.s advanceLayoutD3
	bra.w orgBad

null
	moveq #1, d5
	bsr.w textDirectiveSizeForStatement
	beq.s advanceLayoutD3
	bra.w orgBad

ptext
	moveq #2, d5
	bsr.w textDirectiveSizeForStatement
	beq.s advanceLayoutD3
	bra.w orgBad

advanceOne
	moveq #1, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

advanceTwo
	moveq #2, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

advanceThree
	moveq #3, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

done
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a3
	moveq #0, d0
	rts
	.bend  ; opasmDriverAdvancePc

trySelectedEncodeSizeForStatement	.block
	movem.l d2-d7/a0-a2, -(sp)
	move.w d0, d6
	clr.w d4
	bsr.w prepareEncodeSelectedRequestForStatement
	bne.w prepareFail
	tst.w OpasmDriverEvalRequestLen
	beq.w empty
	bsr.w prepareEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.dispatchEncodeSelectedV1
	move.w d2, d4
	tst.b d0
	bne.w fail
	moveq #0, d0
	bra.s return

empty
	moveq #0, d1
	moveq #0, d0
	bra.s return

prepareFail

fail
	tst.w d4
	beq.w failReturn
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	clr.b 0(a0, d4.W)
	bsr.w emitSelectorFailureEvent
	bne.w failReturn
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	move.w d4, d1
	moveq #abi.OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w appendTextEvent

failReturn
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a0-a2
	rts
	.bend  ; trySelectedEncodeSizeForStatement

; Inputs:
;   D7.W = statement index
;   D5.B = operand width policy used for range checks
; Outputs:
;   D0.L = 0 on success, 1 when the operand cannot be resolved or fails width checks
;   D3.L = resolved operand value on success
; Clobbers:
;   D0-D7/A0-A2/CCR
; CCR:
;   Reflects D0.L on return
readOperandValueForStatement	.block
	movem.l d1-d2/d4-d7/a0-a2, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	tst.w OpasmDriverForceStoredOperand
	bne.w storedText
	moveq #0, d0
	move.w d7, d0
	jsr eng.statementHasExprMetadataV1
	tst.l d0
	beq.s storedText
	moveq #1, d6
	bra.w loadExprSlice

loadExprSlice
	suba.l #eng.OPASM_ENGINE_EXPR_META_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementExprMetadataV1
	tst.l d0
	beq.s exprSliceStoredTextFallback
	move.l eng.OPASM_ENGINE_EXPR_META_SPAN_START(a0), d1
	move.l eng.OPASM_ENGINE_EXPR_META_SPAN_END(a0), d2
	adda.l #eng.OPASM_ENGINE_EXPR_META_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementExprTextSliceV1
	tst.l d0
	beq.w storedText
	bra.s haveText

exprSliceStoredTextFallback
	adda.l #eng.OPASM_ENGINE_EXPR_META_BYTES, sp
	bra.w storedText

storedText
	clr.w d6
	clr.l d3
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w fail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	bne.s haveText
	moveq #1, d0
	bra.w return

haveText
	tst.w d6
	bne.s prepareRequest
	bsr.w skipLineWhitespace
	bne.s prepareRequest
	moveq #1, d0
	bra.w return

prepareRequest
	move.l a0, OpasmDriverEvalFallbackPtr
	move.l d0, OpasmDriverEvalFallbackLen
	bsr.w parseDirectiveLiteralValue
	beq.w checkWidth
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w skipLineWhitespace
	bsr.w trimLiteralFallbackTrailing
	jsr compile_values.resolveBindingV1
	beq.w checkWidth
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w skipLineWhitespace
	bsr.w trimLiteralFallbackTrailing
	jsr compile_values.resolveSequenceExpressionV1
	beq.w checkWidth
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w skipLineWhitespace
	bsr.w trimLiteralFallbackTrailing
	jsr scopes.resolveLabelValueV1
	beq.w checkWidth
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w skipLineWhitespace
	bsr.w trimLiteralFallbackTrailing
	jsr eng.opasmEngineResolveLabelValueV1
	beq.w checkWidth
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w resolveDottedLabelSuffixValue
	beq.w checkWidth
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w prepareEvaluateExpressionRequest
	beq.w prepareExtension
	bra.w evalFallback

prepareExtension
	bsr.w prepareEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.dispatchEvaluateExpressionV1
	move.w d0, d4
	beq.w readValue
	tst.w d2
	beq.w evalFallback
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	move.w d2, d1
	moveq #abi.OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w appendTextEvent
	bra.w evalFallback

readValue
	bsr.w readEvaluateExpressionValue
	bra.w checkWidth

checkWidth
	cmpi.b #1, d5
	bne.s ok
	cmpi.l #$000000FF, d3
	bls.s ok

fail
	moveq #abi.OPASM_EVENT_UNRESOLVED_LABEL, d0
	bsr.w appendKindEvent
	moveq #1, d0
	bra.s return

ok
	moveq #0, d0
	bra.s return

evalFallback
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w parseDirectiveLiteralValue
	beq.w ok
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w resolveDottedLabelSuffixValue
	beq.w ok

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a2
	rts
	.bend  ; readOperandValueForStatement

; Evaluate one `.while` condition from its complete stored operand.
; Inputs/outputs/clobbers match readOperandValueForStatement.
readWhileConditionForStatement	.block
	bsr.w readCurrentPcWhileCondition
	beq.s return
	move.w #1, OpasmDriverForceStoredOperand
	bsr.w readOperandValueForStatement
	move.l d0, -(sp)
	clr.w OpasmDriverForceStoredOperand
	move.l (sp)+, d0
return
	rts
	.bend  ; readWhileConditionForStatement

; Evaluate `$ < literal` or `$ <= literal` against the current session PC.
; Inputs: D7.W = statement index.
; Outputs: D0 = status; D3 = 0 or 1 on success.
; Clobbers: D0-D7/A0-A2/CCR.
; CCR: reflects D0 on return.
readCurrentPcWhileCondition	.block
	movem.l d1-d2/d4-d7/a0-a2, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w fail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	bsr.w skipLineWhitespace
	tst.l d0
	beq.w fail
	cmpi.b #'$', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	bsr.w skipLineWhitespace
	tst.l d0
	beq.w fail
	cmpi.b #'<', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d0
	clr.w d6
	tst.l d0
	beq.w fail
	cmpi.b #'=', (a0)
	bne.s right
	moveq #1, d6
	addq.l #1, a0
	subq.l #1, d0
right
	bsr.w skipLineWhitespace
	bsr.w parseDirectiveLiteralValue
	bne.w fail
	move.l d3, d4
	jsr eng.opasmEngineGetSessionCurrentPcV1
	tst.w OpasmDriverWhileReevaluation
	beq.s comparePc
	tst.l d0
	beq.s comparePc
	subq.l #1, d0
comparePc
	clr.l d3
	tst.w d6
	bne.s lessEqual
	cmp.l d4, d0
	bcs.s true
	bra.s ok
lessEqual
	cmp.l d4, d0
	bls.s true
	bra.s ok
true
	moveq #1, d3
ok
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a2
	moveq #0, d0
	rts
fail
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a2
	moveq #1, d0
	rts
	.bend  ; readCurrentPcWhileCondition

; Reset native layout directive state for one assembly session.
; Outputs: D0.L = 0.
; Clobbers: D0/CCR.
; CCR: reflects D0.L on return.
resetLayoutState	.block
	clr.w OpasmLayoutRegionCount
	clr.w OpasmLayoutSectionCount
	clr.w OpasmLayoutSectionActive
	move.w #OPASM_LAYOUT_INDEX_NONE, OpasmLayoutActiveSectionIndex
	clr.w OpasmLayoutScratchNameLen
	clr.b OpasmLayoutScratchName
	clr.w OpasmLayoutPlaceSectionNameLen
	clr.b OpasmLayoutPlaceSectionName
	clr.w OpasmLayoutPlaceRegionNameLen
	clr.b OpasmLayoutPlaceRegionName
	moveq #0, d0
	rts
	.bend  ; resetLayoutState

; Store a native layout region from `.region <name>, <start>, <end>`.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 on invalid region bounds.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
processRegionDirectiveForStatement	.block
	movem.l d1-d7/a0-a3, -(sp)
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #2, d0
	beq.w ok
	lea OpasmLayoutScratchName.l, a1
	moveq #1, d6
	bsr.w readCommaNameForStatement
	bne.w fail
	move.w d3, OpasmLayoutScratchNameLen
	moveq #2, d6
	bsr.w readCommaOperandValueForStatement
	bne.w fail
	move.l d3, OpasmLayoutScratchStart
	moveq #3, d6
	bsr.w readCommaOperandValueForStatement
	bne.w fail
	move.l d3, OpasmLayoutScratchEnd
	cmp.l OpasmLayoutScratchStart, d3
	blo.w fail
	moveq #4, d6
	bsr.w readAlignOptionForStatement
	bne.w fail
	moveq #0, d0
	move.w OpasmLayoutRegionCount, d0
	cmpi.w #OPASM_LAYOUT_REGION_CAPACITY, d0
	bhs.w fail
	bsr.w findRegionByScratchName
	beq.w fail
	bsr.w layoutScratchRegionOverlapsExisting
	bne.w fail
	lea OpasmLayoutScratchName.l, a0
	lea OpasmLayoutRegionNames.l, a1
	moveq #0, d0
	move.w OpasmLayoutRegionCount, d0
	lsl.l #5, d0
	lea 0(a1, d0.w), a1
	move.w OpasmLayoutScratchNameLen, d0
	bsr.w copyNameBytes
	lea OpasmLayoutRegionNameLens.l, a0
	moveq #0, d0
	move.w OpasmLayoutRegionCount, d0
	lsl.l #1, d0
	lea 0(a0, d0.w), a0
	move.w OpasmLayoutScratchNameLen, (a0)
	lea OpasmLayoutRegionStarts.l, a0
	moveq #0, d0
	move.w OpasmLayoutRegionCount, d0
	lsl.l #2, d0
	lea 0(a0, d0.w), a0
	move.l OpasmLayoutScratchStart, (a0)
	lea OpasmLayoutRegionEnds.l, a0
	moveq #0, d0
	move.w OpasmLayoutRegionCount, d0
	lsl.l #2, d0
	lea 0(a0, d0.w), a0
	move.l OpasmLayoutScratchEnd, (a0)
	lea OpasmLayoutRegionCursors.l, a0
	moveq #0, d0
	move.w OpasmLayoutRegionCount, d0
	lsl.l #2, d0
	lea 0(a0, d0.w), a0
	move.l OpasmLayoutScratchStart, (a0)
	lea OpasmLayoutRegionAligns.l, a0
	moveq #0, d0
	move.w OpasmLayoutRegionCount, d0
	lsl.l #2, d0
	lea 0(a0, d0.w), a0
	move.l d3, (a0)
	lea OpasmLayoutRegionCount.l, a0
	addq.w #1, (a0)

ok
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; processRegionDirectiveForStatement

; Enter a native layout section slice.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
processSectionDirectiveForStatement	.block
	movem.l d1-d7/a0-a3, -(sp)
	lea OpasmLayoutScratchName.l, a1
	moveq #1, d6
	bsr.w readCommaNameForStatement
	bne.w fail
	move.w d3, OpasmLayoutScratchNameLen
	moveq #2, d6
	bsr.w readAlignOptionForStatement
	bne.w fail
	tst.w OpasmLayoutSectionActive
	bne.w fail
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #2, d0
	beq.w passTwo
	moveq #0, d0
	move.w OpasmLayoutSectionCount, d0
	cmpi.w #OPASM_LAYOUT_SECTION_CAPACITY, d0
	bhs.w fail
	bsr.w findSectionByScratchName
	beq.w fail
	lea OpasmLayoutScratchName.l, a0
	lea OpasmLayoutSectionNames.l, a1
	moveq #0, d0
	move.w OpasmLayoutSectionCount, d0
	lsl.l #5, d0
	lea 0(a1, d0.w), a1
	move.w OpasmLayoutScratchNameLen, d0
	bsr.w copyNameBytes
	lea OpasmLayoutSectionNameLens.l, a0
	moveq #0, d0
	move.w OpasmLayoutSectionCount, d0
	lsl.l #1, d0
	lea 0(a0, d0.w), a0
	move.w OpasmLayoutScratchNameLen, (a0)
	lea OpasmLayoutSectionPlacedFlags.l, a0
	moveq #0, d0
	move.w OpasmLayoutSectionCount, d0
	lsl.l #1, d0
	lea 0(a0, d0.w), a0
	clr.w (a0)
	lea OpasmLayoutSectionAligns.l, a0
	moveq #0, d0
	move.w OpasmLayoutSectionCount, d0
	lsl.l #2, d0
	lea 0(a0, d0.w), a0
	move.l d3, (a0)
	lea OpasmLayoutSectionSizes.l, a0
	moveq #0, d0
	move.w OpasmLayoutSectionCount, d0
	lsl.l #2, d0
	lea 0(a0, d0.w), a0
	clr.l (a0)
	lea OpasmLayoutSectionBases.l, a0
	moveq #0, d0
	move.w OpasmLayoutSectionCount, d0
	lsl.l #2, d0
	lea 0(a0, d0.w), a0
	clr.l (a0)
	move.w #1, OpasmLayoutSectionActive
	move.w OpasmLayoutSectionCount, d0
	move.w d0, d1
	move.w d1, OpasmLayoutActiveSectionIndex
	lea OpasmLayoutSectionCount.l, a0
	addq.w #1, (a0)
	jsr eng.opasmEngineGetSessionCurrentPcV1
	move.l d0, d1
	lea OpasmLayoutSectionStartPcs.l, a0
	moveq #0, d0
	move.w OpasmLayoutActiveSectionIndex, d0
	lsl.l #2, d0
	lea 0(a0, d0.w), a0
	move.l d1, (a0)
	bra.s ok

passTwo
	bsr.w findSectionByScratchName
	bne.w fail
	bsr.w layoutSectionPlacedPtr
	tst.w (a0)
	beq.s startSection
	bsr.w layoutSectionBasePtr
	move.l (a0), d0
	bsr.w setPlacedSectionOriginWithImageGap
	bne.w fail

startSection
	move.w #1, OpasmLayoutSectionActive
	move.w d5, OpasmLayoutActiveSectionIndex
	jsr eng.opasmEngineGetSessionCurrentPcV1
	bsr.w layoutSectionStartPcPtr
	move.l d0, (a0)

ok
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; processSectionDirectiveForStatement

; Leave the active native section slice and record its pass-one size.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 when no section is active.
; Clobbers: D0-D5/A0/CCR.
; CCR: reflects D0.L on return.
processEndsectionDirectiveForStatement	.block
	movem.l d1-d5/a0, -(sp)
	tst.w OpasmLayoutSectionActive
	beq.w fail
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #1, d0
	bne.s clearActive
	jsr eng.opasmEngineGetSessionCurrentPcV1
	move.l d0, d1
	moveq #0, d5
	move.w OpasmLayoutActiveSectionIndex, d5
	cmpi.w #OPASM_LAYOUT_INDEX_NONE, d5
	beq.w fail
	lea OpasmLayoutSectionStartPcs.l, a0
	bsr.w layoutLongPtr
	sub.l (a0), d1
	lea OpasmLayoutSectionSizes.l, a0
	bsr.w layoutLongPtr
	move.l d1, (a0)

clearActive
	clr.w OpasmLayoutSectionActive
	move.w #OPASM_LAYOUT_INDEX_NONE, OpasmLayoutActiveSectionIndex
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d5/a0
	rts
	.bend  ; processEndsectionDirectiveForStatement

; Place a named native section slice in a named region.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 on missing state or range overflow.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
processPlaceDirectiveForStatement	.block
	movem.l d1-d7/a0-a3, -(sp)
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #1, d0
	bne.w ok
	tst.w OpasmLayoutSectionActive
	bne.w fail
	tst.w OpasmLayoutRegionCount
	beq.w fail
	tst.w OpasmLayoutSectionCount
	beq.w fail
	bsr.w parsePlaceDirectiveNamesForStatement
	bne.w fail
	bsr.w findSectionByPlaceName
	bne.w fail
	move.w d5, OpasmLayoutPlaceSectionIndex
	bsr.w layoutSectionPlacedPtr
	tst.w (a0)
	bne.w fail
	bsr.w findRegionByPlaceName
	bne.w fail
	move.w d5, OpasmLayoutPlaceRegionIndex
	moveq #2, d6
	bsr.w readAlignOptionForStatement
	bne.w fail
	move.l d3, d4
	moveq #0, d5
	move.w OpasmLayoutPlaceRegionIndex, d5
	bsr.w layoutRegionAlignPtr
	cmp.l (a0), d4
	bhs.s haveRegionAlign
	move.l (a0), d4

haveRegionAlign
	moveq #0, d5
	move.w OpasmLayoutPlaceSectionIndex, d5
	bsr.w layoutSectionAlignPtr
	cmp.l (a0), d4
	bhs.s haveSectionAlign
	move.l (a0), d4

haveSectionAlign
	moveq #0, d5
	move.w OpasmLayoutPlaceRegionIndex, d5
	bsr.w layoutRegionCursorPtr
	move.l (a0), d1
	move.l d4, d0
	bsr.w alignLayoutCursor
	bne.w fail
	moveq #0, d5
	move.w OpasmLayoutPlaceSectionIndex, d5
	bsr.w layoutSectionSizePtr
	move.l (a0), d2
	beq.s store
	move.l d1, d0
	add.l d2, d0
	bcs.w fail
	subq.l #1, d0
	moveq #0, d5
	move.w OpasmLayoutPlaceRegionIndex, d5
	bsr.w layoutRegionEndPtr
	cmp.l (a0), d0
	bhi.w fail
	addq.l #1, d0
	moveq #0, d5
	move.w OpasmLayoutPlaceRegionIndex, d5
	bsr.w layoutRegionCursorPtr
	move.l d0, (a0)
	bra.s storeBase

store
	moveq #0, d5
	move.w OpasmLayoutPlaceRegionIndex, d5
	bsr.w layoutRegionCursorPtr
	move.l d1, (a0)

storeBase
	moveq #0, d5
	move.w OpasmLayoutPlaceSectionIndex, d5
	bsr.w layoutSectionBasePtr
	move.l d1, (a0)
	bsr.w layoutSectionPlacedPtr
	move.w #1, (a0)

ok
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; processPlaceDirectiveForStatement

; Set origin for a placed section, preserving first image origin and filling gaps.
; Inputs: D0.L = placed section base.
; Outputs: D0.L = 0 on success, 1 on image capacity failure.
; Clobbers: D0-D4/A0/CCR.
; CCR: reflects D0.L on return.
setPlacedSectionOriginWithImageGap	.block
	movem.l d1-d4/a0, -(sp)
	move.l d0, d4
	jsr eng.opasmEngineGetImageByteCountV1
	tst.l d0
	beq.s firstImageOrigin
	jsr eng.opasmEngineGetSessionCurrentPcV1
	move.l d4, d1
	cmp.l d0, d1
	bls.s setCurrentPc
	sub.l d0, d1
	move.l d1, d0
	moveq #0, d1
	bsr.w appendRepeatedByte
	bne.s fail

setCurrentPc
	move.l d4, d0
	jsr eng.opasmEngineSetCurrentPcV1
	bra.s return

firstImageOrigin
	move.l d4, d0
	jsr eng.opasmEngineSetOriginV1
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d4/a0
	rts
	.bend  ; setPlacedSectionOriginWithImageGap

; Copy a bounded layout name into a table slot.
; Inputs: A0 = source; A1 = destination; D0.W = byte count.
; Outputs: destination bytes plus trailing NUL.
; Clobbers: D0-D1/A0-A1/CCR.
; CCR: reflects D0.W on return.
copyNameBytes	.block
	move.w d0, d1
	beq.s done

loop
	move.b (a0)+, (a1)+
	subq.w #1, d1
	bne.s loop

done
	clr.b (a1)
	rts
	.bend  ; copyNameBytes

; Return the region name buffer pointer for an index.
; Inputs: D5.W = region index.
; Outputs: A1 = name buffer pointer.
; Clobbers: D0/A1/CCR.
; CCR: reflects D0.L after index scaling.
layoutRegionNamePtr	.block
	moveq #0, d0
	move.w d5, d0
	lsl.l #5, d0
	lea OpasmLayoutRegionNames.l, a1
	adda.l d0, a1
	rts
	.bend  ; layoutRegionNamePtr

; Return the section name buffer pointer for an index.
; Inputs: D5.W = section index.
; Outputs: A1 = name buffer pointer.
; Clobbers: D0/A1/CCR.
; CCR: reflects D0.L after index scaling.
layoutSectionNamePtr	.block
	moveq #0, d0
	move.w d5, d0
	lsl.l #5, d0
	lea OpasmLayoutSectionNames.l, a1
	adda.l d0, a1
	rts
	.bend  ; layoutSectionNamePtr

; Return an indexed region name-length pointer.
; Inputs: D5.W = region index.
; Outputs: A0 = field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutRegionNameLenPtr	.block
	lea OpasmLayoutRegionNameLens.l, a0
	bsr.w layoutWordPtr
	rts
	.bend  ; layoutRegionNameLenPtr

; Return an indexed region start pointer.
; Inputs: D5.W = region index.
; Outputs: A0 = field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutRegionStartPtr	.block
	lea OpasmLayoutRegionStarts.l, a0
	bsr.w layoutLongPtr
	rts
	.bend  ; layoutRegionStartPtr

; Return an indexed region end pointer.
; Inputs: D5.W = region index.
; Outputs: A0 = field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutRegionEndPtr	.block
	lea OpasmLayoutRegionEnds.l, a0
	bsr.w layoutLongPtr
	rts
	.bend  ; layoutRegionEndPtr

; Return an indexed region cursor pointer.
; Inputs: D5.W = region index.
; Outputs: A0 = field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutRegionCursorPtr	.block
	lea OpasmLayoutRegionCursors.l, a0
	bsr.w layoutLongPtr
	rts
	.bend  ; layoutRegionCursorPtr

; Return an indexed region alignment pointer.
; Inputs: D5.W = region index.
; Outputs: A0 = field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutRegionAlignPtr	.block
	lea OpasmLayoutRegionAligns.l, a0
	bsr.w layoutLongPtr
	rts
	.bend  ; layoutRegionAlignPtr

; Return an indexed section name-length pointer.
; Inputs: D5.W = section index.
; Outputs: A0 = field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutSectionNameLenPtr	.block
	lea OpasmLayoutSectionNameLens.l, a0
	bsr.w layoutWordPtr
	rts
	.bend  ; layoutSectionNameLenPtr

; Return an indexed section placed-flag pointer.
; Inputs: D5.W = section index.
; Outputs: A0 = field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutSectionPlacedPtr	.block
	lea OpasmLayoutSectionPlacedFlags.l, a0
	bsr.w layoutWordPtr
	rts
	.bend  ; layoutSectionPlacedPtr

; Return an indexed section start-PC pointer.
; Inputs: D5.W = section index.
; Outputs: A0 = field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutSectionStartPcPtr	.block
	lea OpasmLayoutSectionStartPcs.l, a0
	bsr.w layoutLongPtr
	rts
	.bend  ; layoutSectionStartPcPtr

; Return an indexed section size pointer.
; Inputs: D5.W = section index.
; Outputs: A0 = field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutSectionSizePtr	.block
	lea OpasmLayoutSectionSizes.l, a0
	bsr.w layoutLongPtr
	rts
	.bend  ; layoutSectionSizePtr

; Return an indexed section base pointer.
; Inputs: D5.W = section index.
; Outputs: A0 = field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutSectionBasePtr	.block
	lea OpasmLayoutSectionBases.l, a0
	bsr.w layoutLongPtr
	rts
	.bend  ; layoutSectionBasePtr

; Return an indexed section alignment pointer.
; Inputs: D5.W = section index.
; Outputs: A0 = field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutSectionAlignPtr	.block
	lea OpasmLayoutSectionAligns.l, a0
	bsr.w layoutLongPtr
	rts
	.bend  ; layoutSectionAlignPtr

; Index into a word table.
; Inputs: A0 = table base; D5.W = index.
; Outputs: A0 = indexed field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutWordPtr	.block
	moveq #0, d2
	move.w d5, d2
	lsl.l #1, d2
	lea 0(a0, d2.w), a0
	rts
	.bend  ; layoutWordPtr

; Index into a long table.
; Inputs: A0 = table base; D5.W = index.
; Outputs: A0 = indexed field pointer.
; Clobbers: D0/A0/CCR.
; CCR: reflects D0.L after index scaling.
layoutLongPtr	.block
	moveq #0, d2
	move.w d5, d2
	lsl.l #2, d2
	lea 0(a0, d2.w), a0
	rts
	.bend  ; layoutLongPtr

; Find a region by the scratch layout name.
; Outputs: D0.L = 0 on found, 1 when missing; D5.W = index on found.
; Clobbers: D0-D5/A0-A1/CCR.
; CCR: reflects D0.L on return.
findRegionByScratchName	.block
	moveq #0, d5

loop
	lea OpasmLayoutRegionCount.l, a0
	cmp.w (a0), d5
	bhs.s missing
	lea OpasmLayoutRegionNameLens.l, a0
	bsr.w layoutWordPtr
	moveq #0, d0
	move.w (a0), d0
	moveq #0, d1
	move.w OpasmLayoutScratchNameLen, d1
	lea OpasmLayoutRegionNames.l, a1
	moveq #0, d2
	move.w d5, d2
	lsl.l #5, d2
	lea 0(a1, d2.w), a1
	movea.l a1, a0
	lea OpasmLayoutScratchName.l, a1
	bsr.w layoutNamesMatch
	beq.s found
	addq.w #1, d5
	bra.s loop

found
	moveq #0, d0
	rts

missing
	moveq #1, d0
	rts
	.bend  ; findRegionByScratchName

; Find a section by the scratch layout name.
; Outputs: D0.L = 0 on found, 1 when missing; D5.W = index on found.
; Clobbers: D0-D5/A0-A1/CCR.
; CCR: reflects D0.L on return.
findSectionByScratchName	.block
	moveq #0, d5

loop
	lea OpasmLayoutSectionCount.l, a0
	cmp.w (a0), d5
	bhs.s missing
	lea OpasmLayoutSectionNameLens.l, a0
	bsr.w layoutWordPtr
	moveq #0, d0
	move.w (a0), d0
	moveq #0, d1
	move.w OpasmLayoutScratchNameLen, d1
	lea OpasmLayoutSectionNames.l, a1
	moveq #0, d2
	move.w d5, d2
	lsl.l #5, d2
	lea 0(a1, d2.w), a1
	movea.l a1, a0
	lea OpasmLayoutScratchName.l, a1
	bsr.w layoutNamesMatch
	beq.s found
	addq.w #1, d5
	bra.s loop

found
	moveq #0, d0
	rts

missing
	moveq #1, d0
	rts
	.bend  ; findSectionByScratchName

; Find a region by the parsed `.place` region name.
; Outputs: D0.L = 0 on found, 1 when missing; D5.W = index on found.
; Clobbers: D0-D5/A0-A1/CCR.
; CCR: reflects D0.L on return.
findRegionByPlaceName	.block
	moveq #0, d5

loop
	lea OpasmLayoutRegionCount.l, a0
	cmp.w (a0), d5
	bhs.s missing
	lea OpasmLayoutRegionNameLens.l, a0
	bsr.w layoutWordPtr
	moveq #0, d0
	move.w (a0), d0
	moveq #0, d1
	move.w OpasmLayoutPlaceRegionNameLen, d1
	lea OpasmLayoutRegionNames.l, a1
	moveq #0, d2
	move.w d5, d2
	lsl.l #5, d2
	lea 0(a1, d2.w), a1
	movea.l a1, a0
	lea OpasmLayoutPlaceRegionName.l, a1
	bsr.w layoutNamesMatch
	beq.s found
	addq.w #1, d5
	bra.s loop

found
	moveq #0, d0
	rts

missing
	moveq #1, d0
	rts
	.bend  ; findRegionByPlaceName

; Find a section by the parsed `.place` section name.
; Outputs: D0.L = 0 on found, 1 when missing; D5.W = index on found.
; Clobbers: D0-D5/A0-A1/CCR.
; CCR: reflects D0.L on return.
findSectionByPlaceName	.block
	moveq #0, d5

loop
	lea OpasmLayoutSectionCount.l, a0
	cmp.w (a0), d5
	bhs.s missing
	lea OpasmLayoutSectionNameLens.l, a0
	bsr.w layoutWordPtr
	moveq #0, d0
	move.w (a0), d0
	moveq #0, d1
	move.w OpasmLayoutPlaceSectionNameLen, d1
	lea OpasmLayoutSectionNames.l, a1
	moveq #0, d2
	move.w d5, d2
	lsl.l #5, d2
	lea 0(a1, d2.w), a1
	movea.l a1, a0
	lea OpasmLayoutPlaceSectionName.l, a1
	bsr.w layoutNamesMatch
	beq.s found
	addq.w #1, d5
	bra.s loop

found
	moveq #0, d0
	rts

missing
	moveq #1, d0
	rts
	.bend  ; findSectionByPlaceName

; Check whether the scratch region range overlaps any existing region.
; Outputs: D0.L = 0 when clear, 1 on overlap.
; Clobbers: D0-D6/A0/CCR.
; CCR: reflects D0.L on return.
layoutScratchRegionOverlapsExisting	.block
	moveq #0, d6

loop
	lea OpasmLayoutRegionCount.l, a0
	cmp.w (a0), d6
	bhs.s clear
	move.w d6, d5
	lea OpasmLayoutRegionEnds.l, a0
	bsr.w layoutLongPtr
	move.l OpasmLayoutScratchStart, d0
	cmp.l (a0), d0
	bhi.s next
	lea OpasmLayoutRegionStarts.l, a0
	bsr.w layoutLongPtr
	move.l (a0), d0
	cmp.l OpasmLayoutScratchEnd, d0
	bhi.s next
	moveq #1, d0
	rts

next
	addq.w #1, d6
	bra.s loop

clear
	moveq #0, d0
	rts
	.bend  ; layoutScratchRegionOverlapsExisting

; Read a comma-separated directive identifier part into a bounded layout buffer.
; Inputs: D7.W = statement index; D6.W = one-based part; A1 = destination buffer.
; Outputs: D0.L = 0 on success, 1 on missing/over-capacity name; D3.W = copied length.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
readCommaNameForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	move.l a1, OpasmLayoutNameDestPtr
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w fail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	beq.w fail
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2
	moveq #1, d4

partStart
	bsr.w skipPartWhitespace
	movea.l a2, a3
	moveq #0, d5

partScan
	tst.l d2
	beq.s partEnd
	move.b (a2), d0
	cmpi.b #',', d0
	beq.s partEnd
	addq.l #1, a2
	subq.l #1, d2
	addq.l #1, d5
	bra.s partScan

partEnd
	cmp.w d6, d4
	beq.s copyPart
	tst.l d2
	beq.w fail
	addq.l #1, a2
	subq.l #1, d2
	addq.w #1, d4
	bra.s partStart

copyPart
	movea.l a3, a0
	move.l d5, d0
	bsr.w trimPartTrailing
	movea.l OpasmLayoutNameDestPtr, a1
	bsr.w copyNameTokenSlice
	bra.s return

fail
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; readCommaNameForStatement

; Read optional align=<n> for a comma-delimited directive operand.
; Inputs: D7.W = statement index; D6.W = one-based comma part.
; Outputs: D0.L = 0 on success; D3.L = parsed step or 1 by default.
; Clobbers: D0-D2/D4-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
readAlignOptionForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w defaultAlign
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	beq.w defaultAlign
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2
	moveq #1, d4

partStart
	bsr.w skipPartWhitespace
	movea.l a2, a3
	moveq #0, d5

partScan
	tst.l d2
	beq.s partEnd
	move.b (a2), d0
	cmpi.b #',', d0
	beq.s partEnd
	addq.l #1, a2
	subq.l #1, d2
	addq.l #1, d5
	bra.s partScan

partEnd
	cmp.w d6, d4
	beq.s parsePart
	tst.l d2
	beq.s defaultAlign
	addq.l #1, a2
	subq.l #1, d2
	addq.w #1, d4
	bra.s partStart

parsePart
	movea.l a3, a0
	move.l d5, d0
	bsr.w trimPartTrailing
	bsr.w parseAlignOptionValue
	bra.s return

defaultAlign
	moveq #1, d3
	moveq #0, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; readAlignOptionForStatement

; Parse `.place <section> in <region>` names for the current statement.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 on malformed names; place-name globals updated.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
parsePlaceDirectiveNamesForStatement	.block
	movem.l d1-d7/a0-a3, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w fail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	beq.w fail
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2
	lea OpasmLayoutPlaceSectionName.l, a1
	bsr.w copyNameTokenSlice
	bne.w fail
	move.w d3, OpasmLayoutPlaceSectionNameLen
	adda.l d3, a2
	sub.l d3, d2
	movea.l a2, a0
	move.l d2, d0
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2
	lea InText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	beq.w fail
	addq.l #2, a2
	subq.l #2, d2
	movea.l a2, a0
	move.l d2, d0
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2
	lea OpasmLayoutPlaceRegionName.l, a1
	bsr.w copyNameTokenSlice
	bne.w fail
	move.w d3, OpasmLayoutPlaceRegionNameLen
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; parsePlaceDirectiveNamesForStatement

; Copy the first identifier-like token from a text slice.
; Inputs: A0/D0 = source slice; A1 = destination buffer.
; Outputs: D0.L = 0 on success, 1 on empty/over-capacity token; D3.W = length.
; Clobbers: D0-D4/A0-A1/CCR.
; CCR: reflects D0.L on return.
copyNameTokenSlice	.block
	movem.l d1-d2/d4/a0-a1, -(sp)
	bsr.w skipLineWhitespace
	moveq #0, d3
	moveq #OPASM_LAYOUT_NAME_CAPACITY - 1, d4

loop
	tst.l d0
	beq.s finish
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s finish
	cmpi.b #9, d1
	beq.s finish
	cmpi.b #',', d1
	beq.s finish
	tst.w d4
	beq.w fail
	move.b d1, (a1)+
	addq.l #1, a0
	subq.l #1, d0
	addq.w #1, d3
	subq.w #1, d4
	bra.s loop

finish
	tst.w d3
	beq.w fail
	clr.b (a1)
	movem.l (sp)+, d1-d2/d4/a0-a1
	moveq #0, d0
	rts

fail
	movem.l (sp)+, d1-d2/d4/a0-a1
	moveq #1, d0
	rts
	.bend  ; copyNameTokenSlice

; Compare two bounded layout names case-insensitively.
; Inputs: A0/D0 = left pointer/length; A1/D1 = right pointer/length.
; Outputs: D0.L = 0 on match, 1 on mismatch.
; Clobbers: D0-D4/A0-A1/CCR.
; CCR: reflects D0.L on return.
layoutNamesMatch	.block
	movem.l d1-d4/a0-a1, -(sp)
	cmp.l d1, d0
	bne.w fail
	move.l d0, d2
	beq.w fail

loop
	tst.l d2
	beq.s ok
	move.b (a0)+, d3
	move.b (a1)+, d4
	bsr.w lowerD3
	exg d3, d4
	bsr.w lowerD3
	exg d3, d4
	cmp.b d4, d3
	bne.w fail
	subq.l #1, d2
	bra.s loop

ok
	movem.l (sp)+, d1-d4/a0-a1
	moveq #0, d0
	rts

fail
	movem.l (sp)+, d1-d4/a0-a1
	moveq #1, d0
	rts
	.bend  ; layoutNamesMatch

; Parse a comma part as `align=<expr>`, defaulting non-align options to 1.
; Inputs: A0/D0 = comma-part text.
; Outputs: D0.L = 0 on success, 1 on invalid align; D3.L = align value.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
parseAlignOptionValue	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	bsr.w skipLineWhitespace
	bsr.w trimLiteralFallbackTrailing
	cmpi.l #6, d0
	bcs.w defaultAlign
	move.b (a0), d3
	bsr.w lowerD3
	cmpi.b #'a', d3
	bne.w defaultAlign
	move.b 1(a0), d3
	bsr.w lowerD3
	cmpi.b #'l', d3
	bne.w defaultAlign
	move.b 2(a0), d3
	bsr.w lowerD3
	cmpi.b #'i', d3
	bne.w defaultAlign
	move.b 3(a0), d3
	bsr.w lowerD3
	cmpi.b #'g', d3
	bne.w defaultAlign
	move.b 4(a0), d3
	bsr.w lowerD3
	cmpi.b #'n', d3
	bne.w defaultAlign
	cmpi.b #'=', 5(a0)
	bne.w defaultAlign
	addq.l #6, a0
	subq.l #6, d0
	beq.w fail
	move.l a0, OpasmDriverEvalFallbackPtr
	move.l d0, OpasmDriverEvalFallbackLen
	bsr.w parseDirectiveLiteralValue
	beq.s checkPositive
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w prepareEvaluateExpressionRequest
	bne.w fail
	bsr.w prepareEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.dispatchEvaluateExpressionV1
	bne.w fail
	bsr.w readEvaluateExpressionValue

checkPositive
	tst.l d3
	beq.w fail
	moveq #0, d0
	bra.s return

defaultAlign
	moveq #1, d3
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; parseAlignOptionValue

; Align a layout cursor with Rust-compatible positive-integer `align_up`.
; Inputs: D0.L = align; D1.L = cursor.
; Outputs: D0.L = 0 on success, 1 on invalid/overflow; D1.L = aligned cursor.
; Clobbers: D0/D2-D4/CCR.
; CCR: reflects D0.L on return.
alignLayoutCursor	.block
	tst.l d0
	beq.w fail
	cmpi.l #1, d0
	bls.s ok
	move.l d0, d4
	moveq #0, d3
	move.l d1, d2
	divu.l d4, d3:d2
	tst.l d3
	beq.s ok
	move.l d4, d2
	sub.l d3, d2
	move.l d1, d3
	add.l d2, d3
	bcs.w fail
	move.l d3, d1

ok
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; alignLayoutCursor

; Lowercase an ASCII byte when it is in D3.B.
; Inputs: D3.B = byte.
; Outputs: D3.B = lowercase byte for ASCII A-Z, otherwise unchanged.
; Clobbers: D3/CCR.
; CCR: unspecified on return.
lowerD3	.block
	cmpi.b #'A', d3
	bcs.s done
	cmpi.b #'Z', d3
	bhi.s done
	addi.b #32, d3

done
	rts
	.bend  ; lowerD3

; Evaluate a comma-separated directive operand part.
; Inputs: D7.W = statement index; D6.W = one-based operand part number.
; Outputs: D0.L = 0 on success, 1 on parse/evaluation failure; D3.L = value.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
readCommaOperandValueForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w fail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	beq.w fail
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2

	moveq #1, d4

partStart
	bsr.w skipPartWhitespace
	movea.l a2, a3
	moveq #0, d5

partScan
	tst.l d2
	beq.s partEnd
	move.b (a2), d0
	cmpi.b #',', d0
	beq.s partEnd
	addq.l #1, a2
	subq.l #1, d2
	addq.l #1, d5
	bra.s partScan

partEnd
	cmp.w d6, d4
	beq.s evaluatePart
	tst.l d2
	beq.w fail
	addq.l #1, a2
	subq.l #1, d2
	addq.w #1, d4
	bra.s partStart

evaluatePart
	movea.l a3, a0
	move.l d5, d0
	bsr.w trimPartTrailing
	move.l a0, OpasmDriverEvalFallbackPtr
	move.l d0, OpasmDriverEvalFallbackLen
	beq.w fail
	bsr.w parseDirectiveLiteralValue
	beq.w evalPartOk
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	jsr eng.opasmEngineResolveLabelValueV1
	beq.w evalPartOk
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w resolveDottedLabelSuffixValue
	beq.w evalPartOk
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w prepareEvaluateExpressionRequest
	bne.s evalPartFallback
	bsr.w prepareEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.dispatchEvaluateExpressionV1
	bne.s evalPartFallback
	bsr.w readEvaluateExpressionValue
	tst.l d3
	bne.w evalPartOk
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w parseDirectiveLiteralValue
	bne.w fail

evalPartOk
	moveq #0, d0
	bra.s return

evalPartFallback
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w parseDirectiveLiteralValue
	beq.w return
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	bsr.w resolveDottedLabelSuffixValue
	beq.w return

fail
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; readCommaOperandValueForStatement

; Resolve the final segment of an alias-qualified imported symbol token.
; Inputs: A0/D0 = trimmed text slice.
; Outputs: D0.L = 0 on success, 1 on failure; D3.L = resolved value.
; Clobbers: D0-D2/D4/A0-A2/CCR.
; CCR: reflects D0.L on return.
resolveDottedLabelSuffixValue	.block
	movem.l d1-d2/d4/a0-a2, -(sp)
	bsr.w skipLineWhitespace
	bsr.w trimLiteralFallbackTrailing
	tst.l d0
	beq.w fail
	movea.l a0, a1
	movea.l a0, a2
	move.l d0, d2
	moveq #-1, d4

scan
	tst.l d2
	beq.s scanned
	move.b (a2)+, d1
	subq.l #1, d2
	cmpi.b #'.', d1
	bne.s scan
	move.l a2, d4
	bra.s scan

scanned
	cmpi.l #-1, d4
	beq.s fail
	movea.l d4, a0
	move.l a2, d0
	sub.l d4, d0
	beq.s fail
	jsr eng.opasmEngineResolveLabelValueV1
	beq.s ok

fail
	movem.l (sp)+, d1-d2/d4/a0-a2
	moveq #1, d0
	rts

ok
	movem.l (sp)+, d1-d2/d4/a0-a2
	moveq #0, d0
	rts
	.bend  ; resolveDottedLabelSuffixValue

; Compute Rust-compatible `.align` padding for the current native PC.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 on invalid expression/boundary; D3.L = pad.
; Clobbers: D0-D7/A0-A2/CCR.
; CCR: reflects D0.L on return.
readAlignPadForStatement	.block
	movem.l d1-d2/d4-d7/a0-a2, -(sp)
	moveq #2, d5
	bsr.w readOperandValueForStatement
	bne.w fail
	move.l d3, d4
	beq.w fail
	move.l d4, d0
	subq.l #1, d0
	move.l d0, d5
	and.l d4, d0
	bne.w fail
	jsr eng.opasmEngineGetSessionCurrentPcV1
	and.l d5, d0
	beq.s aligned
	move.l d4, d3
	sub.l d0, d3
	bra.s ok

aligned
	clr.l d3

ok
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4-d7/a0-a2
	rts
	.bend  ; readAlignPadForStatement

; Return the byte size of a numeric data directive.
; Inputs: D7.W = statement index; D5.W = unit size (1, 2, or 4).
; Outputs: D0.L = 0 on success, 1 on malformed data list; D3.L = byte size.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
dataDirectiveSizeForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	bsr.w countCommaPartsForStatement
	bne.s fail
	moveq #0, d2
	move.w d5, d2
	mulu.l d2, d3
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; dataDirectiveSizeForStatement

; Emit a numeric data directive in first-run MOS little-endian order.
; Inputs: D7.W = statement index; D5.W = unit size (1, 2, or 4).
; Outputs: D0.L = 0 on success, 1 on malformed data list or image overflow.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
emitDataDirectiveForStatement	.block
	movem.l d1-d7/a0-a3, -(sp)
	move.w d5, d4
	bsr.w countCommaPartsForStatement
	bne.w fail
	move.w d3, d2
	moveq #1, d6

partLoop
	cmp.w d2, d6
	bhi.w ok
	move.w d4, d5
	cmpi.w #1, d4
	bne.s readPart
	moveq #1, d5

readPart
	cmpi.w #1, d2
	bne.s readSplitPart
	bsr.w readOperandValueForStatement
	bra.s havePartValue

readSplitPart
	bsr.w readCommaOperandValueForStatement
havePartValue
	bne.w fail
	cmpi.w #1, d4
	beq.s emitOne
	cmpi.w #2, d4
	beq.s emitTwo
	cmpi.w #4, d4
	beq.s emitFour
	bra.w fail

emitOne
	cmpi.l #$000000FF, d3
	bhi.w fail
	lea OpasmDataScratch.l, a0
	move.b d3, (a0)
	moveq #1, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.w fail
	bra.s nextPart

emitTwo
	lea OpasmDataScratch.l, a0
	move.b d3, (a0)
	move.l d3, d0
	lsr.l #8, d0
	move.b d0, 1(a0)
	moveq #2, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.w fail
	bra.s nextPart

emitFour
	lea OpasmDataScratch.l, a0
	move.b d3, (a0)
	move.l d3, d0
	lsr.l #8, d0
	move.b d0, 1(a0)
	move.l d3, d0
	lsr.l #8, d0
	lsr.l #8, d0
	move.b d0, 2(a0)
	move.l d3, d0
	lsr.l #8, d0
	lsr.l #8, d0
	lsr.l #8, d0
	move.b d0, 3(a0)
	moveq #4, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.w fail

nextPart
	addq.w #1, d6
	bra.w partLoop

ok
	moveq #0, d0
	bra.w return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; emitDataDirectiveForStatement

; Count comma-delimited operands after the directive mnemonic.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 on empty/malformed list; D3.W = part count.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
countCommaPartsForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w fail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	beq.w fail
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2
	moveq #1, d3
	moveq #0, d4

scan
	tst.l d2
	beq.s finish
	move.b (a2)+, d0
	subq.l #1, d2
	cmpi.b #',', d0
	beq.s comma
	cmpi.b #' ', d0
	beq.s scan
	cmpi.b #9, d0
	beq.s scan
	moveq #1, d4
	bra.s scan

comma
	tst.w d4
	beq.s fail
	addq.w #1, d3
	moveq #0, d4
	bra.s scan

finish
	tst.w d4
	beq.s fail
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; countCommaPartsForStatement

; Return the emitted byte size of a text directive.
; Inputs: D7.W = statement index; D5.W = mode (0 .text, 1 .null, 2 .ptext).
; Outputs: D0.L = 0 on success, 1 on malformed text; D3.L = byte size.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
textDirectiveSizeForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	bsr.w parseTextDirectiveForStatement
	bne.s fail
	move.l OpasmTextScratchLen, d3
	cmpi.w #0, d5
	beq.s ok
	cmpi.w #1, d5
	beq.s nullSize
	cmpi.l #255, d3
	bhi.s fail
	addq.l #1, d3
	bra.s ok

nullSize
	bsr.w textScratchContainsZero
	bne.s fail
	addq.l #1, d3

ok
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; textDirectiveSizeForStatement

; Emit a parsed text directive.
; Inputs: D7.W = statement index; D5.W = mode (0 .text, 1 .null, 2 .ptext).
; Outputs: D0.L = 0 on success, 1 on malformed text or image overflow.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
emitTextDirectiveForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	bsr.w parseTextDirectiveForStatement
	bne.s fail
	cmpi.w #0, d5
	beq.s emitTextBytes
	cmpi.w #1, d5
	beq.s emitNullBytes
	move.l OpasmTextScratchLen, d0
	cmpi.l #255, d0
	bhi.s fail
	lea OpasmDataScratch.l, a0
	move.b d0, (a0)
	moveq #1, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.s fail
	bra.s emitTextBytes

emitNullBytes
	bsr.w textScratchContainsZero
	bne.s fail

emitTextBytes
	move.l OpasmTextScratchLen, d0
	beq.s suffix
	lea OpasmTextScratch.l, a0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.s fail

suffix
	cmpi.w #1, d5
	bne.s ok
	lea OpasmDataScratch.l, a0
	clr.b (a0)
	moveq #1, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.s fail

ok
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; emitTextDirectiveForStatement

; Parse quoted text operands into the scratch buffer.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 on malformed text or scratch overflow.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
parseTextDirectiveForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	clr.l OpasmTextScratchLen
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w fail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	beq.w fail
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2

operandStart
	bsr.w skipPartWhitespace
	tst.l d2
	beq.s ok
	cmpi.b #'"', (a2)
	bne.w fail
	addq.l #1, a2
	subq.l #1, d2

charLoop
	tst.l d2
	beq.w fail
	move.b (a2)+, d1
	subq.l #1, d2
	cmpi.b #'"', d1
	beq.s operandEnd
	cmpi.b #92, d1
	bne.s appendChar
	tst.l d2
	beq.w fail
	move.b (a2)+, d1
	subq.l #1, d2
	bsr.w decodeTextEscape

appendChar
	bsr.w appendTextScratchByte
	bne.w fail
	bra.s charLoop

operandEnd
	bsr.w skipPartWhitespace
	tst.l d2
	beq.s ok
	cmpi.b #',', (a2)
	bne.w fail
	addq.l #1, a2
	subq.l #1, d2
	bra.s operandStart

ok
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; parseTextDirectiveForStatement

; Decode a Rust-compatible first-run string escape subset.
; Inputs: D1.B = escaped byte.
; Outputs: D1.B = decoded byte.
; Clobbers: D1/CCR.
decodeTextEscape	.block
	cmpi.b #'0', d1
	bne.s checkN
	moveq #0, d1
	rts

checkN
	cmpi.b #'n', d1
	bne.s checkR
	moveq #10, d1
	rts

checkR
	cmpi.b #'r', d1
	bne.s checkT
	moveq #13, d1
	rts

checkT
	cmpi.b #'t', d1
	bne.s done
	moveq #9, d1

done
	rts
	.bend  ; decodeTextEscape

; Append D1.B to the text scratch buffer.
; Outputs: D0.L = 0 on success, 1 on capacity overflow.
appendTextScratchByte	.block
	movem.l d2/a0, -(sp)
	move.l OpasmTextScratchLen, d2
	cmpi.l #OPASM_TEXT_SCRATCH_CAPACITY, d2
	bhs.s fail
	lea OpasmTextScratch.l, a0
	move.b d1, 0(a0, d2.l)
	addq.l #1, d2
	move.l d2, OpasmTextScratchLen
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d2/a0
	rts
	.bend  ; appendTextScratchByte

; Return 1 when the parsed text scratch contains a zero byte.
; Outputs: D0.L = 0 when clear, 1 when a zero byte is present.
textScratchContainsZero	.block
	movem.l d1-d2/a0, -(sp)
	move.l OpasmTextScratchLen, d2
	lea OpasmTextScratch.l, a0

loop
	tst.l d2
	beq.s clear
	move.b (a0)+, d1
	beq.s found
	subq.l #1, d2
	bra.s loop

clear
	moveq #0, d0
	bra.s return

found
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/a0
	rts
	.bend  ; textScratchContainsZero

skipPartWhitespace	.block
loop
	tst.l d2
	beq.s done
	move.b (a2), d0
	cmpi.b #' ', d0
	beq.s skip
	cmpi.b #9, d0
	beq.s skip
	bra.s done

skip
	addq.l #1, a2
	subq.l #1, d2
	bra.s loop

done
	rts
	.bend  ; skipPartWhitespace

skipSourceHeadToken	.block
loop
	tst.l d0
	beq.s done
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s done
	cmpi.b #9, d1
	beq.s done
	addq.l #1, a0
	subq.l #1, d0
	bra.s loop

done
	rts
	.bend  ; skipSourceHeadToken

trimPartTrailing	.block
	tst.l d0
	beq.s done
	movea.l a0, a1
	adda.l d0, a1

loop
	tst.l d0
	beq.s done
	move.b -(a1), d1
	cmpi.b #' ', d1
	beq.s trim
	cmpi.b #9, d1
	beq.s trim
	bra.s done

trim
	subq.l #1, d0
	bra.s loop

done
	rts
	.bend  ; trimPartTrailing

; Parse a simple directive literal used as a fallback after package eval.
; Inputs: A0/D0 = text slice (`$hex` or decimal).
; Outputs: D0.L = 0 on success, 1 on parse failure; D3.L = value.
; Clobbers: D0-D4/A0-A1/CCR.
; CCR: reflects D0.L on return.
parseDirectiveLiteralValue	.block
	movem.l d1-d2/d4/a0-a1, -(sp)
	bsr.w skipLineWhitespace
	bsr.w trimLiteralFallbackTrailing
	tst.l d0
	beq.w fail
	clr.l d3
	move.l d0, d4
	move.b (a0), d1
	cmpi.b #'$', d1
	beq.s hexPrefix
	movea.l a0, a1
	adda.l d4, a1
	move.b -1(a1), d1
	ori.b #32, d1
	cmpi.b #'h', d1
	bne.s decimalLoop
	subq.l #1, d4
	beq.w fail
	bra.s hexLoop

hexPrefix
	addq.l #1, a0
	subq.l #1, d4
	beq.w fail

hexLoop
	tst.l d4
	beq.s ok
	moveq #0, d1
	move.b (a0)+, d1
	bsr.w hexNibbleValue
	bmi.s fail
	lsl.l #4, d3
	or.l d1, d3
	subq.l #1, d4
	bra.s hexLoop

decimalLoop
	tst.l d4
	beq.s ok
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #'0', d1
	blo.w fail
	cmpi.b #'9', d1
	bhi.w fail
	subi.b #'0', d1
	move.l d3, d2
	lsl.l #3, d3
	add.l d2, d3
	add.l d2, d3
	add.l d1, d3
	subq.l #1, d4
	bra.s decimalLoop

ok
	movem.l (sp)+, d1-d2/d4/a0-a1
	moveq #0, d0
	rts

fail
	movem.l (sp)+, d1-d2/d4/a0-a1
	moveq #1, d0
	rts
	.bend  ; parseDirectiveLiteralValue

trimLiteralFallbackTrailing	.block
	tst.l d0
	beq.s done
	movea.l a0, a1
	adda.l d0, a1

loop
	tst.l d0
	beq.s done
	move.b -(a1), d1
	cmpi.b #' ', d1
	beq.s trim
	cmpi.b #9, d1
	beq.s trim
	bra.s done

trim
	subq.l #1, d0
	bra.s loop

done
	rts
	.bend  ; trimLiteralFallbackTrailing

hexNibbleValue	.block
	cmpi.b #'0', d1
	blo.w fail
	cmpi.b #'9', d1
	bls.s decimal
	cmpi.b #'A', d1
	blo.s lower
	cmpi.b #'F', d1
	bls.s upper

lower
	cmpi.b #'a', d1
	blo.w fail
	cmpi.b #'f', d1
	bhi.w fail
	subi.b #'a' - 10, d1
	moveq #0, d0
	rts

upper
	subi.b #'A' - 10, d1
	moveq #0, d0
	rts

decimal
	subi.b #'0', d1
	moveq #0, d0
	rts

fail
	moveq #-1, d0
	rts
	.bend  ; hexNibbleValue

; Append one byte value repeatedly to the current image stream.
; Inputs: D0.L = count; D1.B = byte value.
; Outputs: D0.L = 0 on success, 1 on image capacity failure.
; Clobbers: D0-D3/A0/CCR.
; CCR: reflects D0.L on return.
appendRepeatedByte	.block
	movem.l d1-d3/a0, -(sp)
	move.l d0, d2
	move.b d1, d3
	subq.l #2, sp
	move.b d3, (sp)

loop
	tst.l d2
	beq.s ok
	movea.l sp, a0
	moveq #1, d0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.w fail
	subq.l #1, d2
	bra.s loop

ok
	addq.l #2, sp
	movem.l (sp)+, d1-d3/a0
	moveq #0, d0
	rts

fail
	addq.l #2, sp
	movem.l (sp)+, d1-d3/a0
	moveq #1, d0
	rts
	.bend  ; appendRepeatedByte

; Inputs:
;   D6.W = statement index
; Outputs:
;   D0.L = 0 on success, 1 when selector request preparation fails
;   OpasmDriverEvalRequestLen updated from D1 on success
; Clobbers:
;   D0-D1/D6/A0-A1/CCR
; CCR:
;   Reflects D0.L on return
prepareEncodeSelectedRequestForStatement	.block
	movem.l d1/d6/a1, -(sp)
	clr.w OpasmDriverEvalRequestLen
	moveq #0, d0
	move.w d6, d0
	bsr.w serviceIoBufferPtr
	movea.l a0, a1
	jsr eng.prepareSelectedEvaluateRequestV1
	bne.s return
	move.w d1, OpasmDriverEvalRequestLen
	tst.l d0

return
	movem.l (sp)+, d1/d6/a1
	rts
	.bend  ; prepareEncodeSelectedRequestForStatement

; Inputs:
;   A0 = expression text pointer
;   D0.L = expression text length
;   D7.W = statement index
; Outputs:
;   D0.L = 0 on success, 1 when expression request preparation fails
;   OpasmDriverEvalRequestLen updated from D1 on success
; Clobbers:
;   D0-D1/D7/A0-A2/CCR
; CCR:
;   Reflects D0.L on return
prepareEvaluateExpressionRequest	.block
	movem.l d1/a0-a2, -(sp)
	movea.l a0, a2
	clr.w OpasmDriverEvalRequestLen
	bsr.w serviceIoBufferPtr
	movea.l a0, a1
	movea.l a2, a0
	move.w d7, d1
	jsr eng.prepareEvaluateExpressionRequestV1
	bne.s return
	move.w d1, OpasmDriverEvalRequestLen
	tst.l d0

return
	movem.l (sp)+, d1/a0-a2
	rts
	.bend  ; prepareEvaluateExpressionRequest

prepareEvaluateExpressionExtension	.block
	movem.l d0-d1/a0-a2, -(sp)
	bsr.w serviceIoBufferPtr
	moveq #0, d0
	move.w OpasmDriverEvalRequestLen, d0
	movea.l a0, a2
	bsr.w serviceEvalExtensionPtr
	movea.l a0, a1
	movea.l a2, a0
	jsr eng.prepareEvaluateExpressionExtensionV1
	movem.l (sp)+, d0-d1/a0-a2
	rts
	.bend  ; prepareEvaluateExpressionExtension

prepareDirectiveEvaluateExpressionExtension	.block
	movem.l a0, -(sp)
	bsr.w serviceEvalExtensionPtr
	jsr eng.prepareDirectiveEvaluateExpressionExtensionV1
	movem.l (sp)+, a0
	rts
	.bend  ; prepareDirectiveEvaluateExpressionExtension

readEvaluateExpressionValue	.block
	movem.l d0-d1/a0, -(sp)
	bsr.w serviceEvalExtensionPtr
	move.l 16(a0), d3
	bne.s return
	bsr.w serviceFramePtr
	jsr tkpkg.readOutputLenV1
	move.w d0, d1
	beq.s return
	move.l d1, -(sp)
	bsr.w serviceFramePtr
	jsr tkpkg.readOutputPtrV1
	move.l (sp)+, d0
	bsr.w parseEvaluateExpressionOutputValue

return
	movem.l (sp)+, d0-d1/a0
	rts
	.bend  ; readEvaluateExpressionValue

; Parse a tkpkg evaluate-expression textual result of the form `VALUE <signed-decimal>`.
; Inputs: A0 = output text pointer; D0.W = byte length.
; Outputs: D0.L = 0 on success, 1 on malformed text; D3.L = parsed value on success.
; Clobbers: D0-D3/A0/CCR.
; CCR: reflects D0.L on return.
parseEvaluateExpressionOutputValue	.block
	movem.l d1-d2/a0, -(sp)
	moveq #6, d1
	cmp.w d1, d0
	blo.s fail
	cmpi.b #'V', (a0)+
	bne.s fail
	cmpi.b #'A', (a0)+
	bne.s fail
	cmpi.b #'L', (a0)+
	bne.s fail
	cmpi.b #'U', (a0)+
	bne.s fail
	cmpi.b #'E', (a0)+
	bne.s fail
	cmpi.b #' ', (a0)+
	bne.s fail
	subq.w #6, d0
	clr.l d3
	clr.l d2
	tst.w d0
	beq.s fail
	cmpi.b #'-', (a0)
	bne.s digits
	moveq #1, d2
	addq.l #1, a0
	subq.w #1, d0
	beq.s fail

digits
	moveq #0, d1

digitLoop
	tst.w d0
	beq.s done
	move.b (a0)+, d1
	subi.b #'0', d1
	bcs.s fail
	cmpi.b #9, d1
	bhi.s fail
	move.l d3, -(sp)
	lsl.l #3, d3
	move.l (sp)+, d1
	add.l d1, d3
	add.l d1, d3
	moveq #0, d1
	move.b -1(a0), d1
	subi.b #'0', d1
	add.l d1, d3
	subq.w #1, d0
	bra.s digitLoop

done
	tst.l d2
	beq.s ok
	neg.l d3

ok
	moveq #0, d0
	movem.l (sp)+, d1-d2/a0
	rts

fail
	moveq #1, d0
	movem.l (sp)+, d1-d2/a0
	rts
	.bend  ; parseEvaluateExpressionOutputValue

serviceFramePtr	.block
	movea.l OpasmActiveAssembleReqPtr, a0
	movea.l abi.OPASM_ASSEMBLE_REQ_SERVICE_FRAME_PTR(a0), a0
	rts
	.bend  ; serviceFramePtr

serviceIoBufferPtr	.block
	bsr.w serviceFramePtr
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a0), a0
	rts
	.bend  ; serviceIoBufferPtr

serviceEvalExtensionPtr	.block
	bsr.w serviceFramePtr
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a0
	rts
	.bend  ; serviceEvalExtensionPtr

emitSelectorFailureEvent	.block
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	lea DriverSelectorUnknownRawText, a1
	bsr.w tokenEquals
	bne.s unknownMnemonic
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	lea DriverSelectorUnsupportedRawText, a1
	bsr.w tokenEquals
	bne.s unsupportedAddressing
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	lea DriverSelectorOperandRawText, a1
	bsr.w tokenEquals
	bne.s operandError
	bsr.w serviceFramePtr
	jsr tkpkg.readLastErrorPtrV1
	lea DriverSelectedOperandCompileRawText, a1
	bsr.w tokenEquals
	bne.s operandError
	moveq #0, d0
	rts

unknownMnemonic
	moveq #abi.OPASM_EVENT_UNKNOWN_MNEMONIC, d0
	bsr.w appendKindEvent
	moveq #1, d0
	rts

unsupportedAddressing
	moveq #abi.OPASM_EVENT_UNSUPPORTED_ADDRESSING, d0
	bsr.w appendKindEvent
	moveq #1, d0
	rts

operandError
	moveq #abi.OPASM_EVENT_UNRESOLVED_LABEL, d0
	bsr.w appendKindEvent
	moveq #1, d0
	rts
	.bend  ; emitSelectorFailureEvent

appendKindEvent	.block
	movem.l d1/a0-a2, -(sp)
	suba.l #abi.OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w clearEventFrame
	move.w d0, abi.OPASM_EVENT_KIND(a0)
	movea.l a0, a2
	bsr.w appendEventFrame
	adda.l #abi.OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d1/a0-a2
	rts
	.bend  ; appendKindEvent

appendPassEvent	.block
	movem.l d2/a0-a2, -(sp)
	suba.l #abi.OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w clearEventFrame
	move.w d0, abi.OPASM_EVENT_KIND(a0)
	move.w d1, abi.OPASM_EVENT_PASS(a0)
	movea.l a0, a2
	bsr.w appendEventFrame
	adda.l #abi.OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d2/a0-a2
	rts
	.bend  ; appendPassEvent

appendTextEvent	.block
	movem.l d2/a0-a2, -(sp)
	movea.l a0, a1
	suba.l #abi.OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w clearEventFrame
	move.w d0, abi.OPASM_EVENT_KIND(a0)
	move.l a1, abi.OPASM_EVENT_TEXT_PTR(a0)
	move.w d1, abi.OPASM_EVENT_TEXT_LEN(a0)
	movea.l a0, a2
	bsr.w appendEventFrame
	adda.l #abi.OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d2/a0-a2
	rts
	.bend  ; appendTextEvent

appendTextValueEvent	.block
	movem.l d3/a0-a2, -(sp)
	movea.l a0, a1
	suba.l #abi.OPASM_EVENT_BYTES, sp
	movea.l sp, a0
	bsr.w clearEventFrame
	move.w d0, abi.OPASM_EVENT_KIND(a0)
	move.l a1, abi.OPASM_EVENT_TEXT_PTR(a0)
	move.w d1, abi.OPASM_EVENT_TEXT_LEN(a0)
	move.l d2, abi.OPASM_EVENT_VALUE(a0)
	movea.l a0, a2
	bsr.w appendEventFrame
	adda.l #abi.OPASM_EVENT_BYTES, sp
	movem.l (sp)+, d3/a0-a2
	rts
	.bend  ; appendTextValueEvent

appendEventFrame	.block
	movem.l d0-d1/a0-a1/a3, -(sp)
	movea.l OpasmActiveAssembleReqPtr, a3
	move.l a3, d0
	beq.s ok
	tst.l abi.OPASM_ASSEMBLE_REQ_EVENT_BUFFER_PTR(a3)
	beq.s ok
	tst.l abi.OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a3)
	beq.s ok
	move.w abi.OPASM_ASSEMBLE_REQ_EVENT_CAPACITY(a3), d0
	beq.s ok
	movea.l abi.OPASM_ASSEMBLE_REQ_EVENT_BUFFER_PTR(a3), a0
	movea.l abi.OPASM_ASSEMBLE_REQ_EVENT_COUNT_PTR(a3), a1
	jsr events.appendV1

ok
	movem.l (sp)+, d0-d1/a0-a1/a3
	rts
	.bend  ; appendEventFrame

clearEventFrame	.block
	movem.l d0-d1/a0, -(sp)
	moveq #abi.OPASM_EVENT_BYTES - 1, d1

loop
	clr.b (a0)+
	dbf d1, loop
	movem.l (sp)+, d0-d1/a0
	rts
	.bend  ; clearEventFrame

tokenLen	.block
	movem.l d1/a0, -(sp)
	moveq #0, d0

loop
	move.b (a0)+, d1
	beq.s done
	addq.w #1, d0
	bra.s loop

done
	movem.l (sp)+, d1/a0
	rts
	.bend  ; tokenLen

tokenEquals	.block
	movem.l d1-d2/a0-a1, -(sp)

loop
	move.b (a0)+, d1
	move.b (a1)+, d2
	cmp.b d2, d1
	bne.s no
	tst.b d1
	beq.s yes
	bra.s loop

yes
	movem.l (sp)+, d1-d2/a0-a1
	moveq #1, d0
	rts

no
	movem.l (sp)+, d1-d2/a0-a1
	moveq #0, d0
	rts
	.bend  ; tokenEquals

lineStartsWith	.block
	movem.l d2-d4/a0-a3, -(sp)
	tst.l d0
	beq.s no
	cmpi.b #'.', (a0)
	bne.s compareStart
	addq.l #1, a0
	subq.l #1, d0

compareStart
	cmp.l d1, d0
	bcs.s no
	movea.l a0, a2
	movea.l a1, a3
	move.l d1, d2
	beq.s boundary
	subq.l #1, d2

loop
	move.b (a2)+, d3
	move.b (a3)+, d4
	cmpi.b #'A', d3
	bcs.s compare
	cmpi.b #'Z', d3
	bhi.s compare
	addi.b #32, d3

compare
	cmp.b d4, d3
	bne.s no
	dbra d2, loop

boundary
	cmp.l d1, d0
	beq.s yes
	move.b 0(a0, d1.l), d3
	cmpi.b #' ', d3
	beq.s yes
	cmpi.b #9, d3
	beq.s yes
	cmpi.b #';', d3
	beq.s yes
	moveq #0, d0
	movem.l (sp)+, d2-d4/a0-a3
	rts

yes
	movem.l (sp)+, d2-d4/a0-a3
	moveq #1, d0
	rts

no
	movem.l (sp)+, d2-d4/a0-a3
	moveq #0, d0
	rts
	.bend  ; lineStartsWith

; Inputs:
; - A0/D0: source-line pointer and remaining length.
;
; Outputs:
; - A0/D0: advanced pointer and remaining length after skipping leading spaces/tabs.
;
; Clobbers:
; - D0-D1/A0/CCR
;
; CCR:
; - Reflects D0 on return.
skipLineWhitespace	.block
loop
	tst.l d0
	beq.s done
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s skip
	cmpi.b #9, d1
	beq.s skip
	bra.s done

skip
	addq.l #1, a0
	subq.l #1, d0
	bra.s loop

done
	tst.l d0
	rts
	.bend  ; skipLineWhitespace

	.endsection

	.section data, kind=data

OrgMnemonicText
	.byte "org", 0

CpuMnemonicText
	.byte "cpu", 0

ConstMnemonicText
	.byte "const", 0

VarMnemonicText
	.byte "var", 0

SetMnemonicText
	.byte "set", 0

EndMnemonicText
	.byte "end", 0

RegionMnemonicText
	.byte "region", 0

SectionMnemonicText
	.byte "section", 0

EndsectionMnemonicText
	.byte "endsection", 0

PlaceMnemonicText
	.byte "place", 0

InText
	.byte "in", 0

AlignMnemonicText
	.byte "align", 0

DsMnemonicText
	.byte "ds", 0

ResMnemonicText
	.byte "res", 0

FillMnemonicText
	.byte "fill", 0

ByteMnemonicText
	.byte "byte", 0

DbMnemonicText
	.byte "db", 0

WordMnemonicText
	.byte "word", 0

DwMnemonicText
	.byte "dw", 0

LongMnemonicText
	.byte "long", 0

TextMnemonicText
	.byte "text", 0

NullMnemonicText
	.byte "null", 0

PtextMnemonicText
	.byte "ptext", 0

ForMnemonicText
	.byte "for", 0

EndforMnemonicText
	.byte "endfor", 0

WhileMnemonicText
	.byte "while", 0

EndwhileMnemonicText
	.byte "endwhile", 0

IfMnemonicText
	.byte "if", 0

ElseifMnemonicText
	.byte "elseif", 0

ElseMnemonicText
	.byte "else", 0

EndifMnemonicText
	.byte "endif", 0

MatchMnemonicText
	.byte "match", 0

CaseMnemonicText
	.byte "case", 0

DefaultMnemonicText
	.byte "default", 0

EndmatchMnemonicText
	.byte "endmatch", 0

BlockMnemonicText
	.byte "block", 0

EndblockMnemonicText
	.byte "endblock", 0

BendMnemonicText
	.byte "bend", 0

NamespaceMnemonicText
	.byte "namespace", 0

EndnamespaceMnemonicText
	.byte "endnamespace", 0

EndnMnemonicText
	.byte "endn", 0

DriverSelectorUnknownRawText
	.byte "OTR901: selector unknown mnemonic", 0

DriverSelectorUnsupportedRawText
	.byte "OTR901: selector unsupported address", 0

DriverSelectorOperandRawText
	.byte "OTR901: selector operand error", 0

DriverSelectedOperandCompileRawText
	.byte "OTR901: selected operand compile failed", 0

	.endsection

	.section bss, kind=bss

OpasmActiveAssembleReqPtr
	.res long, 1

OpasmDriverEvalRequestLen
	.res word, 1

OpasmDriverEvalFallbackPtr
	.res long, 1

OpasmDriverEvalFallbackLen
	.res long, 1

OpasmDriverForceStoredOperand
	.res word, 1

OpasmDriverWhileReevaluation
	.res word, 1

OpasmDataScratch
	.res byte, 4

OpasmTextScratchLen
	.res long, 1

OpasmTextScratch
	.res byte, OPASM_TEXT_SCRATCH_CAPACITY

OpasmRepeatDepth
	.res word, 1

OpasmRepeatBodyStart
	.res word, OPASM_REPEAT_STACK_CAPACITY

OpasmRepeatOpening
	.res word, OPASM_REPEAT_STACK_CAPACITY

OpasmRepeatRemaining
	.res long, OPASM_REPEAT_STACK_CAPACITY

OpasmRepeatValuePtr
	.res long, OPASM_REPEAT_STACK_CAPACITY

OpasmRepeatCurrentValue
	.res long, OPASM_REPEAT_STACK_CAPACITY

OpasmRepeatStep
	.res long, OPASM_REPEAT_STACK_CAPACITY

OpasmRepeatHasBinding
	.res byte, OPASM_REPEAT_STACK_CAPACITY

OpasmRepeatKind
	.res byte, OPASM_REPEAT_STACK_CAPACITY

OpasmMatchValue
	.res long, 1

OpasmMatchDefaultIndex
	.res word, 1

OpasmIfDepth
	.res word, 1

OpasmIfMatched
	.res byte, OPASM_REPEAT_STACK_CAPACITY

OpasmLayoutNameDestPtr
	.res long, 1

OpasmLayoutRegionCount
	.res word, 1

OpasmLayoutSectionCount
	.res word, 1

OpasmLayoutSectionActive
	.res word, 1

OpasmLayoutActiveSectionIndex
	.res word, 1

OpasmLayoutPlaceSectionIndex
	.res word, 1

OpasmLayoutPlaceRegionIndex
	.res word, 1

OpasmLayoutScratchStart
	.res long, 1

OpasmLayoutScratchEnd
	.res long, 1

OpasmLayoutScratchNameLen
	.res word, 1

OpasmLayoutScratchName
	.res byte, OPASM_LAYOUT_NAME_CAPACITY

OpasmLayoutRegionNameLens
	.res word, OPASM_LAYOUT_REGION_CAPACITY

OpasmLayoutRegionNames
	.res byte, OPASM_LAYOUT_REGION_CAPACITY * OPASM_LAYOUT_NAME_CAPACITY

OpasmLayoutRegionStarts
	.res long, OPASM_LAYOUT_REGION_CAPACITY

OpasmLayoutRegionEnds
	.res long, OPASM_LAYOUT_REGION_CAPACITY

OpasmLayoutRegionCursors
	.res long, OPASM_LAYOUT_REGION_CAPACITY

OpasmLayoutRegionAligns
	.res long, OPASM_LAYOUT_REGION_CAPACITY

OpasmLayoutSectionNameLens
	.res word, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionNames
	.res byte, OPASM_LAYOUT_SECTION_CAPACITY * OPASM_LAYOUT_NAME_CAPACITY

OpasmLayoutSectionPlacedFlags
	.res word, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionStartPcs
	.res long, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionSizes
	.res long, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionBases
	.res long, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutSectionAligns
	.res long, OPASM_LAYOUT_SECTION_CAPACITY

OpasmLayoutPlaceSectionNameLen
	.res word, 1

OpasmLayoutPlaceSectionName
	.res byte, OPASM_LAYOUT_NAME_CAPACITY

OpasmLayoutPlaceRegionNameLen
	.res word, 1

OpasmLayoutPlaceRegionName
	.res byte, OPASM_LAYOUT_NAME_CAPACITY

	.endsection
	.endmodule
