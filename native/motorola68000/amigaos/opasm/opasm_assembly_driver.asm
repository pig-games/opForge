; Native opasm assembly-session driver.

	.module opasm.amigaos.assembly_driver
	.cpu 68020

	.use opasm.amigaos.callback_abi as abi
	.use opasm.amigaos.compile_values as compile_values
	.use opasm.amigaos.directive_router as directives
	.use opasm.amigaos.directive_data as data_directives
	.use opasm.amigaos.directive_text as text_directives
	.use opasm.amigaos.engine as eng
	.use opasm.amigaos.events
	.use opasm.amigaos.flow_conditionals as conditionals
	.use opasm.amigaos.flow_navigation as navigation
	.use opasm.amigaos.flow_repetition as repetition
	.use opasm.amigaos.flow_scopes as scopes
	.use opasm.amigaos.flow_structs as structs
	.use opasm.amigaos.flow_text_encoding as text_encoding
	.use opasm.amigaos.layout as layout
	.use opasm.amigaos.operand_eval as operand_eval
	.use opasm.amigaos.tkpkg_bridge as tkpkg
.ifdef OPFORGE_DEBUG_CONTRACTS
	.use opforge.debug.contracts as debug_contracts
	.use opforge.debug.events as debug_events
	.include "debug_macros.i"
.endif

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
	movem.l a4-a5, -(sp)
	moveq #abi.OPASM_EVENT_PASS_BEGIN, d0
	moveq #1, d1
	bsr.w appendPassEvent
	jsr eng.opasmEngineBeginPassOneV1
	clr.w OpasmDriverImageBaseSeen
	jsr layout.resetStateV1
	movea.l OpasmActiveAssembleReqPtr, a0
	movea.l abi.OPASM_ASSEMBLE_REQ_LAYOUT_INIT_CB(a0), a0
	move.l a0, d0
	beq.s layoutReady
	jsr (a0)
	tst.l d0
	bne.s layoutInitFail
layoutReady
	clr.w OpasmRepeatDepth
	jsr scopes.resetStateV1
	jsr structs.resetStateV1
	jsr text_encoding.resetStateV1
	jsr compile_values.resetBindingsV1
	movem.l (sp)+, a4-a5
	moveq #0, d0
	rts
layoutInitFail
	movem.l (sp)+, a4-a5
	moveq #1, d0
	rts
	.bend  ; opasmDriverPassOneBegin

opasmDriverPassOneOk	.block
	movem.l a4-a5, -(sp)
	moveq #abi.OPASM_EVENT_PASS_OK, d0
	moveq #1, d1
	bsr.w appendPassEvent
	movem.l (sp)+, a4-a5
	moveq #0, d0
	rts
	.bend  ; opasmDriverPassOneOk

opasmDriverPassTwoBegin	.block
	movem.l a4-a5, -(sp)
	moveq #abi.OPASM_EVENT_PASS_BEGIN, d0
	moveq #2, d1
	bsr.w appendPassEvent
	jsr eng.opasmEngineBeginPassTwoV1
	jsr layout.finalizeReachableSectionMapsV1
	bne.s layoutFail
	bsr.w finalizeImageOriginForPassTwo
	bne.s layoutFail
	bsr.w rebasePlacedLabelsForPassTwo
	bne.s rebaseFail
	jsr layout.beginPassTwoV1
	clr.w OpasmRepeatDepth
	jsr scopes.resetStateV1
	jsr structs.resetStateV1
	jsr text_encoding.resetStateV1
	movem.l (sp)+, a4-a5
	moveq #0, d0
	rts
rebaseFail
	moveq #1, d2
	bra.w fail
layoutFail
	cmpi.w #31, d0
	beq.s layoutMapEmpty
	cmpi.w #32, d0
	blo.s layoutResolveChecks
	cmpi.w #35, d0
	bhi.s layoutResolveChecks
layoutMapRangeFailure
	lea LayoutMapRangeFailureText, a0
	bra.s layoutResolveText
layoutResolveChecks
	cmpi.w #70, d0
	beq.s layoutResolveFailure
	cmpi.w #71, d0
	beq.s layoutResolveMismatch
	move.l d0, d2
	bra.s fail
layoutMapEmpty
	lea LayoutMapEmptyFailureText, a0
	bra.s layoutResolveText
layoutResolveFailure
	lea LayoutResolveFailureText, a0
	bra.s layoutResolveText
layoutResolveMismatch
	lea LayoutResolveMismatchText, a0
layoutResolveText
	bsr.w tokenLen
	move.l d0, d1
	moveq #abi.OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w appendTextEvent
	moveq #0, d2
	bra.s failDone
fail
	suba.l a0, a0
	moveq #0, d1
	moveq #abi.OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w appendTextValueEvent
failDone
	movem.l (sp)+, a4-a5
	moveq #1, d0
	rts
	.bend  ; opasmDriverPassTwoBegin

; Fold finalized placed concrete non-BSS sections into the flat image origin.
; A default zero PC is not an emitted address. Explicit origins or bytes
; outside sections keep their retained lower base.
; @opforge-owner: opasm.amigaos.image
; @opforge-slice: documentation/plans/slices/native-porting-slice-overlapping-origin-image.toml
; @opforge-role: delegation
; Outputs: D0.L = 0.
; Clobbers: D0-D7/A0/CCR.
finalizeImageOriginForPassTwo	.block
	moveq #0, d7
	moveq #0, d4
	jsr layout.getSectionCountV1
	move.w d0, d6
	moveq #0, d5
sectionLoop
	cmp.w d6, d5
	bhs.s haveMinimum
	jsr layout.getPlacedSectionImageOriginCandidateV1
	tst.l d1
	beq.s next
	tst.w d7
	beq.s storeMinimum
	cmp.l d4, d0
	bhs.s next
storeMinimum
	move.l d0, d4
	moveq #1, d7
next
	addq.w #1, d5
	bra.s sectionLoop

haveMinimum
	tst.w d7
	beq.s success
	tst.w OpasmDriverImageBaseSeen
	beq.s setMinimum
	jsr eng.opasmEngineGetSessionOriginV1
	cmp.l d0, d4
	bhs.s success
setMinimum
	move.l d4, d0
	jsr eng.opasmEngineSetImageOriginV1
success
	moveq #0, d0
	rts
	.bend  ; finalizeImageOriginForPassTwo

; Rebase every PC-backed pass-one label through the finalized section layout
; before pass-two expression evaluation begins. This includes forward
; references whose defining statement has not yet been revisited in pass two.
; @opforge-owner: opasm.amigaos.layout
; @opforge-slice: documentation/plans/slices/native-porting-slice-source-artifact-output.toml
; @opforge-role: delegation
rebasePlacedLabelsForPassTwo	.block
	movem.l d2-d5, -(sp)
	jsr eng.opasmEngineGetLabelCountV1
	move.l d0, d5
	moveq #0, d4
labelLoop
	cmp.l d5, d4
	bhs.s success
	move.l d4, d0
	jsr layout.labelIsMappedV1
	tst.l d0
	bne.s next
	move.l d4, d0
	jsr eng.opasmEngineGetPcBackedLabelValueV1
	tst.l d1
	beq.s next
	jsr layout.translatePassOneAddressV1
	tst.l d1
	beq.s next
	move.l d0, d3
	move.l d4, d0
	move.l d3, d1
	jsr eng.opasmEngineSetPcBackedLabelValueV1
	bne.s fail
next
	addq.l #1, d4
	bra.s labelLoop
success
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d2-d5
	rts
	.bend  ; rebasePlacedLabelsForPassTwo

; Apply counted-repetition control before one statement reaches pass logic.
; Inputs: D0.W = current statement index.
; Outputs: D0 = status; D1 = 0 to process or 1 to skip; D2.W = next index when skipped.
; Clobbers: D0-D2/CCR.
; CCR: reflects D0 on return.
opasmDriverApplyFlowControl	.block
	movem.l d3-d7/a0-a2/a4-a5, -(sp)
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
	bne.s checkModule
compareEndn
	moveq #0, d0
	move.w d4, d0
	lea EndnMnemonicText, a1
	moveq #4, d1
	bsr.w lineStartsWith
	beq.s checkModule
	bsr.w scopes.endScopeDirectiveV1
	bne.w fail
	bra.w success

; A module owns an independent local symbol root so identical labels in
; separate modules remain distinct in both passes.
checkModule
	cmpi.l #6, d4
	beq.s compareModule
	cmpi.l #7, d4
	bne.s checkEndmodule
compareModule
	moveq #0, d0
	move.w d4, d0
	lea ModuleMnemonicText, a1
	moveq #6, d1
	bsr.w lineStartsWith
	beq.s checkEndmodule
	bsr.w scopes.beginModuleScopeV1
	bne.w fail
	bra.w success

checkEndmodule
	cmpi.l #9, d4
	beq.s compareEndmodule
	cmpi.l #10, d4
	bne.s checkConditional
compareEndmodule
	moveq #0, d0
	move.w d4, d0
	lea EndmoduleMnemonicText, a1
	moveq #9, d1
	bsr.w lineStartsWith
	beq.s checkConditional
	bsr.w scopes.endModuleScopeV1
	bne.w fail
	bra.w success

checkConditional
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	move.l d4, d0
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a1
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d1
	jsr text_encoding.routeDirectiveV1
	bne.w fail
	tst.w d3
	bne.w skipTextEncodingDirective
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
	move.l d4, d0
	jsr structs.routeDirectiveV1
	bne.w fail
	tst.w d3
	beq.w checkConditionalDirective
	cmpi.w #1, d3
	beq.w beginStructDefinition
	cmpi.w #2, d3
	beq.w endStructDefinition
	cmpi.w #3, d3
	beq.w skipStructField
	bra.w fail

beginStructDefinition
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	jsr structs.beginDefinitionV1
	bne.w fail
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	bra.w success

endStructDefinition
	jsr structs.endDefinitionV1
	bne.w fail
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	bra.w success

skipStructField
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	movea.l a0, a1
	move.l d0, d2
	move.l d4, d1
	movea.l a1, a0
	move.l d2, d0
	jsr structs.captureFieldV1
	bne.w fail
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	bra.w success

skipTextEncodingDirective
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	bra.w success

checkConditionalDirective
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
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
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d4
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
	cmpi.w #5, d3
	beq.w beginBfor
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
	moveq #0, d2
	move.w d3, d2
	jsr structs.advanceScopedRepeatV1
	bne.w fail
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
	moveq #0, d2
	move.w d3, d2
	jsr structs.clearScopedRepeatV1
	bne.w fail
	lea OpasmRepeatHasBinding, a0
	tst.b 0(a0, d3.l)
	beq.s finishPop
	jsr compile_values.popBindingV1
	bne.w fail
finishPop
	move.w d3, OpasmRepeatDepth
	move.w d7, d2
	addq.w #1, d2
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
	move.w d7, d2
	addq.w #1, d2
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
	jsr navigation.findMatchingEndwhileV1
	bne.w fail
	moveq #1, d1
	addq.w #1, d2
	bra.w success

beginIfBranch
	moveq #4, d5
	bsr.w readOperandValueForStatement
	bne.w fail
	tst.l d3
	beq.s ifFalse
	bra.w selectIfBranch
ifFalse
	jsr navigation.findNextIfBranchV1
	bne.w fail
	cmpi.w #1, d1
	beq.w selectIfBranchAtD2
	cmpi.w #2, d1
	beq.w selectElseIfBranchAtD2
	addq.w #1, d2
	bra.w finishIfBranch

handleElseifBranch
	jsr eng.opasmEngineGetFlowRedirectedV1
	tst.w d0
	beq.w skipSelectedIfBranch
	moveq #4, d5
	bsr.w readOperandValueForStatement
	bne.w fail
	tst.l d3
	beq.s elseifFalse
	bra.w selectIfBranch
elseifFalse
	jsr navigation.findNextIfBranchV1
	bne.w fail
	cmpi.w #1, d1
	beq.w selectIfBranchAtD2
	cmpi.w #2, d1
	beq.w selectElseIfBranchAtD2
	addq.w #1, d2
	bra.w finishIfBranch

handleElseBranch
	bra.w skipSelectedIfBranch

finishIfBranch
	; Skip the zero-width marker while marking the next statement as a sequential
	; arrival, so an outer `.elseif` is not mistaken for a false-branch target.
	move.w d7, d2
	addq.w #1, d2
	ori.w #$8000, d2
	moveq #1, d1
	bra.w success

beginMatchBranch
	moveq #4, d5
	bsr.w readOperandValueForStatement
	bne.w fail
	move.l d3, OpasmMatchValue
	lea caseMatchesCurrentValue, a0
	jsr navigation.findSelectedMatchBranchV1
	bne.w fail
	addq.w #1, d2
	moveq #1, d1
	bra.w success

skipSelectedMatchBranch
	jsr navigation.findMatchingEndmatchV1
	bne.w fail
	addq.w #1, d2
	moveq #1, d1
	bra.w success

finishEndmatchBranch
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	bra.w success

selectIfBranch
	move.w d7, d2
	addq.w #1, d2
selectIfBranchAtD2
	moveq #1, d1
	bra.w success

selectElseIfBranchAtD2
	addq.w #1, d2
	moveq #1, d1
	bra.w success

skipSelectedIfBranch
	jsr navigation.findMatchingEndifV1
	bne.w fail
	addq.w #1, d2
	moveq #1, d1
	bra.w success

beginFor
	moveq #0, d5
	move.w OpasmRepeatDepth, d5
	cmpi.w #OPASM_REPEAT_STACK_CAPACITY, d5
	bhs.w fail
	moveq #0, d2
	move.w d5, d2
	jsr structs.clearScopedRepeatV1
	bne.w fail
	bra.s beginForShared

beginBfor
	moveq #0, d5
	move.w OpasmRepeatDepth, d5
	cmpi.w #OPASM_REPEAT_STACK_CAPACITY, d5
	bhs.w fail
	jsr eng.opasmEngineGetSessionCurrentPcV1
	move.l d5, d6
	lsl.l #2, d6
	lea OpasmRepeatBforBasePc.l, a0
	move.l d0, 0(a0, d6.l)
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	moveq #0, d2
	move.w d5, d2
	jsr structs.beginScopedRepeatV1
	bne.w fail

beginForShared
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
	jsr navigation.findMatchingEndforV1
	bne.w fail
	moveq #1, d1
	addq.w #1, d2
	bra.w success

processStatement
	bsr.w tryCaptureTypedStructInstanceForStatement
	clr.w d1

success
	tst.w d1
	beq.s successStatus
	move.w d2, d0
	jsr eng.opasmEngineSetFlowNextV1
successStatus
	moveq #0, d0
	bra.s return

fail
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementSourceTextV1
	move.l d0, d1
	moveq #abi.OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w appendTextEvent
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d3-d7/a0-a2/a4-a5
	rts
	.bend  ; opasmDriverApplyFlowControl

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

opasmDriverPassTwoOk	.block
	jsr eng.opasmEngineFlushMappedImageV1
	bne.s fail
	moveq #abi.OPASM_EVENT_PASS_OK, d0
	moveq #2, d1
	bsr.w appendPassEvent
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; opasmDriverPassTwoOk

opasmDriverRecordLabel	.block
	movem.l d1-d7/a0-a1/a5, -(sp)
	move.w d0, d7
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	clr.w OpasmDriverScopedRepeatLabel.l
	bsr.w qualifyScopedRepeatLabelForStatement
	bne.w return
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
	jsr directives.classifyV1
	cmpi.w #directives.OPASM_DIRECTIVE_CONST, d3
	beq.w recordConstSymbol
	cmpi.w #directives.OPASM_DIRECTIVE_VAR, d3
	beq.w recordMutableSymbol
	cmpi.w #directives.OPASM_DIRECTIVE_SET, d3
	beq.w recordMutableSymbol

recordPcLabel
	tst.w OpasmDriverScopedRepeatLabel.l
	beq.s recordAbsolutePcLabel
	move.l OpasmDriverScopedRepeatValue.l, d3
	moveq #0, d4
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineRecordStatementLabelValueV1
	bra.w handleLabelEvent

recordAbsolutePcLabel
	jsr scopes.qualifyStatementLabelIfScopedV1
	tst.l d0
	bne.w symbolValueFail
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
	bsr.w tryCaptureTypedStructInstanceForStatement
	tst.l d0
	beq.s recordCapturedStructSymbol
	moveq #2, d5
	move.l d7, -(sp)
	bsr.w readOperandValueForStatement
	move.l d0, d5
	move.l (sp)+, d7
	tst.l d5
	bne.w symbolValueFail
	bra.s recordResolvedSymbolValue

recordCapturedStructSymbol
	moveq #0, d5

recordResolvedSymbolValue
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
	movem.l (sp)+, d1-d7/a0-a1/a5
	rts

symbolValueFail
	move.l d0, d2
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementOwnerTextV1
	move.l d0, d1
	moveq #abi.OPASM_EVENT_SERVICE_FAILURE, d0
	bsr.w appendTextValueEvent
	moveq #1, d0
	bra.s return
	.bend  ; opasmDriverRecordLabel

; Replace a label inside `.bfor` with its zero-based scoped name before the
; ordinary label table sees it. Repeated visits use the final component of the
; prior replacement, so the shared statement table remains reusable.
; @opforge-owner: opasm.amigaos.flow_structs
; @opforge-slice: documentation/plans/slices/native-porting-slice-structs.toml
; @opforge-role: delegation
; Inputs: D7.W = statement index.
; Outputs: D0 = 0 on success/no active `.bfor`, 1 on capacity failure.
qualifyScopedRepeatLabelForStatement	.block
	tst.w OpasmRepeatDepth
	beq.s scopedLabelOk
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	movea.l a0, a1
	move.l d0, d4
	moveq #0, d2
	move.w OpasmRepeatDepth, d2
	subq.w #1, d2
	movea.l a1, a0
	move.l d4, d0
	jsr structs.qualifyScopedRepeatLabelV1
	bne.s scopedLabelFail
	tst.w d3
	beq.s scopedLabelOk
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineSetStatementLabelTextV1
	bne.s scopedLabelFail
	jsr eng.opasmEngineGetSessionCurrentPcV1
	moveq #0, d2
	move.w OpasmRepeatDepth, d2
	subq.w #1, d2
	lsl.l #2, d2
	lea OpasmRepeatBforBasePc.l, a0
	sub.l 0(a0, d2.l), d0
	move.l d0, OpasmDriverScopedRepeatValue.l
	move.w #1, OpasmDriverScopedRepeatLabel.l
	moveq #0, d0
	rts
scopedLabelOk
	moveq #0, d0
	rts
scopedLabelFail
	moveq #1, d0
	rts
	.bend  ; qualifyScopedRepeatLabelForStatement

; Capture a typed struct instance from a const, var, or set statement.
; Inputs: D7.W = statement index.
; Outputs: D0 = 0 when captured and D3 = struct size; D0 = 1 otherwise.
; Clobbers: D0-D3/A0-A2/CCR.
; CCR: reflects D0 on return.
tryCaptureTypedStructInstanceForStatement	.block
	movem.l d1-d2/d4-d6/a0-a2, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movea.l sp, a0
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.s fail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_MNEM_LEN(sp), d0
	jsr directives.classifyV1
	cmpi.w #directives.OPASM_DIRECTIVE_CONST, d3
	beq.s capture
	cmpi.w #directives.OPASM_DIRECTIVE_VAR, d3
	beq.s capture
	cmpi.w #directives.OPASM_DIRECTIVE_SET, d3
	bne.s fail
capture
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	movea.l a0, a2
	move.l d0, d2
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a1
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d1
	movea.l a2, a0
	move.l d2, d0
	jsr structs.captureTypedInstanceV1
	bra.s return
fail
	moveq #1, d0
return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d6/a0-a2
	tst.l d0
	rts
	.bend  ; tryCaptureTypedStructInstanceForStatement

opasmDriverEmitImageBytes	.block
	movem.l d1-d6/a0-a4, -(sp)
	move.w d0, d6
	jsr layout.statementImageRouteV1
	jsr eng.opasmEngineSetImageRouteV1
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
	jsr directives.classifyV1
	cmpi.w #directives.OPASM_DIRECTIVE_ALIGN, d3
	beq.w emitAlign
	cmpi.w #directives.OPASM_DIRECTIVE_DS, d3
	beq.w emitDs
	cmpi.w #directives.OPASM_DIRECTIVE_FILL, d3
	beq.w emitFill
	cmpi.w #directives.OPASM_DIRECTIVE_BYTE, d3
	beq.w emitByte
	cmpi.w #directives.OPASM_DIRECTIVE_WORD, d3
	beq.w emitWord
	cmpi.w #directives.OPASM_DIRECTIVE_LONG, d3
	beq.w emitLong
	cmpi.w #directives.OPASM_DIRECTIVE_TEXT, d3
	beq.w emitText
	cmpi.w #directives.OPASM_DIRECTIVE_NULL, d3
	beq.w emitNull
	cmpi.w #directives.OPASM_DIRECTIVE_PTEXT, d3
	beq.w emitPtext
	tst.w d3
	bne.w ok
	jsr eng.opasmEngineResetLastResolvedLabelV1
	bsr.w prepareEncodeSelectedRequestForStatement
	bne.w return
	tst.w OpasmDriverEvalRequestLen
	beq.w ok
	bsr.w prepareSelectedEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.adaptSelectedEncodeRequestV1
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
	; `.ds` reserves address space during opasmDriverAdvancePc. Rust does not
	; materialize reservation bytes in the emitted-entry artifact.
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
	bsr.w emitByteDirectiveForStatement
	bne.w emitLayoutFail
	moveq #0, d0
	bra.w return

emitWord
	move.w d6, d7
	moveq #0, d5
	bsr.w parseTextDirectiveForStatement
	bne.s emitWordNumeric
	move.l OpasmTextScratchLen, d0
	beq.s emitWordOk
	lea OpasmTextScratch.l, a0
	jsr eng.opasmEngineAppendImageBytesV1
	bne.w emitLayoutFail
	bra.s emitWordOk
emitWordNumeric
	moveq #2, d5
	bsr.w emitDataDirectiveForStatement
	bne.w emitLayoutFail
emitWordOk
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
	movem.l d0-d7/a0-a5, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	move.l d0, d7
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #1, d0
	bne.s statementLayoutRecorded
	move.l d7, d0
	jsr layout.recordStatementSectionV1
	bne.w done
statementLayoutRecorded
	movea.l sp, a0
	move.l d7, d0
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
	jsr directives.classifyV1
	cmpi.w #directives.OPASM_DIRECTIVE_ORG, d3
	beq.w org
	cmpi.w #directives.OPASM_DIRECTIVE_REGION, d3
	beq.w region
	cmpi.w #directives.OPASM_DIRECTIVE_SECTION, d3
	beq.w section
	cmpi.w #directives.OPASM_DIRECTIVE_ENDSECTION, d3
	beq.w endsection
	cmpi.w #directives.OPASM_DIRECTIVE_PLACE, d3
	beq.w place
	cmpi.w #directives.OPASM_DIRECTIVE_PACK, d3
	beq.w pack
	cmpi.w #directives.OPASM_DIRECTIVE_ALIGN, d3
	beq.w align
	cmpi.w #directives.OPASM_DIRECTIVE_DS, d3
	beq.w ds
	cmpi.w #directives.OPASM_DIRECTIVE_RES, d3
	beq.w res
	cmpi.w #directives.OPASM_DIRECTIVE_FILL, d3
	beq.w fill
	cmpi.w #directives.OPASM_DIRECTIVE_BYTE, d3
	beq.w byte
	cmpi.w #directives.OPASM_DIRECTIVE_WORD, d3
	beq.w word
	cmpi.w #directives.OPASM_DIRECTIVE_LONG, d3
	beq.w long
	cmpi.w #directives.OPASM_DIRECTIVE_TEXT, d3
	beq.w text
	cmpi.w #directives.OPASM_DIRECTIVE_NULL, d3
	beq.w null
	cmpi.w #directives.OPASM_DIRECTIVE_PTEXT, d3
	beq.w ptext
	tst.w d3
	bne.w done
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
	movem.l (sp)+, d0-d7/a0-a5
	moveq #1, d0
	rts

selectedSizeOk
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #1, d0
	bne.s selectedSizeKnown
	jsr eng.opasmEngineGetLastResolvedLabelV1
	cmpi.w #$ffff, d0
	beq.s selectedSizeKnown
	jsr layout.recordReachableLabelV1
	bne.w done
selectedSizeKnown
	cmpi.w #1, d1
	beq.w advanceOne
	cmpi.w #2, d1
	beq.w advanceTwo
	cmpi.w #3, d1
	beq.w advanceThree
	bra.w done

org
	; Origin literals do not require an expression-service round trip.  Resolve
	; the statement's already captured operand before falling back to the
	; general evaluator, keeping consecutive module origins independent.
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	bsr.w parseDirectiveLiteralValue
	beq.s orgOk
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementSourceTextV1
	beq.s orgGeneralExpression
	bsr.w skipLineWhitespace
	bsr.w skipSourceHeadToken
	bsr.w skipLineWhitespace
	bsr.w parseDirectiveLiteralValue
	beq.s orgOk

orgGeneralExpression
	moveq #2, d5
	bsr.w readOperandValueForStatement
	beq.s orgOk

orgBad
	moveq #abi.OPASM_EVENT_BAD_ORG, d0
	bsr.w appendKindEvent
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a5
	moveq #1, d0
	rts

orgOk
	move.l d3, d0
	; The first origin establishes the image base. Later forward origins must
	; advance the current PC and materialize the binary gap, matching the Rust
	; CLI's flat --bin artifact rather than concatenating addressed fragments.
	bsr.w setPlacedSectionOriginWithImageGap
	bne.s orgBad
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

pack
	bsr.w processPackDirectiveForStatement
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
	tst.l d3
	beq.s advanceLayoutReady
	jsr layout.sectionActiveV1
	tst.l d0
	bne.s advanceLayoutReady
	move.w #1, OpasmDriverImageBaseSeen
advanceLayoutReady
	move.l d3, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

byte
	bsr.w byteDirectiveSizeForStatement
	beq.s advanceLayoutD3
	bra.w orgBad

word
	moveq #0, d5
	bsr.w parseTextDirectiveForStatement
	bne.s wordNumeric
	move.l OpasmTextScratchLen, d3
	bra.s advanceLayoutD3
wordNumeric
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
	jsr layout.sectionActiveV1
	tst.l d0
	bne.s advanceOneReady
	move.w #1, OpasmDriverImageBaseSeen
advanceOneReady
	moveq #1, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

advanceTwo
	jsr layout.sectionActiveV1
	tst.l d0
	bne.s advanceTwoReady
	move.w #1, OpasmDriverImageBaseSeen
advanceTwoReady
	moveq #2, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

advanceThree
	jsr layout.sectionActiveV1
	tst.l d0
	bne.s advanceThreeReady
	move.w #1, OpasmDriverImageBaseSeen
advanceThreeReady
	moveq #3, d0
	jsr eng.opasmEngineAdvancePcBySizeV1
	bra.w done

done
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d0-d7/a0-a5
	moveq #0, d0
	rts
	.bend  ; opasmDriverAdvancePc

trySelectedEncodeSizeForStatement	.block
	movem.l d2-d7/a0-a2, -(sp)
	move.w d0, d6
	jsr eng.opasmEngineResetLastResolvedLabelV1
	clr.w d4
	bsr.w prepareEncodeSelectedRequestForStatement
	bne.w prepareFail
	tst.w OpasmDriverEvalRequestLen
	beq.w empty
	bsr.w prepareSelectedEvaluateExpressionExtension
	bsr.w serviceFramePtr
	move.w OpasmDriverEvalRequestLen, d0
	jsr tkpkg.adaptSelectedEncodeRequestV1
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
	.ifdef OPFORGE_MACRO_EXPR_EVAL_PROBE
	; Instrumentation point: `.byte` text parser immediately after retrieving
	; the statement's bounded operand metadata.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D7/A0-A3 by the routine's existing save/restore.
	; SR/CCR preserved: no branch consumes flags across this diagnostic block.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: it is diagnostic-only and the
	; metadata fetch status has already been consumed by the branch above.
	; Removal/stabilization plan: remove after the byte-list input invariant is
	; confirmed by the authoritative macro fixture.
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d3
	moveq #0, d4
	move.w d7, d4
	moveq #0, d5
	move.w d5, d5
	moveq #0, d6
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_EXPR_REQUEST
	.endif
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
	jsr scopes.resolveLabelValueV1
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
	jsr compile_values.resolveBindingExpressionV1
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
	jsr eng.opasmEngineResolveLabelValueV1
	beq.w checkWidth
	bsr.w resolveImportedLabelValue
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
	bra.w ok
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
	jsr layout.scratchNamePtrV1
	movea.l a0, a1
	moveq #1, d6
	bsr.w readCommaNameForStatement
	bne.w fail
	moveq #0, d0
	move.w d3, d0
	jsr layout.setScratchNameLenV1
	moveq #2, d6
	bsr.w readCommaOperandValueForStatement
	bne.w fail
	move.l d3, d0
	moveq #0, d1
	jsr layout.setScratchRegionBoundV1
	moveq #3, d6
	bsr.w readCommaOperandValueForStatement
	bne.w fail
	move.l d3, d0
	moveq #1, d1
	jsr layout.setScratchRegionBoundV1
	moveq #4, d6
	bsr.w readAlignOptionForStatement
	bne.w fail
	movea.l d3, a3
	moveq #0, d1
	jsr layout.findScratchNameV1
	beq.w fail
	jsr layout.scratchRegionOverlapsV1
	bne.w fail
	jsr layout.getScratchRegionRequestV1
	move.l a3, d3
	jsr layout.appendRegionV1
	bne.w fail

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
	jsr layout.scratchNamePtrV1
	movea.l a0, a1
	moveq #1, d6
	bsr.w readCommaNameForStatement
	bne.w fail
	moveq #0, d0
	move.w d3, d0
	jsr layout.setScratchNameLenV1
	moveq #2, d6
	bsr.w readAlignOptionForStatement
	bne.w fail
	movea.l d3, a3
	bsr.w readSectionKindForStatement
	bne.w fail
	move.l d3, d0
	jsr layout.setScratchSectionKindV1
	bne.w fail
	bsr.w readSectionLogicalForStatement
	bne.w fail
	move.l d3, d0
	jsr layout.setScratchSectionLogicalV1
	bne.w fail
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementOwnerTextV1
	jsr layout.setScratchSectionOwnerV1
	bne.w fail
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #2, d0
	beq.w passTwo
	jsr layout.findScratchOwnedSectionV1
	beq.w fail
	jsr eng.opasmEngineGetSessionCurrentPcV1
	move.l d0, d2
	jsr layout.getScratchNameV1
	move.l a3, d1
	jsr layout.appendSectionV1
	bne.w fail
	bra.w ok

passTwo
	jsr layout.findScratchOwnedSectionV1
	bne.w fail
	jsr layout.sectionPlacedPtrV1
	tst.w (a0)
	beq.w startSection
	jsr layout.sectionBasePtrV1
	move.l (a0), d0
	bsr.w setPlacedSectionOriginWithImageGap
	bne.w fail

startSection
	jsr eng.opasmEngineGetSessionCurrentPcV1
	jsr layout.beginSectionPassTwoV1
	bne.w fail

ok
	moveq #0, d0
	bra.w return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; processSectionDirectiveForStatement

; Read `kind=bss` from a `.section` option list, defaulting to code.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success; D3.L = OPASM_LAYOUT_SECTION_KIND_*.
; Clobbers: D0-D6/A0-A3/CCR.
; CCR: reflects D0.L on return.
; @opforge-owner: opasm.amigaos.layout
; @opforge-slice: documentation/plans/slices/native-porting-slice-source-artifact-output.toml
; @opforge-role: delegation
readSectionKindForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w defaultCode
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a2
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d2
	cmpi.l #8, d2
	blo.w defaultCode

scan
	move.b (a2), d3
	bsr.w lowerD3
	cmpi.b #'k', d3
	bne.s next
	move.b 1(a2), d3
	bsr.w lowerD3
	cmpi.b #'i', d3
	bne.s next
	move.b 2(a2), d3
	bsr.w lowerD3
	cmpi.b #'n', d3
	bne.s next
	move.b 3(a2), d3
	bsr.w lowerD3
	cmpi.b #'d', d3
	bne.s next
	cmpi.b #'=', 4(a2)
	bne.s next
	move.b 5(a2), d3
	bsr.w lowerD3
	cmpi.b #'b', d3
	bne.s next
	move.b 6(a2), d3
	bsr.w lowerD3
	cmpi.b #'s', d3
	bne.s next
	move.b 7(a2), d3
	bsr.w lowerD3
	cmpi.b #'s', d3
	bne.s next
	moveq #layout.OPASM_LAYOUT_SECTION_KIND_BSS, d3
	moveq #0, d0
	bra.s return

next
	addq.l #1, a2
	subq.l #1, d2
	cmpi.l #8, d2
	bhs.s scan

defaultCode
	moveq #layout.OPASM_LAYOUT_SECTION_KIND_CODE, d3
	moveq #0, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; readSectionKindForStatement

; Read the structural `logical` section option, defaulting to concrete.
; Inputs: D7.W = statement index. Outputs: D0.L = 0; D3.L = 0 or 1.
; @opforge-owner: opasm.amigaos.layout
; @opforge-slice: documentation/plans/slices/native-porting-slice-opasm-layout.toml
; @opforge-role: delegation
readSectionLogicalForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w concrete
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a2
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d2
	cmpi.l #7, d2
	blo.w concrete
scanLogical
	move.b (a2), d3
	bsr.w lowerD3
	cmpi.b #'l', d3
	bne.w nextLogical
	move.b 1(a2), d3
	bsr.w lowerD3
	cmpi.b #'o', d3
	bne.w nextLogical
	move.b 2(a2), d3
	bsr.w lowerD3
	cmpi.b #'g', d3
	bne.w nextLogical
	move.b 3(a2), d3
	bsr.w lowerD3
	cmpi.b #'i', d3
	bne.w nextLogical
	move.b 4(a2), d3
	bsr.w lowerD3
	cmpi.b #'c', d3
	bne.w nextLogical
	move.b 5(a2), d3
	bsr.w lowerD3
	cmpi.b #'a', d3
	bne.w nextLogical
	move.b 6(a2), d3
	bsr.w lowerD3
	cmpi.b #'l', d3
	beq.w logical
nextLogical
	addq.l #1, a2
	subq.l #1, d2
	cmpi.l #7, d2
	bhs.w scanLogical
concrete
	moveq #0, d3
	bra.w logicalReturn
logical
	moveq #1, d3
logicalReturn
	moveq #0, d0
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; readSectionLogicalForStatement

; Leave the active native section slice and record its pass-one size.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 when no section is active.
; Clobbers: D0-D5/A0/CCR.
; CCR: reflects D0.L on return.
processEndsectionDirectiveForStatement	.block
	jsr layout.processEndsectionV1
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
	jsr layout.sectionActiveV1
	bne.w fail
	bsr.w parsePlaceDirectiveNamesForStatement
	bne.w fail
	moveq #1, d1
	jsr layout.findPlaceNameV1
	bne.w fail
	moveq #0, d0
	move.w d5, d0
	moveq #0, d1
	jsr layout.setPlaceIndexV1
	jsr layout.sectionPlacedPtrV1
	tst.w (a0)
	bne.w fail
	moveq #0, d1
	jsr layout.findPlaceNameV1
	bne.w fail
	moveq #0, d0
	move.w d5, d0
	moveq #1, d1
	jsr layout.setPlaceIndexV1
	moveq #2, d6
	bsr.w readAlignOptionForStatement
	bne.w fail
	move.l d3, d0
	jsr layout.placeSectionV1
	bne.w fail

ok
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; processPlaceDirectiveForStatement

; Pack named native section slices into one region in source order.
; @opforge-owner: opasm.amigaos.layout
; @opforge-slice: documentation/plans/slices/native-porting-slice-opasm-layout.toml
; @opforge-role: delegation
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 on malformed, missing, duplicate, or overflowing state.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
processPackDirectiveForStatement	.block
	movem.l d1-d7/a0-a3, -(sp)
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #1, d0
	bne.w ok
	jsr layout.sectionActiveV1
	bne.w fail
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.w metadataFail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	beq.w metadataFail
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2
	lea InText, a1
	moveq #2, d1
	bsr.w lineStartsWith
	beq.w metadataFail
	addq.l #2, a2
	subq.l #2, d2
	movea.l a2, a0
	move.l d2, d0
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2
	jsr layout.placeRegionNamePtrV1
	movea.l a0, a1
	movea.l a2, a0
	move.l d2, d0
	bsr.w copyNameTokenSlice
	bne.w metadataFail
	moveq #0, d0
	move.w d3, d0
	moveq #1, d1
	jsr layout.setPlaceNameLenV1
	adda.l d3, a2
	sub.l d3, d2
	movea.l a2, a0
	move.l d2, d0
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2
	beq.w metadataFail
	cmpi.b #':', (a2)
	bne.w metadataFail
	addq.l #1, a2
	subq.l #1, d2
	movea.l a2, a0
	move.l d2, d0
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2
	beq.w metadataFail

	move.l d2, -(sp)
	move.l a2, -(sp)
	moveq #0, d1
	jsr layout.findPlaceNameV1
	bne.w regionLookupFail
	moveq #0, d0
	move.w d5, d0
	moveq #1, d1
	jsr layout.setPlaceIndexV1
	movea.l (sp)+, a2
	move.l (sp)+, d2

sectionLoop
	movea.l a2, a0
	move.l d2, d0
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2
	beq.w metadataFail
	jsr layout.placeSectionNamePtrV1
	movea.l a0, a1
	movea.l a2, a0
	move.l d2, d0
	bsr.w copyNameTokenSlice
	bne.w metadataFail
	moveq #0, d0
	move.w d3, d0
	moveq #0, d1
	jsr layout.setPlaceNameLenV1
	adda.l d3, a2
	sub.l d3, d2
	movea.l a2, a0
	move.l d2, d0
	bsr.w skipLineWhitespace
	movea.l a0, a2
	move.l d0, d2

	move.l d2, -(sp)
	move.l a2, -(sp)
	moveq #1, d1
	jsr layout.findPlaceNameV1
	bne.w sectionLookupFail
	moveq #0, d0
	move.w d5, d0
	moveq #0, d1
	jsr layout.setPlaceIndexV1
	moveq #1, d0
	jsr layout.placeSectionV1
	bne.w sectionLookupFail
	movea.l (sp)+, a2
	move.l (sp)+, d2
	beq.s metadataOk
	cmpi.b #',', (a2)
	bne.w metadataFail
	addq.l #1, a2
	subq.l #1, d2
	bne.w sectionLoop
	bra.w metadataFail

regionLookupFail
	addq.l #8, sp
	bra.s metadataFail

sectionLookupFail
	addq.l #8, sp
	bra.s metadataFail

metadataOk
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	bra.s ok

metadataFail
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	bra.s fail

ok
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; processPackDirectiveForStatement

; Retain the minimum pass-one origin, then position pass two by address. The
; engine's statement write cursor materializes forward gaps and overwrites
; backward ranges, so source-order `.org` changes never depend on append order.
; @opforge-owner: opasm.amigaos.image
; @opforge-slice: documentation/plans/slices/native-porting-slice-overlapping-origin-image.toml
; @opforge-role: implementation
; Inputs: D0.L = placed section base.
; Outputs: D0.L = 0 on success.
; Clobbers: D0-D4/A0/CCR.
; CCR: reflects D0.L on return.
setPlacedSectionOriginWithImageGap	.block
	movem.l d1-d4/a0, -(sp)
	move.l d0, d4
	jsr eng.opasmEngineGetSessionPassV1
	cmpi.w #1, d0
	bne.s setCurrentPc
	tst.w OpasmDriverImageBaseSeen
	bne.s updateMinimum
	move.w #1, OpasmDriverImageBaseSeen
	move.l d4, d0
	jsr eng.opasmEngineSetOriginV1
	bra.s return

updateMinimum
	jsr eng.opasmEngineGetSessionOriginV1
	cmp.l d0, d4
	bhs.s setCurrentPc
	move.l d4, d0
	jsr eng.opasmEngineSetImageOriginV1

setCurrentPc
	move.l d4, d0
	jsr eng.opasmEngineSetCurrentPcV1

return
	movem.l (sp)+, d1-d4/a0
	rts
	.bend  ; setPlacedSectionOriginWithImageGap

; Read a comma-separated directive identifier part into a bounded layout buffer.
; Inputs: D7.W = statement index; D6.W = one-based part; A1 = destination buffer.
; Outputs: D0.L = 0 on success, 1 on missing/over-capacity name; D3.W = copied length.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
readCommaNameForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	move.l a1, -(sp)
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
	movea.l eng.OPASM_ENGINE_STMT_TEXT_BYTES(sp), a1
	bsr.w copyNameTokenSlice
	bra.s return

fail
	moveq #1, d0

return
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	addq.l #4, sp
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
	jsr layout.placeSectionNamePtrV1
	movea.l a0, a1
	movea.l a2, a0
	move.l d2, d0
	bsr.w copyNameTokenSlice
	bne.w fail
	moveq #0, d0
	move.w d3, d0
	moveq #0, d1
	jsr layout.setPlaceNameLenV1
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
	jsr layout.placeRegionNamePtrV1
	movea.l a0, a1
	movea.l a2, a0
	move.l d2, d0
	bsr.w copyNameTokenSlice
	bne.w fail
	moveq #0, d0
	move.w d3, d0
	moveq #1, d1
	jsr layout.setPlaceNameLenV1
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
	cmpi.b #':', d1
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

; Lowercase an ASCII byte when it is in D3.B.
; Inputs: D3.B = byte.
; Outputs: D3.B = lowercase byte for ASCII A-Z, otherwise unchanged.
; Clobbers: D3/CCR.
; CCR: unspecified on return.
lowerD3	.block
	jsr layout.lowerD3
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
	bsr.w scopes.resolveLabelValueV1
	beq.w evalPartOk
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	jsr compile_values.resolveBindingV1
	beq.w evalPartOk
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	jsr compile_values.resolveBindingExpressionV1
	beq.w evalPartOk
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	jsr compile_values.resolveSequenceExpressionV1
	beq.w evalPartOk
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	jsr eng.opasmEngineResolveLabelValueV1
	beq.w evalPartOk
	bsr.w resolveImportedLabelValue
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
.ifdef OPFORGE_MACRO_EXPR_EVAL_PROBE
	; Instrumentation point: expression service dispatch after the request is
	; complete and before its status is consumed by the fallback branch.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D6/A0 by the balanced save/restore below.
	; SR/CCR preserved: restored before the following BNE consumes dispatch status.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: the saved CCR is restored before
	; the original branch, and this code exists only in the diagnostic build.
	; Removal/stabilization plan: remove after the expression-return invariant is
	; established by the authoritative FS-UAE fixture.
	move.w ccr, -(sp)
	movem.l d1-d6/a0, -(sp)
	move.l d0, d4
	move.l d1, d5
	move.l d2, d6
	moveq #0, d1
	move.w OpasmDriverEvalRequestLen, d1
	moveq #0, d2
	move.w d5, d2
	moveq #0, d3
	move.w d6, d3
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_EXPR_REQUEST
	movem.l (sp)+, d1-d6/a0
	move.w (sp)+, ccr
	.endif
	bne.s evalPartFallback
	bsr.w readEvaluateExpressionValue
	bra.w evalPartOk

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

; Ask the embedding CLI to rewrite one imported ordinary name, then resolve
; the returned fully-qualified label in the engine-owned pass-one table.
; @opforge-owner: opasm.amigaos.operand_eval
; @opforge-slice: documentation/plans/slices/native-porting-slice-module-visibility.toml
; @opforge-role: delegation
; Inputs: OpasmDriverEvalFallbackPtr/Len = original token.
; Outputs: D0 = 0 success/1 unavailable or unresolved; D3 = value on success.
; Clobbers: D0-D2/A0-A2/CCR.
resolveImportedLabelValue	.block
	bsr.w serviceFramePtr
	movea.l abi.OPASM_SERVICE_IMPORT_NAME_RESOLVER_PTR(a0), a2
	move.l a2, d0
	beq.s importedResolveFail
	jsr scopes.activeModuleNameV1
	tst.l d1
	beq.s importedResolveFail
	movea.l OpasmDriverEvalFallbackPtr, a0
	move.l OpasmDriverEvalFallbackLen, d0
	jsr (a2)
	tst.l d1
	bne.s importedResolveFail
	jsr eng.opasmEngineResolveLabelValueV1
	rts
importedResolveFail
	moveq #1, d0
	rts
	.bend  ; resolveImportedLabelValue

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
	beq.w ok

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
	jsr eng.opasmEngineGetSessionCurrentPcV1
	jsr layout.alignPadV1
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
	lea countCommaPartsForStatement, a0
	jsr data_directives.sizeNumericDirectiveV1
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; dataDirectiveSizeForStatement

; Return the byte size of a `.byte`/`.db` list, including quoted operands.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 on malformed data; D3.L = byte size.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
byteDirectiveSizeForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	moveq #3, d5
	bsr.w parseTextDirectiveForStatement
	bne.s fail
	move.l OpasmTextScratchLen, d3
	moveq #0, d0
	bra.s return

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; byteDirectiveSizeForStatement

; Emit a numeric data directive in first-run MOS little-endian order.
; Inputs: D7.W = statement index; D5.W = unit size (1, 2, or 4).
; Outputs: D0.L = 0 on success, 1 on malformed data list or image overflow.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
emitDataDirectiveForStatement	.block
	movem.l d1-d7/a0-a3, -(sp)
	lea countCommaPartsForStatement, a0
	lea resolveNumericDataPartForOwner, a1
	jsr data_directives.emitNumericDirectiveV1

return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; emitDataDirectiveForStatement

; Resolve the current numeric-data part for the directive-data owner.
; Inputs: D7.W = statement; D2.W = part count; D5.W = unit bytes; D6.W = part.
; Outputs: D0.L = status; D3.L = resolved value.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0 on return.
resolveNumericDataPartForOwner	.block
	cmpi.w #1, d2
	bne.s splitPart
	bsr.w readOperandValueForStatement
	rts

splitPart
	bsr.w readCommaOperandValueForStatement
	rts
	.bend  ; resolveNumericDataPartForOwner

; Emit a `.byte`/`.db` list whose quoted operands use the active encoding.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 on malformed data or image overflow.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
emitByteDirectiveForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	moveq #3, d5
	bsr.w parseTextDirectiveForStatement
	bne.s fail
	move.l OpasmTextScratchLen, d0
	beq.s ok
	lea OpasmTextScratch.l, a0
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
	.bend  ; emitByteDirectiveForStatement

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
	lea parseTextDirectiveForOwner, a0
	lea textScratchContainsZero, a1
	jsr text_directives.sizeTextDirectiveV1

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
	lea parseTextDirectiveForOwner, a0
	lea textScratchContainsZero, a1
	lea OpasmTextScratch.l, a2
	jsr text_directives.emitTextDirectiveV1

return
	movem.l (sp)+, d1-d2/d4-d7/a0-a3
	rts
	.bend  ; emitTextDirectiveForStatement

; Parse text and expose its current scratch length to the text owner.
; Inputs: D7.W = statement; D5.W = text mode.
; Outputs: D0.L = status; D3.L = parsed text length.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0 on return.
parseTextDirectiveForOwner	.block
	bsr.w parseTextDirectiveForStatement
	move.l OpasmTextScratchLen, d3
	tst.l d0
	rts
	.bend  ; parseTextDirectiveForOwner

; Parse quoted text operands into the scratch buffer.
; Inputs: D7.W = statement index.
; Outputs: D0.L = 0 on success, 1 on malformed text or scratch overflow.
; Clobbers: D0-D7/A0-A3/CCR.
; CCR: reflects D0.L on return.
parseTextDirectiveForStatement	.block
	movem.l d1-d2/d4-d7/a0-a3, -(sp)
	clr.l OpasmTextScratchLen
	clr.w d6
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
	beq.w ok
	addq.w #1, d6
	moveq #0, d3
	move.b (a2), d3
	cmpi.b #'"', d3
	beq.s quotedOperand
	cmpi.b #39, d3
	beq.s quotedOperand
	cmpi.w #3, d5
	bne.w fail
	bra.w numericOperand

quotedOperand
	move.l OpasmTextScratchLen, d4
	addq.l #1, a2
	subq.l #1, d2

charLoop
	tst.l d2
	beq.w fail
	move.b (a2)+, d1
	subq.l #1, d2
	cmp.b d3, d1
	beq.s operandEnd
	cmpi.b #92, d1
	bne.s appendChar
	tst.l d2
	beq.w fail
	move.b (a2)+, d1
	subq.l #1, d2
	cmpi.b #'x', d1
	bne.s simpleEscape
	cmpi.l #2, d2
	blo.w fail
	move.b (a2)+, d1
	subq.l #1, d2
	bsr.w hexNibbleValue
	bmi.w fail
	move.l d1, -(sp)
	move.b (a2)+, d1
	subq.l #1, d2
	bsr.w hexNibbleValue
	bmi.s hexEscapeFail
	move.l (sp)+, d0
	lsl.b #4, d0
	or.b d0, d1
	bra.s appendChar

hexEscapeFail
	addq.l #4, sp
	bra.w fail

simpleEscape
	bsr.w decodeTextEscape

appendChar
	bsr.w appendTextScratchByte
	bne.w fail
	bra.s charLoop

operandEnd
	cmpi.w #3, d5
	bne.w operandSeparator
	movem.l d2/d4-d6/a2, -(sp)
	lea OpasmTextScratch.l, a0
	adda.l d4, a0
	move.l OpasmTextScratchLen, d0
	sub.l d4, d0
	jsr text_encoding.encodeBytesV1
	movem.l (sp)+, d2/d4-d6/a2
	beq.s quotedEncodeOk
	bra.w fail
quotedEncodeOk
	add.l d4, d1
	move.l d1, OpasmTextScratchLen
operandSeparator
	bsr.w skipPartWhitespace
	tst.l d2
	beq.w ok
	cmpi.b #',', (a2)
	bne.w fail
	addq.l #1, a2
	subq.l #1, d2
	bra.w operandStart

numericOperand
	tst.l d2
	beq.s numericEnd
	cmpi.b #',', (a2)
	beq.s numericEnd
	addq.l #1, a2
	subq.l #1, d2
	bra.s numericOperand

numericEnd
	bsr.w readCommaOperandValueForStatement
.ifdef OPFORGE_MACRO_EXPR_EVAL_PROBE
	; Instrumentation point: numeric `.byte` operand parsed from the bounded
	; statement metadata, before the original failure branch consumes D0.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D6/A0 by the balanced save/restore below; D3 is
	; preserved by debugEventU32x4 while carrying the parsed value.
	; SR/CCR preserved: restored before the following BNE consumes parse status.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: the saved CCR is restored before
	; the original branch, and this code exists only in the diagnostic build.
	; Removal/stabilization plan: remove after the numeric list boundary is
	; confirmed by the authoritative macro fixture.
	move.w ccr, -(sp)
	movem.l d0-d2/d4-d6/a0, -(sp)
	move.l d6, d1
	move.l d0, d4
	moveq #0, d5
	move.w d7, d5
	moveq #0, d6
	move.w d1, d6
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_EXPR_RESULT
	movem.l (sp)+, d0-d2/d4-d6/a0
	move.w (sp)+, ccr
.endif
	bne.w fail
	cmpi.l #$000000ff, d3
	bhi.w fail
	move.b d3, d1
	bsr.w appendTextScratchByte
	bne.w fail
	tst.l d2
	beq.w ok
	addq.l #1, a2
	subq.l #1, d2
	bra.w operandStart

ok
	cmpi.w #3, d5
	beq.s parsedOk
	lea OpasmTextScratch.l, a0
	move.l OpasmTextScratchLen, d0
	jsr text_encoding.encodeBytesV1
	bne.s fail
	move.l d1, OpasmTextScratchLen
parsedOk
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
	bsr.w serviceFramePtr
	jsr operand_eval.prepareSelectedRequestV1
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
	bsr.w serviceFramePtr
	movea.l a0, a1
	movea.l a2, a0
	move.w d7, d1
	jsr operand_eval.prepareExpressionRequestV1
	bne.s return
	move.w d1, OpasmDriverEvalRequestLen
	tst.l d0

return
	movem.l (sp)+, d1/a0-a2
	rts
	.bend  ; prepareEvaluateExpressionRequest

prepareEvaluateExpressionExtension	.block
	movem.l d0-d1/a0-a2, -(sp)
	bsr.w serviceFramePtr
	moveq #0, d0
	move.w OpasmDriverEvalRequestLen, d0
	jsr operand_eval.prepareExpressionExtensionV1
	movem.l (sp)+, d0-d1/a0-a2
	rts
	.bend  ; prepareEvaluateExpressionExtension

; @opforge-owner: opasm.amigaos.operand_eval
; @opforge-slice: documentation/plans/slices/native-porting-slice-module-visibility.toml
; @opforge-role: delegation
prepareSelectedEvaluateExpressionExtension	.block
	movem.l d0-d1/a0-a2, -(sp)
	jsr eng.opasmEngineResetLastResolvedLabelV1
	bsr.w serviceFramePtr
	moveq #0, d0
	move.w OpasmDriverEvalRequestLen, d0
	moveq #0, d1
	move.w d6, d1
	jsr operand_eval.prepareSelectedExtensionV1
	movem.l (sp)+, d0-d1/a0-a2
	rts
	.bend  ; prepareSelectedEvaluateExpressionExtension

prepareDirectiveEvaluateExpressionExtension	.block
	movem.l a0, -(sp)
	bsr.w serviceFramePtr
	jsr operand_eval.prepareDirectiveExpressionExtensionV1
	movem.l (sp)+, a0
	rts
	.bend  ; prepareDirectiveEvaluateExpressionExtension

readEvaluateExpressionValue	.block
	movem.l d0-d1/a0, -(sp)
	bsr.w serviceEvalExtensionPtr
	move.l 16(a0), d3
.ifdef OPFORGE_MACRO_EXPR_EVAL_PROBE
	; Instrumentation point: expression result read from the service extension.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D6/A0 by the balanced save/restore below; D3 is
	; restored by debugEventU32x4 after being copied into the event payload.
	; SR/CCR preserved: restored before the following BNE consumes D3's result.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: the saved CCR is restored before
	; the original branch, and this code exists only in the diagnostic build.
	; Removal/stabilization plan: remove after the expression-return invariant is
	; established by the authoritative FS-UAE fixture.
	move.w ccr, -(sp)
	movem.l d0-d2/d4-d6/a0, -(sp)
	move.l d3, d1
	moveq #0, d2
	moveq #0, d3
	moveq #0, d4
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_EXPR_RESULT
	movem.l (sp)+, d0-d2/d4-d6/a0
	move.w (sp)+, ccr
.endif
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
	blo.w fail
	cmpi.b #'V', (a0)+
	bne.w fail
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
	move.w d7, abi.OPASM_EVENT_STMT_INDEX(a0)
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

InText
	.byte "in", 0

AlignMnemonicText
	.byte "align", 0

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

ModuleMnemonicText
	.byte "module", 0

EndmoduleMnemonicText
	.byte "endmodule", 0

DriverSelectorUnknownRawText
	.byte "OTR901: selector unknown mnemonic", 0

DriverSelectorUnsupportedRawText
	.byte "OTR901: selector unsupported address", 0

DriverSelectorOperandRawText
	.byte "OTR901: selector operand error", 0

DriverSelectedOperandCompileRawText
	.byte "OTR901: selected operand compile failed", 0

LayoutResolveFailureText
	.byte "layout finalize: mapped label no longer resolves", 0
LayoutResolveMismatchText
	.byte "layout finalize: mapped label resolves to the wrong value", 0
LayoutMapRangeFailureText
	.byte "layout finalize: reachable label matches no mapped logical PC range", 0
LayoutMapEmptyFailureText
	.byte "layout finalize: no structural section map was retained", 0

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
OpasmRepeatBforBasePc
	.res long, OPASM_REPEAT_STACK_CAPACITY
OpasmDriverScopedRepeatLabel
	.res word, 1
OpasmDriverScopedRepeatValue
	.res long, 1

OpasmMatchValue
	.res long, 1

OpasmDriverImageBaseSeen
	.res word, 1

	.endsection
	.endmodule
