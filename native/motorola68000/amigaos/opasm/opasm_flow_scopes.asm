; Native opasm scope-flow and scoped-symbol helpers.

	.module opasm.amigaos.flow_scopes
	.cpu 68020
	.use opasm.amigaos.engine as eng

OPASM_SCOPE_DEPTH_CAPACITY = 8
OPASM_SCOPE_NAME_CAPACITY = 64
OPASM_SCOPE_TEXT_CAPACITY = 64

	.section code, kind=code
	.pub

; Reset bounded block/namespace state for one assembly pass.
; Outputs: D0 = 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
resetStateV1	.block
	clr.w ScopeDepth.l
	clr.w ModuleParentDepth.l
	clr.w RootModuleNameSet.l
	move.w #-1, ActiveModuleStatementIndex.l
	moveq #0, d0
	rts
	.bend  ; resetStateV1

; Return the active top-level module name from its retained source statement.
; Outputs: A1/D1 = name slice, or D1 = 0 when no module is active.
; Clobbers: D0-D2/A0-A1/CCR.
activeModuleNameV1	.block
	moveq #0, d1
	moveq #0, d0
	move.w ActiveModuleStatementIndex.l, d0
	bmi.s activeModuleDone
	jsr eng.opasmEngineGetStatementSourceTextV1
	tst.l d0
	beq.s activeModuleDone
	bsr.w skipWhitespace
activeModuleMnemonic
	tst.l d0
	beq.s activeModuleDone
	move.b (a0), d2
	cmpi.b #' ', d2
	beq.s activeModuleOperand
	cmpi.b #9, d2
	beq.s activeModuleOperand
	addq.l #1, a0
	subq.l #1, d0
	bra.s activeModuleMnemonic
activeModuleOperand
	bsr.w skipWhitespace
	movea.l a0, a1
activeModuleLen
	tst.l d0
	beq.s activeModuleDone
	cmpi.l #OPASM_SCOPE_NAME_CAPACITY - 1, d1
	bhs.s activeModuleDone
	move.b 0(a1, d1.l), d2
	cmpi.b #' ', d2
	beq.s activeModuleDone
	cmpi.b #9, d2
	beq.s activeModuleDone
	cmpi.b #';', d2
	beq.s activeModuleDone
	addq.l #1, d1
	subq.l #1, d0
	bra.s activeModuleLen
activeModuleDone
	tst.l d1
	rts
	.bend  ; activeModuleNameV1

; Return the top-level module name retained for listing/map presentation after
; the module scope itself has closed at the end of a pass.
; Outputs: A0/D0 = name slice, or D0 = 0 when the source has no root module.
rootModuleNameV1	.block
	lea RootModuleName.l, a0
	moveq #0, d0
	tst.w RootModuleNameSet.l
	beq.s done
len
	cmpi.l #OPASM_SCOPE_NAME_CAPACITY, d0
	bhs.s done
	tst.b 0(a0, d0.l)
	beq.s done
	addq.l #1, d0
	bra.s len
done
	tst.l d0
	rts
	.bend  ; rootModuleNameV1

; Apply the current `.block` scope directive and skip it.
; Inputs: D7.W = current statement index.
; Outputs: D0 = status; D1 = 1; D2.W = next statement index.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
beginBlockScopeV1	.block
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	bsr.w pushText
	bne.s fail
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; beginBlockScopeV1

; Apply the current `.namespace` scope directive and skip it.
; Inputs: D7.W = current statement index.
; Outputs: D0 = status; D1 = 1; D2.W = next statement index.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
beginNamespaceScopeV1	.block
	bsr.w pushFromStatementOperand
	bne.s fail
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; beginNamespaceScopeV1

; Begin one top-level module-local scope.  Native CLI modules are independent
; compilation roots, so a preceding module cannot contribute a prefix to the
; next module's local symbols.
; Inputs: D7.W = current statement index.
; Outputs: D0 = 0 on success, 1 on malformed/capacity failure.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
beginModuleScopeV1	.block
	move.w ScopeDepth.l, d0
	beq.s begin
	cmpi.w #1, d0
	bne.s moduleFail
	tst.w ModuleParentDepth.l
	bne.s moduleFail
	move.w ActiveModuleStatementIndex.l, d3
	move.w d3, ParentModuleStatementIndex.l
	move.w #1, ModuleParentDepth.l
begin
	clr.w ScopeDepth.l
	move.w d7, ActiveModuleStatementIndex.l
	bsr.w beginNamespaceScopeV1
	bne.s moduleFail
	tst.w RootModuleNameSet.l
	bne.s moduleDone
	lea ScopeNames.l, a0
	lea RootModuleName.l, a1
	moveq #OPASM_SCOPE_NAME_CAPACITY - 1, d3
copyRoot
	move.b (a0)+, d0
	move.b d0, (a1)+
	beq.s rootCopied
	dbra d3, copyRoot
	clr.b -(a1)
rootCopied
	move.w #1, RootModuleNameSet.l
moduleDone
	moveq #0, d0
	rts
moduleFail
	moveq #1, d0
	rts
	.bend  ; beginModuleScopeV1

; Close one module root and restore the importer module root when recursive
; `.use` tokenization interleaved a dependency into the statement stream.
; Inputs: D7.W = current statement index.
; Outputs: D0 = status; D1 = 1; D2.W = next statement index.
; Clobbers: D0-D3/A0-A1/CCR.
; CCR: reflects D0 on return.
endModuleScopeV1	.block
	cmpi.w #1, ScopeDepth.l
	bne.s moduleEndFail
	move.w d7, -(sp)
	clr.w ScopeDepth.l
	tst.w ModuleParentDepth.l
	beq.s moduleEndRoot
	subq.w #1, ModuleParentDepth.l
	move.w ParentModuleStatementIndex.l, d7
	move.w d7, ActiveModuleStatementIndex.l
	bsr.w pushFromStatementOperand
	move.w (sp)+, d7
	tst.l d0
	bne.s moduleEndFail
	bra.s moduleEndDone
moduleEndRoot
	move.w (sp)+, d7
	move.w #-1, ActiveModuleStatementIndex.l
moduleEndDone
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	moveq #0, d0
	rts
moduleEndFail
	moveq #1, d0
	rts
	.bend  ; endModuleScopeV1

; Apply a scope close directive and skip it.
; Inputs: D7.W = current statement index.
; Outputs: D0 = status; D1 = 1; D2.W = next statement index.
; Clobbers: D0-D1/CCR.
; CCR: reflects D0 on return.
endScopeDirectiveV1	.block
	bsr.w popScope
	bne.s fail
	move.w d7, d2
	addq.w #1, d2
	moveq #1, d1
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; endScopeDirectiveV1

; Rewrite the current symbol directive's label with active scope prefixes.
; Inputs: D7.W = statement index.
; Outputs: D0 = 0 on success, 1 on qualification failure.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
qualifyStatementLabelIfScopedV1	.block
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementOwnerTextV1
	tst.l d0
	bne.s haveStatementOwner
	tst.w ScopeDepth.l
	beq.w ok
	bra.s statementOwnerReady
haveStatementOwner
	cmpi.l #OPASM_SCOPE_NAME_CAPACITY - 1, d0
	bhi.w ownerCapacityFail
	move.l d0, d4
	move.l d0, d3
	lea ScopeScratch.l, a1
statementOwnerCopy
	tst.l d3
	beq.s statementOwnerCopied
	move.b (a0)+, (a1)+
	subq.l #1, d3
	bra.s statementOwnerCopy
statementOwnerCopied
	move.b #'.', (a1)+
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	tst.l d0
	beq.w ok
	move.l d0, d5
	move.l d4, d1
	addq.l #1, d1
	add.l d5, d1
	cmpi.l #OPASM_SCOPE_NAME_CAPACITY - 1, d1
	bhi.w ownerCapacityFail
	movea.l a0, a2
	lea ScopeScratch.l, a1
	adda.l d4, a1
	addq.l #1, a1
statementLabelCopy
	tst.l d5
	beq.s statementLabelCopied
	move.b (a2)+, (a1)+
	subq.l #1, d5
	bra.s statementLabelCopy
statementLabelCopied
	clr.b (a1)
	lea ScopeScratch.l, a0
	bra.s qualifiedTextReady
statementOwnerReady
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	tst.l d0
	beq.s ok
	move.l d0, d5
	movea.l a0, a2
	moveq #0, d2
	move.w ScopeDepth.l, d2
	movea.l a2, a0
	move.l d5, d0
	bsr.w buildTextAtDepth
	bne.s buildFail
qualifiedTextReady
	move.l d1, d5
	movea.l a0, a1
	moveq #0, d0
	move.w d7, d0
	movea.l a1, a0
	move.l d5, d1
	jsr eng.opasmEngineSetStatementLabelTextV1
	tst.l d0
	bne.s setFail
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	cmp.l d5, d0
	bne.s lengthFail
	lea ScopeScratch.l, a1
verifyQualified
	tst.l d0
	beq.s ok
	cmpm.b (a0)+, (a1)+
	bne.s verifyFail
	subq.l #1, d0
	bra.s verifyQualified
ok
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
ownerCapacityFail
	moveq #2, d0
	rts
buildFail
	moveq #3, d0
	rts
setFail
	moveq #4, d0
	rts
lengthFail
	moveq #5, d0
	rts
verifyFail
	moveq #6, d0
	rts
	.bend  ; qualifyStatementLabelIfScopedV1

; Resolve an unqualified token through active scope prefixes before globals.
; Inputs: A0/D0 = trimmed token text/length.
; Outputs: D0 = 0 on success, 1 when no scoped value matches; D3 on success.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
resolveLabelValueV1	.block
	movem.l d1-d2/d4-d6/a0-a2, -(sp)
	tst.w ScopeDepth.l
	beq.s fail
	movea.l a0, a2
	move.l d0, d6
	movea.l a0, a1
	move.l d0, d4
dotScan
	tst.l d4
	beq.s noDot
	cmpi.b #'.', (a1)+
	beq.s fail
	subq.l #1, d4
	bra.s dotScan
noDot
	moveq #0, d2
	move.w ScopeDepth.l, d2
scan
	tst.w d2
	beq.s fail
	movea.l a2, a0
	move.l d6, d0
	bsr.w buildTextAtDepth
	bne.s next
	move.l d1, d0
	jsr eng.opasmEngineResolveLabelValueV1
	beq.s ok
next
	subq.w #1, d2
	bra.s scan
ok
	movem.l (sp)+, d1-d2/d4-d6/a0-a2
	moveq #0, d0
	rts
fail
	movem.l (sp)+, d1-d2/d4-d6/a0-a2
	moveq #1, d0
	rts
	.bend  ; resolveLabelValueV1

; Return the unqualified alias of a label owned by the complete active scope.
; Inputs: A0/D0 = null-terminated qualified label text/length.
; Outputs: A0/D0 = alias text/length, or D0 = 0 when the label is not owned by
;          the complete active scope.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
activeLabelAliasV1	.block
	movem.l d1-d6/a1-a2, -(sp)
	movea.l a0, a1
	move.l d0, d5
	moveq #0, d6
	moveq #0, d4
	move.w ScopeDepth.l, d4
	beq.s none

scopeLoop
	move.l d6, d3
	lsl.l #6, d3
	lea ScopeNames.l, a2
	adda.l d3, a2

scopeChar
	move.b (a2)+, d1
	beq.s scopeEnd
	tst.l d5
	beq.s none
	cmp.b (a1)+, d1
	bne.s none
	subq.l #1, d5
	bra.s scopeChar

scopeEnd
	tst.l d5
	beq.s none
	cmpi.b #'.', (a1)+
	bne.s none
	subq.l #1, d5
	addq.w #1, d6
	cmp.w d4, d6
	blo.s scopeLoop
	tst.l d5
	beq.s none
	movea.l a1, a0
	move.l d5, d0
	bra.w return

none
	moveq #0, d0

return
	movem.l (sp)+, d1-d6/a1-a2
	rts
	.bend  ; activeLabelAliasV1

	.priv

; Push the label attached to the current `.block` as one scope component.
; Inputs: D7.W = statement index.
; Outputs: D0 = 0 on success, 1 on missing/over-capacity name.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
pushFromStatementLabel	.block
	moveq #0, d0
	move.w d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	bsr.w pushText
	rts
	.bend  ; pushFromStatementLabel

; Push the first operand token of the current `.namespace` as one scope component.
; Inputs: D7.W = statement index.
; Outputs: D0 = 0 on success, 1 on missing/over-capacity name.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
pushFromStatementOperand	.block
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #0, d0
	move.w d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.s fail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	beq.s fail
	bsr.w pushText
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	rts
fail
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	moveq #1, d0
	rts
	.bend  ; pushFromStatementOperand

; Push one identifier token onto the bounded scope stack.
; Inputs: A0/D0 = name text/length.
; Outputs: D0 = 0 on success, 1 on malformed/capacity failure.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
pushText	.block
	bsr.w skipWhitespace
	moveq #0, d2
	move.w ScopeDepth.l, d2
	cmpi.w #OPASM_SCOPE_DEPTH_CAPACITY, d2
	bhs.s fail
	move.l d2, d3
	lsl.l #6, d3
	lea ScopeNames.l, a1
	adda.l d3, a1
	moveq #OPASM_SCOPE_NAME_CAPACITY - 1, d4
copy
	tst.l d0
	beq.s finish
	move.b (a0), d1
	cmpi.b #' ', d1
	beq.s finish
	cmpi.b #9, d1
	beq.s finish
	cmpi.b #';', d1
	beq.s finish
	tst.w d4
	beq.s fail
	move.b (a0)+, (a1)+
	subq.l #1, d0
	subq.w #1, d4
	bra.s copy
finish
	clr.b (a1)
	move.w ScopeDepth.l, d2
	addq.w #1, d2
	move.w d2, ScopeDepth.l
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; pushText

; Pop one active scope component.
; Outputs: D0 = 0 on success, 1 when no scope is active.
; Clobbers: D0-D1/CCR.
; CCR: reflects D0 on return.
popScope	.block
	tst.w ScopeDepth.l
	beq.s fail
	move.w ScopeDepth.l, d1
	subq.w #1, d1
	move.w d1, ScopeDepth.l
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; popScope

; Build `scope[0]....scope[depth-1].name` into the fixed scratch buffer.
; Inputs: A0/D0 = raw name; D2.W = requested scope depth.
; Outputs: D0 = 0 on success; A0/D1 = scratch text/length.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
buildTextAtDepth	.block
	movea.l a0, a2
	move.l d0, d6
	lea ScopeScratch.l, a1
	movea.l a1, a0
	moveq #OPASM_SCOPE_TEXT_CAPACITY - 1, d4
	clr.l d1
	clr.w d3
scopeLoop
	cmp.w d2, d3
	bhs.s rawName
	move.l d3, d5
	lsl.l #6, d5
	lea ScopeNames.l, a1
	adda.l d5, a1
	tst.b (a1)
	beq.s nextScope
scopeChar
	move.b (a1)+, d5
	beq.s scopeEnd
	tst.w d4
	beq.s fail
	move.b d5, (a0)+
	addq.l #1, d1
	subq.w #1, d4
	bra.s scopeChar
scopeEnd
	tst.w d4
	beq.s fail
	move.b #'.', (a0)+
	addq.l #1, d1
	subq.w #1, d4
	addq.w #1, d3
	bra.s scopeLoop
nextScope
	addq.w #1, d3
	bra.s scopeLoop
rawName
	tst.l d6
	beq.s fail
rawLoop
	tst.l d6
	beq.s done
	tst.w d4
	beq.s fail
	move.b (a2)+, (a0)+
	addq.l #1, d1
	subq.w #1, d4
	subq.l #1, d6
	bra.s rawLoop
done
	clr.b (a0)
	lea ScopeScratch.l, a0
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; buildTextAtDepth

; Skip ASCII spaces and tabs in A0/D0.
; Inputs: A0/D0 = text/length.
; Outputs: A0/D0 = first non-whitespace byte/remaining length.
; Clobbers: CCR.
; CCR: reflects D0 on return.
skipWhitespace	.block
	tst.l d0
	beq.s done
	cmpi.b #' ', (a0)
	beq.s skip
	cmpi.b #9, (a0)
	bne.s done
skip
	addq.l #1, a0
	subq.l #1, d0
	bra.s skipWhitespace
done
	rts
	.bend  ; skipWhitespace

	.endsection

	.section bss, kind=bss

ScopeDepth
	.res word, 1

ModuleParentDepth
	.res word, 1

ActiveModuleStatementIndex
	.res word, 1

RootModuleNameSet
	.res word, 1

RootModuleName
	.res byte, OPASM_SCOPE_NAME_CAPACITY

ScopeNames
	.res byte, OPASM_SCOPE_DEPTH_CAPACITY * OPASM_SCOPE_NAME_CAPACITY

ParentModuleStatementIndex
	.res word, 1

ScopeScratch
	.res byte, OPASM_SCOPE_TEXT_CAPACITY

	.endsection

	.endmodule
