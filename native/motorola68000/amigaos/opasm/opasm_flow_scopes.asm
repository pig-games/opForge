; Native opasm scope-flow and scoped-symbol helpers.

	.module opasm.amigaos.flow_scopes
	.cpu 68020
	.use opasm.amigaos.engine as eng

OPASM_SCOPE_DEPTH_CAPACITY = 8
; Recursive `.use` tokenization interleaves the canonical 18-edge module graph
; in the retained statement stream. Keep module parents independent from the
; ordinary block/namespace scope stack and reject a thirty-third parent.
OPASM_MODULE_PARENT_DEPTH_CAPACITY = 32
OPASM_SCOPE_NAME_CAPACITY = 64
; The exact Rust-expanded product reaches 107 bytes after module, nested block,
; and local-label qualification. Retain its terminating NUL as well.
OPASM_SCOPE_TEXT_CAPACITY = 108
OPASM_SCOPE_KIND_MODULE = 1
OPASM_SCOPE_KIND_BLOCK = 2
OPASM_SCOPE_KIND_NAMESPACE = 3

	.section code, kind=code
	.pub

; Reset bounded block/namespace state for one assembly pass.
; Outputs: D0 = 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
resetStateV1	.block
	moveq #0, d0
	move.w d0, ScopeDepth.l
	move.w d0, ModuleParentDepth.l
	move.w d0, RootModuleNameSet.l
	moveq #-1, d0
	move.l d0, ActiveModuleStatementIndex.l
	rts
	.bend  ; resetStateV1

; Return the active top-level module name from its retained source statement.
; Outputs: A1/D1 = name slice, or D1 = 0 when no module is active.
; Clobbers: D0-D2/A0-A1/CCR.
activeModuleNameV1	.block
	moveq #0, d1
	move.l ActiveModuleStatementIndex.l, d0
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
	; The source-text getter uses D1 for its table offset.  Module-name length is
	; a new result and must start at zero for every statement, exactly like
	; Rust's freshly parsed module identifier.
	moveq #0, d1
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
; Inputs: D7.L = current statement index.
; Outputs: D0 = status; D1 = 1; D2.L = next statement index.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
beginBlockScopeV1	.block
	move.l d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	moveq #OPASM_SCOPE_KIND_BLOCK, d6
	bsr.w pushText
	bne.s fail
	move.l d7, d2
	addq.l #1, d2
	moveq #1, d1
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; beginBlockScopeV1

; Apply the current `.namespace` scope directive and skip it.
; Inputs: D7.L = current statement index.
; Outputs: D0 = status; D1 = 1; D2.L = next statement index.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
beginNamespaceScopeV1	.block
	moveq #OPASM_SCOPE_KIND_NAMESPACE, d6
	bsr.w pushFromStatementOperand
	bne.s fail
	move.l d7, d2
	addq.l #1, d2
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
; Inputs: D7.L = current statement index.
; Outputs: D0 = status; D1 = 1; D2.L = next statement index on success.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
beginModuleScopeV1	.block
	move.w ScopeDepth.l, d0
	beq.s begin
	cmpi.w #1, d0
	bne.s moduleFail
	moveq #0, d1
	move.w ModuleParentDepth.l, d1
	cmpi.w #OPASM_MODULE_PARENT_DEPTH_CAPACITY, d1
	bhs.s moduleFail
	move.l d1, d2
	lsl.l #2, d2
	lea ParentModuleStatementIndexStack.l, a0
	move.l ActiveModuleStatementIndex.l, 0(a0, d2.l)
	addq.w #1, d1
	move.w d1, ModuleParentDepth.l
begin
	moveq #0, d0
	move.w d0, ScopeDepth.l
	move.l d7, ActiveModuleStatementIndex.l
	moveq #OPASM_SCOPE_KIND_MODULE, d6
	bsr.w pushFromStatementOperand
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
	move.l d7, d2
	addq.l #1, d2
	moveq #1, d1
	moveq #0, d0
	rts
moduleFail
	moveq #1, d0
	rts
	.bend  ; beginModuleScopeV1

; Close one module root and restore the importer module root when recursive
; `.use` tokenization interleaved a dependency into the statement stream.
; Inputs: D7.L = current statement index.
; Outputs: D0 = status; D1 = 1; D2.L = next statement index.
; Clobbers: D0-D3/A0-A1/CCR.
; CCR: reflects D0 on return.
endModuleScopeV1	.block
	move.w ScopeDepth.l, d0
	cmpi.w #1, d0
	bne.s moduleEndFail
	move.l d7, -(sp)
	moveq #0, d0
	move.w d0, ScopeDepth.l
	move.w ModuleParentDepth.l, d0
	beq.s moduleEndRoot
	subq.w #1, d0
	move.w d0, ModuleParentDepth.l
	move.l d0, d1
	lsl.l #2, d1
	lea ParentModuleStatementIndexStack.l, a0
	move.l 0(a0, d1.l), d7
	move.l d7, ActiveModuleStatementIndex.l
	moveq #OPASM_SCOPE_KIND_MODULE, d6
	bsr.w pushFromStatementOperand
	move.l (sp)+, d7
	tst.l d0
	bne.s moduleEndFail
	bra.s moduleEndDone
moduleEndRoot
	move.l (sp)+, d7
	moveq #-1, d0
	move.l d0, ActiveModuleStatementIndex.l
moduleEndDone
	move.l d7, d2
	addq.l #1, d2
	moveq #1, d1
	moveq #0, d0
	rts
moduleEndFail
	moveq #1, d0
	rts
	.bend  ; endModuleScopeV1

; Apply a scope close directive and skip it.
; Inputs: D7.L = current statement index.
; Outputs: D0 = status; D1 = 1; D2.L = next statement index.
; Clobbers: D0-D1/CCR.
; CCR: reflects D0 on return.
endScopeDirectiveV1	.block
	bsr.w popScope
	bne.s fail
	move.l d7, d2
	addq.l #1, d2
	moveq #1, d1
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; endScopeDirectiveV1

; Return Rust ScopeStack::has_block_deeper_than(module_scope_depth) for the
; active native scope stack. The module root occupies depth zero; namespaces
; and repeat scopes do not satisfy the block-only predicate.
; Outputs: D0 = 1 when a block exists below the module root, 0 otherwise.
hasBlockDeeperThanModuleV1	.block
	moveq #0, d0
	moveq #1, d1
	move.w ScopeDepth.l, d2
scanKind
	cmp.w d2, d1
	bhs.s kindDone
	lea ScopeKinds.l, a0
	cmpi.b #OPASM_SCOPE_KIND_BLOCK, 0(a0, d1.l)
	beq.s blockFound
	addq.w #1, d1
	bra.s scanKind
blockFound
	moveq #1, d0
kindDone
	tst.l d0
	rts
	.bend  ; hasBlockDeeperThanModuleV1

; Rewrite the current symbol directive's label with active scope prefixes.
; Inputs: D7.L = statement index.
; Outputs: D0 = 0 on success, 1 on qualification failure.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
qualifyStatementLabelIfScopedV1	.block
	; Rust's ScopeStack::qualify uses every active segment. Native recursive
	; module replay retains the root separately per statement, so rebuild the
	; equivalent name from that stable owner plus live block/namespace suffixes.
	move.l d7, d0
	jsr eng.opasmEngineGetStatementOwnerTextV1
	tst.l d0
	bne.s haveStatementOwner
	bsr.w activeModuleNameV1
	beq.w checkScopeDepth
	movea.l a1, a0
	move.l d1, d0
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
	addq.l #1, d4
	moveq #1, d3
	moveq #0, d2
	move.w ScopeDepth.l, d2
statementOwnerScopeLoop
	cmp.w d2, d3
	bhs.s statementOwnerPrefixReady
	move.l d3, d0
	lsl.l #6, d0
	lea ScopeNames.l, a0
	adda.l d0, a0
statementOwnerScopeChar
	move.b (a0)+, d5
	beq.s statementOwnerScopeEnd
	cmpi.l #OPASM_SCOPE_TEXT_CAPACITY - 1, d4
	bhs.w ownerCapacityFail
	move.b d5, (a1)+
	addq.l #1, d4
	bra.s statementOwnerScopeChar
statementOwnerScopeEnd
	cmpi.l #OPASM_SCOPE_TEXT_CAPACITY - 1, d4
	bhs.w ownerCapacityFail
	move.b #'.', (a1)+
	addq.l #1, d4
	addq.w #1, d3
	bra.s statementOwnerScopeLoop
statementOwnerPrefixReady
	move.l d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	tst.l d0
	beq.w ok
	move.l d0, d5
	move.l d4, d1
	add.l d5, d1
	cmpi.l #OPASM_SCOPE_TEXT_CAPACITY - 1, d1
	bhi.w ownerCapacityFail
	movea.l a0, a2
	lea ScopeScratch.l, a1
	adda.l d4, a1
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
checkScopeDepth
	tst.w ScopeDepth.l
	beq.w ok
statementOwnerReady
	move.l d7, d0
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
	move.l d7, d0
	movea.l a1, a0
	move.l d5, d1
	jsr eng.opasmEngineSetStatementLabelTextV1
	tst.l d0
	bne.s setFail
	move.l d7, d0
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
	move.w ScopeDepth.l, d1
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
	; Every Rust lexical-scope candidate is built from the original token.
	; buildTextAtDepth consumes A2/D6 while copying that token, so retain both
	; across an unsuccessful inner-scope lookup before trying the parent scope.
	movem.l d6/a2, -(sp)
	bsr.w buildTextAtDepth
	movem.l (sp)+, d6/a2
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
; Inputs: D7.L = statement index.
; Outputs: D0 = 0 on success, 1 on missing/over-capacity name.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
pushFromStatementLabel	.block
	move.l d7, d0
	jsr eng.opasmEngineGetStatementLabelTextV1
	moveq #OPASM_SCOPE_KIND_BLOCK, d6
	bsr.w pushText
	rts
	.bend  ; pushFromStatementLabel

; Push the parser-retained module owner as one scope component.
; Inputs: D7.L = module statement index.
; Outputs: D0 = 0 on success, 1 on missing/over-capacity name.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
pushFromStatementOwner	.block
	move.l d7, d0
	jsr eng.opasmEngineGetStatementOwnerTextV1
	moveq #OPASM_SCOPE_KIND_MODULE, d6
	bsr.w pushText
	rts
	.bend  ; pushFromStatementOwner

; Push the first operand token of the current `.namespace` as one scope component.
; Inputs: D7.L = statement index; D6.B = OPASM_SCOPE_KIND_*.
; Outputs: D0 = 0 on success, 1 on missing/over-capacity name.
; Clobbers: D0-D5/A0-A2/CCR.
; CCR: reflects D0 on return.
pushFromStatementOperand	.block
	move.w d6, -(sp)
	suba.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	move.l d7, d0
	movea.l sp, a0
	jsr eng.opasmEngineGetStatementTextMetadataV1
	bne.s fail
	movea.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_PTR(sp), a0
	move.l eng.OPASM_ENGINE_STMT_TEXT_OPERAND_LEN(sp), d0
	beq.s fail
	move.w eng.OPASM_ENGINE_STMT_TEXT_BYTES(sp), d6
	bsr.w pushText
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	addq.l #2, sp
	rts
fail
	adda.l #eng.OPASM_ENGINE_STMT_TEXT_BYTES, sp
	addq.l #2, sp
	moveq #1, d0
	rts
	.bend  ; pushFromStatementOperand

; Push one identifier token onto the bounded scope stack.
; Inputs: A0/D0 = name text/length; D6.B = OPASM_SCOPE_KIND_*.
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
	lea ScopeKinds.l, a0
	move.b d6, 0(a0, d2.l)
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
	.res long, 1

RootModuleNameSet
	.res word, 1

RootModuleName
	.res byte, OPASM_SCOPE_NAME_CAPACITY

ScopeNames
	.res byte, OPASM_SCOPE_DEPTH_CAPACITY * OPASM_SCOPE_NAME_CAPACITY

ScopeKinds
	.res byte, OPASM_SCOPE_DEPTH_CAPACITY

ParentModuleStatementIndexStack
	.res long, OPASM_MODULE_PARENT_DEPTH_CAPACITY

ScopeScratch
	.res byte, OPASM_SCOPE_TEXT_CAPACITY

	.endsection

	.endmodule
