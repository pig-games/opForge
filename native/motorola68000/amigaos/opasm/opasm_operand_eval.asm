; Native opasm operand/evaluation request construction.

	.module opasm.amigaos.operand_eval
	.cpu 68020

	.use opasm.amigaos.callback_abi as abi
	.use opasm.amigaos.engine as eng
	.use opasm.amigaos.flow_scopes as scopes

; Rust evaluates against the complete session symbol table. Native keeps a
; fixed snapshot, so both the qualified source rows and their possible active
; lexical aliases must cover opasm's complete 16,384-label session domain.
SCOPED_SNAPSHOT_SOURCE_CAPACITY = 16384
SCOPED_SNAPSHOT_CAPACITY = 32768
; The expression bridge consumes the same fixed-width symbol rows as the
; engine label table. A smaller stride makes every row after the first visible
; at the wrong address and diverges from Rust's complete symbol-name lookup.
SCOPED_SNAPSHOT_NAME_BYTES = eng.LABEL_NAME_CAPACITY

	.section code, kind=code
	.pub

; Build one selected-instruction evaluation request.
; Inputs: D0.L = statement index; A0 = OPASM_SERVICE_* frame.
; Outputs: D0 = status; D1.W = request bytes on success.
prepareSelectedRequestV1	.block
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a0), a1
	jsr eng.prepareSelectedEvaluateRequestV1
	rts
	.bend  ; prepareSelectedRequestV1

; Build one textual expression evaluation request.
; Inputs: A0 = expression text; D0.L = text bytes; D1.L = statement index;
;         A1 = OPASM_SERVICE_* frame.
; Outputs: D0 = status; D1.W = request bytes on success.
prepareExpressionRequestV1	.block
	; Rust keeps the original expression text and resolves every identifier in
	; its lexical/import context. Retain the statement owner so the extension can
	; materialize the same request-local aliases without rewriting the expression.
	move.l d1, SelectedStatementIndex.l
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a1), a1
	jsr eng.prepareEvaluateExpressionRequestV1
	rts
	.bend  ; prepareExpressionRequestV1

; Append the evaluation extension to a prepared request.
; Inputs: A0 = OPASM_SERVICE_* frame; D0.W = request bytes.
; Outputs: D0 = engine status.
prepareExpressionExtensionV1	.block
	; Directive and selected-instruction expressions share Rust's request-local
	; lexical/import context. Materialize aliases once before ExprVM evaluation.
	moveq #1, d1
	bsr.w prepareExtensionCommon
	tst.l d0
	bne.s expressionExtensionReturn
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a1
	move.l #resolveExpressionSymbolV1, 28(a1)
expressionExtensionReturn
	rts
	.bend  ; prepareExpressionExtensionV1

; Append the evaluation extension and imported aliases for a selected CPU
; instruction request. The original operand text remains authoritative.
; Inputs: A0 = OPASM_SERVICE_* frame; D0.W = request bytes;
;         D1.L = stored statement index.
; Outputs: D0 = engine status.
prepareSelectedExtensionV1	.block
	move.l d1, SelectedStatementIndex.l
	moveq #1, d1
	bsr.w prepareExtensionCommon
	tst.l d0
	bne.s selectedExtensionReturn
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a1
	move.l #resolveSelectedSymbolV1, 28(a1)
	; Rust defers an unstable explicitly sized branch during stabilization only
	; when its nearest binding can still change inside a lexical block. Native's
	; layout-only retry supplies that signal; the distinct final emission pass
	; resolves the converged target and emits the actual displacement.
	move.l a1, -(sp)
	jsr scopes.hasBlockDeeperThanModuleV1
	tst.l d0
	beq.s selectedDeferReady
	jsr eng.opasmEngineIsLayoutStabilizationV1
selectedDeferReady
	movea.l (sp)+, a1
	move.l d0, 32(a1)
	moveq #0, d0
selectedExtensionReturn
	rts
	.bend  ; prepareSelectedExtensionV1

; Classify the symbol provenance of one already parsed directive expression.
; Rust performs this decision over its Expr tree. Native retains the original
; bounded source span, so this scanner projects the same architecture-neutral
; facts needed by the executable-Hunk contract: one PC-backed target, absolute
; value symbols, non-absolute value symbols, and the outer binary operator.
; Numeric spelling, byte order, and CPU syntax remain outside this routine.
; @opforge-owner: opasm.amigaos.operand_eval
; @opforge-slice: documentation/plans/slices/native-porting-slice-hunk-fixup-relocation-v1.toml
; @opforge-role: context
; Inputs: A0/D0 = expression text/bytes; D1.W = mode:
;   0 data-long (target plus absolute expression),
;   1 generic long (target plus/minus relocation-free literal),
;   2 absolute-constant provenance.
; Outputs: D0=0 accepted/absolute, 1 unsupported/non-absolute;
;          D1=1 and D2.W=target label index when relocation is retained.
; Clobbers: D0-D2/CCR. Preserves D3-D7/A0-A3.
classifyAbsoluteRelocationExpressionV1	.block
	movem.l d3-d7/a0-a3, -(sp)
	move.w d1, RelocScanMode.l
	clr.w RelocScanTargetCount.l
	move.w #$ffff, RelocScanTargetIndex.l
	clr.w RelocScanAbsoluteSymbolCount.l
	clr.w RelocScanNonAbsoluteSymbolCount.l
	clr.w RelocScanRootPlusCount.l
	clr.w RelocScanRootMinusCount.l
	clr.w RelocScanRootOtherCount.l
	move.w #$7fff, RelocScanRootDepth.l
	move.l #-1, RelocScanFirstRootMinusOffset.l
	clr.l RelocScanTargetOffset.l
	clr.w RelocScanParenDepth.l
	move.w #1, RelocScanExpectOperand.l
	movea.l a0, a2
	move.l d0, d6
	movea.l a0, a3

relocScanLoop
	tst.l d6
	beq.w relocScanDecide
	moveq #0, d3
	move.b (a2), d3
	cmpi.b #' ', d3
	beq.w relocScanAdvance
	cmpi.b #9, d3
	beq.w relocScanAdvance
	cmpi.b #';', d3
	beq.w relocScanDecide
	cmpi.b #'"', d3
	beq.w relocScanQuoted
	cmpi.b #39, d3
	beq.w relocScanQuoted
	cmpi.b #'(', d3
	beq.w relocScanOpen
	cmpi.b #'[', d3
	beq.w relocScanOpen
	cmpi.b #'{', d3
	beq.w relocScanOpen
	cmpi.b #')', d3
	beq.w relocScanClose
	cmpi.b #']', d3
	beq.w relocScanClose
	cmpi.b #'}', d3
	beq.w relocScanClose
	cmpi.b #'$', d3
	beq.w relocScanNumber
	cmpi.b #'%', d3
	beq.w relocScanNumber
	cmpi.b #'0', d3
	blo.s relocScanIdentifierStart
	cmpi.b #'9', d3
	bls.w relocScanNumber

relocScanIdentifierStart
	move.b d3, d4
	ori.b #$20, d4
	cmpi.b #'a', d4
	blo.s relocScanIdentifierPunctuation
	cmpi.b #'z', d4
	bls.w relocScanIdentifier
relocScanIdentifierPunctuation
	cmpi.b #'_', d3
	beq.w relocScanIdentifier
	cmpi.b #'.', d3
	beq.w relocScanIdentifier
	cmpi.b #'@', d3
	beq.w relocScanIdentifier

	; Only plus/minus need distinct treatment. Any other outer operator makes a
	; target-bearing expression unsupported, while operators inside an absolute
	; addend remain part of that addend just as they do in Rust's Expr subtree.
	tst.w RelocScanExpectOperand.l
	bne.w relocScanOperatorAdvance
	cmpi.b #'+', d3
	beq.s relocScanBinaryPlus
	cmpi.b #'-', d3
	beq.s relocScanBinaryMinus
	bsr.w relocScanRecordRootOther
	bra.w relocScanOperatorAdvance

relocScanBinaryPlus
	bsr.w relocScanRecordRootPlus
	bra.w relocScanOperatorAdvance

relocScanBinaryMinus
	bsr.w relocScanRecordRootMinus

relocScanOperatorAdvance
	move.w #1, RelocScanExpectOperand.l
	bra.w relocScanAdvance

relocScanOpen
	addq.w #1, RelocScanParenDepth.l
	move.w #1, RelocScanExpectOperand.l
	bra.w relocScanAdvance

relocScanClose
	tst.w RelocScanParenDepth.l
	beq.w relocScanMalformed
	subq.w #1, RelocScanParenDepth.l
	clr.w RelocScanExpectOperand.l
	bra.w relocScanAdvance

relocScanQuoted
	move.b d3, d5
	bsr.w relocScanAdvanceOne
relocScanQuoteLoop
	tst.l d6
	beq.w relocScanMalformed
	moveq #0, d3
	move.b (a2), d3
	cmp.b d5, d3
	beq.s relocScanQuoteDone
	cmpi.b #'\\', d3
	bne.s relocScanQuoteAdvance
	bsr.w relocScanAdvanceOne
	tst.l d6
	beq.w relocScanMalformed
relocScanQuoteAdvance
	bsr.w relocScanAdvanceOne
	bra.s relocScanQuoteLoop
relocScanQuoteDone
	clr.w RelocScanExpectOperand.l
	bra.w relocScanAdvance

relocScanNumber
	bsr.w relocScanAdvanceOne
relocScanNumberLoop
	tst.l d6
	beq.s relocScanNumberDone
	moveq #0, d3
	move.b (a2), d3
	move.b d3, d4
	ori.b #$20, d4
	cmpi.b #'0', d3
	blo.s relocScanNumberLetter
	cmpi.b #'9', d3
	bls.s relocScanNumberAdvance
relocScanNumberLetter
	cmpi.b #'a', d4
	blo.s relocScanNumberExtra
	cmpi.b #'f', d4
	bls.s relocScanNumberAdvance
relocScanNumberExtra
	cmpi.b #'_', d3
	bne.s relocScanNumberDone
relocScanNumberAdvance
	bsr.w relocScanAdvanceOne
	bra.s relocScanNumberLoop
relocScanNumberDone
	clr.w RelocScanExpectOperand.l
	bra.w relocScanLoop

relocScanIdentifier
	movea.l a2, a1
	moveq #0, d7
relocScanIdentifierLoop
	cmp.l d6, d7
	bhs.s relocScanIdentifierReady
	moveq #0, d3
	move.b 0(a2, d7.l), d3
	move.b d3, d4
	ori.b #$20, d4
	cmpi.b #'a', d4
	blo.s relocScanIdentifierDigit
	cmpi.b #'z', d4
	bls.s relocScanIdentifierContinue
relocScanIdentifierDigit
	cmpi.b #'0', d3
	blo.s relocScanIdentifierExtra
	cmpi.b #'9', d3
	bls.s relocScanIdentifierContinue
relocScanIdentifierExtra
	cmpi.b #'_', d3
	beq.s relocScanIdentifierContinue
	cmpi.b #'.', d3
	beq.s relocScanIdentifierContinue
	cmpi.b #'@', d3
	bne.s relocScanIdentifierReady
relocScanIdentifierContinue
	addq.l #1, d7
	bra.s relocScanIdentifierLoop

relocScanIdentifierReady
	tst.l d7
	beq.w relocScanMalformed
	; The generic relocation surface treats terminal .W/.L as a width member
	; on the base symbol. Data-long uses the parsed identifier unchanged.
	cmpi.w #1, RelocScanMode.l
	bne.s relocScanResolve
	cmpi.l #2, d7
	bls.s relocScanResolve
	moveq #0, d3
	move.b -2(a1, d7.l), d3
	cmpi.b #'.', d3
	bne.s relocScanResolve
	moveq #0, d3
	move.b -1(a1, d7.l), d3
	ori.b #$20, d3
	cmpi.b #'w', d3
	beq.s relocScanStripWidth
	cmpi.b #'l', d3
	bne.s relocScanResolve
relocScanStripWidth
	subq.l #2, d7

relocScanResolve
	jsr eng.opasmEngineResetLastResolvedLabelV1
	movea.l a1, a0
	move.l d7, d0
	jsr scopes.resolveLabelValueV1
	beq.s relocScanResolved
	movea.l a1, a0
	move.l d7, d0
	jsr eng.opasmEngineResolveLabelValueV1
	beq.s relocScanResolved
	movea.l a1, a0
	move.l d7, d0
	jsr eng.opasmEngineResolveUniqueLabelFinalComponentV1
	bne.s relocScanUnresolved

relocScanResolved
	jsr eng.opasmEngineGetLastResolvedLabelV1
	move.w d0, d5
	jsr eng.opasmEngineLastResolvedLabelIsTargetReferenceV1
	tst.l d0
	beq.s relocScanResolvedValue
	addq.w #1, RelocScanTargetCount.l
	move.w d5, RelocScanTargetIndex.l
	move.l a1, d0
	sub.l a3, d0
	move.l d0, RelocScanTargetOffset.l
	bra.s relocScanIdentifierAdvance

relocScanResolvedValue
	move.w d5, d0
	jsr eng.opasmEngineLabelIsAbsoluteConstantV1
	tst.l d0
	beq.s relocScanResolvedNonAbsolute
	addq.w #1, RelocScanAbsoluteSymbolCount.l
	bra.s relocScanIdentifierAdvance
relocScanResolvedNonAbsolute
	addq.w #1, RelocScanNonAbsoluteSymbolCount.l
	bra.s relocScanIdentifierAdvance

relocScanUnresolved
	addq.w #1, RelocScanNonAbsoluteSymbolCount.l

relocScanIdentifierAdvance
	adda.l d7, a2
	sub.l d7, d6
	clr.w RelocScanExpectOperand.l
	bra.w relocScanLoop

relocScanAdvance
	bsr.w relocScanAdvanceOne
	bra.w relocScanLoop

relocScanDecide
	tst.w RelocScanParenDepth.l
	bne.w relocScanMalformed
	moveq #1, d3
	moveq #0, d4
	move.w RelocScanTargetCount.l, d4
	moveq #0, d5
	move.w RelocScanNonAbsoluteSymbolCount.l, d5
	cmpi.w #2, RelocScanMode.l
	beq.s relocScanDecideAbsolute
	tst.w d4
	bne.s relocScanDecideTarget
	; A scalar expression is relocation-free only when every resolved symbol is
	; an absolute constant. Unknown/non-absolute value provenance fails closed.
	tst.w d5
	bne.s relocScanPublish
	moveq #0, d3
	bra.s relocScanPublish

relocScanDecideAbsolute
	tst.w d4
	bne.s relocScanPublish
	tst.w d5
	bne.s relocScanPublish
	moveq #0, d3
	bra.s relocScanPublish

relocScanDecideTarget
	cmpi.w #1, d4
	bne.s relocScanPublish
	tst.w d5
	bne.s relocScanPublish
	tst.w RelocScanRootOtherCount.l
	bne.s relocScanPublish
	cmpi.w #1, RelocScanMode.l
	beq.s relocScanDecideGeneric
	; Data-long's frozen v0.3 shape permits only addition of an absolute
	; constant expression; subtraction remains explicitly unsupported.
	tst.w RelocScanRootMinusCount.l
	bne.s relocScanPublish
	moveq #0, d3
	bra.s relocScanPublishTarget

relocScanDecideGeneric
	; Generic `.emit long` accepts only relocation-free literal addends, not
	; named constant-symbol addends. Subtraction requires the target on the left.
	tst.w RelocScanAbsoluteSymbolCount.l
	bne.s relocScanPublish
	tst.w RelocScanRootMinusCount.l
	beq.s relocScanGenericAccepted
	move.l RelocScanFirstRootMinusOffset.l, d0
	cmp.l RelocScanTargetOffset.l, d0
	bls.s relocScanPublish
relocScanGenericAccepted
	moveq #0, d3

relocScanPublishTarget
	moveq #1, d4
	moveq #0, d5
	move.w RelocScanTargetIndex.l, d5
	bra.s relocScanPublishValues

relocScanMalformed
	moveq #1, d3
relocScanPublish
	moveq #0, d4
	moveq #0, d5
	move.w #$ffff, d5
relocScanPublishValues
	move.l d3, d0
	move.l d4, d1
	move.l d5, d2
	movem.l (sp)+, d3-d7/a0-a3
	tst.l d0
	rts
	.bend  ; classifyAbsoluteRelocationExpressionV1

	.priv

; Advance the bounded relocation scanner by one byte.
relocScanAdvanceOne	.block
	addq.l #1, a2
	subq.l #1, d6
	rts
	.bend  ; relocScanAdvanceOne

; Keep only operators at the shallowest expression depth. This removes outer
; wrapper parentheses while retaining the true root operator.
relocScanPrepareRootOperator	.block
	move.w RelocScanParenDepth.l, d4
	cmp.w RelocScanRootDepth.l, d4
	bhi.s relocScanRootDeeper
	beq.s relocScanRootReady
	move.w d4, RelocScanRootDepth.l
	clr.w RelocScanRootPlusCount.l
	clr.w RelocScanRootMinusCount.l
	clr.w RelocScanRootOtherCount.l
	move.l #-1, RelocScanFirstRootMinusOffset.l
relocScanRootReady
	moveq #0, d0
	rts
relocScanRootDeeper
	moveq #1, d0
	rts
	.bend  ; relocScanPrepareRootOperator

relocScanRecordRootPlus	.block
	bsr.s relocScanPrepareRootOperator
	bne.s relocScanRootPlusReturn
	addq.w #1, RelocScanRootPlusCount.l
relocScanRootPlusReturn
	rts
	.bend  ; relocScanRecordRootPlus

relocScanRecordRootMinus	.block
	bsr.s relocScanPrepareRootOperator
	bne.s relocScanRootMinusReturn
	addq.w #1, RelocScanRootMinusCount.l
	move.l RelocScanFirstRootMinusOffset.l, d0
	bpl.s relocScanRootMinusReturn
	move.l a2, d0
	sub.l a3, d0
	move.l d0, RelocScanFirstRootMinusOffset.l
relocScanRootMinusReturn
	rts
	.bend  ; relocScanRecordRootMinus

relocScanRecordRootOther	.block
	bsr.s relocScanPrepareRootOperator
	bne.s relocScanRootOtherReturn
	addq.w #1, RelocScanRootOtherCount.l
relocScanRootOtherReturn
	rts
	.bend  ; relocScanRecordRootOther

; Resolve one selected-instruction token through the active assembler lexical
; context. This is the native callback form of Rust's evaluation-context symbol
; lookup; compound expressions fall through to the existing ExprVM snapshot.
; Inputs: A0/D0 = token text/length. Outputs: D0 = status; D3 = value.
resolveSelectedSymbolV1	.block
	jsr scopes.resolveLabelValueV1
	rts
	.bend  ; resolveSelectedSymbolV1

	.priv
; Resolve one directive-expression symbol through the same lexical context
; used by Rust before falling back to the qualified engine table.  ExprVM uses
; this only when its immutable snapshot lookup misses.
; Inputs: A0/D0 = token text/length. Outputs: D0 = status; D3 = value.
resolveExpressionSymbolV1	.block
	jsr scopes.resolveLabelValueV1
	rts
	.bend  ; resolveExpressionSymbolV1

	.pub
; Prepare the directive-specific evaluation extension.
; Inputs: A0 = OPASM_SERVICE_* frame.
; Outputs: D0 = engine status.
prepareDirectiveExpressionExtensionV1	.block
	movem.l d2/a0-a1, -(sp)
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a1
	movea.l a1, a0
	jsr eng.prepareDirectiveEvaluateExpressionExtensionV1
	move.l d0, d2
	movea.l a1, a0
	suba.l a1, a1
	moveq #0, d0
	moveq #0, d1
	bsr.w materializeScopedSnapshot
	move.l d2, d0
	movem.l (sp)+, d2/a0-a1
	tst.l d0
	rts
	.bend  ; prepareDirectiveExpressionExtensionV1

	.priv

prepareExtensionCommon	.block
	movem.l d3-d5/a0-a4, -(sp)
	move.l d0, d4
	move.l d1, d5
	movea.l a0, a4
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a0), a2
	movea.l abi.OPASM_SERVICE_EVAL_EXTENSION_PTR(a0), a3
	movea.l a3, a1
	movea.l a2, a0
	jsr eng.prepareEvaluateExpressionExtensionV1
	move.l d0, d3
	movea.l a3, a0
	movea.l a4, a1
	move.l d4, d0
	move.l d5, d1
	bsr.w materializeScopedSnapshot
	move.l d3, d0
	movem.l (sp)+, d3-d5/a0-a4
	tst.l d0
	rts
	.bend  ; prepareExtensionCommon

; Add imported ordinary names referenced by one request expression to the
; package evaluation snapshot. The request text is never rewritten, so its
; consumer sees the actual source spelling.
; Inputs: D0.W = selected request bytes; A6 = service frame.
materializeSelectedImportAliases	.block
	movem.l d0-d7/a0-a6, -(sp)
	moveq #0, d7
	move.w d0, d7
	cmpi.w #9, d7
	blo.w selectedAliasReturn
	movea.l abi.OPASM_SERVICE_IMPORT_NAME_RESOLVER_PTR(a6), a2
	move.l a2, d0
	beq.w selectedAliasReturn
	suba.l a4, a4
	moveq #0, d5
	move.l SelectedStatementIndex.l, d0
	jsr eng.opasmEngineGetStatementOwnerTextV1
	tst.l d0
	beq.s selectedAliasOwnerReady
	movea.l a0, a4
	move.l d0, d5
selectedAliasOwnerReady
	movea.l abi.OPASM_SERVICE_IO_BUFFER_PTR(a6), a3
	moveq #0, d4
	move.b 8(a3), d4
	addi.l #9, d4
	cmp.l d7, d4
	bhi.w selectedAliasReturn
	adda.l d4, a3
	sub.l d4, d7
	moveq #0, d2

selectedAliasScan
	tst.l d7
	beq.w selectedAliasReturn
	moveq #0, d0
	move.b (a3), d0
	tst.b d2
	beq.s selectedAliasUnquoted
	cmp.b d2, d0
	beq.w selectedAliasQuoteEnd
	cmpi.b #'\\', d0
	bne.w selectedAliasAdvanceOne
	cmpi.l #1, d7
	bls.w selectedAliasAdvanceOne
	addq.l #2, a3
	subq.l #2, d7
	bra.s selectedAliasScan

selectedAliasQuoteEnd
	moveq #0, d2
	bra.w selectedAliasAdvanceOne

selectedAliasUnquoted
	cmpi.b #'"', d0
	beq.w selectedAliasQuoteBegin
	cmpi.b #39, d0
	beq.w selectedAliasQuoteBegin
	bsr.w selectedImportTokenStart
	tst.l d0
	beq.w selectedAliasAdvanceOne
	moveq #1, d4

selectedAliasTokenScan
	cmp.l d7, d4
	bhs.s selectedAliasTokenReady
	moveq #0, d0
	move.b 0(a3, d4.l), d0
	bsr.w selectedImportTokenContinue
	tst.l d0
	beq.s selectedAliasTokenReady
	addq.l #1, d4
	bra.s selectedAliasTokenScan

selectedAliasTokenReady
	; Rust resolves an instruction operand in its active lexical scope before
	; import and global fallbacks, then exposes that request-local spelling to
	; the family VM.  Materialize the same neutral alias/value pair here.
	movem.l d2-d7/a2-a5, -(sp)
	movea.l a3, a0
	move.l d4, d0
	jsr scopes.resolveLabelValueV1
	move.l d0, -(sp)
	move.l d3, -(sp)
	movem.l 8(sp), d2-d7/a2-a5
	move.l (sp)+, d3
	move.l (sp)+, d0
	adda.l #40, sp
	tst.l d0
	bne.s selectedAliasTryOwner
selectedAliasAppendLocal
	movea.l a3, a0
	move.l d4, d0
	bsr.w appendSelectedSnapshotAliasV1
	bra.w selectedAliasAdvanceToken

selectedAliasTryOwner
	; Rust resolves an unqualified name against its owning module after active
	; nested scopes and before imports. The retained statement owner makes that
	; fallback independent of transient recursive-import scope state.
	tst.l d5
	beq.s selectedAliasTryImport
	movea.l a3, a0
	move.l d4, d0
	movea.l a4, a1
	move.l d5, d1
	bsr.w resolveSelectedOwnerSymbolV1
	tst.l d0
	beq.s selectedAliasAppendLocal
	movea.l a3, a0
	move.l d4, d0
	movea.l a4, a1
	move.l d5, d1
	bsr.w materializeSelectedOwnerMemberBaseAliasV1

selectedAliasTryImport
	tst.l d5
	beq.s selectedAliasTrySuffix
	movem.l d2-d7/a2-a5, -(sp)
	movea.l a3, a0
	move.l d4, d0
	movea.l a4, a1
	move.l d5, d1
	jsr (a2)
	move.l d1, -(sp)
	move.l d0, -(sp)
	move.l a0, -(sp)
	movem.l 12(sp), d2-d7/a2-a5
	movea.l (sp), a0
	move.l 4(sp), d0
	move.l 8(sp), d1
	adda.l #52, sp
	tst.l d1
	bne.s selectedAliasTryImportMember
	move.l d0, -(sp)
	move.l a0, -(sp)
	jsr eng.opasmEngineResolveLabelValueV1
	tst.l d0
	beq.s selectedAliasMappedExact
	movea.l (sp), a0
	move.l 4(sp), d0
	jsr eng.opasmEngineResolveUniqueLabelFinalComponentV1
	tst.l d0
	beq.s selectedAliasMappedExact
	addq.l #8, sp
	bra.s selectedAliasTryImportMember
selectedAliasMappedExact
	addq.l #8, sp
	bra.s selectedAliasMappedResolved
selectedAliasTryImportMember
	; Rust evaluates an Expr::Member base independently before its consumer
	; interprets the field. If the complete dotted token is not one import,
	; expose the longest imported base without assigning meaning to its suffix.
	tst.l d5
	beq.s selectedAliasTrySuffix
	movea.l a3, a0
	move.l d4, d0
	movea.l a4, a1
	move.l d5, d1
	bsr.w materializeSelectedImportedMemberBaseAliasV1
	tst.l d0
	beq.w selectedAliasAdvanceToken
selectedAliasTrySuffix
	moveq #0, d2
	movea.l a3, a0
	move.l d4, d0
	bsr.w resolveSelectedLastComponentV1
	tst.l d0
	bne.w selectedAliasAdvanceToken
selectedAliasMappedResolved
	; Rust resolves the source spelling directly to the imported full name for
	; each expression term. Keep only that request-local source alias here;
	; inserting an auxiliary final-component alias between source terms changes
	; their snapshot ordering and has no Rust counterpart.
selectedAliasResolved
	movea.l a3, a0
	move.l d4, d0
	bsr.w appendSelectedSnapshotAliasV1
	bra.w selectedAliasAdvanceToken

selectedAliasAdvanceToken
	adda.l d4, a3
	sub.l d4, d7
	bra.w selectedAliasScan

selectedAliasQuoteBegin
	move.b d0, d2

selectedAliasAdvanceOne
	addq.l #1, a3
	subq.l #1, d7
	bra.w selectedAliasScan

selectedAliasReturn
	movem.l (sp)+, d0-d7/a0-a6
	rts
	.bend  ; materializeSelectedImportAliases

; Resolve `{statement-owner}.{token}` through the exact engine label table.
; Inputs: A0/D0 = token; A1/D1 = owner. Outputs: D0 = status; D3 = value.
resolveSelectedOwnerSymbolV1	.block
	movem.l d1-d2/d4-d7/a0-a5, -(sp)
	movea.l a0, a2
	move.l d0, d6
	movea.l a1, a3
	move.l d1, d7
	beq.s ownerResolveFail
ownerResolveAttempt
	move.l d7, d4
	addq.l #1, d4
	add.l d6, d4
	cmpi.l #eng.LABEL_NAME_CAPACITY, d4
	bhs.s ownerResolveTrim
	lea SelectedOwnerLookupName.l, a1
	movea.l a3, a4
	move.l d7, d2
ownerCopyLoop
	tst.l d2
	beq.s ownerCopyDot
	move.b (a4)+, (a1)+
	subq.l #1, d2
	bra.s ownerCopyLoop
ownerCopyDot
	move.b #'.', (a1)+
ownerTokenLoop
	tst.l d6
	beq.s ownerResolve
	move.b (a2)+, (a1)+
	subq.l #1, d6
	bra.s ownerTokenLoop
ownerResolve
	clr.b (a1)
	lea SelectedOwnerLookupName.l, a0
	move.l d4, d0
	jsr eng.opasmEngineResolveLabelValueV1
	tst.l d0
	beq.s ownerResolveReturn
ownerResolveTrim
	tst.l d7
	beq.s ownerResolveFail
	subq.l #1, d7
	cmpi.b #'.', 0(a3, d7.l)
	bne.s ownerResolveTrim
	tst.l d7
	bne.s ownerResolveAttempt
ownerResolveFail
	moveq #1, d0
ownerResolveReturn
	movem.l (sp)+, d1-d2/d4-d7/a0-a5
	rts
	.bend  ; resolveSelectedOwnerSymbolV1

; Materialize the longest resolvable owner-relative prefix of a dotted token.
; Rust evaluates an Expr::Member base independently before a package interprets
; the field. This helper preserves that neutral boundary: it resolves no field
; spelling and only exposes the already-owned base symbol to the request.
; Inputs: A0/D0 = token; A1/D1 = owner. Output: D0 = status.
materializeSelectedOwnerMemberBaseAliasV1	.block
	movem.l d1-d7/a0-a5, -(sp)
	movea.l a0, a2
	move.l d0, d6
	movea.l a1, a3
	move.l d1, d7
ownerMemberBaseScan
	tst.l d6
	beq.s ownerMemberBaseFail
	subq.l #1, d6
	cmpi.b #'.', 0(a2, d6.l)
	bne.s ownerMemberBaseScan
	tst.l d6
	beq.s ownerMemberBaseFail
	movea.l a2, a0
	move.l d6, d0
	movea.l a3, a1
	move.l d7, d1
	bsr.w resolveSelectedOwnerSymbolV1
	tst.l d0
	bne.s ownerMemberBaseScan
	movea.l a2, a0
	move.l d6, d0
	bsr.w appendSelectedSnapshotAliasV1
	bra.s ownerMemberBaseReturn
ownerMemberBaseFail
	moveq #1, d0
ownerMemberBaseReturn
	movem.l (sp)+, d1-d7/a0-a5
	rts
	.bend  ; materializeSelectedOwnerMemberBaseAliasV1

; Materialize the longest import-resolvable prefix of a dotted token. This is
; the imported-name counterpart of the owner-member helper above and retains
; the same architecture-neutral Expr::Member boundary.
; Inputs: A0/D0 = token; A1/D1 = owner; A2 = import resolver.
; Output: D0 = status.
materializeSelectedImportedMemberBaseAliasV1	.block
	movem.l d1-d7/a0-a5, -(sp)
	movea.l a0, a3
	move.l d0, d6
	movea.l a1, a4
	move.l d1, d7
	movea.l a2, a5
importMemberBaseScan
	tst.l d6
	beq.s importMemberBaseFail
	subq.l #1, d6
	cmpi.b #'.', 0(a3, d6.l)
	bne.s importMemberBaseScan
	tst.l d6
	beq.s importMemberBaseFail
	movea.l a3, a0
	move.l d6, d0
	movea.l a4, a1
	move.l d7, d1
	movea.l a5, a2
	bsr.w resolveSelectedImportedPrefixV1
	tst.l d0
	bne.s importMemberBaseScan
	movea.l a3, a0
	move.l d6, d0
	bsr.w appendSelectedSnapshotAliasV1
	move.l d0, -(sp)
	movem.l 4(sp), d1-d7/a0-a5
	move.l (sp)+, d0
	adda.l #52, sp
	rts
importMemberBaseFail
	movem.l (sp)+, d1-d7/a0-a5
	moveq #1, d0
	rts
	.bend  ; materializeSelectedImportedMemberBaseAliasV1

; Resolve one already-bounded import prefix to the engine's authoritative
; fully-qualified value while preserving the caller's scan state.
; Inputs: A0/D0 = token; A1/D1 = owner; A2 = import resolver.
; Outputs: D0 = status; D3 = value on success.
resolveSelectedImportedPrefixV1	.block
	movem.l d1-d2/d4-d7/a0-a5, -(sp)
	jsr (a2)
	tst.l d1
	bne.s importedPrefixFail
	jsr eng.opasmEngineResolveLabelValueV1
	tst.l d0
	bne.s importedPrefixFail
	move.l d3, -(sp)
	movem.l 4(sp), d1-d2/d4-d7/a0-a5
	move.l (sp)+, d3
	adda.l #48, sp
	moveq #0, d0
	rts
importedPrefixFail
	movem.l (sp)+, d1-d2/d4-d7/a0-a5
	moveq #1, d0
	rts
	.bend  ; resolveSelectedImportedPrefixV1

; Append one already-authorized alias/value to the bounded selected snapshot.
; Inputs: A0/D0 = alias text/length; D3 = value. Outputs: D0 = status.
appendSelectedSnapshotAliasV1	.block
	cmpi.l #SCOPED_SNAPSHOT_NAME_BYTES, d0
	bhs.s appendAliasFail
	move.l d0, -(sp)
	moveq #0, d1
	move.w ScopedSnapshotCount.l, d1
	cmpi.w #SCOPED_SNAPSHOT_CAPACITY, d1
	bhs.s appendAliasStackFail
	move.l d3, d6
	move.l d1, d0
	mulu.w #SCOPED_SNAPSHOT_NAME_BYTES, d0
	lea ScopedSnapshotNames.l, a1
	adda.l d0, a1
	move.l (sp), d0
	bsr.w copySnapshotName
	moveq #0, d0
	move.w ScopedSnapshotCount.l, d0
	lsl.l #2, d0
	lea ScopedSnapshotValues.l, a0
	move.l d6, 0(a0, d0.l)
	lea ScopedSnapshotCount.l, a0
	addq.w #1, (a0)
	addq.l #4, sp
	moveq #0, d0
	rts
appendAliasStackFail
	addq.l #4, sp
appendAliasFail
	moveq #1, d0
	rts
	.bend  ; appendSelectedSnapshotAliasV1

; Resolve the final component of a dotted architecture-neutral identifier.
; This supports engines whose structural module pass retained an unqualified
; exported label while the import token itself remains fully qualified.
; Inputs: A0/D0 = token text/length.
; Outputs: D0 = engine resolve status; D3 = value on success.
resolveSelectedLastComponentV1	.block
	movem.l d1-d2/a0-a1, -(sp)
	movea.l a0, a1
	move.l d0, d2
	moveq #0, d1
selectedComponentScan
	tst.l d2
	beq.s selectedComponentReady
	cmpi.b #'.', (a1)+
	bne.s selectedComponentNext
	movea.l a1, a0
	move.l d2, d1
	subq.l #1, d1
selectedComponentNext
	subq.l #1, d2
	bra.s selectedComponentScan
selectedComponentReady
	tst.l d1
	beq.s selectedComponentFail
	move.l d1, d0
	jsr eng.opasmEngineResolveLabelValueV1
	bra.s selectedComponentReturn
selectedComponentFail
	moveq #1, d0
selectedComponentReturn
	movem.l (sp)+, d1-d2/a0-a1
	rts
	.bend  ; resolveSelectedLastComponentV1

; Architecture-neutral opForge identifier boundaries used only to query the
; embedding import resolver. Registers, literals, and unmapped names remain
; unchanged in the package request.
selectedImportTokenStart	.block
	cmpi.b #'A', d0
	blo.s punctuation
	cmpi.b #'Z', d0
	bls.s accepted
	cmpi.b #'a', d0
	blo.s punctuation
	cmpi.b #'z', d0
	bls.s accepted
punctuation
	cmpi.b #'_', d0
	beq.s accepted
	cmpi.b #'.', d0
	beq.s accepted
	moveq #0, d0
	rts
accepted
	moveq #1, d0
	rts
	.bend  ; selectedImportTokenStart

selectedImportTokenContinue	.block
	move.l d0, -(sp)
	bsr.s selectedImportTokenStart
	tst.l d0
	bne.s accepted
	move.l (sp), d0
	cmpi.b #'0', d0
	blo.s rejected
	cmpi.b #'9', d0
	bls.s accepted
rejected
	addq.l #4, sp
	moveq #0, d0
	rts
accepted
	addq.l #4, sp
	moveq #1, d0
	rts
	.bend  ; selectedImportTokenContinue

; Copy the engine label snapshot and prepend unqualified aliases owned by the
; complete active scope.  Scoped aliases precede globals so Rust-compatible
; module/block shadowing is preserved without exposing scope state to tkpkg.
; Inputs: A0 = evaluation extension base; A1 = service frame; D0 = request
; bytes; D1 = 1 only for a selected CPU instruction request.
; Outputs: extension label pointers/count reference the bounded snapshot.
materializeScopedSnapshot	.block
	movem.l d0-d7/a0-a6, -(sp)
	move.l d0, d6
	move.l d1, d5
	movea.l a0, a3
	movea.l a1, a6
	jsr eng.opasmEngineGetLabelCountV1
	cmpi.w #SCOPED_SNAPSHOT_SOURCE_CAPACITY, d0
	bhi.w done
	move.w d0, ScopedSnapshotSourceCount.l
	clr.w ScopedSnapshotCount.l
	moveq #0, d7

aliasLoop
	cmp.w ScopedSnapshotSourceCount.l, d7
	bhs.s importedAliasBegin
	move.l d7, d0
	jsr eng.opasmEngineGetLabelNameV1
	bsr.w snapshotNameLen
	jsr scopes.activeLabelAliasV1
	tst.l d0
	beq.s aliasNext
	moveq #0, d1
	move.w ScopedSnapshotCount.l, d1
	cmpi.w #SCOPED_SNAPSHOT_CAPACITY, d1
	bhs.s copyOriginalBegin
	move.l d1, d2
	mulu.w #SCOPED_SNAPSHOT_NAME_BYTES, d2
	lea ScopedSnapshotNames.l, a1
	adda.l d2, a1
	bsr.w copySnapshotName
	move.l d7, d0
	jsr eng.opasmEngineGetLabelValueV1
	move.l d0, d3
	moveq #0, d1
	move.w ScopedSnapshotCount.l, d1
	lsl.l #2, d1
	lea ScopedSnapshotValues.l, a0
	adda.l d1, a0
	move.l d3, (a0)
	lea ScopedSnapshotCount.l, a0
	addq.w #1, (a0)

aliasNext
	addq.w #1, d7
	bra.s aliasLoop

importedAliasBegin
	tst.l d5
	beq.s copyOriginalBegin
	; Expression contexts are request-local in Rust. Rebuild their
	; aliases in operand-token order so lexical resolution wins over the broad
	; scope aliases materialized above; qualified originals follow below.
	clr.w ScopedSnapshotCount.l
	move.l d6, d0
	bsr.w materializeSelectedImportAliases

copyOriginalBegin
	moveq #0, d7

copyOriginalLoop
	cmp.w ScopedSnapshotSourceCount.l, d7
	bhs.s publish
	moveq #0, d1
	move.w ScopedSnapshotCount.l, d1
	cmpi.w #SCOPED_SNAPSHOT_CAPACITY, d1
	bhs.s publish
	move.l d7, d0
	jsr eng.opasmEngineGetLabelNameV1
	move.l d1, d3
	mulu.w #SCOPED_SNAPSHOT_NAME_BYTES, d3
	lea ScopedSnapshotNames.l, a1
	adda.l d3, a1
	moveq #SCOPED_SNAPSHOT_NAME_BYTES, d0
	bsr.w copySnapshotName
	move.l d7, d0
	jsr eng.opasmEngineGetLabelValueV1
	move.l d0, d3
	moveq #0, d1
	move.w ScopedSnapshotCount.l, d1
	lsl.l #2, d1
	lea ScopedSnapshotValues.l, a0
	adda.l d1, a0
	move.l d3, (a0)
	lea ScopedSnapshotCount.l, a0
	addq.w #1, (a0)
	addq.w #1, d7
	bra.s copyOriginalLoop

publish
	move.l #ScopedSnapshotNames, 0(a3)
	move.l #ScopedSnapshotValues, 4(a3)
	moveq #0, d0
	move.w ScopedSnapshotCount.l, d0
	move.l d0, 8(a3)

done
	movem.l (sp)+, d0-d7/a0-a6
	rts
	.bend  ; materializeScopedSnapshot

; Inputs: A0 = null-terminated fixed-width label name.
; Outputs: D0 = byte length, capped at the fixed-width name capacity.
snapshotNameLen	.block
	moveq #0, d0

loop
	cmpi.w #SCOPED_SNAPSHOT_NAME_BYTES, d0
	bhs.s done
	tst.b 0(a0, d0.w)
	beq.s done
	addq.w #1, d0
	bra.s loop

done
	rts
	.bend  ; snapshotNameLen

; Inputs: A0/D0 = source name/bytes; A1 = destination fixed-width slot.
; Outputs: destination contains the copied bytes and a trailing null when the
;          source is shorter than the fixed-width slot.
copySnapshotName	.block
	move.l d0, d1
	beq.s terminate
	subq.w #1, d1

loop
	move.b (a0)+, (a1)+
	dbf d1, loop
	cmpi.w #SCOPED_SNAPSHOT_NAME_BYTES, d0
	bhs.s done

terminate
	clr.b (a1)

done
	rts
	.bend  ; copySnapshotName

	.endsection

	.section bss, kind=bss
	.align 4

ScopedSnapshotSourceCount
	.res word, 1
ScopedSnapshotCount
	.res word, 1
SelectedStatementIndex
	.res long, 1
SelectedOwnerLookupName
	.res byte, eng.LABEL_NAME_CAPACITY
RelocScanMode
	.res word, 1
RelocScanTargetCount
	.res word, 1
RelocScanTargetIndex
	.res word, 1
RelocScanAbsoluteSymbolCount
	.res word, 1
RelocScanNonAbsoluteSymbolCount
	.res word, 1
RelocScanRootPlusCount
	.res word, 1
RelocScanRootMinusCount
	.res word, 1
RelocScanRootOtherCount
	.res word, 1
RelocScanRootDepth
	.res word, 1
RelocScanParenDepth
	.res word, 1
RelocScanExpectOperand
	.res word, 1
	.align 4
RelocScanFirstRootMinusOffset
	.res long, 1
RelocScanTargetOffset
	.res long, 1
ScopedSnapshotNames
	.res byte, SCOPED_SNAPSHOT_CAPACITY * SCOPED_SNAPSHOT_NAME_BYTES
ScopedSnapshotValues
	.res long, SCOPED_SNAPSHOT_CAPACITY

	.endsection
	.endmodule
