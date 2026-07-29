; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.line_processor
	.cpu 68020

	.use opforge.cli.tkpkg_control_block
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.service
	.use opforge.cli.dos

	.use opforge.cli.strings
	.use opforge.cli.constants

	.use opforge.cli.state

	.use opforge.cli.include_use
	.use opforge.cli.directive_handlers
	.use opforge.cli.prvm_bridge

	.use opforge.cli.assembly_session
	.use opasm.amigaos.engine as engine

	.use opforge.cli.report
	.use opforge.cli.line_text
	.use opforge.cli.text_output
	.use opforge.cli.copy
	.use opforge.cli.preprocessor
	.use opforge.cli.preprocessor_expansion
	.use opforge.cli.preprocessor_definitions
	.use opforge.cli.preprocessor_invocation
	.use opforge.cli.preprocessor_substitution
.ifdef OPFORGE_DEBUG_CONTRACTS
	.use opforge.debug.contracts as debug_contracts
	.use opforge.debug.events as debug_events
	.include "debug_macros.i"
.endif

	.section code, kind=code
	.pub

opforgeNativeCliTokenizeCurrentLine	.block
	jsr preprocessor_definitions.opforgeNativeCliCaptureMacroDefinitionLineV1
	tst.l d0
	beq.s preprocessPass
	bmi.w fail
	moveq #0, d0
	rts

preprocessPass
	jsr preprocessor_invocation.opforgeNativeCliParseMacroInvocationV1
	tst.l d0
	beq.s invocationPass

invocationObserved
.ifdef OPFORGE_DEBUG_CONTRACTS
	; Instrumentation point: a recognized or malformed macro invocation.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D7/A0-A6.
	; SR/CCR preserved: restored before the invocation status is tested again.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: the parser status is saved and
	; re-tested after the event before the existing malformed/expand branches.
	; Removal/stabilization plan: retain as the native macro invocation contract.
	move.l d0, -(sp)
	move.w ccr, -(sp)
	movem.l d1-d6/a0, -(sp)
	moveq #0, d1
	moveq #4, d2
	move.l 30(sp), d3
	moveq #0, d4
	move.w state.NativeCliPreprocessDefinitionCount, d4
	moveq #0, d5
	move.w state.NativeCliPreprocessInvocationDefinition, d5
	moveq #0, d6
	move.w state.NativeCliPreprocessInvocationArgCount, d6
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_MACRO_INVOCATION
	movem.l (sp)+, d1-d6/a0
	move.w (sp)+, ccr
	move.l (sp)+, d0
.endif
	tst.l d0
	bmi.w fail
	bsr.w opforgeNativeCliExpandActiveMacroV1
	rts

invocationPass
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s record
	lea strings.EndMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w record
	tst.w state.NativeCliIncludeDepth
	beq.s record
	jsr report.opforgeNativeCliEmitIncludeLineRecord

record
	jsr assembly_session.opforgeNativeCliRecordSourceLine
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w commentOnly
	cmpi.b #';', (a0)
	beq.w commentOnly
	lea strings.EndMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w commentOnly
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkPackage
	jsr directive_handlers.opforgeNativeCliParseUseLine
	rts

checkPackage
	bsr.w opforgeNativeCliHasDottedLeadingLabelV1
	tst.l d0
	beq.s packageRoute

sourceFallback
	; The packaged parser has no label token form for a leading dotted name.
	; Preserve this ordinary source statement through the bounded fallback used
	; by macro-expanded lines so qualified local constants and their `.word`
	; references retain real symbol/operand text.
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w fail
	bra.w commentOnly

packageRoute
	tst.w state.NativeCliPackagePipelineReady
	beq.w parseOnly
	bsr.w opforgeNativeCliDispatchTokenizeLineEnvelope
	bne.w fail
	jsr prvm_bridge.opforgeNativeCliSampleActivePrvmLengthField
	move.l d0, state.NativeCliPrvmTokenizerDetail
	jsr prvm_bridge.opforgeNativeCliDispatchParseLineUntilReady
	bne.w fail

ok
	bsr.w opforgeNativeCliParseCurrentLine
	tst.l d0
	bne.w fail
commentOnly
	moveq #0, d0
	rts

parseOnly
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w parseOnlyOk
	movea.l a0, a4
	move.l d0, d7
	lea strings.EndmoduleDirectiveText, a1
	moveq #10, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyCheckModule
	jsr directive_handlers.opforgeNativeCliParseEndmoduleLine
	tst.l d0
	bne.w parseOnlyStatus
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	bra.w parseOnlyStatus

parseOnlyCheckModule
	movea.l a4, a0
	move.l d7, d0
	lea strings.ModuleDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyCheckUse
	jsr directive_handlers.opforgeNativeCliParseModuleLine
	tst.l d0
	bne.w parseOnlyStatus
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	bra.w parseOnlyStatus

parseOnlyCheckUse
	movea.l a4, a0
	move.l d7, d0
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyCheckCpu
	move.w state.NativeCliImportCount, d6
	jsr directive_handlers.opforgeNativeCliParseUseLine
	tst.l d0
	bne.w parseOnlyStatus
	cmp.w state.NativeCliImportCount, d6
	bne.w parseOnlyOk
	move.l #strings.ModuleResolveFailureText, d1
	jsr dos.putStr
	move.l #state.NativeCliArgToken, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	moveq #1, d0

parseOnlyCheckCpu
	movea.l a4, a0
	move.l d7, d0
	lea strings.CpuMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyCheckInclude
	jsr directive_handlers.opforgeNativeCliParseCpuLine
	tst.l d0
	bne.w parseOnlyStatus
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	bra.w parseOnlyStatus

parseOnlyCheckInclude
	movea.l a4, a0
	move.l d7, d0
	lea strings.IncludeDirectiveText, a1
	moveq #8, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyCheckOutput
	jsr include_use.opforgeNativeCliParseIncludeLine
	bra.w parseOnlyStatus

parseOnlyCheckOutput
	movea.l a4, a0
	move.l d7, d0
	lea strings.OutputDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w parseOnlyRecordSourceStatement
	bra.w parseOnlyOk

parseOnlyRecordSourceStatement
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine

parseOnlyStatus
	tst.l d0
	bne.w return

parseOnlyOk
	moveq #0, d0
	rts

fail
	lea buffers.ControlBlockV1, a0
	jsr tkpkg_control_block.opforgeNativeCliReadLastErrorLen
	tst.w d0
	bne.w haveMessage
	tst.l state.NativeCliPrvmRouteStatus
	beq.w readOutput
	move.l #strings.PrvmRouteStatusText, d1
	jsr dos.putStr
	move.l state.NativeCliPrvmRouteStatus, d0
	jsr text_output.opforgeNativeCliPutHexU32
	move.l #strings.NewlineText, d1
	jsr dos.putStr
	tst.l state.NativeCliPrvmPipelineDetail
	beq.s maybeTokenizerDetail
	move.l #strings.PrvmPipelineDetailText, d1
	jsr dos.putStr
	move.l state.NativeCliPrvmPipelineDetail, d0
	jsr text_output.opforgeNativeCliPutHexU32
	move.l #strings.NewlineText, d1
	jsr dos.putStr
maybeTokenizerDetail
	tst.l state.NativeCliPrvmTokenizerDetail
	beq.w maybeRouteDetail
	move.l #strings.PrvmTokenizerDetailText, d1
	jsr dos.putStr
	move.l state.NativeCliPrvmTokenizerDetail, d0
	jsr text_output.opforgeNativeCliPutHexU32
	move.l #strings.NewlineText, d1
	jsr dos.putStr
maybeRouteDetail
	tst.l state.NativeCliPrvmRouteDetail
	beq.w readOutput
	move.l #strings.PrvmRouteDetailText, d1
	jsr dos.putStr
	move.l state.NativeCliPrvmRouteDetail, d0
	jsr text_output.opforgeNativeCliPutHexU32
	move.l #strings.NewlineText, d1
	jsr dos.putStr

readOutput
	lea buffers.ControlBlockV1, a0
	jsr tkpkg_control_block.opforgeNativeCliReadOutputLen
	beq.w return

haveMessage
	lea buffers.lastErrorBuffer, a1
	clr.b 0(a1, d0.W)
	move.l #buffers.lastErrorBuffer, d1
	jsr dos.putStr
	move.l #strings.NewlineText, d1
	jsr dos.putStr

return
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliTokenizeCurrentLine

; Detect a dotted identifier in the leading label token, excluding directives.
; Outputs: D0 = 1 for a dotted leading label, otherwise 0.
; Clobbers: D0-D2/A0/CCR.
opforgeNativeCliHasDottedLeadingLabelV1	.block
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.s no
	cmpi.b #'.', (a0)
	beq.s no
	moveq #0, d1

scan
	tst.l d0
	beq.s no
	move.b (a0)+, d2
	subq.l #1, d0
	cmpi.b #' ', d2
	beq.s found
	cmpi.b #9, d2
	beq.s found
	cmpi.b #':', d2
	beq.s found
	cmpi.b #';', d2
	beq.s no
	cmpi.b #'.', d2
	bne.s scan
	moveq #1, d1
	bra.s scan

found
	move.l d1, d0
	rts
no
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliHasDottedLeadingLabelV1

; Re-enter the ordinary native line pipeline for one bounded expanded line.
; Inputs: A0/D0 = expansion text/length; current source line is the caller frame.
; Outputs: D0 = 0 on success, 1 on length/depth or pipeline failure.
; Clobbers: D0-D3/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliProcessExpandedLineV1	.block
	movem.l d4-d7, -(sp)
	jsr preprocessor_expansion.opforgeNativeCliBeginExpandedLineV1.l
	bne.w fail
	jsr engine.opasmEngineGetSourceRecordCountV1
	move.l d0, d4
	jsr engine.opasmEngineGetStatementCountV1
	move.l d0, d5
	jsr engine.opasmEngineGetImageByteCountV1
	move.l d0, d6
	jsr engine.opasmEngineGetSessionCurrentPcV1
	move.l d0, d7
	movem.l d4-d7, -(sp)
	; The substituted source must follow the ordinary frontend path.  In
	; particular, a nested `.NAME` call re-enters macro recognition and fails
	; against the active bounded frame instead of becoming a silent statement.
	jsr opforgeNativeCliTokenizeCurrentLine
	move.l d0, -(sp)
.ifdef OPFORGE_DEBUG_CONTRACTS
	; Instrumentation point: one expanded body line after ordinary tokenization.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D7/A0-A6.
	; SR/CCR preserved: restored before the saved tokenizer status is consumed.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: the tokenizer result remains saved
	; on the stack and is restored after the event before EndExpandedLineV1.
	; Removal/stabilization plan: retain as the expanded-line bridge contract.
	move.w ccr, -(sp)
	movem.l d1-d6/a0, -(sp)
	moveq #0, d1
	moveq #3, d2
	.ifdef OPFORGE_MACRO_EXPR_EVAL_PROBE
	; The expression probe repurposes this diagnostic-only event payload to
	; capture the expanded source bytes at the record boundary. The ordinary
	; stable event retains its status/depth/body-index payload below.
	; Instrumentation point: expanded source is active after statement recording.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D7/A0-A6 by the surrounding save/restore.
	; SR/CCR preserved: restored before the saved record status is consumed.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change branch decisions: it runs only in the diagnostic
	; build and the original record status remains saved on the stack.
	; Removal/stabilization plan: remove when macro-expanded statement shape is
	; confirmed against the authoritative fixture.
	lea state.NativeCliSourceLine, a0
	move.l (a0), d3
	move.l 4(a0), d4
	move.l 8(a0), d5
	moveq #0, d6
	move.w state.NativeCliSourceLineLen, d6
	.else
	move.l 30(sp), d3
	moveq #0, d4
	move.w state.NativeCliSourceLineLen, d4
	moveq #0, d5
	move.w state.NativeCliPreprocessExpansionDepth, d5
	moveq #0, d6
	move.w state.NativeCliPreprocessInvocationBodyIndex, d6
	.endif
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_MACRO_EXPANDED_LINE
	movem.l (sp)+, d1-d6/a0
	move.w (sp)+, ccr
.endif
	jsr preprocessor_expansion.opforgeNativeCliEndExpandedLineV1.l
	move.l d0, d2
	or.l (sp)+, d0
	movem.l (sp)+, d4-d7
	tst.l d0
	beq.s done
	tst.l d2
	beq.s rollback
	jsr preprocessor_expansion.opforgeNativeCliAbortExpandedLineV1.l
rollback
	move.l d4, d0
	move.l d5, d1
	move.l d6, d2
	move.l d7, d3
	jsr engine.opasmEngineRollbackCollectionV1
	moveq #1, d0
done
	movem.l (sp)+, d4-d7
	rts

fail
	movem.l (sp)+, d4-d7
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliProcessExpandedLineV1

; Re-enter the ordinary parser path for one generated macro scope line.
; Label-attached `.block` / `.endblock` lines need the package parser's label
; and flow-control metadata; unlike a substituted body line, they must not use
; the source-only statement fallback.
; Inputs: A0/D0 = scope text/length; current source line is the caller frame.
; Outputs: D0 = 0 on success, 1 on staging or source-recording failure.
; Clobbers: D0-D3/A0-A1/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliProcessExpandedScopeLineV1	.block
	movem.l d4-d7, -(sp)
	jsr preprocessor_expansion.opforgeNativeCliBeginExpandedLineV1.l
	bne.s fail
	jsr engine.opasmEngineGetSourceRecordCountV1
	move.l d0, d4
	jsr engine.opasmEngineGetStatementCountV1
	move.l d0, d5
	jsr engine.opasmEngineGetImageByteCountV1
	move.l d0, d6
	jsr engine.opasmEngineGetSessionCurrentPcV1
	move.l d0, d7
	movem.l d4-d7, -(sp)
	; `.block` and `.endblock` are frontend flow records, not package mnemonics.
	; Preserve them through the existing source/session flow fallback; substituted
	; macro body lines still use the full tokenizer → PRVM path above.
	jsr assembly_session.opforgeNativeCliRecordSourceLine
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	move.l d0, -(sp)
	jsr preprocessor_expansion.opforgeNativeCliEndExpandedLineV1.l
	move.l d0, d2
	or.l (sp)+, d0
	movem.l (sp)+, d4-d7
	tst.l d0
	beq.s done
	tst.l d2
	beq.s rollback
	jsr preprocessor_expansion.opforgeNativeCliAbortExpandedLineV1.l
rollback
	move.l d4, d0
	move.l d5, d1
	move.l d6, d2
	move.l d7, d3
	jsr engine.opasmEngineRollbackCollectionV1
	moveq #1, d0
done
	movem.l (sp)+, d4-d7
	rts

fail
	movem.l (sp)+, d4-d7
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliProcessExpandedScopeLineV1

; Drain the active bounded macro frame through the ordinary CLI line path.
; Inputs: a complete preprocessor invocation frame is active.
; Outputs: D0 = 0 on success, 1 on substitution, recursion, or pipeline failure.
; Clobbers: D0-D7/A0-A2/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliExpandActiveMacroV1	.block
	tst.w state.NativeCliPreprocessInvocationDefinition
	bmi.w fail
.ifdef OPFORGE_DEBUG_CONTRACTS
	; Instrumentation point: macro expansion entry, after the complete invocation
	; frame has been selected and before its body is consumed.
	; Macro/routine used: DEBUG_EVENT_U32X4 / debugEventU32x4.
	; Registers preserved: D0-D7/A0-A6.
	; SR/CCR preserved: not relied upon by the diagnostic-only return below.
	; Stack delta at return: zero.
	; Shared buffers touched: dedicated debug event buffer only.
	; Why this cannot change release branch decisions: this entire probe is absent
	; outside the explicit debug-contract diagnostic build.
	; Removal/stabilization plan: remove after the expansion-frame hang is fixed.
	move.w ccr, -(sp)
	movem.l d1-d6/a0, -(sp)
	moveq #0, d1
	moveq #5, d2
	moveq #0, d3
	move.w state.NativeCliPreprocessInvocationDefinition, d3
	moveq #0, d4
	move.w state.NativeCliPreprocessInvocationArgCount, d4
	moveq #0, d5
	move.w state.NativeCliPreprocessInvocationBodyIndex, d5
	moveq #0, d6
	move.w state.NativeCliPreprocessInvocationLabelLen, d6
	.DEBUG_EVENT_U32X4 debug_contracts.EVENT_MACRO_EXPANSION_ENTRY
	movem.l (sp)+, d1-d6/a0
	move.w (sp)+, ccr
	; Only the frame probe stops before release expansion. Other debug-contract
	; builds execute the body so route checkpoints can localize the failure.
.ifdef OPFORGE_MACRO_EXPANSION_FRAME_PROBE
	; Deliberately terminate this diagnostic-only expansion after recording the
	; frame. This lets the guest harness inspect the imported invocation frame
	; without entering the known non-returning body path.
	moveq #1, d0
	rts
.endif
.endif
	clr.w state.NativeCliPreprocessInvocationBodyIndex
	; Rust macro expansion wraps every `.macro` invocation in a lexical block.
	; Process the generated start line before body substitution so a caller label
	; belongs to this scope rather than to a rewritten body declaration.
	bsr.w emitMacroBlockStart
	bne.w fail
	lea state.NativeCliPreprocessExpansionLine, a0
	moveq #0, d0
	move.w state.NativeCliPreprocessExpansionLineLen, d0
	bsr.w opforgeNativeCliProcessExpandedScopeLineV1
	bne.w fail

bodyLoop
	move.w state.NativeCliPreprocessInvocationDefinition, d2
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationBodyIndex, d0
	; The capture path is bounded to this many body slots.  Keep expansion
	; bounded independently of the per-definition count table so corrupted or
	; stale metadata cannot make the native CLI iterate unboundedly.
	cmpi.w #constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY, d0
	bcc.s close
	lea state.NativeCliPreprocessDefinitionBodyCount, a0
	moveq #0, d4
	add.w d2, d2
	move.w 0(a0, d2.w), d4
	cmpi.w #constants.NATIVE_PREPROCESS_BODY_LINE_CAPACITY, d4
	bhi.w fail
	cmp.w d4, d0
	bcc.s close
	jsr preprocessor_substitution.opforgeNativeCliSubstituteMacroBodyLineV1.l
	bne.w fail
	move.w d1, state.NativeCliPreprocessExpansionLineLen
	lea state.NativeCliPreprocessExpansionLine, a0
	move.l d1, d0
	bsr.w opforgeNativeCliProcessExpandedLineV1
	bne.w fail
	addq.w #1, state.NativeCliPreprocessInvocationBodyIndex
	bra.s bodyLoop

close
	bsr.w emitMacroBlockEnd
	bne.w fail
	lea state.NativeCliPreprocessExpansionLine, a0
	moveq #0, d0
	move.w state.NativeCliPreprocessExpansionLineLen, d0
	bsr.w opforgeNativeCliProcessExpandedScopeLineV1
	bne.w fail
	move.w #-1, state.NativeCliPreprocessInvocationDefinition
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliExpandActiveMacroV1

; Build the Rust-compatible macro scope start line in ExpansionLine.
; A caller label attaches to `.block`; an indented call keeps its indentation.
emitMacroBlockStart	.block
	lea state.NativeCliPreprocessExpansionLine, a2
	moveq #0, d2
	moveq #0, d0
	move.w state.NativeCliPreprocessInvocationLabelLen, d0
	beq.s indent
	lea state.NativeCliPreprocessInvocationLabel, a1
	jsr copy.copyBytes
	move.w state.NativeCliPreprocessInvocationLabelLen, d2
	move.b #' ', (a2)+
	move.b #'.', (a2)+
	move.b #'b', (a2)+
	move.b #'l', (a2)+
	move.b #'o', (a2)+
	move.b #'c', (a2)+
	move.b #'k', (a2)+
	addq.w #7, d2
	bra.s done
indent
	lea state.NativeCliSourceLine, a1
indentLoop
	move.b 0(a1, d2.w), d0
	cmpi.b #' ', d0
	beq.s copyIndent
	cmpi.b #9, d0
	bne.s block
copyIndent
	move.b d0, 0(a2, d2.w)
	addq.w #1, d2
	bra.s indentLoop
block
	adda.l d2, a2
	move.b #'.', (a2)+
	move.b #'b', (a2)+
	move.b #'l', (a2)+
	move.b #'o', (a2)+
	move.b #'c', (a2)+
	move.b #'k', (a2)+
	addq.w #6, d2
done
	move.w d2, state.NativeCliPreprocessExpansionLineLen
	moveq #0, d0
	rts
	.bend  ; emitMacroBlockStart

; Build the matching indented `.endblock` line in ExpansionLine.
emitMacroBlockEnd	.block
	lea state.NativeCliPreprocessExpansionLine, a2
	lea state.NativeCliSourceLine, a1
	clr.w d2
indentLoop
	move.b 0(a1, d2.w), d0
	cmpi.b #' ', d0
	beq.s copyIndent
	cmpi.b #9, d0
	bne.s endblock
copyIndent
	move.b d0, 0(a2, d2.w)
	addq.w #1, d2
	bra.s indentLoop
endblock
	adda.l d2, a2
	move.b #'.', (a2)+
	move.b #'e', (a2)+
	move.b #'n', (a2)+
	move.b #'d', (a2)+
	move.b #'b', (a2)+
	move.b #'l', (a2)+
	move.b #'o', (a2)+
	move.b #'c', (a2)+
	move.b #'k', (a2)+
	addi.w #9, d2
	move.w d2, state.NativeCliPreprocessExpansionLineLen
	moveq #0, d0
	rts
	.bend  ; emitMacroBlockEnd

opforgeNativeCliParseCurrentLine	.block
	movem.l d2-d7/a2-a4, -(sp)
	lea state.NativeCliSourceLine, a0
	moveq #0, d0
	move.w state.NativeCliSourceLineLen, d0
	jsr line_text.opforgeNativeCliSkipLineWhitespace
	beq.w done
	movea.l a0, a4
	move.l d0, d7

	lea strings.IfdefDirectiveText, a1
	moveq #6, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.IfndefDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.ElseifDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.ElseDirectiveText, a1
	moveq #5, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.EndifDirectiveText, a1
	moveq #6, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.IfDirectiveText, a1
	moveq #3, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.w conditionalLine

	movea.l a4, a0
	move.l d7, d0
	lea strings.IncludeDirectiveText, a1
	moveq #8, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkOrg
	jsr include_use.opforgeNativeCliParseIncludeLine
	bra.w return

checkOrg
	movea.l a4, a0
	move.l d7, d0
	lea strings.OutputDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkUseDirective
	jsr directive_handlers.opforgeNativeCliParseOutputLine
	bra.w return

checkUseDirective
	movea.l a4, a0
	move.l d7, d0
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkCpuDirective
	jsr directive_handlers.opforgeNativeCliParseUseLine
	bra.w return

checkCpuDirective
	movea.l a4, a0
	move.l d7, d0
	lea strings.CpuMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkOrgDirective
	jsr directive_handlers.opforgeNativeCliParseCpuLine
	tst.l d0
	bne.w return
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w fail
	bra.w done

checkOrgDirective
	movea.l a4, a0
	move.l d7, d0
	lea strings.OrgMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith

	tst.w state.NativeCliPackagePipelineReady
	beq.w sourceDirectiveFallback
	movea.l a4, a0
	move.l d7, d0
	lea strings.EndmoduleDirectiveText, a1
	moveq #10, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.s routeModuleDirective
	movea.l a4, a0
	move.l d7, d0
	lea strings.ModuleDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.s routeModuleDirective
	movea.l a4, a0
	move.l d7, d0
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	bne.s routeModuleDirective
	tst.w state.NativeCliModuleDepth
	bne.s recordActiveModuleSourceStatement

routeModuleDirective
	bsr.w opforgeNativeCliRouteParserModuleUseLine
	bra.s haveDirectiveKind

recordActiveModuleSourceStatement
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w fail
	bra.w done

sourceDirectiveFallback
	movea.l a4, a0
	move.l d7, d0
	lea strings.EndmoduleDirectiveText, a1
	moveq #10, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkModuleFallback
	moveq #constants.NCLI_PARSER_DIRECTIVE_ENDMODULE, d0
	bra.s haveDirectiveKind

checkModuleFallback
	movea.l a4, a0
	move.l d7, d0
	lea strings.ModuleDirectiveText, a1
	moveq #7, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s checkUseFallback
	moveq #constants.NCLI_PARSER_DIRECTIVE_MODULE, d0
	bra.s haveDirectiveKind

checkUseFallback
	movea.l a4, a0
	move.l d7, d0
	lea strings.UseDirectiveText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.w done
	moveq #constants.NCLI_PARSER_DIRECTIVE_USE, d0

haveDirectiveKind
	cmpi.w #constants.NCLI_PARSER_DIRECTIVE_MODULE, d0
	bne.s checkEndmodule
	jsr directive_handlers.opforgeNativeCliParseModuleLine
	tst.l d0
	bne.w return
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	bra.w return

checkEndmodule
	cmpi.w #constants.NCLI_PARSER_DIRECTIVE_ENDMODULE, d0
	bne.s checkUse
	jsr directive_handlers.opforgeNativeCliParseEndmoduleLine
	tst.l d0
	bne.w return
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	bra.w return

checkUse
	cmpi.w #constants.NCLI_PARSER_DIRECTIVE_USE, d0
	bne.s recordStatement
	jsr directive_handlers.opforgeNativeCliParseUseLine
	bra.w return

recordStatement
	; `.org` is evaluated directly by opasm.  Preserve its source span rather
	; than a parser result from a preceding module transition so each module's
	; origin is evaluated from its own literal text.
	movea.l a4, a0
	move.l d7, d0
	lea strings.OrgMnemonicText, a1
	moveq #4, d1
	jsr line_text.opforgeNativeCliLineStartsWith
	beq.s recordParsedStatement
	moveq #-1, d0
	move.l d0, state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount

recordParsedStatement
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w fail

done
	moveq #0, d0
	bra.w return

conditionalLine
	jsr assembly_session.opforgeNativeCliRecordPrvmStatementLine
	tst.l d0
	bne.w fail
	bra.w done

fail
	move.l #strings.ParserFailureText, d1
	jsr dos.putStr
	moveq #1, d0

return
	movem.l (sp)+, d2-d7/a2-a4
	rts
	.bend  ; opforgeNativeCliParseCurrentLine

opforgeNativeCliRouteParserModuleUseLine	.block
	movem.l d1-d7/a0-a3, -(sp)
	clr.l state.NativeCliPrvmRouteStatus
	clr.w state.NativeCliPrvmResultCount
	jsr prvm_bridge.opforgeNativeCliDispatchParseLineUntilReady
	jsr prvm_bridge.opforgeNativeCliParserDirectiveKind
	
return
	movem.l (sp)+, d1-d7/a0-a3
	rts
	.bend  ; opforgeNativeCliRouteParserModuleUseLine

; Dispatch the shared tkpkg tokenizer service for the current logical line.
; Inputs: current line state is already staged in state.NativeCliSourceLine*.
; Outputs: D0 = tkpkg STATUS_*_V1.
; Clobbers: D0-D1/A0/CCR.
; CCR: reflects D0 on return.
opforgeNativeCliDispatchTokenizeLineEnvelope	.block
	bsr.w opforgeNativeCliPrepareLineServiceRequest
	bne.s done
	lea buffers.ControlBlockV1, a0
	move.w #buffers.LAST_ERROR_BUFFER_PTR_V1, d0
	move.w state.NativeCliLineRequestLen, d1
	jsr tkpkg_control_block.opforgeNativeCliWriteInputWindow
	moveq #abi.ENTRY_ORD_TOKENIZE_LINE, d0
	jsr service.dispatchV1
	jsr tkpkg_control_block.opforgeNativeCliReadStatus

done
	rts
	.bend  ; opforgeNativeCliDispatchTokenizeLineEnvelope

; Build the tokenizer request payload: u32 line number plus source bytes.
; Build the tokenizer request payload: u32 line number plus source bytes.
; Inputs: state.NativeCliSourceLine/state.NativeCliSourceLineLen/state.NativeCliSourceLineNum describe the current logical line.
; Outputs: D0 = 0; D1 = request byte length; state.NativeCliLineRequestLen updated; buffers.lastErrorBuffer populated with the request payload.
; Clobbers: D0-D2/A1-A2/CCR.
; CCR: reflects D0 on return. This helper has no failure path with the current fixed-size request layout.
opforgeNativeCliPrepareLineServiceRequest	.block
	lea buffers.lastErrorBuffer, a2
	move.l state.NativeCliSourceLineNum, d2  ; line number is little-endian to match package fixtures
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lsr.l #8, d2
	move.b d2, (a2)+
	lea state.NativeCliSourceLine, a1
	move.w state.NativeCliSourceLineLen, d0
	jsr copy.copyBytes
	move.w state.NativeCliSourceLineLen, d1
	addq.w #4, d1
	move.w d1, state.NativeCliLineRequestLen
	moveq #0, d0
	rts
	.bend  ; opforgeNativeCliPrepareLineServiceRequest

	.endsection

	.endmodule
