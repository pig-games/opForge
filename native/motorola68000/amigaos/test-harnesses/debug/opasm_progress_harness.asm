; Focused guest harness for the bounded native progress bridge.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module opasm.progress.harness
	.cpu 68020

	.use opasm.amigaos.progress as progress
	.use opforge.debug.contracts as contracts
	.use opforge.debug.events as events
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	.use debug.amigaos.symbol_expr_profile as symbol_expr_profile
.endif

HARNESS_FAIL = 20
HARNESS_PACKAGE_TICKS_OFFSET = progress.OPASM_PROGRESS_PHASE_TICKS_OFFSET + 4
HARNESS_PASS_ONE_TICKS_OFFSET = progress.OPASM_PROGRESS_PHASE_TICKS_OFFSET + 16

	.section entry, kind=code
	.pub

; Execute deterministic phase, heartbeat, saturation-boundary, abort, and
; incomplete-terminal checks against the real progress routines.
start	.block
	jsr events.debugEventReset
	clr.l HarnessTick
	lea nextTick, a0
	jsr progress.opasmProgressBeginRunV1

	jsr progress.opasmProgressGetRecordV1
	moveq #21, d7
	cmpi.l #progress.OPASM_PROGRESS_MAGIC, progress.OPASM_PROGRESS_MAGIC_OFFSET(a0)
	bne.w fail
	moveq #22, d7
	cmpi.w #progress.OPASM_PROGRESS_SCHEMA_VERSION, progress.OPASM_PROGRESS_SCHEMA_OFFSET(a0)
	bne.w fail
	moveq #23, d7
	cmpi.l #10, progress.OPASM_PROGRESS_RUN_ID_OFFSET(a0)
	bne.w fail
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	jsr symbol_expr_profile.opforgeSymbolExprProfileGetRecordV1
	movea.l a0, a2
	moveq #27, d7
	cmpi.l #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_MAGIC, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_MAGIC_OFFSET(a2)
	bne.w fail
	cmpi.l #10, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_RUN_ID_OFFSET(a2)
	bne.w fail
.endif

.ifdef OPFORGE_PROGRESS_WORK_COUNTERS
	jsr progress.opasmProgressGetWorkRecordV1
	movea.l a0, a1
	moveq #24, d7
	cmpi.l #progress.OPASM_WORK_MAGIC, progress.OPASM_WORK_MAGIC_OFFSET(a1)
	bne.w fail
	cmpi.l #10, progress.OPASM_WORK_RUN_ID_OFFSET(a1)
	bne.w fail

	; Deterministic work-multiplication oracle across all three pass modes.
	moveq #25, d7
	moveq #progress.OPASM_WORK_MODE_PASS_ONE, d0
	jsr progress.opasmProgressWorkPassBeginV1
	moveq #2, d0
	moveq #1, d1
	jsr progress.opasmProgressWorkStatementV1
	moveq #7, d0
	moveq #4, d1
	jsr progress.opasmProgressWorkStatementV1
	moveq #2, d0
	moveq #8, d1
	moveq #1, d2
	jsr progress.opasmProgressWorkFlowV1
	moveq #8, d0
	moveq #3, d1
	moveq #1, d2
	jsr progress.opasmProgressWorkFlowV1
	moveq #3, d0
	moveq #4, d1
	moveq #0, d2
	jsr progress.opasmProgressWorkFlowV1
	moveq #progress.OPASM_WORK_LAYOUT_CHANGE_LABEL, d0
	jsr progress.opasmProgressWorkLayoutChangeV1
	moveq #progress.OPASM_WORK_LAYOUT_CHANGE_PLACEMENT, d0
	jsr progress.opasmProgressWorkLayoutChangeV1

	moveq #progress.OPASM_WORK_MODE_LAYOUT, d0
	jsr progress.opasmProgressWorkPassBeginV1
	moveq #4, d0
	moveq #3, d1
	jsr progress.opasmProgressWorkStatementV1
	moveq #100, d0
	jsr progress.opasmProgressWorkPassEndV1
	moveq #progress.OPASM_WORK_MODE_LAYOUT, d0
	jsr progress.opasmProgressWorkPassBeginV1
	moveq #5, d0
	moveq #2, d1
	jsr progress.opasmProgressWorkStatementV1
	moveq #50, d0
	jsr progress.opasmProgressWorkPassEndV1

	moveq #progress.OPASM_WORK_MODE_FINAL_EMISSION, d0
	jsr progress.opasmProgressWorkPassBeginV1
	moveq #9, d0
	moveq #4, d1
	jsr progress.opasmProgressWorkStatementV1
	moveq #64, d0
	jsr progress.opasmProgressWorkPassEndV1
	cmpi.l #2, progress.OPASM_WORK_PASS_ONE_VISITS_OFFSET(a1)
	bne.w fail
	cmpi.l #2, progress.OPASM_WORK_LAYOUT_VISITS_OFFSET(a1)
	bne.w fail
	cmpi.l #1, progress.OPASM_WORK_FINAL_VISITS_OFFSET(a1)
	bne.w fail
	cmpi.l #2, progress.OPASM_WORK_LAYOUT_ROUNDS_OFFSET(a1)
	bne.w fail
	cmpi.l #1, progress.OPASM_WORK_FINAL_EMISSIONS_OFFSET(a1)
	bne.w fail
	cmpi.l #1, progress.OPASM_WORK_LAYOUT_LABEL_CHANGES_OFFSET(a1)
	bne.w fail
	cmpi.l #1, progress.OPASM_WORK_LAYOUT_PLACEMENT_CHANGES_OFFSET(a1)
	bne.w fail
	cmpi.l #3, progress.OPASM_WORK_FLOW_ROWS_OFFSET(a1)
	bne.w fail
	cmpi.l #1, progress.OPASM_WORK_FORWARD_REDIRECTS_OFFSET(a1)
	bne.w fail
	cmpi.l #1, progress.OPASM_WORK_BACKWARD_REDIRECTS_OFFSET(a1)
	bne.w fail
	cmpi.l #1, progress.OPASM_WORK_MODULE_ROWS_OFFSET(a1)
	bne.w fail
	cmpi.l #1, progress.OPASM_WORK_ENDMODULE_ROWS_OFFSET(a1)
	bne.w fail
	cmpi.l #1, progress.OPASM_WORK_USE_ROWS_OFFSET(a1)
	bne.w fail
	cmpi.l #2, progress.OPASM_WORK_GENERIC_ROWS_OFFSET(a1)
	bne.w fail
	cmpi.l #9, progress.OPASM_WORK_MAX_STATEMENT_OFFSET(a1)
	bne.w fail
	cmpi.l #6, progress.OPASM_WORK_MAX_FORWARD_SPAN_OFFSET(a1)
	bne.w fail
	cmpi.l #5, progress.OPASM_WORK_MAX_BACKWARD_SPAN_OFFSET(a1)
	bne.w fail
	cmpi.l #150, progress.OPASM_WORK_CONVERGENCE_IMAGE_BYTES_OFFSET(a1)
	bne.w fail
	cmpi.l #64, progress.OPASM_WORK_FINAL_IMAGE_BYTES_OFFSET(a1)
	bne.w fail

	; Execute every saturating counter group through the public routines.
	moveq #26, d7
	move.l #$fffffffe, progress.OPASM_WORK_PASS_ONE_VISITS_OFFSET(a1)
	moveq #progress.OPASM_WORK_MODE_PASS_ONE, d0
	jsr progress.opasmProgressWorkPassBeginV1
	moveq #10, d0
	moveq #0, d1
	jsr progress.opasmProgressWorkStatementV1
	jsr progress.opasmProgressWorkStatementV1
	move.l #$ffffffff, progress.OPASM_WORK_LAYOUT_ROUNDS_OFFSET(a1)
	moveq #progress.OPASM_WORK_MODE_LAYOUT, d0
	jsr progress.opasmProgressWorkPassBeginV1
	move.l #$ffffffff, progress.OPASM_WORK_FLOW_ROWS_OFFSET(a1)
	moveq #1, d0
	moveq #2, d1
	moveq #0, d2
	jsr progress.opasmProgressWorkFlowV1
	move.l #$ffffffff, progress.OPASM_WORK_GENERIC_ROWS_OFFSET(a1)
	moveq #11, d0
	moveq #4, d1
	jsr progress.opasmProgressWorkStatementV1
	move.l #$fffffffe, progress.OPASM_WORK_CONVERGENCE_IMAGE_BYTES_OFFSET(a1)
	moveq #4, d0
	jsr progress.opasmProgressWorkPassEndV1
	cmpi.l #$ffffffff, progress.OPASM_WORK_PASS_ONE_VISITS_OFFSET(a1)
	bne.w fail
	cmpi.l #$ffffffff, progress.OPASM_WORK_CONVERGENCE_IMAGE_BYTES_OFFSET(a1)
	bne.w fail
	cmpi.l #31, progress.OPASM_WORK_OVERFLOW_OFFSET(a1)
	bne.w fail
.endif

	jsr progress.opasmProgressGetRecordV1

	moveq #30, d7
	moveq #progress.OPASM_PROGRESS_PHASE_PACKAGE, d0
	moveq #0, d1
	moveq #0, d2
	jsr progress.opasmProgressSetPhaseV1
	moveq #progress.OPASM_PROGRESS_PHASE_PASS_ONE, d0
	moveq #1, d1
	moveq #0, d2
	jsr progress.opasmProgressSetPhaseV1
	cmpi.l #10, HARNESS_PACKAGE_TICKS_OFFSET(a0)
	bne.w fail

.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	; Deterministic exact/scoped/imported/final lookup and expression oracle.
	moveq #36, d7
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_EXACT, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_HIT, d1
	moveq #2, d2
	moveq #7, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_EXACT, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_MISS, d1
	moveq #0, d2
	moveq #0, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_SCOPED, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_HIT, d1
	moveq #3, d2
	moveq #0, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_SCOPED, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_MISS, d1
	moveq #2, d2
	moveq #0, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_IMPORTED, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_HIT, d1
	moveq #1, d2
	moveq #0, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_IMPORTED, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_MISS, d1
	moveq #1, d2
	moveq #0, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_FINAL_COMPONENT, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_HIT, d1
	moveq #5, d2
	moveq #8, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_FINAL_COMPONENT, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_MISS, d1
	moveq #4, d2
	moveq #6, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_FINAL_COMPONENT, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_AMBIGUOUS, d1
	moveq #2, d2
	moveq #5, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1

	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_REQUEST, d0
	moveq #0, d1
	moveq #0, d2
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_PARSE, d0
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_COMPILE, d0
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_BIND, d0
	moveq #4, d1
	moveq #10, d2
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	moveq #6, d1
	moveq #12, d2
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_EVALUATE, d0
	moveq #0, d1
	moveq #0, d2
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_SUCCESS, d0
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_FAILURE, d0
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL
	moveq #7, d0
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordChainInsertV1
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordChainInsertV1
	moveq #8, d0
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordChainInsertV1
.endif

	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EXACT_CALLS_OFFSET(a2)
	bne.w fail
	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_SCOPED_CALLS_OFFSET(a2)
	bne.w fail
	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_IMPORTED_CALLS_OFFSET(a2)
	bne.w fail
	cmpi.l #3, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_FINAL_CALLS_OFFSET(a2)
	bne.w fail
	cmpi.l #1, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EXACT_HITS_OFFSET(a2)
	bne.w fail
	cmpi.l #1, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EXACT_MISSES_OFFSET(a2)
	bne.w fail
	cmpi.l #1, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_FINAL_AMBIGUOUS_OFFSET(a2)
	bne.w fail
	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_REQUESTS_OFFSET(a2)
	bne.w fail
	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_PARSE_CALLS_OFFSET(a2)
	bne.w fail
	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_COMPILE_CALLS_OFFSET(a2)
	bne.w fail
	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_BIND_CALLS_OFFSET(a2)
	bne.w fail
	cmpi.l #1, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVALUATE_CALLS_OFFSET(a2)
	bne.w fail
	cmpi.l #1, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_SUCCESSES_OFFSET(a2)
	bne.w fail
	cmpi.l #1, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_FAILURES_OFFSET(a2)
	bne.w fail
	cmpi.l #9, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_LOOKUP_PASS_ONE_OFFSET(a2)
	bne.w fail
	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EXPR_PASS_ONE_OFFSET(a2)
	bne.w fail
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL
	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EXACT_CANDIDATES_OFFSET(a2)
	bne.w fail
	cmpi.l #5, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_SCOPED_CANDIDATES_OFFSET(a2)
	bne.w fail
	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_IMPORTED_CANDIDATES_OFFSET(a2)
	bne.w fail
	cmpi.l #11, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_FINAL_CANDIDATES_OFFSET(a2)
	bne.w fail
	cmpi.l #7, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EXACT_BYTES_OFFSET(a2)
	bne.w fail
	cmpi.l #19, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_FINAL_BYTES_OFFSET(a2)
	bne.w fail
	cmpi.l #10, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_SNAPSHOT_CANDIDATES_OFFSET(a2)
	bne.w fail
	cmpi.l #22, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_SNAPSHOT_BYTES_OFFSET(a2)
	bne.w fail
	cmpi.l #1, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_PROBE_ZERO_OFFSET(a2)
	bne.w fail
	cmpi.l #1, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_PROBE_TWO_OFFSET(a2)
	bne.w fail
	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_MAX_PROBES_OFFSET(a2)
	bne.w fail
	cmpi.l #2, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_MAX_CHAIN_OFFSET(a2)
	bne.w fail

	; Saturate every externally writable group through the public APIs.
	move.l #$ffffffff, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EXACT_CALLS_OFFSET(a2)
	move.l #$ffffffff, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_SCOPED_CANDIDATES_OFFSET(a2)
	move.l #$ffffffff, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_IMPORTED_BYTES_OFFSET(a2)
	move.l #$ffffffff, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EXACT_HITS_OFFSET(a2)
	move.l #$ffffffff, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_REQUESTS_OFFSET(a2)
	move.l #$ffffffff, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_PROBE_ZERO_OFFSET(a2)
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_EXACT, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_HIT, d1
	moveq #0, d2
	moveq #0, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_SCOPED, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_HIT, d1
	moveq #1, d2
	moveq #0, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_CLASS_IMPORTED, d0
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OUTCOME_HIT, d1
	moveq #0, d2
	moveq #1, d3
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordLookupV1
	moveq #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EVENT_REQUEST, d0
	moveq #0, d1
	moveq #0, d2
	jsr symbol_expr_profile.opforgeSymbolExprProfileRecordExpressionV1
	cmpi.l #63, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_OVERFLOW_OFFSET(a2)
	bne.w fail
.endif
.endif

	moveq #31, d7
	moveq #2, d0
	jsr progress.opasmProgressSetHeartbeatV1
	moveq #0, d0
	moveq #3, d1
	jsr progress.opasmProgressStatementBeginV1
	jsr progress.opasmProgressStatementCompleteV1
	moveq #1, d0
	moveq #3, d1
	jsr progress.opasmProgressStatementBeginV1
	jsr progress.opasmProgressStatementCompleteV1
	cmpi.w #1, events.DebugEventCount
	bne.w fail
	lea events.DebugEventBuffer, a1
	cmpi.w #contracts.EVENT_ASSEMBLY_PROGRESS, events.DEBUG_EVENT_KIND(a1)
	bne.w fail
	cmpi.l #2, events.DEBUG_EVENT_ARG3(a1)
	bne.w fail

	moveq #32, d7
	moveq #3, d0
	jsr progress.opasmProgressSetAbortVisitsV1
	moveq #2, d0
	moveq #3, d1
	jsr progress.opasmProgressStatementBeginV1
	jsr progress.opasmProgressAbortRequestedV1
	cmpi.l #1, d0
	bne.w fail

	; Exercise the two saturating counters through their production routines.
	moveq #34, d7
	moveq #0, d0
	jsr progress.opasmProgressSetHeartbeatV1
	move.l #$fffffffe, progress.OPASM_PROGRESS_VISITS_OFFSET(a0)
	moveq #3, d0
	moveq #5, d1
	jsr progress.opasmProgressStatementBeginV1
	moveq #4, d0
	moveq #5, d1
	jsr progress.opasmProgressStatementBeginV1
	cmpi.l #$ffffffff, progress.OPASM_PROGRESS_VISITS_OFFSET(a0)
	bne.w fail
	move.l progress.OPASM_PROGRESS_OVERFLOW_OFFSET(a0), d0
	andi.l #progress.OPASM_PROGRESS_OVERFLOW_VISITS, d0
	beq.w fail

	moveq #35, d7
	move.l #$fffffffe, HARNESS_PASS_ONE_TICKS_OFFSET(a0)
	moveq #progress.OPASM_PROGRESS_PHASE_LAYOUT, d0
	moveq #1, d1
	moveq #1, d2
	jsr progress.opasmProgressSetPhaseV1
	cmpi.l #$ffffffff, HARNESS_PASS_ONE_TICKS_OFFSET(a0)
	bne.w fail
	move.l progress.OPASM_PROGRESS_OVERFLOW_OFFSET(a0), d0
	andi.l #progress.OPASM_PROGRESS_OVERFLOW_PHASE_TICKS, d0
	beq.w fail

	moveq #33, d7
	moveq #HARNESS_FAIL, d0
	jsr progress.opasmProgressFinishV1
	jsr progress.opasmProgressGetRecordV1
	move.w progress.OPASM_PROGRESS_FLAGS_OFFSET(a0), d0
	andi.w #progress.OPASM_PROGRESS_FLAG_ACTIVE, d0
	bne.w fail
	move.w progress.OPASM_PROGRESS_FLAGS_OFFSET(a0), d0
	andi.w #progress.OPASM_PROGRESS_FLAG_COMPLETE, d0
	bne.w fail
	move.w progress.OPASM_PROGRESS_FLAGS_OFFSET(a0), d0
	andi.w #progress.OPASM_PROGRESS_FLAG_INCOMPLETE, d0
	beq.w fail
	cmpi.l #$ffffffff, progress.OPASM_PROGRESS_VISITS_OFFSET(a0)
	bne.w fail
	cmpi.l #1, progress.OPASM_PROGRESS_LAST_STMT_OFFSET(a0)
	bne.w fail
	cmpi.l #HARNESS_FAIL, progress.OPASM_PROGRESS_EXIT_STATUS_OFFSET(a0)
	bne.w fail
.ifdef OPFORGE_PROGRESS_WORK_COUNTERS
	jsr progress.opasmProgressGetWorkRecordV1
	movea.l a0, a1
	move.w progress.OPASM_WORK_FLAGS_OFFSET(a1), d0
	andi.w #progress.OPASM_WORK_FLAG_INCOMPLETE, d0
	beq.w fail
	cmpi.l #HARNESS_FAIL, progress.OPASM_WORK_EXIT_STATUS_OFFSET(a1)
	bne.w fail
.endif
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_COUNTERS
	jsr symbol_expr_profile.opforgeSymbolExprProfileGetRecordV1
	movea.l a0, a2
	move.w symbol_expr_profile.OPFORGE_SYMBOL_EXPR_FLAGS_OFFSET(a2), d0
	andi.w #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_FLAG_INCOMPLETE, d0
	beq.w fail
	cmpi.l #HARNESS_FAIL, symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EXIT_STATUS_OFFSET(a2)
	bne.w fail
	; The work-record success branch must not bypass OFSE terminal sealing.
	lea nextTick, a0
	jsr progress.opasmProgressBeginRunV1
	moveq #0, d0
	jsr progress.opasmProgressFinishV1
	jsr symbol_expr_profile.opforgeSymbolExprProfileGetRecordV1
	movea.l a0, a2
	move.w symbol_expr_profile.OPFORGE_SYMBOL_EXPR_FLAGS_OFFSET(a2), d0
	andi.w #symbol_expr_profile.OPFORGE_SYMBOL_EXPR_FLAG_COMPLETE, d0
	beq.w fail
	tst.l symbol_expr_profile.OPFORGE_SYMBOL_EXPR_EXIT_STATUS_OFFSET(a2)
	bne.w fail
.endif

	moveq #0, d0
	rts
fail
	move.l d7, d0
	rts
	.bend  ; start

; Deterministic coarse-tick callback. Every sampled boundary advances 10 ticks.
nextTick	.block
	addq.l #5, HarnessTick
	addq.l #5, HarnessTick
	move.l HarnessTick, d0
	rts
	.bend  ; nextTick

	.endsection

	.section bss, kind=bss
	.align 4
HarnessTick
	.res long, 1
	.endsection

	.output "build/opasm_progress_harness", format=hunk, sections=entry, code, bss
	.endmodule
