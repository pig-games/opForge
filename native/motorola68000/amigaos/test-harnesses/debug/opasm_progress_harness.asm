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
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	.use debug.amigaos.runtime_profile as runtime_profile
.endif
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	.use debug.amigaos.platform_profile as platform_profile
	.use opforge.cli.copy as copy
.endif

HARNESS_FAIL = 20
HARNESS_PACKAGE_TICKS_OFFSET = progress.OPASM_PROGRESS_PHASE_TICKS_OFFSET + 4
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
HARNESS_BOOTSTRAP_OPENS_OFFSET = platform_profile.OPFORGE_PLATFORM_OPENS_OFFSET + 4
HARNESS_MODULE_OPENS_OFFSET = platform_profile.OPFORGE_PLATFORM_OPENS_OFFSET + 8
HARNESS_PACKAGE_OPENS_OFFSET = platform_profile.OPFORGE_PLATFORM_OPENS_OFFSET + 12
HARNESS_MODULE_CLOSES_OFFSET = platform_profile.OPFORGE_PLATFORM_CLOSES_OFFSET + 8
HARNESS_PACKAGE_CLOSES_OFFSET = platform_profile.OPFORGE_PLATFORM_CLOSES_OFFSET + 12
HARNESS_MODULE_READS_OFFSET = platform_profile.OPFORGE_PLATFORM_READS_OFFSET + 8
HARNESS_PACKAGE_READS_OFFSET = platform_profile.OPFORGE_PLATFORM_READS_OFFSET + 12
HARNESS_MODULE_READ_BYTES_OFFSET = platform_profile.OPFORGE_PLATFORM_READ_BYTES_OFFSET + 8
HARNESS_PACKAGE_READ_BYTES_OFFSET = platform_profile.OPFORGE_PLATFORM_READ_BYTES_OFFSET + 12
HARNESS_PLATFORM_INCOMPLETE = platform_profile.OPFORGE_PLATFORM_FLAG_INCOMPLETE + platform_profile.OPFORGE_PLATFORM_FLAG_IO_ENABLED + platform_profile.OPFORGE_PLATFORM_FLAG_BULK_ENABLED
HARNESS_PLATFORM_COMPLETE = platform_profile.OPFORGE_PLATFORM_FLAG_COMPLETE + platform_profile.OPFORGE_PLATFORM_FLAG_IO_ENABLED + platform_profile.OPFORGE_PLATFORM_FLAG_BULK_ENABLED
HARNESS_ARTIFACT_OPENS_OFFSET = platform_profile.OPFORGE_PLATFORM_OPENS_OFFSET + 16
HARNESS_ARTIFACT_WRITE_BYTES_OFFSET = platform_profile.OPFORGE_PLATFORM_WRITE_BYTES_OFFSET + 16
HARNESS_SESSION_CLEAR_REQUESTED = platform_profile.OPFORGE_PLATFORM_BULK_RANGES_OFFSET + 24 + 4
HARNESS_SESSION_CLEAR_COMPLETED = platform_profile.OPFORGE_PLATFORM_BULK_RANGES_OFFSET + 24 + 8
HARNESS_OTHER_COPY_COMPLETED = platform_profile.OPFORGE_PLATFORM_BULK_RANGES_OFFSET + 20
HARNESS_LAYOUT_CLEAR_COMPLETED = platform_profile.OPFORGE_PLATFORM_BULK_PHASES_OFFSET + 6 * 24 + 8
.endif
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

.ifdef OPFORGE_PROGRESS_PLATFORM_NO_IO
	bsr.w verifyPlatformSubgroups
	rts
.else
.ifdef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	bsr.w verifyPlatformSubgroups
	rts
.endif
.endif
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
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	jsr runtime_profile.opforgeRuntimeProfileGetRecordV1
	movea.l a0, a3
	moveq #28, d7
	cmpi.l #runtime_profile.OPFORGE_RUNTIME_MAGIC, runtime_profile.OPFORGE_RUNTIME_MAGIC_OFFSET(a3)
	bne.w fail
	cmpi.l #10, runtime_profile.OPFORGE_RUNTIME_RUN_ID_OFFSET(a3)
	bne.w fail
.endif
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileGetRecordV1
	movea.l a0, a4
	moveq #29, d7
	cmpi.l #platform_profile.OPFORGE_PLATFORM_MAGIC, platform_profile.OPFORGE_PLATFORM_MAGIC_OFFSET(a4)
	bne.w fail
	cmpi.l #10, platform_profile.OPFORGE_PLATFORM_RUN_ID_OFFSET(a4)
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

.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	; Deterministic VM/program/service nesting, phase, candidate, and overflow oracle.
	moveq #37, d7
	moveq #runtime_profile.OPFORGE_RUNTIME_VM_TKVM, d0
	moveq #runtime_profile.OPFORGE_RUNTIME_PROGRAM_TOKENIZER, d1
	jsr runtime_profile.opforgeRuntimeProfileEnterVmV1
	jsr runtime_profile.opforgeRuntimeProfileRecordOpcodeV1
	jsr runtime_profile.opforgeRuntimeProfileRecordOpcodeV1
	jsr runtime_profile.opforgeRuntimeProfileRecordOpcodeV1
	moveq #runtime_profile.OPFORGE_RUNTIME_VM_EXVM, d0
	moveq #runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_FRONTEND, d1
	jsr runtime_profile.opforgeRuntimeProfileEnterVmV1
	moveq #runtime_profile.OPFORGE_RUNTIME_VM_EXPRVM, d0
	moveq #runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_EVALUATOR, d1
	jsr runtime_profile.opforgeRuntimeProfileEnterVmV1
	cmpi.l #$00040000, progress.OPASM_PROGRESS_VM_SERVICE_ID_OFFSET(a0)
	bne.w fail
	cmpi.l #runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_EVALUATOR, progress.OPASM_PROGRESS_PROGRAM_ID_OFFSET(a0)
	bne.w fail
	jsr runtime_profile.opforgeRuntimeProfileLeaveVmV1
	cmpi.l #$00030000, progress.OPASM_PROGRESS_VM_SERVICE_ID_OFFSET(a0)
	bne.w fail
	cmpi.l #runtime_profile.OPFORGE_RUNTIME_PROGRAM_EXPRESSION_FRONTEND, progress.OPASM_PROGRESS_PROGRAM_ID_OFFSET(a0)
	bne.w fail
	jsr runtime_profile.opforgeRuntimeProfileLeaveVmV1
	cmpi.l #$00010000, progress.OPASM_PROGRESS_VM_SERVICE_ID_OFFSET(a0)
	bne.w fail
	cmpi.l #runtime_profile.OPFORGE_RUNTIME_PROGRAM_TOKENIZER, progress.OPASM_PROGRESS_PROGRAM_ID_OFFSET(a0)
	bne.w fail
	moveq #runtime_profile.OPFORGE_RUNTIME_SERVICE_SELECTION, d0
	jsr runtime_profile.opforgeRuntimeProfileEnterServiceV1
	cmpi.l #$00010002, progress.OPASM_PROGRESS_VM_SERVICE_ID_OFFSET(a0)
	bne.w fail
	cmpi.l #runtime_profile.OPFORGE_RUNTIME_PROGRAM_TOKENIZER, progress.OPASM_PROGRESS_PROGRAM_ID_OFFSET(a0)
	bne.w fail
	moveq #runtime_profile.OPFORGE_RUNTIME_SERVICE_VALUE, d0
	jsr runtime_profile.opforgeRuntimeProfileEnterServiceV1
	cmpi.l #$00010008, progress.OPASM_PROGRESS_VM_SERVICE_ID_OFFSET(a0)
	bne.w fail
	jsr runtime_profile.opforgeRuntimeProfileLeaveServiceV1
	cmpi.l #$00010002, progress.OPASM_PROGRESS_VM_SERVICE_ID_OFFSET(a0)
	bne.w fail
	jsr runtime_profile.opforgeRuntimeProfileLeaveServiceV1
	jsr runtime_profile.opforgeRuntimeProfileLeaveVmV1
	tst.l progress.OPASM_PROGRESS_VM_SERVICE_ID_OFFSET(a0)
	bne.w fail
	tst.l progress.OPASM_PROGRESS_PROGRAM_ID_OFFSET(a0)
	bne.w fail
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_SELECTION, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_ENCODING, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	jsr runtime_profile.opforgeRuntimeProfileGetRecordV1
	movea.l a0, a3
	lea runtime_profile.OPFORGE_RUNTIME_VM_INVOCATIONS_OFFSET(a3), a4
	cmpi.l #1, (a4)
	bne.w fail
	cmpi.l #1, 8(a4)
	bne.w fail
	cmpi.l #1, 12(a4)
	bne.w fail
	cmpi.l #3, runtime_profile.OPFORGE_RUNTIME_VM_OPCODES_OFFSET(a3)
	bne.w fail
	lea runtime_profile.OPFORGE_RUNTIME_PROGRAM_INVOCATIONS_OFFSET(a3), a4
	cmpi.l #1, (a4)
	bne.w fail
	cmpi.l #1, 8(a4)
	bne.w fail
	cmpi.l #1, 12(a4)
	bne.w fail
	lea runtime_profile.OPFORGE_RUNTIME_SERVICE_INVOCATIONS_OFFSET(a3), a4
	cmpi.l #1, 4(a4)
	bne.w fail
	cmpi.l #1, 28(a4)
	bne.w fail
	cmpi.l #3, runtime_profile.OPFORGE_RUNTIME_OPCODE_PHASE_OFFSET(a3)
	bne.w fail
	cmpi.l #2, runtime_profile.OPFORGE_RUNTIME_SERVICE_PHASE_OFFSET(a3)
	bne.w fail
	cmpi.l #1, runtime_profile.OPFORGE_RUNTIME_SELECTION_CANDIDATES_OFFSET(a3)
	bne.w fail
	cmpi.l #1, runtime_profile.OPFORGE_RUNTIME_ENCODING_CANDIDATES_OFFSET(a3)
	bne.w fail

	; Saturate all five counter groups and issue an unknown ID visibly.
	move.l #$ffffffff, runtime_profile.OPFORGE_RUNTIME_VM_INVOCATIONS_OFFSET(a3)
	move.l #$ffffffff, runtime_profile.OPFORGE_RUNTIME_VM_OPCODES_OFFSET(a3)
	move.l #$ffffffff, runtime_profile.OPFORGE_RUNTIME_SERVICE_INVOCATIONS_OFFSET(a3)
	move.l #$ffffffff, runtime_profile.OPFORGE_RUNTIME_SELECTION_CANDIDATES_OFFSET(a3)
	move.l #$ffffffff, runtime_profile.OPFORGE_RUNTIME_OPCODE_PHASE_OFFSET(a3)
	moveq #runtime_profile.OPFORGE_RUNTIME_VM_TKVM, d0
	moveq #runtime_profile.OPFORGE_RUNTIME_PROGRAM_TOKENIZER, d1
	jsr runtime_profile.opforgeRuntimeProfileEnterVmV1
	jsr runtime_profile.opforgeRuntimeProfileRecordOpcodeV1
	moveq #runtime_profile.OPFORGE_RUNTIME_SERVICE_EXPRESSION, d0
	jsr runtime_profile.opforgeRuntimeProfileEnterServiceV1
	moveq #runtime_profile.OPFORGE_RUNTIME_CANDIDATE_SELECTION, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	moveq #99, d0
	jsr runtime_profile.opforgeRuntimeProfileEnterServiceV1
	cmpi.l #63, runtime_profile.OPFORGE_RUNTIME_OVERFLOW_OFFSET(a3)
	bne.w fail
	; Restore the authoritative OFPR pointer before the existing bridge checks.
	jsr progress.opasmProgressGetRecordV1
.endif
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	moveq #38, d7
	jsr platform_profile.opforgePlatformProfileClassSourceV1
	jsr platform_profile.opforgePlatformProfileRecordOpenV1
	moveq #1, d0
	moveq #1, d1
	jsr platform_profile.opforgePlatformProfileRecordReadV1
	moveq #-1, d0
	moveq #8, d1
	jsr platform_profile.opforgePlatformProfileRecordReadV1
	moveq #3, d0
	jsr platform_profile.opforgePlatformProfileRecordReadV1
	jsr platform_profile.opforgePlatformProfileRecordCloseV1
	jsr platform_profile.opforgePlatformProfileClassBootstrapV1
	jsr platform_profile.opforgePlatformProfileRecordOpenV1
	moveq #0, d0
	moveq #1, d1
	jsr platform_profile.opforgePlatformProfileRecordReadV1
	jsr platform_profile.opforgePlatformProfileRecordCloseV1
	jsr platform_profile.opforgePlatformProfileClassArtifactV1
	jsr platform_profile.opforgePlatformProfileClassModuleV1
	jsr platform_profile.opforgePlatformProfileRecordOpenV1
	moveq #6, d0
	moveq #6, d1
	jsr platform_profile.opforgePlatformProfileRecordReadV1
	jsr platform_profile.opforgePlatformProfileRecordCloseV1
	jsr platform_profile.opforgePlatformProfileClassPackageV1
	jsr platform_profile.opforgePlatformProfileRecordOpenV1
	moveq #7, d0
	moveq #7, d1
	jsr platform_profile.opforgePlatformProfileRecordReadV1
	jsr platform_profile.opforgePlatformProfileRecordCloseV1
	jsr platform_profile.opforgePlatformProfileClassArtifactV1
	jsr platform_profile.opforgePlatformProfileRecordOpenV1
	moveq #4, d0
	moveq #4, d1
	jsr platform_profile.opforgePlatformProfileRecordWriteV1
	moveq #-1, d0
	jsr platform_profile.opforgePlatformProfileRecordWriteV1
	moveq #2, d0
	jsr platform_profile.opforgePlatformProfileRecordWriteV1
	jsr platform_profile.opforgePlatformProfileRecordCloseV1
	moveq #6, d0
	moveq #1, d1
	jsr platform_profile.opforgePlatformProfileSetContextV1
	jsr platform_profile.opforgePlatformProfileGetRecordV1
	movea.l a0, a4
	jsr platform_profile.opforgePlatformProfileRangeSessionV1
	move.l #8, d0
	jsr platform_profile.opforgePlatformProfileClearRequestedV1
	cmpi.l #8, HARNESS_SESSION_CLEAR_REQUESTED(a4)
	bne.w fail
	tst.l HARNESS_SESSION_CLEAR_COMPLETED(a4)
	bne.w fail
	jsr platform_profile.opforgePlatformProfileClearCompletedV1
	cmpi.l #8, HARNESS_SESSION_CLEAR_COMPLETED(a4)
	bne.w fail
	cmpi.l #8, HARNESS_LAYOUT_CLEAR_COMPLETED(a4)
	bne.w fail
	tst.w platform_profile.OPFORGE_PLATFORM_CURRENT_RANGE_OFFSET(a4)
	bne.w fail
	move.l #3, d0
	jsr platform_profile.opforgePlatformProfileRecordCopyV1
	jsr platform_profile.opforgePlatformProfileRecordLogicalLineV1
	jsr platform_profile.opforgePlatformProfileRecordModuleCandidateV1
	jsr platform_profile.opforgePlatformProfileGetRecordV1
	movea.l a0, a4
	cmpi.l #1, platform_profile.OPFORGE_PLATFORM_OPENS_OFFSET(a4)
	bne.w fail
	cmpi.l #1, HARNESS_BOOTSTRAP_OPENS_OFFSET(a4)
	bne.w fail
	cmpi.l #1, HARNESS_ARTIFACT_OPENS_OFFSET(a4)
	bne.w fail
	cmpi.l #1, HARNESS_MODULE_OPENS_OFFSET(a4)
	bne.w fail
	cmpi.l #1, HARNESS_PACKAGE_OPENS_OFFSET(a4)
	bne.w fail
	cmpi.l #1, HARNESS_MODULE_CLOSES_OFFSET(a4)
	bne.w fail
	cmpi.l #1, HARNESS_PACKAGE_CLOSES_OFFSET(a4)
	bne.w fail
	cmpi.l #1, HARNESS_MODULE_READS_OFFSET(a4)
	bne.w fail
	cmpi.l #1, HARNESS_PACKAGE_READS_OFFSET(a4)
	bne.w fail
	cmpi.l #6, HARNESS_MODULE_READ_BYTES_OFFSET(a4)
	bne.w fail
	cmpi.l #7, HARNESS_PACKAGE_READ_BYTES_OFFSET(a4)
	bne.w fail
	cmpi.l #4, platform_profile.OPFORGE_PLATFORM_READ_BYTES_OFFSET(a4)
	bne.w fail
	cmpi.l #4, platform_profile.OPFORGE_PLATFORM_SOURCE_BYTES_OFFSET(a4)
	bne.w fail
	cmpi.l #3, platform_profile.OPFORGE_PLATFORM_READS_OFFSET(a4)
	bne.w fail
	cmpi.l #2, platform_profile.OPFORGE_PLATFORM_SHORT_READS_OFFSET(a4)
	bne.w fail
	cmpi.l #6, HARNESS_ARTIFACT_WRITE_BYTES_OFFSET(a4)
	bne.w fail
	cmpi.l #8, platform_profile.OPFORGE_PLATFORM_CLEAR_REQUESTED_OFFSET(a4)
	bne.w fail
	cmpi.l #3, platform_profile.OPFORGE_PLATFORM_COPY_COMPLETED_OFFSET(a4)
	bne.w fail
	cmpi.l #3, HARNESS_OTHER_COPY_COMPLETED(a4)
	bne.w fail
	cmpi.l #1, platform_profile.OPFORGE_PLATFORM_LOGICAL_LINES_OFFSET(a4)
	bne.w fail
	cmpi.l #1, platform_profile.OPFORGE_PLATFORM_MODULE_CANDIDATES_OFFSET(a4)
	bne.w fail
	; Exercise both bulk groups at saturation without touching a real buffer.
	move.l #$ffffffff, platform_profile.OPFORGE_PLATFORM_CLEAR_CALLS_OFFSET(a4)
	move.l #$fffffffe, platform_profile.OPFORGE_PLATFORM_CLEAR_REQUESTED_OFFSET(a4)
	move.l #$fffffffe, platform_profile.OPFORGE_PLATFORM_CLEAR_COMPLETED_OFFSET(a4)
	move.l #$ffffffff, platform_profile.OPFORGE_PLATFORM_COPY_CALLS_OFFSET(a4)
	move.l #$fffffffe, platform_profile.OPFORGE_PLATFORM_COPY_REQUESTED_OFFSET(a4)
	move.l #$fffffffe, platform_profile.OPFORGE_PLATFORM_COPY_COMPLETED_OFFSET(a4)
	moveq #8, d0
	jsr platform_profile.opforgePlatformProfileRecordClearV1
	jsr platform_profile.opforgePlatformProfileRecordCopyV1
	cmpi.l #$ffffffff, platform_profile.OPFORGE_PLATFORM_CLEAR_REQUESTED_OFFSET(a4)
	bne.w fail
	cmpi.l #$ffffffff, platform_profile.OPFORGE_PLATFORM_CLEAR_COMPLETED_OFFSET(a4)
	bne.w fail
	cmpi.l #$ffffffff, platform_profile.OPFORGE_PLATFORM_COPY_REQUESTED_OFFSET(a4)
	bne.w fail
	cmpi.l #$ffffffff, platform_profile.OPFORGE_PLATFORM_COPY_COMPLETED_OFFSET(a4)
	bne.w fail
	move.l platform_profile.OPFORGE_PLATFORM_OVERFLOW_OFFSET(a4), d0
	andi.l #platform_profile.OPFORGE_PLATFORM_OVERFLOW_CLEAR, d0
	beq.w fail
	move.l platform_profile.OPFORGE_PLATFORM_OVERFLOW_OFFSET(a4), d0
	andi.l #platform_profile.OPFORGE_PLATFORM_OVERFLOW_COPY, d0
	beq.w fail
	move.l #$ffffffff, platform_profile.OPFORGE_PLATFORM_OPENS_OFFSET(a4)
	jsr platform_profile.opforgePlatformProfileClassSourceV1
	jsr platform_profile.opforgePlatformProfileRecordOpenV1
	moveq #99, d0
	jsr platform_profile.opforgePlatformProfileSetClassV1
	move.l platform_profile.OPFORGE_PLATFORM_OVERFLOW_OFFSET(a4), d0
	andi.l #platform_profile.OPFORGE_PLATFORM_OVERFLOW_OPENS, d0
	beq.w fail
	move.l platform_profile.OPFORGE_PLATFORM_OVERFLOW_OFFSET(a4), d0
	andi.l #platform_profile.OPFORGE_PLATFORM_OVERFLOW_UNKNOWN_ID, d0
	beq.w fail
	jsr progress.opasmProgressGetRecordV1
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
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	jsr runtime_profile.opforgeRuntimeProfileGetRecordV1
	movea.l a0, a3
	move.w runtime_profile.OPFORGE_RUNTIME_FLAGS_OFFSET(a3), d0
	andi.w #runtime_profile.OPFORGE_RUNTIME_FLAG_INCOMPLETE, d0
	beq.w fail
	cmpi.l #HARNESS_FAIL, runtime_profile.OPFORGE_RUNTIME_EXIT_STATUS_OFFSET(a3)
	bne.w fail
	cmpi.l #63, runtime_profile.OPFORGE_RUNTIME_OVERFLOW_OFFSET(a3)
	bne.w fail
.endif
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileGetRecordV1
	movea.l a0, a4
	cmpi.w #HARNESS_PLATFORM_INCOMPLETE, platform_profile.OPFORGE_PLATFORM_FLAGS_OFFSET(a4)
	bne.w fail
	cmpi.l #HARNESS_FAIL, platform_profile.OPFORGE_PLATFORM_EXIT_STATUS_OFFSET(a4)
	bne.w fail
	moveq #0, d0
	jsr platform_profile.opforgePlatformProfileFinishV1
	cmpi.l #HARNESS_FAIL, platform_profile.OPFORGE_PLATFORM_EXIT_STATUS_OFFSET(a4)
	bne.w fail
	; Exercise the actual bulk helpers, not just direct counter calls.
	moveq #39, d7
	lea nextTick, a0
	jsr progress.opasmProgressBeginRunV1
	move.l #$aabbccdd, HarnessBulkBuffer
	move.l #$aabbccdd, HarnessBulkBufferTail
	move.l #$11223344, HarnessBulkSource
	jsr platform_profile.opforgePlatformProfileRangeStateV1
	lea HarnessBulkBuffer, a0
	addq.l #1, a0
	moveq #5, d0
	jsr copy.clearBytes
	tst.l d0
	bne.w fail
	cmpi.l #$aa000000, HarnessBulkBuffer
	bne.w fail
	cmpi.l #$0000ccdd, HarnessBulkBufferTail
	bne.w fail
	jsr platform_profile.opforgePlatformProfileRangePackageV1
	lea HarnessBulkSource, a1
	lea HarnessBulkBuffer, a2
	addq.l #1, a2
	moveq #3, d0
	jsr copy.copyBytes
	tst.l d0
	bne.w fail
	cmpi.l #$aa112233, HarnessBulkBuffer
	bne.w fail
	; Fixed strings retain the high half of D0 while copying only its low word.
	lea HarnessBulkSource, a0
	lea HarnessBulkBuffer, a1
	move.l #$abcd0003, d0
	jsr copy.copyFixedString
	cmpi.l #$abcd0003, d0
	bne.w fail
	cmpi.l #$11223333, HarnessBulkBuffer
	bne.w fail
	; Zero length must not access either null pointer and must retain X/Z.
	suba.l a1, a1
	suba.l a2, a2
	moveq #0, d0
	move.w #$15, ccr
	jsr copy.copyBytes
	move.w ccr, -(sp)
	move.w (sp)+, d3
	cmpi.w #$14, d3
	bne.w fail
	jsr platform_profile.opforgePlatformProfileGetRecordV1
	movea.l a0, a4
	cmpi.l #5, platform_profile.OPFORGE_PLATFORM_CLEAR_COMPLETED_OFFSET(a4)
	bne.w fail
	cmpi.l #3, platform_profile.OPFORGE_PLATFORM_COPY_CALLS_OFFSET(a4)
	bne.w fail
	cmpi.l #6, platform_profile.OPFORGE_PLATFORM_COPY_COMPLETED_OFFSET(a4)
	bne.w fail
	moveq #0, d0
	jsr progress.opasmProgressFinishV1
	jsr platform_profile.opforgePlatformProfileGetRecordV1
	movea.l a0, a4
	move.w platform_profile.OPFORGE_PLATFORM_FLAGS_OFFSET(a4), d0
	andi.w #platform_profile.OPFORGE_PLATFORM_FLAG_COMPLETE, d0
	beq.w fail
	tst.l platform_profile.OPFORGE_PLATFORM_EXIT_STATUS_OFFSET(a4)
	bne.w fail
	; Repeated sealing also preserves a successful terminal state.
	moveq #HARNESS_FAIL, d0
	jsr platform_profile.opforgePlatformProfileFinishV1
	cmpi.w #HARNESS_PLATFORM_COMPLETE, platform_profile.OPFORGE_PLATFORM_FLAGS_OFFSET(a4)
	bne.w fail
	tst.l platform_profile.OPFORGE_PLATFORM_EXIT_STATUS_OFFSET(a4)
	bne.w fail
.endif
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
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	jsr runtime_profile.opforgeRuntimeProfileGetRecordV1
	movea.l a0, a3
	move.w runtime_profile.OPFORGE_RUNTIME_FLAGS_OFFSET(a3), d0
	andi.w #runtime_profile.OPFORGE_RUNTIME_FLAG_COMPLETE, d0
	beq.w fail
	tst.l runtime_profile.OPFORGE_RUNTIME_EXIT_STATUS_OFFSET(a3)
	bne.w fail
	tst.l runtime_profile.OPFORGE_RUNTIME_OVERFLOW_OFFSET(a3)
	bne.w fail
.endif
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileGetRecordV1
	movea.l a0, a4
	move.w platform_profile.OPFORGE_PLATFORM_FLAGS_OFFSET(a4), d0
	andi.w #platform_profile.OPFORGE_PLATFORM_FLAG_COMPLETE, d0
	beq.w fail
	tst.l platform_profile.OPFORGE_PLATFORM_EXIT_STATUS_OFFSET(a4)
	bne.w fail
.endif

	moveq #0, d0
	rts
fail
	move.l d7, d0
	rts
	.bend  ; start

; Deterministic coarse-tick callback. Every sampled boundary advances 10 ticks.
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
; Exact subgroup oracle: a disabled observer is a CCR/register-preserving RTS,
; while the other group still records its own operation and terminal metadata.
verifyPlatformSubgroups	.block
	jsr platform_profile.opforgePlatformProfileClassSourceV1
	moveq #3, d0
	moveq #8, d1
	jsr platform_profile.opforgePlatformProfileRecordReadV1
	jsr platform_profile.opforgePlatformProfileRangeStateV1
	moveq #5, d0
	jsr platform_profile.opforgePlatformProfileRecordClearV1
	jsr platform_profile.opforgePlatformProfileGetRecordV1
	movea.l a0, a4
	move.w platform_profile.OPFORGE_PLATFORM_FLAGS_OFFSET(a4), d2
.ifdef OPFORGE_PROGRESS_PLATFORM_NO_IO
	andi.w #platform_profile.OPFORGE_PLATFORM_FLAG_IO_ENABLED, d2
	bne.s fail
	tst.l platform_profile.OPFORGE_PLATFORM_READS_OFFSET(a4)
	bne.s fail
	tst.w platform_profile.OPFORGE_PLATFORM_CURRENT_CLASS_OFFSET(a4)
	bne.s fail
.else
	andi.w #platform_profile.OPFORGE_PLATFORM_FLAG_IO_ENABLED, d2
	beq.s fail
	cmpi.l #1, platform_profile.OPFORGE_PLATFORM_READS_OFFSET(a4)
	bne.s fail
	cmpi.l #3, platform_profile.OPFORGE_PLATFORM_READ_BYTES_OFFSET(a4)
	bne.s fail
.endif
	move.w platform_profile.OPFORGE_PLATFORM_FLAGS_OFFSET(a4), d2
.ifdef OPFORGE_PROGRESS_PLATFORM_NO_BULK
	andi.w #platform_profile.OPFORGE_PLATFORM_FLAG_BULK_ENABLED, d2
	bne.s fail
	tst.l platform_profile.OPFORGE_PLATFORM_CLEAR_CALLS_OFFSET(a4)
	bne.s fail
.else
	andi.w #platform_profile.OPFORGE_PLATFORM_FLAG_BULK_ENABLED, d2
	beq.s fail
	cmpi.l #1, platform_profile.OPFORGE_PLATFORM_CLEAR_CALLS_OFFSET(a4)
	bne.s fail
	cmpi.l #5, platform_profile.OPFORGE_PLATFORM_CLEAR_COMPLETED_OFFSET(a4)
	bne.s fail
.endif
	tst.w platform_profile.OPFORGE_PLATFORM_CURRENT_RANGE_OFFSET(a4)
	bne.s fail
	tst.l platform_profile.OPFORGE_PLATFORM_OVERFLOW_OFFSET(a4)
	bne.s fail
	moveq #0, d0
	jsr progress.opasmProgressFinishV1
	rts
fail
	moveq #HARNESS_FAIL, d0
	jsr progress.opasmProgressFinishV1
	rts
	.bend  ; verifyPlatformSubgroups
.endif

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
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
HarnessBulkBuffer
	.res long, 1
HarnessBulkBufferTail
	.res long, 1
HarnessBulkSource
	.res long, 1
.endif
	.endsection

	.output "build/opasm_progress_harness", format=hunk, sections=entry, code, bss
	.endmodule
