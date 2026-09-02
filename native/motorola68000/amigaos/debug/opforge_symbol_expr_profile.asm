; Bounded observation-only symbol and expression work profile.
;
; @opforge-owner: debug.amigaos.symbol_expr_profile
; @opforge-slice: documentation/plans/slices/native-porting-slice-symbol-expression-work-v1.toml
; @opforge-role: implementation

	.module debug.amigaos.symbol_expr_profile
	.cpu 68020

	.pub

OPFORGE_SYMBOL_EXPR_MAGIC                 = $4f465345; "OFSE"
OPFORGE_SYMBOL_EXPR_SCHEMA_VERSION        = 1
OPFORGE_SYMBOL_EXPR_RECORD_BYTES          = 256

OPFORGE_SYMBOL_EXPR_FLAG_ACTIVE           = 1
OPFORGE_SYMBOL_EXPR_FLAG_COMPLETE         = 2
OPFORGE_SYMBOL_EXPR_FLAG_INCOMPLETE       = 4
OPFORGE_SYMBOL_EXPR_FLAG_DETAIL           = 8

OPFORGE_SYMBOL_EXPR_CLASS_EXACT           = 0
OPFORGE_SYMBOL_EXPR_CLASS_SCOPED          = 1
OPFORGE_SYMBOL_EXPR_CLASS_IMPORTED        = 2
OPFORGE_SYMBOL_EXPR_CLASS_FINAL_COMPONENT = 3
OPFORGE_SYMBOL_EXPR_CLASS_COUNT           = 4

OPFORGE_SYMBOL_EXPR_OUTCOME_HIT           = 0
OPFORGE_SYMBOL_EXPR_OUTCOME_MISS          = 1
OPFORGE_SYMBOL_EXPR_OUTCOME_AMBIGUOUS     = 2

OPFORGE_SYMBOL_EXPR_EVENT_REQUEST         = 0
OPFORGE_SYMBOL_EXPR_EVENT_PARSE           = 1
OPFORGE_SYMBOL_EXPR_EVENT_COMPILE         = 2
OPFORGE_SYMBOL_EXPR_EVENT_BIND            = 3
OPFORGE_SYMBOL_EXPR_EVENT_EVALUATE        = 4
OPFORGE_SYMBOL_EXPR_EVENT_SUCCESS         = 5
OPFORGE_SYMBOL_EXPR_EVENT_FAILURE         = 6

OPFORGE_SYMBOL_EXPR_OVERFLOW_CALLS         = 1
OPFORGE_SYMBOL_EXPR_OVERFLOW_CANDIDATES    = 2
OPFORGE_SYMBOL_EXPR_OVERFLOW_BYTES         = 4
OPFORGE_SYMBOL_EXPR_OVERFLOW_OUTCOMES      = 8
OPFORGE_SYMBOL_EXPR_OVERFLOW_EXPRESSION    = 16
OPFORGE_SYMBOL_EXPR_OVERFLOW_HISTOGRAM     = 32
OPFORGE_SYMBOL_EXPR_OVERFLOW_CHAIN         = 64

OPFORGE_SYMBOL_EXPR_MAGIC_OFFSET           = 0
OPFORGE_SYMBOL_EXPR_SCHEMA_OFFSET          = 4
OPFORGE_SYMBOL_EXPR_FLAGS_OFFSET           = 6
OPFORGE_SYMBOL_EXPR_RUN_ID_OFFSET          = 8
OPFORGE_SYMBOL_EXPR_PHASE_OFFSET           = 12
OPFORGE_SYMBOL_EXPR_PASS_OFFSET            = 14
OPFORGE_SYMBOL_EXPR_EXACT_CALLS_OFFSET     = 20
OPFORGE_SYMBOL_EXPR_SCOPED_CALLS_OFFSET    = 24
OPFORGE_SYMBOL_EXPR_IMPORTED_CALLS_OFFSET  = 28
OPFORGE_SYMBOL_EXPR_FINAL_CALLS_OFFSET     = 32
OPFORGE_SYMBOL_EXPR_EXACT_CANDIDATES_OFFSET = 36
OPFORGE_SYMBOL_EXPR_SCOPED_CANDIDATES_OFFSET = 40
OPFORGE_SYMBOL_EXPR_IMPORTED_CANDIDATES_OFFSET = 44
OPFORGE_SYMBOL_EXPR_FINAL_CANDIDATES_OFFSET = 48
OPFORGE_SYMBOL_EXPR_EXACT_BYTES_OFFSET     = 52
OPFORGE_SYMBOL_EXPR_SCOPED_BYTES_OFFSET    = 56
OPFORGE_SYMBOL_EXPR_IMPORTED_BYTES_OFFSET  = 60
OPFORGE_SYMBOL_EXPR_FINAL_BYTES_OFFSET     = 64
OPFORGE_SYMBOL_EXPR_EXACT_HITS_OFFSET      = 68
OPFORGE_SYMBOL_EXPR_SCOPED_HITS_OFFSET     = 72
OPFORGE_SYMBOL_EXPR_IMPORTED_HITS_OFFSET   = 76
OPFORGE_SYMBOL_EXPR_FINAL_HITS_OFFSET      = 80
OPFORGE_SYMBOL_EXPR_EXACT_MISSES_OFFSET    = 84
OPFORGE_SYMBOL_EXPR_SCOPED_MISSES_OFFSET   = 88
OPFORGE_SYMBOL_EXPR_IMPORTED_MISSES_OFFSET = 92
OPFORGE_SYMBOL_EXPR_FINAL_MISSES_OFFSET    = 96
OPFORGE_SYMBOL_EXPR_FINAL_AMBIGUOUS_OFFSET = 100
OPFORGE_SYMBOL_EXPR_SNAPSHOT_CANDIDATES_OFFSET = 104
OPFORGE_SYMBOL_EXPR_SNAPSHOT_BYTES_OFFSET  = 108
OPFORGE_SYMBOL_EXPR_REQUESTS_OFFSET        = 112
OPFORGE_SYMBOL_EXPR_PARSE_CALLS_OFFSET     = 116
OPFORGE_SYMBOL_EXPR_COMPILE_CALLS_OFFSET   = 120
OPFORGE_SYMBOL_EXPR_BIND_CALLS_OFFSET      = 124
OPFORGE_SYMBOL_EXPR_EVALUATE_CALLS_OFFSET  = 128
OPFORGE_SYMBOL_EXPR_SUCCESSES_OFFSET       = 132
OPFORGE_SYMBOL_EXPR_FAILURES_OFFSET        = 136
OPFORGE_SYMBOL_EXPR_PROBE_ZERO_OFFSET      = 140
OPFORGE_SYMBOL_EXPR_PROBE_ONE_OFFSET       = 144
OPFORGE_SYMBOL_EXPR_PROBE_TWO_OFFSET       = 148
OPFORGE_SYMBOL_EXPR_PROBE_THREE_OFFSET     = 152
OPFORGE_SYMBOL_EXPR_PROBE_FOUR_PLUS_OFFSET = 156
OPFORGE_SYMBOL_EXPR_MAX_PROBES_OFFSET      = 160
OPFORGE_SYMBOL_EXPR_MAX_CHAIN_OFFSET       = 164
OPFORGE_SYMBOL_EXPR_LOOKUP_PASS_ONE_OFFSET = 168
OPFORGE_SYMBOL_EXPR_LOOKUP_LAYOUT_OFFSET   = 172
OPFORGE_SYMBOL_EXPR_LOOKUP_FINAL_OFFSET    = 176
OPFORGE_SYMBOL_EXPR_LOOKUP_OTHER_OFFSET    = 180
OPFORGE_SYMBOL_EXPR_EXPR_PASS_ONE_OFFSET   = 184
OPFORGE_SYMBOL_EXPR_EXPR_LAYOUT_OFFSET     = 188
OPFORGE_SYMBOL_EXPR_EXPR_FINAL_OFFSET      = 192
OPFORGE_SYMBOL_EXPR_EXPR_OTHER_OFFSET      = 196
OPFORGE_SYMBOL_EXPR_OVERFLOW_OFFSET        = 200
OPFORGE_SYMBOL_EXPR_EXIT_STATUS_OFFSET     = 204

OPFORGE_SYMBOL_EXPR_PHASE_PASS_ONE         = 5
OPFORGE_SYMBOL_EXPR_PHASE_LAYOUT           = 6
OPFORGE_SYMBOL_EXPR_PHASE_FINAL            = 7
OPFORGE_SYMBOL_EXPR_CHAIN_BUCKETS           = 256

	.section code, kind=code

; Return the authoritative fixed-size symbol/expression record.
; Inputs: none. Output: A0 = 256-byte record. Other registers and CCR preserved.
opforgeSymbolExprProfileGetRecordV1	.block
	lea OpcoreSymbolExprRecord, a0
	rts
	.bend  ; opforgeSymbolExprProfileGetRecordV1

; Start one correlated symbol/expression profile.
; Inputs: D0.L = progress run ID. Outputs/clobbers: none; CCR preserved.
opforgeSymbolExprProfileBeginRunV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	move.l d0, d7
	lea OpcoreSymbolExprRecord, a5
	movea.l a5, a0
	moveq #0, d0
	moveq #(OPFORGE_SYMBOL_EXPR_RECORD_BYTES / 4) - 1, d1
clearRecordLoop
	move.l d0, (a0)+
	dbf d1, clearRecordLoop
	move.l #OPFORGE_SYMBOL_EXPR_MAGIC, OPFORGE_SYMBOL_EXPR_MAGIC_OFFSET(a5)
	move.w #OPFORGE_SYMBOL_EXPR_SCHEMA_VERSION, OPFORGE_SYMBOL_EXPR_SCHEMA_OFFSET(a5)
	move.w #OPFORGE_SYMBOL_EXPR_FLAG_ACTIVE, OPFORGE_SYMBOL_EXPR_FLAGS_OFFSET(a5)
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL
	ori.w #OPFORGE_SYMBOL_EXPR_FLAG_DETAIL, OPFORGE_SYMBOL_EXPR_FLAGS_OFFSET(a5)
	lea OpcoreSymbolExprChainDepths, a0
	moveq #0, d0
	move.w #OPFORGE_SYMBOL_EXPR_CHAIN_BUCKETS - 1, d1
clearChainLoop
	move.w d0, (a0)+
	dbf d1, clearChainLoop
.endif
	move.l d7, OPFORGE_SYMBOL_EXPR_RUN_ID_OFFSET(a5)
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeSymbolExprProfileBeginRunV1

; Retain the current progress phase and pass for attribution.
; Inputs: D0.W = phase; D1.W = pass. Outputs/clobbers: none; CCR preserved.
opforgeSymbolExprProfileSetContextV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpcoreSymbolExprRecord, a5
	bsr.w profileIsActive
	beq.s return
	move.w d0, OPFORGE_SYMBOL_EXPR_PHASE_OFFSET(a5)
	move.w d1, OPFORGE_SYMBOL_EXPR_PASS_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeSymbolExprProfileSetContextV1

; Count one completed lookup cascade.
; Inputs: D0.W = class; D1.W = outcome; D2.L = candidates; D3.L = compared bytes.
; Outputs/clobbers: none. CCR and D0-D7/A0-A6 are preserved; stack delta zero.
opforgeSymbolExprProfileRecordLookupV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	move.l d3, d7
	lea OpcoreSymbolExprRecord, a5
	bsr.w profileIsActive
	beq.w return
	moveq #0, d4
	move.w d0, d4
	cmpi.w #OPFORGE_SYMBOL_EXPR_CLASS_COUNT, d4
	bhs.w return
	moveq #0, d5
	move.w d1, d5
	move.l d2, d6
	move.l d4, d0
	lsl.l #2, d0
	lea OPFORGE_SYMBOL_EXPR_EXACT_CALLS_OFFSET(a5), a0
	adda.l d0, a0
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_CALLS, d1
	bsr.w profileIncrement
	bsr.w countLookupPhase
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL
	lea OPFORGE_SYMBOL_EXPR_EXACT_CANDIDATES_OFFSET(a5), a0
	adda.l d0, a0
	move.l d6, d0
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_CANDIDATES, d1
	bsr.w profileAdd
	move.l d4, d0
	lsl.l #2, d0
	lea OPFORGE_SYMBOL_EXPR_EXACT_BYTES_OFFSET(a5), a0
	adda.l d0, a0
	move.l d7, d0
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_BYTES, d1
	bsr.w profileAdd
.endif
	cmpi.w #OPFORGE_SYMBOL_EXPR_OUTCOME_HIT, d5
	bne.s checkMiss
	move.l d4, d0
	lsl.l #2, d0
	lea OPFORGE_SYMBOL_EXPR_EXACT_HITS_OFFSET(a5), a0
	adda.l d0, a0
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_OUTCOMES, d1
	bsr.w profileIncrement
	bra.s histogram
checkMiss
	cmpi.w #OPFORGE_SYMBOL_EXPR_OUTCOME_MISS, d5
	bne.s checkAmbiguous
	move.l d4, d0
	lsl.l #2, d0
	lea OPFORGE_SYMBOL_EXPR_EXACT_MISSES_OFFSET(a5), a0
	adda.l d0, a0
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_OUTCOMES, d1
	bsr.w profileIncrement
	bra.s histogram
checkAmbiguous
	cmpi.w #OPFORGE_SYMBOL_EXPR_OUTCOME_AMBIGUOUS, d5
	bne.s return
	cmpi.w #OPFORGE_SYMBOL_EXPR_CLASS_FINAL_COMPONENT, d4
	bne.s return
	lea OPFORGE_SYMBOL_EXPR_FINAL_AMBIGUOUS_OFFSET(a5), a0
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_OUTCOMES, d1
	bsr.w profileIncrement
histogram
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL
	tst.w d4
	bne.s return
	move.l d6, d0
	bsr.w countProbeHistogram
.endif
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeSymbolExprProfileRecordLookupV1

; Count one expression lifecycle event. Bind events also carry snapshot work.
; Inputs: D0.W = event; D1.L = candidates; D2.L = compared bytes.
; Outputs/clobbers: none. CCR and D0-D7/A0-A6 are preserved; stack delta zero.
opforgeSymbolExprProfileRecordExpressionV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpcoreSymbolExprRecord, a5
	bsr.w profileIsActive
	beq.w return
	moveq #0, d4
	move.w d0, d4
	move.l d1, d6
	move.l d2, d7
	cmpi.w #OPFORGE_SYMBOL_EXPR_EVENT_REQUEST, d4
	bne.s parse
	lea OPFORGE_SYMBOL_EXPR_REQUESTS_OFFSET(a5), a0
	bsr.w countExpressionField
	bsr.w countExpressionPhase
	bra.w return
parse
	cmpi.w #OPFORGE_SYMBOL_EXPR_EVENT_PARSE, d4
	bne.s compile
	lea OPFORGE_SYMBOL_EXPR_PARSE_CALLS_OFFSET(a5), a0
	bsr.w countExpressionField
	bra.w return
compile
	cmpi.w #OPFORGE_SYMBOL_EXPR_EVENT_COMPILE, d4
	bne.s bind
	lea OPFORGE_SYMBOL_EXPR_COMPILE_CALLS_OFFSET(a5), a0
	bsr.w countExpressionField
	bra.w return
bind
	cmpi.w #OPFORGE_SYMBOL_EXPR_EVENT_BIND, d4
	bne.s evaluate
	lea OPFORGE_SYMBOL_EXPR_BIND_CALLS_OFFSET(a5), a0
	bsr.w countExpressionField
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL
	lea OPFORGE_SYMBOL_EXPR_SNAPSHOT_CANDIDATES_OFFSET(a5), a0
	move.l d6, d0
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_CANDIDATES, d1
	bsr.w profileAdd
	lea OPFORGE_SYMBOL_EXPR_SNAPSHOT_BYTES_OFFSET(a5), a0
	move.l d7, d0
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_BYTES, d1
	bsr.w profileAdd
.endif
	bra.s return
evaluate
	cmpi.w #OPFORGE_SYMBOL_EXPR_EVENT_EVALUATE, d4
	bne.s success
	lea OPFORGE_SYMBOL_EXPR_EVALUATE_CALLS_OFFSET(a5), a0
	bsr.w countExpressionField
	bra.s return
success
	cmpi.w #OPFORGE_SYMBOL_EXPR_EVENT_SUCCESS, d4
	bne.s failure
	lea OPFORGE_SYMBOL_EXPR_SUCCESSES_OFFSET(a5), a0
	bsr.w countExpressionField
	bra.s return
failure
	cmpi.w #OPFORGE_SYMBOL_EXPR_EVENT_FAILURE, d4
	bne.s return
	lea OPFORGE_SYMBOL_EXPR_FAILURES_OFFSET(a5), a0
	bsr.w countExpressionField
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeSymbolExprProfileRecordExpressionV1

; Observe one label insertion without walking the production hash chain.
; Inputs: D0.W = hash bucket. Outputs/clobbers: none; CCR preserved.
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL
opforgeSymbolExprProfileRecordChainInsertV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpcoreSymbolExprRecord, a5
	bsr.w profileIsActive
	beq.s return
	cmpi.w #OPFORGE_SYMBOL_EXPR_CHAIN_BUCKETS, d0
	bhs.s return
	moveq #0, d1
	move.w d0, d1
	add.w d1, d1
	lea OpcoreSymbolExprChainDepths, a0
	adda.w d1, a0
	cmpi.w #-1, (a0)
	beq.s overflow
	addq.w #1, (a0)
	moveq #0, d2
	move.w (a0), d2
	cmp.l OPFORGE_SYMBOL_EXPR_MAX_CHAIN_OFFSET(a5), d2
	bls.s return
	move.l d2, OPFORGE_SYMBOL_EXPR_MAX_CHAIN_OFFSET(a5)
	bra.s return
overflow
	ori.l #OPFORGE_SYMBOL_EXPR_OVERFLOW_CHAIN, OPFORGE_SYMBOL_EXPR_OVERFLOW_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeSymbolExprProfileRecordChainInsertV1
.endif

; Seal the correlated record at the controlled progress boundary.
; Inputs: D0.L = guest/CLI status. Outputs/clobbers: none; CCR preserved.
opforgeSymbolExprProfileFinishV1	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	lea OpcoreSymbolExprRecord, a5
	bsr.w profileIsActive
	beq.s return
	move.l d0, OPFORGE_SYMBOL_EXPR_EXIT_STATUS_OFFSET(a5)
	andi.w #$fffe, OPFORGE_SYMBOL_EXPR_FLAGS_OFFSET(a5)
	tst.l d0
	bne.s incomplete
	ori.w #OPFORGE_SYMBOL_EXPR_FLAG_COMPLETE, OPFORGE_SYMBOL_EXPR_FLAGS_OFFSET(a5)
	bra.s return
incomplete
	ori.w #OPFORGE_SYMBOL_EXPR_FLAG_INCOMPLETE, OPFORGE_SYMBOL_EXPR_FLAGS_OFFSET(a5)
return
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; opforgeSymbolExprProfileFinishV1

	.priv

profileIsActive	.block
	move.w OPFORGE_SYMBOL_EXPR_FLAGS_OFFSET(a5), d3
	andi.w #OPFORGE_SYMBOL_EXPR_FLAG_ACTIVE, d3
	rts
	.bend  ; profileIsActive

; A0 = field; D1.L = overflow bit; A5 = record.
profileIncrement	.block
	cmpi.l #-1, (a0)
	beq.s overflow
	addq.l #1, (a0)
	rts
overflow
	or.l d1, OPFORGE_SYMBOL_EXPR_OVERFLOW_OFFSET(a5)
	rts
	.bend  ; profileIncrement

; A0 = field; D0.L = amount; D1.L = overflow bit; A5 = record.
profileAdd	.block
	move.l (a0), d2
	add.l d0, d2
	bcc.s store
	move.l #-1, d2
	or.l d1, OPFORGE_SYMBOL_EXPR_OVERFLOW_OFFSET(a5)
store
	move.l d2, (a0)
	rts
	.bend  ; profileAdd

countExpressionField	.block
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_EXPRESSION, d1
	bsr.w profileIncrement
	rts
	.bend  ; countExpressionField

countLookupPhase	.block
	moveq #0, d2
	move.w OPFORGE_SYMBOL_EXPR_PHASE_OFFSET(a5), d2
	lea OPFORGE_SYMBOL_EXPR_LOOKUP_OTHER_OFFSET(a5), a0
	cmpi.w #OPFORGE_SYMBOL_EXPR_PHASE_PASS_ONE, d2
	bne.s layout
	lea OPFORGE_SYMBOL_EXPR_LOOKUP_PASS_ONE_OFFSET(a5), a0
	bra.s count
layout
	cmpi.w #OPFORGE_SYMBOL_EXPR_PHASE_LAYOUT, d2
	bne.s final
	lea OPFORGE_SYMBOL_EXPR_LOOKUP_LAYOUT_OFFSET(a5), a0
	bra.s count
final
	cmpi.w #OPFORGE_SYMBOL_EXPR_PHASE_FINAL, d2
	bne.s count
	lea OPFORGE_SYMBOL_EXPR_LOOKUP_FINAL_OFFSET(a5), a0
count
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_CALLS, d1
	bsr.w profileIncrement
	rts
	.bend  ; countLookupPhase

countExpressionPhase	.block
	moveq #0, d2
	move.w OPFORGE_SYMBOL_EXPR_PHASE_OFFSET(a5), d2
	lea OPFORGE_SYMBOL_EXPR_EXPR_OTHER_OFFSET(a5), a0
	cmpi.w #OPFORGE_SYMBOL_EXPR_PHASE_PASS_ONE, d2
	bne.s layout
	lea OPFORGE_SYMBOL_EXPR_EXPR_PASS_ONE_OFFSET(a5), a0
	bra.s count
layout
	cmpi.w #OPFORGE_SYMBOL_EXPR_PHASE_LAYOUT, d2
	bne.s final
	lea OPFORGE_SYMBOL_EXPR_EXPR_LAYOUT_OFFSET(a5), a0
	bra.s count
final
	cmpi.w #OPFORGE_SYMBOL_EXPR_PHASE_FINAL, d2
	bne.s count
	lea OPFORGE_SYMBOL_EXPR_EXPR_FINAL_OFFSET(a5), a0
count
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_EXPRESSION, d1
	bsr.w profileIncrement
	rts
	.bend  ; countExpressionPhase

.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL
countProbeHistogram	.block
	lea OPFORGE_SYMBOL_EXPR_PROBE_FOUR_PLUS_OFFSET(a5), a0
	cmpi.l #4, d0
	bhs.s count
	move.l d0, d2
	lsl.l #2, d2
	lea OPFORGE_SYMBOL_EXPR_PROBE_ZERO_OFFSET(a5), a0
	adda.l d2, a0
count
	moveq #OPFORGE_SYMBOL_EXPR_OVERFLOW_HISTOGRAM, d1
	bsr.w profileIncrement
	cmp.l OPFORGE_SYMBOL_EXPR_MAX_PROBES_OFFSET(a5), d0
	bls.s return
	move.l d0, OPFORGE_SYMBOL_EXPR_MAX_PROBES_OFFSET(a5)
return
	rts
	.bend  ; countProbeHistogram
.endif

	.endsection

	.pub

	.section bss, kind=bss
	.align 4

OpcoreSymbolExprRecord
	.res byte, OPFORGE_SYMBOL_EXPR_RECORD_BYTES
.ifdef OPFORGE_PROGRESS_SYMBOL_EXPR_DETAIL
OpcoreSymbolExprChainDepths
	.res word, OPFORGE_SYMBOL_EXPR_CHAIN_BUCKETS
.endif

	.endsection
	.endmodule
