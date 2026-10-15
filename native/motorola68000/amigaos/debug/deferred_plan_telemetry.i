; Zero optional source-selected fields without nesting assembler conditionals
; inside preprocessing gates. Every emitted helper preserves registers and CCR.
DEFERRED_ZERO_PAIR	.macro state, created, bound
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	clr.l .created(.state)
	clr.l .bound(.state)
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
; Deferred-plan owner counters; ordinary builds emit no storage or updates.
DEFERRED_COUNTER	.macro state, field, amount
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l .amount, d0
	add.l d0, .field(.state)
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endif
.endmacro
DEFERRED_CLEAR	.macro state
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	clr.l State.Created(.state)
	clr.l State.Bound(.state)
	clr.l State.Hits(.state)
	clr.l State.StoredBytes(.state)
	move.w (sp)+, ccr
.endif
.endif
.endif
.if ACQUIRE_MACRO_CANDIDATES
	.DEFERRED_ZERO_PAIR .state, State.AcquiredCreated, State.AcquiredBound
.endif
.if DEFER_MACRO_LINES
	.DEFERRED_ZERO_PAIR .state, State.LinesCreated, State.LinesBound
.endif
.endmacro
; Count only acquisition recipes; preserve flags across tag selection too.
DEFERRED_ACQUISITION_COUNTER	.macro state, field, tag
.if ACQUIRE_MACRO_CANDIDATES
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	cmpi.w #ACQUIRE_CALL, .tag
	bne.w done
	.DEFERRED_COUNTER .state, .field, #1
done
	move.w (sp)+, ccr
.endif
.endif
.endif
.endif
.endmacro
; Count whole-line recipes only, including flag-neutral tag selection.
DEFERRED_LINE_COUNTER	.macro state, field, tag
.if DEFER_MACRO_LINES
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	move.w ccr, -(sp)
	cmpi.w #ACQUIRE_LINE, .tag
	bne.w done
	.DEFERRED_COUNTER .state, .field, #1
done
	move.w (sp)+, ccr
.endif
.endif
.endif
.endif
.endmacro
