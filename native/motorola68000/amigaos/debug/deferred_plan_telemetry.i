; Deferred-plan owner counters; ordinary builds emit no storage or updates.
DEFERRED_COUNTER .macro state, field, amount
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
DEFERRED_CLEAR .macro state
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
.endmacro
