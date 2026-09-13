; Optional runtime telemetry call-site macros.
;
; Telemetry exists only when both the debug-contract and runtime-counter build
; gates are enabled. Enabled expansions preserve every caller register and the
; original CCR. Disabled expansions emit no bytes and import no runtime module.

.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	.use debug.amigaos.runtime_profile as runtime_profile
.endif
.endif

TELEMETRY_VM_ENTER	.macro vm_id, program_id
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	moveq #.vm_id, d0
	moveq #.program_id, d1
	jsr runtime_profile.opforgeRuntimeProfileEnterVmV1
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endif
.endmacro

TELEMETRY_VM_OPCODE	.macro vm_id, program_id
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	move.w ccr, -(sp)
	movem.l d0-d1, -(sp)
	moveq #.vm_id, d0
	moveq #.program_id, d1
	jsr runtime_profile.opforgeRuntimeProfileRecordOpcodeV1
	movem.l (sp)+, d0-d1
	move.w (sp)+, ccr
.endif
.endif
.endmacro

TELEMETRY_VM_LEAVE	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	move.w ccr, -(sp)
	jsr runtime_profile.opforgeRuntimeProfileLeaveVmV1
	move.w (sp)+, ccr
.endif
.endif
.endmacro

TELEMETRY_SERVICE_ENTER	.macro service_id
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #.service_id, d0
	jsr runtime_profile.opforgeRuntimeProfileEnterServiceV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro

TELEMETRY_SERVICE_LEAVE	.macro
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	move.w ccr, -(sp)
	jsr runtime_profile.opforgeRuntimeProfileLeaveServiceV1
	move.w (sp)+, ccr
.endif
.endif
.endmacro

TELEMETRY_CANDIDATE	.macro candidate_kind
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_PROGRESS_RUNTIME_COUNTERS
	move.w ccr, -(sp)
	move.l d0, -(sp)
	moveq #.candidate_kind, d0
	jsr runtime_profile.opforgeRuntimeProfileRecordCandidateV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
.endif
.endmacro
