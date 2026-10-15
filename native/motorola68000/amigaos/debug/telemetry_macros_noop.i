; Disabled telemetry API: keep signatures without capturing implementation bodies.
TELEMETRY_VM_ENTER	.macro vm_id, program_id
.endmacro
TELEMETRY_VM_OPCODE	.macro vm_id, program_id
.endmacro
TELEMETRY_VM_LEAVE	.macro
.endmacro
TELEMETRY_SERVICE_ENTER	.macro service_id
.endmacro
TELEMETRY_SERVICE_LEAVE	.macro
.endmacro
TELEMETRY_CANDIDATE	.macro candidate_kind
.endmacro
TELEMETRY_COMPACT	.macro kind, amount
.endmacro
TELEMETRY_COMPACT_WORD	.macro kind, amount
.endmacro
