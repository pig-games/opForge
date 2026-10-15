; Disabled telemetry API: keep signatures without capturing implementation bodies.
BINDING_DIAGNOSTIC_CLEAR	.macro
.endmacro
BINDING_ATTEMPT	.macro stage, entry, related
.endmacro
BINDING_ATTEMPT_WORD	.macro stage, entryWord, related
.endmacro
BINDING_CANONICAL_TARGET	.macro stage, target, baseWord
.endmacro
BINDING_DIAGNOSTIC_COMMIT	.macro status, state, view
.endmacro
BINDING_DIAGNOSTIC_REPORT	.macro dosbase
.endmacro
BINDING_RECORD_FAILURE	.macro stage, cursor, limit, savedBaseOffset
.endmacro
BINDING_RECORD_VIEW_FAILURE	.macro stage, cursor, limit, base
.endmacro
