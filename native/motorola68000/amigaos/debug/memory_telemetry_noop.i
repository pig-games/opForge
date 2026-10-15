; Disabled telemetry API: keep signatures without capturing implementation bodies.
MEMORY_ALLOC	.macro amount
.endmacro
MEMORY_COUNTER_CLEAR	.macro counter
.endmacro
MEMORY_COUNTER_INC	.macro counter
.endmacro
MEMORY_COUNTER_INC_ZERO	.macro counter, value
.endmacro
MEMORY_COUNTER_ADD	.macro counter, amount
.endmacro
MEMORY_WORK	.macro index, amount
.endmacro
MEMORY_CLOCK	.macro dosbase, index
.endmacro
MEMORY_FREE	.macro amount
.endmacro
MEMORY_FAILURE	.macro kind, request, capacity, used
.endmacro
MEMORY_PROGRESS	.macro dosbase, phase, source, line, records
.endmacro
MEMORY_SETUP_RESULT	.macro dosbase, phase
.endmacro
ASSEMBLY_POSITION_CLEAR	.macro capture
.endmacro
ASSEMBLY_POSITION	.macro capture, passValue, sweepValue, state, modeOffset, sectionOffset, countOffset
.endmacro
SELECTION_POSITION_CLEAR	.macro capture
.endmacro
SELECTION_POSITION_CANDIDATE	.macro capture, priorityValue, recipeValue
.endmacro
SELECTION_POSITION_PROJECTION	.macro capture, kind
.endmacro
MEMORY_PROGRESS_BLOCK	.macro dosbase, phase, base, firstOffset, secondOffset, thirdOffset
.endmacro
MEMORY_PROGRESS_RECORDS	.macro dosbase, phase, sourceValue, lineValue, base, usedOffset
.endmacro
MEMORY_ASSEMBLY_RECORD	.macro dosbase, phase, frame, recordsOffset, offsetOffset, bytesOffset
.endmacro
ASSEMBLY_FAILURE_STAGE	.macro slot, stage
.endmacro
MEMORY_PREPARATION_INPUT	.macro snapshot, capacity
.endmacro
MEMORY_PREPARATION_FAILURE	.macro dosbase, assembling, stage, used, count, phase
.endmacro
MEMORY_PREPARATION_RECORD	.macro dosbase, assembling, stage, output, capacity, snapshot, phase
.endmacro
MEMORY_PHASE	.macro value
.endmacro
MEMORY_SAVE	.macro dosbase
.endmacro
MEMORY_LAYOUT	.macro runtime_bytes, record_bytes, source_bytes
.endmacro
MEMORY_STAGE	.macro index
.endmacro
MEMORY_DETAIL_BEGIN	.macro index
.endmacro
MEMORY_DETAIL_END	.macro index
.endmacro
MEMORY_BIND_SAMPLE_BEGIN	.macro
.endmacro
MEMORY_BIND_SAMPLE_END	.macro
.endmacro
MEMORY_TEMPLATE_WORK	.macro index, amount
.endmacro
MEMORY_INPUT_BEGIN	.macro bytes
.endmacro
MEMORY_INPUT_END	.macro bytes
.endmacro
MEMORY_INPUT_READ	.macro
.endmacro
TOKEN_BEGIN	.macro amount
.endmacro
TOKEN_OPCODE	.macro opcode
.endmacro
TOKEN_SCOPE_BEGIN	.macro index
.endmacro
TOKEN_SCOPE_END	.macro index
.endmacro
TOKEN_WORK	.macro index, amount
.endmacro
TOKEN_SCOPE_CLOSE	.macro index
.endmacro
