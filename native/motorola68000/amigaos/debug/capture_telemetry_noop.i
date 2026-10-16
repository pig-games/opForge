; Disabled telemetry API: keep signatures without capturing implementation bodies.
CAPTURE_CLEAR	.macro
.endmacro
CAPTURE_GROW_BEGIN	.macro
.endmacro
CAPTURE_GROW_END	.macro
.endmacro
CAPTURE_REPLAY_BEGIN	.macro
.endmacro
CAPTURE_CONSTRUCTION_CLEAR_BEGIN	.macro counter
.endmacro
CAPTURE_CONSTRUCTION_CLEAR_GAP	.macro counter, bytes
.endmacro
CAPTURE_CONSTRUCTION	.macro clearBytes, count, lexemeBytes, planBytes
.endmacro
CAPTURE_APPEND	.macro arena, headerBytes, countOffset, lexemeOffset, planOffset, sourceOffset, bytes
.endmacro
CAPTURE_COPY	.macro amount
.endmacro
CAPTURE_GROW	.macro used, oldCapacity, newCapacity
.endmacro
CAPTURE_RETAINED_RELEASE	.macro capacity
.endmacro
CAPTURE_SNAPSHOT	.macro boundary, table, count, temp, order, stride, arenaOffset, orderCount, orderStride
.endmacro
CAPTURE_CHUNK_SNAPSHOT	.macro boundary, directory, rowShift, rowBytes, storageOffset, temp, order, orderCount, orderStride
.endmacro
CAPTURE_REPORT	.macro dosbase
.endmacro
CAPTURE_STORE_CLEAR	.macro reads, readBytes, writes, writeBytes, faults, evictions, peak
.endmacro
CAPTURE_STORE_REPORT	.macro dosbase, reads, readBytes, writes, writeBytes, faults, evictions, peak
.endmacro
