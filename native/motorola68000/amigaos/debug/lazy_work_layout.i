; Bounded opt-in work observer ABI and counter identities; no storage.
BIND = 0
COMPILE = 4
PLAN_BIND = 8
PROCESS = 12
BOUND_BYTES = 16
TEMPLATE_BYTES = 20 ; committed Def/BODY/default/default-spelling payload only
PREPARED_COPY = 24
REPLAY_COPY = 28
CAPTURE_HEADER = 32
CAPTURE_TOKENS = 36
CAPTURE_LEXEMES = 40
CAPTURE_PLANS = 44
CAPTURE_PADDING = 48
CAPTURE_BYTES = 52
CANDIDATE = 56
DESCRIPTOR = 60
FRAGMENT = 64
PLAN_CREATE = 68
METRICS = 18
VECTOR_BYTES = METRICS*4
GENERATED = 1
MULTIFILE = 2
MULTI_OWNER = 4
UNMATCHED = 8
BAD_RANGE = 16
PREVIOUS_UNKNOWN = 32
SELECTION_MISSING = 64
SEMANTIC_EFFECT = 128
NONMONOTONIC = 256
Row	.struct
Reasons	.long ?
Unit	.long ?
Class	.long ? ; 1 selected, 2 unreferenced, zero unknown
LastCapture	.long ? ; monotonicity within this active unit, never between peers
PreviousOwner	.long ? ; structural event index+1, zero outside units
Values	.res VECTOR_BYTES
Previous	.res VECTOR_BYTES
.endstruct
ROW_BYTES = Row.Previous+VECTOR_BYTES
WorkFields	.struct
Errors	.long ?
Count	.long ?
UnknownReasons	.long ?
SelectionDone	.long ?
SampleOwner	.long ? ; physical invocation event ordinal+1
SampleReasons	.long ?
FilterCount	.long ?
BoundBefore	.long ?
LastCapture	.long ?
LastOrigin	.long ?
LastOwner	.long ?
CaptureRecords	.long ?
Capacity	.long ?
Totals	.res VECTOR_BYTES
Selected	.res VECTOR_BYTES
Unreferenced	.res VECTOR_BYTES
Shared	.res VECTOR_BYTES
Unknown	.res VECTOR_BYTES
Sample	.res VECTOR_BYTES
PreviousSample	.res VECTOR_BYTES
Record	.res VECTOR_BYTES
Filter	.res LIMIT*4
Rows	.res LIMIT*ROW_BYTES
.endstruct
WORK_STATE_BYTES = WorkFields.Rows+LIMIT*ROW_BYTES
