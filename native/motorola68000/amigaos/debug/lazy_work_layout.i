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
CONTEXT_DEFINITION = 0
CONTEXT_EXPANSION = 1
CONTEXT_ORDINARY = 2
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
; Fixed audit extension; no per-record or per-unit storage growth.
TOP_COUNT = 3
Top	.struct
Metric	.long ?
Event	.long ?
Unit	.long ?
Reasons	.long ?
CaptureOpen	.long ?
CaptureClose	.long ?
Origin	.long ?
Line	.long ?
OriginalOpen	.long ?
Values	.res VECTOR_BYTES
.endstruct
TOP_BYTES = Top.Values+VECTOR_BYTES
RANGE_EXAMPLE_COUNT = 4
RangeExample .struct
Event .long ?
Unit .long ?
Cause .long ?
CaptureOpen .long ?
CaptureClose .long ?
Origin .long ?
Line .long ?
RegionBase .long ?
RegionEnd .long ?
.endstruct
RANGE_EXAMPLE_BYTES = RangeExample.RegionEnd+4
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
UnknownRows	.res VECTOR_BYTES
Residual	.res VECTOR_BYTES
RangeCounts	.res 5*4 ; close-before, close>=end, missing-end, open<base, parent
RangeExamples	.res RANGE_EXAMPLE_COUNT*RANGE_EXAMPLE_BYTES
TopRows	.res TOP_COUNT*TOP_BYTES
PlanContext	.long ? ; 0 definition,1 expansion,2 ordinary
Contexts	.res 6*2*4 ; inactive then active SampleOwner, calls/committed bytes
Filter	.res LIMIT*4
Rows	.res LIMIT*ROW_BYTES
.endstruct
WORK_STATE_BYTES = WorkFields.Rows+LIMIT*ROW_BYTES
