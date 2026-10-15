; Passive storage/lifetime investigation schema 1. No production record changes.
STARTUP = 0
ACQUISITION = 1
CONFIGURATION = 2
REPLAY_INIT = 3
REPLAY = 4
CAPTURE_RELEASE = 5
FINALIZATION = 6
ASSEMBLY = 7
OUTPUT = 8
CLEANUP = 9
PHASE_COUNT = 10
CURVE_LIMIT = 256
REGION_LIMIT = 256
WIDE_TOKEN_BYTES = 20
COMPACT_TOKEN_BYTES = 12
TOKEN_LIMIT = 64
OVERFLOW = 1
BOUNDS = 2
BAD_MODE = 4
UNDERFLOW = 8
RECONCILE = 16
Layout	.struct
HeaderBytes	.word ?
CountOffset	.word ?
BytesOffset	.word ?
LexemesOffset	.word ?
PlansOffset	.word ?
SourceOffset	.word ?
TokenBytes	.word ?
KindOffset	.word ?
ReservedOffset	.word ?
StartOffset	.word ?
EndOffset	.word ?
OffsetOffset	.word ?
LengthOffset	.word ?
.endstruct
LAYOUT_BYTES = Layout.LengthOffset+2
PeakRow	.struct
Event	.long ?
Phase	.long ?
Heap	.long ?
Capture	.long ?
Eligible	.long ?
Span	.long ?
.endstruct
PEAK_BYTES = PeakRow.Span+4
CurveRow	.struct
Span	.long ?
Event	.long ?
Phase	.long ?
Heap	.long ?
Capture	.long ?
Eligible	.long ?
ModelCapacity	.long ?
Pins	.long ?
Regions	.long ?
.endstruct
CURVE_BYTES = CurveRow.Regions+4
RegionRow	.struct
Index	.long ?
File	.long ?
Records	.long ?
Tokens	.long ?
OriginalUsed	.long ?
CompactUsed	.long ?
ActualCapacity	.long ?
FallbackRecords	.long ?
FallbackTokens	.long ?
.endstruct
REGION_BYTES = RegionRow.FallbackTokens+4
Maxima	.struct
Count	.long ?
SourceBytes	.long ?
LexemeBytes	.long ?
Kind	.long ?
Reserved	.long ?
Start	.long ?
End	.long ?
Offset	.long ?
Length	.long ?
.endstruct
MAXIMA_BYTES = Maxima.Length+4
Fields	.struct
Errors	.long ?
Mode	.long ?
Phase	.long ?
Event	.long ?
HeapLive	.long ?
HeapPeak	.long ?
CaptureLive	.long ?
CapturePeak	.long ?
Allocated	.long ?
Freed	.long ?
AllocCalls	.long ?
FreeCalls	.long ?
CurveCount	.long ?
RegionCount	.long ?
Records	.long ?
Tokens	.long ?
OriginalBytes	.long ?
CompactBytes	.long ?
FallbackRecords	.long ?
FallbackTokens	.long ?
HeaderBytes	.long ?
LexemeBytes	.long ?
PlanBytes	.long ?
RecordHeaderWidth	.long ?
ActualCapSum	.long ?
CompactCapSum	.long ?
Eligible	.long ?  ; last completed L1 span, not current liveness proof
Span	.long ?
Maxima	.res MAXIMA_BYTES
HeapPeakRow	.res PEAK_BYTES
CapturePeakRow	.res PEAK_BYTES
PhasePeaks	.res PHASE_COUNT*PEAK_BYTES
Curves	.res CURVE_LIMIT*CURVE_BYTES
Regions	.res REGION_LIMIT*REGION_BYTES
.endstruct
STATE_BYTES = Fields.Regions+REGION_LIMIT*REGION_BYTES
