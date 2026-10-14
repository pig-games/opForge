; Shared bounded passive structural observer layout. No emitted storage.
LIMIT = 4096
DETAIL_LIMIT = 16
OVERFLOW = 1
EXHAUSTED = 2
INVALID = 4
Event	.struct
OriginalOpen	.long ?
OriginalEnd	.long ?
OriginalPrevious	.long ?
CaptureOpen	.long ?
CaptureClose	.long ?
Origin	.long ?
Line	.long ?
Generated	.long ?  ; nonzero template depth is conservatively deferred
FinalOpen	.long ?
FinalEnd	.long ?
FinalPrevious	.long ?
Unit	.long ?  ; authoritative block ordinal+1, zero when unresolved
PreviousCapture	.long ?
PreviousOrigin	.long ?
PreviousLine	.long ?
.endstruct
EVENT_BYTES = Event.PreviousLine+4
Fields	.struct
Errors	.long ?
Count	.long ?
Depth	.long ?
Matched	.long ?
Deferred	.long ?
Unmatched	.long ?
Previous	.long ?
PreviousCapture	.long ?
PreviousOrigin	.long ?
PreviousLine	.long ?
Events	.res LIMIT*EVENT_BYTES
Stack	.res LIMIT*4
.endstruct
STATE_BYTES = Fields.Stack+LIMIT*4
