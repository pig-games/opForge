; Block-index owns this row ABI. Kept in the existing shared include root so
; the index, passive observer and isolated probes read one canonical layout.
; Authoritative block-index row layout, shared by its passive observer.
Span	.struct
Start	.long ?
End	.long ?
Entry	.word ?
Parent	.word ?
Live	.word ?
	.endstruct
SPAN_BYTES = Span.Live+2
