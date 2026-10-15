; Passive packing audit schema 1. Scalars only; row ordinals join the existing
; template inventory. Inclusive sampled writer intervals may overlap when nested.
PACKING_ROW_LIMIT = TEMPLATE_DEFINITION_LIMIT+1
PACKING_DEPTH_LIMIT = 32
PACKING_OVERFLOW = 1
PACKING_NESTING = 2
PACKING_CLOCK = 4
PACKING_BOUNDS = 8
PACKING_IDENTITY = 16
PACKING_RECONCILE = 32
PackingRow	.struct
Writes	.long ?
Tokens	.long ?
LexemeBytes	.long ?
MapBytes	.long ?
PackedBytes	.long ?
Names	.long ?
Members	.long ?
PackageHits	.long ?
SourceBinds	.long ?
CaptureBinds	.long ?
StoredRecords	.long ?
StoredBytes	.long ?
Samples	.long ?
TicksHigh	.long ?
TicksLow	.long ?
Failures	.long ?
PrefixWrites	.long ?
Reserved	.long ?
.endstruct
PACKING_ROW_BYTES = PackingRow.Reserved+4
PackingFrame	.struct
Owner	.long ?  ; numeric template inventory ordinal, zero ordinary
Sampled	.long ?
StartHigh	.long ?
StartLow	.long ?
.endstruct
PACKING_FRAME_BYTES = PackingFrame.StartLow+4
PackingFields	.struct
Errors	.long ?
Depth	.long ?
OverflowDepth	.long ?  ; balance ends after a rejected overflowing begin
Completed	.long ?
MaxDepth	.long ?
Started	.long ?
Frequency	.long ?
UsedRows	.long ?
ClockEndHigh	.long ?
ClockEndLow	.long ?
Gross	.res PACKING_ROW_BYTES
Outside	.res PACKING_ROW_BYTES
Frames	.res PACKING_DEPTH_LIMIT*PACKING_FRAME_BYTES
Rows	.res PACKING_ROW_LIMIT*PACKING_ROW_BYTES
.endstruct
PACKING_STATE_BYTES = PackingFields.Rows+PACKING_ROW_LIMIT*PACKING_ROW_BYTES
.ifndef OPFORGE_PACKING_SAMPLE_PERIOD
OPFORGE_PACKING_SAMPLE_PERIOD = 64
.endif
.ifndef OPFORGE_PACKING_PHASE_SEED
OPFORGE_PACKING_PHASE_SEED = 0
.endif
