; Separate acquisition audit ABI. Offsets and owner ordinals only.
CANDIDATE_MACRO_KIND = 1; TemplateDefinePacket.Kind ABI, same as templates.KIND_MACRO
CANDIDATE_NON_MACRO = $ffffffff
CANDIDATE_UNKNOWN = $fffffffe
CANDIDATE_INVALID = 1
CANDIDATE_OVERFLOW = 2
CANDIDATE_DUPLICATE = 4
CANDIDATE_CLOCK = 8
CANDIDATE_UNFINISHED = 16
CANDIDATE_TOTAL_GROUP = 0
CANDIDATE_NON_MACRO_GROUP = 1
CANDIDATE_NEVER_INVOKED_GROUP = 2
CANDIDATE_INVOKED_GROUP = 3
CANDIDATE_UNKNOWN_GROUP = 4
CANDIDATE_NOT_RETAINED_GROUP = 5
CANDIDATE_GROUPS = CANDIDATE_NOT_RETAINED_GROUP+1
CandidateVector	.struct
Records	.long ?
CaptureBytes	.long ?
PlanBytes	.long ?
SourceBytes	.long ?
TokenBytes	.long ?
LexemeBytes	.long ?
CandidateCalls	.long ?
DescriptorCalls	.long ?
FragmentCalls	.long ?
PlanCreates	.long ?
Samples	.long ?
TicksHigh	.long ?
TicksLow	.long ?
Reserved0	.long ?
Reserved1	.long ?
.endstruct
CANDIDATE_VECTOR_BYTES = CandidateVector.Reserved1+4
CandidateStamp	.struct
High	.long ?
Low	.long ?
.endstruct
CANDIDATE_STAMP_BYTES = CandidateStamp.Low+4
CandidateFields	.struct
Errors	.long ?
Created	.long ?
PlanBytes	.long ?
Duplicates	.long ?
Frequency	.long ?
TimedCalls	.long ?
TimedSamples	.long ?
Unassigned	.long ?
ClockActive	.long ?
ClockStart	.res CANDIDATE_STAMP_BYTES
ClockEnd	.res CANDIDATE_STAMP_BYTES
Vectors	.res CANDIDATE_GROUPS*CANDIDATE_VECTOR_BYTES
Record	.res CANDIDATE_VECTOR_BYTES
Gross	.res CANDIDATE_VECTOR_BYTES
.endstruct
CANDIDATE_STATE_BYTES = CandidateFields.Gross+CANDIDATE_VECTOR_BYTES
.ifndef OPFORGE_CANDIDATE_SAMPLE_PERIOD
OPFORGE_CANDIDATE_SAMPLE_PERIOD = 64
.endif
.ifndef OPFORGE_CANDIDATE_PHASE_SEED
OPFORGE_CANDIDATE_PHASE_SEED = 0
.endif
