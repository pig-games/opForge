; Shared BE16/BE32 numeric binding plan and candidate wire layout.
; @opforge-owner: experimental.amigaos.binary_binding_layout
	.module experimental.amigaos.binary_binding_layout
	.cpu 68020
	.pub
Plan	.struct
FullId	.word ?
CandidateCount	.word ?
Bytes	.long ?
.endstruct
HEADER_BYTES = Plan.Bytes+4
Candidate	.struct
Base	.word ?
MemberCount	.word ?
.endstruct
CANDIDATE_HEADER_BYTES = Candidate.MemberCount+2
	.endmodule
