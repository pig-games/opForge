; Shared BE16/BE32 numeric binding plan and candidate wire layout.
; @opforge-owner: opforge.assembler.symbols.binding_layout
	.module opforge.assembler.symbols.binding_layout
	.cpu 68020
	.pub
; Largest even unsigned-long extent; plans retain long byte offsets.
ARENA_LIMIT = $fffffffe
Plan	.struct
FullId	.word ?
CandidateCount	.word ?
Bytes	.long ?
.endstruct
HEADER_BYTES = Plan.Bytes+4
Candidate	.struct
Base	.word ?
MemberCount	.word ?  ; zero selects the whole declared value, without projection
.endstruct
CANDIDATE_HEADER_BYTES = Candidate.MemberCount+2
	.endmodule
