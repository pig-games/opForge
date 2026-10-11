; Frozen-source lifecycle metadata. Persist source cursors as offsets; execution
; owners still borrow this checkpoint's pinned record allocation.
; @opforge-owner: experimental.amigaos.binary_assembly_session
	.module experimental.amigaos.binary_assembly_session
	.cpu 68020
	.pub
BEGIN = 1
ADVANCE = 2
FINISH = 3
EMIT = 4
RELEASE = 5
IDLE = 0
SIZING = 1
SIZED = 2
EMITTED = 3
ADVANCED = 0
FRONTIER = 1
TERMINATED = 2
State	.struct
Frame	.long ?
Base	.long ?
Bytes	.long ?
Context	.long ?
Count	.long ?
Values	.long ?
Defined	.long ?
SectionIds	.long ?
Owner	.long ?
MemberKeys	.long ?
ModuleStarts	.long ?
ModuleCount	.long ?
AsciiName	.word ?
PetsciiName	.word ?
Cursor	.long ?
Published	.long ?
Budget	.long ?
Unlimited	.word ?
Ended	.word ?
Pass	.word ?
Sweep	.word ?
Selection	.word ?
Operation	.word ?
Phase	.word ?
Result	.word ?
.endstruct
STATE_BYTES = State.Result+2
	.endmodule
