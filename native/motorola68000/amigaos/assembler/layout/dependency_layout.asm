; Private graph-state protocol shared by global and scoped constant roots.
; @opforge-owner: opforge.assembler.layout.dependency_layout
	.module opforge.assembler.layout.dependency_layout
	.cpu 68020
	.pub
ABSOLUTE = 2
PENDING = 3
VISITING = 4
LAYOUT = 5
LABEL = 6
MUTABLE = 7
SNAPSHOT = 8
Frame	.struct
Records	.long ?
RecordBytes	.long ?
Declarations	.long ?
Context	.long ?
Owner	.long ?
Ids	.long ?
Count	.long ?
States	.long ?
Offsets	.long ?
Branch	.long ?  ; transient selected control offset; not persisted
	.endstruct
FRAME_BYTES = Frame.Branch+4
Offset	.struct
Expression	.long ?
	.endstruct
OFFSET_BYTES = Offset.Expression+4
OFFSET_SHIFT = 2
	.endmodule
