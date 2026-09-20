; Shared preparation-only scope storage. Offsets and IDs, never stored pointers.
; @opforge-owner: experimental.amigaos.binary_scope_layout
	.module experimental.amigaos.binary_scope_layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_modules as modules
	.pub
LIMIT = 512
ARENA_BYTES = 16384
ENTRY_BYTES = records.ENTRY_BYTES
State	.struct
Base	.word ?
Count	.word ?
Current	.word ?
Ended	.word ?
ArenaUsed	.word ?
FirstBound	.word ?
FirstExplicit	.word ?
EndDirective	.word ?
Changed	.word ?
FileContent	.word ?
.endstruct
ENTRIES = State.FileContent+2
BUCKETS = ENTRIES+LIMIT*ENTRY_BYTES
ARENA = BUCKETS+256*2
BUFFER = ARENA+ARENA_BYTES
MODULE_STATE = BUFFER+64
IMPORT_STATE = MODULE_STATE+modules.SCRATCH_BYTES
	.endmodule
