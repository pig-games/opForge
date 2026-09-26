; Shared preparation-only scope storage. Persisted names retain offsets and IDs.
; @opforge-owner: experimental.amigaos.binary_scope_layout
	.module experimental.amigaos.binary_scope_layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_memory as memory
	.pub
; Source IDs remain words; each owned allocation is bounded by memory.LIMIT.
; The 20-byte entry allocation caps identities at 52428, below wildcard $ffff.
LIMIT = memory.LIMIT/records.ENTRY_BYTES
ARENA_BYTES = memory.LIMIT
; Composed lexical paths include invocation scopes as well as source names.
NAME_BYTES = records.NAME_BYTES
ENTRY_BYTES = records.ENTRY_BYTES
State	.struct
Base	.word ?
Count	.word ?
Current	.word ?
Ended	.word ?
ArenaUsed	.long ?
FirstBound	.word ?
FirstExplicit	.word ?
EndDirective	.word ?
Changed	.word ?
FileContent	.word ?
ReserveRoutine	.long ?
.endstruct
ENTRIES = State.ReserveRoutine+4
ENTRIES_POINTER = ENTRIES+memory.Block.Pointer
BUCKETS = ENTRIES+memory.Block.Used+4
ARENA = BUCKETS+256*2
ARENA_POINTER = ARENA+memory.Block.Pointer
BUFFER = ARENA+memory.Block.Used+4
MODULE_STATE = BUFFER+NAME_BYTES
IMPORT_STATE = MODULE_STATE+modules.SCRATCH_BYTES
	.endmodule
