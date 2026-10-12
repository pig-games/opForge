; Shared preparation-only scope storage. Persisted names retain offsets and IDs.
; @opforge-owner: experimental.amigaos.binary_scope_layout
	.module experimental.amigaos.binary_scope_layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_iterator_bindings as iterators
	.pub
; Source counts and index+1 links are words; zero denotes an empty link.
; The package base further bounds the available global identity range.
LIMIT = 65535
ENTRIES_BYTES = LIMIT*records.ENTRY_BYTES
; Composed lexical paths include invocation scopes as well as source names.
NAME_BYTES = records.NAME_BYTES
; Spellings grow independently: every identity may own a full composed name.
; Stored lengths exclude the composition buffer's terminator.
ARENA_BYTES = LIMIT*(NAME_BYTES-1)
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
Package	.long ?  ; validated preparation capsule, never serialized
LoopDepth	.word ?
LoopBindings	.res iterators.SLOTS_BYTES
LoopParents	.res iterators.LIMIT*2
PendingRepeat	.word ?
Deferred	.word ?  ; shared conditional preparation depth for this record
MaterializedLoops	.word ?  ; physical preparation loops with selected iterations
PreviewBinding	.word ?  ; identity-only preview forbids candidate publication
BindingNamespace	.word ?  ; zero=value/template spelling, one=encoding name
EarlierReference	.long ?  ; preparation-only lexical snapshot binder
.endstruct
ITERATOR_STATE = State.LoopDepth
ENTRIES = State.EarlierReference+4
ENTRIES_POINTER = ENTRIES+memory.Block.Pointer
BUCKETS = ENTRIES+memory.Block.Used+4
ARENA = BUCKETS+256*2
ARENA_POINTER = ARENA+memory.Block.Pointer
BUFFER = ARENA+memory.Block.Used+4
MODULE_STATE = BUFFER+NAME_BYTES
IMPORT_STATE = MODULE_STATE+modules.SCRATCH_BYTES
	.endmodule
