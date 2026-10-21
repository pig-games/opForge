; Shared preparation-only scope storage. Persisted names retain offsets and IDs.
; @opforge-owner: experimental.amigaos.binary_scope_layout
	.module experimental.amigaos.binary_scope_layout
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_name_store as names
	.use experimental.amigaos.binary_iterator_bindings as iterators
	.use experimental.amigaos.binary_lexical_store as lexical
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
ArenaUsed	.long ?  ; logical name end, including sealed page-tail gaps
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
DependencyResolve	.long ?  ; optional demanded stored-interface binding callback
DependencyContext	.long ?  ; borrowed active dependency registry, never serialized
DependencyOwners	.long ?  ; optional sparse used-address whole-block owner bridge
DependencyBinder	.long ?  ; real binder for immutable dependency admission at references
DependencyFailed	.word ?  ; sticky failed demand; partial publication cannot be replayed
BucketCount	.long ?
BucketMask	.long ?
IndexedCount	.long ?  ; only entries reachable from the scope buckets
.endstruct
ITERATOR_STATE = State.LoopDepth
ENTRIES = State.IndexedCount+4
ENTRIES_POINTER = ENTRIES+memory.Block.Pointer
; Owned power-of-two buckets; doubled offsets stay within signed-word indexing.
BUCKET_INITIAL = 256
BUCKET_MAX = 16384
BUCKETS = ENTRIES+memory.Block.Used+4
BUCKETS_POINTER = BUCKETS+memory.Block.Pointer
ARENA = BUCKETS+memory.Block.Used+4
; The lexical owner resolves opaque canonical-name handles through its bridge.
; The former arena descriptor stays empty during consumer migration.
LEXICAL_STATE = ARENA+names.STATE_BYTES
BUFFER = LEXICAL_STATE+lexical.STATE_BYTES
MODULE_STATE = BUFFER+NAME_BYTES
IMPORT_STATE = MODULE_STATE+modules.SCRATCH_BYTES
	.endmodule
