; Shared session-local text registry. Source names are tokenizer-owned IDs.
; Entry/Escape/Scope links are arena offsets plus one. Variable Escape payloads
; contain pattern bytes then replacement bytes; lengths count decoded raw bytes.
; One Block owns all records under BUDGET. Superseded definitions, replacement
; records and popped scopes remain allocated until release; only live links emit.
; Mutations may relocate the arena: retain offsets, never derived pointers.
	.module experimental.amigaos.binary_text_layout
	.use experimental.amigaos.binary_memory as memory
	.pub
STATUS_OK = 0
STATUS_INVALID = 1
STATUS_OVERFLOW = 2
STATUS_MISSING = 3
STATUS_DUPLICATE = 4
BUDGET = memory.LIMIT
MISSING = $ffff
CHARACTERS = 256
Entry	.struct
Next	.long ?
Id	.word ?
Map	.res CHARACTERS*2
Escapes	.long ?
.endstruct
ENTRY_BYTES = Entry.Escapes+4
Escape	.struct
Next	.long ?
PatternLength	.long ?
ReplacementLength	.long ?
Payload	.byte ?
.endstruct
ESCAPE_HEADER = Escape.Payload
Scope	.struct
Next	.long ?
Previous	.word ?
Definition	.word ?
.endstruct
SCOPE_BYTES = Scope.Definition+2
State	.struct
AsciiId	.word ?
PetsciiId	.word ?
Active	.word ?
Entries	.long ?
Scopes	.long ?
Arena	.res memory.Block.Used+4
.endstruct
STATE_BYTES = State.Arena+memory.Block.Used+4
	.endmodule
