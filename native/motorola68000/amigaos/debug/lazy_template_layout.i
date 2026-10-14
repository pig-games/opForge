; Fixed scalar inventory. No production record gains fields or observer pointers.
TEMPLATE_DEFINITION_LIMIT = 2048
; Disjoint diagnostic bands sized for every registered row and edge.
TEMPLATE_DEFINITION_PHASE = 3100
TEMPLATE_EDGE_PHASE = 20000
TEMPLATE_PATH_HEADER = 34999
TEMPLATE_PATH_FIRST = 35000
TEMPLATE_EDGE_LIMIT = 4096
TEMPLATE_BUCKETS = 256
TEMPLATE_OWNER_LIMIT = 4
TEMPLATE_OVERFLOW = 1
TEMPLATE_EXHAUSTED = 2
TEMPLATE_INVALID = 4
TEMPLATE_UNCLOSED = 8
TEMPLATE_UNFINISHED = 16
TEMPLATE_RECONCILE = 32
TemplateDefinePacket .struct
Session .long ?
Handle .long ?
Pool .long ?
Kind .long ?
Name .long ?
Origin .long ?
Line .long ?
HeaderPlan .long ?
First .long ?
DefBytes .long ?
DefaultBytes .long ?
TextBytes .long ?
.endstruct
TEMPLATE_DEFINE_PACKET_BYTES = TemplateDefinePacket.TextBytes+4
TemplateCallPacket .struct
Session .long ?
Handle .long ?
Pool .long ?
ParentHandle .long ?
ParentPool .long ?
Origin .long ?
Line .long ?
DefaultMask .long ?
.endstruct
TEMPLATE_CALL_PACKET_BYTES = TemplateCallPacket.DefaultMask+4
TemplatePoolsPacket .struct
Session .long ?
InternalBytes .long ?
ExternalBytes .long ?
DefaultBytes .long ?
TextBytes .long ?
BodyBytes .long ?
PlanBytes .long ?
.endstruct
TEMPLATE_POOLS_PACKET_BYTES = TemplatePoolsPacket.PlanBytes+4
TemplatePoolValues .struct
InternalBytes .long ?
ExternalBytes .long ?
DefaultBytes .long ?
TextBytes .long ?
BodyBytes .long ?
PlanBytes .long ?
.endstruct
TEMPLATE_POOL_BYTES = TemplatePoolValues.PlanBytes+4
TemplateOwnerSlot .struct
Epoch .long ?
Active .long ?
Maps .res TEMPLATE_DEFINITION_LIMIT*2*4
.endstruct
TEMPLATE_OWNER_BYTES = TemplateOwnerSlot.Maps+TEMPLATE_DEFINITION_LIMIT*2*4
TemplateDefinition .struct
Epoch .long ?
Handle .long ?
Pool .long ?
Kind .long ?
Name .long ?
Origin .long ?
Line .long ?
HeaderPlan .long ?
First .long ?
Last .long ?
DefBytes .long ?
DefaultBytes .long ?
TextBytes .long ?
Closed .long ?
Calls .long ?
NestedCalls .long ?
DefaultCalls .long ?
FirstCaller .long ?
FirstOrigin .long ?
FirstLine .long ?
PlanBinds .long ?
BoundBytes .long ?
DefaultSlots .long ?
Reserved .long ?
.endstruct
TEMPLATE_DEFINITION_BYTES = TemplateDefinition.Reserved+4
TemplateEdge .struct
Caller .long ?
Callee .long ?
Origin .long ?
Line .long ?
Calls .long ?
DefaultCalls .long ?
DefaultSlots .long ?
Next .long ? ; private bucket-chain ordinal, never an address
.endstruct
TEMPLATE_EDGE_BYTES = TemplateEdge.Next+4
TemplateFields .struct
Errors .long ?
Epoch .long ?
Active .long ?
Count .long ?
EdgeCount .long ?
Finished .long ?
OwnerRow .long ?
BindingCalls .long ?
BindingBytes .long ?
Calls .long ?
NestedCalls .long ?
DefaultCalls .long ?
DefaultSlots .long ?
Closed .long ?
OpenRows .long ?
Unfinished .long ?
UnattributedCalls .long ?
UnattributedBytes .long ?
Pools .res TEMPLATE_POOL_BYTES
Payload .res TEMPLATE_POOL_BYTES ; last field stays zero: no inferred plan ownership
Residual .res TEMPLATE_POOL_BYTES
RowCalls .long ?
EdgeCalls .long ?
RowBinds .long ?
RowBoundBytes .long ?
EdgeDefaults .long ?
EdgeSlots .long ?
Owners .res TEMPLATE_OWNER_LIMIT*TEMPLATE_OWNER_BYTES
Buckets .res TEMPLATE_BUCKETS*4
Rows .res TEMPLATE_DEFINITION_LIMIT*TEMPLATE_DEFINITION_BYTES
Edges .res TEMPLATE_EDGE_LIMIT*TEMPLATE_EDGE_BYTES
.endstruct
TEMPLATE_STATE_BYTES = TemplateFields.Edges+TEMPLATE_EDGE_LIMIT*TEMPLATE_EDGE_BYTES
