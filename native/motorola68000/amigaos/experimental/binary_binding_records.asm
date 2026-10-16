; Rewrite provisional numeric identities after declaration-aware scope binding.
; @opforge-owner: experimental.amigaos.binary_binding_records
	.module experimental.amigaos.binary_binding_records
	.cpu 68020
	.include "binding_telemetry.i"
	.use opasm.amigaos.binary_expression as expr
	.use exprvm.amigaos.runtime as runtime
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_payload_bounds as payloads
	.use experimental.amigaos.binary_materialized_scope as materialized
	.pub
Entry	.struct
Name	.long ?
Length	.word ?
Owner	.word ?
Leaf	.word ?
Flags	.word ?
Target	.word ?
Next	.word ?
ScopeKind	.word ?
Padding	.word ?  ; proxy origin lexical scope; zero for ordinary entries
MemberBase	.word ?  ; qualified proxy's definition-time struct identity+1
TemplateModule	.word ?  ; declaration ownership independent of the numeric value
TemplateFlags	.word ?  ; preparation-only declaration and visibility facet
StatementModule	.word ?  ; independent module-flat statement keyword ownership
RegisterName	.word ?  ; original spelling's package register ID+1; zero absent
RegisterQualifier	.byte ?
RegisterReserved	.byte ?  ; zero; candidate metadata never follows Target
MemberId	.word ?  ; immutable global bare field key; $ffff until declared
OriginalId	.word ?  ; original complete name ID for BINDING_PROXY
.endstruct
ENTRY_BYTES = Entry.OriginalId+2
REMAP_SAVED_BASE = 7*4; D1-D7 precede saved A0 in remap's MOVEM frame
ENCODING_NAME_HIGH_BIT = 7
ENCODING_NAME = 1<<(ENCODING_NAME_HIGH_BIT+8); independent non-value identity namespace
REFERENCED = 2
OPTIONAL_REFERENCED = 512
IMPORT_PROXY_BIT = 3
IMPORT_PROXY = 1<<IMPORT_PROXY_BIT; preparation-only imported/qualified occurrence
CONDITIONAL_VALUE = 1<<11; preparation retains candidate immutable definitions
MUTABLE_BIT = 8
MUTABLE = 1<<MUTABLE_BIT
ITERATOR_HIGH_BIT = 2
ITERATOR = 1<<(ITERATOR_HIGH_BIT+8)
LABEL_VALUE_BIT = 7
LABEL_VALUE = 1<<LABEL_VALUE_BIT; preparation-time address declaration availability
BINDING_PROXY = 1<<13; per-occurrence numeric dotted-value origin
UPDATE_LOCAL_HIGH_BIT = 6
UPDATE_LOCAL = 1<<(UPDATE_LOCAL_HIGH_BIT+8); update targets inside blocks cannot bind at module scope
COMPOUND_LABEL = 1<<12; collection identity, distinct from an address label
STRUCT_TYPE = 32
TEMPLATE_PROXY_BIT = 6
TEMPLATE_PROXY = 1<<TEMPLATE_PROXY_BIT
TEMPLATE_DECLARED = 1
TEMPLATE_PUBLIC = 2
STATEMENT_DECLARED = 4
STATEMENT_PUBLIC = 8
STATEMENT_DECLARED_BIT = 2
STATEMENT_PUBLIC_BIT = 3
NAMESPACE_VALUE = 0
NAMESPACE_TEMPLATE = 1
; Includes room for the terminator in preparation-only composition/copy buffers.
NAME_BYTES = 256
	.section code, kind=code

; A0=packed records,D0=bytes,A1=dense long source-index-to-target-ID map,
; D1=source ID base,D2=map entries (at most 65535). D0/CCR=status; other
; registers preserved. A negative/absent map entry or non-word target rejects.
; Package IDs below the source base remain unchanged, as in remap. Rewrites
; in place through the same payload walker; failure may leave earlier IDs changed.
; Callers validating a dependency closure must therefore use a private copy.
remapDense	.block
	cmpi.l #65535, d2
	bhi.w bad
	move.l d2, -(sp)
	bset #31, d2  ; internal dense-map mode; low word remains the bounded count
	bsr.w remap
	move.l (sp)+, d2
	tst.l d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; remapDense

; A0=packed records,D0=bytes,A1=Entry array,
; D1=source ID base,D2=entry count. D0/CCR=status; other registers preserved.
; Compact and canonical typed payloads are skipped by their encoded widths. Only
; symbol operands are rewritten, with their original BE/LE representation.
remap	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a1, a6
	move.l d1, d6
	move.l d2, d7
	movea.l a0, a5
	adda.l d0, a5
line
	cmpa.l a5, a0
	beq.w ok
	bhi.w bad
	moveq #0, d0
	move.b (a0), d0
	addq.w #1, d0
	cmpi.w #4, d0
	blo.w bad
	movea.l a0, a4
	adda.w d0, a4
	cmpa.l a5, a4
	bhi.w bad
	btst #4, 1(a0)
	beq.w tokens
	movea.l a4, a0
	bra.w line
tokens
	movea.l a0, a2
	moveq #0, d4
	move.b source.RecordHeader.Flags(a2), d4
	andi.w #source.FLAG_PLAN, d4  ; a call-plan flag requires its terminal trailer
	cmpa.l a4, a0
	beq.w bad
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #source.RECORD_HEADER_BYTES, d0
	beq.w emptyRecord
	cmpi.b #materialized.TOKEN, materialized.Record.Kind(a0)
	beq.w materializedRecord
	addq.l #4, a0
token
	cmpa.l a4, a0
	beq.w recordDone
	bhi.w bad
	moveq #0, d0
	move.b (a0)+, d0
	cmpi.b #1, d0
	bls.w name
	cmpi.b #expr.COMPILED_TAG, d0
	beq.w compactExpression
	cmpi.b #expr.VALUE_TAG, d0
	beq.w typedExpression
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, d0
	beq.w schema
	cmpi.b #source.TOKEN_ITERATOR_BINDING, d0
	beq.w iterator
	cmpi.b #source.MACRO_PLAN, d0
	beq.w callPlan
	cmpi.b #source.LINE_PLAN, d0
	beq.w linePlan
	cmpi.b #source.OPTIONAL_LINE_PLAN, d0
	beq.w linePlan
	cmpi.b #2, d0
	beq.w literal
	cmpi.b #source.TOKEN_WIDE_NUMBER, d0
	beq.w wideLiteral
	cmpi.b #source.TOKEN_BINARY_BYTES, d0
	beq.w binary
	cmpi.b #3, d0
	beq.w string
	cmpi.b #41, d0
	beq.w bad  ; composite recipes must be expanded before remapping
	bra.w token
callPlan
linePlan
	movea.l a4, a1
	moveq #0, d1
	move.b source.RecordHeader.Flags(a2), d1
	jsr payloads.plan
	bne.w bad
	tst.l d1
	beq.w token
	moveq #0, d4
recordDone
	tst.w d4
	bne.w bad
	bra.w line
binary
	subq.l #1, a0
	movea.l a4, a1
	jsr payloads.binary
	bne.w bad
	adda.l d2, a0
	bra.w token

string
	cmpa.l a4, a0
	bhs.w bad
	moveq #0, d0
	move.b (a0)+, d0
	move.l a4, d1
	sub.l a0, d1
	cmp.l d0, d1
	blo.w bad
	adda.l d0, a0
	bra.w token
literal
	moveq #source.NUMBER_BYTES-1, d0
	bra.w literalPayload
wideLiteral
	moveq #source.WIDE_NUMBER_BYTES-1, d0
	bra.w literalPayload
schema
	moveq #4, d0  ; opaque BE32 value-seed offset
literalPayload
	move.l a4, d1
	sub.l a0, d1
	cmp.l d0, d1
	blo.w bad
	adda.l d0, a0
	bra.w token
materializedRecord
	movea.l a4, a1
	jsr materialized.validate
	bne.w bad
	moveq #0, d0
	move.w materialized.Record.Label(a0), d0
	cmpi.l #materialized.ANONYMOUS, d0
	beq.w markerDone
	bsr.w identity
	bne.w bad
	move.w d1, materialized.Record.Label(a0)
markerDone
emptyRecord
	tst.w d4
	bne.w bad
	movea.l a4, a0
	bra.w line

name
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #3, d0
	blo.w bad
	cmpi.b #source.FIELD_KEY_QUALIFIER, 2(a0)
	beq.w bareField
	moveq #0, d0
	move.w (a0), d0
	bsr.w identity
	bne.w bad
	move.w d1, (a0)
bareField
	addq.l #3, a0
	bra.w token
iterator
	move.l a4, d0
	sub.l a0, d0
	cmpi.l #2, d0
	blo.w bad
	moveq #0, d0
	move.w (a0), d0
	bsr.w identity
	bne.w bad
	move.w d1, (a0)
	addq.l #2, a0
	bra.w token
compactExpression
	moveq #0, d5
	bra.w expression
typedExpression
	moveq #1, d5
expression
	cmpa.l a4, a0
	bhs.w bad
	moveq #0, d0
	move.b (a0)+, d0
	beq.w bad
	movea.l a0, a3
	adda.w d0, a3
	cmpa.l a4, a3
	bhi.w bad
opcode
	cmpa.l a3, a0
	bhs.w bad
	moveq #0, d0
	move.b (a0)+, d0
	beq.w endExpression
	cmpi.b #runtime.SELECT, d0
	beq.w opcode
	tst.l d5
	bne.w typedOpcode
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_SYMBOL, d0
	beq.w symbol
	cmpi.b #runtime.COMPACT_I8, d0
	beq.w one
	cmpi.b #runtime.EXPRVM_OPCODE_APPLY_UNARY, d0
	beq.w one
	cmpi.b #runtime.EXPRVM_OPCODE_APPLY_BINARY, d0
	beq.w one
	cmpi.b #runtime.COMPACT_I16, d0
	beq.w two
	cmpi.b #runtime.COMPACT_I32, d0
	beq.w four
	cmpi.b #runtime.COMPACT_U32, d0
	beq.w four
	cmpi.b #runtime.COMPACT_I64, d0
	beq.w eight
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_CURRENT_ADDR, d0
	beq.w opcode
	cmpi.b #runtime.COMPACT_NEGATE, d0
	beq.w opcode
	cmpi.b #runtime.COMPACT_ADD, d0
	beq.w opcode
	cmpi.b #runtime.COMPACT_SUBTRACT, d0
	beq.w opcode
	cmpi.b #runtime.COMPACT_MULTIPLY, d0
	beq.w opcode
	bra.w bad
typedOpcode
	cmpi.b #runtime.EXPRVM_OPCODE_PUSH_STRING_LITERAL, d0
	beq.w typedString
	cmpi.b #$10, d0
	beq.w eight
	cmpi.b #$11, d0
	beq.w opcode
	cmpi.b #$12, d0
	beq.w symbol
	cmpi.b #$20, d0
	beq.w one
	cmpi.b #$21, d0
	beq.w one
	cmpi.b #$51, d0
	beq.w two
	cmpi.b #$52, d0
	beq.w one
	cmpi.b #runtime.EXPRVM_OPCODE_BUILD_STRUCT, d0
	beq.w struct
	cmpi.b #runtime.EXPRVM_OPCODE_GET_MEMBER, d0
	beq.w two  ; immutable global bare member identity
	cmpi.b #$61, d0
	beq.w opcode
	cmpi.b #$62, d0
	beq.w three
	cmpi.b #$70, d0
	beq.w opcode
	bra.w bad
typedString
	movea.l a3, a1
	jsr payloads.string
	bne.w bad
	bra.w opcode
struct
	move.l a3, d0
	sub.l a0, d0
	cmpi.l #4, d0
	blo.w bad
	moveq #0, d0
	move.b 1(a0), d0
	lsl.w #8, d0
	move.b (a0), d0
	bsr.w identity
	bne.w bad
	move.b d1, (a0)+
	lsr.w #8, d1
	move.b d1, (a0)+
	moveq #0, d2
	move.b 1(a0), d2
	lsl.w #8, d2
	move.b (a0), d2
	add.l d2, d2
	addq.l #2, d2  ; count plus immutable bare member IDs
	move.l a3, d0
	sub.l a0, d0
	cmp.l d2, d0
	blo.w bad
	adda.l d2, a0
	bra.w opcode
three
	addq.l #3, a0
	bra.w opcode

one
	addq.l #1, a0
	bra.w opcode
two
	addq.l #2, a0
	bra.w opcode
four
	addq.l #4, a0
	bra.w opcode
eight
	addq.l #8, a0
	bra.w opcode
symbol
	move.l a3, d0
	sub.l a0, d0
	cmpi.l #2, d0
	blo.w bad
	moveq #0, d0
	move.b 1(a0), d0
	lsl.w #8, d0
	move.b (a0), d0
	bsr.w identity
	bne.w bad
	move.b d1, (a0)+
	lsr.w #8, d1
	move.b d1, (a0)+
	bra.w opcode
endExpression
	cmpa.l a3, a0
	bne.w bad
	bra.w token
ok
	moveq #0, d0
	bra.w done
bad
	.BINDING_REMAP_FAILURE a0, a5, REMAP_SAVED_BASE
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; remap
	.priv

; D0=ID; D1=final ID,D0/CCR=status. A1 scratch; package IDs remain unchanged.
; D7 carries a word entry count and optional bit31 selecting a dense long map.
identity	.block
	move.l d0, d1
	sub.l d6, d0
	bcs.w ok
	cmp.w d7, d0  ; source IDs and counts are words; ignore the mode bit
	bhs.w bad
	btst #31, d7
	bne.w dense
	mulu.w #ENTRY_BYTES, d0
	movea.l a6, a1
	adda.l d0, a1
	moveq #0, d1
	move.w Entry.Target(a1), d1
	bra.w ok
dense
	lsl.l #2, d0
	move.l 0(a6, d0.l), d1
	cmpi.l #65535, d1
	bhi.w bad  ; includes the absent-entry sentinel -1
ok
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; identity
	.endsection
	.endmodule
