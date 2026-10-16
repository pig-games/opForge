; Prepared declaration leaves: evaluated constants, schemas and a spelling closure.
; This private experiment publishes no machine code and owns no process pointers.
	.module experimental.amigaos.binary_leaf_object
	.cpu 68020
	.use experimental.amigaos.binary_leaf_dictionary as dictionary
	.use experimental.amigaos.binary_leaf_schemas as schemas
	.use experimental.amigaos.binary_leaf_evaluate as leaf_evaluate
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as binding
	.use experimental.amigaos.binary_imports as imports
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_struct_values as structs
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_source as source
	.use opasm.amigaos.binary_expression as expression
	.use exprvm.amigaos.runtime as runtime
	.include "memory_telemetry.i"
	.pub
MAGIC = $4c4f4231
EMPTY = $ffff
Header	.struct
Magic	.long ?
Bytes	.long ?
Dictionary	.long ?
DictionaryBytes	.long ?
Rows	.long ?
Count	.long ?
Schemas	.long ?
SchemaBytes	.long ?
CaptureBytes	.long ?  ; physical body extent, checked against this run's capture
	.endstruct
HEADER_BYTES = Header.CaptureBytes+4
Row	.struct
Line	.word ?
Flags	.byte ?
Kind	.byte ?  ; SCALAR, STRUCT_TYPE or $ff for a non-emitting source record
Id	.word ?
Reserved	.word ?
Low	.long ?
High	.long ?
	.endstruct
ROW_BYTES = Row.High+4
Frame	.struct
Scope	.long ?
Module	.long ?
Records	.long ?
RecordBytes	.long ?
CaptureBytes	.long ?
Object	.res memory.Block.Used+4
Dictionary	.res memory.Block.Used+4
Map	.res memory.Block.Used+4
Rows	.res memory.Block.Used+4
Schemas	.res memory.Block.Used+4
Evaluation	.res leaf_evaluate.STATE_BYTES
SeedBase	.long ?
Count	.long ?
	.endstruct
FRAME_BYTES = Frame.Count+4
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
	.section bss, kind=bss
DiagnosticStage	.res long, 1
	.endsection
.endif
.endif
.endif
	.section code, kind=code
; A0=zeroed/private Frame. Release all owned temporary and object buffers.
; Registers preserved; CCR unspecified. Safe after any failed export/import.
release	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	lea Frame.Object(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Frame.Dictionary(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Frame.Map(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Frame.Rows(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Frame.Schemas(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Frame.Evaluation(a1), a0
	jsr leaf_evaluate.release
	movem.l (sp)+, a0-a1
	rts
	.bend  ; release

; A0=Frame with a complete ordinary module body and its scope/module owner.
; D0/CCR=0 exported,1 ineligible/storage. Scope semantics are never changed.
; Every executable row must be a known immutable scalar or plain schema.
; Folding here retains meaning, not source syntax or current-session identities.
export	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	bsr.w release
	movea.l Frame.Scope(a6), a0
	move.l Frame.Module(a6), d1
	lea Frame.Dictionary(a6), a1
	lea Frame.Map(a6), a2
	.ASSEMBLY_FAILURE_STAGE DiagnosticStage, #1
	jsr dictionary.export
	bne.w bad
	bsr.w mapReferences
	.ASSEMBLY_FAILURE_STAGE DiagnosticStage, #2
	bsr.w validateOrigins
	bne.w bad
	; Check every original expression dependency against the exported closure
	; before replacing known values with literals. Never hide an external value
	; read by folding its current result into a persistent object.
	move.l Frame.RecordBytes(a6), d0
	lea Frame.Rows(a6), a0
	jsr memory.reserveExact
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	movea.l Frame.Records(a6), a0
	move.l Frame.RecordBytes(a6), d0
	bsr.w copy
	movea.l Frame.Rows+memory.Block.Pointer(a6), a0
	move.l Frame.RecordBytes(a6), d0
	movea.l Frame.Map+memory.Block.Pointer(a6), a1
	movea.l Frame.Scope(a6), a2
	moveq #0, d1
	move.w layout.State.Base(a2), d1
	moveq #0, d2
	move.w layout.State.Count(a2), d2
	.ASSEMBLY_FAILURE_STAGE DiagnosticStage, #3
	jsr binding.remapDense
	bne.w bad
	movea.l Frame.Scope(a6), a0
	moveq #0, d0
	move.w layout.State.Base(a0), d0
	moveq #0, d1
	move.w layout.State.Count(a0), d1
	add.l d1, d0
	lea Frame.Evaluation(a6), a0
	jsr leaf_evaluate.begin
	bne.w bad
	move.l Frame.RecordBytes(a6), d0
	; Each source record is at least four bytes; this bounds the row arena.
	lsl.l #2, d0
	bcs.w bad
	cmpi.l #memory.LIMIT, d0
	bhi.w bad
	lea Frame.Rows(a6), a0
	jsr memory.reserveExact
	bne.w bad
	movea.l Frame.Records(a6), a4
	movea.l a4, a5
	adda.l Frame.RecordBytes(a6), a5
	clr.l Frame.Count(a6)
	.ASSEMBLY_FAILURE_STAGE DiagnosticStage, #4
next
	cmpa.l a5, a4
	beq.w seal
	bhi.w bad
	moveq #0, d7
	move.b source.RecordHeader.Length(a4), d7
	addq.w #1, d7
	cmpi.l #source.RECORD_HEADER_BYTES, d7
	blo.w bad
	move.l a5, d0
	sub.l a4, d0
	cmp.l d0, d7
	bhi.w bad
	lea Frame.Rows(a6), a0
	movea.l memory.Block.Pointer(a0), a3
	adda.l memory.Block.Used(a0), a3
	clr.l (a3)
	clr.l Row.Id(a3)
	clr.l Row.Low(a3)
	clr.l Row.High(a3)
	move.w source.RecordHeader.Line(a4), Row.Line(a3)
	move.b source.RecordHeader.Flags(a4), d0
	andi.b #$ff-source.FLAG_INDENT, d0
	bne.w bad
	move.b source.RecordHeader.Flags(a4), Row.Flags(a3)
	move.b #$ff, Row.Kind(a3)
	cmpi.l #source.RECORD_HEADER_BYTES, d7
	beq.w rowReady
	cmpi.l #source.DECLARATION_HEADER_BYTES, d7
	bls.w bad
	cmpi.b #1, source.RECORD_HEADER_BYTES(a4)
	bhi.w bad
	tst.b source.DeclarationRecord.Name+source.NameToken.Qualifier(a4)
	bne.w bad
	moveq #0, d0
	move.w source.DeclarationRecord.Name+source.NameToken.Id(a4), d0
	move.l d0, d6
	.ASSEMBLY_FAILURE_STAGE DiagnosticStage, #5
	bsr.w sourceId
	bne.w bad
	move.w d1, Row.Id(a3)
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, source.DeclarationRecord.Marker(a4)
	beq.w schema
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, source.DeclarationRecord.Marker(a4)
	bne.w bad
	lea Frame.Evaluation(a6), a0
	move.l d6, d0
	lea source.DeclarationRecord.Rhs(a4), a1
	movea.l a4, a2
	adda.l d7, a2
	jsr leaf_evaluate.scalar
	bne.w bad
	move.l d1, Row.Low(a3)
	move.l d2, Row.High(a3)
	move.b #values.SCALAR, Row.Kind(a3)
	bra.w rowReady
schema
	.ASSEMBLY_FAILURE_STAGE DiagnosticStage, #6
	cmpi.l #source.DECLARATION_HEADER_BYTES+4, d7
	bne.w bad
	movea.l Frame.Scope(a6), a0
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER+values.Owner.Seeds(a0), a0
	move.l source.DeclarationRecord.Rhs(a4), d0
	move.l d0, d5
	andi.l #7, d0
	bne.w bad
	movea.l Frame.Scope(a6), a1
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER+values.Owner.Seeds(a1), a1
	move.l memory.Block.Used(a1), d0
	sub.l d5, d0
	bcs.w bad
	cmpi.l #values.STRUCT_BASE_BYTES, d0
	blo.w bad
	movea.l memory.Block.Pointer(a1), a2
	adda.l d5, a2
	movea.l a2, a1
	jsr values.descriptorBytes
	bne.w bad
	move.l d2, d4
	add.l d5, d2
	bcs.w bad
	movea.l Frame.Scope(a6), a0
	cmp.l layout.IMPORT_STATE+imports.KNOWN_OWNER+values.Owner.Seeds+memory.Block.Used(a0), d2
	bhi.w bad
	cmpi.l #values.STRUCT_TYPE, values.Record.Kind(a2)
	bne.w bad
	cmp.l values.StructRecord.Nominal(a2), d6
	bne.w bad
	movea.l Frame.Scope(a6), a0
	lea layout.IMPORT_STATE+imports.KNOWN_OWNER+values.Owner.Seeds(a0), a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Used(a0), d2
	move.l d5, d1
	move.l d6, d0
	lea Frame.Evaluation(a6), a0
	jsr leaf_evaluate.type
	bne.w bad
	lea Frame.Schemas(a6), a0
	move.l memory.Block.Used(a0), d5
	move.l d5, Row.Low(a3)
	move.b #values.STRUCT_TYPE, Row.Kind(a3)
	move.l d5, d0
	add.l d4, d0
	bcs.w bad
	lea Frame.Schemas(a6), a0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d5, a1
	movea.l a2, a0
	move.l d4, d0
	bsr.w copy
	lea Frame.Schemas(a6), a0
	add.l d4, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a2
	adda.l d5, a2
	move.l values.StructRecord.Nominal(a2), d0
	bsr.w sourceId
	bne.w bad
	move.l d1, values.StructRecord.Nominal(a2)
	move.l values.Record.Count(a2), d5
	adda.w #values.STRUCT_BASE_BYTES, a2
field
	tst.l d5
	beq.w rowReady
	move.l values.StructField.Member(a2), d0
	bsr.w sourceId
	bne.w bad
	move.l d1, values.StructField.Member(a2)
	adda.w #values.STRUCT_FIELD_BYTES, a2
	subq.l #1, d5
	bra.w field
rowReady
	lea Frame.Rows(a6), a0
	addi.l #ROW_BYTES, memory.Block.Used(a0)
	addq.l #1, Frame.Count(a6)
	adda.l d7, a4
	bra.w next
seal
	.ASSEMBLY_FAILURE_STAGE DiagnosticStage, #7
	move.l #HEADER_BYTES, d0
	add.l Frame.Dictionary+memory.Block.Used(a6), d0
	addq.l #3, d0
	andi.l #$fffffffc, d0
	add.l Frame.Rows+memory.Block.Used(a6), d0
	add.l Frame.Schemas+memory.Block.Used(a6), d0
	cmpi.l #memory.LIMIT, d0
	bhi.w bad
	move.l d0, d7
	lea Frame.Object(a6), a0
	jsr memory.reserveExact
	bne.w bad
	move.l d7, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a4
	move.l #MAGIC, Header.Magic(a4)
	move.l d7, Header.Bytes(a4)
	move.l #HEADER_BYTES, Header.Dictionary(a4)
	move.l Frame.Dictionary+memory.Block.Used(a6), Header.DictionaryBytes(a4)
	move.l Frame.Count(a6), Header.Count(a4)
	move.l Frame.CaptureBytes(a6), Header.CaptureBytes(a4)
	lea HEADER_BYTES(a4), a1
	movea.l Frame.Dictionary+memory.Block.Pointer(a6), a0
	move.l Frame.Dictionary+memory.Block.Used(a6), d0
	bsr.w copy
	move.l a1, d0
	sub.l a4, d0
align
	btst #1, d0
	bne.w pad
	btst #0, d0
	beq.w aligned
pad
	clr.b (a1)+
	addq.l #1, d0
	bra.w align
aligned
	move.l d0, Header.Rows(a4)
	movea.l Frame.Rows+memory.Block.Pointer(a6), a0
	move.l Frame.Rows+memory.Block.Used(a6), d0
	bsr.w copy
	move.l a1, d0
	sub.l a4, d0
	move.l d0, Header.Schemas(a4)
	move.l Frame.Schemas+memory.Block.Used(a6), Header.SchemaBytes(a4)
	movea.l Frame.Schemas+memory.Block.Pointer(a6), a0
	move.l Frame.Schemas+memory.Block.Used(a6), d0
	bsr.w copy
	movea.l a4, a0
	move.l d7, d0
	.ASSEMBLY_FAILURE_STAGE DiagnosticStage, #8
	bsr.w validate
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; export
	.priv
; Reuse explicit numeric canonical links for reference-only occurrence IDs.
; They do not publish additional names/declarations. A dotted occurrence whose
; original complete declaration is in this leaf shares that declaration's local
; ID; unresolved alternatives remain absent and fail the dependency check.
; A6=Frame; all registers preserved, CCR unspecified.
mapReferences	.block
	movem.l d0-d6/a0-a4, -(sp)
	movea.l Frame.Scope(a6), a0
	movea.l layout.ENTRIES_POINTER(a0), a4
	movea.l Frame.Map+memory.Block.Pointer(a6), a1
	moveq #0, d6
	move.w layout.State.Count(a0), d6
again
	moveq #0, d5
	moveq #0, d4
entry
	cmp.l d6, d4
	bhs.w finished
	move.l d4, d0
	lsl.l #2, d0
	tst.l 0(a1, d0.l)
	bpl.w next
	move.l d4, d0
	mulu.w #binding.ENTRY_BYTES, d0
	lea 0(a4, d0.l), a2
	moveq #0, d0
	move.w binding.Entry.Target(a2), d0
	bsr.w mapped
	beq.w publish
	move.w binding.Entry.Flags(a2), d0
	andi.w #binding.BINDING_PROXY, d0
	beq.w next
	moveq #0, d0
	move.w binding.Entry.OriginalId(a2), d0
	bsr.w mapped
	bne.w next
publish
	move.l d4, d0
	lsl.l #2, d0
	move.l d1, 0(a1, d0.l)
	moveq #1, d5
next
	addq.l #1, d4
	bra.w entry
finished
	tst.l d5
	bne.w again
	movem.l (sp)+, d0-d6/a0-a4
	rts
mapped
	sub.w layout.State.Base(a0), d0
	bcs.w absent
	andi.l #$ffff, d0
	cmp.l d6, d0
	bhs.w absent
	lsl.l #2, d0
	move.l 0(a1, d0.l), d1
	bmi.w absent
	moveq #0, d0
	rts
absent
	moveq #1, d0
	rts
	.bend  ; mapReferences

; A6=export Frame whose dictionary and source-index map were just built.
; D0/CCR=0 dependency-free,1 ineligible; all other registers preserved.
; Preparation may already have folded a value read out of a record. Retained
; numeric reference origins must therefore also stay within this module's
; exported spelling closure. Mixed origins conservatively reject the object;
; this may decline a safe cache candidate but never drops an external use.
validateOrigins	.block
	movem.l d1-d3/a0-a3, -(sp)
	movea.l Frame.Scope(a6), a0
	movea.l Frame.Map+memory.Block.Pointer(a6), a1
	movea.l layout.MODULE_STATE+modules.FLAGS_POINTER(a0), a2
	movea.l layout.MODULE_STATE+modules.ORIGINS_POINTER(a0), a3
	moveq #0, d3
	move.w layout.State.Count(a0), d3
entry
	tst.l d3
	beq.w good
	move.w (a2)+, d2
	move.w (a3)+, d1
	tst.l (a1)+
	bpl.w next
	move.w d2, d0
	andi.w #modules.USED, d0
	beq.w next
	andi.w #modules.MIXED, d2
	bne.w bad
	cmp.w Frame.Module+2(a6), d1
	beq.w bad
next
	subq.l #1, d3
	bra.w entry
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; validateOrigins

; D0=current source ID,A6=Frame. D0/CCR=status,D1=object-local ID.
sourceId	.block
	movem.l d2/a0-a1, -(sp)
	movea.l Frame.Scope(a6), a0
	sub.w layout.State.Base(a0), d0
	bcs.w bad
	andi.l #$ffff, d0
	cmp.w layout.State.Count(a0), d0
	bhs.w bad
	lsl.l #2, d0
	movea.l Frame.Map+memory.Block.Pointer(a6), a1
	move.l 0(a1, d0.l), d1
	bmi.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2/a0-a1
	tst.l d0
	rts
	.bend  ; sourceId
; A0=source,A1=destination,D0=bytes. Clobbers all three; CCR unspecified.
copy	.block
	tst.l d0
	beq.w done
next
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w next
done
	rts
	.bend  ; copy
	.pub
; A0=external object,D0=exact bytes. Read-only combined structural validator.
; D0/CCR=status; all other registers preserved. Checked before dictionary binding.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d0, d7
	move.l a0, d1
	beq.w bad
	btst #0, d1
	bne.w bad
	add.l d0, d1
	bcs.w bad
	cmpi.l #HEADER_BYTES, d7
	blo.w bad
	cmpi.l #memory.LIMIT, d7
	bhi.w bad
	cmpi.l #MAGIC, Header.Magic(a6)
	bne.w bad
	cmp.l Header.Bytes(a6), d7
	bne.w bad
	cmpi.l #HEADER_BYTES, Header.Dictionary(a6)
	bne.w bad
	move.l Header.DictionaryBytes(a6), d0
	cmpi.l #dictionary.HEADER_BYTES, d0
	blo.w bad
	move.l d0, d1
	addi.l #HEADER_BYTES+3, d1
	bcs.w bad
	andi.l #$fffffffc, d1
	cmp.l Header.Rows(a6), d1
	bne.w bad
	cmp.l d7, d1
	bhi.w bad
	lea HEADER_BYTES(a6), a0
	jsr dictionary.validate
	bne.w bad
	moveq #0, d6
	move.w HEADER_BYTES+dictionary.Header.Count(a6), d6
	move.l Header.Count(a6), d5
	cmpi.l #memory.LIMIT/ROW_BYTES, d5
	bhi.w bad
	move.l d5, d0
	lsl.l #4, d0
	add.l Header.Rows(a6), d0
	bcs.w bad
	cmp.l Header.Schemas(a6), d0
	bne.w bad
	add.l Header.SchemaBytes(a6), d0
	bcs.w bad
	cmp.l d7, d0
	bne.w bad
	movea.l a6, a0
	adda.l Header.Schemas(a6), a0
	move.l Header.SchemaBytes(a6), d0
	move.l d6, d1
	jsr schemas.validate
	bne.w bad
	movea.l a6, a4
	adda.l Header.Rows(a6), a4
	moveq #0, d3  ; next schema offset: rows must exhaust the arena in order
row
	tst.l d5
	beq.w coverage
	tst.w Row.Reserved(a4)
	bne.w bad
	move.b Row.Flags(a4), d0
	andi.b #$ff-source.FLAG_INDENT, d0
	bne.w bad
	cmpi.b #$ff, Row.Kind(a4)
	beq.w empty
	moveq #0, d0
	move.w Row.Id(a4), d0
	cmp.l d6, d0
	bhs.w bad
	mulu.w #dictionary.ITEM_BYTES, d0
	lea HEADER_BYTES+dictionary.HEADER_BYTES(a6, d0.l), a5
	btst #0, dictionary.Item.Flags+1(a5)
	beq.w bad
	tst.b Row.Kind(a4)
	bne.w schemaRow
	btst #5, dictionary.Item.Flags+1(a5)
	bne.w bad
	bra.w next
schemaRow
	btst #5, dictionary.Item.Flags+1(a5)
	beq.w bad
	cmpi.b #values.STRUCT_TYPE, Row.Kind(a4)
	bne.w bad
	tst.l Row.High(a4)
	bne.w bad
	cmp.l Row.Low(a4), d3
	bne.w bad
	movea.l a6, a0
	adda.l Header.Schemas(a6), a0
	move.l Header.SchemaBytes(a6), d0
	move.l Row.Low(a4), d1
	moveq #0, d2
	jsr schemas.relocateOffset
	bne.w bad
	movea.l a6, a0
	adda.l Header.Schemas(a6), a0
	adda.l Row.Low(a4), a0
	moveq #0, d0
	move.w Row.Id(a4), d0
	cmp.l values.StructRecord.Nominal(a0), d0
	bne.w bad
	movea.l a0, a1
	jsr values.descriptorBytes
	bne.w bad
	add.l d2, d3
	move.l values.Record.Count(a0), d1
	lea values.STRUCT_BASE_BYTES(a0), a1
member
	tst.l d1
	beq.w next
	move.l values.StructField.Member(a1), d0
	mulu.w #dictionary.ITEM_BYTES, d0
	lea HEADER_BYTES+dictionary.HEADER_BYTES(a6, d0.l), a2
	tst.w dictionary.Item.Flags(a2)
	bne.w bad
	tst.w dictionary.Item.Owner(a2)
	bne.w bad
	tst.w dictionary.Item.Kind(a2)
	bne.w bad
	cmpi.w #$ffff, dictionary.Item.Member(a2)
	bne.w bad
	adda.w #values.STRUCT_FIELD_BYTES, a1
	subq.l #1, d1
	bra.w member
empty
	tst.w Row.Id(a4)
	bne.w bad
	tst.l Row.Low(a4)
	bne.w bad
	tst.l Row.High(a4)
	bne.w bad
next
	adda.w #ROW_BYTES, a4
	subq.l #1, d5
	bra.w row
coverage
	cmp.l Header.SchemaBytes(a6), d3
	bne.w bad
	; Every published declaration has exactly one value row. Lexical owners and
	; bare field keys have none; duplicate rows cannot silently replace a value.
	moveq #0, d4
	lea HEADER_BYTES+dictionary.HEADER_BYTES(a6), a5
declaration
	cmp.l d6, d4
	bhs.w good
	moveq #0, d3
	move.l Header.Count(a6), d5
	movea.l a6, a4
	adda.l Header.Rows(a6), a4
countRows
	tst.l d5
	beq.w counted
	cmpi.b #$ff, Row.Kind(a4)
	beq.w countNext
	cmp.w Row.Id(a4), d4
	bne.w countNext
	addq.l #1, d3
countNext
	adda.w #ROW_BYTES, a4
	subq.l #1, d5
	bra.w countRows
counted
	btst #0, dictionary.Item.Flags+1(a5)
	beq.w undeclared
	cmpi.l #1, d3
	bne.w bad
	bra.w declarationNext
undeclared
	tst.l d3
	bne.w bad
declarationNext
	adda.w #dictionary.ITEM_BYTES, a5
	addq.l #1, d4
	bra.w declaration
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; validate

; A0=Frame with a validated privately owned Object and current opened module.
; D0/CCR=0 imported,1 malformed before publication,2 fatal after publication.
; Rebind dictionary identities, then append relocated schema seeds. No parser
; or expression compiler runs. The caller publishes rows through the frontend.
beginImport	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l Frame.Object+memory.Block.Pointer(a6), a5
	movea.l a5, a0
	move.l Frame.Object+memory.Block.Used(a6), d0
	bsr.w validate
	bne.w malformed
	movea.l Frame.Scope(a6), a0
	lea HEADER_BYTES(a5), a1
	move.l Header.DictionaryBytes(a5), d0
	lea Frame.Map(a6), a2
	jsr dictionary.import
	bne.w done
	; All subsequent failures abort this assembly rather than falling back into
	; ordinary preparation with partly published declarations.
	movea.l a5, a0
	adda.l Header.Schemas(a5), a0
	move.l Header.SchemaBytes(a5), d0
	movea.l Frame.Map+memory.Block.Pointer(a6), a1
	moveq #0, d1
	move.w HEADER_BYTES+dictionary.Header.Count(a5), d1
	jsr schemas.remap
	bne.w fatal
	movea.l Frame.Scope(a6), a4
	adda.l #layout.IMPORT_STATE+imports.KNOWN_OWNER, a4
	lea values.Owner.Seeds(a4), a0
	move.l memory.Block.Used(a0), d7
	move.l d7, Frame.SeedBase(a6)
	move.l d7, d0
	add.l Header.SchemaBytes(a5), d0
	bcs.w fatal
	jsr memory.reserve
	bne.w fatal
	move.l d7, memory.Block.Used(a0)
	move.l Header.SchemaBytes(a5), d0
	add.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	adda.l d7, a1
	movea.l a5, a0
	adda.l Header.Schemas(a5), a0
	move.l Header.SchemaBytes(a5), d0
	bsr.w copy
	movea.l a5, a3
	adda.l Header.Rows(a5), a3
	move.l Header.Count(a5), d6
row
	tst.l d6
	beq.w good
	cmpi.b #values.STRUCT_TYPE, Row.Kind(a3)
	bne.w next
	movea.l a4, a0
	move.l Row.Low(a3), d1
	add.l d7, d1
	bcs.w fatal
	jsr structs.captureReplaySeed
	bne.w fatal
next
	adda.w #ROW_BYTES, a3
	subq.l #1, d6
	bra.w row
good
	moveq #0, d0
	bra.w done
malformed
	moveq #1, d0
	bra.w done
fatal
	moveq #2, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; beginImport

; A0=Frame,D1=row index,A1=256-byte output. D0/CCR=status,D1=record bytes.
; Other registers preserved. Generated records contain current IDs and compiled
; literal ExprVM programs; no source decoding, name lookup or compilation occurs.
record	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a4
	movea.l Frame.Object+memory.Block.Pointer(a6), a5
	cmp.l Header.Count(a5), d1
	bhs.w bad
	lsl.l #4, d1
	movea.l a5, a3
	adda.l Header.Rows(a5), a3
	adda.l d1, a3
	move.b #source.RECORD_HEADER_BYTES-1, source.RecordHeader.Length(a4)
	move.b Row.Flags(a3), source.RecordHeader.Flags(a4)
	move.w Row.Line(a3), source.RecordHeader.Line(a4)
	moveq #source.RECORD_HEADER_BYTES, d1
	cmpi.b #$ff, Row.Kind(a3)
	beq.w good
	moveq #0, d0
	move.w Row.Id(a3), d0
	lsl.l #2, d0
	movea.l Frame.Map+memory.Block.Pointer(a6), a0
	move.l 0(a0, d0.l), d6
	move.b #1, source.DeclarationRecord.Name+source.NameToken.Kind(a4)
	move.w d6, source.DeclarationRecord.Name+source.NameToken.Id(a4)
	clr.b source.DeclarationRecord.Name+source.NameToken.Qualifier(a4)
	cmpi.b #values.STRUCT_TYPE, Row.Kind(a3)
	beq.w schema
	move.b #source.TOKEN_CONSTANT_DECLARATION, source.DeclarationRecord.Marker(a4)
	lea source.DeclarationRecord.Rhs(a4), a0
	move.b #expression.COMPILED_TAG, (a0)+
	move.b #10, (a0)+
	move.b #runtime.COMPACT_I64, (a0)+
	move.l Row.Low(a3), d2
	move.l Row.High(a3), d3
	moveq #3, d0
low
	move.b d2, (a0)+
	lsr.l #8, d2
	dbra d0, low
	moveq #3, d0
high
	move.b d3, (a0)+
	lsr.l #8, d3
	dbra d0, high
	move.b #runtime.EXPRVM_OPCODE_END, (a0)+
	move.l a0, d1
	sub.l a4, d1
	bra.w sized
schema
	move.b #source.TOKEN_STRUCT_DECLARATION, source.DeclarationRecord.Marker(a4)
	move.l Row.Low(a3), d0
	add.l Frame.SeedBase(a6), d0
	bcs.w bad
	move.l d0, source.DeclarationRecord.Rhs(a4)
	moveq #source.DECLARATION_HEADER_BYTES+4, d1
sized
	move.l d1, d0
	subq.l #1, d0
	move.b d0, source.RecordHeader.Length(a4)
	; Prepared scalar capsules are execution records, whereas captureValue's
	; scalar path expects raw lexical tokens. Publish their already-known value
	; through the ordinary known-cell setter; schema capture remains seed-based.
	cmpi.b #values.STRUCT_TYPE, Row.Kind(a3)
	beq.w captureType
	movea.l Frame.Scope(a6), a0
	move.l d6, d0
	move.l Row.Low(a3), d1
	move.l Row.High(a3), d2
	moveq #1, d3
	jsr imports.setKnownScalar
	bne.w bad
	bra.w snapshotReady
captureType
	movea.l a4, a0
	movea.l Frame.Scope(a6), a1
	jsr imports.captureValue
	bne.w bad
snapshotReady
	movea.l Frame.Scope(a6), a0
	move.l d6, d0
	sub.w layout.State.Base(a0), d0
	andi.l #$ffff, d0
	movea.l Frame.Scope(a6), a0
	movea.l layout.IMPORT_STATE+imports.KNOWN_DEFINED_POINTER(a0), a1
	tst.b 0(a1, d0.l)
	beq.w bad
	movea.l Frame.Scope(a6), a0
	adda.l #layout.IMPORT_STATE+imports.KNOWN_OWNER, a0
	move.l d6, d1
	jsr values.getKind
	bne.w bad
	moveq #0, d0
	move.b Row.Kind(a3), d0
	cmp.l d0, d2
	bne.w bad
	; Values.getKind uses D1/D2 outputs; recover this record's published size.
	moveq #0, d1
	move.b source.RecordHeader.Length(a4), d1
	addq.l #1, d1
good
	moveq #0, d0
	bra.w done
bad
	moveq #2, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; record
	.endsection
	.endmodule
