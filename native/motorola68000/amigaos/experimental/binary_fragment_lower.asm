; Closed-module instruction lowering through the shared frozen-input engine.
; @opforge-owner: experimental.amigaos.binary_fragment_lower
	.module experimental.amigaos.binary_fragment_lower
	.cpu 68020
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_package as pkg
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_imports as imports
	.use experimental.amigaos.binary_binding_records as records
	.use experimental.amigaos.binary_lexical_store as lexical
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_encoding as encoding
	.use experimental.amigaos.binary_state as state
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_symbol_storage as storage
	.use experimental.amigaos.binary_dependencies as dependencies
	.use exprvm.amigaos.runtime as exprvm
	.use prvm.amigaos.abi as abi
	.pub
Frame	.struct
Input	.long ?
Bytes	.long ?
Package	.long ?
Scope	.long ?
Output	.long ?
Map	.long ?
Origins	.long ?
OriginCount	.long ?
Base	.long ?
	.endstruct
FRAME_BYTES = Frame.Base+4
Boundary	.struct
Old	.long ?
New	.long ?
	.endstruct
BOUNDARY_BYTES = Boundary.New+4
Origin	.struct
Start	.long ?
End	.long ?
File	.long ?
	.endstruct
ORIGIN_BYTES = Origin.File+4
LIMIT = 1024
CONVERTED = 0
INELIGIBLE = 1
ERROR = 2
	.priv
Row	.struct
Old	.long ?
Temp	.long ?
Bytes	.long ?
Block	.long ?
Instruction	.long ?
Data	.long ?
Length	.long ?
Fixup	.long ?  ; -1 or byte offset inside instruction
Target	.long ?  ; canonical global ID
Addend	.long ?
	.endstruct
ROW_BYTES = Row.Addend+4
Bridge	.struct
Global	.long ?
Flags	.long ?
Low	.long ?
High	.long ?
Known	.long ?
Block	.long ?  ; zero unless defined by a retained temporary code label
	.endstruct
BRIDGE_BYTES = Bridge.Block+4
RAW_LIMIT = source.MAX_LINE-source.BINARY_BYTES_RECORD_BYTES
	.section bss, kind=bss
Active	.res long, 1
Caller	.res long, 1
Status	.res long, 1
Rows	.res byte, memory.Block.Used+4
BridgeOwner	.res byte, memory.Block.Used+4
Temporary	.res byte, memory.Block.Used+4
Image	.res byte, memory.Block.Used+4
Result	.res byte, memory.Block.Used+4
BoundaryOwner	.res byte, memory.Block.Used+4
Symbols	.res byte, storage.STATE_BYTES
ValueOwner	.res byte, values.OWNER_BYTES
Cells	.res byte, pkg.CONTEXT_BYTES
Work	.res byte, assembly.FRAME_BYTES
Snapshot	.res byte, scopes.SNAPSHOT_BYTES
Count	.res long, 1
BridgeCount	.res long, 1
ClosedCount	.res long, 1
Base	.res long, 1
ExecBase	.res long, 1
Block	.res long, 1
BlockSerial	.res long, 1
Code	.res long, 1
RowCursor	.res long, 1
InputCursor	.res long, 1
PendingBytes	.res long, 1
PendingHeader	.res byte, source.RECORD_HEADER_BYTES
Pending	.res byte, RAW_LIMIT
	.align 4
Record	.res byte, source.MAX_LINE
Entry	.res byte, records.ENTRY_BYTES
	.align 4
TransientEnd
	.endsection
	.section code, kind=code
	.pub
; A0=Frame. D0/CCR=CONVERTED,INELIGIBLE,ERROR; all other registers kept.
; Input and prepared scope stay immutable. Output/Map point to zero Block owners;
; success transfers both, all other returns leave both empty. Sequential only:
; the ordinary assembly engine and package execution are single-session owners.
; Map contains every original record boundary and an end sentinel, in order;
; Old offsets are relative to Input. Origin rows use global prepared offsets.
; Only code instructions within nonnested whole blocks are candidates. Retained
; directives/data/labels are copied exactly. Unsupported proof rejects the whole
; module, without replaying preparation or file effects. IDs are build-local.
lower	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l Active
	bne.w busy
	move.l #1, Active
	move.l a0, Caller
	move.l #ERROR, Status
	movea.l Frame.Output(a0), a1
	bsr.w emptyOwner
	bne.w done
	movea.l Frame.Map(a0), a1
	bsr.w emptyOwner
	bne.w done
	move.l Frame.Output(a0), d0
	cmp.l Frame.Map(a0), d0
	beq.w done
	tst.l Frame.Package(a0)
	beq.w done
	tst.l Frame.Input(a0)
	beq.w done
	move.l Frame.Bytes(a0), d0
	beq.w unsupported
	cmpi.l #memory.LIMIT, d0
	bhi.w unsupported
	movea.l Frame.Scope(a0), a6
	move.l a6, d0
	beq.w done
	tst.w layout.State.Current(a6)
	bne.w unsupported
	tst.w layout.State.LoopDepth(a6)
	bne.w unsupported
	moveq #0, d0
	move.w layout.State.Base(a6), d0
	move.l d0, Base
	; Text initialization requires two distinct nonzero prepared name IDs. They
	; are private execution slots, never borrowed global source identities.
	tst.l d0
	beq.w unsupported
	move.w d0, Work+assembly.Frame.AsciiName
	addq.l #1, d0
	cmpi.l #65535, d0
	bhi.w unsupported
	move.w d0, Work+assembly.Frame.PetsciiName
	addq.l #1, d0
	move.l d0, ExecBase
	moveq #0, d0
	move.w layout.State.Count(a6), d0
	move.l d0, ClosedCount
	move.l a6, Snapshot+scopes.Snapshot.Scope
	move.l #closed, Snapshot+scopes.Snapshot.Closed
	move.l a6, Snapshot+scopes.Snapshot.User
	move.l #LIMIT*ROW_BYTES, d0
	lea Rows, a0
	jsr memory.reserveExact
	bne.w done
	move.l #LIMIT*BRIDGE_BYTES, d0
	lea BridgeOwner, a0
	jsr memory.reserveExact
	bne.w done
	movea.l Caller, a0
	move.l Frame.Bytes(a0), d0
	lea Temporary, a0
	jsr memory.reserveExact
	bne.w done
	bsr.w inventory
	bne.w done
	tst.l Temporary+memory.Block.Used
	beq.w unsupported
	move.l #INELIGIBLE, Status
	movea.l Temporary+memory.Block.Pointer, a0
	move.l Temporary+memory.Block.Used, d0
	lea Snapshot, a1
	lea compactEntry, a2
	move.l Base, d1
	move.l ClosedCount, d2
	jsr records.remap
	bne.w remapFailed
	move.l #ERROR, Status
	move.l ExecBase, d0
	add.l BridgeCount, d0
	cmpi.l #65536, d0
	bhi.w unsupported
	lea ValueOwner, a0
	jsr values.init
	bne.w done
	move.l ExecBase, d0
	add.l BridgeCount, d0
	lea Symbols, a0
	lea Cells, a1
	move.l #ValueOwner, pkg.Context.Owner(a1)
	jsr storage.grow
	bne.w done
	movea.l Caller, a0
	move.l Frame.Package(a0), Cells+pkg.Context.Package
	move.l Temporary+memory.Block.Pointer, Work+assembly.Frame.Records
	move.l Temporary+memory.Block.Used, Work+assembly.Frame.RecordBytes
	move.l #Cells, Work+assembly.Frame.Context
	move.l #allocate, Work+assembly.Frame.Allocate
	move.l #rejectReloc, Work+assembly.Frame.AddReloc
	move.l #seed, Work+assembly.Frame.Seed
	move.l #encoded, Work+assembly.Frame.Encoded
	move.l Symbols+storage.State.MemberKeys+memory.Block.Pointer, Work+assembly.Frame.MemberKeys
	move.l #INELIGIBLE, Status
	lea Work, a0
	jsr assembly.assemble
	bne.w unsupportedUnlessError
	move.l #ERROR, Status
	movea.l Caller, a0
	move.l Frame.Bytes(a0), d0
	lea Result, a0
	jsr memory.reserveExact
	bne.w done
	move.l Count, d0
	addq.l #1, d0
	mulu.w #BOUNDARY_BYTES, d0
	lea BoundaryOwner, a0
	jsr memory.reserveExact
	bne.w done
	bsr.w reconstruct
	bne.w done
	movea.l Caller, a6
	move.l Result+memory.Block.Used, d0
	cmp.l Frame.Bytes(a6), d0
	bhs.w unsupported
	; Transfer an exact-sized result, not the discarded worst-case capacity.
	movea.l Frame.Output(a6), a0
	jsr memory.reserveExact
	bne.w done
	move.l Result+memory.Block.Used, d0
	move.l d0, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a1
	movea.l Result+memory.Block.Pointer, a0
	bsr.w copy
	movea.l Frame.Map(a6), a1
	lea BoundaryOwner, a0
	moveq #(memory.Block.Used+4)/4-1, d0
transfer
	move.l (a0), (a1)+
	clr.l (a0)+
	dbra d0, transfer
	move.l #CONVERTED, Status
	bra.w done
remapFailed
	cmpi.l #ERROR, Status
	beq.w done
	cmpi.l #scopes.SNAPSHOT_INVALID, Snapshot+scopes.Snapshot.Result
	bne.w unsupported
	move.l #ERROR, Status
	bra.w done
unsupportedUnlessError
	cmpi.l #ERROR, Status
	beq.w done
unsupported
	move.l #INELIGIBLE, Status
done
	bsr.w cleanup
	move.l Status, d0
	clr.l Active
	bra.w exit
busy
	moveq #ERROR, d0
exit
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; lower
	.priv

; A1=caller Block, D0/CCR=0 only for an empty descriptor.
emptyOwner	.block
	move.l a1, d0
	beq.w bad
	move.l memory.Block.Pointer(a1), d0
	or.l memory.Block.Capacity(a1), d0
	or.l memory.Block.Used(a1), d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; emptyOwner

; Inventory complete records and construct the flat execution subset. Each
; instruction is associated with its original row, never an expected byte path.
inventory	.block
	movea.l Caller, a6
	movea.l Frame.Input(a6), a4
	movea.l a4, a5
	move.l a5, d0
	add.l Frame.Bytes(a6), d0
	bcs.w bad
	movea.l d0, a5
	movea.l Rows+memory.Block.Pointer, a3
next
	cmpa.l a5, a4
	beq.w complete
	bhi.w bad
	cmpi.l #LIMIT, Count
	bhs.w unsupported
	moveq #0, d6
	move.b (a4), d6
	addq.l #1, d6
	cmpi.l #source.RECORD_HEADER_BYTES, d6
	blo.w bad
	move.l a5, d0
	sub.l a4, d0
	cmp.l d0, d6
	bhi.w bad
	move.l a4, d0
	sub.l Frame.Input(a6), d0
	move.l d0, Row.Old(a3)
	move.l #$ffffffff, Row.Temp(a3)
	move.l d6, Row.Bytes(a3)
	clr.l Row.Block(a3)
	clr.l Row.Instruction(a3)
	clr.l Row.Length(a3)
	move.l #-1, Row.Fixup(a3)
	moveq #0, d0
	move.b source.RecordHeader.Flags(a4), d0
	move.l d0, d5
	andi.l #source.FLAG_PLAN, d0
	bne.w unsupported
	btst #source.FLAG_OMIT_BIT, d5
	bne.w advance
	btst #source.FLAG_LAYOUT_BIT, d5
	bne.w section
	tst.l Code
	beq.w outside
	btst #source.FLAG_BLOCK_CLOSE_BIT, d5
	bne.w blockClose
	btst #source.FLAG_BLOCK_OPEN_BIT, d5
	beq.w blockReady
	tst.l Block
	bne.w unsupported
	addq.l #1, BlockSerial
	move.l BlockSerial, Block
blockReady
	move.l Block, Row.Block(a3)
	tst.l Block
	beq.w outside
	lea source.RECORD_HEADER_BYTES(a4), a2
	move.l d6, d7
	subi.l #source.RECORD_HEADER_BYTES, d7
	beq.w append
	; A normalized label is the only prefix removed before head classification.
	cmpi.l #source.NAME_TOKEN_BYTES+1, d7
	blo.w head
	cmpi.b #source.TOKEN_NAME_MAX, (a2)
	bhi.w head
	cmpi.b #source.TOKEN_LABEL_DECLARATION, source.NAME_TOKEN_BYTES(a2)
	bne.w head
	moveq #0, d0
	move.w source.NameToken.Id(a2), d0
	bsr.w labelBlock
	bne.w failed
	btst #source.FLAG_BLOCK_OPEN_BIT, d5
	bne.w blockOpenLabel
	adda.w #source.NAME_TOKEN_BYTES+1, a2
	subi.l #source.NAME_TOKEN_BYTES+1, d7
	beq.w append
	; An inline instruction would also need a split original label record.
	bra.w unsupported
blockOpenLabel
	; The open flag likewise proves the scope role independently of any
	; retained dot body. Its address label alone enters the temporary stream.
	move.l Temporary+memory.Block.Used, d0
	move.l d0, Row.Temp(a3)
	movea.l Temporary+memory.Block.Pointer, a1
	adda.l d0, a1
	movea.l a4, a0
	moveq #source.RECORD_HEADER_BYTES+source.NAME_TOKEN_BYTES+1, d0
	bsr.w copy
	move.b #source.RECORD_HEADER_BYTES+source.NAME_TOKEN_BYTES, source.RecordHeader.Length(a1)
	addi.l #source.RECORD_HEADER_BYTES+source.NAME_TOKEN_BYTES+1, Temporary+memory.Block.Used
	bra.w advance
blockClose
	; Closed prepared scopes can retain a dot-statement body until global
	; completion. The shared boundary flag is sufficient for the private flat
	; session: preserve the original final row, normalize only its temporary copy.
	btst #source.FLAG_BLOCK_OPEN_BIT, d5
	bne.w unsupported
	tst.l Block
	beq.w unsupported
	move.l Block, Row.Block(a3)
	move.l Temporary+memory.Block.Used, d0
	move.l d0, Row.Temp(a3)
	movea.l Temporary+memory.Block.Pointer, a1
	adda.l d0, a1
	movea.l a4, a0
	moveq #source.RECORD_HEADER_BYTES, d0
	bsr.w copy
	move.b #source.RECORD_HEADER_BYTES-1, source.RecordHeader.Length(a1)
	addi.l #source.RECORD_HEADER_BYTES, Temporary+memory.Block.Used
	clr.l Block
	bra.w advance
head
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, (a2)
	beq.w directive
	cmpi.b #source.TOKEN_NAME_MAX, (a2)
	bhi.w unsupported
	; Immutable declarations are seeded, not executed in the flat code session.
	cmpi.l #source.NAME_TOKEN_BYTES, d7
	blo.w unsupported
	cmpi.l #source.NAME_TOKEN_BYTES+1, d7
	blo.w instruction
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, source.NAME_TOKEN_BYTES(a2)
	beq.w advance
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, source.NAME_TOKEN_BYTES(a2)
	beq.w advance
instruction
	move.l d5, d0
	andi.l #source.FLAG_BLOCK_OPEN+source.FLAG_BLOCK_CLOSE, d0
	bne.w unsupported
	move.l #1, Row.Instruction(a3)
	bra.w append
directive
	cmpi.l #abi.PRVM_PACKED_DOT_BYTES, d7
	blo.w bad
	moveq #0, d0
	move.w abi.PackedDot.Name+abi.PackedName.Id(a2), d0
	movea.l Frame.Package(a6), a2
	cmp.w pkg.Header.CpuDirective(a2), d0
	beq.w append
	jsr state.find
	tst.l d0
	bne.w unsupported
	bra.w append
outside
	; Package state changes remain ordered even outside selected code blocks.
	cmpi.l #source.RECORD_HEADER_BYTES+abi.PRVM_PACKED_DOT_BYTES, d6
	blo.w advance
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, source.RECORD_HEADER_BYTES(a4)
	bne.w advance
	moveq #0, d0
	move.w source.RECORD_HEADER_BYTES+abi.PackedDot.Name+abi.PackedName.Id(a4), d0
	movea.l Frame.Package(a6), a2
	cmp.w pkg.Header.CpuDirective(a2), d0
	beq.w append
	jsr state.find
	tst.l d0
	bne.w advance
append
	move.l Temporary+memory.Block.Used, d0
	move.l d0, Row.Temp(a3)
	movea.l Temporary+memory.Block.Pointer, a1
	adda.l d0, a1
	movea.l a4, a0
	move.l d6, d0
	bsr.w copy
	add.l d6, Temporary+memory.Block.Used
	btst #source.FLAG_BLOCK_CLOSE_BIT, d5
	beq.w advance
	clr.l Block
	bra.w advance
section
	cmpi.l #5, d6
	blo.w bad
	moveq #0, d0
	move.b source.LayoutRecord.Opcode(a4), d0
	cmpi.l #source.LAYOUT_SECTION_END, d0
	beq.w closeSection
	cmpi.l #source.LAYOUT_PENDING_CONCRETE, d0
	beq.w openSection
	cmpi.l #source.LAYOUT_PENDING_LOGICAL, d0
	bne.w advance
openSection
	tst.l Block
	bne.w unsupported
	cmpi.l #source.CONCRETE_SECTION_BYTES, d6
	blo.w bad
	clr.l Code
	cmpi.b #1, source.ConcreteSection.Kind(a4)
	bne.w advance
	move.l #1, Code
	bra.w advance
closeSection
	tst.l Block
	bne.w unsupported
	clr.l Code
advance
	adda.l d6, a4
	adda.w #ROW_BYTES, a3
	addq.l #1, Count
	bra.w next
complete
	tst.l Block
	bne.w unsupported
	tst.l BlockSerial
	beq.w unsupported
	moveq #0, d0
	rts
unsupported
	move.l #INELIGIBLE, Status
failed
	moveq #1, d0
	rts
bad
	move.l #ERROR, Status
	bra.w failed
	.bend  ; inventory

; Lookup-only ownership predicate: every observed ID must predate this module's
; closed frontier. Snapshot additionally checks complete declaration semantics.
closed	.block
	sub.l Base, d0
	bcs.w bad
	cmp.l ClosedCount, d0
	bhs.w bad
	moveq #0, d0
	rts
bad
	moveq #scopes.SNAPSHOT_INELIGIBLE, d0
	rts
	.bend  ; closed

; Reader for records.remap. Snapshot resolves canonical globals; bridge assigns
; compact execution identities while retaining a global-symbol return path.
compactEntry	.block
	movem.l d1-d7/a0-a6, -(sp)
	jsr scopes.readClosedEntry
	bne.w done
	moveq #0, d0
	move.w records.Entry.Target(a1), d0
	bsr.w lookupBridge
	bne.w done
	move.l d1, d0
	add.l ExecBase, d0
	move.w d0, records.Entry.Target(a1)
	moveq #0, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; compactEntry

; D0=canonical global; D1=bridge ordinal,A0=bridge row. Existing rows unchanged.
; New rows require scalar known state or a declared label. Preserves D2-D7/A1-A6.
lookupBridge	.block
	movem.l d2-d7/a1-a6, -(sp)
	move.l d0, d7
	moveq #0, d1
	movea.l BridgeOwner+memory.Block.Pointer, a0
find
	cmp.l BridgeCount, d1
	bhs.w create
	cmp.l Bridge.Global(a0), d7
	beq.w good
	adda.w #BRIDGE_BYTES, a0
	addq.l #1, d1
	bra.w find
create
	cmpi.l #LIMIT, d1
	bhs.w unsupported
	movea.l a0, a4
	move.l d1, d6
	movea.l Caller, a6
	movea.l Frame.Scope(a6), a6
	move.l d7, d0
	sub.l Base, d0
	bcs.w bad
	cmp.l ClosedCount, d0
	bhs.w bad
	move.l d0, d5
	lea layout.LEXICAL_STATE(a6), a0
	lea Entry, a1
	jsr lexicalEntry
	bne.w bad
	btst #0, Entry+records.Entry.Flags+1
	beq.w unsupported
	move.l d7, Bridge.Global(a4)
	moveq #0, d0
	move.w Entry+records.Entry.Flags, d0
	move.l d0, Bridge.Flags(a4)
	clr.l Bridge.Known(a4)
	clr.l Bridge.Low(a4)
	clr.l Bridge.High(a4)
	clr.l Bridge.Block(a4)
	btst #records.LABEL_VALUE_BIT, Entry+records.Entry.Flags+1
	bne.w unknown
	; openScope owns the address role of named .block declarations even when
	; their normalized colon was introduced after ordinary label declaration.
	; ScopeKind's high byte is visibility; namespaces are never address roles.
	move.w Entry+records.Entry.ScopeKind, d0
	andi.w #$ff, d0
	cmpi.w #scopes.KIND_BLOCK, d0
	beq.w created
	lea layout.IMPORT_STATE(a6), a3
	movea.l imports.KNOWN_DEFINED_POINTER(a3), a0
	move.l a0, d0
	beq.w unknown
	cmp.l imports.KNOWN_DEFINED+memory.Block.Used(a3), d5
	bhs.w unknown
	tst.b 0(a0, d5.l)
	beq.w unknown
	lea imports.KNOWN_OWNER(a3), a0
	move.l d7, d1
	jsr values.getKind
	bne.w unsupported
	cmpi.l #values.SCALAR, d2
	bne.w unsupported
	move.l d5, d0
	lsl.l #3, d0
	addq.l #8, d0
	cmp.l imports.KNOWN_VALUES+memory.Block.Used(a3), d0
	bhi.w bad
	subq.l #8, d0
	movea.l imports.KNOWN_VALUES_POINTER(a3), a0
	move.l exprvm.Value.Low(a0, d0.l), Bridge.Low(a4)
	move.l exprvm.Value.High(a0, d0.l), Bridge.High(a4)
	move.l #1, Bridge.Known(a4)
	bra.w created
unknown
	btst #records.LABEL_VALUE_BIT, Entry+records.Entry.Flags+1
	bne.w created
	move.w Entry+records.Entry.ScopeKind, d0
	andi.w #$ff, d0
	cmpi.w #scopes.KIND_BLOCK, d0
	bne.w unsupported
created
	addq.l #1, BridgeCount
	movea.l a4, a0
	move.l d6, d1
good
	moveq #0, d0
	bra.w done
unsupported
	move.l #INELIGIBLE, Status
	bra.w failed
bad
	move.l #ERROR, Status
failed
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a1-a6
	tst.l d0
	rts
	.bend  ; lookupBridge

; Adapter kept here to avoid exposing lexical cache ownership to consumers.
lexicalEntry	.block
	jsr lexical.readEntry
	rts
	.bend  ; lexicalEntry

; D0=label global ID. Canonical label declarations are already final identities.
labelBlock	.block
	movem.l d1/a0, -(sp)
	bsr.w lookupBridge
	bne.w done
	move.l Block, Bridge.Block(a0)
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; labelBlock

	.include "binary_fragment_execution.i"
	.include "binary_fragment_lower_output.i"
; A0/A1=nonoverlapping byte spans,D0=bytes; all other registers preserved.
copy	.block
	movem.l d0/a0-a1, -(sp)
	tst.l d0
	beq.w done
next
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w next
done
	movem.l (sp)+, d0/a0-a1
	rts
	.bend  ; copy

cleanup	.block
	lea Symbols, a0
	lea Cells, a1
	jsr storage.release
	lea ValueOwner, a0
	jsr values.release
	lea Rows, a0
	jsr memory.release
	lea BridgeOwner, a0
	jsr memory.release
	lea Temporary, a0
	jsr memory.release
	lea Image, a0
	jsr memory.release
	lea Result, a0
	jsr memory.release
	lea BoundaryOwner, a0
	jsr memory.release
	; All session-local counters/frames return to their zero-owner initial state.
	lea Rows, a0
	move.l #(TransientEnd-Rows)/4, d0
clear
	clr.l (a0)+
	subq.l #1, d0
	bne.w clear
	rts
	.bend  ; cleanup
	.endsection
	.endmodule
