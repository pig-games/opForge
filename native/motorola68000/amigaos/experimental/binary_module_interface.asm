; Owned module-local semantic exports. No parsing, placement or CPU semantics.
; @opforge-owner: experimental.amigaos.binary_module_interface
	.module experimental.amigaos.binary_module_interface
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_values as values
	.pub
Header	.struct
Magic	.long ?
Bytes	.long ?
ExportCount	.long ?
Exports	.long ?
MemberCount	.long ?
Members	.long ?
Names	.long ?
NameBytes	.long ?
Arena	.long ?
ArenaBytes	.long ?
	.endstruct
HEADER_BYTES = Header.ArenaBytes+4
MAGIC = $4f464d53; OFMS integrity tag, not a contract-version selector
NAME_LIMIT = 255
Export	.struct
Id	.long ?  ; canonical module-local value identity
Name	.long ?  ; input pointer; serialized offset, zero for private closure entity
NameBytes	.long ?
Kind	.long ?
Low	.long ?  ; compound: arena-relative offset
High	.long ?
Provenance	.long ?  ; scalar: 0 absolute, 1..8 module-local section
	.endstruct
EXPORT_BYTES = Export.Provenance+4
Member	.struct
Id	.long ?
Name	.long ?  ; input pointer; serialized image-relative offset
NameBytes	.long ?
	.endstruct
MEMBER_BYTES = Member.NameBytes+4
Frame	.struct
Owner	.long ?  ; borrowed source values.Owner for compound exports
Exports	.long ?
ExportCount	.long ?
Members	.long ?
MemberCount	.long ?
Destination	.long ?  ; empty memory.Block; ownership transfers on success
	.endstruct
FRAME_BYTES = Frame.Destination+4
OK = 0
INVALID = 1
NOT_FOUND = 2
; Artifact address recipe; never a runtime values.Kind.
FRAGMENT_ADDRESS = $100
FORWARDED_ENTITY = $101; dependency ordinal in Low, provider entity in High
	.priv
	.section bss, kind=bss
Active	.res long, 1
Caller	.res long, 1
Compact	.res byte, values.OWNER_BYTES
Offsets	.res byte, memory.Block.Used+4
Image	.res byte, memory.Block.Used+4
NameBytes	.res long, 1
TableEnd	.res long, 1
ArenaOffset	.res long, 1
	.endsection
	.section code, kind=code
	.pub
; A0=Frame. D0/CCR=status; other registers preserved. Sequential build only.
; Input rows/names/source Owner are borrowed and immutable throughout the call.
; IDs must increase strictly within each table. Member names are bare keys.
; Destination must be empty and disjoint from all input/storage descriptors.
; Publication is transactional; failure leaves Destination empty. The caller
; supplies complete nominal/member dependency closure. IDs remain module-local.
; Nameless rows retain private value/schema roots required by public closure.
; Current values.copyRecord accepts scalar-cell compound descriptors only;
; unsupported nested compounds reject rather than retaining borrowed offsets.
; Templates, configuration and deferred recipe programs are not in this image.
build	.block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l Active
	bne.w busy
	move.l #1, Active
	move.l a0, Caller
	movea.l a0, a6
	movea.l Frame.Destination(a6), a0
	move.l a0, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	move.l memory.Block.Pointer(a0), d0
	or.l memory.Block.Capacity(a0), d0
	or.l memory.Block.Used(a0), d0
	bne.w bad
	move.l Frame.ExportCount(a6), d7
	cmpi.l #memory.LIMIT/EXPORT_BYTES, d7
	bhi.w bad
	move.l d7, d0
	mulu.l #EXPORT_BYTES, d0
	addi.l #HEADER_BYTES, d0
	move.l d0, TableEnd
	move.l Frame.MemberCount(a6), d0
	cmpi.l #memory.LIMIT/MEMBER_BYTES, d0
	bhi.w bad
	mulu.l #MEMBER_BYTES, d0
	add.l d0, TableEnd
	bcs.w bad
	cmpi.l #memory.LIMIT, TableEnd
	bhi.w bad
	move.l d7, d0
	lsl.l #2, d0
	lea Offsets, a0
	jsr memory.reserveExact
	bne.w bad
	clr.l NameBytes
	movea.l Frame.Exports(a6), a4
	movea.l Offsets+memory.Block.Pointer, a5
	tst.l d7
	beq.w members
	move.l a4, d0
	beq.w bad
	btst #0, d0
	bne.w bad
exports
	tst.l Export.NameBytes(a4)
	bne.w publicName
	tst.l Export.Name(a4)
	bne.w bad
	bra.w exportValue
publicName
	movea.l Export.Name(a4), a0
	move.l Export.NameBytes(a4), d0
	bsr.w inputName
	bne.w bad
	move.l Export.NameBytes(a4), d0
	add.l d0, NameBytes
	bcs.w bad
exportValue
	move.l Export.Kind(a4), d3
	move.l Export.Low(a4), d2
	cmpi.l #FRAGMENT_ADDRESS, d3
	beq.w fragment
	cmpi.l #FORWARDED_ENTITY, d3
	beq.w forwarded
	cmpi.l #values.STRUCT_LITERAL, d3
	bhi.w bad
	tst.l d3
	beq.w scalar
	tst.l Export.High(a4)
	bne.w bad
	tst.l Export.Provenance(a4)
	bne.w bad
	movea.l Frame.Owner(a6), a0
	move.l a0, d0
	beq.w bad
	btst #0, d0
	bne.w bad
	move.l values.Owner.Arena+memory.Block.Used(a0), d0
	movea.l values.Owner.Arena+memory.Block.Pointer(a0), a1
	move.l d2, d1
	lea Compact, a0
	jsr values.copyRecord
	bne.w bad
	bra.w offsetReady
fragment
	tst.l Export.Provenance(a4)
	bne.w bad
	tst.l Export.High(a4)
	beq.w bad
	cmpi.l #65535, Export.High(a4)
	bhi.w bad
	cmpi.l #memory.LIMIT, d2
	bhi.w bad
	bra.w offsetReady
forwarded
	tst.l Export.Provenance(a4)
	bne.w bad
	cmpi.l #65535, Export.Low(a4)
	bhi.w bad
	cmpi.l #65535, Export.High(a4)
	bhi.w bad
	bra.w offsetReady
scalar
	cmpi.l #8, Export.Provenance(a4)
	bhi.w bad
	tst.l Export.Provenance(a4)
	beq.w offsetReady
	tst.l Export.High(a4)
	bne.w bad
offsetReady
	move.l d2, (a5)+
	adda.w #EXPORT_BYTES, a4
	subq.l #1, d7
	bne.w exports
members
	move.l Frame.MemberCount(a6), d7
	movea.l Frame.Members(a6), a4
	tst.l d7
	beq.w allocate
	move.l a4, d0
	beq.w bad
	btst #0, d0
	bne.w bad
memberNames
	movea.l Member.Name(a4), a0
	move.l Member.NameBytes(a4), d0
	bsr.w inputName
	bne.w bad
	move.l Member.NameBytes(a4), d0
	add.l d0, NameBytes
	bcs.w bad
	adda.w #MEMBER_BYTES, a4
	subq.l #1, d7
	bne.w memberNames
allocate
	move.l TableEnd, d0
	add.l NameBytes, d0
	bcs.w bad
	addq.l #7, d0
	bcs.w bad
	andi.l #$fffffff8, d0
	move.l d0, ArenaOffset
	add.l Compact+values.Owner.Arena+memory.Block.Used, d0
	bcs.w bad
	move.l d0, d6
	lea Image, a0
	jsr memory.reserveExact
	bne.w bad
	move.l d6, Image+memory.Block.Used
	movea.l Image+memory.Block.Pointer, a3
	move.l #MAGIC, Header.Magic(a3)
	move.l d6, Header.Bytes(a3)
	move.l Frame.ExportCount(a6), Header.ExportCount(a3)
	move.l #HEADER_BYTES, Header.Exports(a3)
	move.l Frame.MemberCount(a6), Header.MemberCount(a3)
	move.l Frame.ExportCount(a6), d0
	mulu.l #EXPORT_BYTES, d0
	addi.l #HEADER_BYTES, d0
	move.l d0, Header.Members(a3)
	move.l TableEnd, Header.Names(a3)
	move.l NameBytes, Header.NameBytes(a3)
	move.l ArenaOffset, Header.Arena(a3)
	move.l Compact+values.Owner.Arena+memory.Block.Used, Header.ArenaBytes(a3)
	lea HEADER_BYTES(a3), a2
	movea.l a3, a5
	adda.l TableEnd, a5
	movea.l Frame.Exports(a6), a4
	move.l Frame.ExportCount(a6), d7
	movea.l Offsets+memory.Block.Pointer, a1
writeExports
	tst.l d7
	beq.w writeMembersBegin
	move.l Export.Id(a4), (a2)+
	moveq #0, d0
	tst.l Export.NameBytes(a4)
	beq.w noName
	move.l a5, d0
	sub.l a3, d0
noName
	move.l d0, (a2)+
	move.l Export.NameBytes(a4), (a2)+
	move.l Export.Kind(a4), (a2)+
	move.l (a1)+, (a2)+
	move.l Export.High(a4), (a2)+
	move.l Export.Provenance(a4), (a2)+
	movea.l Export.Name(a4), a0
	move.l Export.NameBytes(a4), d0
	bsr.w appendName
	adda.w #EXPORT_BYTES, a4
	subq.l #1, d7
	bra.w writeExports
writeMembersBegin
	movea.l Frame.Members(a6), a4
	move.l Frame.MemberCount(a6), d7
writeMembers
	tst.l d7
	beq.w payload
	move.l Member.Id(a4), (a2)+
	move.l a5, d0
	sub.l a3, d0
	move.l d0, (a2)+
	move.l Member.NameBytes(a4), (a2)+
	movea.l Member.Name(a4), a0
	move.l Member.NameBytes(a4), d0
	bsr.w appendName
	adda.w #MEMBER_BYTES, a4
	subq.l #1, d7
	bra.w writeMembers
payload
	movea.l a3, a2
	adda.l ArenaOffset, a2
padding
	cmpa.l a2, a5
	bhs.w copyArena
	clr.b (a5)+
	bra.w padding
copyArena
	movea.l Compact+values.Owner.Arena+memory.Block.Pointer, a0
	move.l Compact+values.Owner.Arena+memory.Block.Used, d0
	bsr.w appendName
	movea.l a3, a0
	move.l Image+memory.Block.Used, d0
	bsr.w validate
	bne.w bad
	movea.l Frame.Destination(a6), a0
	move.l Image+memory.Block.Pointer, memory.Block.Pointer(a0)
	move.l Image+memory.Block.Capacity, memory.Block.Capacity(a0)
	move.l Image+memory.Block.Used, memory.Block.Used(a0)
	clr.l Image+memory.Block.Pointer
	clr.l Image+memory.Block.Capacity
	clr.l Image+memory.Block.Used
	moveq #OK, d0
	bra.w cleanup
bad
	moveq #INVALID, d0
cleanup
	move.l d0, -(sp)
	lea Compact, a0
	jsr values.release
	lea Offsets, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Image, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l Caller
	clr.l NameBytes
	clr.l TableEnd
	clr.l ArenaOffset
	clr.l Active
	move.l (sp)+, d0
	bra.w done
busy
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; build
	.priv
; A0=borrowed name,D0=length. D0/CCR=status; reject missing/NUL names.
inputName	.block
	movem.l d1/a0, -(sp)
	tst.l d0
	beq.w bad
	cmpi.l #NAME_LIMIT, d0
	bhi.w bad
	move.l a0, d1
	beq.w bad
	add.l d0, d1
	bcs.w bad
	move.l d0, d1
next
	tst.b (a0)+
	beq.w bad
	subq.l #1, d1
	bne.w next
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; inputName
; A0=source,D0=bytes,A5=destination cursor. Preserve other registers.
appendName	.block
	movem.l d0/a0, -(sp)
	tst.l d0
	beq.w done
next
	move.b (a0)+, (a5)+
	subq.l #1, d0
	bne.w next
done
	movem.l (sp)+, d0/a0
	rts
	.bend  ; appendName
	.include "binary_module_interface_read.i"
	.endsection
	.endmodule
