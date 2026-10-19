; Immutable fixed fragments and opaque absolute-long relocations; no grammar.
; @opforge-owner: experimental.amigaos.binary_module_image
	.module experimental.amigaos.binary_module_image
	.cpu 68020
	.use experimental.amigaos.binary_module_interface as semantics
	.pub
Header	.struct
Magic	.long ?
Bytes	.long ?
BlockCount	.long ?
Blocks	.long ?
RelocCount	.long ?
Relocs	.long ?
Payload	.long ?
Semantics	.long ?
SemanticsBytes	.long ?
	.endstruct
HEADER_BYTES = Header.SemanticsBytes+4
MAGIC = $4f464d49; OFMI: integrity tag, not executor version dispatch
Block	.struct
Id	.long ?
Offset	.long ?
Bytes	.long ?
	.endstruct
BLOCK_BYTES = Block.Bytes+4
Input	.struct
Id	.long ?
Data	.long ?
Bytes	.long ?
	.endstruct
INPUT_BYTES = Input.Bytes+4
Reloc	.struct
Block	.long ?
Offset	.long ?
Target	.long ?
Addend	.long ?
	.endstruct
RELOC_BYTES = Reloc.Addend+4
Frame	.struct
Blocks	.long ?
Count	.long ?
Relocs	.long ?
RelocCount	.long ?
Output	.long ?
Capacity	.long ?
Used	.long ?
Semantics	.long ?  ; optional borrowed owned-interface image
SemanticsBytes	.long ?
	.endstruct
FRAME_BYTES = Frame.SemanticsBytes+4
LinkFrame	.struct
Image	.long ?
ImageBytes	.long ?
BlockId	.long ?
Base	.long ?  ; placement input for the resolver; no implicit self relocation
Output	.long ?
Capacity	.long ?
Used	.long ?
Resolve	.long ?
Context	.long ?
	.endstruct
LINK_FRAME_BYTES = LinkFrame.Context+4
	.section code, kind=code
	.pub
; A0=Frame. Output=0 measures; otherwise Capacity must cover the result.
; D0/CCR=0 success,1 invalid/capacity. Other registers preserved. Used changes
; only on success. Input rows, source bytes, Frame and output must not overlap;
; all caller buffers must be readable/writable and immutable during this call.
; Blocks require strictly increasing unsigned IDs. Relocations require increasing
; (Block,Offset) order with disjoint four-byte fields. Tables and payload are
; serialized in input order. Optional semantic data follows zero padding to an
; eight-byte boundary; no process pointers are serialized.
; Every input and relocation is checked before the first output write.
build	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l Frame.Count(a6), d6
	move.l Frame.RelocCount(a6), d7
	; Multiplication is bounded before shifts/adds, never a truncated MULU.
	cmpi.l #$0fffffff, d6
	bhi.w bad
	cmpi.l #$0fffffff, d7
	bhi.w bad
	move.l d6, d4
	lsl.l #2, d4
	move.l d4, d0
	add.l d4, d4
	add.l d0, d4
	addi.l #HEADER_BYTES, d4
	bcs.w bad
	move.l d7, d0
	lsl.l #4, d0
	add.l d0, d4
	bcs.w bad
	move.l Frame.Blocks(a6), d0
	tst.l d6
	beq.w inputReady
	tst.l d0
	beq.w bad
inputReady
	movea.l d0, a2
	move.l d6, d1
	lsl.l #2, d1
	move.l d1, d2
	add.l d1, d1
	add.l d2, d1
	add.l d1, d0
	bcs.w bad
	move.l Frame.Relocs(a6), d0
	tst.l d7
	beq.w relocReady
	tst.l d0
	beq.w bad
relocReady
	move.l d7, d1
	lsl.l #4, d1
	add.l d1, d0
	bcs.w bad
	moveq #0, d5
checkBlock
	cmp.l d6, d5
	bhs.w checkRelocs
	move.l Input.Bytes(a2), d0
	add.l d0, d4
	bcs.w bad
	tst.l d0
	beq.w checkId
	move.l Input.Data(a2), d1
	beq.w bad
	add.l d0, d1
	bcs.w bad
checkId
	tst.l d5
	beq.w blockChecked
	move.l -INPUT_BYTES+Input.Id(a2), d0
	cmp.l Input.Id(a2), d0
	bhs.w bad
blockChecked
	adda.w #INPUT_BYTES, a2
	addq.l #1, d5
	bra.w checkBlock
checkRelocs
	movea.l Frame.Relocs(a6), a4
	movea.l Frame.Blocks(a6), a2
	moveq #0, d3
	moveq #0, d2
	moveq #0, d5
nextReloc
	cmp.l d7, d5
	bhs.w checked
locate
	cmp.l d6, d3
	bhs.w bad
	move.l Input.Id(a2), d0
	cmp.l Reloc.Block(a4), d0
	beq.w field
	bhi.w bad
	moveq #0, d2
	adda.w #INPUT_BYTES, a2
	addq.l #1, d3
	bra.w locate
field
	move.l Reloc.Offset(a4), d0
	cmp.l d2, d0
	blo.w bad
	addq.l #4, d0
	bcs.w bad
	cmp.l Input.Bytes(a2), d0
	bhi.w bad
	move.l d0, d2
	adda.w #RELOC_BYTES, a4
	addq.l #1, d5
	bra.w nextReloc
checked
	tst.l Frame.SemanticsBytes(a6)
	beq.w noSemantics
	move.l d4, d0
	addq.l #7, d0
	bcs.w bad
	andi.l #$fffffff8, d0
	move.l d0, d4
	add.l Frame.SemanticsBytes(a6), d4
	bcs.w bad
	movea.l Frame.Semantics(a6), a0
	move.l Frame.SemanticsBytes(a6), d0
	jsr semantics.validate
	bne.w bad
	bra.w outputReady
noSemantics
	tst.l Frame.Semantics(a6)
	bne.w bad
outputReady
	move.l Frame.Output(a6), d0
	beq.w good
	cmp.l Frame.Capacity(a6), d4
	bhi.w bad
	; Check representable destination extent before constructing any pointers.
	add.l d4, d0
	bcs.w bad
	movea.l Frame.Output(a6), a5
	move.l #MAGIC, Header.Magic(a5)
	move.l d4, Header.Bytes(a5)
	move.l d6, Header.BlockCount(a5)
	clr.l Header.Semantics(a5)
	move.l Frame.SemanticsBytes(a6), Header.SemanticsBytes(a5)
	beq.w semanticHeaderReady
	move.l d4, d0
	sub.l Frame.SemanticsBytes(a6), d0
	move.l d0, Header.Semantics(a5)
semanticHeaderReady
	move.l #HEADER_BYTES, Header.Blocks(a5)
	move.l d7, Header.RelocCount(a5)
	move.l d6, d0
	lsl.l #2, d0
	move.l d0, d1
	add.l d0, d0
	add.l d1, d0
	addi.l #HEADER_BYTES, d0
	move.l d0, Header.Relocs(a5)
	movea.l a5, a1
	adda.l d0, a1
	movea.l Frame.Relocs(a6), a0
	move.l d7, d0
	lsl.l #4, d0
	bsr.w copyBytes
	move.l d7, d1
	lsl.l #4, d1
	add.l Header.Relocs(a5), d1
	move.l d1, Header.Payload(a5)
	movea.l a5, a3
	adda.w #HEADER_BYTES, a3
	movea.l Frame.Blocks(a6), a2
	moveq #0, d5
writeBlock
	cmp.l d6, d5
	bhs.w writeSemantics
	move.l Input.Id(a2), Block.Id(a3)
	move.l d1, Block.Offset(a3)
	move.l Input.Bytes(a2), Block.Bytes(a3)
	movea.l Input.Data(a2), a0
	movea.l a5, a1
	adda.l d1, a1
	move.l Input.Bytes(a2), d0
	bsr.w copyBytes
	add.l Input.Bytes(a2), d1
	adda.w #INPUT_BYTES, a2
	adda.w #BLOCK_BYTES, a3
	addq.l #1, d5
	bra.w writeBlock
writeSemantics
	tst.l Frame.SemanticsBytes(a6)
	beq.w good
	movea.l a5, a1
	adda.l d1, a1
padding
	cmp.l Header.Semantics(a5), d1
	bhs.w semanticPayload
	clr.b (a1)+
	addq.l #1, d1
	bra.w padding
semanticPayload
	movea.l Frame.Semantics(a6), a0
	move.l Frame.SemanticsBytes(a6), d0
	bsr.w copyBytes
good
	move.l d4, Frame.Used(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; build

; A0=image,D0=exact bytes. D0/CCR=status; all other registers preserved.
; Canonical contiguous table/payload layout is required. Empty blocks are legal.
; All words use native 68020 big-endian order. Caller supplies readable bytes;
; base and length must form a nonwrapping address extent.
validate	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l d0, d7
	move.l a0, d0
	beq.w bad
	cmpi.l #HEADER_BYTES, d7
	blo.w bad
	move.l a0, d0
	add.l d7, d0
	bcs.w bad
	cmpi.l #MAGIC, Header.Magic(a6)
	bne.w bad
	cmp.l Header.Bytes(a6), d7
	bne.w bad
	cmpi.l #HEADER_BYTES, Header.Blocks(a6)
	bne.w bad
	move.l Header.BlockCount(a6), d6
	move.l d7, d0
	subi.l #HEADER_BYTES, d0
	moveq #BLOCK_BYTES, d1
	divu.l d1, d0
	cmp.l d0, d6
	bhi.w bad
	move.l d6, d4
	lsl.l #2, d4
	move.l d4, d0
	add.l d4, d4
	add.l d0, d4
	addi.l #HEADER_BYTES, d4
	cmp.l Header.Relocs(a6), d4
	bne.w bad
	move.l d7, d0
	sub.l d4, d0
	lsr.l #4, d0
	move.l Header.RelocCount(a6), d5
	cmp.l d0, d5
	bhi.w bad
	lsl.l #4, d5
	add.l d5, d4
	cmp.l Header.Payload(a6), d4
	bne.w bad
	movea.l a6, a2
	adda.w #HEADER_BYTES, a2
	moveq #0, d5
nextBlock
	cmp.l d6, d5
	bhs.w relocs
	cmp.l Block.Offset(a2), d4
	bne.w bad
	add.l Block.Bytes(a2), d4
	bcs.w bad
	cmp.l d7, d4
	bhi.w bad
	tst.l d5
	beq.w blockOk
	move.l -BLOCK_BYTES+Block.Id(a2), d0
	cmp.l Block.Id(a2), d0
	bhs.w bad
blockOk
	adda.w #BLOCK_BYTES, a2
	addq.l #1, d5
	bra.w nextBlock
relocs
	tst.l Header.SemanticsBytes(a6)
	beq.w noSemantics
	move.l d4, d0
	addq.l #7, d0
	bcs.w bad
	andi.l #$fffffff8, d0
	cmp.l Header.Semantics(a6), d0
	bne.w bad
	cmp.l d7, d0
	bhi.w bad
	move.l d7, d1
	sub.l d0, d1
	cmp.l Header.SemanticsBytes(a6), d1
	bne.w bad
	movea.l a6, a0
	adda.l d4, a0
padding
	cmp.l Header.Semantics(a6), d4
	bhs.w semanticPayload
	tst.b (a0)+
	bne.w bad
	addq.l #1, d4
	bra.w padding
semanticPayload
	move.l Header.SemanticsBytes(a6), d0
	jsr semantics.validate
	bne.w bad
	bra.w relocationTable
noSemantics
	tst.l Header.Semantics(a6)
	bne.w bad
	cmp.l d7, d4
	bne.w bad
relocationTable
	movea.l a6, a4
	adda.l Header.Relocs(a6), a4
	movea.l a6, a2
	adda.w #HEADER_BYTES, a2
	moveq #0, d3
	moveq #0, d2
	moveq #0, d5
nextReloc
	cmp.l Header.RelocCount(a6), d5
	bhs.w good
locate
	cmp.l d6, d3
	bhs.w bad
	move.l Block.Id(a2), d0
	cmp.l Reloc.Block(a4), d0
	beq.w field
	bhi.w bad
	moveq #0, d2
	adda.w #BLOCK_BYTES, a2
	addq.l #1, d3
	bra.w locate
field
	move.l Reloc.Offset(a4), d0
	cmp.l d2, d0
	blo.w bad
	addq.l #4, d0
	bcs.w bad
	cmp.l Block.Bytes(a2), d0
	bhi.w bad
	move.l d0, d2
	adda.w #RELOC_BYTES, a4
	addq.l #1, d5
	bra.w nextReloc
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

; A0=image,D0=bytes. Success A1=borrowed semantic image,D1=bytes.
; D0/CCR=status; missing semantics is NOT_FOUND; others preserved.
; The view owns no pointers and remains valid only while its image is retained.
semanticView	.block
	bsr.w validate
	bne.w done
	move.l Header.SemanticsBytes(a0), d0
	beq.w absent
	move.l d0, d1
	movea.l a0, a1
	adda.l Header.Semantics(a0), a1
	moveq #0, d0
	bra.w done
absent
	moveq #semantics.NOT_FOUND, d0
done
	tst.l d0
	rts
	.bend  ; semanticView

; A0=image,D0=bytes,D1=block ID. Success: A1=payload,D1=length.
; D0/CCR=status (missing is invalid); remaining registers preserved.
; On failure A1 and D1 are unchanged. No borrowed pointer survives image movement.
find	.block
	bsr.w validate
	bne.w done
	bsr.w findValidated
done
	tst.l d0
	rts
	.bend  ; find

; Same result contract as find. Image MUST have passed validate for these exact
; bytes and remained immutable since then. No validation or table scan repeats.
findValidated	.block
	movem.l d2-d5/a0/a2, -(sp)
	moveq #0, d2
	move.l Header.BlockCount(a0), d3
next
	cmp.l d3, d2
	bhs.w bad
	move.l d3, d4
	sub.l d2, d4
	lsr.l #1, d4
	add.l d2, d4
	move.l d4, d5
	lsl.l #2, d5
	move.l d5, d0
	add.l d5, d5
	add.l d0, d5
	movea.l a0, a2
	adda.l Header.Blocks(a0), a2
	adda.l d5, a2
	cmp.l Block.Id(a2), d1
	beq.w found
	blo.w lower
	move.l d4, d2
	addq.l #1, d2
	bra.w next
lower
	move.l d4, d3
	bra.w next
found
	movea.l a0, a1
	adda.l Block.Offset(a2), a1
	move.l Block.Bytes(a2), d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d5/a0/a2
	tst.l d0
	rts
	.bend  ; findValidated

; A0=LinkFrame. Copies one selected block and patches its BE32 absolute fields.
; D0/CCR=status; other registers preserved. Used changes only on success.
; Resolve(A0=Context,D1=opaque target)->D0=status,D1=absolute address;
; it preserves other registers and MUST be pure/stable across both passes.
; Base is available in the caller's context; targets never implicitly add Base.
; Address+unsigned Addend overflow is rejected. All selected resolutions and
; ranges are checked before output mutation. Atomic failure relies on the pure
; resolver precondition; a changing second-pass resolver violates this contract.
; Input/output/Frame do not overlap.
link	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	move.l LinkFrame.ImageBytes(a1), d0
	movea.l LinkFrame.Image(a1), a0
	bsr.w validate
	movem.l (sp)+, a0-a1
	bne.w done
	bsr.w linkValidated
done
	tst.l d0
	rts
	.bend  ; link

; Same linking contract; Image MUST have passed validate for ImageBytes and
; remained immutable. Avoids validating unrelated payload/directory rows again.
linkValidated	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l LinkFrame.Image(a6), a0
	move.l LinkFrame.ImageBytes(a6), d0
	move.l LinkFrame.BlockId(a6), d1
	bsr.w findValidated
	bne.w bad
	movea.l a1, a5
	move.l d1, d7
	move.l LinkFrame.Base(a6), d0
	add.l d7, d0
	bcs.w bad
	cmp.l LinkFrame.Capacity(a6), d7
	bhi.w bad
	move.l LinkFrame.Output(a6), d0
	tst.l d7
	beq.w outputReady
	tst.l d0
	beq.w bad
outputReady
	add.l d7, d0
	bcs.w bad
	; Binary lower bound reads relocation metadata only; no payload is touched.
	movea.l LinkFrame.Image(a6), a4
	moveq #0, d2
	move.l Header.RelocCount(a4), d3
	adda.l Header.Relocs(a4), a4
lowerBound
	cmp.l d3, d2
	bhs.w rangeReady
	move.l d3, d4
	sub.l d2, d4
	lsr.l #1, d4
	add.l d2, d4
	move.l d4, d0
	lsl.l #4, d0
	movea.l a4, a2
	adda.l d0, a2
	move.l Reloc.Block(a2), d0
	cmp.l LinkFrame.BlockId(a6), d0
	bhs.w lowerHalf
	move.l d4, d2
	addq.l #1, d2
	bra.w lowerBound
lowerHalf
	move.l d4, d3
	bra.w lowerBound
rangeReady
	movea.l LinkFrame.Image(a6), a2
	move.l Header.RelocCount(a2), d3
	sub.l d2, d3
	lsl.l #4, d2
	adda.l d2, a4
	movea.l a4, a2
	moveq #0, d6
pass
	movea.l a2, a4
	move.l d3, d5
next
	tst.l d5
	beq.w passDone
	move.l Reloc.Block(a4), d0
	cmp.l LinkFrame.BlockId(a6), d0
	bne.w passDone
	move.l LinkFrame.Resolve(a6), d0
	beq.w bad
	movea.l d0, a3
	movea.l LinkFrame.Context(a6), a0
	move.l Reloc.Target(a4), d1
	jsr (a3)
	tst.l d0
	bne.w bad
	add.l Reloc.Addend(a4), d1
	bcs.w bad
	tst.l d6
	beq.w advance
	movea.l LinkFrame.Output(a6), a1
	adda.l Reloc.Offset(a4), a1
	; The native 68020 supports unaligned long stores; wire order is BE32.
	move.l d1, (a1)
advance
	adda.w #RELOC_BYTES, a4
	subq.l #1, d5
	bra.w next
passDone
	tst.l d6
	bne.w good
	movea.l a5, a0
	movea.l LinkFrame.Output(a6), a1
	move.l d7, d0
	bsr.w copyBytes
	moveq #1, d6
	bra.w pass
good
	move.l d7, LinkFrame.Used(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; linkValidated

	.priv
; A0/A1=nonoverlapping readable/writable ranges,D0=bytes. Other data registers
; preserved. The caller validated extents; empty ranges need no valid pointer.
copyBytes	.block
longs
	cmpi.l #4, d0
	blo.w tail
	move.l (a0)+, (a1)+
	subq.l #4, d0
	bra.w longs
tail
	tst.l d0
	beq.w done
next
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w next
done
	rts
	.bend  ; copyBytes
	.endsection
	.endmodule
