; Immutable section fragments and opaque bounded patch recipes; no grammar.
; @opforge-owner: experimental.amigaos.binary_module_image
	.module experimental.amigaos.binary_module_image
	.cpu 68020
	.use experimental.amigaos.binary_module_interface as semantics
	.use experimental.amigaos.binary_memory as memory
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
Bytes	.long ?  ; initialized payload only
Owner	.long ?  ; whole lexical block; zero is unconditional module content
Section	.long ?  ; numeric section identity, independent of target CPU
Kind	.long ?
Alignment	.long ?  ; placement constraint, never frozen payload padding
Extent	.long ?  ; allocation extent; BSS has no initialized payload
	.endstruct
BLOCK_BYTES = Block.Extent+4
Input	.struct
Id	.long ?
Data	.long ?
Bytes	.long ?
Owner	.long ?
Section	.long ?
Kind	.long ?
Alignment	.long ?
Extent	.long ?
	.endstruct
INPUT_BYTES = Input.Extent+4
KIND_CODE = 1
KIND_DATA = 2
KIND_BSS = 3
Reloc	.struct
Block	.long ?
Offset	.long ?
Target	.long ?
Addend	.long ?
Span	.long ?
Program	.long ?
Arguments	.long ?
	.endstruct
RELOC_BYTES = Reloc.Arguments+4
PATCH_CAPACITY = 4096
PROGRAM_ABSOLUTE = $ffffffff
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
Patch	.long ?
	.endstruct
LINK_FRAME_BYTES = LinkFrame.Patch+4
	.section code, kind=code
	.pub
; A0=Frame. Output=0 measures; otherwise Capacity must cover the result.
; D0/CCR=0 success,1 invalid/capacity. Other registers preserved. Used changes
; only on success. Input rows, source bytes, Frame and output must not overlap;
; all caller buffers must be readable/writable and immutable during this call.
; Blocks require strictly increasing unsigned IDs. Relocations require increasing
; (Block,Offset) order with disjoint bounded spans. Tables and payload are
; serialized in input order. Optional semantic data follows zero padding to an
; eight-byte boundary; no process pointers are serialized.
; Every input and relocation is checked before the first output write.
build	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	move.l Frame.Count(a6), d6
	move.l Frame.RelocCount(a6), d7
	; Multiplication is bounded before shifts/adds, never a truncated MULU.
	cmpi.l #$ffffffff/BLOCK_BYTES, d6
	bhi.w bad
	cmpi.l #$ffffffff/RELOC_BYTES, d7
	bhi.w bad
	move.l d6, d4
	mulu.l #BLOCK_BYTES, d4
	addi.l #HEADER_BYTES, d4
	bcs.w bad
	move.l d7, d0
	mulu.l #RELOC_BYTES, d0
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
	mulu.l #INPUT_BYTES, d1
	add.l d1, d0
	bcs.w bad
	move.l Frame.Relocs(a6), d0
	tst.l d7
	beq.w relocReady
	tst.l d0
	beq.w bad
relocReady
	move.l d7, d1
	mulu.l #RELOC_BYTES, d1
	add.l d1, d0
	bcs.w bad
	moveq #0, d5
checkBlock
	cmp.l d6, d5
	bhs.w checkRelocs
	bsr.w checkInputGeometry
	bne.w bad
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
	move.l Reloc.Span(a4), d1
	beq.w bad
	cmpi.l #PATCH_CAPACITY, d1
	bhi.w bad
	cmpi.l #PROGRAM_ABSOLUTE, Reloc.Program(a4)
	bne.w spanReady
	cmpi.l #4, d1
	bne.w bad
spanReady
	move.l Reloc.Offset(a4), d0
	cmp.l d2, d0
	blo.w bad
	add.l d1, d0
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
	mulu.l #BLOCK_BYTES, d0
	addi.l #HEADER_BYTES, d0
	move.l d0, Header.Relocs(a5)
	movea.l a5, a1
	adda.l d0, a1
	movea.l Frame.Relocs(a6), a0
	move.l d7, d0
	mulu.l #RELOC_BYTES, d0
	bsr.w copyBytes
	move.l d7, d1
	mulu.l #RELOC_BYTES, d1
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
	move.l Input.Owner(a2), Block.Owner(a3)
	move.l Input.Section(a2), Block.Section(a3)
	move.l Input.Kind(a2), Block.Kind(a3)
	move.l Input.Alignment(a2), Block.Alignment(a3)
	move.l Input.Extent(a2), Block.Extent(a3)
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
	mulu.l #BLOCK_BYTES, d4
	addi.l #HEADER_BYTES, d4
	cmp.l Header.Relocs(a6), d4
	bne.w bad
	move.l d7, d0
	sub.l d4, d0
	moveq #RELOC_BYTES, d1
	divu.l d1, d0
	move.l Header.RelocCount(a6), d5
	cmp.l d0, d5
	bhi.w bad
	mulu.l #RELOC_BYTES, d5
	add.l d5, d4
	cmp.l Header.Payload(a6), d4
	bne.w bad
	movea.l a6, a2
	adda.w #HEADER_BYTES, a2
	moveq #0, d5
nextBlock
	cmp.l d6, d5
	bhs.w relocs
	bsr.w checkBlockGeometry
	bne.w bad
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
	move.l Reloc.Span(a4), d1
	beq.w bad
	cmpi.l #PATCH_CAPACITY, d1
	bhi.w bad
	cmpi.l #PROGRAM_ABSOLUTE, Reloc.Program(a4)
	bne.w spanReady
	cmpi.l #4, d1
	bne.w bad
spanReady
	move.l Reloc.Offset(a4), d0
	cmp.l d2, d0
	blo.w bad
	add.l d1, d0
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

; A0=owned image memory.Block. After canonical interface installation, remove
; that duplicate semantic payload. Success replaces the owner with a compact
; image; failure preserves it completely. No borrowed views may cross this call.
; D0/CCR=status; other registers preserved. At most one image overlaps copying.
retireSemantics	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #memory.Block.Used+4, sp
	movea.l sp, a5
	clr.l memory.Block.Pointer(a5)
	clr.l memory.Block.Capacity(a5)
	clr.l memory.Block.Used(a5)
	movea.l a0, a6
	move.l memory.Block.Used(a6), d0
	movea.l memory.Block.Pointer(a6), a0
	bsr.w validate
	bne.w bad
	tst.l Header.SemanticsBytes(a0)
	beq.w good
	move.l Header.Payload(a0), d7
	move.l Header.BlockCount(a0), d1
	beq.w extentReady
	subq.l #1, d1
	mulu.l #BLOCK_BYTES, d1
	movea.l a0, a1
	adda.l Header.Blocks(a0), a1
	adda.l d1, a1
	move.l Block.Offset(a1), d7
	add.l Block.Bytes(a1), d7
extentReady
	move.l d7, d0
	movea.l a5, a0
	jsr memory.reserveExact
	bne.w bad
	move.l d7, memory.Block.Used(a5)
	movea.l memory.Block.Pointer(a6), a0
	movea.l memory.Block.Pointer(a5), a1
	move.l d7, d0
	bsr.w copyBytes
	movea.l memory.Block.Pointer(a5), a0
	move.l d7, Header.Bytes(a0)
	clr.l Header.Semantics(a0)
	clr.l Header.SemanticsBytes(a0)
	move.l d7, d0
	bsr.w validate
	bne.w bad
	movea.l a6, a0
	jsr memory.release
	move.l memory.Block.Pointer(a5), memory.Block.Pointer(a6)
	move.l memory.Block.Capacity(a5), memory.Block.Capacity(a6)
	move.l d7, memory.Block.Used(a6)
	clr.l memory.Block.Pointer(a5)
	clr.l memory.Block.Capacity(a5)
	clr.l memory.Block.Used(a5)
good
	moveq #0, d7
	bra.w cleanup
bad
	moveq #1, d7
cleanup
	movea.l a5, a0
	jsr memory.release
	adda.w #memory.Block.Used+4, sp
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; retireSemantics

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
	movem.l d1/a2, -(sp)
	bsr.w descriptorValidated
	bne.w done
	movea.l a1, a2
	movea.l a0, a1
	adda.l Block.Offset(a2), a1
	move.l Block.Bytes(a2), d0
	move.l d0, 0(sp)
	moveq #0, d0
done
	movem.l (sp)+, d1/a2
	tst.l d0
	rts
	.bend  ; findValidated

; A0=validated image,D1=fragment ID. A1=borrowed Block row on success.
; D0/CCR=status; other registers preserved, A1 unchanged on failure. The row
; contains only offsets and numeric geometry, and moves with the image.
descriptorValidated	.block
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
	mulu.l #BLOCK_BYTES, d5
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
	movea.l a2, a1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d5/a0/a2
	tst.l d0
	rts
	.bend  ; descriptorValidated

; A0=LinkFrame. Copies one selected block and executes bounded patch recipes.
; D0/CCR=status; other registers preserved. Used changes only on success.
; Resolve(A0=Context,D1=opaque target,D2=program)->D0=status,D1=address;
; Absolute recipes may resolve section-relative fields for a relocatable writer;
; other recipes require absolute targets. The consumer owns this choice.
; it preserves other registers and MUST be pure/stable across both passes.
; Base is available in the caller's context; targets never implicitly add Base.
; Address+unsigned Addend overflow is rejected. All selected resolutions and
; ranges are checked before output mutation. Atomic failure relies on the pure
; resolver/patcher precondition; changing callbacks violate this contract.
; Patch(A0=Context,A1=destination,D0=span capacity,D1=program,D2=arguments,
; D3=Base+field offset,D4=resolved target+addend)->D0=status,D1=written.
; Other registers preserved. Patch must be pure/stable across both passes and
; write only within its capacity. Exactly Span bytes must be produced.
; Nonabsolute recipes require Patch and the same package identity used by the
; producer. This private image does not yet serialize a package fingerprint.
; Preflight uses bounded stack scratch.
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
	suba.l #PATCH_CAPACITY, sp
	movea.l a0, a6
	movea.l LinkFrame.Image(a6), a0
	move.l LinkFrame.ImageBytes(a6), d0
	move.l LinkFrame.BlockId(a6), d1
	bsr.w descriptorValidated
	bne.w bad
	move.l LinkFrame.Base(a6), d0
	move.l Block.Alignment(a1), d2
	subq.l #1, d2
	and.l d0, d2
	bne.w bad
	add.l Block.Extent(a1), d0
	bcs.w bad
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
	mulu.l #RELOC_BYTES, d0
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
	mulu.l #RELOC_BYTES, d2
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
	move.l d2, -(sp)
	move.l Reloc.Program(a4), d2
	jsr (a3)
	move.l (sp)+, d2
	tst.l d0
	bne.w bad
	add.l Reloc.Addend(a4), d1
	bcs.w bad
	movea.l sp, a1
	tst.l d6
	beq.w destinationReady
	movea.l LinkFrame.Output(a6), a1
	adda.l Reloc.Offset(a4), a1
destinationReady
	cmpi.l #PROGRAM_ABSOLUTE, Reloc.Program(a4)
	bne.w program
	; The native 68020 supports unaligned long stores; wire order is BE32.
	move.l d1, (a1)
	bra.w advance
program
	move.l LinkFrame.Patch(a6), d0
	beq.w bad
	movea.l d0, a3
	; D2/D3/D4 retain the relocation range across callback arguments.
	movem.l d2-d4, -(sp)
	move.l d1, d4
	move.l LinkFrame.Base(a6), d3
	add.l Reloc.Offset(a4), d3
	move.l Reloc.Arguments(a4), d2
	move.l Reloc.Program(a4), d1
	move.l Reloc.Span(a4), d0
	movea.l LinkFrame.Context(a6), a0
	jsr (a3)
	movem.l (sp)+, d2-d4
	tst.l d0
	bne.w bad
	cmp.l Reloc.Span(a4), d1
	bne.w bad
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
	adda.l #PATCH_CAPACITY, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; linkValidated

	.priv
; A2=input/serialized row. Their numeric geometry shares explicit offsets.
; D0/CCR=status; D1-D2 clobbered. No pointers or payload bytes are read.
checkInputGeometry	.block
	move.l Input.Kind(a2), d0
	move.l Input.Alignment(a2), d1
	move.l Input.Extent(a2), d2
	bra.w checkGeometry
	.bend  ; checkInputGeometry
checkBlockGeometry	.block
	move.l Block.Kind(a2), d0
	move.l Block.Alignment(a2), d1
	move.l Block.Extent(a2), d2
	bra.w checkGeometry
	.bend  ; checkBlockGeometry
; Both row layouts intentionally share Bytes, Kind, Alignment and Extent.
checkGeometry	.block
	cmpi.l #KIND_CODE, d0
	blo.w bad
	cmpi.l #KIND_BSS, d0
	bhi.w bad
	tst.l d1
	beq.w bad
	move.l d1, -(sp)
	subq.l #1, d1
	and.l (sp)+, d1
	bne.w bad
	cmpi.l #KIND_BSS, d0
	beq.w bss
	cmp.l Input.Bytes(a2), d2
	bne.w bad
	bra.w good
bss
	tst.l Input.Bytes(a2)
	bne.w bad
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; checkGeometry
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
