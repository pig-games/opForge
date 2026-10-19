; Reconstruct ordinary prepared records. Immutable instructions share bounded
; binary-byte records; retained statements and relocation expressions are barriers.
reconstruct	.block
	clr.l RowCursor
next
	move.l RowCursor, d0
	cmp.l Count, d0
	bhs.w complete
	mulu.w #ROW_BYTES, d0
	movea.l Rows+memory.Block.Pointer, a4
	adda.l d0, a4
	movea.l Caller, a6
	movea.l Frame.Input(a6), a5
	adda.l Row.Old(a4), a5
	bsr.w originBoundary
	bne.w failed
	tst.l Row.Instruction(a4)
	beq.w barrier
	tst.l Row.Length(a4)
	beq.w unsupported
	cmpi.l #-1, Row.Fixup(a4)
	beq.w mapped
barrier
	bsr.w flush
	bne.w failed
mapped
	move.l Row.Old(a4), d0
	bsr.w mapBoundary
	tst.l Row.Instruction(a4)
	beq.w retained
	movea.l Image+memory.Block.Pointer, a0
	adda.l Row.Data(a4), a0
	move.l Row.Length(a4), d0
	cmpi.l #-1, Row.Fixup(a4)
	beq.w raw
	move.l Row.Fixup(a4), d0
	bsr.w rawBytes
	bne.w failed
	bsr.w flush
	bne.w failed
	bsr.w longRecord
	bne.w failed
	movea.l Image+memory.Block.Pointer, a0
	adda.l Row.Data(a4), a0
	adda.l Row.Fixup(a4), a0
	addq.l #4, a0
	move.l Row.Length(a4), d0
	sub.l Row.Fixup(a4), d0
	subq.l #4, d0
raw
	bsr.w rawBytes
	bne.w failed
	bra.w advance
retained
	movea.l a5, a0
	move.l Row.Bytes(a4), d0
	bsr.w append
	bne.w failed
advance
	addq.l #1, RowCursor
	bra.w next
complete
	bsr.w flush
	bne.w failed
	movea.l Caller, a0
	move.l Frame.Bytes(a0), d0
	bsr.w mapBoundary
	moveq #0, d0
	rts
unsupported
	move.l #INELIGIBLE, Status
failed
	moveq #1, d0
	rts
	.bend  ; reconstruct

; Flush at every physical-file boundary before publishing its exact map row.
; Origin storage is borrowed and never written; overlap/misalignment is a caller
; ownership error, not a reason to guess provenance from record line numbers.
originBoundary	.block
	movem.l d1-d3/a0, -(sp)
	move.l Frame.OriginCount(a6), d3
	beq.w good
	movea.l Frame.Origins(a6), a0
	move.l a0, d0
	beq.w bad
	move.l Row.Old(a4), d1
	add.l Frame.Base(a6), d1
	bcs.w bad
next
	cmp.l Origin.Start(a0), d1
	beq.w boundary
	cmp.l Origin.End(a0), d1
	beq.w boundary
	adda.w #ORIGIN_BYTES, a0
	subq.l #1, d3
	bne.w next
good
	moveq #0, d0
	bra.w done
boundary
	bsr.w flush
	bra.w done
bad
	move.l #ERROR, Status
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0
	tst.l d0
	rts
	.bend  ; originBoundary

; D0=old-relative boundary. Map capacity was reserved for Count+1 rows.
mapBoundary	.block
	movem.l a0, -(sp)
	movea.l BoundaryOwner+memory.Block.Pointer, a0
	adda.l BoundaryOwner+memory.Block.Used, a0
	move.l d0, Boundary.Old(a0)
	move.l Result+memory.Block.Used, Boundary.New(a0)
	addi.l #BOUNDARY_BYTES, BoundaryOwner+memory.Block.Used
	movem.l (sp)+, a0
	rts
	.bend  ; mapBoundary

; A0=bytes,D0=count,A5=original instruction header. Preserve other registers.
rawBytes	.block
	movem.l d1-d3/a0-a2, -(sp)
	move.l d0, d3
	movea.l a0, a2
next
	tst.l d3
	beq.w good
	move.l PendingBytes, d1
	bne.w headerReady
	move.l (a5), PendingHeader
headerReady
	move.l #RAW_LIMIT, d2
	sub.l d1, d2
	cmp.l d3, d2
	bls.w countReady
	move.l d3, d2
countReady
	lea Pending, a1
	adda.l d1, a1
	movea.l a2, a0
	move.l d2, d0
	bsr.w copy
	add.l d2, PendingBytes
	adda.l d2, a2
	sub.l d2, d3
	cmpi.l #RAW_LIMIT, PendingBytes
	bne.w next
	bsr.w flush
	bne.w done
	bra.w next
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; rawBytes

flush	.block
	movem.l d1-d3/a0-a2, -(sp)
	move.l PendingBytes, d3
	beq.w good
	lea Record, a2
	move.l PendingHeader, (a2)
	andi.b #source.FLAG_INDENT, source.RecordHeader.Flags(a2)
	move.l d3, d0
	addi.l #source.BINARY_BYTES_RECORD_BYTES-1, d0
	move.b d0, (a2)
	lea source.RECORD_HEADER_BYTES(a2), a1
	move.b #abi.PRVM_TOKEN_KIND_DOT, (a1)+
	clr.b (a1)+
	movea.l Caller, a0
	movea.l Frame.Package(a0), a0
	move.w pkg.Header.ByteDirective(a0), (a1)+
	clr.b (a1)+
	move.b #source.TOKEN_BINARY_BYTES, (a1)+
	move.b d3, (a1)+
	lea Pending, a0
	move.l d3, d0
	bsr.w copy
	movea.l a2, a0
	move.l d3, d0
	addi.l #source.BINARY_BYTES_RECORD_BYTES, d0
	bsr.w append
	bne.w done
	clr.l PendingBytes
good
	moveq #0, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; flush

; A4=instruction row,A5=original header. The ordinary .long evaluator/linker
; consumes a compact EXVM symbol+unsigned-addend capsule using global IDs.
longRecord	.block
	movem.l d1-d3/a0-a2, -(sp)
	lea Record, a2
	move.l (a5), (a2)
	andi.b #source.FLAG_INDENT, source.RecordHeader.Flags(a2)
	move.b #20, (a2)
	lea source.RECORD_HEADER_BYTES(a2), a1
	move.b #abi.PRVM_TOKEN_KIND_DOT, (a1)+
	clr.b (a1)+
	movea.l Caller, a0
	movea.l Frame.Package(a0), a0
	move.w pkg.Header.LongDirective(a0), (a1)+
	clr.b (a1)+
	move.b #abi.PRVM_TOKEN_KIND_COMPILED_EXPRESSION, (a1)+
	move.b #10, (a1)+
	move.b #exprvm.EXPRVM_OPCODE_PUSH_SYMBOL, (a1)+
	move.l Row.Target(a4), d1
	move.b d1, (a1)+
	lsr.l #8, d1
	move.b d1, (a1)+
	move.b #exprvm.COMPACT_U32, (a1)+
	move.l Row.Addend(a4), d1
	moveq #3, d2
literal
	move.b d1, (a1)+
	lsr.l #8, d1
	dbra d2, literal
	move.b #exprvm.COMPACT_ADD, (a1)+
	move.b #exprvm.EXPRVM_OPCODE_END, (a1)+
	movea.l a2, a0
	moveq #21, d0
	bsr.w append
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; longRecord

; A0=complete bytes,D0=count. Output may never grow beyond original extent;
; exceeding that resource ceiling is a whole-module fallback, not partial success.
append	.block
	movem.l d1-d2/a0-a2, -(sp)
	move.l Result+memory.Block.Used, d1
	move.l d1, d2
	add.l d0, d2
	bcs.w bad
	cmp.l Result+memory.Block.Capacity, d2
	bhi.w bad
	movea.l Result+memory.Block.Pointer, a1
	adda.l d1, a1
	bsr.w copy
	move.l d2, Result+memory.Block.Used
	moveq #0, d0
	bra.w done
bad
	move.l #INELIGIBLE, Status
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; append
