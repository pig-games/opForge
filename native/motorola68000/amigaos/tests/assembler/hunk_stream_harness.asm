; Focused bounded-generator proof. Host assembly is not native execution proof.
	.module main
	.cpu 68020
	.use opforge.assembler.output.hunk_image as hunk
	.use opforge.assembler.output.hunk_stream as stream
	.use opforge.assembler.output.io as io
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
MAX_BYTES = 1024
	.section entry, kind=code
	.pub
; AmigaDOS entry; D0=exit code. Preserve Shell nonvolatile registers.
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	lea DosName, a1
	moveq #36, d0
	movea.l 4.w, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w bad
	move.l d0, DosBase
	bsr.w rejects
	bne.w closeBad
	; Materialized bytes are built by the unchanged native serializer.
	move.l #Reference, Source+hunk.Frame.Output
	move.l #MAX_BYTES, Source+hunk.Frame.Capacity
	lea Source, a0
	jsr hunk.build
	bne.w closeBad
	clr.l Source+hunk.Frame.Output
	move.l #64, Stream+stream.Frame.Capacity
	bsr.w compare
	bne.w closeBad
	; A non-multiple-of-four capacity must keep aligned metadata writes bounded.
	move.l #65, Stream+stream.Frame.Capacity
	bsr.w compare
	bne.w closeBad
	lea Stream, a0
	jsr stream.begin
	bne.w closeBad
	movea.l DosBase, a6
	lea Output, a0
	jsr io.write
	bne.w closeBad
	moveq #0, d7
	bra.w close
closeBad
	moveq #20, d7
close
	movea.l DosBase, a1
	movea.l 4.w, a6
	jsr CLOSE_LIBRARY(a6)
	move.l d7, d0
	bra.w done
bad
	moveq #20, d0
done
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start
	.priv
; Mutate one validated field at a time and restore it before the next case.
; D0/CCR=status. Test-only assertions never enter production modules.
rejects	.block
	move.l #63, Stream+stream.Frame.Capacity
	bsr.w reject
	bne.w done
	move.l #64, Stream+stream.Frame.Capacity
	move.l #Scratch+1, Stream+stream.Frame.Buffer
	bsr.w reject
	bne.w done
	move.l #Scratch, Stream+stream.Frame.Buffer
	move.l #Reference, Source+hunk.Frame.Output
	bsr.w reject
	bne.w done
	clr.l Source+hunk.Frame.Output
	clr.l Source+hunk.Frame.Count
	bsr.w reject
	bne.w done
	move.l #3, Source+hunk.Frame.Count
	move.w #2, Parts+hunk.Part.Kind
	bsr.w reject
	bne.w done
	move.w #1, Parts+hunk.Part.Kind
	move.l #$ffffffff, Parts+hunk.Part.Size
	bsr.w reject
	bne.w done
	move.l #257, Parts+hunk.Part.Size
	move.l #65, Parts+hunk.Part.FixupCount
	bsr.w reject
	bne.w done
	move.l #64, Parts+hunk.Part.FixupCount
	move.l #3, Fixups+hunk.Reloc.Target
	bsr.w reject
	bne.w done
	move.l #1, Fixups+hunk.Reloc.Target
	clr.l Fixups+hunk.FIXUP_BYTES+hunk.Reloc.Offset
	bsr.w reject
	bne.w done
	move.l #4, Fixups+hunk.FIXUP_BYTES+hunk.Reloc.Offset
	move.l #Payload, Parts+2*hunk.SEGMENT_BYTES+hunk.Part.Data
	bsr.w reject
	bne.w done
	clr.l Parts+2*hunk.SEGMENT_BYTES+hunk.Part.Data
	moveq #0, d0
done
	rts
	.bend  ; rejects

; Each failed begin must latch ERROR, with no item available afterwards.
reject	.block
	lea Stream, a0
	jsr stream.begin
	cmpi.l #stream.ERROR, d0
	bne.w bad
	jsr stream.next
	cmpi.l #stream.ERROR, d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; reject

; Gather and compare all bytes, assert source borrowing and metadata bounds.
; Chunk count is bounded independently of payload length; a relocation group
; here exceeds the 64-byte buffer and must resume without losing offsets.
compare	.block
	lea Stream, a0
	jsr stream.begin
	bne.w bad
	move.l #$a5a5a5a5, Scratch+64
	lea Gathered, a2
	moveq #0, d6
	moveq #0, d7
chunk
	lea Stream, a0
	jsr stream.next
	cmpi.l #stream.END, d0
	beq.w finish
	tst.l d0
	bne.w bad
	addq.l #1, d7
	cmpi.l #32, d7
	bhi.w bad
	tst.l d1
	ble.w bad
	lea Payload, a1
	cmpa.l a1, a0
	beq.w borrowedCode
	lea DataPayload, a1
	cmpa.l a1, a0
	beq.w borrowedData
	lea Scratch, a1
	cmpa.l a1, a0
	beq.w metadata
	; Only the small zero-padding slices may use another address.
	cmpi.l #3, d1
	bhi.w bad
	moveq #0, d2
zero
	tst.b 0(a0, d2.l)
	bne.w bad
	addq.l #1, d2
	cmp.l d1, d2
	blo.w zero
	bra.w copy
borrowedCode
	cmpi.l #257, d1
	bne.w bad
	bset #0, d6
	bne.w bad
	bra.w copy
borrowedData
	cmpi.l #3, d1
	bne.w bad
	bset #1, d6
	bne.w bad
	bra.w copy
metadata
	cmp.l Stream+stream.Frame.Capacity, d1
	bhi.w bad
	move.l d1, d2
	andi.l #3, d2
	bne.w bad
copy
	move.l a2, d2
	lea Gathered, a1
	sub.l a1, d2
	add.l d1, d2  ; validate the destination extent before copying
	bcs.w bad
	cmpi.l #MAX_BYTES, d2
	bhi.w bad
	move.l d1, d2
copyByte
	move.b (a0)+, (a2)+
	subq.l #1, d2
	bne.w copyByte
	bra.w chunk
finish
	cmpi.l #3, d6
	bne.w bad
	cmpi.l #$a5a5a5a5, Scratch+64
	bne.w bad
	move.l a2, d1
	lea Gathered, a1
	sub.l a1, d1
	cmp.l Source+hunk.Frame.Used, d1
	bne.w bad
	lea Gathered, a1
	lea Reference, a2
equal
	cmpm.b (a1)+, (a2)+
	bne.w bad
	subq.l #1, d1
	bne.w equal
	lea Stream, a0
	jsr stream.next
	cmpi.l #stream.END, d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; compare
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
Path	.byte "Work:output.hunk", 0
	.align 4
Source	.long Parts, 3, 0, 0, 0
Stream	.long Source, Scratch, 64
	.for field in 0..(stream.FRAME_BYTES-12)/4
	.long 0
	.endfor
Output	.long Path, 0, 0, 0, 0, stream.next, Stream
Parts
	.word 1, 0
	.long Payload, 257, 257, Fixups, 64
	.word 2, 0
	.long DataPayload, 3, 3, 0, 0
	.word 3, 0
	.long 0, 0, 17, 0, 0
Fixups
	.for index in 0..64
	.long 1+(index&1), index*4
	.endfor
Payload
	.for value in 0..64
	.long 0
	.endfor
	.byte $a5
DataPayload	.byte 7, 8, 9
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
Scratch	.res byte, 68
Reference	.res byte, MAX_BYTES
Gathered	.res byte, MAX_BYTES
	.endsection
	.output "build/hunk_stream_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
