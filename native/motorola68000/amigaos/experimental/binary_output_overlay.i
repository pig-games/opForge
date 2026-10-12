; General overwrite/insertion path. Preflight canonical input before mutation.
; A2=Block,A6=Overlay,D1/D2/D3=incoming row,D4=old used bytes.
replace .block
 move.l d4,Overlay.Start(a6)
 move.l d4,Overlay.After(a6)
 clr.l Overlay.Located(a6)
 clr.l Overlay.Left+records.Span.Bytes(a6)
 clr.l Overlay.Right+records.Span.Bytes(a6)
 movea.l memory.Block.Pointer(a2),a1
 moveq #0,d6
scan
 cmp.l d4,d6
 beq.w sized
 move.l d4,d0
 sub.l d6,d0
 cmpi.l #records.SPAN_BYTES,d0
 blo.w bad
 bsr.w rowBounds
 bne.w bad
 tst.l d6
 beq.w sorted
 move.l Overlay.PreviousEnd(a6),d5
 cmp.l records.Span.Address(a1),d5
 bhs.w bad
sorted
 move.l d0,Overlay.PreviousEnd(a6)
 cmp.l d1,d0
 blo.w next
 tst.l Overlay.Located(a6)
 bne.w located
 move.l #1,Overlay.Located(a6)
 move.l d6,Overlay.Start(a6)
 move.l d6,Overlay.After(a6)
located
 move.l records.Span.Address(a1),d5
 cmp.l Overlay.End(a6),d5
 bhi.w next
 move.l d6,d5
 addi.l #records.SPAN_BYTES,d5
 move.l d5,Overlay.After(a6)
 move.l records.Span.Address(a1),d5
 cmp.l d1,d5
 bhs.w right
 move.l d5,Overlay.Left+records.Span.Address(a6)
 move.l records.Span.Offset(a1),Overlay.Left+records.Span.Offset(a6)
 move.l d1,d7
 sub.l d5,d7
 move.l d7,Overlay.Left+records.Span.Bytes(a6)
right
 cmp.l Overlay.End(a6),d0
 bls.w next
 move.l Overlay.End(a6),d5
 addq.l #1,d5 ; old end is greater, so the increment cannot wrap
 move.l d5,Overlay.Right+records.Span.Address(a6)
 sub.l records.Span.Address(a1),d5
 add.l records.Span.Offset(a1),d5
 bcs.w bad
 move.l d5,Overlay.Right+records.Span.Offset(a6)
 sub.l Overlay.End(a6),d0
 move.l d0,Overlay.Right+records.Span.Bytes(a6)
next
 adda.w #records.SPAN_BYTES,a1
 addi.l #records.SPAN_BYTES,d6
 bra.w scan
sized
 move.l Overlay.After(a6),d0
 sub.l Overlay.Start(a6),d0
 move.l d4,d7
 sub.l d0,d7
 addi.l #records.SPAN_BYTES,d7
 tst.l Overlay.Left+records.Span.Bytes(a6)
 beq.w rightSize
 addi.l #records.SPAN_BYTES,d7
rightSize
 tst.l Overlay.Right+records.Span.Bytes(a6)
 beq.w reserve
 addi.l #records.SPAN_BYTES,d7
reserve
 move.l d7,Overlay.Required(a6)
 move.l d7,d0
 movea.l a2,a0
 jsr memory.reserve
 bne.w bad
 ; Shift the untouched suffix in the appropriate memmove direction. Old
 ; first/last overlap rows were already retained in local fragment packets.
 movea.l memory.Block.Pointer(a2),a0
 movea.l a0,a1
 adda.l Overlay.After(a6),a0
 move.l d4,d6
 sub.l Overlay.After(a6),d6
 move.l d7,d5
 sub.l d6,d5
 adda.l d5,a1
 cmpa.l a0,a1
 beq.w publish
 blo.w shiftForward
 adda.l d6,a0
 adda.l d6,a1
 lsr.l #2,d6
backward
 tst.l d6
 beq.w publish
 move.l -(a0),-(a1)
 subq.l #1,d6
 bra.w backward
shiftForward
 lsr.l #2,d6
forward
 tst.l d6
 beq.w publish
 move.l (a0)+,(a1)+
 subq.l #1,d6
 bra.w forward
publish
 movea.l memory.Block.Pointer(a2),a1
 adda.l Overlay.Start(a6),a1
 tst.l Overlay.Left+records.Span.Bytes(a6)
 beq.w incoming
 lea Overlay.Left(a6),a0
 bsr.w copyRow
incoming
 move.l d1,records.Span.Address(a1)
 move.l d2,records.Span.Offset(a1)
 move.l d3,records.Span.Bytes(a1)
 adda.w #records.SPAN_BYTES,a1
 tst.l Overlay.Right+records.Span.Bytes(a6)
 beq.w coalesce
 lea Overlay.Right(a6),a0
 bsr.w copyRow
coalesce
 ; Only the general path walks the image; terminal ordinary emissions take
 ; append's O(1) coalescing route. Compact in place, without another allocation.
 movea.l memory.Block.Pointer(a2),a0
 movea.l a0,a1
 move.l Overlay.Required(a6),d6
 moveq #0,d7
compact
 tst.l d6
 beq.w complete
 tst.l d7
 beq.w keep
 lea -records.SPAN_BYTES(a1),a3
 move.l records.Span.Address(a3),d5
 add.l records.Span.Bytes(a3),d5
 bcs.w keep ; a span ending at $ffffffff has no successor
 cmp.l records.Span.Address(a0),d5
 bne.w keep
 move.l records.Span.Offset(a3),d5
 add.l records.Span.Bytes(a3),d5
 cmp.l records.Span.Offset(a0),d5
 bne.w keep
 move.l records.Span.Bytes(a0),d5
 add.l d5,records.Span.Bytes(a3)
 adda.w #records.SPAN_BYTES,a0
 bra.w advanced
keep
 bsr.w copyRow
 addi.l #records.SPAN_BYTES,d7
advanced
 subi.l #records.SPAN_BYTES,d6
 bra.w compact
complete
 move.l d7,memory.Block.Used(a2)
 moveq #0,d0
 rts
bad
 moveq #1,d0
 rts
 .bend ; replace
; A0=row,A1=destination; advances both by one complete numeric row.
copyRow .block
 move.l (a0)+,(a1)+
 move.l (a0)+,(a1)+
 move.l (a0)+,(a1)+
 rts
 .bend ; copyRow
