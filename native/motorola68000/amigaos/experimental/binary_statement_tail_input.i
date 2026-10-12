; Tail-owner input/output separation. Runs before resizing reusable storage.
; A5=Frame,A4=State,A3=validated source Snapshot; no borrowed block may alias it.
disjointInputs .block
 movem.l d1-d7/a0-a2,-(sp)
 movea.l Frame.Output(a5),a0
 moveq #session.SNAPSHOT_BYTES,d0
 bsr.w ownedSpan
 bne.w done
 movea.l a5,a0
 moveq #FRAME_BYTES,d0
 bsr.w inputSpan
 bne.w done
 movea.l a3,a0
 moveq #session.SNAPSHOT_BYTES,d0
 bsr.w inputSpan
 bne.w done
 movea.l Frame.Route(a5),a0
 moveq #abi.ROUTE_BYTES,d0
 bsr.w inputSpan
 bne.w done
 move.l session.Snapshot.Count(a3),d0
 cmpi.l #TOKEN_LIMIT,d0
 bhi.w invalid
 mulu.w #abi.TOKEN_BYTES,d0
 movea.l session.Snapshot.Views(a3),a0
 bsr.w inputSpan
 bne.w done
 movea.l session.Snapshot.Arena(a3),a0
 move.l session.Snapshot.ArenaBytes(a3),d0
 bsr.w inputSpan
 bne.w done
 movea.l Frame.Package(a5),a0
 move.l #package.HEADER_BYTES,d0
 bsr.w inputSpan
 bne.w done
 move.l package.Header.Bytes(a0),d0
 bsr.w inputSpan
 bra.w done
invalid
 moveq #abi.INVALID_ARGUMENT,d0
done
 tst.l d0
 movem.l (sp)+,d1-d7/a0-a2
 rts
 .bend ; disjointInputs
; A0/D0=input. Check output packet and every existing dynamic block.
inputSpan .block
 movem.l d1/d4/a1,-(sp)
 move.l d0,d4
 jsr inputs.span
 bne.w done
 movea.l Frame.Output(a5),a1
 moveq #session.SNAPSHOT_BYTES,d1
 move.l d4,d0
 jsr inputs.separate
 bne.w done
 move.l d4,d0
 bsr.w ownedSpan
done
 tst.l d0
 movem.l (sp)+,d1/d4/a1
 rts
 .bend ; inputSpan
; A0/D0=caller span. State and all old capacities are inaccessible to inputs.
ownedSpan .block
 movem.l d1-d4/a1-a2,-(sp)
 move.l d0,d4
 jsr inputs.span
 bne.w done
 movea.l a4,a1
 move.l #STATE_BYTES,d1
 move.l d4,d0
 jsr inputs.separate
 bne.w done
 lea OwnedBlocks,a2
 moveq #2,d3
loop
 moveq #0,d2
 move.w (a2)+,d2
 movea.l memory.Block.Pointer(a4,d2.l),a1
 move.l memory.Block.Capacity(a4,d2.l),d1
 move.l d4,d0
 jsr inputs.separate
 bne.w done
 dbra d3,loop
 moveq #abi.OK,d0
done
 tst.l d0
 movem.l (sp)+,d1-d4/a1-a2
 rts
 .bend ; ownedSpan
; This table describes owned Blocks; it carries no source classification policy.
.align 2
OwnedBlocks .word State.Raw,State.Merged,State.SuffixSession+session.State.Buffers
