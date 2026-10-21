; Mock DOS packet protocol proof with real Exec ports, then one real DOS write.
; Mock cases do NOT prove filesystem behavior. Rust generates the output oracle.
; @opforge-owner: tests.amigaos.module_async_write_probe
	.module tests.amigaos.module_async_write_probe
	.cpu 68020
	.use experimental.amigaos.binary_module_async_write as writer
	.priv
EXEC_PUT_MSG = -366
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
MODE_NEWFILE = 1006
MESSAGE_BYTES = 20
MOCK_ARGUMENT = $12345678
MockPacket	.struct
Link	.long ?
Port	.long ?
Type	.long ?
Result	.long ?
Error	.long ?
Argument1	.long ?
Buffer	.long ?
Length	.long ?
	.endstruct
	.section entry, kind=code
	.pub
; D0=0 only after mock ownership/error cases and a complete real DOS file close.
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	moveq #20, d7
	movea.l 4.w, a6
	lea DosName, a1
	moveq #36, d0
	jsr EXEC_OPEN_LIBRARY(a6)
	move.l d0, DosBase
	beq.w cleanup
	bsr.w mockCases
	tst.l d0
	bne.w cleanup
	movea.l DosBase, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, File
	beq.w cleanup
	move.l d0, d1
	movea.l DosBase, a1
	lea State, a0
	lea Payload, a2
	move.l #PayloadEnd-Payload, d2
	jsr writer.begin
	cmpi.l #writer.PENDING, d0
	bne.w cleanup
	jsr writer.wait
	tst.l d0
	bne.w cleanup
	cmpi.l #PayloadEnd-Payload, writer.State.Written(a0)
	bne.w cleanup
	moveq #0, d7
cleanup
	; The lease always ends before either Close or CloseLibrary, even on failure.
	lea State, a0
	jsr writer.release
	tst.l d0
	beq.w closeFile
	moveq #20, d7
closeFile
	move.l File, d1
	beq.w closeLibrary
	movea.l DosBase, a6
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	moveq #20, d7
closeLibrary
	move.l DosBase, d0
	beq.w done
	movea.l d0, a1
	movea.l 4.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
done
	move.l d7, d0
	movem.l (sp)+, d2-d7/a2-a6
	tst.l d0
	rts
	.bend  ; start
	.priv

mockCases	.block
	lea State, a0
	jsr writer.poll
	tst.l d0
	bne.w bad
	jsr writer.release
	tst.l d0
	bne.w bad
	jsr writer.release
	tst.l d0
	bne.w bad
	; Ten bytes in chunks 3+3+3+1; rejected reentry must retain the first lease.
	move.l #1, Mode
	bsr.w beginMock
	cmpi.l #writer.PENDING, d0
	bne.w bad
	lea Payload+1, a2
	jsr writer.begin
	cmpi.l #writer.INVALID, d0
	bne.w bad
	lea Payload, a3
	cmpa.l writer.State.Buffer(a0), a3
	bne.w bad
	cmpi.l #10, writer.State.Remaining(a0)
	bne.w bad
	tst.l writer.State.Failure(a0)
	bne.w bad
	jsr writer.wait
	tst.l d0
	bne.w bad
	cmpi.l #10, writer.State.Written(a0)
	bne.w bad
	cmpi.l #4, Sends
	bne.w bad
	tst.l ProtocolFailure
	bne.w bad
	jsr writer.poll
	tst.l d0
	bne.w bad
	jsr writer.release
	tst.l d0
	bne.w bad
	; release itself must drain all four chunks before deleting its reply port.
	bsr.w beginMock
	cmpi.l #writer.PENDING, d0
	bne.w bad
	jsr writer.release
	tst.l d0
	bne.w bad
	cmpi.l #10, writer.State.Written(a0)
	bne.w bad
	tst.l writer.State.Port(a0)
	bne.w bad
	; Zero, -1 and larger-than-requested replies fail without an endless retry.
	moveq #2, d6
failures
	move.l d6, Mode
	bsr.w beginMock
	cmpi.l #writer.PENDING, d0
	bne.w bad
	jsr writer.wait
	cmpi.l #writer.INVALID, d0
	bne.w bad
	cmpi.l #222, writer.State.Error(a0)
	bne.w bad
	tst.l writer.State.Pending(a0)
	bne.w bad
	jsr writer.poll
	cmpi.l #writer.INVALID, d0
	bne.w bad
	jsr writer.begin
	cmpi.l #writer.INVALID, d0
	bne.w bad
	cmpi.l #1, Sends
	bne.w bad
	jsr writer.release
	cmpi.l #writer.INVALID, d0
	bne.w bad
	jsr writer.release
	tst.l d0
	bne.w bad
	addq.l #1, d6
	cmpi.l #4, d6
	bls.w failures
	; A foreign reply must not release the buffer while our packet is held.
	clr.l Mode
	move.l #1, Hold
	bsr.w beginMock
	cmpi.l #writer.PENDING, d0
	bne.w bad
	movea.l writer.State.Port(a0), a0
	lea Foreign, a1
	movea.l 4.w, a6
	jsr EXEC_PUT_MSG(a6)
	lea State, a0
	jsr writer.poll
	cmpi.l #writer.PENDING, d0
	bne.w bad
	cmpi.l #1, writer.State.Pending(a0)
	bne.w bad
	lea Payload, a3
	cmpa.l writer.State.Buffer(a0), a3
	bne.w bad
	lea Foreign, a3
	cmpa.l writer.State.Unexpected(a0), a3
	bne.w bad
	clr.l Hold
	move.l LastPacket, d1
	move.l LastPort, d3
	bsr.w submit
	lea State, a0
	jsr writer.wait
	cmpi.l #writer.INVALID, d0
	bne.w bad
	jsr writer.release
	cmpi.l #writer.INVALID, d0
	bne.w bad
	jsr writer.poll
	tst.l d0
	bne.w bad
	tst.l ProtocolFailure
	bne.w bad
	moveq #0, d0
	rts
bad
	; All mock sends except Hold immediately queue their reply. A failed probe
	; must also return the held packet before its caller drains on cleanup.
	tst.l Hold
	beq.w failed
	clr.l Hold
	lea State, a0
	tst.l writer.State.Pending(a0)
	beq.w failed
	move.l LastPacket, d1
	move.l LastPort, d3
	bsr.w submit
failed
	moveq #20, d0
	rts
	.bend  ; mockCases

; Select mock DOS, a valid BPTR and ten stable caller bytes; clear send count.
beginMock	.block
	clr.l Sends
	clr.l ExpectedOffset
	lea State, a0
	lea FakeDos, a1
	move.l #MockFile, d1
	lsr.l #2, d1
	lea Payload, a2
	moveq #10, d2
	jsr writer.begin
	rts
	.bend  ; beginMock

; Exactly one mock vector: SendPkt at -246, including both private DOS slots.
FakeSendVector
	jmp mockSend
	.fill byte, 240, 0
FakeDos
mockSend	.block
	movem.l d2-d3/a2/a6, -(sp)
	addq.l #1, Sends
	move.l d1, LastPacket
	move.l d3, LastPort
	movea.l d1, a2
	cmpi.l #87, MockPacket.Type(a2)
	bne.w bad
	cmpi.l #MOCK_ARGUMENT, MockPacket.Argument1(a2)
	bne.w bad
	lea FakeDos, a0
	cmp.l a0, d2
	bne.w bad
	move.l MockPacket.Buffer(a2), d0
	move.l #Payload, d2
	add.l ExpectedOffset, d2
	cmp.l d2, d0
	bne.w bad
	move.l MockPacket.Length(a2), d0
	add.l ExpectedOffset, d0
	cmpi.l #10, d0
	bne.w bad
	tst.l Hold
	bne.w done
	bsr.w submit
	bra.w done
bad
	move.l #1, ProtocolFailure
	bsr.w submit
done
	movem.l (sp)+, d2-d3/a2/a6
	rts
	.bend  ; mockSend

; D1=our DosPacket,D3=reply port. Model handler completion via real Exec PutMsg.
; Clobbers D0-D1/A0-A1/A6. No serialized bytes contain any of these pointers.
submit	.block
	movea.l d1, a1
	move.l MockPacket.Length(a1), d0
	clr.l MockPacket.Error(a1)
	cmpi.l #1, Mode
	bne.w zero
	cmpi.l #3, d0
	bls.w result
	moveq #3, d0
	bra.w result
zero
	cmpi.l #2, Mode
	bne.w negative
	moveq #0, d0
	bra.w error
negative
	cmpi.l #3, Mode
	bne.w oversized
	moveq #-1, d0
	bra.w error
oversized
	cmpi.l #4, Mode
	bne.w result
	addq.l #1, d0
error
	move.l #222, MockPacket.Error(a1)
result
	move.l d0, MockPacket.Result(a1)
	tst.l d0
	ble.w reply
	cmp.l MockPacket.Length(a1), d0
	bhi.w reply
	add.l d0, ExpectedOffset
reply
	movea.l MockPacket.Link(a1), a1
	movea.l d3, a0
	movea.l 4.w, a6
	jsr EXEC_PUT_MSG(a6)
	rts
	.bend  ; submit
	.endsection
	.section data, kind=data
	.align 4
MockFile
	.long 0, 0, FakeDos, 0, 0, 0, 0, 0, 0, MOCK_ARGUMENT
DosName	.byte "dos.library", 0
OutputName	.byte "Work:output.bin", 0
	.align 4
Payload
	.incbin "module_async_payload.bin"
PayloadEnd
	.endsection
	.section bss, kind=bss
	.align 4
State	.res byte, writer.STATE_BYTES
Foreign	.res byte, MESSAGE_BYTES
DosBase	.res long, 1
File	.res long, 1
Mode	.res long, 1
Hold	.res long, 1
Sends	.res long, 1
LastPacket	.res long, 1
LastPort	.res long, 1
ExpectedOffset	.res long, 1
ProtocolFailure	.res long, 1
	.endsection
	.output "module-async-write-probe.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
