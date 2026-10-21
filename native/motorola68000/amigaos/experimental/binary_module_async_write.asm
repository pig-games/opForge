; Experimental single-packet writer. State and buffer are caller-owned RAM.
; Requires DOS/Exec V36+. No task, process or file ownership is transferred.
; Sources: https://developer.amigaos3.net/autodocs/dos.library/SendPkt.html
; https://developer.amigaos3.net/autodocs/include_h/dos/dosextens.h
; https://developer.amigaos3.net/autodocs/include_h/exec/ports.h
; https://developer.amigaos3.net/autodocs/include_h/exec/nodes.h
; https://developer.amigaos3.net/autodocs/include_h/exec/lists.h
; https://developer.amigaos3.net/autodocs/exec.library/CreateMsgPort.html
; https://developer.amigaos3.net/autodocs/exec.library/GetMsg.html
; https://developer.amigaos3.net/autodocs/exec.library/WaitPort.html
; NDK FD declarations (including DOS's private vector slots):
; http://amigadev.elowar.com/read/ADCD_2.1/Includes_and_Autodocs_2._guide/node0584.html
; http://amigadev.elowar.com/read/ADCD_2.1/Includes_and_Autodocs_2._guide/node0586.html
; @opforge-owner: experimental.amigaos.binary_module_async_write
	.module experimental.amigaos.binary_module_async_write
	.cpu 68020
	.include "memory_telemetry.i"
	.priv
; Classic NDK layouts use two-byte structure alignment. Message is 20 bytes;
; its following DosPacket is therefore longword aligned in our aligned State.
Message	.struct
Successor	.long ?
Predecessor	.long ?
Type	.byte ?
Priority	.byte ?
Name	.long ?
ReplyPort	.long ?
Length	.word ?
	.endstruct
MESSAGE_BYTES = Message.Length+2
Packet	.struct
Link	.long ?
Port	.long ?
Type	.long ?
Result	.long ?
Error	.long ?
Argument1	.long ?
Argument2	.long ?
Argument3	.long ?
Argument4	.long ?
Argument5	.long ?
Argument6	.long ?
Argument7	.long ?
	.endstruct
PACKET_BYTES = Packet.Argument7+4
FileHandle	.struct
Link	.long ?
Interactive	.long ?
Handler	.long ?
Buffer	.long ?
Position	.long ?
End	.long ?
Function1	.long ?
Function2	.long ?
Function3	.long ?
Argument1	.long ?
	.endstruct
; NDK fd/exec_lib.fd and fd/dos_lib.fd; six-byte vectors.
EXEC_GET_MSG = -372
EXEC_WAIT_PORT = -384
EXEC_CREATE_MSG_PORT = -666
EXEC_DELETE_MSG_PORT = -672
DOS_SEND_PKT = -246
ACTION_WRITE = 87
NT_MESSAGE = 5
; sizeof(struct MsgPort): Node14 + flags/signal2 + task4 + List14.
PORT_BYTES = 34
ERROR_NO_FREE_STORE = 103
	.pub
OK = 0
INVALID = 1
PENDING = 2
LIMIT = 65536
State	.struct
Dos	.long ?
Port	.long ?
Handler	.long ?
Argument1	.long ?
Buffer	.long ?
Remaining	.long ?
Pending	.long ?
Failure	.long ?
Error	.long ?
Written	.long ?
Unexpected	.long ?
Message	.res MESSAGE_BYTES
Packet	.res PACKET_BYTES
	.endstruct
STATE_BYTES = State.Packet+PACKET_BYTES
	.section code, kind=code
; A0=zero or successfully idle, longword-aligned State; A1=DOS base;
; D1=opened BPTR; A2=stable buffer; D2=1..LIMIT. D0/CCR=PENDING/INVALID;
; all other registers preserved. Only direct Open/Read/Write/Seek handles:
; buffered DOS APIs, NIL:, concurrent access to this handle or State forbidden.
; The creating task must call every operation and stay alive through release.
; PENDING leases State, DOS base, open handle and the ENTIRE buffer until poll
; or wait becomes terminal. Do not move, modify, free, close or reuse them.
; An overlapping begin rejects without changing the existing operation.
; Other failures stick through poll/wait/begin until explicit release.
; State.Unexpected retains the last foreign message pointer for diagnostics;
; such a protocol violation never ends our outstanding packet's lease.
begin	.block
	movem.l d1-d3/a0-a3/a5-a6, -(sp)
	movea.l a0, a5
	tst.l State.Pending(a5)
	bne.w rejected
	tst.l State.Failure(a5)
	bne.w rejected
	move.l a1, d0
	beq.w bad
	tst.l d1
	beq.w bad
	cmpi.l #$3fffffff, d1
	bhi.w bad
	tst.l d2
	beq.w bad
	cmpi.l #LIMIT, d2
	bhi.w bad
	move.l a2, d0
	beq.w bad
	add.l d2, d0
	bcs.w bad
	lsl.l #2, d1
	movea.l d1, a3
	tst.l FileHandle.Handler(a3)
	beq.w bad
	tst.l State.Port(a5)
	beq.w allocate
	cmpa.l State.Dos(a5), a1
	bne.w bad
	bra.w ready
allocate
	move.l a1, State.Dos(a5)
	movea.l 4.w, a6
	jsr EXEC_CREATE_MSG_PORT(a6)
	tst.l d0
	beq.w noPort
	move.l d0, State.Port(a5)
	.MEMORY_ALLOC #PORT_BYTES
ready
	move.l FileHandle.Handler(a3), State.Handler(a5)
	move.l FileHandle.Argument1(a3), State.Argument1(a5)
	move.l a2, State.Buffer(a5)
	move.l d2, State.Remaining(a5)
	clr.l State.Written(a5)
	clr.l State.Error(a5)
	clr.l State.Unexpected(a5)
	bsr.w send
	moveq #PENDING, d0
	bra.w done
noPort
	move.l #ERROR_NO_FREE_STORE, State.Error(a5)
bad
	move.l #1, State.Failure(a5)
rejected
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d3/a0-a3/a5-a6
	tst.l d0
	rts
	.bend  ; begin

; A0=State. D0/CCR=PENDING/OK/INVALID; all other registers preserved.
; Nonblocking: consumes at most one reply. A positive short write submits
; its remainder before returning PENDING. Zero/negative/oversized replies
; terminate with sticky INVALID; Error retains dp_Res2, Written the prefix.
; Terminal status ends the lease even on failure; the caller must discard an
; incomplete output. An idle zero State returns OK without allocating anything.
; Foreign messages mark failure but retain the lease until our packet returns.
poll	.block
	movem.l d1-d3/a0-a1/a5-a6, -(sp)
	movea.l a0, a5
	bsr.w collect
	movem.l (sp)+, d1-d3/a0-a1/a5-a6
	tst.l d0
	rts
	.bend  ; poll

; A0=State. D0/CCR=OK/INVALID; all other registers preserved.
; Waits until the complete buffer or a failure has replied, including retries.
; There is no timeout/cancellation: abandoning a live packet is unsafe.
wait	.block
	movem.l d1-d3/a0-a1/a5-a6, -(sp)
	movea.l a0, a5
again
	bsr.w collect
	cmpi.l #PENDING, d0
	bne.w done
	movea.l State.Port(a5), a0
	movea.l 4.w, a6
	jsr EXEC_WAIT_PORT(a6)
	bra.w again
done
	movem.l (sp)+, d1-d3/a0-a1/a5-a6
	tst.l d0
	rts
	.bend  ; wait

; A0=State. D0/CCR=OK/INVALID; all other registers preserved.
; Drains any outstanding write BEFORE deleting the private port. Never closes
; the caller's handle or frees its buffer. Retains terminal Error/Written for
; inspection, clears sticky Failure and ownership, and permits a fresh begin.
; Call before the task exits, DOS closes, or State storage goes out of scope.
release	.block
	movem.l d1-d3/a0-a1/a5-a6, -(sp)
	movea.l a0, a5
	bsr.w wait
	move.l d0, d3
	move.l State.Port(a5), d0
	beq.w reset
	; Do not delete a nonempty port, even after a handler violated our protocol.
drain
	movea.l State.Port(a5), a0
	movea.l 4.w, a6
	jsr EXEC_GET_MSG(a6)
	tst.l d0
	beq.w dispose
	move.l d0, State.Unexpected(a5)
	moveq #INVALID, d3
	bra.w drain
dispose
	move.l State.Port(a5), d0
	movea.l d0, a0
	movea.l 4.w, a6
	jsr EXEC_DELETE_MSG_PORT(a6)
	.MEMORY_FREE #PORT_BYTES
reset
	clr.l State.Port(a5)
	clr.l State.Dos(a5)
	clr.l State.Handler(a5)
	clr.l State.Argument1(a5)
	clr.l State.Buffer(a5)
	clr.l State.Remaining(a5)
	clr.l State.Pending(a5)
	clr.l State.Failure(a5)
	move.l d3, d0
	movem.l (sp)+, d1-d3/a0-a1/a5-a6
	tst.l d0
	rts
	.bend  ; release
	.priv

; A5=idle State with a nonempty remaining range. No returned status.
; Clobbers D0-D3/A0-A1/A6. The packet cannot be touched again until GetMsg.
send	.block
	lea State.Message(a5), a0
	lea State.Packet(a5), a1
	clr.l Message.Successor(a0)
	clr.l Message.Predecessor(a0)
	move.b #NT_MESSAGE, Message.Type(a0)
	clr.b Message.Priority(a0)
	move.l a1, Message.Name(a0)
	move.l State.Port(a5), Message.ReplyPort(a0)
	move.w #MESSAGE_BYTES+PACKET_BYTES, Message.Length(a0)
	move.l a0, Packet.Link(a1)
	move.l State.Port(a5), Packet.Port(a1)
	move.l #ACTION_WRITE, Packet.Type(a1)
	clr.l Packet.Result(a1)
	clr.l Packet.Error(a1)
	move.l State.Argument1(a5), Packet.Argument1(a1)
	move.l State.Buffer(a5), Packet.Argument2(a1)
	move.l State.Remaining(a5), Packet.Argument3(a1)
	clr.l Packet.Argument4(a1)
	clr.l Packet.Argument5(a1)
	clr.l Packet.Argument6(a1)
	clr.l Packet.Argument7(a1)
	move.l #1, State.Pending(a5)
	move.l a1, d1
	move.l State.Handler(a5), d2
	move.l State.Port(a5), d3
	.MEMORY_COUNTER_INC Packets
	movea.l State.Dos(a5), a6
	jsr DOS_SEND_PKT(a6)
	rts
	.bend  ; send

; A5=State. D0=status; clobbers D1-D3/A0-A1/A6. Private port is never
; published. Only GetMsg of our exact message is the packet ownership fence.
collect	.block
	tst.l State.Pending(a5)
	beq.w status
	movea.l State.Port(a5), a0
	movea.l 4.w, a6
	jsr EXEC_GET_MSG(a6)
	tst.l d0
	beq.w notReady
	lea State.Message(a5), a1
	cmp.l a1, d0
	beq.w returned
	move.l d0, State.Unexpected(a5)
	move.l #1, State.Failure(a5)
	bra.w notReady
returned
	clr.l State.Pending(a5)
	lea State.Packet(a5), a1
	move.l Packet.Error(a1), State.Error(a5)
	move.l Packet.Result(a1), d0
	ble.w failed
	cmp.l State.Remaining(a5), d0
	bhi.w failed
	add.l d0, State.Written(a5)
	.MEMORY_COUNTER_ADD WrittenBytes, d0
	add.l d0, State.Buffer(a5)
	sub.l d0, State.Remaining(a5)
	beq.w completed
	tst.l State.Failure(a5)
	bne.w completed
	.MEMORY_COUNTER_INC ShortWrites
	bsr.w send
notReady
	moveq #PENDING, d0
	rts
failed
	move.l #1, State.Failure(a5)
completed
	clr.l State.Buffer(a5)
status
	tst.l State.Failure(a5)
	bne.w bad
	moveq #OK, d0
	rts
bad
	moveq #INVALID, d0
	rts
	.bend  ; collect
	.endsection
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
	.section data, kind=data
	.pub
Packets
	.long 0
WrittenBytes
	.long 0
ShortWrites
	.long 0
	.endsection
.endif
.endif
	.endmodule
