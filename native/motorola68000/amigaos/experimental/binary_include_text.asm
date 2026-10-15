; Build-local immutable raw include bytes; every occurrence reruns all semantics.
; @opforge-owner: experimental.amigaos.binary_include_text
	.module experimental.amigaos.binary_include_text
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.include "memory_telemetry.i"
	.pub
BLOCK_BYTES = memory.Block.Used+4
PATH_BYTES = 256
ROW_LIMIT = 64
PAYLOAD_LIMIT = 131072
FILE_LIMIT = 32768
DOS_READ = -42
DOS_SEEK = -66
SEEK_BEGIN = -1
SEEK_END = 1
READY = 0
BYPASS = 1
ERROR = 2
State	.struct
Rows	.res BLOCK_BYTES
Count	.long ?
PayloadBytes	.long ?
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
Requests	.long ?
Hits	.long ?
HitBytes	.long ?
ReadBytes	.long ?
ReadCalls	.long ?
Bypasses	.long ?
.endif
.endif
.endstruct
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
STATE_BYTES = State.Bypasses+4
.else
STATE_BYTES = State.PayloadBytes+4
.endif
.else
STATE_BYTES = State.PayloadBytes+4
.endif
Frame	.struct
State	.long ?
Dos	.long ?
Handle	.long ?
Path	.long ?
Data	.long ?
Bytes	.long ?
.endstruct
FRAME_BYTES = Frame.Bytes+4
	.priv
Row	.struct
Path	.res PATH_BYTES
Storage	.res BLOCK_BYTES
.endstruct
ROW_BYTES = Row.Storage+BLOCK_BYTES
Local	.struct
Payload	.res BLOCK_BYTES
Probe	.byte ?
Padding	.byte ?
.endstruct
LOCAL_BYTES = Local.Padding+1
	.pub
	.section code, kind=code

; A0=Frame, State initially zeroed. The caller has resolved/authorized the path,
; checked include cycles/depth, and opened a fresh DOS Handle at offset zero.
; Inputs are immutable for this build. Hits share raw bytes only, never bound IDs,
; source origins or interpreted/configured data. Empty READY has Data=Bytes=0.
; READY borrows stable payload until release; BYPASS leaves the handle at zero.
; ERROR may have consumed input. D0/CCR=status; all other registers preserved.
acquire	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a3
	clr.l Frame.Data(a3)
	clr.l Frame.Bytes(a3)
	move.l Frame.State(a3), d0
	beq.w bad
	movea.l d0, a4
	.MEMORY_COUNTER_INC State.Requests(a4)
	move.l Frame.Dos(a3), d0
	beq.w bad
	movea.l d0, a6
	tst.l Frame.Handle(a3)
	beq.w bad
	move.l Frame.Path(a3), d0
	beq.w bad
	movea.l d0, a0
	moveq #0, d6
pathLength
	cmpi.l #PATH_BYTES, d6
	bhs.w bad
	addq.l #1, d6
	tst.b (a0)+
	bne.w pathLength
	cmpi.l #1, d6
	beq.w bad
	move.l State.Count(a4), d4
	cmpi.l #ROW_LIMIT, d4
	bhi.w bad
	movea.l State.Rows+memory.Block.Pointer(a4), a2
	tst.l d4
	beq.w miss
	move.l a2, d0
	beq.w bad
find
	movea.l Frame.Path(a3), a0
	lea Row.Path(a2), a1
	bsr.w samePath
	beq.w hit
	adda.w #ROW_BYTES, a2
	subq.l #1, d4
	bne.w find
miss
	cmpi.l #ROW_LIMIT, State.Count(a4)
	bhs.w streamFallback
	; Seek returns the previous position. END must return the initial zero;
	; BEGIN then returns file size and restores the original reader position.
	move.l Frame.Handle(a3), d1
	moveq #0, d2
	moveq #SEEK_END, d3
	jsr DOS_SEEK(a6)
	tst.l d0
	bmi.w streamFallback  ; failed Seek leaves this fresh handle's position unchanged
	move.l d0, d7
	move.l Frame.Handle(a3), d1
	moveq #0, d2
	moveq #SEEK_BEGIN, d3
	jsr DOS_SEEK(a6)
	tst.l d0
	bmi.w bad  ; successful END followed by failed restore is never a bypass
	tst.l d7
	bne.w bad
	move.l d0, d5
	cmpi.l #FILE_LIMIT, d5
	bhi.w streamFallback
	move.l State.PayloadBytes(a4), d0
	add.l d5, d0
	bcs.w streamFallback
	cmpi.l #PAYLOAD_LIMIT, d0
	bhi.w streamFallback
	move.l State.Count(a4), d0
	addq.l #1, d0
	mulu.w #ROW_BYTES, d0
	lea State.Rows(a4), a0
	jsr memory.reserveExact
	bne.w streamFallback
	suba.w #LOCAL_BYTES, sp
	movea.l sp, a5
	lea Local.Payload(a5), a0
	clr.l memory.Block.Pointer(a0)
	clr.l memory.Block.Capacity(a0)
	clr.l memory.Block.Used(a0)
	move.l d5, d0
	jsr memory.reserveExact
	bne.w allocationBypass
	moveq #0, d4
read
	cmp.l d5, d4
	beq.w verifyEof
	move.l Frame.Handle(a3), d1
	move.l Local.Payload+memory.Block.Pointer(a5), d2
	add.l d4, d2
	move.l d5, d3
	sub.l d4, d3
	.MEMORY_COUNTER_INC State.ReadCalls(a4)
	.MEMORY_INPUT_READ
	jsr DOS_READ(a6)
	tst.l d0
	ble.w readBad
	.MEMORY_COUNTER_ADD State.ReadBytes(a4), d0
	add.l d0, d4
	bcs.w readBad
	cmp.l d5, d4
	bhi.w readBad
	bra.w read
verifyEof
	move.l Frame.Handle(a3), d1
	lea Local.Probe(a5), a0
	move.l a0, d2
	moveq #1, d3
	.MEMORY_COUNTER_INC State.ReadCalls(a4)
	.MEMORY_INPUT_READ
	jsr DOS_READ(a6)
	tst.l d0
	bmi.w readBad
	beq.w publish
	.MEMORY_COUNTER_ADD State.ReadBytes(a4), d0
	bra.w readBad
publish
	move.l State.Count(a4), d0
	mulu.w #ROW_BYTES, d0
	movea.l State.Rows+memory.Block.Pointer(a4), a2
	adda.l d0, a2
	movea.l Frame.Path(a3), a0
	lea Row.Path(a2), a1
	move.l d6, d0
copyPath
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyPath
	move.l Local.Payload+memory.Block.Pointer(a5), Row.Storage+memory.Block.Pointer(a2)
	move.l Local.Payload+memory.Block.Capacity(a5), Row.Storage+memory.Block.Capacity(a2)
	move.l d5, Row.Storage+memory.Block.Used(a2)
	add.l d5, State.PayloadBytes(a4)
	addq.l #1, State.Count(a4)
	move.l State.Count(a4), d0
	mulu.w #ROW_BYTES, d0
	move.l d0, State.Rows+memory.Block.Used(a4)
	adda.w #LOCAL_BYTES, sp
	bra.w payloadReady
allocationBypass
	adda.w #LOCAL_BYTES, sp
	bra.w streamFallback
readBad
	lea Local.Payload(a5), a0
	jsr memory.release
	adda.w #LOCAL_BYTES, sp
	bra.w bad
hit
	.MEMORY_COUNTER_INC State.Hits(a4)
	.MEMORY_COUNTER_ADD State.HitBytes(a4), Row.Storage+memory.Block.Used(a2)
payloadReady
	move.l Row.Storage+memory.Block.Pointer(a2), Frame.Data(a3)
	move.l Row.Storage+memory.Block.Used(a2), Frame.Bytes(a3)
	moveq #READY, d0
	bra.w done
streamFallback
	.MEMORY_COUNTER_INC State.Bypasses(a4)
	moveq #BYPASS, d0
	bra.w done
bad
	moveq #ERROR, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; acquire

; A0=State. No live reader may retain payload views. All registers preserved;
; releases each separately owned payload then the movable directory, zeroing state.
release	.block
	movem.l d0-d2/a0-a3, -(sp)
	movea.l a0, a3
	move.l State.Count(a3), d2
	movea.l State.Rows+memory.Block.Pointer(a3), a2
next
	tst.l d2
	beq.w rows
	lea Row.Storage(a2), a0
	jsr memory.release
	adda.w #ROW_BYTES, a2
	subq.l #1, d2
	bra.w next
rows
	lea State.Rows(a3), a0
	jsr memory.release
	movea.l a3, a0
	move.w #STATE_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	movem.l (sp)+, d0-d2/a0-a3
	rts
	.bend  ; release
	.priv

; A0/A1=bounded NUL paths. D0/CCR=0 equal,1 distinct. Other registers preserved.
samePath	.block
	movem.l d1-d2/a0-a1, -(sp)
compare
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #'A', d1
	blo.w firstReady
	cmpi.b #'Z', d1
	bhi.w firstReady
	addi.b #32, d1
firstReady
	moveq #0, d2
	move.b (a1)+, d2
	cmpi.b #'A', d2
	blo.w secondReady
	cmpi.b #'Z', d2
	bhi.w secondReady
	addi.b #32, d2
secondReady
	cmp.b d1, d2
	bne.w distinct
	tst.b d1
	bne.w compare
	moveq #0, d0
	bra.w done
distinct
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend  ; samePath
	.endsection
	.endmodule
