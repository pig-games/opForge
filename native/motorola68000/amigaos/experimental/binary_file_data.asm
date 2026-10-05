; Stream a binary asset into ordinary packed data records during preparation.
; No file handle, path or source text survives into assembly replay.
; @opforge-owner: experimental.amigaos.binary_file_data

	.module experimental.amigaos.binary_file_data
	.cpu 68020
	.use experimental.amigaos.binary_source as source
	.use experimental.amigaos.binary_memory as memory
	.include "memory_telemetry.i"
	.pub
Frame	.struct
Dos	.long ?
Handle	.long ?  ; borrowed; caller closes on both success and failure
Record	.long ?
PrefixBytes	.long ?  ; normalized label prefix selected by PRVM, zero or five
ByteName	.word ?
Reserved	.word ?
Callback	.long ?
Context	.long ?
	.endstruct
FRAME_BYTES = Frame.Context+4
IO_BYTES = 4096
DOS_READ = -42
	.priv
State	.struct
Prefix	.res 12  ; nine possible header/label bytes; keep stack storage aligned
Record	.res 256
Buffer	.res IO_BYTES
	.endstruct
STATE_BYTES = State.Buffer+IO_BYTES
BLOCK_BYTES = memory.Block.Used+4
	.section code, kind=code
	.pub
; A0=Frame with an open asset and PRVM-validated source record. Callback receives
; A0=packed record, D0=bytes, A1=Context, returns D0/CCR=status; preserves others.
; Every byte is published as a .byte string, including NUL/high-bit/quote bytes.
; Empty assets publish a label-only record if labeled, otherwise no record.
; D0/CCR=status; preserves other registers. Owns one temporary STATE_BYTES block;
; only its small allocation descriptor and saved registers occupy the stack.
stream	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	suba.l #BLOCK_BYTES, sp
	movea.l sp, a0
	clr.l memory.Block.Pointer(a0)
	clr.l memory.Block.Capacity(a0)
	clr.l memory.Block.Used(a0)
	move.l #STATE_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	movea.l memory.Block.Pointer(a0), a5
	move.l Frame.PrefixBytes(a6), d7
	beq.w prefixReady
	cmpi.l #5, d7
	bne.w bad
prefixReady
	movea.l Frame.Record(a6), a0
	lea State.Prefix(a5), a1
	move.l d7, d0
	addq.l #4, d0
copyPrefix
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyPrefix
	moveq #0, d6  ; at least one data byte has been read
read
	movea.l Frame.Dos(a6), a0
	move.l a6, -(sp)
	movea.l a0, a6
	movea.l (sp), a0
	move.l Frame.Handle(a0), d1
	lea State.Buffer(a5), a0
	move.l a0, d2
	move.l #IO_BYTES, d3
	.MEMORY_INPUT_READ
	jsr DOS_READ(a6)
	movea.l (sp)+, a6
	tst.l d0
	bmi.w bad
	beq.w eof
	cmpi.l #IO_BYTES, d0
	bhi.w bad
	move.l d0, d5
	lea State.Buffer(a5), a4
	moveq #1, d6
chunk
	move.l #source.MAX_LINE-11, d4
	sub.l d7, d4
	cmp.l d5, d4
	bls.w sizeReady
	move.l d5, d4
sizeReady
	lea State.Record(a5), a3
	lea State.Prefix(a5), a0
	move.l d7, d0
	addq.l #4, d0
header
	move.b (a0)+, (a3)+
	subq.l #1, d0
	bne.w header
	; Only indentation remains: scope ownership was already resolved by PRVM
	; and the shared frontend before the callback opened this asset.
	andi.b #source.FLAG_INDENT, State.Record+1(a5)
	move.b #7, (a3)+
	move.b #0, (a3)+
	move.w Frame.ByteName(a6), (a3)+
	clr.b (a3)+
	move.b #3, (a3)+
	move.b d4, (a3)+
	move.l d4, d0
payload
	move.b (a4)+, (a3)+
	subq.l #1, d0
	bne.w payload
	move.l d4, d0
	add.l d7, d0
	addi.l #11, d0
	bsr.w publish
	bne.w bad
	moveq #0, d7  ; the label is defined only by the first data record
	sub.l d4, d5
	bne.w chunk
	bra.w read
eof
	tst.l d6
	bne.w good
	tst.l d7
	beq.w good
	lea State.Prefix(a5), a0
	lea State.Record(a5), a1
	moveq #8, d0
emptyLabel
	move.b (a0)+, (a1)+
	dbra d0, emptyLabel
	andi.b #source.FLAG_INDENT, State.Record+1(a5)
	moveq #9, d0
	bsr.w publish
	bne.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movea.l sp, a0
	jsr memory.release
	adda.l #BLOCK_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; stream
	.priv
; D0=complete record bytes; assigns the bounded length byte before publication.
publish	.block
	move.l d0, d1
	subq.l #1, d1
	move.b d1, State.Record(a5)
	lea State.Record(a5), a0
	movea.l Frame.Context(a6), a1
	movea.l Frame.Callback(a6), a2
	jsr (a2)
	tst.l d0
	rts
	.bend  ; publish
	.endsection
	.endmodule
