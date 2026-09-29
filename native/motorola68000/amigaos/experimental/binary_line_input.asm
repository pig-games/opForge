; Buffered physical source-line collection, independent of lowering and paths.
; @opforge-owner: experimental.amigaos.binary_line_input
	.module experimental.amigaos.binary_line_input
	.cpu 68020
	.include "memory_telemetry.i"
	.pub

LINE = 0
EOF = 1
ERROR = 2
LIMIT = 3
DOS_READ = -42

State	.struct
Cursor	.long ?
End	.long ?
Buffer	.long ?
Capacity	.long ?
Handle	.long ?
Dos	.long ?
	.endstruct
SCRATCH_BYTES = State.Dos+4

	.section code, kind=code

; Collect one physical line from a buffered DOS source. LF is consumed but not
; copied; CR remains in the output for the caller to handle. The caller owns all
; storage and initializes Cursor/End to an empty span before the first call.
; Inputs: A0=State, A1=output, D0=output capacity in bytes.
; Outputs: D0=LINE on LF, EOF on end of input, ERROR on read/invalid state, or
; LIMIT after consuming the first non-LF byte beyond capacity. D1=copied bytes;
; D2=source bytes consumed, including LF or the offending overflow byte.
; An EOF with D1>0 returns that final partial line once. State.Cursor is advanced
; on every return, including read failure, and refill replaces the exhausted span.
; Other registers are preserved; CCR reflects D0 on return.
next	.block
	movem.l d3-d7/a0-a6, -(sp)
	movea.l a0, a2
	movea.l a1, a3
	move.l d0, d3
	moveq #0, d1
	moveq #0, d2
	move.l a2, d0
	beq.w invalidState
	tst.l d3
	bmi.w invalidState
	tst.l d3
	beq.w outputReady
	move.l a3, d0
	beq.w invalidState
outputReady
	movea.l State.Cursor(a2), a4
	movea.l State.End(a2), a5
	cmpa.l a5, a4
	bhi.w readError
scan
	cmpa.l a5, a4
	bhs.w refill
	moveq #0, d0
	move.b (a4)+, d0
	addq.l #1, d2
	cmpi.b #10, d0
	beq.w lineReady
	tst.l d3
	beq.w lineLimit
	move.b d0, (a3)+
	addq.l #1, d1
	subq.l #1, d3
	bra.w scan
refill
	move.l a4, State.Cursor(a2)
	move.l State.Buffer(a2), d0
	beq.w readError
	move.l State.Capacity(a2), d0
	ble.w readError
	move.l State.Handle(a2), d0
	beq.w readError
	move.l State.Dos(a2), d0
	beq.w readError
	movem.l d1-d3/a0-a1, -(sp)
	move.l State.Handle(a2), d1
	move.l State.Buffer(a2), d2
	move.l State.Capacity(a2), d3
	movea.l State.Dos(a2), a6
	.MEMORY_INPUT_READ
	jsr DOS_READ(a6)
	move.l d0, d4
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d4
	bmi.w readError
	beq.w endOfFile
	cmp.l State.Capacity(a2), d4
	bhi.w readError
	movea.l State.Buffer(a2), a4
	movea.l a4, a5
	adda.l d4, a5
	move.l a4, State.Cursor(a2)
	move.l a5, State.End(a2)
	bra.w scan
lineReady
	moveq #LINE, d0
	bra.w done
endOfFile
	moveq #EOF, d0
	bra.w done
lineLimit
	moveq #LIMIT, d0
	bra.w done
readError
	moveq #ERROR, d0
done
	move.l a4, State.Cursor(a2)
	bra.w finish
invalidState
	moveq #ERROR, d0
finish
	tst.l d0
	movem.l (sp)+, d3-d7/a0-a6
	rts
	.bend  ; next

	.endsection
	.endmodule
