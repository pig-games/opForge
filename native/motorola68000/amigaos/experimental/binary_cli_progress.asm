; Shell progress owns no semantic state or heap allocation.
	.module experimental.amigaos.binary_cli_progress
	.cpu 68020
	.pub
PREPARING = 0
PROCESSING = 1
REUSED = 2
LINKING = 3
FINISHED = 4
FAILED = 5
ASSEMBLED = 6
DEFERRED = 7
LINKED = 8
PREPARED = 9
LABEL_BYTES = 256
BUFFER_BYTES = 384
DateStamp	.struct
Days	.long ?
Minutes	.long ?
Ticks	.long ?
	.endstruct
Frame	.struct
DosBase	.long ?
Enabled	.word ?
Reserved	.word ?
Started	.long ?
Active	.word ?
ActiveReserved	.word ?
Label	.res LABEL_BYTES
Stamp	.res DateStamp.Ticks+4
Buffer	.res BUFFER_BYTES
Decimal	.res 12
	.endstruct
FRAME_BYTES = Frame.Decimal+12
	.priv
DATE_STAMP = -192
FIND_TASK = -294
WRITE = -48
PR_CES = 224
PR_COS = 160
TICKS_SECOND = 50
TICKS_MINUTE = 3000
MINUTES_DAY = 1440
	.section code, kind=code
	.pub
; Begin one visible phase. A0=caller-owned Frame, A1=NUL label, D0=action.
; Caller initializes DosBase/Enabled; begin replaces the active label/timestamp.
; Disabled or unavailable DOS is a no-op. All registers and incoming CCR kept.
; Writes immediately to process ErrorOutput, falling back to Output.
begin	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a5
	tst.w Frame.Enabled(a5)
	beq.w done
	tst.l Frame.DosBase(a5)
	beq.w done
	move.l d0, d7
	lea Frame.Label(a5), a2
	move.w #LABEL_BYTES-2, d2
copyLabel
	move.b (a1)+, (a2)+
	beq.w copied
	dbra d2, copyLabel
	clr.b (a2)
copied
	bsr.w tick
	move.l d0, Frame.Started(a5)
	move.w #1, Frame.Active(a5)
	bsr.w heading
	move.b #10, (a3)+
	bsr.w emit
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; begin

; Finish the active phase. A0=Frame, D0=action (normally FINISHED/FAILED).
; Emits elapsed seconds to hundredths at the AmigaDOS 50 Hz resolution.
; Clears Active; inactive/disabled frames emit nothing. All registers/CCR kept.
finish	.block
	move.w ccr, -(sp)
	movem.l d0-d7/a0-a6, -(sp)
	movea.l a0, a5
	tst.w Frame.Active(a5)
	beq.w done
	clr.w Frame.Active(a5)
	tst.w Frame.Enabled(a5)
	beq.w done
	tst.l Frame.DosBase(a5)
	beq.w done
	move.l d0, d7
	bsr.w tick
	sub.l Frame.Started(a5), d0
	move.l d0, d6
	bsr.w heading
	move.b #' ', (a3)+
	move.b #'[', (a3)+
	move.l d6, d0
	moveq #TICKS_SECOND, d1
	divu.l d1, d0
	move.l d0, d2
	mulu.l #TICKS_SECOND, d2
	sub.l d2, d6
	bsr.w decimal
	move.b #'.', (a3)+
	add.l d6, d6
	move.l d6, d0
	moveq #10, d1
	divu.l d1, d0
	move.l d0, d2
	addi.b #'0', d0
	move.b d0, (a3)+
	mulu.l #10, d2
	sub.l d2, d6
	addi.b #'0', d6
	move.b d6, (a3)+
	move.b #'s', (a3)+
	move.b #']', (a3)+
	move.b #10, (a3)+
	bsr.w emit
done
	movem.l (sp)+, d0-d7/a0-a6
	move.w (sp)+, ccr
	rts
	.bend  ; finish
	.priv
; A5=Frame. D0=wrapping 50 Hz tick; clobbers D1-D2/A6/CCR.
tick	.block
	lea Frame.Stamp(a5), a0
	move.l a0, d1
	movea.l Frame.DosBase(a5), a6
	jsr DATE_STAMP(a6)
	lea Frame.Stamp(a5), a0
	move.l DateStamp.Days(a0), d2
	mulu.l #MINUTES_DAY, d2
	add.l DateStamp.Minutes(a0), d2
	mulu.l #TICKS_MINUTE, d2
	move.l DateStamp.Ticks(a0), d0
	add.l d2, d0
	rts
	.bend  ; tick

; A5=Frame,D7=action. A3=buffer end; clobbers D0/A1-A2/CCR.
heading	.block
	lea Frame.Buffer(a5), a3
	cmpi.l #PREPARED, d7
	bls.w known
	moveq #FAILED, d7
known
	lea Actions, a1
	lsl.l #2, d7
	movea.l 0(a1, d7.l), a1
	bsr.w append
	lea Frame.Label(a5), a1
	bsr.w append
	rts
	.bend  ; heading

; A1=NUL text,A3=destination cursor; clobbers D0/A1/A3/CCR.
append	.block
next
	move.b (a1)+, d0
	beq.w done
	move.b d0, (a3)+
	bra.w next
done
	rts
	.bend  ; append

; D0=unsigned value,A3=destination cursor,A5=Frame.
; Clobbers D0-D3/A1-A3/CCR. At most ten digits.
decimal	.block
	lea Frame.Decimal+12(a5), a1
	moveq #10, d1
digit
	move.l d0, d2
	divu.l d1, d0
	move.l d0, d3
	mulu.l #10, d3
	sub.l d3, d2
	addi.b #'0', d2
	move.b d2, -(a1)
	tst.l d0
	bne.w digit
copy
	move.b (a1)+, (a3)+
	lea Frame.Decimal+12(a5), a2
	cmpa.l a2, a1
	blo.w copy
	rts
	.bend  ; decimal

; A5=Frame,A3=buffer end. Write failures do not affect assembly status.
; Clobbers D0-D3/A0-A1/A6/CCR; preserves A3/A5.
emit	.block
	suba.l a1, a1
	movea.l 4.w, a6
	jsr FIND_TASK(a6)
	tst.l d0
	beq.w done
	movea.l d0, a0
	move.l PR_CES(a0), d1
	bne.w handle
	move.l PR_COS(a0), d1
	beq.w done
handle
	lea Frame.Buffer(a5), a0
	move.l a0, d2
	move.l a3, d3
	sub.l d2, d3
	movea.l Frame.DosBase(a5), a6
	jsr WRITE(a6)
done
	rts
	.bend  ; emit
	.endsection
	.section data, kind=data
Actions	.long TextPreparing, TextProcessing, TextReused, TextLinking, TextFinished, TextFailed, TextAssembled, TextDeferred, TextLinked, TextPrepared
TextPreparing	.byte "   Preparing ", 0
TextProcessing	.byte "  Processing ", 0
TextReused	.byte "      Reused ", 0
TextLinking	.byte "     Linking ", 0
TextFinished	.byte "    Finished ", 0
TextFailed	.byte "      Failed ", 0
TextAssembled	.byte "   Assembled ", 0
TextDeferred	.byte "    Deferred ", 0
TextLinked	.byte "      Linked ", 0
TextPrepared	.byte "    Prepared ", 0
	.endsection
	.endmodule
