; Fixed-buffer flat BIN generator component proof, without file/source parsing.
.module main
.cpu 68020
.use opforge.assembler.output.flat as flat
.use opforge.assembler.output.records as records
.priv
Header	.struct
Fault	.long ?
Capacity	.long ?
SpanBytes	.long ?
DataBytes	.long ?
InitOnly	.long ?
Negative	.long ?
RangeSet	.long ?
RangeStart	.long ?
RangeEnd	.long ?
Fill	.long ?
.endstruct
HEADER_BYTES = Header.Fill+4
FixtureHeader	.struct
CaseCount	.long ?
.endstruct
FIXTURE_HEADER_BYTES = FixtureHeader.CaseCount+4
Result	.struct
Status	.long ?
Value	.long ?
Extent	.long ?
ExtentHigh	.long ?
.endstruct
RESULT_PAIR_BYTES = Result.Value+4
RESULT_STATUS_BYTES = Result.Value
RESULT_BYTES = Result.ExtentHigh+4
BUFFER_BYTES = 64
TAG = $11223344
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
	.pub
	.section entry, kind=code
; No inputs. D0=0 on success or 20 on failure; CCR reflects D0.
; D2-D7/A2-A6 are preserved; other registers may be clobbered.
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	movea.l d0, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, OutputHandle
	beq.w closeLibrary
	jsr cases
	move.l d0, ReturnCode
	movea.l DosBase, a6
	move.l OutputHandle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	move.l #20, ReturnCode
closeLibrary
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	tst.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start
	.endsection

	.priv
	.section code, kind=code
cases	.block
	move.l #Fixture, Cursor
	moveq #FIXTURE_HEADER_BYTES, d0
	bsr.w take
	bne.w bad
	move.l FixtureHeader.CaseCount(a0), Count
	lea Count, a0
	moveq #4, d0
	bsr.w writeSpan
	bne.w bad
	lea Buffer, a0
	moveq #BUFFER_BYTES/4-1, d0
	bsr.w sentinel
	lea Frame, a0
	move.l #TAG, d1
	jsr flat.next
	cmpi.l #flat.INVALID, d0
	bne.w bad
	cmpi.l #TAG, d1
	bne.w bad
	cmpa.l #Frame, a0
	bne.w bad
	move.l d0, ResultData+Result.Status
	move.l d1, ResultData+Result.Value
	lea ResultData, a0
	moveq #RESULT_PAIR_BYTES, d0
	bsr.w writeSpan
	bne.w bad
	lea Buffer, a0
	moveq #BUFFER_BYTES, d0
	bsr.w writeSpan
	bne.w bad
nextCase
	tst.l Count
	beq.w valid
	moveq #HEADER_BYTES, d0
	bsr.w take
	bne.w bad
	move.l a0, Current
	move.l Header.DataBytes(a0), d0
	bsr.w padded
	bne.w bad
	move.l a0, Data
	movea.l Current, a0
	move.l Header.SpanBytes(a0), d0
	bsr.w padded
	bne.w bad
	move.l a0, Spans
	lea Frame, a0
	moveq #flat.FRAME_BYTES/4-1, d0
clearFrame
	clr.l (a0)+
	dbra d0, clearFrame
	lea Buffer, a0
	moveq #BUFFER_BYTES/4-1, d0
	bsr.w sentinel
	lea Frame, a0
	move.l #TAG, flat.Frame.Valid(a0)
	move.l Data, flat.Frame.Data(a0)
	moveq #8, d0
	move.l d0, flat.Frame.DataBytes(a0)
	move.l Spans, flat.Frame.Spans(a0)
	movea.l Current, a1
	move.l Header.SpanBytes(a1), flat.Frame.SpanBytes(a0)
	move.l #Buffer, flat.Frame.Buffer(a0)
	move.l Header.Capacity(a1), flat.Frame.Capacity(a0)
	move.w Header.RangeSet+2(a1), flat.Frame.RangeSet(a0)
	move.l Header.RangeStart(a1), flat.Frame.RangeStart(a0)
	move.l Header.RangeEnd(a1), flat.Frame.RangeEnd(a0)
	move.w Header.Fill+2(a1), flat.Frame.Fill(a0)
	move.l Header.Fault(a1), d0
	cmpi.l #1, d0
	bne.w spanWrap
	move.l #$fffffff0, Frame+flat.Frame.Data
	move.l #32, Frame+flat.Frame.DataBytes
spanWrap
	cmpi.l #2, d0
	bne.w nullBuffer
	move.l #$fffffff0, Frame+flat.Frame.Spans
	move.l #32, Frame+flat.Frame.SpanBytes
nullBuffer
	cmpi.l #3, d0
	bne.w frameBuffer
	clr.l Frame+flat.Frame.Buffer
frameBuffer
	cmpi.l #4, d0
	bne.w inputBuffer
	move.l #Frame+flat.Frame.Valid, Frame+flat.Frame.Buffer
inputBuffer
	cmpi.l #5, d0
	bne.w spansBuffer
	move.l Data, Frame+flat.Frame.Buffer
spansBuffer
	cmpi.l #6, d0
	bne.w initialize
	move.l Spans, Frame+flat.Frame.Buffer
initialize
	cmpi.l #7, d0
	bne.w pointersReady
	clr.l Frame+flat.Frame.Data
	clr.l Frame+flat.Frame.DataBytes
	clr.l Frame+flat.Frame.Spans
pointersReady
	lea Frame, a0
	jsr flat.init
	move.l d0, ResultData+Result.Status
	move.l d1, ResultData+Result.Value
	move.l Frame+flat.Frame.Extent, ResultData+Result.Extent
	move.l Frame+flat.Frame.ExtentHigh, ResultData+Result.ExtentHigh
	lea ResultData, a0
	moveq #RESULT_BYTES, d0
	bsr.w writeSpan
	bne.w bad
	movea.l Current, a0
	tst.l Header.Negative(a0)
	beq.w initialized
	move.l Frame+flat.Frame.Valid, ResultData+Result.Status
	lea ResultData, a0
	moveq #RESULT_STATUS_BYTES, d0
	bsr.w writeSpan
	bne.w bad
	lea Buffer, a0
	moveq #BUFFER_BYTES, d0
	bsr.w writeSpan
	bne.w bad
	movea.l Data, a0
	moveq #8, d0
	bsr.w writeSpan
	bne.w bad
	bra.w advanceCase
initialized
	tst.l Header.InitOnly(a0)
	bne.w advanceCase
chunks
	lea Frame, a0
	move.l #TAG, d1
	jsr flat.next
	move.l d0, ResultData+Result.Status
	cmpi.l #flat.END, d0
	beq.w endChunks
	tst.l d0
	bne.w bad
	tst.l d1
	ble.w bad
	cmpi.l #BUFFER_BYTES, d1
	bhi.w bad
	move.l d1, ResultData+Result.Value
	move.l a0, -(sp)
	lea ResultData, a0
	moveq #RESULT_PAIR_BYTES, d0
	bsr.w writeSpan
	movea.l (sp)+, a0
	bne.w bad
	move.l ResultData+Result.Value, d0
	bsr.w writeSpan
	bne.w bad
	bra.w chunks
endChunks
	cmpi.l #TAG, d1
	bne.w bad
	cmpa.l #Frame, a0
	bne.w bad
	clr.l ResultData+Result.Value
	lea ResultData, a0
	moveq #RESULT_PAIR_BYTES, d0
	bsr.w writeSpan
	bne.w bad
advanceCase
	subq.l #1, Count
	bra.w nextCase
valid
	moveq #0, d0
	rts
bad
	moveq #20, d0
	rts
	.bend  ; cases
; A0=Buffer,D0=longwords-1. Clobbers D0/A0, CCR unspecified.
sentinel	.block
	move.l #$a5a5a5a5, (a0)+
	dbra d0, sentinel
	rts
	.bend  ; sentinel
; Consume an exactly bounded fixture span. A0=bytes,D0/CCR=status.
take	.block
	move.l #FixtureEnd, d1
	sub.l Cursor, d1
	cmp.l d1, d0
	bhi.w bad
	movea.l Cursor, a0
	add.l d0, Cursor
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; take
padded	.block
	addq.l #3, d0
	andi.l #$fffffffc, d0
	bsr.w take
	rts
	.bend  ; padded
; A0/D0=output span. D0=status/CCR; others preserved. Exact partial-write loop.
writeSpan	.block
	movem.l d1-d5/a0-a6, -(sp)
	movea.l a0, a3
	move.l d0, d5
next
	tst.l d5
	beq.w valid
	movea.l DosBase, a6
	move.l OutputHandle, d1
	move.l a3, d2
	move.l d5, d3
	jsr DOS_WRITE(a6)
	tst.l d0
	ble.w fail
	cmp.l d5, d0
	bhi.w fail
	adda.l d0, a3
	sub.l d0, d5
	bra.w next
valid
	moveq #0, d0
	bra.w done
fail
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, d1-d5/a0-a6
	rts
	.bend  ; writeSpan
	.endsection
.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:flat-output-results.bin", 0
.align 4
Fixture	.incbin "flat-output-fixture.bin"
FixtureEnd
.endsection
.section bss, kind=bss
.align 4
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
Cursor	.res long, 1
Count	.res long, 1
Current	.res long, 1
Data	.res long, 1
Spans	.res long, 1
ResultData	.res byte, RESULT_BYTES
Frame	.res byte, flat.FRAME_BYTES
Buffer	.res byte, BUFFER_BYTES
.endsection
.output "flat-output.hunk", format=hunk, sections=entry, code, data, bss
.endmodule
