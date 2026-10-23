; Convert PRVM-selected capture spellings to the common packed argument form.
; Captures are owned normalized facets, never slices of the original source.
; TKVM and the packed writer own lexical grammar and numeric name binding.
; @opforge-owner: opforge.assembler.language.statement_arguments
	.module opforge.assembler.language.statement_arguments
	.cpu 68020
	.use opforge.vm.prvm.statement_abi as abi
	.use opforge.assembler.source.records as writer
	.use opforge.assembler.language.templates as templates
	.use opforge.vm.tkvm.runtime as tokenizer
	.pub
Frame	.struct
Captures	.long ?
CaptureCount	.long ?
CaptureBytes	.long ?
Views	.long ?
ViewCount	.long ?
ViewBytes	.long ?
Arena	.long ?
ArenaBytes	.long ?
Program	.long ?
ProgramBytes	.long ?
Tokens	.long ?
TokenCapacity	.long ?  ; number of original TKVM rows
Lexemes	.long ?
LexemeCapacity	.long ?
Writer	.long ?  ; exclusive live binding/text-policy frame, restored on every exit
Arguments	.long ?  ; template ExternalArgument rows, caller-owned scratch
ArgumentCapacity	.long ?
Packed	.long ?  ; aggregate packed arguments, without writer record headers
PackedCapacity	.long ?
Full	.long ?  ; capture spellings joined with comma/space, for .@
FullCapacity	.long ?
	.endstruct
FRAME_BYTES = Frame.FullCapacity+4
	.priv
TOKEN_LIMIT = 64; current owned lexical/facet acquisition bound
VIEW_LIMIT = 2*TOKEN_LIMIT; decimal fallback may split each original view
FULL_LIMIT = 251; current common template full-list spelling capacity
Local	.struct
Index	.long ?
PackedUsed	.long ?
FullUsed	.long ?
TextUsed	.long ?
WriterSaved	.long ?
Spelling	.long ?
SpellingBytes	.long ?
Write	.res writer.FRAME_BYTES
Record	.res writer.MAX_LINE
	.endstruct
LOCAL_BYTES = Local.Record+writer.MAX_LINE
	.section code, kind=code
	.pub
; A0=Frame. D0/CCR=status,D1=full spelling bytes,D2=packed bytes; others
; preserved. The live writer frame is exclusively borrowed while callbacks run,
; then restored on success or failure. Input and scratch output spans must not
; overlap. Scratch outputs
; are meaningful only on success. The caller then
; publishes ExternalCall atomically through the existing template owner.
; Every capture is retokenized, including strings: str "1+2" substitutes the
; decoded expression 1+2, not the original string token. Duplicate formal names
; retain distinct positional captures; named-last precedence belongs to templates.
run	.block
	movem.l d3-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l a0, a5
	movea.l sp, a6
	clr.l Local.Index(a6)
	clr.l Local.PackedUsed(a6)
	clr.l Local.FullUsed(a6)
	clr.l Local.TextUsed(a6)
	clr.l Local.WriterSaved(a6)
	tst.l Frame.CaptureBytes(a5)
	bmi.w invalid
	tst.l Frame.ViewBytes(a5)
	bmi.w invalid
	tst.l Frame.ArenaBytes(a5)
	bmi.w invalid
	tst.l Frame.ArgumentCapacity(a5)
	bmi.w invalid
	tst.l Frame.PackedCapacity(a5)
	bmi.w invalid
	tst.l Frame.FullCapacity(a5)
	bmi.w invalid
	cmpi.l #templates.PARAM_LIMIT, Frame.CaptureCount(a5)
	bhi.w invalid
	move.l Frame.CaptureCount(a5), d0
	mulu.l #abi.CAPTURE_BYTES, d0
	cmp.l Frame.CaptureBytes(a5), d0
	bhi.w invalid
	move.l Frame.CaptureCount(a5), d0
	mulu.l #templates.EXTERNAL_ARGUMENT_BYTES, d0
	cmp.l Frame.ArgumentCapacity(a5), d0
	bhi.w overflow
	move.l Frame.ViewCount(a5), d0
	cmpi.l #VIEW_LIMIT, d0
	bhi.w invalid
	mulu.l #abi.TOKEN_BYTES, d0
	cmp.l Frame.ViewBytes(a5), d0
	bhi.w invalid
	tst.l Frame.CaptureCount(a5)
	beq.w success
	tst.l Frame.Captures(a5)
	beq.w invalid
	tst.l Frame.Views(a5)
	beq.w invalid
	tst.l Frame.Arena(a5)
	beq.w invalid
	tst.l Frame.Program(a5)
	beq.w invalid
	tst.l Frame.ProgramBytes(a5)
	ble.w invalid
	tst.l Frame.Tokens(a5)
	beq.w invalid
	tst.l Frame.Lexemes(a5)
	beq.w invalid
	tst.l Frame.Writer(a5)
	beq.w invalid
	tst.l Frame.Arguments(a5)
	beq.w invalid
	tst.l Frame.Packed(a5)
	beq.w invalid
	tst.l Frame.Full(a5)
	beq.w invalid
	cmpi.l #TOKEN_LIMIT, Frame.TokenCapacity(a5)
	bhi.w invalid
	tst.l Frame.TokenCapacity(a5)
	ble.w invalid
	tst.l Frame.LexemeCapacity(a5)
	ble.w invalid
	movea.l Frame.Writer(a5), a0
	lea Local.Write(a6), a1
	moveq #writer.FRAME_BYTES/4-1, d0
copyFrame
	move.l (a0)+, (a1)+
	dbra d0, copyFrame
	move.l #1, Local.WriterSaved(a6)
next
	move.l Local.Index(a6), d0
	cmp.l Frame.CaptureCount(a5), d0
	bhs.w success
	mulu.l #abi.CAPTURE_BYTES, d0
	movea.l Frame.Captures(a5), a0
	adda.l d0, a0
	move.l abi.Capture.TokenIndex(a0), d0
	cmp.l Frame.ViewCount(a5), d0
	bhs.w invalid
	mulu.l #abi.TOKEN_BYTES, d0
	movea.l Frame.Views(a5), a0
	adda.l d0, a0
	move.l abi.TokenView.LiteralOffset(a0), d0
	cmp.l Frame.ArenaBytes(a5), d0
	bhi.w invalid
	move.l Frame.ArenaBytes(a5), d1
	sub.l d0, d1
	move.l abi.TokenView.LiteralBytes(a0), d2
	cmp.l d1, d2
	bhi.w invalid
	movea.l Frame.Arena(a5), a0
	adda.l d0, a0
	move.l a0, Local.Spelling(a6)
	move.l d2, Local.SpellingBytes(a6)
	add.l Local.TextUsed(a6), d2
	cmpi.l #templates.TEXT_LIMIT, d2
	bhi.w overflow
	move.l d2, Local.TextUsed(a6)
	bsr.w fullSpelling
	bne.w failed
	movea.l Local.Spelling(a6), a0
	move.l Local.SpellingBytes(a6), d0
	movea.l Frame.Tokens(a5), a1
	move.l Frame.TokenCapacity(a5), d1
	movea.l Frame.Lexemes(a5), a2
	move.l Frame.LexemeCapacity(a5), d2
	movea.l Frame.Program(a5), a3
	move.l Frame.ProgramBytes(a5), d3
	jsr tokenizer.tkvmRun68000
	bne.w lexicalFailure
	movea.l Frame.Writer(a5), a4
	move.l Frame.Tokens(a5), writer.Frame.Tokens(a4)
	move.l Frame.TokenCapacity(a5), d0
	mulu.l #tokenizer.TOKEN_RECORD_SIZE, d0
	move.l d0, writer.Frame.TokenBytes(a4)
	move.l d1, writer.Frame.Count(a4)
	move.l Frame.Lexemes(a5), writer.Frame.Lexemes(a4)
	move.l d3, writer.Frame.LexemeBytes(a4)
	lea Local.Record(a6), a0
	move.l a0, writer.Frame.Output(a4)
	move.l #writer.MAX_LINE, writer.Frame.Capacity(a4)
	move.l Local.Spelling(a6), writer.Frame.Source(a4)
	move.l Local.SpellingBytes(a6), writer.Frame.SourceBytes(a4)
	clr.l writer.Frame.PackedMap(a4)
	movea.l a4, a0
	jsr writer.writeValues
	bne.w lexicalFailure
	moveq #0, d3
	move.b Local.Record+writer.RecordHeader.Length(a6), d3
	addq.l #1, d3
	subq.l #writer.RECORD_HEADER_BYTES, d3
	move.l Local.PackedUsed(a6), d0
	add.l d3, d0
	cmpi.l #templates.ARG_LIMIT, d0
	bhi.w overflow
	cmp.l Frame.PackedCapacity(a5), d0
	bhi.w overflow
	move.l Local.Index(a6), d1
	mulu.l #templates.EXTERNAL_ARGUMENT_BYTES, d1
	movea.l Frame.Arguments(a5), a0
	adda.l d1, a0
	movea.l Frame.Packed(a5), a1
	adda.l Local.PackedUsed(a6), a1
	move.l a1, templates.ExternalArgument.Packed(a0)
	move.w d3, templates.ExternalArgument.PackedBytes(a0)
	move.l Local.Spelling(a6), templates.ExternalArgument.Spelling(a0)
	move.l Local.SpellingBytes(a6), d1
	move.w d1, templates.ExternalArgument.SpellingBytes(a0)
	lea Local.Record+writer.RECORD_HEADER_BYTES(a6), a0
	move.l d0, Local.PackedUsed(a6)
	tst.l d3
	beq.w packedReady
copyPacked
	move.b (a0)+, (a1)+
	subq.l #1, d3
	bne.w copyPacked
packedReady
	addq.l #1, Local.Index(a6)
	bra.w next
success
	move.l Local.FullUsed(a6), d1
	move.l Local.PackedUsed(a6), d2
	moveq #abi.OK, d0
	bra.w done
lexicalFailure
	moveq #abi.INVALID_TOKEN, d0
	bra.w failed
invalid
	moveq #abi.INVALID_ARGUMENT, d0
	bra.w failed
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
failed
	moveq #0, d1
	moveq #0, d2
done
	tst.l Local.WriterSaved(a6)
	beq.w restored
	movem.l d0-d2, -(sp)
	movea.l Frame.Writer(a5), a0
	lea Local.Write(a6), a1
	moveq #writer.FRAME_BYTES/4-1, d3
restoreFrame
	move.l (a1)+, (a0)+
	dbra d3, restoreFrame
	movem.l (sp)+, d0-d2
restored
	adda.w #LOCAL_BYTES, sp
	tst.l d0
	movem.l (sp)+, d3-d7/a0-a6
	rts
	.bend  ; run
	.priv
; A5=request,A6=locals. Build the Rust capture-list spelling, not source operands.
; D0/CCR=status; preserves other registers. Bounds checked before copying.
fullSpelling	.block
	movem.l d1-d3/a0-a1, -(sp)
	move.l Local.FullUsed(a6), d1
	move.l d1, d2
	tst.l Local.Index(a6)
	beq.w extent
	addq.l #2, d2
extent
	add.l Local.SpellingBytes(a6), d2
	cmpi.l #FULL_LIMIT, d2
	bhi.w bad
	cmp.l Frame.FullCapacity(a5), d2
	bhi.w bad
	movea.l Frame.Full(a5), a1
	adda.l d1, a1
	tst.l Local.Index(a6)
	beq.w text
	move.b #',', (a1)+
	move.b #' ', (a1)+
text
	movea.l Local.Spelling(a6), a0
	move.l Local.SpellingBytes(a6), d3
	beq.w ready
copy
	move.b (a0)+, (a1)+
	subq.l #1, d3
	bne.w copy
ready
	move.l d2, Local.FullUsed(a6)
	moveq #abi.OK, d0
	bra.w done
bad
	moveq #abi.OUTPUT_OVERFLOW, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; fullSpelling
	.endsection
	.endmodule
