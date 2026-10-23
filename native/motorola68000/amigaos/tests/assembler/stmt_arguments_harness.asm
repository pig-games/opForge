; Selected-capture adapter proof; shared TKVM feeds the common packed writer.
	.module main
	.cpu 68020
	.use opforge.assembler.language.statement_arguments as arguments
	.use opforge.assembler.language.templates as templates
	.use opforge.assembler.source.records as writer
	.use opforge.vm.prvm.statement_abi as abi
	.priv
FixtureHeader	.struct
Views	.long ?
Captures	.long ?
ArenaBytes	.long ?
ProgramBytes	.long ?
	.endstruct
FIXTURE_HEADER_BYTES = FixtureHeader.ProgramBytes+4
Result	.struct
Status	.long ?
FullBytes	.long ?
PackedBytes	.long ?
	.endstruct
RESULT_BYTES = Result.PackedBytes+4
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
	.pub
	.section entry, kind=code
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
	move.l FixtureHeader.Views(a0), d6
	move.l FixtureHeader.Captures(a0), d5
	move.l FixtureHeader.ArenaBytes(a0), d4
	move.l FixtureHeader.ProgramBytes(a0), d7
	move.l d6, Request+arguments.Frame.ViewCount
	move.l d5, Request+arguments.Frame.CaptureCount
	move.l d4, Request+arguments.Frame.ArenaBytes
	move.l d7, Request+arguments.Frame.ProgramBytes
	move.l d6, d0
	mulu.l #abi.TOKEN_BYTES, d0
	move.l d0, Request+arguments.Frame.ViewBytes
	bsr.w take
	bne.w bad
	move.l a0, Request+arguments.Frame.Views
	move.l d5, d0
	mulu.l #abi.CAPTURE_BYTES, d0
	move.l d0, Request+arguments.Frame.CaptureBytes
	bsr.w take
	bne.w bad
	move.l a0, Request+arguments.Frame.Captures
	move.l d4, d0
	bsr.w padded
	bne.w bad
	move.l a0, Request+arguments.Frame.Arena
	move.l d7, d0
	bsr.w take
	bne.w bad
	move.l a0, Request+arguments.Frame.Program
	move.l #FixtureEnd, d0
	cmp.l Cursor, d0
	bne.w bad
	move.l #Tokens, Request+arguments.Frame.Tokens
	move.l #64, Request+arguments.Frame.TokenCapacity
	move.l #Lexemes, Request+arguments.Frame.Lexemes
	move.l #2048, Request+arguments.Frame.LexemeCapacity
	move.l #Writer, Request+arguments.Frame.Writer
	move.l #bindSource, Writer+writer.Frame.Binder
	move.l #Writer, Writer+writer.Frame.Context
	move.l #ArgumentRows, Request+arguments.Frame.Arguments
	move.l #templates.PARAM_LIMIT*templates.EXTERNAL_ARGUMENT_BYTES, Request+arguments.Frame.ArgumentCapacity
	move.l #Packed, Request+arguments.Frame.Packed
	move.l #templates.ARG_LIMIT, Request+arguments.Frame.PackedCapacity
	move.l #Full, Request+arguments.Frame.Full
	move.l #251, Request+arguments.Frame.FullCapacity
	lea Request, a0
	jsr arguments.run
	bne.w bad
	tst.l Writer+writer.Frame.Output
	bne.w bad
	tst.l Writer+writer.Frame.Count
	bne.w bad
	tst.w Writer+writer.Frame.HeadToken
	bne.w bad
	move.l d0, ResultData+Result.Status
	move.l d1, ResultData+Result.FullBytes
	move.l d2, ResultData+Result.PackedBytes
	lea ResultData, a0
	moveq #RESULT_BYTES, d0
	bsr.w writeSpan
	bne.w bad
	lea ArgumentRows, a3
rows
	tst.l d5
	beq.w payload
	move.w templates.ExternalArgument.PackedBytes(a3), Sizes
	move.w templates.ExternalArgument.SpellingBytes(a3), Sizes+2
	lea Sizes, a0
	moveq #4, d0
	bsr.w writeSpan
	bne.w bad
	adda.w #templates.EXTERNAL_ARGUMENT_BYTES, a3
	subq.l #1, d5
	bra.w rows
payload
	lea Packed, a0
	move.l ResultData+Result.PackedBytes, d0
	bsr.w writeSpan
	bne.w bad
	lea Full, a0
	move.l ResultData+Result.FullBytes, d0
	bsr.w writeSpan
	bne.w bad
	; A tiny output bound must fail without claiming successful byte counts.
	clr.l Request+arguments.Frame.PackedCapacity
	lea Request, a0
	jsr arguments.run
	cmpi.l #abi.OUTPUT_OVERFLOW, d0
	bne.w bad
	tst.l d1
	bne.w bad
	tst.l d2
	bne.w bad
	; Empty invocation needs no lexical buffers and remains successful.
	clr.l Request+arguments.Frame.CaptureCount
	lea Request, a0
	jsr arguments.run
	bne.w bad
	tst.l d1
	bne.w bad
	tst.l d2
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #20, d0
	rts
	.bend  ; cases
; Fixture binding supplies a stable source identity, never CPU-specific names.
bindSource	.block
	; The callback must see the live borrowed frame, rather than a stale
	; outer source-line frame. This also pins the no-head argument role.
	movea.l a1, a0
	moveq #0, d0
	move.w writer.Frame.HeadToken(a0), d0
	cmp.l writer.Frame.Count(a0), d0
	bne.w bad
	tst.l writer.Frame.Output(a0)
	beq.w bad
	moveq #100, d1
	moveq #0, d2
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; bindSource
; D0=nonnegative bytes. A0=bounded fixture span,D0=status/CCR; others preserved.
take	.block
	movem.l d1, -(sp)
	tst.l d0
	bmi.w invalid
	move.l #FixtureEnd, d1
	sub.l Cursor, d1
	cmp.l d1, d0
	bhi.w invalid
	movea.l Cursor, a0
	add.l d0, Cursor
	moveq #0, d0
	bra.w done
invalid
	moveq #1, d0
done
	tst.l d0
	movem.l (sp)+, d1
	rts
	.bend  ; take
; D0=raw bytes; consume its even-byte-padded fixture block. Same outputs as take.
padded	.block
	tst.l d0
	bmi.w invalid
	btst #0, d0
	beq.w ready
	addq.l #1, d0
ready
	bsr.w take
	rts
invalid
	moveq #1, d0
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
OutputName	.byte "Work:stmt-argument-results.bin", 0
	.align 4
Fixture	.incbin "stmt-argument-fixture.bin"
FixtureEnd
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
Cursor	.res long, 1
Request	.res byte, arguments.FRAME_BYTES
Writer	.res byte, writer.FRAME_BYTES
ResultData	.res byte, RESULT_BYTES
Sizes	.res word, 2
ArgumentRows	.res byte, templates.PARAM_LIMIT*templates.EXTERNAL_ARGUMENT_BYTES
Packed	.res byte, templates.ARG_LIMIT
Full	.res byte, 252
Tokens	.res byte, 64*20
Lexemes	.res byte, 2048
	.endsection
	.output "stmt-arguments.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
