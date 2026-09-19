; Streaming line tokenization and numeric lowering. Scratch is caller-owned and
; bounded; no source text enters packed records.
; @opforge-owner: experimental.amigaos.binary_frontend
	.module experimental.amigaos.binary_frontend
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_source as writer
	.use tkvm.amigaos.runtime as tokenizer
	.use tkvm.amigaos.control as control
	.pub
Frame	.struct
Package	.long ?
Source	.long ?
SourceBytes	.long ?
Output	.long ?
Capacity	.long ?
Used	.long ?
NameCount	.long ?
Scratch	.long ?
	.endstruct
	.priv
ARENA_BYTES = 16384
SYMBOL_LIMIT = 512
PROGRAM = 0
PROGRAM_BYTES = 4
DICTIONARY = 8
DICTIONARY_COUNT = 12
PACKAGE_END = 16
LINE_NUMBER = 20
NEXT_ID = 24
SYMBOL_COUNT = 28
ARENA_USED = 32
LINE_FRAME = 36
TOKENS = 76
LEXEMES = TOKENS+64*20
ENTRIES = LEXEMES+1024
ARENA = ENTRIES+SYMBOL_LIMIT*8
	.pub
SCRATCH_BYTES = ARENA+ARENA_BYTES
	.priv
Entry	.struct
Name	.long ?
Length	.word ?
Id	.word ?
	.endstruct
	.section code, kind=code
	.pub
; Begin a streaming frontend session. A0=Frame with a readable package capsule
; and SCRATCH_BYTES of caller-owned aligned scratch. D0=0 success, 1 invalid.
; Resets symbols and source-line numbering. Preserves other registers; CCR=D0.
begin	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	clr.l Frame.Used(a5)
	clr.l Frame.NameCount(a5)
	movea.l Frame.Scratch(a5), a6
	move.l a6, d0
	beq.w failed
	move.l a6, d0
	andi.l #3, d0
	bne.w failed
	clr.l SYMBOL_COUNT(a6)
	clr.l ARENA_USED(a6)
	bsr.w configure
	bne.w failed
	move.l #1, LINE_NUMBER(a6)
	move.l NEXT_ID(a6), Frame.NameCount(a5)
	moveq #0, d0
	bra.w done
failed
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; begin
; Lower one caller-bounded line. A0=the session Frame; Source excludes its line
; ending and Output has per-line packed-record capacity. D0=0 success, 1 failure.
; Used is this line's packed byte count; NameCount is the next free identifier.
; Preserves other registers; CCR reflects D0. Used=0 on failure.
line	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	clr.l Frame.Used(a5)
	movea.l Frame.Scratch(a5), a6
	move.l a6, d0
	beq.w failed
	cmpi.l #65535, LINE_NUMBER(a6)
	bhi.w failed
	movea.l Frame.Source(a5), a0
	move.l Frame.SourceBytes(a5), d0
	bmi.w failed
	move.l a0, d1
	add.l d0, d1
	bcs.w failed
	movea.l Frame.Output(a5), a4
	move.l Frame.Capacity(a5), d1
	beq.w failed
	move.l a4, d2
	add.l d1, d2
	bcs.w failed
	lea TOKENS(a6), a1
	lea LEXEMES(a6), a2
	movea.l PROGRAM(a6), a3
	moveq #64, d1
	move.l #1024, d2
	move.l PROGRAM_BYTES(a6), d3
	jsr tokenizer.tkvmRun68000
	bne.w failed
	lea LINE_FRAME(a6), a0
	lea TOKENS(a6), a1
	move.l a1, writer.Frame.Tokens(a0)
	move.l #64*20, writer.Frame.TokenBytes(a0)
	move.l d1, writer.Frame.Count(a0)
	lea LEXEMES(a6), a1
	move.l a1, writer.Frame.Lexemes(a0)
	move.l d3, writer.Frame.LexemeBytes(a0)
	move.l Frame.Output(a5), writer.Frame.Output(a0)
	move.l Frame.Capacity(a5), writer.Frame.Capacity(a0)
	move.l #bind, writer.Frame.Binder(a0)
	move.l a6, writer.Frame.Context(a0)
	move.l LINE_NUMBER(a6), d0
	move.w d0, writer.Frame.SourceLine(a0)
	jsr writer.writeLine
	bne.w failed
	move.l d1, Frame.Used(a5)
	move.l NEXT_ID(a6), Frame.NameCount(a5)
	addq.l #1, LINE_NUMBER(a6)
	moveq #0, d0
	bra.w done
failed
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; line
; End a streaming session. A0=Frame. Clears scratch-resident pointers and resets
; tokenizer control state before the caller frees scratch. D0=0. Preserves other
; registers; CCR reflects D0.
finish	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l Frame.Scratch(a0), a6
	move.l a6, d0
	beq.w resetControl
	clr.l PROGRAM(a6)
	clr.l PROGRAM_BYTES(a6)
	clr.l DICTIONARY(a6)
	clr.l DICTIONARY_COUNT(a6)
	clr.l PACKAGE_END(a6)
	lea LINE_FRAME(a6), a1
	moveq #10-1, d0
clearFrame
	clr.l (a1)+
	dbra d0, clearFrame
resetControl
	moveq #0, d0
	jsr control.tkvmSetStepBudget68000
	moveq #0, d0
	moveq #0, d1
	suba.l a0, a0
	jsr control.tkvmSetProgramStateTable68000
	moveq #0, d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; finish
	.priv
; Validate only the package surfaces consumed by this frontend. Execution has
; independent bounds checks for candidate/program tables.
configure	.block
	movea.l Frame.Package(a5), a4
	cmpi.l #$42535032, package.Header.Magic(a4)
	bne.w bad
	move.l package.Header.Bytes(a4), d7
	cmpi.l #76, d7
	blo.w bad
	move.l a4, d0
	add.l d7, d0
	bcs.w bad
	move.l d0, PACKAGE_END(a6)
	moveq #0, d0
	move.w package.Header.NameCount(a4), d0
	move.l d0, NEXT_ID(a6)
	move.l package.Header.Dictionary(a4), d0
	cmpi.l #76, d0
	blo.w bad
	cmp.l d7, d0
	bhi.w bad
	movea.l a4, a2
	adda.l d0, a2
	move.l a2, DICTIONARY(a6)
	move.l package.Header.DictionaryCount(a4), d6
	move.l d6, DICTIONARY_COUNT(a6)
dictLoop
	tst.l d6
	beq.w configureTokenizer
	move.l PACKAGE_END(a6), d0
	sub.l a2, d0
	cmpi.l #6, d0
	blo.w bad
	moveq #0, d1
	move.w (a2), d1
	beq.w bad
	moveq #0, d2
	move.w 2(a2), d2
	cmp.l NEXT_ID(a6), d2
	bhs.w bad
	addi.l #6, d1
	addq.l #1, d1
	andi.l #$fffffffe, d1
	cmp.l d0, d1
	bhi.w bad
	adda.l d1, a2
	subq.l #1, d6
	bra.w dictLoop
configureTokenizer
	move.l package.Header.Tokenizer(a4), d0
	cmpi.l #76, d0
	blo.w bad
	cmp.l d7, d0
	bhi.w bad
	sub.l d0, d7
	move.l package.Header.TokenizerBytes(a4), d6
	cmp.l d7, d6
	bhi.w bad
	cmpi.l #16, d6
	blo.w bad
	lea 0(a4, d0.l), a2
	cmpi.w #1, (a2)
	bne.w bad
	moveq #0, d4
	move.w 4(a2), d4
	beq.w bad
	moveq #0, d5
	move.w 2(a2), d5
	cmp.l d4, d5
	bhs.w bad
	move.l d4, d0
	lsl.l #2, d0
	addi.l #12, d0
	cmp.l d6, d0
	bhs.w bad
	sub.l d0, d6
	move.l d6, PROGRAM_BYTES(a6)
	lea 0(a2, d0.l), a0
	move.l a0, PROGRAM(a6)
	lea 12(a2), a0
	move.l d4, d0
checkStates
	move.l (a0)+, d1
	cmp.l d6, d1
	bhs.w bad
	subq.l #1, d0
	bne.w checkStates
	move.l 8(a2), d0
	ble.w bad
	jsr control.tkvmSetStepBudget68000
	lea 12(a2), a0
	move.l d4, d0
	move.l d5, d1
	jsr control.tkvmSetProgramStateTable68000
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; configure
; Writer callback ABI: lexical bytes A0/D0; D1=id,D2=qualifier,D0=status.
; A1=Scratch context. Preserves D3-D7/A2-A6.
bind	.block
	movem.l d3-d7/a2-a6, -(sp)
	movea.l a1, a6
	movea.l a0, a2
	move.l d0, d6
	movea.l DICTIONARY(a6), a3
	move.l DICTIONARY_COUNT(a6), d7
findPackage
	tst.l d7
	beq.w findSymbol
	moveq #0, d5
	move.w (a3), d5
	cmp.l d6, d5
	bne.w advance
	movea.l a2, a0
	lea 6(a3), a1
	move.l d6, d0
	bsr.w equal
	bne.w advance
	moveq #0, d1
	move.w 2(a3), d1
	moveq #0, d2
	move.b 4(a3), d2
	bra.w good
advance
	addi.l #7, d5
	andi.l #$fffffffe, d5
	adda.l d5, a3
	subq.l #1, d7
	bra.w findPackage
findSymbol
	lea ENTRIES(a6), a3
	move.l SYMBOL_COUNT(a6), d7
symbolLoop
	tst.l d7
	beq.w create
	cmp.w Entry.Length(a3), d6
	bne.w nextSymbol
	movea.l a2, a0
	movea.l Entry.Name(a3), a1
	move.l d6, d0
	bsr.w equal
	bne.w nextSymbol
	moveq #0, d1
	move.w Entry.Id(a3), d1
	moveq #0, d2
	bra.w good
nextSymbol
	addq.l #8, a3
	subq.l #1, d7
	bra.w symbolLoop
create
	cmpi.l #63, d6
	bhi.w bad
	tst.l d6
	beq.w bad
	cmpi.l #SYMBOL_LIMIT, SYMBOL_COUNT(a6)
	bhs.w bad
	cmpi.l #65535, NEXT_ID(a6)
	bhi.w bad
	move.l ARENA_USED(a6), d0
	add.l d6, d0
	cmpi.l #ARENA_BYTES, d0
	bhi.w bad
	lea ARENA(a6), a1
	adda.l ARENA_USED(a6), a1
	move.l a1, Entry.Name(a3)
	move.w d6, Entry.Length(a3)
	move.l NEXT_ID(a6), d1
	move.w d1, Entry.Id(a3)
	move.l d0, ARENA_USED(a6)
	addq.l #1, NEXT_ID(a6)
	addq.l #1, SYMBOL_COUNT(a6)
	move.l d6, d0
copy
	move.b (a2)+, (a1)+
	subq.l #1, d0
	bne.w copy
	moveq #0, d2
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d3-d7/a2-a6
	rts
	.bend  ; bind
; Compare D0 nonempty ASCII lexical bytes, A0/A1; D0=0 equal, 1 unequal.
; Clobbers D1/D2/A0/A1; preserves remaining registers; CCR reflects D0.
equal	.block
loop
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #'A', d1
	blo.w leftReady
	cmpi.b #'Z', d1
	bhi.w leftReady
	addi.b #32, d1
leftReady
	moveq #0, d2
	move.b (a1)+, d2
	cmpi.b #'A', d2
	blo.w rightReady
	cmpi.b #'Z', d2
	bhi.w rightReady
	addi.b #32, d2
rightReady
	cmp.b d1, d2
	bne.w different
	subq.l #1, d0
	bne.w loop
	moveq #0, d0
	rts
different
	moveq #1, d0
	rts
	.bend  ; equal
	.endsection
	.endmodule
