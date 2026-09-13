; One-shot line tokenization and numeric lowering. Scratch is session-owned,
; non-reentrant, and erased before return; no source text enters packed records.
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
	.endstruct
	.priv
ARENA_BYTES = 16384
SYMBOL_LIMIT = 512
Entry	.struct
Name	.long ?
Length	.word ?
Id	.word ?
	.endstruct
	.section bss, kind=bss
	.align 4
Session	.res long, 1
Cursor	.res long, 1
SourceEnd	.res long, 1
Output	.res long, 1
Remaining	.res long, 1
Program	.res long, 1
ProgramBytes	.res long, 1
Dictionary	.res long, 1
DictionaryCount	.res long, 1
PackageEnd	.res long, 1
LineNumber	.res long, 1
NextId	.res long, 1
SymbolCount	.res long, 1
ArenaUsed	.res long, 1
LineFrame	.res byte, writer.Frame.Used+2
Tokens	.res byte, 64*20
Lexemes	.res byte, 1024
Entries	.res byte, SYMBOL_LIMIT*8
Arena	.res byte, ARENA_BYTES
	.endsection
	.section code, kind=code
	.pub
; A0=Frame; caller validates readable package capsule against Header.Bytes and
; provides nonoverlapping input/output storage. D0=0 success, 1 invalid/bounded
; subset failure. Preserves other registers; CCR reflects D0. Used=0 on failure.
; NameCount is the first free numeric ID after package and source bindings.
prepare	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l a0, Session
	movea.l a0, a5
	clr.l Frame.Used(a5)
	clr.l Frame.NameCount(a5)
	clr.l SymbolCount
	clr.l ArenaUsed
	bsr.w configure
	bne.w failed
	movea.l Session, a5
	move.l Frame.Source(a5), Cursor
	move.l Frame.SourceBytes(a5), d0
	bmi.w failed
	add.l Cursor, d0
	bcs.w failed
	move.l d0, SourceEnd
	move.l Frame.Output(a5), Output
	move.l Frame.Capacity(a5), Remaining
	move.l Remaining, d0
	add.l Output, d0
	bcs.w failed
	move.l #1, LineNumber
lineLoop
	movea.l Cursor, a0
	cmpa.l SourceEnd, a0
	beq.w complete
	cmpi.l #65535, LineNumber
	bhi.w failed
	movea.l a0, a1
scan
	cmpa.l SourceEnd, a1
	beq.w lastLine
	cmpi.b #10, (a1)+
	bne.w scan
	move.l a1, Cursor
	subq.l #1, a1
	bra.w trim
lastLine
	move.l a1, Cursor
trim
	cmpa.l a0, a1
	beq.w tokenize
	cmpi.b #13, -1(a1)
	bne.w tokenize
	subq.l #1, a1
tokenize
	move.l a1, d0
	sub.l a0, d0
	lea Tokens, a1
	lea Lexemes, a2
	movea.l Program, a3
	moveq #64, d1
	move.l #1024, d2
	move.l ProgramBytes, d3
	jsr tokenizer.tkvmRun68000
	bne.w failed
	lea LineFrame, a0
	move.l #Tokens, writer.Frame.Tokens(a0)
	move.l #64*20, writer.Frame.TokenBytes(a0)
	move.l d1, writer.Frame.Count(a0)
	move.l #Lexemes, writer.Frame.Lexemes(a0)
	move.l d3, writer.Frame.LexemeBytes(a0)
	move.l Output, writer.Frame.Output(a0)
	move.l Remaining, writer.Frame.Capacity(a0)
	move.l #bind, writer.Frame.Binder(a0)
	clr.l writer.Frame.Context(a0)
	move.l LineNumber, d0
	lea LineFrame, a0
	move.w d0, writer.Frame.SourceLine(a0)
	jsr writer.writeLine
	bne.w failed
	add.l d1, Output
	sub.l d1, Remaining
	addq.l #1, LineNumber
	bra.w lineLoop
complete
	movea.l Session, a5
	move.l Frame.Capacity(a5), d0
	sub.l Remaining, d0
	move.l d0, Frame.Used(a5)
	move.l NextId, Frame.NameCount(a5)
	moveq #0, d7
	bra.w cleanup
failed
	moveq #1, d7
cleanup
	lea Lexemes, a0
	move.l #1024/4, d0
	bsr.w erase
	lea Arena, a0
	move.l #ARENA_BYTES/4, d0
	bsr.w erase
	lea Entries, a0
	move.l #SYMBOL_LIMIT*8/4, d0
	bsr.w erase
	clr.l Cursor
	clr.l SourceEnd
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; prepare
	.priv
; Validate only the package surfaces consumed by this frontend. Execution has
; independent bounds checks for candidate/program tables.
configure	.block
	movea.l Frame.Package(a5), a4
	cmpi.l #$42535031, package.Header.Magic(a4)
	bne.w bad
	move.l package.Header.Bytes(a4), d7
	cmpi.l #72, d7
	blo.w bad
	move.l a4, d0
	add.l d7, d0
	bcs.w bad
	move.l d0, PackageEnd
	moveq #0, d0
	move.w package.Header.NameCount(a4), d0
	move.l d0, NextId
	move.l package.Header.Dictionary(a4), d0
	cmpi.l #72, d0
	blo.w bad
	cmp.l d7, d0
	bhi.w bad
	movea.l a4, a2
	adda.l d0, a2
	move.l a2, Dictionary
	move.l package.Header.DictionaryCount(a4), d6
	move.l d6, DictionaryCount
dictLoop
	tst.l d6
	beq.w configureTokenizer
	move.l PackageEnd, d0
	sub.l a2, d0
	cmpi.l #6, d0
	blo.w bad
	moveq #0, d1
	move.w (a2), d1
	beq.w bad
	moveq #0, d2
	move.w 2(a2), d2
	cmp.l NextId, d2
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
	cmpi.l #72, d0
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
	move.l d6, ProgramBytes
	lea 0(a2, d0.l), a0
	move.l a0, Program
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
; Preserves D3-D7/A2-A6. Its symbol arena exists only during prepare.
bind	.block
	movem.l d3-d7/a2-a6, -(sp)
	movea.l a0, a2
	move.l d0, d6
	movea.l Dictionary, a3
	move.l DictionaryCount, d7
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
	lea Entries, a3
	move.l SymbolCount, d7
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
	cmpi.l #SYMBOL_LIMIT, SymbolCount
	bhs.w bad
	cmpi.l #65535, NextId
	bhi.w bad
	move.l ArenaUsed, d0
	add.l d6, d0
	cmpi.l #ARENA_BYTES, d0
	bhi.w bad
	lea Arena, a1
	adda.l ArenaUsed, a1
	move.l a1, Entry.Name(a3)
	move.w d6, Entry.Length(a3)
	move.l NextId, d1
	move.w d1, Entry.Id(a3)
	move.l d0, ArenaUsed
	addq.l #1, NextId
	addq.l #1, SymbolCount
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
; Clear D0 longwords at A0; clobbers A0/D0/CCR.
erase	.block
loop
	clr.l (a0)+
	subq.l #1, d0
	bne.w loop
	rts
	.bend  ; erase
	.endsection
	.endmodule
