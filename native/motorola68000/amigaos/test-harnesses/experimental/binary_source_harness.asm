; Real-native experimental text-to-binary-source assembly contract.
; Input/oracle identity belongs to the caller, never this executable.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent
	.module main
	.cpu 68020
	.use experimental.amigaos.binary_frontend as frontend
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_package as package
SYS_BASE = 4
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
OPEN = -30
CLOSE = -36
READ = -42
WRITE = -48
STDOUT = -60
MODE_OLDFILE = 1005
MODE_NEWFILE = 1006
RETURN_FAIL = 20
INPUT_CAPACITY = 1048576
RECORD_CAPACITY = 65536
OUTPUT_CAPACITY = 65536
NAME_CAPACITY = 65536

	.section entry, kind=code
	.pub
; AmigaDOS entry, no arguments. D0=0 only after preparation, both binary
; assembly passes and complete output write/close. Other registers preserved.
start	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #RETURN_FAIL, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.w, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	bsr.w readInput
	bne.w closeDos
	bsr.w run
	beq.w outputReady
	bsr.w reportFailure
	bra.w closeDos
outputReady
	bsr.w writeOutput
	bne.w closeDos
	clr.l ReturnCode
closeDos
	movea.l DosBase, a1
	movea.l SYS_BASE.w, a6
	jsr CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; start
	.priv
; Stable harness failure diagnostic for completed negative contracts. No source,
; mutable service buffers or debug state are printed; ReturnCode remains failure.
reportFailure	.block
	movea.l DosBase, a6
	jsr STDOUT(a6)
	move.l d0, d1
	beq.w done
	move.l #FailureMessage, d2
	move.l #FailureMessageEnd, d3
	sub.l d2, d3
	jsr WRITE(a6)
done
	rts
	.bend  ; reportFailure

readInput	.block
	movea.l DosBase, a6
	move.l #InputPath, d1
	move.l #MODE_OLDFILE, d2
	jsr OPEN(a6)
	tst.l d0
	beq.w bad
	move.l d0, d4
	moveq #0, d5
loop
	move.l d4, d1
	move.l #Input, d2
	add.l d5, d2
	move.l #INPUT_CAPACITY+1, d3
	sub.l d5, d3
	jsr READ(a6)
	tst.l d0
	bmi.w closeBad
	beq.w complete
	add.l d0, d5
	cmpi.l #INPUT_CAPACITY, d5
	bhi.w closeBad
	bra.w loop
complete
	move.l d5, InputLength
	move.l d4, d1
	jsr CLOSE(a6)
	tst.l d0
	beq.w bad
	moveq #0, d0
	rts
closeBad
	move.l d4, d1
	jsr CLOSE(a6)
bad
	moveq #1, d0
	rts
	.bend  ; readInput

run	.block
	cmpi.l #72, InputLength
	blo.w bad
	lea Input, a4
	cmpi.l #$42535031, package.Header.Magic(a4)
	bne.w bad
	move.l package.Header.Bytes(a4), d4
	cmpi.l #72, d4
	blo.w bad
	cmp.l InputLength, d4
	bhi.w bad
	moveq #0, d0
	move.w package.Header.NameCount(a4), d0
	addi.l #512, d0
	cmpi.l #NAME_CAPACITY, d0
	bhi.w bad
	lea Front, a0
	move.l a4, frontend.Frame.Package(a0)
	lea 0(a4, d4.l), a1
	move.l a1, frontend.Frame.Source(a0)
	move.l InputLength, d0
	sub.l d4, d0
	lea Front, a0
	move.l d0, frontend.Frame.SourceBytes(a0)
	move.l #Records, frontend.Frame.Output(a0)
	move.l #RECORD_CAPACITY, frontend.Frame.Capacity(a0)
	jsr frontend.prepare
	bne.w bad
	lea Context, a0
	move.l #Values, package.Context.Values(a0)
	move.l #Defined, package.Context.Defined(a0)
	lea Front, a1
	move.l frontend.Frame.NameCount(a1), package.Context.Count(a0)
	move.l #Input, package.Context.Package(a0)
	clr.l package.Context.Pc(a0)
	clr.w package.Context.Pass(a0)
	bsr.w poisonText
	lea Work, a0
	move.l #Records, assembly.Frame.Records(a0)
	lea Front, a1
	move.l frontend.Frame.Used(a1), assembly.Frame.RecordBytes(a0)
	move.l #Context, assembly.Frame.Context(a0)
	move.l #Output, assembly.Frame.Output(a0)
	move.l #OUTPUT_CAPACITY, assembly.Frame.Capacity(a0)
	jsr assembly.assemble
	rts
bad
	moveq #1, d0
	rts
	.bend  ; run

; Preparation validated every dictionary span. Erase source plus dictionary
; spelling bytes before assembly, preserving only numeric metadata/programs.
poisonText	.block
	lea Front, a1
	movea.l frontend.Frame.Source(a1), a0
	move.l frontend.Frame.SourceBytes(a1), d0
	bsr.w erase
	clr.l frontend.Frame.Source(a1)
	lea Input, a4
	movea.l a4, a3
	adda.l package.Header.Dictionary(a4), a3
	move.l package.Header.DictionaryCount(a4), d7
loop
	tst.l d7
	beq.w done
	moveq #0, d5
	move.w (a3), d5
	lea 6(a3), a0
	move.l d5, d0
	bsr.w erase
	addi.l #7, d5
	andi.l #$fffffffe, d5
	adda.l d5, a3
	subq.l #1, d7
	bra.w loop
done
	rts
	.bend  ; poisonText

; D0 byte count at A0; clobbers D0/A0/CCR, including the empty range.
erase	.block
	tst.l d0
	beq.w done
loop
	clr.b (a0)+
	subq.l #1, d0
	bne.w loop
done
	rts
	.bend  ; erase

writeOutput	.block
	movea.l DosBase, a6
	move.l #OutputPath, d1
	move.l #MODE_NEWFILE, d2
	jsr OPEN(a6)
	tst.l d0
	beq.w bad
	move.l d0, d4
	moveq #0, d5
	lea Work, a5
loop
	move.l assembly.Frame.Used(a5), d3
	sub.l d5, d3
	beq.w complete
	move.l #Output, d2
	add.l d5, d2
	move.l d4, d1
	jsr WRITE(a6)
	tst.l d0
	ble.w closeBad
	add.l d0, d5
	cmp.l assembly.Frame.Used(a5), d5
	bhi.w closeBad
	bra.w loop
complete
	move.l d4, d1
	jsr CLOSE(a6)
	tst.l d0
	beq.w bad
	moveq #0, d0
	rts
closeBad
	move.l d4, d1
	jsr CLOSE(a6)
bad
	moveq #1, d0
	rts
	.bend  ; writeOutput
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
InputPath	.byte "Work:input.bin", 0
OutputPath	.byte "Work:output.bin", 0
FailureMessage	.byte "binary source: unsupported or invalid input", 10
FailureMessageEnd
	.endsection
	.section bss, kind=bss
	.align 4
DosBase	.res long, 1
ReturnCode	.res long, 1
InputLength	.res long, 1
Front	.res byte, frontend.Frame.NameCount+4
Work	.res byte, assembly.Frame.Reserved+2
Context	.res byte, package.Context.Reserved+2
; Deliberately generous proof buffers, not a 2 MiB product-memory claim.
; Input 1 MiB plus overflow sentinel; records/output each 64 KiB;
; values 65,536 longs, defined flags 65,536 bytes. Imported module BSS is extra.
Input	.res byte, INPUT_CAPACITY+1
	.align 2
Records	.res byte, RECORD_CAPACITY
Output	.res byte, OUTPUT_CAPACITY
Values	.res long, NAME_CAPACITY
Defined	.res byte, NAME_CAPACITY
	.endsection
	.output "build/binary_source_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
