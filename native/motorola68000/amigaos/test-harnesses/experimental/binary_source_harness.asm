; Streaming native binary-source contract; input identity belongs to the caller.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent
	.module main
	.cpu 68020
	.use experimental.amigaos.binary_frontend as frontend
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_memory as memory
	.include "memory_telemetry.i"
HEADER_BYTES = 76
IO_BYTES = 4096
LINE_BYTES = 4096
RECORD_BYTES = 256
IO_SCRATCH_BYTES = IO_BYTES+LINE_BYTES+RECORD_BYTES
	.section entry, kind=code
	.pub
; AmigaDOS entry. D0=0 only after complete preparation, assembly and output.
; Preserves other registers. Every success/failure path frees owned allocations.
start	.block
	movem.l d1-d7/a0-a6, -(sp)
	.MEMORY_PHASE #0
	move.l #20, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l 4.w, a6
	jsr -552(a6)
	tst.l d0
	beq.w cleanup
	move.l d0, DosBase
	.MEMORY_CLOCK DosBase, #0
	.MEMORY_STAGE #1
	bsr.w prepare
	bne.w failed
	.MEMORY_CLOCK DosBase, #1
	.MEMORY_PHASE #1
	bsr.w run
	bne.w failed
	.MEMORY_CLOCK DosBase, #2
	.MEMORY_PHASE #2
	bsr.w writeOutput
	bne.w failed
	clr.l ReturnCode
	bra.w cleanup
failed
	bsr.w reportFailure
cleanup
	tst.l FrontStarted
	beq.w freeBlocks
	lea Front, a0
	jsr frontend.finish
freeBlocks
	lea PackageBlock, a0
	jsr memory.release
	lea RuntimeBlock, a0
	jsr memory.release
	lea PrepBlock, a0
	jsr memory.release
	lea Records, a0
	jsr memory.release
	lea Symbols, a0
	jsr memory.release
	lea Output, a0
	jsr memory.release
	.MEMORY_PHASE #3
	move.l DosBase, d0
	beq.w done
	.MEMORY_SAVE DosBase
	movea.l d0, a1
	movea.l 4.w, a6
	jsr -414(a6)
done
	move.l ReturnCode, d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; start
	.priv
reportFailure	.block
	movea.l DosBase, a6
	jsr -60(a6)
	move.l d0, d1
	beq.w done
	move.l #FailureMessage, d2
	move.l #FailureMessageEnd, d3
	sub.l d2, d3
	jsr -48(a6)
done
	rts
	.bend  ; reportFailure

; Read exactly D3 bytes into D2 from the open InputHandle. D0/CCR=status.
; Preserves all other registers. No seek, whole-source buffer or byte-at-a-time I/O.
readExact	.block
	movem.l d1-d4/a0-a1/a6, -(sp)
	movea.l DosBase, a6
loop
	tst.l d3
	beq.w good
	move.l d3, d4
	move.l InputHandle, d1
	jsr -42(a6)
	tst.l d0
	ble.w bad
	cmp.l d4, d0
	bhi.w bad
	add.l d0, d2
	sub.l d0, d3
	bra.w loop
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a1/a6
	tst.l d0
	rts
	.bend  ; readExact

prepare	.block
	movea.l DosBase, a6
	move.l #InputPath, d1
	move.l #1005, d2
	jsr -30(a6)
	tst.l d0
	beq.w bad
	move.l d0, InputHandle
	move.l #Header, d2
	moveq #HEADER_BYTES, d3
	bsr.w readExact
	bne.w closeBad
	lea Header, a4
	cmpi.l #$42535033, package.Header.Magic(a4)
	bne.w closeBad
	move.l package.Header.Bytes(a4), d0
	cmpi.l #HEADER_BYTES, d0
	blo.w closeBad
	lea PackageBlock, a0
	jsr memory.reserve
	bne.w closeBad
	lea PackageBlock, a0
	movea.l memory.Block.Pointer(a0), a1
	lea Header, a0
	moveq #HEADER_BYTES, d0
	bsr.w copy
	lea PackageBlock, a0
	movea.l memory.Block.Pointer(a0), a4
	move.l a4, d2
	addi.l #HEADER_BYTES, d2
	move.l package.Header.Bytes(a4), d3
	subi.l #HEADER_BYTES, d3
	bsr.w readExact
	bne.w closeBad
	move.l package.Header.RuntimeBytes(a4), d0
	cmpi.l #HEADER_BYTES, d0
	blo.w closeBad
	cmp.l package.Header.Bytes(a4), d0
	bhi.w closeBad
	btst #0, d0
	bne.w closeBad
	cmp.l package.Header.Dictionary(a4), d0
	bhi.w closeBad
	cmp.l package.Header.Tokenizer(a4), d0
	bhi.w closeBad
	movea.l a4, a0
	jsr frontend.scratchSize
	bne.w closeBad
	move.l d1, d0
	addi.l #IO_SCRATCH_BYTES, d0
	bcs.w closeBad
	lea PrepBlock, a0
	jsr memory.reserve
	bne.w closeBad
	lea PrepBlock, a0
	movea.l memory.Block.Pointer(a0), a1
	lea Front, a0
	move.l a4, frontend.Frame.Package(a0)
	move.l a1, frontend.Frame.Scratch(a0)
	adda.l d1, a1
	move.l a1, IoBuffer
	move.l a1, IoCursor
	move.l a1, IoEnd
	adda.l #IO_BYTES, a1
	move.l a1, LineBuffer
	adda.l #LINE_BYTES, a1
	move.l a1, frontend.Frame.Output(a0)
	move.l #RECORD_BYTES, frontend.Frame.Capacity(a0)
	move.l #1, FrontStarted
	jsr frontend.begin
	bne.w closeBad
	.MEMORY_STAGE #0
	clr.l LineUsed
sourceLoop
	bsr.w readByte
	cmpi.l #-1, d0
	beq.w sourceDone
	tst.l d0
	bmi.w closeBad
	addq.l #1, SourceBytes
	cmpi.b #10, d0
	beq.w lineReady
	move.l LineUsed, d1
	cmpi.l #LINE_BYTES, d1
	bhs.w closeBad
	movea.l LineBuffer, a0
	move.b d0, 0(a0, d1.l)
	addq.l #1, LineUsed
	bra.w sourceLoop
lineReady
	bsr.w lowerLine
	bne.w closeBad
	bra.w sourceLoop
sourceDone
	tst.l LineUsed
	beq.w prepared
	bsr.w lowerLine
	bne.w closeBad
prepared
	.MEMORY_STAGE #5
	bsr.w closeInput
	bne.w bad
	lea Records, a0
	movea.l memory.Block.Pointer(a0), a1
	move.l memory.Block.Used(a0), d0
	lea Front, a0
	jsr frontend.complete
	bne.w bad
	move.l frontend.Frame.NameCount(a0), NameCount
	jsr frontend.finish
	clr.l FrontStarted
	lea PrepBlock, a0
	jsr memory.release
	clr.l IoBuffer
	clr.l IoCursor
	clr.l IoEnd
	clr.l LineBuffer
; Copy relocatable execution prefix while the old allocation is still live.
; No lexical storage or source buffer remains when either assembly pass starts.
	lea PackageBlock, a0
	movea.l memory.Block.Pointer(a0), a4
	move.l package.Header.RuntimeBytes(a4), d0
	lea RuntimeBlock, a0
	jsr memory.reserve
	bne.w bad
	lea RuntimeBlock, a0
	movea.l memory.Block.Pointer(a0), a1
	movea.l a4, a0
	move.l package.Header.RuntimeBytes(a4), d0
	bsr.w copy
	lea RuntimeBlock, a0
	movea.l memory.Block.Pointer(a0), a4
	move.l package.Header.RuntimeBytes(a4), package.Header.Bytes(a4)
	clr.l package.Header.Dictionary(a4)
	clr.l package.Header.DictionaryCount(a4)
	clr.l package.Header.Tokenizer(a4)
	clr.l package.Header.TokenizerBytes(a4)
	lea PackageBlock, a0
	jsr memory.release
	lea Records, a0
	.MEMORY_LAYOUT package.Header.RuntimeBytes(a4), memory.Block.Used(a0), SourceBytes
	lea Front, a0
	clr.l frontend.Frame.Package(a0)
	clr.l frontend.Frame.Source(a0)
	clr.l frontend.Frame.SourceBytes(a0)
	clr.l frontend.Frame.Scratch(a0)
	clr.l frontend.Frame.Output(a0)
	moveq #0, d0
	rts
closeBad
	bsr.w closeInput
bad
	moveq #1, d0
	rts
	.bend  ; prepare

closeInput	.block
	movea.l DosBase, a6
	move.l InputHandle, d1
	clr.l InputHandle
	jsr -36(a6)
	tst.l d0
	beq.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; closeInput

; D0=unsigned byte, -1 EOF, -2 read failure. Other registers preserved.
readByte	.block
	movem.l d1-d3/a0-a1/a6, -(sp)
	movea.l IoCursor, a0
	cmpa.l IoEnd, a0
	bne.w available
	movea.l DosBase, a6
	move.l InputHandle, d1
	move.l IoBuffer, d2
	move.l #IO_BYTES, d3
	jsr -42(a6)
	tst.l d0
	bmi.w bad
	beq.w eof
	cmpi.l #IO_BYTES, d0
	bhi.w bad
	movea.l IoBuffer, a0
	move.l a0, IoCursor
	add.l a0, d0
	move.l d0, IoEnd
available
	moveq #0, d0
	move.b (a0)+, d0
	move.l a0, IoCursor
	bra.w done
eof
	moveq #-1, d0
	bra.w done
bad
	moveq #-2, d0
done
	movem.l (sp)+, d1-d3/a0-a1/a6
	rts
	.bend  ; readByte

lowerLine	.block
	lea Front, a0
	move.l LineBuffer, frontend.Frame.Source(a0)
	move.l LineUsed, d0
	beq.w trimmed
	movea.l LineBuffer, a1
	cmpi.b #13, -1(a1, d0.l)
	bne.w trimmed
	subq.l #1, d0
trimmed
	move.l d0, frontend.Frame.SourceBytes(a0)
	jsr frontend.line
	bne.w bad
	lea Records, a0
	move.l memory.Block.Used(a0), d0
	lea Front, a1
	add.l frontend.Frame.Used(a1), d0
	bcs.w bad
	jsr memory.reserve
	bne.w bad
	lea Records, a1
	movea.l memory.Block.Pointer(a1), a2
	adda.l memory.Block.Used(a1), a2
	lea Front, a0
	move.l frontend.Frame.Used(a0), d0
	add.l d0, memory.Block.Used(a1)
	movea.l frontend.Frame.Output(a0), a0
	movea.l a2, a1
	bsr.w copy
	clr.l LineUsed
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; lowerLine

run	.block
	move.l NameCount, d0
	beq.w bad
	cmpi.l #65536, d0
	bhi.w bad
	move.l d0, d1
	lsl.l #2, d0
	add.l d1, d0
	lea Symbols, a0
	jsr memory.reserve
	bne.w bad
	lea Context, a0
	lea Symbols, a1
	movea.l memory.Block.Pointer(a1), a2
	move.l a2, package.Context.Values(a0)
	move.l NameCount, d0
	lsl.l #2, d0
	adda.l d0, a2
	lea Context, a0
	move.l a2, package.Context.Defined(a0)
	move.l NameCount, package.Context.Count(a0)
	lea RuntimeBlock, a1
	move.l memory.Block.Pointer(a1), package.Context.Package(a0)
	lea Work, a0
	lea Records, a1
	move.l memory.Block.Pointer(a1), assembly.Frame.Records(a0)
	move.l memory.Block.Used(a1), assembly.Frame.RecordBytes(a0)
	move.l #Context, assembly.Frame.Context(a0)
	move.l #allocateOutput, assembly.Frame.Allocate(a0)
	jsr assembly.assemble
	rts
bad
	moveq #1, d0
	rts
	.bend  ; run

; Assembly sizing-pass callback. D0=needed bytes,A0=Frame; other registers preserved.
allocateOutput	.block
	movem.l d1-d2/a0-a2, -(sp)
	movea.l a0, a2
	lea Output, a0
	jsr memory.reserve
	bne.w done
	move.l memory.Block.Pointer(a0), assembly.Frame.Output(a2)
	move.l memory.Block.Capacity(a0), assembly.Frame.Capacity(a2)
done
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; allocateOutput

writeOutput	.block
	movea.l DosBase, a6
	move.l #OutputPath, d1
	move.l #1006, d2
	jsr -30(a6)
	tst.l d0
	beq.w bad
	move.l d0, d4
	moveq #0, d5
	lea Work, a5
loop
	move.l assembly.Frame.Used(a5), d3
	sub.l d5, d3
	beq.w complete
	lea Output, a0
	move.l memory.Block.Pointer(a0), d2
	add.l d5, d2
	move.l d4, d1
	jsr -48(a6)
	tst.l d0
	ble.w closeBad
	add.l d0, d5
	cmp.l assembly.Frame.Used(a5), d5
	bhi.w closeBad
	bra.w loop
complete
	move.l d4, d1
	jsr -36(a6)
	tst.l d0
	beq.w bad
	moveq #0, d0
	rts
closeBad
	move.l d4, d1
	jsr -36(a6)
bad
	moveq #1, d0
	rts
	.bend  ; writeOutput
; D0 bytes A0->A1, distinct allocations. Clobbers D0/A0/A1/CCR.
copy	.block
	tst.l d0
	beq.w done
loop
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w loop
done
	rts
	.bend  ; copy
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
InputHandle	.res long, 1
FrontStarted	.res long, 1
IoBuffer	.res long, 1
IoCursor	.res long, 1
IoEnd	.res long, 1
LineBuffer	.res long, 1
LineUsed	.res long, 1
SourceBytes	.res long, 1
NameCount	.res long, 1
Header	.res byte, HEADER_BYTES
Front	.res byte, frontend.Frame.Scratch+4
Work	.res byte, assembly.Frame.Allocate+4
Context	.res byte, package.Context.Reserved+2
PackageBlock	.res byte, memory.Block.Used+4
RuntimeBlock	.res byte, memory.Block.Used+4
PrepBlock	.res byte, memory.Block.Used+4
Records	.res byte, memory.Block.Used+4
Symbols	.res byte, memory.Block.Used+4
Output	.res byte, memory.Block.Used+4
	.endsection
	.output "build/binary_source_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
