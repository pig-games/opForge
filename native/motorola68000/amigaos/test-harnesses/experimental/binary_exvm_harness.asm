; Direct canonical EXVM numeric compiler batch: scalar trees and failures.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module main
	.cpu 68020
	.use experimental.amigaos.binary_exvm as compiler
	.use experimental.amigaos.binary_exvm_lower as lowering

SYS_BASE = 4
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
OPEN = -30
CLOSE = -36
READ = -42
WRITE = -48
MODE_OLDFILE = 1005
MODE_NEWFILE = 1006
RETURN_FAIL = 20
CASE_CAPACITY = 64
INPUT_CAPACITY = 65536
INPUT_BUFFER_BYTES = INPUT_CAPACITY+1
OUTPUT_CAPACITY = 16384
ARENA_CAPACITY = 3072

	.section entry, kind=code
	.pub
start	.block
	move.l #RETURN_FAIL, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.w, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	move.l #InputEnd-InputBuffer, InputLength
	bsr.w evaluateCases
	bne.w closeDos
	bsr.w writeOutput
	bne.w closeDos
	clr.l ReturnCode
closeDos
	movea.l DosBase, a1
	movea.l SYS_BASE.w, a6
	jsr CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	rts
	.bend  ; start

; Input BE: count.u32, repeated tokens.u16/program.u16/arena.u16/reserved.u16/
; steps.u32, numeric tokens, canonical program, zero pad to even record end.
; Output BE: status/consumed/root/used.u32, then Used raw 24-byte BE nodes.
; Compile failures are records; malformed batch envelopes fail the harness.
evaluateCases	.block
	lea InputBuffer, a3
	movea.l a3, a4
	adda.l InputLength, a4
	move.l InputLength, d0
	cmpi.l #4, d0
	blo.w fail
	move.l (a3)+, d7
	cmpi.l #CASE_CAPACITY, d7
	bhi.w fail
	lea ResultBuffer, a5
caseLoop
	tst.l d7
	beq.w casesDone
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #12, d0
	blo.w fail
	lea CompileRequest, a0
	moveq #0, d1
	move.w (a3)+, d1
	move.l d1, compiler.Request.TokenBytes(a0)
	moveq #0, d2
	move.w (a3)+, d2
	move.l d2, compiler.Request.ProgramBytes(a0)
	moveq #0, d3
	move.w (a3)+, d3
	cmpi.l #ARENA_CAPACITY, d3
	bhi.w fail
	move.l d3, compiler.Request.ArenaBytes(a0)
	move.w (a3)+, d0
	move.w d0, (LowerMode).l
	cmpi.w #8, (LowerMode).l
	bhi.w fail
	move.l (a3)+, compiler.Request.StepBudget(a0)
	move.l #NodeArena, compiler.Request.Arena(a0)
	move.l #CompilerScratch, compiler.Request.Scratch(a0)
	move.l #compiler.SCRATCH_BYTES, compiler.Request.ScratchBytes(a0)
	move.l a4, d0
	sub.l a3, d0
	add.l d2, d1
	cmp.l d0, d1
	bhi.w fail
	move.l a3, compiler.Request.Tokens(a0)
	adda.l compiler.Request.TokenBytes(a0), a3
	move.l a3, compiler.Request.Program(a0)
	adda.l d2, a3
	move.l a3, d0
	btst #0, d0
	beq.w aligned
	cmpa.l a4, a3
	bhs.w fail
	tst.b (a3)+
	bne.w fail
aligned
	; Malformed caller workspace must fail before any dereference.
	cmpi.w #5, (LowerMode).l
	bne.w oddScratch
	clr.l compiler.Request.Scratch(a0)
oddScratch
	cmpi.w #6, (LowerMode).l
	bne.w smallScratch
	addq.l #1, compiler.Request.Scratch(a0)
smallScratch
	cmpi.w #7, (LowerMode).l
	bne.w wrappingScratch
	subq.l #1, compiler.Request.ScratchBytes(a0)
wrappingScratch
	cmpi.w #8, (LowerMode).l
	bne.w compile
	move.l #$fffffffc, compiler.Request.Scratch(a0)
compile
	jsr compiler.compile
	lea CompileRequest, a0
	move.l compiler.Request.Used(a0), d6
	move.l a5, d0
	lea ResultBuffer, a1
	sub.l a1, d0
	add.l d6, d0
	addi.l #16, d0
	cmpi.l #OUTPUT_CAPACITY, d0
	bhi.w fail
	move.l compiler.Request.Status(a0), (a5)+
	move.l compiler.Request.Consumed(a0), (a5)+
	move.l compiler.Request.Root(a0), (a5)+
	move.l d6, (a5)+
	lea NodeArena, a0
copyNodes
	tst.l d6
	beq.w copied
	move.b (a0)+, (a5)+
	subq.l #1, d6
	bra.w copyNodes
copied
	movem.l d7/a3-a5, -(sp)
	lea CompileRequest, a1
	moveq #lowering.STATUS_MALFORMED, d0
	moveq #0, d2
	tst.l compiler.Request.Status(a1)
	bne.w lowerDone
	lea NodeArena, a0
	move.l compiler.Request.Used(a1), d0
	move.l compiler.Request.Root(a1), d1
	lea LowerBuffer, a3
	lea LowerBuffer+512, a4
	moveq #0, d2
	move.w (LowerMode).l, d2
	cmpi.w #1, d2
	bne.w cycleMode
	addq.l #1, d1
cycleMode
	cmpi.w #2, d2
	bne.w arityMode
	lea 0(a0,d1.l), a2
	move.l d1, compiler.Node.First(a2)
arityMode
	cmpi.w #3, d2
	bne.w capacityMode
	lea 0(a0,d1.l), a2
	clr.w compiler.Node.Count(a2)
capacityMode
	cmpi.w #4, d2
	bne.w callLower
	lea LowerBuffer+1, a4
callLower
	lea CompilerScratch, a1
	move.l #compiler.SCRATCH_BYTES, d2
	jsr lowering.lower
	moveq #0, d2
	tst.l d0
	bne.w lowerDone
	move.l a3, d2
	lea LowerBuffer, a1
	sub.l a1, d2
lowerDone
	movem.l (sp)+, d7/a3-a5
	move.l a5, d1
	lea ResultBuffer, a1
	sub.l a1, d1
	add.l d2, d1
	addi.l #8, d1
	cmpi.l #OUTPUT_CAPACITY, d1
	bhi.w fail
	move.l d0, (a5)+
	move.l d2, (a5)+
	lea LowerBuffer, a0
copyLower
	tst.l d2
	beq.w lowerCopied
	move.b (a0)+, (a5)+
	subq.l #1, d2
	bra.w copyLower
lowerCopied
	; Result records remain aligned for the next BE status longs.
	move.l a5, d0
	btst #0, d0
	beq.w lowerAligned
	clr.b (a5)+
lowerAligned
	subq.l #1, d7
	bra.w caseLoop
casesDone
	cmpa.l a4, a3
	bne.w fail
	move.l a5, d0
	lea ResultBuffer, a1
	sub.l a1, d0
	move.l d0, OutputLength
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; evaluateCases

; Write the exact result stream to the fixed Work: output path.
; Outputs: D0=0 success/1 failure.
; Clobbers: D0-D4/A6/CCR.
writeOutput	.block
	move.l #OutputPath, d1
	move.l #MODE_NEWFILE, d2
	movea.l DosBase, a6
	jsr OPEN(a6)
	tst.l d0
	beq.s fail
	move.l d0, d4
	move.l d0, d1
	move.l #ResultBuffer, d2
	move.l OutputLength, d3
	jsr WRITE(a6)
	move.l d0, -(sp)
	move.l d4, d1
	jsr CLOSE(a6)
	move.l (sp)+, d0
	cmp.l OutputLength, d0
	bne.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; writeOutput
	.endsection

	.section data, kind=data
DosName
	.byte "dos.library", 0
OutputPath
	.byte "Work:binary-exvm-nodes.bin", 0
	.align 2
InputBuffer
	.incbin "binary-exvm-cases.bin"
InputEnd
	.endsection

	.section bss, kind=bss
DosBase
	.res long, 1
ReturnCode
	.res long, 1
InputLength
	.res long, 1
OutputLength
	.res long, 1
ResultBuffer
	.res byte, OUTPUT_CAPACITY
	.align 4
LowerMode
	.res word, 1
LowerBuffer
	.res byte, 512
	.align 4
CompileRequest
	.res byte, compiler.REQUEST_BYTES
NodeArena
	.res byte, ARENA_CAPACITY
	.align 4
CompilerScratch
	.res byte, compiler.SCRATCH_BYTES
	.endsection

	.output "binary-exvm.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
