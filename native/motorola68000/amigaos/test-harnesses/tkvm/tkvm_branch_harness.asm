; Focused conditional-branch tokenizer VM contract harness.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module main
	.cpu 68020
	.use tkvm.amigaos.runtime
	.use tkvm.amigaos.state
	.use tkvm.amigaos.control

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
PROGRAM_CAPACITY = 64
INPUT_CAPACITY = 8192
INPUT_BUFFER_BYTES = INPUT_CAPACITY + 1
OUTPUT_CAPACITY = CASE_CAPACITY * 4

	.section entry, kind=code
	.pub
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #RETURN_FAIL, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.w, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	moveq #0, d0
	jsr control.tkvmSetProgramStateTable68000
	bsr.w readInput
	bne.w closeDos
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
	movem.l (sp)+, d2-d7/a2-a6
	rts
	.bend  ; start

; Read the complete bounded case stream from the fixed Work: input path.
; Outputs: D0=0 success/1 failure; InputLength updated.
; Clobbers: D0-D4/A0/A6/CCR.
readInput	.block
	move.l #InputPath, d1
	move.l #MODE_OLDFILE, d2
	movea.l DosBase, a6
	jsr OPEN(a6)
	tst.l d0
	beq.s fail
	move.l d0, d4
	move.l d0, d1
	move.l #InputBuffer, d2
	move.l #INPUT_BUFFER_BYTES, d3
	jsr READ(a6)
	move.l d0, InputLength
	move.l d4, d1
	jsr CLOSE(a6)
	tst.l InputLength
	bmi.s fail
	cmpi.l #INPUT_CAPACITY, InputLength
	bhi.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; readInput

; Parse and execute all records. Multi-byte input and output fields are BE.
; Outputs: D0=0 success/1 structural failure; OutputLength updated.
; Clobbers: D0-D7/A0-A6/CCR.
evaluateCases	.block
	lea InputBuffer, a3
	movea.l a3, a4
	adda.l InputLength, a4
	movea.l a4, a0
	suba.l a3, a0
	cmpa.l #4, a0
	blo.w fail
	move.l (a3)+, d7
	cmpi.l #CASE_CAPACITY, d7
	bhi.w fail
	lea ResultBuffer, a5
caseLoop
	tst.l d7
	beq.w done
	movea.l a4, a0
	suba.l a3, a0
	cmpa.l #8, a0
	blo.w fail
	move.l (a3)+, d0
	move.l d0, state.TkvmStepBudget
	moveq #0, d3
	move.w (a3)+, d3
	cmpi.l #PROGRAM_CAPACITY, d3
	bhi.w fail
	moveq #0, d0
	move.w (a3)+, d0
	cmpi.l #1, d0
	bhi.w fail
	movea.l a3, a0
	adda.l d3, a0
	adda.l #2, a0
	cmpa.l a4, a0
	bhi.w fail
	move.l a0, NextCase
	moveq #0, d0
	; Source length is the BE word immediately before the program.
	move.w -2(a3), d0
	movea.l a3, a0
	adda.l d3, a0
	lea Tokens, a1
	lea Scratch, a2
	moveq #8, d1
	moveq #64, d2
	movem.l d7/a4-a5, -(sp)
	jsr runtime.tkvmRun68000
	movem.l (sp)+, d7/a4-a5
	move.l d0, (a5)+
	movea.l NextCase, a3
	subq.l #1, d7
	bra.w caseLoop
done
	cmpa.l a4, a3
	bne.s fail
	lea ResultBuffer, a0
	suba.l a0, a5
	move.l a5, OutputLength
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend

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
InputPath
	.byte "Work:tkvm-branch-cases.bin", 0
OutputPath
	.byte "Work:build/tkvm-branch-values.bin", 0
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
NextCase
	.res long, 1
Tokens
	.res byte, 160
Scratch
	.res byte, 64
InputBuffer
	.res byte, INPUT_BUFFER_BYTES
ResultBuffer
	.res byte, OUTPUT_CAPACITY
	.endsection

	.output "build/tkvm_branch_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
