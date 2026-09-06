; Focused direct native ExprVM signed-i64 evaluation harness.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module main
	.cpu 68020
	.use exprvm.amigaos.runtime

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
CASE_CAPACITY = 512
PROGRAM_CAPACITY = 256
INPUT_CAPACITY = 143360
INPUT_BUFFER_BYTES = INPUT_CAPACITY + 1
OUTPUT_CAPACITY = CASE_CAPACITY * 24

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
	beq.w casesDone
	movea.l a4, a0
	suba.l a3, a0
	cmpa.l #12, a0
	blo.w fail
	moveq #0, d6
	move.w (a3)+, d6
	cmpi.w #1, d6
	beq.s versionOk
	cmpi.w #2, d6
	bne.w fail
versionOk
	moveq #0, d0
	move.w (a3)+, d0
	cmpi.l #PROGRAM_CAPACITY, d0
	bhi.w fail
	move.l d0, ProgramLength
	move.l (a3)+, d2
	move.l d2, CurrentPc
	move.l (a3)+, d2
	move.l d2, SymbolValue
	move.l d0, d1
	addq.l #1, d1
	andi.l #$fffffffe, d1
	movea.l a4, a0
	suba.l a3, a0
	cmp.l a0, d1
	bhi.w fail
	move.w d6, runtime.ExprvmSelectedOpcodeVersion
	movea.l a3, a0
	move.l ProgramLength, d0
	lea SymbolName, a1
	lea SymbolValue, a2
	moveq #1, d1
	move.l CurrentPc, d2
	lea SymbolStable, a6
	jsr runtime.exprvmEvalProgramV1
	move.l d0, (a5)
	move.l d3, 12(a5)
	move.l d4, 16(a5)
	move.l d5, 20(a5)
	jsr runtime.exprvmGetLastResultHighV1
	move.l d0, 4(a5)
	move.l d1, 8(a5)
	tst.l (a5)
	beq.s resultReady
	; Keep the getter's actual high payload so stale-result leakage remains
	; observable; only the evaluator's unspecified failed low value is cleared.
	clr.l 12(a5)
resultReady
	adda.l #24, a5
	move.l ProgramLength, d0
	addq.l #1, d0
	andi.l #$fffffffe, d0
	adda.l d0, a3
	subq.l #1, d7
	bra.w caseLoop
casesDone
	cmpa.l a4, a3
	bne.s fail
	move.l #ResultBuffer, d0
	suba.l d0, a5
	move.l a5, OutputLength
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
InputPath
	.byte "Work:exprvm-i64-cases.bin", 0
OutputPath
	.byte "Work:build/exprvm-i64-values.bin", 0
SymbolName
	.byte 0
SymbolStable
	.byte 1
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
ProgramLength
	.res long, 1
CurrentPc
	.res long, 1
SymbolValue
	.res long, 1
InputBuffer
	.res byte, INPUT_BUFFER_BYTES
ResultBuffer
	.res byte, OUTPUT_CAPACITY
	.endsection

	.output "build/exprvm_i64_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
