; Focused conditional-branch tokenizer VM contract harness.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module main
	.cpu 68020
	.use tkvm.amigaos.runtime
	.use tkvm.amigaos.state
	.use tkvm.amigaos.control
	.use tkvm.amigaos.fragments

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
PROGRAM_CAPACITY = 256
SOURCE_CAPACITY = 256
SOURCE_CAPTURE_NUMERIC = $8000
SOURCE_CAPACITY_PROBE = $4000
SOURCE_RECIPE_CAPTURE = $2000
SOURCE_LEXICAL_CAPTURE = $1000
SOURCE_FRAGMENT_INPUT = $0800
SOURCE_FRAGMENT_INVALID = $0400
SOURCE_LENGTH_MASK = $03ff
LEXICAL_KIND_CAPACITY = 8
RECIPE_CAPTURE_BYTES = 268
RECIPE_PAYLOAD_BYTES = 256
PROBE_SCRATCH_CAPACITY = 14
SCRATCH_CAPACITY = 1024
SECOND_TOKEN_STATUS = runtime.TOKEN_RECORD_SIZE+2
INPUT_CAPACITY = 8192
INPUT_BUFFER_BYTES = INPUT_CAPACITY + 1
OUTPUT_CAPACITY = CASE_CAPACITY * 344

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
	move.w d0, CaptureFlags
	andi.l #SOURCE_LENGTH_MASK, d0
	cmpi.l #SOURCE_CAPACITY, d0
	bhi.w fail
	move.l d0, SourceLength
	; Preserve the original stream's minimum two source padding bytes.
	addq.l #1, d0
	andi.l #$fffffffe, d0
	cmpi.l #2, d0
	bhs.w paddingReady
	moveq #2, d0
paddingReady
	movea.l a3, a0
	adda.l d3, a0
	adda.l d0, a0
	cmpa.l a4, a0
	bhi.w fail
	move.l a0, NextCase
	move.l SourceLength, d0
	movea.l a3, a0
	adda.l d3, a0
	lea Tokens, a1
	lea Scratch, a2
	moveq #8, d1
	move.l #SCRATCH_CAPACITY, d2
	btst #6, CaptureFlags
	beq.w scratchReady
	moveq #PROBE_SCRATCH_CAPACITY, d2
scratchReady
	movem.l d7/a4-a5, -(sp)
	btst #3, CaptureFlags
	beq.w contiguousInput
	lea FragmentViews, a4
	move.l a0, fragments.Fragment.Bytes(a4)
	move.l d0, d4
	lsr.l #1, d4
	move.l d4, fragments.Fragment.Length(a4)
	adda.l d4, a0
	move.l a0, fragments.FRAGMENT_BYTES+fragments.Fragment.Bytes(a4)
	move.l d0, d5
	sub.l d4, d5
	move.l d5, fragments.FRAGMENT_BYTES+fragments.Fragment.Length(a4)
	lea FragmentFrame, a0
	move.l a4, fragments.Frame.Fragments(a0)
	move.l #2, fragments.Frame.Count(a0)
	move.l d0, fragments.Frame.InputBytes(a0)
	btst #2, CaptureFlags
	beq.w fragmentRequestReady
	addq.l #1, fragments.Frame.InputBytes(a0)
fragmentRequestReady
	move.l a1, fragments.Frame.Tokens(a0)
	move.l d1, fragments.Frame.TokenCapacity(a0)
	move.l a2, fragments.Frame.Lexemes(a0)
	move.l d2, fragments.Frame.LexemeCapacity(a0)
	move.l a3, fragments.Frame.Program(a0)
	move.l d3, fragments.Frame.ProgramBytes(a0)
	btst #2, CaptureFlags
	beq.w fragmentBuffersReady
	lea Tokens, a0
	move.l #160+SCRATCH_CAPACITY-1, d4
fillFragmentSentinel
	move.b #$a5, (a0)+
	dbra d4, fillFragmentSentinel
fragmentBuffersReady
	lea FragmentFrame, a0
	jsr fragments.run
	btst #2, CaptureFlags
	beq.w tokenized
	movem.l d0-d3, -(sp)
	lea Tokens, a0
	move.l #160+SCRATCH_CAPACITY-1, d4
checkFragmentSentinel
	cmpi.b #$a5, (a0)+
	bne.w fragmentSentinelChanged
	dbra d4, checkFragmentSentinel
	movem.l (sp)+, d0-d3
	bra.w tokenized
fragmentSentinelChanged
	movem.l (sp)+, d0-d3
	movem.l (sp)+, d7/a4-a5
	bra.w fail
contiguousInput
	jsr runtime.tkvmRun68000
tokenized
	movem.l (sp)+, d7/a4-a5
	move.l d1, ReturnedCount
	move.l d0, (a5)+
	tst.w CaptureFlags
	bpl.w captured
	clr.l (a5)+
	clr.l (a5)+
	clr.l (a5)+
	tst.l d0
	bne.w captured
	tst.l d1
	beq.w captured
	lea Tokens, a0
	cmpi.w #runtime.TK_KIND_NUMBER, (a0)
	bne.w captured
	moveq #0, d0
	move.w 2(a0), d0
	move.l d0, -12(a5)
	cmpi.w #runtime.NUMBER_VALID, d0
	bne.w captured
	move.l 12(a0), d0
	add.l 16(a0), d0
	bcs.w fail
	move.l d0, d1
	addq.l #8, d1
	bcs.w fail
	cmp.l d3, d1
	bhi.w fail
	lea Scratch, a0
	adda.l d0, a0
	move.l (a0)+, -8(a5)
	move.l (a0), -4(a5)
captured
	btst #6, CaptureFlags
	beq.w probeCaptured
	; Capacity probes append cursor, committed bytes, two record statuses, u64.
	move.l d2, (a5)+
	move.l d3, (a5)+
	lea Tokens, a0
	moveq #0, d0
	move.w 2(a0), d0
	move.l d0, (a5)+
	moveq #0, d0
	move.w SECOND_TOKEN_STATUS(a0), d0
	move.l d0, (a5)+
	clr.l (a5)+
	clr.l (a5)+
	cmpi.w #runtime.NUMBER_VALID, 2(a0)
	bne.w probeCaptured
	move.l 12(a0), d0
	add.l 16(a0), d0
	bcs.w fail
	move.l d0, d1
	addq.l #8, d1
	bcs.w fail
	cmp.l d3, d1
	bhi.w fail
	lea Scratch, a0
	adda.l d0, a0
	move.l (a0)+, -8(a5)
	move.l (a0), -4(a5)
probeCaptured
	btst #5, CaptureFlags
	beq.w recipeCaptured
	movea.l a5, a1
	move.l #RECIPE_CAPTURE_BYTES/4-1, d0
clearRecipe
	clr.l (a5)+
	dbra d0, clearRecipe
	lea Tokens, a0
	moveq #0, d0
	move.w 2(a0), d0
	move.l d0, (a1)
	btst #15, d0
	beq.w recipeCaptured
	move.l 12(a0), d0
	add.l 16(a0), d0
	bcs.w fail
	move.l d0, d1
	addq.l #2, d1
	bcs.w fail
	cmp.l d3, d1
	bhi.w fail
	lea Scratch, a0
	adda.l d0, a0
	moveq #0, d0
	move.b (a0)+, d0
	move.l d0, 4(a1)
	moveq #0, d0
	move.b (a0)+, d0
	move.l d0, 8(a1)
	add.l d0, d1
	cmp.l d3, d1
	bhi.w fail
	lea 12(a1), a1
copyRecipe
	tst.l d0
	beq.w recipeCaptured
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bra.w copyRecipe
recipeCaptured
	btst #4, CaptureFlags
	beq.w lexicalCaptured
	move.l ReturnedCount, (a5)+
	lea Tokens, a0
	moveq #0, d1
captureKind
	moveq #-1, d0
	cmp.l ReturnedCount, d1
	bcc.w storeKind
	moveq #0, d0
	move.w (a0), d0
storeKind
	move.l d0, (a5)+
	adda.w #runtime.TOKEN_RECORD_SIZE, a0
	addq.l #1, d1
	cmpi.l #LEXICAL_KIND_CAPACITY, d1
	blo.w captureKind
lexicalCaptured
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
SourceLength
	.res long, 1
ReturnedCount
	.res long, 1
CaptureFlags
	.res word, 1
FragmentFrame
	.res byte, fragments.FRAME_BYTES
FragmentViews
	.res byte, 2*fragments.FRAGMENT_BYTES
Tokens
	.res byte, 160
Scratch
	.res byte, SCRATCH_CAPACITY
InputBuffer
	.res byte, INPUT_BUFFER_BYTES
ResultBuffer
	.res byte, OUTPUT_CAPACITY
	.endsection

	.output "build/tkvm_branch_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
