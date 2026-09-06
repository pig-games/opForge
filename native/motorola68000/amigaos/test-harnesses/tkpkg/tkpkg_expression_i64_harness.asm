; Direct typed expression-service boundary proof without package/CPU selection.
; This harness sets only the selected-state bit and explicit neutral contract
; versions; it does not prove the full tkpkg service facade.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module main
	.cpu 68020
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers
	.use tkpkg.amigaos.expression_service as expression

SYS_BASE = 4
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
OPEN = -30
CLOSE = -36
READ = -42
WRITE = -48
OUTPUT = -60
MODE_OLDFILE = 1005
MODE_NEWFILE = 1006
RETURN_FAIL = 20
CASE_CAPACITY = 64
EXPRESSION_CAPACITY = 256
INPUT_CAPACITY = 17412
INPUT_BUFFER_BYTES = INPUT_CAPACITY + 1
OUTPUT_RECORD_BYTES = 64
OUTPUT_CAPACITY = CASE_CAPACITY * OUTPUT_RECORD_BYTES
EXTENSION_BUFFER_BYTES = 40

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
	bset #1, buffers.PackageStateFlags
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

; Input records: BE version, extension length, expression length, reserved zero,
; current PC, expression bytes, then one pad byte when needed.
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
.ifdef OPFORGE_EXPRESSION_I64_EXPECT_FAILURE
	cmpi.l #1, d7
	bne.w fail
.endif
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
	moveq #0, d5
	move.w (a3)+, d5
	cmpi.w #16, d5
	blo.w fail
	cmpi.w #EXTENSION_BUFFER_BYTES, d5
	bhi.w fail
	moveq #0, d0
	move.w (a3)+, d0
	cmpi.l #EXPRESSION_CAPACITY, d0
	bhi.w fail
	move.l d0, ExpressionLength
	tst.w (a3)+
	bne.w fail
	move.l (a3)+, d2
	move.l d2, CurrentPc
	move.l d0, d1
	addq.l #1, d1
	andi.l #$fffffffe, d1
	movea.l a4, a0
	suba.l a3, a0
	cmp.l a0, d1
	bhi.w fail
	bsr.w prepareBuffers
	moveq #1, d4
	move.l d6, d5
	lea ControlBlock, a0
	movem.l d6-d7/a3-a5, -(sp)
	jsr expression.prepareV1
	movem.l (sp)+, d6-d7/a3-a5
	move.l d0, (a5)
.ifdef OPFORGE_EXPRESSION_I64_EXPECT_FAILURE
	tst.l d0
	beq.s negativePrepareOk
	bsr.w writeFailureDiagnostic
	bra.w fail
negativePrepareOk
.endif
	moveq #-1, d2
	move.l d2, 4(a5)
	clr.l 8(a5)
	clr.l 12(a5)
	clr.l 16(a5)
	clr.l 20(a5)
	move.l ExtensionBuffer + 16, d2
	move.l d2, 12(a5)
	move.l ExtensionBuffer + 20, d2
	move.l d2, 16(a5)
	move.l ExtensionBuffer + 24, d2
	move.l d2, 20(a5)
	move.l ExtensionBuffer + 32, d2
	move.l d2, 56(a5)
	move.l ExtensionBuffer + 36, d2
	move.l d2, 60(a5)
	lea 24(a5), a1
	moveq #7, d2
clearOutputText
	clr.l (a1)+
	dbf d2, clearOutputText
	tst.l d0
	bne.s recordReady
	moveq #1, d4
	move.l d6, d5
	movem.l d6-d7/a3-a5, -(sp)
	jsr expression.executePreparedV1
	movem.l (sp)+, d6-d7/a3-a5
	move.l d0, 4(a5)
	move.l d1, 8(a5)
.ifdef OPFORGE_EXPRESSION_I64_EXPECT_FAILURE
	tst.l d0
	beq.s negativeExecuteUnexpectedSuccess
	bsr.w writeFailureDiagnostic
	bra.w fail
negativeExecuteUnexpectedSuccess
.endif
	move.l ExtensionBuffer + 16, d2
	move.l d2, 12(a5)
	move.l ExtensionBuffer + 20, d2
	move.l d2, 16(a5)
	move.l ExtensionBuffer + 24, d2
	move.l d2, 20(a5)
	move.l ExtensionBuffer + 32, d2
	move.l d2, 56(a5)
	move.l ExtensionBuffer + 36, d2
	move.l d2, 60(a5)
	tst.l d0
	bne.s recordReady
	cmpi.w #32, d1
	bhi.w fail
	lea buffers.LastErrorBuffer, a0
	lea 24(a5), a1
	move.w d1, d2
	beq.s recordReady
	subq.w #1, d2
copyOutputText
	move.b (a0)+, (a1)+
	dbf d2, copyOutputText
recordReady
	adda.l #OUTPUT_RECORD_BYTES, a5
	move.l ExpressionLength, d0
	addq.l #1, d0
	andi.l #$fffffffe, d0
	adda.l d0, a3
	subq.l #1, d7
	bra.w caseLoop
casesDone
	cmpa.l a4, a3
	bne.s fail
.ifdef OPFORGE_EXPRESSION_I64_EXPECT_FAILURE
	; A negative-mode service success is itself a failed proof.
	moveq #1, d0
	rts
.endif
	move.l #ResultBuffer, d0
	suba.l d0, a5
	move.l a5, OutputLength
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; evaluateCases

; Build one request/control block and seed extension result/guard canaries.
; Inputs: A3 expression, D5 extension length, CurrentPc/ExpressionLength.
; Clobbers: D0-D3/A0-A2/CCR.
prepareBuffers	.block
	lea ControlBlock, a0
	moveq #7, d0
clearControl
	clr.l (a0)+
	dbf d0, clearControl
	lea ExtensionBuffer, a0
	moveq #9, d0
clearExtension
	clr.l (a0)+
	dbf d0, clearExtension
	move.l #$11223344, ExtensionBuffer + 16
	move.l #$55667788, ExtensionBuffer + 20
	move.l #$99aabbcc, ExtensionBuffer + 24
	clr.l ExtensionBuffer + 28
	move.l #$ddeeff00, ExtensionBuffer + 32
	move.l #$a1b2c3d4, ExtensionBuffer + 36
	lea RequestBuffer, a1
	clr.l (a1)+
	move.b #1, (a1)+
	clr.b (a1)+
	move.l ExpressionLength, d0
	addq.w #1, d0
	move.b d0, (a1)+
	lsr.w #8, d0
	move.b d0, (a1)+
	clr.b (a1)+
	movea.l a3, a0
	move.l ExpressionLength, d0
	beq.s requestReady
	subq.w #1, d0
copyExpression
	move.b (a0)+, (a1)+
	dbf d0, copyExpression
requestReady
	lea ControlBlock, a0
	lea RequestBuffer, a1
	move.l a1, d0
	move.l a0, d1
	sub.l d1, d0
	move.b d0, abi.CB_INPUT_PTR(a0)
	lsr.w #8, d0
	move.b d0, abi.CB_INPUT_PTR + 1(a0)
	move.l ExpressionLength, d0
	addi.w #9, d0
	move.b d0, abi.CB_INPUT_LEN(a0)
	lsr.w #8, d0
	move.b d0, abi.CB_INPUT_LEN + 1(a0)
	lea ExtensionBuffer, a1
	move.l a1, d0
	move.l a0, d1
	sub.l d1, d0
	move.b d0, abi.CB_EXTENSION_PTR(a0)
	lsr.w #8, d0
	move.b d0, abi.CB_EXTENSION_PTR + 1(a0)
	move.w d5, d0
	move.b d0, abi.CB_EXTENSION_LEN(a0)
	lsr.w #8, d0
	move.b d0, abi.CB_EXTENSION_LEN + 1(a0)
	lea EmptyNames, a1
	lea EmptyValues, a2
	lea ExtensionBuffer, a0
	move.l a1, (a0)+
	move.l a2, (a0)+
	clr.l (a0)+
	move.l CurrentPc, d0
	move.l d0, (a0)
	rts
	.bend  ; prepareBuffers

; Forward the service-owned diagnostic bytes unchanged to the Shell stream.
; Inputs: A1/D1 = diagnostic pointer/length returned by prepare/execute.
; Outputs: D0=0. Clobbers D0-D3/A6/CCR.
writeFailureDiagnostic	.block
	move.l a1, d2
	move.l d1, d3
	movea.l DosBase, a6
	jsr OUTPUT(a6)
	move.l d0, d1
	jsr WRITE(a6)
	move.l d0, -(sp)
	movea.l DosBase, a6
	jsr OUTPUT(a6)
	move.l d0, d1
	move.l #Newline, d2
	moveq #1, d3
	jsr WRITE(a6)
	move.l (sp)+, d0
	moveq #0, d0
	rts
	.bend  ; writeFailureDiagnostic

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
	.byte "Work:expression-i64-cases.bin", 0
OutputPath
	.byte "Work:build/expression-i64-values.bin", 0
EmptyNames
	.byte 0
EmptyValues
	.long 0
Newline
	.byte 10
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
ExpressionLength
	.res long, 1
CurrentPc
	.res long, 1
ControlBlock
	.res byte, abi.NATIVE_CONTROL_BLOCK_SIZE_V1
RequestBuffer
	.res byte, 9 + EXPRESSION_CAPACITY
ExtensionBuffer
	.res byte, EXTENSION_BUFFER_BYTES
InputBuffer
	.res byte, INPUT_BUFFER_BYTES
ResultBuffer
	.res byte, OUTPUT_CAPACITY
	.endsection

	.output "build/tkpkg_expression_i64_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
