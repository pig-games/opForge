; Component proof: numeric registry API and exact ordinary-Rust text payload.
; This executable does not parse source directives; language integration is separate.
.module main
.cpu 68020
.use opforge.assembler.language.text as text
.use opforge.assembler.language.text_layout as layout
.use opforge.assembler.language.text_registry as registry
.use opforge.assembler.output.io as io
; Amiga Exec library ABI used only by this standalone harness.
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
RESULT_CAPACITY = 512
ASCII_ID = 17
PETSCII_ID = 23
GAME_ID = 31
CLONE_ID = 37
SHIFTED_ID = 41
.section entry, kind=code
.pub
; Amiga Shell entry; D0=0 or 20. Preserve public registers and release every path.
start	.block
	movem.l d1-d7/a0-a6, -(sp)
	jsr exercise
	move.l d0, d7
	lea State, a0
	jsr text.release
	tst.l d7
	bne.w failed
	jsr writeResults
	tst.l d0
	bne.w failed
	moveq #0, d0
	bra.w done
failed
	moveq #20, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
.bend  ; start
.endsection
.section code, kind=code
.priv
exercise	.block
	lea State, a0
	moveq #ASCII_ID, d1
	moveq #PETSCII_ID, d2
	jsr text.init
	bne.w failed
	moveq #PETSCII_ID, d1
	jsr text.select
	bne.w failed
	lea Builtin, a1
	moveq #4, d1
	bsr.w appendResult
	bne.w failed
	moveq #GAME_ID, d1
	moveq #0, d2
	jsr registry.begin
	bne.w failed
	moveq #'A', d1
	moveq #'Z', d2
	moveq #1, d3
	jsr registry.defineRange
	bne.w failed
	moveq #'x', d1
	moveq #'y', d2
	moveq #$40, d3
	jsr registry.defineRange
	bne.w failed
	moveq #'!', d1
	moveq #'!', d2
	move.l #$80, d3
	jsr registry.defineRange
	bne.w failed
	moveq #'?', d1
	moveq #'?', d2
	move.l #$81, d3
	jsr registry.defineRange
	bne.w failed
	lea Cr, a1
	moveq #4, d1
	lea Cr13, a2
	moveq #1, d2
	jsr registry.defineEscape
	bne.w failed
	lea Erase, a1
	moveq #7, d1
	lea Empty, a2
	moveq #0, d2
	jsr registry.defineEscape
	bne.w failed
	lea LongPattern, a1
	moveq #LongPatternEnd-LongPattern, d1
	lea LongReplacement, a2
	moveq #LongReplacementEnd-LongReplacement, d2
	jsr registry.defineEscape
	bne.w failed
	moveq #0, d6
smallEscape
	move.l d6, d0
	mulu.w #3, d0
	lea SmallPatterns, a1
	adda.l d0, a1
	moveq #3, d1
	lea SmallValues, a2
	adda.l d6, a2
	moveq #1, d2
	jsr registry.defineEscape
	bne.w failed
	addq.l #1, d6
	cmpi.l #9, d6
	blo.w smallEscape
	lea Cr, a1
	moveq #4, d1
	lea Cr10, a2
	moveq #1, d2
	jsr registry.defineEscape
	bne.w failed
	lea Longest, a1
	moveq #2, d1
	lea Ff, a2
	moveq #1, d2
	jsr registry.defineEscape
	bne.w failed
	; Known negative boundaries must reject without damaging the live registry.
	moveq #90, d1
	moveq #65, d2
	moveq #1, d3
	jsr registry.defineRange
	cmpi.l #layout.STATUS_INVALID, d0
	bne.w failed
	moveq #65, d1
	moveq #65, d2
	moveq #1, d3
	jsr registry.defineRange
	cmpi.l #layout.STATUS_DUPLICATE, d0
	bne.w failed
	lea Empty, a1
	moveq #0, d1
	lea Ff, a2
	moveq #1, d2
	jsr registry.defineEscape
	cmpi.l #layout.STATUS_INVALID, d0
	bne.w failed
	jsr registry.endDefinition
	bne.w failed
	moveq #GAME_ID, d1
	jsr text.select
	bne.w failed
	lea GameInput, a1
	moveq #GameInputEnd-GameInput, d1
	bsr.w appendResult
	bne.w failed
	moveq #CLONE_ID, d1
	moveq #GAME_ID, d2
	jsr registry.begin
	bne.w failed
	lea Cr, a1
	moveq #4, d1
	lea Cr11, a2
	moveq #1, d2
	jsr registry.defineEscape
	bne.w failed
	jsr registry.endDefinition
	bne.w failed
	moveq #CLONE_ID, d1
	jsr text.select
	bne.w failed
	lea CloneInput, a1
	moveq #CloneInputEnd-CloneInput, d1
	bsr.w appendResult
	bne.w failed
	moveq #GAME_ID, d1
	jsr text.select
	bne.w failed
	lea CloneInput, a1
	moveq #CloneInputEnd-CloneInput, d1
	bsr.w appendResult
	bne.w failed
	moveq #SHIFTED_ID, d1
	moveq #PETSCII_ID, d2
	jsr registry.begin
	bne.w failed
	lea Home, a1
	moveq #6, d1
	lea Home19, a2
	moveq #1, d2
	jsr registry.defineEscape
	bne.w failed
	jsr registry.endDefinition
	bne.w failed
	moveq #SHIFTED_ID, d1
	jsr text.select
	bne.w failed
	lea ShiftedInput, a1
	moveq #ShiftedInputEnd-ShiftedInput, d1
	bsr.w appendResult
	bne.w failed
	; Profile reset restores ASCII and retains definitions.
	jsr text.resetProfile
	bne.w failed
	lea Az, a1
	moveq #2, d1
	bsr.w appendResult
	bne.w failed
	moveq #GAME_ID, d1
	jsr text.select
	bne.w failed
	lea CloneInput, a1
	moveq #CloneInputEnd-CloneInput, d1
	bsr.w appendResult
	bne.w failed
	moveq #ASCII_ID, d1
	jsr text.select
	bne.w failed
	lea HighByte, a1
	moveq #1, d1
	lea Probe, a2
	moveq #1, d2
	jsr text.encode
	cmpi.l #layout.STATUS_MISSING, d0
	bne.w failed
	lea Az, a1
	moveq #2, d1
	lea Probe, a2
	moveq #1, d2
	jsr text.encode
	cmpi.l #layout.STATUS_OVERFLOW, d0
	bne.w failed
	move.l #$10000, d1
	jsr text.select
	cmpi.l #layout.STATUS_INVALID, d0
	bne.w failed
	; Request exceeds the combined arena budget before reading caller payload.
	moveq #GAME_ID, d1
	moveq #0, d2
	jsr registry.begin
	bne.w failed
	lea Cr, a1
	move.l #layout.BUDGET, d1
	lea Empty, a2
	moveq #0, d2
	jsr registry.defineEscape
	cmpi.l #layout.STATUS_OVERFLOW, d0
	bne.w failed
	jsr registry.endDefinition
	bne.w failed
	moveq #0, d0
	rts
failed
	moveq #20, d0
	rts
.bend  ; exercise

; A0=State,A1/D1=input. Append encoded bytes to bounded result buffer.
; D0/CCR=status; preserve other registers (including the owner's state pointer).
appendResult	.block
	movem.l d1-d2/a2, -(sp)
	lea Results, a2
	adda.l ResultsBytes, a2
	move.l #RESULT_CAPACITY, d2
	sub.l ResultsBytes, d2
	jsr text.encode
	bne.w done
	add.l d1, ResultsBytes
done
	movem.l (sp)+, d1-d2/a2
	tst.l d0
	rts
.bend  ; appendResult

; Write the complete encoded payload through the maintained short-write owner.
; D0/CCR=status. The Shell entry preserves registers and handles all failures.
writeResults	.block
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	tst.l d0
	beq.w failed
	movea.l d0, a5
	movea.l a5, a6
	lea FileFrame, a0
	move.l #OutputName, io.Frame.Path(a0)
	move.l #Results, io.Frame.Data(a0)
	move.l ResultsBytes, io.Frame.Bytes(a0)
	jsr io.write
	move.l d0, d7
	movea.l a5, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
	move.l d7, d0
	tst.l d0
	rts
failed
	moveq #20, d0
	rts
.bend  ; writeResults
.endsection
.section data, kind=data
Builtin	.byte "Az09"
Az	.byte "Az"
Cr	.byte "{cr}"
Cr13	.byte 13
Cr10	.byte 10
Cr11	.byte 11
Erase	.byte "{erase}"
Empty	.byte 0
LongPattern	.byte "{long-pattern-over-sixteen}"
LongPatternEnd
LongReplacement	.byte "replacement-over-sixteen"
LongReplacementEnd
SmallPatterns	.byte "{1}{2}{3}{4}{5}{6}{7}{8}{9}"
SmallValues	.byte 1, 2, 3, 4, 5, 6, 7, 8, 9
Longest	.byte "A!"
Ff	.byte $ff
Home	.byte "{home}"
Home19	.byte 19
GameInput	.byte "A{cr}xy!?{erase}{long-pattern-over-sixteen}{9}A!"
GameInputEnd
CloneInput	.byte "A{cr}xy{9}"
CloneInputEnd
ShiftedInput	.byte "Az{home}"
ShiftedInputEnd
HighByte	.byte $80
DosName	.byte "dos.library", 0
OutputName	.byte "Work:build/text-results.bin", 0
.endsection
.section bss, kind=bss
.align 4
State	.res byte, layout.STATE_BYTES
Results	.res byte, RESULT_CAPACITY
ResultsBytes	.res long, 1
Probe	.res byte, 2
FileFrame	.res byte, io.FRAME_BYTES
.endsection
.output "text-component.hunk", format=hunk, sections=entry, code, data, bss
.endmodule
.end
