; Native opasm struct directive routing.

	.module opasm.amigaos.flow_structs
	.cpu 68020

	.use opasm.amigaos.compile_values as compile_values

STRUCT_NAME_CAPACITY = 32
STRUCT_FIELD_CAPACITY = 8
STRUCT_SCOPED_REPEAT_CAPACITY = 8
STRUCT_LABEL_CAPACITY = 64

	.section code, kind=code
	.pub

; Reset struct-definition routing state at an assembly-pass boundary.
; Outputs: D0 = 0.
; Clobbers: D0/CCR.
; CCR: reflects D0 on return.
resetStateV1	.block
	movem.l d0/a0, -(sp)
	clr.w StructDefinitionActive.l
	clr.w StructFieldCount.l
	clr.l StructSize.l
	lea StructScopedRepeatActive.l, a0
	moveq #STRUCT_SCOPED_REPEAT_CAPACITY - 1, d0
clearScopedRepeats
	clr.b (a0)+
	dbra d0, clearScopedRepeats
	movem.l (sp)+, d0/a0
	moveq #0, d0
	rts
	.bend  ; resetStateV1

; Start one labeled scoped repetition.
; Inputs: D2.W = repetition depth; A0/D0 = base label.
; Outputs: D0 = 0 on success, 1 for malformed/capacity input.
beginScopedRepeatV1	.block
	movem.l d1-d4/a0-a2, -(sp)
	cmpi.w #STRUCT_SCOPED_REPEAT_CAPACITY, d2
	bhs.s scopedBeginFail
	tst.l d0
	beq.s scopedBeginFail
	cmpi.l #STRUCT_NAME_CAPACITY, d0
	bhs.s scopedBeginFail
	move.l d2, d3
	lsl.l #5, d3
	lea StructScopedRepeatNames.l, a1
	adda.l d3, a1
	move.l d0, d1
scopedBeginCopy
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bne.s scopedBeginCopy
	clr.b (a1)
	lea StructScopedRepeatActive.l, a2
	move.b #1, 0(a2, d2.w)
	move.l d2, d3
	add.w d3, d3
	lea StructScopedRepeatIteration.l, a2
	clr.w 0(a2, d3.l)
	moveq #0, d0
	bra.s scopedBeginReturn
scopedBeginFail
	moveq #1, d0
scopedBeginReturn
	movem.l (sp)+, d1-d4/a0-a2
	rts
	.bend  ; beginScopedRepeatV1

; Clear one repetition slot before reuse or after completion.
; Inputs: D2.W = repetition depth.
; Outputs: D0 = 0 on success, 1 for an invalid depth.
clearScopedRepeatV1	.block
	cmpi.w #STRUCT_SCOPED_REPEAT_CAPACITY, d2
	bhs.s scopedClearFail
	lea StructScopedRepeatActive.l, a0
	clr.b 0(a0, d2.w)
	moveq #0, d0
	rts
scopedClearFail
	moveq #1, d0
	rts
	.bend  ; clearScopedRepeatV1

; Advance the zero-based ordinal for one active scoped repetition.
; Inputs: D2.W = repetition depth.
; Outputs: D0 = 0 on success, 1 for an invalid/inactive depth.
advanceScopedRepeatV1	.block
	cmpi.w #STRUCT_SCOPED_REPEAT_CAPACITY, d2
	bhs.s scopedAdvanceFail
	lea StructScopedRepeatActive.l, a0
	tst.b 0(a0, d2.w)
	beq.s scopedAdvanceDone
	move.l d2, d0
	add.w d0, d0
	lea StructScopedRepeatIteration.l, a0
	addq.w #1, 0(a0, d0.l)
scopedAdvanceDone
	moveq #0, d0
	rts
scopedAdvanceFail
	moveq #1, d0
	rts
	.bend  ; advanceScopedRepeatV1

; Build `base[ordinal].member` for the active repetition at D2.
; Inputs: D2.W = repetition depth; A0/D0 = current statement label.
; Outputs: D0 = status; D3.W = 1 and A0/D1 = replacement when active,
;          otherwise D3.W = 0.
qualifyScopedRepeatLabelV1	.block
	movem.l d2/d4-d7/a1-a4, -(sp)
	clr.w d3
	cmpi.w #STRUCT_SCOPED_REPEAT_CAPACITY, d2
	bhs.w scopedQualifyFail
	tst.l d0
	beq.w scopedQualifyOk
	lea StructScopedRepeatActive.l, a1
	tst.b 0(a1, d2.w)
	beq.w scopedQualifyOk
	movea.l a0, a3
	move.l d0, d6
	movea.l a0, a4
	move.l d0, d7
scopedSuffixLoop
	tst.l d7
	beq.s scopedSuffixReady
	cmpi.b #'.', (a0)+
	bne.s scopedSuffixNext
	movea.l a0, a3
	move.l d7, d6
	subq.l #1, d6
scopedSuffixNext
	subq.l #1, d7
	bra.s scopedSuffixLoop
scopedSuffixReady
	move.l d2, d4
	lsl.l #5, d4
	lea StructScopedRepeatNames.l, a0
	adda.l d4, a0
	lea StructScopedRepeatLabelScratch.l, a1
	clr.l d5
scopedCopyBase
	move.b (a0)+, d4
	beq.s scopedOpenIndex
	cmpi.l #STRUCT_LABEL_CAPACITY - 1, d5
	bhs.w scopedQualifyFail
	move.b d4, (a1)+
	addq.l #1, d5
	bra.s scopedCopyBase
scopedOpenIndex
	move.b #'[', (a1)+
	addq.l #1, d5
	move.l d2, d4
	add.w d4, d4
	lea StructScopedRepeatIteration.l, a0
	moveq #0, d7
	move.w 0(a0, d4.l), d7
	moveq #0, d4
	moveq #0, d2
scopedThousands
	cmpi.w #1000, d7
	blo.s scopedHundreds
	subi.w #1000, d7
	addq.w #1, d4
	bra.s scopedThousands
scopedHundreds
	tst.w d4
	beq.s scopedHundredsCount
	move.b d4, d0
	addi.b #'0', d0
	move.b d0, (a1)+
	addq.l #1, d5
	moveq #1, d2
	moveq #0, d4
scopedHundredsCount
	cmpi.w #100, d7
	blo.s scopedHundredsWrite
	subi.w #100, d7
	addq.w #1, d4
	bra.s scopedHundredsCount
scopedHundredsWrite
	tst.w d4
	bne.s scopedWriteHundreds
	tst.w d2
	beq.s scopedTensCount
scopedWriteHundreds
	move.b d4, d0
	addi.b #'0', d0
	move.b d0, (a1)+
	addq.l #1, d5
	moveq #1, d2
	moveq #0, d4
scopedTensCount
	cmpi.w #10, d7
	blo.s scopedTensWrite
	subi.w #10, d7
	addq.w #1, d4
	bra.s scopedTensCount
scopedTensWrite
	tst.w d4
	bne.s scopedWriteTens
	tst.w d2
	beq.s scopedWriteOnes
scopedWriteTens
	move.b d4, d0
	addi.b #'0', d0
	move.b d0, (a1)+
	addq.l #1, d5
scopedWriteOnes
	addi.b #'0', d7
	move.b d7, (a1)+
	addq.l #1, d5
	move.b #']', (a1)+
	move.b #'.', (a1)+
	addq.l #2, d5
	add.l d6, d5
	cmpi.l #STRUCT_LABEL_CAPACITY, d5
	bhs.s scopedQualifyFail
	move.l d6, d7
scopedCopySuffix
	tst.l d7
	beq.s scopedQualifyDone
	move.b (a3)+, (a1)+
	subq.l #1, d7
	bra.s scopedCopySuffix
scopedQualifyDone
	clr.b (a1)
	lea StructScopedRepeatLabelScratch.l, a0
	move.l d5, d1
	moveq #1, d3
	moveq #0, d0
	bra.s scopedQualifyReturn
scopedQualifyOk
	moveq #0, d0
	bra.s scopedQualifyReturn
scopedQualifyFail
	moveq #1, d0
scopedQualifyReturn
	movem.l (sp)+, d2/d4-d7/a1-a4
	rts
	.bend  ; qualifyScopedRepeatLabelV1

; Classify one struct directive or struct-body field.
; Inputs: A0/D0 = mnemonic text.
; Outputs: D0 = 0; D3.W = 0 unhandled, 1 struct, 2 endstruct, 3 field,
;          or 4 invalid struct-body statement.
; Clobbers: D0-D6/A0-A2/CCR.
; CCR: reflects D0 on return.
routeDirectiveV1	.block
	movea.l a0, a2
	move.l d0, d6
	movea.l a2, a0
	move.l d6, d0
	lea StructMnemonicText, a1
	moveq #6, d1
	bsr.w structLineStartsWith
	bne.w structBegin
	movea.l a2, a0
	move.l d6, d0
	lea EndstructMnemonicText, a1
	moveq #9, d1
	bsr.w structLineStartsWith
	bne.w structEnd
	tst.w StructDefinitionActive.l
	beq.w structUnhandled
	movea.l a2, a0
	move.l d6, d0
	lea ByteMnemonicText, a1
	moveq #4, d1
	bsr.w structLineStartsWith
	bne.w structByteField
	movea.l a2, a0
	move.l d6, d0
	lea WordMnemonicText, a1
	moveq #4, d1
	bsr.w structLineStartsWith
	bne.w structWordField
	movea.l a2, a0
	move.l d6, d0
	lea LongMnemonicText, a1
	moveq #4, d1
	bsr.w structLineStartsWith
	bne.w structLongField
	movea.l a2, a0
	move.l d6, d0
	lea ResMnemonicText, a1
	moveq #3, d1
	bsr.w structLineStartsWith
	bne.w structByteField
	moveq #4, d3
	moveq #0, d0
	rts

structBegin
	moveq #1, d3
	moveq #0, d0
	rts
structEnd
	moveq #2, d3
	moveq #0, d0
	rts
structField
	moveq #3, d3
	moveq #0, d0
	rts
structByteField
	moveq #1, d4
	bra.s structField
structWordField
	moveq #2, d4
	bra.s structField
structLongField
	moveq #4, d4
	bra.s structField
structUnhandled
	clr.w d3
	moveq #0, d0
	rts
	.bend  ; routeDirectiveV1

; Enter one bounded struct definition.
; Inputs: A0/D0 = struct type name.
; Outputs: D0 = 0 on success, 1 for a nested or malformed definition.
; Clobbers: D0-D2/A0-A1/CCR.
; CCR: reflects D0 on return.
beginDefinitionV1	.block
	movem.l d1-d2/a0-a1, -(sp)
	tst.w StructDefinitionActive.l
	bne.s fail
	tst.l d0
	beq.s fail
	cmpi.l #STRUCT_NAME_CAPACITY, d0
	bhs.s fail
	lea StructName.l, a1
	move.l d0, d1
copyName
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bne.s copyName
	clr.b (a1)
	move.w #1, StructDefinitionActive.l
	clr.w StructFieldCount.l
	clr.l StructSize.l
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d2/a0-a1
	rts
	.bend  ; beginDefinitionV1

; Record one field in the active struct definition.
; Inputs: A0/D0 = field name; D1 = field byte width.
; Outputs: D0 = 0 on success, 1 for malformed input or capacity exhaustion.
; Clobbers: D0-D4/A0-A2/CCR.
; CCR: reflects D0 on return.
captureFieldV1	.block
	movem.l d2-d4/a0-a2, -(sp)
	tst.w StructDefinitionActive.l
	beq.w fail
	tst.l d0
	beq.w fail
	cmpi.l #STRUCT_NAME_CAPACITY, d0
	bhs.w fail
	tst.l d1
	beq.w fail
	moveq #0, d2
	move.w StructFieldCount.l, d2
	cmpi.w #STRUCT_FIELD_CAPACITY, d2
	bhs.w fail
	move.l d2, d3
	lsl.l #5, d3
	lea StructFieldNames.l, a1
	adda.l d3, a1
	move.l d0, d4
copyFieldName
	move.b (a0)+, (a1)+
	subq.l #1, d4
	bne.s copyFieldName
	clr.b (a1)
	lsl.l #2, d2
	lea StructFieldOffsets.l, a2
	adda.l d2, a2
	move.l StructSize.l, d3
	move.l d3, (a2)
	add.l d1, StructSize.l
	addq.w #1, StructFieldCount.l
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d2-d4/a0-a2
	rts
	.bend  ; captureFieldV1

; Leave one struct definition and bind its scalar byte size.
; Outputs: D0 = 0 on success, 1 without a matching definition.
; Clobbers: D0-D1/A0/CCR.
; CCR: reflects D0 on return.
endDefinitionV1	.block
	movem.l d1/a0, -(sp)
	tst.w StructDefinitionActive.l
	beq.s fail
	clr.w StructDefinitionActive.l
	lea StructName.l, a0
	moveq #0, d0
	moveq #STRUCT_NAME_CAPACITY - 1, d1
findNameEnd
	tst.b (a0, d0.l)
	beq.s bindSize
	addq.l #1, d0
	dbra d1, findNameEnd
	bra.s fail
bindSize
	move.l StructSize.l, d1
	jsr compile_values.upsertBindingV1
	bne.s fail
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1/a0
	rts
	.bend  ; endDefinitionV1

; Capture one `name .const/.var/.set Type { field: value, ... }` instance.
; Inputs: A0/D0 = instance name; A1/D1 = directive operand text.
; Outputs: D0 = 0 when captured and D3 = struct size; D0 = 1 when not typed.
; Clobbers: D0-D7/A0-A4/CCR.
; CCR: reflects D0 on return.
captureTypedInstanceV1	.block
	movem.l d1-d2/d4-d7/a0-a4, -(sp)
	movea.l a0, a4
	move.l d0, d6
	movea.l a1, a0
	move.l d1, d7
	bsr.w skipWhitespace
	tst.l d7
	beq.w fail
	movea.l a0, a2
	clr.l d2
typeNameLoop
	tst.l d7
	beq.s typeNameDone
	move.b (a0), d4
	cmpi.b #' ', d4
	beq.s typeNameDone
	cmpi.b #9, d4
	beq.s typeNameDone
	addq.l #1, a0
	subq.l #1, d7
	addq.l #1, d2
	bra.s typeNameLoop
typeNameDone
	tst.l d2
	beq.w fail
	movea.l a2, a0
	move.l d2, d0
	lea StructName.l, a1
	bsr.w nameEquals
	beq.w fail
	bsr.w skipWhitespace
	tst.l d7
	beq.w bindOffsets
	cmpi.b #'{', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d7
fieldLoop
	bsr.w skipWhitespace
	tst.l d7
	beq.w fail
	cmpi.b #'}', (a0)
	beq.w finishLiteral
	movea.l a0, a3
	clr.l d5
fieldNameLoop
	tst.l d7
	beq.w fail
	move.b (a0), d4
	cmpi.b #':', d4
	beq.s fieldNameDone
	cmpi.b #' ', d4
	beq.s fieldNameDone
	cmpi.b #9, d4
	beq.s fieldNameDone
	addq.l #1, a0
	subq.l #1, d7
	addq.l #1, d5
	bra.s fieldNameLoop
fieldNameDone
	tst.l d5
	beq.w fail
	bsr.w skipWhitespace
	tst.l d7
	beq.w fail
	cmpi.b #':', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d7
	bsr.w skipWhitespace
	move.l d5, -(sp)
	bsr.w parseNumber
	bne.w fieldParseFail
	move.l d3, -(sp)
	move.l a0, -(sp)
	movea.l a3, a0
	move.l 8(sp), d0
	bsr.w findFieldOffset
	bne.w discardFail
	movea.l (sp)+, a0
	move.l (sp)+, d1
	move.l (sp)+, d5
	move.l a0, -(sp)
	movea.l a4, a0
	move.l d6, d0
	movea.l a3, a1
	move.l d5, d2
	bsr.w bindMemberValue
	movea.l (sp)+, a0
	bne.w fail
	bsr.w skipWhitespace
	tst.l d7
	beq.w fail
	cmpi.b #'}', (a0)
	beq.s finishLiteral
	cmpi.b #',', (a0)
	bne.w fail
	addq.l #1, a0
	subq.l #1, d7
	bra.w fieldLoop
fieldParseFail
	adda.l #4, sp
	bra.w fail
discardFail
	adda.l #12, sp
	bra.w fail
finishLiteral
	addq.l #1, a0
	subq.l #1, d7
	bsr.w skipWhitespace
	tst.l d7
	bne.w fail
	bra.s success
bindOffsets
	clr.l d5
offsetLoop
	moveq #0, d2
	move.w StructFieldCount.l, d2
	cmp.l d2, d5
	bhs.s success
	move.l d5, d4
	lsl.l #5, d4
	lea StructFieldNames.l, a1
	adda.l d4, a1
	movea.l a4, a0
	move.l d6, d0
	movea.l a1, a2
	movea.l a1, a0
	bsr.w tokenLen
	move.l d0, d2
	move.l d5, d4
	lsl.l #2, d4
	lea StructFieldOffsets.l, a3
	move.l 0(a3, d4.l), d1
	movea.l a4, a0
	move.l d6, d0
	movea.l a2, a1
	bsr.w bindMemberValue
	bne.w fail
	addq.l #1, d5
	bra.s offsetLoop
success
	move.l StructSize.l, d3
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d1-d2/d4-d7/a0-a4
	rts
	.bend  ; captureTypedInstanceV1

; Match a directive mnemonic with optional leading dot and a token boundary.
; Inputs: A0/D0 = candidate text; A1/D1 = lowercase mnemonic text/length.
; Outputs: D0 = 1 on match, 0 otherwise.
; Clobbers: D0-D4/A0-A3/CCR.
; CCR: reflects D0 on return.
structLineStartsWith	.block
	movem.l d2-d4/a0-a3, -(sp)
	tst.l d0
	beq.w no
	cmpi.b #'.', (a0)
	bne.w compareStart
	addq.l #1, a0
	subq.l #1, d0
compareStart
	cmp.l d1, d0
	bcs.w no
	movea.l a0, a2
	movea.l a1, a3
	move.l d1, d2
	beq.w boundary
	subq.l #1, d2
loop
	move.b (a2)+, d3
	move.b (a3)+, d4
	cmpi.b #'A', d3
	bcs.w compare
	cmpi.b #'Z', d3
	bhi.w compare
	addi.b #32, d3
compare
	cmp.b d4, d3
	bne.w no
	dbra d2, loop
boundary
	cmp.l d1, d0
	beq.w yes
	move.b 0(a0, d1.l), d3
	cmpi.b #' ', d3
	beq.w yes
	cmpi.b #9, d3
	beq.w yes
	cmpi.b #';', d3
	beq.w yes
no
	movem.l (sp)+, d2-d4/a0-a3
	moveq #0, d0
	rts
yes
	movem.l (sp)+, d2-d4/a0-a3
	moveq #1, d0
	rts
	.bend  ; structLineStartsWith

; Find one active struct field by name.
; Inputs: A0/D0 = field name.
; Outputs: D0 = 0 on success; D3 = field offset. D0 = 1 when absent.
; Clobbers: D0-D2/A0-A1/CCR.
findFieldOffset	.block
	movea.l a0, a2
	move.l d0, d2
	clr.l d1
fieldScan
	cmp.w StructFieldCount.l, d1
	bhs.s fieldMissing
	move.l d1, d3
	lsl.l #5, d3
	lea StructFieldNames.l, a1
	adda.l d3, a1
	movea.l a2, a0
	move.l d2, d0
	bsr.w nameEquals
	bne.s fieldFound
	addq.l #1, d1
	bra.s fieldScan
fieldFound
	move.l d1, d3
	lsl.l #2, d3
	lea StructFieldOffsets.l, a0
	move.l 0(a0, d3.l), d3
	moveq #0, d0
	rts
fieldMissing
	moveq #1, d0
	rts
	.bend  ; findFieldOffset

; Build `instance.field` and store one scalar through compile_values.
; Inputs: A0/D0 = instance; A1/D2 = field; D1 = value.
; Outputs: D0 = status.
; Clobbers: D0-D5/A0-A2/CCR.
bindMemberValue	.block
	movem.l d1-d5/a0-a2, -(sp)
	move.l d1, d5
	lea StructMemberNameScratch, a2
	move.l d0, d3
copyInstance
	tst.l d3
	beq.s dot
	move.b (a0)+, (a2)+
	subq.l #1, d3
	bra.s copyInstance
dot
	move.b #'.', (a2)+
	move.l d2, d3
copyMember
	tst.l d3
	beq.s bind
	move.b (a1)+, (a2)+
	subq.l #1, d3
	bra.s copyMember
bind
	clr.b (a2)
	lea StructMemberNameScratch, a0
	move.l a2, d0
	sub.l a0, d0
	move.l d5, d1
	jsr compile_values.upsertBindingV1
	movem.l (sp)+, d1-d5/a0-a2
	rts
	.bend  ; bindMemberValue

skipWhitespace	.block
skipLoop
	tst.l d7
	beq.s skipDone
	cmpi.b #' ', (a0)
	beq.s skipOne
	cmpi.b #9, (a0)
	bne.s skipDone
skipOne
	addq.l #1, a0
	subq.l #1, d7
	bra.s skipLoop
skipDone
	rts
	.bend  ; skipWhitespace

parseNumber	.block
	clr.l d3
	tst.l d7
	beq.s numberFail
numberLoop
	tst.l d7
	beq.s numberOk
	moveq #0, d4
	move.b (a0), d4
	cmpi.b #'0', d4
	blo.s numberOk
	cmpi.b #'9', d4
	bhi.s numberOk
	subi.b #'0', d4
	move.l d3, d5
	lsl.l #3, d3
	add.l d5, d3
	add.l d5, d3
	add.l d4, d3
	addq.l #1, a0
	subq.l #1, d7
	bra.s numberLoop
numberOk
	moveq #0, d0
	rts
numberFail
	moveq #1, d0
	rts
	.bend  ; parseNumber

tokenLen	.block
	clr.l d0
tokenLenLoop
	tst.b (a0, d0.l)
	beq.s tokenLenDone
	addq.l #1, d0
	bra.s tokenLenLoop
tokenLenDone
	rts
	.bend  ; tokenLen

nameEquals	.block
	tst.l d0
	beq.s nameEnd
nameLoop
	move.b (a0)+, d3
	move.b (a1)+, d4
	cmpi.b #'A', d3
	bcs.s compare
	cmpi.b #'Z', d3
	bhi.s compare
	addi.b #32, d3
compare
	cmpi.b #'A', d4
	bcs.s compareNormalized
	cmpi.b #'Z', d4
	bhi.s compareNormalized
	addi.b #32, d4
compareNormalized
	cmp.b d4, d3
	bne.s nameNo
	subq.l #1, d0
	bne.s nameLoop
nameEnd
	tst.b (a1)
	bne.s nameNo
	moveq #1, d0
	rts
nameNo
	moveq #0, d0
	rts
	.bend  ; nameEquals

	.endsection
	.section data, kind=data
StructMnemonicText
	.byte "struct", 0
EndstructMnemonicText
	.byte "endstruct", 0
ByteMnemonicText
	.byte "byte", 0
WordMnemonicText
	.byte "word", 0
LongMnemonicText
	.byte "long", 0
ResMnemonicText
	.byte "res", 0
	.endsection
	.section bss, kind=bss
StructDefinitionActive
	.res byte, 2
StructFieldCount
	.res word, 1
StructSize
	.res long, 1
StructName
	.res byte, STRUCT_NAME_CAPACITY
StructFieldNames
	.res byte, STRUCT_FIELD_CAPACITY * STRUCT_NAME_CAPACITY
StructFieldOffsets
	.res long, STRUCT_FIELD_CAPACITY
StructMemberNameScratch
	.res byte, (STRUCT_NAME_CAPACITY * 2) + 2
StructScopedRepeatActive
	.res byte, STRUCT_SCOPED_REPEAT_CAPACITY
StructScopedRepeatIteration
	.res word, STRUCT_SCOPED_REPEAT_CAPACITY
StructScopedRepeatNames
	.res byte, STRUCT_NAME_CAPACITY * STRUCT_SCOPED_REPEAT_CAPACITY
StructScopedRepeatLabelScratch
	.res byte, STRUCT_LABEL_CAPACITY
	.endsection
	.endmodule
