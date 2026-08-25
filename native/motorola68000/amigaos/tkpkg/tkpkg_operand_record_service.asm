; CPU-neutral scoped operand-record lookup and base-record execution.
; @opforge-owner: tkpkg.amigaos.operand_record_service
; @opforge-slice: documentation/plans/slices/native-porting-slice-effective-address-record-v1.toml

	.module tkpkg.amigaos.operand_record_service
	.cpu 68020
	.pub
	.use tkpkg.amigaos.abi
	.use tkpkg.amigaos.buffers

COMPACT_RECORD_CHUNK_VERSION_V1      = 1
OPERAND_RECORD_SCHEMA_VERSION_V1     = 1
OPERAND_RECORD_OP_REGISTER           = $01
OPERAND_RECORD_OP_INDIRECT           = $02
OPERAND_RECORD_OP_DISPLACEMENT       = $03
OPERAND_RECORD_OP_INDEXED            = $04
OPERAND_RECORD_OP_ABSOLUTE           = $05
OPERAND_RECORD_OP_IMMEDIATE          = $06
OPERAND_RECORD_OP_NESTED_ADDRESS     = $07
OPERAND_RECORD_OP_REGISTER_PAIR      = $08
OPERAND_RECORD_OP_REGISTER_RANGE     = $09
OPERAND_RECORD_OP_REGISTER_LIST      = $0A
OPERAND_RECORD_OP_FIELD              = $0B
OPERAND_RECORD_OP_COMPOSITE          = $0C
OPERAND_RECORD_OP_END                = $FF
SCOPED_OWNER_FAMILY                  = 0
SCOPED_OWNER_CPU                     = 1
SCOPED_OWNER_DIALECT                 = 2
NO_OWNER_INDEX                       = $FFFF
MAX_REGISTER_INPUTS_V1               = 32
MAX_VALUE_INPUTS_V1                  = 16
MAX_OWNER_COUNT_V1                   = 32
MAX_PROGRAM_COUNT_V1                 = 256
NEEDS_PIPELINE_TEXT_LEN              = 45
BAD_REQUEST_TEXT_LEN                 = 40
MALFORMED_CHUNK_TEXT_LEN             = 40
MISSING_PROGRAM_TEXT_LEN             = 38
UNSUPPORTED_SCHEMA_TEXT_LEN          = 41
MALFORMED_PROGRAM_TEXT_LEN           = 40
MISSING_INPUT_TEXT_LEN               = 39

	.section data, kind=data
	.priv

NeedsPipelineText
	.byte "OTR001: evaluate_expression requires pipeline", 0

BadRequestText
	.byte "OTR901: operand-record request malformed", 0

MalformedChunkText
	.byte "OTR901: compact operand-record malformed", 0

MissingProgramText
	.byte "OTR901: operand-record program missing", 0

UnsupportedSchemaText
	.byte "OTR901: unsupported operand-record schema", 0

MalformedProgramText
	.byte "OTR901: operand-record program malformed", 0

MissingInputText
	.byte "OTR901: operand-record input is missing", 0

	.endsection

	.section bss, kind=bss
	.priv

RequestProgramIdPtr
	.res long, 1
RequestProgramIdLen
	.res word, 1
RequestRegisterPtr
	.res long, 1
RequestRegisterCount
	.res word, 1
RequestValuePtr
	.res long, 1
RequestValueCount
	.res word, 1
OwnerCount
	.res word, 1
OwnerIndexByRank
	.res word, 3
OwnerTagTable
	.res byte, MAX_OWNER_COUNT_V1
	.align 2
OwnerIdPtrTable
	.res long, MAX_OWNER_COUNT_V1
OwnerIdLenTable
	.res word, MAX_OWNER_COUNT_V1
ProgramSeenCount
	.res word, 1
ProgramOwnerIndexTable
	.res word, MAX_PROGRAM_COUNT_V1
ProgramIdPtrTable
	.res long, MAX_PROGRAM_COUNT_V1
ProgramIdLenTable
	.res word, MAX_PROGRAM_COUNT_V1
SelectedProgramPtr
	.res long, 1
SelectedProgramLen
	.res word, 1
SelectedProgramSchema
	.res word, 1
SelectedProgramRank
	.res word, 1

	.endsection

	.section code, kind=code
	.pub

; ---------------------------------------------------------------------------
; Execute one scoped package-owned operand-record request.
;
; Request bytes:
; - byte: request version (1)
; - byte: program-id length, followed by program-id bytes
; - byte: register count, followed by `(u16 class, u16 index)` LE records
; - byte: value count, followed by signed i64 LE values
;
; Result bytes use the neutral fixed 24-byte v1 layout declared in the public
; tkpkg ABI. The package program supplies every kind, update, base, width, and
; scale choice; this module assigns no family meaning to those values.
;
; Inputs:
; - A0: validated tkpkg control block with an input request window.
;
; Outputs:
; - D0: 0 on success, STATUS_BAD_REQUEST_V1 for malformed request bytes, or
;   STATUS_RUNTIME_ERROR_V1 for package/program/input failure.
; - D1: result length on success or diagnostic length on runtime failure.
; - A1: result buffer on success or diagnostic text on runtime failure.
;
; Clobbers:
; - D0-D7/A0-A6/CCR. Caller-visible registers are protected by the body.
;
; CCR:
; - Reflects D0 on return.
; ---------------------------------------------------------------------------
executeRequestV1	.block
	movem.l d2-d7/a2-a6, -(sp)
	btst #1, buffers.PackageStateFlags
	beq.w needsPipeline
	bsr.w parseRequestV1
	bne.w badRequest
	bsr.w findScopedProgramV1
	bne.w return
	bsr.w executeProgramV1
	bne.w return
	lea buffers.OperandRecordResultBuffer, a1
	moveq #abi.OPERAND_RECORD_RESULT_SIZE_V1, d1
	moveq #0, d0
	bra.s return

needsPipeline
	lea NeedsPipelineText, a1
	moveq #NEEDS_PIPELINE_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	bra.s return

badRequest
	lea BadRequestText, a1
	moveq #BAD_REQUEST_TEXT_LEN, d1
	moveq #abi.STATUS_BAD_REQUEST_V1, d0

return
	movem.l (sp)+, d2-d7/a2-a6
	tst.l d0
	rts
	.bend  ; executeRequestV1

	.priv

; Parse and retain bounded request fields without interpreting target meaning.
; Inputs: A0 = request control block. Outputs: D0 = 0 success, 1 malformed.
; Clobbers: D0-D7/A2-A5/CCR. CCR: reflects D0 on return.
parseRequestV1	.block
	moveq #0, d0
	move.b abi.CB_INPUT_PTR(a0), d0
	moveq #0, d1
	move.b 17(a0), d1
	lsl.w #8, d1
	or.w d1, d0
	lea 0(a0, d0.w), a4
	moveq #0, d7
	move.b abi.CB_INPUT_LEN(a0), d7
	moveq #0, d0
	move.b 19(a0), d0
	lsl.w #8, d0
	or.w d0, d7
	movea.l a0, a2
	adda.l #buffers.OPERAND_RECORD_RESULT_BUFFER_PTR_V1, a2
	movea.l a2, a3
	adda.l #abi.OPERAND_RECORD_RESULT_SIZE_V1, a3
	movea.l a4, a5
	adda.w d7, a5
	cmpa.l a2, a5
	bls.s inputDoesNotOverlapResult
	cmpa.l a3, a4
	bcs.w fail
inputDoesNotOverlapResult
	cmpi.w #4, d7
	bcs.w fail
	cmpi.b #abi.OPERAND_RECORD_REQUEST_VERSION_V1, (a4)+
	bne.w fail
	subq.w #1, d7
	moveq #0, d6
	move.b (a4)+, d6
	subq.w #1, d7
	tst.w d6
	beq.w fail
	cmp.w d7, d6
	bhs.w fail
	move.l a4, RequestProgramIdPtr
	move.w d6, RequestProgramIdLen
	adda.w d6, a4
	sub.w d6, d7
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d7
	cmpi.w #MAX_REGISTER_INPUTS_V1, d5
	bhi.w fail
	move.w d5, RequestRegisterCount
	move.l a4, RequestRegisterPtr
	move.w d5, d0
	lsl.w #2, d0
	cmp.w d7, d0
	bhs.w fail
	adda.w d0, a4
	sub.w d0, d7
	moveq #0, d5
	move.b (a4)+, d5
	subq.w #1, d7
	cmpi.w #MAX_VALUE_INPUTS_V1, d5
	bhi.s fail
	move.w d5, RequestValueCount
	move.l a4, RequestValuePtr
	move.w d5, d0
	lsl.w #3, d0
	cmp.w d7, d0
	bne.s fail
	moveq #0, d0
	rts

fail
	moveq #1, d0
	rts
	.bend  ; parseRequestV1

; Locate CPRD and select the first matching program in Rust scope order:
; dialect, CPU, then family. All chunk walks are bounds checked and consume the
; exact chunk; a malformed/trailing chunk fails closed.
; Outputs: A2/D7 = program pointer/length; D6 = schema; D0 = status.
; Clobbers: D0-D7/A0-A6/CCR. CCR: reflects D0 on return.
findScopedProgramV1	.block
	bsr.w loadCompactRecordChunkV1
	bne.w malformedChunk
	bsr.w readU16LeV1
	bne.w malformedChunk
	cmpi.w #COMPACT_RECORD_CHUNK_VERSION_V1, d0
	bne.w malformedChunk
	bsr.w readU16LeV1
	bne.w malformedChunk
	tst.w d0
	beq.w malformedChunk
	cmpi.w #MAX_OWNER_COUNT_V1, d0
	bhi.w malformedChunk
	move.w d0, OwnerCount
	move.w #NO_OWNER_INDEX, OwnerIndexByRank
	move.w #NO_OWNER_INDEX, OwnerIndexByRank + 2
	move.w #NO_OWNER_INDEX, OwnerIndexByRank + 4
	moveq #0, d7

ownerLoop
	cmp.w OwnerCount, d7
	bhs.s ownersDone
	moveq #1, d0
	bsr.w requireBytesV1
	bne.w malformedChunk
	moveq #0, d6
	move.b (a2)+, d6
	cmpi.b #SCOPED_OWNER_DIALECT, d6
	bhi.w malformedChunk
	bsr.w locateStringV1
	bne.w malformedChunk
	bsr.w storeOwnerDescriptorV1
	bsr.w ownerDescriptorIsDuplicateV1
	bne.w malformedChunk
	bsr.w loadOwnerDescriptorV1
	move.w d7, -(sp)
	bsr.w ownerMatchesActiveV1
	move.w (sp)+, d7
	tst.b d0
	beq.s nextOwner
	moveq #0, d0
	move.b d6, d0
	lsl.w #1, d0
	lea OwnerIndexByRank, a0
	cmpi.w #NO_OWNER_INDEX, 0(a0, d0.w)
	bne.w malformedChunk
	move.w d7, 0(a0, d0.w)

nextOwner
	addq.w #1, d7
	bra.s ownerLoop

ownersDone
	bsr.w readU32LeV1
	bne.w malformedChunk
	swap d0
	tst.w d0
	bne.w malformedChunk
	swap d0
	cmpi.w #MAX_PROGRAM_COUNT_V1, d0
	bhi.w malformedChunk
	move.w d0, d7
	clr.w ProgramSeenCount
	move.w #4, SelectedProgramRank
	clr.l SelectedProgramPtr
	clr.w SelectedProgramLen
	clr.w SelectedProgramSchema

entryLoop
	tst.w d7
	beq.w entriesDone
	bsr.w readU16LeV1
	bne.w malformedChunk
	cmp.w OwnerCount, d0
	bhs.w malformedChunk
	move.w d0, d5
	bsr.w locateStringV1
	bne.w malformedChunk
	tst.w d0
	beq.w malformedChunk
	bsr.w storeProgramDescriptorV1
	bsr.w programDescriptorIsDuplicateV1
	bne.w malformedChunk
	bsr.w loadCurrentProgramDescriptorV1
	move.l a2, -(sp)
	move.w d7, -(sp)
	move.w d5, -(sp)
	bsr.w requestIdEqualsV1
	move.b d0, d4
	move.w (sp)+, d5
	move.w (sp)+, d7
	movea.l (sp)+, a2
	bsr.w readU16LeV1
	bne.w malformedChunk
	move.w d0, d3
	bsr.w readU32LeV1
	bne.w malformedChunk
	swap d0
	tst.w d0
	bne.w malformedChunk
	swap d0
	move.w d0, d2
	move.w d2, d0
	bsr.w requireBytesV1
	bne.w malformedChunk
	bsr.w validateProgramShapeV1
	bne.w malformedChunk
	tst.b d4
	beq.s skipCandidate
	move.w d5, d0
	bsr.w ownerRankForIndexV1
	cmpi.w #3, d0
	bhi.s skipCandidate
	cmp.w SelectedProgramRank, d0
	bhi.s skipCandidate
	beq.w malformedChunk
	move.w d0, SelectedProgramRank
	move.l a2, SelectedProgramPtr
	move.w d2, SelectedProgramLen
	move.w d3, SelectedProgramSchema

skipCandidate
	adda.w d2, a2
	addq.w #1, ProgramSeenCount
	subq.w #1, d7
	bra.w entryLoop

entriesDone
	cmpa.l a6, a2
	bne.s malformedChunk
	tst.l SelectedProgramPtr
	beq.s missingProgram
	movea.l SelectedProgramPtr, a2
	move.w SelectedProgramLen, d7
	move.w SelectedProgramSchema, d6
	moveq #0, d0
	rts

missingProgram
	lea MissingProgramText, a1
	moveq #MISSING_PROGRAM_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

malformedChunk
	lea MalformedChunkText, a1
	moveq #MALFORMED_CHUNK_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts
	.bend  ; findScopedProgramV1

; Execute exactly the Rust OPRD schema-v1 base opcode set.
; Inputs: A2/D7 = program bytes/length; D6 = schema. Outputs: neutral result.
; Clobbers: D0-D7/A0-A5/CCR. CCR: reflects D0 on return.
executeProgramV1	.block
	cmpi.w #OPERAND_RECORD_SCHEMA_VERSION_V1, d6
	bne.w unsupportedSchema
	cmpi.w #3, d7
	bcs.w malformedProgram
	lea buffers.OperandRecordResultBuffer, a1
	moveq #abi.OPERAND_RECORD_RESULT_SIZE_V1 - 1, d0
clearResult
	clr.b (a1)+
	dbf d0, clearResult
	lea buffers.OperandRecordResultBuffer, a1
	move.b #abi.OPERAND_RECORD_RESULT_VERSION_V1, abi.OPERAND_RECORD_RESULT_VERSION(a1)
	moveq #0, d4
	move.b (a2), d4
	move.b d4, abi.OPERAND_RECORD_RESULT_KIND(a1)
	cmpi.b #OPERAND_RECORD_OP_REGISTER, d4
	beq.w executeRegister
	cmpi.b #OPERAND_RECORD_OP_INDIRECT, d4
	beq.w executeIndirect
	cmpi.b #OPERAND_RECORD_OP_DISPLACEMENT, d4
	beq.w executeDisplacement
	cmpi.b #OPERAND_RECORD_OP_INDEXED, d4
	beq.w executeIndexed
	cmpi.b #OPERAND_RECORD_OP_ABSOLUTE, d4
	beq.w executeAbsolute
	cmpi.b #OPERAND_RECORD_OP_IMMEDIATE, d4
	beq.w executeImmediate
	bra.w malformedProgram

executeRegister
	cmpi.w #3, d7
	bne.w malformedProgram
	cmpi.b #OPERAND_RECORD_OP_END, 2(a2)
	bne.w malformedProgram
	moveq #0, d0
	move.b 1(a2), d0
	lea abi.OPERAND_RECORD_RESULT_PRIMARY_CLASS(a1), a0
	bsr.w copyRegisterInputV1
	bne.w missingInput
	bra.w programOk

executeIndirect
	cmpi.w #4, d7
	bne.w malformedProgram
	cmpi.b #OPERAND_RECORD_OP_END, 3(a2)
	bne.w malformedProgram
	cmpi.b #2, 2(a2)
	bhi.w malformedProgram
	move.b 2(a2), abi.OPERAND_RECORD_RESULT_VARIANT(a1)
	moveq #0, d0
	move.b 1(a2), d0
	lea abi.OPERAND_RECORD_RESULT_PRIMARY_CLASS(a1), a0
	bsr.w copyRegisterInputV1
	bne.w missingInput
	bra.w programOk

executeDisplacement
	cmpi.w #5, d7
	bne.w malformedProgram
	cmpi.b #OPERAND_RECORD_OP_END, 4(a2)
	bne.w malformedProgram
	cmpi.b #1, 1(a2)
	bhi.w malformedProgram
	move.b 1(a2), abi.OPERAND_RECORD_RESULT_VARIANT(a1)
	tst.b 1(a2)
	bne.s displacementPc
	moveq #0, d0
	move.b 2(a2), d0
	lea abi.OPERAND_RECORD_RESULT_PRIMARY_CLASS(a1), a0
	bsr.w copyRegisterInputV1
	bne.w missingInput
	bra.s displacementValue
displacementPc
	tst.b 2(a2)
	bne.w malformedProgram
displacementValue
	moveq #0, d0
	move.b 3(a2), d0
	lea abi.OPERAND_RECORD_RESULT_VALUE(a1), a0
	bsr.w copyValueInputV1
	bne.w missingInput
	bra.w programOk

executeIndexed
	cmpi.w #8, d7
	bne.w malformedProgram
	cmpi.b #OPERAND_RECORD_OP_END, 7(a2)
	bne.w malformedProgram
	cmpi.b #1, 1(a2)
	bhi.w malformedProgram
	move.b 4(a2), d0
	cmpi.b #8, d0
	beq.s indexedWidthOk
	cmpi.b #16, d0
	beq.s indexedWidthOk
	cmpi.b #32, d0
	beq.s indexedWidthOk
	cmpi.b #64, d0
	bne.w malformedProgram
indexedWidthOk
	moveq #0, d0
	move.b 5(a2), d0
	beq.w malformedProgram
	move.w d0, d1
	subq.w #1, d1
	and.w d0, d1
	bne.w malformedProgram
	move.b 1(a2), abi.OPERAND_RECORD_RESULT_VARIANT(a1)
	move.b 4(a2), abi.OPERAND_RECORD_RESULT_WIDTH(a1)
	move.b 5(a2), abi.OPERAND_RECORD_RESULT_SCALE(a1)
	tst.b 1(a2)
	bne.s indexedPc
	moveq #0, d0
	move.b 2(a2), d0
	lea abi.OPERAND_RECORD_RESULT_PRIMARY_CLASS(a1), a0
	bsr.w copyRegisterInputV1
	bne.w missingInput
	bra.s indexedIndex
indexedPc
	tst.b 2(a2)
	bne.w malformedProgram
indexedIndex
	moveq #0, d0
	move.b 3(a2), d0
	lea abi.OPERAND_RECORD_RESULT_SECONDARY_CLASS(a1), a0
	bsr.w copyRegisterInputV1
	bne.w missingInput
	moveq #0, d0
	move.b 6(a2), d0
	lea abi.OPERAND_RECORD_RESULT_VALUE(a1), a0
	bsr.w copyValueInputV1
	bne.w missingInput
	bra.w programOk

executeAbsolute
	cmpi.w #4, d7
	bne.w malformedProgram
	cmpi.b #OPERAND_RECORD_OP_END, 3(a2)
	bne.w malformedProgram
	move.b 2(a2), d0
	cmpi.b #8, d0
	beq.s absoluteWidthOk
	cmpi.b #16, d0
	beq.s absoluteWidthOk
	cmpi.b #24, d0
	beq.s absoluteWidthOk
	cmpi.b #32, d0
	beq.s absoluteWidthOk
	cmpi.b #64, d0
	bne.w malformedProgram
absoluteWidthOk
	move.b 2(a2), abi.OPERAND_RECORD_RESULT_WIDTH(a1)
	moveq #0, d0
	move.b 1(a2), d0
	lea abi.OPERAND_RECORD_RESULT_VALUE(a1), a0
	bsr.w copyValueInputV1
	bne.w missingInput
	bra.s programOk

executeImmediate
	cmpi.w #3, d7
	bne.s malformedProgram
	cmpi.b #OPERAND_RECORD_OP_END, 2(a2)
	bne.s malformedProgram
	moveq #0, d0
	move.b 1(a2), d0
	lea abi.OPERAND_RECORD_RESULT_VALUE(a1), a0
	bsr.w copyValueInputV1
	bne.s missingInput

programOk
	moveq #0, d0
	rts

unsupportedSchema
	lea UnsupportedSchemaText, a1
	moveq #UNSUPPORTED_SCHEMA_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

malformedProgram
	lea MalformedProgramText, a1
	moveq #MALFORMED_PROGRAM_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts

missingInput
	lea MissingInputText, a1
	moveq #MISSING_INPUT_TEXT_LEN, d1
	moveq #abi.STATUS_RUNTIME_ERROR_V1, d0
	rts
	.bend  ; executeProgramV1

; Copy one register input's four LE bytes to A0.
; Inputs: D0.W = index; A0 = destination. Outputs: D0 = 0/1.
copyRegisterInputV1	.block
	cmp.w RequestRegisterCount, d0
	bhs.s fail
	lsl.w #2, d0
	movea.l RequestRegisterPtr, a3
	adda.w d0, a3
	move.b (a3)+, (a0)+
	move.b (a3)+, (a0)+
	move.b (a3)+, (a0)+
	move.b (a3), (a0)
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; copyRegisterInputV1

; Copy one signed i64 input's eight LE bytes to A0.
; Inputs: D0.W = index; A0 = destination. Outputs: D0 = 0/1.
copyValueInputV1	.block
	cmp.w RequestValueCount, d0
	bhs.s fail
	lsl.w #3, d0
	movea.l RequestValuePtr, a3
	adda.w d0, a3
	moveq #7, d0
loop
	move.b (a3)+, (a0)+
	dbf d0, loop
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; copyValueInputV1

; Return D0 = precedence rank for an owner index (0 dialect, 1 CPU, 2 family,
; 4 no active match). Inputs: D0.W = owner index. Clobbers: D1/A0/CCR.
ownerRankForIndexV1	.block
	move.w d0, d1
	lea OwnerIndexByRank, a0
	cmp.w 4(a0), d1
	beq.s dialect
	cmp.w 2(a0), d1
	beq.s cpu
	cmp.w (a0), d1
	beq.s family
	moveq #4, d0
	rts
dialect
	moveq #0, d0
	rts
cpu
	moveq #1, d0
	rts
family
	moveq #2, d0
	rts
	.bend  ; ownerRankForIndexV1

; Compare one CPRD owner against its active pipeline id.
; Inputs: D6.B tag; A1/D0 owner id. Outputs: D0 = 1 match, 0 no match.
ownerMatchesActiveV1	.block
	movem.l d1-d5/a0-a4, -(sp)
	movea.l a1, a3
	move.w d0, d5
	cmpi.b #SCOPED_OWNER_DIALECT, d6
	bne.s checkCpu
	lea buffers.ActiveDialectBuffer, a4
	tst.b (a4)
	beq.s noMatch
	bra.s compare
checkCpu
	cmpi.b #SCOPED_OWNER_CPU, d6
	bne.s family
	lea buffers.ActiveCpuBuffer, a4
	bra.s compare
family
	lea buffers.ActiveFamilyBuffer, a4
compare
	movea.l a4, a0
	bsr.w zeroTerminatedLenV1
	move.w d0, d1
	movea.l a3, a1
	move.w d5, d0
	movea.l a4, a0
	bsr.w stringEqAsciiCasefoldV1
	bra.s return
noMatch
	moveq #0, d0
return
	movem.l (sp)+, d1-d5/a0-a4
	rts
	.bend  ; ownerMatchesActiveV1

; Compare located CPRD id A1/D0 against the request program id.
requestIdEqualsV1	.block
	movea.l RequestProgramIdPtr, a0
	move.w RequestProgramIdLen, d1
	bsr.w stringEqAsciiCasefoldV1
	rts
	.bend  ; requestIdEqualsV1

; Store and reload one bounded owner descriptor while validating the complete
; CPRD owner table before program selection.
storeOwnerDescriptorV1	.block
	move.w d7, d1
	lea OwnerTagTable, a0
	move.b d6, 0(a0, d1.w)
	move.w d1, d2
	lsl.w #2, d2
	lea OwnerIdPtrTable, a0
	move.l a1, 0(a0, d2.w)
	lsl.w #1, d1
	lea OwnerIdLenTable, a0
	move.w d0, 0(a0, d1.w)
	rts
	.bend  ; storeOwnerDescriptorV1

loadOwnerDescriptorV1	.block
	move.w d7, d1
	lea OwnerTagTable, a0
	moveq #0, d6
	move.b 0(a0, d1.w), d6
	move.w d1, d2
	lsl.w #2, d2
	lea OwnerIdPtrTable, a0
	movea.l 0(a0, d2.w), a1
	lsl.w #1, d1
	lea OwnerIdLenTable, a0
	moveq #0, d0
	move.w 0(a0, d1.w), d0
	rts
	.bend  ; loadOwnerDescriptorV1

; Return D0 = 1 when the current owner duplicates an earlier case-folded key.
ownerDescriptorIsDuplicateV1	.block
	movem.l d1-d5/a0-a4, -(sp)
	moveq #0, d5
loop
	cmp.w d7, d5
	bhs.s no
	lea OwnerTagTable, a4
	move.b 0(a4, d7.w), d0
	cmp.b 0(a4, d5.w), d0
	bne.s next
	move.w d5, d2
	lsl.w #2, d2
	lea OwnerIdPtrTable, a4
	movea.l 0(a4, d2.w), a0
	move.w d7, d2
	lsl.w #2, d2
	movea.l 0(a4, d2.w), a1
	move.w d5, d2
	lsl.w #1, d2
	lea OwnerIdLenTable, a4
	moveq #0, d1
	move.w 0(a4, d2.w), d1
	move.w d7, d2
	lsl.w #1, d2
	moveq #0, d0
	move.w 0(a4, d2.w), d0
	bsr.w stringEqAsciiCasefoldV1
	tst.b d0
	bne.s yes
next
	addq.w #1, d5
	bra.s loop
no
	moveq #0, d0
	bra.s return
yes
	moveq #1, d0
return
	movem.l (sp)+, d1-d5/a0-a4
	tst.l d0
	rts
	.bend  ; ownerDescriptorIsDuplicateV1

storeProgramDescriptorV1	.block
	move.w ProgramSeenCount, d1
	move.w d1, d2
	lsl.w #1, d2
	lea ProgramOwnerIndexTable, a0
	move.w d5, 0(a0, d2.w)
	move.w d1, d2
	lsl.w #2, d2
	lea ProgramIdPtrTable, a0
	move.l a1, 0(a0, d2.w)
	lsl.w #1, d1
	lea ProgramIdLenTable, a0
	move.w d0, 0(a0, d1.w)
	rts
	.bend  ; storeProgramDescriptorV1

loadCurrentProgramDescriptorV1	.block
	move.w ProgramSeenCount, d1
	move.w d1, d2
	lsl.w #1, d2
	lea ProgramOwnerIndexTable, a0
	move.w 0(a0, d2.w), d5
	move.w d1, d2
	lsl.w #2, d2
	lea ProgramIdPtrTable, a0
	movea.l 0(a0, d2.w), a1
	lsl.w #1, d1
	lea ProgramIdLenTable, a0
	moveq #0, d0
	move.w 0(a0, d1.w), d0
	rts
	.bend  ; loadCurrentProgramDescriptorV1

; Return D0 = 1 when the current owner/id duplicates an earlier entry.
programDescriptorIsDuplicateV1	.block
	movem.l d1-d6/a0-a4, -(sp)
	move.w ProgramSeenCount, d6
	moveq #0, d5
loop
	cmp.w d6, d5
	bhs.s no
	lea ProgramOwnerIndexTable, a4
	move.w d5, d2
	lsl.w #1, d2
	move.w 0(a4, d2.w), d0
	move.w d6, d2
	lsl.w #1, d2
	cmp.w 0(a4, d2.w), d0
	bne.s next
	move.w d5, d2
	lsl.w #2, d2
	lea ProgramIdPtrTable, a4
	movea.l 0(a4, d2.w), a0
	move.w d6, d2
	lsl.w #2, d2
	movea.l 0(a4, d2.w), a1
	move.w d5, d2
	lsl.w #1, d2
	lea ProgramIdLenTable, a4
	moveq #0, d1
	move.w 0(a4, d2.w), d1
	move.w d6, d2
	lsl.w #1, d2
	moveq #0, d0
	move.w 0(a4, d2.w), d0
	bsr.w stringEqAsciiCasefoldV1
	tst.b d0
	bne.s yes
next
	addq.w #1, d5
	bra.s loop
no
	moveq #0, d0
	bra.s return
yes
	moveq #1, d0
return
	movem.l (sp)+, d1-d6/a0-a4
	tst.l d0
	rts
	.bend  ; programDescriptorIsDuplicateV1

; Validate one complete OPRD v1-v3 program exactly as the Rust package codec.
; Inputs: A2 = program; D2.W = length; D3.W = schema. Outputs: D0 = 0/1.
; Clobbers: D0-D1/D6/CCR.
validateProgramShapeV1	.block
	cmpi.w #1, d3
	bcs.w fail
	cmpi.w #3, d3
	bhi.w fail
	cmpi.w #3, d2
	bcs.w fail
	moveq #0, d1
	move.b (a2), d1
	moveq #0, d6
	cmpi.b #OPERAND_RECORD_OP_REGISTER, d1
	beq.s length3
	cmpi.b #OPERAND_RECORD_OP_IMMEDIATE, d1
	beq.s length3
	cmpi.b #OPERAND_RECORD_OP_INDIRECT, d1
	beq.s length4
	cmpi.b #OPERAND_RECORD_OP_ABSOLUTE, d1
	beq.s length4
	cmpi.b #OPERAND_RECORD_OP_DISPLACEMENT, d1
	beq.s length5
	cmpi.b #OPERAND_RECORD_OP_INDEXED, d1
	beq.s length8
	cmpi.w #2, d3
	bcs.w fail
	cmpi.b #OPERAND_RECORD_OP_NESTED_ADDRESS, d1
	beq.s length12
	cmpi.b #OPERAND_RECORD_OP_REGISTER_PAIR, d1
	beq.s length5
	cmpi.b #OPERAND_RECORD_OP_REGISTER_RANGE, d1
	beq.s length4
	cmpi.b #OPERAND_RECORD_OP_REGISTER_LIST, d1
	beq.s length3
	cmpi.b #OPERAND_RECORD_OP_FIELD, d1
	beq.s length7
	cmpi.w #3, d3
	bcs.w fail
	cmpi.b #OPERAND_RECORD_OP_COMPOSITE, d1
	bne.w fail
	moveq #5, d6
	bra.s lengthReady
length3
	moveq #3, d6
	bra.s lengthReady
length4
	moveq #4, d6
	bra.s lengthReady
length5
	moveq #5, d6
	bra.s lengthReady
length7
	moveq #7, d6
	bra.s lengthReady
length8
	moveq #8, d6
	bra.s lengthReady
length12
	moveq #12, d6
lengthReady
	cmp.w d6, d2
	bne.w fail
	subq.w #1, d6
	cmpi.b #OPERAND_RECORD_OP_END, 0(a2, d6.w)
	bne.w fail
	cmpi.b #OPERAND_RECORD_OP_INDIRECT, d1
	beq.w validateIndirect
	cmpi.b #OPERAND_RECORD_OP_DISPLACEMENT, d1
	beq.w validateBase
	cmpi.b #OPERAND_RECORD_OP_INDEXED, d1
	beq.w validateIndexed
	cmpi.b #OPERAND_RECORD_OP_ABSOLUTE, d1
	beq.w validateAbsolute
	cmpi.b #OPERAND_RECORD_OP_NESTED_ADDRESS, d1
	beq.w validateNested
	cmpi.b #OPERAND_RECORD_OP_REGISTER_PAIR, d1
	beq.w validatePair
	cmpi.b #OPERAND_RECORD_OP_FIELD, d1
	beq.w validateField
	bra.w ok

validateIndirect
	cmpi.b #2, 2(a2)
	bhi.w fail
	bra.w ok

validateBase
	cmpi.b #1, 1(a2)
	bhi.w fail
	tst.b 1(a2)
	beq.w ok
	tst.b 2(a2)
	bne.w fail
	bra.w ok

validateIndexed
	cmpi.b #1, 1(a2)
	bhi.w fail
	tst.b 1(a2)
	beq.s indexedWidth
	tst.b 2(a2)
	bne.w fail
indexedWidth
	move.b 4(a2), d0
	cmpi.b #8, d0
	beq.s indexedScale
	cmpi.b #16, d0
	beq.s indexedScale
	cmpi.b #32, d0
	beq.s indexedScale
	cmpi.b #64, d0
	bne.w fail
indexedScale
	moveq #0, d0
	move.b 5(a2), d0
	beq.w fail
	move.w d0, d6
	subq.w #1, d6
	and.w d0, d6
	bne.w fail
	bra.w ok

validateAbsolute
	move.b 2(a2), d0
	cmpi.b #8, d0
	beq.w ok
	cmpi.b #16, d0
	beq.w ok
	cmpi.b #24, d0
	beq.w ok
	cmpi.b #32, d0
	beq.w ok
	cmpi.b #64, d0
	bne.w fail
	bra.w ok

validateNested
	cmpi.b #2, 1(a2)
	bhi.w fail
	tst.b 1(a2)
	beq.s nestedBaseOk
	tst.b 2(a2)
	bne.w fail
nestedBaseOk
	moveq #3, d6
	bsr.s validateOptionalDisplacement
	bne.w fail
	moveq #9, d6
	bsr.s validateOptionalDisplacement
	bne.w fail
	cmpi.b #$FF, 5(a2)
	bne.s nestedHasIndex
	tst.b 6(a2)
	bne.w fail
	tst.b 7(a2)
	bne.w fail
	bra.s nestedIndirection
nestedHasIndex
	move.b 6(a2), d0
	cmpi.b #16, d0
	beq.s nestedIndexScale
	cmpi.b #32, d0
	bne.w fail
nestedIndexScale
	moveq #0, d0
	move.b 7(a2), d0
	beq.w fail
	move.w d0, d6
	subq.w #1, d6
	and.w d0, d6
	bne.w fail
nestedIndirection
	cmpi.b #2, 8(a2)
	bhi.w fail
	bra.w ok

validateOptionalDisplacement
	cmpi.b #$FF, 0(a2, d6.w)
	bne.s optionalPresent
	tst.b 1(a2, d6.w)
	bne.s optionalFail
	moveq #0, d0
	rts
optionalPresent
	move.b 1(a2, d6.w), d0
	cmpi.b #16, d0
	beq.s optionalOk
	cmpi.b #32, d0
	bne.s optionalFail
optionalOk
	moveq #0, d0
	rts
optionalFail
	moveq #1, d0
	rts

validatePair
	cmpi.b #1, 3(a2)
	bhi.s fail
	bra.s ok

validateField
	cmpi.b #1, 2(a2)
	bhi.s fail
	cmpi.b #1, 4(a2)
	bhi.s fail

ok
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; validateProgramShapeV1

; Inputs: A0/D1 and A1/D0 are byte strings. Outputs: D0 = 1 equal, 0 otherwise.
stringEqAsciiCasefoldV1	.block
	cmp.w d1, d0
	bne.s no
	move.w d0, d2
	beq.s yes
	subq.w #1, d2
loop
	moveq #0, d3
	move.b (a0)+, d3
	bsr.s foldAsciiV1
	move.b d0, d4
	moveq #0, d3
	move.b (a1)+, d3
	bsr.s foldAsciiV1
	cmp.b d4, d0
	bne.s no
	dbf d2, loop
yes
	moveq #1, d0
	rts
no
	moveq #0, d0
	rts
	.bend  ; stringEqAsciiCasefoldV1

foldAsciiV1	.block
	move.l d3, d0
	cmpi.b #'A', d0
	bcs.s done
	cmpi.b #'Z', d0
	bhi.s done
	addi.b #32, d0
done
	rts
	.bend  ; foldAsciiV1

zeroTerminatedLenV1	.block
	moveq #0, d0
loop
	tst.b 0(a0, d0.w)
	beq.s done
	addq.w #1, d0
	bra.s loop
done
	rts
	.bend  ; zeroTerminatedLenV1

; Validate one bounded byte string with the same Unicode scalar-value rules as
; Rust UTF-8 decoding. ASCII case folding remains a separate lookup concern.
; Inputs: A1 = bytes; D0.W = length. Outputs: D0 = 0 valid, 1 invalid.
; Clobbers: D0-D1/CCR.
validateUtf8V1	.block
	movem.l d2-d7/a0, -(sp)
	movea.l a1, a0
	move.w d0, d6
loop
	tst.w d6
	beq.w ok
	moveq #0, d1
	move.b (a0)+, d1
	subq.w #1, d6
	cmpi.b #$80, d1
	bcs.s loop
	cmpi.b #$C2, d1
	bcs.w fail
	cmpi.b #$DF, d1
	bls.w twoByte
	cmpi.b #$E0, d1
	beq.w threeByteE0
	cmpi.b #$EC, d1
	bls.w threeByte
	cmpi.b #$ED, d1
	beq.w threeByteEd
	cmpi.b #$EF, d1
	bls.w threeByte
	cmpi.b #$F0, d1
	beq.w fourByteF0
	cmpi.b #$F3, d1
	bls.w fourByte
	cmpi.b #$F4, d1
	bne.w fail
	bsr.w readContinuationV1
	bne.w fail
	cmpi.b #$8F, d1
	bhi.w fail
	bra.s fourByteTail

twoByte
	bsr.w readContinuationV1
	bne.w fail
	bra.w loop

threeByteE0
	bsr.w readContinuationV1
	bne.w fail
	cmpi.b #$A0, d1
	bcs.w fail
	bra.s threeByteTail

threeByteEd
	bsr.w readContinuationV1
	bne.w fail
	cmpi.b #$9F, d1
	bhi.w fail
	bra.s threeByteTail

threeByte
	bsr.w readContinuationV1
	bne.w fail
threeByteTail
	bsr.w readContinuationV1
	bne.w fail
	bra.w loop

fourByteF0
	bsr.w readContinuationV1
	bne.w fail
	cmpi.b #$90, d1
	bcs.w fail
	bra.s fourByteTail

fourByte
	bsr.w readContinuationV1
	bne.w fail
fourByteTail
	bsr.w readContinuationV1
	bne.w fail
	bsr.w readContinuationV1
	bne.w fail
	bra.w loop

ok
	moveq #0, d0
	bra.s return
fail
	moveq #1, d0
return
	movem.l (sp)+, d2-d7/a0
	tst.l d0
	rts
	.bend  ; validateUtf8V1

; Consume one UTF-8 continuation byte.
; Inputs: A0 = cursor; D6.W = remaining. Outputs: D0 = 0/1; D1 = byte.
readContinuationV1	.block
	tst.w d6
	beq.s fail
	moveq #0, d1
	move.b (a0)+, d1
	subq.w #1, d6
	cmpi.b #$80, d1
	bcs.s fail
	cmpi.b #$BF, d1
	bhi.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; readContinuationV1

; Resolve CPRD locator to a bounded cursor. Outputs: A2 start, A6 end, D0 0/1.
loadCompactRecordChunkV1	.block
	lea buffers.CprdChunkOffsetLo, a3
	bsr.w readLocatorU32V1
	move.l d0, d4
	bsr.w readLocatorU32V1
	tst.l d0
	beq.s fail
	lea buffers.PackageStorage, a2
	adda.l d4, a2
	movea.l a2, a6
	adda.l d0, a6
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; loadCompactRecordChunkV1

readLocatorU32V1	.block
	moveq #0, d0
	move.b (a3)+, d0
	moveq #0, d1
	move.b (a3)+, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a3)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a3)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	rts
	.bend  ; readLocatorU32V1

requireBytesV1	.block
	movea.l a2, a0
	adda.l d0, a0
	cmpa.l a6, a0
	bhi.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; requireBytesV1

readU16LeV1	.block
	moveq #2, d0
	bsr.s requireBytesV1
	bne.s fail
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.w #8, d1
	or.w d1, d0
	moveq #0, d1
	rts
fail
	moveq #1, d1
	rts
	.bend  ; readU16LeV1

readU32LeV1	.block
	moveq #4, d0
	bsr.s requireBytesV1
	bne.s fail
	moveq #0, d0
	move.b (a2)+, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	move.b (a2)+, d1
	lsl.l #8, d1
	lsl.l #8, d1
	lsl.l #8, d1
	or.l d1, d0
	moveq #0, d1
	rts
fail
	moveq #1, d1
	rts
	.bend  ; readU32LeV1

; Read one u32-length-prefixed string. Outputs: A1/D0 string, D1 status.
locateStringV1	.block
	bsr.w readU32LeV1
	bne.s fail
	move.l d0, d2
	swap d0
	tst.w d0
	bne.s fail
	swap d0
	move.w d0, d2
	bsr.w requireBytesV1
	bne.s fail
	movea.l a2, a1
	adda.w d2, a2
	move.w d2, d0
	bsr.w validateUtf8V1
	bne.s fail
	move.w d2, d0
	moveq #0, d1
	rts
fail
	moveq #1, d1
	rts
	.bend  ; locateStringV1

	.endsection
	.endmodule
