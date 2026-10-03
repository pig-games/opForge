; Package-selected initial macro descriptor service. Events retain source/token
; offsets; publication occurs only after complete program validation.
; @opforge-owner: prvm.amigaos.macro_descriptors
	.module prvm.amigaos.macro_descriptors
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.include "telemetry_macros.i"
	.pub
MAX_RECORDS = 64
RECORD_BYTES = 32
OP_ENVELOPE = $80
OP_SPLIT = $81
OP_FORMALS = $82
OP_PUBLISH = $83
State	.struct
Source	.long ?
SourceLen	.long ?
CodeEnd	.long ?
Tokens	.long ?
TokenCount	.long ?
Budget	.long ?
Capacity	.long ?
Count	.long ?
ListStart	.long ?
ListEnd	.long ?
Header	.long ?
Envelope	.long ?
Split	.long ?
Formal	.long ?
Published	.long ?
Policy	.long ?
LabelStart	.long ?
LabelEnd	.long ?
HasLabel	.long ?
NameStart	.long ?
NameEnd	.long ?
Role	.long ?
Pos	.long ?
Start	.long ?
End	.long ?
Scan	.long ?
Quote	.long ?
Paren	.long ?
Bracket	.long ?
Brace	.long ?
FormalIndex	.long ?
FormalCount	.long ?
DefaultStart	.long ?
DefaultEnd	.long ?
HasDefault	.long ?
NameToken	.long ?
TypeToken	.long ?
OriginalNameStart	.long ?
OriginalNameEnd	.long ?
Record	.res 32
Stage	.res MAX_RECORDS * RECORD_BYTES
	.endstruct
STATE_BYTES = State.Stage + MAX_RECORDS * RECORD_BYTES

STATE_POLICY_BYTE = State.Policy + 3
STAGE_CHILD_COUNT = State.Stage + 24
RECORD_FLAGS = State.Record + 2
RECORD_TOKEN_START = State.Record + 4
RECORD_TOKEN_END = State.Record + 8
RECORD_SOURCE_START = State.Record + 12
RECORD_SOURCE_END = State.Record + 16
RECORD_AUX0 = State.Record + 20
RECORD_AUX1 = State.Record + 24
RECORD_AUX2 = State.Record + 28

	.section code, kind=code
	.pub

; Execute entry 2 against immutable initial lexical spans and original source.
; Inputs: A0 request frame, D0 available frame bytes. Outputs: D0 status,
; D1 record count, D2 error offset (zero on success), D3 published bytes.
; Clobbers: D1-D3/A0-A2/CCR; preserves D4-D7/A3-A6. Stack owns bounded staging.
; CCR: reflects D0. Caller owns PRVM profiling invocation.
run	.block
	movem.l d4-d7/a3-a6, -(sp)
	move.l a0, d1
	beq invalidFrame
	cmpi.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	bcs invalidFrame
	movea.l a0, a4
	cmpi.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a4)
	bne invalidFrame
	cmpi.w #abi.PRVM_ABI_VERSION_V1, abi.PRVM_FRAME_ABI_VERSION(a4)
	bne invalidFrame
	cmpi.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a4)
	bcs invalidFrame
	cmpi.w #abi.PRVM_ENTRY_KIND_MACRO_DESCRIPTORS, abi.PRVM_FRAME_ENTRY_KIND(a4)
	bne invalidFrame
	tst.w abi.PRVM_FRAME_CALL_MODE(a4)
	bne invalidFrame
	cmpi.l #abi.PRVM_PARSER_CONTRACT_VERSION_V2, abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a4)
	bne invalidFrame
	tst.l abi.PRVM_FRAME_FLAGS(a4)
	bne invalidFrame
	cmpi.w #abi.PRVM_TOKEN_RECORD_SIZE, abi.PRVM_FRAME_TOKEN_RECORD_SIZE(a4)
	bne invalidFrame
	suba.l #STATE_BYTES, sp
	movea.l sp, a3
	movea.l a3, a0
	move.w #State.Record / 4 - 1, d0
clearState
	clr.l (a0)+
	dbra d0, clearState
	move.l abi.PRVM_FRAME_SOURCE_PTR(a4), State.Source(a3)
	move.l abi.PRVM_FRAME_SOURCE_LEN(a4), d0
	bmi argumentFailure
	move.l d0, State.SourceLen(a3)
	move.l d0, State.CodeEnd(a3)
	beq sourceReady
	tst.l State.Source(a3)
	beq argumentFailure
sourceReady
	move.l abi.PRVM_FRAME_TOKEN_PTR(a4), State.Tokens(a3)
	move.l abi.PRVM_FRAME_TOKEN_COUNT(a4), d0
	bmi argumentFailure
	cmpi.l #$0CCCCCCC, d0
	bhi argumentFailure
	move.l d0, State.TokenCount(a3)
	beq tokensReady
	tst.l State.Tokens(a3)
	beq argumentFailure
tokensReady
	move.l abi.PRVM_FRAME_RESULT_PTR(a4), d0
	beq argumentFailure
	move.l abi.PRVM_FRAME_RESULT_CAPACITY(a4), d0
	bmi argumentFailure
	lsr.l #5, d0
	cmpi.l #MAX_RECORDS, d0
	bls capacityReady
	moveq #MAX_RECORDS, d0
capacityReady
	move.l d0, State.Capacity(a3)
	move.l abi.PRVM_FRAME_STEP_BUDGET(a4), d0
	bmi argumentFailure
	move.l d0, State.Budget(a3)
	move.l abi.PRVM_FRAME_PROGRAM_PTR(a4), d0
	beq argumentFailure
	movea.l d0, a5
	move.l abi.PRVM_FRAME_PROGRAM_LEN(a4), d1
	ble programFailureZero
	add.l d1, d0
	bcs argumentFailure
	movea.l d0, a6
	bsr.w validateTokens
	tst.l d0
	bne failure
loop
	cmpa.l a6, a5
	bcc missingEnd
	moveq #1, d0
	bsr.w tick
	bne failure
	moveq #0, d7
	move.b (a5)+, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	tst.b d7
	beq endProgram
	tst.l State.Published(a3)
	bne programFailureAtPc
	cmpi.b #OP_ENVELOPE, d7
	beq envelopeOpcode
	cmpi.b #OP_SPLIT, d7
	beq splitOpcode
	cmpi.b #OP_FORMALS, d7
	beq formalOpcode
	cmpi.b #OP_PUBLISH, d7
	beq publishOpcode
	subq.l #1, a5
	bra programFailureAtPc
envelopeOpcode
	moveq #2, d0
	bsr.w requireBytes
	bne failure
	moveq #0, d4
	move.b (a5)+, d4
	moveq #0, d5
	move.b (a5)+, d5
	bsr.w envelope
	bne failure
	bra loop
splitOpcode
	moveq #2, d0
	bsr.w requireBytes
	bne failure
	tst.l State.Split(a3)
	bne programFailureAtPc
	moveq #0, d4
	move.b (a5)+, d4
	moveq #0, d5
	move.b (a5)+, d5
	cmpi.b #1, d4
	bne programFailureZero
	cmpi.b #44, d5
	bne programFailureZero
	bsr.w split
	bne failure
	move.l #1, State.Split(a3)
	bra loop

formalOpcode
	moveq #1, d0
	bsr.w requireBytes
	bne failure
	tst.l State.Split(a3)
	beq programFailureAtPc
	tst.l State.Formal(a3)
	bne programFailureAtPc
	cmpi.b #1, (a5)+
	bne formalPolicyFailure
	tst.l State.Header(a3)
	beq formalPolicyFailure
	bsr.w formals
	bne failure
	move.l #1, State.Formal(a3)
	bra loop
formalPolicyFailure
	bra programFailureZero
publishOpcode
	tst.l State.Published(a3)
	bne programFailureAtPc
	tst.l State.Split(a3)
	beq programFailureAtPc
	tst.l State.Header(a3)
	beq markPublished
	tst.l State.Formal(a3)
	beq programFailureAtPc
markPublished
	move.l #1, State.Published(a3)
	bra loop
endProgram
	tst.l State.Published(a3)
	beq programFailureAtPc
	cmpa.l a6, a5
	bne programFailureAtPc
	move.l State.Count(a3), d1
	move.l d1, d3
	lsl.l #5, d3
	movea.l abi.PRVM_FRAME_RESULT_PTR(a4), a0
	lea State.Stage(a3), a1
	move.l d3, d0
	beq published
publishLoop
	move.l (a1)+, (a0)+
	subq.l #4, d0
	bne publishLoop
published
	clr.l d0
	clr.l d2
	bra return
argumentFailure
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
	clr.l d2
	bra failure
programFailureZero
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
	clr.l d2
	bra failure
missingEnd
programFailureAtPc
	move.l a5, d2
	sub.l abi.PRVM_FRAME_PROGRAM_PTR(a4), d2
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
failure
	clr.l d1
	clr.l d3
return
	adda.l #STATE_BYTES, sp
	movem.l (sp)+, d4-d7/a3-a6
	tst.l d0
	rts
invalidFrame
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
	clr.l d1
	clr.l d2
	clr.l d3
	movem.l (sp)+, d4-d7/a3-a6
	tst.l d0
	rts
	.bend  ; run
	.priv

; Consume abstract VM work; failure carries offset zero.
; Inputs D0 cost; outputs D0 status / D2 error offset; clobbers CCR.
tick	.block
	cmp.l State.Budget(a3), d0
	bhi exceeded
	sub.l d0, State.Budget(a3)
	clr.l d0
	rts
exceeded
	moveq #abi.PRVM_STATUS_BUDGET_EXCEEDED, d0
	clr.l d2
	tst.l d0
	rts
	.bend  ; tick

requireBytes	.block
	move.l a6, d1
	sub.l a5, d1
	cmp.l d0, d1
	bcs invalid
	clr.l d0
	rts
invalid
	move.l a5, d2
	sub.l abi.PRVM_FRAME_PROGRAM_PTR(a4), d2
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
	rts
	.bend  ; requireBytes

; Source columns are one-based byte offsets, ordered and non-overlapping.
validateTokens	.block
	movea.l State.Tokens(a3), a0
	movea.l State.Source(a3), a1
	move.l State.TokenCount(a3), d5
	clr.l d4
loop
	tst.l d5
	beq ready
	move.l 4(a0), d1
	subq.l #1, d1
	move.l d1, d2
	cmp.l d4, d1
	bcs invalid
	move.l 8(a0), d3
	subq.l #1, d3
	cmp.l d1, d3
	bls invalid
	cmp.l State.SourceLen(a3), d3
	bhi invalid
	move.b 0(a1, d1.l), d0
	andi.b #$C0, d0
	cmpi.b #$80, d0
	beq invalid
	cmp.l State.SourceLen(a3), d3
	beq aligned
	move.b 0(a1, d3.l), d0
	andi.b #$C0, d0
	cmpi.b #$80, d0
	beq invalid
aligned
	move.l d3, d4
	adda.l #abi.PRVM_TOKEN_RECORD_SIZE, a0
	subq.l #1, d5
	bra loop
ready
	clr.l d0
	rts
invalid
	moveq #abi.PRVM_STATUS_INVALID_TOKEN, d0
	rts
	.bend  ; validateTokens

; Construct one event in the temporary record without publishing it.
; Inputs D0 kind, D1/D2 byte span; outputs D0 status, Record complete.
; Clobbers D1-D7/A0-A1; CCR reflects D0.
record	.block
	lea State.Record(a3), a1
	move.w d0, 0(a1)
	clr.w 2(a1)
	move.l d1, 12(a1)
	move.l d2, 16(a1)
	move.l #$FFFFFFFF, 20(a1)
	move.l #$FFFFFFFF, 24(a1)
	move.l #$FFFFFFFF, 28(a1)
	moveq #1, d0
	bsr.w tick
	bne return
	move.l State.TokenCount(a3), d4
	clr.l d5
	movea.l State.Tokens(a3), a0
	clr.l d3
loop
	cmp.l State.TokenCount(a3), d3
	bcc mapped
	move.l 8(a0), d6
	subq.l #1, d6
	cmp.l d1, d6
	bls next
	cmp.l State.TokenCount(a3), d4
	bne next
	move.l d3, d4
next
	move.l 4(a0), d6
	subq.l #1, d6
	cmp.l d2, d6
	bcc noEnd
	move.l d3, d5
	addq.l #1, d5
noEnd
	adda.l #abi.PRVM_TOKEN_RECORD_SIZE, a0
	addq.l #1, d3
	bra loop
mapped
	move.l d4, 4(a1)
	move.l d5, 8(a1)
	cmp.l d1, d2
	beq ready
	cmp.l d5, d4
	bcc unaligned
	move.l d4, d0
	bsr.w tokenPtr
	move.l 4(a0), d6
	subq.l #1, d6
	cmp.l d1, d6
	bcs unaligned
	move.l d5, d0
	subq.l #1, d0
	bsr.w tokenPtr
	move.l 8(a0), d6
	subq.l #1, d6
	cmp.l d2, d6
	bhi unaligned
ready
	clr.l d0
return
	rts
unaligned
	move.l d1, d2
	moveq #abi.PRVM_STATUS_INVALID_TOKEN, d0
	rts
	.bend  ; record

tokenPtr	.block
	move.l d0, d6
	lsl.l #4, d0
	lsl.l #2, d6
	add.l d6, d0
	movea.l State.Tokens(a3), a0
	adda.l d0, a0
	rts
	.bend  ; tokenPtr

pushRecord	.block
	move.l State.Count(a3), d0
	cmp.l State.Capacity(a3), d0
	bcc overflow
	lsl.l #5, d0
	lea State.Stage(a3), a0
	adda.l d0, a0
	lea State.Record(a3), a1
	moveq #7, d0
copy
	move.l (a1)+, (a0)+
	dbra d0, copy
	addq.l #1, State.Count(a3)
	clr.l d0
	rts
overflow
	move.l RECORD_SOURCE_START(a3), d2
	moveq #abi.PRVM_STATUS_OUTPUT_OVERFLOW, d0
	rts
	.bend  ; pushRecord

; Inputs D1 byte offset; outputs D0 byte or -1 at the effective code end.
byteAt	.block
	cmp.l State.CodeEnd(a3), d1
	bcc end
	movea.l State.Source(a3), a0
	moveq #0, d0
	move.b 0(a0, d1.l), d0
	rts
end
	moveq #-1, d0
	rts
	.bend  ; byteAt

ws	.block
loop
	bsr.w byteAt
	cmpi.b #32, d0
	beq next
	cmpi.b #9, d0
	bne return
next
	addq.l #1, d1
	bra loop
return
	rts
	.bend  ; ws

; ASCII identifier, matching the shared source cursor.
; Inputs D1 start; outputs D0 boolean, D2 end; clobbers D6/A0.
ident	.block
	bsr.w byteAt
	cmpi.b #95, d0
	beq accepted
	bsr.w letter
	tst.l d0
	beq return
accepted
	move.l d1, d2
next
	addq.l #1, d2
	cmp.l State.CodeEnd(a3), d2
	bcc done
	movea.l State.Source(a3), a0
	moveq #0, d0
	move.b 0(a0, d2.l), d0
	cmpi.b #95, d0
	beq next
	cmpi.b #46, d0
	beq next
	cmpi.b #36, d0
	beq next
	cmpi.b #48, d0
	bcs alphabetic
	cmpi.b #57, d0
	bls next
alphabetic
	bsr.w letter
	tst.l d0
	bne next
done
	moveq #1, d0
return
	rts
	.bend  ; ident

letter	.block
	andi.b #$DF, d0
	cmpi.b #65, d0
	bcs no
	cmpi.b #90, d0
	bhi no
	moveq #1, d0
	rts
no
	clr.l d0
	rts
	.bend  ; letter

; UTF-8 Unicode White_Space widths used by Rust str.trim/split_whitespace.
; Inputs D1 offset, D2 exclusive limit; outputs D0 width or zero.
; Clobbers D6/A0; preserves D1-D5/D7.
spaceWidth	.block
	cmp.l d2, d1
	bcc none
	movea.l State.Source(a3), a0
	adda.l d1, a0
	moveq #0, d0
	move.b (a0), d0
	cmpi.b #32, d0
	beq one
	cmpi.b #9, d0
	bcs none
	cmpi.b #13, d0
	bls one
	move.l d2, d6
	sub.l d1, d6
	cmpi.l #2, d6
	bcs none
	cmpi.b #$C2, d0
	bne threeByte
	cmpi.b #$85, 1(a0)
	beq two
	cmpi.b #$A0, 1(a0)
	beq two
	bra none
threeByte
	cmpi.l #3, d6
	bcs none
	cmpi.b #$E1, d0
	bne utf8E2
	cmpi.w #$9A80, 1(a0)
	beq three
	bra none
utf8E2
	cmpi.b #$E2, d0
	bne utf8E3
	cmpi.b #$80, 1(a0)
	bne mediumMath
	move.b 2(a0), d0
	cmpi.b #$80, d0
	bcs none
	cmpi.b #$8a, d0
	bls three
	cmpi.b #$A8, d0
	beq three
	cmpi.b #$A9, d0
	beq three
	cmpi.b #$AF, d0
	beq three
	bra none
mediumMath
	cmpi.w #$819F, 1(a0)
	beq three
	bra none
utf8E3
	cmpi.b #$E3, d0
	bne none
	cmpi.w #$8080, 1(a0)
	beq three
none
	clr.l d0
	rts
one
	moveq #1, d0
	rts
two
	moveq #2, d0
	rts
three
	moveq #3, d0
	rts
	.bend  ; spaceWidth

; Inputs D1/D2 byte span; outputs trimmed D1/D2; clobbers D0/D3-D7/A0.
trim	.block
left
	bsr.w spaceWidth
	tst.l d0
	beq right
	add.l d0, d1
	bra left
right
	cmp.l d2, d1
	bcc return
	move.l d1, d4
	move.l d2, d5
	move.l d2, d1
	subq.l #1, d1
	movea.l State.Source(a3), a0
back
	move.b 0(a0, d1.l), d0
	andi.b #$C0, d0
	cmpi.b #$80, d0
	bne candidate
	cmp.l d4, d1
	bls candidate
	subq.l #1, d1
	bra back
candidate
	move.l d1, d7
	bsr.w spaceWidth
	tst.l d0
	beq untrimmed
	add.l d1, d0
	cmp.l d5, d0
	bne untrimmed
	move.l d4, d1
	move.l d7, d2
	bra right
untrimmed
	move.l d4, d1
	move.l d5, d2
return
	rts
	.bend  ; trim

; Comment recognition preserves all bytes before the first unquoted semicolon.
commentEnd	.block
	movea.l State.Source(a3), a0
	clr.l d1
	clr.l d4
loop
	cmp.l State.SourceLen(a3), d1
	bcc return
	moveq #0, d0
	move.b 0(a0, d1.l), d0
	tst.b d4
	beq unquoted
	cmpi.b #92, d0
	beq escaped
	cmp.b d4, d0
	bne next
	clr.l d4
	bra next
unquoted
	cmpi.b #59, d0
	beq end
	cmpi.b #39, d0
	beq quote
	cmpi.b #34, d0
	bne next
quote
	move.l d0, d4
	bra next
escaped
	addq.l #1, d1
next
	addq.l #1, d1
	bra loop
end
	move.l d1, State.CodeEnd(a3)
return
	rts
	.bend  ; commentEnd

; Inputs D1 opening parenthesis. Outputs D0 status/D2 closing offset.
; Escapes and quotes are original source bytes; bracket depths do not mask ')'.
parenEnd	.block
	move.l d1, State.Pos(a3)
	addq.l #1, d1
	moveq #1, d4
	clr.l d5
	movea.l State.Source(a3), a0
loop
	cmp.l State.CodeEnd(a3), d1
	bcc invalid
	moveq #0, d0
	move.b 0(a0, d1.l), d0
	tst.b d5
	beq unquoted
	cmpi.b #92, d0
	beq escaped
	cmp.b d5, d0
	bne next
	clr.l d5
	bra next
unquoted
	cmpi.b #39, d0
	beq quote
	cmpi.b #34, d0
	beq quote
	cmpi.b #40, d0
	beq open
	cmpi.b #41, d0
	bne next
	subq.l #1, d4
	beq ready
	bra next
open
	addq.l #1, d4
	bra next
quote
	move.l d0, d5
	bra next
escaped
	addq.l #1, d1
next
	addq.l #1, d1
	bra loop
ready
	move.l d1, d2
	clr.l d0
	rts
invalid
	move.l State.Pos(a3), d2
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
	rts
	.bend  ; parenEnd

; Compare only the selected header keyword, leaving name binding to the host.
; Inputs D1/D2 keyword span. Outputs D0 role (2/3 or zero).
headerRole	.block
	move.l d2, d6
	sub.l d1, d6
	cmpi.l #5, d6
	beq macro
	cmpi.l #7, d6
	bne no
	lea SegmentText(PC), a1
	moveq #3, d3
	bra compare
macro
	lea MacroText(PC), a1
	moveq #2, d3
compare
	movea.l State.Source(a3), a0
	adda.l d1, a0
loop
	move.b (a0)+, d0
	andi.b #$DF, d0
	cmp.b (a1)+, d0
	bne no
	subq.l #1, d6
	bne loop
	move.l d3, d0
	rts
no
	clr.l d0
	rts
	.bend  ; headerRole

; Inputs D4 mode / D5 policy flags; output D0 status/D2 error offset.
envelope	.block
	tst.l State.Envelope(a3)
	bne policyFailure
	cmpi.b #1, d4
	bcs policyFailure
	cmpi.b #2, d4
	bhi policyFailure
	move.l d5, d0
	andi.l #$FFFFFFF0, d0
	bne policyFailure
	cmpi.b #2, d4
	bne policyReady
	btst #2, d5
	bne policyFailure
policyReady
	move.l d5, State.Policy(a3)
	subq.l #1, d4
	move.l d4, State.Header(a3)
	move.l State.SourceLen(a3), d0
	bsr.w tick
	bne return
	btst #3, STATE_POLICY_BYTE(a3)
	beq codeReady
	bsr.w commentEnd
codeReady
	clr.l d1
	bsr.w ws
	btst #0, STATE_POLICY_BYTE(a3)
	beq dot
	bsr.w ident
	tst.l d0
	beq dot
	move.l d1, State.LabelStart(a3)
	move.l d2, State.LabelEnd(a3)
	move.l #1, State.HasLabel(a3)
	move.l d2, d1
	bsr.w byteAt
	cmpi.b #58, d0
	bne labelDone
	addq.l #1, d1
labelDone
	bsr.w ws
dot
	bsr.w byteAt
	cmpi.b #46, d0
	bne grammarFailure
	addq.l #1, d1
	tst.l State.Header(a3)
	beq keyword
	bsr.w ws
keyword
	move.l d1, State.OriginalNameStart(a3)
	bsr.w ident
	tst.l d0
	beq grammarFailure
	move.l d1, State.NameStart(a3)
	move.l d2, State.NameEnd(a3)
	move.l d2, State.OriginalNameEnd(a3)
	move.l d2, d1
	bsr.w ws
	move.l d1, State.Pos(a3)
	move.l #1, State.Role(a3)
	tst.l State.Header(a3)
	beq list
	move.l State.NameStart(a3), d1
	move.l State.NameEnd(a3), d2
	bsr.w headerRole
	tst.l d0
	beq keywordFailure
	move.l d0, State.Role(a3)
	tst.l State.HasLabel(a3)
	beq directiveName
	move.l State.LabelStart(a3), State.NameStart(a3)
	move.l State.LabelEnd(a3), State.NameEnd(a3)
	bra list
directiveName
	move.l State.Pos(a3), d1
	bsr.w ident
	tst.l d0
	beq grammarFailure
	move.l d1, State.NameStart(a3)
	move.l d2, State.NameEnd(a3)
	move.l d2, d1
	bsr.w ws
	move.l d1, State.Pos(a3)
list
	move.l State.Pos(a3), d1
	move.l d1, State.ListStart(a3)
	move.l State.CodeEnd(a3), State.ListEnd(a3)
	bsr.w byteAt
	cmpi.b #40, d0
	bne bare
	btst #1, STATE_POLICY_BYTE(a3)
	beq bare
	tst.l State.Header(a3)
	beq parentheses
	tst.l State.HasLabel(a3)
	bne bare
parentheses
	bsr.w parenEnd
	bne return
	move.l d2, State.ListEnd(a3)
	move.l State.Pos(a3), d1
	addq.l #1, d1
	move.l d1, State.ListStart(a3)
	move.l d2, d1
	addq.l #1, d1
	move.l d1, State.Scan(a3)
	move.l State.CodeEnd(a3), d2
	bsr.w trim
	cmp.l d2, d1
	bne trailingFailure
	bra trimHeader
bare
	tst.l State.Header(a3)
	bne trimHeader
	btst #2, STATE_POLICY_BYTE(a3)
	beq trimHeader
	move.l State.Pos(a3), d1
	bsr.w byteAt
	cmpi.b #44, d0
	bne trimHeader
	addq.l #1, d1
	bsr.w ws
	move.l d1, State.ListStart(a3)
	cmp.l State.CodeEnd(a3), d1
	bcc commaFailure
trimHeader
	tst.l State.Header(a3)
	beq line
	move.l State.ListStart(a3), d1
	move.l State.ListEnd(a3), d2
	bsr.w trim
	move.l d1, State.ListStart(a3)
	move.l d2, State.ListEnd(a3)
line
	moveq #abi.PRVM_RESULT_MACRO_LINE, d0
	move.l State.NameStart(a3), d1
	move.l State.NameEnd(a3), d2
	bsr.w record
	bne return
	move.l State.Role(a3), d0
	move.w d0, RECORD_FLAGS(a3)
	move.l State.ListStart(a3), RECORD_SOURCE_START(a3)
	move.l State.ListEnd(a3), RECORD_SOURCE_END(a3)
	move.l #1, RECORD_AUX0(a3)
	clr.l RECORD_AUX1(a3)
	tst.l State.HasLabel(a3)
	beq push
	move.l State.LabelStart(a3), d1
	movea.l State.Tokens(a3), a0
	clr.l d0
labelToken
	cmp.l State.TokenCount(a3), d0
	bcc labelMapped
	move.l 8(a0), d6
	subq.l #1, d6
	cmp.l d1, d6
	bhi labelMapped
	addq.l #1, d0
	adda.l #abi.PRVM_TOKEN_RECORD_SIZE, a0
	bra labelToken
labelMapped
	move.l d0, RECORD_AUX2(a3)
push
	bsr.w pushRecord
	bne return
	move.l #1, State.Envelope(a3)
	clr.l d0
return
	rts
keywordFailure
	move.l State.OriginalNameStart(a3), d1
	bra grammarFailure
trailingFailure
	move.l State.Scan(a3), d1
	bra grammarFailure
commaFailure
	move.l State.Pos(a3), d1
grammarFailure
	move.l d1, d2
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
	rts
policyFailure
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
	clr.l d2
	tst.l d0
	rts
	.bend  ; envelope

; Split the VM-selected source list with three independent saturated depths.
split	.block
	cmpi.l #1, State.Count(a3)
	bne policyFailure
	move.l State.ListStart(a3), d1
	move.l State.ListEnd(a3), d2
	move.l d2, d0
	sub.l d1, d0
	bsr.w tick
	bne return
	bsr.w trim
	cmp.l d1, d2
	beq empty
	move.l State.ListStart(a3), State.Start(a3)
	move.l State.ListStart(a3), State.Scan(a3)
	clr.l State.Quote(a3)
	clr.l State.Paren(a3)
	clr.l State.Bracket(a3)
	clr.l State.Brace(a3)
loop
	move.l State.Scan(a3), d1
	cmp.l State.ListEnd(a3), d1
	beq part
	bsr.w byteAt
	cmpi.b #44, d0
	bne character
	tst.l State.Quote(a3)
	bne character
	tst.l State.Paren(a3)
	bne character
	tst.l State.Bracket(a3)
	bne character
	tst.l State.Brace(a3)
	bne character
part
	move.l State.Scan(a3), d2
	move.l State.Start(a3), d1
	bsr.w trim
	cmp.l d2, d1
	beq emptyPart
	moveq #abi.PRVM_RESULT_MACRO_ARGUMENT, d0
	bsr.w record
	bne return
	bsr.w pushRecord
	bne return
	move.l State.Scan(a3), d1
	cmp.l State.ListEnd(a3), d1
	beq done
	addq.l #1, d1
	move.l d1, State.Start(a3)
	move.l d1, State.Scan(a3)
	bra loop
character
	tst.l State.Quote(a3)
	beq outside
	cmpi.b #92, d0
	beq escape
	cmp.l State.Quote(a3), d0
	bne next
	clr.l State.Quote(a3)
	bra next
outside
	cmpi.b #39, d0
	beq quote
	cmpi.b #34, d0
	beq quote
	cmpi.b #40, d0
	beq openParen
	cmpi.b #41, d0
	beq closeParen
	cmpi.b #91, d0
	beq openBracket
	cmpi.b #93, d0
	beq closeBracket
	cmpi.b #123, d0
	beq openBrace
	cmpi.b #125, d0
	beq closeBrace
	bra next
quote
	move.l d0, State.Quote(a3)
	bra next
escape
	move.l State.Scan(a3), d1
	addq.l #1, d1
	cmp.l State.ListEnd(a3), d1
	bcc next
	move.l d1, State.Scan(a3)
	bra next
openParen
	addq.l #1, State.Paren(a3)
	bra next
closeParen
	tst.l State.Paren(a3)
	beq next
	subq.l #1, State.Paren(a3)
	bra next
openBracket
	addq.l #1, State.Bracket(a3)
	bra next
closeBracket
	tst.l State.Bracket(a3)
	beq next
	subq.l #1, State.Bracket(a3)
	bra next
openBrace
	addq.l #1, State.Brace(a3)
	bra next
closeBrace
	tst.l State.Brace(a3)
	beq next
	subq.l #1, State.Brace(a3)
next
	addq.l #1, State.Scan(a3)
	bra loop
done
	move.l State.Count(a3), d0
	subq.l #1, d0
	move.l d0, STAGE_CHILD_COUNT(a3)
empty
	clr.l d0
return
	rts
emptyPart
	move.l d1, d2
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
	rts
policyFailure
	clr.l d2
	moveq #abi.PRVM_STATUS_INVALID_PROGRAM, d0
	rts
	.bend  ; split

; Find a non-whitespace word inside the current formal's selected left span.
; Inputs D1 start; outputs D2 end; clobbers D0/D6/A0.
wordEnd	.block
	move.l d1, -(sp)
	move.l State.End(a3), d2
loop
	cmp.l d2, d1
	bcc done
	bsr.w spaceWidth
	tst.l d0
	bne done
	addq.l #1, d1
	bra loop
done
	move.l d1, d2
	move.l (sp)+, d1
	rts
	.bend  ; wordEnd

formalSpaces	.block
	move.l State.End(a3), d2
loop
	bsr.w spaceWidth
	tst.l d0
	beq return
	add.l d0, d1
	bra loop
return
	rts
	.bend  ; formalSpaces

; A0 addresses the staged row for the current formal index.
formalRow	.block
	move.l State.FormalIndex(a3), d0
	lsl.l #5, d0
	lea State.Stage(a3), a0
	adda.l d0, a0
	rts
	.bend  ; formalRow

; Interpret optional type/name and first original '='; defaults retain spelling.
formals	.block
	move.l STAGE_CHILD_COUNT(a3), State.FormalCount(a3)
	move.l #1, State.FormalIndex(a3)
loop
	move.l State.FormalIndex(a3), d0
	cmp.l State.FormalCount(a3), d0
	bhi done
	bsr.w formalRow
	move.l 12(a0), State.Start(a3)
	move.l 16(a0), State.End(a3)
	move.l 16(a0), State.DefaultEnd(a3)
	move.l State.End(a3), d0
	sub.l State.Start(a3), d0
	bsr.w tick
	bne return
	clr.l State.HasDefault(a3)
	move.l State.Start(a3), d1
	movea.l State.Source(a3), a0
findEq
	cmp.l State.End(a3), d1
	bcc left
	cmpi.b #61, 0(a0, d1.l)
	beq foundEq
	addq.l #1, d1
	bra findEq
foundEq
	move.l #1, State.HasDefault(a3)
	move.l d1, State.End(a3)
	addq.l #1, d1
	move.l d1, State.DefaultStart(a3)
left
	move.l State.Start(a3), d1
	move.l State.End(a3), d2
	bsr.w trim
	move.l d1, State.Start(a3)
	move.l d2, State.End(a3)
	cmp.l d2, d1
	beq formatFailure
	bsr.w wordEnd
	move.l d1, State.OriginalNameStart(a3)
	move.l d2, State.OriginalNameEnd(a3)
	move.l d1, State.NameStart(a3)
	move.l d2, State.NameEnd(a3)
	move.l #$FFFFFFFF, State.TypeToken(a3)
	move.l d2, d1
	bsr.w formalSpaces
	cmp.l State.End(a3), d1
	bcc validateNames
	bsr.w wordEnd
	move.l d1, State.NameStart(a3)
	move.l d2, State.NameEnd(a3)
	clr.l State.TypeToken(a3)
	move.l d2, d1
	bsr.w formalSpaces
	cmp.l State.End(a3), d1
	bcs formatFailure
validateNames
	move.l State.OriginalNameStart(a3), d1
	bsr.w ident
	tst.l d0
	beq identityFailure
	cmp.l State.OriginalNameEnd(a3), d2
	bne identityFailure
	move.l State.NameStart(a3), d1
	bsr.w ident
	tst.l d0
	beq identityFailure
	cmp.l State.NameEnd(a3), d2
	bne identityFailure
	moveq #abi.PRVM_RESULT_MACRO_FORMAL, d0
	move.l State.NameStart(a3), d1
	move.l State.NameEnd(a3), d2
	bsr.w record
	bne return
	bsr.w formalRow
	lea State.Record(a3), a1
	moveq #7, d0
copyFormal
	move.l (a1)+, (a0)+
	dbra d0, copyFormal
	tst.l State.TypeToken(a3)
	bne default
	moveq #abi.PRVM_RESULT_MACRO_FORMAL, d0
	move.l State.OriginalNameStart(a3), d1
	move.l State.OriginalNameEnd(a3), d2
	bsr.w record
	bne return
	move.l RECORD_TOKEN_START(a3), d1
	bsr.w formalRow
	move.l d1, 20(a0)
default
	tst.l State.HasDefault(a3)
	beq next
	move.l State.DefaultStart(a3), d1
	move.l State.DefaultEnd(a3), d2
	bsr.w trim
	move.l d1, State.DefaultStart(a3)
	move.l d2, State.DefaultEnd(a3)
	bsr.w formalRow
	move.l State.Count(a3), 24(a0)
	moveq #abi.PRVM_RESULT_MACRO_DEFAULT, d0
	bsr.w record
	bne return
	bsr.w pushRecord
	bne return
next
	addq.l #1, State.FormalIndex(a3)
	bra loop
done
	clr.l d0
return
	rts
formatFailure
	move.l State.Start(a3), d1
identityFailure
	move.l d1, d2
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
	rts
	.bend  ; formals

MacroText
	.byte "MACRO"
SegmentText
	.byte "SEGMENT"
	.align 2
	.endsection
	.endmodule
