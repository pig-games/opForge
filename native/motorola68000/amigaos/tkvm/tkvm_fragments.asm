; VM-owned logical line materialization. Fragment pointers are transient request
; views; persisted spelling recipes must retain offsets into owned storage.
; @opforge-owner: tkvm.amigaos.fragments
	.module tkvm.amigaos.fragments
	.cpu 68020
	.include "memory_telemetry.i"
	.pub
	.use tkvm.amigaos.runtime

Frame	.struct
Fragments	.long ?
Count	.long ?
InputBytes	.long ?
Tokens	.long ?
TokenCapacity	.long ?
Lexemes	.long ?
LexemeCapacity	.long ?
Program	.long ?
ProgramBytes	.long ?
	.endstruct
Fragment	.struct
Bytes	.long ?
Length	.long ?
	.endstruct
FRAME_BYTES = Frame.ProgramBytes+4
FRAGMENT_BYTES = Fragment.Length+4
MAX_BYTES = 1024
; Three half-open writable ranges, then the private logical line.
RANGES = 0
LINE = 24
LOCAL_BYTES = LINE+MAX_BYTES

	.section code, kind=code
; A0 = readable aligned Frame. Nonempty fragment/table/program views must be
; readable; nonempty outputs must be writable. Pointer accessibility remains a
; caller obligation. Counts/capacities are unsigned nonnegative signed longs.
; D0 = runtime.TK_STATUS_*; D1 = tokens; D2 = logical cursor; D3 = lexeme bytes.
; Clobbers A0-A3/CCR; preserves D4-D7/A4-A6. CCR reflects D0.
; Joins <=1024 bytes privately on stack, including boundaries inside a token;
; package-selected runtime program/state determines all token semantics. The
; caller installs the same TKVM state as for runtime.tkvmRun68000. No pointer to
; joined text escapes. CR/LF and partial-progress statuses come from that VM.
; Invalid requests return INVALID_ARGUMENT and zero progress without VM writes.
; Writable ranges cannot overlap each other, this call's stack frame, request,
; fragment table, bytecode or fragment bytes. Zero-length ranges do not alias.
run	.block
	movem.l d4-d7/a4-a6, -(sp)
	suba.l #LOCAL_BYTES, sp
	movea.l sp, a6
	movea.l a0, a4
	move.l a0, d0
	beq invalid
	btst #0, d0
	bne invalid
	moveq #FRAME_BYTES, d1
	jsr rangeEnd
	bne invalid
	move.l Frame.Count(a4), d7
	cmpi.l #MAX_BYTES, d7
	bhi invalid
	move.l Frame.InputBytes(a4), d0
	cmpi.l #MAX_BYTES, d0
	bhi invalid

	move.l Frame.Tokens(a4), d0
	btst #0, d0
	bne invalid
	move.l Frame.TokenCapacity(a4), d1
	cmpi.l #$06666666, d1  ; 20*capacity must fit a signed long
	bhi invalid
	move.l d1, d2
	lsl.l #2, d1
	lsl.l #4, d2
	add.l d2, d1
	jsr rangeEnd
	bne invalid
	move.l d2, RANGES(a6)
	move.l d3, RANGES+4(a6)
	move.l Frame.Lexemes(a4), d0
	move.l Frame.LexemeCapacity(a4), d1
	jsr rangeEnd
	bne invalid
	move.l d2, RANGES+8(a6)
	move.l d3, RANGES+12(a6)
	move.l a6, RANGES+16(a6)
	lea LOCAL_BYTES+32(a6), a0  ; saved registers and return address included
	move.l a0, RANGES+20(a6)

	; Each output must be disjoint from the other output and private stack.
	moveq #0, d6
outputPair
	move.l RANGES(a6), d4
	move.l RANGES+4(a6), d5
	move.l RANGES+8(a6, d6.l), d2
	move.l RANGES+12(a6, d6.l), d3
	jsr disjoint
	bne invalid
	addq.l #8, d6
	cmpi.l #16, d6
	blo outputPair
	move.l RANGES+8(a6), d4
	move.l RANGES+12(a6), d5
	move.l RANGES+16(a6), d2
	move.l RANGES+20(a6), d3
	jsr disjoint
	bne invalid

	move.l a4, d0
	moveq #FRAME_BYTES, d1
	jsr readable
	bne invalid
	move.l Frame.Fragments(a4), d0
	btst #0, d0
	bne invalid
	move.l d7, d1
	lsl.l #3, d1
	jsr readable
	bne invalid
	move.l Frame.Program(a4), d0
	move.l Frame.ProgramBytes(a4), d1
	jsr readable
	bne invalid
	movea.l Frame.Fragments(a4), a5
	moveq #0, d6
validateFragment
	tst.l d7
	beq validated
	move.l Fragment.Bytes(a5), d0
	move.l Fragment.Length(a5), d1
	cmpi.l #MAX_BYTES, d1
	bhi invalid
	add.l d1, d6
	cmpi.l #MAX_BYTES, d6
	bhi invalid
	jsr readable
	bne invalid
	adda.l #FRAGMENT_BYTES, a5
	subq.l #1, d7
	bra validateFragment
validated
	cmp.l Frame.InputBytes(a4), d6
	bne invalid
	; Validate the complete request before writing even private materialization.
	movea.l Frame.Fragments(a4), a5
	move.l Frame.Count(a4), d7
	lea LINE(a6), a1
copyFragment
	tst.l d7
	beq tokenize
	movea.l Fragment.Bytes(a5), a0
	move.l Fragment.Length(a5), d0
	.TOKEN_WORK #3, d0  ; source reads for private materialization
copyByte
	tst.l d0
	beq nextFragment
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bra copyByte
nextFragment
	adda.l #FRAGMENT_BYTES, a5
	subq.l #1, d7
	bra copyFragment
tokenize
	lea LINE(a6), a0
	move.l Frame.InputBytes(a4), d0
	movea.l Frame.Tokens(a4), a1
	move.l Frame.TokenCapacity(a4), d1
	movea.l Frame.Lexemes(a4), a2
	move.l Frame.LexemeCapacity(a4), d2
	movea.l Frame.Program(a4), a3
	move.l Frame.ProgramBytes(a4), d3
	jsr runtime.tkvmRun68000
	bra return
invalid
	moveq #runtime.TK_STATUS_INVALID_ARGUMENT, d0
	moveq #0, d1
	moveq #0, d2
	moveq #0, d3
return
	adda.l #LOCAL_BYTES, sp
	movem.l (sp)+, d4-d7/a4-a6
	tst.l d0
	rts
	.bend  ; run

	.priv
; D0 base/D1 length -> D2 base/D3 exclusive end; D0 status, CCR reflects it.
rangeEnd	.block
	move.l d0, d2
	move.l d0, d3
	tst.l d1
	bmi invalid
	beq valid
	tst.l d0
	beq invalid
	add.l d1, d3
	bcs invalid
valid
	moveq #0, d0
	rts
invalid
	moveq #runtime.TK_STATUS_INVALID_ARGUMENT, d0
	rts
	.bend  ; rangeEnd

; D4/D5 and D2/D3 half-open ranges; D0 status, CCR reflects it.
disjoint	.block
	cmp.l d4, d5
	beq valid
	cmp.l d2, d3
	beq valid
	cmp.l d3, d4
	bcc valid
	cmp.l d5, d2
	bcc valid
	moveq #runtime.TK_STATUS_INVALID_ARGUMENT, d0
	rts
valid
	moveq #0, d0
	rts
	.bend  ; disjoint

; Check D0 base/D1 length against all writable ranges. Clobbers D0-D5/A0;
; D6/D7 and A4-A6 retain orchestration state. CCR reflects D0.
readable	.block
	jsr rangeEnd
	bne return
	move.l d2, d4
	move.l d3, d5
	lea RANGES(a6), a0
	move.l (a0)+, d2
	move.l (a0)+, d3
	jsr disjoint
	bne return
	move.l (a0)+, d2
	move.l (a0)+, d3
	jsr disjoint
	bne return
	move.l (a0)+, d2
	move.l (a0)+, d3
	jsr disjoint
return
	rts
	.bend  ; readable
	.endsection
	.endmodule
