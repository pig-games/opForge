; Package-selected boundaries over immutable compact records. Tokens are opaque
; payloads; only package-selected punctuation grammar is traversed here.
	.module prvm.amigaos.packed_macro
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.include "telemetry_macros.i"
	.pub
State	.struct
Source	.long ?
End	.long ?
Budget	.long ?
Count	.long ?
Capacity	.long ?
Flags	.long ?
Head	.long ?
Label	.long ?
ListStart	.long ?
ListEnd	.long ?
ArgStart	.long ?
Depth	.long ?
DelimiterStack	.res 16
Records	.long ?
Kinds	.res 256
Starts	.res 256*2
Stage	.res 64*32
	.endstruct
STATE_BYTES = State.Stage+64*32
FLAGS_BYTE = State.Flags+3
CHILD_COUNT = State.Stage+24
CLEAR_LONGS = State.Kinds/4-1
	.section code, kind=code
; A0 frame/D0 frame bytes; identical PRVM return ABI, atomic publication.
run	.block
	movem.l d4-d7/a3-a6, -(sp)
	move.l a0, d1
	beq.w invalidFrame
	cmpi.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	bcs.w invalidFrame
	movea.l a0, a4
	cmpi.l #abi.PRVM_MAGIC_OPRP, abi.PRVM_FRAME_MAGIC(a4)
	bne.w invalidFrame
	cmpi.w #abi.PRVM_ABI_VERSION_V1, abi.PRVM_FRAME_ABI_VERSION(a4)
	bne.w invalidFrame
	cmpi.w #abi.PRVM_REQUEST_FRAME_SIZE, abi.PRVM_FRAME_FRAME_SIZE(a4)
	bcs.w invalidFrame
	cmpi.w #abi.PRVM_ENTRY_KIND_PACKED_MACRO, abi.PRVM_FRAME_ENTRY_KIND(a4)
	bne.w invalidFrame
	tst.w abi.PRVM_FRAME_CALL_MODE(a4)
	bne.w invalidFrame
	cmpi.l #abi.PRVM_PARSER_CONTRACT_VERSION_V2, abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_FLAGS(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_TOKEN_PTR(a4)
	bne.w invalidFrame
	tst.l abi.PRVM_FRAME_TOKEN_COUNT(a4)
	bne.w invalidFrame
	suba.l #STATE_BYTES, sp
	movea.l sp, a3
	movea.l a3, a0
	moveq #CLEAR_LONGS, d0
clear
	clr.l (a0)+
	dbra d0, clear
	clr.l d2
	move.l #-1, State.Label(a3)
	move.l abi.PRVM_FRAME_STEP_BUDGET(a4), State.Budget(a3)
	bmi.w argumentFailure
	move.l abi.PRVM_FRAME_RESULT_PTR(a4), d0
	beq.w argumentFailure
	move.l abi.PRVM_FRAME_RESULT_CAPACITY(a4), d0
	bmi.w argumentFailure
	lsr.l #5, d0
	cmpi.l #64, d0
	bls.w capacity
	moveq #64, d0
capacity
	move.l d0, State.Capacity(a3)
	move.l abi.PRVM_FRAME_SOURCE_PTR(a4), d0
	beq.w argumentFailure
	move.l d0, State.Source(a3)
	movea.l d0, a6
	move.l abi.PRVM_FRAME_SOURCE_LEN(a4), d6
	cmpi.l #4, d6
	bcs.w argumentFailure
	cmpi.l #256, d6
	bhi.w argumentFailure
	moveq #0, d0
	move.b (a6), d0
	addq.l #1, d0
	cmp.l d6, d0
	bne.w argumentFailure
	move.l d6, State.Head(a3)
	btst #5, 1(a6)
	beq.w rawEnd
	cmpi.l #10, d6
	bcs.w tokenFailure
	cmpi.b #42, -6(a6, d6.l)
	bne.w tokenFailure
	cmpi.b #6, -1(a6, d6.l)
	bne.w tokenFailure
	subq.l #6, d6
rawEnd
	move.l d6, State.End(a3)
	moveq #4, d2
	clr.l d4
validate
	cmp.l d6, d2
	bcc.w tokensReady
	move.l d2, State.Head(a3)
	bsr.w tick
	bne.w failure
	moveq #0, d0
	move.b 0(a6, d2.l), d0
	move.b d0, State.Kinds(a3, d4.l)
	move.l d4, d1
	add.l d1, d1
	lea State.Starts(a3), a2
	move.w d2, 0(a2, d1.l)
	cmpi.b #1, d0
	bls.w name
	cmpi.b #2, d0
	beq.w number
	cmpi.b #3, d0
	beq.w payload
	cmpi.b #41, d0
	beq.w payload
	cmpi.b #40, d0
	bhi.w tokenFailure
	addq.l #1, d2
	bra.w extent
name
	addq.l #4, d2
	bra.w extent
number
	addq.l #5, d2
	bra.w extent
payload
	move.l d2, d5
	addq.l #2, d5
	cmp.l d6, d5
	bhi.w tokenFailure
	moveq #0, d0
	move.b 1(a6, d2.l), d0
	add.l d0, d5
	move.l d5, d2
extent
	cmp.l d6, d2
	bhi.w tokenFailure
	addq.l #1, d4
	bra.w validate
tokensReady
	move.l d4, State.Count(a3)
	move.l abi.PRVM_FRAME_PROGRAM_PTR(a4), d0
	beq.w argumentFailure
	movea.l d0, a5
	cmpi.l #7, abi.PRVM_FRAME_PROGRAM_LEN(a4)
	bne.w programExtentFailure
	cmpi.b #$84, (a5)
	bne.w programFailure
	moveq #0, d0
	move.b 1(a5), d0
	move.l d0, State.Flags(a3)
	andi.b #$F8, d0
	bne.w programFlagsFailure
	cmpi.b #$85, 2(a5)
	bne.w programFailure
	cmpi.b #2, 3(a5)
	bne.w programFailure
	cmpi.b #4, 4(a5)
	bne.w programFailure
	cmpi.b #$83, 5(a5)
	bne.w programFailure
	tst.b 6(a5)
	bne.w programFailure
	bsr.w tick
	bne.w failure
	move.l #$84, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	clr.l d4
	btst #0, FLAGS_BYTE(a3)
	beq.w dot
	cmp.l State.Count(a3), d4
	bcc.w envelopeFailure
	cmpi.b #1, State.Kinds(a3, d4.l)
	bhi.w dot
	move.l #4, State.Label(a3)
	addq.l #1, d4
	cmp.l State.Count(a3), d4
	bcc.w envelopeFailure
	cmpi.b #5, State.Kinds(a3, d4.l)
	bne.w dot
	addq.l #1, d4
dot
	move.l d4, d0
	addq.l #2, d0
	cmp.l State.Count(a3), d0
	bhi.w envelopeFailure
	cmpi.b #7, State.Kinds(a3, d4.l)
	bne.w envelopeFailure
	addq.l #1, d4
	cmpi.b #1, State.Kinds(a3, d4.l)
	bhi.w envelopeFailure
	move.l d4, State.Head(a3)
	addq.l #1, d4
	move.l State.Count(a3), d5
	cmp.l d5, d4
	bcc.w listReady
	btst #1, FLAGS_BYTE(a3)
	beq.w leadingComma
	cmpi.b #14, State.Kinds(a3, d4.l)
	bne.w leadingComma
	move.l d4, State.ArgStart(a3)
	moveq #1, d6
	addq.l #1, d4
	move.l d4, d2
outer
	cmp.l d5, d2
	bcc.w outerFailure
	bsr.w tick
	bne.w failure
	cmpi.b #14, State.Kinds(a3, d2.l)
	bne.w close
	addq.l #1, d6
close
	cmpi.b #15, State.Kinds(a3, d2.l)
	bne.w outerNext
	subq.l #1, d6
	beq.w outerDone
outerNext
	addq.l #1, d2
	bra.w outer
outerDone
	move.l d2, d0
	addq.l #1, d0
	cmp.l d5, d0
	bne.w envelopeFailure
	move.l d2, d5
	bra.w listReady
outerFailure
	move.l State.ArgStart(a3), d4
	bra.w envelopeFailure
leadingComma
	btst #2, FLAGS_BYTE(a3)
	beq.w listReady
	cmpi.b #4, State.Kinds(a3, d4.l)
	bne.w listReady
	addq.l #1, d4
	cmp.l d5, d4
	bcc.w envelopeFailure
listReady
	move.l d4, State.ListStart(a3)
	move.l d5, State.ListEnd(a3)
	move.l d4, State.ArgStart(a3)
	lea State.Stage(a3), a0
	move.w #8, (a0)+
	move.w #1, (a0)+
	move.l d4, d6
	move.l State.Head(a3), d4
	bsr.w sourceStart
	move.l d0, (a0)+
	addq.l #1, d4
	bsr.w sourceStart
	move.l d0, (a0)+
	move.l d6, d4
	bsr.w sourceStart
	move.l d0, (a0)+
	move.l d5, d4
	bsr.w sourceStart
	move.l d0, (a0)+
	move.l #1, (a0)+
	clr.l (a0)+
	move.l State.Label(a3), (a0)+
	move.l #1, State.Records(a3)
	bsr.w tick
	bne.w failure
	move.l #$85, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.l State.ListStart(a3), d4
split
	bsr.w tick
	bne.w failure
	cmp.l State.ListEnd(a3), d4
	beq.w boundary
	moveq #0, d0
	move.b State.Kinds(a3, d4.l), d0
	cmpi.b #4, d0
	bne.w delimiter
	move.l State.Depth(a3), d1
	bne.w next
boundary
	tst.l State.Depth(a3)
	bne.w envelopeFailure
	cmp.l State.ArgStart(a3), d4
	bne.w argument
	cmp.l State.ListStart(a3), d4
	bne.w envelopeFailure
	cmp.l State.ListEnd(a3), d4
	beq.w publish
	bra.w envelopeFailure
argument
	move.l State.Records(a3), d0
	cmp.l State.Capacity(a3), d0
	bcc.w overflow
	lsl.l #5, d0
	lea State.Stage(a3), a2
	lea 0(a2, d0.l), a0
	move.w #9, (a0)+
	clr.w (a0)+
	move.l d4, d6
	move.l State.ArgStart(a3), d4
	bsr.w sourceStart
	move.l d0, (a0)+
	move.l d6, d4
	bsr.w sourceStart
	move.l d0, (a0)+
	move.l d4, d6
	move.l State.ArgStart(a3), d4
	bsr.w sourceStart
	move.l d0, (a0)+
	move.l d6, d4
	bsr.w sourceStart
	move.l d0, (a0)+
	move.l #-1, (a0)+
	move.l #-1, (a0)+
	move.l #-1, (a0)+
	addq.l #1, State.Records(a3)
	move.l d4, d0
	addq.l #1, d0
	move.l d0, State.ArgStart(a3)
	cmp.l State.ListEnd(a3), d4
	beq.w publish
	bra.w next
delimiter
	cmpi.b #14, d0
	beq.w opening
	cmpi.b #10, d0
	beq.w opening
	cmpi.b #12, d0
	beq.w opening
	cmpi.b #15, d0
	beq.w closing
	cmpi.b #11, d0
	beq.w closing
	cmpi.b #13, d0
	bne.w next
closing
	move.l State.Depth(a3), d1
	beq.w envelopeFailure
	subq.l #1, d1
	subq.b #1, d0
	lea State.DelimiterStack(a3), a0
	cmp.b 0(a0, d1.l), d0
	bne.w envelopeFailure
	move.l d1, State.Depth(a3)
	bra.w next
opening
	move.l State.Depth(a3), d1
	cmpi.l #16, d1
	bcc.w envelopeFailure
	lea State.DelimiterStack(a3), a0
	move.b d0, 0(a0, d1.l)
	addq.l #1, State.Depth(a3)
next
	addq.l #1, d4
	bra.w split
publish
	move.l State.Records(a3), d1
	cmp.l State.Capacity(a3), d1
	bhi.w overflow
	move.l d1, d0
	subq.l #1, d0
	move.l d0, CHILD_COUNT(a3)
	bsr.w tick
	bne.w failure
	move.l #$83, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	bsr.w tick
	bne.w failure
	moveq #0, d7
	.TELEMETRY_VM_OPCODE runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.l State.Records(a3), d1
	move.l d1, d3
	lsl.l #5, d3
	move.l d3, d0
	lea State.Stage(a3), a0
	movea.l abi.PRVM_FRAME_RESULT_PTR(a4), a1
copy
	move.l (a0)+, (a1)+
	subq.l #4, d0
	bne.w copy
	clr.l d2
	bra.w return
argumentFailure
	moveq #4, d0
	bra.w zeroError
envelopeFailure
	bsr.w sourceStart
	move.l d0, d2
	moveq #4, d0
	bra.w failure
tokenFailure
	move.l State.Head(a3), d2
	moveq #5, d0
	bra.w failure
programExtentFailure
	moveq #7, d2
	bra.w programError
programFlagsFailure
	moveq #2, d2
	bra.w programError
programFailure
	clr.l d2
programError
	moveq #6, d0
	bra.w failure
overflow
	moveq #7, d0
zeroError
	clr.l d2
failure
	clr.l d1
	clr.l d3
return
	adda.l #STATE_BYTES, sp
	movem.l (sp)+, d4-d7/a3-a6
	tst.l d0
	rts
invalidFrame
	moveq #4, d0
	clr.l d1
	clr.l d2
	clr.l d3
	movem.l (sp)+, d4-d7/a3-a6
	tst.l d0
	rts
	.bend  ; run
	.priv
; D4 token ordinal -> D0 source offset. End ordinal resolves raw record end.
sourceStart	.block
	move.l State.End(a3), d0
	cmp.l State.Count(a3), d4
	bcc.w done
	move.l d4, d0
	add.l d0, d0
	lea State.Starts(a3), a2
	move.w 0(a2, d0.l), d0
	andi.l #$FFFF, d0
done
	rts
	.bend  ; sourceStart
tick	.block
	tst.l State.Budget(a3)
	beq.w exceeded
	subq.l #1, State.Budget(a3)
	clr.l d0
	rts
exceeded
	moveq #12, d0
	clr.l d2
	tst.l d0
	rts
	.bend  ; tick
	.endsection
	.endmodule
