; Bounded preparation-only macro and segment templates over numeric writer records.
; Definition storage contains record bytes and numeric identifiers, never text.
	.module experimental.amigaos.binary_templates
	.cpu 68020
	.use experimental.amigaos.binary_scopes as scopes
	.pub
LIMIT = 8
ARG_LIMIT = 64
BODY_BYTES = 4096
TOKEN_OPEN_PAREN = 14
TOKEN_CLOSE_PAREN = 15
ACTION_REGULAR = 0
ACTION_CONSUMED = 1
ACTION_INVOKE = 2
State	.struct
Count	.word ?
Open	.word ?
Skipping	.word ?
Used	.word ?
Call	.word ?
Cursor	.word ?
ArgBytes	.word ?
CallLine	.word ?
CallLabel	.long ?
CallLabelPresent	.word ?
CallPhase	.word ?
Serial	.word ?
	.endstruct
Def	.struct
Name	.word ?
Parameter	.word ?
First	.word ?
Last	.word ?
Kind	.word ?
	.endstruct
DEF_BYTES = Def.Kind+2
KIND_SEGMENT = 0
KIND_MACRO = 1
DEFS = State.Serial+2
ARGUMENT = DEFS+LIMIT*DEF_BYTES
BODY = ARGUMENT+ARG_LIMIT
SCRATCH_BYTES = BODY+BODY_BYTES
	.section code, kind=code

; A0=caller-owned state. Clears definitions for a new assembly session.
; D0/CCR=zero; other registers preserved.
begin	.block
	clr.w State.Count(a0)
	clr.w State.Open(a0)
	clr.w State.Skipping(a0)
	clr.w State.Used(a0)
	clr.w State.Call(a0)
	clr.w State.Cursor(a0)
	clr.w State.ArgBytes(a0)
	clr.w State.CallLine(a0)
	clr.l State.CallLabel(a0)
	clr.w State.CallLabelPresent(a0)
	clr.w State.CallPhase(a0)
	clr.w State.Serial(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=state. A definition cannot cross a physical source boundary.
; D0/CCR=status; other registers preserved. Definitions remain reusable.
endFile	.block
	moveq #0, d0
	tst.w State.Open(a0)
	bne.w bad
	tst.w State.Skipping(a0)
	beq.w done
bad
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; endFile

; A0=raw writer record,A1=template state,A2=scope state,D0=conditional
; active flag. D0/CCR=status,D1=ACTION_*; other registers preserved.
; Invoke action queues body records for next. The caller must drain them before
; another line; expanded records go directly to normal scope/prepare handling.
line	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	movea.l a2, a4
	move.l d0, d7
	moveq #ACTION_REGULAR, d1
	moveq #0, d6
	move.b (a5), d6
	addq.w #1, d6
	cmpi.w #4, d6
	blo.w bad
	lea 0(a5, d6.w), a3
	lea 4(a5), a2
	clr.w State.CallLabelPresent(a6)
	; A name-first header has a four-byte name followed by .segment.
	cmpi.w #17, d6
	blo.w directive
	cmpi.b #1, (a2)
	bhi.w directive
	cmpi.b #7, 4(a2)
	bne.w directive
	cmpi.b #1, 5(a2)
	bhi.w directive
	tst.b 8(a2)
	bne.w directive
	moveq #0, d0
	move.w 6(a2), d0
	movea.l a4, a0
	jsr scopes.classifyDirective
	cmpi.l #scopes.KEY_SEGMENT, d0
	beq.w segmentHeader
	cmpi.l #scopes.KEY_MACRO, d0
	beq.w macroHeader
directive
	; A call may carry one numeric entry-label token, with an optional colon.
	cmpi.w #13, d6
	blo.w directiveName
	cmpi.b #1, (a2)
	bhi.w directiveName
	movea.l a2, a1
	addq.l #4, a1
	cmpi.b #5, (a1)
	bne.w labelDirective
	addq.l #1, a1
labelDirective
	cmpi.b #7, (a1)
	bne.w directiveName
	move.l (a2), State.CallLabel(a6)
	move.w #1, State.CallLabelPresent(a6)
	movea.l a1, a2
directiveName
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #5, d0
	blo.w ordinary
	cmpi.b #7, (a2)
	bne.w ordinary
	cmpi.b #1, 1(a2)
	bhi.w ordinary
	tst.b 4(a2)
	bne.w ordinary
	moveq #0, d0
	move.w 2(a2), d0
	movea.l a4, a0
	jsr scopes.classifyDirective
	cmpi.l #scopes.KEY_ENDSEGMENT, d0
	beq.w closeSegment
	cmpi.l #scopes.KEY_ENDMACRO, d0
	beq.w closeMacro
	cmpi.l #scopes.KEY_SEGMENT, d0
	beq.w directiveHeader
	cmpi.l #scopes.KEY_MACRO, d0
	beq.w bad  ; only the name-first macro header is supported
	tst.w State.Open(a6)
	bne.w capture
	tst.w State.Skipping(a6)
	bne.w consumed
	tst.l d7
	beq.w ordinary
	move.w 2(a2), d5
	moveq #0, d4
findCall
	cmp.w State.Count(a6), d4
	bhs.w ordinary
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a6), a0
	adda.w d0, a0
	cmp.w Def.Name(a0), d5
	beq.w call
	addq.w #1, d4
	bra.w findCall
directiveHeader
	moveq #KIND_SEGMENT, d2
	; .segment NAME(parameter) has one exact numeric-token shape.
	cmpi.w #19, d6
	bne.w bad
	cmpi.b #1, 5(a2)
	bhi.w bad
	tst.b 8(a2)
	bne.w bad
	cmpi.b #TOKEN_OPEN_PAREN, 9(a2)
	bne.w bad
	cmpi.b #1, 10(a2)
	bhi.w bad
	tst.b 13(a2)
	bne.w bad
	cmpi.b #TOKEN_CLOSE_PAREN, 14(a2)
	bne.w bad
	move.w 6(a2), d5
	move.w 11(a2), d3
	bra.w checkedHeader
segmentHeader
	moveq #KIND_SEGMENT, d2
	bra.w header
macroHeader
	moveq #KIND_MACRO, d2
header
	cmpi.b #1, 9(a2)
	bhi.w bad
	cmpi.w #17, d6
	bne.w bad  ; one bare parameter, no trailing tokens
	tst.b 12(a2)
	bne.w bad
	move.w 1(a2), d5
	move.w 10(a2), d3
checkedHeader
	tst.w State.Open(a6)
	bne.w bad
	tst.w State.Skipping(a6)
	bne.w bad
	tst.l d7
	beq.w skipDefinition
	cmpi.w #LIMIT, State.Count(a6)
	bhs.w bad
	moveq #0, d4
duplicate
	cmp.w State.Count(a6), d4
	bhs.w newDefinition
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a6), a0
	adda.w d0, a0
	cmp.w Def.Name(a0), d5
	beq.w bad
	addq.w #1, d4
	bra.w duplicate
newDefinition
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a6), a0
	adda.w d0, a0
	move.w d5, Def.Name(a0)
	move.w d3, Def.Parameter(a0)
	move.w State.Used(a6), Def.First(a0)
	move.w State.Used(a6), Def.Last(a0)
	move.w d2, Def.Kind(a0)
	addq.w #1, State.Count(a6)
	addq.w #1, d4
	move.w d4, State.Open(a6)
	bra.w consumed
skipDefinition
	move.w #1, State.Skipping(a6)
	bra.w consumed
closeSegment
	moveq #KIND_SEGMENT, d2
	bra.w close
closeMacro
	moveq #KIND_MACRO, d2
close
	cmpi.w #9, d6
	bne.w bad
	tst.w State.Skipping(a6)
	beq.w closeOpen
	clr.w State.Skipping(a6)
	bra.w consumed
closeOpen
	move.w State.Open(a6), d4
	beq.w bad
	subq.w #1, d4
	mulu.w #DEF_BYTES, d4
	lea DEFS(a6), a0
	adda.w d4, a0
	cmp.w Def.Kind(a0), d2
	bne.w bad
	move.w State.Used(a6), d0
	cmp.w Def.First(a0), d0
	beq.w bad  ; an invocation must yield at least one record
	move.w State.Used(a6), Def.Last(a0)
	clr.w State.Open(a6)
	bra.w consumed
capture
	; Store the complete raw record; offsets in Def survive relocation.
	tst.l d7
	beq.w consumed
	moveq #0, d0
	move.w State.Used(a6), d0
	add.l d6, d0
	cmpi.l #BODY_BYTES, d0
	bhi.w bad
	lea BODY(a6), a0
	moveq #0, d4
	move.w State.Used(a6), d4
	adda.l d4, a0
	movea.l a5, a1
	move.w d6, d4
copyBody
	move.b (a1)+, (a0)+
	subq.w #1, d4
	bne.w copyBody
	move.w d0, State.Used(a6)
	bra.w consumed
call
	tst.w State.Call(a6)
	bne.w bad
	tst.w State.CallLabelPresent(a6)
	beq.w callSyntax
	btst #0, 1(a5)
	bne.w bad
callSyntax
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #5, d0
	bls.w bad
	lea 5(a2), a1
	moveq #0, d3
	cmpi.b #TOKEN_OPEN_PAREN, (a1)
	bne.w bareArgument
	cmpi.b #TOKEN_CLOSE_PAREN, -1(a3)
	bne.w bad
	addq.l #1, a1
	subq.l #1, a3
	cmpa.l a3, a1
	bhs.w bad
	moveq #1, d3
bareArgument
	move.l a3, d0
	sub.l a1, d0
	cmpi.l #ARG_LIMIT, d0
	bhi.w bad
	move.w d0, State.ArgBytes(a6)
	; A single nonempty expression: comma starts a second argument.
	movea.l a1, a0
checkArgument
	cmpa.l a3, a0
	beq.w argumentReady
	moveq #0, d0
	move.b (a0), d0
	cmpi.b #4, d0
	beq.w bad
	tst.w d3
	beq.w argumentToken
	cmpi.b #TOKEN_OPEN_PAREN, d0
	beq.w bad
	cmpi.b #TOKEN_CLOSE_PAREN, d0
	beq.w bad
argumentToken
	moveq #1, d2
	cmpi.b #1, d0
	bls.w argName
	cmpi.b #2, d0
	beq.w argNumber
	cmpi.b #7, d0
	beq.w argDot
	cmpi.b #39, d0
	bhi.w bad
	bra.w argAdvance
argDot
	moveq #5, d2
	bra.w argAdvance
argName
	moveq #4, d2
	bra.w argAdvance
argNumber
	moveq #5, d2
argAdvance
	adda.w d2, a0
	cmpa.l a3, a0
	bhi.w bad
	bra.w checkArgument
argumentReady
	lea ARGUMENT(a6), a0
	move.w State.ArgBytes(a6), d2
copyArgument
	move.b (a1)+, (a0)+
	subq.w #1, d2
	bne.w copyArgument
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a6), a0
	adda.w d0, a0
	move.w Def.First(a0), State.Cursor(a6)
	clr.w State.CallPhase(a6)
	tst.w Def.Kind(a0)
	beq.w callQueued
	move.w #1, State.CallPhase(a6)
callQueued
	addq.w #1, d4
	move.w d4, State.Call(a6)
	move.w 2(a5), State.CallLine(a6)
	moveq #ACTION_INVOKE, d1
	bra.w ok
ordinary
	tst.w State.Open(a6)
	bne.w capture
	tst.w State.Skipping(a6)
	bne.w consumed
	moveq #ACTION_REGULAR, d1
	bra.w ok
consumed
	moveq #ACTION_CONSUMED, d1
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
	moveq #ACTION_REGULAR, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line

; A0=segment state,A1=distinct 256-byte output,A2=scope state.
; D0/CCR=status;
; D1=expanded raw record length, zero when the queued call is exhausted.
; The source line is the invocation line. Other registers preserved.
next	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	movea.l a2, a0
	move.l a0, -(sp)
	moveq #0, d1
	move.w State.Call(a6), d0
	beq.w exhausted
	subq.w #1, d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a6), a4
	adda.w d0, a4
	tst.w Def.Kind(a4)
	beq.w bodyRecord
	cmpi.w #1, State.CallPhase(a6)
	beq.w macroOpen
	cmpi.w #3, State.CallPhase(a6)
	beq.w macroClose
	cmpi.w #4, State.CallPhase(a6)
	beq.w exhausted
bodyRecord
	moveq #0, d0
	move.w State.Cursor(a6), d0
	cmp.w Def.Last(a4), d0
	blo.w bodyAvailable
	tst.w Def.Kind(a4)
	beq.w exhausted
	move.w #3, State.CallPhase(a6)
	bra.w macroClose
bodyAvailable
	lea BODY(a6), a3
	adda.l d0, a3
	moveq #0, d5
	move.b (a3), d5
	addq.w #1, d5
	move.l d0, d2
	add.w d5, d2
	cmp.w Def.Last(a4), d2
	bhi.w bad
	move.w d2, State.Cursor(a6)
	lea 0(a3, d5.w), a2
	lea 256(a5), a1
	move.b (a3)+, (a5)+
	move.b (a3)+, (a5)+
	move.w State.CallLine(a6), (a5)+
	addq.l #2, a3  ; source line is replaced by the invocation line
	cmp.w Def.First(a4), d0
	bne.w tokens
	tst.w Def.Kind(a4)
	bne.w tokens
	tst.w State.CallLabelPresent(a6)
	beq.w tokens
	; The first body record cannot already declare an entry label.
	cmpa.l a2, a3
	beq.w bad
	cmpi.b #1, (a3)
	bhi.w attachLabel
	lea 4(a3), a0
	cmpa.l a2, a0
	bhs.w bodyEntryCheck
	cmpi.b #5, (a0)
	beq.w bad
bodyEntryCheck
	btst #0, -3(a3)
	bne.w attachLabel
	cmpa.l a2, a0
	bhs.w bad
	cmpi.b #34, (a0)
	bne.w bad
attachLabel
	movea.l a5, a0
	addq.l #5, a0
	cmpa.l a1, a0
	bhi.w bad
	; The generated entry label starts in column one even when the body is indented.
	andi.b #$fe, -3(a5)
	move.l State.CallLabel(a6), (a5)+
	move.b #5, (a5)+
tokens
	cmpa.l a2, a3
	beq.w complete
	moveq #0, d0
	move.b (a3), d0
	moveq #1, d4
	cmpi.b #1, d0
	bls.w name
	cmpi.b #2, d0
	beq.w number
	cmpi.b #7, d0
	bne.w copyToken
	moveq #5, d4
	lea 5(a3), a0
	cmpa.l a2, a0
	bhi.w bad
	cmpi.b #1, 1(a3)
	bhi.w bad
	tst.b 4(a3)
	bne.w copyToken
	move.w 2(a3), d0
	cmp.w Def.Parameter(a4), d0
	bne.w copyToken
	moveq #0, d4
	move.w State.ArgBytes(a6), d4
	movea.l a5, a0
	adda.w d4, a0
	cmpa.l a1, a0
	bhi.w bad
	lea ARGUMENT(a6), a0
substitute
	move.b (a0)+, (a5)+
	subq.w #1, d4
	bne.w substitute
	addq.l #5, a3
	bra.w tokens
name
	moveq #4, d4
	bra.w copyToken
number
	moveq #5, d4
copyToken
	movea.l a3, a0
	adda.w d4, a0
	cmpa.l a2, a0
	bhi.w bad
	movea.l a5, a0
	adda.w d4, a0
	cmpa.l a1, a0
	bhi.w bad
	cmpi.b #1, (a3)
	bls.w rebindToken
	cmpi.b #7, (a3)
	beq.w rebindToken
	bra.w copyBytes
rebindToken
	tst.w Def.Kind(a4)
	beq.w copyBytes
	cmpi.b #7, (a3)
	beq.w rebindDot
	moveq #0, d0
	move.w 1(a3), d0
	moveq #0, d1
	move.b 3(a3), d1
	bra.w bindBodyName
rebindDot
	moveq #0, d0
	move.w 2(a3), d0
	moveq #0, d1
	move.b 4(a3), d1
bindBodyName
	movea.l (sp), a0
	jsr scopes.rebindLocal
	bne.w bad
	move.l a5, d0
	add.l d4, d0
	cmp.l a1, d0
	bhi.w bad
	cmpi.b #7, (a3)
	beq.w writeDot
	move.b (a3)+, (a5)+
	move.w d1, (a5)+
	addq.l #2, a3
	move.b (a3)+, (a5)+
	bra.w tokens
writeDot
	move.b (a3)+, (a5)+
	move.b (a3)+, (a5)+
	move.w d1, (a5)+
	addq.l #2, a3
	move.b (a3)+, (a5)+
	bra.w tokens
copyBytes
	move.b (a3)+, (a5)+
	subq.w #1, d4
	bne.w copyBytes
	bra.w tokens
macroOpen
	tst.w State.CallLabelPresent(a6)
	beq.w syntheticLabel
	move.l State.CallLabel(a6), d5
	bra.w openerDirective
syntheticLabel
	addq.w #1, State.Serial(a6)
	beq.w bad
	move.w State.Serial(a6), d0
	lea 240(a5), a0
	move.b #1, (a0)+
	moveq #3, d2
syntheticNibble
	move.w d0, d3
	lsr.w #8, d3
	lsr.w #4, d3
	andi.w #15, d3
	addi.b #16, d3
	move.b d3, (a0)+
	lsl.w #4, d0
	dbra d2, syntheticNibble
	lea 240(a5), a0
	moveq #5, d0
	movea.l (sp), a1
	jsr scopes.bind
	bne.w bad
	moveq #0, d5
	move.w d1, d5
	lsl.l #8, d5
openerDirective
	lea BlockWord, a0
	moveq #5, d0
	movea.l (sp), a1
	jsr scopes.bind
	bne.w bad
	move.b #13, (a5)
	clr.b 1(a5)
	move.w State.CallLine(a6), 2(a5)
	move.l d5, 4(a5)
	move.b #5, 8(a5)
	move.b #7, 9(a5)
	clr.b 10(a5)
	move.w d1, 11(a5)
	clr.b 13(a5)
	move.w #2, State.CallPhase(a6)
	moveq #14, d1
	moveq #0, d0
	bra.w done
macroClose
	lea EndblockWord, a0
	moveq #8, d0
	movea.l (sp), a1
	jsr scopes.bind
	bne.w bad
	move.b #8, (a5)
	clr.b 1(a5)
	move.w State.CallLine(a6), 2(a5)
	move.b #7, 4(a5)
	clr.b 5(a5)
	move.w d1, 6(a5)
	clr.b 8(a5)
	move.w #4, State.CallPhase(a6)
	moveq #9, d1
	moveq #0, d0
	bra.w done
complete
	movea.l a1, a0
	suba.w #256, a0
	move.l a5, d1
	sub.l a0, d1
	move.l d1, d0
	subq.w #1, d0
	move.b d0, (a0)
	moveq #0, d0
	bra.w done
exhausted
	clr.w State.Call(a6)
	clr.w State.CallPhase(a6)
	moveq #0, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
	clr.w State.Call(a6)
	moveq #0, d1
done
	addq.l #4, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; next
BlockWord
	.byte "block"
EndblockWord
	.byte "endblock"
	.align 2  ; the next module shares this instruction section
	.endsection
	.endmodule
