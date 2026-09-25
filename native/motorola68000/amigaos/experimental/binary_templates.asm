; Bounded preparation-only macro and segment templates over numeric writer records.
; Definition storage contains record bytes and numeric identifiers, never text.
	.module experimental.amigaos.binary_templates
	.cpu 68020
	.use experimental.amigaos.binary_scopes as scopes
	.pub
LIMIT = 8
ARG_LIMIT = 192
PARAM_LIMIT = 9
DEFAULT_LIMIT = 512
BODY_BYTES = 4096
TOKEN_COMMA = 4
TOKEN_EQ = 34
TOKEN_OPEN_BRACKET = 10
TOKEN_CLOSE_BRACKET = 11
TOKEN_OPEN_BRACE = 12
TOKEN_CLOSE_BRACE = 13
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
ArgEnd0	.word ?
ArgEnd1	.word ?
ArgEnd2	.word ?
ArgEnd3	.word ?
ArgEnd4	.word ?
ArgEnd5	.word ?
ArgEnd6	.word ?
ArgEnd7	.word ?
ArgEnd8	.word ?
DefaultMask	.word ?
CallLine	.word ?
CallLabel	.long ?
CallLabelPresent	.word ?
CallPhase	.word ?
Serial	.word ?
	.endstruct
Def	.struct
Name	.word ?
Parameter	.word ?
Parameter1	.word ?
Parameter2	.word ?
Parameter3	.word ?
Parameter4	.word ?
Parameter5	.word ?
Parameter6	.word ?
Parameter7	.word ?
Parameter8	.word ?
DefaultStart0	.word ?
DefaultStart1	.word ?
DefaultStart2	.word ?
DefaultStart3	.word ?
DefaultStart4	.word ?
DefaultStart5	.word ?
DefaultStart6	.word ?
DefaultStart7	.word ?
DefaultStart8	.word ?
DefaultEnd0	.word ?
DefaultEnd1	.word ?
DefaultEnd2	.word ?
DefaultEnd3	.word ?
DefaultEnd4	.word ?
DefaultEnd5	.word ?
DefaultEnd6	.word ?
DefaultEnd7	.word ?
DefaultEnd8	.word ?
First	.word ?
Last	.word ?
Kind	.word ?
ParamCount	.word ?
	.endstruct
DEF_BYTES = Def.ParamCount+2
KIND_SEGMENT = 0
KIND_MACRO = 1
DEFAULT_USED = State.Serial+2
DEFS = DEFAULT_USED+2
ARGUMENT = DEFS+LIMIT*DEF_BYTES
DEFAULTS = ARGUMENT+ARG_LIMIT
BODY = DEFAULTS+DEFAULT_LIMIT
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
	clr.w State.DefaultMask(a0)
	clr.w DEFAULT_USED(a0)
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
	; A name-first header has a four-byte name followed by .segment/.macro.
	cmpi.w #13, d6
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
	beq.w directiveMacroHeader
	tst.w State.Open(a6)
	bne.w capture
	tst.w State.Skipping(a6)
	bne.w consumed
	tst.l d7
	beq.w ordinary
	move.w 2(a2), d5
	moveq #0, d4
	moveq #-1, d3
	move.w #$ffff, d6
findCall
	cmp.w State.Count(a6), d4
	bhs.w selectedCall
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a6), a1
	adda.w d0, a1
	moveq #0, d0
	move.w d5, d0
	moveq #0, d1
	move.w Def.Name(a1), d1
	movea.l a4, a0
	jsr scopes.templateDistance
	bne.w nextCall
	cmp.w d6, d1
	bhs.w nextCall
	move.w d1, d6
	move.w d4, d3
	tst.w d6
	beq.w selectedCall
nextCall
	addq.w #1, d4
	bra.w findCall
selectedCall
	tst.w d3
	bmi.w ordinary
	move.w d3, d4
	bra.w call
directiveHeader
	moveq #KIND_SEGMENT, d2
	bra.w directiveParameters
directiveMacroHeader
	moveq #KIND_MACRO, d2
directiveParameters
	; The parenthesized header contains only bare parameter names and commas.
	cmpi.w #15, d6
	blo.w bad
	cmpi.b #1, 5(a2)
	bhi.w bad
	tst.b 8(a2)
	bne.w bad
	cmpi.b #TOKEN_OPEN_PAREN, 9(a2)
	bne.w bad
	cmpi.b #TOKEN_CLOSE_PAREN, -1(a3)
	bne.w bad
	move.w 6(a2), d5
	lea 10(a2), a1
	lea -1(a3), a2
	bra.w checkedHeader
segmentHeader
	moveq #KIND_SEGMENT, d2
	bra.w header
macroHeader
	moveq #KIND_MACRO, d2
header
	move.w 1(a2), d5
	lea 9(a2), a1
	movea.l a3, a2
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
	clr.w Def.ParamCount(a0)
	lea Def.DefaultStart0(a0), a4
	moveq #18-1, d0
clearDefaults
	clr.w (a4)+
	dbra d0, clearDefaults
	cmpa.l a2, a1
	beq.w parametersDone
parameters
	move.l a2, d0
	sub.l a1, d0
	cmpi.l #4, d0
	blo.w bad
	cmpi.b #1, (a1)
	bhi.w bad
	tst.b 3(a1)
	bne.w bad
	move.w Def.ParamCount(a0), d0
	cmpi.w #PARAM_LIMIT, d0
	bhs.w bad
	add.w d0, d0
	move.w 1(a1), Def.Parameter(a0, d0.w)
	addq.w #1, Def.ParamCount(a0)
	adda.w #4, a1
	cmpa.l a2, a1
	beq.w parametersDone
	cmpi.b #TOKEN_EQ, (a1)
	bne.w nextParameter
	addq.l #1, a1
	cmpa.l a2, a1
	bhs.w bad
	movea.l a1, a3
	movea.l a1, a4
	moveq #0, d6  ; two-bit delimiter stack
	moveq #0, d7  ; delimiter depth
scanDefault
	cmpa.l a2, a4
	beq.w defaultEnd
	moveq #0, d0
	move.b (a4), d0
	cmpi.b #TOKEN_COMMA, d0
	bne.w defaultOpenParen
	tst.w d7
	beq.w defaultEnd
	bra.w defaultToken
defaultOpenParen
	cmpi.b #TOKEN_OPEN_PAREN, d0
	bne.w defaultOpenBracket
	cmpi.w #16, d7
	bhs.w bad
	lsl.l #2, d6
	ori.b #1, d6
	addq.w #1, d7
	bra.w defaultToken
defaultOpenBracket
	cmpi.b #TOKEN_OPEN_BRACKET, d0
	bne.w defaultOpenBrace
	cmpi.w #16, d7
	bhs.w bad
	lsl.l #2, d6
	ori.b #2, d6
	addq.w #1, d7
	bra.w defaultToken
defaultOpenBrace
	cmpi.b #TOKEN_OPEN_BRACE, d0
	bne.w defaultCloseParen
	cmpi.w #16, d7
	bhs.w bad
	lsl.l #2, d6
	ori.b #3, d6
	addq.w #1, d7
	bra.w defaultToken
defaultCloseParen
	cmpi.b #TOKEN_CLOSE_PAREN, d0
	bne.w defaultCloseBracket
	moveq #1, d1
	bra.w defaultClose
defaultCloseBracket
	cmpi.b #TOKEN_CLOSE_BRACKET, d0
	bne.w defaultCloseBrace
	moveq #2, d1
	bra.w defaultClose
defaultCloseBrace
	cmpi.b #TOKEN_CLOSE_BRACE, d0
	bne.w defaultToken
	moveq #3, d1
defaultClose
	tst.w d7
	beq.w bad
	move.l d6, d0
	andi.l #3, d0
	cmp.l d1, d0
	bne.w bad
	lsr.l #2, d6
	subq.w #1, d7
defaultToken
	moveq #0, d0
	move.b (a4), d0
	moveq #1, d1
	cmpi.b #1, d0
	bls.w defaultName
	cmpi.b #2, d0
	beq.w defaultNumber
	cmpi.b #39, d0
	bhi.w bad
	bra.w defaultAdvance
defaultName
	moveq #4, d1
	bra.w defaultAdvance
defaultNumber
	moveq #5, d1
defaultAdvance
	adda.w d1, a4
	cmpa.l a2, a4
	bhi.w bad
	bra.w scanDefault
defaultEnd
	tst.w d7
	bne.w bad
	move.l a4, d1
	sub.l a3, d1
	beq.w bad
	moveq #0, d0
	move.w DEFAULT_USED(a6), d0
	move.l d0, d6
	add.l d1, d6
	cmpi.l #DEFAULT_LIMIT, d6
	bhi.w bad
	move.w Def.ParamCount(a0), d7
	subq.w #1, d7
	add.w d7, d7
	move.w d0, Def.DefaultStart0(a0, d7.w)
	move.w d6, Def.DefaultEnd0(a0, d7.w)
	lea DEFAULTS(a6), a5
	adda.l d0, a5
copyDefaultDefinition
	move.b (a3)+, (a5)+
	subq.l #1, d1
	bne.w copyDefaultDefinition
	move.w d6, DEFAULT_USED(a6)
	movea.l a4, a1
nextParameter
	cmpa.l a2, a1
	beq.w parametersDone
	cmpi.b #TOKEN_COMMA, (a1)+
	bne.w bad
	cmpa.l a2, a1
	bhs.w bad
	bra.w parameters
parametersDone
	tst.w d2
	bne.w parametersValid
	tst.w Def.ParamCount(a0)
	beq.w bad  ; zero-parameter segments are outside this bounded slice
parametersValid
	move.w d5, Def.Name(a0)
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
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a6), a0
	adda.w d0, a0
	move.w Def.ParamCount(a0), d5
	clr.w State.DefaultMask(a6)
	lea State.ArgEnd0(a6), a0
	moveq #PARAM_LIMIT-1, d0
clearArgumentEnds
	clr.w (a0)+
	dbra d0, clearArgumentEnds
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #5, d0
	blo.w bad
	lea 5(a2), a1
	cmpa.l a3, a1
	beq.w emptyArguments
	cmpi.b #TOKEN_OPEN_PAREN, (a1)
	bne.w arguments
	cmpi.b #TOKEN_CLOSE_PAREN, -1(a3)
	bne.w bad
	addq.l #1, a1
	subq.l #1, a3
arguments
	cmpa.l a3, a1
	bhi.w bad
	beq.w emptyArguments
	clr.w State.ArgBytes(a6)
	moveq #0, d1
	moveq #0, d2  ; two-bit delimiter stack
	moveq #0, d3  ; delimiter depth
	movea.l a1, a0
checkArgument
	cmpa.l a3, a0
	beq.w lastArgument
	moveq #0, d0
	move.b (a0), d0
	cmpi.b #TOKEN_COMMA, d0
	bne.w openParen
	tst.w d3
	bne.w argumentToken
	bsr.w appendArgument
	bne.w bad
	addq.l #1, a0
	movea.l a0, a1
	bra.w checkArgument
openParen
	cmpi.b #TOKEN_OPEN_PAREN, d0
	bne.w openBracket
	cmpi.w #16, d3
	bhs.w bad
	lsl.l #2, d2
	ori.b #1, d2
	addq.w #1, d3
	bra.w argumentToken
openBracket
	cmpi.b #TOKEN_OPEN_BRACKET, d0
	bne.w openBrace
	cmpi.w #16, d3
	bhs.w bad
	lsl.l #2, d2
	ori.b #2, d2
	addq.w #1, d3
	bra.w argumentToken
openBrace
	cmpi.b #TOKEN_OPEN_BRACE, d0
	bne.w closeParen
	cmpi.w #16, d3
	bhs.w bad
	lsl.l #2, d2
	ori.b #3, d2
	addq.w #1, d3
	bra.w argumentToken
closeParen
	cmpi.b #TOKEN_CLOSE_PAREN, d0
	bne.w closeBracket
	moveq #1, d6
	bra.w closeDelimiter
closeBracket
	cmpi.b #TOKEN_CLOSE_BRACKET, d0
	bne.w closeBrace
	moveq #2, d6
	bra.w closeDelimiter
closeBrace
	cmpi.b #TOKEN_CLOSE_BRACE, d0
	bne.w argumentToken
	moveq #3, d6
closeDelimiter
	tst.w d3
	beq.w bad
	move.l d2, d0
	andi.l #3, d0
	cmp.l d6, d0
	bne.w bad
	lsr.l #2, d2
	subq.w #1, d3
argumentToken
	moveq #0, d0
	move.b (a0), d0
	moveq #1, d6
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
	moveq #1, d6
	bra.w argAdvance
argName
	moveq #4, d6
	bra.w argAdvance
argNumber
	moveq #5, d6
argAdvance
	adda.w d6, a0
	cmpa.l a3, a0
	bhi.w bad
	bra.w checkArgument
lastArgument
	tst.w d3
	bne.w bad
	bsr.w appendArgument
	bne.w bad
	bra.w argumentsReady
emptyArguments
	clr.w State.ArgBytes(a6)
	moveq #0, d1
argumentsReady
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a6), a0
	adda.w d0, a0
	move.w d1, d7
fillOmitted
	cmpi.w #PARAM_LIMIT, d7
	bhs.w argumentsBound
	cmp.w Def.ParamCount(a0), d7
	bhs.w omittedEnd
	move.w d7, d0
	add.w d0, d0
	moveq #0, d3
	move.w Def.DefaultStart0(a0, d0.w), d3
	moveq #0, d6
	move.w Def.DefaultEnd0(a0, d0.w), d6
	sub.l d3, d6
	beq.w omittedEnd
	moveq #0, d2
	move.w State.ArgBytes(a6), d2
	add.l d6, d2
	cmpi.l #ARG_LIMIT, d2
	bhi.w bad
	lea DEFAULTS(a6), a1
	adda.l d3, a1
	lea ARGUMENT(a6), a2
	adda.w State.ArgBytes(a6), a2
copyDefault
	move.b (a1)+, (a2)+
	subq.l #1, d6
	bne.w copyDefault
	move.w d2, State.ArgBytes(a6)
	moveq #1, d0
	lsl.w d7, d0
	or.w d0, State.DefaultMask(a6)
omittedEnd
	move.w d7, d0
	add.w d0, d0
	move.w State.ArgBytes(a6), State.ArgEnd0(a6, d0.w)
	addq.w #1, d7
	bra.w fillOmitted
argumentsBound
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
; A1..A0 is one nonempty argument in the source record. Append its packed
; tokens and record its cumulative end offset. D1 is the current argument count.
appendArgument
	move.l a0, d0
	sub.l a1, d0
	beq.w argumentBad
	cmpi.w #PARAM_LIMIT, d1
	bhs.w argumentBad
	moveq #0, d6
	move.w State.ArgBytes(a6), d6
	add.l d0, d6
	cmpi.l #ARG_LIMIT, d6
	bhi.w argumentBad
	lea ARGUMENT(a6), a2
	adda.w State.ArgBytes(a6), a2
argumentCopy
	move.b (a1)+, (a2)+
	subq.l #1, d0
	bne.w argumentCopy
	move.w d6, State.ArgBytes(a6)
	move.w d1, d0
	add.w d0, d0
	move.w d6, State.ArgEnd0(a6, d0.w)
	addq.w #1, d1
	moveq #0, d0
	rts
argumentBad
	moveq #1, d0
	rts
	.bend  ; line

; A0=template state,A1=distinct 256-byte output,A2=scope state.
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
	bhi.w positionalParameter
	tst.b 4(a3)
	bne.w copyToken
	move.w 2(a3), d0
	moveq #0, d7
findParameter
	cmp.w Def.ParamCount(a4), d7
	bhs.w copyToken
	move.w d7, d6
	add.w d6, d6
	cmp.w Def.Parameter(a4, d6.w), d0
	beq.w substituteParameter
	addq.w #1, d7
	bra.w findParameter
positionalParameter
	cmpi.b #2, 1(a3)
	bne.w bad
	lea 6(a3), a0
	cmpa.l a2, a0
	bhi.w bad
	moveq #6, d4
	move.l 2(a3), d7
	subq.l #1, d7
	cmpi.l #PARAM_LIMIT, d7
	bhs.w copyToken
substituteParameter
	move.w d7, d0
	add.w d0, d0
	moveq #0, d6
	tst.w d0
	beq.w argumentStart
	subq.w #2, d0
	move.w State.ArgEnd0(a6, d0.w), d6
	addq.w #2, d0
argumentStart
	moveq #0, d4
	move.w State.ArgEnd0(a6, d0.w), d4
	sub.w d6, d4
	beq.w advanceArgument
	movea.l a5, a0
	adda.w d4, a0
	cmpa.l a1, a0
	bhi.w bad
	lea ARGUMENT(a6), a0
	adda.w d6, a0
	moveq #0, d0
	move.w State.DefaultMask(a6), d0
	btst d7, d0
	bne.w substituteDefault
substitute
	move.b (a0)+, (a5)+
	subq.w #1, d4
	bne.w substitute
	bra.w advanceArgument
substituteDefault
	; Defaults were captured in the definition scope. Rebind their source IDs
	; as the expanded line is emitted in the invocation scope.
	tst.w d4
	beq.w advanceArgument
	moveq #0, d0
	move.b (a0), d0
	cmpi.b #1, d0
	bhi.w defaultValue
	cmpi.w #4, d4
	blo.w bad
	moveq #0, d0
	move.w 1(a0), d0
	moveq #0, d1
	move.b 3(a0), d1
	move.l a0, -(sp)
	movea.l 4(sp), a0
	jsr scopes.rebindLocal
	movea.l (sp)+, a0
	bne.w bad
	move.b (a0)+, (a5)+
	move.w d1, (a5)+
	addq.l #2, a0
	move.b (a0)+, (a5)+
	subq.w #4, d4
	bra.w substituteDefault
defaultValue
	moveq #1, d6
	cmpi.b #2, d0
	bne.w defaultTokenReady
	moveq #5, d6
defaultTokenReady
	cmp.w d6, d4
	blo.w bad
	sub.w d6, d4
defaultCopy
	move.b (a0)+, (a5)+
	subq.w #1, d6
	bne.w defaultCopy
	bra.w substituteDefault
advanceArgument
	cmpi.b #2, 1(a3)
	beq.w advancePositional
	addq.l #5, a3
	bra.w tokens
advancePositional
	addq.l #6, a3
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
	bne.w rebindName
	cmpi.b #2, 1(a3)
	beq.w copyBytes
	bra.w rebindDot
rebindName
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
