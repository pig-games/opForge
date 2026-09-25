; Bounded preparation-only macro and segment templates over numeric writer records.
; Definition storage contains record bytes and numeric identifiers, never text.
	.module experimental.amigaos.binary_templates
	.cpu 68020
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as records
	.pub
LIMIT = 8
ARG_LIMIT = 192
TEXT_LIMIT = 192
PARAM_LIMIT = 9
DEPTH_LIMIT = 64
DEFAULT_LIMIT = 512
DEFAULT_TEXT_LIMIT = 512
BODY_BYTES = 4096
TOKEN_COMMA = 4
TOKEN_EQ = 34
TOKEN_OPEN_BRACKET = 10
TOKEN_CLOSE_BRACKET = 11
TOKEN_OPEN_BRACE = 12
TOKEN_CLOSE_BRACE = 13
TOKEN_OPEN_PAREN = 14
TOKEN_CLOSE_PAREN = 15
TOKEN_AT = 40
TOKEN_COMPOSITE = 41
TOKEN_CALL_TEXT = 42
ACTION_REGULAR = 0
ACTION_CONSUMED = 1
ACTION_INVOKE = 2
State	.struct
Count	.word ?
Open	.word ?
Skipping	.word ?
Used	.word ?
Depth	.word ?
CallLabel	.long ?
CallLabelPresent	.word ?
Serial	.word ?
RawBytes	.word ?
TextOffset	.word ?
HeaderParen	.word ?
	.endstruct
CallFrame	.struct
Definition	.word ?
Cursor	.word ?
ArgBytes	.word ?
ArgCount	.word ?  ; supplied arguments, excluding omitted defaults
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
CallName	.word ?  ; source ID records the invocation scope origin
Argument	.byte ?
	.endstruct
TEXT_PAREN = CallFrame.Argument+ARG_LIMIT
TEXT_BYTES = TEXT_PAREN+2
TEXT_END0 = TEXT_BYTES+2
TEXT = TEXT_END0+PARAM_LIMIT*2
SIDE_BYTES = TEXT+TEXT_LIMIT
FULL_BYTES = SIDE_BYTES+2
FULL_TEXT = FULL_BYTES+2
FRAME_BYTES = FULL_TEXT+252
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
TextStart0	.word ?
TextStart1	.word ?
TextStart2	.word ?
TextStart3	.word ?
TextStart4	.word ?
TextStart5	.word ?
TextStart6	.word ?
TextStart7	.word ?
TextStart8	.word ?
TextEnd0	.word ?
TextEnd1	.word ?
TextEnd2	.word ?
TextEnd3	.word ?
TextEnd4	.word ?
TextEnd5	.word ?
TextEnd6	.word ?
TextEnd7	.word ?
TextEnd8	.word ?
First	.word ?
Last	.word ?
Kind	.word ?
ParamCount	.word ?
	.endstruct
DEF_BYTES = Def.ParamCount+2
KIND_SEGMENT = 0
KIND_MACRO = 1
DEFAULT_USED = State.HeaderParen+2
DEFAULT_TEXT_USED = DEFAULT_USED+2
DEFS = DEFAULT_TEXT_USED+2
DEFAULTS = DEFS+LIMIT*DEF_BYTES
DEFAULT_TEXT = DEFAULTS+DEFAULT_LIMIT
BODY = DEFAULT_TEXT+DEFAULT_TEXT_LIMIT
FRAMES = BODY+BODY_BYTES
COMPOSITE_TEXT = FRAMES+DEPTH_LIMIT*FRAME_BYTES
HEADER_FRAME = COMPOSITE_TEXT+256
SCRATCH_BYTES = HEADER_FRAME+FRAME_BYTES
	.section code, kind=code

; A0=caller-owned state. Clears definitions for a new assembly session.
; D0/CCR=zero; other registers preserved.
begin	.block
	clr.w State.Count(a0)
	clr.w State.Open(a0)
	clr.w State.Skipping(a0)
	clr.w State.Used(a0)
	clr.w State.Depth(a0)
	clr.w DEFAULT_USED(a0)
	clr.w DEFAULT_TEXT_USED(a0)
	clr.l State.CallLabel(a0)
	clr.w State.CallLabelPresent(a0)
	clr.w State.Serial(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=state. A definition cannot cross a physical source boundary.
; D0/CCR=status; other registers preserved. Definitions remain reusable.
endFile	.block
	moveq #0, d0
	tst.w State.Depth(a0)
	bne.w bad
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
	move.l a2, -(sp)  ; scope survives parameter/default parsing
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
	move.w d6, State.RawBytes(a6)
	clr.w State.TextOffset(a6)
	clr.w State.HeaderParen(a6)
	btst #5, 1(a5)
	beq.w noCallText
	moveq #0, d0
	move.b -1(a5, d6.w), d0
	cmpi.w #3, d0
	blo.w bad
	move.w d6, d2
	sub.w d0, d2
	cmpi.w #4, d2
	blo.w bad
	lea 0(a5, d2.w), a0
	cmpi.b #TOKEN_CALL_TEXT, (a0)
	bne.w bad
	moveq #0, d1
	move.b 1(a0), d1
	addq.w #3, d1
	cmp.w d0, d1
	bne.w bad
	move.w d2, State.TextOffset(a6)
	move.w d2, d6
noCallText
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
	bne.w qualifiedCall
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
qualifiedCall
	tst.w State.Open(a6)
	bne.w capture
	tst.w State.Skipping(a6)
	bne.w consumed
	tst.l d7
	beq.w ordinary
	move.w 2(a2), d5
	moveq #0, d4
	moveq #-1, d3
	moveq #0, d2  ; read-only candidate before mutable import lookup
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
	jsr scopes.templateCandidate
	bne.w nextCall
	moveq #1, d2
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
	bpl.w chosenCall
	tst.w d2
	beq.w ordinary
	lea 1(a2), a0
	movea.l (sp), a1
	jsr scopes.resolveTemplate
	bne.w ordinary
	move.w d1, d7  ; keep D5 as the caller-scope invocation ID
	moveq #0, d4
findImportedCall
	cmp.w State.Count(a6), d4
	bhs.w ordinary
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a6), a1
	adda.w d0, a1
	cmp.w Def.Name(a1), d7
	beq.w call
	addq.w #1, d4
	bra.w findImportedCall
chosenCall
	move.w d3, d4
	bra.w call
directiveHeader
	moveq #KIND_SEGMENT, d2
	bra.w directiveParameters
directiveMacroHeader
	moveq #KIND_MACRO, d2
directiveParameters
	move.w #2, State.HeaderParen(a6)
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
	clr.w State.HeaderParen(a6)
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
	moveq #36-1, d0
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
	cmpi.b #3, d0
	beq.w defaultString
	cmpi.b #39, d0
	bhi.w bad
	bra.w defaultAdvance
defaultName
	moveq #4, d1
	bra.w defaultAdvance
defaultNumber
	moveq #5, d1
	bra.w defaultAdvance
defaultString
	move.l a2, d0
	sub.l a4, d0
	cmpi.l #2, d0
	blo.w bad
	moveq #0, d1
	move.b 1(a4), d1
	addq.w #2, d1
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
	move.l a5, -(sp)
	lea DEFAULTS(a6), a5
	adda.l d0, a5
copyDefaultDefinition
	move.b (a3)+, (a5)+
	subq.l #1, d1
	bne.w copyDefaultDefinition
	move.w d6, DEFAULT_USED(a6)
	movea.l (sp)+, a5
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
	bsr.w captureHeaderDefaults
	bne.w bad
	tst.w d2
	bne.w parametersValid
	tst.w Def.ParamCount(a0)
	beq.w bad  ; zero-parameter segments are outside this bounded slice
parametersValid
	; Definitions are ordinary numeric declarations for import visibility,
	; but scopes marks them template-only so expressions cannot use their IDs.
	subq.l #4, sp
	clr.b (sp)
	move.w d5, 1(sp)
	clr.b 3(sp)
	movea.l sp, a0
	movea.l 4(sp), a1
	jsr scopes.declareTemplate
	addq.l #4, sp
	bne.w bad
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a6), a0
	adda.w d0, a0
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
	move.w State.RawBytes(a6), d6
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
	cmpi.w #DEPTH_LIMIT, State.Depth(a6)
	bhs.w bad
	tst.w State.CallLabelPresent(a6)
	beq.w callSyntax
	btst #0, 1(a5)
	bne.w bad
callSyntax
	moveq #0, d0
	move.w State.Depth(a6), d0
	mulu.w #FRAME_BYTES, d0
	lea FRAMES(a6), a4
	adda.l d0, a4
	move.w d4, CallFrame.Definition(a4)
	move.w d5, CallFrame.CallName(a4)
	move.l State.CallLabel(a6), CallFrame.CallLabel(a4)
	move.w State.CallLabelPresent(a6), CallFrame.CallLabelPresent(a4)
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a6), a0
	adda.w d0, a0
	move.w Def.ParamCount(a0), d5
	clr.w CallFrame.DefaultMask(a4)
	clr.w TEXT_PAREN(a4)
	clr.w TEXT_BYTES(a4)
	clr.w FULL_BYTES(a4)
	lea CallFrame.ArgEnd0(a4), a0
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
	move.w #1, TEXT_PAREN(a4)
	addq.l #1, a1
	subq.l #1, a3
arguments
	cmpa.l a3, a1
	bhi.w bad
	beq.w emptyArguments
	clr.w CallFrame.ArgBytes(a4)
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
	cmpi.b #3, d0
	beq.w argString
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
	bra.w argAdvance
argString
	move.l a3, d0
	sub.l a0, d0
	cmpi.l #2, d0
	blo.w bad
	moveq #0, d6
	move.b 1(a0), d6
	addq.w #2, d6
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
	clr.w CallFrame.ArgBytes(a4)
	moveq #0, d1
argumentsReady
	move.w d1, CallFrame.ArgCount(a4)
	bsr.w captureCallText
	bne.w bad
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
	move.w CallFrame.ArgBytes(a4), d2
	add.l d6, d2
	cmpi.l #ARG_LIMIT, d2
	bhi.w bad
	lea DEFAULTS(a6), a1
	adda.l d3, a1
	lea CallFrame.Argument(a4), a2
	adda.w CallFrame.ArgBytes(a4), a2
copyDefault
	move.b (a1)+, (a2)+
	subq.l #1, d6
	bne.w copyDefault
	move.w d2, CallFrame.ArgBytes(a4)
	moveq #0, d3
	move.w Def.TextStart0(a0, d0.w), d3
	moveq #0, d6
	move.w Def.TextEnd0(a0, d0.w), d6
	sub.l d3, d6
	beq.w bad
	moveq #0, d2
	move.w TEXT_BYTES(a4), d2
	add.l d6, d2
	cmpi.l #TEXT_LIMIT, d2
	bhi.w bad
	lea DEFAULT_TEXT(a6), a1
	adda.l d3, a1
	lea TEXT(a4), a2
	adda.w TEXT_BYTES(a4), a2
copyDefaultText
	move.b (a1)+, (a2)+
	subq.l #1, d6
	bne.w copyDefaultText
	move.w d2, TEXT_BYTES(a4)
	moveq #1, d0
	lsl.w d7, d0
	or.w d0, CallFrame.DefaultMask(a4)
omittedEnd
	move.w d7, d0
	add.w d0, d0
	move.w CallFrame.ArgBytes(a4), CallFrame.ArgEnd0(a4, d0.w)
	lea TEXT_END0(a4), a1
	move.w TEXT_BYTES(a4), 0(a1, d0.w)
	addq.w #1, d7
	bra.w fillOmitted
argumentsBound
	move.w Def.First(a0), CallFrame.Cursor(a4)
	clr.w CallFrame.CallPhase(a4)
	tst.w Def.Kind(a0)
	beq.w callQueued
	move.w #1, CallFrame.CallPhase(a4)
callQueued
	addq.w #1, State.Depth(a6)
	move.w 2(a5), CallFrame.CallLine(a4)
	moveq #ACTION_INVOKE, d1
	bra.w ok
ordinary
	tst.w State.Open(a6)
	bne.w capture
	tst.w State.Skipping(a6)
	bne.w consumed
	tst.w State.TextOffset(a6)
	beq.w ordinaryReady
	move.w State.TextOffset(a6), d0
	subq.w #1, d0
	move.b d0, (a5)
	andi.b #$df, 1(a5)
ordinaryReady
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
	addq.l #4, sp
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
	move.w CallFrame.ArgBytes(a4), d6
	add.l d0, d6
	cmpi.l #ARG_LIMIT, d6
	bhi.w argumentBad
	lea CallFrame.Argument(a4), a2
	adda.w CallFrame.ArgBytes(a4), a2
argumentCopy
	move.b (a1)+, (a2)+
	subq.l #1, d0
	bne.w argumentCopy
	move.w d6, CallFrame.ArgBytes(a4)
	move.w d1, d0
	add.w d0, d0
	move.w d6, CallFrame.ArgEnd0(a4, d0.w)
	addq.w #1, d1
	moveq #0, d0
	rts
argumentBad
	moveq #1, d0
	rts

; Keep the definition's trimmed default spelling separately from executable
; default tokens. Only offsets enter Def; the source record is not retained.
captureHeaderDefaults
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a0, a3  ; definition
	movea.l a6, a4
	adda.l #HEADER_FRAME, a4
	move.w Def.ParamCount(a3), CallFrame.ArgCount(a4)
	move.w State.HeaderParen(a6), TEXT_PAREN(a4)
	clr.w TEXT_BYTES(a4)
	clr.w FULL_BYTES(a4)
	bsr.w captureCallText
	bne.w headerTextDone
	moveq #0, d7
headerTextNext
	cmp.w Def.ParamCount(a3), d7
	bhs.w headerTextGood
	move.w d7, d0
	add.w d0, d0
	move.w Def.DefaultEnd0(a3, d0.w), d1
	cmp.w Def.DefaultStart0(a3, d0.w), d1
	beq.w headerTextAdvance
	moveq #0, d3
	tst.w d7
	beq.w headerTextStart
	lea TEXT_END0(a4), a1
	move.w -2(a1, d0.w), d3
headerTextStart
	moveq #0, d6
	lea TEXT_END0(a4), a1
	move.w 0(a1, d0.w), d6
	cmp.l d3, d6
	bls.w headerTextBad
	lea TEXT(a4), a1
	adda.l d3, a1
	lea TEXT(a4), a2
	adda.l d6, a2
headerTextEqual
	cmpa.l a2, a1
	bhs.w headerTextBad
	cmpi.b #'=', (a1)+
	bne.w headerTextEqual
headerTextTrimStart
	cmpa.l a2, a1
	bhs.w headerTextBad
	cmpi.b #' ', (a1)
	beq.w headerTextSkipStart
	cmpi.b #9, (a1)
	bne.w headerTextTrimEnd
headerTextSkipStart
	addq.l #1, a1
	bra.w headerTextTrimStart
headerTextTrimEnd
	cmpa.l a1, a2
	bls.w headerTextBad
	cmpi.b #' ', -1(a2)
	beq.w headerTextSkipEnd
	cmpi.b #9, -1(a2)
	bne.w headerTextCopyReady
headerTextSkipEnd
	subq.l #1, a2
	bra.w headerTextTrimEnd
headerTextCopyReady
	move.l a2, d2
	sub.l a1, d2
	moveq #0, d4
	move.w DEFAULT_TEXT_USED(a6), d4
	move.l d4, d5
	add.l d2, d5
	cmpi.l #DEFAULT_TEXT_LIMIT, d5
	bhi.w headerTextBad
	move.w d4, Def.TextStart0(a3, d0.w)
	move.w d5, Def.TextEnd0(a3, d0.w)
	lea DEFAULT_TEXT(a6), a2
	adda.l d4, a2
headerTextCopy
	move.b (a1)+, (a2)+
	subq.l #1, d2
	bne.w headerTextCopy
	move.w d5, DEFAULT_TEXT_USED(a6)
headerTextAdvance
	addq.w #1, d7
	bra.w headerTextNext
headerTextBad
	moveq #1, d0
	bra.w headerTextDone
headerTextGood
	moveq #0, d0
headerTextDone
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts

; Split original argument spelling at top-level commas. Quoted punctuation and
; nested delimiters remain part of one trimmed argument. Numeric tokens remain
; the execution representation; this payload serves embedded placeholders.
captureCallText
	movem.l d1-d7/a0-a3, -(sp)
	moveq #0, d0
	tst.w State.TextOffset(a6)
	beq.w textDone
	moveq #0, d2
	move.w State.TextOffset(a6), d2
	lea 0(a5, d2.w), a0
	cmpi.b #TOKEN_CALL_TEXT, (a0)
	bne.w textBad
	moveq #0, d2
	move.b 1(a0), d2
	lea 2(a0), a0
	lea 0(a0, d2.w), a1
textLeading
	cmpa.l a1, a0
	blo.w textLeadingByte
	tst.w CallFrame.ArgCount(a4)
	bne.w textBad
	clr.w TEXT_BYTES(a4)
	bra.w textDone
textLeadingByte
	cmpi.b #' ', (a0)
	beq.w skipLeading
	cmpi.b #9, (a0)
	bne.w textTrailing
skipLeading
	addq.l #1, a0
	bra.w textLeading
textTrailing
	cmpa.l a0, a1
	bls.w textBad
	cmpi.b #' ', -1(a1)
	beq.w skipTrailing
	cmpi.b #9, -1(a1)
	bne.w textParens
skipTrailing
	subq.l #1, a1
	bra.w textTrailing
textParens
	cmpi.w #2, TEXT_PAREN(a4)
	bne.w textOrdinaryParens
textHeaderName
	cmpa.l a1, a0
	bhs.w textBad
	cmpi.b #'(', (a0)
	beq.w textHeaderParen
	addq.l #1, a0
	bra.w textHeaderName
textHeaderParen
	move.w #1, TEXT_PAREN(a4)
textOrdinaryParens
	cmpi.w #1, TEXT_PAREN(a4)
	bne.w textReady
	cmpi.b #'(', (a0)
	bne.w textBad
	cmpi.b #')', -1(a1)
	bne.w textBad
	addq.l #1, a0
	subq.l #1, a1
	bsr.w captureFullText
	bne.w textBad
	move.w #-1, TEXT_PAREN(a4)
	bra.w textLeading
textReady
	tst.w TEXT_PAREN(a4)
	bmi.w textSplit
	bsr.w captureFullText
	bne.w textBad
textSplit
	clr.w TEXT_BYTES(a4)
	movea.l a0, a2  ; current argument start
	movea.l a0, a3  ; scan cursor
	moveq #0, d3  ; delimiter depth
	moveq #0, d4  ; active quote
	moveq #0, d5  ; escaped quoted byte
	moveq #0, d7  ; argument index
textScan
	cmpa.l a1, a3
	beq.w textLast
	moveq #0, d6
	move.b (a3), d6
	tst.w d4
	beq.w textUnquoted
	tst.w d5
	beq.w textEscapeCheck
	moveq #0, d5
	bra.w textAdvance
textEscapeCheck
	cmpi.b #92, d6
	bne.w textQuoteEnd
	moveq #1, d5
	bra.w textAdvance
textQuoteEnd
	cmp.b d4, d6
	bne.w textAdvance
	moveq #0, d4
	bra.w textAdvance
textUnquoted
	cmpi.b #'"', d6
	beq.w textQuoteOpen
	cmpi.b #39, d6
	bne.w textDelimiter
textQuoteOpen
	move.w d6, d4
	bra.w textAdvance
textDelimiter
	cmpi.b #'(', d6
	beq.w textOpen
	cmpi.b #'[', d6
	beq.w textOpen
	cmpi.b #'{', d6
	beq.w textOpen
	cmpi.b #')', d6
	beq.w textClose
	cmpi.b #']', d6
	beq.w textClose
	cmpi.b #'}', d6
	beq.w textClose
	cmpi.b #',', d6
	bne.w textAdvance
	tst.w d3
	bne.w textAdvance
	bsr.w appendTextArgument
	bne.w textBad
	addq.w #1, d7
	addq.l #1, a3
	movea.l a3, a2
	bra.w textScan
textOpen
	addq.w #1, d3
	cmpi.w #16, d3
	bhi.w textBad
	bra.w textAdvance
textClose
	tst.w d3
	beq.w textBad
	subq.w #1, d3
textAdvance
	addq.l #1, a3
	bra.w textScan
textLast
	tst.w d3
	bne.w textBad
	tst.w d4
	bne.w textBad
	bsr.w appendTextArgument
	bne.w textBad
	addq.w #1, d7
	cmp.w CallFrame.ArgCount(a4), d7
	bne.w textBad
	bra.w textDone
textBad
	moveq #1, d0
textDone
	movem.l (sp)+, d1-d7/a0-a3
	tst.l d0
	rts

; A0..A1 is the complete supplied argument region, preserving spaces around
; commas for nested .@. Both pointers stay within the bounded writer sidecar.
captureFullText
	move.l a1, d2
	sub.l a0, d2
	cmpi.l #251, d2
	bhi.w fullTextBad
	move.w d2, FULL_BYTES(a4)
	beq.w fullTextGood
	movea.l a0, a2
	lea FULL_TEXT(a4), a3
fullTextCopy
	move.b (a2)+, (a3)+
	subq.l #1, d2
	bne.w fullTextCopy
fullTextGood
	moveq #0, d0
	rts
fullTextBad
	moveq #1, d0
	rts

; A2..A3 is an argument, D7 its index. Append trimmed bytes to frame text.
appendTextArgument
	move.l a1, -(sp)
	movea.l a2, a0
	movea.l a3, a1
trimArgumentStart
	cmpa.l a1, a0
	bhs.w textArgumentBad
	cmpi.b #' ', (a0)
	beq.w skipArgumentStart
	cmpi.b #9, (a0)
	bne.w trimArgumentEnd
skipArgumentStart
	addq.l #1, a0
	bra.w trimArgumentStart
trimArgumentEnd
	cmpa.l a0, a1
	bls.w textArgumentBad
	cmpi.b #' ', -1(a1)
	beq.w skipArgumentEnd
	cmpi.b #9, -1(a1)
	bne.w argumentTextReady
skipArgumentEnd
	subq.l #1, a1
	bra.w trimArgumentEnd
argumentTextReady
	cmpi.w #PARAM_LIMIT, d7
	bhs.w textArgumentBad
	move.l a1, d2
	sub.l a0, d2
	moveq #0, d1
	move.w TEXT_BYTES(a4), d1
	add.l d2, d1
	cmpi.l #TEXT_LIMIT, d1
	bhi.w textArgumentBad
	lea TEXT(a4), a1
	adda.w TEXT_BYTES(a4), a1
copyArgumentText
	move.b (a0)+, (a1)+
	subq.l #1, d2
	bne.w copyArgumentText
	move.w d1, TEXT_BYTES(a4)
	move.w d7, d2
	add.w d2, d2
	lea TEXT_END0(a4), a0
	move.w d1, 0(a0, d2.w)
	moveq #0, d0
	movea.l (sp)+, a1
	rts
textArgumentBad
	moveq #1, d0
	movea.l (sp)+, a1
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
	move.l a6, -(sp)  ; session state
	move.l a2, -(sp)  ; scope state
nextFrame
	moveq #0, d1
	movea.l 4(sp), a0
	move.w State.Depth(a0), d0
	beq.w noFrames
	subq.w #1, d0
	mulu.w #FRAME_BYTES, d0
	lea FRAMES(a0), a6
	adda.l d0, a6
	moveq #0, d0
	move.w CallFrame.Definition(a6), d0
	mulu.w #DEF_BYTES, d0
	lea DEFS(a0), a4
	adda.w d0, a4
	tst.w Def.Kind(a4)
	beq.w bodyRecord
	cmpi.w #1, CallFrame.CallPhase(a6)
	beq.w macroOpen
	cmpi.w #3, CallFrame.CallPhase(a6)
	beq.w macroClose
	cmpi.w #4, CallFrame.CallPhase(a6)
	beq.w exhausted
bodyRecord
	moveq #0, d0
	move.w CallFrame.Cursor(a6), d0
	cmp.w Def.Last(a4), d0
	blo.w bodyAvailable
	tst.w Def.Kind(a4)
	beq.w exhausted
	move.w #3, CallFrame.CallPhase(a6)
	bra.w macroClose
bodyAvailable
	movea.l 4(sp), a3
	adda.l #BODY, a3
	adda.l d0, a3
	moveq #0, d5
	move.b (a3), d5
	addq.w #1, d5
	move.l d0, d2
	add.w d5, d2
	cmp.w Def.Last(a4), d2
	bhi.w bad
	move.w d2, CallFrame.Cursor(a6)
	lea 0(a3, d5.w), a2
	clr.w SIDE_BYTES(a6)
	btst #5, 1(a3)
	beq.w bodyTextReady
	moveq #0, d0
	move.b -1(a2), d0
	cmpi.w #3, d0
	blo.w bad
	movea.l a2, a0
	suba.w d0, a0
	cmpa.l a3, a0
	bls.w bad
	cmpi.b #TOKEN_CALL_TEXT, (a0)
	bne.w bad
	moveq #0, d2
	move.b 1(a0), d2
	addq.w #3, d2
	cmp.w d0, d2
	bne.w bad
	move.w d0, SIDE_BYTES(a6)
	movea.l a0, a2
bodyTextReady
	lea 256(a5), a1
	move.b (a3)+, (a5)+
	move.b (a3)+, (a5)+
	move.w CallFrame.CallLine(a6), (a5)+
	addq.l #2, a3  ; source line is replaced by the invocation line
	cmp.w Def.First(a4), d0
	bne.w tokens
	tst.w Def.Kind(a4)
	bne.w tokens
	tst.w CallFrame.CallLabelPresent(a6)
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
	move.l CallFrame.CallLabel(a6), (a5)+
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
	cmpi.b #3, d0
	beq.w stringToken
	cmpi.b #TOKEN_COMPOSITE, d0
	beq.w compositeToken
	cmpi.b #TOKEN_AT, d0
	beq.w atParameter
	cmpi.b #7, d0
	bne.w copyToken
	lea 2(a3), a0
	cmpa.l a2, a0
	bhi.w bad
	cmpi.b #TOKEN_AT, 1(a3)
	beq.w allArguments
	cmpi.b #TOKEN_OPEN_BRACE, 1(a3)
	beq.w bracedParameter
	moveq #5, d4
	moveq #5, d5
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
	moveq #6, d5
	move.l 2(a3), d7
	subq.l #1, d7
	cmpi.l #PARAM_LIMIT, d7
	bhs.w copyToken
	bra.w substituteParameter
bracedParameter
	lea 7(a3), a0
	cmpa.l a2, a0
	bhi.w bad
	cmpi.b #1, 2(a3)
	bhi.w bad
	tst.b 5(a3)
	bne.w copyToken
	cmpi.b #TOKEN_CLOSE_BRACE, 6(a3)
	bne.w bad
	moveq #7, d4
	moveq #7, d5
	move.w 3(a3), d0
	moveq #0, d7
findBracedParameter
	cmp.w Def.ParamCount(a4), d7
	bhs.w copyToken
	move.w d7, d6
	add.w d6, d6
	cmp.w Def.Parameter(a4, d6.w), d0
	beq.w substituteParameter
	addq.w #1, d7
	bra.w findBracedParameter
atParameter
	moveq #6, d4
	moveq #6, d5
	lea 6(a3), a0
	cmpa.l a2, a0
	bhi.w bad
	cmpi.b #2, 1(a3)
	bne.w copyToken
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
	move.w CallFrame.ArgEnd0(a6, d0.w), d6
	addq.w #2, d0
argumentStart
	moveq #0, d4
	move.w CallFrame.ArgEnd0(a6, d0.w), d4
	sub.w d6, d4
	beq.w advanceArgument
	movea.l a5, a0
	adda.w d4, a0
	cmpa.l a1, a0
	bhi.w bad
	lea CallFrame.Argument(a6), a0
	adda.w d6, a0
	moveq #0, d0
	move.w CallFrame.DefaultMask(a6), d0
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
	beq.w defaultNumberToken
	cmpi.b #3, d0
	bne.w defaultTokenReady
	cmpi.w #2, d4
	blo.w bad
	moveq #0, d6
	move.b 1(a0), d6
	addq.w #2, d6
	bra.w defaultTokenReady
defaultNumberToken
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
	adda.w d5, a3
	bra.w tokens
allArguments
	; Recreate only the supplied list. Defaults are absent from Rust's .@.
	moveq #0, d7
	moveq #0, d6
allArgumentNext
	cmp.w CallFrame.ArgCount(a6), d7
	bhs.w allArgumentDone
	tst.w d7
	beq.w allArgumentFirst
	cmpa.l a1, a5
	bhs.w bad
	move.b #TOKEN_COMMA, (a5)+
allArgumentFirst
	move.w d7, d0
	add.w d0, d0
	moveq #0, d4
	move.w CallFrame.ArgEnd0(a6, d0.w), d4
	sub.w d6, d4
	movea.l a5, a0
	adda.w d4, a0
	cmpa.l a1, a0
	bhi.w bad
	lea CallFrame.Argument(a6), a0
	adda.w d6, a0
	tst.w d4
	beq.w allArgumentAdvance
allArgumentCopy
	move.b (a0)+, (a5)+
	subq.w #1, d4
	bne.w allArgumentCopy
allArgumentAdvance
	move.w d7, d0
	add.w d0, d0
	move.w CallFrame.ArgEnd0(a6, d0.w), d6
	addq.w #1, d7
	bra.w allArgumentNext
allArgumentDone
	addq.l #2, a3
	bra.w tokens
name
	moveq #4, d4
	bra.w copyToken
number
	moveq #5, d4
	bra.w copyToken
stringToken
	move.l a1, -(sp)  ; output limit
	move.l a1, d7
	movea.l 8(sp), a0  ; session state
	movea.l 4(sp), a1  ; scope state
	exg a2, a3  ; string token and bounded body-record end
	jsr expandStringToken
	exg a2, a3
	movea.l (sp)+, a1
	tst.l d0
	bne.w bad
	adda.l d1, a3
	bra.w tokens
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
compositeToken
	move.l a1, -(sp)  ; preserve output limit across the helper call
	move.l a4, -(sp)  ; preserve definition cursor
	movea.l 12(sp), a0  ; template session state
	movea.l 8(sp), a1  ; scope state
	exg a2, a3  ; recipe start and bounded body-record end
	movea.l a6, a4  ; invocation frame
	movea.l 4(sp), a6  ; output limit
	jsr expandComposite
	movea.l a4, a6
	exg a2, a3
	movea.l (sp)+, a4
	movea.l (sp)+, a1
	tst.l d0
	bne.w bad
	adda.l d1, a3
	bra.w tokens
macroOpen
	tst.w CallFrame.CallLabelPresent(a6)
	beq.w syntheticLabel
	move.l CallFrame.CallLabel(a6), d5
	bra.w openerDirective
syntheticLabel
	movea.l 4(sp), a0
	addq.w #1, State.Serial(a0)
	beq.w bad
	move.w State.Serial(a0), d0
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
	move.w CallFrame.CallLine(a6), 2(a5)
	move.l d5, 4(a5)
	move.b #5, 8(a5)
	move.b #7, 9(a5)
	clr.b 10(a5)
	move.w d1, 11(a5)
	clr.b 13(a5)
	move.w #2, CallFrame.CallPhase(a6)
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
	move.w CallFrame.CallLine(a6), 2(a5)
	move.b #7, 4(a5)
	clr.b 5(a5)
	move.w d1, 6(a5)
	clr.b 8(a5)
	move.w #4, CallFrame.CallPhase(a6)
	moveq #9, d1
	moveq #0, d0
	bra.w done
complete
	moveq #0, d2
	move.w SIDE_BYTES(a6), d2
	beq.w completeLength
	movea.l (sp), a0
	bsr.w rewriteCallText
	bne.w bad
completeLength
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
	movea.l 4(sp), a0
	subq.w #1, State.Depth(a0)
	bne.w nextFrame
noFrames
	moveq #0, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
	movea.l 4(sp), a0
	clr.w State.Depth(a0)
	moveq #0, d1
done
	addq.l #8, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; next

	.priv
; Rewrite only a captured invocation's exact argument spelling. Positional
; placeholders are replaced from the current frame before nested lookup.
; A0=scope,A1=output end,A2=sidecar,A4=definition,A5=output,A6=frame,
; D2=sidecar bytes. D0/CCR=status,A5 advances; other registers preserved.
rewriteCallText	.block
	movem.l d1-d7/a0-a4/a6, -(sp)
	move.l a4, -(sp)
	move.l a5, -(sp)
	movea.l a2, a3
	adda.w d2, a3
	subq.l #1, a3  ; trailing sidecar size byte
	lea 2(a2), a2
	movea.l a5, a4
	addq.l #2, a4
	cmpa.l a1, a4
	bhi.w rewriteBad
	move.b #TOKEN_CALL_TEXT, (a5)+
	clr.b (a5)+
rewriteByte
	cmpa.l a3, a2
	beq.w rewriteComplete
	bhi.w rewriteBad
	moveq #0, d0
	move.b (a2), d0
	cmpi.b #'@', d0
	beq.w rewritePositional
	cmpi.b #'.', d0
	beq.w rewritePositional
	bra.w rewriteLiteral
rewritePositional
	movea.l a2, a4
	addq.l #1, a4
	cmpa.l a3, a4
	bhs.w rewriteLiteral
	moveq #0, d7
	move.b 1(a2), d7
	cmpi.b #'1', d7
	blo.w rewriteMaybeNamed
	cmpi.b #'9', d7
	bhi.w rewriteMaybeNamed
	subi.w #'1', d7
	add.w d7, d7
	moveq #2, d6
	bra.w rewriteArgument
rewriteMaybeNamed
	cmpi.b #'.', (a2)
	bne.w rewriteLiteral
	cmpi.b #'@', 1(a2)
	beq.w rewriteAllArguments
	movea.l a2, a4
	addq.l #1, a4
	moveq #1, d3  ; identifier byte offset
	cmpi.b #'{', 1(a2)
	bne.w rewriteNameBegin
	addq.l #1, a4
	moveq #2, d3
rewriteNameBegin
	moveq #0, d6
rewriteNameLength
	cmpa.l a3, a4
	bhs.w rewriteNameReady
	moveq #0, d1
	move.b (a4), d1
	cmpi.b #'A', d1
	blo.w rewriteNameLower
	cmpi.b #'Z', d1
	bls.w rewriteNameByte
rewriteNameLower
	cmpi.b #'a', d1
	blo.w rewriteNameDigit
	cmpi.b #'z', d1
	bls.w rewriteNameByte
rewriteNameDigit
	cmpi.b #'0', d1
	blo.w rewriteNameUnderscore
	cmpi.b #'9', d1
	bls.w rewriteNameByte
rewriteNameUnderscore
	cmpi.b #'_', d1
	bne.w rewriteNameReady
rewriteNameByte
	addq.w #1, d6
	addq.l #1, a4
	bra.w rewriteNameLength
rewriteNameReady
	tst.w d6
	beq.w rewriteLiteral
	cmpi.w #2, d3
	bne.w rewriteFormalBegin
	cmpa.l a3, a4
	bhs.w rewriteLiteral
	cmpi.b #'}', (a4)
	bne.w rewriteLiteral
rewriteFormalBegin
	moveq #0, d7
rewriteFormal
	movea.l 4(sp), a4
	cmp.w Def.ParamCount(a4), d7
	bhs.w rewriteLiteral
	move.w d7, d1
	add.w d1, d1
	moveq #0, d4
	move.w Def.Parameter(a4, d1.w), d4
	sub.w layout.State.Base(a0), d4
	bcs.w rewriteBad
	cmp.w layout.State.Count(a0), d4
	bhs.w rewriteBad
	lsl.l #4, d4
	lea layout.ENTRIES(a0), a4
	adda.l d4, a4
	moveq #0, d5
	move.w records.Entry.Length(a4), d5
	sub.w records.Entry.Leaf(a4), d5
	cmp.w d6, d5
	bne.w rewriteNextFormal
	moveq #0, d4
	move.w records.Entry.Name(a4), d4
	add.w records.Entry.Leaf(a4), d4
	lea layout.ARENA(a0), a4
	adda.l d4, a4
	moveq #0, d4
rewriteCompare
	cmp.w d6, d4
	bhs.w rewriteMatched
	move.w d4, d1
	add.w d3, d1
	moveq #0, d2
	move.b 0(a2, d1.w), d2
	move.w d2, d1
	cmpi.b #'A', d1
	blo.w rewriteLeftFolded
	cmpi.b #'Z', d1
	bhi.w rewriteLeftFolded
	addi.b #32, d1
rewriteLeftFolded
	moveq #0, d2
	move.b 0(a4, d4.w), d2
	cmpi.b #'A', d2
	blo.w rewriteRightFolded
	cmpi.b #'Z', d2
	bhi.w rewriteRightFolded
	addi.b #32, d2
rewriteRightFolded
	cmp.b d1, d2
	bne.w rewriteNextFormal
	addq.w #1, d4
	bra.w rewriteCompare
rewriteMatched
	add.w d7, d7
	add.w d3, d6
	cmpi.w #2, d3
	bne.w rewriteArgument
	addq.w #1, d6
	bra.w rewriteArgument
rewriteNextFormal
	addq.w #1, d7
	bra.w rewriteFormal
rewriteArgument
	lea TEXT_END0(a6), a4
	moveq #0, d4
	move.w 0(a4, d7.w), d4
	tst.w d7
	beq.w rewriteFirst
	moveq #0, d5
	move.w -2(a4, d7.w), d5
	bra.w rewriteTextReady
rewriteFirst
	moveq #0, d5
rewriteTextReady
	sub.w d5, d4
	beq.w rewriteBad
	movea.l a5, a4
	adda.w d4, a4
	cmpa.l a1, a4
	bhs.w rewriteBad
	lea TEXT(a6), a4
	adda.w d5, a4
rewriteTextCopy
	move.b (a4)+, (a5)+
	subq.w #1, d4
	bne.w rewriteTextCopy
	adda.w d6, a2
	bra.w rewriteByte
rewriteAllArguments
	moveq #0, d4
	move.w FULL_BYTES(a6), d4
	tst.w d4
	beq.w rewriteAllDone
	movea.l a5, a4
	adda.w d4, a4
	cmpa.l a1, a4
	bhs.w rewriteBad
	lea FULL_TEXT(a6), a4
rewriteAllCopy
	move.b (a4)+, (a5)+
	subq.w #1, d4
	bne.w rewriteAllCopy
rewriteAllDone
	addq.l #2, a2
	bra.w rewriteByte
rewriteLiteral
	movea.l a5, a4
	addq.l #1, a4
	cmpa.l a1, a4
	bhs.w rewriteBad
	move.b (a2)+, (a5)+
	bra.w rewriteByte
rewriteComplete
	movea.l (sp), a4
	move.l a5, d0
	sub.l a4, d0
	subq.l #2, d0
	cmpi.l #252, d0
	bhi.w rewriteBad
	move.b d0, 1(a4)
	addq.w #3, d0
	cmpa.l a1, a5
	bhs.w rewriteBad
	move.b d0, (a5)+
	moveq #0, d0
	bra.w rewriteDone
rewriteBad
	moveq #1, d0
rewriteDone
	addq.l #8, sp
	movem.l (sp)+, d1-d7/a0-a4/a6
	tst.l d0
	rts
	.bend  ; rewriteCallText

; Reuse the exact-text placeholder expander for decoded string bytes. The
; temporary sidecar and result occupy separate bounded session scratch areas;
; the emitted record remains a packed kind-3 string with no source pointer.
; A0=session,A1=scope,A2=string token,A3=body end,A4=definition,A5=output,
; A6=frame,D7=output end. D0=status,D1=source bytes consumed,A5 advances.
expandStringToken	.block
	movem.l d2-d7/a0-a4/a6, -(sp)
	move.l a0, -(sp)
	move.l a1, -(sp)
	move.l a2, -(sp)
	move.l a5, -(sp)
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #2, d0
	blo.w stringBad
	moveq #0, d4
	move.b 1(a2), d4
	move.l d4, d5
	addq.l #2, d5
	cmp.l d5, d0
	blo.w stringBad
	cmpi.l #250, d4
	bhi.w stringBad
	movea.l 12(sp), a3
	adda.l #COMPOSITE_TEXT, a3
	move.b #TOKEN_CALL_TEXT, (a3)+
	move.b d4, (a3)+
	lea 2(a2), a1
	move.w d4, d6
stringInputCopy
	tst.w d6
	beq.w stringInputDone
	move.b (a1)+, (a3)+
	subq.w #1, d6
	bra.w stringInputCopy
stringInputDone
	move.l d5, d2
	addq.w #1, d2
	move.b d2, (a3)+
	movea.l 12(sp), a2
	adda.l #COMPOSITE_TEXT, a2
	movea.l 12(sp), a5
	adda.l #HEADER_FRAME, a5
	movea.l a5, a1
	adda.w #256, a1
	movea.l 8(sp), a0
	jsr rewriteCallText
	bne.w stringBad
	movea.l 12(sp), a2
	adda.l #HEADER_FRAME, a2
	cmpi.b #TOKEN_CALL_TEXT, (a2)
	bne.w stringBad
	moveq #0, d4
	move.b 1(a2), d4
	cmpi.w #250, d4
	bhi.w stringBad
	movea.l (sp), a5
	movea.l a5, a3
	adda.w #2, a3
	adda.w d4, a3
	movea.l d7, a0
	cmpa.l a0, a3
	bhi.w stringBad
	move.b #3, (a5)+
	move.b d4, (a5)+
	addq.l #2, a2
	move.w d4, d6
stringOutputCopy
	tst.w d6
	beq.w stringOutputDone
	move.b (a2)+, (a5)+
	subq.w #1, d6
	bra.w stringOutputCopy
stringOutputDone
	move.l d5, d1
	moveq #0, d0
	bra.w stringExit
stringBad
	movea.l (sp), a5
	moveq #0, d1
	moveq #1, d0
stringExit
	lea 16(sp), sp
	movem.l (sp)+, d2-d7/a0-a4/a6
	tst.l d0
	rts
	.bend  ; expandStringToken

; Expand a composite recipe from an invocation frame. A0=session,A1=scope,
; A2=recipe,A3=record end,A4=call frame,A5=output,A6=output end.
; D0/CCR=status,D1=consumed bytes,A5 advances; other registers preserved.
expandComposite	.block
	movem.l d2-d7/a0-a4/a6, -(sp)
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #4, d0
	blo.w bad
	moveq #0, d1
	move.b 1(a2), d1
	addq.l #2, d1
	cmp.l d1, d0
	blo.w bad
	move.l d1, -(sp)
	move.l a2, d7
	add.l d1, d7
	bcs.w badLocal
	moveq #0, d5
	move.b 2(a2), d5
	moveq #0, d6
	move.b 3(a2), d6
	lea 4(a2), a2
	adda.l #COMPOSITE_TEXT, a0
	movea.l a0, a3
	moveq #0, d4
fragment
	tst.w d6
	beq.w fragmentsDone
	move.l a2, d0
	cmp.l d7, d0
	bhs.w badLocal
	moveq #0, d2
	move.b (a2)+, d2
	tst.w d2
	beq.w literalFragment
	cmpi.w #9, d2
	bhi.w badLocal
	bra.w positionalFragment
literalFragment
	move.l a2, d0
	cmp.l d7, d0
	bhs.w badLocal
	moveq #0, d3
	move.b (a2)+, d3
	move.l a2, d0
	add.l d3, d0
	bcs.w badLocal
	cmp.l d7, d0
	bhi.w badLocal
	move.l d4, d0
	add.l d3, d0
	cmpi.l #255, d0
	bhi.w badLocal
	move.l d0, d4
	tst.w d3
	beq.w nextFragment
copyLiteralFragment
	move.b (a2)+, (a0)+
	subq.w #1, d3
	bne.w copyLiteralFragment
	bra.w nextFragment
positionalFragment
	subq.w #1, d2
	move.w d2, d3
	add.w d3, d3
	moveq #0, d0
	move.l a0, -(sp)
	lea TEXT_END0(a4), a0
	move.w 0(a0, d3.w), d0
	tst.w d3
	beq.w firstArgument
	subq.w #2, d3
	moveq #0, d1
	move.w 0(a0, d3.w), d1
	sub.l d1, d0
	bra.w argumentLength
firstArgument
	moveq #0, d1
argumentLength
	movea.l (sp)+, a0
	tst.l d0
	beq.w badLocal
	move.l d0, d3
	move.l d4, d0
	add.l d3, d0
	cmpi.l #255, d0
	bhi.w badLocal
	move.l d0, d4
	move.l a2, -(sp)
	lea TEXT(a4), a2
	adda.l d1, a2
copyArgumentText
	move.b (a2)+, (a0)+
	subq.w #1, d3
	bne.w copyArgumentText
	movea.l (sp)+, a2
nextFragment
	subq.w #1, d6
	bra.w fragment
fragmentsDone
	move.l a2, d0
	cmp.l d7, d0
	bne.w badLocal
	tst.w d4
	beq.w badLocal
	cmpi.w #3, d5
	beq.w stringResult
	cmpi.w #1, d5
	bhi.w badLocal
	move.l a6, d0
	sub.l a5, d0
	cmpi.l #4, d0
	blo.w badLocal
	movea.l a3, a0
	move.l d4, d0
	jsr scopes.bind
	bne.w badLocal
	move.b #0, (a5)+
	move.w d1, d0
	lsr.w #8, d0
	move.b d0, (a5)+
	move.b d1, (a5)+
	move.b d2, (a5)+
	bra.w success
stringResult
	cmpi.w #250, d4
	bhi.w badLocal
	move.l a6, d0
	sub.l a5, d0
	move.l d4, d1
	addq.l #2, d1
	cmp.l d1, d0
	blo.w badLocal
	move.b #3, (a5)+
	move.b d4, (a5)+
	movea.l a3, a0
stringResultCopy
	move.b (a0)+, (a5)+
	subq.w #1, d4
	bne.w stringResultCopy
success
	moveq #0, d0
	bra.w doneLocal
badLocal
	moveq #1, d0
doneLocal
	move.l (sp)+, d1
done
	movem.l (sp)+, d2-d7/a0-a4/a6
	tst.l d0
	rts
bad
	moveq #1, d0
	moveq #0, d1
	bra.w done
	.bend  ; expandComposite
BlockWord
	.byte "block"
EndblockWord
	.byte "endblock"
	.align 2  ; the next module shares this instruction section
	.endsection
	.endmodule
