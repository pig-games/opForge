; Preparation-only macro and segment templates over numeric writer records.
; Bodies/default expressions use records and IDs; explicit textual substitution
; retains its bounded spelling bytes in a separate preparation-only pool.
; @opforge-owner: experimental.amigaos.binary_templates
	.module experimental.amigaos.binary_templates
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_macro_plans as plans
	.use experimental.amigaos.binary_macro_fragments as fragments
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_binding_records as records
	.pub
ARG_LIMIT = 192
TEXT_LIMIT = 192
PARAM_LIMIT = 9
DEPTH_LIMIT = 64
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
TOKEN_PLAN = 42
TOKEN_LINE_PLAN = 43
LINE_PLAN_BIT = 6
LINE_PLAN_FLAG = 1<<LINE_PLAN_BIT
TEXT_SCRATCH = 42; private placeholder-consumer scratch, never a writer record
ACTION_REGULAR = 0
ACTION_CONSUMED = 1
ACTION_INVOKE = 2
State	.struct
Count	.word ?
Open	.word ?
Skipping	.word ?
Used	.long ?
Depth	.word ?
CallLabel	.long ?
CallLabelPresent	.word ?
Serial	.word ?
RawBytes	.word ?
TextOffset	.word ?
HeaderParen	.word ?
Plans	.long ?
ParserContext	.long ?
GeneratedPlan	.long ?
ActivePlan	.long ?
Package	.long ?  ; session capsule supplies shared core directive identities
FragmentLine	.long ?  ; VM-owned whole-line expansion callback
	.endstruct
CallFrame	.struct
Definition	.word ?
Cursor	.long ?
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
DefaultStart0	.long ?
DefaultStart1	.long ?
DefaultStart2	.long ?
DefaultStart3	.long ?
DefaultStart4	.long ?
DefaultStart5	.long ?
DefaultStart6	.long ?
DefaultStart7	.long ?
DefaultStart8	.long ?
DefaultEnd0	.long ?
DefaultEnd1	.long ?
DefaultEnd2	.long ?
DefaultEnd3	.long ?
DefaultEnd4	.long ?
DefaultEnd5	.long ?
DefaultEnd6	.long ?
DefaultEnd7	.long ?
DefaultEnd8	.long ?
TextStart0	.long ?
TextStart1	.long ?
TextStart2	.long ?
TextStart3	.long ?
TextStart4	.long ?
TextStart5	.long ?
TextStart6	.long ?
TextStart7	.long ?
TextStart8	.long ?
TextEnd0	.long ?
TextEnd1	.long ?
TextEnd2	.long ?
TextEnd3	.long ?
TextEnd4	.long ?
TextEnd5	.long ?
TextEnd6	.long ?
TextEnd7	.long ?
TextEnd8	.long ?
First	.long ?
Last	.long ?
Kind	.word ?
ParamCount	.word ?
HeaderPlan	.long ?
	.endstruct
DEF_BYTES = Def.HeaderPlan+4
KIND_SEGMENT = 0
KIND_MACRO = 1
BLOCK_BYTES = memory.Block.Used+4
DEFS = State.FragmentLine+4
DEFAULTS = DEFS+BLOCK_BYTES
DEFAULT_TEXT = DEFAULTS+BLOCK_BYTES
BODY = DEFAULT_TEXT+BLOCK_BYTES
DEFAULT_USED = DEFAULTS+memory.Block.Used
DEFAULT_TEXT_USED = DEFAULT_TEXT+memory.Block.Used
FRAMES = BODY+BLOCK_BYTES
COMPOSITE_TEXT = FRAMES+DEPTH_LIMIT*FRAME_BYTES
HEADER_FRAME = COMPOSITE_TEXT+256
SCRATCH_BYTES = HEADER_FRAME+FRAME_BYTES
	.section code, kind=code

; A0=caller-owned zero-initialized state, or a previously begun session.
; Frees prior pools and clears definitions for a new assembly session.
; D0/CCR=zero; other registers preserved.
begin	.block
	bsr.w finish
	clr.w State.Count(a0)
	clr.w State.Open(a0)
	clr.w State.Skipping(a0)
	clr.l State.Used(a0)
	clr.w State.Depth(a0)
	clr.l DEFAULT_USED(a0)
	clr.l DEFAULT_TEXT_USED(a0)
	clr.l DEFS+memory.Block.Used(a0)
	clr.l BODY+memory.Block.Used(a0)
	clr.l State.CallLabel(a0)
	clr.w State.CallLabelPresent(a0)
	clr.w State.Serial(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=state. Release all session-owned pools before caller scratch is freed.
; D0/CCR=zero; other registers preserved. Safe on zero-initialized state.
finish	.block
	movem.l a0, -(sp)
	lea DEFS(a0), a0
	jsr memory.release
	adda.l #BLOCK_BYTES, a0
	jsr memory.release
	adda.l #BLOCK_BYTES, a0
	jsr memory.release
	adda.l #BLOCK_BYTES, a0
	jsr memory.release
	movea.l (sp)+, a0
	moveq #0, d0
	rts
	.bend  ; finish

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

; Classify bound statement identity before selecting a macro VM program.
; A0=packed record,A1=scope state,A2=template state. D0/CCR=0 ordinary,
; 1 known/body call,2 macro header,3 segment header. Preserves others.
role	.block
	movem.l d1-d4/a0-a4, -(sp)
	movea.l a0, a3
	movea.l a1, a4
	moveq #0, d3
	move.b (a3), d3
	addq.l #1, d3
	cmpi.l #9, d3
	blo.w none
	lea 4(a3), a1
	cmpi.b #1, (a1)
	bhi.w dot
	cmpi.l #13, d3
	blo.w none
	addq.l #4, a1
	cmpi.b #5, (a1)
	bne.w dot
	cmpi.l #14, d3
	blo.w none
	addq.l #1, a1
dot
	cmpi.b #7, (a1)
	bne.w none
	cmpi.b #1, 1(a1)
	bhi.w none
	moveq #0, d4
	move.w 2(a1), d4
	tst.b 4(a1)
	bne.w lookup
	; Scoped names cannot be package core identities.
	cmp.w layout.State.Base(a4), d4
	bhs.w scopeDirective
	movea.l State.Package(a2), a3
	; Core directives never acquire invocation spelling or argument plans.
	; Their numeric identities are shared package metadata, not target grammar.
	lea package.Header.CpuDirective(a3), a3
	moveq #5, d1
coreDirective
	cmp.w (a3)+, d4
	beq.w none
	dbra d1, coreDirective
	movea.l State.Package(a2), a3
	cmp.w package.Header.AlignDirective(a3), d4
	beq.w none
	cmp.w package.Header.ResDirective(a3), d4
	beq.w none
scopeDirective
	move.l d4, d0
	movea.l a4, a0
	jsr scopes.classifyDirective
	cmpi.l #scopes.KEY_MACRO, d0
	beq.w macro
	cmpi.l #scopes.KEY_SEGMENT, d0
	beq.w segment
	tst.l d0
	bne.w none
lookup
	moveq #0, d2
	movea.l DEFS+memory.Block.Pointer(a2), a3
next
	cmp.w State.Count(a2), d2
	bhs.w body
	move.l d4, d0
	moveq #0, d1
	move.w Def.Name(a3), d1
	movea.l a4, a0
	jsr scopes.templateCandidate
	beq.w call
	adda.l #DEF_BYTES, a3
	addq.w #1, d2
	bra.w next
body
	tst.w State.Open(a2)
	beq.w none
call
	moveq #1, d0
	bra.w done
macro
	moveq #2, d0
	bra.w done
segment
	moveq #3, d0
	bra.w done
none
	moveq #0, d0
done
	movem.l (sp)+, d1-d4/a0-a4
	tst.l d0
	rts
	.bend  ; role

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
	clr.l State.ActivePlan(a6)
	clr.w State.HeaderParen(a6)
	move.b 1(a5), d0
	andi.b #$60, d0
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
	cmpi.w #6, d0
	bne.w bad
	cmpi.b #TOKEN_LINE_PLAN, (a0)
	beq.w lineRecipe
	cmpi.b #TOKEN_PLAN, (a0)
	bne.w bad
	move.l 1(a0), d1
	movea.l State.Plans(a6), a0
	jsr plans.resolve
	bne.w bad
	move.l a1, State.ActivePlan(a6)
lineRecipe
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
	movea.l DEFS+memory.Block.Pointer(a6), a1
	adda.l d0, a1
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
	movea.l DEFS+memory.Block.Pointer(a6), a1
	adda.l d0, a1
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
	moveq #0, d4
duplicate
	cmp.w State.Count(a6), d4
	bhs.w newDefinition
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	movea.l DEFS+memory.Block.Pointer(a6), a0
	adda.l d0, a0
	cmp.w Def.Name(a0), d5
	beq.w bad
	addq.w #1, d4
	bra.w duplicate
newDefinition
	moveq #0, d0
	move.w d4, d0
	addq.l #1, d0
	mulu.w #DEF_BYTES, d0
	movem.l d0/a0, -(sp)
	lea DEFS(a6), a0
	jsr memory.reserve
	movem.l (sp)+, d0/a0
	bne.w bad
	move.l d0, DEFS+memory.Block.Used(a6)
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	movea.l DEFS+memory.Block.Pointer(a6), a0
	adda.l d0, a0
	clr.w Def.ParamCount(a0)
	lea Def.DefaultStart0(a0), a4
	moveq #36-1, d0
clearDefaults
	clr.l (a4)+
	dbra d0, clearDefaults
	bsr.w bindHeaderPlan
	bne.w bad
parametersDone
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
	movea.l DEFS+memory.Block.Pointer(a6), a0
	adda.l d0, a0
	move.w d5, Def.Name(a0)
	move.l State.Used(a6), Def.First(a0)
	move.l State.Used(a6), Def.Last(a0)
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
	movea.l DEFS+memory.Block.Pointer(a6), a0
	adda.l d4, a0
	cmp.w Def.Kind(a0), d2
	bne.w bad
	move.l State.Used(a6), d0
	cmp.l Def.First(a0), d0
	beq.w bad  ; an invocation must yield at least one record
	move.l State.Used(a6), Def.Last(a0)
	clr.w State.Open(a6)
	bra.w consumed
capture
	; Store the complete raw record; offsets in Def survive relocation.
	tst.l d7
	beq.w consumed
	moveq #0, d6
	move.w State.RawBytes(a6), d6
	moveq #0, d0
	move.l State.Used(a6), d0
	add.l d6, d0
	movem.l d0/a0, -(sp)
	lea BODY(a6), a0
	jsr memory.reserve
	movem.l (sp)+, d0/a0
	bne.w bad
	movea.l BODY+memory.Block.Pointer(a6), a0
	moveq #0, d4
	move.l State.Used(a6), d4
	adda.l d4, a0
	movea.l a5, a1
	move.w d6, d4
copyBody
	move.b (a1)+, (a0)+
	subq.w #1, d4
	bne.w copyBody
	move.l d0, State.Used(a6)
	move.l d0, BODY+memory.Block.Used(a6)
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
	movea.l DEFS+memory.Block.Pointer(a6), a0
	adda.l d0, a0
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
	bsr.w bindCallPlan
	bne.w bad
	moveq #0, d1
	move.w CallFrame.ArgCount(a4), d1
	move.w d4, d0
	mulu.w #DEF_BYTES, d0
	movea.l DEFS+memory.Block.Pointer(a6), a0
	adda.l d0, a0
	move.w d1, d7
fillOmitted
	cmpi.w #PARAM_LIMIT, d7
	bhs.w argumentsBound
	cmp.w Def.ParamCount(a0), d7
	bhs.w omittedEnd
	move.w d7, d0
	lsl.w #2, d0
	moveq #0, d3
	move.l Def.DefaultStart0(a0, d0.w), d3
	moveq #0, d6
	move.l Def.DefaultEnd0(a0, d0.w), d6
	sub.l d3, d6
	beq.w omittedEnd
	moveq #0, d2
	move.w CallFrame.ArgBytes(a4), d2
	add.l d6, d2
	cmpi.l #ARG_LIMIT, d2
	bhi.w bad
	movea.l DEFAULTS+memory.Block.Pointer(a6), a1
	adda.l d3, a1
	lea CallFrame.Argument(a4), a2
	adda.w CallFrame.ArgBytes(a4), a2
copyDefault
	move.b (a1)+, (a2)+
	subq.l #1, d6
	bne.w copyDefault
	move.w d2, CallFrame.ArgBytes(a4)
	moveq #0, d3
	move.l Def.TextStart0(a0, d0.w), d3
	moveq #0, d6
	lea Def.TextEnd0(a0), a1
	move.l 0(a1, d0.w), d6
	sub.l d3, d6
	beq.w bad
	moveq #0, d2
	move.w TEXT_BYTES(a4), d2
	add.l d6, d2
	cmpi.l #TEXT_LIMIT, d2
	bhi.w bad
	movea.l DEFAULT_TEXT+memory.Block.Pointer(a6), a1
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
	move.l Def.First(a0), CallFrame.Cursor(a4)
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

	; VM rows select packed and spelling spans. These helpers only bind/copy.
; A0=definition,A5=record,A6=state. Preserve caller registers except status.
bindHeaderPlan	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a0, a3
	move.l State.ActivePlan(a6), d0
	beq.w bad
	movea.l d0, a4
	movea.l State.Plans(a6), a1
	sub.l memory.Block.Pointer(a1), d0
	addq.l #1, d0
	move.l d0, Def.HeaderPlan(a3)
	lea plans.HEADER_BYTES(a4), a1
	move.l plans.Row.Aux1(a1), d7
	cmpi.l #PARAM_LIMIT, d7
	bhi.w bad
	move.w d7, Def.ParamCount(a3)
	lea plans.ROW_BYTES(a1), a4
	moveq #0, d6
next
	cmp.l d7, d6
	bhs.w good
	cmpi.w #10, plans.Row.Kind(a4)
	bne.w bad
	move.l plans.Row.PackedStart(a4), d0
	move.l plans.Row.PackedEnd(a4), d1
	sub.l d0, d1
	cmpi.l #4, d1
	bne.w bad
	lea 0(a5, d0.l), a1
	cmpi.b #1, (a1)
	bhi.w bad
	move.l d6, d0
	add.w d0, d0
	move.w 1(a1), Def.Parameter(a3, d0.w)
	move.l plans.Row.Aux1(a4), d0
	cmpi.l #-1, d0
	beq.w advance
	movea.l State.ActivePlan(a6), a1
	cmp.l plans.Plan.Count(a1), d0
	bhs.w bad
	lsl.l #5, d0
	lea plans.HEADER_BYTES(a1, d0.l), a2
	cmpi.w #11, plans.Row.Kind(a2)
	bne.w bad
	move.l plans.Row.PackedStart(a2), d0
	move.l plans.Row.PackedEnd(a2), d1
	sub.l d0, d1
	lea 0(a5, d0.l), a1
	movea.l a2, a0
	lea DEFAULTS(a6), a2
	bsr.w storeDefault
	bne.w bad
	move.l d6, d0
	lsl.w #2, d0
	move.l d2, Def.DefaultStart0(a3, d0.w)
	move.l d3, Def.DefaultEnd0(a3, d0.w)
	movea.l a0, a2
	move.l plans.Row.SpellingStart(a2), d0
	move.l plans.Row.SpellingEnd(a2), d1
	sub.l d0, d1
	movea.l State.Plans(a6), a1
	movea.l memory.Block.Pointer(a1), a1
	adda.l d0, a1
	lea DEFAULT_TEXT(a6), a2
	bsr.w storeDefault
	bne.w bad
	move.l d6, d0
	lsl.w #2, d0
	move.l d2, Def.TextStart0(a3, d0.w)
	lea Def.TextEnd0(a3), a2
	move.l d3, 0(a2, d0.w)
advance
	addq.l #1, d6
	adda.l #plans.ROW_BYTES, a4
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; bindHeaderPlan

; A1=selected bytes,D1=length,A2=pool,D6=formal index; D2/D3=pool offsets.
; Preserve A0 (selected row),A3/A4/D6/D7. Empty defaults remain empty spans.
storeDefault	.block
	move.l memory.Block.Used(a2), d2
	move.l d2, d3
	add.l d1, d3
	bcs.w bad
	movem.l a0, -(sp)
	movea.l a2, a0
	move.l d3, d0
	jsr memory.reserve
	movea.l (sp)+, a0
	bne.w bad
	move.l d3, memory.Block.Used(a2)
	movea.l memory.Block.Pointer(a2), a2
	adda.l d2, a2
copy
	tst.l d1
	beq.w good
	move.b (a1)+, (a2)+
	subq.l #1, d1
	bra.w copy
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; storeDefault

; A4=invocation frame,A5=record,A6=state. D0=status; others preserved.
bindCallPlan	.block
	movem.l d1-d7/a0-a3, -(sp)
	move.l State.ActivePlan(a6), d0
	beq.w bad
	movea.l d0, a3
	lea plans.HEADER_BYTES(a3), a3
	move.l plans.Row.Aux1(a3), d7
	cmpi.l #PARAM_LIMIT, d7
	bhi.w bad
	move.w d7, CallFrame.ArgCount(a4)
	clr.w CallFrame.ArgBytes(a4)
	clr.w TEXT_BYTES(a4)
	movea.l State.Plans(a6), a0
	movea.l memory.Block.Pointer(a0), a0
	move.l plans.Row.SpellingStart(a3), d0
	move.l plans.Row.SpellingEnd(a3), d2
	sub.l d0, d2
	cmpi.l #251, d2
	bhi.w bad
	move.w d2, FULL_BYTES(a4)
	lea 0(a0, d0.l), a1
	lea FULL_TEXT(a4), a2
full
	tst.l d2
	beq.w arguments
	move.b (a1)+, (a2)+
	subq.l #1, d2
	bra.w full
arguments
	adda.l #plans.ROW_BYTES, a3
	moveq #0, d1
next
	cmp.l d7, d1
	bhs.w good
	cmpi.w #9, plans.Row.Kind(a3)
	bne.w bad
	move.l plans.Row.PackedStart(a3), d0
	lea 0(a5, d0.l), a1
	move.l plans.Row.PackedEnd(a3), d0
	lea 0(a5, d0.l), a0
	bsr.w appendArgument
	bne.w bad
	move.l plans.Row.SpellingStart(a3), d0
	move.l plans.Row.SpellingEnd(a3), d2
	sub.l d0, d2
	moveq #0, d3
	move.w TEXT_BYTES(a4), d3
	add.l d2, d3
	cmpi.l #TEXT_LIMIT, d3
	bhi.w bad
	movea.l State.Plans(a6), a0
	movea.l memory.Block.Pointer(a0), a0
	lea 0(a0, d0.l), a0
	lea TEXT(a4), a1
	adda.w TEXT_BYTES(a4), a1
copyArgumentSpelling
	tst.l d2
	beq.w textDone
	move.b (a0)+, (a1)+
	subq.l #1, d2
	bra.w copyArgumentSpelling
textDone
	move.w d3, TEXT_BYTES(a4)
	move.w d1, d0
	subq.w #1, d0
	add.w d0, d0
	lea TEXT_END0(a4), a0
	move.w d3, 0(a0, d0.w)
	adda.l #plans.ROW_BYTES, a3
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a3
	tst.l d0
	rts
	.bend  ; bindCallPlan

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
	movea.l DEFS+memory.Block.Pointer(a0), a4
	adda.l d0, a4
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
	move.l CallFrame.Cursor(a6), d0
	cmp.l Def.Last(a4), d0
	blo.w bodyAvailable
	tst.w Def.Kind(a4)
	beq.w exhausted
	move.w #3, CallFrame.CallPhase(a6)
	bra.w macroClose
bodyAvailable
	movea.l 4(sp), a3
	movea.l BODY+memory.Block.Pointer(a3), a3
	adda.l d0, a3
	moveq #0, d5
	move.b (a3), d5
	addq.w #1, d5
	move.l d0, d2
	add.l d5, d2
	cmp.l Def.Last(a4), d2
	bhi.w bad
	move.l d2, CallFrame.Cursor(a6)
	lea 0(a3, d5.w), a2
	clr.w SIDE_BYTES(a6)
	btst #LINE_PLAN_BIT, 1(a3)
	beq.w tokenBody
	cmpi.w #10, d5
	blo.w bad
	cmpi.b #TOKEN_LINE_PLAN, -6(a2)
	bne.w bad
	cmpi.b #6, -1(a2)
	bne.w bad
	move.l -5(a2), d1
	movea.l 4(sp), a0
	move.l State.FragmentLine(a0), d0
	beq.w bad
	movea.l d0, a3
	movea.l State.ParserContext(a0), a0
	movea.l a5, a1
	movea.l a4, a2
	movea.l a6, a4
	jsr (a3)
	tst.l d0
	bne.w bad
	bra.w done
tokenBody
	btst #5, 1(a3)
	beq.w bodyTextReady
	moveq #0, d0
	move.b -1(a2), d0
	cmpi.w #6, d0
	bne.w bad
	movea.l a2, a0
	suba.w d0, a0
	cmpa.l a3, a0
	bls.w bad
	cmpi.b #TOKEN_PLAN, (a0)
	bne.w bad
	move.w d0, SIDE_BYTES(a6)
	movea.l a0, a2
bodyTextReady
	lea 256(a5), a1
	move.b (a3)+, (a5)+
	move.b (a3)+, (a5)+
	move.w CallFrame.CallLine(a6), (a5)+
	addq.l #2, a3  ; source line is replaced by the invocation line
	cmp.l Def.First(a4), d0
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
	movea.l a2, a3
	move.l 1(a3), d1
	movea.l 4(sp), a0
	bsr.w copyCapturedCall
	bne.w bad
	movea.l 4(sp), a0
	move.l State.GeneratedPlan(a0), d0
	beq.w bad
	movea.l d0, a3
	movea.l a0, a2
	adda.l #HEADER_FRAME, a2
	moveq #0, d2
	move.b 1(a2), d2
	addq.l #2, a2
	movea.l 4(sp), a0
	movea.l 36(sp), a1
	move.l a5, d1
	sub.l a1, d1
	move.l d1, d0
	subq.w #1, d0
	move.b d0, (a1)
	andi.b #$df, 1(a1)
	move.l d2, d1
	movea.l State.ParserContext(a0), a0
	jsr (a3)
	bne.w bad
	moveq #0, d0
	bra.w done
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
; A0=session,A4=definition,A6=invocation,D1=captured handle.
; D0/CCR=status; preserves other registers. Scratch frame lives on the stack.
copyCapturedCall	.block
	movem.l d1-d3/a0-a3, -(sp)
	suba.w #fragments.FRAME_BYTES, sp
	movea.l sp, a3
	move.l State.Plans(a0), fragments.Frame.Arena(a3)
	move.l d1, fragments.Frame.Plan(a3)
	move.l Def.HeaderPlan(a4), fragments.Frame.Header(a3)
	moveq #0, d2
	move.w Def.ParamCount(a4), d2
	move.l d2, fragments.Frame.FormalCount(a3)
	lea TEXT(a6), a1
	move.l a1, fragments.Frame.Text(a3)
	lea TEXT_END0(a6), a1
	move.l a1, fragments.Frame.TextEnds(a3)
	moveq #0, d2
	move.w TEXT_BYTES(a6), d2
	move.l d2, fragments.Frame.TextBytes(a3)
	lea FULL_TEXT(a6), a1
	move.l a1, fragments.Frame.Full(a3)
	moveq #0, d2
	move.w FULL_BYTES(a6), d2
	move.l d2, fragments.Frame.FullBytes(a3)
	movea.l a0, a2
	adda.l #HEADER_FRAME, a2
	move.b #TEXT_SCRATCH, (a2)
	lea 2(a2), a1
	move.l a1, fragments.Frame.Output(a3)
	move.l #252, fragments.Frame.Capacity(a3)
	movea.l a3, a0
	jsr fragments.run
	bne.w done
	move.l fragments.Frame.Used(a3), d2
	move.b d2, 1(a2)
	move.l d2, d3
	addq.w #3, d3
	move.b d3, 2(a2, d2.l)
	moveq #0, d0
done
	adda.w #fragments.FRAME_BYTES, sp
	movem.l (sp)+, d1-d3/a0-a3
	tst.l d0
	rts
	.bend  ; copyCapturedCall

; Rewrite decoded string bytes using the current invocation placeholders.
; Captured generated calls consume cached fragments through copyCapturedCall.
; A0=scope,A1=output end,A2=sidecar,A4=definition,A5=output,A6=frame,
; D0=plan arena base,D2=sidecar bytes. D0/CCR=status,A5 advances.
; Other registers are preserved. Formal spelling comes from the header plan.
rewriteCallText	.block
	movem.l d1-d7/a0-a4/a6, -(sp)
	move.l d0, -(sp)
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
	move.b #TEXT_SCRATCH, (a5)+
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
	move.l Def.HeaderPlan(a4), d4
	beq.w rewriteBad
	subq.l #1, d4
	move.l d7, d1
	addq.l #1, d1
	lsl.l #5, d1
	add.l d1, d4
	addi.l #plans.HEADER_BYTES, d4
	movea.l 8(sp), a4
	adda.l d4, a4
	cmpi.w #10, plans.Row.Kind(a4)
	bne.w rewriteBad
	move.l plans.Row.SpellingStart(a4), d4
	move.l plans.Row.SpellingEnd(a4), d5
	sub.l d4, d5
	cmp.l d6, d5
	bne.w rewriteNextFormal
	movea.l 8(sp), a4
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
	adda.w #12, sp
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
	move.b #TEXT_SCRATCH, (a3)+
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
	movea.l 12(sp), a3
	movea.l State.Plans(a3), a3
	move.l memory.Block.Pointer(a3), d0
	jsr rewriteCallText
	bne.w stringBad
	movea.l 12(sp), a2
	adda.l #HEADER_FRAME, a2
	cmpi.b #TEXT_SCRATCH, (a2)
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
