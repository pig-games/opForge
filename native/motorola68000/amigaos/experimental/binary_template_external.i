; Numeric storage facade for owners whose header grammar lives elsewhere.
; Included inside binary_templates: no source synthesis or second expander.
; Handles are one-based Def/index cursors, valid until begin/finish. Span
; pointers are borrowed only during invocation and copied into bounded frames.
	.pub
ExternalDefinition	.struct
Name	.word ?
ParamCount	.word ?
Parameter	.word ?  ; PARAM_LIMIT consecutive numeric formal identities
Parameter1	.word ?
Parameter2	.word ?
Parameter3	.word ?
Parameter4	.word ?
Parameter5	.word ?
Parameter6	.word ?
Parameter7	.word ?
Parameter8	.word ?
Origin	.long ?
HeaderPlan	.long ?  ; optional plans handle, supplies formal spelling facets
	.endstruct
ExternalArgument	.struct
Packed	.long ?
PackedBytes	.word ?
Spelling	.long ?
SpellingBytes	.word ?
	.endstruct
EXTERNAL_ARGUMENT_BYTES = ExternalArgument.SpellingBytes+2
ExternalCall	.struct
Definition	.word ?
CallName	.word ?
CallLine	.word ?
LabelPresent	.word ?
Label	.long ?  ; complete four-byte bound value token
ArgCount	.word ?
Arguments	.long ?  ; ArgCount ExternalArgument rows
FullSpelling	.long ?
FullBytes	.word ?
	.endstruct
STATUS_OK = 0
STATUS_BAD = 1

; Open an externally parsed definition. A0=state,A1=scope,A2=packet.
; D0/CCR=status,D1=handle (zero on failure); other registers preserved.
; Same-name external overloads share one scope declaration, but each owns a
; Def and same-index Node. Internal macros/segments keep duplicate rejection.
; HeaderPlan may be zero for numeric-only bodies; textual recipes require the
; normal session-owned formal spelling plan. No defaults are inferred here.
externalBegin	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	movea.l a2, a3
	tst.w State.Open(a6)
	bne.w bad
	tst.w State.Skipping(a6)
	bne.w bad
	cmpi.w #PARAM_LIMIT, ExternalDefinition.ParamCount(a3)
	bhi.w bad
	moveq #0, d5
	move.w ExternalDefinition.Name(a3), d5
	move.l d5, d0
	movea.l a6, a0
	adda.l #CANDIDATES, a0
	jsr index.find
	moveq #1, d3
	tst.w d1
	beq.w allocate
	moveq #0, d0
	move.w d1, d0
	subq.l #1, d0
	mulu.w #DEF_BYTES, d0
	movea.l DEFS+memory.Block.Pointer(a6), a0
	adda.l d0, a0
	cmpi.w #KIND_EXTERNAL, Def.Kind(a0)
	bne.w bad
	moveq #0, d3
allocate
	moveq #0, d4
	move.w State.Count(a6), d4
	bsr.w reserveDefinition
	bne.w bad
	move.w ExternalDefinition.ParamCount(a3), Def.ParamCount(a0)
	move.l ExternalDefinition.HeaderPlan(a3), Def.HeaderPlan(a0)
	moveq #0, d6
copyFormal
	cmp.w Def.ParamCount(a0), d6
	bhs.w publish
	move.w d6, d0
	add.w d0, d0
	move.w ExternalDefinition.Parameter(a3, d0.w), d1
	move.w d1, Def.Parameter(a0, d0.w)
	addq.w #1, d6
	bra.w copyFormal
publish
	moveq #KIND_EXTERNAL, d2
	movea.l a5, a1
	bsr.w publishDefinition
	bne.w bad
	move.l ExternalDefinition.Origin(a3), Def.Origin(a0)
	moveq #0, d1
	move.w State.Open(a6), d1
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #0, d1
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; externalBegin

; Capture one complete raw writer record in the open external body.
; A0=state,A1=record; caller guarantees the encoded extent is readable.
; D0/CCR=status; other registers preserved. Records retain opaque plan handles
; and source origins; the header/signature owner selects what belongs in a body.
externalCapture	.block
	movem.l d1-d6/a0-a1/a5-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	moveq #0, d0
	move.w State.Open(a6), d0
	beq.w bad
	subq.l #1, d0
	mulu.w #DEF_BYTES, d0
	movea.l DEFS+memory.Block.Pointer(a6), a0
	adda.l d0, a0
	cmpi.w #KIND_EXTERNAL, Def.Kind(a0)
	bne.w bad
	moveq #0, d6
	move.b (a5), d6
	addq.w #1, d6
	cmpi.w #source.RECORD_HEADER_BYTES, d6
	blo.w bad
	bsr.w storeBody
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d6/a0-a1/a5-a6
	tst.l d0
	rts
	.bend  ; externalCapture

; Close the selected external body. A0=state,D1=handle from externalBegin.
; D0/CCR=status; other registers preserved. Empty bodies are valid.
externalClose	.block
	movem.l d1/a1, -(sp)
	tst.w d1
	beq.w bad
	cmp.w State.Open(a0), d1
	bne.w bad
	subq.w #1, d1
	mulu.w #DEF_BYTES, d1
	movea.l DEFS+memory.Block.Pointer(a0), a1
	adda.l d1, a1
	cmpi.w #KIND_EXTERNAL, Def.Kind(a1)
	bne.w bad
	move.l State.Used(a0), Def.Last(a1)
	clr.w State.Open(a0)
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; externalClose

; Queue a selected external body. A0=state,A1=ExternalCall packet.
; D0/CCR=status; other registers preserved. Caller owns signature selection and
; supplies validated packed spans, spelling facets and caller-bound label/IDs.
; Exactly ParamCount arguments are required, with <=ARG_LIMIT packed bytes,
; <=TEXT_LIMIT spelling bytes and <=251 full spelling bytes. Failure never
; increments Depth; next/cursor expand success through the common engine.
externalInvoke	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a3
	tst.w State.Open(a6)
	bne.w bad
	moveq #0, d4
	move.w ExternalCall.Definition(a3), d4
	beq.w bad
	cmp.w State.Count(a6), d4
	bhi.w bad
	subq.w #1, d4
	move.l d4, d0
	mulu.w #DEF_BYTES, d0
	movea.l DEFS+memory.Block.Pointer(a6), a0
	adda.l d0, a0
	cmpi.w #KIND_EXTERNAL, Def.Kind(a0)
	bne.w bad
	move.w ExternalCall.ArgCount(a3), d0
	cmp.w Def.ParamCount(a0), d0
	bne.w bad
	cmpi.w #1, ExternalCall.LabelPresent(a3)
	bhi.w bad
	tst.w ExternalCall.LabelPresent(a3)
	beq.w labelReady
	cmpi.b #1, ExternalCall.Label(a3)
	bhi.w bad
	cmpi.b #1, ExternalCall.Label+3(a3)
	bhi.w bad
labelReady
	move.w ExternalCall.LabelPresent(a3), State.CallLabelPresent(a6)
	move.l ExternalCall.Label(a3), State.CallLabel(a6)
	moveq #0, d5
	move.w ExternalCall.CallName(a3), d5
	bsr.w startCall
	bne.w bad
	move.w ExternalCall.ArgCount(a3), CallFrame.ArgCount(a4)
	movea.l ExternalCall.Arguments(a3), a5
	moveq #0, d7
argument
	cmp.w CallFrame.ArgCount(a4), d7
	bhs.w fullSpelling
	move.l a5, d0
	beq.w bad
	movea.l ExternalArgument.Packed(a5), a1
	move.l a1, d0
	beq.w bad
	moveq #0, d0
	move.w ExternalArgument.PackedBytes(a5), d0
	beq.w bad
	movea.l a1, a0
	adda.l d0, a0
	move.w d7, d1
	bsr.w appendArgument
	bne.w bad
	moveq #0, d2
	move.w ExternalArgument.SpellingBytes(a5), d2
	moveq #0, d3
	move.w TEXT_BYTES(a4), d3
	add.l d2, d3
	cmpi.l #TEXT_LIMIT, d3
	bhi.w bad
	movea.l ExternalArgument.Spelling(a5), a0
	tst.l d2
	beq.w spellingDone
	move.l a0, d0
	beq.w bad
	lea TEXT(a4), a1
	adda.w TEXT_BYTES(a4), a1
copySpelling
	move.b (a0)+, (a1)+
	subq.w #1, d2
	bne.w copySpelling
spellingDone
	move.w d3, TEXT_BYTES(a4)
	move.w d7, d0
	add.w d0, d0
	lea TEXT_END0(a4), a0
	move.w d3, 0(a0, d0.w)
	addq.w #1, d7
	adda.w #EXTERNAL_ARGUMENT_BYTES, a5
	bra.w argument
fullSpelling
	; Absent positional placeholders expand to empty text, as in the common
	; macro cursor. Their cumulative endpoints must never run backwards.
	cmpi.w #PARAM_LIMIT, d7
	bhs.w fullReady
	move.w d7, d0
	add.w d0, d0
	move.w CallFrame.ArgBytes(a4), CallFrame.ArgEnd0(a4, d0.w)
	lea TEXT_END0(a4), a0
	move.w TEXT_BYTES(a4), 0(a0, d0.w)
	addq.w #1, d7
	bra.w fullSpelling
fullReady
	moveq #0, d2
	move.w ExternalCall.FullBytes(a3), d2
	cmpi.w #251, d2
	bhi.w bad
	move.w d2, FULL_BYTES(a4)
	movea.l ExternalCall.FullSpelling(a3), a0
	tst.l d2
	beq.w queue
	move.l a0, d0
	beq.w bad
	lea FULL_TEXT(a4), a1
copyFull
	move.b (a0)+, (a1)+
	subq.w #1, d2
	bne.w copyFull
queue
	move.w ExternalCall.Definition(a3), d0
	subq.w #1, d0
	mulu.w #DEF_BYTES, d0
	movea.l DEFS+memory.Block.Pointer(a6), a0
	adda.l d0, a0
	moveq #0, d1
	move.w ExternalCall.CallLine(a3), d1
	bsr.w queueCall
	moveq #STATUS_OK, d0
	bra.w done
bad
	moveq #STATUS_BAD, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; externalInvoke
