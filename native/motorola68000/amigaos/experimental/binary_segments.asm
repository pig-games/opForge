; Bounded preparation-only inline segment templates over numeric writer records.
; Definition storage contains record bytes and numeric identifiers, never text.
	.module experimental.amigaos.binary_segments
	.cpu 68020
	.use experimental.amigaos.binary_scopes as scopes
	.pub
LIMIT = 8
ARG_LIMIT = 64
BODY_BYTES = 4096
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
	.endstruct
Def	.struct
Name	.word ?
Parameter	.word ?
First	.word ?
Last	.word ?
	.endstruct
DEFS = State.CallLine+2
ARGUMENT = DEFS+LIMIT*8
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

; A0=raw writer record,A1=segment state,A2=scope state,D0=conditional
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
	beq.w header
directive
	cmpi.w #9, d6
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
	beq.w close
	cmpi.l #scopes.KEY_SEGMENT, d0
	beq.w bad  ; unsupported directive-first definition
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
	lsl.w #3, d0
	lea DEFS(a6), a0
	adda.w d0, a0
	cmp.w Def.Name(a0), d5
	beq.w call
	addq.w #1, d4
	bra.w findCall
header
	tst.w State.Open(a6)
	bne.w bad
	tst.w State.Skipping(a6)
	bne.w bad
	tst.l d7
	beq.w skipDefinition
	cmpi.w #LIMIT, State.Count(a6)
	bhs.w bad
	cmpi.b #1, 9(a2)
	bhi.w bad
	cmpi.w #17, d6
	bne.w bad  ; one bare parameter, no trailing tokens
	tst.b 12(a2)
	bne.w bad
	move.w 1(a2), d5
	moveq #0, d4
duplicate
	cmp.w State.Count(a6), d4
	bhs.w newDefinition
	move.w d4, d0
	lsl.w #3, d0
	lea DEFS(a6), a0
	adda.w d0, a0
	cmp.w Def.Name(a0), d5
	beq.w bad
	addq.w #1, d4
	bra.w duplicate
newDefinition
	move.w d4, d0
	lsl.w #3, d0
	lea DEFS(a6), a0
	adda.w d0, a0
	move.w d5, Def.Name(a0)
	move.w 10(a2), Def.Parameter(a0)
	move.w State.Used(a6), Def.First(a0)
	move.w State.Used(a6), Def.Last(a0)
	addq.w #1, State.Count(a6)
	addq.w #1, d4
	move.w d4, State.Open(a6)
	bra.w consumed
skipDefinition
	move.w #1, State.Skipping(a6)
	bra.w consumed
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
	lsl.w #3, d4
	lea DEFS(a6), a0
	adda.w d4, a0
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
	move.l a3, d0
	sub.l a2, d0
	cmpi.l #5, d0
	bls.w bad
	lea 5(a2), a1
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
	lsl.w #3, d0
	lea DEFS(a6), a0
	adda.w d0, a0
	move.w Def.First(a0), State.Cursor(a6)
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

; A0=segment state,A1=distinct 256-byte output. D0/CCR=status;
; D1=expanded raw record length, zero when the queued call is exhausted.
; The source line is the invocation line. Other registers preserved.
next	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l a1, a5
	moveq #0, d1
	move.w State.Call(a6), d0
	beq.w exhausted
	subq.w #1, d0
	lsl.w #3, d0
	lea DEFS(a6), a4
	adda.w d0, a4
	moveq #0, d0
	move.w State.Cursor(a6), d0
	cmp.w Def.Last(a4), d0
	bhs.w exhausted
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
copyBytes
	move.b (a3)+, (a5)+
	subq.w #1, d4
	bne.w copyBytes
	bra.w tokens
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
	moveq #0, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
	clr.w State.Call(a6)
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; next
	.endsection
	.endmodule
