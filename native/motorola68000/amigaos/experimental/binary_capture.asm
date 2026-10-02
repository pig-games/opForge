; Owned unbound TKVM lines and VM-selected candidate plans. Offsets only.
; @opforge-owner: experimental.amigaos.binary_capture
	.module experimental.amigaos.binary_capture
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_source as writer
	.pub
MAGIC = $42435031; BCP1, latest capture contract
TOKEN_LIMIT = 64
Frame	.struct
Arena	.long ?
Tokens	.long ?
Count	.long ?
Lexemes	.long ?
LexemeBytes	.long ?
SourceBytes	.long ?
Origin	.long ?
Line	.long ?
RootFile	.word ?
Reserved	.word ?
Plans	.long ?  ; temporary memory.Block, copied into the owned record
CallStatus	.long ?
CallHandle	.long ?
CallRecipeStatus	.long ?
HeaderStatus	.long ?
HeaderHandle	.long ?
LineStatus	.long ?
LineHandle	.long ?
.endstruct
FRAME_BYTES = Frame.LineHandle+4
Record	.struct
Magic	.long ?
Bytes	.long ?
Count	.long ?
LexemeBytes	.long ?
SourceBytes	.long ?
Origin	.long ?
Line	.long ?
RootFile	.word ?
Reserved	.word ?
PlanBytes	.long ?
CallStatus	.long ?
CallHandle	.long ?
CallRecipeStatus	.long ?
HeaderStatus	.long ?
HeaderHandle	.long ?
LineStatus	.long ?
LineHandle	.long ?
Tokens	.byte ?
.endstruct
HEADER_BYTES = Record.Tokens
	.section code, kind=code
; A0=Frame. Append a variable-size record to Arena. Caller owns input views;
; they must not overlap destination storage. D0/CCR=status,D1=offset+1 handle.
; Other registers preserved. A failed record is never published in Arena.Used.
create	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a6
	movea.l Frame.Arena(a6), a4
	movea.l Frame.Plans(a6), a5
	cmpa.l a4, a5
	beq.w bad
	move.l Frame.Count(a6), d6
	cmpi.l #TOKEN_LIMIT, d6
	bhi.w bad
	mulu.w #20, d6
	addi.l #HEADER_BYTES, d6
	add.l Frame.LexemeBytes(a6), d6
	bcs.w bad
	addq.l #3, d6
	bcs.w bad
	andi.l #$fffffffc, d6
	move.l d6, d5
	add.l memory.Block.Used(a5), d6
	bcs.w bad
	addq.l #3, d6
	bcs.w bad
	andi.l #$fffffffc, d6
	move.l memory.Block.Used(a4), d3
	addq.l #3, d3
	bcs.w bad
	andi.l #$fffffffc, d3
	move.l d3, d0
	add.l d6, d0
	bcs.w bad
	move.l d0, d7
	movea.l a4, a0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a4), a3
	adda.l d3, a3
	movea.l a3, a0
	move.l d6, d0
clear
	clr.b (a0)+
	subq.l #1, d0
	bne.w clear
	move.l #MAGIC, Record.Magic(a3)
	move.l d6, Record.Bytes(a3)
	move.l Frame.Count(a6), Record.Count(a3)
	move.l Frame.LexemeBytes(a6), Record.LexemeBytes(a3)
	move.l Frame.SourceBytes(a6), Record.SourceBytes(a3)
	move.l Frame.Origin(a6), Record.Origin(a3)
	move.l Frame.Line(a6), Record.Line(a3)
	move.w Frame.RootFile(a6), Record.RootFile(a3)
	move.l memory.Block.Used(a5), Record.PlanBytes(a3)
	lea Frame.CallStatus(a6), a0
	lea Record.CallStatus(a3), a1
	moveq #6, d0
outcomes
	move.l (a0)+, (a1)+
	dbra d0, outcomes
	lea HEADER_BYTES(a3), a1
	movea.l Frame.Tokens(a6), a0
	move.l Frame.Count(a6), d0
	mulu.w #20, d0
	bsr.w copy
	movea.l Frame.Lexemes(a6), a0
	move.l Frame.LexemeBytes(a6), d0
	bsr.w copy
	movea.l a3, a1
	adda.l d5, a1
	movea.l memory.Block.Pointer(a5), a0
	move.l memory.Block.Used(a5), d0
	bsr.w copy
	movea.l a3, a1
	move.l d6, d0
	bsr.w validate
	bne.w bad
	move.l d7, memory.Block.Used(a4)
	move.l d3, d1
	addq.l #1, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; create

; A0=memory.Block,D1=handle. D0/CCR=status,A1=immutable bounded record.
; Other registers preserved. A failed handle has A1=0.
resolve	.block
	movem.l d1-d3/a0, -(sp)
	tst.l d1
	beq.w bad
	subq.l #1, d1
	move.l d1, d0
	andi.l #3, d0
	bne.w bad
	move.l memory.Block.Used(a0), d2
	sub.l d1, d2
	bcs.w bad
	cmpi.l #HEADER_BYTES, d2
	blo.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l d1, a1
	move.l Record.Bytes(a1), d0
	cmp.l d2, d0
	bhi.w bad
	bsr.w validate
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	suba.l a1, a1
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0
	tst.l d0
	rts
	.bend  ; resolve

; A1=validated record. A0=owned plan region, D0=region bytes. Other regs kept.
planView	.block
	move.l Record.Count(a1), d0
	mulu.w #20, d0
	addi.l #HEADER_BYTES, d0
	add.l Record.LexemeBytes(a1), d0
	addq.l #3, d0
	andi.l #$fffffffc, d0
	movea.l a1, a0
	adda.l d0, a0
	move.l Record.PlanBytes(a1), d0
	rts
	.bend  ; planView
	.priv
; A1=record,D0=readable record bytes. Validate every token's owned spans.
validate	.block
	movem.l d1-d5/a0-a2, -(sp)
	cmpi.l #HEADER_BYTES, d0
	blo.w bad
	cmpi.l #MAGIC, Record.Magic(a1)
	bne.w bad
	cmp.l Record.Bytes(a1), d0
	bne.w bad
	move.l d0, d5
	move.l Record.Count(a1), d4
	cmpi.l #TOKEN_LIMIT, d4
	bhi.w bad
	move.l d4, d0
	mulu.w #20, d0
	addi.l #HEADER_BYTES, d0
	add.l Record.LexemeBytes(a1), d0
	bcs.w bad
	addq.l #3, d0
	bcs.w bad
	andi.l #$fffffffc, d0
	add.l Record.PlanBytes(a1), d0
	bcs.w bad
	addq.l #3, d0
	bcs.w bad
	andi.l #$fffffffc, d0
	cmp.l d5, d0
	bne.w bad
	cmpi.l #65535, Record.Line(a1)
	bhi.w bad
	cmpi.w #1, Record.RootFile(a1)
	bhi.w bad
	tst.w Record.Reserved(a1)
	bne.w bad
	move.l Record.SourceBytes(a1), d3
	addq.l #1, d3
	bcs.w bad
	lea HEADER_BYTES(a1), a2
token
	tst.l d4
	beq.w good
	cmpi.w #40, writer.Token.Kind(a2)
	bhi.w bad
	move.l writer.Token.Start(a2), d0
	beq.w bad
	move.l writer.Token.End(a2), d1
	cmp.l d0, d1
	blo.w bad
	cmp.l d3, d1
	bhi.w bad
	move.l Record.LexemeBytes(a1), d0
	sub.l writer.Token.Offset(a2), d0
	bcs.w bad
	cmp.l writer.Token.Length(a2), d0
	blo.w bad
	adda.w #20, a2
	subq.l #1, d4
	bra.w token
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d5/a0-a2
	tst.l d0
	rts
	.bend  ; validate
copy	.block
	tst.l d0
	beq.w done
next
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w next
done
	rts
	.bend  ; copy
	.endsection
	.endmodule
