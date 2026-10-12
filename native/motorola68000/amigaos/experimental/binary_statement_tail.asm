; Owned invocation argument facets. Shared TKVM lexes only keyword residuals;
; original tail facets (including decoded strings) are copied without reparsing.
; @opforge-owner: experimental.amigaos.binary_statement_tail
.module experimental.amigaos.binary_statement_tail
.cpu 68020
.use experimental.amigaos.binary_memory as memory
.use experimental.amigaos.binary_package as package
.use experimental.amigaos.binary_statement_session as session
.use prvm.amigaos.statement_abi as abi
.use prvm.amigaos.statement_inputs as inputs
.use tkvm.amigaos.runtime as tokenizer
.use experimental.amigaos.binary_tokenizer_capsule as capsule_tokenizer
.pub
TOKEN_LIMIT = 64
BLOCK_BYTES = memory.Block.Used+4
State	.struct
SuffixSession	.res session.STATE_BYTES
Raw	.res BLOCK_BYTES
Merged	.res BLOCK_BYTES
.endstruct
STATE_BYTES = State.Merged+BLOCK_BYTES
Frame	.struct
Source	.long ?  ; session.Snapshot, retained by a separate source session
Route	.long ?  ; verified abi.Route
Package	.long ?
HeadNames	.long ?
Output	.long ?  ; session.Snapshot, published only after complete success
.endstruct
FRAME_BYTES = Frame.Output+4
.priv
RAW_TOKEN_BYTES = TOKEN_LIMIT*tokenizer.TOKEN_RECORD_SIZE
NUMERIC_META_BYTES = 12
LEXICAL_FACTOR = 5
Local	.struct
Suffix	.res session.SNAPSHOT_BYTES
Session	.res session.FRAME_BYTES
Origin	.long ?
HeadIndex	.long ?
TailCount	.long ?
Residual	.long ?
ResidualBytes	.long ?
LexicalCapacity	.long ?
Total	.long ?
Views	.long ?
Arena	.long ?
ArenaBytes	.long ?
Budget	.long ?
Required	.long ?
.endstruct
LOCAL_BYTES = Local.Required+4
.section code, kind=code
.pub
; A0=zero-initialized/prior State; begin releases prior ownership.
; D0/CCR=abi.OK; other registers preserved.
begin	.block
	bsr.w finish
	rts
	.bend  ; begin
; A0=State. Releases all owned storage; safe for zero-initialized ownership.
; D0/CCR=abi.OK; other registers preserved.
finish	.block
	movem.l a0-a1, -(sp)
	movea.l a0, a1
	jsr session.finish
	lea State.Raw(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea State.Merged(a1), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	movem.l (sp)+, a0-a1
	moveq #abi.OK, d0
	rts
	.bend  ; finish
; A0=Frame,A1=zeroed/prior State. D0/status/CCR; all other registers kept.
; Inputs/output/frame must be disjoint from this owner's dynamic storage.
; Returned Snapshot borrows State until next prepare/finish (even on failure).
prepare	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l a0, a5
	movea.l a1, a4
	movea.l sp, a6
	bsr.w request
	bne.w done
	lea Local.Suffix(a6), a0
	moveq #session.SNAPSHOT_BYTES/4-1, d0
clearSuffix
	clr.l (a0)+
	dbra d0, clearSuffix
	tst.l Local.ResidualBytes(a6)
	beq.w merge
	bsr.w suffix
	bne.w done
merge
	bsr.w mergeViews
	bne.w done
	movea.l Frame.Output(a5), a0
	move.l Local.Views(a6), session.Snapshot.Views(a0)
	move.l Local.Total(a6), session.Snapshot.Count(a0)
	move.l Local.Arena(a6), session.Snapshot.Arena(a0)
	move.l Local.ArenaBytes(a6), session.Snapshot.ArenaBytes(a0)
	moveq #abi.OK, d0
done
	adda.w #LOCAL_BYTES, sp
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; prepare
.priv
; Validate acquired numeric facets and route before allocations or publication.
request	.block
	move.l a0, d0
	move.l a1, d1
	or.l d1, d0
	andi.l #3, d0
	bne.w invalid
	moveq #FRAME_BYTES, d0
	jsr inputs.span
	bne.w done
	movea.l a4, a0
	move.l #STATE_BYTES, d0
	jsr inputs.span
	bne.w done
	move.l Frame.Output(a5), d0
	or.l Frame.Source(a5), d0
	or.l Frame.Route(a5), d0
	andi.l #3, d0
	bne.w invalid
	movea.l Frame.Output(a5), a0
	moveq #session.SNAPSHOT_BYTES, d0
	jsr inputs.span
	bne.w done
	movea.l Frame.Route(a5), a0
	moveq #abi.ROUTE_BYTES, d0
	jsr inputs.span
	bne.w done
	movea.l Frame.Source(a5), a3
	movea.l a3, a0
	moveq #session.SNAPSHOT_BYTES, d0
	jsr inputs.span
	bne.w done
	bsr.w disjointInputs
	bne.w done
	move.l session.Snapshot.Count(a3), d0
	cmpi.l #TOKEN_LIMIT, d0
	bhi.w invalid
	movea.l Frame.Route(a5), a2
	move.l abi.Route.HeadToken(a2), d1
	cmp.l d0, d1
	bhs.w invalid
	move.l d1, Local.HeadIndex(a6)
	sub.l d1, d0
	subq.l #1, d0
	move.l d0, Local.TailCount(a6)
	move.l #TOKEN_LIMIT+1, Local.Budget(a6)
	movea.l session.Snapshot.Views(a3), a0
	move.l session.Snapshot.Count(a3), d0
	movea.l session.Snapshot.Arena(a3), a1
	move.l session.Snapshot.ArenaBytes(a3), d1
	lea Local.Budget(a6), a2
	jsr inputs.tokens
	bne.w done
	move.l Local.HeadIndex(a6), d0
	mulu.w #abi.TOKEN_BYTES, d0
	movea.l session.Snapshot.Views(a3), a0
	adda.l d0, a0
	cmpi.b #abi.K_REGISTER, abi.TokenView.Kind(a0)
	bhi.w invalid
	movea.l Frame.Route(a5), a2
	move.l abi.Route.PrefixBytes(a2), d1
	cmp.l abi.TokenView.LiteralBytes(a0), d1
	bhi.w invalid
	move.l abi.TokenView.ColStart(a0), d0
	add.l d1, d0
	bcs.w invalid
	subq.l #1, d0
	bcs.w invalid
	move.l d0, Local.Origin(a6)
	move.l abi.TokenView.LiteralBytes(a0), d0
	sub.l d1, d0
	move.l d0, Local.ResidualBytes(a6)
	add.l abi.TokenView.LiteralOffset(a0), d1
	bcs.w invalid
	add.l session.Snapshot.Arena(a3), d1
	bcs.w invalid
	move.l d1, Local.Residual(a6)
	; Original tail spans must survive translation without unsigned underflow.
	adda.w #abi.TOKEN_BYTES, a0
	move.l Local.TailCount(a6), d2
columns
	tst.l d2
	beq.w valid
	move.l abi.TokenView.ColStart(a0), d0
	sub.l Local.Origin(a6), d0
	bcs.w invalid
	move.l abi.TokenView.ColEnd(a0), d0
	sub.l Local.Origin(a6), d0
	bcs.w invalid
	adda.w #abi.TOKEN_BYTES, a0
	subq.l #1, d2
	bra.w columns
valid
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	rts
	.bend  ; request
suffix	.block
	; Same lexical-capacity bound as the source owner, including numeric metadata.
	move.l Local.ResidualBytes(a6), d0
	move.l d0, d1
	add.l d1, d0
	bcs.w invalid
	add.l d1, d0
	bcs.w invalid
	add.l d1, d0
	bcs.w invalid
	add.l d1, d0
	bcs.w invalid
	add.l #TOKEN_LIMIT*NUMERIC_META_BYTES, d0
	bcs.w invalid
	move.l d0, Local.LexicalCapacity(a6)
	add.l #RAW_TOKEN_BYTES, d0
	bcs.w invalid
	lea State.Raw(a4), a0
	jsr memory.reserveExact
	bne.w overflow
	movea.l Frame.Package(a5), a0
	jsr capsule_tokenizer.activate
	bne.w acquiredFailure
	movea.l a1, a3
	move.l d1, d3
	movea.l State.Raw+memory.Block.Pointer(a4), a1
	lea RAW_TOKEN_BYTES(a1), a2
	movea.l Local.Residual(a6), a0
	move.l Local.ResidualBytes(a6), d0
	moveq #TOKEN_LIMIT, d1
	move.l Local.LexicalCapacity(a6), d2
	jsr tokenizer.tkvmRun68000
	bne.w invalid
	move.l d1, d7
	move.l d3, d6
	lea Local.Session(a6), a0
	moveq #session.FRAME_BYTES/4-1, d0
clearFrame
	clr.l (a0)+
	dbra d0, clearFrame
	lea Local.Session(a6), a0
	move.l a4, session.Frame.State(a0)
	move.l Frame.Package(a5), session.Frame.Package(a0)
	move.l Frame.HeadNames(a5), session.Frame.HeadNames(a0)
	move.l State.Raw+memory.Block.Pointer(a4), d0
	move.l d0, Local.Session+session.Frame.Tokens(a6)
	add.l #RAW_TOKEN_BYTES, d0
	move.l d0, Local.Session+session.Frame.Lexemes(a6)
	move.l d7, Local.Session+session.Frame.Count(a6)
	move.l d6, Local.Session+session.Frame.LexemeBytes(a6)
	lea Local.Session(a6), a0
	lea Local.Suffix(a6), a1
	jsr session.prepare
acquiredFailure
	rts
invalid
	moveq #abi.INVALID_ARGUMENT, d0
	rts
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
	rts
	.bend  ; suffix
mergeViews	.block
	move.l Local.Suffix+session.Snapshot.Count(a6), d0
	add.l Local.TailCount(a6), d0
	cmpi.l #TOKEN_LIMIT, d0
	bhi.w overflow
	move.l d0, Local.Total(a6)
	mulu.w #abi.TOKEN_BYTES, d0
	move.l d0, d7
	movea.l Frame.Source(a5), a3
	move.l session.Snapshot.ArenaBytes(a3), d1
	add.l Local.Suffix+session.Snapshot.ArenaBytes(a6), d1
	bcs.w overflow
	move.l d1, Local.ArenaBytes(a6)
	add.l d1, d0
	bcs.w overflow
	move.l d0, Local.Required(a6)
	lea State.Merged(a4), a0
	jsr memory.reserveExact
	bne.w overflow
	move.l Local.Required(a6), memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a2
	move.l a2, Local.Views(a6)
	adda.l d7, a2
	move.l a2, Local.Arena(a6)
	movea.l session.Snapshot.Arena(a3), a0
	move.l session.Snapshot.ArenaBytes(a3), d0
	bsr.w copyBytes
	movea.l Local.Suffix+session.Snapshot.Arena(a6), a0
	move.l Local.Suffix+session.Snapshot.ArenaBytes(a6), d0
	bsr.w copyBytes
	movea.l Local.Views(a6), a2
	movea.l Local.Suffix+session.Snapshot.Views(a6), a0
	move.l Local.Suffix+session.Snapshot.Count(a6), d6
suffixRows
	tst.l d6
	beq.w tailRows
	movea.l a2, a1
	moveq #abi.TOKEN_BYTES, d0
	bsr.w copyBytes
	move.l session.Snapshot.ArenaBytes(a3), d0
	add.l d0, abi.TokenView.LiteralOffset(a1)
	subq.l #1, d6
	bra.w suffixRows
tailRows
	move.l Local.HeadIndex(a6), d0
	addq.l #1, d0
	mulu.w #abi.TOKEN_BYTES, d0
	movea.l session.Snapshot.Views(a3), a0
	adda.l d0, a0
	move.l Local.TailCount(a6), d6
tailLoop
	tst.l d6
	beq.w valid
	movea.l a2, a1
	moveq #abi.TOKEN_BYTES, d0
	bsr.w copyBytes
	move.l Local.Origin(a6), d0
	sub.l d0, abi.TokenView.ColStart(a1)
	sub.l d0, abi.TokenView.ColEnd(a1)
	subq.l #1, d6
	bra.w tailLoop
valid
	moveq #abi.OK, d0
	rts
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
	rts
	.bend  ; mergeViews
; A0/D0=input bytes,A2=output cursor; only A0/A2/D0 change.
copyBytes	.block
	tst.l d0
	beq.w done
loop
	move.b (a0)+, (a2)+
	subq.l #1, d0
	bne.w loop
done
	rts
	.bend  ; copyBytes
	.include "binary_statement_tail_input.i"
.endsection
.endmodule
