; Preparation adapter from owned TKVM records to neutral statement facets.
; Number spelling/u64 metadata is already produced by TKVM normalization;
; punctuation text belongs to the package policy, decoded strings to shared UTF8.
; @opforge-owner: experimental.amigaos.binary_statement_facets
	.module experimental.amigaos.binary_statement_facets
	.cpu 68020
	.use experimental.amigaos.binary_source as writer
	.use experimental.amigaos.binary_package as package
	.use tkvm.amigaos.runtime as tokenizer
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_inputs as inputs
	.use prvm.amigaos.statement_policy as policy
	.use prvm.amigaos.statement_text as text
	.use prvm.amigaos.statement_facets as facets
	.pub
TOKEN_LIMIT = 64
NAME_BYTES = 2
NO_NAME = $ffff
Frame	.struct
Package	.long ?  ; previously validated native capsule
Tokens	.long ?  ; original writer.Token records, not packed writer bytes
TokenBytes	.long ?
Count	.long ?  ; rows; every other extent/capacity is measured in bytes
Lexemes	.long ?
LexemeBytes	.long ?
Ids	.long ?  ; caller-bound u16 identities for identifier/register rows
IdsBytes	.long ?
Views	.long ?  ; destination abi.TokenView rows
ViewCapacity	.long ?
Names	.long ?  ; destination u16 identity per token, NO_NAME for nonnames
NameCapacity	.long ?
Arena	.long ?  ; destination normalized literal bytes
ArenaCapacity	.long ?
Workspace	.long ?  ; disjoint staging: Count*(TOKEN_BYTES+NAME_BYTES)+ArenaCapacity
WorkspaceBytes	.long ?
Budget	.long ?  ; disjoint, aligned remaining-step cell; consumed on failure too
ViewCount	.long ?  ; published only after the complete request succeeds
ArenaUsed	.long ?
	.endstruct
FRAME_BYTES = Frame.ArenaUsed+4
	.priv
Local	.struct
RenderRows	.long ?
Used	.long ?
Index	.long ?
Views	.long ?
Names	.long ?
Arena	.long ?
Required	.long ?
SpanCount	.long ?
	.endstruct
Bound	.struct
Start	.long ?
End	.long ?
Mutable	.long ?
	.endstruct
BOUND_BYTES = Bound.Mutable+4
BOUND_LIMIT = 10
BOUNDS = Local.SpanCount+4
LOCAL_BYTES = BOUNDS+BOUND_LIMIT*BOUND_BYTES
Check	.struct
Pointer	.word ?
Extent	.word ?
Mutable	.word ?
Aligned	.word ?
	.endstruct
CHECK_BYTES = Check.Aligned+2
CHECK_COUNT = 7
NumberValue	.struct
High	.long ?
Low	.long ?
	.endstruct
NUMBER_VALUE_BYTES = NumberValue.Low+4
	.section code, kind=code
	.pub
; A0=caller-owned Frame. D0/CCR=statement status; other registers preserved.
; Counts are rows; all capacities/extents are bytes. At most TOKEN_LIMIT rows.
; Caller owns bound IDs and all storage; no binder, source reread or allocation.
; Numeric metadata must come from the current TKVM numeric normalizer. Values
; above i64::MAX have failed numeric facets, matching shared parse_number.
; Views/Names/Arena and publication fields stay untouched on any failure;
; workspace and budget may change. All writable spans must be disjoint from
; readable spans and each other. Workspace requirement is computed, not a
; language text limit. Source columns retain their existing 1-based spans.
prepare	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l a0, a5
	movea.l sp, a6
	clr.l Local.SpanCount(a6)
	bsr.w request
	bne.w done
	movea.l Frame.Package(a5), a0
	move.l package.Header.StatementCompile(a0), d0
	cmpi.l #package.HEADER_BYTES, d0
	blo.w invalidProgram
	move.l package.Header.Bytes(a0), d1
	sub.l d0, d1
	bcs.w invalidProgram
	move.l package.Header.StatementCompileBytes(a0), d2
	beq.w invalidProgram
	cmp.l d1, d2
	bhi.w invalidProgram
	adda.l d0, a0
	move.l d2, d0
	movea.l Frame.Budget(a5), a2
	jsr policy.compile
	bne.w done
	move.l a1, Local.RenderRows(a6)
	move.l Frame.Workspace(a5), Local.Views(a6)
	move.l Frame.Count(a5), d0
	lsl.l #5, d0  ; abi.TOKEN_BYTES = 32
	add.l Frame.Workspace(a5), d0
	move.l d0, Local.Names(a6)
	move.l Frame.Count(a5), d1
	add.l d1, d1
	add.l d1, d0
	move.l d0, Local.Arena(a6)
	clr.l Local.Used(a6)
	clr.l Local.Index(a6)
	movea.l Frame.Tokens(a5), a4
	movea.l Local.Views(a6), a3
row
	move.l Local.Index(a6), d0
	cmp.l Frame.Count(a5), d0
	bhs.w validated
	movea.l Frame.Budget(a5), a0
	jsr inputs.tick
	bne.w done
	bsr.w token
	bne.w done
	adda.w #tokenizer.TOKEN_RECORD_SIZE, a4
	adda.w #abi.TOKEN_BYTES, a3
	addq.l #1, Local.Index(a6)
	bra.w row
validated
	movea.l Local.Views(a6), a0
	move.l Frame.Count(a5), d0
	movea.l Local.Arena(a6), a1
	move.l Local.Used(a6), d1
	movea.l Frame.Budget(a5), a2
	jsr facets.validate
	bne.w done
	; No operation after this point can fail: publish the complete staged result.
	movea.l Local.Views(a6), a0
	movea.l Frame.Views(a5), a1
	move.l Frame.Count(a5), d0
	lsl.l #5, d0
	bsr.w copy
	movea.l Local.Names(a6), a0
	movea.l Frame.Names(a5), a1
	move.l Frame.Count(a5), d0
	add.l d0, d0
	bsr.w copy
	movea.l Local.Arena(a6), a0
	movea.l Frame.Arena(a5), a1
	move.l Local.Used(a6), d0
	bsr.w copy
	move.l Frame.Count(a5), Frame.ViewCount(a5)
	move.l Local.Used(a6), Frame.ArenaUsed(a5)
	moveq #abi.OK, d0
	bra.w done
invalidProgram
	moveq #abi.INVALID_PROGRAM, d0
done
	adda.w #LOCAL_BYTES, sp
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; prepare
	.priv
; A5=Frame,A6=locals. Validate every input/capacity and disjoint writable span.
; D0/status/CCR; other registers preserved. Only local bounds are written.
request	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a5, a0
	moveq #FRAME_BYTES, d0
	moveq #1, d1  ; publication fields make the Frame a writable span
	moveq #1, d2
	bsr.w recordSpan
	bne.w done
	move.l Frame.Count(a5), d3
	cmpi.l #TOKEN_LIMIT, d3
	bhi.w invalid
	move.l d3, d0
	mulu.w #tokenizer.TOKEN_RECORD_SIZE, d0
	cmp.l Frame.TokenBytes(a5), d0
	bhi.w invalid
	move.l d3, d0
	add.l d0, d0
	cmp.l Frame.IdsBytes(a5), d0
	bhi.w invalid
	cmp.l Frame.NameCapacity(a5), d0
	bhi.w overflow
	move.l d3, d0
	lsl.l #5, d0
	cmp.l Frame.ViewCapacity(a5), d0
	bhi.w overflow
	move.l d3, d0
	mulu.w #abi.TOKEN_BYTES+NAME_BYTES, d0
	add.l Frame.ArenaCapacity(a5), d0
	bcs.w invalid
	tst.l d0
	bmi.w invalid
	move.l d0, Local.Required(a6)
	cmp.l Frame.WorkspaceBytes(a5), d0
	bhi.w overflow
	movea.l Frame.Package(a5), a0
	move.l a0, d0
	btst #0, d0
	bne.w invalid
	move.l #package.HEADER_BYTES, d0
	moveq #0, d1
	moveq #1, d2
	jsr inputs.span
	bne.w done
	move.l package.Header.Bytes(a0), d0
	cmpi.l #package.HEADER_BYTES, d0
	blo.w invalid
	bsr.w recordSpan
	bne.w done
	lea Checks, a4
	moveq #CHECK_COUNT-1, d7
next
	moveq #0, d0
	move.w Check.Pointer(a4), d0
	movea.l 0(a5, d0.w), a0
	move.w Check.Extent(a4), d0
	move.l 0(a5, d0.w), d0
	moveq #0, d1
	move.w Check.Mutable(a4), d1
	moveq #0, d2
	move.w Check.Aligned(a4), d2
	bsr.w recordSpan
	bne.w done
	adda.w #CHECK_BYTES, a4
	dbra d7, next
	movea.l Frame.Budget(a5), a0
	moveq #4, d0
	moveq #1, d1
	moveq #1, d2
	bsr.w recordSpan
	bne.w done
	jsr inputs.budget
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
	bra.w done
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; request

; A0/D0=span,D1=mutable flag,D2=even alignment flag,A6=locals.
; D0/status/CCR; other registers preserved. Read spans may overlap each other.
recordSpan	.block
	movem.l d1-d7/a0-a2, -(sp)
	move.l d0, d4
	move.l a0, d3
	tst.l d2
	beq.w aligned
	btst #0, d3
	bne.w invalid
aligned
	jsr inputs.span
	bne.w done
	add.l d3, d4  ; end; inputs.span checked wraparound
	move.l Local.SpanCount(a6), d5
	cmpi.l #BOUND_LIMIT, d5
	bhs.w invalid
	lea BOUNDS(a6), a1
	moveq #0, d6
scan
	cmp.l d5, d6
	bhs.w publish
	cmp.l d3, d4
	beq.w next
	move.l Bound.Start(a1), d0
	cmp.l Bound.End(a1), d0
	beq.w next
	move.l Bound.Mutable(a1), d7
	or.l d1, d7
	beq.w next
	cmp.l Bound.Start(a1), d4
	bls.w next
	cmp.l Bound.End(a1), d3
	blo.w invalid
next
	adda.w #BOUND_BYTES, a1
	addq.l #1, d6
	bra.w scan
publish
	move.l d3, Bound.Start(a1)
	move.l d4, Bound.End(a1)
	move.l d1, Bound.Mutable(a1)
	addq.l #1, Local.SpanCount(a6)
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	movem.l (sp)+, d1-d7/a0-a2
	tst.l d0
	rts
	.bend  ; recordSpan

; A4=original token,A3=staged TokenView,A5=Frame,A6=locals.
; D0/status/CCR; other registers preserved. Writes staged rows/arena only.
token	.block
	movem.l d1-d7/a0-a2, -(sp)
	movea.l a3, a1
	moveq #abi.TOKEN_BYTES/4-1, d1
clear
	clr.l (a1)+
	dbra d1, clear
	moveq #0, d7
	move.w writer.Token.Kind(a4), d7
	cmpi.l #abi.RENDER_LAST, d7
	bhi.w invalid
	move.b d7, abi.TokenView.Kind(a3)
	move.l writer.Token.Start(a4), d0
	beq.w invalid
	cmp.l writer.Token.End(a4), d0
	bhi.w invalid
	move.l d0, abi.TokenView.ColStart(a3)
	move.l writer.Token.End(a4), abi.TokenView.ColEnd(a3)
	move.l Local.Used(a6), abi.TokenView.LiteralOffset(a3)
	move.l Local.Index(a6), d2
	add.l d2, d2
	movea.l Local.Names(a6), a1
	move.w #NO_NAME, 0(a1, d2.w)
	cmpi.l #abi.K_REGISTER, d7
	bhi.w lexeme
	movea.l Frame.Ids(a5), a0
	move.w 0(a0, d2.w), d1
	move.w d1, 0(a1, d2.w)
lexeme
	move.l writer.Token.Offset(a4), d1
	move.l Frame.LexemeBytes(a5), d0
	sub.l d1, d0
	bcs.w invalid
	move.l writer.Token.Length(a4), d6
	cmp.l d0, d6
	bhi.w invalid
	movea.l Frame.Lexemes(a5), a0
	adda.l d1, a0
	cmpi.l #abi.K_NUMBER, d7
	bhi.w rendered
	tst.l d6
	beq.w invalid
	cmpi.l #abi.K_NUMBER, d7
	bne.w literal
	move.w writer.Token.Reserved(a4), d1
	andi.w #tokenizer.TOKEN_RECIPE_LOW_MASK, d1
	cmpi.w #tokenizer.NUMBER_VALID, d1
	beq.w numeric
	cmpi.w #tokenizer.NUMBER_MALFORMED, d1
	beq.w literal
	cmpi.w #tokenizer.NUMBER_OVERFLOW, d1
	bne.w invalid
	bra.w literal
numeric
	sub.l d6, d0
	cmpi.l #NUMBER_VALUE_BYTES, d0  ; u64 follows the unchanged spelling
	blo.w invalid
	lea 0(a0, d6.l), a1
	move.l NumberValue.High(a1), d1
	bmi.w literal  ; shared parse_number rejects positive i64 overflow
	move.b #1, abi.TokenView.NumericStatus(a3)
	move.l d1, abi.TokenView.NumericHigh(a3)
	move.l NumberValue.Low(a1), abi.TokenView.NumericLow(a3)
	bra.w literal
rendered
	cmpi.l #abi.K_STRING, d7
	beq.w string
	movea.l Local.RenderRows(a6), a0
	move.l d7, d0
	jsr policy.render
	beq.w invalid
	move.l d0, d6
literal
	move.l Frame.ArenaCapacity(a5), d1
	sub.l Local.Used(a6), d1
	cmp.l d1, d6
	bhi.w overflow
	movea.l Local.Arena(a6), a1
	adda.l Local.Used(a6), a1
	move.l d6, d0
	bsr.w copy
	bra.w ready
string
	move.l d6, abi.TokenView.DecodedBytes(a3)
	movea.l Local.Arena(a6), a1
	adda.l Local.Used(a6), a1
	move.l d6, d0
	move.l Frame.ArenaCapacity(a5), d1
	sub.l Local.Used(a6), d1
	movea.l Frame.Budget(a5), a2
	jsr text.lossy
	bne.w done
	move.l d1, d6
ready
	move.l d6, abi.TokenView.LiteralBytes(a3)
	add.l d6, Local.Used(a6)
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_TOKEN, d0
	bra.w done
overflow
	moveq #abi.OUTPUT_OVERFLOW, d0
done
	movem.l (sp)+, d1-d7/a0-a2
	tst.l d0
	rts
	.bend  ; token

; A0/A1=source/destination,D0=checked byte count. Clobbers A0/A1/D0;
; CCR unspecified. No parsing or failure after caller capacity validation.
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
Checks
	.word Frame.Tokens, Frame.TokenBytes, 0, 1
	.word Frame.Lexemes, Frame.LexemeBytes, 0, 0
	.word Frame.Ids, Frame.IdsBytes, 0, 1
	.word Frame.Views, Frame.ViewCapacity, 1, 1
	.word Frame.Names, Frame.NameCapacity, 1, 1
	.word Frame.Arena, Frame.ArenaCapacity, 1, 0
	.word Frame.Workspace, Frame.WorkspaceBytes, 1, 1
	.endsection
	.endmodule
