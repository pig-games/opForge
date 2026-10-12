; Borrowed lexical head adapter for acquisition and preparation.
; Only PRVM selects the definition/end grammar; this owner translates the
; already captured TKVM fields and package dictionary identities.
; @opforge-owner: experimental.amigaos.binary_statement_heads
	.module experimental.amigaos.binary_statement_heads
	.cpu 68020
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_source as source
	.use tkvm.amigaos.runtime as tokenizer
	.use prvm.amigaos.statement_abi as abi
	.use prvm.amigaos.statement_head as head
	.pub
Frame	.struct
Package	.long ?  ; validated preparation capsule
Names	.long ?  ; session-owned Cache initialized once by configure
Tokens	.long ?  ; readable original TKVM records
Count	.long ?
Lexemes	.long ?  ; owned decoded lexical arena
LexemeBytes	.long ?
	.endstruct
FRAME_BYTES = Frame.LexemeBytes+4
Cache	.struct
Definition	.long ?  ; capsule-relative spelling, never a memory pointer
DefinitionBytes	.long ?
EndMarker	.long ?
EndMarkerBytes	.long ?
	.endstruct
CACHE_BYTES = Cache.EndMarkerBytes+4
	.priv
; A8 examines a possible label/colon, dot/name and definition keyword only.
; A bounded prefix is sufficient in CLASSIFY mode; no operand interpretation.
HEAD_LIMIT = 6
TOKEN_LIMIT = 64; current captured-line contract
NO_NAME = $ffff
VIEWS = 0
NAMES = VIEWS+HEAD_LIMIT*abi.TOKEN_BYTES
REQUEST = NAMES+HEAD_LIMIT*abi.IDENTITY_BYTES
LOCAL_BYTES = REQUEST+abi.HEAD_FRAME_BYTES
	.section code, kind=code
	.pub
; A0=validated capsule,A1=caller-owned Cache. D0/CCR=status; other registers
; preserved. Resolve both package-owned head spellings once, not for every line.
; A failed configure leaves an unusable partial cache; begin must abort on error.
configure	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a0, a4
	clr.l Cache.Definition(a1)
	clr.l Cache.DefinitionBytes(a1)
	clr.l Cache.EndMarker(a1)
	clr.l Cache.EndMarkerBytes(a1)
	movea.l a4, a0
	adda.l package.Header.StatementHead(a4), a0
	move.w abi.HeadProgram.Definition(a0), d6
	move.w abi.HeadProgram.EndMarker(a0), d7
	movea.l a4, a3
	adda.l package.Header.Dictionary(a4), a3
	move.l package.Header.DictionaryCount(a4), d5
next
	tst.l d5
	beq.w checked
	moveq #0, d3
	move.w package.DictionaryEntry.Length(a3), d3
	move.w package.DictionaryEntry.Name(a3), d0
	cmp.w d6, d0
	beq.w definition
	cmp.w d7, d0
	bne.w advance
	lea Cache.EndMarker(a1), a2
	bra.w store
definition
	movea.l a1, a2
store
	lea package.DICTIONARY_ENTRY_BYTES(a3), a0
	suba.l a4, a0
	move.l a0, (a2)
	move.l d3, Cache.DefinitionBytes(a2)  ; same pair for definition/end spelling
advance
	add.l #package.DICTIONARY_ENTRY_BYTES+1, d3
	andi.l #$fffffffe, d3
	adda.l d3, a3
	subq.l #1, d5
	bra.w next
checked
	tst.l Cache.DefinitionBytes(a1)
	beq.w invalid
	tst.l Cache.EndMarkerBytes(a1)
	beq.w invalid
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_PROGRAM, d0
done
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a4
	rts
	.bend  ; configure

; A0=Frame,A1=caller workspace for Count u16 IDs. D0/CCR=status; others
; preserved. Resolve package head identities only; all other names stay NO_NAME.
; The signature/template owner interns source names separately. Workspace may
; be partially written on failure; it is never a published source-name table.
identities	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a6
	movea.l Frame.Package(a5), a4
	movea.l Frame.Tokens(a5), a3
	move.l Frame.Count(a5), d7
	cmpi.l #TOKEN_LIMIT, d7
	bhi.w invalid
next
	tst.l d7
	beq.w complete
	move.w #NO_NAME, (a6)
	cmpi.w #abi.K_REGISTER, source.Token.Kind(a3)
	bhi.w advance
	bsr.w identity
	bne.w done
	move.w d1, (a6)
advance
	addq.l #abi.IDENTITY_BYTES, a6
	adda.w #tokenizer.TOKEN_RECORD_SIZE, a3
	subq.l #1, d7
	bra.w next
complete
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_ARGUMENT, d0
done
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; identities

; A0=Frame. D0/CCR=status,D1=A8 role (ordinary/definition/end).
; Other registers preserved. Read-only inputs; no binding, allocation or source
; reread. Missing dictionary names use NO_NAME. Tail operands are not examined.
classify	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l a0, a5
	movea.l sp, a6
	move.l Frame.Count(a5), d7
	cmpi.l #HEAD_LIMIT, d7
	bls.w bounded
	moveq #HEAD_LIMIT, d7
bounded
	movea.l a6, a0
	moveq #LOCAL_BYTES/4-1, d0
clear
	clr.l (a0)+
	dbra d0, clear
	movea.l Frame.Package(a5), a4
	movea.l Frame.Tokens(a5), a3
	lea VIEWS(a6), a2
	lea NAMES(a6), a1
	moveq #0, d6
next
	cmp.l d7, d6
	bhs.w select
	moveq #0, d0
	move.w source.Token.Kind(a3), d0
	cmpi.l #abi.RENDER_LAST, d0
	bhi.w invalid
	move.b d0, abi.TokenView.Kind(a2)
	move.l source.Token.Start(a3), abi.TokenView.ColStart(a2)
	move.l source.Token.End(a3), abi.TokenView.ColEnd(a2)
	move.w #NO_NAME, (a1)
	cmpi.l #abi.K_REGISTER, d0
	bhi.w advance
	bsr.w identity
	bne.w done
	move.w d1, (a1)
advance
	adda.w #abi.TOKEN_BYTES, a2
	adda.w #abi.IDENTITY_BYTES, a1
	adda.w #tokenizer.TOKEN_RECORD_SIZE, a3
	addq.l #1, d6
	bra.w next
select
	lea REQUEST(a6), a0
	move.l a4, d0
	add.l package.Header.StatementHead(a4), d0
	move.l d0, abi.HeadFrame.ProgramPtr(a0)
	move.l package.Header.StatementHeadBytes(a4), abi.HeadFrame.ProgramBytes(a0)
	move.l a6, abi.HeadFrame.TokenPtr(a0)
	move.l d7, abi.HeadFrame.TokenCount(a0)
	lea NAMES(a6), a1
	move.l a1, abi.HeadFrame.NamesPtr(a0)
	move.l d7, abi.HeadFrame.NamesCount(a0)
	move.l #abi.HEAD_CLASSIFY, abi.HeadFrame.Mode(a0)
	move.l #64, abi.HeadFrame.StepBudget(a0)
	moveq #abi.HEAD_FRAME_BYTES, d0
	jsr head.run
	bra.w done
invalid
	moveq #abi.INVALID_TOKEN, d0
done
	adda.w #LOCAL_BYTES, sp
	tst.l d0
	movem.l (sp)+, d2-d7/a0-a6
	rts
	.bend  ; classify
	.priv
; A3=original token,A4=capsule,A5=Frame. D0/status,D1=package head ID or
; NO_NAME. Other registers preserved. Only the two A8 identities are candidates;
; matching dictionary spelling is representation lookup, not head grammar.
identity	.block
	movem.l d2-d7/a0-a4, -(sp)
	moveq #-1, d1
	move.l source.Token.Offset(a3), d2
	move.l Frame.LexemeBytes(a5), d0
	sub.l d2, d0
	bcs.w invalid
	move.l source.Token.Length(a3), d3
	cmp.l d0, d3
	bhi.w invalid
	movea.l Frame.Lexemes(a5), a2
	adda.l d2, a2
	movea.l a4, a0
	adda.l package.Header.StatementHead(a4), a0
	move.w abi.HeadProgram.Definition(a0), d6
	move.w abi.HeadProgram.EndMarker(a0), d7
	movea.l Frame.Names(a5), a3
	moveq #1, d5
candidate
	cmp.l Cache.DefinitionBytes(a3), d3
	bne.w skip
	movea.l a2, a0
	movea.l a4, a1
	adda.l Cache.Definition(a3), a1
	move.l d3, d4
	beq.w skip
compare
	moveq #0, d0
	move.b (a0)+, d0
	bsr.w fold
	move.b d0, d2
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w fold
	cmp.b d0, d2
	bne.w skip
	subq.l #1, d4
	bne.w compare
	moveq #0, d1
	move.w d6, d1
	bra.w absent
skip
	adda.w #Cache.EndMarker, a3
	move.w d7, d6
	dbra d5, candidate
absent
	moveq #abi.OK, d0
	bra.w done
invalid
	moveq #abi.INVALID_TOKEN, d0
done
	movem.l (sp)+, d2-d7/a0-a4
	tst.l d0
	rts
	.bend  ; identity

; D0=one dictionary byte, return ASCII-folded byte. Clobbers D0; CCR unspecified.
fold	.block
	cmpi.b #'A', d0
	blo.w done
	cmpi.b #'Z', d0
	bhi.w done
	addi.b #'a'-'A', d0
done
	rts
	.bend  ; fold
	.endsection
	.endmodule
