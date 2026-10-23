; Scope-private call frames. Entry copies never borrow a backing page; their
; index travels with them so writes do not depend on an arena address. Nested
; helpers restore their caller's frame, including any live entry/name copies.
View	.struct
Index	.long ?
Entry	.res records.ENTRY_BYTES
.endstruct
ViewFrame	.struct
Entry0	.res View.Entry+records.ENTRY_BYTES
Entry1	.res View.Entry+records.ENTRY_BYTES
Entry2	.res View.Entry+records.ENTRY_BYTES
Entry3	.res View.Entry+records.ENTRY_BYTES
Entry4	.res View.Entry+records.ENTRY_BYTES
Entry5	.res View.Entry+records.ENTRY_BYTES
Name0	.res layout.NAME_BYTES
Name1	.res layout.NAME_BYTES
.endstruct
VIEW_BYTES = ViewFrame.Name1+layout.NAME_BYTES
VIEW_RESULT = 4*4
VIEW_SAVED_LONG = VIEW_BYTES+4
VIEW_ENTER	.macro ownerArg
	move.l VIEWS(.ownerArg), -(sp)
	suba.w #VIEW_BYTES, sp
	move.l sp, VIEWS(.ownerArg)
.endmacro
VIEW_LEAVE	.macro ownerArg
	movea.l VIEWS(.ownerArg), sp
	adda.w #VIEW_BYTES, sp
	move.l (sp)+, VIEWS(.ownerArg)
.endmacro
; Read an indexed entry into one named slot. The address result is returned
; after restoring staging registers, including when it aliases A0 or A1.
VIEW_READ	.macro ownerArg, indexArg, resultArg, slotArg, frameArg, errorArg
	subq.l #4, sp
	movem.l d0-d1/a0-a1, -(sp)
	move.l .indexArg, d0
	move.l .ownerArg, d1
	movea.l .frameArg, a1
	movea.l VIEWS(a1), a1
	adda.w #.slotArg, a1
	move.l d0, View.Index(a1)
	adda.w #View.Entry, a1
	move.l a1, VIEW_RESULT(sp)
	movea.l d1, a0
	adda.w #layout.LEXICAL_STATE, a0
	jsr lexical.readEntry
	movem.l (sp)+, d0-d1/a0-a1
	movea.l (sp)+, .resultArg
	bne.w .errorArg
.endmacro
; Commit before calling a helper that may read or change this identity.
VIEW_WRITE	.macro ownerArg, viewArg, errorArg
	movem.l d0-d1/a0-a1, -(sp)
	move.l .ownerArg, d1
	movea.l .viewArg, a1
	move.l View.Index-View.Entry(a1), d0
	movea.l d1, a0
	adda.w #layout.LEXICAL_STATE, a0
	jsr lexical.writeEntry
	movem.l (sp)+, d0-d1/a0-a1
	bne.w .errorArg
.endmacro
; Copy a complete spelling; two slots allow both compared spellings to survive
; nested acquisition. The returned bytes belong to this frame.
VIEW_NAME	.macro ownerArg, viewArg, resultArg, slotArg, frameArg, errorArg
	subq.l #4, sp
	movem.l d0-d1/a0-a1, -(sp)
	move.l .ownerArg, d1
	movea.l .viewArg, a1
	movea.l d1, a0
	move.l records.Entry.Name(a1), d0
	moveq #0, d1
	move.w records.Entry.Length(a1), d1
	movea.l .frameArg, a1
	movea.l VIEWS(a1), a1
	adda.w #.slotArg, a1
	move.l a1, VIEW_RESULT(sp)
	adda.w #layout.LEXICAL_STATE, a0
	jsr lexical.copyName
	movem.l (sp)+, d0-d1/a0-a1
	movea.l (sp)+, .resultArg
	bne.w .errorArg
.endmacro
