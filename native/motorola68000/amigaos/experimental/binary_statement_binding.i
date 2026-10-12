; Statement declarations share source identities with independent value/template
; facets. Keyword context is module-flat; formal binding remains lexical.
.priv
StatementNameToken .struct
Kind .byte ?
Identity .word ?
Qualifier .byte ?
.endstruct
.pub
; A0/D0=opaque keyword bytes,A1=scope. D0/status,D1=canonical source ID.
; A0/A1 clobbered; other registers preserved. The import owner composes the
; complete module name without interpreting dots inside the retained keyword.
bindStatementKeyword .block
 movem.l d2-d7/a2-a6, -(sp)
 movea.l a1, a6
 jsr imports.statementKeywordBytes
 bne.w done
 move.l d1, d0
 move.w layout.State.Current(a6), -(sp)
 move.w layout.State.BindingNamespace(a6), -(sp)
 clr.w layout.State.Current(a6)
 clr.w layout.State.BindingNamespace(a6)
 movea.l a6, a1
 bsr.w bindOrigin
 move.w (sp)+, layout.State.BindingNamespace(a6)
 move.w (sp)+, layout.State.Current(a6)
done
 movem.l (sp)+, d2-d7/a2-a6
 tst.l d0
 rts
 .bend ; bindStatementKeyword
; A0=four-byte name token,A1=scope,D0=expected module index+1 (zero global).
; D0/status; all other registers preserved. No allocation or publication.
validateStatement .block
 movem.l d1-d7/a0-a6, -(sp)
 movea.l a1, a6
 bsr.w statementEntry
 movem.l (sp)+, d1-d7/a0-a6
 tst.l d0
 rts
 .bend ; validateStatement
; Same inputs/outputs as validateStatement. Validates before publishing; never
; allocates. After validation this is guaranteed if binding/current-module state
; has not changed. Existing same-key overloads append, rather than duplicate-fail.
declareStatement .block
 movem.l d1-d7/a0-a6, -(sp)
 movea.l a1, a6
 bsr.w statementEntry
 bne.w done
 ori.w #records.STATEMENT_DECLARED, records.Entry.TemplateFlags(a2)
 movea.l a2, a1
 lea MODULE_STATE(a6), a0
 jsr modules.claimStatement
 moveq #0, d0
done
 movem.l (sp)+, d1-d7/a0-a6
 tst.l d0
 rts
 .bend ; declareStatement
.priv
; A0=token,A6=scope,D0=expected module. D0/status,A2=entry on success.
statementEntry .block
 move.l d0, d4
 cmpi.l #$ffff, d4
 bhi.w invalid
 cmp.w MODULE_STATE+modules.State.Active(a6), d4
 bne.w invalid
 cmpi.b #1, StatementNameToken.Kind(a0)
 bhi.w invalid
 tst.b StatementNameToken.Qualifier(a0)
 bne.w invalid
 moveq #0, d0
 move.w StatementNameToken.Identity(a0), d0
 sub.w layout.State.Base(a6), d0
 bcs.w invalid
 cmp.w layout.State.Count(a6), d0
 bhs.w invalid
 mulu.w #records.ENTRY_BYTES, d0
 movea.l ENTRIES_POINTER(a6), a2
 adda.l d0, a2
 btst #records.STATEMENT_DECLARED_BIT, records.Entry.TemplateFlags+1(a2)
 beq.w valid
 cmp.w records.Entry.StatementModule(a2), d4
 bne.w invalid
valid
 moveq #0, d0
 rts
invalid
 moveq #1, d0
 rts
 .bend ; statementEntry
