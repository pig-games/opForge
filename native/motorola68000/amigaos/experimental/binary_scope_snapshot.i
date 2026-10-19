; Read-only canonicalization for a copied closed-module record span. These IDs
; belong to the current preparation session, not a persistent fragment format.
	.pub
Snapshot	.struct
Scope	.long ?
Closed	.long ?
User	.long ?
Result	.long ?
	.endstruct
SNAPSHOT_BYTES = Snapshot.Result+4
SNAPSHOT_READY = 0
SNAPSHOT_INELIGIBLE = 1
SNAPSHOT_INVALID = 2

; records.remap checked-reader ABI: A0=Snapshot,D0=source entry INDEX (not full
; ID),A1=caller Entry destination. D0/CCR=0 success,1 failure; others preserved.
; Result distinguishes unsupported/open binding from malformed/storage failure.
; Closed callback: A0=User,D0=full build-local identity; returns D0/CCR=0 when
; its declaration/origin owner is closed,1 open/unsupported,2 invalid. Preserve
; all other registers. It must be lookup-only and check publication ownership,
; including global/configuration identities, not merely a DECLARED flag.
; Both occurrence and canonical declaration pass this check. Caller supplies a
; closed module and all dependency interfaces; no global finish is performed.
; Only complete unconditional bindings qualify. Deferred member alternatives,
; conditional/mutable bindings and unresolved names retain their live owners.
; Only the caller Entry.Target changes. Live entries/plans are never rewritten;
; lexical paging and temporary scope views retain their ordinary cache effects.
readClosedEntry	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a4
	move.l d0, d7
	move.l #SNAPSHOT_INVALID, Snapshot.Result(a5)
	tst.l Snapshot.Scope(a5)
	beq.w invalidNoView
	tst.l Snapshot.Closed(a5)
	beq.w invalidNoView
	movea.l Snapshot.Scope(a5), a6
	.VIEW_ENTER a6
	cmpi.l #$ffff, d7
	bhi.w invalid
	cmp.w layout.State.Count(a6), d7
	bhs.w invalid
	lea layout.LEXICAL_STATE(a6), a0
	move.l d7, d0
	movea.l a4, a1
	jsr lexical.readEntry
	bne.w invalid
	moveq #0, d0
	move.w records.Entry.Owner(a4), d0
	cmp.w layout.State.Count(a6), d0
	bhi.w invalid
	moveq #0, d0
	move.w records.Entry.Padding(a4), d0
	cmp.w layout.State.Count(a6), d0
	bhi.w invalid
	move.l d7, d0
	add.w layout.State.Base(a6), d0
	bcs.w invalid
	bsr.w snapshotClosed
	bne.w callbackFailure
	move.w records.Entry.Flags(a4), d0
	andi.w #records.CONDITIONAL_VALUE+records.MUTABLE+records.ENCODING_NAME+(1<<(records.ITERATOR_HIGH_BIT+8))+(1<<(records.UPDATE_LOCAL_HIGH_BIT+8)), d0
	bne.w ineligible
	move.w records.Entry.Flags(a4), d0
	andi.w #records.BINDING_PROXY, d0
	bne.w occurrence
	btst #records.IMPORT_PROXY_BIT, records.Entry.Flags+1(a4)
	bne.w imported
	btst #0, records.Entry.Flags+1(a4)
	bne.w direct
	; Ordinary forward/local names use exactly the declaration-aware lookup.
	move.l d7, d0
	bra.w spelling
occurrence
	moveq #0, d0
	move.w records.Entry.OriginalId(a4), d0
	sub.w layout.State.Base(a6), d0
	bcs.w invalid
	cmp.w layout.State.Count(a6), d0
	bhs.w invalid
spelling
	.VIEW_READ a6, d0, a3, ViewFrame.Entry3, a6, invalid
	.VIEW_NAME a6, a3, a0, ViewFrame.Name1, a6, invalid
	moveq #0, d0
	move.w records.Entry.Length(a3), d0
	move.w layout.State.Current(a6), -(sp)
	move.w layout.State.BindingNamespace(a6), -(sp)
	move.w records.Entry.Owner(a4), layout.State.Current(a6)
	clr.w layout.State.BindingNamespace(a6)
	bsr.w declaredName
	move.w (sp)+, layout.State.BindingNamespace(a6)
	move.w (sp)+, layout.State.Current(a6)
	tst.l d0
	bne.w lookupFailure
	moveq #0, d4
	move.w records.Entry.Target(a3), d4
	bra.w target
imported
	btst #records.TEMPLATE_PROXY_BIT, records.Entry.Flags+1(a4)
	bne.w ineligible
	movea.l a4, a0
	movea.l a6, a1
	jsr imports.resolveClosedEntry
	bne.w lookupFailure
	move.l d1, d4
	bra.w target
direct
	moveq #0, d4
	move.w records.Entry.Target(a4), d4
target
	move.l d4, d0
	sub.w layout.State.Base(a6), d0
	bcs.w invalid
	cmp.w layout.State.Count(a6), d0
	bhs.w invalid
	move.l d0, d6
	.VIEW_READ a6, d0, a3, ViewFrame.Entry3, a6, invalid
	btst #0, records.Entry.Flags+1(a3)
	beq.w ineligible
	move.w records.Entry.Flags(a3), d0
	andi.w #records.CONDITIONAL_VALUE+records.MUTABLE+records.BINDING_PROXY+records.IMPORT_PROXY, d0
	bne.w ineligible
	move.l d4, d0
	bsr.w snapshotClosed
	bne.w callbackFailure
	move.l d7, d0
	move.l d6, d1
	lea MODULE_STATE(a6), a0
	jsr modules.check
	bne.w ineligible
	move.w d4, records.Entry.Target(a4)
	move.l #SNAPSHOT_READY, Snapshot.Result(a5)
	moveq #0, d0
	bra.w done
lookupFailure
	tst.l layout.LEXICAL_STATE+lexical.State.Failure(a6)
	bne.w invalid
	bra.w ineligible
callbackFailure
	cmpi.l #SNAPSHOT_INELIGIBLE, d0
	bne.w invalid
ineligible
	move.l #SNAPSHOT_INELIGIBLE, Snapshot.Result(a5)
	bra.w failed
invalid
	move.l #SNAPSHOT_INVALID, Snapshot.Result(a5)
failed
	moveq #1, d0
done
	.VIEW_LEAVE a6
	bra.w exit
invalidNoView
	moveq #1, d0
exit
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; readClosedEntry
	.priv

; A5=Snapshot,D0=full identity; invoke its lookup-only ownership predicate.
; D0/CCR=callback result; other registers preserved by the callback contract.
snapshotClosed	.block
	movem.l a0-a1, -(sp)
	movea.l Snapshot.User(a5), a0
	movea.l Snapshot.Closed(a5), a1
	jsr (a1)
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; snapshotClosed
