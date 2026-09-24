; Preparation-only module ownership and visibility for numeric source identities.
; @opforge-owner: experimental.amigaos.binary_modules
	.module experimental.amigaos.binary_modules
	.cpu 68020
	.include "telemetry_macros.i"
	.use experimental.amigaos.binary_binding_records as records
	.pub
LIMIT = 512
State	.struct
Active	.word ?
Explicit	.word ?
Outside	.word ?
Visibility	.word ?
Base	.word ?
Selection	.word ?
FileDerived	.word ?
.endstruct
OWNERS = State.FileDerived+2
ORIGINS = OWNERS+LIMIT*2
FLAGS = ORIGINS+LIMIT*2
SCRATCH_BYTES = FLAGS+LIMIT*2
PUBLIC = 1
USED = 2
MIXED = 4
MODULE_ID = 8
SELECTED = 16
KIND_MODULE = 3
	.section code, kind=code

; A0=state,D0=first source ID. Clear bounded metadata. D0/CCR=status;
; other registers preserved. Zero module ID denotes the unscoped global unit.
begin	.block
	movem.l d1/a0, -(sp)
	move.w #SCRATCH_BYTES/2-1, d1
clear
	clr.w (a0)+
	dbra d1, clear
	movem.l (sp)+, d1/a0
	move.w d0, State.Base(a0)
	moveq #0, d0
	rts
	.bend  ; begin

; A0=state. Ordinary content is allowed in global mode or an active module.
; Remember pre-module content so a later explicit module cannot legalize it.
; D0/CCR=status; other registers preserved.
content	.block
	tst.w State.Active(a0)
	bne.w ok
	tst.w State.Explicit(a0)
	bne.w bad
	move.w #1, State.Outside(a0)
ok
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; content

; D0=source index,A0=state. Capture declaration ownership and visibility without
; disturbing accumulated reference origins or the independent module identity.
; Preserves all registers; CCR unspecified.
claim	.block
	movem.l d0-d1/a0-a1, -(sp)
	add.w d0, d0
	lea OWNERS(a0), a1
	move.w State.Active(a0), 0(a1, d0.w)
	lea FLAGS(a0), a1
	andi.w #$fffe, 0(a1, d0.w)
	move.w State.Visibility(a0), d1
	or.w d1, 0(a1, d0.w)
	movem.l (sp)+, d0-d1/a0-a1
	rts
	.bend  ; claim

; D0=source index,A0=state. Aggregate all reference origins, including references
; before declaration. Preserves all registers; CCR unspecified.
reference	.block
	movem.l d0-d1/a0-a2, -(sp)
	add.w d0, d0
	lea FLAGS(a0), a1
	lea ORIGINS(a0), a2
	btst #1, 1(a1, d0.w)
	bne.w previous
	ori.w #USED, 0(a1, d0.w)
	move.w State.Active(a0), 0(a2, d0.w)
	bra.w done
previous
	move.w State.Active(a0), d1
	cmp.w 0(a2, d0.w), d1
	beq.w done
	ori.w #MIXED, 0(a1, d0.w)
done
	movem.l (sp)+, d0-d1/a0-a2
	rts
	.bend  ; reference

; A0=state,D0=reference index,D1=resolved declaration index. Public definitions
; permit every origin; a private definition requires all uses from its owner.
; D0/CCR=status; other registers preserved.
check	.block
	movem.l d1-d3/a0-a2, -(sp)
	add.w d0, d0
	add.w d1, d1
	lea FLAGS(a0), a1
	move.w 0(a1, d0.w), d2
	btst #1, d2
	beq.w ok
	btst #0, 1(a1, d1.w)
	bne.w ok
	lea OWNERS(a0), a2
	tst.w 0(a2, d1.w)
	beq.w ok  ; private global symbols remain visible across modules
	btst #2, d2
	bne.w bad
	lea ORIGINS(a0), a1
	move.w 0(a1, d0.w), d3
	lea OWNERS(a0), a1
	cmp.w 0(a1, d1.w), d3
	bne.w bad
ok
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; check

; D0=module source index,A0=metadata,A1=Entry array,A2=name arena,
; A3=scope state,A4=source binder callback. Caller proves root scope and ID bounds.
; Build dotted lexical prefixes separately from module identity. Binder follows
; writer ABI and preserves D3-D7/A2-A6. D0/CCR=status, other registers preserved.
open	.block
	movem.l d1-d7/a0-a6, -(sp)
	.TELEMETRY_SERVICE_ENTER runtime_profile.OPFORGE_RUNTIME_SERVICE_STATE
	movea.l a0, a6
	movea.l a1, a5
	move.l d0, d7
	tst.w State.Active(a6)
	bne.w bad
	tst.w State.Outside(a6)
	bne.w bad
	move.l d0, d1
	add.w d1, d1
	lea FLAGS(a6), a0
	btst #3, 1(a0, d1.w)
	bne.w bad  ; explicit module identities cannot reopen
	move.l d7, d0
	lsl.l #4, d0
	movea.l a5, a0
	adda.l d0, a0
	moveq #0, d0
	move.w records.Entry.Name(a0), d0
	adda.l d0, a2
	moveq #0, d6
	move.w records.Entry.Length(a0), d6
	moveq #0, d5
	moveq #0, d4
scan
	cmp.l d6, d5
	bhs.w ready
	cmpi.b #'.', 0(a2, d5.w)
	bne.w next
	tst.w d5
	beq.w bad
	cmpi.b #'.', -1(a2, d5.w)
	beq.w bad
	move.l d5, d0
	addq.w #1, d0
	cmp.w d6, d0
	bhs.w bad
	movea.l a2, a0
	movea.l a3, a1
	move.l d5, d0
	jsr (a4)
	tst.l d0
	bne.w bad
	sub.w State.Base(a6), d1
	move.l d1, d0
	lsl.l #4, d0
	movea.l a5, a0
	adda.l d0, a0
	move.w d4, records.Entry.Owner(a0)
	addq.w #1, d1
	move.w d1, d4
next
	addq.w #1, d5
	bra.w scan
ready
	move.l d7, d0
	lsl.l #4, d0
	movea.l a5, a0
	adda.l d0, a0
	move.w d4, records.Entry.Owner(a0)
	move.w #KIND_MODULE, records.Entry.ScopeKind(a0)
	move.l d7, d0
	add.w d0, d0
	lea FLAGS(a6), a0
	ori.w #MODULE_ID, 0(a0, d0.w)
	addq.w #1, d7
	move.w d7, State.Active(a6)
	move.w #1, State.Explicit(a6)
	clr.w State.Visibility(a6)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	.TELEMETRY_SERVICE_LEAVE
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; open

; A0=metadata,D0=current lexical scope index+1. Close only the module frame;
; a nested unclosed block/namespace cannot be silently discarded.
; D0/CCR=status; other registers preserved.
close	.block
	tst.w State.Active(a0)
	beq.w bad
	cmp.w State.Active(a0), d0
	bne.w bad
	clr.w State.Active(a0)
	clr.w State.Visibility(a0)
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; close
	.endsection
	.endmodule
