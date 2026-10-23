; Temporary public-module interface from the one retained semantic scope.
; @opforge-owner: opforge.assembler.modules.snapshot
	.module opforge.assembler.modules.snapshot
	.cpu 68020
	.use opforge.assembler.prepare.frontend as frontend
	.use opforge.assembler.modules.exports as exports
	.use opforge.platform.memory as memory
	.pub
OK = 0
UNSUPPORTED = 1
	.section code, kind=code
; A0=retained frontend.Frame,D0=owning module lexical index+1,A1=empty Block.
; D0/CCR=OK/UNSUPPORTED; all other registers preserved. Capture only that
; module's canonical public values and their numeric schema/member closure.
; One transient image owns all copied bytes; caller frees it after installing
; into a private scope. No retained duplicate of the global semantic owner.
; Public undefined LABEL_VALUE entries are external-symbol declarations with
; zero payload and section marker1. This is an import-seeding boundary only,
; never a completed code image or finalized placement/address observation.
; This values-only projection is paired with module_facets.import, which copies
; independent template/statement facets and their closed binary bodies before
; invocation. Never install this projection without those facets; omission could
; silently select a builtin instead. This is not a complete
; module export. Undefined nonlabels and unsupported value closure still reject
; transactionally; final producer publication remains strict.
capture	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #exports.STATE_BYTES, sp
	movea.l sp, a3
	movea.l a0, a4
	movea.l a1, a2
	move.l d0, d7
	movea.l a3, a0
	moveq #exports.STATE_BYTES/4-1, d1
clear
	clr.l (a0)+
	dbra d1, clear
	tst.l d7
	beq.w bad
	movea.l a3, a0
	movea.l a4, a1
	move.l d7, d0
	jsr exports.prepareModuleValues
	bne.w bad
	movea.l frontend.Frame.Scratch(a4), a1
	adda.l #frontend.SCOPE_STATE, a1
	movea.l a3, a0
	jsr exports.publishKnown
	bne.w bad
	moveq #OK, d7
	bra.w release
bad
	moveq #UNSUPPORTED, d7
release
	movea.l a3, a0
	jsr exports.release
	move.l d7, d0
	adda.w #exports.STATE_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; capture
	.endsection
	.endmodule
