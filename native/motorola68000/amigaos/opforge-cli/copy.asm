; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.copy
	.cpu 68020
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	.use debug.amigaos.platform_profile as platform_profile
.endif

	.section code, kind=code
	.pub

; Copy D0 bytes from A1 to A2.
copyBytes	.block
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	move.l d0, -(sp)
	jsr platform_profile.opforgePlatformProfileCopyRequestedV1
.endif
	tst.l d0
	beq.s done

loop
	move.b (a1)+, (a2)+
	subq.l #1, d0
	bne.s loop

done
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l 6(sp), d0
	jsr platform_profile.opforgePlatformProfileCopyCompletedV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
	lea 4(sp), sp
.endif
	rts
	.bend  ; copyBytes

; Copy a C string from A0 to A1 and return copied byte count, including NUL.
copyCString	.block
	moveq #0, d0

loop
	move.b (a0)+, d1
	move.b d1, (a1)+
	addq.w #1, d0
	tst.b d1
	bne.s loop
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	jsr platform_profile.opforgePlatformProfileRecordCopyV1
.endif
	rts
	.bend  ; copyCString

; Copy exactly D0 bytes from A0 to A1.
copyFixedString	.block
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	move.l d0, -(sp)
	andi.l #$ffff, d0
	jsr platform_profile.opforgePlatformProfileCopyRequestedV1
	move.l (sp)+, d0
.endif
	move.w d0, d2
	beq.s done

loop
	move.b (a0)+, (a1)+
	subq.w #1, d2
	bne.s loop

done
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	move.w ccr, -(sp)
	move.l d0, -(sp)
	andi.l #$ffff, d0
	jsr platform_profile.opforgePlatformProfileCopyCompletedV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
.endif
	rts
	.bend  ; copyFixedString

; Clear D0 bytes at A0.
clearBytes	.block
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	move.l d0, -(sp)
	jsr platform_profile.opforgePlatformProfileClearRequestedV1
.endif
	tst.l d0
	beq.s done
	moveq #0, d1

loop
	move.b d1, (a0)+
	subq.l #1, d0
	bne.s loop

done
.ifdef OPFORGE_PROGRESS_PLATFORM_COUNTERS
	move.w ccr, -(sp)
	move.l d0, -(sp)
	move.l 6(sp), d0
	jsr platform_profile.opforgePlatformProfileClearCompletedV1
	move.l (sp)+, d0
	move.w (sp)+, ccr
	lea 4(sp), sp
.endif
	rts
	.bend  ; clearBytes

	.endsection
	.endmodule
