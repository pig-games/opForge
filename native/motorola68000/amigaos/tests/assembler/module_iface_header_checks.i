; Retained header reads require no DOS context; reader validation still owns
; its bounded lease and invalidates it when a later selection fails.
	.priv
	.section code, kind=code
headerChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #FAIL_IMAGE, ReturnCode
	lea HeaderImage, a0
	move.l #HeaderImageEnd-HeaderImage, d0
	jsr image.validate
	bne.w bad
	; These descriptors borrow static bytes and are never released as owners.
	lea HeaderRegistry, a0
	move.l #HeaderModule, registry.State.Modules+memory.Block.Pointer(a0)
	move.l #registry.MODULE_BYTES, registry.State.Modules+memory.Block.Capacity(a0)
	move.l #registry.MODULE_BYTES, registry.State.Modules+memory.Block.Used(a0)
	lea HeaderModule, a4
	move.l #HeaderImage, registry.Module.Metadata+memory.Block.Pointer(a4)
	move.l #image.HEADER_BYTES, registry.Module.Metadata+memory.Block.Capacity(a4)
	move.l #image.HEADER_BYTES, registry.Module.Metadata+memory.Block.Used(a4)
	move.l #HeaderImageEnd-HeaderImage, registry.Module.ImageBytes(a4)
	move.l #1, registry.Module.Stored(a4)
	lea HeaderOutput, a1
	moveq #0, d1
	moveq #0, d2
	moveq #image.HEADER_BYTES, d3
	jsr registry.readRange
	bne.w bad
	bsr.w headerOutputMatches
	bne.w bad
	; The final byte is covered, but extending past the retained prefix must
	; use disk storage, which is deliberately unavailable in this fixture.
	moveq #image.HEADER_BYTES-1, d2
	moveq #2, d3
	jsr registry.readRange
	cmpi.l #registry.INVALID, d0
	bne.w bad
	bsr.w headerOutputMatches
	bne.w bad
	moveq #-1, d2
	jsr registry.readRange
	cmpi.l #registry.INVALID, d0
	bne.w bad
	bsr.w headerOutputMatches
	bne.w bad
	; Pending full-image ownership wins even when no metadata pointer exists.
	clr.l registry.Module.Metadata+memory.Block.Pointer(a4)
	move.l #HeaderImage, registry.Module.Image+memory.Block.Pointer(a4)
	move.l #HeaderImageEnd-HeaderImage, registry.Module.Image+memory.Block.Capacity(a4)
	move.l #HeaderImageEnd-HeaderImage, registry.Module.Image+memory.Block.Used(a4)
	moveq #0, d2
	moveq #image.HEADER_BYTES, d3
	jsr registry.readRange
	bne.w bad
	bsr.w headerOutputMatches
	bne.w bad
	clr.l registry.Module.Image+memory.Block.Pointer(a4)
	jsr registry.readRange
	cmpi.l #registry.INVALID, d0
	bne.w bad
	bsr.w headerOutputMatches
	bne.w bad
	; A complete resident image supplies the same bounded read/select path.
	move.l #HeaderImage, registry.Module.Image+memory.Block.Pointer(a4)
	movea.l a0, a1
	lea HeaderReader, a0
	jsr storedInterface.select
	bne.w bad
	cmpi.l #HeaderImageEnd-HeaderInterface, storedInterface.State.Bytes(a0)
	bne.w bad
	cmpi.l #HeaderImageEnd-HeaderInterface, storedInterface.State.Window+memory.Block.Used(a0)
	bne.w bad
	tst.l storedInterface.State.Start(a0)
	bne.w bad
	movea.l storedInterface.State.Window+memory.Block.Pointer(a0), a5
	cmpi.l #interface.MAGIC, interface.Header.Magic(a5)
	bne.w bad
	jsr storedInterface.select
	bne.w bad
	cmpa.l storedInterface.State.Window+memory.Block.Pointer(a0), a5
	bne.w bad
	; Invalid OFMA and OFMS headers must both clear the provisional selection.
	moveq #1, d1
	jsr storedInterface.select
	cmpi.l #storedInterface.INVALID, d0
	bne.w bad
	bsr.w headerReaderInvalid
	bne.w bad
	moveq #0, d1
	clr.l HeaderInterface+interface.Header.Magic
	jsr storedInterface.select
	move.l #interface.MAGIC, HeaderInterface+interface.Header.Magic
	cmpi.l #storedInterface.INVALID, d0
	bne.w bad
	bsr.w headerReaderInvalid
	bne.w bad
	clr.l HeaderValues+semantics.Header.Magic
	jsr storedInterface.select
	move.l #semantics.MAGIC, HeaderValues+semantics.Header.Magic
	cmpi.l #storedInterface.INVALID, d0
	bne.w bad
	bsr.w headerReaderInvalid
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l d0, -(sp)
	lea HeaderReader, a0
	jsr storedInterface.release
	move.l (sp)+, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; headerChecks

; Compare the complete copied header and output sentinels. D0/CCR=status;
; other registers preserved, so another registry read retains its arguments.
headerOutputMatches	.block
	movem.l d1/a1-a2, -(sp)
	cmpi.l #GUARD_VALUE, HeaderBefore
	bne.w bad
	cmpi.l #GUARD_VALUE, HeaderAfter
	bne.w bad
	lea HeaderOutput, a1
	lea HeaderImage, a2
	moveq #image.HEADER_BYTES/4-1, d1
next
	move.l (a2)+, d0
	cmp.l (a1)+, d0
	bne.w bad
	dbra d1, next
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a1-a2
	tst.l d0
	rts
	.bend  ; headerOutputMatches

; A0=reader. A failed selection clears both validity and readable window;
; a subsequent read rejects with a null lease. D0/CCR=status; others preserved.
headerReaderInvalid	.block
	movem.l d2-d3/a1, -(sp)
	tst.l storedInterface.State.Bytes(a0)
	bne.w bad
	tst.l storedInterface.State.Window+memory.Block.Used(a0)
	bne.w bad
	moveq #0, d2
	moveq #4, d3
	jsr storedInterface.read
	cmpi.l #storedInterface.INVALID, d0
	bne.w bad
	move.l a1, d0
	bne.w bad
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d3/a1
	tst.l d0
	rts
	.bend  ; headerReaderInvalid
	.endsection
	.section data, kind=data
	.align 4
; Canonical empty OFMI: no blocks/relocations/payload, padded OFMA with OFMS.
HeaderImage
	.long image.MAGIC, HeaderImageEnd-HeaderImage, 0, image.HEADER_BYTES
	.long 0, image.HEADER_BYTES, image.HEADER_BYTES
	.long HeaderInterface-HeaderImage, HeaderImageEnd-HeaderInterface
	.long 0
HeaderInterface
	.long interface.MAGIC, HeaderImageEnd-HeaderInterface
	.long interface.HEADER_BYTES, semantics.HEADER_BYTES, 0, 0
	.long 0, interface.HEADER_BYTES, 0, interface.HEADER_BYTES
	.long interface.HEADER_BYTES, 0
HeaderValues
	.long semantics.MAGIC, semantics.HEADER_BYTES, 0, semantics.HEADER_BYTES
	.long 0, semantics.HEADER_BYTES, semantics.HEADER_BYTES, 0, semantics.HEADER_BYTES, 0
HeaderImageEnd
HeaderBefore	.long GUARD_VALUE
HeaderOutput	.fill byte, image.HEADER_BYTES, 0
HeaderAfter	.long GUARD_VALUE
	.endsection
	.section bss, kind=bss
	.align 4
HeaderRegistry	.res byte, registry.STATE_BYTES
HeaderModule	.res byte, registry.MODULE_BYTES
HeaderReader	.res byte, storedInterface.STATE_BYTES
	.endsection
