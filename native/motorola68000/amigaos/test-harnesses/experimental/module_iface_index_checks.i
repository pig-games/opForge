; Public spelling order differs from stable entity-ID order. The same producer
; bytes pass semantic validation and the bounded stored-interface reader.
	.priv
	.section code, kind=code
indexChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #FAIL_IMAGE, ReturnCode
	lea IndexBuild, a0
	jsr semantics.build
	bne.w bad
	movea.l IndexOwner+memory.Block.Pointer, a0
	move.l IndexOwner+memory.Block.Used, d0
	jsr semantics.validate
	bne.w bad
	cmpi.l #5, semantics.Header.IndexCount(a0)
	bne.w bad
	; Both permutations contain public row ordinals exactly once. Private row
	; four is omitted. Full spelling and leaf spelling deliberately differ.
	movea.l a0, a2
	adda.l semantics.Header.Index(a0), a2
	lea IndexExpected, a3
	moveq #9, d7
order
	move.l (a3)+, d0
	cmp.l (a2)+, d0
	bne.w bad
	dbra d7, order
	; Reject an ordinal beyond the export table, then a duplicate public row,
	; then a private row replacing a required public row, and swapped order.
	movea.l a0, a2
	adda.l semantics.Header.Index(a0), a2
	move.l (a2), d6
	move.l #INDEX_EXPORT_COUNT, (a2)
	bsr.w indexReject
	bne.w bad
	move.l 4(a2), (a2)
	bsr.w indexReject
	bne.w bad
	move.l #4, (a2)
	bsr.w indexReject
	bne.w bad
	move.l d6, (a2)
	move.l 4(a2), d5
	move.l d5, (a2)
	move.l d6, 4(a2)
	bsr.w indexReject
	bne.w bad
	move.l d6, (a2)
	move.l d5, 4(a2)
	move.l IndexOwner+memory.Block.Used, d0
	jsr semantics.validate
	bne.w bad
	; Wrap producer output using the actual OFMA and OFMI writers.
	move.l a0, IndexInterfaceBuild+interface.Frame.Values
	move.l IndexOwner+memory.Block.Used, IndexInterfaceBuild+interface.Frame.ValueBytes
	lea IndexInterfaceBuild, a0
	jsr interface.build
	bne.w bad
	move.l IndexInterfaceBuild+interface.Frame.Used, IndexImageBuild+image.Frame.SemanticsBytes
	lea IndexImageBuild, a0
	jsr image.build
	bne.w bad
	lea IndexRegistry, a1
	move.l #IndexModule, registry.State.Modules+memory.Block.Pointer(a1)
	move.l #registry.MODULE_BYTES, registry.State.Modules+memory.Block.Used(a1)
	move.l #registry.MODULE_BYTES, registry.State.Modules+memory.Block.Capacity(a1)
	move.l #IndexImage, IndexModule+registry.Module.Image+memory.Block.Pointer
	move.l IndexImageBuild+image.Frame.Used, d0
	move.l d0, IndexModule+registry.Module.Image+memory.Block.Used
	move.l #INDEX_IMAGE_CAPACITY, IndexModule+registry.Module.Image+memory.Block.Capacity
	move.l d0, IndexModule+registry.Module.ImageBytes
	lea IndexReader, a0
	moveq #0, d1
	jsr storedInterface.select
	bne.w bad
	; A read spanning the 64 KiB boundary forces a fresh bounded lease.
	move.l #storedInterface.WINDOW_BYTES-2, d2
	moveq #8, d3
	jsr storedInterface.read
	bne.w bad
	lea IndexQueries, a4
	moveq #8, d7
query
	movea.l (a4)+, a1
	move.l (a4)+, d1
	jsr storedInterface.lookupName
	cmp.l (a4)+, d0
	bne.w bad
	tst.l d0
	bne.w queryNext
	cmp.l (a4), d1
	bne.w bad
queryNext
	addq.l #4, a4
	dbra d7, query
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	move.l d0, -(sp)
	lea IndexReader, a0
	jsr storedInterface.release
	lea IndexOwner, a0
	jsr memory.release
	move.l (sp)+, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; indexChecks

; A0=serialized semantic owner, preserve registers. D0/CCR=0 only on rejection.
indexReject	.block
	move.l IndexOwner+memory.Block.Used, d0
	jsr semantics.validate
	cmpi.l #semantics.INVALID, d0
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; indexReject
	.endsection
INDEX_EXPORT_COUNT = 2406
INDEX_INTERFACE_CAPACITY = 70000
INDEX_IMAGE_CAPACITY = 71000
	.section data, kind=data
	.align 4
IndexBuild
	.long 0, IndexRows, INDEX_EXPORT_COUNT, 0, 0, IndexOwner
IndexRows
	.long 1, IndexZeta, 4, 0, 11, 0, 0
	.long 2, IndexAlpha, 8, 0, 22, 0, 0
	.long 3, IndexOther, 11, 0, 33, 0, 0
	.long 4, IndexUnique, 6, 0, 44, 0, 0
	.long 5, 0, 0, 0, 55, 0, 0
	.long 6, IndexTrailing, 6, 0, 66, 0, 0
	; Thousands of nameless closure roots put both indexes and names beyond
	; the first reader window, without changing the five public spellings.
	.for entity in 7..INDEX_EXPORT_COUNT+1
	.long entity, 0, 0, 0, entity, 0, 0
	.endfor
IndexExpected
	.long 1, 2, 5, 3, 0
	.long 5, 1, 2, 3, 0
IndexInterfaceBuild
	.long 0, 0, 0, 0, IndexInterface, INDEX_INTERFACE_CAPACITY, 0, 0, 0, 0, 0
IndexImageBuild
	.long 0, 0, 0, 0, IndexImage, INDEX_IMAGE_CAPACITY, 0, IndexInterface, 0
IndexQueries
	.long IndexFoldedFull, 8, storedInterface.OK, 2
	.long IndexFoldedLeaf, 6, storedInterface.OK, 4
	.long IndexLeaf, 5, storedInterface.INVALID, 0
	.long IndexZeta, 4, storedInterface.OK, 1
	.long IndexMissing, 7, storedInterface.NOT_FOUND, 0
	.long IndexPrivate, 7, storedInterface.NOT_FOUND, 0
	.long IndexMissing, 0, storedInterface.INVALID, 0
	.long IndexMissingDot, 8, storedInterface.NOT_FOUND, 0
	.long IndexTrailing, 6, storedInterface.OK, 6
IndexZeta	.byte "Zeta"
IndexAlpha	.byte "ns.Alpha"
IndexOther	.byte "other.alpha"
IndexUnique	.byte "Unique"
IndexTrailing	.byte "trail."
IndexFoldedFull	.byte "NS.aLPHA"
IndexFoldedLeaf	.byte "uNIQUE"
IndexLeaf	.byte "ALPHA"
IndexMissing	.byte "missing"
IndexMissingDot	.byte "missing."
IndexPrivate	.byte "private"
	.endsection
	.section bss, kind=bss
	.align 4
IndexOwner	.res byte, memory.Block.Used+4
IndexInterface	.res byte, INDEX_INTERFACE_CAPACITY
IndexImage	.res byte, INDEX_IMAGE_CAPACITY
IndexRegistry	.res byte, registry.STATE_BYTES
IndexModule	.res byte, registry.MODULE_BYTES
IndexReader	.res byte, storedInterface.STATE_BYTES
	.endsection
