; One persistent module image serves incremental reuse and final linking.
; Capture/preparation scratch remains separate and transient. Single writer.
CACHE_DOS_IOERR = -132
CACHE_OBJECT_NOT_FOUND = 205

; A0=zero registry,A1=DOS,A2=capture parent. D0/CCR=status; others preserved.
; Normal CLI uses its exact incremental directory. Reference/component requests
; without persistent caching use one stable objects child of the capture parent.
configureModuleObjects .block
	movem.l d1-d7/a0-a6, -(sp)
	lea -PATH_BYTES(sp), sp
	tst.l IncrementalEnabled
	beq.w scratch
	movea.l IncrementalDirectory, a2
	bra.w configure
scratch
	movea.l a0, a4
	movea.l a1, a5
	movea.l sp, a1
	move.w #PATH_BYTES-1, d0
copyParent
	move.b (a2)+, (a1)+
	beq.w copied
	dbra d0, copyParent
	bra.w bad
copied
	lea ObjectDirectoryName, a0
	movea.l sp, a1
	bsr.w incrementalJoin
	bne.w bad
	movea.l a4, a0
	movea.l a5, a1
	movea.l sp, a2
configure
	jsr moduleRegistry.configure
	bne.w done
	move.l IncrementalEnabled, SharedModuleStore
	bra.w done
bad
	moveq #1, d0
done
	lea PATH_BYTES(sp), sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; configureModuleObjects

; A0=registry,D1=module. D0/CCR=status; others preserved.
; Hits and successfully saved misses already own the exact named .ofm in this
; directory. Adopt without IO; otherwise invalidate old metadata before writing.
storeModuleObject .block
	movem.l d1-d7/a0-a6, -(sp)
	tst.l SharedModuleStore
	beq.w store
	tst.l IncrementalEnabled
	beq.w invalidate
	tst.l IncrementalHit
	bne.w adopt
	tst.l IncrementalSaved
	bne.w adopt
invalidate
	; Source recording can disable reuse after the directory was configured.
	; Recompute from the admitted module ID, never a previous attempt's path.
	bsr.w incrementalModulePaths
	bne.w done
	bsr.w invalidateModuleCache
	bne.w done
store
	jsr moduleRegistry.store
	bra.w done
adopt
	lea IncrementalFileName, a2
	jsr moduleRegistry.adoptStored
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; storeModuleObject

; Remove the validity marker before replacing its .ofm. Missing is harmless;
; permission/device errors must not leave a stale marker beside a changed image.
; D0/CCR=status; other registers preserved. No file image is touched here.
invalidateModuleCache .block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l DosBase, a6
	move.l #IncrementalMetaPath, d1
	jsr CACHE_DOS_DELETE(a6)
	tst.l d0
	bne.w good
	jsr CACHE_DOS_IOERR(a6)
	cmpi.l #CACHE_OBJECT_NOT_FOUND, d0
	bne.w bad
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; invalidateModuleCache
	.endsection
	.section bss, kind=bss
SharedModuleStore .res byte, 4
	.endsection
	.section data, kind=data
ObjectDirectoryName .byte "objects", 0
	.endsection
	.section code, kind=code
