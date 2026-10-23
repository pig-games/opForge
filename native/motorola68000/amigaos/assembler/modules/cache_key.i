; Exact pointer-free ambient identity for incremental private module lookup.
; All variable fields carry byte lengths; numeric lexical IDs retain the complete
; spelling/ownership table that gives them meaning. Extra ambient state may miss
; a reusable entry, but cannot admit a differently configured compilation.
	.priv
; No inputs. D0/CCR=status; other registers preserved. Replaces IncrementalKey
; contents, retaining capacity. Caller releases the block on failure/teardown.
incrementalBuildKey	.block
	movem.l d1-d7/a0-a6, -(sp)
	clr.l IncrementalKey+memory.Block.Used
	lea PrivateModuleName, a0
	move.l PrivateModuleBytes, d0
	bsr.w incrementalKeyField
	bne.w done
	lea IncrementalWorkingPath, a0
	bsr.w incrementalKeyString
	bne.w done
	lea IncrementalRootPath, a0
	bsr.w incrementalKeyString
	bne.w done
	lea CliMode, a0
	moveq #2, d0
	bsr.w incrementalKeyAppend
	bne.w done
	lea CliWarningsAsErrors, a0
	moveq #2, d0
	bsr.w incrementalKeyAppend
	bne.w done
	lea CliOutputKind, a0
	moveq #2, d0
	bsr.w incrementalKeyAppend
	bne.w done
	lea OutputStartSet, a0
	moveq #2, d0
	bsr.w incrementalKeyAppend
	bne.w done
	lea OutputStart, a0
	moveq #4, d0
	bsr.w incrementalKeyAppend
	bne.w done
	movea.l PackageSource+loader.Frame.Cpu, a0
	bsr.w incrementalKeyString
	bne.w done
	movea.l PackageSource+loader.Frame.Dialect, a0
	bsr.w incrementalKeyString
	bne.w done
	movea.l PackageSource+loader.Frame.Root, a0
	bsr.w incrementalKeyString
	bne.w done
	movea.l CliModuleRoots, a5
	move.l CliModuleCount, d7
	move.l d7, d0
	bsr.w incrementalKeyNumber
	bne.w done
moduleRoot
	tst.l d7
	beq.w moduleReady
	movea.l a5, a0
	bsr.w incrementalKeyString
	bne.w done
	adda.w #PATH_BYTES, a5
	subq.l #1, d7
	bra.w moduleRoot
moduleReady
	movea.l CliIncludeRoots, a5
	move.l CliIncludeCount, d7
	move.l d7, d0
	bsr.w incrementalKeyNumber
	bne.w done
includeRoot
	tst.l d7
	beq.w includeReady
	movea.l a5, a0
	bsr.w incrementalKeyString
	bne.w done
	adda.w #PATH_BYTES, a5
	subq.l #1, d7
	bra.w includeRoot
includeReady
	; Closed templates retain numeric source origins. Their meaning includes the
	; complete current origin-to-path table, not just module entity identities.
	move.l OriginPaths+memory.Block.Used, d7
	move.l d7, d0
	bsr.w incrementalKeyNumber
	bne.w done
	movea.l OriginPaths+memory.Block.Pointer, a5
origin
	tst.l d7
	beq.w originsReady
	movea.l a5, a0
	bsr.w incrementalKeyString
	bne.w done
	adda.w #PATH_BYTES, a5
	subi.l #PATH_BYTES, d7
	bra.w origin
originsReady
	movea.l PrivateConfigScope, a6
	move.l a6, d0
	beq.w bad
	moveq #0, d0
	move.w layout.IMPORT_STATE+imports.PARAM_COUNT(a6), d0
	cmpi.l #package.PARAMETER_LIMIT, d0
	bhi.w bad
	mulu.w #imports.PARAM_BYTES, d0
	lea layout.IMPORT_STATE+imports.PARAMS(a6), a0
	bsr.w incrementalKeyField
	bne.w done
	lea layout.IMPORT_STATE+imports.PARAM_OWNER+values.Owner.Arena(a6), a1
	move.l memory.Block.Used(a1), d0
	movea.l memory.Block.Pointer(a1), a0
	bsr.w incrementalKeyField
	bne.w done
	lea scopes.SECTION_STATE(a6), a0
	move.l #sectionPrepare.SCRATCH_BYTES, d0
	bsr.w incrementalKeyField
	bne.w done
	lea layout.LEXICAL_STATE(a6), a6
	move.l lexical.State.Count(a6), d7
	move.l d7, d0
	bsr.w incrementalKeyNumber
	bne.w done
	moveq #0, d6
entry
	cmp.l d7, d6
	bhs.w good
	movea.l a6, a0
	lea IncrementalKeyEntry, a1
	move.l d6, d0
	jsr lexical.readEntry
	bne.w done
	; Name is an opaque storage handle; serialize its bytes separately.
	lea IncrementalKeyEntry+records.Entry.Length, a0
	moveq #records.ENTRY_BYTES-records.Entry.Length, d0
	bsr.w incrementalKeyAppend
	bne.w done
	movea.l a6, a0
	lea IncrementalKeyName, a1
	move.l IncrementalKeyEntry+records.Entry.Name, d0
	moveq #0, d1
	move.w IncrementalKeyEntry+records.Entry.Length, d1
	jsr lexical.copyName
	bne.w done
	lea IncrementalKeyName, a0
	move.l d1, d0
	bsr.w incrementalKeyField
	bne.w done
	addq.l #1, d6
	bra.w entry
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; incrementalBuildKey
	.priv

; A0=optional NUL string. D0/CCR=status; other registers preserved.
; Null settings encode an empty field; strings include their terminator.
; Bound scanning to the existing CLI path capacity.
incrementalKeyString	.block
	movem.l d1/a0-a1, -(sp)
	moveq #0, d0
	move.l a0, d1
	beq.w append
	movea.l a0, a1
scan
	cmpi.l #PATH_BYTES, d0
	bhs.w bad
	addq.l #1, d0
	tst.b (a1)+
	bne.w scan
append
	bsr.w incrementalKeyField
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; incrementalKeyString

; D0=number. Append one native big-endian long; other registers preserved.
; D0/CCR=status. Stack bytes are copied before return.
incrementalKeyNumber	.block
	move.l a0, -(sp)
	move.l d0, -(sp)
	movea.l sp, a0
	moveq #4, d0
	bsr.w incrementalKeyAppend
	addq.l #4, sp
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; incrementalKeyNumber

; A0=bytes,D0=length. Append length plus contents. D0/CCR=status;
; other registers preserved. Empty fields do not dereference A0.
incrementalKeyField	.block
	move.l d1, -(sp)
	move.l d0, d1
	bsr.w incrementalKeyNumber
	bne.w done
	move.l d1, d0
	bsr.w incrementalKeyAppend
done
	move.l (sp)+, d1
	tst.l d0
	rts
	.bend  ; incrementalKeyField

; A0=bytes,D0=length. Append raw bytes to IncrementalKey, bounded by memory.LIMIT.
; D0/CCR=status; all other registers preserved. Never serializes an address.
incrementalKeyAppend	.block
	movem.l d1-d3/a0-a2, -(sp)
	movea.l a0, a2
	move.l d0, d2
	beq.w good
	move.l a2, d0
	beq.w bad
	move.l IncrementalKey+memory.Block.Used, d3
	move.l d3, d0
	add.l d2, d0
	bcs.w bad
	lea IncrementalKey, a0
	jsr memory.reserve
	bne.w done
	movea.l IncrementalKey+memory.Block.Pointer, a1
	adda.l d3, a1
copy
	move.b (a2)+, (a1)+
	subq.l #1, d2
	bne.w copy
	; Reserve returns status, so derive the committed extent from the end pointer.
	suba.l IncrementalKey+memory.Block.Pointer, a1
	move.l a1, IncrementalKey+memory.Block.Used
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; incrementalKeyAppend
	.endsection
	.section bss, kind=bss
	.align 4
	.priv
IncrementalKey	.res byte, memory.Block.Used+4
	.priv
IncrementalKeyEntry	.res byte, records.ENTRY_BYTES
IncrementalKeyName	.res byte, records.NAME_BYTES
	.endsection
	.section code, kind=code
