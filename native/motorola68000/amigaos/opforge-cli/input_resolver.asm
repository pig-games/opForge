; Native AmigaOS directory-input resolver.
;
; Rust accepts a directory input when it contains exactly one main.asm or
; main.inc root module.  AmigaDOS paths have no host-side Path metadata, so the
; native CLI performs the equivalent Lock/Examine/ExNext classification before
; opening the root source file.
; @opforge-owner: opforge.cli.input_resolver
; @opforge-slice: documentation/plans/slices/native-porting-slice-self-host-gen1-v1.toml

	.module opforge.cli.input_resolver
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.dos
	.use opforge.cli.path
	.use opforge.cli.state

	.section code, kind=code
	.pub

; Resolve a file-or-directory CLI input to the concrete root source file.
; Directory inputs accept one case-insensitive main.asm or main.inc entry and
; reject zero or multiple matches, matching Rust's folder-input boundary.
; Outputs: D0 = 0 on success, 1 on missing/ambiguous/I/O/capacity failure.
resolveInputPathV1	.block
	movem.l d1-d7/a0-a5, -(sp)
	lea state.NativeCliInputPath, a0
	jsr dos.lockRead
	tst.l d0
	beq.w fail
	movea.l d0, a4
	lea InputResolverFib.l, a0
	move.l a4, d1
	jsr dos.examine
	tst.l d0
	beq.w unlockFail
	lea InputResolverFib.l, a0
	tst.l constants.FIB_DIR_ENTRY_TYPE(a0)
	bgt.s directory
	move.l a4, d1
	jsr dos.unlock
	moveq #0, d0
	bra.w return

directory
	clr.w InputResolverMatchCount.l
	clr.b InputResolverRootName.l

entryLoop
	lea InputResolverFib.l, a0
	move.l a4, d1
	jsr dos.exNext
	tst.l d0
	beq.s entriesDone
	lea InputResolverFib.l, a0
	tst.l constants.FIB_DIR_ENTRY_TYPE(a0)
	bgt.s entryLoop
	lea constants.FIB_FILE_NAME(a0), a0
	lea InputResolverMainAsm.l, a1
	bsr.w foldedNameEquals
	tst.l d0
	bne.s matched
	lea InputResolverFib.l, a0
	lea constants.FIB_FILE_NAME(a0), a0
	lea InputResolverMainInc.l, a1
	bsr.w foldedNameEquals
	tst.l d0
	beq.s entryLoop

matched
	tst.w InputResolverMatchCount.l
	bne.s unlockFail
	lea InputResolverFib.l, a0
	lea constants.FIB_FILE_NAME(a0), a0
	lea InputResolverRootName.l, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s unlockFail
	move.w #1, InputResolverMatchCount.l
	bra.s entryLoop

entriesDone
	jsr dos.ioErr
	cmpi.l #constants.ERROR_NO_MORE_ENTRIES, d0
	bne.s unlockFail
	move.l a4, d1
	jsr dos.unlock
	cmpi.w #1, InputResolverMatchCount.l
	bne.s fail
	lea state.NativeCliInputPath, a0
	lea state.NativeCliModulePathTable, a1
	jsr path.opforgeNativeCliCopyPathBuffer
	bne.s fail
	lea InputResolverRootName.l, a0
	lea state.NativeCliInputPath, a1
	jsr path.opforgeNativeCliAppendPathSegmentBuffer
	bra.s return

unlockFail
	move.l a4, d1
	jsr dos.unlock

fail
	moveq #1, d0

return
	movem.l (sp)+, d1-d7/a0-a5
	rts
	.bend  ; resolveInputPathV1

	.priv

; Case-insensitive equality for AmigaDOS directory-entry names.
; Inputs: A0/A1 = NUL strings. Outputs: D0 = boolean.
foldedNameEquals	.block
	moveq #0, d0

compareLoop
	move.b (a0)+, d1
	move.b (a1)+, d2
	cmpi.b #'A', d1
	blo.s foldRight
	cmpi.b #'Z', d1
	bhi.s foldRight
	ori.b #$20, d1
foldRight
	cmpi.b #'A', d2
	blo.s compare
	cmpi.b #'Z', d2
	bhi.s compare
	ori.b #$20, d2
compare
	cmp.b d2, d1
	bne.s notEqual
	tst.b d1
	bne.s compareLoop
	moveq #1, d0
notEqual
	rts
	.bend  ; foldedNameEquals
	.endsection

	.section data, kind=data
InputResolverMainAsm
	.byte "main.asm", 0
InputResolverMainInc
	.byte "main.inc", 0
	.endsection

	.section bss, kind=bss
	.align 4
InputResolverFib
	.res byte, constants.FILE_INFO_BLOCK_SIZE
InputResolverRootName
	.res byte, constants.PATH_BUFFER_CAPACITY
InputResolverMatchCount
	.res word, 1

	.endsection
	.endmodule
