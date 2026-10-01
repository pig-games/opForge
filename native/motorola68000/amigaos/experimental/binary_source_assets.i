; Application glue for PRVM-selected binary file inclusion. Reuse the textual
; include resolver's current-file/root search and authorization; keep asset I/O
; and packed data materialization in binary_file_data.
ORIGIN_LIMIT = memory.LIMIT/PATH_BYTES-1

; A0=frontend session,A1=PRVM file plan. The path view is preparation-only.
; D0/CCR=status; preserves other registers. Always closes its asset handle.
includeBinary .block
	movem.l d1-d7/a0-a6, -(sp)
	suba.l #filedata.FRAME_BYTES, sp
	movea.l sp, a4
	movea.l a0, a5
	move.l DosBase, filedata.Frame.Dos(a4)
	clr.l filedata.Frame.Handle(a4)
	move.l frontend.Frame.Output(a5), filedata.Frame.Record(a4)
	move.l parser_abi.PRVM_FILE_PREFIX_BYTES(a1), filedata.Frame.PrefixBytes(a4)
	movea.l frontend.Frame.Package(a5), a2
	move.w package.Header.ByteDirective(a2), filedata.Frame.ByteName(a4)
	clr.w filedata.Frame.Reserved(a4)
	move.l #appendBinaryData, filedata.Frame.Callback(a4)
	move.l a5, filedata.Frame.Context(a4)
	move.l parser_abi.PRVM_FILE_PATH_BYTES(a1), d0
	beq.w bad
	cmpi.l #PATH_BYTES-1, d0
	bhi.w bad
	movea.l frontend.Frame.Output(a5), a0
	adda.l parser_abi.PRVM_FILE_PATH_OFFSET(a1), a0
	lea IncludeName, a1
path
	move.b (a0)+, d1
	cmpi.b #32, d1
	blo.w bad
	cmpi.b #126, d1
	bhi.w bad
	cmpi.b #':', d1
	beq.w bad  ; relative asset paths; explicit search roots own volumes
	cmpi.b #'\\', d1
	beq.w bad
	move.b d1, (a1)+
	subq.l #1, d0
	bne.w path
	clr.b (a1)
	move.l frontend.Frame.Origin(a5), d0
	beq.w bad
	lsl.l #8, d0
	lea OriginPaths, a0
	cmp.l memory.Block.Used(a0), d0
	bhs.w bad
	movea.l memory.Block.Pointer(a0), a0
	adda.l d0, a0
	movea.l a0, a2  ; preparation-only definition path, preserved across root attempts
	lea IncludePath, a1
	bsr.w parentPath
	bne.w bad
	bsr.w openBinaryPath
	beq.w stream
	bmi.w bad
	moveq #0, d5
root
	cmp.l RootCount, d5
	bhs.w bad
	move.l d5, d0
	lsl.l #8, d0
	lea RootPaths, a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l d0, a0
	lea IncludePath, a1
	bsr.w copyIncludeBase
	bne.w bad
	bsr.w openBinaryPath
	beq.w stream
	bmi.w bad
	addq.l #1, d5
	bra.w root
stream
	.MEMORY_STAGE #1
	movea.l a4, a0
	jsr filedata.stream
	.MEMORY_STAGE #3
	move.l d0, d7
	move.l filedata.Frame.Handle(a4), d1
	movea.l DosBase, a6
	jsr DOS_CLOSE(a6)
	tst.l d0
	beq.w bad
	move.l d7, d0
	bra.w done
bad
	moveq #1, d0
done
	adda.l #filedata.FRAME_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; includeBinary

; IncludePath is a directory prefix, IncludeName a relative decoded asset name.
; A2=definition source path,A4=filedata frame. D0/CCR=0 opened,1 missing/denied,-1 invalid path.
; Other registers preserved. The caller owns the returned handle.
openBinaryPath .block
	movem.l d1-d2/a6, -(sp)
	bsr.w resolveIncludeFrom
	bne.w done
	movea.l DosBase, a6
	move.l #IncludePath, d1
	move.l #1005, d2
	jsr DOS_OPEN(a6)
	tst.l d0
	beq.w missing
	move.l d0, filedata.Frame.Handle(a4)
	moveq #0, d0
	bra.w done
missing
	moveq #1, d0
done
	movem.l (sp)+, d1-d2/a6
	tst.l d0
	rts
	.bend ; openBinaryPath

; A0=packed data record,D0=bytes,A1=frontend session; D0/CCR=status.
; Preserve other registers as required by the streaming callback contract.
appendBinaryData .block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a2
	movea.l a1, a0
	movea.l a2, a1
	jsr frontend.data
	bne.w done
	bsr.w appendPrepared
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; appendBinaryData

; Retain the current physical source path under its opaque OriginId.
; D0/CCR=status; other registers preserved. Storage holds bytes, never pointers.
; The bounded registry supports IDs 1..4095; larger IDs fail preparation.
retainOriginPath .block
	movem.l d1-d3/a0-a2, -(sp)
	move.l OriginId, d2
	beq.w bad
	cmpi.l #ORIGIN_LIMIT, d2
	bhi.w bad
	lsl.l #8, d2
	move.l d2, d3
	addi.l #PATH_BYTES, d3
	lea OriginPaths, a0
	move.l d3, d0
	jsr memory.reserve
	bne.w done
	cmp.l memory.Block.Used(a0), d3
	bls.w retained
	move.l d3, memory.Block.Used(a0)
retained
	movea.l memory.Block.Pointer(a0), a1
	adda.l d2, a1
	lea SourcePath, a0
	move.w #PATH_BYTES/4-1, d1
copy
	move.l (a0)+, (a1)+
	dbra d1, copy
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend ; retainOriginPath
