; Final-cohort requests retain spellings and dependency relationships only.
; Configuration values/maps are rebound into the semantic scope separately.
PrivateRequestName	.struct
Name	.long ?
Bytes	.long ?
.endstruct
PRIVATE_REQUEST_NAME_BYTES = PrivateRequestName.Bytes+4
PrivateRequest	.struct
Name	.long ?
Bytes	.long ?
Start	.long ?
End	.long ?
.endstruct
PRIVATE_REQUEST_BYTES = PrivateRequest.End+4

; ConfigFront and discovery graph are live; GraphSpans is the final order.
; D0/CCR=status; other registers preserved. Unnamed spans retain fallback rows.
buildPrivateRequests	.block
	movem.l d1-d7/a0-a6, -(sp)
	bsr.w releasePrivateRequests
	tst.w ReferenceModules
	bne.w good
	tst.l CaptureDirectory
	beq.w good
	movea.l ConfigFront+frontend.Frame.Scratch, a0
	adda.l #frontend.SCOPE_STATE, a0
	move.l a0, PrivateConfigScope
	move.l OrderedCount, d0
	mulu.w #PRIVATE_REQUEST_BYTES, d0
	lea PrivateRequests, a0
	jsr memory.reserveExact
	bne.w bad
	move.l OrderedCount, d0
	mulu.w #PRIVATE_REQUEST_BYTES, d0
	move.l d0, memory.Block.Used(a0)
	moveq #0, d4
next
	cmp.l OrderedCount, d4
	bhs.w enabled
	move.l d4, d0
	mulu.w #SPAN_BYTES, d0
	movea.l GraphSpans+memory.Block.Pointer, a0
	adda.l d0, a0
	lea PublishedCapture, a1
	moveq #SPAN_BYTES, d0
	bsr.w copy
	move.l d4, d0
	mulu.w #PRIVATE_REQUEST_BYTES, d0
	movea.l PrivateRequests+memory.Block.Pointer, a4
	adda.l d0, a4
	clr.l PrivateRequest.Bytes(a4)
	move.l PrivateRequestRows+memory.Block.Used, PrivateRequest.Start(a4)
	move.l PrivateRequestRows+memory.Block.Used, PrivateRequest.End(a4)
	bsr.w privateFindCapture
	bne.w advance
	lea PrivateModuleName, a0
	move.l PrivateModuleBytes, d1
	bsr.w appendRequestName
	bne.w bad
	move.l d2, PrivateRequest.Name(a4)
	move.l PrivateModuleBytes, PrivateRequest.Bytes(a4)
	movea.l PrivateConfigScope, a5
	move.l PrivateConfigModule, d0
	subq.l #1, d0
	add.l d0, d0
	movea.l scopes.IMPORT_STATE+imports.HEADS_POINTER(a5), a0
	moveq #0, d7
	move.w 0(a0, d0.l), d7
importsNext
	tst.l d7
	beq.w importsDone
	cmp.w scopes.IMPORT_STATE+imports.COUNT(a5), d7
	bhi.w bad
	move.l d7, d0
	subq.l #1, d0
	mulu.w #imports.ITEM_BYTES, d0
	movea.l scopes.IMPORT_STATE+imports.ITEMS_POINTER(a5), a0
	adda.l d0, a0
	moveq #0, d7
	move.w imports.Item.Next(a0), d7
	moveq #0, d0
	move.w imports.Item.Target(a0), d0
	lea layout.LEXICAL_STATE(a5), a0
	lea PrivateEntry, a1
	jsr lexical.readEntry
	bne.w bad
	moveq #0, d1
	move.w records.Entry.Length(a1), d1
	move.l d1, PrivateDependencyBytes
	move.l records.Entry.Name(a1), d0
	lea PrivateDependencyName, a1
	jsr lexical.copyName
	bne.w bad
	lea PrivateDependencyName, a0
	move.l PrivateDependencyBytes, d1
	bsr.w appendRequestName
	bne.w bad
	move.l d2, d6
	lea PrivateRequestRows, a0
	move.l memory.Block.Used(a0), d0
	addq.l #PRIVATE_REQUEST_NAME_BYTES, d0
	jsr memory.reserve
	bne.w bad
	movea.l memory.Block.Pointer(a0), a1
	adda.l memory.Block.Used(a0), a1
	move.l d6, PrivateRequestName.Name(a1)
	move.l PrivateDependencyBytes, PrivateRequestName.Bytes(a1)
	addq.l #PRIVATE_REQUEST_NAME_BYTES, memory.Block.Used(a0)
	bra.w importsNext
importsDone
	move.l PrivateRequestRows+memory.Block.Used, PrivateRequest.End(a4)
advance
	addq.l #1, d4
	bra.w next
enabled
	move.l #1, PrivateRequestMode
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; buildPrivateRequests

; A0/D1=spelling/length; D2=owned pool offset. Other registers preserved.
; D0/CCR=status. Names are bounded by the same public module-name limit.
appendRequestName	.block
	movem.l d1/d3/a0-a3, -(sp)
	movea.l a0, a3
	move.l d1, d3
	cmpi.l #PATH_BYTES, d3
	bhi.w bad
	lea PrivateRequestNames, a2
	move.l memory.Block.Used(a2), d2
	move.l d2, d0
	add.l d3, d0
	bcs.w bad
	movea.l a2, a0
	jsr memory.reserve
	bne.w bad
	movea.l a3, a0
	movea.l memory.Block.Pointer(a2), a1
	adda.l d2, a1
	move.l d3, d0
	bsr.w copy
	add.l d3, memory.Block.Used(a2)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/d3/a0-a3
	tst.l d0
	rts
	.bend  ; appendRequestName

; CaptureOrderIndex selects an immutable final-cohort row. D0/CCR=status;
; other registers preserved. A zero module length requests ordinary replay.
privateSelectRequest	.block
	movem.l d1/a0-a1, -(sp)
	move.l CaptureOrderIndex, d0
	mulu.w #PRIVATE_REQUEST_BYTES, d0
	movea.l PrivateRequests+memory.Block.Pointer, a0
	adda.l d0, a0
	move.l a0, PrivateRequestCurrent
	move.l PrivateRequest.Bytes(a0), d0
	beq.w bad
	move.l d0, PrivateModuleBytes
	move.l PrivateRequest.Name(a0), d1
	movea.l PrivateRequestNames+memory.Block.Pointer, a0
	adda.l d1, a0
	lea PrivateModuleName, a1
	bsr.w copy
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1/a0-a1
	tst.l d0
	rts
	.bend  ; privateSelectRequest

; Idempotent final-cohort ownership cleanup. Registers preserved; CCR unspecified.
releasePrivateRequests	.block
	movem.l d0/a0, -(sp)
	lea PrivateRequests, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateRequestRows, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea PrivateRequestNames, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l PrivateRequestMode
	clr.l PrivateRequestCurrent
	clr.l PrivateRequestEnd
	movem.l (sp)+, d0/a0
	rts
	.bend  ; releasePrivateRequests
