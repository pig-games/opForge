; Native AmigaOS executable Hunk payload writer.

	.module opforge.cli.hunk_output
	.cpu 68020

	.use opasm.amigaos.engine as engine
	.use opasm.amigaos.layout as layout
	.use opasm.amigaos.output_artifacts as artifacts
	.use opforge.cli.constants
	.use opforge.cli.state

HUNK_HEADER = $000003f3
HUNK_CODE = $000003e9
HUNK_DATA = $000003ea
HUNK_BSS = $000003eb
HUNK_END = $000003f2
NATIVE_HUNK_HEADER_BYTES = 36
NATIVE_HUNK_BUFFER_CAPACITY = constants.NATIVE_IMAGE_BUFFER_CAPACITY + NATIVE_HUNK_HEADER_BYTES + 3

	.section code, kind=code
	.pub

; Build Rust's relocation-free single implicit CODE-segment Hunk payload.
; Header/table words and payload lengths are big-endian because the native
; 68020 stores longwords in the target byte order. The source image is padded
; with zero bytes to its next longword boundary.
; @opforge-owner: opforge.cli.hunk_output
; @opforge-slice: documentation/plans/slices/native-porting-slice-hunk-segment-surface-v1.toml
; @opforge-role: implementation
; Outputs: D0.L = 0 success/1 failure; A0 = payload; D1.L = byte count.
; Clobbers: D0-D1/A0/CCR. Preserves D2-D7/A1-A3.
buildFlatCodeV1	.block
	movem.l d2-d7/a1-a3, -(sp)
	jsr engine.opasmEngineGetImageByteCountV1
	move.l d0, d7
	beq.w fail
	move.l d7, d6
	addq.l #3, d6
	lsr.l #2, d6
	move.l d6, d5
	lsl.l #2, d5
	move.l d5, d4
	addi.l #NATIVE_HUNK_HEADER_BYTES, d4
	cmpi.l #NATIVE_HUNK_BUFFER_CAPACITY, d4
	bhi.w fail

	lea NativeHunkOutputBuffer.l, a2
	move.l #HUNK_HEADER, (a2)+
	clr.l (a2)+
	move.l #1, (a2)+
	clr.l (a2)+
	clr.l (a2)+
	move.l d6, (a2)+
	move.l #HUNK_CODE, (a2)+
	move.l d6, (a2)+
	jsr engine.opasmEngineGetImageBufferPtrV1
	move.l d7, d3
copyImage
	tst.l d3
	beq.s padImage
	move.b (a0)+, (a2)+
	subq.l #1, d3
	bra.s copyImage
padImage
	move.l d5, d3
	sub.l d7, d3
padLoop
	tst.l d3
	beq.s finish
	clr.b (a2)+
	subq.l #1, d3
	bra.s padLoop
finish
	move.l #HUNK_END, (a2)+
	lea NativeHunkOutputBuffer.l, a0
	move.l d4, d1
	moveq #0, d0
	bra.s return
fail
	suba.l a0, a0
	moveq #0, d1
	moveq #1, d0
return
	movem.l (sp)+, d2-d7/a1-a3
	tst.l d0
	rts
	.bend  ; buildFlatCodeV1

; Build Rust's relocation-free Hunk surface for the source-declared section
; list. The list order is authoritative; empty initialized sections are
; omitted, while BSS allocation size is retained without payload bytes.
; @opforge-owner: opforge.cli.hunk_output
; @opforge-slice: documentation/plans/slices/native-porting-slice-hunk-segment-surface-v1.toml
; @opforge-role: implementation
; Outputs: D0.L = 0 success/1 failure; A0 = payload; D1.L = byte count.
buildSelectedSectionsV1	.block
	movem.l d2-d7/a1-a3, -(sp)
	moveq #0, d6
	moveq #0, d7
countLoop
	cmp.w state.NativeCliSourceOutputSectionCount, d6
	bhs.s countDone
	bsr.w selectedSectionInfoV1
	bne.w failSelected
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_BSS, d2
	beq.s countOne
	tst.l d1
	beq.s countNext
countOne
	tst.w d7
	bne.s countAccept
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_CODE, d2
	bne.w failSelected
countAccept
	addq.w #1, d7
countNext
	addq.w #1, d6
	bra.s countLoop
countDone
	tst.w d7
	beq.w failSelected

	lea NativeHunkOutputBuffer.l, a3
	move.l #HUNK_HEADER, (a3)+
	clr.l (a3)+
	moveq #0, d0
	move.w d7, d0
	move.l d0, (a3)+
	clr.l (a3)+
	subq.l #1, d0
	move.l d0, (a3)+

	moveq #0, d6
allocationLoop
	cmp.w state.NativeCliSourceOutputSectionCount, d6
	bhs.s segmentBegin
	bsr.w selectedSectionInfoV1
	bne.w failSelected
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_BSS, d2
	beq.s allocationOne
	tst.l d1
	beq.s allocationNext
allocationOne
	move.l d1, d4
	addq.l #3, d4
	lsr.l #2, d4
	jsr layout.getSectionMemoryTypeV1
	cmpi.l #layout.OPASM_LAYOUT_HUNK_MEMORY_CHIP, d0
	bne.s allocationMaybeFast
	ori.l #$40000000, d4
	bra.s allocationWrite
allocationMaybeFast
	cmpi.l #layout.OPASM_LAYOUT_HUNK_MEMORY_FAST, d0
	bne.s allocationWrite
	ori.l #$80000000, d4
allocationWrite
	move.l d4, (a3)+
allocationNext
	addq.w #1, d6
	bra.s allocationLoop

segmentBegin
	moveq #0, d6
segmentLoop
	cmp.w state.NativeCliSourceOutputSectionCount, d6
	bhs.w selectedDone
	bsr.w selectedSectionInfoV1
	bne.w failSelected
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_BSS, d2
	beq.s segmentReady
	tst.l d1
	beq.w segmentNext
segmentReady
	move.l d0, d4
	move.l d1, d7
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_CODE, d2
	beq.s segmentCode
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_DATA, d2
	beq.s segmentData
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_BSS, d2
	bne.w failSelected
	move.l #HUNK_BSS, (a3)+
	bra.s segmentLength
segmentCode
	move.l #HUNK_CODE, (a3)+
	bra.s segmentLength
segmentData
	move.l #HUNK_DATA, (a3)+
segmentLength
	move.l d7, d3
	addq.l #3, d3
	lsr.l #2, d3
	move.l d3, (a3)+
	cmpi.l #layout.OPASM_LAYOUT_SECTION_KIND_BSS, d2
	beq.s segmentEnd
	jsr artifacts.opasmOutputGetSessionOriginV1
	move.l d4, d2
	sub.l d0, d2
	bcs.w failSelected
	move.l d7, d3
	jsr artifacts.opasmOutputGetImageRangeV1
	bne.w failSelected
	move.l d7, d4
segmentCopy
	tst.l d4
	beq.s segmentPadReady
	move.b (a0)+, (a3)+
	subq.l #1, d4
	bra.s segmentCopy
segmentPadReady
	move.l d7, d4
	addq.l #3, d4
	andi.l #$fffffffc, d4
	sub.l d7, d4
segmentPad
	tst.l d4
	beq.s segmentEnd
	clr.b (a3)+
	subq.l #1, d4
	bra.s segmentPad
segmentEnd
	move.l #HUNK_END, (a3)+
segmentNext
	addq.w #1, d6
	bra.w segmentLoop

selectedDone
	lea NativeHunkOutputBuffer.l, a0
	move.l a3, d1
	sub.l a0, d1
	moveq #0, d0
	bra.s selectedReturn
failSelected
	suba.l a0, a0
	moveq #0, d1
	moveq #1, d0
selectedReturn
	movem.l (sp)+, d2-d7/a1-a3
	tst.l d0
	rts
	.bend  ; buildSelectedSectionsV1

; Resolve selected-section slot D6.W through layout-owned state.
; Outputs: D0=base, D1=size, D2=kind, D3=region, D5=section index;
; CCR reports success/failure.
selectedSectionInfoV1	.block
	move.l d6, d0
	lsl.l #5, d0
	lea state.NativeCliSourceOutputSectionNames, a0
	adda.l d0, a0
	movea.l a0, a1
	moveq #0, d0
selectedNameLen
	tst.b (a1)+
	beq.s selectedNameReady
	addq.l #1, d0
	bra.s selectedNameLen
selectedNameReady
	jsr layout.findSectionByNameV1
	bne.s selectedInfoFail
	jsr layout.getSectionInfoV1
	moveq #0, d4
	rts
selectedInfoFail
	moveq #1, d4
	tst.l d4
	rts
	.bend  ; selectedSectionInfoV1

	.endsection

	.section bss, kind=bss

NativeHunkOutputBuffer
	.res byte, NATIVE_HUNK_BUFFER_CAPACITY

	.endsection

	.endmodule
