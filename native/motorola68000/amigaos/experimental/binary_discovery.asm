; Preparation-only AmigaDOS search for candidate module source files.
; The caller handles module identities numerically after tokenization.
; @opforge-owner: experimental.amigaos.binary_discovery
	.module experimental.amigaos.binary_discovery
	.cpu 68020
	.pub
DEPTH_LIMIT = 8
PATH_BYTES = 256
FIB_BYTES = 260
Frame	.struct
Dos	.long ?
Callback	.long ?
Context	.long ?
.endstruct
CANDIDATE = 12
FRAMES = CANDIDATE+PATH_BYTES
ROOT = 0
FIB = PATH_BYTES
FRAME_BYTES = PATH_BYTES+FIB_BYTES
SCRATCH_BYTES = FRAMES+FRAME_BYTES*DEPTH_LIMIT
	.section code, kind=code

; Scan one root recursively. A0=caller-owned aligned scratch, A1=NUL root,
; A2=callback, A3=context, A4=dos.library. Callback receives A0=temporary
; NUL source path,A1=context and returns D0=0 on success. The callback must
; preserve D3-D7/A2-A6. D0/CCR=status, other registers preserved.
scan	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	move.l a2, Frame.Callback(a5)
	move.l a3, Frame.Context(a5)
	move.l a4, Frame.Dos(a5)
	lea 268(a5), a2
	movea.l a1, a0
	bsr.w copyPath
	bne.w bad
	moveq #0, d7
	bsr.w directory
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; scan
	.priv

; D7=depth,A5=base. Each depth owns its path, FileInfoBlock and lock.
; D0/CCR=status, other registers preserved. Depth and path caps are errors.
directory	.block
	movem.l d1-d7/a0-a6, -(sp)
	cmpi.w #DEPTH_LIMIT, d7
	bhs.w fail
	move.l d7, d0
	mulu.w #FRAME_BYTES, d0
	lea 268(a5), a4
	adda.l d0, a4
	movea.l Frame.Dos(a5), a6
	lea 0(a4), a0
	move.l a0, d1
	moveq #-2, d2
	jsr -84(a6)
	tst.l d0
	beq.w fail
	move.l d0, d6
	move.l d6, d1
	lea 256(a4), a0
	move.l a0, d2
	jsr -102(a6)
	tst.l d0
	beq.w unlockBad
next
	move.l d6, d1
	lea 256(a4), a0
	move.l a0, d2
	jsr -108(a6)
	tst.l d0
	beq.w endEntries
	lea 264(a4), a1
	cmpi.b #'.', (a1)
	bne.w candidate
	tst.b 1(a1)
	beq.w next
	cmpi.b #'.', 1(a1)
	bne.w candidate
	tst.b 2(a1)
	beq.w next
candidate
	lea 0(a4), a0
	lea 12(a5), a2
	bsr.w copyPath
	bne.w unlockBad
	lea 264(a4), a0
	bsr.w appendName
	bne.w unlockBad
	tst.l FIB+4(a4)
	bgt.w descend
	lea 264(a4), a0
	bsr.w sourceSuffix
	tst.l d0
	beq.w next
	movea.l Frame.Callback(a5), a2
	lea 12(a5), a0
	movea.l Frame.Context(a5), a1
	jsr (a2)
	tst.l d0
	bne.w unlockBad
	bra.w next
descend
	move.l d7, d0
	addq.w #1, d0
	cmpi.w #DEPTH_LIMIT, d0
	bhs.w unlockBad
	mulu.w #FRAME_BYTES, d0
	lea 268(a5), a2
	adda.l d0, a2
	lea 0(a2), a2
	lea 12(a5), a0
	bsr.w copyPath
	bne.w unlockBad
	addq.w #1, d7
	bsr.w directory
	subq.w #1, d7
	tst.l d0
	bne.w unlockBad
	bra.w next
endEntries
	jsr -132(a6)
	cmpi.l #232, d0
	bne.w unlockBad
	move.l d6, d1
	jsr -90(a6)
	moveq #0, d0
	bra.w done
unlockBad
	move.l d6, d1
	jsr -90(a6)
fail
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; directory

; A0=NUL source,A2=256-byte destination. D0/CCR=status; A0/A2 advanced.
copyPath	.block
	move.w #PATH_BYTES-1, d0
copy
	move.b (a0)+, (a2)+
	beq.w good
	dbra d0, copy
	moveq #1, d0
	rts
good
	moveq #0, d0
	rts
	.bend  ; copyPath

; A0=NUL filename,A2=NUL candidate path. Append separator if needed.
; D0/CCR=status; clobbers D0-D2/A0/A2, other registers preserved.
appendName	.block
	lea 12(a5), a2
	moveq #0, d1
findEnd
	cmpi.w #PATH_BYTES-1, d1
	bhs.w bad
	tst.b (a2)+
	beq.w join
	addq.w #1, d1
	bra.w findEnd
join
	subq.w #1, a2  ; overwrite the existing terminator
	tst.w d1
	beq.w copy
	move.b -1(a2), d2
	cmpi.b #':', d2
	beq.w copy
	cmpi.b #'/', d2
	beq.w copy
	move.b #'/', (a2)+
	addq.w #1, d1
copy
	move.b (a0)+, d2
	cmpi.w #PATH_BYTES, d1
	bhs.w bad
	move.b d2, (a2)+
	beq.w good
	addq.w #1, d1
	bra.w copy
good
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; appendName

; A0=NUL filename. D0=1 for .asm/.inc (ASCII case-insensitive), else 0.
; Clobbers D0-D3/A0; other registers preserved.
sourceSuffix	.block
	movea.l a0, a1
	moveq #0, d0
length
	tst.b (a1)+
	beq.w end
	addq.w #1, d0
	cmpi.w #255, d0
	bhi.w no
	bra.w length
end
	cmpi.w #4, d0
	blo.w no
	movea.l a0, a1
	adda.w d0, a1
	suba.w #4, a1
	cmpi.b #'.', (a1)+
	bne.w no
	moveq #0, d1
	move.b (a1)+, d1
	bsr.w fold
	cmpi.b #'a', d1
	beq.w asm
	cmpi.b #'i', d1
	bne.w no
	move.b (a1)+, d1
	bsr.w fold
	cmpi.b #'n', d1
	bne.w no
	move.b (a1), d1
	bsr.w fold
	cmpi.b #'c', d1
	beq.w yes
	bra.w no
asm
	move.b (a1)+, d1
	bsr.w fold
	cmpi.b #'s', d1
	bne.w no
	move.b (a1), d1
	bsr.w fold
	cmpi.b #'m', d1
	bne.w no
yes
	moveq #1, d0
	rts
no
	moveq #0, d0
	rts
	.bend  ; sourceSuffix

fold	.block
	cmpi.b #'A', d1
	blo.w done
	cmpi.b #'Z', d1
	bhi.w done
	addi.b #32, d1
done
	rts
	.bend  ; fold
	.endsection
	.endmodule
