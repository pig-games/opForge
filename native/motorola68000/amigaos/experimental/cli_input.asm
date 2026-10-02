; Shell input resolution and default include root; no assembler syntax parsing.
; @opforge-owner: experimental.amigaos.cli_input
	.module experimental.amigaos.cli_input
	.cpu 68020
	.use experimental.amigaos.cli_arguments as args
	.pub
OK = 0
MISSING = 1
CAPACITY = 2
LOCK = -84
UNLOCK = -90
EXAMINE = -102
READ_LOCK = -2
FIB_BYTES = 260
FIB_TYPE = 4
FIB_NAME = 8
	.section code, kind=code
	.pub
; Resolve State.Input in place. A0=args.State, A6=dos.library.
; A1=256-byte root directory buffer, also inserted before explicit include roots.
; Directory input selects main.asm. D0/CCR=status; other registers preserved.
; Owns one temporary lock and FIB; all return paths release them.
resolve	.block
	movem.l d1-d7/a0-a5, -(sp)
	suba.w #FIB_BYTES, sp
	movea.l a0, a4
	movea.l a1, a5
	lea args.State.Input(a4), a0
	cmpi.w #$2e00, (a0)  ; Shell '.' maps to the AmigaDOS empty current-dir path
	bne.w named
	clr.b (a0)
named
	bsr.w inspect
	bne.w done
	move.l FIB_TYPE(sp), d5
	bgt.w inputDirectory
	lea args.State.Input(a4), a0
	bsr.w sourceName
	bne.w failMissing
inputDirectory
	bsr.w outputName
	bne.w done
	tst.l FIB_TYPE(sp)
	ble.w file
	lea args.State.Input(a4), a0
	movea.l a0, a1
	moveq #0, d3
length
	tst.b (a1)+
	beq.w append
	addq.w #1, d3
	bra.w length
append
	subq.l #1, a1
	cmpi.w #args.PATH_BYTES-10, d3
	bhi.w failCapacity
	tst.w d3
	beq.w suffix
	cmpi.b #':', -1(a1)
	beq.w suffix
	cmpi.b #'/', -1(a1)
	beq.w suffix
	move.b #'/', (a1)+
suffix
	lea MainName, a2
copyMain
	move.b (a2)+, (a1)+
	bne.w copyMain
	bsr.w inspect
	bne.w done
	tst.l FIB_TYPE(sp)
	bgt.w failMissing
file
	lea args.State.Input(a4), a0
	movea.l a0, a1
	movea.l a0, a2
findRoot
	move.b (a1)+, d0
	beq.w root
	cmpi.b #'/', d0
	beq.w separator
	cmpi.b #':', d0
	bne.w findRoot
separator
	movea.l a1, a2
	bra.w findRoot
root
	movea.l a5, a1
copyRoot
	cmpa.l a0, a2
	beq.w rootEnd
	move.b (a0)+, (a1)+
	bra.w copyRoot
rootEnd
	clr.b (a1)
	move.l args.State.IncludeCount(a4), d3
	move.l d3, d0
	lsl.l #8, d0
	lea args.State.IncludePaths(a4), a0
	adda.l d0, a0
	lea args.PATH_BYTES(a0), a1
	move.l d0, d2
	beq.w seed
shift
	move.b -(a0), -(a1)
	subq.l #1, d2
	bne.w shift
seed
	lea args.State.IncludePaths(a4), a1
	movea.l a5, a0
copySeed
	move.b (a0)+, (a1)+
	bne.w copySeed
	addq.l #1, args.State.IncludeCount(a4)
	moveq #OK, d0
	bra.w done
failMissing
	moveq #MISSING, d0
	bra.w done
failCapacity
	moveq #CAPACITY, d0
done
	adda.w #FIB_BYTES, sp
	tst.l d0
	movem.l (sp)+, d1-d7/a0-a5
	rts
	.bend  ; resolve
	.priv
; A0=NUL path. D0/CCR=OK iff the final component has an .asm extension.
; Case-insensitive, clobbers D1/A0. Folder classification happens before this.
sourceName	.block
	moveq #0, d1
scan
	tst.b (a0)+
	beq.w extension
	addq.w #1, d1
	bra.w scan
extension
	subq.l #1, a0
	cmpi.w #4, d1
	bls.w bad
	cmpi.b #'/', -5(a0)
	beq.w bad
	cmpi.b #':', -5(a0)
	beq.w bad
	move.l -4(a0), d0
	ori.l #$20202020, d0
	cmpi.l #$2e61736d, d0  ; .asm
	bne.w bad
	moveq #OK, d0
	rts
bad
	moveq #MISSING, d0
	rts
	.bend  ; sourceName
; Fill an omitted explicit output name from the original input basename.
; D5=entry type, A4=args.State. Explicit names gain an extension if absent.
; D0/CCR=status, clobbers D1-D4/A0-A2. All copies stay within PATH_BYTES.
outputName	.block
	bsr.w outputBase
	tst.l d0
	bne.w bad
	tst.w args.State.OutputKind(a4)
	beq.w ready
	lea args.State.Output(a4), a1
	tst.b (a1)
	bne.w explicit
	lea args.State.OutputBase(a4), a0
	lea args.State.Output(a4), a1
	moveq #0, d3
copyDefault
	move.b (a0)+, (a1)+
	beq.w defaultCopied
	addq.w #1, d3
	bra.w copyDefault
defaultCopied
	subq.l #1, a1  ; overwrite the copied terminator with the suffix
	bra.w extension
explicit
	moveq #0, d3
	moveq #0, d4
	movea.l a1, a2
scanOutput
	move.b (a1)+, d0
	beq.w scanned
	addq.w #1, d3
	cmpi.b #'/', d0
	beq.w resetDot
	cmpi.b #':', d0
	beq.w resetDot
	cmpi.b #'.', d0
	bne.w scanOutput
	movea.l a1, a0
	subq.l #1, a0
	cmpa.l a2, a0
	beq.w scanOutput
	moveq #1, d4
	bra.w scanOutput
resetDot
	moveq #0, d4
	movea.l a1, a2
	bra.w scanOutput
scanned
	subq.l #1, a1
	tst.l d4
	bne.w ready
extension
	cmpi.w #args.PATH_BYTES-6, d3
	bhi.w bad
	tst.w d3
	beq.w bad
	lea BinExtension, a0
	cmpi.w #2, args.State.OutputKind(a4)
	beq.w useHunkSuffix
	cmpi.w #4, args.State.OutputKind(a4)
	beq.w useHexSuffix
	cmpi.w #5, args.State.OutputKind(a4)
	bne.w addExtension
	lea SrecExtension, a0
	bra.w addExtension
useHunkSuffix
	lea HunkExtension, a0
	bra.w addExtension
useHexSuffix
	lea HexExtension, a0
addExtension
	move.b (a0)+, (a1)+
	bne.w addExtension
ready
	moveq #OK, d0
	rts
bad
	moveq #CAPACITY, d0
	rts
	.bend  ; outputName

; Save the basename of the original input before directory resolution mutates
; State.Input to its selected root source. D5 is the original Examine type.
; File inputs lose their final extension; directory names retain dots.
outputBase	.block
	lea args.State.Input(a4), a0
	tst.l d5
	ble.s outputBasePath
	lea FIB_NAME+8(sp), a0  ; outputName plus this helper each add a return address
outputBasePath
	movea.l a0, a2
outputBaseFind
	move.b (a0)+, d0
	beq.s outputBaseCopy
	cmpi.b #':', d0
	beq.s outputBaseNext
	cmpi.b #'/', d0
	bne.s outputBaseFind
outputBaseNext
	tst.b (a0)
	beq.s outputBaseFind
	movea.l a0, a2
	bra.s outputBaseFind
outputBaseCopy
	movea.l a2, a0
	lea args.State.OutputBase(a4), a1
	moveq #0, d3
	moveq #0, d4
outputBaseByte
	move.b (a0)+, d0
	beq.s outputBaseDone
	cmpi.b #'/', d0
	beq.s outputBaseDone
	cmpi.b #':', d0
	beq.s outputBaseDone
	cmpi.b #'.', d0
	bne.s outputBaseStore
	move.l a1, d4
outputBaseStore
	cmpi.w #args.PATH_BYTES-1, d3
	bhs.s outputBaseBad
	move.b d0, (a1)+
	addq.w #1, d3
	bra.s outputBaseByte
outputBaseDone
	clr.b (a1)
	tst.l d5
	bgt.s outputBaseOk
	tst.l d4
	beq.s outputBaseOk
	lea args.State.OutputBase(a4), a2
	cmpa.l d4, a2  ; keep a leading dot as part of the filename
	beq.s outputBaseOk
	movea.l d4, a1
	clr.b (a1)
outputBaseOk
	moveq #OK, d0
	rts
outputBaseBad
	moveq #CAPACITY, d0
	rts
	.bend  ; outputBase
; A0=path. The caller's FIB is above our return address on the stack.
; D0/CCR=status, clobbers D1/D2/A2; OS calls may destroy scratch registers.
inspect	.block
	move.l a0, d1
	moveq #READ_LOCK, d2
	jsr LOCK(a6)
	tst.l d0
	beq.w fail
	move.l d0, d7
	move.l d0, d1
	lea 4(sp), a2
	move.l a2, d2
	jsr EXAMINE(a6)
	move.l d0, d6
	move.l d7, d1
	jsr UNLOCK(a6)
	tst.l d6
	beq.w fail
	moveq #OK, d0
	rts
fail
	moveq #MISSING, d0
	rts
	.bend  ; inspect
	.endsection
	.section data, kind=data
MainName	.byte "main.asm", 0
BinExtension	.byte ".bin", 0
HunkExtension	.byte ".hunk", 0
HexExtension	.byte ".hex", 0
SrecExtension	.byte ".srec", 0
	.endsection
	.endmodule
