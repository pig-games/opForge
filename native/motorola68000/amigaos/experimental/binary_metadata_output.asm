; Resolve prepared root output metadata and CLI names without source parsing.
; @opforge-owner: experimental.amigaos.binary_metadata_output
	.module experimental.amigaos.binary_metadata_output
	.cpu 68020
	.use experimental.amigaos.binary_metadata_prepare as metadata
	.use experimental.amigaos.binary_memory as memory
	.pub
PATH_BYTES = 256
BIN_KIND = 1
HUNK_KIND = 2
SOURCE_ONLY_KIND = 3
HEX_KIND = 4
SREC_KIND = 5
BIN_ITEM = 0
BIN_END = 1
BIN_INVALID = 2
Frame	.struct
Base	.long ?
Config	.long ?
CliPath	.long ?
CliKind	.word ?
Default	.word ?
ResolvedCli	.res PATH_BYTES
ResolvedHex	.res PATH_BYTES
HexSet	.word ?
BinReserved	.word ?
BinIndex	.long ?
ResolvedBin	.res PATH_BYTES
AutoRangeSet	.word ?
AutoRangeReserved	.word ?
AutoRangeStart	.long ?
	.endstruct
FRAME_BYTES = Frame.AutoRangeStart+4
	.section code, kind=code
	.pub
; A0=Frame. D0/CCR=0 success, 1 invalid/overflow/escaping path.
; D1-D7/A1-A6 are preserved. Null Base/Config support standalone harnesses.
resolve	.block
	movem.l d1-d7/a1-a6, -(sp)
	movea.l a0, a4
	clr.w Frame.HexSet(a4)
	clr.b Frame.ResolvedHex(a4)
	clr.b Frame.ResolvedCli(a4)
	moveq #0, d6  ; effective-base-present flag
	lea EmptyPath, a5
	movea.l Frame.Config(a4), a6
	cmpa.l #0, a6
	beq.w selectBase
	tst.w metadata.Config.NameSet(a6)
	beq.w selectBase
	lea metadata.Config.Name(a6), a5
	moveq #1, d6
	bra.w baseReady
selectBase
	move.l Frame.Base(a4), d0
	beq.w baseReady
	movea.l d0, a5
	moveq #1, d6
baseReady
	move.w Frame.CliKind(a4), d7
	cmpi.w #SREC_KIND, d7
	bhi.w failure
	cmpi.w #SOURCE_ONLY_KIND, d7
	beq.w resolveHex
	cmpi.w #0, d7
	beq.w cliCopy
	tst.w Frame.Default(a4)
	beq.w explicitCli
	movea.l Frame.Config(a4), a6
	cmpa.l #0, a6
	beq.w explicitCli
	tst.w metadata.Config.NameSet(a6)
	beq.w explicitCli
	movea.l a5, a0
	lea Frame.ResolvedCli(a4), a1
	bsr.w selectCliSuffix
	bsr.w appendSuffix
	tst.l d0
	bne.w failure
	bra.w resolveHex
explicitCli
	move.l Frame.CliPath(a4), d0
	beq.w clearCli
	movea.l d0, a0
	lea Frame.ResolvedCli(a4), a1
	tst.l d6
	beq.w cliCopyPath
	movea.l a5, a2
	bsr.w resolveRelativePath
	tst.l d0
	bne.w failure
	bra.w resolveHex
cliCopyPath
	bsr.w copyPath
	tst.l d0
	bne.w failure
	bra.w resolveHex
cliCopy
	move.l Frame.CliPath(a4), d0
	beq.w clearCli
	movea.l d0, a0
	lea Frame.ResolvedCli(a4), a1
	bsr.w copyPath
	tst.l d0
	bne.w failure
	bra.w resolveHex
clearCli
	clr.b Frame.ResolvedCli(a4)
resolveHex
	movea.l Frame.Config(a4), a6
	cmpa.l #0, a6
	beq.w success
	tst.w metadata.Config.HexSet(a6)
	beq.w success
	cmpi.w #HEX_KIND, Frame.CliKind(a4)
	beq.w success  ; CLI HEX already owns this record format.
	move.w #1, Frame.HexSet(a4)
	lea metadata.Config.Hex(a6), a0
	tst.b (a0)
	bne.w namedHex
	movea.l a5, a0
	lea Frame.ResolvedHex(a4), a1
	lea HexSuffix, a2
	bsr.w appendSuffix
	tst.l d0
	bne.w failure
	bra.w success
namedHex
	lea HexScratch, a1
	bsr.w copyPath
	tst.l d0
	bne.w failure
	lea HexScratch, a0
	bsr.w hasExtension
	tst.l d0
	bmi.w failure
	bne.w hexReady
	lea HexScratch, a0
	bsr.w pathEnd
	tst.l d0
	bmi.w failure
	lea HexSuffix, a2
	bsr.w appendAtEnd
	tst.l d0
	bne.w failure
hexReady
	lea HexScratch, a0
	lea Frame.ResolvedHex(a4), a1
	tst.l d6
	beq.w hexCopyPath
	movea.l a5, a2
	bsr.w resolveRelativePath
	tst.l d0
	bne.w failure
	bra.w success
hexCopyPath
	bsr.w copyPath
	tst.l d0
	bne.w failure
success
	moveq #0, d0
	bra.w done
failure
	clr.w Frame.HexSet(a4)
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a1-a6
	tst.l d0
	rts
	.bend  ; resolve
; A0=Frame, with BinIndex reset by caller before traversal. D0/CCR=BIN_ITEM,
; BIN_END or BIN_INVALID; ITEM returns A1=ResolvedBin,D1=range flag,D2/D3=bounds.
; D4-D7/A0/A2-A6 preserved. Cursor increments only after successful resolution.
; Explicit CLI BIN replaces the source vector. AutoRangeSet/Start are caller
; inputs from completed image geometry, used for unnamed multiple-output paths.
; No metadata text is interpreted.
resolveBin	.block
	movem.l d4-d7/a0/a2-a6, -(sp)
	movea.l a0, a4
	clr.b Frame.ResolvedBin(a4)
	cmpi.w #BIN_KIND, Frame.CliKind(a4)
	beq.w binEnd
	movea.l Frame.Config(a4), a6
	cmpa.l #0, a6
	beq.w binEnd
	move.l Frame.BinIndex(a4), d7
	cmp.l metadata.Config.BinCount(a6), d7
	bhs.w binEnd
	move.l d7, d0
	mulu.l #metadata.BIN_BYTES, d0
	bvs.w binBad
	move.l d0, d1
	addi.l #metadata.BIN_BYTES, d1
	bcs.w binBad
	cmp.l metadata.Config.Bins+memory.Block.Used(a6), d1
	bhi.w binBad
	movea.l metadata.Config.Bins+memory.Block.Pointer(a6), a3
	cmpa.l #0, a3
	beq.w binBad
	adda.l d0, a3
	movea.l a3, a6  ; selected owned specification
	lea EmptyPath, a5
	movea.l Frame.Config(a4), a0
	tst.w metadata.Config.NameSet(a0)
	beq.w binInputBase
	lea metadata.Config.Name(a0), a5
	bra.w binBaseReady
binInputBase
	move.l Frame.Base(a4), d0
	beq.w binBaseReady
	movea.l d0, a5
binBaseReady
	lea metadata.Bin.Name(a6), a0
	tst.b (a0)
	bne.w binNamed
	movea.l Frame.Config(a4), a0
	cmpi.l #1, metadata.Config.BinCount(a0)
	beq.w binSingle
	tst.w metadata.Bin.RangeSet(a6)
	beq.w binUnnamed
	movea.l a5, a0
	lea BinScratch, a1
	lea DashSuffix, a2
	bsr.w appendSuffix
	bne.w binBad
	lea BinScratch, a0
	bsr.w pathEnd
	tst.l d0
	bmi.w binBad
	lea metadata.Bin.RangeText(a6), a2
	bsr.w appendAtEnd
	bne.w binBad
	bra.w binExtension
binUnnamed
	tst.w Frame.AutoRangeSet(a4)
	beq.w binOrdinal
	move.l Frame.AutoRangeStart(a4), d0
	bsr.w autoSuffix
	bra.w binIndexReady
binOrdinal
	move.l d7, d0
	addq.l #1, d0
	bsr.w indexSuffix
binIndexReady
	movea.l a5, a0
	lea BinScratch, a1
	lea IndexText, a2
	bsr.w appendSuffix
	bne.w binBad
	bra.w binExtension
binSingle
	movea.l a5, a0
	lea BinScratch, a1
	bsr.w copyPath
	bne.w binBad
	bra.w binExtension
binNamed
	lea BinScratch, a1
	bsr.w copyPath
	bne.w binBad
binExtension
	lea BinScratch, a0
	bsr.w hasExtension
	tst.l d0
	bmi.w binBad
	bne.w binPathReady
	lea BinScratch, a0
	bsr.w pathEnd
	tst.l d0
	bmi.w binBad
	lea BinSuffix, a2
	bsr.w appendAtEnd
	bne.w binBad
binPathReady
	lea BinScratch, a0
	lea Frame.ResolvedBin(a4), a1
	tst.b metadata.Bin.Name(a6)
	beq.w binRawPath
	movea.l a5, a2
	bsr.w resolveRelativePath
	bra.w binPathDone
binRawPath
	bsr.w copyPath
binPathDone
	tst.l d0
	bne.w binBad
	addq.l #1, Frame.BinIndex(a4)
	moveq #0, d1
	move.w metadata.Bin.RangeSet(a6), d1
	move.l metadata.Bin.Start(a6), d2
	move.l metadata.Bin.End(a6), d3
	lea Frame.ResolvedBin(a4), a1
	moveq #BIN_ITEM, d0
	bra.w binDone
binEnd
	moveq #BIN_END, d0
	bra.w binDone
binBad
	clr.b Frame.ResolvedBin(a4)
	moveq #BIN_INVALID, d0
binDone
	movem.l (sp)+, d4-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; resolveBin

	.priv
; D0=automatic image start. Match shared format_addr's uppercase 4/6/8 digits.
; Clobbers D0-D2/A0-A1; returns '-' plus digits in IndexText.
autoSuffix	.block
	moveq #4, d2
	cmpi.l #$ffff, d0
	bls.w widthReady
	moveq #6, d2
	cmpi.l #$ffffff, d0
	bls.w widthReady
	moveq #8, d2
widthReady
	lea IndexText, a0
	move.b #'-', (a0)+
	adda.l d2, a0
	clr.b (a0)
	lea HexDigits, a1
hexDigit
	move.l d0, d1
	andi.l #15, d1
	move.b 0(a1, d1.w), -(a0)
	lsr.l #4, d0
	subq.w #1, d2
	bne.w hexDigit
	rts
	.bend  ; autoSuffix

; D0=one-based ordinal. Produce '-' plus unsigned decimal digits.
; Clobbers D0-D3/A0-A2; scratch is owned by this output resolver.
indexSuffix	.block
	lea -12(sp), sp
	lea 12(sp), a0
	moveq #0, d2
digits
	move.l d0, d1
	moveq #10, d3
	divu.l d3, d0
	move.l d0, d3
	mulu.l #10, d3
	sub.l d3, d1
	addi.b #'0', d1
	move.b d1, -(a0)
	addq.w #1, d2
	tst.l d0
	bne.w digits
	lea IndexText, a1
	move.b #'-', (a1)+
copyDigits
	move.b (a0)+, (a1)+
	subq.w #1, d2
	bne.w copyDigits
	clr.b (a1)
	lea 12(sp), sp
	rts
	.bend  ; indexSuffix

; D7=CLI kind; return A2=suffix.
selectCliSuffix	.block
	cmpi.w #BIN_KIND, d7
	beq.w chooseBin
	cmpi.w #HUNK_KIND, d7
	beq.w chooseHunk
	cmpi.w #HEX_KIND, d7
	beq.w chooseHex
	lea SrecSuffix, a2
	rts
chooseBin
	lea BinSuffix, a2
	rts
chooseHunk
	lea HunkSuffix, a2
	rts
chooseHex
	lea HexSuffix, a2
	rts
	.bend  ; selectCliSuffix

; A0=base,A1=destination,A2=suffix. Append even when base has an extension.
appendSuffix	.block
	movea.l a1, a3
	moveq #0, d1
baseCopy
	move.b (a0)+, d0
	beq.w suffixCopy
	cmpi.w #PATH_BYTES-1, d1
	bhs.w appendBad
	move.b d0, (a3)+
	addq.w #1, d1
	bra.w baseCopy
suffixCopy
	move.b (a2)+, d0
	beq.w suffixDone
	cmpi.w #PATH_BYTES-1, d1
	bhs.w appendBad
	move.b d0, (a3)+
	addq.w #1, d1
	bra.w suffixCopy
suffixDone
	clr.b (a3)
	moveq #0, d0
	rts
appendBad
	moveq #1, d0
	rts
	.bend  ; appendSuffix

; Bounded NUL string copy, A0=source,A1=destination; D0=status.
copyPath	.block
	moveq #0, d1
copyLoop
	move.b (a0)+, d0
	beq.w copyEnd
	cmpi.w #PATH_BYTES-1, d1
	bhs.w copyBad
	move.b d0, (a1)+
	addq.w #1, d1
	bra.w copyLoop
copyEnd
	clr.b (a1)
	moveq #0, d0
	rts
copyBad
	moveq #1, d0
	rts
	.bend  ; copyPath

; A0=path; D0=length,A0=end, or D0=-1 if no NUL within PATH_BYTES.
pathEnd	.block
	moveq #0, d0
pathEndLoop
	cmpi.w #PATH_BYTES, d0
	bhs.w pathEndBad
	tst.b 0(a0, d0.w)
	beq.w pathEndGood
	addq.w #1, d0
	bra.w pathEndLoop
pathEndGood
	adda.w d0, a0
	rts
pathEndBad
	moveq #-1, d0
	rts
	.bend  ; pathEnd

; A0=end of path,A2=suffix; bounded append.
appendAtEnd	.block
	move.l d0, d1
appendEndLoop
	move.b (a2)+, d0
	beq.w appendEndDone
	cmpi.w #PATH_BYTES-1, d1
	bhs.w appendEndBad
	move.b d0, (a0)+
	addq.w #1, d1
	bra.w appendEndLoop
appendEndDone
	clr.b (a0)
	moveq #0, d0
	rts
appendEndBad
	moveq #1, d0
	rts
	.bend  ; appendAtEnd

; A0=name; D0=1 if final component has a non-leading dot, else 0.
hasExtension	.block
	moveq #0, d1  ; final component width
	moveq #0, d2  ; extension marker
	moveq #0, d3
extensionScan
	cmpi.w #PATH_BYTES, d3
	bhs.w extensionBad
	move.b 0(a0, d3.w), d0
	beq.w extensionDone
	cmpi.b #'/', d0
	beq.w extensionReset
	cmpi.b #':', d0
	beq.w extensionReset
	cmpi.b #'.', d0
	bne.w extensionOther
	tst.w d1
	beq.w extensionOther
	moveq #1, d2
	bra.w extensionOther
extensionReset
	moveq #0, d1
	moveq #0, d2
	bra.w extensionNext
extensionOther
	addq.w #1, d1
extensionNext
	addq.w #1, d3
	bra.w extensionScan
extensionDone
	cmpi.w #2, d1
	bne.w extensionResult
	cmpi.b #'.', -1(a0, d3.w)
	bne.w extensionResult
	cmpi.b #'.', -2(a0, d3.w)
	bne.w extensionResult
	moveq #0, d2
extensionResult
	move.l d2, d0
	rts
extensionBad
	moveq #-1, d0
	rts
	.bend  ; hasExtension

; A0=name,A1=destination,A2=base. Absolute volume/slash names pass through.
; Normalize the base parent first, then relative components without crossing it.
; A stack of prior lengths makes '..' removal bounded and keeps pointers intact.
resolveRelativePath	.block
	movem.l d1-d7/a3-a6, -(sp)
	lea -512(sp), sp
	movea.l a0, a4
	movea.l a1, a5
	movea.l a2, a6
	bsr.w pathEnd
	tst.l d0
	bmi.w badPath
	movea.l a4, a0
	cmpi.b #'/', (a0)
	beq.w absolutePath
absoluteScan
	move.b (a0)+, d0
	beq.w relativePath
	cmpi.b #'/', d0
	beq.w relativePath
	cmpi.b #':', d0
	beq.w absolutePath
	bra.w absoluteScan
absolutePath
	movea.l a4, a0
	movea.l a5, a1
	bsr.w copyPath
	bra.w pathDone
relativePath
	; Retain only the parent extent, including its volume/root separator.
	movea.l a6, a0
	bsr.w pathEnd
	tst.l d0
	bmi.w badPath
	movea.l a6, a0
	movea.l a6, a3
parentScan
	move.b (a0)+, d0
	beq.w parentReady
	cmpi.b #'/', d0
	beq.w parentSeparator
	cmpi.b #':', d0
	bne.w parentScan
parentSeparator
	movea.l a0, a3
	bra.w parentScan
parentReady
	movea.l a6, a0
	movea.l a5, a1
	moveq #0, d5  ; destination length
	moveq #0, d7  ; component stack depth
	moveq #0, d6  ; relative-name depth boundary
	moveq #0, d4  ; zero normalizes parent; one handles requested name
	moveq #0, d2  ; anchored prefix (volume or slash)
	cmpa.l a3, a0
	beq.w phaseDone
	cmpi.b #'/', (a0)
	bne.w volumeScan
	move.b (a0)+, (a1)+
	moveq #1, d5
	moveq #1, d2
	bra.w componentNext
volumeScan
	movea.l a0, a2
volumeFind
	cmpa.l a3, a2
	beq.w componentNext
	move.b (a2)+, d0
	cmpi.b #'/', d0
	beq.w componentNext
	cmpi.b #':', d0
	bne.w volumeFind
volumeCopy
	cmpa.l a2, a0
	beq.w volumeCopied
	move.b (a0)+, (a1)+
	addq.w #1, d5
	bra.w volumeCopy
volumeCopied
	moveq #1, d2
componentNext
	tst.w d4
	bne.w nameExtent
	cmpa.l a3, a0
	bhs.w phaseDone
nameExtent
	tst.b (a0)
	beq.w phaseDone
	cmpi.b #'/', (a0)
	bne.w componentStart
	addq.l #1, a0
	bra.w componentNext
componentStart
	movea.l a0, a2
	moveq #0, d3
componentScan
	tst.w d4
	bne.w componentName
	cmpa.l a3, a0
	bhs.w componentReady
componentName
	move.b (a0), d0
	beq.w componentReady
	cmpi.b #'/', d0
	beq.w componentReady
	addq.l #1, a0
	addq.w #1, d3
	bra.w componentScan
componentReady
	cmpi.w #1, d3
	bne.w checkParent
	cmpi.b #'.', (a2)
	beq.w componentNext
checkParent
	cmpi.w #2, d3
	bne.w normalComponent
	cmpi.b #'.', (a2)
	bne.w normalComponent
	cmpi.b #'.', 1(a2)
	bne.w normalComponent
	cmp.w d6, d7
	bhi.w popComponent
	tst.w d4
	bne.w badPath
	tst.w d2
	bne.w componentNext  ; parent normalization cannot ascend a volume/root
	bra.w normalComponent  ; retain a leading relative '..' in the base parent
popComponent
	move.w d7, d0
	subq.w #1, d0
	lsl.w #2, d0
	tst.w 2(sp, d0.w)
	beq.w normalComponent  ; consecutive leading '..' remain in the base parent
	move.w 0(sp, d0.w), d5
	movea.l a5, a1
	adda.w d5, a1
	subq.w #1, d7
	bra.w componentNext
normalComponent
	cmpi.w #128, d7
	bhs.w badPath
	move.w d7, d0
	lsl.w #2, d0
	move.w d5, 0(sp, d0.w)
	move.w #1, 2(sp, d0.w)
	cmpi.w #2, d3
	bne.w separator
	cmpi.b #'.', (a2)
	bne.w separator
	cmpi.b #'.', 1(a2)
	bne.w separator
	clr.w 2(sp, d0.w)
separator
	tst.w d5
	beq.w componentBytes
	move.b -1(a1), d0
	cmpi.b #'/', d0
	beq.w componentBytes
	cmpi.b #':', d0
	beq.w componentBytes
	cmpi.w #PATH_BYTES-1, d5
	bhs.w badPath
	move.b #'/', (a1)+
	addq.w #1, d5
componentBytes
	move.w d3, d0
	add.w d5, d0
	cmpi.w #PATH_BYTES-1, d0
	bhi.w badPath
	add.w d3, d5
copyComponent
	move.b (a2)+, (a1)+
	subq.w #1, d3
	bne.w copyComponent
	addq.w #1, d7
	bra.w componentNext
phaseDone
	tst.w d4
	bne.w normalized
	moveq #1, d4
	move.w d7, d6
	movea.l a4, a0
	bra.w componentNext
normalized
	clr.b (a1)
	moveq #0, d0
	bra.w pathDone
badPath
	moveq #1, d0
pathDone
	lea 512(sp), sp
	movem.l (sp)+, d1-d7/a3-a6
	tst.l d0
	rts
	.bend  ; resolveRelativePath
	.endsection

	.section data, kind=data
EmptyPath	.byte 0
HexDigits	.byte "0123456789ABCDEF"
DashSuffix	.byte "-", 0
BinSuffix	.byte ".bin", 0
HunkSuffix	.byte ".hunk", 0
HexSuffix	.byte ".hex", 0
SrecSuffix	.byte ".srec", 0
	.endsection
	.section bss, kind=bss
	.align 2
HexScratch	.res byte, PATH_BYTES
BinScratch	.res byte, PATH_BYTES
IndexText	.res byte, 12
	.endsection
	.endmodule
