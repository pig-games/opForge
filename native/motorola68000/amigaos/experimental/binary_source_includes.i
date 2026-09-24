; Preparation-only textual inclusion. Each active file owns a buffered reader;
; the only retained execution provenance is a numeric run of packed offsets.

; Keep the configured search roots for selected-file include resolution.
saveRoot .block
	move.l RootCount, d0
	sub.l IncludeRootsRemaining, d0
	bmi.w bad
	lsl.l #8, d0
	lea RootPaths, a0
	movea.l memory.Block.Pointer(a0), a1
	adda.l d0, a1
	lea SourcePath, a0
	move.w #PATH_BYTES/4-1, d0
copyRoot
	move.l (a0)+, (a1)+
	dbra d0, copyRoot
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend ; saveRoot

; D0=0 ordinary line, 1 opened include, -1 malformed or unresolved include.
; Only a whole-line generic .include directive is handled at this boundary.
handleInclude .block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l LineBuffer, a0
	move.l LineUsed, d7
	beq.w ordinary
	cmpi.b #13, -1(a0,d7.l)
	bne.w scan
	subq.l #1, d7
scan
	moveq #0, d6
white
	cmp.l d7, d6
	bhs.w ordinary
	move.b 0(a0,d6.l), d0
	cmpi.b #' ', d0
	beq.w skip
	cmpi.b #9, d0
	bne.w dot
skip
	addq.l #1, d6
	bra.w white
dot
	cmpi.b #'.', d0
	bne.w ordinary
	addq.l #1, d6
	lea includeKeyword, a1
keyword
	move.b (a1)+, d1
	beq.w keywordEnd
	cmp.l d7, d6
	bhs.w ordinary
	move.b 0(a0,d6.l), d0
	bsr.w foldInclude
	cmp.b d1, d0
	bne.w ordinary
	addq.l #1, d6
	bra.w keyword
keywordEnd
	cmp.l d7, d6
	bhs.w invalid
	move.b 0(a0,d6.l), d0
	cmpi.b #' ', d0
	beq.w afterKeyword
	cmpi.b #9, d0
	bne.w ordinary
afterKeyword
	addq.l #1, d6
skipBeforeQuote
	cmp.l d7, d6
	bhs.w invalid
	move.b 0(a0,d6.l), d0
	cmpi.b #' ', d0
	beq.w nextBefore
	cmpi.b #9, d0
	bne.w quote
nextBefore
	addq.l #1, d6
	bra.w skipBeforeQuote
quote
	cmpi.b #'"', d0
	bne.w invalid
	addq.l #1, d6
	lea IncludeName, a1
	moveq #0, d5
name
	cmp.l d7, d6
	bhs.w invalid
	move.b 0(a0,d6.l), d0
	cmpi.b #'"', d0
	beq.w nameEnd
	cmpi.b #33, d0
	blo.w invalid
	cmpi.b #126, d0
	bhi.w invalid
	cmpi.b #':', d0
	beq.w invalid
	cmpi.b #'\\', d0
	beq.w invalid
	cmpi.l #PATH_BYTES-1, d5
	bhs.w invalid
	move.b d0, (a1)+
	addq.l #1, d5
	addq.l #1, d6
	bra.w name
nameEnd
	tst.l d5
	beq.w invalid
	clr.b (a1)
	addq.l #1, d6
tail
	cmp.l d7, d6
	bhs.w resolve
	move.b 0(a0,d6.l), d0
	cmpi.b #';', d0
	beq.w resolve
	cmpi.b #' ', d0
	beq.w tailNext
	cmpi.b #9, d0
	bne.w invalid
tailNext
	addq.l #1, d6
	bra.w tail
resolve
	cmpi.l #INCLUDE_DEPTH, IncludeDepthNow
	bhs.w invalid
	; First try the including file's directory, then configured roots.
	lea SourcePath, a0
	lea IncludePath, a1
	bsr.w parentPath
	bne.w invalid
	bsr.w openIncludedPath
	beq.w included
	bmi.w invalid
	moveq #0, d5
roots
	cmp.l RootCount, d5
	bhs.w invalid
	move.l d5, d0
	lsl.l #8, d0
	lea RootPaths, a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l d0, a0
	lea IncludePath, a1
	bsr.w copyIncludeBase
	bne.w invalid
	bsr.w openIncludedPath
	beq.w included
	bmi.w invalid
	addq.l #1, d5
	bra.w roots
included
	moveq #1, d0
	bra.w done
ordinary
	moveq #0, d0
	bra.w done
invalid
	moveq #-1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; handleInclude

includeKeyword .byte "include", 0

; A0=current path, A1=destination. Copy through its final slash.
parentPath .block
	movem.l d1-d3/a0-a2, -(sp)
	movea.l a0, a2
	moveq #0, d2
	moveq #-1, d3
findSlash
	cmpi.l #PATH_BYTES, d2
	bhs.w pathBad
	move.b (a2)+, d1
	beq.w foundSlash
	cmpi.b #'/', d1
	bne.w nextSlash
	move.l d2, d3
nextSlash
	addq.l #1, d2
	bra.w findSlash
foundSlash
	tst.l d3
	bmi.w pathBad
	move.l d3, d2
	addq.l #1, d2
copyParent
	move.b (a0)+, (a1)+
	subq.l #1, d2
	bne.w copyParent
	clr.b (a1)
	moveq #0, d0
	bra.w parentDone
pathBad
	moveq #1, d0
parentDone
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend ; parentPath

; A0=NUL base, A1=IncludePath. Append IncludeName after a slash.
copyIncludeBase .block
	movem.l d1-d2/a0-a1, -(sp)
	moveq #0, d1
baseByte
	cmpi.l #PATH_BYTES-2, d1
	bhs.w baseBad
	move.b (a0)+, d2
	beq.w baseEnd
	move.b d2, (a1)+
	addq.l #1, d1
	bra.w baseByte
baseEnd
	cmpi.b #'/', -1(a1)
	beq.w baseReady
	cmpi.b #':', -1(a1)
	beq.w baseReady
	move.b #'/', (a1)+
baseReady
	clr.b (a1)
	moveq #0, d0
	bra.w baseDone
baseBad
	moveq #1, d0
baseDone
	movem.l (sp)+, d1-d2/a0-a1
	tst.l d0
	rts
	.bend ; copyIncludeBase

; IncludePath holds a directory prefix. Normalize the relative name in place,
; stopping parent traversal at the volume boundary before authorization.
openIncludedPath .block
	movem.l d1-d7/a0-a6, -(sp)
	lea IncludePath, a0
	bsr.w normalizeIncludePath
	bne.w invalidPath
	lea IncludePath, a0
	moveq #0, d7
	moveq #0, d4
prefixLength
	cmpi.l #PATH_BYTES-1, d7
	bhs.w invalidPath
	move.b (a0)+, d0
	beq.w appendName
	cmpi.b #':', d0
	bne.w prefixNext
	move.l d7, d4
	addq.l #1, d4
prefixNext
	addq.l #1, d7
	bra.w prefixLength
appendName
	suba.l #1, a0
	tst.l d7
	beq.w invalidPath
	cmpi.b #':', -1(a0)
	beq.w baseReady
	cmpi.l #PATH_BYTES-2, d7
	bhs.w invalidPath
	move.b #'/', (a0)+
	addq.l #1, d7
baseReady
	clr.b (a0)
	lea IncludeName, a1
	moveq #0, d6
checkComponent
	move.b (a1), d0
	beq.w componentEnd
	cmpi.b #'/', d0
	beq.w componentEnd
	addq.l #1, d6
	addq.l #1, a1
	bra.w checkComponent
componentEnd
	tst.l d6
	beq.w invalidPath
	cmpi.l #1, d6
	bne.w checkDouble
	cmpi.b #'.', -1(a1)
	bne.w copyName
	bra.w skipComponent
checkDouble
	cmpi.l #2, d6
	bne.w copyName
	cmpi.b #'.', -2(a1)
	bne.w copyName
	cmpi.b #'.', -1(a1)
	bne.w copyName
	cmp.l d4, d7
	bls.w invalidPath
	; A0/D7 point past the last slash. Back up to the preceding slash.
	subq.l #1, a0
	subq.l #1, d7
parentComponent
	subq.l #1, a0
	subq.l #1, d7
	cmp.l d4, d7
	blo.w invalidPath
	beq.w parentAtFloor
	cmpi.b #'/', (a0)
	bne.w parentComponent
	addq.l #1, a0
	addq.l #1, d7
parentAtFloor
	clr.b (a0)
skipComponent
	tst.b (a1)
	beq.w invalidPath
	addq.l #1, a1
	moveq #0, d6
	bra.w checkComponent
copyName
	suba.l d6, a1
	move.l d7, d2
	add.l d6, d2
	cmpi.l #PATH_BYTES-1, d2
	bhs.w invalidPath
copyComponent
	move.b (a1)+, (a0)+
	subq.l #1, d6
	bne.w copyComponent
	move.b (a1), d0
	beq.w nameDone
	move.b (a1)+, (a0)+
	addq.l #1, d2
	move.l d2, d7
	bra.w checkComponent
nameDone
	clr.b (a0)
	lea IncludePath, a0
	bsr.w normalizeIncludePath
	bne.w invalidPath
	bsr.w authorizeIncludePath
	bne.w openFailed
	; A repeated active path is a cycle, even if AmigaDOS would open it.
	lea IncludePath, a0
	lea SourcePath, a1
	bsr.w sameNormalizedPath
	beq.w invalidPath
	moveq #0, d5
cycle
	cmp.l IncludeDepthNow, d5
	bhs.w tryOpen
	move.l d5, d0
	mulu.w #INCLUDE_FRAME_BYTES, d0
	lea IncludeStack, a1
	adda.l d0, a1
	lea IncludeFrame.Path(a1), a1
	lea IncludePath, a0
	bsr.w sameNormalizedPath
	beq.w invalidPath
	addq.l #1, d5
	bra.w cycle
tryOpen
	movea.l DosBase, a6
	move.l #IncludePath, d1
	move.l #1005, d2
	jsr -30(a6)
	tst.l d0
	beq.w openFailed
	move.l d0, IncludeHandle
	bsr.w pushInclude
	bne.w invalidPath
	moveq #0, d0
	bra.w openDone
openFailed
	moveq #1, d0
	bra.w openDone
invalidPath
	moveq #-1, d0
openDone
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend ; openIncludedPath

; A0=NUL path. Collapse separator and dot components in place. D0=0/1.
; A parent component cannot move above an Amiga volume or relative-path start.
normalizeIncludePath .block
	movem.l d1-d6/a0-a2, -(sp)
	movea.l a0, a1
	moveq #0, d3
	moveq #0, d4
volumeByte
	cmpi.l #PATH_BYTES-1, d3
	bhs.w pathBad
	move.b 0(a0,d3.l), d0
	beq.w noVolume
	cmpi.b #'/', d0
	beq.w noVolume
	cmpi.b #':', d0
	beq.w volumeEnd
	addq.l #1, d3
	bra.w volumeByte
volumeEnd
	addq.l #1, d3
	move.l d3, d4
	adda.l d3, a0
	adda.l d3, a1
	bra.w component
noVolume
	moveq #0, d3
component
	move.b (a0), d0
	beq.w pathEnd
	cmpi.b #'/', d0
	bne.w componentStart
	addq.l #1, a0
	bra.w component
componentStart
	movea.l a0, a2
	moveq #0, d6
componentByte
	move.b (a0), d0
	beq.w componentEnd
	cmpi.b #'/', d0
	beq.w componentEnd
	addq.l #1, a0
	addq.l #1, d6
	bra.w componentByte
componentEnd
	cmpi.l #1, d6
	bne.w doubleDot
	cmpi.b #'.', (a2)
	beq.w component
doubleDot
	cmpi.l #2, d6
	bne.w keepComponent
	cmpi.b #'.', (a2)
	bne.w keepComponent
	cmpi.b #'.', 1(a2)
	bne.w keepComponent
	cmp.l d4, d3
	bls.w pathBad
popComponent
	subq.l #1, a1
	subq.l #1, d3
	cmp.l d4, d3
	bls.w component
	cmpi.b #'/', (a1)
	bne.w popComponent
	bra.w component
keepComponent
	cmp.l d4, d3
	beq.w copyComponent
	move.b #'/', (a1)+
	addq.l #1, d3
copyComponent
	move.b (a2)+, (a1)+
	addq.l #1, d3
	subq.l #1, d6
	bne.w copyComponent
	bra.w component
pathEnd
	clr.b (a1)
	moveq #0, d0
	bra.w pathDone
pathBad
	moveq #1, d0
pathDone
	movem.l (sp)+, d1-d6/a0-a2
	tst.l d0
	rts
	.bend ; normalizeIncludePath

; The Rust include guard accepts a path only below the including directory or
; a configured include root. Normalize each root before comparing components.
authorizeIncludePath .block
	movem.l d1-d5/a0-a2, -(sp)
	lea SourcePath, a0
	lea AllowedPath, a1
	bsr.w parentPath
	bne.w denied
	bsr.w allowedCandidate
	beq.w permitted
	moveq #0, d5
root
	cmp.l RootCount, d5
	bhs.w denied
	move.l d5, d0
	lsl.l #8, d0
	lea RootPaths, a0
	movea.l memory.Block.Pointer(a0), a0
	adda.l d0, a0
	lea AllowedPath, a1
	move.w #PATH_BYTES-1, d2
copyRootByte
	move.b (a0)+, (a1)+
	beq.w compareRoot
	dbra d2, copyRootByte
	bra.w denied
compareRoot
	bsr.w allowedCandidate
	beq.w permitted
	addq.l #1, d5
	bra.w root
permitted
	moveq #0, d0
	bra.w authorizationDone
denied
	moveq #1, d0
authorizationDone
	movem.l (sp)+, d1-d5/a0-a2
	tst.l d0
	rts
	.bend ; authorizeIncludePath

; AllowedPath is a bounded scratch copy; D0=0 if IncludePath is its child.
allowedCandidate .block
	lea AllowedPath, a0
	bsr.w normalizeIncludePath
	bne.w notAllowed
	lea IncludePath, a0
	lea AllowedPath, a1
compareByte
	move.b (a1)+, d1
	beq.w rootEnd
	move.b (a0)+, d0
	beq.w notAllowed
	bsr.w foldInclude
	move.b d0, d2
	move.b d1, d0
	bsr.w foldInclude
	cmp.b d0, d2
	bne.w notAllowed
	bra.w compareByte
rootEnd
	lea AllowedPath+1, a2
	cmpa.l a2, a1
	beq.w allowed
	move.b (a0), d0
	beq.w allowed
	cmpi.b #'/', d0
	beq.w allowed
	cmpi.b #':', -2(a1)
	beq.w allowed
notAllowed
	moveq #1, d0
	rts
allowed
	moveq #0, d0
	rts
	.bend ; allowedCandidate

; Compare the normalized new path with an active source path that may retain
; spelling from the CLI or discovery roots.
sameNormalizedPath .block
	movem.l d1/a0-a2, -(sp)
	movea.l a0, a2
	movea.l a1, a0
	lea AllowedPath, a1
	move.w #PATH_BYTES-1, d1
copyByte
	move.b (a0)+, (a1)+
	beq.w copied
	dbra d1, copyByte
	moveq #1, d0
	bra.w compared
copied
	lea AllowedPath, a0
	bsr.w normalizeIncludePath
	bne.w distinct
	movea.l a2, a0
	lea AllowedPath, a1
	bsr.w samePath
	bra.w compared
distinct
	moveq #1, d0
compared
	movem.l (sp)+, d1/a0-a2
	tst.l d0
	rts
	.bend ; sameNormalizedPath

; A0/A1=NUL paths, ASCII case-insensitive. D0=0 equal, 1 distinct.
samePath .block
	moveq #0, d0
compare
	move.b (a0)+, d1
	move.b (a1)+, d2
	cmpi.b #'A', d1
	blo.w firstFolded
	cmpi.b #'Z', d1
	bhi.w firstFolded
	addi.b #32, d1
firstFolded
	cmpi.b #'A', d2
	blo.w secondFolded
	cmpi.b #'Z', d2
	bhi.w secondFolded
	addi.b #32, d2
secondFolded
	cmp.b d2, d1
	bne.w distinct
	tst.b d1
	bne.w compare
	rts
distinct
	moveq #1, d0
	rts
	.bend ; samePath

foldInclude .block
	cmpi.b #'A', d0
	blo.w folded
	cmpi.b #'Z', d0
	bhi.w folded
	addi.b #32, d0
folded
	rts
	.bend ; foldInclude

; Save the including reader and enter a child. IncludeHandle is open.
pushInclude .block
	movem.l d1-d3/a0-a2, -(sp)
	move.l IncludeDepthNow, d0
	mulu.w #INCLUDE_FRAME_BYTES, d0
	lea IncludeStack, a2
	adda.l d0, a2
	move.l SourceHandle, IncludeFrame.Handle(a2)
	move.l IoBuffer, IncludeFrame.Buffer(a2)
	move.l IoCursor, IncludeFrame.Cursor(a2)
	move.l IoEnd, IncludeFrame.End(a2)
	addq.l #1, SourceLine
	move.l SourceLine, IncludeFrame.Line(a2)
	move.l OriginId, IncludeFrame.Origin(a2)
	lea SourcePath, a0
	lea IncludeFrame.Path(a2), a1
	move.w #PATH_BYTES/4-1, d0
savePath
	move.l (a0)+, (a1)+
	dbra d0, savePath
	move.l IncludeHandle, SourceHandle
	clr.l IncludeHandle
	lea IncludePath, a0
	lea SourcePath, a1
	move.w #PATH_BYTES/4-1, d0
usePath
	move.l (a0)+, (a1)+
	dbra d0, usePath
	addq.l #1, IncludeDepthNow
	movea.l IoBuffer, a0
	adda.l #IO_BYTES, a0
	move.l a0, IoBuffer
	move.l a0, IoCursor
	move.l a0, IoEnd
	addq.l #1, NextOrigin
	move.l NextOrigin, OriginId
	move.l #1, SourceLine
	clr.l LineUsed
	moveq #0, d0
	movem.l (sp)+, d1-d3/a0-a2
	rts
	.bend ; pushInclude

; Close the child and resume the exact buffered parent position.
popInclude .block
	movem.l d1-d2/a0-a2/a6, -(sp)
	bsr.w closeSource
	bne.w popDone
	subq.l #1, IncludeDepthNow
	move.l IncludeDepthNow, d1
	mulu.w #INCLUDE_FRAME_BYTES, d1
	lea IncludeStack, a2
	adda.l d1, a2
	move.l IncludeFrame.Handle(a2), SourceHandle
	move.l IncludeFrame.Buffer(a2), IoBuffer
	move.l IncludeFrame.Cursor(a2), IoCursor
	move.l IncludeFrame.End(a2), IoEnd
	move.l IncludeFrame.Line(a2), SourceLine
	move.l IncludeFrame.Origin(a2), OriginId
	lea IncludeFrame.Path(a2), a0
	lea SourcePath, a1
	move.w #PATH_BYTES/4-1, d0
restorePath
	move.l (a0)+, (a1)+
	dbra d0, restorePath
	moveq #0, d0
popDone
	movem.l (sp)+, d1-d2/a0-a2/a6
	tst.l d0
	rts
	.bend ; popInclude

; Failure cleanup closes every still-open included parent handle.
closeIncludes .block
	movem.l d1/a6, -(sp)
	movea.l DosBase, a6
	move.l IncludeHandle, d1
	beq.w parents
	clr.l IncludeHandle
	jsr -36(a6)
parents
	move.l IncludeDepthNow, d1
	beq.w closed
	subq.l #1, d1
	mulu.w #INCLUDE_FRAME_BYTES, d1
	lea IncludeStack, a0
	adda.l d1, a0
	move.l IncludeFrame.Handle(a0), d1
	beq.w nextParent
	jsr -36(a6)
nextParent
	subq.l #1, IncludeDepthNow
	bra.w parents
closed
	movem.l (sp)+, d1/a6
	rts
	.bend ; closeIncludes

; No preparation-only include path may remain when packed execution begins.
clearIncludeText .block
	lea IncludeStack, a0
	move.w #INCLUDE_FRAME_BYTES*INCLUDE_DEPTH+PATH_BYTES*3-1, d0
clearByte
	clr.b (a0)+
	dbra d0, clearByte
	rts
	.bend ; clearIncludeText

; Add/extend a compact run of original packed offsets for diagnostics.
appendOrigin .block
	movem.l d1-d3/a0-a2, -(sp)
	lea Records, a0
	move.l memory.Block.Used(a0), d3
	move.l LineOffset, d2
	cmp.l d2, d3
	beq.w originGood
	lea OriginSpans, a0
	move.l OriginCount, d1
	beq.w newOrigin
	subq.l #1, d1
	mulu.w #SPAN_BYTES, d1
	movea.l memory.Block.Pointer(a0), a1
	adda.l d1, a1
	cmp.l Span.End(a1), d2
	bne.w newOrigin
	move.l OriginId, d0
	cmp.l Span.File(a1), d0
	bne.w newOrigin
	move.l d3, Span.End(a1)
	bra.w originGood
newOrigin
	cmpi.l #65534, OriginCount
	bhi.w originBad
	move.l OriginCount, d0
	addq.l #1, d0
	mulu.w #SPAN_BYTES, d0
	jsr memory.reserve
	bne.w originBad
	move.l OriginCount, d1
	mulu.w #SPAN_BYTES, d1
	movea.l memory.Block.Pointer(a0), a1
	adda.l d1, a1
	move.l d2, Span.Start(a1)
	move.l d3, Span.End(a1)
	move.l OriginId, Span.File(a1)
	addi.l #SPAN_BYTES, memory.Block.Used(a0)
	addq.l #1, OriginCount
originGood
	moveq #0, d0
	bra.w originDone
originBad
	moveq #1, d0
originDone
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend ; appendOrigin
