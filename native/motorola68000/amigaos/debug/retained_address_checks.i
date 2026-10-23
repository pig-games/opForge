; An immutable incoming address remains symbolic through normalization and
; layout replay; an ordinary readonly alias must wait for layout execution.
retainedAddressChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.w #1, RetainedHeader+retainedPackage.Header.NameCount
	move.l #RetainedHeader, RetainedContext+retainedPackage.Context.Package
	move.l #3, RetainedContext+retainedPackage.Context.Count
	move.l #RetainedCells, RetainedContext+retainedPackage.Context.Values
	move.l #RetainedDefined, RetainedContext+retainedPackage.Context.Defined
	move.l #RetainedSections, RetainedContext+retainedPackage.Context.SectionIds
	move.l #$1234, RetainedCells+retainedRuntime.VALUE_BYTES
	move.b #retainedDependencies.RETAINED_ADDRESS, RetainedDefined+1
	move.b #1, RetainedSections+1
	moveq #1, d7
again
	; This non-retained cell must disappear during replay. The seeded address
	; is initialized only once, so the second session also checks its lifetime.
	move.l #$7777, RetainedCells
	move.b #1, RetainedDefined
	move.b #2, RetainedSections
	move.l #$ffff, RetainedCells+2*retainedRuntime.VALUE_BYTES
	lea RetainedAlias, a0
	move.l #RetainedAliasEnd-RetainedAlias, d0
	suba.l a1, a1
	moveq #0, d1
	lea RetainedContext, a2
	jsr retainedDependencies.resolve
	bne.w bad
	lea RetainedContext, a1
	jsr retainedDependencies.observationMetadata
	bne.w bad
	cmpi.b #retainedGraph.LABEL, 1(a3)
	bne.w bad
	cmpi.b #retainedGraph.LAYOUT, 2(a3)
	bne.w bad
	tst.b RetainedDefined+2
	bne.w bad
	tst.l RetainedCells+2*retainedRuntime.VALUE_BYTES
	bne.w bad
	lea RetainedContext, a1
	jsr retainedDependencies.resetLayout
	bne.w bad
	cmpi.b #retainedDependencies.RETAINED_ADDRESS, RetainedDefined+1
	bne.w bad
	cmpi.b #1, RetainedSections+1
	bne.w bad
	cmpi.l #$1234, RetainedCells+retainedRuntime.VALUE_BYTES
	bne.w bad
	tst.l RetainedCells
	bne.w bad
	tst.b RetainedDefined
	bne.w bad
	tst.b RetainedSections
	bne.w bad
	jsr retainedDependencies.release
	dbra d7, again
	moveq #0, d7
	bra.w cleanup
bad
	moveq #1, d7
cleanup
	jsr retainedDependencies.release
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; retainedAddressChecks
	.endsection
	.section data, kind=data
	.priv
RetainedAlias
	.byte RetainedAliasEnd-RetainedAlias-1, 0
	.word 1
	.byte 0
	.word 2
	.byte 0, source.TOKEN_CONSTANT_DECLARATION
	.byte retainedExpression.COMPILED_TAG, 4
	.byte retainedRuntime.EXPRVM_OPCODE_PUSH_SYMBOL, 1, 0
	.byte retainedRuntime.EXPRVM_OPCODE_END
RetainedAliasEnd
	.endsection
	.section bss, kind=bss
	.priv
	.align 4
RetainedHeader	.res byte, retainedPackage.HEADER_BYTES
RetainedContext	.res byte, retainedPackage.CONTEXT_BYTES
RetainedCells	.res byte, 3*retainedRuntime.VALUE_BYTES
RetainedDefined	.res byte, 3
RetainedSections	.res byte, 3
	.endsection
	.section code, kind=code
	.priv
