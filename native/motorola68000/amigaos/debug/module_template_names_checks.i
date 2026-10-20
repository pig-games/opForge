; Exercise owned template origins with a destination canonical spelling whose
; existing leaf must not change. No bytes enter the identity result artifact.
templateNameChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.l #31, ReturnCode
	lea TemplateSourceScope, a0
	move.l #NAME_COUNT, d0
	moveq #0, d1
	move.w Package+package.Header.EndDirective, d1
	jsr scopes.begin
	bne.w bad
	lea TemplateTargetScope, a0
	move.l #NAME_COUNT, d0
	jsr scopes.begin
	bne.w bad
	move.l #32, ReturnCode
	; The destination spelling has an independently redirected numeric value.
	lea TemplateTargetScope, a1
	lea TemplateOriginName, a0
	moveq #TemplateOriginNameEnd-TemplateOriginName, d0
	jsr scopes.bindOrigin
	bne.w bad
	move.l d1, d4
	lea TemplateValueName, a0
	moveq #TemplateValueNameEnd-TemplateValueName, d0
	jsr scopes.bindOrigin
	bne.w bad
	move.l d1, TemplateValueTarget
	lea TemplateTargetScope+scopeLayout.LEXICAL_STATE, a0
	lea TemplateEntry, a1
	move.l d4, d0
	subi.l #NAME_COUNT, d0
	jsr lexical.readEntry
	bne.w bad
	move.w TemplateValueTarget+2, records.Entry.Target(a1)
	move.l d4, d0
	subi.l #NAME_COUNT, d0
	jsr lexical.writeEntry
	bne.w bad
	move.l #33, ReturnCode
	lea TemplateSourceScope, a0
	lea TemplateOriginName, a1
	moveq #TemplateOriginNameEnd-TemplateOriginName, d0
	moveq #6, d1
	moveq #0, d2
	jsr scopes.bindTemplateOrigin
	bne.w bad
	move.l d1, d7
	move.l #34, ReturnCode
	lea TemplateCapture, a0
	lea TemplateSourceScope, a1
	jsr templateNames.begin
	bne.w bad
	move.l #35, ReturnCode
	move.l d7, d0
	moveq #templateNames.VALUE, d2
	jsr templateNames.captureResolve
	bne.w bad
	move.l d1, d5
	move.l d7, d0
	jsr templateNames.captureResolve
	bne.w bad
	cmp.l d5, d1
	bne.w bad
	move.l #36, ReturnCode
	move.l d7, d0
	moveq #templateNames.MEMBER, d2
	jsr templateNames.captureResolve
	bne.w bad
	subq.l #1, d1
	cmp.l d5, d1
	bne.w bad
	cmpi.l #templateNames.ROW_BYTES*2, TemplateCapture+templateNames.State.Rows+memory.Block.Used
	bne.w bad
	; The spelling transport must survive retirement of its original owner.
	lea TemplateSourceScope, a0
	jsr scopes.release
	move.l #TemplateTargetScope, TemplateInstall+templateNames.InstallState.Scope
	move.l #NAME_COUNT, TemplateInstall+templateNames.InstallState.Base
	move.l TemplateCapture+templateNames.State.Rows+memory.Block.Pointer, TemplateInstall+templateNames.InstallState.Rows
	move.l TemplateCapture+templateNames.State.Rows+memory.Block.Used, TemplateInstall+templateNames.InstallState.RowBytes
	move.l TemplateCapture+templateNames.State.Names+memory.Block.Pointer, TemplateInstall+templateNames.InstallState.Names
	move.l TemplateCapture+templateNames.State.Names+memory.Block.Used, TemplateInstall+templateNames.InstallState.NameBytes
	move.l #37, ReturnCode
	lea TemplateInstall, a0
	jsr templateNames.install
	bne.w bad
	move.l #38, ReturnCode
	move.l d5, d0
	jsr templateNames.canonical
	bne.w bad
	move.l d1, d6
	move.l d5, d0
	moveq #templateNames.VALUE, d2
	jsr templateNames.installResolve
	bne.w bad
	move.l d1, d4
	cmp.l d6, d4
	beq.w bad
	lea TemplateTargetScope+scopeLayout.LEXICAL_STATE, a0
	lea TemplateEntry, a1
	move.l d6, d0
	subi.l #NAME_COUNT, d0
	jsr lexical.readEntry
	bne.w bad
	tst.w records.Entry.Leaf(a1)
	bne.w bad
	; Only the canonical independent template facet declares availability.
	move.w #records.TEMPLATE_DECLARED, records.Entry.TemplateFlags(a1)
	move.l d6, d0
	subi.l #NAME_COUNT, d0
	jsr lexical.writeEntry
	bne.w bad
	move.l d4, d0
	subi.l #NAME_COUNT, d0
	jsr lexical.readEntry
	bne.w bad
	cmpi.w #6, records.Entry.Leaf(a1)
	bne.w bad
	move.l TemplateValueTarget, d0
	cmp.w records.Entry.Target(a1), d0
	bne.w bad
	cmp.w records.Entry.OriginalId(a1), d6
	bne.w bad
	tst.w records.Entry.Flags(a1)
	bne.w bad
	cmpi.w #scopes.KIND_TEMPLATE_ORIGIN, records.Entry.ScopeKind(a1)
	bne.w bad
	move.l #39, ReturnCode
	lea TemplateTargetScope, a0
	move.l d4, d0
	jsr scopes.templateAliasTarget
	bne.w bad
	cmp.l d6, d1
	bne.w bad
	move.l #40, ReturnCode
	move.w d4, TemplateCall+source.NameToken.Id
	lea TemplateCall, a0
	lea TemplateTargetScope, a1
	jsr scopes.resolveTemplate
	bne.w bad
	cmp.l d6, d1
	bne.w bad
	lea TemplateTargetScope+scopeLayout.LEXICAL_STATE, a0
	lea TemplateEntry, a1
	move.l d4, d0
	subi.l #NAME_COUNT, d0
	jsr lexical.readEntry
	bne.w bad
	ori.w #records.REFERENCED, records.Entry.Flags(a1)
	move.l d4, d0
	subi.l #NAME_COUNT, d0
	jsr lexical.writeEntry
	bne.w bad
	lea TemplateTargetScope, a0
	move.l #41, ReturnCode
	jsr scopes.finishBindings
	bne.w bad
	move.l d4, d0
	jsr scopes.templateAliasTarget
	bne.w bad
	cmp.l d6, d1
	bne.w bad
	lea TemplateInstall, a0
	move.l #42, ReturnCode
	move.l d5, d0
	addq.l #1, d0
	moveq #templateNames.MEMBER, d2
	jsr templateNames.installResolve
	bne.w bad
	move.l d1, d0
	subi.l #NAME_COUNT, d0
	lea TemplateTargetScope+scopeLayout.LEXICAL_STATE, a0
	lea TemplateEntry, a1
	jsr lexical.readEntry
	bne.w bad
	cmpi.w #5, records.Entry.Length(a1)
	bne.w bad
	tst.w records.Entry.Leaf(a1)
	bne.w bad
	lea TemplateInstall, a0
	move.l #43, ReturnCode
	move.l #MOVE_ID, d0
	moveq #templateNames.VALUE, d2
	jsr templateNames.installResolve
	bne.w bad
	cmpi.l #MOVE_ID, d1
	bne.w bad
	; Import proxies reuse ScopeKind for their original entry index+1. The
	; value four must not become a template origin with OriginalId zero.
	move.l #44, ReturnCode
	move.l d6, d0
	subi.l #NAME_COUNT, d0
	lea TemplateTargetScope+scopeLayout.LEXICAL_STATE, a0
	lea TemplateEntry, a1
	jsr lexical.readEntry
	bne.w bad
	move.w #records.IMPORT_PROXY+scopes.DECLARED, records.Entry.Flags(a1)
	move.w #scopes.KIND_TEMPLATE_ORIGIN, records.Entry.ScopeKind(a1)
	clr.w records.Entry.OriginalId(a1)
	move.l d6, d0
	subi.l #NAME_COUNT, d0
	jsr lexical.writeEntry
	bne.w bad
	lea TemplateTargetScope, a0
	jsr scopes.finishBindings
	bne.w bad
	move.l d6, d0
	moveq #1, d1
	jsr scopes.rebindLocal
	bne.w bad
	cmp.l d6, d1
	bne.w bad
	moveq #0, d7
	bra.w cleanup
bad
	moveq #1, d7
cleanup
	lea TemplateInstall, a0
	jsr templateNames.releaseInstall
	lea TemplateCapture, a0
	jsr templateNames.release
	lea TemplateTargetScope, a0
	jsr scopes.release
	lea TemplateSourceScope, a0
	jsr scopes.release
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; templateNameChecks
	.endsection
	.section data, kind=data
TemplateOriginName	.byte "outer.local"
TemplateOriginNameEnd
TemplateValueName	.byte "outer.value"
TemplateValueNameEnd
TemplateCall	.byte abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 0
	.byte 1
	.endsection
	.section bss, kind=bss
	.align 4
TemplateCapture	.res byte, templateNames.STATE_BYTES
TemplateInstall	.res byte, templateNames.INSTALL_BYTES
TemplateSourceScope	.res byte, scopes.SCRATCH_BYTES
TemplateTargetScope	.res byte, scopes.SCRATCH_BYTES
TemplateEntry	.res byte, records.ENTRY_BYTES
TemplateValueTarget	.res long, 1
	.endsection
	.section code, kind=code
