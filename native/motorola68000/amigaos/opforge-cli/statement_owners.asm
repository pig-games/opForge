; Retained-statement module ownership for the native AmigaOS CLI.
; @opforge-owner: opforge.cli.assembly_session
; @opforge-slice: documentation/plans/slices/native-porting-slice-opasm-layout.toml
; @opforge-role: delegation

	.module opforge.cli.statement_owners
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.state
	.use opforge.cli.copy

	.section code, kind=code
	.pub

; Reset the module-owner stack that follows the retained statement stream.
opforgeNativeCliResetStatementOwnersV1	.block
	moveq #0, d0
	move.w d0, AssemblyOwnerDepth.l
	rts
	.bend  ; opforgeNativeCliResetStatementOwnersV1

; Advance the retained-statement owner stack and return its current owner.
; Module openers own themselves; module closers retain their owner until the
; statement has been stored successfully.
; Outputs: D0 = status; A1/D1 = owner slice, or D1 = 0 outside a module.
opforgeNativeCliPrepareStatementOwnerV1	.block
	move.w state.NativeCliStmtDirectiveKind, d0
	cmpi.w #constants.NCLI_PARSER_DIRECTIVE_ENDMODULE, d0
	bne.s currentStatementOwnerV1
	moveq #0, d0
	move.w AssemblyOwnerDepth.l, d0
	beq.s ownerStateInvalid

currentStatementOwnerV1
	movem.l d2-d3, -(sp)
	moveq #0, d2
	move.w AssemblyOwnerDepth.l, d2
	beq.s ownerNone
	subq.w #1, d2
	move.l d2, d3
	add.l d3, d3
	lea AssemblyOwnerLengthStack.l, a0
	moveq #0, d1
	move.w 0(a0, d3.l), d1
	lsl.l #6, d2
	lea AssemblyOwnerNameStack.l, a1
	adda.l d2, a1
	moveq #0, d0
	bra.s ownerCurrentReturn
ownerNone
	moveq #0, d0
	moveq #0, d1
	suba.l a1, a1
ownerCurrentReturn
	movem.l (sp)+, d2-d3
	rts
ownerStateInvalid
	moveq #1, d0
	moveq #0, d1
	suba.l a1, a1
	rts
	.bend  ; opforgeNativeCliPrepareStatementOwnerV1

; Retain the canonical module token before compatibility emitters reuse CLI
; scratch storage. Inputs: A0/D1 = module name. Output: D0 = status.
opforgeNativeCliOpenStatementOwnerV1	.block
	movem.l d1-d6/a0-a2, -(sp)
	movea.l a0, a2
	move.l d1, d5
	beq.s ownerPushFail
	cmpi.l #constants.TOKEN_BUFFER_CAPACITY, d5
	bhs.s ownerPushFail
	moveq #0, d2
	move.w AssemblyOwnerDepth.l, d2
	cmpi.w #constants.NATIVE_MODULE_SCAN_DEPTH_CAPACITY, d2
	bhs.s ownerPushFail
	move.l d2, d4
	move.l d2, d6
	lsl.l #6, d6
	lea AssemblyOwnerNameStack.l, a1
	adda.l d6, a1
	movea.l a2, a0
	move.l d5, d0
	jsr copy.copyFixedString
	clr.b (a1)
	move.l d4, d6
	add.l d6, d6
	lea AssemblyOwnerLengthStack.l, a0
	move.w d5, 0(a0, d6.l)
	addq.w #1, d2
	move.w d2, AssemblyOwnerDepth.l
	moveq #0, d0
	bra.s ownerPushReturn
ownerPushFail
	moveq #1, d0
ownerPushReturn
	movem.l (sp)+, d1-d6/a0-a2
	rts
	.bend  ; opforgeNativeCliOpenStatementOwnerV1

; Close the current retained-statement owner after the caller has confirmed
; that an authoritative retained `.endmodule` request was stored.
opforgeNativeCliFinishStatementOwnerV1	.block
	moveq #0, d0
	move.w AssemblyOwnerDepth.l, d0
	beq.s ownerFinishFail
	subq.w #1, d0
	move.w d0, AssemblyOwnerDepth.l
	moveq #0, d0
	rts
ownerFinishFail
	moveq #1, d0
	rts
	.bend  ; opforgeNativeCliFinishStatementOwnerV1

	.endsection

	.section bss, kind=bss
AssemblyOwnerDepth
	.res word, 1
AssemblyOwnerLengthStack
	.res word, constants.NATIVE_MODULE_SCAN_DEPTH_CAPACITY
AssemblyOwnerNameStack
	.res byte, constants.NATIVE_MODULE_SCAN_DEPTH_CAPACITY * constants.TOKEN_BUFFER_CAPACITY
	.endsection
	.endmodule
