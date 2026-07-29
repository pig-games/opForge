; FS-UAE regression harness for the converted native CLI debug-header event.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module cli.debug.event.harness
	.cpu 68020

	.use opforge.cli.constants
	.use opforge.cli.run
	.use opforge.cli.state
	.use opforge.debug.contracts as contracts
	.use opforge.debug.events as events

HARNESS_FAIL = 20

	.section entry, kind=code
	.pub

; Execute the real CLI debug branch and validate its structured event.
; Inputs: none.
; Outputs: D0 = zero on success, HARNESS_FAIL on mismatch.
; Clobbers: D0-D2/A0/CCR.
; CCR: reflects D0 on return.
start	.block
	jsr events.debugEventReset
	jsr run.opforgeNativeCliRun

	cmpi.w #1, events.DebugEventCount
	bne.s fail
	lea events.DebugEventBuffer, a0
	cmpi.w #contracts.EVENT_CLI_DEBUG_HEADER, events.DEBUG_EVENT_KIND(a0)
	bne.s fail
	tst.w events.DEBUG_EVENT_CONTRACT_ID(a0)
	bne.s fail
	cmpi.w #1, events.DEBUG_EVENT_ROUTINE_ID(a0)
	bne.s fail
	cmpi.l #1, events.DEBUG_EVENT_ARG0(a0)
	bne.s fail
	cmpi.l #constants.NATIVE_OUTPUT_FORMAT_BIN, events.DEBUG_EVENT_ARG1(a0)
	bne.s fail
	cmpi.l #state.NativeCliInputPath, events.DEBUG_EVENT_ARG2(a0)
	bne.s fail
	cmpi.l #state.NativeCliBinPath, events.DEBUG_EVENT_ARG3(a0)
	bne.s fail

	moveq #0, d0
	rts

fail
	moveq #HARNESS_FAIL, d0
	rts
	.bend  ; start

	.endsection

	.output "build/cli_debug_event_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
