; FS-UAE harness for embedded 65C02 pipeline selection.
; @opforge-evidence: level=D; role=focused-contract; authority=focused-contract; lifecycle=permanent

	.module pipeline.select.harness
	.cpu 68020

	.use opforge.cli.copy
	.use opforge.cli.package_pipeline
	.use opforge.cli.state
	.use opforge.debug.contracts as contracts
	.use opforge.debug.events as events

HARNESS_FAIL = 20
	.section entry, kind=code
	.pub

; Initialize the embedded package with a 65C02 request and inspect the event.
; Inputs: none.
; Outputs: D0 = zero on success, HARNESS_FAIL on mismatch.
; Clobbers: D0-D1/A0-A1/CCR.
; CCR: reflects D0 on return.
start	.block
	lea CpuText.l, a0
	lea state.NativeCliCpuName, a1
	jsr copy.copyCString
	jsr package_pipeline.opforgeNativeCliInitPackagePipeline
	bne.s fail
	cmpi.w #1, events.DebugEventCount
	bne.s fail
	lea events.DebugEventBuffer, a0
	cmpi.w #contracts.EVENT_PIPELINE_SELECT, events.DEBUG_EVENT_KIND(a0)
	bne.s fail
	tst.l events.DEBUG_EVENT_ARG1(a0)
	bne.s fail
	cmpi.l #'6', events.DEBUG_EVENT_ARG2(a0)
	bne.s fail

	moveq #0, d0
	rts

fail
	moveq #HARNESS_FAIL, d0
	rts
	.bend  ; start
	.endsection

	.section data, kind=data
CpuText
	.byte "65c02", 0
	.endsection

	.output "build/pipeline_select_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
