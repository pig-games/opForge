; Experimental shared binary macro services. Acquisition remains eager.
; @opforge-owner: prvm.amigaos.binary_macro_runtime
	.module prvm.amigaos.binary_macro_runtime
	.cpu 68020
	.use prvm.amigaos.binary_macro_descriptors as descriptors
	.use prvm.amigaos.binary_macro_fragments as fragments
	.use prvm.amigaos.abi as abi
	.include "telemetry_macros.i"
	.section code, kind=code
	.pub
; A0 shared request,D0 available frame bytes. Outputs D0 status/CCR,D1 count,
; D2 error,D3 published bytes; preserves D4-D7/A4-A6. Descriptor input is a view.
run	.block
	.TELEMETRY_VM_ENTER runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	jsr descriptors.run
	.TELEMETRY_VM_LEAVE
	tst.l d0
	rts
	.bend  ; run
; Same ABI, D1/D2 select the spelling range; fragment coordinates are relative.
runRange	.block
	.TELEMETRY_VM_ENTER runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	jsr fragments.runRange
	.TELEMETRY_VM_LEAVE
	tst.l d0
	rts
	.bend  ; runRange
	.endsection
	.endmodule
