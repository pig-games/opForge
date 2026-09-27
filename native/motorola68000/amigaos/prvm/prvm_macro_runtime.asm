; Thin shared invocation boundary for package-selected macro PRVM services.
	.module prvm.amigaos.macro_runtime
	.cpu 68020
	.use prvm.amigaos.abi as abi
	.use prvm.amigaos.macro_descriptors as macro_descriptors
	.use prvm.amigaos.packed_macro as packed_macro
	.use prvm.amigaos.macro_fragments as fragments
	.include "telemetry_macros.i"
	.section code, kind=code
	.pub

; Execute macro entry 2, 3 or 4; each executor owns its request/program validation.
; Inputs: A0 request frame, D0 available frame bytes.
; Outputs: D0 status, D1 record count, D2 error offset, D3 published bytes.
; Clobbers: A0-A3; preserves D4-D7/A4-A6. Executors stage atomic publication.
; CCR: reflects D0. One profiling invocation surrounds each service call.
run	.block
	movem.l d4-d7/a4-a6, -(sp)
	.TELEMETRY_VM_ENTER runtime_profile.OPFORGE_RUNTIME_VM_PRVM, runtime_profile.OPFORGE_RUNTIME_PROGRAM_PARSER
	move.l a0, d1
	beq invalidArgument
	cmpi.l #abi.PRVM_REQUEST_FRAME_SIZE, d0
	blt invalidArgument
	cmpi.w #abi.PRVM_ENTRY_KIND_MACRO_DESCRIPTORS, abi.PRVM_FRAME_ENTRY_KIND(a0)
	beq descriptors
	cmpi.w #abi.PRVM_ENTRY_KIND_MACRO_FRAGMENTS, abi.PRVM_FRAME_ENTRY_KIND(a0)
	beq fragmentEntry
	cmpi.w #abi.PRVM_ENTRY_KIND_PACKED_MACRO, abi.PRVM_FRAME_ENTRY_KIND(a0)
	bne invalidArgument
	jsr packed_macro.run
	bra done
fragmentEntry
	jsr fragments.run
	bra done
descriptors
	jsr macro_descriptors.run
	bra done
invalidArgument
	clr.l d1
	clr.l d2
	clr.l d3
	moveq #abi.PRVM_STATUS_INVALID_ARGUMENT, d0
done
	.TELEMETRY_VM_LEAVE
	movem.l (sp)+, d4-d7/a4-a6
	tst.l d0
	rts
	.bend  ; run
	.endsection
	.endmodule
