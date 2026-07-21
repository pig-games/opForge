; Temporary engine-context adapter for the tkpkg expression service.

	.module tkpkg.amigaos.expression_context
	.cpu 68020
	.pub
	.use opasm.amigaos.engine

	.section code, kind=code

; ---------------------------------------------------------------------------
; Load the native expression context from the current assembly session.
;
; This is the named transition boundary until Item 5.7.1 supplies the neutral
; runtime-context ABI. Expression consumers must not address engine storage.
;
; Inputs:
; - none.
;
; Outputs:
; - D6: current session pass.
; - A6: finalized-label table pointer.
;
; Clobbers:
; - D6/A6/CCR.
;
; CCR:
; - Unspecified on return.
; ---------------------------------------------------------------------------
loadV1	.block
	moveq #0, d6
	move.w engine.opasmEngineSessionPass.l, d6
	lea engine.opasmEngineLabelFinalizedTable.l, a6
	rts
	.bend  ; loadV1

	.endsection
	.endmodule
