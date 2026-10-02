; Manifest-backed test entry for the shared compact binary-source engine.
	.module main
	.cpu 68020
	.use experimental.amigaos.binary_app as app
.ifdef OPFORGE_CAPTURE_REPLAY_TEST
	.use experimental.amigaos.binary_frontend as frontend
	.use experimental.amigaos.binary_capture as capture
	.use experimental.amigaos.binary_memory as memory
.endif
	.section entry, kind=code
	.pub
start	.block
	lea Config, a0
	move.l #InputPath, app.Frame.PackagePath(a0)
	clr.l app.Frame.SourcePath(a0)
	move.l #OutputPath, app.Frame.OutputPath(a0)
	clr.w app.Frame.Mode(a0)
.ifdef OPFORGE_CAPTURE_REPLAY_TEST
	move.l #replay, app.Frame.LowerLine(a0)
.endif
	jsr app.execute
	rts
	.bend  ; start
.ifdef OPFORGE_CAPTURE_REPLAY_TEST
; Prove capture ownership before replay: move the whole block, free its original
; allocation, overwrite the caller's source, and clear the frontend source view.
; Input selection and expected output remain the caller's ordinary manifest.
replay	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	lea CaptureRequest, a1
	move.l #Captured, capture.Frame.Arena(a1)
	jsr frontend.captureLine
	bne.w done
	move.l d1, d7
	lea Captured, a0
	move.l memory.Block.Used(a0), d6
	lea Relocated, a0
	move.l d6, d0
	jsr memory.reserveExact
	bne.w done
	move.l d6, memory.Block.Used(a0)
	movea.l memory.Block.Pointer(a0), a2
	lea Captured, a0
	movea.l memory.Block.Pointer(a0), a1
copy
	move.b (a1)+, (a2)+
	subq.l #1, d6
	bne.w copy
	jsr memory.release
	movea.l frontend.Frame.Source(a5), a1
	move.l frontend.Frame.SourceBytes(a5), d6
	beq.w poisoned
poison
	move.b #$a5, (a1)+
	subq.l #1, d6
	bne.w poison
poisoned
	clr.l frontend.Frame.Source(a5)
	clr.l frontend.Frame.SourceBytes(a5)
	movea.l a5, a0
	lea Relocated, a1
	move.l d7, d1
	jsr frontend.replayLine
done
	move.l d0, d7
	lea Captured, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	lea Relocated, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; replay
.endif
	.endsection
	.section data, kind=data
InputPath	.byte "Work:input.bin", 0
OutputPath	.byte "Work:output.bin", 0
	.endsection
	.section bss, kind=bss
	.align 4
Config	.res byte, app.FRAME_BYTES
.ifdef OPFORGE_CAPTURE_REPLAY_TEST
CaptureRequest	.res byte, capture.FRAME_BYTES
Captured	.res byte, memory.Block.Used+4
Relocated	.res byte, memory.Block.Used+4
.endif
	.endsection
	.output "build/binary_source_harness", format=hunk, sections=entry, code, data, bss
	.endmodule
