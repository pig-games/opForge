; Closed dependency preparation facets share the producer publication boundary.
; @opforge-owner: opforge.assembler.modules.facets
	.module opforge.assembler.modules.facets
	.cpu 68020
	.use opforge.assembler.modules.templates as templates
	.pub
Frame	.struct
SourceFrontend	.long ?
DestinationFrontend	.long ?
SourceModule	.long ?
DestinationModule	.long ?
	.endstruct
FRAME_BYTES = Frame.DestinationModule+4
OK = 0
INVALID = 1
	.section code, kind=code
; A0=Frame; both frontend owners are distinct, idle and live. D0/CCR=status;
; other registers preserved. Transfer available public macro/segment/statement
; bodies and closed plans, independently of same-name numeric values. No source
; pointer survives; failure requires discarding the destination private session.
import	.block
	movem.l a0-a1, -(sp)
	suba.w #templates.COPY_BYTES, sp
	movea.l a0, a1
	move.l Frame.SourceFrontend(a1), templates.CopyFrame.SourceFrontend(sp)
	move.l Frame.SourceModule(a1), templates.CopyFrame.SourceModule(sp)
	move.l Frame.DestinationFrontend(a1), templates.CopyFrame.DestinationFrontend(sp)
	move.l Frame.DestinationModule(a1), templates.CopyFrame.DestinationModule(sp)
	movea.l sp, a0
	jsr templates.copyModule
	adda.w #templates.COPY_BYTES, sp
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; import
	.endsection
	.endmodule
