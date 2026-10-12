; Acquire current tokenizer bytecode/control from a validated source capsule.
; Capsule metadata describes the shared TKVM state table, never source grammar.
; @opforge-owner: experimental.amigaos.binary_tokenizer_capsule
.module experimental.amigaos.binary_tokenizer_capsule
.cpu 68020
.use experimental.amigaos.binary_package as package
.use prvm.amigaos.statement_inputs as inputs
.use tkvm.amigaos.control as control
.pub
Bundle	.struct
ContainerPrefix	.res 2  ; container-owned bytes, unused by this transport
Start	.word ?
States	.word ?
Reserved	.word ?
StepBudget	.long ?
.endstruct
BUNDLE_BYTES = Bundle.StepBudget+4
INVALID_PROGRAM = 6
.section code, kind=code
; A0=validated capsule. D0/status,A1=bytecode,D1=bytecode bytes on success.
; CCR reflects D0; others preserved. Installs the same capsule's state table
; and step budget; caller keeps the immutable capsule alive throughout TKVM runs.
activate	.block
	movem.l d2-d7/a0/a2-a4, -(sp)
	movea.l a0, a4
	move.l #package.HEADER_BYTES, d0
	jsr inputs.span
	bne.w invalid
	move.l package.Header.Bytes(a4), d0
	jsr inputs.span
	bne.w invalid
	move.l package.Header.Tokenizer(a4), d2
	cmpi.l #package.HEADER_BYTES, d2
	blo.w invalid
	move.l package.Header.Bytes(a4), d3
	sub.l d2, d3
	bcs.w invalid
	move.l package.Header.TokenizerBytes(a4), d4
	cmp.l d3, d4
	bhi.w invalid
	cmpi.l #BUNDLE_BYTES, d4
	bls.w invalid
	movea.l a4, a2
	adda.l d2, a2
	movea.l a2, a0
	move.l d4, d0
	jsr inputs.span
	bne.w invalid
	moveq #0, d5
	move.w Bundle.States(a2), d5
	beq.w invalid
	moveq #0, d6
	move.w Bundle.Start(a2), d6
	cmp.l d5, d6
	bhs.w invalid
	move.l d5, d2
	lsl.l #2, d2
	add.l #BUNDLE_BYTES, d2
	cmp.l d4, d2
	bhs.w invalid
	sub.l d2, d4
	lea BUNDLE_BYTES(a2), a1
	move.l d5, d3
states
	move.l (a1)+, d0
	cmp.l d4, d0
	bhs.w invalid
	subq.l #1, d3
	bne.w states
	move.l Bundle.StepBudget(a2), d0
	ble.w invalid
	jsr control.tkvmSetStepBudget68000
	lea BUNDLE_BYTES(a2), a0
	move.l d5, d0
	move.l d6, d1
	jsr control.tkvmSetProgramStateTable68000
	movea.l a2, a1
	adda.l d2, a1
	move.l d4, d1
	moveq #0, d0
	bra.w done
invalid
	moveq #INVALID_PROGRAM, d0
done
	tst.l d0
	movem.l (sp)+, d2-d7/a0/a2-a4
	rts
	.bend  ; activate
.endsection
.endmodule
