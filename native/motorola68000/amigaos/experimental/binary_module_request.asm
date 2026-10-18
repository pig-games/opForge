; First-reference ownership for an immutable, configured module image.
; The compiler owns language processing; this boundary owns publication/reuse.
	.module experimental.amigaos.binary_module_request
	.cpu 68020
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_module_image as image
	.pub
EMPTY = 0
BUILDING = 1
READY = 2
FAILED = 3
STATUS_OK = 0
STATUS_FAILED = 1
STATUS_MISSING = 2
STATUS_BUSY = 3
State	.struct
Phase	.long ?
Failure	.long ?  ; latched rejection; BUILDING retains sole cleanup ownership
Image	.res memory.Block.Used+4
	.endstruct
STATE_BYTES = State.Image+memory.Block.Used+4
	.section code, kind=code
; A0=zero-initialized State,A1=compiler context,A2=compiler,D1=block identity.
; Each State belongs to one already validated configured module. The caller owns
; configuration equality; READY requests ignore compiler/context and retain neither.
; Compiler(A0=context,A1=owned memory.Block) returns D0/CCR=status and preserves
; all other registers. It transfers one complete image allocation, including its
; exact Used and Capacity, and releases every other private owner before return.
; It must preserve current language semantics and cannot retain source borrows.
; Success: D0=0,A1=image base,D1=image bytes,A2=block bytes,D2=block length.
; Failure: D0=status; A1/A2/D1/D2 unspecified. Other registers preserved.
; Images are immutable through release; returned views cannot outlive State.
; Compilation happens once per ownership interval. Failure is sticky, including
; recursive requests whose rejection was ignored by the compiler callback.
acquire	.block
	movem.l d3-d5/a0/a3-a6, -(sp)
	movea.l a0, a6
	move.l d1, d5
	cmpi.l #READY, State.Phase(a6)
	beq.w lookup
	cmpi.l #EMPTY, State.Phase(a6)
	beq.w compile
	cmpi.l #BUILDING, State.Phase(a6)
	bne.w returnFailure
	move.l #1, State.Failure(a6)
	moveq #STATUS_BUSY, d0
	bra.w done
compile
	; An EMPTY state cannot hide an allocation from an earlier owner.
	tst.l State.Image+memory.Block.Pointer(a6)
	bne.w failOwner
	tst.l State.Image+memory.Block.Capacity(a6)
	bne.w failOwner
	tst.l State.Image+memory.Block.Used(a6)
	bne.w failOwner
	move.l a2, d0
	beq.w failOwner
	movea.l a2, a3
	movea.l a1, a0
	lea State.Image(a6), a1
	move.l #BUILDING, State.Phase(a6)
	clr.l State.Failure(a6)
	jsr (a3)
	tst.l d0
	bne.w failOwner
	cmpi.l #BUILDING, State.Phase(a6)
	bne.w failOwner
	tst.l State.Failure(a6)
	bne.w failOwner
	move.l State.Image+memory.Block.Used(a6), d0
	cmp.l State.Image+memory.Block.Capacity(a6), d0
	bhi.w failOwner
	movea.l State.Image+memory.Block.Pointer(a6), a0
	jsr image.validate
	bne.w failOwner
	move.l #READY, State.Phase(a6)
lookup
	movea.l State.Image+memory.Block.Pointer(a6), a0
	move.l d5, d1
	jsr image.findValidated
	bne.w missing
	movea.l a1, a2
	move.l d1, d2
	movea.l State.Image+memory.Block.Pointer(a6), a1
	move.l State.Image+memory.Block.Used(a6), d1
	moveq #STATUS_OK, d0
	bra.w done
missing
	moveq #STATUS_MISSING, d0
	bra.w done
failOwner
	lea State.Image(a6), a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	move.l #FAILED, State.Phase(a6)
returnFailure
	moveq #STATUS_FAILED, d0
done
	movem.l (sp)+, d3-d5/a0/a3-a6
	tst.l d0
	rts
	.bend  ; acquire

; A0=State. Release its image and invalidate all borrowed views. D0/CCR=status;
; other registers preserved. Idempotent except BUILDING, which rejects and latches
; failure for the outer acquire; that acquire still owns compiler cleanup.
release	.block
	movem.l a0-a1, -(sp)
	cmpi.l #BUILDING, State.Phase(a0)
	beq.w busy
	movea.l a0, a1
	adda.w #State.Image, a0
	jsr memory.release
	clr.l memory.Block.Used(a0)
	clr.l State.Phase(a1)
	clr.l State.Failure(a1)
	moveq #STATUS_OK, d0
	bra.w done
busy
	move.l #1, State.Failure(a0)
	moveq #STATUS_BUSY, d0
done
	movem.l (sp)+, a0-a1
	tst.l d0
	rts
	.bend  ; release
	.endsection
	.endmodule
