; Conservative private-cache fingerprint of ordered acquisition bytes.
; Two independent 32-bit streaming hashes; not a security/authentication digest.
	.module experimental.amigaos.binary_leaf_key
	.cpu 68020
	.pub
State	.struct
High	.long ?
Low	.long ?
	.endstruct
STATE_BYTES = State.Low+4
	.section code, kind=code
; A0=State. Reset the current producer contract/input fingerprint.
; All registers preserved; CCR unspecified. Cache compatibility is independent
; of VM versioning: an incompatible producer changes these private seeds.
begin	.block
	move.l #$811c9dc5, State.High(a0)
	move.l #$4c463034, State.Low(a0)
	rts
	.bend  ; begin

; A0=State,A1=bytes,D0=extent. Mix an extent-delimited byte string, including
; zero-length records. Other registers preserved; CCR unspecified.
update	.block
	movem.l d0-d5/a0-a1, -(sp)
	move.l d0, d5
	move.l State.High(a0), d1
	move.l State.Low(a0), d2
	move.l d0, d4
	moveq #3, d3
length
	moveq #0, d0
	move.b d4, d0
	bsr.w byte
	lsr.l #8, d4
	dbra d3, length
next
	tst.l d5
	beq.w done
	moveq #0, d0
	move.b (a1)+, d0
	bsr.w byte
	subq.l #1, d5
	bra.w next
done
	move.l d1, State.High(a0)
	move.l d2, State.Low(a0)
	movem.l (sp)+, d0-d5/a0-a1
	rts
	.bend  ; update
	.priv
; D0=byte,D1/D2=hash states; clobbers D0/CCR.
byte	.block
	eor.l d0, d1
	mulu.l #16777619, d1
	add.l d0, d2
	move.l d2, d0
	lsl.l #5, d2
	add.l d0, d2
	rts
	.bend  ; byte
	.endsection
	.endmodule
