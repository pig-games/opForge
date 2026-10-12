; Producer and immutable consumer grow the same numeric binding contract.
; @opforge-owner: test.amigaos.binding_plan_growth
.module test.amigaos.binding_plan_growth
.cpu 68020
.use experimental.amigaos.binary_binding_plans as plans
.use experimental.amigaos.binary_binding_layout as layout
.use experimental.amigaos.binary_numeric_bindings as bindings
.use experimental.amigaos.binary_values as values
.use experimental.amigaos.binary_memory as memory
.priv
EXEC_BASE = 4
EXEC_OPEN_LIBRARY = -552
EXEC_CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
.pub
.section entry, kind=code
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	jsr cases
	move.l d0, d7
	lea Plans, a0
	jsr memory.release
	lea Owner, a0
	jsr values.release
	tst.l d7
	bne.w done
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	move.l d0, DosBase
	beq.w done
	movea.l d0, a6
	move.l #OutputName, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, OutputHandle
	beq.w closeLibrary
	move.l d0, d1
	move.l #Result, d2
	moveq #1, d3
	jsr DOS_WRITE(a6)
	cmpi.l #1, d0
	bne.w closeFile
	clr.l ReturnCode
closeFile
	move.l OutputHandle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	move.l #20, ReturnCode
closeLibrary
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr EXEC_CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	tst.l d0
	movem.l (sp)+, d2-d7/a2-a6
	rts
.bend  ; start
.endsection
.priv
.section code, kind=code
CANDIDATES = 65535
MEMBERS = 7
TOTAL_BYTES = layout.HEADER_BYTES+CANDIDATES*(layout.CANDIDATE_HEADER_BYTES+MEMBERS*2)
cases	.block
	lea Owner, a0
	moveq #8, d0
	jsr values.init
	bne.w bad
	lea Plans, a0
	moveq #7, d0
	jsr plans.start
	bne.w bad
	move.l #CANDIDATES, d6
append
	lea Plans, a0
	lea MemberIds, a1
	moveq #0, d0
	moveq #MEMBERS, d1
	jsr plans.append
	bne.w bad
	subq.l #1, d6
	bne.w append
	cmpi.l #TOTAL_BYTES, Plans+memory.Block.Used
	bne.w bad
	movea.l Plans+memory.Block.Pointer, a4
	cmpi.w #$ffff, layout.Plan.CandidateCount(a4)
	bne.w bad
	cmpi.l #TOTAL_BYTES, layout.Plan.Bytes(a4)
	bne.w bad
	; Rejection must preserve descriptors and every byte, not merely the count.
	lea Owner, a0
	movea.l a4, a1
	move.l #TOTAL_BYTES, d0
	jsr bindings.install
	bne.w bad
	move.l Plans+memory.Block.Pointer, Snapshot+memory.Block.Pointer
	move.l Plans+memory.Block.Capacity, Snapshot+memory.Block.Capacity
	move.l Plans+memory.Block.Used, Snapshot+memory.Block.Used
	lea Plans, a0
	lea MemberIds, a1
	moveq #0, d0
	moveq #MEMBERS, d1
	jsr plans.append
	beq.w bad
	.for offset in {memory.Block.Pointer, memory.Block.Capacity, memory.Block.Used}
	move.l Plans+offset, d0
	cmp.l Snapshot+offset, d0
	bne.w bad
	.endfor
	bsr.w comparePlans
	bne.w bad
	lea Owner, a0
	moveq #7, d1
	jsr bindings.getBinding
	bne.w bad
	cmpi.l #TOTAL_BYTES, d2
	bne.w bad
	cmpa.l Owner+values.Owner.Bindings+memory.Block.Pointer, a1
	bne.w bad
	; A borrowed view of the destination cannot be installed over itself.
	move.l Owner+values.Owner.Bindings+memory.Block.Pointer, Snapshot+memory.Block.Pointer
	move.l Owner+values.Owner.Bindings+memory.Block.Capacity, Snapshot+memory.Block.Capacity
	move.l Owner+values.Owner.Bindings+memory.Block.Used, Snapshot+memory.Block.Used
	move.l d2, d0
	jsr bindings.install
	beq.w bad
	.for offset in {memory.Block.Pointer, memory.Block.Capacity, memory.Block.Used}
	move.l Owner+values.Owner.Bindings+offset, d0
	cmp.l Snapshot+offset, d0
	bne.w bad
	.endfor
	bsr.w comparePlans
	bne.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
.bend  ; cases
; Compare independent allocations exactly after each rejected mutation.
comparePlans	.block
	movea.l Plans+memory.Block.Pointer, a1
	movea.l Owner+values.Owner.Bindings+memory.Block.Pointer, a2
	move.l #TOTAL_BYTES, d0
byte
	cmpm.b (a1)+, (a2)+
	bne.w bad
	subq.l #1, d0
	bne.w byte
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
.bend  ; comparePlans
.endsection
.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:binding-plan-growth.bin", 0
Result	.byte $5a
.align 2
MemberIds	.word 1, 2, 3, 4, 5, 6, 7
.endsection
.section bss, kind=bss
.align 4
Plans	.res byte, plans.STATE_BYTES
Owner	.res byte, values.OWNER_BYTES
Snapshot	.res long, 3
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
.endsection
.output "binding-plan-growth.hunk", format=hunk, sections=entry, code, data, bss
.endmodule
