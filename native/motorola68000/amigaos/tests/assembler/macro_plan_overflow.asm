; Synthetic overflow inputs reject before source reads or allocation.
; @opforge-owner: opforge.tests.assembler.macro_plan_overflow
.module opforge.tests.assembler.macro_plan_overflow
.cpu 68020
.use opforge.assembler.language.macro_plans as plans
.use opforge.platform.memory as memory
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
	tst.l d0
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
; D0/CCR=status. Each rejected call must preserve its published arena and ABI.
cases	.block
	lea Inputs, a5
	moveq #3, d7
next
	move.l (a5)+, Row+plans.Row.SpellingEnd
	move.l (a5)+, Frame+plans.Frame.RecipeEvents
	move.l (a5)+, Arena+memory.Block.Used
	lea Frame, a0
	movem.l d2-d7/a0-a6, Before
	jsr plans.create
	beq.w bad
	tst.l d0
	beq.w bad
	tst.l d1
	bne.w bad
	movem.l d2-d7/a0-a6, After
	lea Before, a0
	lea After, a1
	moveq #12, d0
compare
	move.l (a0)+, d1
	cmp.l (a1)+, d1
	bne.w bad
	dbra d0, compare
	move.l #Sentinel, d0
	cmp.l Arena+memory.Block.Pointer, d0
	bne.w bad
	cmpi.l #16, Arena+memory.Block.Capacity
	bne.w bad
	move.l -4(a5), d0
	cmp.l Arena+memory.Block.Used, d0
	bne.w bad
	lea Sentinel, a0
	moveq #3, d0
contents
	cmpi.l #$a5a5a5a5, (a0)+
	bne.w bad
	dbra d0, contents
	dbra d7, next
	; Arena storage is a static sentinel, never owned by memory allocation.
	; Restore the synthetic invalid extent without calling the freeing finish API.
	move.l #7, Arena+memory.Block.Used
	clr.l Row+plans.Row.SpellingEnd
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
.bend  ; cases
.endsection
.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:macro-plan-overflow.bin", 0
Result	.byte $5a
.align 4
; Spelling addition, recipe addition, rounding addition, prior Used addition.
Inputs
	.long $ffffffd4, 0, 7
	.long $ffffffd0, Row, 7
	.long $ffffffd3, 0, 7
	.long 0, 0, $ffffffd4
Arena	.long Sentinel, 16, 7
Sentinel	.long $a5a5a5a5, $a5a5a5a5, $a5a5a5a5, $a5a5a5a5
Frame
	; Arena, events/count, source/span, packed map/count, recipes/count.
	.long Arena, Row, 1, Sentinel, $ffffffff, PackedMap, 0, 0, 0
Row
	; Kind/flags; packed span, spelling span, auxiliary fields.
	.word 0, 0
	.long 0, 0, 0, 0, 0, 0, 0
PackedMap	.word 0
.endsection
.section bss, kind=bss
.align 4
Before	.res long, 13
After	.res long, 13
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
.endsection
.output "macro-plan-overflow.hunk", format=hunk, sections=entry, code, data, bss
.endmodule
