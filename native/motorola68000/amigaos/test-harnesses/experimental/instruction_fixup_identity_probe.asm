; Actual package encoding with repeated, distinct and affine source identities.
	.module tests.amigaos.instruction_fixup_identity_probe
	.cpu 68020
	.use experimental.amigaos.binary_encoding as encoding
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_dependency_layout as dependencies
	.priv
EXEC_BASE = 4
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
DOS_OPEN = -30
DOS_CLOSE = -36
DOS_WRITE = -48
MODE_NEWFILE = 1006
	.section entry, kind=code
	.pub
; D0=0 only after all real package instructions and the result file complete.
start	.block
	movem.l d2-d7/a2-a6, -(sp)
	move.l #20, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr OPEN_LIBRARY(a6)
	move.l d0, DosBase
	beq.w done
	jsr encodeCases
	bne.w closeLibrary
	movea.l DosBase, a6
	move.l #OutputPath, d1
	move.l #MODE_NEWFILE, d2
	jsr DOS_OPEN(a6)
	move.l d0, Handle
	beq.w closeLibrary
	move.l d0, d1
	move.l #Result, d2
	move.l ResultBytes, d3
	jsr DOS_WRITE(a6)
	cmp.l ResultBytes, d0
	bne.w closeFile
	clr.l ReturnCode
closeFile
	move.l Handle, d1
	jsr DOS_CLOSE(a6)
	tst.l d0
	bne.w closeLibrary
	move.l #20, ReturnCode
closeLibrary
	movea.l DosBase, a1
	movea.l EXEC_BASE.w, a6
	jsr CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	movem.l (sp)+, d2-d7/a2-a6
	tst.l d0
	rts
	.bend  ; start
	.endsection
	.section code, kind=code
	.priv
encodeCases	.block
	lea Context, a2
	move.l #Values, package.Context.Values(a2)
	move.l #Defined, package.Context.Defined(a2)
	move.l #SectionIds, package.Context.SectionIds(a2)
	move.l #SYMBOL_COUNT, package.Context.Count(a2)
	move.l #Package, package.Context.Package(a2)
	move.l #$1020, package.Context.Pc(a2)
	move.w #2, package.Context.Pass(a2)
	move.w #1, package.Context.Relocatable(a2)
	move.w #1, package.Context.CurrentSection(a2)
	move.l #$1000, Values+FIRST_ID*8
	move.l #$1010, Values+SECOND_ID*8
	move.b #dependencies.LABEL, Defined+FIRST_ID
	move.b #dependencies.LABEL, Defined+SECOND_ID
	move.b #1, SectionIds+FIRST_ID
	move.b #1, SectionIds+SECOND_ID
	lea Cases, a3
	lea Result, a5
	moveq #3, d6
nextCase
	movea.l (a3)+, a0
	movea.l (a3)+, a1
	move.l #MOVE_ID, d0
	moveq #LONG_QUALIFIER, d1
	jsr encoding.encode
	tst.l d0
	bne.w bad
	move.l d1, (a5)+
	move.l d1, d4
	jsr encoding.outputFixupCount
	move.l d0, (a5)+
	move.l d0, d5
copyBytes
	move.b (a1)+, (a5)+
	subq.l #1, d4
	bne.w copyBytes
	moveq #0, d7
nextFixup
	cmp.l d5, d7
	bhs.w nextInstruction
	move.l d7, d0
	jsr encoding.outputFixup
	tst.l d0
	bne.w bad
	move.l d1, (a5)+
	move.l d2, (a5)+
	move.l d3, (a5)+
	move.l d4, (a5)+
	move.l d7, d0
	jsr encoding.outputFixupIdentity
	bne.w bad
	move.l d3, (a5)+
	addq.l #1, d7
	bra.w nextFixup
nextInstruction
	move.l d5, d0
	jsr encoding.outputFixupIdentity
	beq.w bad
	dbra d6, nextCase
	; Both requests may select a narrow encodable displacement. Retain whether
	; package input requested a candidate or automatic selection chose one.
	move.l #$1040, package.Context.Pc(a2)
	lea BranchQualifiers, a3
	moveq #1, d6
branchRequest
	lea BranchOperand, a0
	lea BranchOperandEnd, a1
	move.l #BSR_ID, d0
	move.l (a3)+, d1
	jsr encoding.encode
	tst.l d0
	bne.w bad
	move.l d1, (a5)+
	add.l d1, package.Context.Pc(a2)
	move.l d1, d4
copyBranch
	move.b (a1)+, (a5)+
	subq.l #1, d4
	bne.w copyBranch
	jsr encoding.outputPositionRequestedCandidate
	bne.w bad
	move.l d1, (a5)+
	dbra d6, branchRequest
	move.l a5, d0
	lea Result, a0
	sub.l a0, d0
	move.l d0, ResultBytes
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; encodeCases
	.endsection
	.section data, kind=data
	.priv
	.include "instruction_fixup_identity_fixture.i"
	.align 4
Cases	.long Operands0, Operands0End, Operands1, Operands1End
	.long Operands2, Operands2End, Operands3, Operands3End
BranchQualifiers	.long WORD_QUALIFIER, 0
Package	.incbin "instruction_fixup_identity_package.bin"
DosName	.byte "dos.library", 0
OutputPath	.byte "Work:output.bin", 0
	.endsection
	.section bss, kind=bss
	.priv
	.align 4
Context	.res byte, package.CONTEXT_BYTES
Values	.res byte, SYMBOL_COUNT*8
Defined	.res byte, SYMBOL_COUNT
SectionIds	.res byte, SYMBOL_COUNT
	.align 4
Result	.res byte, 256
ResultBytes	.res long, 1
ReturnCode	.res long, 1
DosBase	.res long, 1
Handle	.res long, 1
	.endsection
	.output "instruction-fixup-identity.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
