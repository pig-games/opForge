; Actual package encoding with repeated, distinct and affine source identities.
	.module tests.amigaos.instruction_fixup_identity_probe
	.cpu 68020
	.use experimental.amigaos.binary_encoding as encoding
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_dependency_layout as dependencies
	.use experimental.amigaos.binary_numeric_bindings as bindings
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_hunk_references as references
	.use experimental.amigaos.binary_module_admission as admission
	.use prvm.amigaos.abi as abi
	.priv
RESET_RECORD_BYTES = abi.PRVM_PACKED_RECORD_BYTES+abi.PRVM_PACKED_DOT_BYTES+abi.PRVM_PACKED_NAME_BYTES
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
	bsr.w admissionChecks
	bne.w bad
	bsr.w qualifiedChecks
	bne.w bad
	bsr.w geometryChecks
	bne.w bad
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

; Package reset is retained; ambient package/text mutations reject admission.
admissionChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	move.w Package+package.Header.CpuDirective, ResetRecord+abi.PRVM_PACKED_RECORD_BYTES+abi.PackedDot.Name+abi.PackedName.Id
	move.w Package+package.Header.CpuName, ResetRecord+abi.PRVM_PACKED_RECORD_BYTES+abi.PRVM_PACKED_DOT_BYTES+abi.PackedName.Id
	lea ResetRecord, a0
	moveq #RESET_RECORD_BYTES, d0
	lea Package, a1
	moveq #1, d1
	moveq #2, d2
	jsr admission.prepare
	bne.w bad
	cmpi.l #RESET_RECORD_BYTES, d1
	bne.w bad
	lea ResetRecord, a0
	cmpa.l a0, a1
	bne.w bad
	moveq #RESET_RECORD_BYTES*2, d0
	lea Package, a1
	moveq #1, d1
	moveq #2, d2
	jsr admission.prepare
	cmpi.l #admission.UNSUPPORTED, d0
	bne.w bad
	lea ResetRecord, a0
	moveq #RESET_RECORD_BYTES*2, d0
	lea Package, a1
	jsr admission.textSafe
	bne.w bad
	lea TextDefinitionRecord, a0
	moveq #RESET_RECORD_BYTES, d0
	jsr admission.textSafe
	cmpi.l #admission.UNSUPPORTED, d0
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; admissionChecks

; A failed address-dependent scalar range must leave the geometry proof
; unproven. A new encode resets that observation; fixed package fixups pass.
geometryChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea IdentityOwner, a0
	move.l #SYMBOL_COUNT, d0
	jsr values.init
	bne.w bad
	move.l #IdentityOwner, package.Context.Owner(a2)
	move.w #package.POSITION_OBSERVATION_POLICY, package.Context.ExpressionPolicy(a2)
	lea IdentityPlans, a1
	move.l #IdentityPlansEnd-IdentityPlans, d0
	jsr bindings.install
	bne.w bad
	bsr.w strictReferences
	bne.w bad
	lea PostincrementOperand, a0
	lea PostincrementOperandEnd, a1
	move.l #MOVE_ID, d0
	moveq #BYTE_QUALIFIER, d1
	jsr encoding.encode
	tst.l d0
	bne.w bad
	cmpi.l #2, d1
	bne.w bad
	cmpi.w #POSTINCREMENT_WORD, (a1)
	bne.w bad
	bsr.w plainProof
	bne.w bad
	lea Operands3, a0
	lea Operands3End, a1
	move.l #MOVE_ID, d0
	moveq #LONG_QUALIFIER, d1
	jsr encoding.encode
	tst.l d0
	bne.w bad
	bsr.w plainProof
	bne.w bad
	bsr.w fixedFields
	bne.w bad
	lea RangeOperand, a0
	lea RangeOperandEnd, a1
	move.l #MOVEQ_ID, d0
	moveq #0, d1
	jsr encoding.encode
	tst.l d0
	beq.w bad
	jsr encoding.outputGeometryProof
	tst.l d0
	beq.w bad
	bsr.w fixedFields
	bne.w bad
	moveq #0, d7
	bra.w cleanup
bad
	moveq #1, d7
cleanup
	clr.w package.Context.ExpressionPolicy(a2)
	clr.l package.Context.Owner(a2)
	lea IdentityOwner, a0
	jsr values.release
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
plainProof
	jsr encoding.outputFixupCount
	tst.l d0
	bne.w missingFields
	jsr encoding.outputGeometryProof
	rts
strictReferences
	lea StrictReferenceCases, a3
	moveq #3, d7
strictNext
	movea.l (a3)+, a0
	movea.l (a3)+, a1
	move.l (a3)+, d6
	movem.l a0-a1, -(sp)
	jsr references.geometryExpression
	cmp.l d6, d0
	movem.l (sp)+, a0-a1
	bne.w missingFields
	jsr references.geometryTokens
	cmp.l d6, d0
	bne.w missingFields
	dbra d7, strictNext
	; Ordinary relocation identity still cancels these same-section bases.
	lea BlockDifference, a0
	lea BlockDifferenceEnd, a1
	jsr references.tokens
	tst.l d0
	bne.w missingFields
	moveq #0, d0
	rts
fixedFields
	lea Operands0, a0
	lea Operands0End, a1
	move.l #MOVE_ID, d0
	moveq #LONG_QUALIFIER, d1
	jsr encoding.encode
	tst.l d0
	bne.w done
	jsr encoding.outputFixupCount
	cmpi.l #2, d0
	bne.w missingFields
	jsr encoding.outputGeometryProof
	rts
missingFields
	moveq #1, d0
done
	tst.l d0
	rts
	.bend  ; geometryChecks

; Numeric qualified plans retain declaration identity even when values coincide.
qualifiedChecks	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea IdentityOwner, a0
	move.l #SYMBOL_COUNT, d0
	jsr values.init
	bne.w bad
	move.l #IdentityOwner, package.Context.Owner(a2)
	lea IdentityPlans, a1
	move.l #IdentityPlansEnd-IdentityPlans, d0
	jsr bindings.install
	bne.w bad
	move.l #$1000, Values+SECOND_ID*8
	lea QualifiedCases, a3
	moveq #2, d7
next
	movea.l (a3)+, a0
	movea.l (a3)+, a1
	move.l (a3)+, d6
	jsr references.affineTarget
	cmpi.l #references.STATUS_SECTION, d0
	bne.w bad
	cmp.l d6, d1
	bne.w bad
	cmpa.l a1, a0
	bne.w bad
	dbra d7, next
	; An active member candidate commits; a later whole scalar cannot win.
	lea IdentityOwner, a0
	move.l #PROJECTED_ID, d1
	jsr bindings.scalarIdentity
	cmpi.l #values.BOUNDS, d0
	bne.w bad
	moveq #0, d7
	bra.w cleanup
bad
	moveq #1, d7
cleanup
	move.l #$1010, Values+SECOND_ID*8
	clr.l package.Context.Owner(a2)
	lea IdentityOwner, a0
	jsr values.release
	move.l d7, d0
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; qualifiedChecks
	.endsection
	.section data, kind=data
	.priv
	.include "instruction_fixup_identity_fixture.i"
	.align 4
Cases	.long Operands0, Operands0End, Operands1, Operands1End
	.long Operands2, Operands2End, Operands3, Operands3End
BranchQualifiers	.long WORD_QUALIFIER, 0
QualifiedCases	.long QualifiedFirst, QualifiedFirstEnd, FIRST_ID
	.long QualifiedSecond, QualifiedSecondEnd, SECOND_ID
	.long QualifiedAddend, QualifiedAddendEnd, FIRST_ID
StrictReferenceCases	.long BlockDifference, BlockDifferenceEnd, references.STATUS_SECTION
	.long UnknownExpression, UnknownExpressionEnd, references.STATUS_SECTION
	.long CurrentExpression, CurrentExpressionEnd, references.STATUS_SECTION
	.long TypedDifference, TypedDifferenceEnd, references.STATUS_BAD
IdentityPlans	.word QUALIFIED_FIRST_ID, 1
	.long 12
	.word FIRST_ID, 0
	.word QUALIFIED_SECOND_ID, 1
	.long 12
	.word SECOND_ID, 0
	.word PROJECTED_ID, 2
	.long 18
	.word FIRST_ID, 1, MEMBER_ID
	.word SECOND_ID, 0
IdentityPlansEnd
ResetRecord	.byte RESET_RECORD_BYTES-1,abi.PRVM_PACKED_FLAG_INDENT
	.word 0
	.byte abi.PRVM_TOKEN_KIND_DOT,abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 0
	.byte 0,abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 0
	.byte 0
PackageStateRecord	.byte RESET_RECORD_BYTES-1,abi.PRVM_PACKED_FLAG_INDENT
	.word 0
	.byte abi.PRVM_TOKEN_KIND_DOT,abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word FPU_ID
	.byte 0,abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word NONE_ID
	.byte 0
TextDefinitionRecord	.byte RESET_RECORD_BYTES-1,abi.PRVM_PACKED_FLAG_INDENT
	.word 0
	.byte abi.PRVM_TOKEN_KIND_DOT,abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word ENCODE_ID
	.byte 0,abi.PRVM_TOKEN_KIND_IDENTIFIER
	.word 1
	.byte 0
	.align 2
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
IdentityOwner	.res byte, values.OWNER_BYTES
Result	.res byte, 256
ResultBytes	.res long, 1
ReturnCode	.res long, 1
DosBase	.res long, 1
Handle	.res long, 1
	.endsection
	.output "instruction-fixup-identity.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
