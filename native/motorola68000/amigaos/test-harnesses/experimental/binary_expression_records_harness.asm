; Direct typed-expression record, projection and relocation provenance contract.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module main
	.cpu 68020
	.use opasm.amigaos.binary_expression as expression
	.use exprvm.amigaos.runtime as runtime
	.use experimental.amigaos.binary_values as values
	.use experimental.amigaos.binary_record_values as records
	.use experimental.amigaos.binary_hunk_references as references
	.use experimental.amigaos.binary_numeric_bindings as bindings
	.use experimental.amigaos.binary_package as package

SYS_BASE = 4
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
OPEN = -30
CLOSE = -36
WRITE = -48
MODE_NEWFILE = 1006
RETURN_FAIL = 20
SYMBOL_COUNT = 8
VALUE_BINDING = 6
ADDRESS_BINDING = 7
MEMBER_VALUE = 4
MEMBER_ADDRESS = 5
MEMBER_UNKNOWN = 6
EQUALITY_CHILD_MEMBER = 7
PLAN_6_CANDIDATES = 1
PLAN_7_CANDIDATES = 2
BINDING_TRUNCATED_BYTES = BindingPlansEnd-BindingPlans-1

	.section entry, kind=code
	.pub
start	.block
	move.l #RETURN_FAIL, ReturnCode
	lea DosName, a1
	moveq #36, d0
	movea.l SYS_BASE.w, a6
	jsr OPEN_LIBRARY(a6)
	tst.l d0
	beq.w done
	move.l d0, DosBase
	bsr.w evaluateRecords
	bne.w closeDos
	bsr.w writeOutput
	bne.w closeDos
	clr.l ReturnCode
closeDos
	movea.l DosBase, a1
	movea.l SYS_BASE.w, a6
	jsr CLOSE_LIBRARY(a6)
done
	move.l ReturnCode, d0
	rts
	.bend  ; start

; Build immutable descriptors once, then evaluate a bounded independent case table.
evaluateRecords	.block
	lea Owner, a0
	moveq #SYMBOL_COUNT, d0
	jsr values.init
	bne.w failed
	lea DefaultCell, a1
	lea Fields, a2
	moveq #2, d0
	moveq #2*records.FIELD_BYTES, d1
	jsr records.appendRecord
	bne.w failed
	move.l d2, SymbolValues
	move.l d2, ListCell+values.Cell.Low
	moveq #0, d1
	moveq #records.ADDRESS_RECORD, d2
	jsr values.setKind
	bne.w failed
	moveq #5, d1
	moveq #records.ADDRESS_RECORD, d2
	jsr values.setKind
	bne.w failed
	lea ListCell, a1
	moveq #1, d0
	moveq #records.CELL_BYTES, d1
	jsr records.appendList
	bne.w failed
	move.l d2, SymbolValues+8
	moveq #1, d1
	moveq #records.TYPED_LIST, d2
	jsr values.setKind
	bne.w failed
	; Install both plans, then prove malformed replacement is atomic.
	lea Owner, a0
	lea BindingPlans, a1
	move.l #BindingPlansEnd-BindingPlans, d0
	jsr bindings.install
	bne.w failed
	lea Owner, a0
	lea BindingPlans, a1
	move.l #BINDING_TRUNCATED_BYTES, d0
	jsr bindings.install
	cmpi.l #values.MALFORMED, d0
	bne.w failed
	lea Owner, a0
	moveq #VALUE_BINDING, d1
	jsr bindings.getBinding
	bne.w failed
	; Build independent record -> list -> record trees for recursive equality.
	lea Owner, a0
	lea EqualityLeafEqualA, a1
	lea EqualityRootA, a2
	lea EqualityLeafA, a3
	bsr.w buildEqualityTree
	bne.w failed
	lea Owner, a0
	lea EqualityLeafEqualB, a1
	lea EqualityRootB, a2
	lea EqualityLeafB, a3
	bsr.w buildEqualityTree
	bne.w failed
	lea Owner, a0
	lea EqualityLeafChanged, a1
	lea EqualityRootChanged, a2
	lea EqualityLeafChangedOffset, a3
	bsr.w buildEqualityTree
	bne.w failed
	lea Owner, a0
	lea EqualityLeafProvenance, a1
	lea EqualityRootProvenance, a2
	lea EqualityLeafProvenanceOffset, a3
	bsr.w buildEqualityTree
	bne.w failed
	lea Owner, a0
	lea EqualityLeafTruncated, a1
	lea EqualityRootTruncated, a2
	lea EqualityLeafTruncatedOffset, a3
	bsr.w buildEqualityTree
	bne.w failed
	lea Owner, a0
	lea EqualityLeafCycle, a1
	lea EqualityRootCycle, a2
	lea EqualityLeafCycleOffset, a3
	bsr.w buildEqualityTree
	bne.w failed
	; Corrupt only after construction; compare against a distinct valid tree.
	lea Owner, a0
	move.l EqualityLeafTruncatedOffset, d1
	jsr values.view
	bne.w failed
	move.l #$7fffffff, values.Record.Count(a1)
	; Form root -> list -> leaf -> root after all descriptors are valid.
	lea Owner, a0
	move.l EqualityRootCycle, d1
	jsr values.view
	bne.w failed
	lea Owner, a0
	move.l EqualityLeafCycleOffset, d1
	jsr values.view
	bne.w failed
	lea values.RECORD_BASE_BYTES(a1), a2
	lea values.Field.Value(a2), a2
	move.l EqualityRootCycle, values.Cell.Low(a2)
	clr.l values.Cell.High(a2)
	move.l #values.ADDRESS_RECORD, values.Cell.Kind(a2)
	clr.l values.Cell.Provenance(a2)
	move.l #SymbolValues, Context+package.Context.Values
	move.l #Defined, Context+package.Context.Defined
	move.l #SYMBOL_COUNT, Context+package.Context.Count
	move.l #Owner, Context+package.Context.Owner
	move.l #SectionIds, Context+package.Context.SectionIds
	move.w #2, Context+package.Context.CurrentSection
	move.w #2, Context+package.Context.Pass
	move.w #1, Context+package.Context.Relocatable
	move.l #80, Context+package.Context.Pc
	lea Cases, a4
	lea ResultBuffer, a5
	moveq #CASE_COUNT-1, d7
nextCase
	movea.l (a4)+, a0
	movea.l (a4)+, a1
	lea Context, a2
	clr.l package.Context.Kind(a2)
	jsr expression.evaluateValue
	move.l d0, (a5)+
	move.l d1, (a5)+
	move.l package.Context.High(a2), (a5)+
	move.l package.Context.Kind(a2), (a5)+
	move.l package.Context.Provenance(a2), (a5)+
	move.l d2, (a5)+
	dbra d7, nextCase
	; Ordinary wrapper projection and affine identity must agree.
	lea Alias, a0
	lea AliasEnd, a1
	lea Context, a2
	jsr expression.evaluate
	move.l d0, (a5)+
	move.l d1, (a5)+
	move.l package.Context.High(a2), (a5)+
	move.l package.Context.Provenance(a2), (a5)+
	lea Index, a0
	lea IndexEnd, a1
	jsr references.affineTarget
	move.l d0, (a5)+
	move.l d1, (a5)+
	jsr references.baseSection
	move.l d0, (a5)+
	move.l d1, (a5)+
	; A bare typed alias must validate its descriptor, even without postfix work.
	lea InvalidAlias, a0
	lea InvalidAliasEnd, a1
	jsr expression.evaluateValue
	move.l d0, (a5)+
	lea EqualityPairs, a4
	moveq #EQUALITY_CASE_COUNT-1, d7
nextEquality
	movea.l (a4)+, a1
	move.l (a1), d1
	movea.l (a4)+, a1
	move.l (a1), d2
	lea Owner, a0
	jsr values.equalValues
	move.l d0, (a5)+
	dbra d7, nextEquality
	lea Owner, a0
	jsr values.release
	move.l #OUTPUT_BYTES, OutputLength
	moveq #0, d0
	rts
failed
	lea Owner, a0
	jsr values.release
	moveq #1, d0
	rts
	.bend  ; evaluateRecords

; A0=Owner,A1=leaf field,A2=root offset output,A3=leaf offset output.
; D0/CCR=status; builds one independent record -> list -> record tree.
buildEqualityTree	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a2, a4
	movea.l a3, a5
	movea.l a0, a3
	movea.l a1, a6
	movea.l a3, a0
	lea EqualityLeafDefault, a1
	movea.l a6, a2
	moveq #1, d0
	moveq #records.FIELD_BYTES, d1
	jsr records.appendRecord
	bne.w failed
	move.l d2, (a5)
	move.l d2, EqualityListCell+values.Cell.Low
	movea.l a3, a0
	lea EqualityListCell, a1
	moveq #1, d0
	moveq #records.CELL_BYTES, d1
	jsr records.appendList
	bne.w failed
	move.l d2, EqualityRootField+values.Field.Value+values.Cell.Low
	movea.l a3, a0
	lea EqualityRootDefault, a1
	lea EqualityRootField, a2
	moveq #1, d0
	moveq #records.FIELD_BYTES, d1
	jsr records.appendRecord
	bne.w failed
	move.l d2, (a4)
	moveq #0, d0
	bra.w done
failed
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; buildEqualityTree

writeOutput	.block
	move.l #OutputPath, d1
	move.l #MODE_NEWFILE, d2
	movea.l DosBase, a6
	jsr OPEN(a6)
	tst.l d0
	beq.s fail
	move.l d0, d4
	move.l d0, d1
	move.l #ResultBuffer, d2
	move.l OutputLength, d3
	jsr WRITE(a6)
	move.l d0, -(sp)
	move.l d4, d1
	jsr CLOSE(a6)
	move.l (sp)+, d0
	cmp.l OutputLength, d0
	bne.s fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; writeOutput
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
OutputPath	.byte "Work:expression-records.bin", 0
	.align 4
DefaultCell	.long 40, 0, values.SCALAR, 2
Fields	.long MEMBER_VALUE, 7, 0, values.SCALAR, 0
	.long MEMBER_ADDRESS, 44, 0, values.SCALAR, 2
ListCell	.long 0, 0, records.ADDRESS_RECORD, 0
EqualityLeafDefault	.long 0, 0, values.SCALAR, 0
EqualityRootDefault	.long 0, 0, values.SCALAR, 0
EqualityRootField	.long EQUALITY_CHILD_MEMBER, 0, 0, values.TYPED_LIST, 0
EqualityLeafEqualA	.long MEMBER_VALUE, 7, 0, values.SCALAR, 0
EqualityLeafEqualB	.long MEMBER_VALUE, 7, 0, values.SCALAR, 0
EqualityLeafChanged	.long MEMBER_VALUE, 8, 0, values.SCALAR, 0
EqualityLeafProvenance	.long MEMBER_VALUE, 7, 0, values.SCALAR, 2
EqualityLeafTruncated	.long MEMBER_VALUE, 7, 0, values.SCALAR, 0
EqualityLeafCycle	.long MEMBER_VALUE, 7, 0, values.SCALAR, 0
EqualityListCell	.long 0, 0, values.ADDRESS_RECORD, 0
SymbolValues	.long 0, 0, 0, 0, 60, 0, 5, 0, 0, 0, $7fffffff, 0, 0, 0, 0, 0
Defined	.byte 1, 1, 1, 2, 1, 1, 0, 0
SectionIds	.byte 0, 0, 2, 0, $ff, 0, 0, 0
	.align 4
Cases	.long Alias, AliasEnd, Index, IndexEnd, Member, MemberEnd
	.long AddressMember, AddressMemberEnd, Add, AddEnd, ReverseAdd, ReverseAddEnd
	.long Subtract, SubtractEnd, Difference, DifferenceEnd, Unsupported, UnsupportedEnd
	.long UnknownMember, UnknownMemberEnd, Pc, PcEnd, ScalarBoundary, ScalarBoundaryEnd
	.long CompactAdd, CompactAddEnd, UnsafeDifference, UnsafeDifferenceEnd
	.long LoadSymbol6, LoadSymbol6End, CompactLoadSymbol7, CompactLoadSymbol7End
	.long CompactLoadSymbol7AddOne, CompactLoadSymbol7AddOneEnd
CASE_COUNT = 17
EQUALITY_CASE_COUNT = 5
OUTPUT_BYTES = CASE_COUNT*24+36+EQUALITY_CASE_COUNT*4
; Compact aliases retain the record in evaluateValue and project in evaluate.
Alias	.byte expression.COMPILED_TAG, AliasEnd-Alias-2, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, 0, runtime.EXPRVM_OPCODE_END
AliasEnd
Index	.byte expression.VALUE_TAG, IndexEnd-Index-2, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 1, 0, runtime.EXPRVM_OPCODE_PUSH_LITERAL
	.long 0, 0
	.byte $61, 0
IndexEnd
Member	.byte expression.VALUE_TAG, MemberEnd-Member-2, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 1, 0, runtime.EXPRVM_OPCODE_PUSH_LITERAL
	.long 0, 0
	.byte $61, runtime.EXPRVM_OPCODE_GET_MEMBER, MEMBER_VALUE, 0, 0
MemberEnd
AddressMember	.byte expression.VALUE_TAG, AddressMemberEnd-AddressMember-2, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, 0, runtime.EXPRVM_OPCODE_GET_MEMBER, MEMBER_ADDRESS, 0, 0
AddressMemberEnd
Add	.byte expression.VALUE_TAG, AddEnd-Add-2, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, 0, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 3, 0, runtime.EXPRVM_OPCODE_APPLY_BINARY, runtime.EXPRVM_BINARY_ADD, 0
AddEnd
ReverseAdd	.byte expression.VALUE_TAG, ReverseAddEnd-ReverseAdd-2, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 3, 0, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, 0, runtime.EXPRVM_OPCODE_APPLY_BINARY, runtime.EXPRVM_BINARY_ADD, 0
ReverseAddEnd
Subtract	.byte expression.VALUE_TAG, SubtractEnd-Subtract-2, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, 0, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 3, 0, runtime.EXPRVM_OPCODE_APPLY_BINARY, runtime.EXPRVM_BINARY_SUBTRACT, 0
SubtractEnd
Difference	.byte expression.VALUE_TAG, DifferenceEnd-Difference-2, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 2, 0, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, 0, runtime.EXPRVM_OPCODE_APPLY_BINARY, runtime.EXPRVM_BINARY_SUBTRACT, 0
DifferenceEnd
Unsupported	.byte expression.VALUE_TAG, UnsupportedEnd-Unsupported-2, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 2, 0, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, 0, runtime.EXPRVM_OPCODE_APPLY_BINARY, runtime.EXPRVM_BINARY_ADD, 0
UnsupportedEnd
UnknownMember	.byte expression.VALUE_TAG, UnknownMemberEnd-UnknownMember-2, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, 0, runtime.EXPRVM_OPCODE_GET_MEMBER, MEMBER_UNKNOWN, 0, 0
UnknownMemberEnd
Pc	.byte expression.VALUE_TAG, PcEnd-Pc-2, runtime.EXPRVM_OPCODE_PUSH_CURRENT_ADDR, runtime.EXPRVM_OPCODE_END
PcEnd
ScalarBoundary	.byte expression.VALUE_TAG, ScalarBoundaryEnd-ScalarBoundary-2, runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, 0, $70, runtime.EXPRVM_OPCODE_END
ScalarBoundaryEnd
CompactAdd	.byte expression.COMPILED_TAG, CompactAddEnd-CompactAdd-2
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 0, 0
	.byte runtime.COMPACT_I8, 5, runtime.COMPACT_ADD, runtime.EXPRVM_OPCODE_END
CompactAddEnd
UnsafeDifference	.byte expression.VALUE_TAG, UnsafeDifferenceEnd-UnsafeDifference-2
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 4, 0
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 4, 0
	.byte runtime.EXPRVM_OPCODE_APPLY_BINARY, runtime.EXPRVM_BINARY_SUBTRACT, runtime.EXPRVM_OPCODE_END
UnsafeDifferenceEnd
; Missing symbols resolve through their installed full-ID numeric plans.
LoadSymbol6	.byte expression.VALUE_TAG, LoadSymbol6End-LoadSymbol6-2
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, VALUE_BINDING, 0, runtime.EXPRVM_OPCODE_END
LoadSymbol6End
CompactLoadSymbol7	.byte expression.COMPILED_TAG, CompactLoadSymbol7End-CompactLoadSymbol7-2
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, ADDRESS_BINDING, 0, runtime.EXPRVM_OPCODE_END
CompactLoadSymbol7End
CompactLoadSymbol7AddOne	.byte expression.COMPILED_TAG, CompactLoadSymbol7AddOneEnd-CompactLoadSymbol7AddOne-2
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, ADDRESS_BINDING, 0
	.byte runtime.COMPACT_I8, 1, runtime.COMPACT_ADD, runtime.EXPRVM_OPCODE_END
CompactLoadSymbol7AddOneEnd
InvalidAlias	.byte expression.COMPILED_TAG, InvalidAliasEnd-InvalidAlias-2
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 5, 0, runtime.EXPRVM_OPCODE_END
InvalidAliasEnd
	.align 2
PLAN_6_BYTES = BindingPlan6End-BindingPlan6
PLAN_7_BYTES = BindingPlan7End-BindingPlan7
BindingPlans
BindingPlan6
	.word VALUE_BINDING, PLAN_6_CANDIDATES
	.long PLAN_6_BYTES
	.word 0, 1, MEMBER_VALUE
BindingPlan6End
BindingPlan7
	.word ADDRESS_BINDING, PLAN_7_CANDIDATES
	.long PLAN_7_BYTES
	.word 3, 1, MEMBER_ADDRESS
	.word 0, 1, MEMBER_ADDRESS
BindingPlan7End
BindingPlansEnd
	.align 4
EqualityPairs
	.long EqualityRootA, EqualityRootB
	.long EqualityRootA, EqualityRootChanged
	.long EqualityRootA, EqualityRootProvenance
	.long EqualityRootA, EqualityRootTruncated
	.long EqualityRootCycle, EqualityRootCycle
	.endsection
	.section bss, kind=bss
DosBase	.res long, 1
ReturnCode	.res long, 1
OutputLength	.res long, 1
ResultBuffer	.res byte, OUTPUT_BYTES
	.align 4
Owner	.res byte, values.OWNER_BYTES
Context	.res byte, package.CONTEXT_BYTES
EqualityRootA	.res long, 1
EqualityRootB	.res long, 1
EqualityRootChanged	.res long, 1
EqualityRootProvenance	.res long, 1
EqualityRootTruncated	.res long, 1
EqualityLeafA	.res long, 1
EqualityLeafB	.res long, 1
EqualityLeafChangedOffset	.res long, 1
EqualityLeafProvenanceOffset	.res long, 1
EqualityLeafTruncatedOffset	.res long, 1
EqualityRootCycle	.res long, 1
EqualityLeafCycleOffset	.res long, 1
	.endsection
	.output "expression-records.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
