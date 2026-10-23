; Raw typed struct programs generated beside their live Rust oracle.
; @opforge-evidence: level=D; role=permanent-contract; authority=focused-contract; lifecycle=permanent

	.module main
	.cpu 68020
	.use opforge.vm.exprvm.values_runtime as typed
	.use opforge.assembler.values.values as values
	.use opforge.assembler.values.structs as structs
	.use opforge.platform.memory as memory
SYS_BASE = 4
OPEN_LIBRARY = -552
CLOSE_LIBRARY = -414
OPEN = -30
CLOSE = -36
WRITE = -48
MODE_NEWFILE = 1006
RETURN_FAIL = 20
SYMBOL_COUNT = 7
CapturedField	.struct
Member	.long ?
Low	.long ?
High	.long ?
Provenance	.long ?
.endstruct
CAPTURED_FIELD_BYTES = CapturedField.Provenance+4
Result	.struct
Status	.long ?
Kind	.long ?
Nominal	.long ?
Count	.long ?
Fields	.res 2*CAPTURED_FIELD_BYTES
.endstruct
RESULT_BYTES = Result.Fields+2*CAPTURED_FIELD_BYTES
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
	bsr.w evaluateCases
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

; Retained seeds survive reset; a copied schema alias keeps nominal identity.
evaluateCases	.block
	lea Owner, a0
	moveq #SYMBOL_COUNT, d0
	jsr values.init
	bne.w failed
	bsr.w checkBoundedType
	bne.w failed
	moveq #0, d0
	moveq #3, d1
	lea SchemaFields, a1
	moveq #2, d3
	moveq #2*structs.FIELD_BYTES, d4
	jsr structs.appendSeed
	bne.w failed
	move.l d2, SeedOffset
	jsr values.reset
	bne.w failed
	move.l SeedOffset, d1
	jsr structs.materialize
	bne.w failed
	move.l d2, SymbolValues
	moveq #0, d1
	moveq #structs.TYPE, d2
	jsr values.setKind
	bne.w failed
	movea.l values.Owner.Seeds+memory.Block.Pointer(a0), a1
	move.l values.Owner.Seeds+memory.Block.Used(a0), d0
	move.l SeedOffset, d1
	moveq #structs.TYPE, d3
	jsr values.copyRecord
	bne.w failed
	move.l d2, SymbolValues+4*8
	moveq #4, d1
	moveq #structs.TYPE, d2
	jsr values.setKind
	bne.w failed
	move.l #SymbolValues, Context+typed.Frame.Values
	move.l #Defined, Context+typed.Frame.Defined
	move.l #SYMBOL_COUNT, Context+typed.Frame.Count
	move.l #Owner, Context+typed.Frame.Owner
	move.w #2, Context+typed.Frame.CurrentSection
	move.l #80, Context+typed.Frame.Pc
	lea Cases, a4
	lea ResultBuffer, a5
	moveq #CASE_COUNT-1, d7
nextCase
	movea.l (a4)+, a0
	move.l (a4)+, d0
	sub.l a0, d0
	lea Context, a2
	jsr typed.evaluate
	bsr.w captureResult
	bne.w failed
	dbra d7, nextCase
	lea Owner, a0
	jsr values.release
	moveq #0, d0
	rts
failed
	lea Owner, a0
	jsr values.release
	moveq #1, d0
	rts
	.bend  ; evaluateCases

; A header-only 65536-field type must size in full width and fail the arena
; extent before any absent field is read. The static control block is not freed.
; D0/CCR=status; other registers preserved. Adds no artifact output bytes.
checkBoundedType	.block
	movem.l d1-d3/a0-a1, -(sp)
	lea OversizedTypeHeader, a1
	jsr values.descriptorBytes
	tst.l d0
	bne.w bad
	cmpi.l #65536*values.STRUCT_FIELD_BYTES+values.STRUCT_BASE_BYTES, d2
	bne.w bad
	lea ControlOwner, a0
	move.l #OversizedTypeHeader, values.Owner.Arena+memory.Block.Pointer(a0)
	move.l #values.STRUCT_BASE_BYTES, values.Owner.Arena+memory.Block.Capacity(a0)
	move.l #values.STRUCT_BASE_BYTES, values.Owner.Arena+memory.Block.Used(a0)
	moveq #0, d1
	jsr values.view
	cmpi.l #values.MALFORMED, d0
	bne.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0-a1
	tst.l d0
	rts
	.bend  ; checkBoundedType

; Normalize offsets to typed contents. Each row is status/kind/nominal/count,
; followed by two member/low/high/provenance cells (type high stores size).
; A5 advances by RESULT_BYTES; scalar rows use the first cell without a member.
captureResult	.block
	movem.l d1-d6/a0-a3, -(sp)
	movea.l a5, a3
	moveq #RESULT_BYTES/4-1, d6
clear
	clr.l (a5)+
	dbra d6, clear
	tst.l d0
	beq.w success
	move.l #1, Result.Status(a3)
	bra.w good
success
	move.l d3, Result.Kind(a3)
	tst.l d3
	bne.w compound
	move.l d1, Result.Fields+CapturedField.Low(a3)
	move.l typed.Frame.High(a2), Result.Fields+CapturedField.High(a3)
	move.l typed.Frame.Provenance(a2), Result.Fields+CapturedField.Provenance(a3)
	bra.w good
compound
	lea Owner, a0
	jsr values.view
	bne.w done
	move.l values.StructRecord.Nominal(a1), Result.Nominal(a3)
	move.l values.StructRecord.Count(a1), Result.Count(a3)
	cmpi.l #2, values.StructRecord.Count(a1)
	bne.w bad
	move.l values.StructRecord.Kind(a1), d6
	lea structs.HEADER_BYTES(a1), a1
	lea Result.Fields(a3), a3
	moveq #1, d5
field
	move.l (a1)+, (a3)+
	move.l (a1)+, (a3)+
	move.l (a1)+, (a3)+
	cmpi.l #structs.TYPE, d6
	beq.w typeField
	addq.l #values.Cell.Kind-values.Cell.High, a1
	move.l (a1)+, (a3)+
	bra.w nextField
typeField
	clr.l (a3)+
nextField
	dbra d5, field
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d6/a0-a3
	tst.l d0
	rts
	.bend  ; captureResult

writeOutput	.block
	move.l #OutputPath, d1
	move.l #MODE_NEWFILE, d2
	movea.l DosBase, a6
	jsr OPEN(a6)
	tst.l d0
	beq.w fail
	move.l d0, d4
	move.l d0, d1
	move.l #ResultBuffer, d2
	move.l #OUTPUT_BYTES, d3
	jsr WRITE(a6)
	move.l d0, -(sp)
	move.l d4, d1
	jsr CLOSE(a6)
	move.l (sp)+, d0
	cmpi.l #OUTPUT_BYTES, d0
	bne.w fail
	moveq #0, d0
	rts
fail
	moveq #1, d0
	rts
	.bend  ; writeOutput
	.endsection
	.section data, kind=data
DosName	.byte "dos.library", 0
OutputPath	.byte "Work:struct-vm.bin", 0
	.align 4
OversizedTypeHeader	.long values.STRUCT_TYPE, 65536, 0, 0
SchemaFields	.long 1, 0, 1, 2, 1, 2
SymbolValues	.long 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0
Defined	.byte 1, 0, 0, 0, 1, 1, 0
	.align 4
; @CASE_DATA@
	.endsection
	.section bss, kind=bss
DosBase	.res long, 1
ReturnCode	.res long, 1
SeedOffset	.res long, 1
ResultBuffer	.res byte, OUTPUT_BYTES
	.align 4
Owner	.res byte, values.OWNER_BYTES
ControlOwner	.res byte, values.OWNER_BYTES
Context	.res byte, typed.Frame.Provenance+4
	.endsection
	.output "struct-vm.hunk", format=hunk, sections=entry, code, data, bss
	.endmodule
