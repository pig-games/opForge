; Opaque plan handles must not become source IDs during remapping.
; @opforge-owner: test.amigaos.remap_plan_trailers
.module test.amigaos.remap_plan_trailers
.cpu 68020
.use experimental.amigaos.binary_binding_records as records
.use opasm.amigaos.binary_expression as expression
.use exprvm.amigaos.runtime as runtime
.use experimental.amigaos.binary_templates as templates
.use experimental.amigaos.binary_source as source
.include "binding_telemetry.i"
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
	move.l d0, CaseStatus
	lea DosName, a1
	moveq #36, d0
	movea.l EXEC_BASE.w, a6
	jsr EXEC_OPEN_LIBRARY(a6)
	move.l d0, DosBase
	beq.w done
	.BINDING_DIAGNOSTIC_REPORT DosBase
	tst.l CaseStatus
	bne.w closeLibrary
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
cases	.block
	move.w #$abcd, Entries+records.Entry.Target
	lea Call, a0
	move.l #CallEnd-Call, d0
	bsr.w remap
	bne.w bad
	cmpi.b #$ab, Call+6
	bne.w bad
	cmpi.b #$cd, Call+7
	bne.w bad
	lea CallEnd, a0
	suba.w #source.PLAN_TOKEN_BYTES, a0
	cmpi.l #$00ffff00, source.PlanToken.Handle(a0)
	bne.w bad
	lea Line, a0
	move.l #LineEnd-Line, d0
	bsr.w remap
	bne.w bad
	cmpi.b #$ab, Line+6
	bne.w bad
	cmpi.b #$cd, Line+7
	bne.w bad
	lea LineEnd, a0
	suba.w #source.PLAN_TOKEN_BYTES, a0
	cmpi.l #$00ffff00, source.PlanToken.Handle(a0)
	bne.w bad
	lea Optional, a0
	move.l #OptionalEnd-Optional, d0
	bsr.w remap
	bne.w bad
	cmpi.b #$ab, Optional+6
	bne.w bad
	cmpi.b #$cd, Optional+7
	bne.w bad
	lea OptionalEnd, a0
	suba.w #source.PLAN_TOKEN_BYTES, a0
	cmpi.l #$00ffff00, source.PlanToken.Handle(a0)
	bne.w bad
	lea Scalar, a0
	move.l #ScalarEnd-Scalar, d0
	bsr.w remap
	bne.w bad
	cmpi.b #$ab, Scalar+5
	bne.w bad
	cmpi.b #$cd, Scalar+6
	bne.w bad
	lea Mutable, a0
	move.l #MutableEnd-Mutable, d0
	bsr.w remap
	bne.w bad
	cmpi.b #$ab, Mutable+5
	bne.w bad
	cmpi.b #$cd, Mutable+6
	bne.w bad
	cmpi.b #$cd, MutableProgram+1
	bne.w bad
	cmpi.b #$ab, MutableProgram+2
	bne.w bad
	lea Conditional, a0
	move.l #ConditionalEnd-Conditional, d0
	bsr.w remap
	bne.w bad
	cmpi.b #$ab, Conditional+5
	bne.w bad
	cmpi.b #$cd, Conditional+6
	bne.w bad
	cmpi.b #$cd, ConditionalProgram+1
	bne.w bad
	cmpi.b #$ab, ConditionalProgram+2
	bne.w bad
	lea ZeroCall, a0
	move.l #ZeroCallEnd-ZeroCall, d0
	bsr.w remap
	beq.w bad
	lea WrongWidth, a0
	move.l #WrongWidthEnd-WrongWidth, d0
	bsr.w remap
	beq.w bad
	lea MissingCall, a0
	move.l #MissingCallEnd-MissingCall, d0
	bsr.w remap
	beq.w bad
	lea EmptyCall, a0
	move.l #EmptyCallEnd-EmptyCall, d0
	bsr.w remap
	beq.w bad
	lea ZeroLine, a0
	move.l #ZeroLineEnd-ZeroLine, d0
	bsr.w remap
	beq.w bad
	lea MixedLine, a0
	move.l #MixedLineEnd-MixedLine, d0
	bsr.w remap
	beq.w bad
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
.bend  ; cases
remap	.block
	.BINDING_DIAGNOSTIC_CLEAR
	.BINDING_ATTEMPT #binding_diagnostic.REMAP, #-1, #-1
	lea Entries, a1
	move.l #$8000, d1
	moveq #1, d2
	jsr records.remap
	lea DiagnosticState, a0
	.BINDING_DIAGNOSTIC_COMMIT d0, a0, DiagnosticView
	rts
.bend  ; remap
.endsection
.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:remap-plan-trailers.bin", 0
Result	.byte $5a
Call
	.byte CallEnd-Call-1,source.FLAG_PLAN
	.word 1
	.byte 7,0,$80,0,0
	.byte source.MACRO_PLAN
	.long $00ffff00
	.byte source.PLAN_TOKEN_BYTES
CallEnd
Line
	.byte LineEnd-Line-1,templates.LINE_PLAN_FLAG
	.word 1
	.byte 7,0,$80,0,0
	.byte templates.TOKEN_LINE_PLAN
	.long $00ffff00
	.byte source.PLAN_TOKEN_BYTES
LineEnd
Optional
	.byte OptionalEnd-Optional-1,templates.LINE_PLAN_FLAG
	.word 1
	.byte 7,0,$80,0,0
	.byte templates.TOKEN_OPTIONAL_LINE_PLAN
	.long $00ffff00
	.byte source.PLAN_TOKEN_BYTES
OptionalEnd
Scalar
 .byte ScalarEnd-Scalar-1,source.FLAG_MUTABLE_SNAPSHOT
 .word 1
 .byte 0,$80,0,0,source.TOKEN_MUTABLE_DECLARATION,2
 .long 6
ScalarEnd
Mutable
 .byte MutableEnd-Mutable-1,source.FLAG_MUTABLE_SNAPSHOT
 .word 1
 .byte 0,$80,0,0,source.TOKEN_MUTABLE_DECLARATION,expression.COMPILED_TAG
 .byte MutableProgramEnd-MutableProgram
MutableProgram
 .byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL,0,$80
 .byte runtime.COMPACT_I8,6,runtime.COMPACT_ADD,runtime.EXPRVM_OPCODE_END
MutableProgramEnd
MutableEnd
Conditional
 .byte ConditionalEnd-Conditional-1,source.FLAG_MUTABLE_SNAPSHOT
 .word 1
 .byte 0,$80,0,0,source.TOKEN_CONDITIONAL_DECLARATION,expression.COMPILED_TAG
 .byte ConditionalProgramEnd-ConditionalProgram
ConditionalProgram
 .byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL,0,$80
 .byte runtime.COMPACT_I8,6,runtime.COMPACT_ADD,runtime.EXPRVM_OPCODE_END
ConditionalProgramEnd
ConditionalEnd
ZeroCall
 .byte ZeroCallEnd-ZeroCall-1,source.FLAG_PLAN
 .word 1
 .byte 7,0,0,0,0,source.MACRO_PLAN
 .long 0
 .byte source.PLAN_TOKEN_BYTES
ZeroCallEnd
WrongWidth
 .byte WrongWidthEnd-WrongWidth-1,source.FLAG_PLAN
 .word 1
 .byte 7,0,0,0,0,source.MACRO_PLAN
 .long $00ffff00
 .byte 5
WrongWidthEnd
ZeroLine
 .byte ZeroLineEnd-ZeroLine-1,source.LINE_PLAN_FLAG
 .word 1
 .byte 7,0,0,0,0,source.LINE_PLAN
 .long 0
 .byte source.PLAN_TOKEN_BYTES
ZeroLineEnd
MixedLine
 .byte MixedLineEnd-MixedLine-1,source.FLAG_PLAN+source.LINE_PLAN_FLAG
 .word 1
 .byte 7,0,0,0,0,source.LINE_PLAN
 .long $00ffff00
 .byte source.PLAN_TOKEN_BYTES
MixedLineEnd
MissingCall
 .byte MissingCallEnd-MissingCall-1,source.FLAG_PLAN
 .word 1
 .byte 7,0,0,0,0
MissingCallEnd
EmptyCall
 .byte EmptyCallEnd-EmptyCall-1,source.FLAG_PLAN
 .word 1
EmptyCallEnd
.endsection
.section bss, kind=bss
.align 4
Entries	.res byte, records.ENTRY_BYTES
CaseStatus	.res long, 1
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
.ifdef OPFORGE_DEBUG_CONTRACTS
.ifdef OPFORGE_MEMORY_TELEMETRY
.ifdef OPFORGE_PREPARATION_PROGRESS
DiagnosticState	.res word, 1
DiagnosticView	.res byte, binding_diagnostic.View.Owner+2
.endif
.endif
.endif
.endsection
.output "remap-plan-trailers.hunk", format=hunk, sections=entry,code,data,bss
.endmodule
