; Current prepared flags preserve named block topology and marker validation.
; @opforge-owner: test.amigaos.block_reference_payload
.module test.amigaos.block_reference_payload
.cpu 68020
.use experimental.amigaos.binary_block_index as blocks
.use experimental.amigaos.binary_memory as memory
.use experimental.amigaos.binary_record_view as views
.use experimental.amigaos.binary_source as source
.use experimental.amigaos.binary_scopes as scopes
.use experimental.amigaos.binary_scope_layout as layout
.use experimental.amigaos.binary_binding_records as records
.use experimental.amigaos.binary_modules as modules
.use experimental.amigaos.binary_imports as imports
.use experimental.amigaos.binary_graph as graph
.use experimental.amigaos.binary_values as values
.use opasm.amigaos.binary_expression as expr
.use exprvm.amigaos.runtime as runtime
.use exprvm.amigaos.values_runtime as value_runtime
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
cases	.block
	lea Scope, a2
	move.w #3, layout.State.Count(a2)
	lea Entries, a0
	move.l a0, layout.ENTRIES_POINTER(a2)
	move.l #3*records.ENTRY_BYTES, layout.ENTRIES+memory.Block.Used(a2)
	move.l #3*records.ENTRY_BYTES, layout.ENTRIES+memory.Block.Capacity(a2)
	lea Owners, a0
	move.l a0, layout.MODULE_STATE+modules.OWNERS_POINTER(a2)
	move.l #6, layout.MODULE_STATE+modules.OWNERS+memory.Block.Used(a2)
	move.l #6, layout.MODULE_STATE+modules.OWNERS+memory.Block.Capacity(a2)
	lea Defined, a0
	move.l a0, layout.IMPORT_STATE+imports.KNOWN_DEFINED_POINTER(a2)
	move.l #3, layout.IMPORT_STATE+imports.KNOWN_DEFINED+memory.Block.Used(a2)
	move.l #3, layout.IMPORT_STATE+imports.KNOWN_DEFINED+memory.Block.Capacity(a2)
	move.l #3, layout.IMPORT_STATE+imports.KNOWN_OWNER+values.Owner.Count(a2)
	lea Graph, a3
	move.w #1, graph.GraphState.Count(a3)
	move.w #2, graph.NODES+graph.Node.Binding(a3)
	move.w #2, graph.NODES+graph.Node.File(a3)
	move.w #1, graph.HASH+4(a3)
	; Module owner2 is binding index+1, matching graph's hash key2.
	movea.l a3, a0
	moveq #2, d1
	jsr graph.nodeForBinding
	bne.w bad
	cmpi.w #2, graph.Node.File(a0)
	bne.w bad
	lea Input, a0
	move.l #InputEnd-Input, d0
	lea Scratch, a1
	jsr blocks.index
	bne.w bad
	cmpi.l #2, d1
	bne.w bad
	lea View, a0
	lea Input, a1
	move.l #InputEnd-Input, d0
	jsr views.initBuffer
	bne.w bad
	move.l #InputEnd-Input, d0
	movea.l Scratch+memory.Block.Pointer, a1
	lea Scope, a2
	lea Graph, a3
	jsr blocks.selectView
	bne.w bad
	; Selection decisions survive independently while the prepared view stays
	; immutable. The later materialization applies only the retained omissions.
	btst #source.FLAG_OMIT_BIT, Opaque+source.RecordHeader.Flags
	bne.w bad
	lea View, a0
	jsr views.finish
	lea Input, a0
	move.l #InputEnd-Input-1, d0
	movea.l Scratch+memory.Block.Pointer, a1
	jsr blocks.applySelection
	beq.w bad
	btst #source.FLAG_OMIT_BIT, Opaque+source.RecordHeader.Flags
	bne.w bad
	move.l #InputEnd-Input, d0
	jsr blocks.applySelection
	bne.w bad
	btst #source.FLAG_OMIT_BIT, Real+source.RecordHeader.Flags
	bne.w bad
	btst #source.FLAG_OMIT_BIT, Opaque+source.RecordHeader.Flags
	beq.w bad
	movea.l Scratch+memory.Block.Pointer, a0
	tst.w blocks.HEADER_BYTES+blocks.Span.Live(a0)
	beq.w bad
	tst.w blocks.HEADER_BYTES+blocks.SPAN_BYTES+blocks.Span.Live(a0)
	bne.w bad
	moveq #0, d0
	bra.w finish
bad
	moveq #1, d0
finish
	move.l d0, -(sp)
	lea View, a0
	jsr views.finish
	lea Scratch, a0
	jsr memory.release
	move.l (sp)+, d0
	tst.l d0
	rts
.bend
.endsection
.section data, kind=data
DosName	.byte "dos.library", 0
OutputName	.byte "Work:block-reference-payload.bin", 0
Result	.byte $5a
Owners	.word 2, 2, 2
Defined	.byte 1, 1, 1
Input
	.byte StringEnd-Input-1, 0
	.word 1
	.byte expr.VALUE_TAG, StringProgramEnd-StringProgram
StringProgram
	.byte runtime.EXPRVM_OPCODE_PUSH_STRING_LITERAL, 5, 0
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 2, 0, runtime.EXPRVM_OPCODE_END, runtime.EXPRVM_OPCODE_BUILD_STRUCT
	.byte runtime.EXPRVM_OPCODE_CALL_BUILTIN, value_runtime.LEN_ID, 0, 1
	.byte runtime.EXPRVM_OPCODE_PUSH_SYMBOL, 1, 0
	.byte runtime.EXPRVM_OPCODE_APPLY_BINARY, runtime.EXPRVM_BINARY_ADD, runtime.EXPRVM_OPCODE_END
StringProgramEnd
StringEnd
	; Layout payloads are opaque to emitted-code reachability even when their
	; numeric bytes resemble a reference to a collectible block.
	.byte 7, source.FLAG_LAYOUT
	.word 2
	.byte 0
	.word 2
	.byte 0
	.byte 9, source.FLAG_PLAN
	.word 2
	.byte source.MACRO_PLAN
	.long $00000200
	.byte source.PLAN_TOKEN_BYTES
	.byte 9, source.LINE_PLAN_FLAG
	.word 2
	.byte source.LINE_PLAN
	.long $00000200
	.byte source.PLAN_TOKEN_BYTES
	.byte 9, source.LINE_PLAN_FLAG
	.word 2
	.byte source.OPTIONAL_LINE_PLAN
	.long $00000200
	.byte source.PLAN_TOKEN_BYTES
Real
	.byte 8, source.FLAG_BLOCK_OPEN
	.word 5
	.byte 0, 0, 1, 0, source.TOKEN_LABEL_DECLARATION
	.byte 3, source.FLAG_BLOCK_CLOSE
	.word 6
Opaque
	.byte 8, source.FLAG_BLOCK_OPEN
	.word 7
	.byte 0, 0, 2, 0, source.TOKEN_LABEL_DECLARATION
	.byte 3, source.FLAG_BLOCK_CLOSE
	.word 8
InputEnd
.endsection
.section bss, kind=bss
.align 4
Scope	.res byte, scopes.SCRATCH_BYTES
Graph	.res byte, graph.SCRATCH_BYTES
Entries	.res byte, 3*records.ENTRY_BYTES
Scratch	.res byte, memory.Block.Used+4
View	.res byte, views.FRAME_BYTES
DosBase	.res long, 1
OutputHandle	.res long, 1
ReturnCode	.res long, 1
.endsection
.output "block-reference-payload.hunk", format=hunk, sections=entry, code, data, bss
.endmodule
