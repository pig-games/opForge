; Streaming line tokenization and numeric lowering. Scratch is caller-owned and
; bounded; no source text enters packed records.
; @opforge-owner: experimental.amigaos.binary_frontend
	.module experimental.amigaos.binary_frontend
	.cpu 68020
	.include "memory_telemetry.i"
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_source as writer
	.use experimental.amigaos.binary_prepare as prepare
	.use experimental.amigaos.binary_data_prepare as data_prepare
	.use experimental.amigaos.binary_metadata_prepare as metadata
	.use experimental.amigaos.binary_scopes as scopes
	.use experimental.amigaos.binary_scope_layout as layout
	.use experimental.amigaos.binary_structs as structs
	.use experimental.amigaos.binary_conditionals as conditionals
	.use experimental.amigaos.binary_templates as templates
	.use experimental.amigaos.binary_capture as capture
	.use experimental.amigaos.binary_macro_plans as macro_plans
	.use experimental.amigaos.binary_imports as imports
	.use experimental.amigaos.binary_graph as graph
	.use experimental.amigaos.binary_block_index as blocks
	.use experimental.amigaos.binary_modules as modules
	.use experimental.amigaos.binary_memory as memory
	.use tkvm.amigaos.runtime as tokenizer
	.use tkvm.amigaos.fragments as fragment_tokenizer
	.use experimental.amigaos.binary_macro_fragments as fragment_binding
	.use tkvm.amigaos.control as control
	.use prvm.amigaos.macro_runtime as macro_runtime
	.use prvm.amigaos.abi as parser_abi
	.use prvm.amigaos.macro_spelling as spelling
	.pub
Frame	.struct
Package	.long ?
Source	.long ?
SourceBytes	.long ?
Output	.long ?
Capacity	.long ?
Used	.long ?
NameCount	.long ?
Scratch	.long ?
Graph	.long ?
GraphBefore	.long ?
FileInclude	.long ?  ; optional preparation callback, A0=Frame,A1=PRVM file plan
Origin	.long ?  ; opaque application source identity for file callbacks
Metadata	.long ?  ; optional metadata.Config, retained outside execution records
RootFile	.word ?  ; current source belongs to the entry file (includes inherit it)
Reserved	.word ?
	.endstruct
	.pub
FRAME_BYTES = Frame.Reserved+2
	.pub
GRAPH_BYTES = graph.SCRATCH_BYTES
GRAPH_SPAN_BYTES = graph.MAX_SPANS*graph.SPAN_BYTES
	.priv
HEADER_BYTES = package.HEADER_BYTES
PROGRAM = 0
PROGRAM_BYTES = 4
PACKAGE_BASE = 8
DICTIONARY_COUNT = 12
PACKAGE_END = 16
LINE_NUMBER = 20
NEXT_ID = 24
CAPTURE_RECORD = 28
LINE_FRAME = CAPTURE_RECORD+4
TOKENS = LINE_FRAME+writer.FRAME_BYTES
TOKEN_CAPACITY = 64
LEXICAL_BYTES = 1024
; Preserve the lexical budget plus copied number spellings and u64 metadata.
NUMBER_BYTES = 2*LEXICAL_BYTES+TOKEN_CAPACITY*8
; Recipe copies and literal-fragment framing are bounded by lexical work.
COMPOSED_BYTES = 3*LEXICAL_BYTES+TOKEN_CAPACITY*4
LEXEME_BYTES = NUMBER_BYTES+COMPOSED_BYTES
PACKED_MAP = TOKENS+TOKEN_CAPACITY*20
LEXEMES = (PACKED_MAP+(TOKEN_CAPACITY+1)*2+3)&$fffffffc
; Keep directly addressed regions below signed d16 displacement limits.
PACKAGE_BUCKETS = LEXEMES+LEXEME_BYTES
	.pub
MACRO_REQUEST = PACKAGE_BUCKETS+256*4
MACRO_EVENTS = MACRO_REQUEST+parser_abi.PRVM_REQUEST_FRAME_SIZE
MACRO_PLANS = MACRO_EVENTS+64*parser_abi.PRVM_RESULT_RECORD_SIZE
MACRO_FRAME = MACRO_PLANS+macro_plans.STATE_BYTES
MACRO_SPELL_EVENTS = MACRO_FRAME+macro_plans.GENERATED_FRAME_BYTES
MACRO_SPELL_FRAME = MACRO_SPELL_EVENTS+64*parser_abi.PRVM_RESULT_RECORD_SIZE
MACRO_SPELL_SCRATCH = MACRO_SPELL_FRAME+spelling.FRAME_BYTES
PREPARED_LINE = MACRO_SPELL_SCRATCH+spelling.SCRATCH_BYTES
SCOPE_STATE = PREPARED_LINE+256
CONDITION_STATE = SCOPE_STATE+scopes.SCRATCH_BYTES
TEMPLATE_STATE = CONDITION_STATE+conditionals.SCRATCH_BYTES
SCRATCH_BYTES = TEMPLATE_STATE+templates.SCRATCH_BYTES
	.priv
FRAGMENT_REQUEST = fragment_binding.FRAME_BYTES
FRAGMENT_VIEWS = FRAGMENT_REQUEST+fragment_tokenizer.FRAME_BYTES
FRAGMENT_SCRATCH = FRAGMENT_VIEWS+64*fragment_binding.FRAGMENT_BYTES
GENERATED_FRAGMENTS = 0
GENERATED_REQUEST = GENERATED_FRAGMENTS+2*fragment_tokenizer.FRAGMENT_BYTES
GENERATED_RECORD = GENERATED_REQUEST+fragment_tokenizer.FRAME_BYTES
GENERATED_SPACE = GENERATED_RECORD+256
GENERATED_BYTES = GENERATED_SPACE+2
	.priv
; Package nodes hold capsule-relative entries and scratch-relative chain links.
Node	.struct
Entry	.long ?
Next	.long ?
	.endstruct
	.section code, kind=code
	.pub
; Compute scratch including one offset-chain node per dictionary entry.
; A0=readable capsule header; D0=status, D1=bytes on success; CCR=D0.
; Other registers preserved. The capsule byte bound limits count, not a new cap.
scratchSize	.block
	movem.l d2, -(sp)
	cmpi.l #package.MAGIC, package.Header.Magic(a0)
	bne.w bad
	move.l package.Header.Bytes(a0), d2
	cmpi.l #HEADER_BYTES, d2
	blo.w bad
	subi.l #HEADER_BYTES, d2
	lsr.l #3, d2
	move.l package.Header.DictionaryCount(a0), d1
	cmp.l d2, d1
	bhi.w bad
	lsl.l #3, d1
	addi.l #SCRATCH_BYTES, d1
	bcs.w bad
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2
	rts
	.bend  ; scratchSize
; Begin a streaming frontend session. A0=Frame with a readable package capsule
; and scratchSize bytes of caller-owned aligned, zero-initialized scratch (or a
; prior session ended with finish). D0=0 success, 1 invalid.
; Resets symbols and source-line numbering. Preserves other registers; CCR=D0.
begin	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	clr.l Frame.Used(a5)
	clr.l Frame.NameCount(a5)
	clr.l Frame.Graph(a5)
	movea.l Frame.Scratch(a5), a6
	move.l a6, d0
	beq.w failed
	move.l a6, d0
	andi.l #3, d0
	bne.w failed
	clr.l CAPTURE_RECORD(a6)
	lea PACKAGE_BUCKETS(a6), a0
	move.l #256-1, d0
clearBuckets
	clr.l (a0)+
	dbra d0, clearBuckets
	bsr.w configure
	bne.w failed
	lea SCOPE_STATE(a6), a0
	move.l NEXT_ID(a6), d0
	movea.l Frame.Package(a5), a1
	moveq #0, d1
	move.w package.Header.EndDirective(a1), d1
	jsr scopes.begin
	bne.w failed
	lea SCOPE_STATE(a6), a0
	adda.l #scopes.STRUCT_STATE, a0
	movea.l Frame.Package(a5), a1
	jsr structs.configure
	lea SCOPE_STATE(a6), a0
	adda.l #scopes.SCRATCH_BYTES, a0
	jsr conditionals.begin
	bne.w failed
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	jsr templates.begin
	bne.w failed
	lea MACRO_PLANS(a6), a0
	jsr macro_plans.begin
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	lea MACRO_PLANS(a6), a1
	move.l a1, templates.State.Plans(a0)
	move.l #generatedPlan, templates.State.GeneratedPlan(a0)
	move.l #fragmentLine, templates.State.FragmentLine(a0)
	move.l a5, templates.State.ParserContext(a0)
	move.l Frame.Package(a5), templates.State.Package(a0)
	move.l #1, LINE_NUMBER(a6)
	jsr scopes.count
	move.l d0, Frame.NameCount(a5)
	moveq #0, d0
	bra.w done
failed
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; begin
; Opt in to numeric module graph ordering. A0=Frame, A1=caller-owned graph
; scratch. D0/CCR=status; other registers preserved.
beginGraph	.block
	movem.l a0-a2, -(sp)
	move.l a1, d0
	beq.w badGraph
	andi.l #3, d0
	bne.w badGraph
	move.l a1, Frame.Graph(a0)
	movea.l a1, a0
	jsr graph.begin
	bra.w graphDone
badGraph
	moveq #1, d0
graphDone
	movem.l (sp)+, a0-a2
	tst.l d0
	rts
	.bend  ; beginGraph

; A0=Frame,A1=file basename,D0=name bytes. Enter file-derived module ownership
; before lowering physical lines. D0/CCR=status; other registers preserved.
beginFileDerived	.block
	movem.l d1-d3/a0-a2, -(sp)
	movea.l a0, a2
	movea.l Frame.Scratch(a2), a0
	lea SCOPE_STATE(a0), a0
	jsr scopes.beginFileDerived
	bne.w fileDerivedDone
	move.l d1, d3
	movea.l Frame.Graph(a2), a0
	move.l a0, d0
	beq.w fileDerivedCount
	moveq #0, d0
	moveq #0, d1
	move.l d3, d2
	jsr graph.line
	bne.w fileDerivedDone
fileDerivedCount
	movea.l Frame.Scratch(a2), a0
	lea SCOPE_STATE(a0), a0
	jsr scopes.count
	move.l d0, Frame.NameCount(a2)
	moveq #0, d0
fileDerivedDone
	movem.l (sp)+, d1-d3/a0-a2
	tst.l d0
	rts
	.bend  ; beginFileDerived

; A0=Frame. Close any file-derived module not already closed by .end; graph
; receives a zero-byte boundary, leaving physical record provenance intact.
; D0/CCR=status; other registers preserved.
endFileDerived	.block
	movem.l d1-d2/a0-a2, -(sp)
	movea.l a0, a2
	movea.l Frame.Scratch(a2), a1
	moveq #0, d2
	tst.w SCOPE_STATE+scopes.MODULE_STATE+modules.State.FileDerived(a1)
	beq.w noFileDerivedActive
	move.w SCOPE_STATE+scopes.MODULE_STATE+modules.State.Active(a1), d2
noFileDerivedActive
	lea SCOPE_STATE(a1), a0
	jsr scopes.endFileDerived
	bne.w endFileDerivedDone
	tst.l d2
	beq.w endFileDerivedOk
	movea.l Frame.Graph(a2), a0
	move.l a0, d0
	beq.w endFileDerivedOk
	moveq #0, d0
	move.l d2, d1
	moveq #0, d2
	jsr graph.line
	bra.w endFileDerivedDone
endFileDerivedOk
	moveq #0, d0
endFileDerivedDone
	movem.l (sp)+, d1-d2/a0-a2
	tst.l d0
	rts
	.bend  ; endFileDerived
; Set the physical source line before lowering a line from an included file.
; A0=active Frame, D0=1..65535. D0/CCR=status; other registers preserved.
setLine	.block
	movem.l d1/a1, -(sp)
	movea.l Frame.Scratch(a0), a1
	move.l a1, d1
	beq.w invalidLine
	tst.l d0
	beq.w invalidLine
	cmpi.l #65535, d0
	bhi.w invalidLine
	move.l d0, LINE_NUMBER(a1)
	moveq #0, d0
	bra.w lineSet
invalidLine
	moveq #1, d0
lineSet
	movem.l (sp)+, d1/a1
	tst.l d0
	rts
	.bend  ; setLine
; Lower one caller-bounded line. A0=the session Frame; Source excludes its line
; ending and Output has per-line packed-record capacity. D0=0 success, 1 failure.
; Used is this line's packed byte count; NameCount is the next free identifier.
; Preserves other registers; CCR reflects D0. Used=0 on failure.
line	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	clr.l Frame.Used(a5)
	movea.l Frame.Scratch(a5), a6
	move.l a6, d0
	beq.w failed
	cmpi.l #65535, LINE_NUMBER(a6)
	bhi.w failed
	movea.l Frame.Source(a5), a0
	move.l Frame.SourceBytes(a5), d0
	bmi.w failed
	move.l a0, d1
	add.l d0, d1
	bcs.w failed
	movea.l Frame.Output(a5), a4
	move.l Frame.Capacity(a5), d1
	beq.w failed
	move.l a4, d2
	add.l d1, d2
	bcs.w failed
	clr.l CAPTURE_RECORD(a6)
	bsr.w tokenizeLine
	bne.w failed
	bsr.w lowerTokens
	bra.w done
failed
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; line

; A0=session,A1=capture.Frame with Arena=caller-owned memory.Block.
; Capture unbound TKVM rows and all VM candidate plans without binding names or
; advancing scope/template/graph/line state. D0/CCR=status,D1=record handle;
; other registers preserved. Source is used only here, never by replayLine.
captureLine	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a4
	movea.l Frame.Scratch(a5), a6
	move.l a6, d0
	beq.w bad
	cmpi.l #65535, LINE_NUMBER(a6)
	bhi.w bad
	bsr.w tokenizeLine
	bne.w bad
	lea TOKENS(a6), a0
	move.l a0, capture.Frame.Tokens(a4)
	move.l d1, capture.Frame.Count(a4)
	move.l d1, LINE_FRAME+writer.Frame.Count(a6)
	lea LEXEMES(a6), a0
	move.l a0, capture.Frame.Lexemes(a4)
	move.l d3, capture.Frame.LexemeBytes(a4)
	move.l Frame.SourceBytes(a5), capture.Frame.SourceBytes(a4)
	move.l Frame.Origin(a5), capture.Frame.Origin(a4)
	move.l LINE_NUMBER(a6), capture.Frame.Line(a4)
	move.w Frame.RootFile(a5), capture.Frame.RootFile(a4)
	clr.w capture.Frame.Reserved(a4)
	suba.w #memory.Block.Used+4, sp
	movea.l sp, a0
	clr.l memory.Block.Pointer(a0)
	clr.l memory.Block.Capacity(a0)
	clr.l memory.Block.Used(a0)
	move.l a0, capture.Frame.Plans(a4)
	bsr.w captureCandidates
	bne.w releaseBad
	movea.l a4, a0
	jsr capture.create
	bra.w release
releaseBad
	moveq #1, d0
	moveq #0, d1
release
	move.l d0, -(sp)
	movea.l capture.Frame.Plans(a4), a0
	jsr memory.release
	move.l (sp)+, d0
	clr.l capture.Frame.Plans(a4)
	adda.w #memory.Block.Used+4, sp
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; captureLine

; A0=session,A1=capture memory.Block,D1=record handle. Replay existing owned
; tokens through current contextual binding and all ordinary semantic consumers.
; No original source is read or re-tokenized. D0/CCR=status; other regs preserved.
; Frame.Used/NameCount and nextExpansion retain the direct line API contract.
replayLine	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	clr.l Frame.Used(a5)
	movea.l Frame.Scratch(a5), a6
	move.l a6, d0
	beq.w bad
	movea.l a1, a0
	jsr capture.resolve
	bne.w bad
	movea.l a1, a4
	move.l capture.Record.LexemeBytes(a4), d3
	cmpi.l #LEXEME_BYTES, d3
	bhi.w bad
	move.l Frame.Source(a5), -(sp)
	move.l Frame.SourceBytes(a5), -(sp)
	clr.l Frame.Source(a5)
	clr.l Frame.SourceBytes(a5)
	move.l a4, CAPTURE_RECORD(a6)
	move.l capture.Record.Line(a4), LINE_NUMBER(a6)
	move.l capture.Record.Origin(a4), Frame.Origin(a5)
	move.w capture.Record.RootFile(a4), Frame.RootFile(a5)
	move.l capture.Record.Count(a4), d1
	move.l d1, d0
	mulu.w #20, d0
	lea capture.HEADER_BYTES(a4), a0
	lea TOKENS(a6), a1
	bsr.w copyCaptureBytes
	lea LEXEMES(a6), a1
	move.l d3, d0
	bsr.w copyCaptureBytes
	move.l capture.Record.Count(a4), d1
	bsr.w lowerTokens
	clr.l CAPTURE_RECORD(a6)
	move.l (sp)+, Frame.SourceBytes(a5)
	move.l (sp)+, Frame.Source(a5)
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; replayLine

; A0=session,A1=capture memory.Block,D1=record handle. Materialize only the
; writer record with the current binder. No plans/templates/scope semantics or
; line advancement. D0/CCR=status; other registers preserved. Used/NameCount
; are outputs. Configuration owners select records before calling this entry.
bindCapture	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	clr.l Frame.Used(a5)
	movea.l Frame.Scratch(a5), a6
	move.l a6, d0
	beq.w bad
	movea.l a1, a0
	jsr capture.resolve
	bne.w bad
	movea.l a1, a4
	move.l capture.Record.LexemeBytes(a4), d3
	cmpi.l #LEXEME_BYTES, d3
	bhi.w bad
	move.l capture.Record.Line(a4), LINE_NUMBER(a6)
	move.l capture.Record.Origin(a4), Frame.Origin(a5)
	move.w capture.Record.RootFile(a4), Frame.RootFile(a5)
	move.l capture.Record.Count(a4), d0
	mulu.w #20, d0
	lea capture.HEADER_BYTES(a4), a0
	lea TOKENS(a6), a1
	bsr.w copyCaptureBytes
	lea LEXEMES(a6), a1
	move.l d3, d0
	bsr.w copyCaptureBytes
	move.l Frame.Source(a5), -(sp)
	move.l Frame.SourceBytes(a5), -(sp)
	clr.l Frame.Source(a5)
	clr.l Frame.SourceBytes(a5)
	move.l capture.Record.Count(a4), d1
	bsr.w writeTokens
	bne.w written
	move.l d1, Frame.Used(a5)
	lea SCOPE_STATE(a6), a0
	jsr scopes.count
	move.l d0, Frame.NameCount(a5)
	moveq #0, d0
written
	move.l (sp)+, Frame.SourceBytes(a5)
	move.l (sp)+, Frame.Source(a5)
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; bindCapture
	.priv

; A0=owned input,A1=destination,D0=bytes. D0/A0/A1 scratch; CCR unspecified.
copyCaptureBytes	.block
	tst.l d0
	beq.w done
next
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w next
done
	rts
	.bend  ; copyCaptureBytes

; A4=capture request,A5=session,A6=scratch. Candidate grammar failures remain
; outcomes until contextual replay selects a role. Allocation failure aborts.
captureCandidates	.block
	movem.l d1-d7/a0-a4, -(sp)
	suba.w #(TOKEN_CAPACITY+1)*2, sp
	movea.l sp, a0
	moveq #0, d0
identityMap
	move.w d0, (a0)+
	addq.w #1, d0
	cmpi.w #TOKEN_CAPACITY+1, d0
	blo.w identityMap
	moveq #1, d4
	bsr.w captureCandidate
	bne.w bad
	move.l d5, capture.Frame.CallStatus(a4)
	move.l d6, capture.Frame.CallHandle(a4)
	move.l d7, capture.Frame.CallRecipeStatus(a4)
	moveq #2, d4
	bsr.w captureCandidate
	bne.w bad
	move.l d5, capture.Frame.HeaderStatus(a4)
	move.l d6, capture.Frame.HeaderHandle(a4)
	; Whole-line spelling plans are needed only for template string consumers.
	moveq #1, d5
	moveq #0, d6
	lea TOKENS(a6), a0
	move.l capture.Frame.Count(a4), d0
findString
	tst.l d0
	beq.w lineReady
	cmpi.w #tokenizer.TK_KIND_STRING, (a0)
	beq.w stringCandidate
	adda.w #20, a0
	subq.l #1, d0
	bra.w findString
stringCandidate
	moveq #3, d4
	bsr.w captureCandidate
	bne.w bad
	tst.l d7
	beq.w lineReady
	move.l d7, d5  ; selected string-template replay requires valid recipes
lineReady
	move.l d5, capture.Frame.LineStatus(a4)
	move.l d6, capture.Frame.LineHandle(a4)
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	adda.w #(TOKEN_CAPACITY+1)*2, sp
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; captureCandidates

; A4=capture request,A5=session,A6=scratch,D4=candidate kind.
; Identity map is at caller SP. D5=descriptor status,D6=handle,D7=recipe status;
; D0/CCR=allocation/capture status. Other registers preserved.
captureCandidate	.block
	movea.l sp, a0
	addq.l #4, a0  ; skip BSR return address to caller identity map
	movem.l d1-d4/a0-a4, -(sp)
	moveq #0, d6
	moveq #0, d7
	cmpi.l #3, d4
	beq.w stringRows
	bsr.w sourceDescriptors
	move.l d0, d5
	bne.w candidateReady
	tst.l d1
	bne.w rowsReady
	moveq #1, d5  ; successful grammar no-match has no candidate plan
	bra.w candidateReady
stringRows
	lea MACRO_EVENTS(a6), a1
	movea.l a1, a0
	moveq #macro_plans.ROW_BYTES/4-1, d0
clearRow
	clr.l (a0)+
	dbra d0, clearRow
	move.w #parser_abi.PRVM_RESULT_MACRO_LINE, macro_plans.Row.Kind(a1)
	move.l capture.Frame.Count(a4), macro_plans.Row.PackedEnd(a1)
	move.l Frame.SourceBytes(a5), macro_plans.Row.SpellingEnd(a1)
	moveq #1, d1
	moveq #0, d5
rowsReady
	lea MACRO_FRAME(a6), a0
	move.l d1, macro_plans.Frame.Count(a0)
	move.l capture.Frame.Plans(a4), macro_plans.Frame.Arena(a0)
	lea MACRO_EVENTS(a6), a1
	move.l a1, macro_plans.Frame.Events(a0)
	move.l Frame.Source(a5), macro_plans.Frame.Source(a0)
	move.l Frame.SourceBytes(a5), macro_plans.Frame.SourceBytes(a0)
	; Saved A0 is the caller's identity map (D1-D4 precede it on stack).
	move.l 16(sp), macro_plans.Frame.PackedMap(a0)
	move.l capture.Frame.Count(a4), macro_plans.Frame.TokenCount(a0)
	clr.l macro_plans.Frame.RecipeEvents(a0)
	clr.l macro_plans.Frame.RecipeCount(a0)
	cmpi.l #2, d4
	beq.w createCandidate
	bsr.w captureFragments
	move.l d0, d7
	beq.w createCandidate
	clr.l macro_plans.Frame.RecipeEvents(a0)
	clr.l macro_plans.Frame.RecipeCount(a0)
createCandidate
	jsr macro_plans.create
	bne.w bad
	move.l d1, d6
candidateReady
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d4/a0-a4
	tst.l d0
	rts
	.bend  ; captureCandidate

; A5=session,A6=scratch. Existing TKVM is the only physical-line lexer.
; D0/CCR=status,D1=token count,D3=lexeme bytes; other registers preserved.
tokenizeLine	.block
	movem.l d2/d4-d7/a0-a4, -(sp)
	movea.l Frame.Source(a5), a0
	move.l Frame.SourceBytes(a5), d0
	bmi.w bad
	move.l a0, d1
	add.l d0, d1
	bcs.w bad
	lea TOKENS(a6), a1
	lea LEXEMES(a6), a2
	movea.l PROGRAM(a6), a3
	moveq #TOKEN_CAPACITY, d1
	move.l #LEXEME_BYTES, d2
	move.l PROGRAM_BYTES(a6), d3
	.MEMORY_STAGE #2
	jsr tokenizer.tkvmRun68000
	bne.w bad
	.MEMORY_STAGE #3
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2/d4-d7/a0-a4
	tst.l d0
	rts
	.bend  ; tokenizeLine

; A5=session,A6=scratch,D1=token count,D3=lexeme bytes. Bind only now.
; D0/CCR=status; other registers preserved.
lowerTokens	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea SCOPE_STATE(a6), a0
	jsr scopes.startLine
	bsr.w writeTokens
	bne.w failed
	.MEMORY_DETAIL_BEGIN #1
	bsr.w initialPlan
	.MEMORY_DETAIL_END #1
	bne.w failed
	.MEMORY_DETAIL_BEGIN #4
	bsr.w stringLinePlan
	.MEMORY_DETAIL_END #4
	bne.w failed
	movea.l Frame.Output(a5), a0
	movea.l a6, a1
	adda.l #TEMPLATE_STATE, a1
	lea SCOPE_STATE(a6), a2
	moveq #0, d0
	movea.l a6, a0
	adda.l #CONDITION_STATE, a0
	move.w conditionals.State.Active(a0), d0
	movea.l Frame.Output(a5), a0
	move.l Frame.Origin(a5), templates.State.Origin(a1)
	.MEMORY_DETAIL_BEGIN #5
	jsr templates.line
	.MEMORY_DETAIL_END #5
	bne.w failed
	cmpi.w #1, d1
	beq.w segmentConsumed
	cmpi.w #2, d1
	bne.w process
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	movea.l Frame.Output(a5), a1
	lea SCOPE_STATE(a6), a2
	.MEMORY_DETAIL_BEGIN #6
	jsr templates.next
	.MEMORY_DETAIL_END #6
	bne.w failed
	tst.l d1
	beq.w failed
	bsr.w expandRecord
	bne.w failed
	tst.l Frame.Used(a5)
	beq.w segmentConsumed
	addq.l #1, LINE_NUMBER(a6)
	moveq #0, d0
	bra.w done
process
	bsr.w processRecord
	bne.w failed
	addq.l #1, LINE_NUMBER(a6)
	moveq #0, d0
	bra.w done
segmentConsumed
	movea.l Frame.Output(a5), a0
	move.b #3, (a0)
	clr.b 1(a0)
	bra.w process
failed
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; lowerTokens

; A5=session,A6=scratch,D1=count,D3=lexeme bytes. Write current lexical
; rows with current binder; no semantic processing or source parsing.
; D0/CCR=status,D1=writer record bytes; other registers preserved.
writeTokens	.block
	movem.l d2-d7/a0-a6, -(sp)
	lea LINE_FRAME(a6), a0
	lea TOKENS(a6), a1
	move.l a1, writer.Frame.Tokens(a0)
	move.l #64*20, writer.Frame.TokenBytes(a0)
	move.l d1, writer.Frame.Count(a0)
	lea LEXEMES(a6), a1
	move.l a1, writer.Frame.Lexemes(a0)
	move.l d3, writer.Frame.LexemeBytes(a0)
	move.l Frame.Output(a5), writer.Frame.Output(a0)
	move.l Frame.Capacity(a5), writer.Frame.Capacity(a0)
	move.l #bind, writer.Frame.Binder(a0)
	move.l a6, writer.Frame.Context(a0)
	move.l LINE_NUMBER(a6), d0
	move.w d0, writer.Frame.SourceLine(a0)
	move.l Frame.Source(a5), writer.Frame.Source(a0)
	move.l Frame.SourceBytes(a5), writer.Frame.SourceBytes(a0)
	movea.l Frame.Package(a5), a1
	move.w package.Header.CpuDirective(a1), writer.Frame.NameDirective(a0)
	move.w package.Header.ResDirective(a1), writer.Frame.WidthDirective(a0)
	move.w package.Header.EmitDirective(a1), writer.Frame.DataWidthDirective(a0)
	clr.w writer.Frame.Reserved(a0)
	lea PACKED_MAP(a6), a1
	move.l a1, writer.Frame.PackedMap(a0)
	.MEMORY_DETAIL_BEGIN #0
	jsr writer.writeLine
	.MEMORY_DETAIL_END #0
	bne.w failed
	bra.w done
failed
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; writeTokens

; Capture pre-decoding spelling for ordinary macro/segment body string lines.
; Known nested calls keep their existing call recipes until that consumer moves.
; Token kind is a VM result; no placeholder or quote grammar is inspected here.
stringLinePlan	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a6, a2
	adda.l #TEMPLATE_STATE, a2
	moveq #0, d0
	move.w templates.State.Open(a2), d0
	beq.w ready
	subq.w #1, d0
	mulu.w #templates.DEF_BYTES, d0
	movea.l templates.DEFS+memory.Block.Pointer(a2), a1
	adda.l d0, a1
	movea.l Frame.Output(a5), a0
	lea SCOPE_STATE(a6), a1
	jsr templates.role
	tst.l d0
	bne.w ready
	lea TOKENS(a6), a1
	move.l LINE_FRAME+writer.Frame.Count(a6), d2
findString
	tst.l d2
	beq.w ready
	cmpi.w #tokenizer.TK_KIND_STRING, (a1)
	beq.w capture
	adda.w #20, a1
	subq.l #1, d2
	bra.w findString
capture
	tst.l CAPTURE_RECORD(a6)
	beq.w physicalCapture
	moveq #0, d4  ; retained whole-line string recipe
	bsr.w bindCapturedPlan
	bne.w bad
	bra.w appendCaptured
physicalCapture
	.MEMORY_TEMPLATE_WORK #11, #1
	lea MACRO_EVENTS(a6), a1
	movea.l a1, a0
	moveq #macro_plans.ROW_BYTES/4-1, d0
clearRow
	clr.l (a0)+
	dbra d0, clearRow
	move.w #parser_abi.PRVM_RESULT_MACRO_LINE, macro_plans.Row.Kind(a1)
	move.l LINE_FRAME+writer.Frame.Count(a6), macro_plans.Row.PackedEnd(a1)
	move.l Frame.SourceBytes(a5), macro_plans.Row.SpellingEnd(a1)
	lea MACRO_FRAME(a6), a0
	lea MACRO_PLANS(a6), a2
	move.l a2, macro_plans.Frame.Arena(a0)
	move.l a1, macro_plans.Frame.Events(a0)
	move.l #1, macro_plans.Frame.Count(a0)
	move.l Frame.Source(a5), macro_plans.Frame.Source(a0)
	move.l Frame.SourceBytes(a5), macro_plans.Frame.SourceBytes(a0)
	lea PACKED_MAP(a6), a1
	move.l a1, macro_plans.Frame.PackedMap(a0)
	move.l LINE_FRAME+writer.Frame.Count(a6), macro_plans.Frame.TokenCount(a0)
	bsr.w captureFragments
	bne.w bad
	jsr macro_plans.create
	bne.w bad
appendCaptured
	lea LINE_FRAME(a6), a0
	jsr writer.appendPlan
	bne.w bad
	movea.l Frame.Output(a5), a0
	andi.b #$ff-writer.FLAG_PLAN, 1(a0)
	ori.b #templates.LINE_PLAN_FLAG, 1(a0)
	move.b #templates.TOKEN_LINE_PLAN, -6(a0, d1.l)
ready
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; stringLinePlan

; A0=session,A1=output,A2=definition,A4=invocation,D1=owned recipe handle.
; Bind fragments, then let TKVM alone materialize and lex their logical stream.
; Returns only a packed line. No original/expanded text enters writer interfaces.
fragmentLine	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l Frame.Scratch(a5), a6
	move.l a1, -(sp)
	suba.w #FRAGMENT_SCRATCH, sp
	movea.l sp, a0
	lea MACRO_PLANS(a6), a3
	move.l a3, fragment_binding.Frame.Arena(a0)
	move.l d1, fragment_binding.Frame.Plan(a0)
	move.l templates.Def.HeaderPlan(a2), fragment_binding.Frame.Header(a0)
	moveq #0, d0
	move.w templates.Def.ParamCount(a2), d0
	move.l d0, fragment_binding.Frame.FormalCount(a0)
	lea templates.TEXT(a4), a3
	move.l a3, fragment_binding.Frame.Text(a0)
	lea templates.TEXT_END0(a4), a3
	move.l a3, fragment_binding.Frame.TextEnds(a0)
	moveq #0, d0
	move.w templates.TEXT_BYTES(a4), d0
	move.l d0, fragment_binding.Frame.TextBytes(a0)
	lea templates.FULL_TEXT(a4), a3
	move.l a3, fragment_binding.Frame.Full(a0)
	moveq #0, d0
	move.w templates.FULL_BYTES(a4), d0
	move.l d0, fragment_binding.Frame.FullBytes(a0)
	lea FRAGMENT_VIEWS(sp), a3
	move.l a3, fragment_binding.Frame.Output(a0)
	move.l #64*fragment_binding.FRAGMENT_BYTES, fragment_binding.Frame.Capacity(a0)
	jsr fragment_binding.runFragments
	bne.w bad
	lea FRAGMENT_REQUEST(sp), a0
	move.l a3, fragment_tokenizer.Frame.Fragments(a0)
	move.l d2, fragment_tokenizer.Frame.Count(a0)
	move.l d1, fragment_tokenizer.Frame.InputBytes(a0)
	lea TOKENS(a6), a3
	move.l a3, fragment_tokenizer.Frame.Tokens(a0)
	move.l #TOKEN_CAPACITY, fragment_tokenizer.Frame.TokenCapacity(a0)
	lea LEXEMES(a6), a3
	move.l a3, fragment_tokenizer.Frame.Lexemes(a0)
	move.l #LEXEME_BYTES, fragment_tokenizer.Frame.LexemeCapacity(a0)
	move.l PROGRAM(a6), fragment_tokenizer.Frame.Program(a0)
	move.l PROGRAM_BYTES(a6), fragment_tokenizer.Frame.ProgramBytes(a0)
	jsr fragment_tokenizer.run
	bne.w bad
	lea SCOPE_STATE(a6), a0
	jsr scopes.startLine
	lea LINE_FRAME(a6), a0
	lea TOKENS(a6), a3
	move.l a3, writer.Frame.Tokens(a0)
	move.l #TOKEN_CAPACITY*20, writer.Frame.TokenBytes(a0)
	move.l d1, writer.Frame.Count(a0)
	lea LEXEMES(a6), a3
	move.l a3, writer.Frame.Lexemes(a0)
	move.l d3, writer.Frame.LexemeBytes(a0)
	move.l FRAGMENT_SCRATCH(sp), writer.Frame.Output(a0)
	move.l #256, writer.Frame.Capacity(a0)
	move.l #bind, writer.Frame.Binder(a0)
	move.l a6, writer.Frame.Context(a0)
	moveq #0, d0
	move.w templates.CallFrame.CallLine(a4), d0
	move.w d0, LINE_FRAME+writer.Frame.SourceLine(a6)
	lea LINE_FRAME(a6), a0
	clr.l writer.Frame.Source(a0)
	clr.l writer.Frame.SourceBytes(a0)
	movea.l Frame.Package(a5), a3
	move.w package.Header.CpuDirective(a3), writer.Frame.NameDirective(a0)
	move.w package.Header.ResDirective(a3), writer.Frame.WidthDirective(a0)
	move.w package.Header.EmitDirective(a3), writer.Frame.DataWidthDirective(a0)
	clr.w writer.Frame.Reserved(a0)
	lea PACKED_MAP(a6), a3
	move.l a3, writer.Frame.PackedMap(a0)
	jsr writer.writeLine
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	adda.w #FRAGMENT_SCRATCH+4, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; fragmentLine

; Bind a VM-owned call/header plan while initial lexical spans are still live.
; A5=session frame,A6=scratch. D0/CCR=status; other registers preserved.
initialPlan	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a6, a0
	adda.l #CONDITION_STATE, a0
	tst.w conditionals.State.Active(a0)
	beq.w ready
	movea.l Frame.Output(a5), a0
	lea SCOPE_STATE(a6), a1
	movea.l a6, a2
	adda.l #TEMPLATE_STATE, a2
	jsr templates.role
	tst.l d0
	beq.w ready
	move.l d0, d4
	tst.l CAPTURE_RECORD(a6)
	beq.w physicalPlan
	bsr.w bindCapturedPlan
	bne.w bad
	bra.w appendCaptured
physicalPlan
	bsr.w sourceDescriptors
	bne.w bad
	lea MACRO_FRAME(a6), a0
	move.l d1, macro_plans.Frame.Count(a0)
	lea MACRO_PLANS(a6), a1
	move.l a1, macro_plans.Frame.Arena(a0)
	lea MACRO_EVENTS(a6), a1
	move.l a1, macro_plans.Frame.Events(a0)
	move.l Frame.Source(a5), macro_plans.Frame.Source(a0)
	move.l Frame.SourceBytes(a5), macro_plans.Frame.SourceBytes(a0)
	lea PACKED_MAP(a6), a1
	move.l a1, macro_plans.Frame.PackedMap(a0)
	move.l LINE_FRAME+writer.Frame.Count(a6), macro_plans.Frame.TokenCount(a0)
	clr.l macro_plans.Frame.RecipeEvents(a0)
	clr.l macro_plans.Frame.RecipeCount(a0)
	movea.l a6, a1
	adda.l #TEMPLATE_STATE, a1
	tst.w templates.State.Open(a1)
	beq.w planReady
	cmpi.l #1, d4
	bne.w planReady
	bsr.w captureFragments
	bne.w bad
planReady
	jsr macro_plans.create
	bne.w bad
appendCaptured
	lea LINE_FRAME(a6), a0
	jsr writer.appendPlan
	bne.w bad
ready
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; initialPlan

; A5=session,A6=scratch,D4=role (zero selects string-line candidate).
; Select only now, after the replay writer has rebound names in current scope.
; D0/CCR=status,D1=normal session plan handle; other registers preserved.
bindCapturedPlan	.block
	movem.l d2-d7/a0-a4, -(sp)
	movea.l CAPTURE_RECORD(a6), a4
	moveq #0, d7
	tst.l d4
	beq.w lineCandidate
	cmpi.l #1, d4
	bne.w headerCandidate
	tst.l capture.Record.CallStatus(a4)
	bne.w bad
	move.l capture.Record.CallHandle(a4), d6
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	tst.w templates.State.Open(a0)
	beq.w candidateReady
	tst.l capture.Record.CallRecipeStatus(a4)
	bne.w bad
	moveq #1, d7
	bra.w candidateReady
headerCandidate
	tst.l capture.Record.HeaderStatus(a4)
	bne.w bad
	move.l capture.Record.HeaderHandle(a4), d6
	bra.w candidateReady
lineCandidate
	tst.l capture.Record.LineStatus(a4)
	bne.w bad
	move.l capture.Record.LineHandle(a4), d6
	moveq #1, d7
candidateReady
	suba.w #memory.Block.Used+4+macro_plans.BIND_FRAME_BYTES, sp
	movea.l a4, a1
	jsr capture.planView
	move.l a0, memory.Block.Pointer(sp)
	move.l d0, memory.Block.Capacity(sp)
	move.l d0, memory.Block.Used(sp)
	lea memory.Block.Used+4(sp), a0
	lea MACRO_PLANS(a6), a1
	move.l a1, macro_plans.BindFrame.Arena(a0)
	move.l sp, macro_plans.BindFrame.Source(a0)
	move.l d6, macro_plans.BindFrame.Handle(a0)
	lea PACKED_MAP(a6), a1
	move.l a1, macro_plans.BindFrame.PackedMap(a0)
	move.l LINE_FRAME+writer.Frame.Count(a6), macro_plans.BindFrame.TokenCount(a0)
	move.w d7, macro_plans.BindFrame.IncludeRecipes(a0)
	clr.w macro_plans.BindFrame.Reserved(a0)
	jsr macro_plans.bind
	adda.w #memory.Block.Used+4+macro_plans.BIND_FRAME_BYTES, sp
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a4
	tst.l d0
	rts
	.bend  ; bindCapturedPlan

; A5=session,A6=scratch,D4=call(1) or header role. Existing shared PRVM
; reads the physical line only during direct preparation or initial capture.
; D0/CCR=status,D1=event count; other registers preserved.
sourceDescriptors	.block
	movem.l d2-d7/a0-a4, -(sp)
	lea MACRO_REQUEST(a6), a0
	movea.l a0, a1
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE/4-1, d0
clearRequest
	clr.l (a1)+
	dbra d0, clearRequest
	move.l #parser_abi.PRVM_MAGIC_OPRP, parser_abi.PRVM_FRAME_MAGIC(a0)
	move.w #parser_abi.PRVM_ABI_VERSION_V1, parser_abi.PRVM_FRAME_ABI_VERSION(a0)
	move.w #parser_abi.PRVM_REQUEST_FRAME_SIZE, parser_abi.PRVM_FRAME_FRAME_SIZE(a0)
	move.w #parser_abi.PRVM_ENTRY_KIND_MACRO_DESCRIPTORS, parser_abi.PRVM_FRAME_ENTRY_KIND(a0)
	move.l LINE_NUMBER(a6), parser_abi.PRVM_FRAME_LINE_NUM(a0)
	move.l Frame.Source(a5), parser_abi.PRVM_FRAME_SOURCE_PTR(a0)
	move.l Frame.SourceBytes(a5), parser_abi.PRVM_FRAME_SOURCE_LEN(a0)
	lea TOKENS(a6), a1
	move.l a1, parser_abi.PRVM_FRAME_TOKEN_PTR(a0)
	move.l LINE_FRAME+writer.Frame.Count(a6), parser_abi.PRVM_FRAME_TOKEN_COUNT(a0)
	move.w #parser_abi.PRVM_TOKEN_RECORD_SIZE, parser_abi.PRVM_FRAME_TOKEN_RECORD_SIZE(a0)
	movea.l Frame.Package(a5), a1
	move.l package.Header.MacroCall(a1), d1
	move.l package.Header.MacroCallBytes(a1), d2
	cmpi.l #1, d4
	beq.w selectedProgram
	move.l package.Header.MacroHeader(a1), d1
	move.l package.Header.MacroHeaderBytes(a1), d2
selectedProgram
	adda.l d1, a1
	move.l a1, parser_abi.PRVM_FRAME_PROGRAM_PTR(a0)
	move.l d2, parser_abi.PRVM_FRAME_PROGRAM_LEN(a0)
	lea MACRO_EVENTS(a6), a1
	move.l a1, parser_abi.PRVM_FRAME_RESULT_PTR(a0)
	move.l #64*parser_abi.PRVM_RESULT_RECORD_SIZE, parser_abi.PRVM_FRAME_RESULT_CAPACITY(a0)
	move.l #parser_abi.PRVM_PARSER_CONTRACT_VERSION_V2, parser_abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a0)
	move.l #65536, parser_abi.PRVM_FRAME_STEP_BUDGET(a0)
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE, d0
	.MEMORY_TEMPLATE_WORK #10, #1
	jsr macro_runtime.run
	bne.w bad
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a4
	tst.l d0
	rts
	.bend  ; sourceDescriptors

; Cache package-selected fragments only for nested calls captured in a template.
; A0=initial plan frame,A5=session,A6=scratch. D0/CCR=status; preserves others.
captureFragments	.block
	movem.l d1-d7/a0-a4, -(sp)
	movea.l a0, a4
	movea.l macro_plans.Frame.Events(a4), a1
	move.l macro_plans.Row.SpellingStart(a1), d1
	move.l macro_plans.Row.SpellingEnd(a1), d2
	sub.l d1, d2
	movea.l macro_plans.Frame.Source(a4), a2
	adda.l d1, a2
	lea MACRO_REQUEST(a6), a0
	movea.l a0, a1
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE/4-1, d0
clear
	clr.l (a1)+
	dbra d0, clear
	move.l #parser_abi.PRVM_MAGIC_OPRP, parser_abi.PRVM_FRAME_MAGIC(a0)
	move.w #parser_abi.PRVM_ABI_VERSION_V1, parser_abi.PRVM_FRAME_ABI_VERSION(a0)
	move.w #parser_abi.PRVM_REQUEST_FRAME_SIZE, parser_abi.PRVM_FRAME_FRAME_SIZE(a0)
	move.w #parser_abi.PRVM_ENTRY_KIND_MACRO_FRAGMENTS, parser_abi.PRVM_FRAME_ENTRY_KIND(a0)
	move.l a2, parser_abi.PRVM_FRAME_SOURCE_PTR(a0)
	move.l d2, parser_abi.PRVM_FRAME_SOURCE_LEN(a0)
	movea.l Frame.Package(a5), a1
	move.l package.Header.MacroFragmentsBytes(a1), parser_abi.PRVM_FRAME_PROGRAM_LEN(a0)
	adda.l package.Header.MacroFragments(a1), a1
	move.l a1, parser_abi.PRVM_FRAME_PROGRAM_PTR(a0)
	movea.l a6, a1
	adda.l #MACRO_SPELL_SCRATCH, a1
	move.l a1, parser_abi.PRVM_FRAME_RESULT_PTR(a0)
	move.l #64*parser_abi.PRVM_RESULT_RECORD_SIZE, parser_abi.PRVM_FRAME_RESULT_CAPACITY(a0)
	move.l #parser_abi.PRVM_PARSER_CONTRACT_VERSION_V2, parser_abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a0)
	move.l #65536, parser_abi.PRVM_FRAME_STEP_BUDGET(a0)
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr macro_runtime.run
	bne.w done
	move.l d1, macro_plans.Frame.RecipeCount(a4)
	movea.l a6, a1
	adda.l #MACRO_SPELL_SCRATCH, a1
	move.l a1, macro_plans.Frame.RecipeEvents(a4)
done
	movem.l (sp)+, d1-d7/a0-a4
	tst.l d0
	rts
	.bend  ; captureFragments

; A0=session,A1=complete packed line,A2=expanded call list,D1=list bytes,
; D2=captured call-plan handle. Lex the list through TKVM before selecting the
; generated packed/spelling descriptors; substitutions may change token shape.
; D0/CCR=status,D1=record bytes including its new plan handle; others preserved.
generatedPlan	.block
	movem.l d2-d7/a0-a6, -(sp)
	movea.l a0, a5
	movea.l a1, a4
	movea.l a2, a3
	move.l d1, d6
	movea.l Frame.Scratch(a5), a6
	bsr.w relexGeneratedCall
	bne.w bad
	move.l d1, d7
	lea MACRO_REQUEST(a6), a0
	movea.l a0, a1
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE/4-1, d0
clearRequest
	clr.l (a1)+
	dbra d0, clearRequest
	move.l #parser_abi.PRVM_MAGIC_OPRP, parser_abi.PRVM_FRAME_MAGIC(a0)
	move.w #parser_abi.PRVM_ABI_VERSION_V1, parser_abi.PRVM_FRAME_ABI_VERSION(a0)
	move.w #parser_abi.PRVM_REQUEST_FRAME_SIZE, parser_abi.PRVM_FRAME_FRAME_SIZE(a0)
	move.w #parser_abi.PRVM_ENTRY_KIND_PACKED_MACRO, parser_abi.PRVM_FRAME_ENTRY_KIND(a0)
	move.l a4, parser_abi.PRVM_FRAME_SOURCE_PTR(a0)
	move.l d7, parser_abi.PRVM_FRAME_SOURCE_LEN(a0)
	movea.l Frame.Package(a5), a1
	move.l package.Header.MacroPackedBytes(a1), parser_abi.PRVM_FRAME_PROGRAM_LEN(a0)
	adda.l package.Header.MacroPacked(a1), a1
	move.l a1, parser_abi.PRVM_FRAME_PROGRAM_PTR(a0)
	lea MACRO_EVENTS(a6), a1
	move.l a1, parser_abi.PRVM_FRAME_RESULT_PTR(a0)
	move.l #64*parser_abi.PRVM_RESULT_RECORD_SIZE, parser_abi.PRVM_FRAME_RESULT_CAPACITY(a0)
	move.l #parser_abi.PRVM_PARSER_CONTRACT_VERSION_V2, parser_abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a0)
	move.l #65536, parser_abi.PRVM_FRAME_STEP_BUDGET(a0)
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr macro_runtime.run
	bne.w bad
	move.l d1, d5
	lea MACRO_SPELL_FRAME(a6), a0
	move.l PROGRAM(a6), spelling.Frame.TokenProgram(a0)
	move.l PROGRAM_BYTES(a6), spelling.Frame.TokenBytes(a0)
	movea.l Frame.Package(a5), a1
	move.l package.Header.MacroSpellingBytes(a1), spelling.Frame.MacroBytes(a0)
	adda.l package.Header.MacroSpelling(a1), a1
	move.l a1, spelling.Frame.MacroProgram(a0)
	move.l a3, spelling.Frame.Source(a0)
	move.l d6, spelling.Frame.SourceBytes(a0)
	lea MACRO_SPELL_SCRATCH(a6), a1
	move.l a1, spelling.Frame.Scratch(a0)
	move.l #spelling.SCRATCH_BYTES, spelling.Frame.ScratchBytes(a0)
	lea MACRO_SPELL_EVENTS(a6), a1
	move.l a1, spelling.Frame.Result(a0)
	move.l #64*parser_abi.PRVM_RESULT_RECORD_SIZE, spelling.Frame.ResultBytes(a0)
	jsr spelling.run
	bne.w bad
	lea MACRO_FRAME(a6), a0
	move.l d1, macro_plans.GeneratedFrame.SpellingCount(a0)
	move.l d5, macro_plans.GeneratedFrame.PackedCount(a0)
	lea MACRO_PLANS(a6), a1
	move.l a1, macro_plans.GeneratedFrame.Arena(a0)
	lea MACRO_EVENTS(a6), a1
	move.l a1, macro_plans.GeneratedFrame.PackedEvents(a0)
	lea MACRO_SPELL_EVENTS(a6), a1
	move.l a1, macro_plans.GeneratedFrame.SpellingEvents(a0)
	move.l a3, macro_plans.GeneratedFrame.Source(a0)
	move.l d6, macro_plans.GeneratedFrame.SourceBytes(a0)
	move.l d7, macro_plans.GeneratedFrame.PackedBytes(a0)
	clr.l macro_plans.GeneratedFrame.RecipeEvents(a0)
	clr.l macro_plans.GeneratedFrame.RecipeCount(a0)
	movea.l a6, a1
	adda.l #TEMPLATE_STATE, a1
	tst.w templates.State.Open(a1)
	beq.w generatedReady
	; A generated call may itself become part of a captured definition.
	suba.l #macro_plans.FRAME_BYTES, sp
	movea.l sp, a0
	lea MACRO_SPELL_EVENTS(a6), a1
	move.l a1, macro_plans.Frame.Events(a0)
	move.l a3, macro_plans.Frame.Source(a0)
	bsr.w captureFragments
	tst.l d0
	bne.w generatedCaptureFailed
	lea MACRO_FRAME(a6), a1
	move.l macro_plans.Frame.RecipeEvents(a0), macro_plans.GeneratedFrame.RecipeEvents(a1)
	move.l macro_plans.Frame.RecipeCount(a0), macro_plans.GeneratedFrame.RecipeCount(a1)
generatedCaptureFailed
	adda.l #macro_plans.FRAME_BYTES, sp
	tst.l d0
	bne.w bad
	lea MACRO_FRAME(a6), a0
generatedReady
	jsr macro_plans.createGenerated
	bne.w bad
	lea LINE_FRAME(a6), a0
	move.l a4, writer.Frame.Output(a0)
	move.l #256, writer.Frame.Capacity(a0)
	move.w d7, writer.Frame.Used(a0)
	jsr writer.appendPlan
	bra.w done
bad
	moveq #1, d0
	moveq #0, d1
done
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; generatedPlan

; Relex the original-spelling recipe's expanded argument list, then retain
; the already bound call head. The VM selects all argument token boundaries.
; A0=session,A1=packed call,A2=list,D1=list bytes,D2=plan handle.
; D0/CCR=status,D1=combined packed bytes; preserves other registers.
relexGeneratedCall	.block
	movem.l d2-d7/a0-a6, -(sp)
	suba.l #GENERATED_BYTES, sp
	movea.l a0, a5
	movea.l a1, a4
	movea.l a2, a3
	move.l d1, d6
	move.l d2, d4
	movea.l Frame.Scratch(a5), a6
	moveq #0, d7
	move.b (a4), d7
	addq.l #1, d7
	lea MACRO_PLANS(a6), a0
	move.l d4, d1
	jsr macro_plans.resolve
	bne.w generatedRelexBad
	lea macro_plans.HEADER_BYTES(a1), a1
	cmpi.w #parser_abi.PRVM_RESULT_MACRO_LINE, macro_plans.Row.Kind(a1)
	bne.w generatedRelexBad
	move.l macro_plans.Row.PackedEnd(a1), d5
	cmpi.l #4, d5
	blo.w generatedRelexBad
	cmp.l d7, d5
	bhi.w generatedRelexBad
	move.b #32, GENERATED_SPACE(sp)
	lea GENERATED_FRAGMENTS(sp), a0
	lea GENERATED_SPACE(sp), a1
	move.l a1, fragment_tokenizer.Fragment.Bytes(a0)
	move.l #1, fragment_tokenizer.Fragment.Length(a0)
	lea fragment_tokenizer.FRAGMENT_BYTES(a0), a1
	move.l a3, fragment_tokenizer.Fragment.Bytes(a1)
	move.l d6, fragment_tokenizer.Fragment.Length(a1)
	lea GENERATED_REQUEST(sp), a0
	lea GENERATED_FRAGMENTS(sp), a1
	move.l a1, fragment_tokenizer.Frame.Fragments(a0)
	move.l #2, fragment_tokenizer.Frame.Count(a0)
	move.l d6, d0
	addq.l #1, d0
	move.l d0, fragment_tokenizer.Frame.InputBytes(a0)
	lea TOKENS(a6), a1
	move.l a1, fragment_tokenizer.Frame.Tokens(a0)
	move.l #TOKEN_CAPACITY, fragment_tokenizer.Frame.TokenCapacity(a0)
	lea LEXEMES(a6), a1
	move.l a1, fragment_tokenizer.Frame.Lexemes(a0)
	move.l #LEXEME_BYTES, fragment_tokenizer.Frame.LexemeCapacity(a0)
	move.l PROGRAM(a6), fragment_tokenizer.Frame.Program(a0)
	move.l PROGRAM_BYTES(a6), fragment_tokenizer.Frame.ProgramBytes(a0)
	jsr fragment_tokenizer.run
	bne.w generatedRelexBad
	lea LINE_FRAME(a6), a0
	lea TOKENS(a6), a1
	move.l a1, writer.Frame.Tokens(a0)
	move.l #TOKEN_CAPACITY*20, writer.Frame.TokenBytes(a0)
	move.l d1, writer.Frame.Count(a0)
	lea LEXEMES(a6), a1
	move.l a1, writer.Frame.Lexemes(a0)
	move.l d3, writer.Frame.LexemeBytes(a0)
	lea GENERATED_RECORD(sp), a1
	move.l a1, writer.Frame.Output(a0)
	move.l #256, writer.Frame.Capacity(a0)
	move.l #bind, writer.Frame.Binder(a0)
	move.l a6, writer.Frame.Context(a0)
	move.w 2(a4), writer.Frame.SourceLine(a0)
	clr.l writer.Frame.Source(a0)
	clr.l writer.Frame.SourceBytes(a0)
	movea.l Frame.Package(a5), a1
	move.w package.Header.CpuDirective(a1), writer.Frame.NameDirective(a0)
	move.w package.Header.ResDirective(a1), writer.Frame.WidthDirective(a0)
	move.w package.Header.EmitDirective(a1), writer.Frame.DataWidthDirective(a0)
	clr.w writer.Frame.Reserved(a0)
	clr.l writer.Frame.PackedMap(a0)
	jsr writer.writeLine
	bne.w generatedRelexBad
	move.l d1, d2
	subq.l #4, d2
	move.l d5, d3
	add.l d2, d3
	cmpi.l #256, d3
	bhi.w generatedRelexBad
	lea GENERATED_RECORD+4(sp), a0
	lea 0(a4, d5.l), a1
generatedRelexCopy
	tst.l d2
	beq.w generatedRelexDone
	move.b (a0)+, (a1)+
	subq.l #1, d2
	bra.w generatedRelexCopy
generatedRelexDone
	move.l d3, d1
	subq.l #1, d3
	move.b d3, (a4)
	moveq #0, d0
	bra.w generatedRelexExit
generatedRelexBad
	moveq #1, d0
	moveq #0, d1
generatedRelexExit
	adda.l #GENERATED_BYTES, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
	.bend  ; relexGeneratedCall

; Emit the next already-tokenized line of a pending segment invocation.
; A0=Frame. Used=0 when the invocation is exhausted; the physical source
; line counter remains at the following line. D0/CCR=status.
	.pub
nextExpansion	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	clr.l Frame.Used(a5)
	movea.l Frame.Scratch(a5), a6
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	movea.l Frame.Output(a5), a1
	lea SCOPE_STATE(a6), a2
	jsr templates.next
	bne.w nextFailed
	tst.l d1
	beq.w nextDone
	bsr.w expandRecord
	bra.w nextDone
nextFailed
	moveq #1, d0
nextDone
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; nextExpansion

	.priv
; A5=Frame,A6=Scratch. The output holds one generated packed record.
; Reenter template lookup before ordinary preparation; nested calls push a
; child frame and resume the parent when that child drains.
expandRecord	.block
generatedLoop
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	move.l templates.State.Origin(a0), Frame.Origin(a5)
	lea SCOPE_STATE(a6), a0
	jsr scopes.startLine
	movea.l Frame.Output(a5), a0
	lea SCOPE_STATE(a6), a2
	moveq #0, d0
	movea.l a6, a1
	adda.l #CONDITION_STATE, a1
	move.w conditionals.State.Active(a1), d0
	movea.l a6, a1
	adda.l #TEMPLATE_STATE, a1
	jsr templates.line
	bne.w failed
	tst.w d1
	beq.w prepareGenerated
nextGenerated
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	movea.l Frame.Output(a5), a1
	lea SCOPE_STATE(a6), a2
	jsr templates.next
	bne.w failed
	tst.l d1
	bne.w generatedLoop
	clr.l Frame.Used(a5)
	moveq #0, d0
	rts
prepareGenerated
	bsr.w processRecord
	bne.w failed
	tst.l Frame.Used(a5)
	beq.w nextGenerated  ; streamed actions publish directly; keep draining the call
	moveq #0, d0  ; return status flags after testing the published byte count
	rts
failed
	moveq #1, d0
	rts
	.bend  ; expandRecord

; A5=Frame,A6=Scratch, Frame.Output contains one writer record. Apply the
; normal numeric selection, binding and expression path to original or expanded
; records. This routine does not tokenize source or advance the physical line.
processRecord	.block
	clr.l Frame.GraphBefore(a5)
	movea.l Frame.Graph(a5), a0
	move.l a0, d0
	beq.w graphBeforeDone
	moveq #0, d0
	move.w SCOPE_STATE+scopes.MODULE_STATE+modules.State.Active(a6), d0
	move.l d0, Frame.GraphBefore(a5)
graphBeforeDone
	movea.l Frame.Output(a5), a0
	lea SCOPE_STATE(a6), a1
	lea SCOPE_STATE(a6), a2
	adda.l #scopes.SCRATCH_BYTES, a2
	.MEMORY_DETAIL_BEGIN #2
	jsr conditionals.line
	.MEMORY_DETAIL_END #2
	bne.w failed
	tst.l d1
	bne.w activeLine
	movea.l Frame.Output(a5), a0
	move.b #3, (a0)
	clr.b 1(a0)
	bra.w conditionReady
activeLine
	movea.l Frame.Output(a5), a0
	; Private macro hygiene markers belong to scopes, not portable PRVM records.
	move.b 1(a0), d0
	andi.b #scopes.LEXICAL_BLOCK, d0
	bne.w bindLine
	movea.l Frame.Package(a5), a1
	lea SCOPE_STATE(a6), a2
	movea.l Frame.Metadata(a5), a3
	moveq #0, d1
	move.w Frame.RootFile(a5), d1
	jsr metadata.check
	bne.w failed
	tst.l d1
	bne.w conditionReady
bindLine
	movea.l Frame.Output(a5), a0
	lea SCOPE_STATE(a6), a1
	move.l Frame.Capacity(a5), d0
	.MEMORY_DETAIL_BEGIN #2
	jsr scopes.line
	.MEMORY_DETAIL_END #2
	bne.w failed
	movea.l Frame.Output(a5), a0
	moveq #0, d0
	move.b (a0), d0
	cmpi.w #9, d0
	blo.w constantCaptured
	cmpi.b #34, 8(a0)
	bne.w constantCaptured
	lea SCOPE_STATE(a6), a1
	.MEMORY_DETAIL_BEGIN #2
	jsr imports.captureConstant
	.MEMORY_DETAIL_END #2
	bne.w failed
constantCaptured
conditionReady
	.MEMORY_DETAIL_BEGIN #3
	bsr.w fileLine
	bne.w failed
	tst.l d1
	bne.w graphLineDone  ; callback published its numeric data and graph bytes
	.MEMORY_STAGE #4
	movea.l Frame.Output(a5), a0
	movea.l Frame.Package(a5), a1
	lea SCOPE_STATE(a6), a2
	jsr data_prepare.check
	bne.w failed
	movea.l Frame.Output(a5), a0
	lea PREPARED_LINE(a6), a1
	movea.l Frame.Package(a5), a2
	jsr prepare.line
	bne.w failed
	.MEMORY_STAGE #0
	cmp.l Frame.Capacity(a5), d1
	bhi.w failed
	lea PREPARED_LINE(a6), a0
	movea.l Frame.Output(a5), a1
	move.l d1, d0
copyPrepared
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w copyPrepared
	move.l d1, Frame.Used(a5)
	movea.l Frame.Graph(a5), a0
	move.l a0, d0
	beq.w graphLineDone
	move.l Frame.Used(a5), d0
	move.l Frame.GraphBefore(a5), d1
	movea.l Frame.Scratch(a5), a1
	moveq #0, d2
	move.w SCOPE_STATE+scopes.MODULE_STATE+modules.State.Active(a1), d2
	jsr graph.line
	bne.w failed
graphLineDone
	lea SCOPE_STATE(a6), a0
	jsr scopes.count
	move.l d0, Frame.NameCount(a5)
	moveq #0, d0
	bra.w done
failed
	moveq #1, d0
done
	.MEMORY_DETAIL_END #3
	tst.l d0
	rts
	.bend  ; processRecord
	.pub
; Publish one already-numeric data record from a preparation-only file callback.
; A0=session,A1=record,D0=bytes. No binding, text parsing or expression compilation
; occurs here. Updates Used and graph ownership; caller then appends Output.
; D0/CCR=status; preserves other registers. Input/output may coincide.
data	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l a0, a5
	cmpi.l #4, d0
	blo.w bad
	cmpi.l #writer.MAX_LINE, d0
	bhi.w bad
	cmp.l Frame.Capacity(a5), d0
	bhi.w bad
	move.l d0, Frame.Used(a5)
	movea.l Frame.Output(a5), a2
copy
	move.b (a1)+, (a2)+
	subq.l #1, d0
	bne.w copy
	movea.l Frame.Graph(a5), a0
	move.l a0, d0
	beq.w good
	move.l Frame.Used(a5), d0
	move.l Frame.GraphBefore(a5), d1
	movea.l Frame.Scratch(a5), a1
	moveq #0, d2
	move.w SCOPE_STATE+scopes.MODULE_STATE+modules.State.Active(a1), d2
	jsr graph.line
	bra.w done
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; data
	.priv
; PRVM selects a file action from active packed records; native code only
; delegates the resulting bounded path/prefix view to caller-owned I/O.
; A5=session,A6=scratch. D0/CCR=status,D1=handled; preserves other registers.
fileLine	.block
	movem.l d2-d7/a0-a4, -(sp)
	lea MACRO_REQUEST(a6), a0
	movea.l a0, a1
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE/4-1, d0
clear
	clr.l (a1)+
	dbra d0, clear
	move.l #parser_abi.PRVM_MAGIC_OPRP, parser_abi.PRVM_FRAME_MAGIC(a0)
	move.w #parser_abi.PRVM_ABI_VERSION_V1, parser_abi.PRVM_FRAME_ABI_VERSION(a0)
	move.w #parser_abi.PRVM_REQUEST_FRAME_SIZE, parser_abi.PRVM_FRAME_FRAME_SIZE(a0)
	move.w #parser_abi.PRVM_ENTRY_KIND_PACKED_FILE, parser_abi.PRVM_FRAME_ENTRY_KIND(a0)
	move.l Frame.Output(a5), parser_abi.PRVM_FRAME_SOURCE_PTR(a0)
	movea.l Frame.Output(a5), a1
	moveq #0, d0
	move.b (a1), d0
	addq.l #1, d0
	lea MACRO_REQUEST(a6), a0
	move.l d0, parser_abi.PRVM_FRAME_SOURCE_LEN(a0)
	movea.l Frame.Package(a5), a1
	move.l package.Header.FilePlanBytes(a1), parser_abi.PRVM_FRAME_PROGRAM_LEN(a0)
	adda.l package.Header.FilePlan(a1), a1
	move.l a1, parser_abi.PRVM_FRAME_PROGRAM_PTR(a0)
	lea MACRO_EVENTS(a6), a1
	move.l a1, parser_abi.PRVM_FRAME_RESULT_PTR(a0)
	move.l #parser_abi.PRVM_RESULT_RECORD_SIZE, parser_abi.PRVM_FRAME_RESULT_CAPACITY(a0)
	move.l #parser_abi.PRVM_PARSER_CONTRACT_VERSION_V2, parser_abi.PRVM_FRAME_PARSER_CONTRACT_VERSION(a0)
	move.l #1024, parser_abi.PRVM_FRAME_STEP_BUDGET(a0)
	moveq #parser_abi.PRVM_REQUEST_FRAME_SIZE, d0
	jsr macro_runtime.run
	bne.w bad
	tst.l d1
	beq.w good
	cmpi.l #1, d1
	bne.w bad
	move.l Frame.FileInclude(a5), d0
	beq.w bad  ; stand-alone frontends must explicitly supply file I/O
	movea.l d0, a2
	movea.l a5, a0
	lea MACRO_EVENTS(a6), a1
	jsr (a2)
	bne.w bad
	clr.l Frame.Used(a5)
	moveq #1, d1
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d7/a0-a4
	tst.l d0
	rts
	.bend  ; fileLine
	.pub
; Finish one source file without discarding identities shared by the session.
; A0=Frame,D0=nonzero to require explicit modules for file content.
; D0/CCR=status, others preserved. Successful EOF resets local lines.
endFile	.block
	movem.l a0-a2, -(sp)
	movea.l a0, a2
	movea.l Frame.Scratch(a0), a1
	movea.l a1, a0
	adda.l #TEMPLATE_STATE, a0
	jsr templates.endFile
	bne.w done
	lea SCOPE_STATE(a1), a0
	adda.l #scopes.SCRATCH_BYTES, a0
	jsr conditionals.endFile
	bne.w done
	lea SCOPE_STATE(a1), a0
	adda.l #scopes.SCRATCH_BYTES, a0
	jsr conditionals.begin
	bne.w done
	lea SCOPE_STATE(a1), a0
	jsr scopes.endFile
	bne.w done
	movea.l Frame.Graph(a2), a0
	move.l a0, d1
	beq.w resetFileLine
	jsr graph.endFile
	bne.w done
resetFileLine
	move.l #1, LINE_NUMBER(a1)
done
	movem.l (sp)+, a0-a2
	tst.l d0
	rts
	.bend  ; endFile
; A0=Frame,A1=caller-owned span output,D0=bytes. Dependency-first spans
; refer to original packed records and original file ordinals. D0=0,D1=span
; count; D0=2,D1=missing module index+1; D0=1,D1=0 for invalid graph.
; CCR reflects D0; other registers preserved.
orderGraph	.block
	movem.l a0-a3, -(sp)
	movea.l a0, a3
	movea.l Frame.Graph(a3), a0
	move.l a0, d1
	beq.w orderBad
	movea.l Frame.Scratch(a3), a2
	lea SCOPE_STATE(a2), a2
	movea.l a2, a3
	movea.l a1, a2
	movea.l a3, a1
	jsr graph.order
	bra.w orderDone
orderBad
	moveq #1, d0
	moveq #0, d1
orderDone
	movem.l (sp)+, a0-a3
	tst.l d0
	rts
	.bend  ; orderGraph
; Finalize scoped identities before lexical scratch is released. A0=Frame,
; A1=packed records,D0=record bytes. D0/CCR=status; other registers preserved.
complete	.block
	movem.l a0-a2, -(sp)
	movea.l Frame.Scratch(a0), a2
	movea.l a1, a0
	lea SCOPE_STATE(a2), a1
	jsr scopes.finish
	movem.l (sp)+, a0-a2
	tst.l d0
	rts
	.bend  ; complete
; A0=Frame. Return D0=parameter record bytes. Preparation scratch remains live.
parameterBytes	.block
	movea.l Frame.Scratch(a0), a0
	lea SCOPE_STATE(a0), a0
	lea scopes.IMPORT_STATE(a0), a0
	moveq #0, d0
	move.w imports.PARAM_COUNT(a0), d0
	mulu.w #imports.PARAM_BYTES, d0
	rts
	.bend  ; parameterBytes
; A0=Frame,A1=destination,D0=exact parameter record bytes. Copy only numeric
; identities and values before preparation scratch is released. D0/CCR=status.
copyParameters	.block
	movem.l d1/a0-a2, -(sp)
	movea.l Frame.Scratch(a0), a0
	lea SCOPE_STATE(a0), a0
	lea scopes.IMPORT_STATE(a0), a0
	moveq #0, d1
	move.w imports.PARAM_COUNT(a0), d1
	mulu.w #imports.PARAM_BYTES, d1
	cmp.l d0, d1
	bne.w parametersBad
	tst.l d1
	beq.w parametersDone
	lea imports.PARAMS(a0), a0
parametersCopy
	move.b (a0)+, (a1)+
	subq.l #1, d1
	bne.w parametersCopy
parametersDone
	moveq #0, d0
	bra.w parametersExit
parametersBad
	moveq #1, d0
parametersExit
	movem.l (sp)+, d1/a0-a2
	tst.l d0
	rts
	.bend  ; copyParameters
; A0=Frame,A1=ordered prepared records,D0=bytes. Index numeric block spans
; in the no-longer-needed name arena, before lexical scratch is released.
; D0/CCR=status,D1=span count; all other registers preserved.
indexBlocks	.block
	movem.l a0-a2, -(sp)
	movea.l Frame.Scratch(a0), a2
	move.l a2, d1
	beq.w indexBad
	move.l d0, -(sp)
	lea SCOPE_STATE+scopes.ARENA(a2), a0
	move.l #blocks.SCRATCH_BYTES, d0
	jsr memory.reserve
	bne.w indexReserveBad
	move.l (sp)+, d0
	movea.l SCOPE_STATE+scopes.ARENA_POINTER(a2), a2
	movea.l a1, a0
	movea.l a2, a1
	jsr blocks.index
	bra.w indexDone
indexReserveBad
	addq.l #4, sp
indexBad
	moveq #0, d1
	moveq #1, d0
indexDone
	movem.l (sp)+, a0-a2
	tst.l d0
	rts
	.bend  ; indexBlocks
; A0=Frame,A1=ordered records,D0=bytes,D1=indexed span count. Select blocks
; from numeric references while the preparation metadata and graph still exist.
; D0/CCR=status; other registers preserved.
selectBlocks	.block
	movem.l d2/a0-a3, -(sp)
	movea.l Frame.Scratch(a0), a2
	movea.l Frame.Graph(a0), a3
	move.l a3, d2
	beq.w selectBad
	movea.l a1, a0
	lea SCOPE_STATE(a2), a2
	movea.l scopes.ARENA_POINTER(a2), a1
	jsr blocks.select
	bra.w selectDone
selectBad
	moveq #1, d0
selectDone
	movem.l (sp)+, d2/a0-a3
	tst.l d0
	rts
	.bend  ; selectBlocks
; End a streaming session. A0=Frame. Clears scratch-resident pointers and resets
; tokenizer control state before the caller frees scratch. D0=0. Preserves other
; registers; CCR reflects D0.
finish	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l Frame.Scratch(a0), a6
	move.l a6, d0
	beq.w resetControl
	movea.l a6, a0
	adda.l #TEMPLATE_STATE, a0
	jsr templates.finish
	lea MACRO_PLANS(a6), a0
	jsr macro_plans.finish
	lea SCOPE_STATE(a6), a0
	jsr scopes.release
	clr.l PROGRAM(a6)
	clr.l PROGRAM_BYTES(a6)
	clr.l PACKAGE_BASE(a6)
	clr.l DICTIONARY_COUNT(a6)
	clr.l PACKAGE_END(a6)
	lea LINE_FRAME(a6), a1
	moveq #10-1, d0
clearFrame
	clr.l (a1)+
	dbra d0, clearFrame
resetControl
	moveq #0, d0
	jsr control.tkvmSetStepBudget68000
	moveq #0, d0
	moveq #0, d1
	suba.l a0, a0
	jsr control.tkvmSetProgramStateTable68000
	moveq #0, d0
	movem.l (sp)+, d1-d7/a0-a6
	rts
	.bend  ; finish
	.priv
; Validate only the package surfaces consumed by this frontend. Execution has
; independent bounds checks for candidate/program tables.
configure	.block
	movea.l Frame.Package(a5), a0
	bsr.w scratchSize
	bne.w bad
	movea.l Frame.Package(a5), a4
	cmpi.l #package.MAGIC, package.Header.Magic(a4)
	bne.w bad
	move.l package.Header.Bytes(a4), d7
	cmpi.l #HEADER_BYTES, d7
	blo.w bad
	move.l a4, d0
	add.l d7, d0
	bcs.w bad
	move.l d0, PACKAGE_END(a6)
	bsr.w validateMacroPrograms
	bne.w bad
	moveq #0, d0
	move.w package.Header.NameCount(a4), d0
	move.l d0, NEXT_ID(a6)
	move.l package.Header.Dictionary(a4), d0
	cmpi.l #HEADER_BYTES, d0
	blo.w bad
	cmp.l d7, d0
	bhi.w bad
	movea.l a4, a2
	adda.l d0, a2
	move.l a4, PACKAGE_BASE(a6)
	move.l package.Header.DictionaryCount(a4), d6
	move.l d6, DICTIONARY_COUNT(a6)
	movea.l a6, a3
	adda.l #SCRATCH_BYTES, a3
dictLoop
	tst.l d6
	beq.w indexDictionary
	move.l PACKAGE_END(a6), d0
	sub.l a2, d0
	cmpi.l #package.DICTIONARY_ENTRY_BYTES, d0
	blo.w bad
	cmpi.b #package.DICTIONARY_ROLE_ALLOWED, package.DictionaryEntry.Roles(a2)
	bhi.w bad
	moveq #0, d1
	move.w (a2), d1
	beq.w bad
	moveq #0, d2
	move.w 2(a2), d2
	cmp.l NEXT_ID(a6), d2
	bhs.w bad
	addi.l #6, d1
	addq.l #1, d1
	andi.l #$fffffffe, d1
	cmp.l d0, d1
	bhi.w bad
	move.l a2, d2
	sub.l a4, d2
	move.l d2, Node.Entry(a3)
	addq.l #8, a3
	adda.l d1, a2
	subq.l #1, d6
	bra.w dictLoop
; Insert backwards so duplicate folded spellings retain original first-match order.
indexDictionary
	move.l DICTIONARY_COUNT(a6), d6
indexLoop
	tst.l d6
	beq.w configureTokenizer
	subq.l #8, a3
	movea.l a4, a2
	adda.l Node.Entry(a3), a2
	moveq #0, d0
	move.w (a2), d0
	lea 6(a2), a0
	bsr.w hash
	lsl.l #2, d0
	lea PACKAGE_BUCKETS(a6), a1
	adda.l d0, a1
	move.l (a1), Node.Next(a3)
	move.l a3, d0
	sub.l a6, d0
	move.l d0, (a1)
	subq.l #1, d6
	bra.w indexLoop
configureTokenizer
	move.l package.Header.Tokenizer(a4), d0
	cmpi.l #HEADER_BYTES, d0
	blo.w bad
	cmp.l d7, d0
	bhi.w bad
	sub.l d0, d7
	move.l package.Header.TokenizerBytes(a4), d6
	cmp.l d7, d6
	bhi.w bad
	cmpi.l #16, d6
	blo.w bad
	lea 0(a4, d0.l), a2
	cmpi.w #1, (a2)
	bne.w bad
	moveq #0, d4
	move.w 4(a2), d4
	beq.w bad
	moveq #0, d5
	move.w 2(a2), d5
	cmp.l d4, d5
	bhs.w bad
	move.l d4, d0
	lsl.l #2, d0
	addi.l #12, d0
	cmp.l d6, d0
	bhs.w bad
	sub.l d0, d6
	move.l d6, PROGRAM_BYTES(a6)
	lea 0(a2, d0.l), a0
	move.l a0, PROGRAM(a6)
	lea 12(a2), a0
	move.l d4, d0
checkStates
	move.l (a0)+, d1
	cmp.l d6, d1
	bhs.w bad
	subq.l #1, d0
	bne.w checkStates
	move.l 8(a2), d0
	ble.w bad
	jsr control.tkvmSetStepBudget68000
	lea 12(a2), a0
	move.l d4, d0
	move.l d5, d1
	jsr control.tkvmSetProgramStateTable68000
	moveq #0, d0
	rts
bad
	moveq #1, d0
	rts
	.bend  ; configure

; The five package-selected macro programs live in the preparation region.
; A4=capsule, D7=complete capsule size; D0/CCR=status, other registers preserved.
validateMacroPrograms	.block
	movem.l d1-d3/a0, -(sp)
	cmpi.w #2, package.Header.MacroVersion(a4)
	bne.w bad
	move.w package.Header.ForDirective(a4), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	move.w package.Header.EndforDirective(a4), d0
	cmp.w package.Header.NameCount(a4), d0
	bhs.w bad
	cmp.w package.Header.ForDirective(a4), d0
	beq.w bad
	move.l package.Header.RuntimeBytes(a4), d3
	cmpi.l #HEADER_BYTES, d3
	blo.w bad
	cmp.l d7, d3
	bhi.w bad
	lea package.Header.MacroCall(a4), a0
	bsr.w region
	bne.w bad
	lea package.Header.MacroHeader(a4), a0
	bsr.w region
	bne.w bad
	lea package.Header.MacroPacked(a4), a0
	bsr.w region
	bne.w bad
	lea package.Header.MacroSpelling(a4), a0
	bsr.w region
	bne.w bad
	lea package.Header.MacroFragments(a4), a0
	bsr.w region
	bne.w bad
	lea package.Header.FilePlan(a4), a0
	bsr.w region
	bne.w bad
	lea package.Header.MetadataPlan(a4), a0
	bsr.w region
	bra.w done
region
	move.l (a0), d0
	cmp.l d3, d0
	blo.w badRegion
	cmp.l d7, d0
	bhi.w badRegion
	move.l 4(a0), d1
	ble.w badRegion
	move.l d7, d2
	sub.l d0, d2
	cmp.l d2, d1
	bhi.w badRegion
	moveq #0, d0
	rts
badRegion
	moveq #1, d0
	rts
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d3/a0
	tst.l d0
	rts
	.bend  ; validateMacroPrograms
; Writer callback ABI: lexical bytes A0/D0, D2=leading-name role;
; outputs D1=id,D2=qualifier,D0/status.
; A1=Scratch context. Preserves D3-D7/A2-A6.
bind	.block
	movem.l d3-d7/a2-a6, -(sp)
	.MEMORY_BIND_SAMPLE_BEGIN
	movea.l a1, a6
	movea.l a0, a2
	move.l d0, d6
	move.l d2, d5
	tst.l d2
	beq.w packageName
	cmpi.l #2, d2
	beq.w packageName
	cmpi.l #writer.BIND_ROLE_WIDTH, d2
	beq.w packageName
	cmpi.l #writer.BIND_ROLE_PACKAGE_NAME, d2
	beq.w packageName
	cmpi.l #writer.BIND_ROLE_MEMBER_NAME, d2
	beq.w packageName
	; Column-one names are declarations even when their spelling also occurs
	; in the package dictionary (for example, an `end` branch label).
	movea.l LINE_FRAME+writer.Frame.Output(a6), a4
	btst #0, 1(a4)
	beq.w findSymbol
	movea.l a6, a4
	adda.l #SCOPE_STATE+scopes.STRUCT_STATE, a4
	tst.w structs.State.Active(a4)
	bne.w findSymbol
	movea.l a6, a4
	adda.l #TEMPLATE_STATE, a4
	tst.w templates.State.Open(a4)
	beq.w packageName
	movea.l LINE_FRAME+writer.Frame.Output(a6), a4
	btst #0, 1(a4)
	beq.w findSymbol
packageName
	bsr.w hash
	move.l d0, d4
	lsl.l #2, d0
	lea PACKAGE_BUCKETS(a6), a1
	move.l 0(a1, d0.l), d7
findPackage
	tst.l d7
	beq.w findSymbol
	movea.l a6, a4
	adda.l d7, a4
	movea.l PACKAGE_BASE(a6), a3
	adda.l Node.Entry(a4), a3
	cmp.w (a3), d6
	bne.w advance
	movea.l a2, a0
	lea 6(a3), a1
	move.l d6, d0
	bsr.w equal
	bne.w advance
	moveq #0, d1
	move.w 2(a3), d1
	; Package metadata owns operand identities. Statement-only spellings
	; bind as source symbols when they occur in ordinary value operands.
	cmpi.l #writer.BIND_ROLE_MEMBER_NAME, d5
	beq.w memberBound
	tst.l d5
	bne.w packageBound
	btst #package.DICTIONARY_REGISTER_OR_NAMED_BIT, package.DictionaryEntry.Roles(a3)
	beq.w findSymbol
	bra.w packageBound
memberBound
	btst #package.DICTIONARY_MEMBER_BIT, package.DictionaryEntry.Roles(a3)
	beq.w findSymbol
packageBound
	cmpi.l #2, d5
	bne.w fixedPackageName
	; Dot heads belong to shared directives/templates, including names that
	; also occur in a CPU package. Declared templates take lexical precedence.
	movea.l a6, a4
	adda.l #TEMPLATE_STATE, a4
	tst.w templates.State.Count(a4)
	beq.w fixedPackageName
	movem.l d1-d2/a3, -(sp)
	bsr.w templateIdentity
	tst.l d0
	bne.w noTemplateIdentity
	adda.w #12, sp
	bra.w good
noTemplateIdentity
	movem.l (sp)+, d1-d2/a3
fixedPackageName
	moveq #0, d2
	move.b 4(a3), d2
	bra.w good
advance
	move.l Node.Next(a4), d7
	bra.w findPackage
findSymbol
	movea.l a2, a0
	move.l d6, d0
	lea SCOPE_STATE(a6), a1
	moveq #0, d3
	move.w layout.State.Current(a1), d3
	tst.l d5
	beq.w valueEnvironment
	cmpi.l #writer.BIND_ROLE_MEMBER_NAME, d5
	bne.w bindScopedName
valueEnvironment
	movea.l a1, a4
	adda.l #scopes.STRUCT_STATE, a4
	tst.w structs.State.Active(a4)
	beq.w bindScopedName
	; Field extents use the surrounding lexical environment. Field names
	; themselves bind within the layout; its total size is assigned at close.
	move.w structs.State.Parent(a4), layout.State.Current(a1)
bindScopedName
	jsr scopes.bind
	lea SCOPE_STATE(a6), a1
	move.w d3, layout.State.Current(a1)
	tst.l d0
	bra.w done
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	.MEMORY_BIND_SAMPLE_END
	movem.l (sp)+, d3-d7/a2-a6
	rts
	.bend  ; bind
; Probe only a declared template; a failed probe retains the package identity.
; A2/D6=lexeme,A6=session scratch. D0=status,D1/D2=source ID/qualifier.
templateIdentity	.block
	movem.l d3-d7/a0-a6, -(sp)
	suba.w #12, sp
	movea.l a2, a0
	move.l d6, d0
	lea SCOPE_STATE(a6), a1
	move.l layout.State.FirstBound(a1), d7
	jsr scopes.bind
	move.l d7, layout.State.FirstBound(a1)
	tst.l d0
	bne.w bad
	move.b #8, (sp)
	move.b #1, 1(sp)
	clr.w 2(sp)
	move.b #7, 4(sp)
	clr.b 5(sp)
	move.w d1, 6(sp)
	move.b d2, 8(sp)
	movea.l sp, a0
	lea SCOPE_STATE(a6), a1
	movea.l a6, a2
	adda.l #TEMPLATE_STATE, a2
	jsr templates.knownRole
	cmpi.l #1, d0
	bne.w bad
	moveq #0, d1
	move.w 6(sp), d1
	moveq #0, d2
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	adda.w #12, sp
	movem.l (sp)+, d3-d7/a0-a6
	tst.l d0
	rts
	.bend  ; templateIdentity

; Case-insensitive ASCII hash shared by dictionary construction and binding.
; A0/D0=bytes/count; D0=8-bit bucket. Clobbers D1-D3/A0; other registers kept.
hash	.block
	moveq #0, d1
	tst.l d0
	beq.w done
loop
	moveq #0, d2
	move.b (a0)+, d2
	cmpi.b #'A', d2
	blo.w ready
	cmpi.b #'Z', d2
	bhi.w ready
	addi.b #32, d2
ready
	move.l d1, d3
	lsl.l #5, d1
	add.l d3, d1
	add.l d2, d1
	subq.l #1, d0
	bne.w loop
done
	move.l d1, d0
	andi.l #255, d0
	rts
	.bend  ; hash
; Compare D0 nonempty ASCII lexical bytes, A0/A1; D0=0 equal, 1 unequal.
; Clobbers D1/D2/A0/A1; preserves remaining registers; CCR reflects D0.
equal	.block
loop
	moveq #0, d1
	move.b (a0)+, d1
	cmpi.b #'A', d1
	blo.w leftReady
	cmpi.b #'Z', d1
	bhi.w leftReady
	addi.b #32, d1
leftReady
	moveq #0, d2
	move.b (a1)+, d2
	cmpi.b #'A', d2
	blo.w rightReady
	cmpi.b #'Z', d2
	bhi.w rightReady
	addi.b #32, d2
rightReady
	cmp.b d1, d2
	bne.w different
	subq.l #1, d0
	bne.w loop
	moveq #0, d0
	rts
different
	moveq #1, d0
	rts
	.bend  ; equal
	.endsection
	.endmodule
