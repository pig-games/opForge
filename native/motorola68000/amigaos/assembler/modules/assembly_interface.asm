; Immutable assembly interface: typed values and closed template facets.
; @opforge-owner: opforge.assembler.modules.assembly_interface
	.module opforge.assembler.modules.assembly_interface
	.cpu 68020
	.use opforge.assembler.modules.semantic_exports as values
	.use opforge.assembler.modules.templates as templates
	.use opforge.platform.memory as memory
	.use opforge.assembler.modules.template_names as names
	.pub
Header	.struct
Magic	.long ?
Bytes	.long ?
Values	.long ?
ValueBytes	.long ?
Templates	.long ?
TemplateBytes	.long ?
DependencyCount	.long ?
Dependencies	.long ?
ImportCount	.long ?
Imports	.long ?
Names	.long ?
NameBytes	.long ?
	.endstruct
HEADER_BYTES = Header.NameBytes+4
MAGIC = $4f464d41; OFMA integrity tag
Frame	.struct
Values	.long ?
ValueBytes	.long ?
Templates	.long ?
TemplateBytes	.long ?
Output	.long ?
Capacity	.long ?
Used	.long ?
Dependencies	.long ?
DependencyCount	.long ?
Imports	.long ?
ImportCount	.long ?
	.endstruct
FRAME_BYTES = Frame.ImportCount+4
Dependency	.struct
Name	.long ?  ; input pointer; serialized image-relative offset
NameBytes	.long ?
	.endstruct
DEPENDENCY_BYTES = Dependency.NameBytes+4
Import	.struct
Dependency	.long ?  ; zero-based ordinal in this object
Entity	.long ?  ; provider-local value identity
	.endstruct
IMPORT_BYTES = Import.Entity+4
OK = 0
INVALID = 1
NOT_FOUND = 2
	.section code, kind=code
	.pub
; A0=Frame; Output=0 measures. Values are required. Templates may be absent
; with zero pointer and byte count.
; D0/CCR=status; others preserved. Used changes only on success. Inputs and
; output must be disjoint; the caller owns and retains every borrowed region.
build	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea -HEADER_BYTES(sp), sp
	movea.l a0, a6
	movea.l sp, a4
	movea.l Frame.Values(a6), a0
	move.l Frame.ValueBytes(a6), d0
	jsr values.validate
	bne.w bad
	tst.l Frame.TemplateBytes(a6)
	beq.w noTemplates
	movea.l Frame.Templates(a6), a0
	move.l Frame.TemplateBytes(a6), d0
	jsr templates.validate
	bne.w bad
	bra.w directories
noTemplates
	tst.l Frame.Templates(a6)
	bne.w bad
directories
	move.l #MAGIC, Header.Magic(a4)
	move.l #HEADER_BYTES, Header.Dependencies(a4)
	move.l Frame.DependencyCount(a6), d7
	cmpi.l #65535, d7
	bhi.w bad
	move.l d7, Header.DependencyCount(a4)
	move.l d7, d0
	mulu.l #DEPENDENCY_BYTES, d0
	addi.l #HEADER_BYTES, d0
	move.l d0, Header.Imports(a4)
	move.l Frame.ImportCount(a6), d0
	cmpi.l #65535, d0
	bhi.w bad
	move.l d0, Header.ImportCount(a4)
	mulu.l #IMPORT_BYTES, d0
	add.l Header.Imports(a4), d0
	move.l d0, Header.Names(a4)
	movea.l Frame.Dependencies(a6), a3
	move.l a3, d0
	tst.l d7
	beq.w dependencyReady
	tst.l d0
	beq.w bad
	move.l d7, d1
	mulu.l #DEPENDENCY_BYTES, d1
	add.l d1, d0
	bcs.w bad
dependencyReady
	moveq #0, d6
dependency
	tst.l d7
	beq.w imports
	movea.l Dependency.Name(a3), a0
	move.l Dependency.NameBytes(a3), d0
	bsr.w objectName
	bne.w bad
	add.l Dependency.NameBytes(a3), d6
	bcs.w bad
	adda.w #DEPENDENCY_BYTES, a3
	subq.l #1, d7
	bra.w dependency
imports
	move.l d6, Header.NameBytes(a4)
	move.l Frame.ImportCount(a6), d7
	movea.l Frame.Imports(a6), a3
	move.l a3, d0
	tst.l d7
	beq.w importReady
	tst.l d0
	beq.w bad
	move.l d7, d1
	mulu.l #IMPORT_BYTES, d1
	add.l d1, d0
	bcs.w bad
importReady
	tst.l d7
	beq.w regions
	move.l Import.Dependency(a3), d0
	cmp.l Frame.DependencyCount(a6), d0
	bhs.w bad
	cmpi.l #65535, Import.Entity(a3)
	bhi.w bad
	adda.w #IMPORT_BYTES, a3
	subq.l #1, d7
	bra.w importReady
regions
	movea.l Frame.Values(a6), a0
	movea.l a0, a3
	adda.l values.Header.Exports(a0), a3
	move.l values.Header.ExportCount(a0), d7
forward
	tst.l d7
	beq.w templateBindings
	cmpi.l #values.FORWARDED_ENTITY, values.Export.Kind(a3)
	bne.w forwardNext
	move.l values.Export.Low(a3), d0
	cmp.l Frame.DependencyCount(a6), d0
	bhs.w bad
forwardNext
	adda.w #values.EXPORT_BYTES, a3
	subq.l #1, d7
	bra.w forward
templateBindings
	tst.l Frame.TemplateBytes(a6)
	beq.w regionsReady
	movea.l Frame.Templates(a6), a0
	movea.l a0, a3
	adda.l templates.Header.Bindings(a0), a3
	move.l templates.Header.BindingCount(a0), d7
binding
	tst.l d7
	beq.w regionsReady
	move.l names.Row.ValueDependency(a3), d0
	cmp.l Frame.DependencyCount(a6), d0
	bhi.w bad
	move.l names.Row.TemplateDependency(a3), d0
	cmp.l Frame.DependencyCount(a6), d0
	bhi.w bad
	adda.w #names.ROW_BYTES, a3
	subq.l #1, d7
	bra.w binding
regionsReady
	add.l Header.Names(a4), d6
	bcs.w bad
	addq.l #7, d6
	bcs.w bad
	andi.l #$fffffff8, d6
	move.l d6, Header.Values(a4)
	move.l Frame.ValueBytes(a6), Header.ValueBytes(a4)
	add.l Frame.ValueBytes(a6), d6
	bcs.w bad
	addq.l #7, d6
	bcs.w bad
	andi.l #$fffffff8, d6
	clr.l Header.Templates(a4)
	move.l Frame.TemplateBytes(a6), Header.TemplateBytes(a4)
	beq.w regionReady
	move.l d6, Header.Templates(a4)
regionReady
	add.l Frame.TemplateBytes(a6), d6
	bcs.w bad
	cmpi.l #memory.LIMIT, d6
	bhi.w bad
	move.l d6, Header.Bytes(a4)
	movea.l Frame.Output(a6), a5
	move.l a5, d0
	beq.w measured
	btst #0, d0
	bne.w bad
	add.l d6, d0
	bcs.w bad
	cmp.l Frame.Capacity(a6), d6
	bhi.w bad
	; Zero padding and copy the checked directory into caller-owned storage.
	movea.l a5, a1
	move.l d6, d0
zero
	clr.b (a1)+
	subq.l #1, d0
	bne.w zero
	movea.l a4, a0
	movea.l a5, a1
	moveq #HEADER_BYTES, d0
	bsr.w copy
	move.l Header.Names(a4), d5
	move.l Frame.DependencyCount(a6), d7
	movea.l Frame.Dependencies(a6), a3
writeDependency
	tst.l d7
	beq.w writeImports
	move.l d5, (a1)+
	move.l Dependency.NameBytes(a3), (a1)+
	movem.l a1, -(sp)
	movea.l a5, a1
	adda.l d5, a1
	movea.l Dependency.Name(a3), a0
	move.l Dependency.NameBytes(a3), d0
	add.l d0, d5
	bsr.w copy
	movem.l (sp)+, a1
	adda.w #DEPENDENCY_BYTES, a3
	subq.l #1, d7
	bra.w writeDependency
writeImports
	movea.l Frame.Imports(a6), a0
	move.l Frame.ImportCount(a6), d0
	mulu.l #IMPORT_BYTES, d0
	bsr.w copy
	movea.l a5, a1
	adda.l Header.Values(a4), a1
	movea.l Frame.Values(a6), a0
	move.l Frame.ValueBytes(a6), d0
	bsr.w copy
	tst.l Frame.TemplateBytes(a6)
	beq.w measured
	movea.l a5, a1
	adda.l Header.Templates(a4), a1
	movea.l Frame.Templates(a6), a0
	move.l Frame.TemplateBytes(a6), d0
	bsr.w copy
measured
	move.l d6, Frame.Used(a6)
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	lea HEADER_BYTES(sp), sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; build

; A0=image,D0=exact readable bytes. D0/CCR=status; others preserved.
; Validate canonical offsets and present regions; no mutable owner retained.
validate	.block
	movem.l d1-d7/a0-a3, -(sp)
	movea.l a0, a2
	move.l a0, d1
	beq.w bad
	btst #0, d1
	bne.w bad
	add.l d0, d1
	bcs.w bad
	cmpi.l #HEADER_BYTES, d0
	blo.w bad
	cmpi.l #memory.LIMIT, d0
	bhi.w bad
	cmpi.l #MAGIC, Header.Magic(a2)
	bne.w bad
	cmp.l Header.Bytes(a2), d0
	bne.w bad
	cmpi.l #HEADER_BYTES, Header.Dependencies(a2)
	bne.w bad
	move.l Header.DependencyCount(a2), d7
	cmpi.l #65535, d7
	bhi.w bad
	move.l d7, d1
	mulu.l #DEPENDENCY_BYTES, d1
	addi.l #HEADER_BYTES, d1
	cmp.l Header.Imports(a2), d1
	bne.w bad
	move.l Header.ImportCount(a2), d2
	cmpi.l #65535, d2
	bhi.w bad
	mulu.l #IMPORT_BYTES, d2
	add.l d2, d1
	cmp.l Header.Names(a2), d1
	bne.w bad
	add.l Header.NameBytes(a2), d1
	bcs.w bad
	addq.l #7, d1
	bcs.w bad
	andi.l #$fffffff8, d1
	cmp.l Header.Values(a2), d1
	bne.w bad
	add.l Header.ValueBytes(a2), d1
	bcs.w bad
	addq.l #7, d1
	bcs.w bad
	andi.l #$fffffff8, d1
	tst.l Header.TemplateBytes(a2)
	beq.w noTemplates
	cmp.l Header.Templates(a2), d1
	bne.w bad
	bra.w regions
noTemplates
	tst.l Header.Templates(a2)
	bne.w bad
regions
	add.l Header.TemplateBytes(a2), d1
	bcs.w bad
	cmp.l d0, d1
	bne.w bad
	lea HEADER_BYTES(a2), a3
	move.l Header.Names(a2), d6
dependency
	tst.l d7
	beq.w imports
	cmp.l Dependency.Name(a3), d6
	bne.w bad
	move.l Dependency.NameBytes(a3), d0
	add.l d0, d6
	bcs.w bad
	move.l Header.Names(a2), d1
	add.l Header.NameBytes(a2), d1
	cmp.l d1, d6
	bhi.w bad
	movea.l a2, a0
	adda.l Dependency.Name(a3), a0
	bsr.w objectName
	bne.w bad
	adda.w #DEPENDENCY_BYTES, a3
	subq.l #1, d7
	bra.w dependency
imports
	move.l Header.Names(a2), d0
	add.l Header.NameBytes(a2), d0
	cmp.l d0, d6
	bne.w bad
	move.l Header.ImportCount(a2), d7
import
	tst.l d7
	beq.w valuesRegion
	move.l Import.Dependency(a3), d0
	cmp.l Header.DependencyCount(a2), d0
	bhs.w bad
	cmpi.l #65535, Import.Entity(a3)
	bhi.w bad
	adda.w #IMPORT_BYTES, a3
	subq.l #1, d7
	bra.w import
valuesRegion
	; Both alignment gaps are canonical zero padding.
	movea.l a2, a0
	adda.l Header.Names(a2), a0
	adda.l Header.NameBytes(a2), a0
	movea.l a2, a1
	adda.l Header.Values(a2), a1
	bsr.w padding
	bne.w bad
	movea.l a1, a0
	move.l Header.ValueBytes(a2), d0
	jsr values.validate
	bne.w bad
	movea.l a0, a3
	adda.l values.Header.Exports(a0), a3
	move.l values.Header.ExportCount(a0), d7
forward
	tst.l d7
	beq.w forwardReady
	cmpi.l #values.FORWARDED_ENTITY, values.Export.Kind(a3)
	bne.w forwardNext
	move.l values.Export.Low(a3), d0
	cmp.l Header.DependencyCount(a2), d0
	bhs.w bad
forwardNext
	adda.w #values.EXPORT_BYTES, a3
	subq.l #1, d7
	bra.w forward
forwardReady
	adda.l Header.ValueBytes(a2), a0
	move.l Header.Values(a2), d1
	add.l Header.ValueBytes(a2), d1
	addq.l #7, d1
	andi.l #$fffffff8, d1
	movea.l a2, a1
	adda.l d1, a1
	bsr.w padding
	bne.w bad
	tst.l Header.TemplateBytes(a2)
	beq.w good
	movea.l a1, a0
	move.l Header.TemplateBytes(a2), d0
	jsr templates.validate
	bne.w bad
	movea.l a0, a3
	adda.l templates.Header.Bindings(a0), a3
	move.l templates.Header.BindingCount(a0), d7
binding
	tst.l d7
	beq.w good
	move.l names.Row.ValueDependency(a3), d0
	cmp.l Header.DependencyCount(a2), d0
	bhi.w bad
	move.l names.Row.TemplateDependency(a3), d0
	cmp.l Header.DependencyCount(a2), d0
	bhi.w bad
	adda.w #names.ROW_BYTES, a3
	subq.l #1, d7
	bra.w binding
good
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0-a3
	tst.l d0
	rts
padding
	cmpa.l a1, a0
	bhs.w padded
	tst.b (a0)+
	bne.w unpadded
	bra.w padding
padded
	moveq #OK, d0
	rts
unpadded
	moveq #INVALID, d0
	rts
	.bend  ; validate

; A0=image,D0=exact bytes. D0/CCR=status,A1=borrowed region,D1=bytes.
; Other registers preserved; views expire with the containing image owner.
valuesView	.block
	bsr.w validate
	bne.w done
	movea.l a0, a1
	adda.l Header.Values(a0), a1
	move.l Header.ValueBytes(a0), d1
done
	tst.l d0
	rts
	.bend  ; valuesView
; Absent templates return NOT_FOUND with A1/D1 cleared.
templatesView	.block
	bsr.w validate
	bne.w done
	tst.l Header.TemplateBytes(a0)
	beq.w absent
	movea.l a0, a1
	adda.l Header.Templates(a0), a1
	move.l Header.TemplateBytes(a0), d1
	bra.w done
absent
	suba.l a1, a1
	moveq #0, d1
	moveq #NOT_FOUND, d0
done
	tst.l d0
	rts
	.bend  ; templatesView
	.priv
; Exact immutable current-build object filename, not a persistent-cache key.
; A0=bytes,D0=length. D0/CCR=status; other registers preserved.
objectName	.block
	movem.l d1/a0, -(sp)
	tst.l d0
	beq.w bad
	cmpi.l #30, d0
	bhi.w bad
	move.l a0, d1
	beq.w bad
	add.l d0, d1
	bcs.w bad
next
	move.b (a0)+, d1
	cmpi.b #'.', d1
	beq.w accepted
	cmpi.b #'-', d1
	beq.w accepted
	cmpi.b #'_', d1
	beq.w accepted
	cmpi.b #'0', d1
	blo.w bad
	cmpi.b #'9', d1
	bls.w accepted
	cmpi.b #'A', d1
	blo.w bad
	cmpi.b #'Z', d1
	bls.w accepted
	cmpi.b #'a', d1
	blo.w bad
	cmpi.b #'z', d1
	bhi.w bad
accepted
	subq.l #1, d0
	bne.w next
	moveq #OK, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	movem.l (sp)+, d1/a0
	tst.l d0
	rts
	.bend  ; objectName

; A0=source,A1=destination,D0=bytes. Advances cursors; D0 becomes zero.
copy	.block
	tst.l d0
	beq.w done
next
	move.b (a0)+, (a1)+
	subq.l #1, d0
	bne.w next
done
	rts
	.bend  ; copy
	.endsection
	.endmodule
