; Bounded leases over immutable stored module assembly interfaces.
; @opforge-owner: experimental.amigaos.binary_module_iface_store
	.module experimental.amigaos.binary_module_iface_store
	.cpu 68020
	.use experimental.amigaos.binary_module_registry as registry
	.use experimental.amigaos.binary_module_image as image
	.use experimental.amigaos.binary_module_iface as interface
	.use experimental.amigaos.binary_module_interface as values
	.use experimental.amigaos.binary_module_templates as templates
	.use experimental.amigaos.binary_module_tmpl_names as names
	.use experimental.amigaos.binary_memory as memory
	.pub
WINDOW_BYTES = 65536
State	.struct
Registry	.long ?
Module	.long ?
ObjectBytes	.long ?
Offset	.long ?  ; object-relative OFMA start
Bytes	.long ?
ValueOffset	.long ?  ; OFMA-relative OFMS start
ValueBytes	.long ?
TemplateOffset	.long ?
TemplateBytes	.long ?
Directory	.res interface.HEADER_BYTES
Values	.res values.HEADER_BYTES
Templates	.res templates.HEADER_BYTES
Window	.res memory.Block.Used+4
Start	.long ?  ; OFMA-relative cached range
	.endstruct
STATE_BYTES = State.Start+4
OK = 0
INVALID = 1
NOT_FOUND = 2
	.section code, kind=code
	.pub
; A0=zero/live State,A1=Registry,D1=module. D0/CCR=status; others preserved.
; Checks directory headers through a bounded window; keeps no object image or
; frontend owner.
; A failed selection invalidates the prior selection and all outstanding leases.
; Re-selecting the same immutable registry object retains its cached page. The
; caller must release this reader before registry removal or object mutation.
select	.block
	movem.l d1-d7/a0-a6, -(sp)
	lea -image.HEADER_BYTES(sp), sp
	movea.l a0, a6
	tst.l State.Bytes(a6)
	beq.w selecting
	cmpa.l State.Registry(a6), a1
	bne.w selecting
	cmp.l State.Module(a6), d1
	beq.w good
selecting
	clr.l State.Bytes(a6)
	clr.l State.Window+memory.Block.Used(a6)
	move.l a1, State.Registry(a6)
	move.l d1, State.Module(a6)
	movea.l a1, a0
	movea.l sp, a1
	moveq #0, d2
	moveq #image.HEADER_BYTES, d3
	jsr registry.readRange
	bne.w bad
	movea.l sp, a5
	cmpi.l #image.MAGIC, image.Header.Magic(a5)
	bne.w bad
	move.l image.Header.Bytes(a5), d7
	cmpi.l #image.HEADER_BYTES, d7
	blo.w bad
	cmpi.l #memory.LIMIT, d7
	bhi.w bad
	move.l d7, State.ObjectBytes(a6)
	move.l image.Header.Semantics(a5), d2
	cmpi.l #image.HEADER_BYTES, d2
	blo.w bad
	move.l d2, State.Offset(a6)
	move.l image.Header.SemanticsBytes(a5), d6
	cmpi.l #interface.HEADER_BYTES, d6
	blo.w bad
	move.l d2, d0
	add.l d6, d0
	bcs.w bad
	cmp.l d7, d0
	bne.w bad
	; The checked OFMI extent permits a bounded fill before OFMA validation.
	; Its lease also serves the later OFMS/OFTM headers on the same page.
	move.l d6, State.Bytes(a6)
	movea.l a6, a0
	moveq #0, d2
	moveq #interface.HEADER_BYTES, d3
	bsr.w read
	bne.w bad
	movea.l a1, a5
	cmpi.l #interface.MAGIC, interface.Header.Magic(a5)
	bne.w bad
	cmp.l interface.Header.Bytes(a5), d6
	bne.w bad
	cmpi.l #interface.HEADER_BYTES, interface.Header.Dependencies(a5)
	bne.w bad
	move.l interface.Header.DependencyCount(a5), d0
	cmpi.l #65535, d0
	bhi.w bad
	mulu.l #interface.DEPENDENCY_BYTES, d0
	addi.l #interface.HEADER_BYTES, d0
	cmp.l interface.Header.Imports(a5), d0
	bne.w bad
	move.l interface.Header.ImportCount(a5), d1
	cmpi.l #65535, d1
	bhi.w bad
	mulu.l #interface.IMPORT_BYTES, d1
	add.l d1, d0
	cmp.l interface.Header.Names(a5), d0
	bne.w bad
	add.l interface.Header.NameBytes(a5), d0
	bcs.w bad
	addq.l #7, d0
	bcs.w bad
	andi.l #$fffffff8, d0
	cmp.l interface.Header.Values(a5), d0
	bne.w bad
	move.l d0, State.ValueOffset(a6)
	move.l interface.Header.ValueBytes(a5), d5
	cmpi.l #values.HEADER_BYTES, d5
	blo.w bad
	move.l d5, State.ValueBytes(a6)
	add.l d0, d5
	bcs.w bad
	addq.l #7, d5
	bcs.w bad
	andi.l #$fffffff8, d5
	move.l interface.Header.TemplateBytes(a5), d4
	move.l d4, State.TemplateBytes(a6)
	move.l interface.Header.Templates(a5), State.TemplateOffset(a6)
	tst.l d4
	beq.w noTemplates
	cmpi.l #templates.HEADER_BYTES, d4
	blo.w bad
	cmp.l interface.Header.Templates(a5), d5
	bne.w bad
	bra.w extent
noTemplates
	tst.l interface.Header.Templates(a5)
	bne.w bad
extent
	add.l d4, d5
	bcs.w bad
	cmp.l d6, d5
	bne.w bad
	movea.l a5, a1
	lea State.Directory(a6), a2
	moveq #interface.HEADER_BYTES/4-1, d0
copyDirectory
	move.l (a1)+, (a2)+
	dbra d0, copyDirectory
	movea.l a6, a0
	move.l State.ValueOffset(a6), d2
	moveq #values.HEADER_BYTES, d3
	bsr.w read
	bne.w bad
	cmpi.l #values.MAGIC, values.Header.Magic(a1)
	bne.w bad
	move.l State.ValueBytes(a6), d0
	cmp.l values.Header.Bytes(a1), d0
	bne.w bad
	; Canonical OFMS directory sizes are checked without loading value records.
	cmpi.l #values.HEADER_BYTES, values.Header.Exports(a1)
	bne.w bad
	move.l values.Header.ExportCount(a1), d0
	cmpi.l #memory.LIMIT/values.EXPORT_BYTES, d0
	bhi.w bad
	mulu.l #values.EXPORT_BYTES, d0
	addi.l #values.HEADER_BYTES, d0
	cmp.l values.Header.Members(a1), d0
	bne.w bad
	move.l values.Header.MemberCount(a1), d1
	cmpi.l #memory.LIMIT/values.MEMBER_BYTES, d1
	bhi.w bad
	mulu.l #values.MEMBER_BYTES, d1
	add.l d1, d0
	bcs.w bad
	cmp.l values.Header.Names(a1), d0
	bne.w bad
	add.l values.Header.NameBytes(a1), d0
	bcs.w bad
	addq.l #7, d0
	bcs.w bad
	andi.l #$fffffff8, d0
	cmp.l values.Header.Arena(a1), d0
	bne.w bad
	add.l values.Header.ArenaBytes(a1), d0
	bcs.w bad
	cmp.l State.ValueBytes(a6), d0
	bne.w bad
	lea State.Values(a6), a2
	moveq #values.HEADER_BYTES/4-1, d0
copyValues
	move.l (a1)+, (a2)+
	dbra d0, copyValues
	tst.l State.TemplateBytes(a6)
	beq.w good
	move.l State.TemplateOffset(a6), d2
	moveq #templates.HEADER_BYTES, d3
	bsr.w read
	bne.w bad
	cmpi.l #templates.MAGIC, templates.Header.Magic(a1)
	bne.w bad
	move.l State.TemplateBytes(a6), d0
	cmp.l templates.Header.Bytes(a1), d0
	bne.w bad
	cmpi.l #templates.HEADER_BYTES, templates.Header.Bindings(a1)
	bne.w bad
	move.l templates.Header.BindingCount(a1), d0
	cmpi.l #65535, d0
	bhi.w bad
	mulu.l #names.ROW_BYTES, d0
	addi.l #templates.HEADER_BYTES, d0
	cmp.l templates.Header.Names(a1), d0
	bne.w bad
	add.l templates.Header.NameBytes(a1), d0
	bcs.w bad
	addq.l #3, d0
	bcs.w bad
	andi.l #$fffffffc, d0
	cmp.l templates.Header.Rows(a1), d0
	bne.w bad
	move.l templates.Header.Count(a1), d1
	cmpi.l #memory.LIMIT/templates.ROW_BYTES, d1
	bhi.w bad
	mulu.l #templates.ROW_BYTES, d1
	add.l d1, d0
	bcs.w bad
	cmp.l templates.Header.Data(a1), d0
	bne.w bad
	add.l templates.Header.DataBytes(a1), d0
	bcs.w bad
	cmp.l State.TemplateBytes(a6), d0
	bne.w bad
	lea State.Templates(a6), a2
	moveq #templates.HEADER_BYTES/4-1, d0
copyTemplates
	move.l (a1)+, (a2)+
	dbra d0, copyTemplates
good
	moveq #OK, d0
	bra.w done
bad
	clr.l State.Bytes(a6)
	clr.l State.Window+memory.Block.Used(a6)
	moveq #INVALID, d0
done
	lea image.HEADER_BYTES(sp), sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; select

; A0=selected State,D2=OFMA-relative offset,D3=positive bytes<=WINDOW_BYTES.
; D0/CCR=status,A1=immutable lease. Others preserved. Every read/select/release
; may invalidate a lease; callers copy records before another reader operation.
; A cache fill reads at most 64 KiB and allocates exactly that bounded capacity.
read	.block
	movem.l d1-d7/a0/a2-a6, -(sp)
	movea.l a0, a6
	tst.l State.Bytes(a6)
	beq.w bad
	tst.l d3
	ble.w bad
	cmpi.l #WINDOW_BYTES, d3
	bhi.w bad
	move.l d2, d4
	add.l d3, d4
	bcs.w bad
	cmp.l State.Bytes(a6), d4
	bhi.w bad
	cmp.l State.Start(a6), d2
	blo.w fill
	move.l State.Start(a6), d0
	add.l State.Window+memory.Block.Used(a6), d0
	cmp.l d0, d4
	bls.w borrowed
fill
	move.l d2, d7
	clr.l State.Window+memory.Block.Used(a6)
	move.l d2, d5
	andi.l #$ffff0000, d5
	move.l d5, d0
	addi.l #WINDOW_BYTES, d0
	cmp.l d0, d4
	bls.w pageReady
	move.l d2, d5
pageReady
	move.l State.Bytes(a6), d6
	sub.l d5, d6
	cmpi.l #WINDOW_BYTES, d6
	bls.w sized
	move.l #WINDOW_BYTES, d6
sized
	lea State.Window(a6), a0
	move.l #WINDOW_BYTES, d0
	jsr memory.reserveExact
	bne.w bad
	movea.l State.Registry(a6), a0
	movea.l State.Window+memory.Block.Pointer(a6), a1
	move.l State.Module(a6), d1
	move.l d5, d2
	add.l State.Offset(a6), d2
	bcs.w bad
	move.l d6, d3
	jsr registry.readRange
	bne.w bad
	move.l d5, State.Start(a6)
	move.l d6, State.Window+memory.Block.Used(a6)
	move.l d7, d2
borrowed
	movea.l State.Window+memory.Block.Pointer(a6), a1
	sub.l State.Start(a6), d2
	adda.l d2, a1
	moveq #OK, d0
	bra.w done
bad
	suba.l a1, a1
	moveq #INVALID, d0
done
	movem.l (sp)+, d1-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; read

; A0=State,D1=module-local entity ID. D0/CCR=status,A1=leased OFMS row.
; Other registers preserved. Entity rows are scanned in bounded cached batches.
valueRow	.block
	movem.l d1-d7/a0/a2, -(sp)
	movea.l a0, a2
	move.l State.Values+values.Header.ExportCount(a2), d7
	move.l State.Values+values.Header.Exports(a2), d2
	add.l State.ValueOffset(a2), d2
next
	tst.l d7
	beq.w missing
	moveq #values.EXPORT_BYTES, d3
	bsr.w read
	bne.w done
	cmp.l values.Export.Id(a1), d1
	beq.w done
	addi.l #values.EXPORT_BYTES, d2
	subq.l #1, d7
	bra.w next
missing
	suba.l a1, a1
	moveq #NOT_FOUND, d0
done
	movem.l (sp)+, d1-d7/a0/a2
	tst.l d0
	rts
	.bend  ; valueRow

; A0=State,D1=zero-based imported target ordinal. D0/CCR=status,
; A1=leased Import row. Other registers preserved. IDs are provider-local.
importRow	.block
	movem.l d2-d3, -(sp)
	cmp.l State.Directory+interface.Header.ImportCount(a0), d1
	bhs.w missing
	move.l d1, d2
	mulu.l #interface.IMPORT_BYTES, d2
	add.l State.Directory+interface.Header.Imports(a0), d2
	moveq #interface.IMPORT_BYTES, d3
	bsr.w read
	bne.w done
	move.l interface.Import.Dependency(a1), d2
	cmp.l State.Directory+interface.Header.DependencyCount(a0), d2
	bhs.w bad
	cmpi.l #65535, interface.Import.Entity(a1)
	bhi.w bad
	bra.w done
missing
	moveq #NOT_FOUND, d0
	bra.w clear
bad
	moveq #INVALID, d0
clear
	suba.l a1, a1
done
	movem.l (sp)+, d2-d3
	tst.l d0
	rts
	.bend  ; importRow

; A0=State,D1=zero-based dependency ordinal. D0/CCR=status,A1=leased
; exact current-build object filename,D1=bytes. Other registers preserved.
dependencyName	.block
	movem.l d2-d4, -(sp)
	cmp.l State.Directory+interface.Header.DependencyCount(a0), d1
	bhs.w missing
	move.l d1, d2
	mulu.l #interface.DEPENDENCY_BYTES, d2
	add.l State.Directory+interface.Header.Dependencies(a0), d2
	moveq #interface.DEPENDENCY_BYTES, d3
	bsr.w read
	bne.w done
	move.l interface.Dependency.Name(a1), d2
	move.l interface.Dependency.NameBytes(a1), d3
	beq.w bad
	cmpi.l #30, d3
	bhi.w bad
	cmp.l State.Directory+interface.Header.Names(a0), d2
	blo.w bad
	move.l d2, d4
	add.l d3, d4
	bcs.w bad
	move.l State.Directory+interface.Header.Names(a0), d1
	add.l State.Directory+interface.Header.NameBytes(a0), d1
	cmp.l d1, d4
	bhi.w bad
	bsr.w read
	bne.w done
	move.l d3, d1
	bra.w done
missing
	moveq #NOT_FOUND, d0
	bra.w clear
bad
	moveq #INVALID, d0
clear
	suba.l a1, a1
done
	movem.l (sp)+, d2-d4
	tst.l d0
	rts
	.bend  ; dependencyName

; A0=State,D1=module-local member ID. D0/CCR=status,A1=leased OFMS row.
; Other registers preserved; shares the same bounded page cache as valueRow.
memberRow	.block
	movem.l d1-d7/a0/a2, -(sp)
	movea.l a0, a2
	move.l State.Values+values.Header.MemberCount(a2), d7
	move.l State.Values+values.Header.Members(a2), d2
	add.l State.ValueOffset(a2), d2
next
	tst.l d7
	beq.w missing
	moveq #values.MEMBER_BYTES, d3
	bsr.w read
	bne.w done
	cmp.l values.Member.Id(a1), d1
	beq.w done
	addi.l #values.MEMBER_BYTES, d2
	subq.l #1, d7
	bra.w next
missing
	suba.l a1, a1
	moveq #NOT_FOUND, d0
done
	movem.l (sp)+, d1-d7/a0/a2
	tst.l d0
	rts
	.bend  ; memberRow

; A0=State,A1=spelling,D1=positive bytes<=255. D0/CCR=status;
; D1=entity ID on success, other registers preserved. ASCII folded exact public
; spelling comparison. A bare spelling also matches a qualified row's leaf;
; multiple matching entities reject as ambiguous. Nameless closure rows stay private.
lookupName	.block
	movem.l d2-d7/a0-a6, -(sp)
	subq.l #8, sp
	movea.l a0, a6
	movea.l a1, a5
	move.l d1, d6
	beq.w bad
	cmpi.l #255, d6
	bhi.w bad
	move.l a1, d0
	beq.w bad
	add.l d6, d0
	bcs.w bad
	clr.l (sp)
	clr.l 4(sp)
	moveq #0, d5
	movea.l a5, a2
	move.l d6, d0
input
	cmpi.b #'.', (a2)+
	bne.w inputNext
	moveq #1, d5
inputNext
	subq.l #1, d0
	bne.w input
	move.l State.Values+values.Header.ExportCount(a6), d7
	move.l State.Values+values.Header.Exports(a6), d4
row
	tst.l d7
	beq.w finished
	move.l d4, d2
	add.l State.ValueOffset(a6), d2
	moveq #values.EXPORT_BYTES, d3
	bsr.w read
	bne.w bad
	move.l values.Export.NameBytes(a1), d3
	beq.w next
	cmpi.l #255, d3
	bhi.w bad
	move.l values.Export.Id(a1), d1
	move.l values.Export.Name(a1), d2
	cmp.l State.Values+values.Header.Names(a6), d2
	blo.w bad
	move.l d2, d0
	add.l d3, d0
	bcs.w bad
	move.l State.Values+values.Header.Names(a6), d2
	add.l State.Values+values.Header.NameBytes(a6), d2
	bcs.w bad
	cmp.l d2, d0
	bhi.w bad
	move.l values.Export.Name(a1), d2
	add.l State.ValueOffset(a6), d2
	bsr.w read
	bne.w bad
	movea.l a1, a2
	tst.l d5
	bne.w compare
	; Select the final dot-delimited component for a bare query.
	move.l d3, d0
	movea.l a1, a3
leaf
	cmpi.b #'.', (a3)+
	bne.w leafNext
	movea.l a3, a2
	move.l d0, d3
	subq.l #1, d3
leafNext
	subq.l #1, d0
	bne.w leaf
compare
	cmp.l d6, d3
	bne.w next
	movea.l a5, a3
	move.l d6, d3
character
	moveq #0, d0
	move.b (a2)+, d0
	bsr.w folded
	move.l d0, d2
	moveq #0, d0
	move.b (a3)+, d0
	bsr.w folded
	cmp.b d2, d0
	bne.w next
	subq.l #1, d3
	bne.w character
	tst.l (sp)
	beq.w found
	cmp.l 4(sp), d1
	bne.w bad
	bra.w next
found
	move.l #1, (sp)
	move.l d1, 4(sp)
next
	addi.l #values.EXPORT_BYTES, d4
	subq.l #1, d7
	bra.w row
finished
	tst.l (sp)
	beq.w missing
	move.l 4(sp), d1
	moveq #OK, d0
	bra.w done
missing
	moveq #NOT_FOUND, d0
	bra.w done
bad
	moveq #INVALID, d0
done
	addq.l #8, sp
	movem.l (sp)+, d2-d7/a0-a6
	tst.l d0
	rts
folded
	cmpi.b #'A', d0
	blo.w unchanged
	cmpi.b #'Z', d0
	bhi.w unchanged
	addi.b #'a'-'A', d0
unchanged
	rts
	.bend  ; lookupName

; A0=State,D2=OFMS-relative offset,D3=bytes. Same lease/status as read.
valueRead	.block
	movem.l d2/d4, -(sp)
	move.l d2, d4
	add.l d3, d4
	bcs.w bad
	cmp.l State.ValueBytes(a0), d4
	bhi.w bad
	add.l State.ValueOffset(a0), d2
	bcs.w bad
	bsr.w read
	bra.w done
bad
	suba.l a1, a1
	moveq #INVALID, d0
done
	movem.l (sp)+, d2/d4
	tst.l d0
	rts
	.bend  ; valueRead
; A0=State,D2=OFTM-relative offset,D3=bytes. Same lease/status as read.
templateRead	.block
	movem.l d2/d4, -(sp)
	move.l d2, d4
	add.l d3, d4
	bcs.w bad
	cmp.l State.TemplateBytes(a0), d4
	bhi.w bad
	add.l State.TemplateOffset(a0), d2
	bcs.w bad
	bsr.w read
	bra.w done
bad
	suba.l a1, a1
	moveq #INVALID, d0
done
	movem.l (sp)+, d2/d4
	tst.l d0
	rts
	.bend  ; templateRead
; A0=State,D1=zero-based template row ordinal. Same lease/status as read.
templateRow	.block
	movem.l d2-d3, -(sp)
	cmp.l State.Templates+templates.Header.Count(a0), d1
	bhs.w missing
	move.l d1, d2
	mulu.l #templates.ROW_BYTES, d2
	add.l State.Templates+templates.Header.Rows(a0), d2
	moveq #templates.ROW_BYTES, d3
	bsr.w templateRead
	bra.w done
missing
	suba.l a1, a1
	moveq #NOT_FOUND, d0
done
	movem.l (sp)+, d2-d3
	tst.l d0
	rts
	.bend  ; templateRow

; A0=State,D2=arena-relative offset,D3=record bytes<=WINDOW_BYTES.
; D0/CCR=status,A1=lease; others preserved. Record decoding belongs to values.
valueRecord	.block
	movem.l d2/d4, -(sp)
	move.l d2, d4
	add.l d3, d4
	bcs.w bad
	cmp.l State.Values+values.Header.ArenaBytes(a0), d4
	bhi.w bad
	add.l State.Values+values.Header.Arena(a0), d2
	bcs.w bad
	add.l State.ValueOffset(a0), d2
	bcs.w bad
	bsr.w read
	bra.w done
bad
	suba.l a1, a1
	moveq #INVALID, d0
done
	movem.l (sp)+, d2/d4
	tst.l d0
	rts
	.bend  ; valueRecord

; A0=State. D0/CCR=OK; others preserved. Releases the sole cache owner.
release	.block
	movem.l d1/a0-a1, -(sp)
	movea.l a0, a1
	lea State.Window(a0), a0
	jsr memory.release
	movea.l a1, a0
	moveq #STATE_BYTES/4-1, d1
clear
	clr.l (a0)+
	dbra d1, clear
	moveq #OK, d0
	movem.l (sp)+, d1/a0-a1
	rts
	.bend  ; release
	.endsection
	.endmodule
