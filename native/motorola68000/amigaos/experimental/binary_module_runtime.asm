; Final numeric placement and VM recipe replay for privately compiled modules.
; @opforge-owner: experimental.amigaos.binary_module_runtime
; Images own bytes/offsets; this session owns only placement and a reusable buffer.
	.module experimental.amigaos.binary_module_runtime
	.cpu 68020
	.use experimental.amigaos.binary_module_registry as registry
	.use experimental.amigaos.binary_module_records as records
	.use experimental.amigaos.binary_module_image as image
	.use experimental.amigaos.binary_assembly as assembly
	.use experimental.amigaos.binary_package as package
	.use experimental.amigaos.binary_sections as sections
	.use experimental.amigaos.binary_memory as memory
	.use experimental.amigaos.binary_encoding as encoding
	.use experimental.amigaos.binary_hunk_references as references
	.use exprvm.amigaos.runtime as exprvm
	.pub
DESCRIBE = 0
PLACE = 1
EMIT = 2
	.priv
Session	.struct
Registry	.long ?
Frame	.long ?
Module	.long ?
Fragment	.long ?
Descriptor	.long ?  ; metadata borrow; contains no payload pointer
Placement	.long ?
	.endstruct
SESSION_BYTES = Session.Placement+4
	.section bss, kind=bss
State	.res byte, SESSION_BYTES
Link	.res byte, image.LINK_FRAME_BYTES
Buffer	.res byte, memory.Block.Used+4
	.endsection
	.section code, kind=code
	.pub
; A0=borrowed registry.State. No owners transfer; one shared assembly session.
; D0/CCR=status; other registers preserved. release ends the borrow.
begin	.block
	move.l a0, State+Session.Registry
	moveq #0, d0
	rts
	.bend  ; begin

; Shared assembly Frame.Fragment callback. A0=Frame,A1=packed fragment record,
; D0=DESCRIBE/PLACE/EMIT. Success D1=alignment,D2=payload bytes,D3=extent,
; A1=borrowed payload on EMIT, zero on DESCRIBE/PLACE. Others preserved.
; Place refreshes only this fragment's exported addresses on each layout pass.
; Emit applies package-owned recipes and forwards absolute Hunk relocations.
; DESCRIBE/PLACE use only the resident linker directory. Only EMIT acquires
; payload. Hunk emission visits each module's fragments together, allowing
; the preceding full image to retire before the next module is read.
fragment	.block
	movem.l d4-d7/a0/a2-a6, -(sp)
	move.l d0, d7
	cmpi.l #EMIT, d7
	bhi.w bad
	move.l a0, State+Session.Frame
	movea.l a1, a0
	moveq #records.FRAGMENT_BYTES, d0
	jsr records.fragmentView
	bne.w bad
	move.l d1, State+Session.Module
	move.l d2, State+Session.Fragment
	movea.l State+Session.Registry, a0
	jsr registry.locate
	bne.w bad
	move.l a2, State+Session.Descriptor
	move.l a3, State+Session.Placement
	move.l d2, Link+image.LinkFrame.BlockId
	tst.l d7
	beq.w describe
	movea.l State+Session.Frame, a4
	movea.l assembly.Frame.Context(a4), a5
	movea.l assembly.Frame.Sections(a4), a6
	cmpi.w #sections.HUNK_MODE, sections.State.Mode(a6)
	bne.w bad
	move.l package.Context.Pc(a5), registry.Placement.Base(a3)
	moveq #0, d0
	move.w sections.State.HunkCurrent(a6), d0
	move.l d0, registry.Placement.Section(a3)
	move.l #1, registry.Placement.Known(a3)
	bsr.w publishAddresses
	bne.w bad
	cmpi.l #EMIT, d7
	bne.w describe
	movea.l State+Session.Descriptor, a2
	move.l image.Block.Bytes(a2), d0
	beq.w describe  ; BSS/empty fragments have no payload or patch recipes
	lea Buffer, a0
	jsr memory.reserve
	bne.w bad
	movea.l State+Session.Registry, a0
	move.l State+Session.Module, d1
	jsr registry.acquireEmit
	bne.w bad
	move.l a1, Link+image.LinkFrame.Image
	move.l image.Header.Bytes(a1), Link+image.LinkFrame.ImageBytes
	movea.l State+Session.Placement, a3
	move.l registry.Placement.Base(a3), Link+image.LinkFrame.Base
	move.l Buffer+memory.Block.Pointer, Link+image.LinkFrame.Output
	move.l Buffer+memory.Block.Capacity, Link+image.LinkFrame.Capacity
	move.l #resolve, Link+image.LinkFrame.Resolve
	move.l #patch, Link+image.LinkFrame.Patch
	move.l #State, Link+image.LinkFrame.Context
	; linkValidated borrows image rows across callbacks. resolve may locate only
	; this same module; patch uses the package and cannot access the registry.
	lea Link, a0
	jsr image.linkValidated
	bne.w bad
	bsr.w publishRelocations
	bne.w bad
	movea.l Buffer+memory.Block.Pointer, a1
	bra.w result
describe
	suba.l a1, a1
result
	movea.l State+Session.Descriptor, a2
	move.l image.Block.Alignment(a2), d1
	move.l image.Block.Bytes(a2), d2
	move.l image.Block.Extent(a2), d3
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d4-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; fragment

; Free only this session's reusable link storage, never the borrowed registry.
; D0/CCR=0; other registers preserved. Idempotent after partial failure.
release	.block
	movem.l d1/a0, -(sp)
	lea Buffer, a0
	jsr memory.release
	lea State, a0
	moveq #SESSION_BYTES/4-1, d1
clear
	clr.l (a0)+
	dbra d1, clear
	lea Link, a0
	moveq #image.LINK_FRAME_BYTES/4-1, d1
clearLink
	clr.l (a0)+
	dbra d1, clearLink
	moveq #0, d0
	movem.l (sp)+, d1/a0
	rts
	.bend  ; release
	.priv

; D1=opaque target. D0=status,D1=absolute value,D2=zero-based section.
; Missing forward placement remains a zero address during pass one; strict
; final emission requires every referenced whole fragment to have been placed.
; Local targets always use the active module, preserving its disk-cache borrow.
; Cross-module references use installed global values, never another image.
target	.block
	movem.l d3-d6/a0-a5, -(sp)
	move.l d1, d6
	movea.l State+Session.Frame, a4
	movea.l assembly.Frame.Context(a4), a5
	btst #31, d1
	bne.w external
	move.l d1, d2
	move.l State+Session.Module, d1
	movea.l State+Session.Registry, a0
	jsr registry.locate
	bne.w bad
	tst.l registry.Placement.Known(a3)
	bne.w localKnown
	cmpi.w #1, package.Context.Pass(a5)
	bne.w bad
localKnown
	move.l registry.Placement.Base(a3), d1
	move.l registry.Placement.Section(a3), d2
	bra.w good
external
	andi.l #$7fff0000, d1
	bne.w bad
	move.l d6, d1
	andi.l #65535, d1
	cmp.l package.Context.Count(a5), d1
	bhs.w bad
	movea.l package.Context.Defined(a5), a1
	tst.b 0(a1, d1.l)
	beq.w bad
	movea.l package.Context.SectionIds(a5), a1
	moveq #0, d2
	move.b 0(a1, d1.l), d2
	beq.w bad
	cmpi.l #sections.HUNK_SLOT_COUNT, d2
	bhi.w bad
	subq.l #1, d2
	lsl.l #3, d1
	movea.l package.Context.Values(a5), a1
	tst.l exprvm.Value.High(a1, d1.l)
	bne.w bad
	move.l exprvm.Value.Low(a1, d1.l), d1
	bra.w good
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d3-d6/a0-a5
	tst.l d0
	rts
	.bend  ; target

; Link resolver. D2 identifies the opaque patch program. Hunk long fields
; encode offsets within the target section; VM branch recipes use addresses
; within the same global section so loader movement preserves their result.
resolve	.block
	movem.l d2-d4/a0-a2, -(sp)
	move.l d2, d4
	bsr.w target
	bne.w done
	cmpi.l #image.PROGRAM_ABSOLUTE, d4
	beq.w absolute
	movea.l State+Session.Placement, a0
	cmp.l registry.Placement.Section(a0), d2
	bne.w bad
	bra.w done
absolute
	move.l d1, d3
	move.l d2, d1
	addq.l #1, d1
	movea.l State+Session.Frame, a2
	movea.l assembly.Frame.Context(a2), a2
	jsr references.sectionBase
	bne.w done
	sub.l d1, d3
	move.l d3, d1
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d2-d4/a0-a2
	tst.l d0
	rts
	.bend  ; resolve

; Package-owned instruction patch execution, preserving its exact recipe ABI.
patch	.block
	move.l a0, -(sp)
	movea.l State+Session.Frame, a0
	movea.l assembly.Frame.Context(a0), a0
	movea.l package.Context.Package(a0), a0
	jsr encoding.patchPosition
	movea.l (sp)+, a0
	tst.l d0
	rts
	.bend  ; patch

; Refresh public numeric labels; private lexical IDs never enter global cells.
; D0/CCR=status, other registers preserved.
publishAddresses	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l State+Session.Registry, a0
	move.l registry.State.Addresses+memory.Block.Used(a0), d7
	movea.l registry.State.Addresses+memory.Block.Pointer(a0), a6
	movea.l State+Session.Frame, a4
	movea.l assembly.Frame.Context(a4), a5
	movea.l State+Session.Placement, a3
next
	tst.l d7
	beq.w good
	move.l registry.Address.Module(a6), d0
	cmp.l State+Session.Module, d0
	bne.w advance
	move.l registry.Address.Fragment(a6), d0
	cmp.l State+Session.Fragment, d0
	bne.w advance
	move.l registry.Address.Symbol(a6), d1
	cmp.l package.Context.Count(a5), d1
	bhs.w bad
	move.l registry.Placement.Base(a3), d2
	add.l registry.Address.Offset(a6), d2
	bcs.w bad
	movea.l package.Context.Values(a5), a1
	move.l d1, d0
	lsl.l #3, d0
	move.l d2, exprvm.Value.Low(a1, d0.l)
	clr.l exprvm.Value.High(a1, d0.l)
	movea.l package.Context.Defined(a5), a1
	move.b #1, 0(a1, d1.l)
	movea.l package.Context.SectionIds(a5), a1
	move.l registry.Placement.Section(a3), d0
	addq.l #1, d0
	move.b d0, 0(a1, d1.l)
advance
	adda.w #registry.ADDRESS_BYTES, a6
	subi.l #registry.ADDRESS_BYTES, d7
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; publishAddresses

; Absolute fields have the same section/offset callback as shared DATA and
; instruction emission. Relative recipe programs emit no Hunk relocation.
; Forward the linked addend and any external global source identity. Internal
; fragment targets have no invented source symbol and retain NO_BASE.
publishRelocations	.block
	movem.l d1-d7/a0-a6, -(sp)
	movea.l Link+image.LinkFrame.Image, a6
	move.l image.Header.RelocCount(a6), d7
	movea.l a6, a5
	adda.l image.Header.Relocs(a6), a5
next
	tst.l d7
	beq.w good
	move.l image.Reloc.Block(a5), d0
	cmp.l State+Session.Fragment, d0
	bne.w advance
	cmpi.l #image.PROGRAM_ABSOLUTE, image.Reloc.Program(a5)
	bne.w advance
	move.l image.Reloc.Target(a5), d1
	bsr.w target
	bne.w bad
	move.l d2, d6
	movea.l State+Session.Placement, a3
	move.l registry.Placement.Base(a3), d5
	move.l registry.Placement.Section(a3), d1
	addq.l #1, d1
	movea.l State+Session.Frame, a0
	movea.l assembly.Frame.Context(a0), a2
	jsr references.sectionBase
	bne.w bad
	sub.l d1, d5
	bcs.w bad
	add.l image.Reloc.Offset(a5), d5
	bcs.w bad
	move.l registry.Placement.Section(a3), d0
	move.l d6, d1
	move.l d5, d2
	movea.l Buffer+memory.Block.Pointer, a1
	move.l image.Reloc.Offset(a5), d4
	move.l 0(a1, d4.l), d4
	move.l image.Reloc.Target(a5), d3
	btst #31, d3
	bne.w externalIdentity
	move.l #references.NO_BASE, d3
	bra.w relocationReady
externalIdentity
	andi.l #65535, d3
relocationReady
	movea.l assembly.Frame.AddReloc(a0), a1
	cmpa.w #0, a1
	beq.w bad
	jsr (a1)
	bne.w bad
advance
	adda.w #image.RELOC_BYTES, a5
	subq.l #1, d7
	bra.w next
good
	moveq #0, d0
	bra.w done
bad
	moveq #1, d0
done
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; publishRelocations
	.endsection
	.endmodule
