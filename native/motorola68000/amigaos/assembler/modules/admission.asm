; Prove private code independent of ambient package/text runtime state.
; @opforge-owner: opforge.assembler.modules.admission
	.module opforge.assembler.modules.admission
	.cpu 68020
	.use opforge.packages.format as package
	.use opforge.assembler.source.records as source
	.use opforge.assembler.layout.state as state
	.use opforge.vm.prvm.packed_text as text
	.use opforge.vm.prvm.abi as abi
	.pub
OK = 0
UNSUPPORTED = 1
INVALID = 2
	.priv
Local	.struct
Reset	.long ?
Bytes	.long ?
Ascii	.long ?
Petscii	.long ?
Mode	.long ?
	.endstruct
LOCAL_BYTES = Local.Mode+4
MODE_RUNTIME = 1
MODE_INTERFACE_FIRST = 2
MODE_INTERFACE_BODY = 3
	.section code, kind=code
	.pub
; A0=prepared records,D0=bytes,A1=package,D1/D2=builtin text identities.
; D0/CCR=status,D1=borrowed reset record bytes,A1=borrowed reset record.
; Other registers preserved. A declaration-only interface needs no reset.
; Otherwise the first effective record must be an unlabeled, unconditional
; package CPU reset. Other package state transitions and custom
; text definitions reject; builtin text selections remain module-local.
; Caller copies the reset before releasing records and emits it before carriers.
prepare	.block
	movem.l d0-d1/a0-a1, -(sp)
	bsr.w interfaceOnly
	cmpi.l #UNSUPPORTED, d0
	beq.w runtimeRecords
	adda.w #16, sp
	tst.l d0
	rts
runtimeRecords
	movem.l (sp)+, d0-d1/a0-a1
	movem.l d2-d7/a0/a2-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l sp, a6
	clr.l Local.Reset(a6)
	clr.l Local.Bytes(a6)
	move.l d1, Local.Ascii(a6)
	move.l d2, Local.Petscii(a6)
	move.l #MODE_RUNTIME, Local.Mode(a6)
	bsr.w scan
	tst.l d0
	bne.w failed
	movea.l Local.Reset(a6), a1
	move.l Local.Bytes(a6), d1
	bra.w done
failed
	suba.l a1, a1
	moveq #0, d1
done
	adda.w #LOCAL_BYTES, sp
	movem.l (sp)+, d2-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; prepare

; Same inputs/outputs as prepare. Only immutable value/schema declarations,
; ignored declarations, and an optional first effective exact CPU reset pass.
; Shared preparation has already removed selected compiletime controls; only
; empty records and authoritative omission flags bypass the live whitelist.
; Zero reset bytes/pointer means no package-state effect. No runtime statement,
; placement control, mutable declaration, or text effect is silently discarded.
interfaceOnly	.block
	movem.l d2-d7/a0/a2-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l sp, a6
	clr.l Local.Reset(a6)
	clr.l Local.Bytes(a6)
	move.l #MODE_INTERFACE_FIRST, Local.Mode(a6)
	bsr.w scan
	tst.l d0
	bne.w failed
	movea.l Local.Reset(a6), a1
	move.l Local.Bytes(a6), d1
	bra.w done
failed
	suba.l a1, a1
	moveq #0, d1
done
	adda.w #LOCAL_BYTES, sp
	movem.l (sp)+, d2-d7/a0/a2-a6
	tst.l d0
	rts
	.bend  ; interfaceOnly

; A0=whole-record append slice,D0=bytes,A1=package. D0/CCR=status; all
; other registers preserved. Only registry mutation is a hazard here: active
; text selection resets at module entries, but definitions survive globally.
; Caller keeps an append offset and sticky hazard for earlier reference modules.
textSafe	.block
	movem.l d1-d7/a0-a6, -(sp)
	suba.w #LOCAL_BYTES, sp
	movea.l sp, a6
	clr.l Local.Mode(a6)
	bsr.w scan
	adda.w #LOCAL_BYTES, sp
	movem.l (sp)+, d1-d7/a0-a6
	tst.l d0
	rts
	.bend  ; textSafe
	.priv
; A6=local request. Scratch registers; bounded prefix inspection only.
scan	.block
	movea.l a0, a3
	movea.l a1, a5
	add.l a0, d0
	bcs.w invalidRecord
	movea.l d0, a4
next
	cmpa.l a4, a3
	beq.w complete
	bhi.w invalidRecord
	move.l a4, d0
	sub.l a3, d0
	cmpi.l #source.RECORD_HEADER_BYTES, d0
	blo.w invalidRecord
	moveq #0, d7
	move.b source.RecordHeader.Length(a3), d7
	addq.l #1, d7
	cmpi.l #source.RECORD_HEADER_BYTES, d7
	blo.w invalidRecord
	cmp.l d0, d7
	bhi.w invalidRecord
	btst #source.FLAG_OMIT_BIT, source.RecordHeader.Flags(a3)
	bne.w advance
	cmpi.l #source.RECORD_HEADER_BYTES, d7
	bne.w effective
	moveq #0, d0
	move.b source.RecordHeader.Flags(a3), d0
	andi.w #$ff-source.FLAG_INDENT, d0
	beq.w advance
effective
	cmpi.l #MODE_INTERFACE_FIRST, Local.Mode(a6)
	bhs.w interfaceRecord
	tst.l Local.Mode(a6)
	beq.w textRecord
	tst.l Local.Reset(a6)
	bne.w laterRecord
	moveq #0, d0
	move.b source.RecordHeader.Flags(a3), d0
	andi.w #$ff-source.FLAG_INDENT, d0
	bne.w notSupported
	movea.l a3, a0
	move.l d7, d0
	bsr.w head
	bne.w invalidRecord
	tst.l d2
	bne.w notSupported
	cmp.w package.Header.CpuDirective(a5), d1
	bne.w notSupported
	bsr.w cpuReset
	bne.w notSupported
	move.l a3, Local.Reset(a6)
	move.l d7, Local.Bytes(a6)
	bra.w advance
laterRecord
	btst #source.FLAG_LAYOUT_BIT, source.RecordHeader.Flags(a3)
	bne.w advance
	movea.l a3, a0
	move.l d7, d0
	bsr.w head
	bne.w invalidRecord
	cmpi.l #-1, d1
	beq.w advance
	cmp.w package.Header.CpuDirective(a5), d1
	bne.w packageControl
	bsr.w cpuReset
	bne.w notSupported
	bra.w advance
packageControl
	move.l d1, d4
	move.l a0, d5
	sub.l a3, d5
	move.l d1, d0
	movea.l a5, a2
	jsr state.find
	tst.l d0
	beq.w notSupported
	cmpi.l #2, d0
	bne.w invalidRecord
	bra.w textHead
textRecord
	btst #source.FLAG_LAYOUT_BIT, source.RecordHeader.Flags(a3)
	bne.w advance
	movea.l a3, a0
	move.l d7, d0
	bsr.w head
	bne.w invalidRecord
	cmpi.l #-1, d1
	beq.w advance
	move.l d1, d4
	move.l a0, d5
	sub.l a3, d5
textHead
	movea.l a5, a0
	adda.l package.Header.TextPlan(a5), a0
	move.l package.Header.TextPlanBytes(a5), d0
	move.l d4, d1
	jsr text.headAction
	bne.w invalidRecord
	tst.l d1
	beq.w advance
	cmpi.l #text.SELECT, d1
	beq.w selection
	cmpi.l #text.TEXT, d1
	blo.w notSupported
	bra.w advance
selection
	tst.l Local.Mode(a6)
	beq.w advance
	movea.l a3, a0
	adda.l d5, a0
	bsr.w nameOperand
	bne.w notSupported
	cmp.l Local.Ascii(a6), d1
	beq.w advance
	cmp.l Local.Petscii(a6), d1
	bne.w notSupported
	bra.w advance
interfaceRecord
	moveq #0, d0
	move.b source.RecordHeader.Flags(a3), d0
	andi.w #$ff-source.FLAG_INDENT, d0
	bne.w notSupported
	cmpi.l #source.DECLARATION_HEADER_BYTES, d7
	blo.w interfaceReset
	cmpi.b #source.TOKEN_NAME_MAX, source.DeclarationRecord.Name+source.NameToken.Kind(a3)
	bhi.w interfaceReset
	tst.b source.DeclarationRecord.Name+source.NameToken.Qualifier(a3)
	bne.w notSupported
	move.b source.DeclarationRecord.Marker(a3), d0
	cmpi.b #source.TOKEN_CONSTANT_DECLARATION, d0
	beq.w interfaceDeclaration
	cmpi.b #source.TOKEN_STRUCT_DECLARATION, d0
	beq.w interfaceDeclaration
	cmpi.b #source.TOKEN_IGNORED_DECLARATION, d0
	beq.w interfaceDeclaration
interfaceReset
	cmpi.l #MODE_INTERFACE_FIRST, Local.Mode(a6)
	bne.w notSupported
	movea.l a3, a0
	move.l d7, d0
	bsr.w head
	bne.w invalidRecord
	tst.l d2
	bne.w notSupported
	cmp.w package.Header.CpuDirective(a5), d1
	bne.w notSupported
	bsr.w cpuReset
	bne.w notSupported
	move.l a3, Local.Reset(a6)
	move.l d7, Local.Bytes(a6)
interfaceDeclaration
	move.l #MODE_INTERFACE_BODY, Local.Mode(a6)
advance
	adda.l d7, a3
	bra.w next
complete
	cmpi.l #MODE_INTERFACE_FIRST, Local.Mode(a6)
	bhs.w good
	tst.l Local.Mode(a6)
	beq.w good
	tst.l Local.Reset(a6)
	beq.w notSupported
good
	moveq #OK, d0
	rts
notSupported
	moveq #UNSUPPORTED, d0
	rts
invalidRecord
	moveq #INVALID, d0
	rts
	.bend  ; scan

; A0=bounded record,D0=bytes. D0=status,D1=head ID/-1,A0=operands,
; D2=has label. Other registers preserved. Shared packed statement prefix.
head	.block
	movem.l d3/a1, -(sp)
	movea.l a0, a1
	adda.l d0, a1
	addq.l #source.RECORD_HEADER_BYTES, a0
	moveq #0, d2
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #abi.PRVM_PACKED_LABEL_BYTES, d0
	blo.w absent
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.PackedName.Kind(a0)
	bhi.w directive
	cmpi.b #abi.PRVM_TOKEN_KIND_COLON, abi.PRVM_PACKED_LABEL_COLON(a0)
	bne.w absent
	moveq #1, d2
	adda.w #abi.PRVM_PACKED_LABEL_BYTES, a0
directive
	move.l a1, d0
	sub.l a0, d0
	cmpi.l #abi.PRVM_PACKED_DOT_BYTES, d0
	blo.w absent
	cmpi.b #abi.PRVM_TOKEN_KIND_DOT, abi.PackedDot.Kind(a0)
	bne.w absent
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.PackedDot.Name+abi.PackedName.Kind(a0)
	bhi.w absent
	tst.b abi.PackedDot.Name+abi.PackedName.Qualifier(a0)
	bne.w absent
	moveq #0, d1
	move.w abi.PackedDot.Name+abi.PackedName.Id(a0), d1
	adda.w #abi.PRVM_PACKED_DOT_BYTES, a0
	moveq #OK, d0
	bra.w done
absent
	moveq #-1, d1
	moveq #OK, d0
done
	movem.l (sp)+, d3/a1
	tst.l d0
	rts
	.bend  ; head

; A0=operands,A3=record,D7=record bytes,A5=package.
cpuReset	.block
	bsr.w nameOperand
	bne.w done
	cmp.w package.Header.CpuName(a5), d1
	bne.w bad
	moveq #OK, d0
	rts
bad
	moveq #UNSUPPORTED, d0
done
	tst.l d0
	rts
	.bend  ; cpuReset

; One complete numeric name, with no qualifier/trailer. D0=status,D1=ID.
nameOperand	.block
	move.l a3, d0
	add.l d7, d0
	sub.l a0, d0
	cmpi.l #abi.PRVM_PACKED_NAME_BYTES, d0
	bne.w bad
	cmpi.b #abi.PRVM_TOKEN_KIND_REGISTER, abi.PackedName.Kind(a0)
	bhi.w bad
	tst.b abi.PackedName.Qualifier(a0)
	bne.w bad
	moveq #0, d1
	move.w abi.PackedName.Id(a0), d1
	moveq #OK, d0
	rts
bad
	moveq #UNSUPPORTED, d0
	rts
	.bend  ; nameOperand
	.endsection
	.endmodule
