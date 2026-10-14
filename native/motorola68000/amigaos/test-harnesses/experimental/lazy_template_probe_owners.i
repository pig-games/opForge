; Overlapping owner lifetimes mirror configuration cleanup after replay begins.
	.section code, kind=code
overlappingOwners .block
	jsr lazy_work.clear
	lea OwnerA, a0
	jsr lazy_work.templateBegin
	bsr.w setSession
	lea InternalDef, a0
	jsr lazy_work.templateDefine
	moveq #1, d0
	moveq #0, d1
	moveq #10, d2
	move.l OwnerA, d3
	jsr lazy_work.templateClose
	moveq #1, d0
	moveq #0, d1
	move.l OwnerA, d2
	jsr lazy_work.templateOwner
	moveq #0, d0
	jsr lazy_work.contextSet
	moveq #0, d1
	moveq #1, d2
	jsr lazy_work.templateCount
	lea OwnerB, a0
	jsr lazy_work.templateBegin
	move.l OwnerB, d0
	bsr.w setPacketSession
	lea InternalDef, a0
	jsr lazy_work.templateDefine
	moveq #1, d0
	moveq #0, d1
	moveq #10, d2
	move.l OwnerB, d3
	jsr lazy_work.templateClose
	moveq #1, d0
	moveq #0, d1
	move.l OwnerB, d2
	jsr lazy_work.templateOwner
	moveq #0, d1
	moveq #2, d2
	jsr lazy_work.templateCount
	move.l OwnerA, Pools+lazy_work.TemplatePoolsPacket.Session
	lea Pools, a0
	jsr lazy_work.templatePools
	.CHECK_FIELDS OtherLive, 37
	bne.w done
	; A's release must preserve B's selected owner and its independent map.
	moveq #1, d1
	moveq #7, d2
	jsr lazy_work.templateCount
	lea RootCall, a0
	jsr lazy_work.templateCall
	move.l OwnerB, Pools+lazy_work.TemplatePoolsPacket.Session
	lea Pools, a0
	jsr lazy_work.templatePools
	.CHECK_FIELDS BothFinished, 37
done
	rts
	.bend  ; overlappingOwners
	.endsection
	.section data, kind=data
OtherLive .long lazy_work.TemplateFields.Errors, 0, lazy_work.TemplateFields.Active, 1
	.long lazy_work.TemplateFields.Finished, 1, lazy_work.TemplateFields.OwnerRow, 2, -1
BothFinished .long lazy_work.TemplateFields.Errors, 0, lazy_work.TemplateFields.Active, 0
	.long lazy_work.TemplateFields.Finished, 2, lazy_work.TemplateFields.Count, 2
	.long lazy_work.TemplateFields.BindingCalls, 3, lazy_work.TemplateFields.BindingBytes, 7
	.long lazy_work.TemplateFields.Rows+lazy_work.TEMPLATE_DEFINITION_BYTES+lazy_work.TemplateDefinition.PlanBinds, 2
	.long lazy_work.TemplateFields.Rows+lazy_work.TEMPLATE_DEFINITION_BYTES+lazy_work.TemplateDefinition.BoundBytes, 7
	.long lazy_work.TemplateFields.Edges+lazy_work.TemplateEdge.Callee, 2, -1
	.endsection
