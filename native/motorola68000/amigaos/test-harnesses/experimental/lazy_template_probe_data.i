; Declarative passive-entry matrix and scalar expectations for the probe.
	.section data, kind=data
Steps .long lazy_work.clear, 0, 0, 0, 0, 0
	.long lazy_work.templatePools, ZeroPools, 0, 0, 0, 0
	.long lazy_work.templateBegin, OwnerA, 0, 0, 0, 0
	.long lazy_work.templateDefine, InternalDef, 0, 0, 0, 0
	.long lazy_work.templateOwner, 0, 1, 0, 1, 0
	.long lazy_work.contextSet, 0, 0, 0, 0, 0
	.long lazy_work.context, 0, 0, 0, 2, 0
	.long lazy_work.templateCount, 0, 0, 1, 9, 0
	.long lazy_work.contextSet, 0, 1, 0, 0, 0
	.long lazy_work.templateCount, 0, 0, 0, 7, 0
	.long lazy_work.contextSet, 0, 2, 0, 0, 0
	.long lazy_work.templateCount, 0, 0, 1, 9, 0
	.long lazy_work.contextSet, 0, 0, 0, 0, 0
	.long lazy_work.templateLine, 0, 1, 0, 7, 1
	.long lazy_work.templateClose, 0, 1, 0, 10, 1
	.long lazy_work.templateDefine, ExternalDef, 0, 0, 0, 0
	.long lazy_work.templateClose, 0, 1, 1, 14, 1
	.long lazy_work.templateCall, RootCall, 0, 0, 0, 0
	.long lazy_work.templateCall, NestedCall, 0, 0, 0, 0
	.long lazy_work.templateCall, RepeatedCall, 0, 0, 0, 0
	.long lazy_work.templatePools, Pools, 0, 0, 0, 0
	.long lazy_work.templatePools, ClosedEmptyPools, 0, 0, 0, 0
	.long 0
InternalDef .long 1, 1, 0, 1, 10, 2, 3, 1, 0, 96, 4, 6
ExternalDef .long 1, 1, 1, 2, 20, 2, 4, 2, 10, 96, 0, 0
RootCall .long 1, 1, 0, 0, 0, 4, 9, 5
NestedCall .long 1, 1, 1, 1, 0, 8, 12, 1
RepeatedCall .long 1, 1, 1, 1, 0, 8, 13, 1
Pools .long 1, 100, 96, 4, 8, 20, 30
ZeroPools .long 0, 0, 0, 0, 0, 0, 0
ClosedEmptyPools .long 1, 0, 0, 0, 0, 0, 0
Accounting .long lazy_work.TemplateFields.Errors, 0
	.long lazy_work.TemplateFields.Count, 2, lazy_work.TemplateFields.BindingCalls, 2
	.long lazy_work.TemplateFields.BindingBytes, 9, lazy_work.TemplateFields.RowBinds, 2
	.long lazy_work.TemplateFields.RowBoundBytes, 9
	.long lazy_work.TemplateFields.Rows+lazy_work.TemplateDefinition.Line, 7
	.long lazy_work.TemplateFields.Rows+lazy_work.TemplateDefinition.PlanBinds, 2
	.long lazy_work.TemplateFields.Rows+lazy_work.TemplateDefinition.BoundBytes, 9, -1
Consumers .long lazy_work.TemplateFields.Calls, 3, lazy_work.TemplateFields.EdgeCount, 2
	.long lazy_work.TemplateFields.NestedCalls, 2, lazy_work.TemplateFields.DefaultCalls, 3
	.long lazy_work.TemplateFields.DefaultSlots, 4, lazy_work.TemplateFields.EdgeSlots, 4
	.long lazy_work.TemplateFields.Edges+lazy_work.TEMPLATE_EDGE_BYTES+lazy_work.TemplateEdge.Caller, 1
	.long lazy_work.TemplateFields.Edges+lazy_work.TEMPLATE_EDGE_BYTES+lazy_work.TemplateEdge.Callee, 2
	.long lazy_work.TemplateFields.Edges+lazy_work.TEMPLATE_EDGE_BYTES+lazy_work.TemplateEdge.Line, 12
	.long lazy_work.TemplateFields.Edges+lazy_work.TEMPLATE_EDGE_BYTES+lazy_work.TemplateEdge.Calls, 2, -1
Residuals .long lazy_work.TemplateFields.Finished, 1, lazy_work.TemplateFields.Active, 0
	.long lazy_work.TemplateFields.Residual+lazy_work.TemplatePoolValues.InternalBytes, 4
	.long lazy_work.TemplateFields.Residual+lazy_work.TemplatePoolValues.TextBytes, 2
	.long lazy_work.TemplateFields.Residual+lazy_work.TemplatePoolValues.BodyBytes, 6
	.long lazy_work.TemplateFields.Residual+lazy_work.TemplatePoolValues.PlanBytes, 30, -1
Epochs .long lazy_work.TemplateFields.Epoch, 2, lazy_work.TemplateFields.Count, 3
	.long lazy_work.TemplateFields.EdgeCount, 3, lazy_work.TemplateFields.Calls, 4
	.long lazy_work.TemplateFields.Rows+lazy_work.TEMPLATE_DEFINITION_BYTES*2+lazy_work.TemplateDefinition.Epoch, 2
	.long lazy_work.TemplateFields.Rows+lazy_work.TEMPLATE_DEFINITION_BYTES*2+lazy_work.TemplateDefinition.Calls, 1
	.long lazy_work.TemplateFields.Owners+lazy_work.TemplateOwnerSlot.Maps, 3
	.long lazy_work.TemplateFields.Owners+lazy_work.TemplateOwnerSlot.Maps+lazy_work.TEMPLATE_DEFINITION_LIMIT*4, 0, -1
Cleared .long lazy_work.TemplateFields.Errors, 0, lazy_work.TemplateFields.Epoch, 0
	.long lazy_work.TemplateFields.Count, 0, lazy_work.TemplateFields.EdgeCount, 0
	.long lazy_work.TemplateFields.OwnerRow, 0, lazy_work.TemplateFields.Pools, 0, -1
Duplicate .long lazy_work.TemplateFields.Errors, lazy_work.TEMPLATE_INVALID
	.long lazy_work.TemplateFields.Count, 1, -1
BadRange .long lazy_work.TemplateFields.Errors, lazy_work.TEMPLATE_INVALID
	.long lazy_work.TemplateFields.Rows+lazy_work.TEMPLATE_DEFINITION_BYTES+lazy_work.TemplateDefinition.Closed, 0, -1
MissingOwner .long lazy_work.TemplateFields.Errors, lazy_work.TEMPLATE_INVALID
	.long lazy_work.TemplateFields.UnattributedCalls, 2, -1
FullDefinitions .long lazy_work.TemplateFields.Errors, lazy_work.TEMPLATE_EXHAUSTED+lazy_work.TEMPLATE_INVALID
	.long lazy_work.TemplateFields.Count, lazy_work.TEMPLATE_DEFINITION_LIMIT, -1
FullEdges .long lazy_work.TemplateFields.Errors, lazy_work.TEMPLATE_EXHAUSTED
	.long lazy_work.TemplateFields.EdgeCount, lazy_work.TEMPLATE_EDGE_LIMIT
	.long lazy_work.TemplateFields.Calls, lazy_work.TEMPLATE_EDGE_LIMIT+1, -1
Overflowed .long lazy_work.TemplateFields.Errors, lazy_work.TEMPLATE_OVERFLOW
	.long lazy_work.TemplateFields.BindingCalls, $ffffffff
	.long lazy_work.TemplateFields.Rows+lazy_work.TemplateDefinition.PlanBinds, $ffffffff, -1
FullOwners .long lazy_work.TemplateFields.Errors, lazy_work.TEMPLATE_EXHAUSTED
	.long lazy_work.TemplateFields.Active, lazy_work.TEMPLATE_OWNER_LIMIT
	.long lazy_work.TemplateFields.Epoch, lazy_work.TEMPLATE_OWNER_LIMIT, -1
Unclosed .long lazy_work.TemplateFields.Errors, lazy_work.TEMPLATE_UNCLOSED
	.long lazy_work.TemplateFields.OpenRows, 1, -1
Seed .long $10101010, $21212121, $32323232, $43434343
	.long $54545454, $65656565, $76767676, $87878787
	.long $18181818, $29292929, $3a3a3a3a, $4b4b4b4b
	.long $5c5c5c5c, $6d6d6d6d, $7e7e7e7e
	.endsection
	.section bss, kind=bss
Before .res long, 15
After .res long, 15
BeforeFlags .res word, 1
AfterFlags .res word, 1
BeforeSp .res long, 1
AfterSp .res long, 1
CaseCursor .res long, 1
OwnerA .res long, 1
OwnerB .res long, 1
ProbeOwners .res long, lazy_work.TEMPLATE_OWNER_LIMIT+1
	.endsection
