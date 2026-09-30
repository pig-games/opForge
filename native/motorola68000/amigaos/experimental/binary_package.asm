; Shared wire and session fields for the bounded binary-source experiment.
; @opforge-owner: experimental.amigaos.binary_package

	.module experimental.amigaos.binary_package
	.cpu 68020
	.pub

MAGIC = $42533131; BS11
DICTIONARY_REGISTER_OR_NAMED = 1
DICTIONARY_MEMBER = 2
DICTIONARY_ROLE_ALLOWED = DICTIONARY_REGISTER_OR_NAMED+DICTIONARY_MEMBER
DICTIONARY_REGISTER_OR_NAMED_BIT = 0
DICTIONARY_MEMBER_BIT = 1

DictionaryEntry	.struct
Length	.word ?
Name	.word ?
Qualifier	.byte ?
Roles	.byte ?
.endstruct
DICTIONARY_ENTRY_BYTES = DictionaryEntry.Roles+1

Header	.struct
Magic	.long ?
Bytes	.long ?
Dictionary	.long ?
DictionaryCount	.long ?
Rows	.long ?
RowCount	.long ?
RegisterRows	.long ?
RegisterCount	.long ?
Programs	.long ?
ProgramCount	.long ?
Tokenizer	.long ?
TokenizerBytes	.long ?
CpuDirective	.word ?
OrgDirective	.word ?
ByteDirective	.word ?
WordDirective	.word ?
LongDirective	.word ?
EndDirective	.word ?
CpuName	.word ?
NameCount	.word ?
LittleEndian	.word ?
ForDirective	.word ?
MaxAddress	.long ?
RuntimeBytes	.long ?
AlignDirective	.word ?
ResDirective	.word ?
MacroCall	.long ?
MacroCallBytes	.long ?
MacroHeader	.long ?
MacroHeaderBytes	.long ?
MacroVersion	.word ?
EndforDirective	.word ?
MacroPacked	.long ?
MacroPackedBytes	.long ?
MacroSpelling	.long ?
MacroSpellingBytes	.long ?
MacroFragments	.long ?
MacroFragmentsBytes	.long ?
.endstruct

Context	.struct
Values	.long ?
Defined	.long ?
Count	.long ?
Pc	.long ?
High	.long ?
Package	.long ?
Pass	.word ?
Reserved	.word ?
Parameters	.long ?
ParameterCount	.long ?
SectionIds	.long ?
	.endstruct

Parameter	.struct
Id	.word ?
Reserved	.word ?
Low	.long ?
High	.long ?
	.endstruct
PARAMETER_BYTES = Parameter.High+4

Row	.struct
Name	.word ?
Qualifier	.byte ?
Shape	.byte ?
Owner	.byte ?
Recipe	.byte ?
Priority	.word ?
Program	.word ?
InputCount	.word ?
Inputs	.long ?
WidthRank	.byte ?
Unstable	.byte ?
MemberExcluded	.byte ?
RequiredForms	.byte ?
Mode	.word ?
TupleClasses	.word ?  ; operand bytes: zero or required register class + 1
Exclusions	.long ?
TableProgram	.word ?
Reserved2	.word ?
.endstruct

Projection	.struct
Kind	.byte ?
Operand	.byte ?
Class	.word ?
Literal	.long ?
ValueProgram	.word ?
Reserved	.word ?
.endstruct

; Scalar projection kind 0 optionally transports one exact numeric target.
; The flag is valid only for the canonical branch target input; other bits reject.
SCALAR_EXACT_IDENTITY = 1
ScalarProjection	.struct
Kind	.byte ?
Operand	.byte ?
Class	.word ?
Literal	.long ?
ValueProgram	.word ?
Flags	.word ?
.endstruct

; Projection kind 18 uses the same 12-byte wire slot with a mask map.
MaskProjection	.struct
Kind	.byte ?
Operand	.byte ?
FirstClass	.word ?
SecondClass	.word ?
FirstShift	.byte ?
SecondShift	.byte ?
ValueProgram	.word ?
Flags	.word ?
.endstruct

SequenceStage	.struct
Kind	.byte ?
Reserved	.byte ?
Program	.word ?
InputCount	.word ?
Reserved2	.word ?
Inputs	.long ?
.endstruct

Program	.struct
Kind	.word ?
Version	.word ?
Offset	.long ?
Bytes	.long ?
.endstruct

	.endmodule
