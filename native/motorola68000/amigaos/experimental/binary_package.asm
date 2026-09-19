; Shared wire and session fields for the bounded binary-source experiment.
; @opforge-owner: experimental.amigaos.binary_package

	.module experimental.amigaos.binary_package
	.cpu 68020
	.pub

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
Reserved	.word ?
MaxAddress	.long ?
RuntimeBytes	.long ?
.endstruct

Context	.struct
Values	.long ?
Defined	.long ?
Count	.long ?
Pc	.long ?
Package	.long ?
Pass	.word ?
Reserved	.word ?
.endstruct

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
Reserved	.byte ?
Mode	.word ?
Padding	.word ?
.endstruct

Projection	.struct
Kind	.byte ?
Operand	.byte ?
Class	.word ?
Literal	.long ?
ValueProgram	.word ?
Reserved	.word ?
.endstruct

Program	.struct
Kind	.word ?
Version	.word ?
Offset	.long ?
Bytes	.long ?
.endstruct

	.endmodule
