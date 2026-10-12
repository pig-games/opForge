; Current statement compiler/matcher wire contract; package policy owns grammar.
; @opforge-owner: prvm.amigaos.statement_abi
	.module prvm.amigaos.statement_abi
	.cpu 68020
	.pub
OK = 0
INVALID_ARGUMENT = 4
INVALID_TOKEN = 5
INVALID_PROGRAM = 6
OUTPUT_OVERFLOW = 7
BUDGET_EXCEEDED = 12
OP_COMPILE = $a6
OP_MATCH = $a7
OP_HEAD = $a8
OP_PUBLISH = $83
OP_END = 0
K_IDENTIFIER = 0
K_REGISTER = 1
K_NUMBER = 2
K_STRING = 3
K_COMMA = 4
K_COLON = 5
K_DOT = 7
ATOM_LITERAL = 1
ATOM_CAPTURE = 2
ATOM_OPEN = 3
ATOM_CLOSE = 4
TYPE_FIRST = 1
TYPE_LAST = 5
NONE = $ffffffff

PlanHeader	.struct
Extent	.long ?
RowCount	.long ?
LiteralScore	.long ?
AtomScore	.long ?
	.endstruct
PLAN_HEADER_BYTES = PlanHeader.AtomScore+4
AtomRow	.struct
Kind	.byte ?
CaptureType	.byte ?
Reserved	.word ?
PayloadOffset	.long ?
PayloadBytes	.long ?
PairedRow	.long ?
ColStart	.long ?
ColEnd	.long ?
	.endstruct
ATOM_ROW_BYTES = AtomRow.ColEnd+4
TokenView	.struct
Kind	.byte ?
NumericStatus	.byte ?
Reserved	.word ?
LiteralOffset	.long ?
LiteralBytes	.long ?
DecodedBytes	.long ?
ColStart	.long ?
ColEnd	.long ?
NumericHigh	.long ?
NumericLow	.long ?
	.endstruct
TOKEN_BYTES = TokenView.NumericLow+4
Capture	.struct
NameOffset	.long ?
NameBytes	.long ?
TokenIndex	.long ?
	.endstruct
CAPTURE_BYTES = Capture.TokenIndex+4

CompileProgram	.struct
Opcode	.byte ?
Quoted	.byte ?
Dot	.byte ?
OpenFirst	.byte ?
OpenSecond	.byte ?
CloseFirst	.byte ?
CloseSecond	.byte ?
Delimiter	.byte ?
TypeCount	.byte ?
	.endstruct
COMPILE_HEADER_BYTES = CompileProgram.TypeCount+1
TypeName	.struct
Id	.byte ?
NameBytes	.byte ?
	.endstruct
TYPE_NAME_HEADER_BYTES = TypeName.NameBytes+1
MatchProgram	.struct
Opcode	.byte ?
Adjacency	.byte ?
TypeCount	.byte ?
	.endstruct
MATCH_HEADER_BYTES = MatchProgram.TypeCount+1
TypePolicy	.struct
Id	.byte ?
Identifier	.byte ?
StringMin	.byte ?
StringMax	.byte ?
NumericMinHigh	.long ?
NumericMinLow	.long ?
NumericMaxHigh	.long ?
NumericMaxLow	.long ?
Reserved	.long ?
	.endstruct
TYPE_POLICY_BYTES = TypePolicy.Reserved+4
POLICY_TYPE_COUNT = 5
RENDER_FIRST = 4
RENDER_LAST = 40
RENDER_COUNT = RENDER_LAST-RENDER_FIRST+1
RenderText	.struct
Kind	.byte ?
TextBytes	.byte ?
	.endstruct
RENDER_HEADER_BYTES = RenderText.TextBytes+1
IDENTITY_BYTES = 2
HEAD_DEFINITION = 1
HEAD_END = 2
HEAD_CLASSIFY = 0
HEAD_COMPLETE = 1
HeadProgram	.struct
Opcode	.byte ?
LabelPolicy	.byte ?
Definition	.word ?
EndMarker	.word ?
Publish	.byte ?
End	.byte ?
	.endstruct
HEAD_PROGRAM_BYTES = HeadProgram.End+1
HeadFrame	.struct
ProgramPtr	.long ?
ProgramBytes	.long ?
TokenPtr	.long ?
TokenCount	.long ?
NamesPtr	.long ?
NamesCount	.long ?
Mode	.long ?
StepBudget	.long ?
	.endstruct
HEAD_FRAME_BYTES = HeadFrame.StepBudget+4

CompileFrame	.struct
ProgramPtr	.long ?
ProgramBytes	.long ?
TokenPtr	.long ?
TokenCount	.long ?
ArenaPtr	.long ?
ArenaBytes	.long ?
OutputPtr	.long ?
OutputCapacity	.long ?
WorkspacePtr	.long ?
WorkspaceCapacity	.long ?
StepBudget	.long ?
	.endstruct
COMPILE_FRAME_BYTES = CompileFrame.StepBudget+4
MatchFrame	.struct
ProgramPtr	.long ?
ProgramBytes	.long ?
PlanPtr	.long ?
PlanBytes	.long ?
TokenPtr	.long ?
TokenCount	.long ?
ArenaPtr	.long ?
ArenaBytes	.long ?
OutputPtr	.long ?
OutputCapacity	.long ?
WorkspacePtr	.long ?
WorkspaceCapacity	.long ?
StepBudget	.long ?
	.endstruct
MATCH_FRAME_BYTES = MatchFrame.StepBudget+4
	.endmodule
