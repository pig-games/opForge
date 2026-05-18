; Native AmigaOS opForge CLI shell.
;
; This first slice mirrors the Rust CLI argument names for a strict native
; subset and provides deterministic stage stubs until parser/emitter VMs exist
; on AmigaOS.

	.module opforge.cli.strings
	.cpu 68020
	.pub
	.section data, kind=data

DosName
	.byte "dos.library", 0
NewlineText
	.byte 10, 0
VersionText
	.byte "opForge native AmigaOS CLI 0.1", 10, 0
HelpText
	.byte "Usage: opForge [OPTIONS] [INPUT]", 10
	.byte "Native subset: INPUT, -i/--infile, --bin [FILE], --hunk [FILE], -o/--outfile, --cpu, --opasm-package, -M/--module-path", 10, 0
UsageText
	.byte "OPC-NCLI001: Usage: opForge [OPTIONS] [INPUT]", 10, 0
QuotedText
	.byte "OPC-NCLI002: quoted arguments are not supported by the native CLI subset", 10, 0
UnsupportedText
	.byte "OPC-NCLI003: recognized Rust CLI option is not implemented by native AmigaOS CLI yet: ", 0
NativeSubsetHelpText
	.byte 10, "Native subset supports INPUT, -i/--infile, --bin [FILE], --hunk [FILE], -o/--outfile, --cpu, --opasm-package, and -M/--module-path; --hunk is not implemented yet.", 10, 0
UnknownFlagText
	.byte "OPC-NCLI004: unknown CLI flag: ", 0
MissingValueText
	.byte "OPC-NCLI005: option requires a value: ", 0
NoInputText
	.byte "OPC-NCLI006: No input files specified. Use -i/--infile", 10, 0
HunkRequiredText
	.byte "OPC-NCLI007: No outputs selected. Native AmigaOS CLI currently requires --bin", 10, 0
MixedInputText
	.byte "OPC-NCLI011: Do not mix positional input with -i/--infile; use one style", 10, 0
MultiplePositionalText
	.byte "OPC-NCLI012: Multiple positional inputs are not supported; use repeatable -i/--infile", 10, 0
ModulePathCapacityText
	.byte "OPC-NCLI017: native module path capacity exceeded", 10, 0
PackageTooLargeText
	.byte "ERROR OPC-NCLI019: opasm package exceeds native package storage capacity", 10, 0
InputOpenErrorText
	.byte "OPC-NCLI008: Input source file not found: ", 0
StubHeaderText
	.byte "OPFORGE-NATIVE 1", 10
	.byte "STATUS emitter-not-implemented", 10, 0
InputLabelText
	.byte "INPUT ", 0
HunkLabelText
	.byte "HUNK ", 0
BinLabelText
	.byte "BIN ", 0
TokenizerOkText
	.byte "STATUS tokenizer-ok", 10, 0
TokenizerFailureText
	.byte "ERROR OPC-NCLI010: native tokenizer stage failed", 10, 0
ParserOkText
	.byte "STAGE parser", 10
	.byte "STATUS parser-module-use-ok", 10, 0
SessionStageText
	.byte "STAGE session", 10, 0
SessionCpuText
	.byte "SESSION-CPU ", 0
SessionPassText
	.byte "SESSION-PASS ", 0
SessionOriginText
	.byte "SESSION-ORIGIN ", 0
SessionPcText
	.byte "SESSION-PC ", 0
SessionSourceCountText
	.byte "SESSION-SOURCE-COUNT ", 0
SessionStmtCountText
	.byte "SESSION-STMT-COUNT ", 0
SessionLabelCountText
	.byte "SESSION-LABEL-COUNT ", 0
SessionImageBytesText
	.byte "SESSION-IMAGE-BYTES ", 0
SessionReadyText
	.byte "STATUS session-ready", 10, 0
NativePassOneText
	.byte "STAGE pass1", 10, 0
NativePassOneOkText
	.byte "STATUS pass1-ok", 10, 0
NativePassTwoText
	.byte "STAGE pass2", 10, 0
NativePassTwoOkText
	.byte "STATUS pass2-ok", 10, 0
NativeSelectorStatusOkText
	.byte "STATUS selector-status-ok", 10, 0
NativePassFailureText
	.byte "ERROR OPC-NCLI020: native pass engine failed", 10, 0
NativeDuplicateLabelText
	.byte "ERROR OPC-NCLI021: duplicate native label: ", 0
NativeUnresolvedLabelText
	.byte "ERROR OPC-NCLI022: unresolved native label", 10, 0
NativeOutputOkText
	.byte "STATUS output-ok", 10, 0
NativeOutputFailureText
	.byte "ERROR OPC-NCLI023: native flat output write failed", 10, 0
NativeImageCapacityText
	.byte "ERROR OPC-NCLI024: native image buffer capacity exceeded", 10, 0
NativeUnknownMnemonicText
	.byte "ERROR OPC-NCLI025: unknown native mnemonic", 10, 0
NativeUnsupportedAddressingText
	.byte "ERROR OPC-NCLI026: unsupported native addressing mode", 10, 0
NativeBadOrgText
	.byte "ERROR OPC-NCLI027: invalid native .org expression", 10, 0
NativeHunkNotImplementedText
	.byte "ERROR OPC-NCLI028: native Hunk output is not implemented; use --bin for flat output", 10, 0
NativeSelectorUnknownRawText
	.byte "OTR901: selector unknown mnemonic", 0
NativeSelectorUnsupportedRawText
	.byte "OTR901: selector unsupported address", 0
NativeSelectorOperandRawText
	.byte "OTR901: selector operand error", 0
NativeSelectedOperandCompileRawText
	.byte "OTR901: selected operand compile failed", 0
NativeLabelText
	.byte "LABEL ", 0
EmitterStubText
	.byte "STAGE emitter", 10
	.byte "ERROR OPC-NCLI009: native emitter VM not implemented", 10, 0
ParserFailureText
	.byte "ERROR OPC-NCLI013: native module/use parser stage failed", 10, 0
ModuleDepthFailureText
	.byte "ERROR OPC-NCLI016: native module depth mismatch", 10, 0
IncludeStageText
	.byte "STAGE include", 10, 0
IncludeOkText
	.byte "STATUS include-ok", 10, 0
IncludeFailureText
	.byte "ERROR OPC-NCLI014: native include expansion failed", 10, 0
ConditionalFailureText
	.byte "ERROR OPC-NCLI015: native conditional preprocessing not implemented", 10, 0
ModuleResolveFailureText
	.byte "ERROR OPC-NCLI018: native module resolution failed: ", 0
IncludeRootText
	.byte "INCLUDE-ROOT 1 ", 0
IncludeFileText
	.byte "INCLUDE-FILE 1 ", 0
IncludeEnterText
	.byte "INCLUDE-ENTER 1 ", 0
IncludeLineText
	.byte "INCLUDE-LINE ", 0
IncludeLeaveText
	.byte "INCLUDE-LEAVE 1", 10, 0
ModRootText
	.byte "MOD-ROOT ", 0
ModDefText
	.byte "MOD-DEF ", 0
ModEndText
	.byte "MOD-END ", 0
ModPathText
	.byte "MOD-PATH ", 0
UseImportText
	.byte "USE-IMPORT ", 0
UseSelectText
	.byte "USE-SELECT ", 0
UseWildcardText
	.byte "USE-WILDCARD ", 0
StatementText
	.byte "STMT ", 0
StatementExprText
	.byte "STMT-EXPR ", 0
ModuleFoundText
	.byte "MODULE ", 0
SpaceText
	.byte " ", 0
HexDigitsText
	.byte "0123456789ABCDEF"
AsKeywordText
	.byte "as"
ModuleSourceExtensionText
	.byte ".asm", 0
ProcessorAsmText
	.byte "asm"
KindStatementText
	.byte "statement"
ModuleMnemonicText
	.byte "module"
EndmoduleMnemonicText
	.byte "endmodule"
UseMnemonicText
	.byte "use"
OrgMnemonicText
	.byte ".org"
CpuMnemonicText
	.byte ".cpu"
EndMnemonicText
	.byte ".end"
NativeCliSelectedShapeAccumulatorText
	.byte "accumulator", 0
NativeCliSelectedShapeImmediateText
	.byte "immediate", 0
NativeCliSelectedShapeDirectText
	.byte "direct", 0
NativeCliSelectedShapeDirectXText
	.byte "direct_x", 0
NativeCliSelectedShapeDirectYText
	.byte "direct_y", 0
NativeCliSelectedShapeIndirectText
	.byte "indirect", 0
NativeCliSelectedShapeIndexedIndirectXText
	.byte "indexed_indirect_x", 0
NativeCliSelectedShapeIndirectIndexedYText
	.byte "indirect_indexed_y", 0
LdaMnemonicText
	.byte "lda"
StaMnemonicText
	.byte "sta"
JmpMnemonicText
	.byte "jmp"
NopMnemonicText
	.byte "nop"
ImmediateModeText
	.byte "immediate"
AbsoluteModeText
	.byte "absolute"
ModuleDirectiveText
	.byte ".module"
EndmoduleDirectiveText
	.byte ".endmodule"
UseDirectiveText
	.byte ".use"
IncludeDirectiveText
	.byte ".include"
IfDirectiveText
	.byte ".if"
IfdefDirectiveText
	.byte ".ifdef"
IfndefDirectiveText
	.byte ".ifndef"
ElseDirectiveText
	.byte ".else"
ElseifDirectiveText
	.byte ".elseif"
EndifDirectiveText
	.byte ".endif"
.ifdef OPFORGE_FS_UAE_SMOKE
defaultFsUaeArgTail
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_6502_OUTPUT
	.byte "Work:opforge_6502_native_cli_smoke.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_65C02_OUTPUT
	.byte "Work:opforge_6502_native_cli_smoke.asm --bin Work:opforge_native_out.bin --cpu 65c02 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_6502_UNKNOWN_MNEMONIC
	.byte "Work:opforge_6502_unknown_mnemonic.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_6502_UNSUPPORTED_ADDRESSING
	.byte "Work:opforge_6502_unsupported_addressing.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_6502_UNRESOLVED_LABEL
	.byte "Work:opforge_6502_unresolved_label.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_6502_BAD_ORG
	.byte "Work:opforge_6502_bad_org.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_UNSUPPORTED_OUTPUT
	.byte "Work:opforge_6502_native_cli_smoke.asm --srec Work:opforge_native_out.srec --cpu m6502 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_INPUT
	.byte "Work:opforge_missing_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_HUNK
	.byte "Work:opforge_fsuae_smoke_input.asm --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_HUNK_OUTPUT
	.byte "Work:opforge_fsuae_smoke_input.asm --hunk Work:opforge_native_out.hunk --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MIXED_INPUT
	.byte "Work:opforge_fsuae_smoke_input.asm --infile Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_BAD_PACKAGE
	.byte "Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_missing_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_PACKAGE_TOO_LARGE
	.byte "Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package_oversized.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_UNMATCHED_ENDMODULE
	.byte "Work:opforge_fsuae_unmatched_endmodule.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_UNTERMINATED_MODULE
	.byte "Work:opforge_fsuae_unterminated_module.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_BAD_USE
	.byte "Work:opforge_fsuae_bad_use.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE
	.byte "Work:opforge_fsuae_missing_module.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MISSING_MODULE_PATH
	.byte "Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm -M", 0
.else
.ifdef OPFORGE_FS_UAE_NATIVE_CLI_MODULE_PATH_OVERFLOW
	.byte "Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m68020 --opasm-package Work:opforge_cli_package.opasm -M Work:mod1 -M Work:mod2 -M Work:mod3 -M Work:mod4 -M Work:mod5 -M Work:mod6 -M Work:mod7 -M Work:mod8", 0
.else
	.byte "Work:opforge_fsuae_smoke_input.asm --bin Work:opforge_native_out.bin --cpu m6502 --opasm-package Work:opforge_cli_package.opasm -M Work:opforge_module_a --module-path Work:opforge_module_b", 0
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif
.endif

FlagHelpLong
	.byte "--help", 0
FlagHelpShort
	.byte "-h", 0
FlagVersionLong
	.byte "--version", 0
FlagVersionShort
	.byte "-V", 0
FlagInfileShort
	.byte "-i", 0
FlagInfileLong
	.byte "--infile", 0
FlagHunkLong
	.byte "--hunk", 0
FlagOutfileShort
	.byte "-o", 0
FlagOutfileLong
	.byte "--outfile", 0
FlagCpuLong
	.byte "--cpu", 0
FlagPackageLong
	.byte "--opasm-package", 0
FlagListShort
	.byte "-l", 0
FlagListLong
	.byte "--list", 0
FlagHexShort
	.byte "-x", 0
FlagHexLong
	.byte "--hex", 0
FlagSrecShort
	.byte "-s", 0
FlagSrecLong
	.byte "--srec", 0
FlagBinShort
	.byte "-b", 0
FlagBinLong
	.byte "--bin", 0
FlagDefineShort
	.byte "-D", 0
FlagDefineLong
	.byte "--define", 0
FlagIncludeShort
	.byte "-I", 0
FlagIncludeLong
	.byte "--include-path", 0
FlagModuleShort
	.byte "-M", 0
FlagModuleLong
	.byte "--module-path", 0

DefaultCpuName
	.byte "m68020", 0
DefaultFamilyName
	.byte "motorola68k"
DefaultFamilyNameEnd
M6502CpuNameText
	.byte "m6502", 0
Mos6502FamilyName
	.byte "mos6502"
mos6502FamilyNameEnd

	.endsection

	.endmodule
