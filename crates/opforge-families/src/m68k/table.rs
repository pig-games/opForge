// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

//! Minimal 68000 instruction metadata for the baseline native encode slice.

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OperationSize {
    Byte,
    Word,
    Long,
}

impl OperationSize {
    pub fn suffix(self) -> &'static str {
        match self {
            Self::Byte => ".B",
            Self::Word => ".W",
            Self::Long => ".L",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MnemonicKind {
    Move,
    MoveA,
    Movem,
    Movep,
    Lea,
    Pea,
    Jmp,
    Jsr,
    Link,
    Unlk,
    Exg,
    Swap,
    Ext,
    Trap,
    Stop,
    Nop,
    Reset,
    Rte,
    Rtr,
    Trapv,
    Illegal,
    Add,
    AddA,
    Addi,
    Addx,
    Abcd,
    Chk,
    Sub,
    SubA,
    Subi,
    Subx,
    Sbcd,
    Cmp,
    CmpA,
    Cmpi,
    Cmpm,
    And,
    Andi,
    Or,
    Ori,
    Eor,
    Eori,
    Divs,
    Divu,
    Bra,
    Bsr,
    Bcc(ConditionCode),
    Dbcc(ConditionCode),
    Rts,
    Moveq,
    Muls,
    Mulu,
    Addq,
    Subq,
    Bit(BitMnemonic),
    Scc(ConditionCode),
    Clr,
    Negx,
    Neg,
    Nbcd,
    Not,
    Tas,
    Tst,
    Shift(ShiftMnemonic),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum M68010MnemonicKind {
    Bkpt,
    Movec,
    Moves,
    Rtd,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum M68020MnemonicKind {
    Extb,
    Cas,
    Cas2,
    Chk2,
    Cmp2,
    BitField(BitFieldMnemonic),
    Pack,
    Unpk,
    Trapcc(ConditionCode),
    Callm,
    Rtm,
    Pflush,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BitFieldMnemonic {
    Bftst,
    Bfextu,
    Bfchg,
    Bfexts,
    Bfclr,
    Bfffo,
    Bfset,
    Bfins,
}

impl BitFieldMnemonic {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Bftst => "BFTST",
            Self::Bfextu => "BFEXTU",
            Self::Bfchg => "BFCHG",
            Self::Bfexts => "BFEXTS",
            Self::Bfclr => "BFCLR",
            Self::Bfffo => "BFFFO",
            Self::Bfset => "BFSET",
            Self::Bfins => "BFINS",
        }
    }

    pub fn opcode_base(self) -> u16 {
        match self {
            Self::Bftst => 0xE8C0,
            Self::Bfextu => 0xE9C0,
            Self::Bfchg => 0xEAC0,
            Self::Bfexts => 0xEBC0,
            Self::Bfclr => 0xECC0,
            Self::Bfffo => 0xEDC0,
            Self::Bfset => 0xEEC0,
            Self::Bfins => 0xEFC0,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ConditionCode {
    True,
    False,
    Hi,
    Ls,
    Cc,
    Cs,
    Ne,
    Eq,
    Vc,
    Vs,
    Pl,
    Mi,
    Ge,
    Lt,
    Gt,
    Le,
}

impl ConditionCode {
    pub fn opcode_bits(self) -> u16 {
        match self {
            Self::True => 0x0,
            Self::False => 0x1,
            Self::Hi => 0x2,
            Self::Ls => 0x3,
            Self::Cc => 0x4,
            Self::Cs => 0x5,
            Self::Ne => 0x6,
            Self::Eq => 0x7,
            Self::Vc => 0x8,
            Self::Vs => 0x9,
            Self::Pl => 0xA,
            Self::Mi => 0xB,
            Self::Ge => 0xC,
            Self::Lt => 0xD,
            Self::Gt => 0xE,
            Self::Le => 0xF,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ShiftMnemonic {
    Asl,
    Asr,
    Lsl,
    Lsr,
    Roxl,
    Roxr,
    Rol,
    Ror,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BitMnemonic {
    Btst,
    Bchg,
    Bclr,
    Bset,
}

impl BitMnemonic {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Btst => "BTST",
            Self::Bchg => "BCHG",
            Self::Bclr => "BCLR",
            Self::Bset => "BSET",
        }
    }

    pub fn dynamic_opcode_base(self) -> u16 {
        match self {
            Self::Btst => 0x0100,
            Self::Bchg => 0x0140,
            Self::Bclr => 0x0180,
            Self::Bset => 0x01C0,
        }
    }

    pub fn static_opcode_base(self) -> u16 {
        match self {
            Self::Btst => 0x0800,
            Self::Bchg => 0x0840,
            Self::Bclr => 0x0880,
            Self::Bset => 0x08C0,
        }
    }
}

impl ShiftMnemonic {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Asl => "ASL",
            Self::Asr => "ASR",
            Self::Lsl => "LSL",
            Self::Lsr => "LSR",
            Self::Roxl => "ROXL",
            Self::Roxr => "ROXR",
            Self::Rol => "ROL",
            Self::Ror => "ROR",
        }
    }

    pub fn direction_bit(self) -> u16 {
        match self {
            Self::Asr | Self::Lsr | Self::Roxr | Self::Ror => 0,
            Self::Asl | Self::Lsl | Self::Roxl | Self::Rol => 1,
        }
    }

    pub fn kind_bits(self) -> u16 {
        match self {
            Self::Asl | Self::Asr => 0b00,
            Self::Lsl | Self::Lsr => 0b01,
            Self::Roxl | Self::Roxr => 0b10,
            Self::Rol | Self::Ror => 0b11,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsedMnemonic {
    pub kind: MnemonicKind,
    pub display_name: String,
    pub size: Option<OperationSize>,
    pub has_unknown_size_suffix: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsedM68010Mnemonic {
    pub kind: M68010MnemonicKind,
    pub display_name: String,
    pub size: Option<OperationSize>,
    pub has_unknown_size_suffix: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsedM68020Mnemonic {
    pub kind: M68020MnemonicKind,
    pub display_name: String,
    pub size: Option<OperationSize>,
    pub has_unknown_size_suffix: bool,
}

fn base_kind(base: &str) -> Option<MnemonicKind> {
    match base {
        "MOVE" => Some(MnemonicKind::Move),
        "MOVEA" => Some(MnemonicKind::MoveA),
        "MOVEM" => Some(MnemonicKind::Movem),
        "MOVEP" => Some(MnemonicKind::Movep),
        "LEA" => Some(MnemonicKind::Lea),
        "PEA" => Some(MnemonicKind::Pea),
        "JMP" => Some(MnemonicKind::Jmp),
        "JSR" => Some(MnemonicKind::Jsr),
        "LINK" => Some(MnemonicKind::Link),
        "UNLK" => Some(MnemonicKind::Unlk),
        "EXG" => Some(MnemonicKind::Exg),
        "SWAP" => Some(MnemonicKind::Swap),
        "EXT" => Some(MnemonicKind::Ext),
        "TRAP" => Some(MnemonicKind::Trap),
        "STOP" => Some(MnemonicKind::Stop),
        "NOP" => Some(MnemonicKind::Nop),
        "RESET" => Some(MnemonicKind::Reset),
        "RTE" => Some(MnemonicKind::Rte),
        "RTR" => Some(MnemonicKind::Rtr),
        "TRAPV" => Some(MnemonicKind::Trapv),
        "ILLEGAL" => Some(MnemonicKind::Illegal),
        "ADD" => Some(MnemonicKind::Add),
        "ADDA" => Some(MnemonicKind::AddA),
        "ADDI" => Some(MnemonicKind::Addi),
        "ADDX" => Some(MnemonicKind::Addx),
        "ABCD" => Some(MnemonicKind::Abcd),
        "CHK" => Some(MnemonicKind::Chk),
        "SUB" => Some(MnemonicKind::Sub),
        "SUBA" => Some(MnemonicKind::SubA),
        "SUBI" => Some(MnemonicKind::Subi),
        "SUBX" => Some(MnemonicKind::Subx),
        "SBCD" => Some(MnemonicKind::Sbcd),
        "CMP" => Some(MnemonicKind::Cmp),
        "CMPA" => Some(MnemonicKind::CmpA),
        "CMPI" => Some(MnemonicKind::Cmpi),
        "CMPM" => Some(MnemonicKind::Cmpm),
        "AND" => Some(MnemonicKind::And),
        "ANDI" => Some(MnemonicKind::Andi),
        "OR" => Some(MnemonicKind::Or),
        "ORI" => Some(MnemonicKind::Ori),
        "EOR" => Some(MnemonicKind::Eor),
        "EORI" => Some(MnemonicKind::Eori),
        "DIVS" => Some(MnemonicKind::Divs),
        "DIVU" => Some(MnemonicKind::Divu),
        "BRA" => Some(MnemonicKind::Bra),
        "BSR" => Some(MnemonicKind::Bsr),
        "BHI" => Some(MnemonicKind::Bcc(ConditionCode::Hi)),
        "BLS" => Some(MnemonicKind::Bcc(ConditionCode::Ls)),
        "BCC" | "BHS" => Some(MnemonicKind::Bcc(ConditionCode::Cc)),
        "BCS" | "BLO" => Some(MnemonicKind::Bcc(ConditionCode::Cs)),
        "BNE" => Some(MnemonicKind::Bcc(ConditionCode::Ne)),
        "BEQ" => Some(MnemonicKind::Bcc(ConditionCode::Eq)),
        "BVC" => Some(MnemonicKind::Bcc(ConditionCode::Vc)),
        "BVS" => Some(MnemonicKind::Bcc(ConditionCode::Vs)),
        "BPL" => Some(MnemonicKind::Bcc(ConditionCode::Pl)),
        "BMI" => Some(MnemonicKind::Bcc(ConditionCode::Mi)),
        "BGE" => Some(MnemonicKind::Bcc(ConditionCode::Ge)),
        "BLT" => Some(MnemonicKind::Bcc(ConditionCode::Lt)),
        "BGT" => Some(MnemonicKind::Bcc(ConditionCode::Gt)),
        "BLE" => Some(MnemonicKind::Bcc(ConditionCode::Le)),
        "DBT" => Some(MnemonicKind::Dbcc(ConditionCode::True)),
        "DBF" | "DBRA" => Some(MnemonicKind::Dbcc(ConditionCode::False)),
        "DBHI" => Some(MnemonicKind::Dbcc(ConditionCode::Hi)),
        "DBLS" => Some(MnemonicKind::Dbcc(ConditionCode::Ls)),
        "DBCC" | "DBHS" => Some(MnemonicKind::Dbcc(ConditionCode::Cc)),
        "DBCS" | "DBLO" => Some(MnemonicKind::Dbcc(ConditionCode::Cs)),
        "DBNE" => Some(MnemonicKind::Dbcc(ConditionCode::Ne)),
        "DBEQ" => Some(MnemonicKind::Dbcc(ConditionCode::Eq)),
        "DBVC" => Some(MnemonicKind::Dbcc(ConditionCode::Vc)),
        "DBVS" => Some(MnemonicKind::Dbcc(ConditionCode::Vs)),
        "DBPL" => Some(MnemonicKind::Dbcc(ConditionCode::Pl)),
        "DBMI" => Some(MnemonicKind::Dbcc(ConditionCode::Mi)),
        "DBGE" => Some(MnemonicKind::Dbcc(ConditionCode::Ge)),
        "DBLT" => Some(MnemonicKind::Dbcc(ConditionCode::Lt)),
        "DBGT" => Some(MnemonicKind::Dbcc(ConditionCode::Gt)),
        "DBLE" => Some(MnemonicKind::Dbcc(ConditionCode::Le)),
        "RTS" => Some(MnemonicKind::Rts),
        "MOVEQ" => Some(MnemonicKind::Moveq),
        "MULS" => Some(MnemonicKind::Muls),
        "MULU" => Some(MnemonicKind::Mulu),
        "ADDQ" => Some(MnemonicKind::Addq),
        "SUBQ" => Some(MnemonicKind::Subq),
        "BTST" => Some(MnemonicKind::Bit(BitMnemonic::Btst)),
        "BCHG" => Some(MnemonicKind::Bit(BitMnemonic::Bchg)),
        "BCLR" => Some(MnemonicKind::Bit(BitMnemonic::Bclr)),
        "BSET" => Some(MnemonicKind::Bit(BitMnemonic::Bset)),
        "ST" => Some(MnemonicKind::Scc(ConditionCode::True)),
        "SF" => Some(MnemonicKind::Scc(ConditionCode::False)),
        "SHI" => Some(MnemonicKind::Scc(ConditionCode::Hi)),
        "SLS" => Some(MnemonicKind::Scc(ConditionCode::Ls)),
        "SCC" | "SHS" => Some(MnemonicKind::Scc(ConditionCode::Cc)),
        "SCS" | "SLO" => Some(MnemonicKind::Scc(ConditionCode::Cs)),
        "SNE" => Some(MnemonicKind::Scc(ConditionCode::Ne)),
        "SEQ" => Some(MnemonicKind::Scc(ConditionCode::Eq)),
        "SVC" => Some(MnemonicKind::Scc(ConditionCode::Vc)),
        "SVS" => Some(MnemonicKind::Scc(ConditionCode::Vs)),
        "SPL" => Some(MnemonicKind::Scc(ConditionCode::Pl)),
        "SMI" => Some(MnemonicKind::Scc(ConditionCode::Mi)),
        "SGE" => Some(MnemonicKind::Scc(ConditionCode::Ge)),
        "SLT" => Some(MnemonicKind::Scc(ConditionCode::Lt)),
        "SGT" => Some(MnemonicKind::Scc(ConditionCode::Gt)),
        "SLE" => Some(MnemonicKind::Scc(ConditionCode::Le)),
        "CLR" => Some(MnemonicKind::Clr),
        "NEGX" => Some(MnemonicKind::Negx),
        "NEG" => Some(MnemonicKind::Neg),
        "NBCD" => Some(MnemonicKind::Nbcd),
        "NOT" => Some(MnemonicKind::Not),
        "TAS" => Some(MnemonicKind::Tas),
        "TST" => Some(MnemonicKind::Tst),
        "ASL" => Some(MnemonicKind::Shift(ShiftMnemonic::Asl)),
        "ASR" => Some(MnemonicKind::Shift(ShiftMnemonic::Asr)),
        "LSL" => Some(MnemonicKind::Shift(ShiftMnemonic::Lsl)),
        "LSR" => Some(MnemonicKind::Shift(ShiftMnemonic::Lsr)),
        "ROXL" => Some(MnemonicKind::Shift(ShiftMnemonic::Roxl)),
        "ROXR" => Some(MnemonicKind::Shift(ShiftMnemonic::Roxr)),
        "ROL" => Some(MnemonicKind::Shift(ShiftMnemonic::Rol)),
        "ROR" => Some(MnemonicKind::Shift(ShiftMnemonic::Ror)),
        _ => None,
    }
}

fn m68010_base_kind(base: &str) -> Option<M68010MnemonicKind> {
    match base {
        "BKPT" => Some(M68010MnemonicKind::Bkpt),
        "MOVEC" => Some(M68010MnemonicKind::Movec),
        "MOVES" => Some(M68010MnemonicKind::Moves),
        "RTD" => Some(M68010MnemonicKind::Rtd),
        _ => None,
    }
}

fn m68020_base_kind(base: &str) -> Option<M68020MnemonicKind> {
    match base {
        "EXTB" => Some(M68020MnemonicKind::Extb),
        "CAS" => Some(M68020MnemonicKind::Cas),
        "CAS2" => Some(M68020MnemonicKind::Cas2),
        "CHK2" => Some(M68020MnemonicKind::Chk2),
        "CMP2" => Some(M68020MnemonicKind::Cmp2),
        "BFTST" => Some(M68020MnemonicKind::BitField(BitFieldMnemonic::Bftst)),
        "BFEXTU" => Some(M68020MnemonicKind::BitField(BitFieldMnemonic::Bfextu)),
        "BFCHG" => Some(M68020MnemonicKind::BitField(BitFieldMnemonic::Bfchg)),
        "BFEXTS" => Some(M68020MnemonicKind::BitField(BitFieldMnemonic::Bfexts)),
        "BFCLR" => Some(M68020MnemonicKind::BitField(BitFieldMnemonic::Bfclr)),
        "BFFFO" => Some(M68020MnemonicKind::BitField(BitFieldMnemonic::Bfffo)),
        "BFSET" => Some(M68020MnemonicKind::BitField(BitFieldMnemonic::Bfset)),
        "BFINS" => Some(M68020MnemonicKind::BitField(BitFieldMnemonic::Bfins)),
        "PACK" => Some(M68020MnemonicKind::Pack),
        "UNPK" => Some(M68020MnemonicKind::Unpk),
        "TRAPT" => Some(M68020MnemonicKind::Trapcc(ConditionCode::True)),
        "TRAPF" => Some(M68020MnemonicKind::Trapcc(ConditionCode::False)),
        "TRAPHI" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Hi)),
        "TRAPLS" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Ls)),
        "TRAPCC" | "TRAPHS" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Cc)),
        "TRAPCS" | "TRAPLO" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Cs)),
        "TRAPNE" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Ne)),
        "TRAPEQ" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Eq)),
        "TRAPVC" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Vc)),
        "TRAPVS" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Vs)),
        "TRAPPL" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Pl)),
        "TRAPMI" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Mi)),
        "TRAPGE" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Ge)),
        "TRAPLT" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Lt)),
        "TRAPGT" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Gt)),
        "TRAPLE" => Some(M68020MnemonicKind::Trapcc(ConditionCode::Le)),
        "CALLM" => Some(M68020MnemonicKind::Callm),
        "RTM" => Some(M68020MnemonicKind::Rtm),
        "PFLUSH" => Some(M68020MnemonicKind::Pflush),
        _ => None,
    }
}

fn split_size_suffix(mnemonic: &str) -> (String, Option<OperationSize>, bool) {
    let upper = mnemonic.to_ascii_uppercase();
    match upper.rsplit_once('.') {
        Some((base, suffix)) => match suffix {
            "B" => (base.to_string(), Some(OperationSize::Byte), false),
            "W" => (base.to_string(), Some(OperationSize::Word), false),
            "L" => (base.to_string(), Some(OperationSize::Long), false),
            _ => (base.to_string(), None, true),
        },
        None => (upper, None, false),
    }
}

pub fn parse_mnemonic(mnemonic: &str) -> Option<ParsedMnemonic> {
    let (base, size, has_unknown_size_suffix) = split_size_suffix(mnemonic);

    Some(ParsedMnemonic {
        kind: base_kind(base.as_str())?,
        display_name: base,
        size,
        has_unknown_size_suffix,
    })
}

pub fn has_mnemonic(mnemonic: &str) -> bool {
    parse_mnemonic(mnemonic).is_some()
}

pub fn parse_m68010_mnemonic(mnemonic: &str) -> Option<ParsedM68010Mnemonic> {
    let (base, size, has_unknown_size_suffix) = split_size_suffix(mnemonic);

    Some(ParsedM68010Mnemonic {
        kind: m68010_base_kind(base.as_str())?,
        display_name: base,
        size,
        has_unknown_size_suffix,
    })
}

pub fn has_m68010_mnemonic(mnemonic: &str) -> bool {
    parse_m68010_mnemonic(mnemonic).is_some()
}

pub fn parse_m68020_mnemonic(mnemonic: &str) -> Option<ParsedM68020Mnemonic> {
    let (base, size, has_unknown_size_suffix) = split_size_suffix(mnemonic);

    Some(ParsedM68020Mnemonic {
        kind: m68020_base_kind(base.as_str())?,
        display_name: base,
        size,
        has_unknown_size_suffix,
    })
}

pub fn has_m68020_mnemonic(mnemonic: &str) -> bool {
    parse_m68020_mnemonic(mnemonic).is_some()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognises_baseline_m68k_mnemonics_with_optional_sizes() {
        assert!(has_mnemonic("MOVE"));
        assert!(has_mnemonic("move.w"));
        assert!(has_mnemonic("MOVEA.L"));
        assert!(has_mnemonic("movem.w"));
        assert!(has_mnemonic("MOVEP.L"));
        assert!(has_mnemonic("link"));
        assert!(has_mnemonic("exg"));
        assert!(has_mnemonic("swap"));
        assert!(has_mnemonic("ext.w"));
        assert!(has_mnemonic("trap"));
        assert!(has_mnemonic("ADDI.W"));
        assert!(has_mnemonic("addx.l"));
        assert!(has_mnemonic("abcd"));
        assert!(has_mnemonic("chk"));
        assert!(has_mnemonic("cmpa.l"));
        assert!(has_mnemonic("CMPI.B"));
        assert!(has_mnemonic("cmpm.w"));
        assert!(has_mnemonic("divu.w"));
        assert!(has_mnemonic("sbcd"));
        assert!(has_mnemonic("addq.w"));
        assert!(has_mnemonic("moveq"));
        assert!(has_mnemonic("muls"));
        assert!(has_mnemonic("btst"));
        assert!(has_mnemonic("bset"));
        assert!(has_mnemonic("clr.l"));
        assert!(has_mnemonic("negx.b"));
        assert!(has_mnemonic("nbcd"));
        assert!(has_mnemonic("tas"));
        assert!(has_mnemonic("tst.w"));
        assert!(has_mnemonic("stop"));
        assert!(has_mnemonic("illegal"));
        assert!(has_mnemonic("bra"));
        assert!(has_mnemonic("bhs"));
        assert!(has_mnemonic("dbra"));
        assert!(has_mnemonic("scc"));
        assert!(has_mnemonic("st"));
        assert!(has_mnemonic("asl.b"));
        assert!(has_mnemonic("roxl.w"));
        assert!(has_mnemonic("roxr"));
        assert!(has_mnemonic("rts"));
        assert!(has_mnemonic("lea"));
        assert!(has_mnemonic("JMP.Q"));
        assert!(!has_mnemonic("MOVEUSP"));
    }

    #[test]
    fn parse_mnemonic_tracks_size_suffix_state() {
        let move_word = parse_mnemonic("move.w").expect("move.w should parse");
        assert_eq!(move_word.kind, MnemonicKind::Move);
        assert_eq!(move_word.size, Some(OperationSize::Word));
        assert!(!move_word.has_unknown_size_suffix);
        assert_eq!(move_word.display_name, "MOVE");

        let jmp_unknown = parse_mnemonic("jmp.q").expect("jmp.q should parse");
        assert_eq!(jmp_unknown.kind, MnemonicKind::Jmp);
        assert_eq!(jmp_unknown.size, None);
        assert!(jmp_unknown.has_unknown_size_suffix);

        let branch = parse_mnemonic("blo.w").expect("blo.w should parse");
        assert_eq!(branch.kind, MnemonicKind::Bcc(ConditionCode::Cs));
        assert_eq!(branch.size, Some(OperationSize::Word));
        assert_eq!(branch.display_name, "BLO");
    }

    #[test]
    fn parse_m68010_mnemonic_tracks_size_suffix_state() {
        assert!(has_m68010_mnemonic("BKPT"));
        assert!(has_m68010_mnemonic("movec"));
        assert!(has_m68010_mnemonic("MOVES.W"));
        assert!(has_m68010_mnemonic("rtd"));

        let moves_unknown = parse_m68010_mnemonic("MOVES.Q").expect("MOVES base should parse");
        assert_eq!(moves_unknown.kind, M68010MnemonicKind::Moves);
        assert_eq!(moves_unknown.size, None);
        assert!(moves_unknown.has_unknown_size_suffix);
    }

    #[test]
    fn parse_m68020_mnemonic_tracks_size_suffix_state() {
        assert!(has_m68020_mnemonic("EXTB"));
        assert!(has_m68020_mnemonic("extb.l"));
        assert!(has_m68020_mnemonic("CAS.B"));
        assert!(has_m68020_mnemonic("CAS2.W"));
        assert!(has_m68020_mnemonic("BFEXTU"));
        assert!(has_m68020_mnemonic("PACK"));
        assert!(has_m68020_mnemonic("TRAPNE.W"));
        assert!(has_m68020_mnemonic("CALLM"));
        assert!(has_m68020_mnemonic("RTM"));
        assert!(has_m68020_mnemonic("PFLUSH"));

        let extb_unknown = parse_m68020_mnemonic("EXTB.Q").expect("EXTB base should parse");
        assert_eq!(extb_unknown.kind, M68020MnemonicKind::Extb);
        assert_eq!(extb_unknown.size, None);
        assert!(extb_unknown.has_unknown_size_suffix);

        let trap_unknown = parse_m68020_mnemonic("TRAPGT.Q").expect("TRAPGT base should parse");
        assert_eq!(
            trap_unknown.kind,
            M68020MnemonicKind::Trapcc(ConditionCode::Gt)
        );
        assert_eq!(trap_unknown.size, None);
        assert!(trap_unknown.has_unknown_size_suffix);
    }
}
