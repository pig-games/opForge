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
    Addiw,
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
    Cmpiw,
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
    Move2,
    Movex,
    Moveh,
    Moviw,
    Mov3q,
    Movs,
    Movz,
    Movz2,
    Touch,
    Load,
    Loadi,
    Store,
    Storei,
    Storec,
    Storeilm,
    Padd,
    Psub,
    Pmul88,
    Pmulh,
    Pmull,
    Pmula,
    Pand,
    Pandn,
    Por,
    Peor,
    Bsel,
    Pcmpeqb,
    Pcmphib,
    Pcmpgeb,
    Pcmpgtb,
    Pcmpeqw,
    Pcmphiw,
    Pcmpgew,
    Pcmpgtw,
    Pack3216,
    Packuswb,
    Unpack1632,
    Vperm,
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
    Divsl,
    Divul,
    BitField(BitFieldMnemonic),
    Pack,
    Unpk,
    Trapcc(ConditionCode),
    Callm,
    Rtm,
    Pflush,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum M68080MnemonicKind {
    Addiw,
    Cmpiw,
    Clrq,
    Move2,
    Movex,
    Moveh,
    Moviw,
    Mov3q,
    Movs,
    Movz,
    Movz2,
    Touch,
    Extub,
    Extuw,
    Perm,
    Load,
    Loadi,
    Store,
    Storei,
    Storec,
    Storeilm,
    Padd,
    Paddb,
    Paddw,
    Paddusb,
    Paddusw,
    Psub,
    Psubb,
    Psubw,
    Psubusb,
    Psubusw,
    Pavgb,
    Pmaxsb,
    Pmaxub,
    Pmaxsw,
    Pmaxuw,
    Pminsb,
    Pminub,
    Pminsw,
    Pminuw,
    Lslq,
    Lsrq,
    Bflyb,
    Bflyw,
    C2p,
    Minterm,
    Transhi,
    Translo,
    Storem,
    Storem3,
    Tex,
    Pmul88,
    Pmulh,
    Pmull,
    Pmula,
    Pand,
    Pandn,
    Por,
    Peor,
    Bsel,
    Pcmpeqb,
    Pcmphib,
    Pcmpgeb,
    Pcmpgtb,
    Pcmpeqw,
    Pcmphiw,
    Pcmpgew,
    Pcmpgtw,
    Pack3216,
    Packuswb,
    Unpack1632,
    Vperm,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FpuMnemonicKind {
    Fnop,
    Fmove,
    Floadi,
    Fstorei,
    Fmoverz,
    Fmoveurz,
    Fmovecr,
    Fmovem,
    Fadd,
    Fsub,
    Fmul,
    Fdiv,
    Fsqrt,
    Fabs,
    Fneg,
    Fcmp,
    Ftst,
    Fint,
    Fintrz,
    Fsgldiv,
    Fsglmul,
    Fsave,
    Frestore,
    Fsin,
    Fcos,
    Fsincos,
    Ftan,
    Fasin,
    Facos,
    Fatan,
    Fsinh,
    Fcosh,
    Ftanh,
    Fatanh,
    Fetox,
    Fetoxm1,
    Ftentox,
    Ftwotox,
    Flogn,
    Flognp1,
    Flog10,
    Flog2,
    Fgetexp,
    Fgetman,
    Fscale,
    Fmod,
    Frem,
    Fbranch,
    Fdbcc,
    Fscc,
    Ftrapcc,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsedFpuMnemonic {
    pub kind: FpuMnemonicKind,
    pub display_name: String,
    pub size: Option<OperationSize>,
    pub format: Option<FpuFormat>,
    pub has_unknown_size_suffix: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsedM68080Mnemonic {
    pub kind: M68080MnemonicKind,
    pub display_name: String,
    pub size: Option<OperationSize>,
    pub has_unknown_size_suffix: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FpuFormat {
    Single,
    Double,
    Extended,
    Packed,
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
        "ADDIW" => Some(MnemonicKind::Addiw),
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
        "CMPIW" => Some(MnemonicKind::Cmpiw),
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
        "MOVE2" => Some(MnemonicKind::Move2),
        "MOVEX" => Some(MnemonicKind::Movex),
        "MOVEH" => Some(MnemonicKind::Moveh),
        "MOVIW" => Some(MnemonicKind::Moviw),
        "MOV3Q" => Some(MnemonicKind::Mov3q),
        "MOVS" => Some(MnemonicKind::Movs),
        "MOVZ" => Some(MnemonicKind::Movz),
        "MOVZ2" => Some(MnemonicKind::Movz2),
        "TOUCH" => Some(MnemonicKind::Touch),
        "LOAD" => Some(MnemonicKind::Load),
        "LOADI" => Some(MnemonicKind::Loadi),
        "STORE" => Some(MnemonicKind::Store),
        "STOREI" => Some(MnemonicKind::Storei),
        "STOREC" => Some(MnemonicKind::Storec),
        "STOREILM" => Some(MnemonicKind::Storeilm),
        "PADD" => Some(MnemonicKind::Padd),
        "PSUB" => Some(MnemonicKind::Psub),
        "PMUL88" => Some(MnemonicKind::Pmul88),
        "PMULH" => Some(MnemonicKind::Pmulh),
        "PMULL" => Some(MnemonicKind::Pmull),
        "PMULA" => Some(MnemonicKind::Pmula),
        "PAND" => Some(MnemonicKind::Pand),
        "PANDN" => Some(MnemonicKind::Pandn),
        "POR" => Some(MnemonicKind::Por),
        "PEOR" => Some(MnemonicKind::Peor),
        "BSEL" => Some(MnemonicKind::Bsel),
        "PCMPEQB" => Some(MnemonicKind::Pcmpeqb),
        "PCMPHIB" => Some(MnemonicKind::Pcmphib),
        "PCMPGEB" => Some(MnemonicKind::Pcmpgeb),
        "PCMPGTB" => Some(MnemonicKind::Pcmpgtb),
        "PCMPEQW" => Some(MnemonicKind::Pcmpeqw),
        "PCMPHIW" => Some(MnemonicKind::Pcmphiw),
        "PCMPGEW" => Some(MnemonicKind::Pcmpgew),
        "PCMPGTW" => Some(MnemonicKind::Pcmpgtw),
        "PACK3216" => Some(MnemonicKind::Pack3216),
        "PACKUSWB" | "PACKUSBW" => Some(MnemonicKind::Packuswb),
        "UNPACK1632" => Some(MnemonicKind::Unpack1632),
        "VPERM" => Some(MnemonicKind::Vperm),
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
        "DIVSL" => Some(M68020MnemonicKind::Divsl),
        "DIVUL" => Some(M68020MnemonicKind::Divul),
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

fn m68080_base_kind(base: &str) -> Option<M68080MnemonicKind> {
    match base {
        "ADDIW" => Some(M68080MnemonicKind::Addiw),
        "CMPIW" => Some(M68080MnemonicKind::Cmpiw),
        "EXTUB" => Some(M68080MnemonicKind::Extub),
        "EXTUW" => Some(M68080MnemonicKind::Extuw),
        "PERM" => Some(M68080MnemonicKind::Perm),
        "MOVE2" => Some(M68080MnemonicKind::Move2),
        "MOVEX" => Some(M68080MnemonicKind::Movex),
        "MOVEH" => Some(M68080MnemonicKind::Moveh),
        "MOVIW" => Some(M68080MnemonicKind::Moviw),
        "MOV3Q" => Some(M68080MnemonicKind::Mov3q),
        "MOVS" => Some(M68080MnemonicKind::Movs),
        "MOVZ" => Some(M68080MnemonicKind::Movz),
        "MOVZ2" => Some(M68080MnemonicKind::Movz2),
        "TOUCH" => Some(M68080MnemonicKind::Touch),
        "LOAD" => Some(M68080MnemonicKind::Load),
        "LOADI" => Some(M68080MnemonicKind::Loadi),
        "STORE" => Some(M68080MnemonicKind::Store),
        "STOREI" => Some(M68080MnemonicKind::Storei),
        "STOREC" => Some(M68080MnemonicKind::Storec),
        "STOREILM" => Some(M68080MnemonicKind::Storeilm),
        "PADD" => Some(M68080MnemonicKind::Padd),
        "PADDB" => Some(M68080MnemonicKind::Paddb),
        "PADDW" => Some(M68080MnemonicKind::Paddw),
        "PADDUSB" => Some(M68080MnemonicKind::Paddusb),
        "PADDUSW" => Some(M68080MnemonicKind::Paddusw),
        "PSUB" => Some(M68080MnemonicKind::Psub),
        "PSUBB" => Some(M68080MnemonicKind::Psubb),
        "PSUBW" => Some(M68080MnemonicKind::Psubw),
        "PSUBUSB" => Some(M68080MnemonicKind::Psubusb),
        "PSUBUSW" => Some(M68080MnemonicKind::Psubusw),
        "PAVGB" => Some(M68080MnemonicKind::Pavgb),
        "PMAXSB" => Some(M68080MnemonicKind::Pmaxsb),
        "PMAXUB" => Some(M68080MnemonicKind::Pmaxub),
        "PMAXSW" => Some(M68080MnemonicKind::Pmaxsw),
        "PMAXUW" => Some(M68080MnemonicKind::Pmaxuw),
        "PMINSB" => Some(M68080MnemonicKind::Pminsb),
        "PMINUB" => Some(M68080MnemonicKind::Pminub),
        "PMINSW" => Some(M68080MnemonicKind::Pminsw),
        "PMINUW" => Some(M68080MnemonicKind::Pminuw),
        "LSLQ" => Some(M68080MnemonicKind::Lslq),
        "LSRQ" => Some(M68080MnemonicKind::Lsrq),
        "BFLYB" => Some(M68080MnemonicKind::Bflyb),
        "BFLYW" => Some(M68080MnemonicKind::Bflyw),
        "C2P" => Some(M68080MnemonicKind::C2p),
        "MINTERM" => Some(M68080MnemonicKind::Minterm),
        "TRANSHI" => Some(M68080MnemonicKind::Transhi),
        "TRANSLO" => Some(M68080MnemonicKind::Translo),
        "STOREM" => Some(M68080MnemonicKind::Storem),
        "STOREM3" => Some(M68080MnemonicKind::Storem3),
        "TEX8" | "TEX16" | "TEX24" | "TEX" => Some(M68080MnemonicKind::Tex),
        "PMUL88" => Some(M68080MnemonicKind::Pmul88),
        "PMULH" => Some(M68080MnemonicKind::Pmulh),
        "PMULL" => Some(M68080MnemonicKind::Pmull),
        "PMULA" => Some(M68080MnemonicKind::Pmula),
        "PAND" => Some(M68080MnemonicKind::Pand),
        "PANDN" => Some(M68080MnemonicKind::Pandn),
        "POR" => Some(M68080MnemonicKind::Por),
        "PEOR" => Some(M68080MnemonicKind::Peor),
        "BSEL" => Some(M68080MnemonicKind::Bsel),
        "PCMPEQB" => Some(M68080MnemonicKind::Pcmpeqb),
        "PCMPHIB" => Some(M68080MnemonicKind::Pcmphib),
        "PCMPGEB" => Some(M68080MnemonicKind::Pcmpgeb),
        "PCMPGTB" => Some(M68080MnemonicKind::Pcmpgtb),
        "PCMPEQW" => Some(M68080MnemonicKind::Pcmpeqw),
        "PCMPHIW" => Some(M68080MnemonicKind::Pcmphiw),
        "PCMPGEW" => Some(M68080MnemonicKind::Pcmpgew),
        "PCMPGTW" => Some(M68080MnemonicKind::Pcmpgtw),
        "PACK3216" => Some(M68080MnemonicKind::Pack3216),
        "PACKUSWB" | "PACKUSBW" => Some(M68080MnemonicKind::Packuswb),
        "UNPACK1632" => Some(M68080MnemonicKind::Unpack1632),
        "VPERM" => Some(M68080MnemonicKind::Vperm),
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

fn split_fpu_suffix(mnemonic: &str) -> (String, Option<OperationSize>, Option<FpuFormat>, bool) {
    let upper = mnemonic.to_ascii_uppercase();
    match upper.rsplit_once('.') {
        Some((base, suffix)) => match suffix {
            "B" => (base.to_string(), Some(OperationSize::Byte), None, false),
            "W" => (base.to_string(), Some(OperationSize::Word), None, false),
            "L" => (base.to_string(), Some(OperationSize::Long), None, false),
            "S" => (base.to_string(), None, Some(FpuFormat::Single), false),
            "D" => (base.to_string(), None, Some(FpuFormat::Double), false),
            "X" => (base.to_string(), None, Some(FpuFormat::Extended), false),
            "P" => (base.to_string(), None, Some(FpuFormat::Packed), false),
            _ => (base.to_string(), None, None, true),
        },
        None => (upper, None, None, false),
    }
}

pub fn parse_mnemonic(mnemonic: &str) -> Option<ParsedMnemonic> {
    if mnemonic.eq_ignore_ascii_case("CLR.Q") {
        return None;
    }

    let (base, size, has_unknown_size_suffix) = split_size_suffix(mnemonic);
    let kind = base_kind(base.as_str())?;
    let is_branch_short_alias = has_unknown_size_suffix
        && mnemonic.to_ascii_uppercase().ends_with(".S")
        && matches!(
            kind,
            MnemonicKind::Bra | MnemonicKind::Bsr | MnemonicKind::Bcc(_)
        );

    Some(ParsedMnemonic {
        kind,
        display_name: base,
        size: if is_branch_short_alias {
            Some(OperationSize::Byte)
        } else {
            size
        },
        has_unknown_size_suffix: has_unknown_size_suffix && !is_branch_short_alias,
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

pub fn parse_m68080_mnemonic(mnemonic: &str) -> Option<ParsedM68080Mnemonic> {
    if mnemonic.eq_ignore_ascii_case("CLR.Q") {
        return Some(ParsedM68080Mnemonic {
            kind: M68080MnemonicKind::Clrq,
            display_name: "CLR.Q".to_string(),
            size: None,
            has_unknown_size_suffix: false,
        });
    }

    let (base, size, has_unknown_size_suffix) = split_size_suffix(mnemonic);

    Some(ParsedM68080Mnemonic {
        kind: m68080_base_kind(base.as_str())?,
        display_name: base,
        size,
        has_unknown_size_suffix,
    })
}

pub fn has_m68080_mnemonic(mnemonic: &str) -> bool {
    parse_m68080_mnemonic(mnemonic).is_some()
}

fn fpu_base_kind(base: &str) -> Option<FpuMnemonicKind> {
    match base {
        "FNOP" => Some(FpuMnemonicKind::Fnop),
        "FMOVE" => Some(FpuMnemonicKind::Fmove),
        "FLOADI" => Some(FpuMnemonicKind::Floadi),
        "FSTOREI" => Some(FpuMnemonicKind::Fstorei),
        "FMOVERZ" => Some(FpuMnemonicKind::Fmoverz),
        "FMOVEURZ" => Some(FpuMnemonicKind::Fmoveurz),
        "FMOVECR" => Some(FpuMnemonicKind::Fmovecr),
        "FMOVEM" => Some(FpuMnemonicKind::Fmovem),
        "FADD" => Some(FpuMnemonicKind::Fadd),
        "FSUB" => Some(FpuMnemonicKind::Fsub),
        "FMUL" => Some(FpuMnemonicKind::Fmul),
        "FDIV" => Some(FpuMnemonicKind::Fdiv),
        "FSQRT" => Some(FpuMnemonicKind::Fsqrt),
        "FABS" => Some(FpuMnemonicKind::Fabs),
        "FNEG" => Some(FpuMnemonicKind::Fneg),
        "FCMP" => Some(FpuMnemonicKind::Fcmp),
        "FTST" => Some(FpuMnemonicKind::Ftst),
        "FINT" => Some(FpuMnemonicKind::Fint),
        "FINTRZ" => Some(FpuMnemonicKind::Fintrz),
        "FSGLDIV" => Some(FpuMnemonicKind::Fsgldiv),
        "FSGLMUL" => Some(FpuMnemonicKind::Fsglmul),
        "FSAVE" => Some(FpuMnemonicKind::Fsave),
        "FRESTORE" => Some(FpuMnemonicKind::Frestore),
        "FSIN" => Some(FpuMnemonicKind::Fsin),
        "FCOS" => Some(FpuMnemonicKind::Fcos),
        "FSINCOS" => Some(FpuMnemonicKind::Fsincos),
        "FTAN" => Some(FpuMnemonicKind::Ftan),
        "FASIN" => Some(FpuMnemonicKind::Fasin),
        "FACOS" => Some(FpuMnemonicKind::Facos),
        "FATAN" => Some(FpuMnemonicKind::Fatan),
        "FSINH" => Some(FpuMnemonicKind::Fsinh),
        "FCOSH" => Some(FpuMnemonicKind::Fcosh),
        "FTANH" => Some(FpuMnemonicKind::Ftanh),
        "FATANH" => Some(FpuMnemonicKind::Fatanh),
        "FETOX" => Some(FpuMnemonicKind::Fetox),
        "FETOXM1" => Some(FpuMnemonicKind::Fetoxm1),
        "FTENTOX" => Some(FpuMnemonicKind::Ftentox),
        "FTWOTOX" => Some(FpuMnemonicKind::Ftwotox),
        "FLOGN" => Some(FpuMnemonicKind::Flogn),
        "FLOGNP1" => Some(FpuMnemonicKind::Flognp1),
        "FLOG10" => Some(FpuMnemonicKind::Flog10),
        "FLOG2" => Some(FpuMnemonicKind::Flog2),
        "FGETEXP" => Some(FpuMnemonicKind::Fgetexp),
        "FGETMAN" => Some(FpuMnemonicKind::Fgetman),
        "FSCALE" => Some(FpuMnemonicKind::Fscale),
        "FMOD" => Some(FpuMnemonicKind::Fmod),
        "FREM" => Some(FpuMnemonicKind::Frem),
        _ if base.starts_with("FDB") && base.len() > 3 => Some(FpuMnemonicKind::Fdbcc),
        _ if base.starts_with("FTRAP") && base.len() > 5 => Some(FpuMnemonicKind::Ftrapcc),
        _ if base.starts_with("FB") && base.len() > 2 => Some(FpuMnemonicKind::Fbranch),
        _ if base.starts_with("FS") && base.len() > 2 => Some(FpuMnemonicKind::Fscc),
        _ => None,
    }
}

pub fn parse_fpu_mnemonic(mnemonic: &str) -> Option<ParsedFpuMnemonic> {
    let (base, size, format, has_unknown_size_suffix) = split_fpu_suffix(mnemonic);

    Some(ParsedFpuMnemonic {
        kind: fpu_base_kind(base.as_str())?,
        display_name: base,
        size,
        format,
        has_unknown_size_suffix,
    })
}

pub fn has_fpu_mnemonic(mnemonic: &str) -> bool {
    parse_fpu_mnemonic(mnemonic).is_some()
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

        let branch_short = parse_mnemonic("bne.s").expect("bne.s should parse");
        assert_eq!(branch_short.kind, MnemonicKind::Bcc(ConditionCode::Ne));
        assert_eq!(branch_short.size, Some(OperationSize::Byte));
        assert!(!branch_short.has_unknown_size_suffix);

        let non_branch_short = parse_mnemonic("move.s").expect("move.s should parse as MOVE");
        assert_eq!(non_branch_short.kind, MnemonicKind::Move);
        assert_eq!(non_branch_short.size, None);
        assert!(non_branch_short.has_unknown_size_suffix);
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
        assert!(has_m68020_mnemonic("DIVSL.L"));
        assert!(has_m68020_mnemonic("DIVUL.L"));
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

    #[test]
    fn parse_m68080_mnemonic_tracks_size_suffix_state() {
        assert!(has_m68080_mnemonic("ADDIW.L"));
        assert!(has_m68080_mnemonic("CMPIW.L"));
        assert!(has_m68080_mnemonic("MOVE2"));
        assert!(has_m68080_mnemonic("MOVEX"));
        assert!(has_m68080_mnemonic("MOVEH"));
        assert!(has_m68080_mnemonic("MOVIW.L"));
        assert!(has_m68080_mnemonic("MOV3Q"));
        assert!(has_m68080_mnemonic("MOVS"));
        assert!(has_m68080_mnemonic("MOVZ"));
        assert!(has_m68080_mnemonic("MOVZ2"));
        assert!(has_m68080_mnemonic("TOUCH"));
        assert!(has_m68080_mnemonic("LOAD"));
        assert!(has_m68080_mnemonic("LOADI"));
        assert!(has_m68080_mnemonic("STORE"));
        assert!(has_m68080_mnemonic("STOREI"));
        assert!(has_m68080_mnemonic("STOREC"));
        assert!(has_m68080_mnemonic("STOREILM"));
        assert!(has_m68080_mnemonic("PADD.B"));
        assert!(has_m68080_mnemonic("PADDB"));
        assert!(has_m68080_mnemonic("PADDW"));
        assert!(has_m68080_mnemonic("PADDUSB"));
        assert!(has_m68080_mnemonic("PADDUSW"));
        assert!(has_m68080_mnemonic("PSUBB"));
        assert!(has_m68080_mnemonic("PSUBW"));
        assert!(has_m68080_mnemonic("PSUBUSB"));
        assert!(has_m68080_mnemonic("PSUBUSW"));
        assert!(has_m68080_mnemonic("PAVGB"));
        assert!(has_m68080_mnemonic("PMAXSB"));
        assert!(has_m68080_mnemonic("PMAXUB"));
        assert!(has_m68080_mnemonic("PMAXSW"));
        assert!(has_m68080_mnemonic("PMAXUW"));
        assert!(has_m68080_mnemonic("PMINSB"));
        assert!(has_m68080_mnemonic("PMINUB"));
        assert!(has_m68080_mnemonic("PMINSW"));
        assert!(has_m68080_mnemonic("PMINUW"));
        assert!(has_m68080_mnemonic("LSLQ"));
        assert!(has_m68080_mnemonic("LSRQ"));
        assert!(has_m68080_mnemonic("BFLYB"));
        assert!(has_m68080_mnemonic("BFLYW"));
        assert!(has_m68080_mnemonic("C2P"));
        assert!(has_m68080_mnemonic("MINTERM"));
        assert!(has_m68080_mnemonic("TRANSHI"));
        assert!(has_m68080_mnemonic("TRANSLO"));
        assert!(has_m68080_mnemonic("STOREM"));
        assert!(has_m68080_mnemonic("STOREM3"));
        assert!(has_m68080_mnemonic("TEX8.512"));
        assert!(has_m68080_mnemonic("TEX16.256"));
        assert!(has_m68080_mnemonic("TEX24.64"));
        assert!(has_m68080_mnemonic("TEX.B"));
        assert!(has_m68080_mnemonic("PMUL88"));
        assert!(has_m68080_mnemonic("PMULH"));
        assert!(has_m68080_mnemonic("PMULL"));
        assert!(has_m68080_mnemonic("PMULA"));
        assert!(has_m68080_mnemonic("PAND"));
        assert!(has_m68080_mnemonic("PANDN"));
        assert!(has_m68080_mnemonic("POR"));
        assert!(has_m68080_mnemonic("PEOR"));
        assert!(has_m68080_mnemonic("BSEL"));
        assert!(has_m68080_mnemonic("PCMPGTB"));
        assert!(has_m68080_mnemonic("PCMPGEW"));
        assert!(has_m68080_mnemonic("PACKUSWB"));
        assert!(has_m68080_mnemonic("PACKUSBW"));
        assert!(has_m68080_mnemonic("UNPACK1632"));

        let move2_unknown = parse_m68080_mnemonic("MOVE2.Q").expect("MOVE2 base should parse");
        assert_eq!(move2_unknown.kind, M68080MnemonicKind::Move2);
        assert_eq!(move2_unknown.size, None);
        assert!(move2_unknown.has_unknown_size_suffix);

        let moviw_long = parse_m68080_mnemonic("MOVIW.L").expect("MOVIW.L should parse");
        assert_eq!(moviw_long.kind, M68080MnemonicKind::Moviw);
        assert_eq!(moviw_long.size, Some(OperationSize::Long));
        assert!(!moviw_long.has_unknown_size_suffix);

        let paddb = parse_m68080_mnemonic("PADDB").expect("PADDB should parse");
        assert_eq!(paddb.kind, M68080MnemonicKind::Paddb);
        assert_eq!(paddb.size, None);
        assert!(!paddb.has_unknown_size_suffix);

        let pavgb = parse_m68080_mnemonic("PAVGB").expect("PAVGB should parse");
        assert_eq!(pavgb.kind, M68080MnemonicKind::Pavgb);
        assert_eq!(pavgb.size, None);
        assert!(!pavgb.has_unknown_size_suffix);

        let pmaxsb = parse_m68080_mnemonic("PMAXSB").expect("PMAXSB should parse");
        assert_eq!(pmaxsb.kind, M68080MnemonicKind::Pmaxsb);
        assert_eq!(pmaxsb.size, None);
        assert!(!pmaxsb.has_unknown_size_suffix);

        let lslq = parse_m68080_mnemonic("LSLQ").expect("LSLQ should parse");
        assert_eq!(lslq.kind, M68080MnemonicKind::Lslq);
        assert_eq!(lslq.size, None);
        assert!(!lslq.has_unknown_size_suffix);

        let bflyb = parse_m68080_mnemonic("BFLYB").expect("BFLYB should parse");
        assert_eq!(bflyb.kind, M68080MnemonicKind::Bflyb);
        assert_eq!(bflyb.size, None);
        assert!(!bflyb.has_unknown_size_suffix);

        let minterm = parse_m68080_mnemonic("MINTERM").expect("MINTERM should parse");
        assert_eq!(minterm.kind, M68080MnemonicKind::Minterm);
        assert_eq!(minterm.size, None);
        assert!(!minterm.has_unknown_size_suffix);

        let storem3 = parse_m68080_mnemonic("STOREM3").expect("STOREM3 should parse");
        assert_eq!(storem3.kind, M68080MnemonicKind::Storem3);
        assert_eq!(storem3.size, None);
        assert!(!storem3.has_unknown_size_suffix);

        let tex8 = parse_m68080_mnemonic("TEX8.512").expect("TEX8.512 should parse");
        assert_eq!(tex8.kind, M68080MnemonicKind::Tex);
        assert_eq!(tex8.size, None);
        assert!(tex8.has_unknown_size_suffix);

        let tex_byte = parse_m68080_mnemonic("TEX.B").expect("TEX.B should parse");
        assert_eq!(tex_byte.kind, M68080MnemonicKind::Tex);
        assert_eq!(tex_byte.size, Some(OperationSize::Byte));
        assert!(!tex_byte.has_unknown_size_suffix);
    }

    #[test]
    fn parse_fpu_mnemonic_tracks_size_suffix_state() {
        assert!(has_fpu_mnemonic("FNOP"));
        assert!(has_fpu_mnemonic("FMOVE"));
        assert!(has_fpu_mnemonic("FLOADI"));
        assert!(has_fpu_mnemonic("FSTOREI"));
        assert!(has_fpu_mnemonic("FMOVERZ"));
        assert!(has_fpu_mnemonic("FMOVEURZ"));
        assert!(has_fpu_mnemonic("FMOVECR"));
        assert!(has_fpu_mnemonic("FMOVEM"));
        assert!(has_fpu_mnemonic("FADD"));
        assert!(has_fpu_mnemonic("FSGLDIV"));
        assert!(has_fpu_mnemonic("FSGLMUL"));
        assert!(has_fpu_mnemonic("FSIN"));
        assert!(has_fpu_mnemonic("FSINCOS"));
        assert!(has_fpu_mnemonic("FATANH"));
        assert!(has_fpu_mnemonic("FETOX"));
        assert!(has_fpu_mnemonic("FLOGNP1"));
        assert!(has_fpu_mnemonic("FSCALE"));
        assert!(has_fpu_mnemonic("FREM"));
        assert!(has_fpu_mnemonic("FBNE"));
        assert!(has_fpu_mnemonic("FDBEQ"));
        assert!(has_fpu_mnemonic("FSGE"));
        assert!(has_fpu_mnemonic("FTRAPGT"));

        let move_unknown = parse_fpu_mnemonic("FMOVE.X").expect("FMOVE base should parse");
        assert_eq!(move_unknown.kind, FpuMnemonicKind::Fmove);
        assert_eq!(move_unknown.size, None);
        assert_eq!(move_unknown.format, Some(FpuFormat::Extended));
        assert!(!move_unknown.has_unknown_size_suffix);

        let add_double = parse_fpu_mnemonic("FADD.D").expect("FADD.D should parse");
        assert_eq!(add_double.kind, FpuMnemonicKind::Fadd);
        assert_eq!(add_double.size, None);
        assert_eq!(add_double.format, Some(FpuFormat::Double));
        assert!(!add_double.has_unknown_size_suffix);

        let sgl_div = parse_fpu_mnemonic("FSGLDIV").expect("FSGLDIV should parse");
        assert_eq!(sgl_div.kind, FpuMnemonicKind::Fsgldiv);
        assert_eq!(sgl_div.display_name, "FSGLDIV");

        let sgl_mul = parse_fpu_mnemonic("FSGLMUL").expect("FSGLMUL should parse");
        assert_eq!(sgl_mul.kind, FpuMnemonicKind::Fsglmul);
        assert_eq!(sgl_mul.display_name, "FSGLMUL");

        let move_unknown = parse_fpu_mnemonic("FMOVE.Q").expect("FMOVE base should parse");
        assert_eq!(move_unknown.kind, FpuMnemonicKind::Fmove);
        assert_eq!(move_unknown.size, None);
        assert_eq!(move_unknown.format, None);
        assert!(move_unknown.has_unknown_size_suffix);

        let move_rz = parse_fpu_mnemonic("FMOVERZ.L").expect("FMOVERZ.L should parse");
        assert_eq!(move_rz.kind, FpuMnemonicKind::Fmoverz);
        assert_eq!(move_rz.size, Some(OperationSize::Long));
        assert_eq!(move_rz.format, None);
        assert!(!move_rz.has_unknown_size_suffix);

        let loadi = parse_fpu_mnemonic("FLOADI.D").expect("FLOADI.D should parse");
        assert_eq!(loadi.kind, FpuMnemonicKind::Floadi);
        assert_eq!(loadi.size, None);
        assert_eq!(loadi.format, Some(FpuFormat::Double));
        assert!(!loadi.has_unknown_size_suffix);
    }
}
