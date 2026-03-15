// SPDX-License-Identifier: GPL-3.0-or-later

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConditionalAst<TKind, TExpr, TSpan> {
    pub kind: TKind,
    pub exprs: Vec<TExpr>,
    pub span: TSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PlaceAst<TExpr, TSpan> {
    pub section: String,
    pub region: String,
    pub align: Option<TExpr>,
    pub span: TSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PackAst<TSpan> {
    pub region: String,
    pub sections: Vec<String>,
    pub span: TSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseItemAst<TSpan> {
    pub name: String,
    pub alias: Option<String>,
    pub span: TSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseParamAst<TExpr, TSpan> {
    pub name: String,
    pub value: TExpr,
    pub span: TSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UseAst<TItem, TParam, TSpan> {
    pub module_id: String,
    pub alias: Option<String>,
    pub items: Vec<TItem>,
    pub params: Vec<TParam>,
    pub span: TSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StatementDefAst<TSignature, TSpan> {
    pub keyword: String,
    pub signature: TSignature,
    pub span: TSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StatementEndAst<TSpan> {
    pub span: TSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AssignmentAst<TLabel, TOp, TExpr, TSpan> {
    pub label: TLabel,
    pub op: TOp,
    pub expr: TExpr,
    pub span: TSpan,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StatementAst<TLabel, TExpr> {
    pub label: Option<TLabel>,
    pub mnemonic: Option<String>,
    pub operands: Vec<TExpr>,
}
