// SPDX-License-Identifier: GPL-3.0-or-later

/// Line processing status.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum LineStatus {
    Ok = 0,
    DirEqu = 1,
    DirDs = 2,
    NothingDone = 3,
    Skip = 4,
    Warning = 5,
    Error = 6,
    Pass1Error = 7,
}

/// Pass statistics.
#[derive(Debug, Default, Clone, Copy)]
pub struct PassCounts {
    pub lines: u32,
    pub errors: u32,
    pub warnings: u32,
}

impl PassCounts {
    pub fn new() -> Self {
        Self::default()
    }
}
