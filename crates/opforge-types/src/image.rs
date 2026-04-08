// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Erik van der Tier

// Image store with hex/bin/S-record output helpers.

use std::cell::Cell;
use std::io::{self, BufReader, Read, Write};
use tempfile::NamedTempFile;

thread_local! {
    static IMAGE_STORE_FORCE_OPEN_FAILURE: Cell<bool> = const { Cell::new(false) };
}

#[doc(hidden)]
pub fn run_with_forced_open_failure_for_tests<T>(f: impl FnOnce() -> T) -> T {
    IMAGE_STORE_FORCE_OPEN_FAILURE.with(|force| force.set(true));
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(f));
    IMAGE_STORE_FORCE_OPEN_FAILURE.with(|force| force.set(false));
    match result {
        Ok(value) => value,
        Err(payload) => std::panic::resume_unwind(payload),
    }
}

/// On-disk entry size: 4 bytes big-endian address + 1 byte value.
const ENTRY_SIZE: usize = 5;

#[derive(Clone, Copy)]
struct ImageStoreEntry {
    addr: u32,
    value: u8,
}

/// Stores assembled bytes using a temp-file-backed buffer.
///
/// Bytes are appended via `store`/`store_slice` and later emitted as
/// Intel HEX or raw binary output files.
pub struct ImageStore {
    backing_file: Option<NamedTempFile>,
    entries: usize,
    write_error: Option<io::Error>,
}

impl ImageStore {
    /// Create a new image store with the default address-space policy.
    pub fn new() -> Self {
        if IMAGE_STORE_FORCE_OPEN_FAILURE.with(Cell::get) {
            return Self {
                backing_file: None,
                entries: 0,
                write_error: Some(io::Error::new(
                    io::ErrorKind::PermissionDenied,
                    "ImageStore temp-file open forced to fail for tests",
                )),
            };
        }
        match NamedTempFile::new() {
            Ok(backing_file) => Self {
                backing_file: Some(backing_file),
                entries: 0,
                write_error: None,
            },
            Err(err) => Self {
                backing_file: None,
                entries: 0,
                write_error: Some(err),
            },
        }
    }

    pub fn init_error(&self) -> Option<&io::Error> {
        self.write_error.as_ref()
    }

    /// Return the number of stored address/byte entries.
    pub fn num_entries(&self) -> usize {
        self.entries
    }

    /// Store a single byte at the given address.
    pub fn store(&mut self, addr: u32, val: u8) {
        if self.write_error.is_some() {
            return;
        }
        let Some(next_entries) = self.entries.checked_add(1) else {
            self.write_error = Some(io::Error::new(
                io::ErrorKind::InvalidData,
                "Image entry count overflow",
            ));
            return;
        };
        let mut buf = [0u8; ENTRY_SIZE];
        buf[..4].copy_from_slice(&addr.to_be_bytes());
        buf[4] = val;
        let Some(backing_file) = self.backing_file.as_mut() else {
            self.write_error = Some(io::Error::other(
                "ImageStore unavailable: no writable temp file",
            ));
            return;
        };
        if let Err(err) = backing_file.as_file_mut().write_all(&buf) {
            self.write_error = Some(err);
            return;
        }
        self.entries = next_entries;
    }

    /// Store a contiguous slice of bytes starting at `addr`.
    pub fn store_slice(&mut self, addr: u32, values: &[u8]) {
        for (ix, val) in values.iter().enumerate() {
            let Ok(offset) = u32::try_from(ix) else {
                self.write_error = Some(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Address overflow while storing image slice",
                ));
                return;
            };
            let Some(next_addr) = addr.checked_add(offset) else {
                self.write_error = Some(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "Address overflow while storing image slice",
                ));
                return;
            };
            self.store(next_addr, *val);
        }
    }

    fn read_entries(&self) -> io::Result<Vec<ImageStoreEntry>> {
        let Some(backing_file) = self.backing_file.as_ref() else {
            return Err(io::Error::other(
                "ImageStore unavailable: no readable temp file",
            ));
        };
        let mut reader = BufReader::new(backing_file.reopen()?);
        let mut entries = Vec::new();
        loop {
            let mut buf = [0u8; ENTRY_SIZE];
            match reader.read_exact(&mut buf) {
                Ok(()) => {
                    let addr = u32::from_be_bytes([buf[0], buf[1], buf[2], buf[3]]);
                    entries.push(ImageStoreEntry {
                        addr,
                        value: buf[4],
                    });
                }
                Err(err) if err.kind() == io::ErrorKind::UnexpectedEof => break,
                Err(err) => return Err(err),
            }
        }
        Ok(entries)
    }

    fn ensure_ready(&self) -> io::Result<()> {
        if let Some(err) = &self.write_error {
            return Err(io::Error::new(err.kind(), err.to_string()));
        }
        if let Some(backing_file) = self.backing_file.as_ref() {
            backing_file.as_file().sync_all()?;
        }
        Ok(())
    }

    /// Write an Intel HEX file. Deduplicates by address (last write wins)
    /// and sorts records by address. Optional `go_addr` emits a start-address record.
    pub fn write_hex_file<W: Write>(&self, mut out: W, go_addr: Option<&str>) -> io::Result<()> {
        self.ensure_ready()?;
        let raw_entries = self.read_entries()?;

        // Deduplicate entries by address (last-write-wins), then sort by address.
        let entries = {
            let mut seen = std::collections::HashMap::<u32, u8>::new();
            for entry in &raw_entries {
                seen.insert(entry.addr, entry.value);
            }
            let mut deduped: Vec<ImageStoreEntry> = seen
                .into_iter()
                .map(|(addr, value)| ImageStoreEntry { addr, value })
                .collect();
            deduped.sort_by_key(|e| e.addr);
            deduped
        };

        let mut current_ela: Option<u16> = None;
        let mut line_addr: u16 = 0;
        let mut line_bytes: u8 = 0;
        let mut checksum: u8 = 0;
        let mut hex_data = String::new();
        const LINE_LIMIT: usize = 32;

        for (ix, entry) in entries.iter().enumerate() {
            let ela = (entry.addr >> 16) as u16;
            if current_ela != Some(ela) {
                if ela != 0 || current_ela.is_some() {
                    write_extended_linear_address_record(&mut out, ela)?;
                }
                current_ela = Some(ela);
                line_bytes = 0;
            }

            let val = entry.value;
            if line_bytes == 0 {
                line_addr = (entry.addr & 0xFFFF) as u16;
                checksum = 0;
                hex_data.clear();
            }
            hex_data.push(hex_digit((val >> 4) & 0x0f));
            hex_data.push(hex_digit(val & 0x0f));
            checksum = checksum.wrapping_add(val);
            line_bytes = line_bytes.wrapping_add(1);

            let should_flush = if (line_bytes as usize) >= LINE_LIMIT {
                true
            } else if let Some(next) = entries.get(ix + 1) {
                let next_ela = (next.addr >> 16) as u16;
                let next_contiguous = entry.addr.checked_add(1) == Some(next.addr);
                next_ela != ela || !next_contiguous
            } else {
                true
            };

            if should_flush {
                checksum = checksum.wrapping_add(line_bytes);
                checksum = checksum.wrapping_add((line_addr >> 8) as u8);
                checksum = checksum.wrapping_add((line_addr & 0xff) as u8);
                checksum = (!checksum).wrapping_add(1);
                writeln!(
                    out,
                    ":{:02X}{:04X}00{}{:02X}",
                    line_bytes, line_addr, hex_data, checksum
                )?;
                line_bytes = 0;
            }
        }

        if let Some(go) = go_addr {
            let addr = match u32::from_str_radix(go, 16) {
                Ok(v) => v,
                Err(_) => {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidInput,
                        "Invalid start address",
                    ))
                }
            };
            if addr <= u16::MAX as u32 {
                let addr16 = addr as u16;
                let mut csum: u8 = 0;
                csum = csum.wrapping_add(4);
                csum = csum.wrapping_add(3);
                csum = csum.wrapping_add((addr16 >> 8) as u8);
                csum = csum.wrapping_add((addr16 & 0xff) as u8);
                csum = (!csum).wrapping_add(1);
                writeln!(out, ":040000030000{:04X}{:02X}", addr16, csum)?;
            } else {
                // Start Linear Address Record (type 05) for 32-bit start addresses.
                let mut csum: u8 = 0;
                csum = csum.wrapping_add(4);
                csum = csum.wrapping_add(5);
                csum = csum.wrapping_add((addr >> 24) as u8);
                csum = csum.wrapping_add((addr >> 16) as u8);
                csum = csum.wrapping_add((addr >> 8) as u8);
                csum = csum.wrapping_add((addr & 0xff) as u8);
                csum = (!csum).wrapping_add(1);
                writeln!(out, ":04000005{:08X}{:02X}", addr, csum)?;
            }
        }

        writeln!(out, ":00000001FF")?;
        Ok(())
    }

    /// Write a Motorola S-record file. Deduplicates by address (last write
    /// wins) and sorts records by address. Optional `go_addr` selects the
    /// termination record start address; otherwise the termination address is 0.
    pub fn write_srec_file<W: Write>(&self, mut out: W, go_addr: Option<&str>) -> io::Result<()> {
        self.ensure_ready()?;
        let raw_entries = self.read_entries()?;

        let entries = {
            let mut seen = std::collections::HashMap::<u32, u8>::new();
            for entry in &raw_entries {
                seen.insert(entry.addr, entry.value);
            }
            let mut deduped: Vec<ImageStoreEntry> = seen
                .into_iter()
                .map(|(addr, value)| ImageStoreEntry { addr, value })
                .collect();
            deduped.sort_by_key(|e| e.addr);
            deduped
        };

        let start_addr = match go_addr {
            Some(go) => u32::from_str_radix(go, 16).map_err(|_| {
                io::Error::new(io::ErrorKind::InvalidInput, "Invalid start address")
            })?,
            None => 0,
        };
        let max_addr = entries
            .iter()
            .map(|entry| entry.addr)
            .max()
            .unwrap_or(0)
            .max(start_addr);
        let address_bytes = if max_addr <= 0xffff {
            2
        } else if max_addr <= 0x00ff_ffff {
            3
        } else {
            4
        };
        let data_record = match address_bytes {
            2 => '1',
            3 => '2',
            _ => '3',
        };
        let termination_record = match address_bytes {
            2 => '9',
            3 => '8',
            _ => '7',
        };

        const LINE_LIMIT: usize = 32;
        let mut line_addr = 0u32;
        let mut line_data = Vec::<u8>::with_capacity(LINE_LIMIT);

        for (ix, entry) in entries.iter().enumerate() {
            if line_data.is_empty() {
                line_addr = entry.addr;
            }
            line_data.push(entry.value);

            let should_flush = if line_data.len() >= LINE_LIMIT {
                true
            } else if let Some(next) = entries.get(ix + 1) {
                entry.addr.checked_add(1) != Some(next.addr)
            } else {
                true
            };

            if should_flush {
                write_srec_record(&mut out, data_record, address_bytes, line_addr, &line_data)?;
                line_data.clear();
            }
        }

        write_srec_record(&mut out, termination_record, address_bytes, start_addr, &[])?;
        Ok(())
    }

    /// Write a raw binary file covering `start..=end`, filling gaps with `fill`.
    pub fn write_bin_file<W: Write>(
        &self,
        mut out: W,
        start_addr: u32,
        end_addr: u32,
        fill: u8,
    ) -> io::Result<()> {
        self.ensure_ready()?;
        let entries = self.read_entries()?;

        if end_addr < start_addr {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "Invalid binary range: end address is less than start address",
            ));
        }
        let size_u64 = end_addr as u64 - start_addr as u64 + 1;
        let alloc_size = usize::try_from(size_u64).map_err(|_| {
            io::Error::new(
                io::ErrorKind::InvalidInput,
                "Binary range is too large for this host",
            )
        })?;
        let mut mem = vec![fill; alloc_size];
        for entry in &entries {
            if entry.addr >= start_addr && entry.addr <= end_addr {
                let offset = (entry.addr - start_addr) as usize;
                mem[offset] = entry.value;
            }
        }

        out.write_all(&mem)?;
        Ok(())
    }

    /// Return the (min, max) address range of emitted bytes, or `None` if empty.
    pub fn output_range(&self) -> io::Result<Option<(u32, u32)>> {
        self.ensure_ready()?;
        let entries = self.read_entries()?;
        let mut iter = entries.iter();
        let Some(first) = iter.next() else {
            return Ok(None);
        };
        let mut min = first.addr;
        let mut max = first.addr;
        for entry in iter {
            min = min.min(entry.addr);
            max = max.max(entry.addr);
        }
        Ok(Some((min, max)))
    }

    /// Return all stored `(address, byte)` pairs.
    pub fn entries(&self) -> io::Result<Vec<(u32, u8)>> {
        self.ensure_ready()?;
        let entries = self.read_entries()?;
        Ok(entries
            .into_iter()
            .map(|entry| (entry.addr, entry.value))
            .collect())
    }
}

impl Default for ImageStore {
    fn default() -> Self {
        Self::new()
    }
}

fn hex_digit(val: u8) -> char {
    match val {
        0..=9 => (b'0' + val) as char,
        _ => (b'A' + (val - 10)) as char,
    }
}

fn write_extended_linear_address_record<W: Write>(out: &mut W, upper: u16) -> io::Result<()> {
    let mut csum: u8 = 0;
    csum = csum.wrapping_add(2); // length
    csum = csum.wrapping_add(4); // record type 04
    csum = csum.wrapping_add((upper >> 8) as u8);
    csum = csum.wrapping_add((upper & 0xff) as u8);
    csum = (!csum).wrapping_add(1);
    writeln!(out, ":02000004{:04X}{:02X}", upper, csum)
}

fn write_srec_record<W: Write>(
    out: &mut W,
    record_type: char,
    address_bytes: usize,
    address: u32,
    data: &[u8],
) -> io::Result<()> {
    debug_assert!((2..=4).contains(&address_bytes));
    let count = u8::try_from(address_bytes + data.len() + 1)
        .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "S-record line is too long"))?;
    let mut sum = count;
    let mut address_text = String::new();
    for shift in (0..address_bytes).rev().map(|idx| idx * 8) {
        let byte = ((address >> shift) & 0xff) as u8;
        sum = sum.wrapping_add(byte);
        address_text.push(hex_digit(byte >> 4));
        address_text.push(hex_digit(byte & 0x0f));
    }
    let mut data_text = String::new();
    for byte in data {
        sum = sum.wrapping_add(*byte);
        data_text.push(hex_digit(byte >> 4));
        data_text.push(hex_digit(byte & 0x0f));
    }
    let checksum = 0xffu8.wrapping_sub(sum);
    writeln!(
        out,
        "S{}{:02X}{}{}{:02X}",
        record_type, count, address_text, data_text, checksum
    )
}

#[cfg(test)]
mod tests {
    use super::run_with_forced_open_failure_for_tests;
    use super::ImageStore;
    use std::io;

    fn parse_hex_byte(s: &str) -> u8 {
        u8::from_str_radix(s, 16).unwrap()
    }

    fn verify_checksum(line: &str) {
        assert!(line.starts_with(':'), "record must start with ':'");
        let bytes = &line[1..];
        let len = parse_hex_byte(&bytes[0..2]) as usize;
        let addr_hi = parse_hex_byte(&bytes[2..4]);
        let addr_lo = parse_hex_byte(&bytes[4..6]);
        let rec_type = parse_hex_byte(&bytes[6..8]);
        let data_start = 8;
        let data_end = data_start + len * 2;
        let checksum = parse_hex_byte(&bytes[data_end..data_end + 2]);

        let mut sum: u8 = 0;
        sum = sum.wrapping_add(len as u8);
        sum = sum.wrapping_add(addr_hi);
        sum = sum.wrapping_add(addr_lo);
        sum = sum.wrapping_add(rec_type);
        for idx in (data_start..data_end).step_by(2) {
            let b = parse_hex_byte(&bytes[idx..idx + 2]);
            sum = sum.wrapping_add(b);
        }
        let expected = (!sum).wrapping_add(1);
        assert_eq!(checksum, expected, "checksum mismatch for {line}");
    }

    fn verify_srec_checksum(line: &str) {
        assert!(line.starts_with('S'), "record must start with S");
        let count = parse_hex_byte(&line[2..4]) as usize;
        let rest = &line[4..];
        assert_eq!(rest.len(), count * 2);
        let mut sum = count as u8;
        for idx in (0..rest.len()).step_by(2) {
            sum = sum.wrapping_add(parse_hex_byte(&rest[idx..idx + 2]));
        }
        assert_eq!(sum, 0xff, "checksum mismatch for {line}");
    }

    #[test]
    fn writes_hex_records_with_valid_checksums() {
        let mut image = ImageStore::new();
        image.store_slice(0x1000, &[0x01, 0x02, 0x03]);
        let mut out = Vec::new();
        image.write_hex_file(&mut out, None).unwrap();
        let text = String::from_utf8(out).unwrap();
        let lines: Vec<&str> = text.lines().collect();
        assert!(lines.len() >= 2);
        for line in &lines {
            verify_checksum(line);
        }
        assert_eq!(lines.last().copied(), Some(":00000001FF"));
    }

    #[test]
    fn includes_start_segment_record_when_requested() {
        let mut image = ImageStore::new();
        image.store_slice(0x0000, &[0xaa]);
        let mut out = Vec::new();
        image.write_hex_file(&mut out, Some("1234")).unwrap();
        let text = String::from_utf8(out).unwrap();
        let mut has_start = false;
        for line in text.lines() {
            if line.starts_with(":04000003") {
                has_start = true;
                verify_checksum(line);
            }
        }
        assert!(has_start);
    }

    #[test]
    fn includes_start_linear_record_for_wide_start_address() {
        let mut image = ImageStore::new();
        image.store_slice(0x123456, &[0xaa]);
        let mut out = Vec::new();
        image.write_hex_file(&mut out, Some("123456")).unwrap();
        let text = String::from_utf8(out).unwrap();
        assert!(text
            .lines()
            .any(|line| line.starts_with(":0400000500123456")));
    }

    #[test]
    fn write_bin_respects_range_and_fill() {
        let mut image = ImageStore::new();
        image.store(0x0010, 0xaa);
        image.store(0x0012, 0xbb);
        let mut out = Vec::new();
        image
            .write_bin_file(&mut out, 0x000f, 0x0013, 0xff)
            .unwrap();
        assert_eq!(out.len(), 5);
        assert_eq!(out, vec![0xff, 0xaa, 0xff, 0xbb, 0xff]);
    }

    #[test]
    fn writes_srec_records_with_valid_checksums() {
        let mut image = ImageStore::new();
        image.store_slice(0x1000, &[0xaa, 0xbb, 0xcc]);
        let mut out = Vec::new();
        image.write_srec_file(&mut out, None).unwrap();
        let text = String::from_utf8(out).unwrap();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["S1061000AABBCCB8", "S9030000FC"]);
        for line in &lines {
            verify_srec_checksum(line);
        }
    }

    #[test]
    fn writes_srec_wide_records_and_start_address() {
        let mut image = ImageStore::new();
        image.store_slice(0x123456, &[0xaa, 0xbb]);
        let mut out = Vec::new();
        image.write_srec_file(&mut out, Some("123456")).unwrap();
        let text = String::from_utf8(out).unwrap();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["S206123456AABBF8", "S8041234565F"]);
        for line in &lines {
            verify_srec_checksum(line);
        }
    }

    #[test]
    fn write_bin_rejects_descending_range() {
        let image = ImageStore::new();
        let mut out = Vec::new();
        let err = image
            .write_bin_file(&mut out, 0x2000, 0x1fff, 0xff)
            .expect_err("descending range should fail");
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
        assert!(err
            .to_string()
            .contains("end address is less than start address"));
    }

    #[test]
    fn write_hex_emits_extended_linear_address_for_wide_addresses() {
        let mut image = ImageStore::new();
        image.store(0x123456, 0xaa);
        image.store(0x123457, 0xbb);
        let mut out = Vec::new();
        image.write_hex_file(&mut out, None).unwrap();
        let text = String::from_utf8(out).unwrap();
        assert!(text.contains(":020000040012"));
        assert!(text.contains(":02345600AABB"));
    }

    #[test]
    fn output_range_supports_wide_addresses() {
        let mut image = ImageStore::new();
        image.store(0x010000, 0xaa);
        let range = image.output_range().expect("range").expect("some range");
        assert_eq!(range, (0x010000, 0x010000));
    }

    #[test]
    fn store_slice_reports_address_overflow() {
        let mut image = ImageStore::new();
        image.store_slice(u32::MAX, &[0xaa, 0xbb]);
        let mut out = Vec::new();
        let err = image
            .write_hex_file(&mut out, None)
            .expect_err("overflow should be reported");
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
        assert!(err
            .to_string()
            .contains("Address overflow while storing image slice"));
    }

    #[test]
    fn store_reports_entry_count_overflow() {
        let mut image = ImageStore::new();
        image.entries = usize::MAX;
        image.store(0x1000, 0xaa);
        let mut out = Vec::new();
        let err = image
            .write_hex_file(&mut out, None)
            .expect_err("overflow should be reported");
        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
        assert!(err.to_string().contains("Image entry count overflow"));
    }

    #[test]
    fn forced_temp_open_failure_surfaces_early() {
        run_with_forced_open_failure_for_tests(|| {
            let image = ImageStore::new();
            let init_error = image.init_error().expect("init error should be present");
            assert_eq!(init_error.kind(), io::ErrorKind::PermissionDenied);
            let mut out = Vec::new();
            let err = image
                .write_hex_file(&mut out, None)
                .expect_err("write should fail when init failed");
            assert_eq!(err.kind(), io::ErrorKind::PermissionDenied);
        });
    }

    #[test]
    fn secure_tempfile_path_does_not_use_legacy_predictable_prefix() {
        let image = ImageStore::new();
        let path = image
            .backing_file
            .as_ref()
            .expect("backing tempfile should exist")
            .path();
        let filename = path
            .file_name()
            .and_then(|value| value.to_str())
            .expect("tempfile name should be utf-8 for this test");

        assert!(
            !filename.starts_with("opForge-image-"),
            "expected secure tempfile creation instead of legacy predictable naming"
        );
    }
}
