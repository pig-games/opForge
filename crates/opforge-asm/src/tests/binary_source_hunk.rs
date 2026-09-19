//! Allocation accounting for the linked load-file subset emitted by opForge.
//! This reads HUNK_HEADER reservations, not file size, live heap or peak RAM.

#[derive(Debug, Default, PartialEq, Eq)]
pub(super) struct Allocation {
    pub code: u64,
    pub data: u64,
    pub bss: u64,
    pub segments: u32,
}
impl Allocation {
    pub fn total(&self) -> u64 {
        self.code + self.data + self.bss
    }
}

struct Reader<'a> {
    bytes: &'a [u8],
    offset: usize,
}
impl Reader<'_> {
    fn word(&mut self) -> Result<u32, &'static str> {
        let end = self.offset.checked_add(4).ok_or("Hunk offset overflow")?;
        let bytes = self.bytes.get(self.offset..end).ok_or("truncated Hunk")?;
        self.offset = end;
        Ok(u32::from_be_bytes(bytes.try_into().unwrap()))
    }
    fn skip_words(&mut self, words: u32) -> Result<(), &'static str> {
        let count = usize::try_from(words)
            .map_err(|_| "Hunk size overflow")?
            .checked_mul(4)
            .ok_or("Hunk size overflow")?;
        self.offset = self
            .offset
            .checked_add(count)
            .filter(|end| *end <= self.bytes.len())
            .ok_or("truncated Hunk payload")?;
        Ok(())
    }
}

pub(super) fn allocation(bytes: &[u8]) -> Result<Allocation, &'static str> {
    let mut r = Reader { bytes, offset: 0 };
    if r.word()? != 0x3f3 {
        return Err("missing HUNK_HEADER");
    }
    if r.word()? != 0 {
        return Err("resident library names unsupported");
    }
    let count = r.word()?;
    if count == 0 || r.word()? != 0 || r.word()? != count - 1 {
        return Err("invalid load-file segment table");
    }
    if u64::from(count) * 4 > (bytes.len() - r.offset) as u64 {
        return Err("truncated allocation table");
    }
    let mut reservations = Vec::new();
    for _ in 0..count {
        let raw = r.word()?;
        if raw >> 30 == 3 {
            return Err("extended memory flags unsupported");
        }
        reservations.push(u64::from(raw & 0x3fff_ffff) * 4);
    }
    let mut result = Allocation {
        segments: count,
        ..Allocation::default()
    };
    for reserved in reservations {
        let kind = r.word()?;
        if !matches!(kind, 0x3e9..=0x3eb) {
            return Err("unsupported segment kind");
        }
        let payload_words = r.word()?;
        let payload_bytes = u64::from(payload_words) * 4;
        if payload_bytes > reserved {
            return Err("payload exceeds reservation");
        }
        match kind {
            0x3e9 => result.code += reserved,
            0x3ea => result.data += reserved,
            0x3eb => result.bss += reserved,
            _ => unreachable!(),
        }
        if kind != 0x3eb {
            r.skip_words(payload_words)?;
        }
        loop {
            match r.word()? {
                0x3f2 => break,
                0x3ec => loop {
                    let relocations = r.word()?;
                    if relocations == 0 {
                        break;
                    }
                    if r.word()? >= count {
                        return Err("invalid relocation target");
                    }
                    if u64::from(relocations) * 4 > (bytes.len() - r.offset) as u64 {
                        return Err("truncated relocation group");
                    }
                    for _ in 0..relocations {
                        let offset = u64::from(r.word()?);
                        if offset + 4 > payload_bytes {
                            return Err("relocation outside payload");
                        }
                    }
                },
                _ => return Err("unsupported or missing segment terminator"),
            }
        }
    }
    if r.offset != bytes.len() {
        return Err("trailing Hunk bytes");
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    fn image(words: &[u32]) -> Vec<u8> {
        words.iter().flat_map(|word| word.to_be_bytes()).collect()
    }
    fn fixture() -> Vec<u8> {
        image(&[
            0x3f3, 0, 3, 0, 2, 0x40000003, 0x80000002, 5, 0x3e9, 2, 0x3f2, 0x3eb, 0x3ec, 1, 1, 0,
            0, 0x3f2, 0x3ea, 1, 0x3e9, 0x3f2, 0x3eb, 5, 0x3f2,
        ])
    }
    #[test]
    fn counts_header_reservations_not_payload_or_marker_bytes() {
        let result = allocation(&fixture()).unwrap();
        assert_eq!(
            result,
            Allocation {
                code: 12,
                data: 8,
                bss: 20,
                segments: 3
            }
        );
        assert_eq!(result.total(), 40);
    }
    #[test]
    fn rejects_every_truncation_and_trailing_data() {
        let bytes = fixture();
        for length in 0..bytes.len() {
            assert!(allocation(&bytes[..length]).is_err(), "{length}");
        }
        let mut bytes = bytes;
        bytes.extend_from_slice(&[0; 4]);
        assert!(allocation(&bytes).is_err());
    }
    #[test]
    fn rejects_bad_tables_payload_sizes_and_relocations() {
        for (index, value) in [
            (0, 0),
            (2, 0),
            (2, u32::MAX),
            (3, 1),
            (4, 3),
            (5, 0xc0000003),
            (5, 1),
            (8, 0x3ff),
            (14, 3),
            (15, 8),
            (17, 0x3e9),
        ] {
            let mut bytes = fixture();
            bytes[index * 4..index * 4 + 4].copy_from_slice(&value.to_be_bytes());
            assert!(allocation(&bytes).is_err(), "word {index}={value:x}");
        }
    }
}
