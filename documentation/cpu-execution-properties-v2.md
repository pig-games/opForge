# CPU execution properties — CPEX version 2

`CPEX` is an optional `.opasm` chunk carrying package-owned CPU properties used by runtime consumers. It leaves the existing container and `CPUS` record formats unchanged. The registry-to-package builder obtains each canonical CPU's `native_word_size_bytes()`, `max_program_address()`, and `is_little_endian()` from its handler; it does not infer values from CPU names.

All integer fields are little-endian. The payload begins with:

| Field | Encoding | Requirement |
| --- | --- | --- |
| Version | `u16` | `2` |
| Reserved | `u16` | `0` |
| Record count | `u32` | Exactly the number of canonical CPU descriptors |

Each record contains a `u32` byte length, that many UTF-8 bytes naming a canonical CPU, a `u32` word size in bytes, a `u32` inclusive maximum program address, and a `u32` data byte order (`0` little-endian, `1` big-endian; other values are invalid). No terminator or padding follows the identifier. Word size must be nonzero; maximum address zero is valid. The decoder bounds counts and lengths before allocating or reading, rejects invalid UTF-8 and trailing bytes, and requires one record per canonical CPU. Keys compare with ASCII case folding; duplicate normalized keys, alias-owned records, dangling keys and missing canonical records are invalid. The native loader additionally retains its existing bounded package/CPU-count capacities.

Aliases in `CALS` or legacy `CPUS` records must target a canonical CPU directly before property lookup. A package with `CPEX` rejects inline alias chains and dangling alias targets. A present but malformed or incomplete chunk is an error. An absent optional chunk supplies no property values. Version 1 is rejected: before 1.0, only the latest contract is supported and packages must be regenerated together with their consumers.

The Rust runtime model exposes `cpu_execution_properties(cpu_id)`. It returns the canonical record for a CPU or alias, `None` for a valid package without the chunk, and a resolution error for an invalid CPU selection. It does not substitute compiled registry defaults for missing package metadata.

Native package loading validates the chunk once. CPU selection stages the canonical word size, maximum address and data byte order, then makes them available together with the selected identity. A reset or package reload clears property availability. A failed commit cannot expose a mixture of a changed CPU identity and an old property record. The runtime-context accessors `getCpuWordSizeBytesV1`, `getCpuMaxProgramAddressV1`, and `getCpuDataByteOrderV1` return D0=0 and D1=value on success; unavailable properties return D0=1 and D1=0. The presence state distinguishes an unavailable maximum from a valid maximum of zero.

This transport does not itself change reservation arithmetic or listing rendering. Consumers must explicitly request and use the properties. Shared native numeric emission requires the active byte-order property and fails when it is absent; it never selects a CPU/family instruction parser or encoder. Rust shared emission uses the package property when present, and otherwise retains registry-backed behavior, including packages without the optional chunk. Selection caches the three values; ordinary VM opcode execution gains no property lookup or CPU-name dispatch.
