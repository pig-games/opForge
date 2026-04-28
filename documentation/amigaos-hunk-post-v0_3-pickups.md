# AmigaOS Hunk Post-v0.3 Pickup Notes

The temporary `v0.3` documentation drafts were promoted into the main AmigaOS
example suite:

- `examples/motorola68000/amigaos/workbench_startup_alert.asm`
- `examples/motorola68000/amigaos/timer_device_benchmark.asm`

Important pickup list after `v0.3`:

- chipset-specific section concepts such as chip/fast memory-oriented section
  kinds or attributes
- hardware include and symbol coverage for custom-chip, CIA, copper, and
  blitter examples
- graphics or asset-heavy examples that combine binary payloads with
  hardware-specific memory, screen, copper, or blitter behavior
- screen, interrupt, and OS-takeover examples such as `InterleavedSimpleScreen`,
  `DoubleBufferingScreen`, `KeyboardAndScreen`, `KeyboardTimerAndScreen`, and
  `RAWImageView`
- broader Hunk relocation support for symbolic code/data references. The v0.3
  matrix currently supports only a narrow set of reloc32 forms; PRVM native
  smoke work exposed remaining gaps for forms such as symbolic immediate
  register loads (`MOVE.L #label,Dn`), some symbolic instruction operands, and
  symbolic `.long` request-frame pointer fields.
