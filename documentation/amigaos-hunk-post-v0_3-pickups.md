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
