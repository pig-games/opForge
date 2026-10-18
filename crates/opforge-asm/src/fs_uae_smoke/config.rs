// SPDX-License-Identifier: GPL-3.0-or-later
//! Guest memory profiles and deterministic FS-UAE template rewriting.
use std::collections::BTreeSet;

const FS_UAE_MEMORY_PROFILE_ENV: &str = "OPFORGE_FS_UAE_MEMORY_PROFILE";
const FS_UAE_NATIVE_ZORRO_III_MEMORY_KIB: u32 = 65_536;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) enum FsUaeMemoryProfile {
    Existing,
    Constrained2MiB,
    Budget8MiB68020,
    Expanded68020,
    ExpandedDiagnostic68020,
}

pub(super) fn fs_uae_memory_profile_from_env() -> Result<FsUaeMemoryProfile, String> {
    match std::env::var(FS_UAE_MEMORY_PROFILE_ENV) {
        Err(std::env::VarError::NotPresent) => Ok(FsUaeMemoryProfile::Existing),
        Ok(value) if value.trim().is_empty() || value == "existing" => {
            Ok(FsUaeMemoryProfile::Existing)
        }
        Ok(value) if value == "2m" => Ok(FsUaeMemoryProfile::Constrained2MiB),
        Ok(value) if value == "68020-8m" => Ok(FsUaeMemoryProfile::Budget8MiB68020),
        Ok(value) if value == "68020-10m" => Ok(FsUaeMemoryProfile::Expanded68020),
        Ok(value) if value == "68020-74m" => Ok(FsUaeMemoryProfile::ExpandedDiagnostic68020),
        Ok(value) => Err(format!(
            "unsupported {FS_UAE_MEMORY_PROFILE_ENV} value '{value}'; expected 'existing', '2m', '68020-8m', '68020-10m' or '68020-74m'"
        )),
        Err(error) => Err(format!("read {FS_UAE_MEMORY_PROFILE_ENV}: {error}")),
    }
}

pub(super) fn validate_budget_launch(
    profile: FsUaeMemoryProfile,
    args_text: &str,
    template_path: Option<&str>,
) -> Result<(), String> {
    if profile != FsUaeMemoryProfile::Budget8MiB68020 {
        return Ok(());
    }
    if template_path.is_none_or(|path| path.trim().is_empty()) {
        return Err(
            "68020-8m requires OPFORGE_FS_UAE_CONFIG_TEMPLATE to enforce the RAM budget".into(),
        );
    }
    if args_text.trim() != "{fsuae_config}" {
        return Err("68020-8m requires OPFORGE_FS_UAE_ARGS={fsuae_config}; extra arguments can override the RAM budget".into());
    }
    Ok(())
}

const CPU_68020_FS_UAE_SETTINGS: &[(&str, &str)] = &[
    ("cpu", "68020"),
    ("chip_memory", "2048"),
    ("slow_memory", "0"),
    ("fast_memory", "0"),
    ("motherboard_ram", "0"),
    ("zorro_iii_memory", "0"),
    ("graphics_card", "none"),
    ("graphics_memory", "0"),
    ("graphics_card_memory", "0"),
];

// FS-UAE 3.1.66 requires power-of-two Zorro II banks. Six MiB must be
// represented as 4 + 2 MiB, not fast_memory = 6144 (which UAE resets to zero).
// Pin raw UAE sizes too: custom template options are applied after normal options.
const BUDGET_8M_FS_UAE_SETTINGS: &[(&str, &str)] = &[
    ("cpu_speed", "max"),
    ("accuracy", "0"),
    ("uae_cpu_speed", "max"),
    ("uae_cpu_cycle_exact", "false"),
    ("uae_cpu_memory_cycle_exact", "false"),
    ("uae_blitter_cycle_exact", "false"),
    ("uae_cycle_exact", "false"),
    ("fast_memory", "4096"),
    ("accelerator", "none"),
    ("accelerator_memory", "0"),
    ("uae_cpu_model", "68020"),
    ("uae_cpuboard_type", "none"),
    ("uae_cpuboardmem1_size", "0"),
    ("uae_cpuboardmem2_size", "0"),
    ("uae_chipmem_size", "4"),
    ("uae_fastmem_size", "4"),
    ("uae_fastmem_size_k", "4096"),
    ("uae_fastmem2_size", "2"),
    ("uae_fastmem2_size_k", "2048"),
    ("uae_bogomem_size", "0"),
    ("uae_a3000mem_size", "0"),
    ("uae_mbresmem_size", "0"),
    ("uae_mem25bit_size", "0"),
    ("uae_z3mem_size", "0"),
    ("uae_z3mem2_size", "0"),
    ("uae_megachipmem_size", "0"),
    ("uae_gfxcard_size", "0"),
];

fn cpu_68020_fs_uae_setting(
    key: Option<&str>,
    memory_profile: FsUaeMemoryProfile,
) -> Option<(&'static str, &'static str)> {
    let key = key?;
    if memory_profile == FsUaeMemoryProfile::Budget8MiB68020 {
        if let Some(setting) = BUDGET_8M_FS_UAE_SETTINGS
            .iter()
            .find(|(candidate, _)| *candidate == key)
        {
            return Some(*setting);
        }
    }
    CPU_68020_FS_UAE_SETTINGS
        .iter()
        .copied()
        .find(|(candidate, _)| *candidate == key)
        .map(|(key, value)| {
            if key == "fast_memory"
                && matches!(
                    memory_profile,
                    FsUaeMemoryProfile::Expanded68020 | FsUaeMemoryProfile::ExpandedDiagnostic68020
                )
            {
                (key, "8192")
            } else if key == "zorro_iii_memory"
                && memory_profile == FsUaeMemoryProfile::ExpandedDiagnostic68020
            {
                (key, "65536")
            } else {
                (key, value)
            }
        })
}

pub(super) fn rewrite_fs_uae_config_work_mount(
    template_text: &str,
    work_mount_path: &str,
    memory_profile: FsUaeMemoryProfile,
) -> String {
    let mut lines = Vec::new();
    let mut replaced_work_mount = false;
    let mut replaced_zorro_memory = false;
    let mut profile_keys = BTreeSet::new();
    for line in template_text.lines() {
        let trimmed = line.trim_start();
        if trimmed.starts_with("hard_drive_1") {
            lines.push(format!("hard_drive_1 = {work_mount_path}"));
            replaced_work_mount = true;
        } else if memory_profile == FsUaeMemoryProfile::Existing
            && trimmed.starts_with("zorro_iii_memory")
        {
            lines.push(format!(
                "zorro_iii_memory = {FS_UAE_NATIVE_ZORRO_III_MEMORY_KIB}"
            ));
            replaced_zorro_memory = true;
        } else if memory_profile != FsUaeMemoryProfile::Existing {
            let key = trimmed.split_once('=').map(|(key, _)| key.trim());
            // Presets can reset the pinned CPU/RAM; zero-sized addmem is a no-op.
            if memory_profile == FsUaeMemoryProfile::Budget8MiB68020
                && matches!(
                    key,
                    Some("uae_quickstart" | "uae_cpu_type" | "uae_addmem1" | "uae_addmem2")
                )
            {
                continue;
            }
            if let Some((key, value)) = cpu_68020_fs_uae_setting(key, memory_profile) {
                lines.push(format!("{key} = {value}"));
                profile_keys.insert(key);
            } else {
                lines.push(line.to_string());
            }
        } else {
            lines.push(line.to_string());
        }
    }
    if !replaced_work_mount {
        lines.push(format!("hard_drive_1 = {work_mount_path}"));
    }
    if memory_profile == FsUaeMemoryProfile::Existing && !replaced_zorro_memory {
        lines.push(format!(
            "zorro_iii_memory = {FS_UAE_NATIVE_ZORRO_III_MEMORY_KIB}"
        ));
    }
    if memory_profile != FsUaeMemoryProfile::Existing {
        let extra_settings = if memory_profile == FsUaeMemoryProfile::Budget8MiB68020 {
            BUDGET_8M_FS_UAE_SETTINGS
        } else {
            &[]
        };
        for (key, _) in CPU_68020_FS_UAE_SETTINGS.iter().chain(extra_settings) {
            if !profile_keys.contains(key) {
                let (_, value) = cpu_68020_fs_uae_setting(Some(*key), memory_profile).unwrap();
                lines.push(format!("{key} = {value}"));
                profile_keys.insert(key);
            }
        }
    }
    let mut rewritten = lines.join("\n");
    rewritten.push('\n');
    rewritten
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn budget_launch_requires_template_and_canonical_arguments() {
        let profile = FsUaeMemoryProfile::Budget8MiB68020;
        assert!(validate_budget_launch(profile, "{fsuae_config}", Some("/template")).is_ok());
        assert!(validate_budget_launch(profile, " \n{fsuae_config}\n ", Some("/template")).is_ok());
        for template in [None, Some(""), Some("  ")] {
            assert!(validate_budget_launch(profile, "{fsuae_config}", template).is_err());
        }
        for args in [
            "",
            "/other/config",
            "{fsuae_config}\n--fast-memory=8192",
            "--fast-memory=8192\n{fsuae_config}",
        ] {
            assert!(validate_budget_launch(profile, args, Some("/template")).is_err());
        }
    }

    #[test]
    fn budget_launch_leaves_other_profiles_unrestricted() {
        for profile in [
            FsUaeMemoryProfile::Existing,
            FsUaeMemoryProfile::Constrained2MiB,
            FsUaeMemoryProfile::Expanded68020,
            FsUaeMemoryProfile::ExpandedDiagnostic68020,
        ] {
            assert!(
                validate_budget_launch(profile, "/custom/config\n--custom-option", None).is_ok()
            );
        }
    }

    #[test]
    fn rewrite_fs_uae_config_work_mount_replaces_hard_drive_1() {
        let template = "[fs-uae]\nhard_drive_0 = /sys\nhard_drive_1 = /old/work\nzorro_iii_memory = 16384\nsave_disk = 0\n";
        let rewritten =
            rewrite_fs_uae_config_work_mount(template, "/new/work", FsUaeMemoryProfile::Existing);

        assert!(rewritten.contains("hard_drive_0 = /sys"));
        assert!(rewritten.contains("hard_drive_1 = /new/work"));
        assert!(!rewritten.contains("hard_drive_1 = /old/work"));
        assert!(rewritten.contains("zorro_iii_memory = 65536"));
        assert!(!rewritten.contains("zorro_iii_memory = 16384"));
    }

    #[test]
    fn rewrite_fs_uae_config_work_mount_appends_missing_hard_drive_1() {
        let template = "[fs-uae]\nhard_drive_0 = /sys\n";
        let rewritten =
            rewrite_fs_uae_config_work_mount(template, "/new/work", FsUaeMemoryProfile::Existing);

        assert!(rewritten.contains("hard_drive_0 = /sys"));
        assert!(rewritten.contains("hard_drive_1 = /new/work"));
        assert!(rewritten.contains("zorro_iii_memory = 65536"));
    }

    #[test]
    fn rewrite_fs_uae_config_work_mount_constrains_total_guest_ram_to_2m() {
        let template = "[fs-uae]\namiga_model = A4000\ncpu = 68040\nchip_memory = 1024\nfast_memory = 8192\nhard_drive_0 = /sys\nhard_drive_1 = /old/work\ngraphics_card = uaegfx-z3\ngraphics_memory = 16384\nzorro_iii_memory = 65536\nsave_disk = 0\n";
        let rewritten = rewrite_fs_uae_config_work_mount(
            template,
            "/new/work",
            FsUaeMemoryProfile::Constrained2MiB,
        );

        for required in [
            "amiga_model = A4000",
            "cpu = 68020",
            "chip_memory = 2048",
            "slow_memory = 0",
            "fast_memory = 0",
            "motherboard_ram = 0",
            "zorro_iii_memory = 0",
            "graphics_card = none",
            "graphics_memory = 0",
            "graphics_card_memory = 0",
            "hard_drive_1 = /new/work",
        ] {
            assert!(
                rewritten.contains(required),
                "missing {required}:\n{rewritten}"
            );
        }
        for removed in ["68040", "8192", "uaegfx-z3", "16384", "65536"] {
            assert!(
                !rewritten.contains(removed),
                "retained {removed}:\n{rewritten}"
            );
        }
    }

    #[test]
    fn rewrite_fs_uae_config_work_mount_sets_exact_8m_budget() {
        let template = "[fs-uae]\namiga_model = A4000\nkickstart_file = /rom\nhard_drive_0 = /sys\nfast_memory = 8192\nuae_fastmem2_size = 8\nuae_z3mem_size = 64\ngraphics_memory = 16384\nuae_cpu_type = 68040\nuae_addmem1 = 0x10000000,0x800000\nuae_addmem2 = 0x20000000,0x800000\nuae_quickstart = A4000,1\ncpu_speed = real\naccuracy = 1\nuae_cpu_speed = real\nuae_cpu_cycle_exact = true\nuae_cpu_memory_cycle_exact = true\nuae_blitter_cycle_exact = true\nuae_cycle_exact = true\n";
        let rewritten = rewrite_fs_uae_config_work_mount(
            template,
            "/new/work",
            FsUaeMemoryProfile::Budget8MiB68020,
        );
        let settings: std::collections::BTreeMap<_, _> = rewritten
            .lines()
            .filter_map(|line| line.split_once('='))
            .map(|(key, value)| (key.trim(), value.trim()))
            .collect();
        for (key, _) in CPU_68020_FS_UAE_SETTINGS
            .iter()
            .chain(BUDGET_8M_FS_UAE_SETTINGS)
        {
            let (_, expected) =
                cpu_68020_fs_uae_setting(Some(key), FsUaeMemoryProfile::Budget8MiB68020).unwrap();
            assert_eq!(settings.get(key), Some(&expected), "{key}");
            assert_eq!(
                rewritten
                    .lines()
                    .filter(|line| line
                        .split_once('=')
                        .is_some_and(|(candidate, _)| candidate.trim() == *key))
                    .count(),
                1,
                "duplicate {key}"
            );
        }
        assert_eq!(settings["amiga_model"], "A4000");
        assert_eq!(settings["kickstart_file"], "/rom");
        assert_eq!(settings["hard_drive_0"], "/sys");
        assert_eq!(settings["hard_drive_1"], "/new/work");
        for removed in [
            "uae_quickstart",
            "uae_cpu_type",
            "uae_addmem1",
            "uae_addmem2",
        ] {
            assert!(!settings.contains_key(removed), "retained {removed}");
        }
        let chip: u32 = settings["chip_memory"].parse().unwrap();
        let fast: u32 = settings["fast_memory"].parse().unwrap();
        let second_fast: u32 = settings["uae_fastmem2_size_k"].parse().unwrap();
        assert_eq!(chip + fast + second_fast, 8192);
    }

    #[test]
    fn rewrite_fs_uae_config_work_mount_expands_ram_but_keeps_68020() {
        let template = "[fs-uae]\namiga_model = A4000\ncpu = 68040\nchip_memory = 1024\nfast_memory = 4096\nhard_drive_1 = /old/work\ngraphics_card = uaegfx-z3\ngraphics_memory = 16384\nzorro_iii_memory = 65536\n";
        let rewritten = rewrite_fs_uae_config_work_mount(
            template,
            "/new/work",
            FsUaeMemoryProfile::Expanded68020,
        );

        for required in [
            "cpu = 68020",
            "chip_memory = 2048",
            "fast_memory = 8192",
            "slow_memory = 0",
            "motherboard_ram = 0",
            "zorro_iii_memory = 0",
            "graphics_card = none",
            "graphics_memory = 0",
            "graphics_card_memory = 0",
            "hard_drive_1 = /new/work",
        ] {
            assert!(
                rewritten.contains(required),
                "missing {required}:\n{rewritten}"
            );
        }
    }

    #[test]
    fn rewrite_fs_uae_config_work_mount_sets_expanded_diagnostic_68020_profile() {
        let template = "[fs-uae]\namiga_model = A4000\ncpu = 68040\nchip_memory = 1024\nfast_memory = 4096\nhard_drive_1 = /old/work\ngraphics_card = uaegfx-z3\ngraphics_memory = 16384\nzorro_iii_memory = 0\n";
        let rewritten = rewrite_fs_uae_config_work_mount(
            template,
            "/new/work",
            FsUaeMemoryProfile::ExpandedDiagnostic68020,
        );

        for required in [
            "cpu = 68020",
            "chip_memory = 2048",
            "fast_memory = 8192",
            "slow_memory = 0",
            "motherboard_ram = 0",
            "zorro_iii_memory = 65536",
            "graphics_card = none",
            "graphics_memory = 0",
            "graphics_card_memory = 0",
            "hard_drive_1 = /new/work",
        ] {
            assert!(
                rewritten.contains(required),
                "missing {required}:\n{rewritten}"
            );
        }
        for removed in [
            "68040",
            "4096",
            "uaegfx-z3",
            "16384",
            "zorro_iii_memory = 0",
        ] {
            assert!(
                !rewritten.contains(removed),
                "retained {removed}:\n{rewritten}"
            );
        }
    }
}
