// SPDX-License-Identifier: GPL-3.0-or-later

use std::path::Path;

#[must_use]
pub fn stable_path_text(path: &str) -> String {
    let normalized = path.replace('\\', "/");
    if let Some(stripped) = normalized.strip_prefix("//?/UNC/") {
        format!("//{stripped}")
    } else if let Some(stripped) = normalized.strip_prefix("//?/") {
        stripped.to_string()
    } else {
        normalized
    }
}

#[must_use]
pub fn stable_path_string(path: &Path) -> String {
    stable_path_text(path.to_string_lossy().as_ref())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stable_path_text_keeps_unix_paths_unchanged() {
        assert_eq!(
            stable_path_text("/virtual/out/main.lst"),
            "/virtual/out/main.lst"
        );
    }

    #[test]
    fn stable_path_text_normalizes_windows_drive_paths() {
        assert_eq!(
            stable_path_text(r"\\?\C:\temp\build\main.lst"),
            "C:/temp/build/main.lst"
        );
        assert_eq!(
            stable_path_text(r"C:\temp\build\main.lst"),
            "C:/temp/build/main.lst"
        );
    }

    #[test]
    fn stable_path_text_normalizes_windows_unc_paths() {
        assert_eq!(
            stable_path_text(r"\\?\UNC\server\share\file.asm"),
            "//server/share/file.asm"
        );
        assert_eq!(
            stable_path_text(r"\\server\share\file.asm"),
            "//server/share/file.asm"
        );
    }
}
