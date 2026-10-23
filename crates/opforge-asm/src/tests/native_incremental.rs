//! Fresh cold/warm/rebuild proof with live Rust source-tree oracles.
use super::native_iterable_loops::{build_image, oracle_with_module_roots, scratch};
use super::*;

// Same section/relocation shape as the maintained private-module control.
const ROOT: &str = ".module main\n.cpu 68020\n.use independent\n.use middle\n.section code,kind=code\n.long middle.entry\n.endsection\n.section data,kind=data\n.byte middle.VALUE,independent.VALUE\n.endsection\n.region ram,0,$ffff\n.place code in ram\n.place data in ram\n.output \"build/module.bin\",format=bin,sections=code,data\n.output \"build/module.hunk\",format=hunk,sections=code,data\n.endmodule\n";
const INDEPENDENT: &str = ".module independent\n.cpu 68020\n.pub\nVALUE .const 3\n.endmodule\n";
const MIDDLE: &str = ".module middle\n.cpu 68020\n.use leaf\n.pub\nVALUE .const leaf.FLAG+1\n.section code,kind=code\nentry .block\n lea leaf.payload.l,a0\n.long leaf.payload\n.word leaf.FLAG\n rts\n.bend\n.endsection\n.endmodule\n";
const LEAF: &str = ".module leaf\n.cpu 68020\n.pub\nFLAG .const 6\n.section data,kind=data\npayload .block\n.long $12345678\n.bend\n.endsection\n.endmodule\n";
// A different extent guarantees a different file stamp even on a coarse clock.
const CHANGED: &str = ".module leaf\n.cpu 68020\n.pub\nFLAG .const 9\n.section data,kind=data\npayload .block\n.long $12345678,$90abcdef\n.bend\n.endsection\n.endmodule\n";

fn oracle(leaf: &str) -> Vec<u8> {
    let dir = scratch();
    fs::create_dir_all(dir.0.join("library")).unwrap();
    fs::write(dir.0.join("library/middle.asm"), MIDDLE).unwrap();
    fs::write(dir.0.join("library/leaf.asm"), leaf).unwrap();
    fs::write(dir.0.join("library/independent.asm"), INDEPENDENT).unwrap();
    oracle_with_module_roots(&dir.0, ROOT, &["library"]).expect("live incremental Rust oracle")
}

pub(super) fn counters(text: &str) -> (u32, u32) {
    let line = text
        .lines()
        .find(|line| line.starts_with("incremental: reused=0x"))
        .unwrap_or_else(|| panic!("missing incremental counters in {text:?}"));
    let (reused, rebuilt) = line
        .trim()
        .strip_prefix("incremental: reused=0x")
        .unwrap()
        .split_once(" rebuilt=0x")
        .expect("incremental counter grammar");
    (
        u32::from_str_radix(reused, 16).unwrap(),
        u32::from_str_radix(rebuilt, 16).unwrap(),
    )
}

#[test]
fn incremental_fixture_live_rust_oracles() {
    let original = oracle(LEAF);
    let changed = oracle(CHANGED);
    assert!(!original.is_empty());
    assert_ne!(original, changed);
}

#[test]
#[ignore = "requires fresh FS-UAE compact CLI cold/warm/rebuild completion"]
fn native_incremental_cold_warm_rebuild() {
    run(Action::Rebuild);
}

#[test]
#[ignore = "requires fresh FS-UAE compact CLI dependency timestamp invalidation"]
fn native_incremental_changed_dependency() {
    run(Action::Change);
}

#[test]
#[ignore = "requires fresh FS-UAE compact CLI timestamp-only invalidation"]
fn native_incremental_timestamp_only_dependency() {
    run(Action::Touch);
}

#[derive(Clone, Copy)]
enum Action {
    Rebuild,
    Change,
    Touch,
}

fn run(action: Action) {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..");
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_INCREMENTAL_IMAGE")
        .map(|path| fs::read(path).expect("current incremental compact CLI image"))
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let original = oracle(LEAF);
    let final_bytes = if matches!(action, Action::Change) {
        oracle(CHANGED)
    } else {
        original.clone()
    };
    let final_action = match action {
        Action::Change => "C:Copy Work:changed.txt Work:library/leaf.asm\nWork:build/opforge Work:entry.asm -M Work:library --incremental-dir Work:cache --capture-dir Work:scratch",
        Action::Rebuild => "Work:build/opforge Work:entry.asm -M Work:library --incremental-dir Work:cache --capture-dir Work:scratch --rebuild",
        // Change only the timestamp; the bytes and extent remain identical.
        Action::Touch => "C:SetDate Work:library/leaf.asm 1-Jan-2000 00:00:01\nEcho $RC >Work:touch.rc\nWork:build/opforge Work:entry.asm -M Work:library --incremental-dir Work:cache --capture-dir Work:scratch",
    };
    // The runner wraps this entire sequence with its fresh start/done challenge.
    // Capture earlier exit codes explicitly so a later success cannot hide them.
    let quiet = if matches!(action, Action::Rebuild) {
        "Work:build/opforge Work:entry.asm -M Work:library --incremental-dir Work:cache --capture-dir Work:scratch --quiet >Work:quiet.txt *>Work:quiet.err\nEcho $RC >Work:quiet.rc\nC:Copy Work:build/module.bin Work:quiet.bin\nWork:build/opforge Work:entry.asm -M Work:library --incremental-dir Work:cache --capture-dir Work:scratch -q >Work:short-quiet.txt *>Work:short-quiet.err\nEcho $RC >Work:short-quiet.rc\nWork:build/opforge --quiet=1 >Work:quiet-invalid.txt\nEcho $RC >Work:quiet-invalid.rc\nWork:build/opforge Work:entry.asm -M Work:library --reference-modules --capture-dir Work:scratch >Work:reference.txt *>Work:reference.progress\nEcho $RC >Work:reference.rc\nC:Copy Work:build/module.bin Work:reference.bin\n"
    } else {
        ""
    };
    // Keep the snapshots outside the live store. List records the guest dates;
    // a two-second gap makes an accidental rewrite observable on coarse clocks.
    let snapshot = |phase: &str| {
        format!(
            "C:MakeDir Work:{phase}-cache\nC:Copy Work:cache Work:{phase}-cache ALL QUIET\nEcho $RC >Work:{phase}-copy.rc\nC:List Work:cache/#?.ofm LFORMAT \"%N|%D|%T\" >Work:{phase}-stamps.txt\nEcho $RC >Work:{phase}-stamps.rc\n"
        )
    };
    let cold_snapshot = snapshot("cold");
    let warm_snapshot = snapshot("warm");
    let final_snapshot = snapshot("final");
    let command = format!(
        "Work:entry.asm -M Work:library --incremental-dir Work:cache --capture-dir Work:scratch --rebuild >Work:cold.txt *>Work:cold.progress\nEcho $RC >Work:cold.rc\nC:Copy Work:build/module.bin Work:cold.bin\n{cold_snapshot}C:Wait 2 SECS\nEcho $RC >Work:cold-wait.rc\nWork:build/opforge Work:entry.asm -M Work:library --incremental-dir Work:cache --capture-dir Work:scratch >Work:warm.txt *>Work:warm.progress\nEcho $RC >Work:warm.rc\nC:Copy Work:build/module.bin Work:warm.bin\n{warm_snapshot}{quiet}C:Wait 2 SECS\nEcho $RC >Work:warm-wait.rc\n{final_action} >Work:final.txt\nEcho $RC >Work:final.rc\n{final_snapshot}C:Type Work:final.txt"
    );
    let files = [
        OpforgeNativeCliGuestFile {
            relative_path: "scratch/.keep",
            bytes: b"",
        },
        OpforgeNativeCliGuestFile {
            relative_path: "entry.asm",
            bytes: ROOT.as_bytes(),
        },
        OpforgeNativeCliGuestFile {
            relative_path: "library/middle.asm",
            bytes: MIDDLE.as_bytes(),
        },
        OpforgeNativeCliGuestFile {
            relative_path: "library/leaf.asm",
            bytes: LEAF.as_bytes(),
        },
        OpforgeNativeCliGuestFile {
            relative_path: "library/independent.asm",
            bytes: INDEPENDENT.as_bytes(),
        },
        OpforgeNativeCliGuestFile {
            relative_path: "changed.txt",
            bytes: CHANGED.as_bytes(),
        },
    ];
    let mut artifacts = vec![
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/cold.bin",
            rust_oracle: &original,
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/warm.bin",
            rust_oracle: &original,
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/build/module.bin",
            rust_oracle: &final_bytes,
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/cold.rc",
            rust_oracle: b"0\n",
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/warm.rc",
            rust_oracle: b"0\n",
        },
        OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/final.rc",
            rust_oracle: b"0\n",
        },
    ];
    for relative_path in [
        "Work/cold-wait.rc",
        "Work/warm-wait.rc",
        "Work/cold-copy.rc",
        "Work/warm-copy.rc",
        "Work/final-copy.rc",
        "Work/cold-stamps.rc",
        "Work/warm-stamps.rc",
        "Work/final-stamps.rc",
    ] {
        artifacts.push(OpforgeNativeCliExpectedArtifact {
            relative_path,
            rust_oracle: b"0\n",
        });
    }
    if matches!(action, Action::Touch) {
        artifacts.push(OpforgeNativeCliExpectedArtifact {
            relative_path: "Work/touch.rc",
            rust_oracle: b"0\n",
        });
    }
    if matches!(action, Action::Rebuild) {
        artifacts.extend([
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/quiet.bin",
                rust_oracle: &original,
            },
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/quiet.rc",
                rust_oracle: b"0\n",
            },
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/short-quiet.rc",
                rust_oracle: b"0\n",
            },
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/reference.bin",
                rust_oracle: &original,
            },
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/reference.rc",
                rust_oracle: b"0\n",
            },
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/quiet-invalid.rc",
                rust_oracle: b"20\n",
            },
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/quiet.txt",
                rust_oracle: b"",
            },
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/quiet.err",
                rust_oracle: b"",
            },
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/short-quiet.txt",
                rust_oracle: b"",
            },
            OpforgeNativeCliExpectedArtifact {
                relative_path: "Work/short-quiet.err",
                rust_oracle: b"",
            },
        ]);
    }
    let case = OpforgeNativeCliParityCase {
        name: match action {
            Action::Change => "incremental-changed-dependency",
            Action::Rebuild => "incremental-cold-warm-rebuild",
            Action::Touch => "incremental-timestamp-only-dependency",
        },
        cpu_override: "68020",
        extra_assembly_defines: &[],
        source_override: Some(ROOT.as_bytes()),
        command_template: Some(&command),
        package_mode: OpforgeNativeCliPackageMode::EmbeddedDefault,
        extra_guest_files: &files,
        proof: OpforgeNativeCliProof::ExactArtifacts(&artifacts),
    };
    let outcome = run_prebuilt_compact_cli_case_from_env(&root, &case, &image)
        .expect("fresh incremental CLI completion");
    let FsUaeSmokeOutcome::Completed { runs } = outcome else {
        panic!("real native execution required")
    };
    assert_eq!(runs.len(), 1);
    let run = &runs[0];
    assert!(run.protocol_completed);
    assert!(run.success);
    assert_eq!(run.exit_code, Some(0));
    let log = |path: &str| {
        String::from_utf8(
            run.captured_artifacts
                .get(&PathBuf::from(path))
                .unwrap_or_else(|| panic!("missing {path}"))
                .clone(),
        )
        .unwrap()
    };
    let cache_files = |directory: &str| {
        let prefix = PathBuf::from(format!("Work/{directory}"));
        run.captured_artifacts
            .iter()
            .filter_map(|(path, bytes)| {
                let name = path.strip_prefix(&prefix).ok()?;
                (name.components().count() == 1
                    && name
                        .extension()
                        .is_some_and(|extension| extension == "ofm" || extension == "ofi"))
                .then_some((name.to_path_buf(), bytes))
            })
            .collect::<std::collections::BTreeMap<_, _>>()
    };
    let cold_files = cache_files("cold-cache");
    let warm_files = cache_files("warm-cache");
    let final_files = cache_files("final-cache");
    for files in [&cold_files, &warm_files, &final_files] {
        let stems = |extension| {
            files
                .keys()
                .filter(|name| name.extension().is_some_and(|value| value == extension))
                .map(|name| name.file_stem().unwrap().to_os_string())
                .collect::<std::collections::BTreeSet<_>>()
        };
        assert_eq!(files.len(), 6, "one image and marker per cached module");
        assert_eq!(stems("ofm"), stems("ofi"));
    }
    assert_eq!(
        cold_files, warm_files,
        "warm reuse must preserve image and marker bytes"
    );
    assert_eq!(
        final_files,
        cache_files("cache"),
        "snapshots describe the live shared store"
    );
    assert_eq!(
        cold_files.keys().collect::<Vec<_>>(),
        final_files.keys().collect::<Vec<_>>()
    );
    assert!(
        run.captured_artifacts.keys().all(|path| {
            path.extension().is_none_or(|extension| extension != "tmp")
                && path.extension().is_none_or(|extension| {
                    (extension != "ofm" && extension != "ofi")
                        || [
                            "Work/cache",
                            "Work/cold-cache",
                            "Work/warm-cache",
                            "Work/final-cache",
                        ]
                        .iter()
                        .any(|directory| path.parent() == Some(Path::new(directory)))
                })
                && !path.components().any(|component| {
                    component
                        .as_os_str()
                        .to_str()
                        .is_some_and(|name| name == "objects" || name.starts_with("objects-"))
                })
        }),
        "assembly must use the shared cache without numbered object directories or temporary files"
    );
    let stamps = |phase: &str| {
        log(&format!("Work/{phase}-stamps.txt"))
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| {
                let (name, date) = line
                    .trim()
                    .split_once('|')
                    .expect("List filename/date format");
                assert!(name.ends_with(".ofm") && date.contains('|'), "{line}");
                (name.to_owned(), date.to_owned())
            })
            .collect::<std::collections::BTreeMap<_, _>>()
    };
    let cold_stamps = stamps("cold");
    let warm_stamps = stamps("warm");
    let final_stamps = stamps("final");
    assert_eq!(cold_stamps.len(), 3);
    assert_eq!(
        cold_stamps, warm_stamps,
        "warm reuse must leave file dates untouched"
    );
    assert_eq!(
        cold_stamps.keys().collect::<Vec<_>>(),
        final_stamps.keys().collect::<Vec<_>>()
    );
    for (name, bytes) in &warm_files {
        if name.to_string_lossy().starts_with("independent-") && !matches!(action, Action::Rebuild)
        {
            assert_eq!(
                *bytes, final_files[name],
                "independent module must remain untouched"
            );
        }
        if matches!(action, Action::Change)
            && name.extension().is_some_and(|extension| extension == "ofm")
            && !name.to_string_lossy().starts_with("independent-")
        {
            assert_ne!(
                *bytes, final_files[name],
                "changed dependency closure must replace its image"
            );
        }
    }
    for (name, stamp) in &warm_stamps {
        if name.starts_with("independent-") && !matches!(action, Action::Rebuild) {
            assert_eq!(
                stamp, &final_stamps[name],
                "independent image date must remain untouched"
            );
        } else {
            assert_ne!(
                stamp, &final_stamps[name],
                "rebuilt image must replace the stale object"
            );
        }
    }
    if matches!(action, Action::Rebuild) {
        let reference = log("Work/reference.progress");
        assert!(
            reference
                .lines()
                .any(|row| row.trim().starts_with("Prepared ") && row.contains("leaf.asm [")),
            "shared replay must report dependency preparation: {reference}"
        );
        assert!(reference
            .lines()
            .any(|row| row.trim().starts_with("Linked Work:entry.asm [")));
        assert!(
            !reference.contains("Assembled "),
            "replay must not claim private assembly"
        );
        assert!(
            log("Work/quiet-invalid.txt").contains("invalid or unsupported arguments"),
            "quiet must not hide errors or accept an attached value"
        );
    }
    let cold_progress = log("Work/cold.progress");
    let warm_progress = log("Work/warm.progress");
    for (text, action) in [(&cold_progress, "Assembled"), (&warm_progress, "Reused")] {
        let rows: Vec<_> = text.lines().map(str::trim).collect();
        let leaf = rows
            .iter()
            .position(|row| row.starts_with(&format!("{action} leaf [")))
            .unwrap_or_else(|| panic!("missing {action} leaf in {text:?}"));
        let middle = rows
            .iter()
            .position(|row| row.starts_with(&format!("{action} middle [")))
            .unwrap_or_else(|| panic!("missing {action} middle in {text:?}"));
        assert!(leaf < middle, "dependency must finish before its consumer");
        assert!(rows
            .iter()
            .any(|row| row.starts_with("Linked Work:entry.asm [")));
        assert!(rows
            .last()
            .unwrap()
            .starts_with("Finished Work:entry.asm ["));
        for row in rows.iter().filter(|row| row.contains(" [")) {
            let time = row.split_once(" [").unwrap().1.strip_suffix("s]").unwrap();
            let (seconds, fraction) = time.split_once('.').unwrap();
            seconds.parse::<u32>().unwrap();
            assert_eq!(fraction.len(), 2);
            let fraction = fraction.parse::<u32>().unwrap();
            assert!(
                fraction < 100 && fraction.is_multiple_of(2),
                "50 Hz time: {row}"
            );
        }
    }
    eprintln!("MODULE_PROGRESS cold:\n{cold_progress}warm:\n{warm_progress}");
    let cold = counters(&log("Work/cold.txt"));
    let warm = counters(&log("Work/warm.txt"));
    let final_counts = counters(&run.stdout);
    eprintln!("INCREMENTAL_CACHE cold={cold:?} warm={warm:?} final={final_counts:?}");
    assert_eq!(cold.0, 0, "forced cold run must not reuse modules");
    assert!(cold.1 > 0, "cold run must compile cacheable modules");
    assert_eq!(
        warm,
        (3, 0),
        "warm run must reuse all modules without rebuilding"
    );
    assert_eq!(
        final_counts,
        if matches!(action, Action::Rebuild) {
            (0, 3)
        } else {
            (1, 2)
        },
        "dependency changes rebuild only their closure; --rebuild recompiles all modules"
    );
    if matches!(action, Action::Rebuild) {
        assert_eq!(final_counts.0, 0, "--rebuild must bypass all cache hits");
    }
}

#[path = "native_incremental_origins.rs"]
mod origins;
