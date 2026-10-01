//! Host-only BS13 inventory; generation and explicit rejection rows are not native proof.
use super::{prepare_package, HEADER, ROW};
use serde_json::{json, Value};
use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
};
use vm::{
    binary_source_package::{BinarySourcePackage, CandidateRecipe},
    runtime_model_core::RuntimeModelCore,
};

fn number(bytes: &[u8], offset: usize, width: usize) -> Result<usize, String> {
    let end = offset.checked_add(width).ok_or("offset overflow")?;
    let field = bytes.get(offset..end).ok_or("truncated integer")?;
    match width {
        2 => Ok(u16::from_be_bytes(field.try_into().unwrap()) as usize),
        4 => Ok(u32::from_be_bytes(field.try_into().unwrap()) as usize),
        _ => Err("invalid integer width".into()),
    }
}

fn region(bytes: &[u8], offset: usize, count: usize, width: usize) -> Result<&[u8], String> {
    let end = count
        .checked_mul(width)
        .and_then(|size| offset.checked_add(size))
        .ok_or("region overflow")?;
    bytes
        .get(offset..end)
        .ok_or_else(|| "truncated region".into())
}

fn inventory(bytes: &[u8], package: &BinarySourcePackage) -> Result<Value, String> {
    if bytes.len() < HEADER
        || bytes.get(..4) != Some(b"BS13")
        || number(bytes, 4, 4)? != bytes.len()
    {
        return Err("invalid BS13 header".into());
    }
    let target_offset = number(bytes, 124, 4)?;
    let target_bytes = number(bytes, 128, 2)?;
    let runtime_bytes = number(bytes, 72, 4)?;
    let target = region(bytes, target_offset, target_bytes, 1)?;
    if target_offset < HEADER
        || target_bytes == 0
        || target_bytes > 26
        || target_offset + target_bytes > runtime_bytes
        || number(bytes, 130, 2)? != 0
        || !target
            .iter()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-'))
    {
        return Err("invalid runtime target identity".into());
    }
    let rows = region(bytes, number(bytes, 16, 4)?, number(bytes, 20, 4)?, ROW)?;
    let programs = region(bytes, number(bytes, 32, 4)?, number(bytes, 36, 4)?, 12)?;
    for program in programs.chunks_exact(12) {
        region(bytes, number(program, 4, 4)?, number(program, 8, 4)?, 1)?;
    }
    let dictionary_count = number(bytes, 12, 4)?;
    let mut dictionary_offset = number(bytes, 8, 4)?;
    for _ in 0..dictionary_count {
        let length = number(bytes, dictionary_offset, 2)?;
        let entry = length.checked_add(6).ok_or("dictionary length overflow")?;
        region(bytes, dictionary_offset, entry, 1)?;
        dictionary_offset = dictionary_offset
            .checked_add(entry)
            .and_then(|end| end.checked_add(end % 2))
            .ok_or("dictionary offset overflow")?;
    }
    if dictionary_offset != number(bytes, 40, 4)? {
        return Err("dictionary does not end at tokenizer".into());
    }
    let name = |id: usize| {
        package
            .names
            .get(id)
            .map(String::as_str)
            .ok_or_else(|| format!("unknown original name {id}"))
    };
    let mut recipes = BTreeMap::<u8, usize>::new();
    let mut table_modes = BTreeMap::<&str, usize>::new();
    for table in &package.table_programs {
        *table_modes.entry(name(table.mode as usize)?).or_default() += 1;
    }
    let mut unsupported = Vec::new();
    let mut rejection_barriers = 0usize;
    let mut intermediate = Vec::new();
    for candidate in &package.candidates {
        if let CandidateRecipe::Unsupported { plan } = &candidate.recipe {
            intermediate.push(json!({"mnemonic": name(candidate.mnemonic as usize)?,
                "mode": name(candidate.mode as usize)?, "qualifier": candidate.qualifier
                    .map(|q| package.qualifiers.get(q as usize).cloned().ok_or("unknown intermediate qualifier"))
                    .transpose()?, "plan": name(*plan as usize)?}));
        }
    }
    for (index, row) in rows.chunks_exact(ROW).enumerate() {
        *recipes.entry(row[5]).or_default() += 1;
        if row[5] != 6 {
            continue;
        }
        let mnemonic = number(row, 0, 2)?;
        let mode = number(row, 20, 2)?;
        let qualifier = if row[2] == 0 {
            None
        } else {
            Some(
                package
                    .qualifiers
                    .get(usize::from(row[2] - 1))
                    .ok_or("unknown qualifier")?
                    .as_str(),
            )
        };
        let shape = match row[3] {
            0 => "implied",
            1 => "direct",
            2 => "immediate",
            3 => "immediate_register",
            4 => "register_direct",
            5 => "register_register",
            6 => "direct_register",
            8 => "immediate_direct",
            9 => "register",
            10 => "direct_direct",
            _ => "unrecognized",
        };
        let mut reasons = Vec::new();
        let mut all_matches_are_rejections = true;
        for candidate in &package.candidates {
            if candidate.mnemonic as usize == mnemonic
                && candidate.mode as usize == mode
                && candidate.qualifier.map(|q| usize::from(q) + 1).unwrap_or(0)
                    == usize::from(row[2])
                && (row[3] == 255 || name(candidate.shape as usize)? == shape)
                && candidate.owner_rank == row[4]
                && candidate.priority as usize == number(row, 6, 2)?
                && candidate.width_rank == row[16]
            {
                if let CandidateRecipe::Unsupported { plan } = &candidate.recipe {
                    let plan = name(*plan as usize)?;
                    all_matches_are_rejections &= plan.starts_with("semv.reject.v1:");
                    reasons.push(plan);
                } else {
                    all_matches_are_rejections = false;
                }
            }
        }
        reasons.sort_unstable();
        reasons.dedup();
        let declared_rejection = !reasons.is_empty() && all_matches_are_rejections;
        rejection_barriers += usize::from(declared_rejection);
        unsupported.push(json!({"row": index, "mnemonic": name(mnemonic)?, "qualifier": qualifier,
            "mode": name(mode)?, "wire_shape": row[3], "shape": shape, "owner_rank": row[4],
            "priority": number(row,6,2)?, "width_rank": row[16],
            "classification": if declared_rejection { "declared_rejection_barrier" } else { "other_or_unclassified_barrier" },
            "intermediate_plans": reasons}));
    }
    Ok(
        json!({"byte_size": bytes.len(), "runtime_byte_size": number(bytes,72,4)?,
        "target_key": std::str::from_utf8(target).map_err(|_| "invalid target UTF-8")?,
        "dictionary_count": dictionary_count, "program_count": programs.len()/12,
        "table_program_count": package.table_programs.len(),
        "table_mode_counts": table_modes,
        "semantic_program_count": package.semantic_programs.len(),
        "value_program_count": package.value_programs.len(),
        "candidate_count": rows.len()/ROW, "intermediate_candidate_count": package.candidates.len(),
        "instruction_coverage": if rows.is_empty() { "no_compact_instruction_candidates" } else { "requires_native_qualification" },
        "final_recipe_counts": recipes, "final_unsupported_count": unsupported.len(),
        "declared_rejection_barrier_count": rejection_barriers,
        "other_or_unclassified_barrier_count": unsupported.len()-rejection_barriers,
        "final_unsupported_forms": unsupported, "intermediate_unsupported_plans": intermediate}),
    )
}

// Provisional readable filenames suitable for classic Amiga directory entries.
fn filename(cpu: &str, dialect: &str) -> Result<String, String> {
    for id in [cpu, dialect] {
        if id.is_empty()
            || !id
                .bytes()
                .all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b'-')
        {
            return Err(format!("unsafe package filename identifier: {id}"));
        }
    }
    let file = format!("{cpu}--{dialect}.bin");
    if file.len() > 30 {
        return Err(format!("package filename exceeds 30 bytes: {file}"));
    }
    Ok(file)
}

#[test]
#[ignore = "host-only export; set OPFORGE_COMPACT_PACKAGE_EXPORT_DIR to a fresh absolute directory"]
fn compact_package_inventory_export() {
    let destination = std::env::var_os("OPFORGE_COMPACT_PACKAGE_EXPORT_DIR")
        .expect("OPFORGE_COMPACT_PACKAGE_EXPORT_DIR is required");
    let destination = Path::new(&destination);
    assert!(
        destination.is_absolute(),
        "export directory must be absolute"
    );
    assert!(!destination.exists(), "export directory must be fresh");
    let parent = destination
        .parent()
        .unwrap()
        .canonicalize()
        .expect("export parent must exist");
    let destination = parent.join(
        destination
            .file_name()
            .expect("export directory needs a name"),
    );
    let target = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap()
        .join("target");
    let target = target.canonicalize().unwrap_or(target);
    assert!(
        !destination.starts_with(target),
        "exports must survive target cleanup"
    );
    fs::create_dir(&destination).unwrap();
    let registry = engine::build_default_asm_registry();
    let core = RuntimeModelCore::from_registry(&registry).map_err(|error| error.to_string());
    let mut targets = Vec::new();
    let mut filenames = BTreeSet::new();
    let mut rejection_barriers = 0usize;
    let mut total_bytes = 0usize;
    let mut successful = 0usize;
    let mut empty_pipelines = 0usize;
    let mut unsupported = 0usize;
    for cpu in registry.cpu_ids() {
        let family = registry
            .cpu_family_id(cpu)
            .expect("registered CPU has a family");
        let default = registry
            .cpu_default_dialect(cpu)
            .expect("registered CPU has a default dialect");
        let mut dialects = registry.dialect_ids_for_family(family);
        dialects.push(default.to_owned());
        dialects.sort();
        dialects.dedup();
        let aliases: Vec<_> = registry
            .cpu_name_list()
            .into_iter()
            .filter(|name| registry.resolve_cpu_name(name) == Some(cpu) && name != cpu.as_str())
            .collect();
        for dialect in dialects {
            let mut target = json!({"cpu": cpu.as_str(), "family": family.as_str(), "dialect": dialect,
                "default_dialect": default, "is_default": dialect == default, "cpu_aliases": aliases});
            let generated = (|| {
                let file = filename(cpu.as_str(), &dialect)?;
                if !filenames.insert(file.to_ascii_lowercase()) {
                    return Err(format!("duplicate package filename: {file}"));
                }
                let core = core.as_ref().map_err(Clone::clone)?;
                let resolved = core
                    .resolve_pipeline(cpu.as_str(), Some(&dialect))
                    .map_err(|error| error.to_string())?;
                let package = BinarySourcePackage::prepare(core, &resolved)?;
                let bytes = prepare_package(core, &resolved)?;
                let counts = inventory(&bytes, &package)?;
                Ok::<_, String>((bytes, counts, file))
            })();
            match generated {
                Ok((bytes, counts, file)) => {
                    fs::write(destination.join(&file), &bytes).unwrap();
                    successful += 1;
                    empty_pipelines += usize::from(counts["candidate_count"] == 0);
                    total_bytes = total_bytes
                        .checked_add(bytes.len())
                        .expect("total package size overflow");
                    rejection_barriers +=
                        counts["declared_rejection_barrier_count"].as_u64().unwrap() as usize;
                    unsupported += counts["final_unsupported_count"].as_u64().unwrap() as usize;
                    target["status"] = json!("generated");
                    target["file"] = json!(file);
                    target["inventory"] = counts;
                }
                Err(error) => {
                    target["status"] = json!("failed");
                    target["error"] = json!(error);
                }
            }
            targets.push(target);
        }
    }
    let report = json!({"format": "BS13", "scope": "host generation only; no native execution or parity claim",
        "unsupported_reason_note": "Final recipe 6 rows are rejection barriers. Nonempty matches consisting entirely of Unsupported semv.reject.v1 declarations identify package rejections. Other or unclassified barriers do not prove gaps in legal instruction support. Empty plans can mean later wire lowering rejected the form. Zero candidates means no compact instruction coverage, not complete support.",
        "summary": {"targets": targets.len(), "generated": successful, "failed": targets.len()-successful,
            "canonical_cpus": registry.cpu_ids().len(), "pipelines_without_instruction_candidates": empty_pipelines,
            "total_byte_size": total_bytes, "final_unsupported_rows": unsupported, "declared_rejection_barriers": rejection_barriers,
            "other_or_unclassified_barriers": unsupported-rejection_barriers}, "targets": targets});
    fs::write(
        destination.join("inventory.json"),
        serde_json::to_vec_pretty(&report).unwrap(),
    )
    .unwrap();
    println!(
        "Compact package inventory: {} ({} generated, {} failed, {} without instruction candidates)",
        destination.join("inventory.json").display(), successful, targets.len()-successful, empty_pipelines
    );
}
