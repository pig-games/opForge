//! Acquisition of the CPU catalog and metadata dispatch program.

use std::collections::BTreeSet;

use package::package as meta;
use vm::runtime_model_core::RuntimeModelCore;

pub(super) fn metadata_program(
    core: &RuntimeModelCore,
    selected_cpu: &str,
    mut acquire_head: impl FnMut(&str) -> Result<u16, String>,
) -> Result<Vec<u8>, String> {
    let cpus = core.supported_cpus();
    let canonical = cpus
        .iter()
        .map(|(spelling, _, _)| {
            core.canonical_cpu_id_for_input(spelling)
                .ok_or("metadata catalog has an unresolved CPU")
        })
        .collect::<Result<BTreeSet<_>, _>>()?
        .into_iter()
        .collect::<Vec<_>>();
    let selected = canonical
        .iter()
        .position(|cpu| cpu.eq_ignore_ascii_case(selected_cpu))
        .ok_or("metadata selected CPU is absent from catalog")?
        + 1;
    let mut rows = Vec::new();
    let mut row = |spelling: &str, contexts, role, key, operand, target| {
        let head = acquire_head(spelling)?;
        rows.push(meta::MetadataRow {
            head,
            contexts,
            role,
            key,
            operand,
            target,
        });
        Ok::<(), String>(())
    };
    let root = 1 << meta::METADATA_CONTEXT_NONE;
    let meta_context = 1 << meta::METADATA_CONTEXT_META;
    let output_context = 1 << meta::METADATA_CONTEXT_OUTPUT;
    let target_context = 1 << meta::METADATA_CONTEXT_TARGET;
    let outputs = [
        (
            "name",
            meta::METADATA_KEY_NAME,
            meta::METADATA_OPERAND_REQUIRED,
        ),
        (
            "hex",
            meta::METADATA_KEY_HEX,
            meta::METADATA_OPERAND_OPTIONAL,
        ),
        ("bin", meta::METADATA_KEY_BIN, meta::METADATA_OPERAND_BIN),
        ("fill", meta::METADATA_KEY_FILL, meta::METADATA_OPERAND_FILL),
    ];
    for (key, id, operand) in outputs {
        row(
            &format!("meta.output.{key}"),
            root,
            meta::METADATA_ROLE_OUTPUT,
            id,
            operand,
            0,
        )?;
    }
    for (key, id) in [
        ("name", meta::METADATA_KEY_NAME),
        ("version", meta::METADATA_KEY_VERSION),
    ] {
        row(
            &format!("meta.{key}"),
            root,
            meta::METADATA_ROLE_DESCRIPTIVE,
            id,
            meta::METADATA_OPERAND_REQUIRED,
            0,
        )?;
        row(
            key,
            meta_context,
            meta::METADATA_ROLE_DESCRIPTIVE,
            id,
            meta::METADATA_OPERAND_REQUIRED,
            0,
        )?;
    }
    for (key, id, operand) in outputs {
        row(
            &format!("output.{key}"),
            meta_context,
            meta::METADATA_ROLE_OUTPUT,
            id,
            operand,
            0,
        )?;
        row(
            key,
            output_context | target_context,
            meta::METADATA_ROLE_OUTPUT,
            id,
            operand,
            0,
        )?;
    }
    for (head, contexts, role) in [
        ("meta", root, meta::METADATA_ROLE_META_OPEN),
        ("endmeta", meta_context, meta::METADATA_ROLE_META_CLOSE),
        ("output", meta_context, meta::METADATA_ROLE_OUTPUT_OPEN),
        (
            "endoutput",
            output_context,
            meta::METADATA_ROLE_OUTPUT_CLOSE,
        ),
    ] {
        row(head, contexts, role, 0, meta::METADATA_OPERAND_NONE, 0)?;
    }
    for (alias, _, _) in cpus {
        let cpu = core
            .canonical_cpu_id_for_input(&alias)
            .ok_or("unresolved metadata target alias")?;
        let target = word(
            canonical
                .iter()
                .position(|name| *name == cpu)
                .ok_or("unknown metadata target")?
                + 1,
        )?;
        for (key, id, operand) in outputs {
            row(
                &format!("meta.output.{alias}.{key}"),
                root,
                meta::METADATA_ROLE_OUTPUT,
                id,
                operand,
                target,
            )?;
            row(
                &format!("output.{alias}.{key}"),
                meta_context,
                meta::METADATA_ROLE_OUTPUT,
                id,
                operand,
                target,
            )?;
        }
        if alias
            .as_bytes()
            .first()
            .is_some_and(|byte| byte.is_ascii_alphabetic() || *byte == b'_')
        {
            row(
                &alias,
                output_context,
                meta::METADATA_ROLE_TARGET_OPEN,
                0,
                meta::METADATA_OPERAND_NONE,
                target,
            )?;
        }
        row(
            &format!("end{alias}"),
            target_context,
            meta::METADATA_ROLE_TARGET_CLOSE,
            0,
            meta::METADATA_OPERAND_NONE,
            target,
        )?;
    }
    meta::metadata_program(&rows, word(selected)?, word(canonical.len())?)
}

fn word(value: usize) -> Result<u16, String> {
    u16::try_from(value).map_err(|_| "binary-source package exceeds u16 capacity".into())
}
