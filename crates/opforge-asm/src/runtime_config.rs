// SPDX-License-Identifier: GPL-3.0-or-later

use vm::rollout::portable_expr_runtime_enabled_for_family as vm_portable_expr_runtime_enabled_for_family;

const OPFORGE_VM_EXPR_EVAL_OPT_IN_FAMILIES_ENV: &str = "OPFORGE_VM_EXPR_EVAL_OPT_IN_FAMILIES";
const OPFORGE_VM_EXPR_EVAL_FORCE_HOST_FAMILIES_ENV: &str =
    "OPFORGE_VM_EXPR_EVAL_FORCE_HOST_FAMILIES";
const LEGACY_OPTHREAD_EXPR_EVAL_OPT_IN_FAMILIES_ENV: &str = "OPTHREAD_EXPR_EVAL_OPT_IN_FAMILIES";
const LEGACY_OPTHREAD_EXPR_EVAL_FORCE_HOST_FAMILIES_ENV: &str =
    "OPTHREAD_EXPR_EVAL_FORCE_HOST_FAMILIES";

fn parse_family_list_from_env(var_name: &str) -> Vec<String> {
    let Ok(raw) = std::env::var(var_name) else {
        return Vec::new();
    };

    let mut families = Vec::new();
    for candidate in raw
        .split(',')
        .map(str::trim)
        .filter(|item| !item.is_empty())
    {
        if !families
            .iter()
            .any(|existing: &String| existing.eq_ignore_ascii_case(candidate))
        {
            families.push(candidate.to_string());
        }
    }
    families
}

#[must_use]
pub fn expr_eval_opt_in_families_from_env() -> Vec<String> {
    let mut families = parse_family_list_from_env(OPFORGE_VM_EXPR_EVAL_OPT_IN_FAMILIES_ENV);
    for candidate in parse_family_list_from_env(LEGACY_OPTHREAD_EXPR_EVAL_OPT_IN_FAMILIES_ENV) {
        if !families
            .iter()
            .any(|existing| existing.eq_ignore_ascii_case(candidate.as_str()))
        {
            families.push(candidate);
        }
    }
    families
}

#[must_use]
pub fn expr_eval_force_host_families_from_env() -> Vec<String> {
    let mut families = parse_family_list_from_env(OPFORGE_VM_EXPR_EVAL_FORCE_HOST_FAMILIES_ENV);
    for candidate in parse_family_list_from_env(LEGACY_OPTHREAD_EXPR_EVAL_FORCE_HOST_FAMILIES_ENV) {
        if !families
            .iter()
            .any(|existing| existing.eq_ignore_ascii_case(candidate.as_str()))
        {
            families.push(candidate);
        }
    }
    families
}

#[must_use]
pub fn portable_expr_runtime_enabled_for_family(
    family_id: &str,
    opt_in_families: &[String],
    force_host_families: &[String],
) -> bool {
    vm_portable_expr_runtime_enabled_for_family(family_id, opt_in_families, force_host_families)
}

#[must_use]
pub fn portable_expr_runtime_force_host_for_family(
    family_id: &str,
    force_host_families: &[String],
) -> bool {
    force_host_families
        .iter()
        .any(|force_host| force_host.eq_ignore_ascii_case(family_id))
}
