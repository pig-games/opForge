use libopforge::registry;
use std::error::Error;

pub fn run_example() -> Result<(), Box<dyn Error>> {
    let asm_registry = registry::default_asm_registry();
    let snapshot = registry::CapabilitySnapshot::from_registry(&asm_registry);
    let resolved = registry::resolve_target_cpu(
        &asm_registry,
        Some("8085"),
        registry::CpuType::new("8085"),
    )?;
    let view = snapshot
        .view_for_cpu(resolved)
        .ok_or("missing capability view")?;

    assert_eq!(view.family_id, "intel8080");
    assert!(!view.mnemonics.is_empty());

    Ok(())
}

#[allow(dead_code)]
fn main() -> Result<(), Box<dyn Error>> {
    run_example()
}