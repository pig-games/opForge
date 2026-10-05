//! Bounded package-owned lowering of canonical xp1 expression paths.
use super::{NameTable, Projection};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ExpressionPathOperation {
    Indirect,
    Bracket,
    TupleChild(u8),
    Register(u16),
    QualifiedRegister { qualifier: u16, class: u16 },
    Scale,
    Member(u16),
}

pub(super) fn parse(value: &str, names: &mut NameTable) -> Option<Projection> {
    let spec = value.strip_prefix("xp1:")?;
    let (operand, tail) = spec.split_once('/')?;
    let operand = operand.parse::<u8>().ok().filter(|operand| *operand <= 1)?;
    let steps = tail.split('/').collect::<Vec<_>>();
    if !(1..=8).contains(&steps.len()) {
        return None;
    }
    let (terminal, containers) = steps.split_last()?;
    let mut operations = Vec::new();
    for step in containers {
        operations.push(match *step {
            "i" => ExpressionPathOperation::Indirect,
            "b" => ExpressionPathOperation::Bracket,
            "t0" => ExpressionPathOperation::TupleChild(0),
            "t1" => ExpressionPathOperation::TupleChild(1),
            "t2" => ExpressionPathOperation::TupleChild(2),
            _ => return None,
        });
    }
    let identifier = |name: &str| {
        !name.is_empty() && name.bytes().all(|b| b.is_ascii_alphanumeric() || b == b'_')
    };
    let operation = if let Some(class) = terminal.strip_prefix('r') {
        ExpressionPathOperation::Register(class.parse().ok()?)
    } else if let Some(qualified) = terminal.strip_prefix('q') {
        let (qualifier, class) = qualified.split_once(".c")?;
        if !identifier(qualifier) {
            return None;
        }
        let class = class.parse().ok()?;
        ExpressionPathOperation::QualifiedRegister {
            qualifier: names.id(qualifier),
            class,
        }
    } else if *terminal == "s" {
        ExpressionPathOperation::Scale
    } else if let Some(field) = terminal.strip_prefix('m') {
        if !identifier(field) {
            return None;
        }
        ExpressionPathOperation::Member(names.id(field))
    } else {
        return None;
    };
    operations.push(operation);
    Some(Projection::ExpressionPath {
        operand,
        operations,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn full_extension_recipe_lowers_all_nested_inputs() {
        let mut names = NameTable::default();
        let plan = "semv.sequence.v1:match:_@xp1:0/i/t0/mW,xp1:0/i/t0/b/t1/r1,xp1:0/i/t0/b/t2/qL.c0,xp1:0/i/t0/b/t2/s;encode:extension@xp1:0/i/t0/mW,xp1:0/i/t0/b/t1/r1,xp1:0/i/t0/b/t2/qL.c0,xp1:0/i/t0/b/t2/s";
        let super::super::CandidateRecipe::SemanticSequence { stages } =
            super::super::parse_recipe(plan, &mut names)
        else {
            panic!("full extension recipe must lower");
        };
        assert_eq!(stages.len(), 2);
        assert!(stages
            .iter()
            .flat_map(|stage| &stage.inputs)
            .all(|input| matches!(input, Projection::ExpressionPath { .. })));
        assert_eq!(super::super::member_excluded(plan), 0);
        assert_eq!(
            super::super::member_binding_fields(plan)
                .into_iter()
                .collect::<Vec<_>>(),
            [(0, "W")]
        );
    }

    #[test]
    fn bounded_paths_preserve_every_step_and_terminal() {
        let mut names = NameTable::default();
        assert_eq!(
            parse("xp1:1/i/t0/b/t2/qL.c0", &mut names),
            Some(Projection::ExpressionPath {
                operand: 1,
                operations: vec![
                    ExpressionPathOperation::Indirect,
                    ExpressionPathOperation::TupleChild(0),
                    ExpressionPathOperation::Bracket,
                    ExpressionPathOperation::TupleChild(2),
                    ExpressionPathOperation::QualifiedRegister {
                        qualifier: names.id("L"),
                        class: 0
                    }
                ]
            })
        );
        for source in [
            "xp1:2/r0",
            "xp1:0",
            "xp1:0/i",
            "xp1:0/t3/r0",
            "xp1:0/l/r0",
            "xp1:0/nPC",
            "xp1:0/qL.c",
            "xp1:0/m",
            "xp1:0/i/i/i/i/i/i/i/i/s",
        ] {
            assert_eq!(parse(source, &mut names), None, "{source}");
        }
        assert!(parse("xp1:0/i/i/i/i/i/i/i/s", &mut names).is_some());
    }
}
