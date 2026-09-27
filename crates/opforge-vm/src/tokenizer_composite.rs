// SPDX-License-Identifier: GPL-3.0-or-later
//! Package-selected composed-name recognition. Recipes retain lexical tokens.
use crate::portable_contract::{PortableComposedName, PortableToken, PortableTokenKind};
use crate::tokenizer_runtime_utils::vm_read_u8;

pub(crate) struct ComposedNamePolicy {
    marker: u8,
    minimum: u8,
    maximum: u8,
    suffix: Vec<u8>,
}

pub(crate) fn read_policy(
    program: &[u8],
    pc: &mut usize,
    diag: &str,
) -> Result<ComposedNamePolicy, String> {
    let marker = vm_read_u8(program, pc, diag, "composed marker")?;
    let minimum = vm_read_u8(program, pc, diag, "composed minimum")?;
    let maximum = vm_read_u8(program, pc, diag, "composed maximum")?;
    let count = usize::from(vm_read_u8(program, pc, diag, "composed suffix count")?);
    let invalid = || format!("{diag}: invalid tokenizer VM composed-name policy");
    let end = pc.checked_add(count).ok_or_else(invalid)?;
    let suffix = program.get(*pc..end).ok_or_else(invalid)?;
    if !marker.is_ascii_graphic()
        || marker.is_ascii_alphanumeric()
        || !(1..=9).contains(&minimum)
        || maximum < minimum
        || maximum > 9
        || suffix.is_empty()
        || suffix
            .iter()
            .any(|byte| !byte.is_ascii_graphic() || *byte == marker)
        || suffix
            .iter()
            .enumerate()
            .any(|(index, byte)| suffix[..index].contains(byte))
    {
        return Err(invalid());
    }
    *pc = end;
    Ok(ComposedNamePolicy {
        marker,
        minimum,
        maximum,
        suffix: suffix.to_vec(),
    })
}

// Policies select recognition within existing lexical names. They do not
// redefine scanner punctuation: only an At token supplies a standalone marker.
fn spelling(kind: &PortableTokenKind, marker: u8) -> Option<&[u8]> {
    match kind {
        PortableTokenKind::Identifier(text)
        | PortableTokenKind::Register(text)
        | PortableTokenKind::Number { text, .. } => Some(text.as_bytes()),
        PortableTokenKind::At if marker == b'@' => Some(b"@"),
        _ => None,
    }
}

pub(crate) fn compose_names(tokens: &mut [PortableToken], policy: &ComposedNamePolicy) {
    for token in tokens.iter_mut() {
        token.composed_name = None;
    }
    let mut index = 0;
    while index < tokens.len() {
        if matches!(tokens[index].kind, PortableTokenKind::Number { .. })
            || spelling(&tokens[index].kind, policy.marker).is_none()
        {
            index += 1;
            continue;
        }
        let mut end = index + 1;
        while end < tokens.len()
            && tokens[end - 1].span.line == tokens[end].span.line
            && tokens[end - 1].span.col_end == tokens[end].span.col_start
            && spelling(&tokens[end].kind, policy.marker).is_some()
        {
            end += 1;
        }
        let text: Vec<u8> = tokens[index..end]
            .iter()
            .flat_map(|token| {
                spelling(&token.kind, policy.marker)
                    .unwrap()
                    .iter()
                    .copied()
            })
            .collect();
        if !text
            .windows(2)
            .any(|pair| pair[0] == policy.marker && pair[1].is_ascii_digit())
        {
            index += 1;
            continue;
        }
        let logical_kind = u8::from(matches!(tokens[index].kind, PortableTokenKind::Register(_)));
        let recipe = parse_recipe(&text, end - index, logical_kind, policy);
        tokens[index].composed_name = recipe;
        index = end;
    }
}

fn parse_recipe(
    text: &[u8],
    consumed: usize,
    logical_kind: u8,
    policy: &ComposedNamePolicy,
) -> Option<PortableComposedName> {
    let invalid = || Some(PortableComposedName::Invalid);
    let mut payload = vec![logical_kind, 0];
    let mut cursor = 0;
    let mut fragments = 0usize;
    let mut literals = 0usize;
    while cursor < text.len() {
        if text[cursor] == policy.marker {
            let Some(&digit) = text.get(cursor + 1) else {
                return invalid();
            };
            if !digit.is_ascii_digit()
                || !(policy.minimum..=policy.maximum).contains(&(digit - b'0'))
            {
                return invalid();
            }
            payload.push(digit - b'0');
            cursor += 2;
        } else {
            let start = cursor;
            while cursor < text.len() && text[cursor] != policy.marker {
                if start != 0 && !policy.suffix.contains(&text[cursor]) {
                    return invalid();
                }
                cursor += 1;
            }
            let length = cursor - start;
            let Ok(length_byte) = u8::try_from(length) else {
                return invalid();
            };
            payload.extend([0, length_byte]);
            payload.extend_from_slice(&text[start..cursor]);
            literals += 1;
        }
        fragments += 1;
        if fragments > 255 || payload.len() > 255 {
            return invalid();
        }
    }
    if literals == 0 {
        return None;
    }
    let Ok(consumed_tokens) = u8::try_from(consumed) else {
        return invalid();
    };
    payload[1] = fragments as u8;
    Some(PortableComposedName::Recipe {
        consumed_tokens,
        packed_payload: payload,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn policy_validation_and_recipe_representation_bounds() {
        for bytes in [
            vec![],
            vec![b'@', 0, 9, 1, b'a'],
            vec![b'@', 1, 10, 1, b'a'],
            vec![b'@', 9, 1, 1, b'a'],
            vec![b'@', 1, 9, 2, b'a'],
            vec![b'@', 1, 9, 2, b'a', b'a'],
            vec![b'@', 1, 9, 1, b'@'],
        ] {
            assert!(read_policy(&bytes, &mut 0, "test").is_err(), "{bytes:?}");
        }
        let bytes = crate::builder::default_composed_name_payload();
        let policy = read_policy(&bytes, &mut 0, "test").unwrap();
        let mut long = vec![b'a'; 250];
        long.extend_from_slice(b"@1");
        assert_eq!(
            parse_recipe(&long, 1, 0, &policy),
            Some(PortableComposedName::Recipe {
                consumed_tokens: 1,
                packed_payload: [vec![0, 2, 0, 250], vec![b'a'; 250], vec![1]].concat()
            })
        );
        long.insert(0, b'a');
        assert_eq!(
            parse_recipe(&long, 1, 0, &policy),
            Some(PortableComposedName::Invalid)
        );
        assert_eq!(
            parse_recipe(b"a@1", 256, 0, &policy),
            Some(PortableComposedName::Invalid)
        );
        assert_eq!(parse_recipe(b"@1", 2, 0, &policy), None);
        let mut tokens =
            vec![crate::tokenizer_runtime_utils::vm_build_token(0, b"name@1", 1, 0, 6, 6).unwrap()];
        compose_names(&mut tokens, &policy);
        assert!(matches!(
            tokens[0].composed_name,
            Some(PortableComposedName::Recipe { .. })
        ));
        let mut invalid_run = vec![
            crate::tokenizer_runtime_utils::vm_build_token(40, b"@", 1, 0, 1, 1).unwrap(),
            crate::tokenizer_runtime_utils::vm_build_token(2, b"0suffix", 1, 1, 8, 7).unwrap(),
            crate::tokenizer_runtime_utils::vm_build_token(40, b"@", 1, 8, 9, 1).unwrap(),
            crate::tokenizer_runtime_utils::vm_build_token(2, b"1good", 1, 9, 14, 5).unwrap(),
        ];
        compose_names(&mut invalid_run, &policy);
        assert_eq!(
            invalid_run[0].composed_name,
            Some(PortableComposedName::Invalid)
        );
        assert!(invalid_run[1..]
            .iter()
            .all(|token| token.composed_name.is_none()));
        let mut alternative = crate::builder::default_composed_name_payload();
        alternative[0] = b'$';
        let alternative = read_policy(&alternative, &mut 0, "test").unwrap();
        compose_names(&mut tokens, &alternative);
        assert_eq!(tokens[0].composed_name, None);
    }
}
