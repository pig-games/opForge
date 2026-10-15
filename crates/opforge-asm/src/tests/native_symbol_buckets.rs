//! Whole-language symbol binding through the compact CLI's expanded hash table.
use super::native_iterable_loops::{build_image, oracle, run_case, scratch};
use super::*;

// Fixture selection only: cover all low-ten-bit hash buckets with diverse names.
// These names exercise several growth boundaries in the adaptive table. This
// does not predict binding results; emitted bytes below are the independent
// oracle. The template candidate key has a separate hash.
fn symbol_bucket(name: &str) -> usize {
    name.bytes().fold(0u32, |hash, byte| {
        hash.wrapping_mul(33)
            .wrapping_add(byte.to_ascii_lowercase() as u32)
    }) as usize
        & 1023
}

fn bucket_names() -> Vec<String> {
    let mut names = vec![None; 1024];
    let mut remaining = names.len();
    for serial in 0..100_000 {
        let name = format!("bucket{serial}");
        let bucket = symbol_bucket(&format!("app.{name}"));
        if names[bucket].is_none() {
            names[bucket] = Some(name);
            remaining -= 1;
            if remaining == 0 {
                break;
            }
        }
    }
    assert_eq!(remaining, 0, "fixture must exercise every symbol bucket");
    let mut names = names.into_iter().map(Option::unwrap).collect::<Vec<_>>();
    // Extra names collide with bucket 1023, including different name lengths.
    // Reverse reads below must traverse the chain rather than returning its head.
    for prefix in ["collision", "longcollision", "evenlongercollision"] {
        let name = (0..100_000)
            .map(|serial| format!("{prefix}{serial}"))
            .find(|name| symbol_bucket(&format!("app.{name}")) == 1023)
            .expect("fixture collision spelling");
        names.push(name);
    }
    let same_length = (10_000..100_000)
        .map(|serial| format!("samecollision{serial}"))
        .filter(|name| symbol_bucket(&format!("app.{name}")) == 1023)
        .take(3)
        .collect::<Vec<_>>();
    assert_eq!(same_length.len(), 3);
    names.extend(same_length);
    names
}

fn all_buckets() -> (String, Vec<u8>) {
    let names = bucket_names();
    let mut source = String::from(".module app\n.cpu 68020\n");
    let values = (0..names.len())
        .map(|index| (index % 251) as u8)
        .collect::<Vec<_>>();
    // References precede definitions. Initial misses must safely traverse empty
    // upper buckets and subsequently resolve the same forward symbol identity.
    for name in &names {
        source.push_str(&format!(".byte {name}\n"));
    }
    for (name, value) in names.iter().zip(&values) {
        source.push_str(&format!("{name} .const {value}\n"));
    }
    for name in names.iter().rev() {
        source.push_str(&format!(".byte {}\n", name.to_ascii_uppercase()));
    }
    source.push_str(".endmodule\n");
    let mut expected = values.clone();
    expected.extend(values.into_iter().rev());
    (source, expected)
}

fn visibility_and_facets() -> (String, Vec<u8>) {
    let source = r#".module provider
.cpu 68020
.pub
value .const 41
.endmodule
.module app
.cpu 68020
.use provider (value as selected)
.namespace inner
value .const 13
.byte VaLuE,app.value,selected
.endnamespace
.byte VALUE,selected
value .const 7
value .macro
.byte 99
.endmacro
.VaLuE
.byte value
.endmodule
"#;
    (source.into(), vec![13, 7, 41, 7, 41, 99, 7])
}

fn imported_struct_member() -> (String, Vec<u8>) {
    // The public type is in bucket 65, but its Used field is in bucket 768.
    // Import completion must use the same mask as scope insertion for both.
    assert_eq!(
        symbol_bucket("experimental.amigaos.binary_memory.Block"),
        65
    );
    assert_eq!(
        symbol_bucket("experimental.amigaos.binary_memory.Block.Used"),
        768
    );
    let source = r#".module experimental.amigaos.binary_memory
.cpu 68020
.pub
Block .struct
Pointer .long ?
Capacity .long ?
Used .long ?
.endstruct
.endmodule
.module experimental.amigaos.binary_values
.cpu 68020
.use experimental.amigaos.binary_memory as memory
Owner .struct
Arena .res memory.Block.Used+4
Kinds .res MEMORY.Block.Used+4
Count .long ?
.endstruct
.byte Owner.Arena,Owner.Kinds,Owner.Count,memory.Block.Used
.endmodule
"#;
    (source.into(), vec![0, 12, 24, 8])
}

#[test]
fn native_symbol_bucket_imported_struct_rust_oracle() {
    let dir = scratch();
    let (source, expected) = imported_struct_member();
    assert_eq!(oracle(&dir.0, &source).unwrap(), expected);
}

#[test]
#[ignore = "requires configured FS-UAE; imported declared struct members in upper symbol buckets"]
fn native_symbol_bucket_imported_struct_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_SYMBOL_BUCKET_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let (source, expected) = imported_struct_member();
    assert_eq!(oracle(&dir.0, &source).unwrap(), expected);
    run_case(
        &root,
        &dir.0,
        &image,
        "imported-struct-upper-bucket",
        "68020",
        &source,
        false,
    );
}

fn cases() -> [(&'static str, String, Vec<u8>); 2] {
    let (wide, wide_expected) = all_buckets();
    let (visibility, visibility_expected) = visibility_and_facets();
    [
        (
            "all-symbol-buckets-forward-and-colliding",
            wide,
            wide_expected,
        ),
        (
            "symbol-buckets-visibility-and-template-facets",
            visibility,
            visibility_expected,
        ),
    ]
}

#[test]
fn native_symbol_buckets_rust_oracles() {
    let dir = scratch();
    for (name, source, expected) in cases() {
        assert_eq!(oracle(&dir.0, &source).unwrap(), expected, "{name}");
    }
}

#[test]
#[ignore = "requires configured FS-UAE; upper symbol buckets, collisions and binding visibility"]
fn native_symbol_buckets_fs_uae() {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .unwrap();
    let dir = scratch();
    let image = std::env::var_os("OPFORGE_SYMBOL_BUCKET_IMAGE")
        .map(|path| fs::read(path).unwrap())
        .unwrap_or_else(|| build_image(&root, &dir.0));
    let mut failures = Vec::new();
    for (name, source, expected) in cases() {
        if std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            assert_eq!(oracle(&dir.0, &source).unwrap(), expected, "{name}");
            run_case(&root, &dir.0, &image, name, "68020", &source, false);
        }))
        .is_err()
        {
            failures.push(name);
        }
    }
    assert!(failures.is_empty(), "symbol bucket failures: {failures:?}");
}
