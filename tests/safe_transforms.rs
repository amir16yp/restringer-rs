use oxc_span::SourceType;

use restringer_rs::{DeobfuscateOptions, Restringer};

#[test]
fn deobfuscate_smoke_test() {
    let restringer = Restringer::default();
    let result = restringer
        .deobfuscate(
            "if(!!x){'a'+'b'}",
            DeobfuscateOptions {
                source_type: Some(SourceType::mjs()),
                ..DeobfuscateOptions::default()
            },
        )
        .expect("deobfuscate failed");

    assert!(result.modified);
    assert!(!result.code.is_empty());
}
