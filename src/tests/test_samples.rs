use crate::{DeobfuscateOptions, Restringer};
use std::fs;
use std::path::Path;

#[test]
fn test_deobfuscate_all_samples() {
    let resources_dir = Path::new("src/tests/resources");
    let output_dir = Path::new("./test_output");

    fs::create_dir_all(output_dir).expect("Failed to create output directory");

    let restringer = Restringer::default();

    let entries = fs::read_dir(resources_dir).expect("Failed to read resources directory");

    for entry in entries {
        let entry = entry.expect("Failed to read directory entry");
        let path = entry.path();

        if path.extension().and_then(|s| s.to_str()) == Some("js") {
            let filename = path.file_name().unwrap().to_str().unwrap();
            let stem = path.file_stem().unwrap().to_str().unwrap();
            
            println!("Processing: {}", filename);

            let source_code = fs::read_to_string(&path)
                .expect(&format!("Failed to read {}", filename));

            let result = restringer.deobfuscate(&source_code, DeobfuscateOptions::default());

            match result {
                Ok(deobf_result) => {
                    let output_filename = format!("{}_deobf.js", stem);
                    let output_path = output_dir.join(output_filename);
                    
                    fs::write(&output_path, deobf_result.code)
                        .expect(&format!("Failed to write output file: {:?}", output_path));
                    
                    println!("  ✓ Deobfuscated {} (modified: {})", filename, deobf_result.modified);
                }
                Err(e) => {
                    println!("  ✗ Failed to deobfuscate {}: {:?}", filename, e);
                    panic!("Deobfuscation failed for {}: {:?}", filename, e);
                }
            }
        }
    }
}
