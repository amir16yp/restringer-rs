use std::{
    ffi::OsStr,
    ffi::OsString,
    fs,
    io::Read,
    path::{Path, PathBuf},
    process,
};

use clap::{ArgGroup, Parser as ClapParser};
use oxc_span::SourceType;

use restringer_rs::{DeobfuscateOptions, Restringer};

#[derive(Debug, ClapParser)]
#[command(name = "restringer", about = "REstringer - a JavaScript deobfuscator (Rust rewrite)")]
#[command(group(
    ArgGroup::new("verbosity")
        .args(["quiet", "verbose"])
        .multiple(false)
))]
struct Cli {
    /// The obfuscated JS/TS file
    input_filename: PathBuf,

    /// Remove dead nodes from script after deobfuscation is complete (unsafe)
    #[arg(short = 'c', long = "clean")]
    clean: bool,

    /// Suppress output to stdout. Output result only to stdout if the -o option is not set
    #[arg(short = 'q', long = "quiet")]
    quiet: bool,

    /// Show more debug messages while deobfuscating
    #[arg(short = 'v', long = "verbose")]
    verbose: bool,

    /// Write deobfuscated script to output filename. <input_filename>-deob.js is used if no filename is provided
    #[arg(short = 'o', long = "output", num_args = 0..=1, default_missing_value = "")]
    output: Option<OsString>,

    /// Run at most M iterations
    #[arg(short = 'm', long = "max-iterations")]
    max_iterations: Option<usize>,
}

fn main() {
    let cli = Cli::parse();

    if cli.quiet && cli.verbose {
        eprintln!("[-] Critical Error: Don't set both -q and -v at the same time *smh*");
        process::exit(2);
    }

    let input_path = cli.input_filename;
    let source_text = match read_file_to_string_with_capacity(&input_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("[-] Critical Error: Failed to read {}: {e}", input_path.display());
            process::exit(1);
        }
    };

    let source_type = match SourceType::from_path(&input_path) {
        Ok(st) => st,
        Err(e) => {
            eprintln!(
                "[-] Critical Error: Failed to determine source type for {}: {e}",
                input_path.display()
            );
            process::exit(1);
        }
    };

    if !cli.quiet {
        eprintln!("[!] Deobfuscating {}...", input_path.display());
        if let Some(m) = cli.max_iterations {
            eprintln!("[!] Running at most {m} iterations");
        }
        if cli.clean {
            eprintln!("[!] Clean enabled (no-op in Milestone 1)");
        }
    }

    let mut restringer = Restringer::default();
    if let Some(m) = cli.max_iterations {
        restringer.set_max_iterations(m);
    }
    let result = match restringer.deobfuscate(
        &source_text,
        DeobfuscateOptions {
            clean: cli.clean,
            run_unsafe: false,
            max_iterations: cli.max_iterations,
            source_type: Some(source_type),
            filename_for_source_type: None,
        },
    ) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("[-] Critical Error: {e}");
            process::exit(1);
        }
    };

    let output_text = result.code;

    let output_path = resolve_output_path(&input_path, cli.output.as_deref());
    let output_to_file = cli.output.is_some();

    if output_to_file {
        if let Err(e) = fs::write(&output_path, output_text.as_bytes()) {
            eprintln!("[-] Critical Error: Failed to write {}: {e}", output_path.display());
            process::exit(1);
        }
        if !cli.quiet {
            eprintln!("[+] Saved {}", output_path.display());
        }
    } else {
        print!("{output_text}");
    }
}

fn read_file_to_string_with_capacity(path: &Path) -> std::io::Result<String> {
    let mut file = fs::File::open(path)?;
    let cap = file.metadata().ok().and_then(|m| usize::try_from(m.len()).ok()).unwrap_or(0);
    let mut s = String::with_capacity(cap.saturating_add(1));
    file.read_to_string(&mut s)?;
    Ok(s)
}

fn resolve_output_path(input_path: &Path, output: Option<&OsStr>) -> PathBuf {
    match output {
        None => input_path.with_file_name(format!(
            "{}-deob.js",
            input_path
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("output")
        )),
        Some(v) if v.is_empty() => input_path.with_file_name(format!(
            "{}-deob.js",
            input_path
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("output")
        )),
        Some(v) => PathBuf::from(v),
    }
}
