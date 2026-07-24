use std::{
    ffi::OsStr,
    ffi::OsString,
    fs,
    path::{Path, PathBuf},
    process,
};

use std::time::Duration;

use clap::{ArgGroup, Parser as ClapParser};
use memmap2::{Mmap, MmapOptions};
use oxc_span::SourceType;

use restringer_rs::{DeobfuscateOptions, Engine, Restringer, set_default_engine};

#[derive(Debug, ClapParser)]
#[command(
    name = "restringer",
    about = "REstringer - a JavaScript deobfuscator (Rust rewrite)"
)]
#[command(group(
    ArgGroup::new("verbosity")
        .args(["quiet", "verbose"])
        .multiple(false)
))]
struct Cli {
    /// The obfuscated JS/TS file
    input_filename: PathBuf,

    /// Remove dead nodes from script after deobfuscation is complete (unsafe_transforms)
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

    /// Abort deobfuscation after N seconds (disabled by default)
    #[arg(long = "timeout-seconds")]
    timeout_seconds: Option<u64>,

    /// Disable unsafe transforms (they are enabled by default)
    #[arg(long = "no-unsafe")]
    no_unsafe: bool,

    /// Unsafe transform engine to use: deno or quickjs (default: deno)
    #[arg(long = "unsafe-engine", default_value = "deno")]
    unsafe_engine: String,

    /// Number of spaces to use per indentation level instead of tabs (default: disabled, use tabs)
    #[arg(long = "indent-spaces", num_args = 0..=1, default_missing_value = "2")]
    indent_spaces: Option<usize>,

    /// Use single quotes instead of double quotes in generated output
    #[arg(long = "single-quote")]
    single_quote: bool,

    /// Do not print comments in generated output
    #[arg(long = "no-comments")]
    no_comments: bool,
}

fn main() {
    let cli = Cli::parse();

    let engine = match cli.unsafe_engine.as_str() {
        #[cfg(feature = "unsafe-transform-quickjs")]
        "quickjs" => Engine::QuickJs,
        #[cfg(feature = "unsafe-transform-deno")]
        "deno" => Engine::Deno,
        _ => {
            eprintln!(
                "[-] Critical Error: unsupported unsafe engine '{}'. Available: deno, quickjs",
                cli.unsafe_engine
            );
            process::exit(2);
        }
    };
    set_default_engine(engine);

    if cli.quiet && cli.verbose {
        eprintln!("[-] Critical Error: Don't set both -q and -v at the same time *smh*");
        process::exit(2);
    }

    let input_path = cli.input_filename;
    let source_map = match map_input_file(&input_path) {
        Ok(map) => map,
        Err(e) => {
            eprintln!(
                "[-] Critical Error: Failed to map {}: {e}",
                input_path.display()
            );
            process::exit(1);
        }
    };
    let source_text = match std::str::from_utf8(source_map.as_deref().unwrap_or_default()) {
        Ok(source) => source,
        Err(e) => {
            eprintln!(
                "[-] Critical Error: Failed to decode {} as UTF-8: {e}",
                input_path.display()
            );
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
        if let Some(s) = cli.timeout_seconds {
            eprintln!("[!] Timeout: {s}s");
        }
        if cli.clean {
            eprintln!("[!] Clean enabled (no-op in Milestone 1)");
        }
    }

    let mut restringer = Restringer::default();
    if let Some(m) = cli.max_iterations {
        restringer.set_max_iterations(m);
    }
    if let Some(width) = cli.indent_spaces {
        restringer.set_indent_spaces(width);
    }
    if cli.single_quote {
        restringer.set_single_quote(true);
    }
    if cli.no_comments {
        restringer.set_print_comments(false);
    }
    let result = match restringer.deobfuscate(
        source_text,
        DeobfuscateOptions {
            clean: cli.clean,
            run_unsafe: !cli.no_unsafe,
            max_iterations: cli.max_iterations,
            timeout: cli.timeout_seconds.map(Duration::from_secs),
            source_type: Some(source_type),
            filename_for_source_type: None,
            verbose: cli.verbose,
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
            eprintln!(
                "[-] Critical Error: Failed to write {}: {e}",
                output_path.display()
            );
            process::exit(1);
        }
        if !cli.quiet {
            eprintln!("[+] Saved {}", output_path.display());
        }
    } else {
        print!("{output_text}");
    }
}

fn map_input_file(path: &Path) -> std::io::Result<Option<Mmap>> {
    let file = fs::File::open(path)?;
    if file.metadata()?.is_file() && file.metadata()?.len() == 0 {
        return Ok(None);
    }
    unsafe { MmapOptions::new().map(&file).map(Some) }
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
