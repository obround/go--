mod analysis;
mod ast;
mod codegen;
mod errors;
mod lexer;
mod parser;
mod tir;
mod token;

use analysis::Analyzer;
use codegen::Codegen;
use inkwell::context::Context;
use parser::Parser;
use std::env;
use std::fs;
use std::path;
use std::path::Path;
use std::process::{self, Command};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("usage: {} [--emit-llvm] <file.go> [output]", args[0]);
        process::exit(1);
    }

    let emit_llvm = args.iter().any(|a| a == "--emit-llvm");
    let file_args: Vec<&String> = args
        .iter()
        .skip(1)
        .filter(|a| *a != "--emit-llvm")
        .collect();

    if file_args.is_empty() {
        eprintln!("usage: {} [--emit-llvm] <file.go> [output]", args[0]);
        process::exit(1);
    }

    let file_path = file_args[0];
    let output_path = file_args
        .get(1)
        .map(|s| Path::new(s.as_str()).to_path_buf())
        .unwrap_or_else(|| {
            let p = Path::new(file_path);
            p.with_extension("")
        });

    let source = match fs::read_to_string(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("{}: {}", file_path, e);
            process::exit(1);
        }
    };

    let mut parser = Parser::new(&source, file_path);
    let ast = match parser.parse_source_file() {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{}", e);
            process::exit(1);
        }
    };

    let analyzer = Analyzer::new(&ast, file_path);
    let result = match analyzer.analyze() {
        Ok(res) => res,
        Err(errors) => {
            let path = Path::new(file_path);
            let display_path = if path.is_absolute() {
                format!(
                    "./{}",
                    path.file_name().unwrap_or_default().to_string_lossy()
                )
            } else {
                file_path.to_string()
            };

            for err in &errors {
                eprintln!(
                    "{}:{}:{}: {}",
                    display_path, err.span.line, err.span.col, err.message
                );
                if let Some(related) = &err.related {
                    eprintln!(
                        "\t{}:{}:{}: {}",
                        display_path, related.span.line, related.span.col, related.message
                    );
                }
            }
            process::exit(1);
        }
    };

    // Code generation phase
    let context = Context::create();
    let module_name = Path::new(file_path)
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");

    let mut codegen = Codegen::new(&context, result.types, result.universe, module_name);
    codegen.compile(&result.program);

    if emit_llvm {
        // Write LLVM IR to file
        let ir_path = output_path.with_extension("ll");
        if let Err(e) = codegen.write_ir(&ir_path) {
            eprintln!("Failed to write LLVM IR: {}", e);
            process::exit(1);
        }
        println!("Wrote LLVM IR to {}", ir_path.display());
    } else {
        // Write object file
        let obj_path = output_path.with_extension("o");
        if let Err(e) = codegen.write_object(&obj_path) {
            eprintln!("Failed to write object file: {}", e);
            process::exit(1);
        }

        let runtime_dir = find_runtime_dir();

        // Link with clang
        let status = Command::new("clang")
            .arg(&obj_path)
            .arg("-o")
            .arg(&output_path)
            .arg(format!("-L{}", runtime_dir.display()))
            .arg("-L/opt/homebrew/lib")
            .arg("-lgoruntime")
            .arg("-lgc")
            .status();

        match status {
            Ok(s) if s.success() => {
                // Clean up object file
                let _ = fs::remove_file(&obj_path);
            }
            Ok(s) => {
                eprintln!("Linking failed with exit code: {:?}", s.code());
                eprintln!("Object file preserved at: {}", obj_path.display());
                eprintln!(
                    "Manual linking: clang {} -o {} -L{} -lgoruntime -lgc",
                    obj_path.display(),
                    output_path.display(),
                    runtime_dir.display()
                );
                process::exit(1);
            }
            Err(e) => {
                eprintln!("Failed to run clang: {}", e);
                eprintln!("Object file preserved at: {}", obj_path.display());
                eprintln!(
                    "Manual linking: clang {} -o {} -L{} -lgoruntime -lgc",
                    obj_path.display(),
                    output_path.display(),
                    runtime_dir.display()
                );
                process::exit(1);
            }
        }
    }
}

/// Find the runtime library directory
fn find_runtime_dir() -> path::PathBuf {
    if let Ok(exe) = env::current_exe()
        && let Some(dir) = exe.parent()
    {
        let runtime = dir.join("runtime");
        if runtime.exists() {
            return runtime;
        }
        let runtime = dir.join("../runtime");
        if runtime.exists() {
            return runtime;
        }
    }

    let cwd_runtime = Path::new("runtime");
    if cwd_runtime.exists() {
        return cwd_runtime.to_path_buf();
    }

    Path::new("runtime").to_path_buf()
}
