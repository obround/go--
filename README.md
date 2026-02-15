# go--
The go-- compiler targets a significant subset of the Go programming language. Validates Go programs and produces native executables.

You can find the first version of this compiler (this is the third) here: https://github.com/obround/go.rs.

## Build and Usage

### Requirements

- Rust toolchain (latest)
- Clang
- LLVM 21
- Make
- Bohem GC

### Building
Clone the repository and run `make`:

```bash
git clone https://github.com/obround/go--
cd go--
make
```

This will create an executable `bin/go--`.

### Running

To compile and run a Go source file:

```bash
/bin/go-- examples/mandelbrot.go
./examples/mandelbrot.go
```

You can also inspect the generated LLVM IR using the `--emit-llvm` option.

## Examples

### Mandelbrot

<img width="913" height="569" alt="image" src="https://github.com/user-attachments/assets/469368f7-33af-418b-b6c7-059caf7e3405" />

## Ray Tracer

```bash
/bin/go-- examples/rayTracer.go
./examples/rayTracer > image.ppm
```

<img width="1320" height="657" alt="image" src="https://github.com/user-attachments/assets/a4b812bb-f64e-43bc-a835-59f6197bd205" />

## Donut.c & Endoh's Fluid Simulation

```bash
bin/go-- examples/donut.go
bin/go-- examples/fluidSim.go

./examples/donut
./examples/fluidSim
```

![donu](https://github.com/user-attachments/assets/41f6bc25-ced2-409d-b45d-661391d8565f) ![fluidsim](https://github.com/user-attachments/assets/836f55a4-a454-499d-8831-709179e74696)

## Language Support

The go-- compiler supports a respectable subset of Go programs with many constructs: structs, methods, slices, arrays, maps, etc.

However, it notably does not support (yet): Interfaces, packages, concurrency, multi-file code, and some syntactic sugars

## How it works

```mermaid
flowchart LR
    A[Source .go] --> C(Lexer / Parser)
    C --> D[AST]
    D --> E(Semantic Check / Lowering)
    E --> F[Go-- IR]
    F --> G(LLVM Codegen)
    G --> H[Native Binary]
```

I am planning on rewriting the analysis phase to separate semantic checking and lowering -- this should make for significantly cleaner code. I also plan to push some of my local tests
for both the semantic checking and code generation phases.

## License

MIT
