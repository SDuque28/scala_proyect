# ocaml2scala

`ocaml2scala` is a small Scala university project that translates a limited subset of OCaml-like code into Scala, writes the generated source to disk, and then tries to compile that generated file.

## What It Does

The entry point is [`src/main/scala/Main.scala`](./src/main/scala/Main.scala). When you run it, it:

1. reads an OCaml-like input file, defaulting to `input.txt`
2. translates supported declarations and expressions into Scala
3. wraps the result inside `object TranslatedCode`
4. writes generated files into `generated/`
5. validates the generated Scala file with `scalac`, or falls back to Scala CLI when `scalac` is not installed
6. runs the generated Scala program automatically
7. prints the translated code and the generated program output

## Supported OCaml Constructs

The translator currently handles:

- top-level `let` value declarations
- single-parameter functions defined with `let`
- recursive functions defined with `let rec`
- typed function headers such as `let f (x : int) : int = ...`
- `let main () = ...` and `let main () : unit = ...` blocks converted into Scala `main`
- `let _ = main ()` and `let () = main ()` entrypoints
- local `let ... in` assignments inside `main`
- mutable refs via `ref`, `!value`, and `:=`
- `while ... do ... done` loops inside `main`
- `if ... then ...` expressions and simple `else` lines
- `print_endline` translated to `println`
- string concatenation with `^`
- common OCaml helpers such as `string_of_int`
- basic `Printf.printf` calls translated to Scala string interpolation
- OCaml function application such as `factorial (n - 1)`, `double x`, or `mostrarMensaje "hola"`

Because the implementation is regex-based and line-oriented, it works best with simple, well-structured input similar to the sample file in this repository.

## Clean Project Layout

```text
src/main/scala/Main.scala
docs/DOCUMENTATION.md
input.txt
build.sbt
README.md
generated/                # created when you run the translator
target/                   # created by sbt, ignored
```

## How To Run

Preferred workflow with SBT:

```bash
sbt --error run
```

Run with a custom input file:

```bash
sbt --error "run my_ocaml_code.txt"
```

If you prefer Scala CLI:

```bash
scala run src/main/scala/Main.scala
scala run src/main/scala/Main.scala -- my_ocaml_code.txt
```

`sbt --error` keeps the SBT output much quieter so you mostly see your program's output.

## How To Clean Generated Files

Remove SBT build output:

```bash
sbt clean
```

Remove translator-generated files:

```bash
Remove-Item -Recurse -Force generated
```

The repo now ignores generated compiler output, Scala CLI cache folders, and SBT `target/` directories through [`.gitignore`](./.gitignore).

## Output Files

After execution, the translator creates:

- `generated/output.scala`: generated Scala source
- `generated/translated_code.txt`: the same source with a short header comment
- `generated/classes/`: compiled classes produced by `scalac -d`

## Testing Status

There are currently no automated tests in the project. The simplest verification flow is:

```bash
sbt run
```

or:

```bash
scala run src/main/scala/Main.scala
```

If the generated code compiles, the program prints `Compilation successful`.

## Additional Documentation

For a deeper explanation of the translator internals and helper functions, see [`docs/DOCUMENTATION.md`](./docs/DOCUMENTATION.md).
