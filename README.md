# ocaml2scala

Small Scala project for a university assignment. It reads simple OCaml code from a `.txt` file, translates it into Scala, writes the result to output files, and tries to compile the generated Scala code with `scalac`.

## Project structure

```text
src/
| -- Main.scala
input.txt
 build.sbt
README.md
```

## What it does

The program:

1. Reads an OCaml file from the command line, or uses `input.txt` if no path is given.
2. Translates some common OCaml constructs into simple Scala.
3. Saves the generated code to:
   - `output.scala`
   - `translated_code.txt`
4. Runs `scalac output.scala` to check whether the translated code compiles.
5. Prints whether compilation was successful or shows the compiler errors.

## Supported translations

- `let` bindings to `val`
- `let rec` to recursive `def`
- `if ... then ... else` to Scala `if (...) ... else ...`
- `print_endline` to `println`
- simple `Printf.printf` cases to Scala string interpolation
- simple arithmetic and function calls

This translator is intentionally small and beginner-friendly, so it does not support all OCaml syntax.

## How to run

```bash
scalac src/Main.scala
scala Main
```

Note: these commands require Scala tools to be installed and available in your terminal path.

Or with a custom input file:

```bash
scala Main my_ocaml_code.txt
```
