# Main.scala Documentation

This document explains how [`Main.scala`](../src/main/scala/Main.scala) works, what each helper does, and what kind of OCaml input the translator expects.

## General Purpose

`Main.scala` implements a small OCaml-to-Scala translator. Its job is to:

1. Read OCaml-like source code from a text file.
2. Convert a limited subset of that code into Scala syntax.
3. Wrap the translated result inside a Scala object called `TranslatedCode`.
4. Save the generated code into output files.
5. Try to compile the generated file with `scalac`, or fall back to Scala CLI if needed.

The implementation is intentionally simple. It does not parse OCaml with a formal grammar. Instead, it uses line processing, regular expressions, and helper functions tailored to a small set of supported language patterns.

## Imports And Their Role

[`Main.scala`](../src/main/scala/Main.scala) imports four modules:

- `scala.io.Source`: used to read the input file.
- `java.io.File`: used when creating output files.
- `java.io.PrintWriter`: used to write translated code to disk.
- `scala.sys.process._`: used to execute a compiler command from inside Scala, preferring `scalac` and falling back to Scala CLI.

## High-Level Execution Flow

The main pipeline lives in `main(args: Array[String])`. The steps are:

1. Determine the input file path.
2. Read the source code from disk.
3. Translate the OCaml text into Scala text.
4. Write two output files.
5. Print the generated Scala code to the terminal.
6. Run the Scala compiler on the generated file.
7. Catch and report any unexpected exception.

In practice, the program behaves like a batch translator: it reads the whole file, transforms it as text, and writes the full result at once.

## Step-By-Step Through `main`

### 1. Input Selection

Inside `main`, the first line chooses the source file:

- If the program receives a command-line argument, that argument is used as the input path.
- If no argument is provided, it falls back to `input.txt`.

This means the program supports both:

- a default workflow for quick testing
- a custom workflow for translating any other text file

### 2. Output File Names

Two output paths are hardcoded inside the `generated/` directory:

- `generated/output.scala`
- `generated/translated_code.txt`

They store similar content, but with a small difference:

- `generated/output.scala` contains pure generated Scala code.
- `generated/translated_code.txt` contains the same generated code prefixed with a comment header.

### 3. Reading The OCaml Source

`main` calls `readFile(inputFile)`.

`readFile`:

- opens the file with `Source.fromFile(path)`
- reads its entire contents with `mkString`
- closes the source inside a `finally` block

Using `finally` is important because it guarantees the file handle is closed even if reading fails.

### 4. Translating The Source

After reading the file, `main` calls:

```scala
val scalaCode = translateOcamlToScala(ocamlCode)
```

This is the core transformation stage. The translator does not work token by token. Instead, it:

1. splits the input into top-level blocks
2. translates each block separately
3. joins the results into a Scala object

This design keeps the implementation short and easier to understand for an academic project.

### 5. Writing Output Files

`main` writes the translated code twice using `writeFile`.

`writeFile`:

- creates a `PrintWriter`
- writes the string content
- closes the writer in a `finally` block

Again, `finally` ensures the writer is closed correctly.

### 6. Printing The Generated Code

The program prints:

- `"Generated Scala code:"`
- the translated Scala content

This gives immediate feedback before compilation.

### 7. Compiling The Generated File

`main` finishes by calling `compileScalaFile(scalaOutputFile, compiledOutputDir)`.

This step does not compile `Main.scala` itself. It compiles the generated `generated/output.scala` file that came from the OCaml translation and sends the compiled classes to `generated/classes/` when `scalac` is available.

### 8. Error Handling

The whole `main` body is wrapped in a `try/catch`.

If any unexpected problem happens, the program prints:

```text
An error happened: ...
```

This covers cases such as:

- missing input files
- read/write failures
- unexpected runtime exceptions during translation

## Translation Pipeline In Detail

### `translateOcamlToScala`

This function is the top-level translator. It coordinates the full conversion.

Its internal flow is:

1. Call `splitIntoBlocks(ocamlCode)` to separate the input into logical units.
2. Translate each block with `translateBlock`.
3. Discard empty translations with `.filter(_.nonEmpty)`.
4. Insert blank lines between translated blocks.
5. Indent every generated line by two spaces.
6. Wrap everything inside:

```scala
object TranslatedCode {
  ...
}
```

The return value is a single `String` representing the complete Scala source file.

### Why The Translator Uses Blocks

OCaml source often contains several top-level declarations, for example:

```ocaml
let double x = x * 2

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let main () =
  ...
```

Instead of translating line by line without context, the program groups related lines first. That is especially useful for:

- multi-line recursive functions
- a multi-line `main` function
- top-level values or functions separated by blank lines

## How `splitIntoBlocks` Works

`splitIntoBlocks(code: String): List[List[String]]` transforms the raw file into a list of blocks, where each block is a list of lines.

### Internal Logic

1. Normalize line endings by removing `\r`.
2. Split the file by newline into `rawLines`.
3. Iterate line by line while building a mutable `current` block.
4. End the current block when:
   - a blank line is found, or
   - a new top-level `let` begins while another block is already being built
5. Add the last block when iteration finishes.

### What Counts As A New Top-Level Block

A line starts a new top-level block when:

- the trimmed line starts with `let `
- the line is not indented with spaces or tabs
- there is already content in `current`

This rule helps distinguish:

- top-level definitions such as `let double x = ...`
- nested or body lines such as `  if n <= 1 then 1`

### Output Shape

For the sample input, the function effectively creates blocks similar to:

1. `let double x = x * 2`
2. `let rec factorial n = ...`
3. `let main () = ...`
4. `let _ = main ()`

## Block Dispatch With `translateBlock`

`translateBlock(block: List[String])` decides which specialized translator should handle a block.

It inspects the first line, called `header`, and applies the following rules:

- `let _ = main ()` -> ignore it by returning `Nil`
- `let rec ...` -> use `translateRecursiveBlock`
- `let main () =` -> use `translateMainBlock`
- `let ...` -> use `translateNormalLetBlock`
- anything else -> generate a comment explaining that the block could not be translated

This function is the main dispatcher of the translator.

## Recursive Function Translation

### `translateRecursiveBlock`

This function handles blocks whose first line starts with `let rec`.

It expects a very specific header format:

```ocaml
let rec functionName parameter =
```

The regex used is:

```scala
"""let rec ([a-zA-Z_]\w*) ([a-zA-Z_]\w*) =""".r
```

This means the translator currently assumes:

- exactly one function name
- exactly one parameter
- simple identifier names

### Successful Translation

If the header matches, the function:

1. takes the body lines from `block.tail`
2. trims and removes empty lines
3. translates each body line with `translateBodyLine`
4. emits Scala code with this fixed signature:

```scala
def name(param: Int): Int = {
  ...
}
```

Notice the strong assumption: both the parameter type and return type are always `Int`.

### Fallback Behavior

If the header does not match the expected pattern, the function emits a comment such as:

```scala
// Could not translate recursive function: ...
```

## Main Function Translation

### `translateMainBlock`

This function handles blocks whose header is:

```ocaml
let main () =
```

It translates the remaining lines with `translateMainLine` and emits:

```scala
def main(args: Array[String]): Unit = {
  ...
}
```

This gives the generated Scala object an entry point compatible with standard Scala execution.

### Why `translateMainLine` Is Separate

The body of `main` usually contains side effects such as:

- local variable declarations
- printing
- conditional statements

Those patterns are a little different from the expressions used in function bodies, so `Main.scala` uses a dedicated helper for them.

## Normal `let` Translation

### `translateNormalLetBlock`

This function handles non-recursive `let` blocks and supports three cases.

### Case 1: Single-Line Function With Expression

Expected form:

```ocaml
let double x = x * 2
```

Translated form:

```scala
def double(x: Int): Int = x * 2
```

### Case 2: Function Header Followed By A Multi-Line Body

Expected form:

```ocaml
let f x =
  ...
```

Translated form:

```scala
def f(x: Int): Int = {
  ...
}
```

Again, `Int` is hardcoded for both parameter and return type.

### Case 3: Value Binding

Expected form:

```ocaml
let x = 5
```

Translated form:

```scala
val x = 5
```

### Fallback

If the header does not match any supported regex, the translator emits a comment noting the failure.

## Translating Body Lines

### `translateBodyLine`

This helper is used mainly inside translated function bodies.

It checks the beginning of each line and delegates as follows:

- `if ...` -> `translateIfLine`
- `else ...` -> translate the expression after `else`
- `match ... with` -> `translateMatchLine`
- anything else -> `translateExpression`

The result type is `List[String]` instead of `String` because some source constructs may expand into multiple output lines.

## Translating Lines Inside `main`

### `translateMainLine`

This helper is similar to `translateBodyLine`, but tuned for imperative statements that often appear inside the source `main` block.

It recognizes:

- local `let` declarations -> `translateLocalLet`
- `Printf.printf` -> `translatePrintf`
- `print_endline` -> `translatePrintEndline`
- `if ... then ...` -> `translateIfLine`
- anything else -> `translateExpression`

This is why the generated `main` method can contain `val` assignments and `println(...)` calls.

## Local Variables Inside `main`

### `translateLocalLet`

This function translates local bindings such as:

```ocaml
let x = 5 in
```

or

```ocaml
let x = 5
```

Both become:

```scala
val x = 5
```

The optional trailing `in` is simply ignored after extracting the expression.

If the pattern does not match, the function returns a comment instead of throwing an error.

## Conditional Translation

### `translateIfLine`

This function expects the compact OCaml shape:

```ocaml
if condition then expression
```

It translates that into:

```scala
if (condition) expression
```

Examples:

- `if n <= 1 then 1` -> `if (n <= 1) 1`
- `if x > 0 then print_endline "ok"` -> `if (x > 0) println("ok")`

### Important Limitation

The translator handles `else` separately in `translateBodyLine`. This works for the current sample style:

```ocaml
if n <= 1 then 1
else n * factorial (n - 1)
```

but it is still a text-based strategy, not a full parser. More complex nested conditionals may require additional work.

## Pattern Matching Support

### `translateMatchLine`

This function recognizes lines like:

```ocaml
match value with
```

and emits a placeholder Scala `match` block:

```scala
value match {
  // Add case lines here if needed
}
```

This means pattern matching support is only partial. The translator can start the structure, but it does not currently translate individual OCaml `| case -> expr` lines.

## Printing Support

### `translatePrintEndline`

This helper converts:

```ocaml
print_endline something
```

into:

```scala
println(something)
```

The content after `print_endline` is passed through `translateExpression`, so function calls or other small transformations can still happen.

### `translatePrintf`

This helper supports a narrow version of:

```ocaml
Printf.printf "..." args...
```

Its job is to convert simple OCaml formatted output into Scala string interpolation.

For example:

```ocaml
Printf.printf "Double of %d is %d\n" x (double x)
```

becomes:

```scala
println(s"Double of $x is ${double(x)}")
```

### Internal Steps Of `translatePrintf`

1. Clean trailing semicolons with `cleanLineEnding`.
2. Extract the format string and the raw argument text using a regex.
3. Split the arguments safely with `splitArguments`.
4. Remove wrapping parentheses from each argument when needed.
5. Translate each argument expression.
6. Build the Scala interpolated string with `buildInterpolatedString`.
7. Wrap the result in `println(...)`.

### Why `splitArguments` Exists

A simple whitespace split would fail for cases like:

```ocaml
(double x)
```

because the argument contains spaces inside parentheses.

`splitArguments` keeps track of parenthesis depth, so it only splits on spaces that occur at depth `0`. This is a useful small parsing trick inside an otherwise regex-driven program.

## Expression Translation

### `translateExpression`

This helper performs small, general-purpose text conversions for expressions.

Current behavior:

1. Remove trailing semicolons with `cleanLineEnding`.
2. Replace `print_endline` with `println`.
3. Pass the result through `translateFunctionCalls`.

You may notice that the code also replaces `true` with `true` and `false` with `false`. Those steps currently have no effect, but they suggest the author intended to keep boolean translation logic explicit or leave room for future extension.

### `translateFunctionCalls`

OCaml often writes function application like this:

```ocaml
factorial (n - 1)
```

Scala requires:

```scala
factorial(n - 1)
```

`translateFunctionCalls` repeatedly applies the regex:

```scala
([a-zA-Z_]\w*) \(([^()]+)\)
```

until no more changes occur.

This repeated loop is useful because replacing one call can expose another call that also needs conversion.

### Helper Utilities

`Main.scala` also defines three small helper utilities:

- `cleanLineEnding(line)`: trims whitespace and removes a trailing `;`
- `removeParentheses(text)`: removes one outer pair of parentheses
- `isSimpleName(text)`: checks whether a string is a simple identifier

These helpers make the higher-level translation functions easier to read.

## How Interpolated Strings Are Built

### `buildInterpolatedString`

This function turns OCaml `Printf.printf` format strings into Scala interpolated strings.

It currently supports `%d` and `%s`.

Example input:

- format text: `"Double of %d is %d\n"`
- arguments: `List("x", "double(x)")`

Internal process:

1. Remove `\n` from the format text.
2. Split the format string around `%d` and `%s`.
3. Reconstruct the message as a Scala `s"..."` string.
4. Insert simple identifiers as `$name`.
5. Insert more complex expressions as `${expression}`.

That distinction is handled by `isSimpleName`.

## Compilation Stage

### `compileScalaFile`

After translation, the program tries to compile the generated Scala file.

This function:

1. creates a `StringBuilder` called `outputBuffer`
2. creates a `ProcessLogger` that collects both standard output and error output
3. tries:

```scala
Seq("scalac", "-d", outputDir, path).!(logger)
```

If `scalac` is not available on the machine, the program falls back to:

```scala
Seq("scala", "compile", "--scala-version", "2.13.14", path).!(logger)
```

4. checks the returned exit code
5. prints success or failure information

### Success Path

If `exitCode == 0`, the program prints:

```text
Compilation successful
```

### Failure Path

If the compiler returns a non-zero status, the program prints:

- `Compilation failed`
- `Compiler output:`
- the captured compiler messages

### Missing Compiler

If neither `scalac` nor Scala CLI can be executed, the program prints a short message explaining that no Scala compiler command was available.

## Example Using The Current Sample

With the current [`input.txt`](../input.txt), the translator reads:

```ocaml
let double x = x * 2

let rec factorial n =
  if n <= 1 then 1
  else n * factorial (n - 1)

let main () =
  let x = 5 in
  Printf.printf "Double of %d is %d\n" x (double x);
  Printf.printf "Factorial of %d is %d\n" x (factorial x)

let _ = main ()
```

The generated [`generated/output.scala`](../generated/output.scala) is:

```scala
object TranslatedCode {
  def double(x: Int): Int = x * 2

  def factorial(n: Int): Int = {
    if (n <= 1) 1
    else n * factorial(n - 1)
  }

  def main(args: Array[String]): Unit = {
    val x = 5
    println(s"Double of $x is ${double(x)}")
    println(s"Factorial of $x is ${factorial(x)}")
  }
}
```

This example shows nearly every major stage of the translator:

- single-line function translation
- recursive function translation
- local `let` translation
- `Printf.printf` conversion
- OCaml function-call syntax normalization

## Important Design Assumptions

The current implementation makes several simplifying assumptions:

- many translated functions use exactly one parameter
- most generated function signatures assume `Int` inputs and `Int` outputs
- block structure depends on blank lines and top-level `let`
- several features are recognized by regex instead of real parsing
- unsupported constructs are often turned into comments instead of causing a crash

These assumptions are useful for a classroom prototype because they keep the code readable, but they also limit the range of valid OCaml input.

## Current Limitations

The translator is intentionally narrow. Some important limitations are:

- no full OCaml parser or AST
- limited support for nested expressions
- limited support for multi-parameter functions
- no full translation of `match` cases
- hardcoded Scala types in many generated functions
- dependence on line layout and indentation style
- generated code is wrapped in a single object with a fixed name

## Why This Design Still Makes Sense

For a small compiler or translator assignment, this design has practical advantages:

- the control flow is easy to follow
- each transformation step is small and testable
- the student can demonstrate understanding of source-to-source translation
- the output is concrete and immediately verifiable through `scalac`

In other words, `Main.scala` is less like a production compiler and more like a focused educational translator that demonstrates the core idea of reading source text, transforming syntax, and validating the generated result.
