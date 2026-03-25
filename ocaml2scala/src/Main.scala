import scala.io.Source
import java.io.File
import java.io.PrintWriter
import scala.sys.process._

object Main {

  def main(args: Array[String]): Unit = {
    val inputFile = if (args.nonEmpty) args(0) else "input.txt"
    val scalaOutputFile = "output.scala"
    val textOutputFile = "translated_code.txt"

    try {
      val ocamlCode = readFile(inputFile)
      val scalaCode = translateOcamlToScala(ocamlCode)

      writeFile(scalaOutputFile, scalaCode)
      writeFile(textOutputFile, "// Translated from OCaml code\n\n" + scalaCode)

      println("Generated Scala code:")
      println(scalaCode)
      println()

      compileScalaFile(scalaOutputFile)
    } catch {
      case e: Exception =>
        println("An error happened: " + e.getMessage)
    }
  }

  def readFile(path: String): String = {
    val source = Source.fromFile(path)
    try {
      source.mkString
    } finally {
      source.close()
    }
  }

  def writeFile(path: String, content: String): Unit = {
    val writer = new PrintWriter(new File(path))
    try {
      writer.write(content)
    } finally {
      writer.close()
    }
  }

  def compileScalaFile(path: String): Unit = {
    val outputBuffer = new StringBuilder
    val logger = ProcessLogger(
      line => outputBuffer.append(line).append("\n"),
      line => outputBuffer.append(line).append("\n")
    )

    try {
      val exitCode = Seq("scalac", path).!(logger)

      if (exitCode == 0) {
        println("Compilation successful")
      } else {
        println("Compilation failed")
        println("Compiler output:")
        println(outputBuffer.toString())
      }
    } catch {
      case e: Exception =>
        println("Could not run scalac: " + e.getMessage)
    }
  }

  def translateOcamlToScala(ocamlCode: String): String = {
    val blocks = splitIntoBlocks(ocamlCode)

    val translatedBlocks = blocks
      .map(translateBlock)
      .filter(_.nonEmpty)

    val bodyLines = translatedBlocks.zipWithIndex.flatMap {
      case (block, index) =>
        if (index == translatedBlocks.size - 1) block
        else block ++ List("")
    }

    val objectLines =
      List("object TranslatedCode {") ++
        bodyLines.map("  " + _) ++
        List("}")

    objectLines.mkString("\n")
  }

  // Split the file into top-level pieces such as one function per block.
  def splitIntoBlocks(code: String): List[List[String]] = {
    val rawLines = code.replace("\r", "").split("\n").toList
    val blocks = scala.collection.mutable.ListBuffer[List[String]]()
    var current = scala.collection.mutable.ListBuffer[String]()

    for (line <- rawLines) {
      val trimmed = line.trim
      val startsNewTopLevelBlock =
        trimmed.startsWith("let ") &&
          !line.startsWith(" ") &&
          !line.startsWith("\t") &&
          current.nonEmpty

      if (trimmed.isEmpty) {
        if (current.nonEmpty) {
          blocks += current.toList
          current.clear()
        }
      } else if (startsNewTopLevelBlock) {
        blocks += current.toList
        current.clear()
        current += line
      } else {
        current += line
      }
    }

    if (current.nonEmpty) {
      blocks += current.toList
    }

    blocks.toList
  }

  def translateBlock(block: List[String]): List[String] = {
    val header = block.head.trim

    if (header == "let _ = main ()") {
      Nil
    } else if (header.startsWith("let rec ")) {
      translateRecursiveBlock(block)
    } else if (header.startsWith("let main () =")) {
      translateMainBlock(block)
    } else if (header.startsWith("let ")) {
      translateNormalLetBlock(block)
    } else {
      List("// Could not translate block: " + header)
    }
  }

  def translateRecursiveBlock(block: List[String]): List[String] = {
    val header = block.head.trim
    val pattern = """let rec ([a-zA-Z_]\w*) ([a-zA-Z_]\w*) =""".r

    header match {
      case pattern(name, param) =>
        val body = block.tail.map(_.trim).filter(_.nonEmpty).flatMap(translateBodyLine)
        List(s"def $name($param: Int): Int = {") ++
          body.map("  " + _) ++
          List("}")
      case _ =>
        List("// Could not translate recursive function: " + header)
    }
  }

  def translateMainBlock(block: List[String]): List[String] = {
    val body = block.tail.map(_.trim).filter(_.nonEmpty).flatMap(translateMainLine)

    List("def main(args: Array[String]): Unit = {") ++
      body.map("  " + _) ++
      List("}")
  }

  def translateNormalLetBlock(block: List[String]): List[String] = {
    val header = block.head.trim
    val functionWithExpr = """let ([a-zA-Z_]\w*) ([a-zA-Z_]\w*) = (.+)""".r
    val functionHeaderOnly = """let ([a-zA-Z_]\w*) ([a-zA-Z_]\w*) =""".r
    val valuePattern = """let ([a-zA-Z_]\w*) = (.+)""".r

    header match {
      case functionWithExpr(name, param, expr) =>
        List(s"def $name($param: Int): Int = ${translateExpression(expr)}")

      case functionHeaderOnly(name, param) =>
        val body = block.tail.map(_.trim).filter(_.nonEmpty).flatMap(translateBodyLine)
        List(s"def $name($param: Int): Int = {") ++
          body.map("  " + _) ++
          List("}")

      case valuePattern(name, expr) =>
        List(s"val $name = ${translateExpression(expr)}")

      case _ =>
        List("// Could not translate let binding: " + header)
    }
  }

  def translateBodyLine(line: String): List[String] = {
    if (line.startsWith("if ")) {
      List(translateIfLine(line))
    } else if (line.startsWith("else ")) {
      List("else " + translateExpression(line.stripPrefix("else ").trim))
    } else if (line.startsWith("match ")) {
      translateMatchLine(line)
    } else {
      List(translateExpression(line))
    }
  }

  def translateMainLine(line: String): List[String] = {
    if (line.startsWith("let ")) {
      List(translateLocalLet(line))
    } else if (line.startsWith("Printf.printf")) {
      List(translatePrintf(line))
    } else if (line.startsWith("print_endline")) {
      List(translatePrintEndline(line))
    } else if (line.startsWith("if ")) {
      List(translateIfLine(line))
    } else {
      List(translateExpression(line))
    }
  }

  def translateLocalLet(line: String): String = {
    val withInPattern = """let ([a-zA-Z_]\w*) = (.+) in""".r
    val plainPattern = """let ([a-zA-Z_]\w*) = (.+)""".r

    line.trim match {
      case withInPattern(name, expr) =>
        s"val $name = ${translateExpression(expr)}"
      case plainPattern(name, expr) =>
        s"val $name = ${translateExpression(expr)}"
      case _ =>
        "// Could not translate local let: " + line.trim
    }
  }

  def translateIfLine(line: String): String = {
    val pattern = """if (.+) then (.+)""".r

    line.trim match {
      case pattern(condition, expr) =>
        s"if (${translateExpression(condition)}) ${translateExpression(expr)}"
      case _ =>
        "// Could not translate if expression: " + line.trim
    }
  }

  // Optional simple pattern matching support.
  def translateMatchLine(line: String): List[String] = {
    val pattern = """match (.+) with""".r

    line.trim match {
      case pattern(value) =>
        List(s"${translateExpression(value)} match {", "  // Add case lines here if needed", "}")
      case _ =>
        List("// Could not translate match expression: " + line.trim)
    }
  }

  def translatePrintEndline(line: String): String = {
    val content = line.trim.stripPrefix("print_endline").trim
    "println(" + translateExpression(content) + ")"
  }

  def translatePrintf(line: String): String = {
    val cleaned = cleanLineEnding(line.trim)
    val pattern = """Printf\.printf "([^"]*)"(.+)""".r

    cleaned match {
      case pattern(formatText, rawArgs) =>
        val args = splitArguments(rawArgs.trim).map(arg => translateExpression(removeParentheses(arg)))
        val scalaText = buildInterpolatedString(formatText, args)
        s"""println($scalaText)"""
      case _ =>
        "// Could not translate Printf.printf: " + line.trim
    }
  }

  def splitArguments(text: String): List[String] = {
    val args = scala.collection.mutable.ListBuffer[String]()
    val current = new StringBuilder
    var depth = 0

    for (char <- text) {
      if (char == '(') {
        depth += 1
        current.append(char)
      } else if (char == ')') {
        depth -= 1
        current.append(char)
      } else if (char.isWhitespace && depth == 0) {
        if (current.nonEmpty) {
          args += current.toString()
          current.clear()
        }
      } else {
        current.append(char)
      }
    }

    if (current.nonEmpty) {
      args += current.toString()
    }

    args.toList
  }

  def buildInterpolatedString(formatText: String, args: List[String]): String = {
    val cleanedText = formatText.replace("\\n", "")
    val pieces = cleanedText.split("%d|%s", -1).toList

    if (args.isEmpty) {
      "\"" + cleanedText + "\""
    } else {
      val result = new StringBuilder("s\"")

      for (index <- pieces.indices) {
        result.append(pieces(index))

        if (index < args.length) {
          val arg = args(index)
          if (isSimpleName(arg)) {
            result.append("$").append(arg)
          } else {
            result.append("${").append(arg).append("}")
          }
        }
      }

      result.append("\"")
      result.toString()
    }
  }

  def isSimpleName(text: String): Boolean = {
    text.matches("""[a-zA-Z_]\w*""")
  }

  def translateExpression(expr: String): String = {
    val cleaned = cleanLineEnding(expr.trim)
    val withPrint = cleaned.replace("print_endline", "println")
    val withBooleanTrue = withPrint.replace("true", "true")
    val withBooleanFalse = withBooleanTrue.replace("false", "false")
    translateFunctionCalls(withBooleanFalse)
  }

  // Change OCaml calls like "factorial (n - 1)" into Scala calls like "factorial(n - 1)".
  def translateFunctionCalls(text: String): String = {
    var result = text
    var changed = true

    while (changed) {
      val updated = result.replaceAll("""([a-zA-Z_]\w*) \(([^()]+)\)""", "$1($2)")
      changed = updated != result
      result = updated
    }

    result
  }

  def cleanLineEnding(line: String): String = {
    line.trim.stripSuffix(";").trim
  }

  def removeParentheses(text: String): String = {
    text.trim.stripPrefix("(").stripSuffix(")")
  }
}
