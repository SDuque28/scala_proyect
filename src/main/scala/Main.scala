//> using scala 2.13.14

import scala.io.Source
import java.io.File
import java.io.PrintWriter
import java.net.URLClassLoader
import scala.sys.process._

object Main {

  def main(args: Array[String]): Unit = {
    val inputFile = if (args.nonEmpty) args(0) else "input.txt"
    val generatedDir = "generated"
    val scalaOutputFile = s"$generatedDir/output.scala"
    val textOutputFile = s"$generatedDir/translated_code.txt"
    val compiledOutputDir = s"$generatedDir/classes"

    try {
      ensureDirectoryExists(generatedDir)
      ensureDirectoryExists(compiledOutputDir)

      val ocamlCode = readFile(inputFile)
      val scalaCode = translateOcamlToScala(ocamlCode)

      writeFile(scalaOutputFile, scalaCode)
      writeFile(textOutputFile, scalaCode)

      println("Generated Scala code:")
      println(scalaCode)
      println()

      if (compileScalaFile(scalaOutputFile, compiledOutputDir)) {
        runGeneratedCode(compiledOutputDir)
      }
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

  def ensureDirectoryExists(path: String): Unit = {
    val directory = new File(path)
    if (!directory.exists()) {
      directory.mkdirs()
    }
  }

  def shellCommand(parts: String*): Seq[String] = {
    val isWindows = System.getProperty("os.name").toLowerCase.contains("win")
    if (isWindows) Seq("cmd", "/c") ++ parts
    else parts
  }

  def compileScalaFile(path: String, outputDir: String): Boolean = {
    val compilerAttempts = List(
      ("scalac", shellCommand("scalac", "-d", outputDir, path)),
      ("Scala CLI", shellCommand("scala", "compile", "--scala-version", "2.13.14", path))
    )

    val attemptResult = compilerAttempts.iterator.map {
      case (compilerName, command) =>
        val outputBuffer = new StringBuilder
        val logger = ProcessLogger(
          line => outputBuffer.append(line).append("\n"),
          line => outputBuffer.append(line).append("\n")
        )

        try {
          Some((compilerName, command.!(logger), outputBuffer.toString()))
        } catch {
          case _: Exception => None
        }
    }.collectFirst {
      case Some(result) => result
    }

    attemptResult match {
      case Some((_, 0, _)) =>
        true
      case Some((compilerName, _, output)) =>
        println(s"Compilation failed ($compilerName)")
        println("Compiler output:")
        println(output)
        false
      case None =>
        println("Could not run a Scala compiler. Install scalac or use Scala CLI.")
        false
    }
  }

  def runGeneratedCode(compiledOutputDir: String): Unit = {
    val loader = new URLClassLoader(
      Array(new File(compiledOutputDir).toURI.toURL),
      getClass.getClassLoader
    )

    try {
      val moduleClass = loader.loadClass("TranslatedCode$")
      val module = moduleClass.getField("MODULE$").get(null)
      val mainMethod = moduleClass.getMethod("main", classOf[Array[String]])

      println("Generated program output:")
      mainMethod.invoke(module, Array.empty[String].asInstanceOf[AnyRef])
    } catch {
      case e: Exception =>
        println("Could not run generated code: " + e.getMessage)
    } finally {
      loader.close()
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
    var groupingDepth = 0

    for (line <- rawLines) {
      val trimmed = line.trim
      val startsNewTopLevelBlock =
        trimmed.startsWith("let ") &&
          !line.startsWith(" ") &&
          !line.startsWith("\t") &&
          groupingDepth == 0 &&
          current.nonEmpty

      if (trimmed.isEmpty && groupingDepth == 0) {
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

      groupingDepth = math.max(0, groupingDepth + countGroupingDelta(line))
    }

    if (current.nonEmpty) {
      blocks += current.toList
    }

    blocks.toList
  }

  def translateBlock(block: List[String]): List[String] = {
    val header = block.head.trim

    if (isMainInvocation(header)) {
      Nil
    } else if (header.startsWith("let rec ")) {
      translateRecursiveBlock(block)
    } else if (isMainDefinition(header)) {
      translateMainBlock(block)
    } else if (header.startsWith("let ")) {
      translateNormalLetBlock(block)
    } else {
      List("// Could not translate block: " + header)
    }
  }

  def translateRecursiveBlock(block: List[String]): List[String] = {
    val header = block.head.trim
    parseFunctionHeader(header, isRecursive = true) match {
      case Some(signature) =>
        val body =
          bodyFromHeader(signature.inlineExpression).getOrElse(block.tail.map(_.trim).filter(_.nonEmpty))

        renderFunction(signature.name, signature.paramName, signature.paramType, signature.returnType, body)

      case None =>
        List("// Could not translate recursive function: " + header)
    }
  }

  def translateMainBlock(block: List[String]): List[String] = {
    val lines = block.tail.map(_.trim).filter(_.nonEmpty)
    val (body, _) = translateStatementSequence(lines, 0, stopAtDone = false)

    List("def main(args: Array[String]): Unit = {") ++
      body.map("  " + _) ++
      List("}")
  }

  def translateNormalLetBlock(block: List[String]): List[String] = {
    val header = block.head.trim
    val valuePattern = """let ([a-zA-Z_]\w*) = (.+)""".r

    parseFunctionHeader(header, isRecursive = false) match {
      case Some(signature) =>
        val body =
          bodyFromHeader(signature.inlineExpression).getOrElse(block.tail.map(_.trim).filter(_.nonEmpty))

        renderFunction(signature.name, signature.paramName, signature.paramType, signature.returnType, body)

      case None =>
        header match {
          case valuePattern(name, expr) =>
            List(s"val $name = ${translateExpression(expr)}")

          case _ =>
            List("// Could not translate let binding: " + header)
        }
    }
  }

  def translateBodyLine(line: String): List[String] = {
    val normalized = normalizeStatementLine(line)

    if (normalized.startsWith("if ")) {
      List(translateIfLine(normalized))
    } else if (normalized.startsWith("Printf.printf")) {
      List(translatePrintf(normalized))
    } else if (normalized.startsWith("print_endline")) {
      List(translatePrintEndline(normalized))
    } else if (normalized.contains(":=")) {
      List(translateAssignment(normalized))
    } else if (normalized == "done") {
      Nil
    } else if (normalized.isEmpty) {
      Nil
    } else if (line.startsWith("else ")) {
      List("else " + translateExpression(line.stripPrefix("else ").trim))
    } else if (line.startsWith("match ")) {
      translateMatchLine(line)
    } else {
      List(translateExpression(normalized))
    }
  }

  def translateMainLine(line: String): List[String] = {
    val normalized = normalizeStatementLine(line)

    if (normalized.startsWith("let ")) {
      List(translateLocalLet(normalized))
    } else if (normalized.startsWith("Printf.printf")) {
      List(translatePrintf(normalized))
    } else if (normalized.startsWith("print_endline")) {
      List(translatePrintEndline(normalized))
    } else if (normalized.startsWith("if ")) {
      List(translateIfLine(normalized))
    } else if (normalized.contains(":=")) {
      List(translateAssignment(normalized))
    } else if (normalized == "done" || normalized.isEmpty) {
      Nil
    } else {
      List(translateExpression(normalized))
    }
  }

  def translateLocalLet(line: String): String = {
    val withInPattern = """let ([a-zA-Z_]\w*) = (.+?) in(?:\s*\()?\s*""".r
    val plainPattern = """let ([a-zA-Z_]\w*) = (.+)""".r

    line.trim match {
      case withInPattern(name, expr) =>
        renderLocalBinding(name, expr)
      case plainPattern(name, expr) =>
        renderLocalBinding(name, expr)
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

  def translateAssignment(line: String): String = {
    val pattern = """([a-zA-Z_]\w*)\s*:=\s*(.+)""".r

    normalizeStatementLine(line) match {
      case pattern(name, expr) =>
        s"$name = ${translateExpression(expr)}"
      case _ =>
        "// Could not translate assignment: " + line.trim
    }
  }

  def translatePrintf(line: String): String = {
    val cleaned = cleanLineEnding(line.trim)
    val pattern = """Printf\.printf "([^"]*)"(.+)""".r

    cleaned match {
      case pattern(formatText, rawArgs) =>
        val args = splitArguments(rawArgs.trim).map(arg => normalizePrintfArgument(arg))
        val scalaText = buildInterpolatedString(formatText, args)
        s"""println($scalaText)"""
      case _ =>
        "// Could not translate Printf.printf: " + line.trim
    }
  }

  def normalizePrintfArgument(arg: String): String = {
    val translated = translateExpression(removeParentheses(arg))
    val simpleFunctionApplication = """([a-zA-Z_]\w*) ([a-zA-Z_]\w*)""".r

    translated match {
      case simpleFunctionApplication(name, value) => s"$name($value)"
      case _                                      => translated
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
    val cleaned = normalizeStatementLine(expr)
    val withoutRefKeyword =
      if (cleaned.startsWith("ref ")) cleaned.stripPrefix("ref ").trim
      else cleaned

    val withPrint = withoutRefKeyword.replace("print_endline", "println")
    val withDereference = withPrint.replaceAll("""!([a-zA-Z_]\w*)""", "$1")
    val withParenthesizedCalls = translateFunctionCalls(withDereference)
    val withBareCalls = translateBareFunctionCalls(withParenthesizedCalls)
    val withStringConversions = translateStringConversions(withBareCalls)
    val withConcatenation = withStringConversions.replace("^", " + ")
    val withModulo = withConcatenation.replaceAll("""\bmod\b""", "%")
    val withNotEqual = withModulo.replace("<>", "!=")

    translateEqualityOperators(withNotEqual)
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

  def translateBareFunctionCalls(text: String): String = {
    val reservedWords = Set("if", "then", "else", "let", "while", "done", "ref", "val", "var", "println", "mod")
    val bareCallPattern = """\b([a-zA-Z_]\w*)\s+("([^"\\]|\\.)*"|\([^()]+\)|![a-zA-Z_]\w*|[a-zA-Z_]\w*)""".r
    val stringArgumentPattern = """\b([a-zA-Z_]\w*)\s+("([^"\\]|\\.)*")""".r
    var result = text
    var changed = true

    while (changed) {
      val withStringArguments = stringArgumentPattern.replaceAllIn(result, m => {
        val name = m.group(1)
        val argument = m.group(2)

        if (
          reservedWords.contains(name) ||
          isInsideStringLiteral(result, m.start(1)) ||
          resultLiftLooksLikeDefinition(result, m.start(1))
        ) m.matched
        else s"$name($argument)"
      })

      val updated = transformOutsideStrings(withStringArguments, segment =>
        bareCallPattern.replaceAllIn(segment, m => {
          val name = m.group(1)
          val argument = m.group(2)

          if (
            reservedWords.contains(name) ||
            reservedWords.contains(argument) ||
            resultLiftLooksLikeDefinition(segment, m.start(1))
          ) m.matched
          else s"$name($argument)"
        })
      )

      changed = updated != result
      result = updated
    }

    result
  }

  def translateStringConversions(text: String): String = {
    var result = text
    var changed = true

    while (changed) {
      val updated = result
        .replaceAll("""string_of_int\(([^()]+)\)""", "($1).toString")
        .replaceAll("""string_of_int\s+([a-zA-Z_]\w*)""", "$1.toString")

      changed = updated != result
      result = updated
    }

    result
  }

  def translateEqualityOperators(text: String): String = {
    val result = new StringBuilder
    var index = 0
    var insideString = false

    while (index < text.length) {
      val current = text.charAt(index)

      if (current == '"' && (index == 0 || text.charAt(index - 1) != '\\')) {
        insideString = !insideString
        result.append(current)
      } else if (!insideString && current == '=') {
        val previous = if (index > 0) text.charAt(index - 1) else '\u0000'
        val next = if (index + 1 < text.length) text.charAt(index + 1) else '\u0000'
        val isComparison = !Set(':', '<', '>', '!', '=').contains(previous) && next != '='

        if (isComparison) result.append("==")
        else result.append(current)
      } else {
        result.append(current)
      }

      index += 1
    }

    result.toString()
  }

  def isInsideStringLiteral(text: String, targetIndex: Int): Boolean = {
    var insideString = false
    var index = 0

    while (index < targetIndex && index < text.length) {
      val current = text.charAt(index)
      val isQuote = current == '"' && (index == 0 || text.charAt(index - 1) != '\\')

      if (isQuote) {
        insideString = !insideString
      }

      index += 1
    }

    insideString
  }

  def transformOutsideStrings(text: String, transform: String => String): String = {
    val result = new StringBuilder
    val segment = new StringBuilder
    var insideString = false
    var index = 0

    while (index < text.length) {
      val current = text.charAt(index)
      val isQuote = current == '"' && (index == 0 || text.charAt(index - 1) != '\\')

      if (isQuote && insideString) {
        segment.append(current)
        result.append(segment.toString())
        segment.clear()
        insideString = false
      } else if (isQuote) {
        result.append(transform(segment.toString()))
        segment.clear()
        segment.append(current)
        insideString = true
      } else {
        segment.append(current)
      }

      index += 1
    }

    if (segment.nonEmpty) {
      if (insideString) result.append(segment.toString())
      else result.append(transform(segment.toString()))
    }

    result.toString()
  }

  def normalizeStatementLine(line: String): String = {
    var normalized = cleanLineEnding(line.trim)

    while (
      normalized.endsWith(")") &&
      normalized.count(_ == '(') < normalized.count(_ == ')')
    ) {
      normalized = normalized.dropRight(1).trim
    }

    normalized
  }

  def translateStatementSequence(
      lines: List[String],
      startIndex: Int,
      stopAtDone: Boolean
  ): (List[String], Int) = {
    val translated = scala.collection.mutable.ListBuffer[String]()
    var index = startIndex

    while (index < lines.length) {
      val line = normalizeStatementLine(lines(index))

      if (line.isEmpty || line == "(" || line == ")") {
        index += 1
      } else if (stopAtDone && line == "done") {
        return (translated.toList, index + 1)
      } else if (line.startsWith("while ")) {
        val whilePattern = """while (.+) do""".r

        line match {
          case whilePattern(condition) =>
            val (body, nextIndex) = translateStatementSequence(lines, index + 1, stopAtDone = true)
            translated += s"while (${translateExpression(condition)}) {"
            translated ++= body.map("  " + _)
            translated += "}"
            index = nextIndex

          case _ =>
            translated += "// Could not translate while loop: " + line
            index += 1
        }
      } else if (line.startsWith("if ")) {
        val (ifLines, nextIndex) = translateIfStatement(lines, index)
        translated ++= ifLines
        index = nextIndex
      } else {
        translated += translateStandaloneStatement(line)
        index += 1
      }
    }

    (translated.toList, index)
  }

  def translateIfStatement(lines: List[String], startIndex: Int): (List[String], Int) = {
    val line = normalizeStatementLine(lines(startIndex))
    val inlineIfElsePattern = """if (.+) then (.+) else (.+)""".r
    val inlineIfPattern = """if (.+) then (.+)""".r
    val multilineIfPattern = """if (.+) then""".r

    line match {
      case inlineIfElsePattern(condition, whenTrue, whenFalse) =>
        (
          List(
            s"if (${translateExpression(condition)}) {",
            "  " + translateStandaloneStatement(whenTrue),
            "} else {",
            "  " + translateStandaloneStatement(whenFalse),
            "}"
          ),
          startIndex + 1
        )

      case multilineIfPattern(condition) =>
        val nextLine =
          if (startIndex + 1 < lines.length) normalizeStatementLine(lines(startIndex + 1))
          else ""

        if (nextLine.endsWith(" else")) {
          val whenTrue = normalizeStatementLine(nextLine.stripSuffix("else").trim)
          val whenFalse =
            if (startIndex + 2 < lines.length) normalizeStatementLine(lines(startIndex + 2))
            else ""

          (
            List(
              s"if (${translateExpression(condition)}) {",
              "  " + translateStandaloneStatement(whenTrue),
              "} else {",
              "  " + translateStandaloneStatement(whenFalse),
              "}"
            ),
            startIndex + 3
          )
        } else {
          (
            List(
              s"if (${translateExpression(condition)}) {",
              "  " + translateStandaloneStatement(nextLine),
              "}"
            ),
            startIndex + 2
          )
        }

      case inlineIfPattern(condition, whenTrue) =>
        (List(s"if (${translateExpression(condition)}) ${translateExpression(whenTrue)}"), startIndex + 1)

      case _ =>
        (List("// Could not translate if expression: " + line), startIndex + 1)
    }
  }

  def translateStandaloneStatement(line: String): String = {
    val normalized = normalizeStatementLine(line)

    if (normalized.startsWith("let ")) {
      translateLocalLet(normalized)
    } else if (normalized.startsWith("Printf.printf")) {
      translatePrintf(normalized)
    } else if (normalized.startsWith("print_endline")) {
      translatePrintEndline(normalized)
    } else if (normalized.contains(":=")) {
      translateAssignment(normalized)
    } else {
      translateExpression(normalized)
    }
  }

  def renderLocalBinding(name: String, expr: String): String = {
    val cleanedExpr = normalizeStatementLine(expr)

    if (cleanedExpr.startsWith("ref ")) {
      s"var $name = ${translateExpression(cleanedExpr.stripPrefix("ref ").trim)}"
    } else {
      s"val $name = ${translateExpression(cleanedExpr)}"
    }
  }

  def countGroupingDelta(line: String): Int = {
    line.count(_ == '(') - line.count(_ == ')')
  }

  def isMainDefinition(header: String): Boolean = {
    header.matches("""let\s+main\s*\(\s*\)\s*(?::\s*[^=]+)?=\s*.*""")
  }

  def isMainInvocation(header: String): Boolean = {
    header.matches("""let\s+(?:_|\(\))\s*=\s*main\s*\(\s*\)\s*""")
  }

  case class FunctionSignature(
      name: String,
      paramName: String,
      paramType: String,
      returnType: String,
      inlineExpression: Option[String]
  )

  def parseFunctionHeader(header: String, isRecursive: Boolean): Option[FunctionSignature] = {
    val prefix = if (isRecursive) "let rec " else "let "
    val typedPattern =
      (prefix + """([a-zA-Z_]\w*)\s+\(([a-zA-Z_]\w*)\s*:\s*([a-zA-Z_]\w*)\)\s*(?::\s*([a-zA-Z_]\w*))?\s*=\s*(.*)""").r
    val simplePattern =
      (prefix + """([a-zA-Z_]\w*)\s+([a-zA-Z_]\w*)\s*(?::\s*([a-zA-Z_]\w*))?\s*=\s*(.*)""").r

    header match {
      case typedPattern(name, paramName, paramType, returnType, inlineExpr) =>
        Some(
          FunctionSignature(
            name,
            paramName,
            mapOcamlType(paramType),
            mapOcamlTypeOption(returnType).getOrElse("Int"),
            optionalInlineExpression(inlineExpr)
          )
        )

      case simplePattern(name, paramName, returnType, inlineExpr) =>
        Some(
          FunctionSignature(
            name,
            paramName,
            "Int",
            mapOcamlTypeOption(returnType).getOrElse("Int"),
            optionalInlineExpression(inlineExpr)
          )
        )

      case _ =>
        None
    }
  }

  def renderFunction(
      name: String,
      paramName: String,
      paramType: String,
      returnType: String,
      bodyLines: List[String]
  ): List[String] = {
    val translatedBody = bodyLines.flatMap(translateBodyLine)

    if (translatedBody.length == 1) {
      List(s"def $name($paramName: $paramType): $returnType = ${translatedBody.head}")
    } else {
      List(s"def $name($paramName: $paramType): $returnType = {") ++
        translatedBody.map("  " + _) ++
        List("}")
    }
  }

  def bodyFromHeader(inlineExpression: Option[String]): Option[List[String]] = {
    inlineExpression.map(expr => List(expr).filter(_.trim.nonEmpty))
  }

  def optionalInlineExpression(text: String): Option[String] = {
    Option(text.trim).filter(_.nonEmpty)
  }

  def mapOcamlTypeOption(ocamlType: String): Option[String] = {
    Option(ocamlType).map(_.trim).filter(_.nonEmpty).map(mapOcamlType)
  }

  def mapOcamlType(ocamlType: String): String = {
    ocamlType.trim match {
      case "int"    => "Int"
      case "string" => "String"
      case "bool"   => "Boolean"
      case "float"  => "Double"
      case "unit"   => "Unit"
      case other    => other
    }
  }

  def resultLiftLooksLikeDefinition(text: String, startIndex: Int): Boolean = {
    val prefix = text.take(startIndex).trim
    prefix.endsWith("def") || prefix.endsWith("val") || prefix.endsWith("var")
  }

  def cleanLineEnding(line: String): String = {
    line.trim.stripSuffix(";").trim
  }

  def removeParentheses(text: String): String = {
    text.trim.stripPrefix("(").stripSuffix(")")
  }
}
