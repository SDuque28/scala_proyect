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
