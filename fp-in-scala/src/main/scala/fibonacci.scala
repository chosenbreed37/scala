object Fibonacci {

  def fib(n: Int): Int = {
    if (n == 1) 0
    else if (n == 2) 1
    else fib(n - 1) + fib(n - 2)
  }

  def fib2(n: Int): Int = {
    def loop(m: Int, n1: Int, n2: Int): Int = {
      if (m == n) n1
      else loop(m + 1, n1 + n2, n1)
    }
    if (n == 1) 0
    else if (n == 2) 1
    else loop(2, 1, 0)
  }

  def main(args: Array[String]): Unit = {
      println(fib(Integer.parseInt(args(0))))
  }
}
