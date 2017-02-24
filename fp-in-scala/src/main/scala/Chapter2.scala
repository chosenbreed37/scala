object Chapter2_ {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    def loop(m: Int, n: Int, acc: Boolean): Boolean = {
      if (acc == false) acc
      else if ((n + 1) == as.length) ordered(as(m), as(n)) && acc
      else loop(n, n + 1, acc)
    }
    as.length < 2 ||
    loop(0, 1, true)
  }

  def partial1[A,B,C](a: A, f: (A, B) => C): B => C = b => f(a, b)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => partial1(a, f)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = ???

  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))
}
