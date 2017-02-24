import fpinscala.datastructures._

object Exercises3 {

  def transform(ns: List[Int]): List[Int] =
    List.foldLeft(ns, Nil: List[Int])((ms, n) => Cons(n + 1, ms))

  def doubleToString(ds: List[Double]): List[String] =
    List.foldLeft(ds, Nil: List[String])((acc, curr) => Cons(curr.toString, acc))

  def zipAdd(as: List[Int], bs: List[Int]): List[Int] = {
    def loop(as: List[Int], bs: List[Int], c: List[Int]): List[Int] =
      as match {
        case Nil => c
        case Cons(x, xs) => bs match {
          case Nil => c
          case Cons(y, ys) => loop(xs, ys, Cons(x + y, c))
        }
      }
    loop(as, bs, Nil: List[Int])
  }

  def maximum(t: Tree[Int]): Int = {
    def loop(curr: Tree[Int], acc: Int): Int = curr match {
      case Leaf(v) => v max acc
      case Branch(l, r) => loop(l, acc) max loop(r, acc)
    }
    loop(t, 0)
  }
}
