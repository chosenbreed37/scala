package fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

  def setHead[A](a: A, as: List[A]): List[A] = as match {
      case Nil => Cons(a, Nil)
      case Cons(_, xs) => Cons(a, xs)
    }

  def drop[A](n: Int, as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(_, xs) =>
        if (n == 0) as
        else drop(n - 1, xs)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def dropWhile[A](as: List[A], f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else as
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs match {
      case Nil => Nil
      case Cons(_, _) => Cons(x, init(xs))
    }
  }

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)(_+_)

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, n) => n + 1)

  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum3(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def product3(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0)((n, _) => n + 1)

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldLeft(as, Nil: List[B])((acc, curr) => Cons(f(curr), acc))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(as, Nil: List[A])((acc, curr) => if (f(curr)) Cons(curr, acc) else acc)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldLeft(map(as)(f), Nil: List[B])((acc, curr) => foldLeft(curr, acc)((a, c) => Cons(c, a)))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    val g = (a: A) => if (f(a)) List(a) else Nil
    flatMap(as)(g)
  }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = {
    def loop(as: List[A], bs: List[B], cs: List[C]): List[C] =
      as match {
        case Nil => cs
        case Cons(x, xs) => bs match {
          case Nil => cs
          case Cons(y, ys) => loop(xs, ys, Cons(f(x, y), cs))
        }
      }
    loop(as, bs, Nil: List[C])
  }

  def zipWith2[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =
    (as, bs) match {
      case (Nil, Nil) => Nil
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith2(xs, ys)(f))
    }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def loop(sup: List[A], sub: List[A], acc: Boolean): Boolean = {
      (sup, sub) match {
        case (_, Nil) => acc
        case (Nil, _) => false
        case (Cons(x, xs), Cons(y, ys)) =>
          if (x == y)loop(xs, ys, acc)
          else loop(xs, sub, acc)
      }
    }
    loop(sup, sub, true)
  }
}
