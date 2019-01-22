package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => l
      case Cons(x, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => l
      case Cons(x, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => l
      case Cons(x, xs) => if (n == 0) l else drop(xs, n - 1)
    }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => l
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => l
      case Cons(x, xs) => if (xs == Nil) xs else Cons(x, init(xs))
    }

  //  Compute the length of a list using foldRight.
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((x,y) => 1 + y)

  //  Our implementation of foldRight is not tail-recursive and will result in a StackOverflowError
  //  for large lists (we say it’s not stack-safe). Convince yourself that this is the
  //  case, and then write another general list-recursion function, foldLeft, that is
  //  tail-recursive, using the techniques we discussed in the previous chapter. Here is its
  //  signature:
  //  how to copy

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def go[A, B](as: List[A], axum: B, f: (B, A) => B): B =
      as match {
        case Nil => axum
        case Cons(x, xs) => go(xs, f(axum, x), f)
      }

    go(l, z, f)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] =
    l match {
      case Nil => Nil
      case Cons(x,xs) => Cons(f(x), map(xs)(f))
    }

  def main(args: Array[String]): Unit = {

    println(tail(List(1, 2, 3)))
    println(tail(List(3, 2, 3)))
    println(tail(List(3, 2, 1)))
    println(tail(List(3)))
    println(tail(List()))
    println()
    println()
    println(setHead(List(1, 2, 3), 5))
    println(setHead(List(3, 2, 3), 2))
    println(setHead(List(3, 2, 1), 1))
    println(setHead(List(3), 5))
    println(setHead(List(), 5))
    println()
    println()


    println(setHead(List(1, 2, 3), 1))
    println(setHead(List(3, 2, 3), 2))
    println(setHead(List(3, 2, 1, 5), 4))
    println(setHead(List(3), 1))
    println(setHead(List(), 0))
    println()
    println()
    println(length(List(1, 2, 3)))
    println(length(List(1)))
    println(length(List(1, 2)))
    println(length(List()))
    println()
    println()

    val primes = List(2, 3, 4, 5)
    println(drop(primes, 2))
    println(drop(primes, 1))
    println(drop(primes, 3))
    println(drop(primes, 4))
    println()
    println()

    println()
    println()

    println(dropWhile(List(1, 2, 3), (x: Int) => x < 2))
    println(dropWhile(List(1, 2, 3), (x: Int) => x > 2))
    println(dropWhile(List(1, 2, 3), (x: Int) => x > 0))
    println(dropWhile(Nil, (x: Int) => x > 0))

    println()
    println()

    println(init(List(1, 2, 3, 4)))
    println(init(List(1, 2)))
    println(init(List(1)))
    println(init(List()))

    println(foldLeft(List(1, 2, 3, 10), 0)((x: Int, y: Int) => x + y))
    println(foldLeft(List(1, 2, 3, 10), 1)((x: Int, y: Int) => x * y))


    println(map(List(1, 2, 3, 10))((x: Int) => 2 * x))
    println(map(List(1, 2, 3, 10))((x: Int) => 2 + x))




  }
}
