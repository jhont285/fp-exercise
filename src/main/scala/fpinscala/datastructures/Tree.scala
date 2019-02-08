package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  //  EXERCISE 3.25
  //  Write a function size that counts the number of nodes (leaves and branches) in a tree.

  def nodesTree[A](as: Tree[A]): Int =
    as match {
      case Leaf(a) => 1
      case Branch(a, b) => nodesTree(a) + nodesTree(b) + 1
    }

  //  EXERCISE 3.26
  //  Write a function maximum that returns the maximum element in a Tree[Int]. (Note:
  //  In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x
  //  and y .)

  def maxElement(as: Tree[Int]): Int =
    as match {
      case Leaf(a) => a
      case Branch(a, b) => maxElement(a) max maxElement(b)
    }

  //  EXERCISE 3.27
  //  Write a function depth that returns the maximum path length from the root of a tree
  //  to any leaf.
  def depth[A](as: Tree[A]): Int =
    as match {
      case Leaf(a) => 1
      case Branch(a, b) => (depth(a) max depth(b)) + 1
    }

  //  EXERCISE 3.28
  //  Write a function map, analogous to the method of the same name on List , that modi-
  //  fies each element in a tree with a given function.

  def map[A](as: Tree[A])(f: A => A): Tree[A] = {
    as match {
      case Leaf(a) => Leaf(f(a))
      case Branch(a, b) => Branch(map(a)(f), map(b)(f)).copy()
    }
  }

  //  EXERCISE 3.29
  //  Generalize size, maximum, depth, and map, writing a new function fold that abstracts
  //  over their similarities. Reimplement them in terms of this more general function. Can
  //  you draw an analogy between this fold function and the left and right folds for List?



  def main(args: Array[String]): Unit = {

    val treeExample = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("a"), Leaf("a")))
    println(nodesTree(treeExample))

    val treeInteger = Branch(Branch(Leaf(5), Leaf(6)), Branch(Leaf(-10), Leaf(50)))
    println(maxElement(treeInteger))

    println(map(treeInteger)(_ + 1))


  }




}