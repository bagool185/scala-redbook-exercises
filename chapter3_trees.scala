sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def fold[A, B](tree: Tree[A], neutralValue: B)(f: B => A): B =
    val todo = "TODO dis"

  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Leaf(value) => value
      case Branch(left, right) => maximum(left) max maximum(right)
    }

  def depth[A](tree: Tree[A]): Int = 
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + (depth(left) max depth(right))
    }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = 
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
}

object Chapter3Trees {
  def main(args: Array[String]): Unit = {
    import Tree._

    val muhTree = 
      Branch(
        Branch(
          Leaf(1), 
          Branch(
            Leaf(4),
            Branch(
              Leaf(6), 
              Leaf(7)
            )
          )
        ), 
        Leaf(3)
      ) 


    println(size(muhTree))
    println(maximum(muhTree))
    println(depth(muhTree))
    println(map(muhTree)(elem => elem * 2))
  }
}
