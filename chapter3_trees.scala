sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def fold[A, B](tree: Tree[A])(leafFn: A => B)(branchFn: (B, B) => B): B = 
    tree match {
      case Leaf(value) => leafFn(value)
      case Branch(left, right) =>
        val leftFolded = fold(left)(leafFn)(branchFn)
        val rightFolded = fold(right)(leafFn)(branchFn)
        branchFn(leftFolded, rightFolded)
    }

  def sizeUsingFold(tree: Tree[Int]): Int = 
    fold(tree)(_ => 1)((a, b) => 1 + a + b)
  
  def maximumUsingFold(tree: Tree[Int]): Int = 
    fold(tree)(a => a)((a, b) => if (a > b) a else b)
  
  def depthUsingFold[A](tree: Tree[A]): Int = 
    fold(tree)(_ => 1)((a, b) => if (a > b) a + 1 else b + 1)

  def mapUsingFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](tree)((leafValue) => Leaf(f(leafValue)))((left, right) => Branch(left, right))
    
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
    println(sizeUsingFold(muhTree)) 
    
    println(maximum(muhTree))
    println(maximumUsingFold(muhTree))
    
    println(depth(muhTree))
    println(depthUsingFold(muhTree))
    
    println(map(muhTree)(elem => elem * 2))
    println(mapUsingFold[Int, Int](muhTree)(elem => elem * 2))
  }
}
