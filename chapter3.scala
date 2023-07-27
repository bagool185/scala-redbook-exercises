// + means A is covariant (i.e. List[Dog] is subtype of List[Animal])
sealed trait List[+A]
// covariant type allows using List[Nothing] of any type A of List[A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](list: List[A], neutralVal: B)(f: (A, B) => B): B =
    list match {
      case Nil => neutralVal
      case Cons(head, tail) => f(head, foldRight(tail, neutralVal)(f))
    }

  def foldLeft[A, B](list: List[A], neutralVal: B)(f: (B, A) => B): B = 
    list match {
      case Nil => neutralVal
      case Cons(head, tail) => f(foldLeft(tail, neutralVal)(f), head)
    }

  def foldRight2[A, B](list: List[A], neutralVal: B)(f: (A, B) => B): B = 
    foldLeft(list, neutralVal)((x, y) => f(y,x))

  def foldLeft2[A, B](list: List[A], neutralVal: B)(f: (B, A) => B): B = 
    foldRight(list, neutralVal)((x, y) => f(y,x))

  def sum2(list: List[Int]) = foldRight(list, 0)((x, y) => x + y)
  def sum3(list: List[Int]) = foldLeft(list, 0)((x, y) => x + y)

  def product2(list: List[Double]) = foldRight(list, 1.0)(_ * _)
  def product3(list: List[Double]) = foldLeft(list, 1.0)(_ * _)

  def length[A](list: List[A]): Int = foldRight(list, 0)((_, accumulator) => accumulator + 1) 
  def length2[A](list: List[A]): Int = foldLeft(list, 0)((accumulator, _) => accumulator + 1) 

  // remove the first element of the list (the head)
  def tail[A](list: List[A]): List[A] = {
    drop(list, 1)
  }

  // overwrite the head of a list
  def setHead[A](list: List[A], newHead: A): List[A] = 
    list match {
      case Nil => List(newHead)
      case Cons(firstElement, rest) => Cons(newHead, rest)
    }
  
  // remove the first n elements from a list
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n == 0) list
    else {
      list match {
        case Nil => Nil
        case Cons(firstElement, rest) => drop(rest, n - 1)
      }
    }
  }

  // remove elements from the list as long as the match the predicate
  // having dropWhile(list)(predicate) helps with type inferrence - basically dropWhile is curried
  def dropWhile[A](list: List[A])(predicate: A => Boolean): List[A] = {
    list match {
      case Nil => Nil
      case Cons(firstElement, rest) if (predicate(firstElement)) => dropWhile(rest)(predicate)
      case Cons(firstElement, rest) => list
    }
  }
   
  // append list2 to list1
  def append[A](list1: List[A], list2: List[A]): List[A] = 
    list1 match {
      case Nil => list2
      case Cons(firstElement, rest) => Cons(firstElement, append(rest, list2))
    }

  // return a copy of the list without the last element
  def init[A](list: List[A]): List[A] = 
    list match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

  // variadic function (i.e. takes a variable number of arguments)
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

object Chapter3 {

  def main(args: Array[String]): Unit = {
    /* val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    } */

    println(List.tail(List(1, 2, 3, 4, 5)))
    println(List.tail(List()))
    // why does this work idk lmao
    println(List.setHead(List[Int](1, 2, 3, 4, 5), "a"))
    println(List.drop(List(1, 2, 3, 4, 5, 6), 5))
    println(List.drop(Cons(1, Nil), 2))
    println(List.dropWhile(List(2, 4, 6, 3, 5, 6))(a => a % 2 == 0))
    println(List.init(List(1, 2, 3, 4)))
    println(List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_,_)))
    println(List.length(List(1, 2, 3, 4)))
    println(List.sum3(List(1, 2, 3, 4)))
    println(List.product3(List(1, 2, 3, 4)))
    println(List.length2(List(1, 2, 3, 4)))
    println(List.append2(List(1, 2), List(3, 4)))
  }
}
