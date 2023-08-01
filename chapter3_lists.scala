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

  def append2[A](list1: List[A], list2: List[A]): List[A] =
    foldLeft(list1, list2)((accumulationList, element) => Cons(element, accumulationList))

  def concatenate[A](lists: List[List[A]]): List[A] = 
    foldLeft(lists, Nil: List[A])((accumulationList, element) => append2(element, accumulationList))

  // return a copy of the list without the last element
  def init[A](list: List[A]): List[A] = 
    list match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

  // write a function that transforms a list of integers by adding 1 to each element
  def exercise3_16(list: List[Int]): List[Int] =
    foldLeft(list, Nil: List[Int])((accumulationList, element) => Cons(element + 1, accumulationList))
 
  // write a function that turns each value in a List[Double] into a String
  def exercise3_17(list: List[Double]): String =
    foldLeft(list, "")((accumulationList, element) => element.toString + " " + accumulationList)


  def map[A, B](list: List[A])(f: A => B): List[B] = 
    foldLeft(list, Nil: List[B])((accumulationList, element) => Cons(f(element), accumulationList))

  def filter[A](list: List[A])(f: A => Boolean): List[A] = 
    foldLeft(list, Nil: List[A])((accumulationList, element) => if (f(element)) Cons(element, accumulationList) else accumulationList)

  def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = 
    foldLeft(list, Nil: List[B])((accumulationList, element) => append(f(element), accumulationList))
  
  def filter2[A](list: List[A])(f: A => Boolean): List[A] = 
    flatMap(list)((elem) => if (f(elem)) List(elem) else Nil)

  def addLists(list1: List[Int], list2: List[Int]): List[Int] = 
    (list1, list2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(head1 + head2, addLists(tail1, tail2))
    }

  def zipWith[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] = 
    (list1, list2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(f(head1, head2), zipWith(tail1, tail2)(f))
    }


 def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith(sup: List[A], sub: List[A]): Boolean = 
      (sup, sub) match {
        case (_, Nil) => true
        case (Cons(head1, tail1), Cons(head2, tail2)) if head1 == head2 => startsWith(tail1, tail2)
        case _ => false
      }

    sup match {
      case Nil => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, tail) => hasSubsequence(tail, sub)
    }
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
    println(List.setHead(List[Int](1, 2, 3, 4, 5), "a"))
    //println(List.setHead[Int](List[Int](1, 2, 3, 4, 5), "a"))
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
    println(List.concatenate(List(List(1, 2), List(3, 4))))
    println(List.exercise3_16(List(1, 2, 3)))
    println(List.exercise3_17(List(1.23, 2.5353, 3.2333)))
    println(List.map(List(1, 2, 3))(el => el + 3))
    println(List.filter(List(1, 2, 3))(el => el % 2 == 0))
    // TODO - should return List(1, 1, 2, 2, 3, 3)
    println(List.flatMap(List(1, 2, 3))(i => List(i, i)))
    println(List.filter2(List(1, 2, 3))(el => el % 2 == 0))
    println(List.addLists(List(1, 2, 3), List(2, 3, 4)))
    println(List.zipWith[Int, Int, Int](List(1, 2, 3), List(2, 3, 4))((a, b) => a + b))
    println(List.hasSubsequence[Int](List(1, 2, 3, 4), List(1, 2)))
    println(List.hasSubsequence[Int](List(1, 2, 3, 4), List(1)))
    println(List.hasSubsequence[Int](List(1, 2, 3, 4), List(1, 2, 3, 4)))
    println(List.hasSubsequence[Int](List(1, 2, 3, 4), List(2, 3)))
    println(List.hasSubsequence[Int](List(1, 2, 3, 4), List(2, 3, 4, 5)))
  }
}
