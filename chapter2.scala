object MyModule {

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int = 
      if (n <= 0) acc
      else go(n-1, n * acc)
    go(n, 1)
  }

  def abs(n: Int): Int = 
    if (n < 0) -n
    else n
  
  private def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  private def compare[A](a: A, b: A)(implicit ordering: Ordering[A]) = ordering.compare(a,b) <= 0

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean)(implicit ordering: Ordering[A]): Boolean = {
    @annotation.tailrec
    def go(as: Array[A], t1: Int, t2: Int): Boolean = 
      if (t2 + 1 == as.length) ordered(as(t1), as(t2))
      else if (ordered(as(t1), as(t2))) go(as, t1 + 1, t2 + 1)
      else false
    
    go(as, 0, 1)
  }


  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = (b: B) => f(a,b)

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))


  def main(args: Array[String]): Unit = {
    println(compare(5, 3))
    println(isSorted[Int](Array(7, 9, 13), (a: Int, b: Int) => a < b))
    println(isSorted[Int](Array(13, 9, 7), compare))
    println(isSorted[String](Array("a", "b"), compare))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("abs", -42, abs))
  }
}
