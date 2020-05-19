package fpinscala.gettingstarted

import scala.annotation.tailrec

private[gettingstarted] object Exercises {
  /*
   * Write a recursive function to get the nth Fibonacci number
   * Your definition should use a local tail-recursive function.
   */
  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else go(n - 1, cur, prev + cur)

    go(n, 0, 1)
  }

  /*
   * Implement isSorted, which checks whether an Array[A] is sorted according to a given comparison function
   */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(i: Int): Boolean = {
      if (i == as.length - 1) true
      else if (!ordered(as(i), as(i + 1))) false
      else go(i + 1)
    }

    go(0)
  }

  /*
   * Let’s look at another example, currying, which converts a function f of two arguments into a
   * function of one argument that partially applies f. Here again there’s only one implementation that compiles.
   * Write this implementation.
   */
  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a,b)

  /*
   * Implement uncurry, which reverses the transformation of curry.
   */
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = f(_)(_)

  /*
   * Implement the higher-order function that composes two functions.
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

}

private[gettingstarted] object Main extends App {
  import Exercises._

  assert(fib(0) == 0)
  assert(fib(1) == 1)
  assert(fib(2) == 1)
  assert(fib(5) == 5)
  assert(fib(10) == 55)

  assert(isSorted(Array(1,2,3), Integer.compare(_: Int, _: Int) < 0))
  assert(!isSorted(Array(1,4,3), Integer.compare(_: Int, _: Int) < 0))

  assert(curry((_: Int) + (_: Int))(2)(2) == 4)
  assert(curry((_: Int) * (_: Int))(2)(3) == 6)

  assert(uncurry((x: Int) => (y: Int) => x + y)(2, 3) == 5)
  assert(uncurry((x: Int) => (y: Int) => x * y)(2, 3) == 6)

  assert(compose((x: Int) => x + 1, (y: Int) => y + 2)(1) == 4)
  assert(compose((x: Int) => x - 1, (y: Int) => y + 2)(1) == 2)
}
