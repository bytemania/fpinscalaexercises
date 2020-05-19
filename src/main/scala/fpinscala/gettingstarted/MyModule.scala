package fpinscala.gettingstarted

import scala.annotation.tailrec

private[gettingstarted] object MyModule extends App {
  private def abs(n: Int): Int = if (n < 0) -n else n
  private def formatAbs(x: Int) = s"The absolute value of $x is ${abs(x)}"

  private def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)
    go(n, 1)
  }

  private def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(i: Int): Int = {
      if (i == as.length - 1) -1
      else if (p(as(i))) i
      else loop(i + 1)
    }
    loop(0)
  }

  private def partial[A, B, C](a: A, f: (A, B) => C): B => C = f(a, _)

  assert(formatAbs(-42) == "The absolute value of -42 is 42")

  assert(factorial(0) == 1)
  assert(factorial(3) == 6)

  assert(findFirst(Array(1,2,3,4), (x: Int) => 5 == x) == -1)
  assert(findFirst(Array(1,2,3,4), (x: Int) => 2 == x) == 1)

  assert(partial(1, (_: Int) * (_: Int))(2) == 2)
  assert(partial(2, (_: Int) + (_: Int))(2) == 4)
}
