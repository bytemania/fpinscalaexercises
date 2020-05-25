package fpinscala.laziness {

  object LazyDef {
    def ifThunk[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = {
      if(cond) onTrue() else onFalse()
    }

    def ifNonStrictParameter[A](cond: Boolean, onTrue : => A, onFalse: => A): A = {
      if(cond) onTrue else onFalse
    }

    def maybeTwice(b: Boolean, i: => Int): Int = if (b) i + i else 0

    def maybeTwiceLazy(b: Boolean, i: => Int): Int = {
      lazy val j = i
      if (b) j + j else 0
    }
  }

  object LazyMain extends App {
    import LazyDef._
    val a = 23
    ifThunk(a < 22, () => println("a"), () => println("b"))
    ifNonStrictParameter(a < 22, println("a"), println("b"))
    println()
    println(maybeTwice(true, {println("hi"); 1 + 41}))
    println()
    println(maybeTwiceLazy(true, {println("hi"); 1 + 41}))

    println(Stream(1,2,3).take(2).toList)


    println(Stream.ones.take(5).toList)
    println(Stream.ones.exists(_ % 2 != 0))
    println(Stream.ones.map(_ + 1).exists(_ % 2 == 0))
    //println(ones.takeWhile(_ == 1).toList)
    //println(ones.forAll(_ != 1))

    assert(Stream(1,2,3).startsWith(Stream(1,2)))

    println(Stream(1,2,3).tails.map(_.toList).toList)

    println(Stream(1,2,3).scanRight(0)(_ + _).toList)
  }

}
