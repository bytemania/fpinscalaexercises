package fpinscala.error {

  object MainOption extends App {

    def meanBad(xs: IndexedSeq[Double], onEmpty: Double): Double = if (xs.isEmpty) onEmpty else xs.sum / xs.length
    def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

    /*
     * Implement the variance function in terms of flatMap. If the mean of a sequence is m, the variance is the
     * mean of math.pow(x - m, 2) for each element x in the sequence.
     */
    def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

    val absO: Option[Double] => Option[Double] = Option.lift(math.abs)

    def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
      val optAge = Option.Try(age.toDouble)
      val optNumberOfSpeedTickets = Option.Try(numberOfSpeedingTickets.toDouble)
      def insuranceQuoteRate= (x: Double, y: Double) => x * y
      Option.map2(optAge, optNumberOfSpeedTickets)(insuranceQuoteRate)
    }

    def parseInt(a: List[String]): Option[List[Int]] = Option.sequence(a map (i => Option.Try(i.toInt)))

    assert(mean(List()) == None)
    assert(mean(List(1,2,3)) == Some(2))
    assert(variance((1 to 10).map(_.toDouble).toList) == Some(8.25))
    assert(absO(Some(-2)) == Some(2))

    assert(parseInsuranceRateQuote("2","2") == Some(4))
    assert(parseInsuranceRateQuote("2","INVALID") == None)

    assert(Option(2).map(_ + 2) == Some(4))
    assert(None.map(identity[Nothing]) == None)

    assert(Option(3).flatMap(x => Some(x + 2)) == Some(5))
    assert(Option(3).flatMap(x => if (x == 3) None else Some(x)) == None)

    assert(Option(3).flatMapViaMapAndGetOrElse(x => Some(x + 2)) == Some(5))
    assert(Option(3).flatMapViaMapAndGetOrElse(x => if (x == 3) None else Some(x)) == None)

    assert(None.getOrElse(2) == 2)
    assert(Some(4).getOrElse(2) == 4)

    assert(None.orElse(Some(2)) == Some(2))
    assert(Some(4).orElse(Some(2)) == Some(4))

    assert(None.orElseViaMapAndGetOrElse(Some(2)) == Some(2))
    assert(Some(4).orElseViaMapAndGetOrElse(Some(2)) == Some(4))

    assert(Some(2).filter(_ == 2) == Some(2))
    assert(Some(3).filter(_ == 2) == None)

    assert(Some(2).filterViaFlatMap(_ == 2) == Some(2))
    assert(Some(3).filterViaFlatMap(_ == 2) == None)

    assert(Some(2).map2(Some("two"))((_,_)) == Some(2, "two"))

    assert(Option.sequence(List(Some(2), Some(3), Some(5))) == Some(List(2,3,5)))
    assert(Option.sequence(List(Some(2), None, Some(5))) == None)

    assert(Option.sequenceViaFoldRightAndFilter(List(Some(2), Some(3), Some(5))) == Some(List(2,3,5)))
    assert(Option.sequenceViaFoldRightAndFilter(List(Some(2), None, Some(5))) == None)
    assert(Option.sequenceViaFoldRightAndFilter(List()) == None)

    assert(parseInt(List("1","3","2")) == Some(List(1,3,2)))
    assert(parseInt(List("1","one","2")) == None)

    assert(Option.traverse(List(1,2,3))(x => Some(x + 1)) == Some(List(2, 3, 4)))

  }

}
