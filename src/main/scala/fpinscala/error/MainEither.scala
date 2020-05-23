package fpinscala.error {

  object MainEither extends App {
    def mean(xs: IndexedSeq[Double]): Either[String, Double] =
      if (xs.isEmpty) Left("Empty List")
      else Right(xs.sum / xs.length)

    def Try[A](a: => A): Either[Exception, A] =
      try Right(a)
      catch {
        case e: Exception => Left(e)
      }

    def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Either[Exception, Double] = {
      def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int) = age * numberOfSpeedingTickets
      for {
        a <- Try {age.toInt}
        tickets <- Try {numberOfSpeedingTickets.toInt}
      } yield insuranceRateQuote(a, tickets)
    }

    sealed case class Person(name: Name, age: Age)
    sealed case class Name(value: String)
    sealed case class Age(value: Int)

    def mkName(name: String): Either[String, Name] =
      if (name == "" || name == null) Left("Name is Empty")
      else Right(Name("Name is empty"))

    def mkAge(age: Int): Either[String, Age] =
      if (age < 0) Left("Age is out of range.")
      else Right(Age(age))

    def mkPerson(name: String, age: Int): Either[String, Person] = mkName(name).map2(mkAge(age))(Person)

  }

}
