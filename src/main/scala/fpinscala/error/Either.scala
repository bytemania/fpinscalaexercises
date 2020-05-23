package fpinscala.error {

  sealed trait Either[+E,+A] {
    /*
     * Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
     */

    def map[B](f: A => B): Either[E, B] = this match {
      case e @ Left(_) => e
      case Right(v) => Right(f(v))
    }

    def flatMap[EE >: E,B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case e @ Left(_) => e
      case Right(v) => f(v)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case e @ Right(_) => e
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
      x <- this
      y <- b
    } yield f(x,y)
  }

  object Either {
    /*
     * Implement sequence and traverse for Either. These should return the first error that’s encountered,
     * if there is one.
     */
    def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] = traverse(es)(identity)

    def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] =
      as.foldRight[Either[E,List[B]]](Right(Nil))((a,acc) => f(a).map2(acc)(_ :: _))

  }

  final case class Left[+E](value: E) extends Either[E, Nothing]
  final case class Right[+A](value: A) extends Either[Nothing, A]
}
