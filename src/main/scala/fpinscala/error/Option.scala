package fpinscala.error {

  import scala.annotation.tailrec

  sealed trait Option[+A] {
    /*
     * Implement all of the functions on Option
     */
    def map[B](f: A => B): Option[B] = ???

    def flatMap[B](f: A => Option[B]): Option[B] = ???

    def flatMapViaMapAndGetOrElse[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    def getOrElse[B >: A](default: B): B = ???

    def orElse[B >: A](o: => Option[B]): Option[B] = ???

    def orElseViaMapAndGetOrElse[B >: A](o: => Option[B]): Option[B] = map(Some(_)) getOrElse o

    def filter(p: A => Boolean): Option[A] = ???

    def filterViaFlatMap(p: A => Boolean): Option[A] = ???

    def map2[B,C](o: Option[B])(f: (A,B) => C): Option[C] = Option.map2(this,o)(f)
  }

  object Option {
    def apply[A](a: A): Option[A] = Some(a)

    def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

    def Try[A](a: => A): Option[A] = try Some(a) catch {case _: Exception => None}

    /*
    * Write a generic function map2 that combines two Option values using a binary function.
    * If either Option value is None, then the return value is too.
    */
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = ???

    def map2ForComprehension[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = ???

    /*
     * Write a function sequence that combines a list of Options into one Option containing a list of all the
     * Some values in the original list. If the original list contains None even once, the result of the function
     * should be None; otherwise the result should be Some with a list of all the values.
     */
    def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

    def sequenceViaFoldRightAndFilter[A](a: List[Option[A]]): Option[List[A]] =
      a.foldRight[Option[List[A]]](Some(Nil))((e, acc) => map2(e, acc)(_ :: _)).filter(_ != Nil)

    /*
     * Implement this function. It’s straightforward to do using map and sequence, but try for a more efficient
     * implementation that only looks at the list once. In fact, implement sequence in terms of traverse.
     */
    def traverseViaMapAndSequence[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a map f)

    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???

    def traverseViaFoldRight[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
      a.foldRight[Option[List[B]]](Some(Nil))((e,acc) => map2(f(e), acc)(_ :: _))

    def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] = ???
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
}
