package fpinscala.laziness {

  import scala.annotation.tailrec

  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    /* 1
     * Write a function to convert a Stream to a List (Lazy to Strict)
     */
    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }

    /* 2
     * Write the function take(n) for returning the first n elements of a Stream,
     * and drop(n) for skipping the first n elements of a Stream.
     */

    import Stream._

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 1 => t().drop(n - 1)
      case _ => this
    }

    /* 3
     * Write the function takeWhile for returning all starting elements of a Stream that match the given predicate.
     */
    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def existsViaFoldRight(p: A => Boolean): Boolean = foldRight(false)((e, b) => p(e) || b)

    def foldRight[B](z: => B)(f: (A, =>B) => B): B = this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    /* 4
     * Implement forAll, which checks that all elements in the Stream match a given predicate.
     * Your implementation should terminate the traversal as soon as it encounters a nonmatching value.
     */
    def forAll(p: A => Boolean): Boolean = foldRight(true)(p(_) && _)

    /* 5
     * Use foldRight to implement takeWhile.
     */
    def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, sa) => if(p(a)) cons(a, sa) else empty)

    /* 6
     * Not So Hard: Implement headOption using foldRight.
     */
    def headOptionViaFoldRight: Option[A] = foldRight[Option[A]](None)((a, _) => Some(a))

    /* 7
     * Implement map, filter, append, and flatMap using foldRight.
     * The append method should be non-strict in its argument.
     */
    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,t) => cons(f(h), t))

    def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h,t) => if(p(h)) cons(h,t) else t)

    def append[B >: A](s: => Stream[B]): Stream[B] = this.foldRight(s)(cons(_,_))

    def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(empty[B])(f(_).append(_))

    def find(p: A => Boolean): Option[A] = filter(p).headOption

    /* 13
     * Use unfold to implement map, take, takeWhile, zipWith, and zipAll.
     * The zipAll function should continue the traversal as long as either stream has more
     * elements—it uses Option to indicate whether each stream has been exhausted.
     */
    def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
      case Cons(h,t) => Some(f(h()), t())
      case Empty => None
    }

    def takeViaUnfold[B](n: Int): Stream[A] = unfold((this,n)){
      case (Cons(h,_), 1) => Some(h(), (empty, 0))
      case (Cons(h,t), i) if i > 1 => Some(h(), (t(), i - 1))
      case _ => None
    }

    def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

    def zipWith[B, C](s: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s)){
      case (Cons(ha,ta), Cons(hb,tb)) => Some(f(ha(), hb()), (ta(), tb()))
      case _ => None
    }

    def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s)){
      case (Cons(ha,ta), Cons(hb,tb)) => Some((Some(ha()), Some(hb())), (ta(),tb()))
      case (Cons(ha,ta), Empty) => Some((Some(ha()),None), (ta(), empty))
      case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (empty, tb()))
      case _ => None
    }

    def zip[B](s: Stream[B]): Stream[(A,B)] = zipWith(s)((_, _))

    /* 14
     * Hard: Implement startsWith using functions you’ve written. It should check if one Stream is a prefix of another.
     * For instance, Stream(1,2,3) startsWith Stream(1,2) would be true.
     */
    def startsWith[B >: A](s: Stream[B]): Boolean = this.zip(s).forAll{case (x,y)=> x == y}

    /*
     * Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the input sequence,
     * starting with the original Stream. For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3),
     * Stream(2,3), Stream(3), Stream()).
     */
    def tails: Stream[Stream[A]] = unfold(this){
      case s@Cons(_,t) => Some(s, t())
      case _ => None
    } append Stream(empty)

    def hasSubsequence[B >: A](s: Stream[B]): Boolean = tails exists (_ startsWith s)

    /*
     * Hard: Generalize tails to the function scanRight, which is like a foldRight that returns a stream of the
     * intermediate results.
     */
    def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
      foldRight((z, Stream(z)))((e, acc) => {
        lazy val p = acc
        val b = f(e, p._1)
        (b, cons(b, p._2))
      })._2
  }

  case object Empty extends Stream[Nothing]

  final case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    val ones: Stream[Int] = Stream.cons(1, ones)

    /* 8
     * Generalize ones slightly to the function constant, which returns an infinite Stream of a given value.
     */
    def constantSlow[A](a:A): Stream[A] = cons(a, constantSlow(a))

    def constant[A](a: A): Stream[A] = {
      // This is more efficient than the previous since it's just one object referencing itself.
      lazy val tail: Stream[A] = Cons(()=> a, () => tail)
      tail
    }

    /* 9
     * Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on.
     */
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    /* 10
     * Write a function fibs that generates the infinite stream of Fibonacci numbers
     * Hint: recursive helper function
     */
    def fibs: Stream[Int] = {
      def go(prev: Int, curr: Int): Stream[Int] = {
        cons(prev, go(curr, curr + prev))
      }
      go(0, 1)
    }

    /* 11
     * Write a more general stream-building function called unfold. It takes an initial state, and a function
     * for producing both the next state and the next value in the generated stream.
     */

    def unfold[A,S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

    /* 12
     * Write fibs, from, constant, and ones in terms of unfold.
     */
    def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1,1))

    def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some(a,a))

    def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s,s + 1))

    def fibsViaUnfold: Stream[Int] = unfold((0,1)){case (p,c)=> Some((p, (c, p + c)))}

  }

}
