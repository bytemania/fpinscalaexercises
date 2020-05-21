package fpinscala.adt {

  import scala.annotation.tailrec

 case object Nil extends List[Nothing]
 final case class Cons[+A](head: A, tail: List[A]) extends List[A]

  sealed trait List[+A] {
    def setHead[B >: A](b: B): List[B] = List.setHead(this, b)
    def drop(n: Int): List[A] = List.drop(this, n)
    def dropWhile(f: A => Boolean): List[A] = List.dropWhile(this, f)
    def append[B >: A](l1: List[B]): List[B] = List.append(this, l1)
    def length: Int = List.length(this)
    def foldRight[B](z: B)(f: (A, B) => B): B = List.foldRight(this, z)(f)
    def foldLeft[B](z: B)(f: (B, A) => B): B = List.foldLeft(this, z)(f)
    def reverse: List[A] = List.reverse(this)
    def init: List[A] = List.init(this)
    def map[B](f: A => B): List[B] = List.map(this)(f)
    def filter(p: A => Boolean): List[A] = List.filter(this)(p)
    def flatMap[B](f: A => List[B]): List[B] = List.flatMap(this)(f)
    def zipWith[B, C](l: List[B])(f: (A, B) => C): List[C] = List.zipWith(this, l)(f)
    def hasSubsequence[B >: A](sub: List[B]): Boolean = List.hasSubsequence(this, sub)

    def take(n: Int): List[A] = {
      def loop(n: Int, l: List[A]): List[A] = (n, l) match {
        case (0, _) => Nil
        case (_, Nil) => Nil
        case (n, Cons(h, t)) => Cons(h, loop(n - 1, t))
      }

      loop(n, this)
    }

    def takeWhile(p: A => Boolean): List[A] = this match {
      case Cons(h, t) if p(h) => Cons(h, t.takeWhile(p))
      case _ => Nil
    }

    def forall(p: A => Boolean): Boolean = this.foldRight(true)(p(_) && _)

    def exists(p: A => Boolean): Boolean = this.foldRight(false)(p(_) || _)

    def scanLeft[B](z: B)(next: (B, A) => B): List[B] = this match {
      case Nil => Cons(z, Nil)
      case Cons(h, t) => Cons(z, t.scanLeft(next(z, h))(next))
    }

    def scanRight[B](z: B)(next: (A, B) => B): List[B] = this match {
      case Nil => Cons(z, Nil)
      case Cons(h, t) =>
        val l = t.scanRight(z)(next)
        l match {
          case Nil => Nil
          case l@Cons(b, _) => Cons(next(h, b), l)
        }
    }
  }

  object List {
    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def fill[A](n: Int, a: A): List[A] = n match {
      case n if n <= 0 => Nil
      case n => Cons(a, fill(n - 1, a))
    }

    def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

    def product(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

    def length[A](l: List[A]): Int = foldLeft(l, 0)((b, _) => b + 1)

    /*
     * Implement the function tail for removing the first element of a List.
     */
    def tail[A](l: List[A]): List[A] = drop(l, 1)

    /*
     * Using the same idea, implement the function setHead for replacing the first element of a List with a different value.
     */
    def setHead[A](l: List[A], a: A): List[A] = Cons(a, tail(l))

    /*
     * Generalize tail to the function drop, which removes the first n elements from a list.
     */
    @tailrec
    @throws(classOf[IllegalArgumentException])
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
      case (Nil, n) if n > 0 => throw new IllegalArgumentException("Empty List")
      case (l, 0) => l
      case (Cons(_, t), n) => drop(t, n -1)
    }

    /*
     * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
     */
    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

    def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

    /*
     * Implement a function, init, that returns a List consisting of all but the last element of a List.
     * So, given List(1,2,3,4), init will return List(1,2,3)
     */
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

    def foldRight[A,B](l: List[A], z: B)(f: (A,B) => B): B = l match {
      case Nil => z
      case Cons(h,t) => f(h, foldRight(t, z)(f))
    }

    /*
     * 1) Define foldLeft
     * 2) Write sum, product and the length using foldLeft
     */
    @tailrec
    def foldLeft[A,B](l: List[A], z: B)(f: (B,A) => B): B = l match {
      case Nil => z
      case Cons(h,t) => foldLeft(t, f(z,h))(f)
    }

    /*
     * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
     * See if you can write it using a fold.
     */
    def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((t, h) => Cons(h, t))

    /*
     *    Hard: Can you write foldLeft in terms of foldRight? How about the other way around?
     *    How about the other way around? Implementing foldRight via foldLeft is useful because it lets us implement
     *    foldRight tail-recursively, which means it works even for large lists without overflowing the stack.
     */
    def foldRightViaFoldLeftFollowingTheTypes[A,B](list: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(reverse(list), z)((b:B, a:A) => f(a,b))

    def foldRightViaFoldLeft[A,B](list: List[A], z: B)(f: (A, B) => B): B =
      foldLeft (list, identity[B](_))((g,a) => b => g(f(a,b)))(z)

    def foldLeftViaFoldRight[A,B](list: List[A], z:B)(f:(B,A) => B): B =
      foldRight(list, identity[B](_))((a, g) => b => g(f(b,a)))(z)

    /*
     * Implement append in terms of either foldLeft or foldRight.
     */
    def appendRight[A](la: List[A], lb: List[A]): List[A] = foldRight(la, lb)((a, l) => List(a).append(l))

    def appendLeft[A](la: List[A], lb: List[A]): List[A] = foldLeft(lb, la)((l, a) => l.append(List(a)))

    /*
     * Easier than it Looks! Write a function that concatenates a list of lists into a single list.
     * Its runtime should be linear in the total length of all lists.
     * Try to use functions we have already defined.
     */
    def concat[A](lla: List[List[A]]): List[A] = foldRight(lla, Nil:List[A])(append)

    /*
     * Write a function that transforms a list of integers by adding 1 to each element.
     */
    def succ(l: List[Int]): List[Int] = l match {
      case Nil => Nil
      case Cons(h,t) => Cons(h + 1, succ(t))
    }

    /*
     * Write a function that turns each value in a List[Double] into a String. You can use the expression d.toString
     * to convert some d: Double to a String.
     */
    def listOfDoubleToString(l: List[Double]): List[String] = l match {
      case Nil => Nil
      case Cons(h,t) => Cons(h.toString, listOfDoubleToString(t))
    }

    def listOfDoubleToStringViaFoldRight(l: List[Double]): List[String] =
      l.foldRight(Nil: List[String])((d,l) => Cons(d.toString, l))

    /*
     * Write a function map that generalizes modifying each element in a list while maintaining the structure of the list.
     */
    def map[A,B](l: List[A])(f: A => B): List[B] = l match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h), map(t)(f))
    }

    def mapViaFoldRight[A,B](l: List[A])(f:A => B): List[B] = l.foldRight(Nil: List[B])((h,t) => Cons(f(h), t))

    /*
     * Write a function filter that removes elements from a list unless they satisfy a given predicate.
     * Use it to remove all odd numbers from a List[Int]
     */
    def filter[A](l: List[A])(p: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(h,t) if !p(h) => filter(t)(p)
      case Cons(h,t) => Cons(h, filter(t)(p))
    }

    def filterViaFoldRight[A](l: List[A])(p: A => Boolean): List[A] =
      l.foldRight(Nil: List[A])((h,t) => if (p(h)) Cons(h,t) else t)

    /*
     * Write a function flatMap that works like map except that the function given will return a list
     * instead of a single result, and that list should be inserted into the final resulting list.
     */
    def flatMap[A,B](l: List[A])(f:A => List[B]): List[B] = concat(map(l)(f))

    /*
     * Use flatMap to implement filter.
     */
    def filterViaFlatMap[A](l: List[A])(p: A => Boolean): List[A] = l.flatMap(x => if (p(x)) List(x) else Nil)

    /*
     * Write a function that accepts two lists and constructs a new list by adding corresponding elements.
     * For example, List(1,2,3) and List(4,5,6) become List(5,7,9).
     */
    def mergeList(la: List[Int], lb: List[Int]): List[Int] = (la, lb) match {
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, mergeList(ta, tb))
      case _ => Nil
    }

    /*
     * Generalize the function you just wrote so that it’s not specific to integers or addition.
     * Name your generalized function zipWith.
     */
    def zipWith[A,B,C](la: List[A], lb: List[B])(f: (A,B) => C): List[C] = (la, lb) match {
      case (Cons(ha,ta), Cons(hb, tb)) => Cons(f(ha,hb), zipWith(ta, tb)(f))
      case _ => Nil
    }

    /*
     * Not so Hard: As an example, implement hasSubsequence for checking whether a List
     * contains another List as a subsequence.
     * For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others.
     */
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
      @tailrec
      def loop(list: List[A], sublist: List[A]): Boolean = (list, sublist) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(hl, tl), Cons(hs, ts)) => if (hl == hs) loop(tl, ts) else loop(tl, sublist)
      }
      loop(sup, sub)
    }


  }
}





