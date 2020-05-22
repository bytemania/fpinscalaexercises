package fpinscala.adt {

  sealed trait Tree[+A] {
    /*
     * Write a function size that counts the number of nodes (leaves and branches) in a tree.
     */
    val size: Int = this match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + l.size + r.size
    }

    /*
    * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
    */
    val depth: Int = this match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (l.depth max r.depth)
    }

    /*
     * Write a function map, analogous to the method of the same name on List,
     * that modifies each element in a tree with a given function.
     */
    def map[B](f: A => B): Tree[B] = this match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l,r) => Branch(l.map(f), r.map(f))
    }

    /*
     * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
     * Reimplement them in terms of this more general function. Can you draw an analogy between this fold function
     * and the left and right folds for List?
     */
    def fold[B](f: A => B)(g: (B,B) => B): B = this match {
      case Leaf(v) => f(v)
      case Branch(l,r) => g(l.fold(f)(g), r.fold(f)(g))
    }

    val sizeViaFold: Int = this.fold(_ => 1)(1 + _ + _)
    val depthViaFold: Int = this.fold(_ => 0)(1 + math.max(_,_))
    def mapViaFold[B >: A, C](t: Tree[B])(f: B => C): Tree[C] = t.fold[Tree[C]](v => Leaf(f(v)))(Branch(_,_))
  }

  object Tree {

    /*
     * Write a function maximum that returns the maximum element in a Tree[Int]
     */
    def maximum(tree: Tree[Int]): Int = tree match {
      case Leaf(v) => v
      case Branch(l,r) => maximum(l) max maximum(r)
    }

    def maximumViaFold(t: Tree[Int]): Int = t.fold(identity)(math.max)

  }

  final case class Leaf[A](value: A) extends Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

}