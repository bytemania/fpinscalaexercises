package fpinscala.adt {

  object MainList extends App {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
    }
    assert(3 == x)

    assert(List(1,2,3,4,5).take(3) == List(1,2,3))
    assert(List(1,2,3,4,5).takeWhile(_ < 3) == List(1,2))
    assert(!List(1,2,3).forall(_ > 5))
    assert(List(1,2,3).forall(_ < 5))
    assert(!List(1,2,3).exists(_ == 5))
    assert(List(1,2,3).exists(_ == 2))
    assert(List(1,2,3).scanLeft(0)(_ + _) == List(0,1,3,6))
    assert(List(1,2,3).scanRight(0)(_ + _) == List(6,5,3,0))

    assert(List.fill(4, 3) == List(3,3,3,3))
    assert(List.sum(List(1,2,3,4)) == 10)
    assert(List.product(List(1,2,3,4)) == 24)
    assert(List(1,2,3,4).length == 4)
    assert(List.tail(List(1,2,3,4)) == List(2,3,4))
    assert(List(1,2,3,4).setHead(5) == List(5,2,3,4))
    assert(List(1,2,3,4).drop(2) == Cons(3, Cons(4, Nil)))
    assert(List(1,2,3,4,5).dropWhile(_ < 4) == Cons(4, Cons(5, Nil)))
    assert(List(1,2).append(List(3,4)) == Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
    assert(List(1,2,3,4,5).init == Cons(1, Cons(2, Cons(3, Cons(4, Nil)))))
    assert(List(1,2,3).foldRight(Nil: List[Int])(Cons(_, _)) == Cons(1, Cons(2, Cons(3, Nil))))
    assert(List(1,2,3).foldLeft(1)(_ * _) == 6)
    assert(List.reverse(List(1, 2, 3, 4)) == List(4,3,2,1))
    assert(List.foldRightViaFoldLeftFollowingTheTypes(List(1,2,3), Nil: List[Int])(Cons(_, _)) == List(1,2,3))
    assert(List.foldRightViaFoldLeft(List(1,2,3), Nil: List[Int])(Cons(_, _)) == List(1,2,3))
    assert(List.foldLeftViaFoldRight(List(1,2,3), 1)(_ * _) == 6)
    assert(List.appendRight(List(1,2),List(3,4)) == Cons(1,Cons(2,Cons(3,Cons(4,Nil)))))
    assert(List.appendLeft(List(1,2), List(3,4)) == Cons(1,Cons(2,Cons(3,Cons(4,Nil)))))
    assert(List.concat(List(List(1,2),List(3,4))) == Cons(1,Cons(2,Cons(3,Cons(4,Nil)))))
    assert(List.succ(List(1,2,3,4)) == Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
    assert(List.listOfDoubleToString(List(1.0,2.0,3.0,4.0)) == Cons("1.0",Cons("2.0",Cons("3.0",Cons("4.0",Nil)))))
    assert(List.listOfDoubleToStringViaFoldRight(List(1.0,2.0,3.0,4.0)) == Cons("1.0",Cons("2.0",Cons("3.0",Cons("4.0",Nil)))))
    assert(List(1.0,2.0,3.0,4.0).map(_.toString) == Cons("1.0",Cons("2.0",Cons("3.0",Cons("4.0",Nil)))))
    assert(List.mapViaFoldRight(List(1.0,2.0,3.0,4.0))(_.toString) == List("1.0","2.0","3.0","4.0"))
    assert(List(1,2,3,4,5).filter(x => x > 1 && x < 4) == List(2,3))
    assert(List.filterViaFoldRight(List(1,2,3,4,5))(x => x > 1 && x < 4) == List(2, 3))
    assert(List(1,2,3).flatMap(i => List(i,i)) == List(1,1,2,2,3,3))
    assert(List.filterViaFlatMap(List(1,2,3,4,5))(x => x > 1 && x < 4) == List(2,3))
    assert(List.mergeList(List(1,2,3), List(4,5,6)) == List(5,7,9))
    assert(List(1,2,3).zipWith(List("un","deux", "trois"))((_,_)) == List((1,"un"),(2, "deux"),(3,"trois")))
    assert(List(1,2,3,4).hasSubsequence(List(2,3)))
    assert(!List(1,2,3,4).hasSubsequence(List(23)))
  }
}
