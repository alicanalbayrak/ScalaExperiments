
object ListFun {

  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")

  nums filter (x => x > 0)
  //  res0: List[Int] = List(2, 5, 7, 1)

  nums filterNot (x => x > 0)
  //  res1: List[Int] = List(-4)

  /* (xs filter p, xs filterNot p) in a single traversal */
  nums partition (x => x > 0)
  // res2: (List[Int], List[Int]) = (List(2, 5, 7, 1),List(-4))

  /* take elements until conditions were not met */
  nums takeWhile (x => x > 0)
  // res3: List[Int] = List(2)

  /* take elements until conditions were not met */

  nums dropWhile (x => x > 0)
  // res4: List[Int] = List(-4, 5, 7, 1)

  /* (xs takeWhile p, xs dropWhile p) in a single traversal */
  nums span (x => x > 0)

  // -----------------------------------------------------------

  /**
    * Function pack that packs consecutive duplicates of list elements into sublists.
    */

  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
  }

  pack(List("a", "a", "a", "b", "c", "c", "a"))
  // res6: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a))


}