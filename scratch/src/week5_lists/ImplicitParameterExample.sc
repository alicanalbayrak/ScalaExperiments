
// =============================================
//  Passing comparison function directly
// =============================================

def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {

  val n = xs.length / 2

  if (n == 0) xs

  else {

    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {

      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>

        if (lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)

    }


    val (fst, snd) = xs splitAt n

    merge(msort(fst)(lt), msort(snd)(lt))

  }
}

val nums = List(2, -4, 5, 7, 1)
val fruits = List("apple", "pineapple", "orange", "banana")

msort(nums)((x: Int, y: Int) => x < y)
msort(fruits)((x: String, y: String) => x.compareTo(y) < 0)


// =============================================
//  Using Scala ordering class
// =============================================

def msortOrdering[T](xs: List[T])(ord: Ordering[T]): List[T] = {

  val n = xs.length / 2

  if (n == 0) xs

  else {

    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {

      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>

        if (ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)

    }

    val (fst, snd) = xs splitAt n

    merge(msortOrdering(fst)(ord), msortOrdering(snd)(ord))
  }
}


msortOrdering(nums)(Ordering.Int)
msortOrdering(fruits)(Ordering.String)



// =============================================
//  "Compiler will figure out right implicit
//  to pass, based on demanded type."



// =============================================

def msortOrderingImplicit[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {

  val n = xs.length / 2

  if (n == 0) xs

  else {

    def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {

      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (x :: xs1, y :: ys1) =>

        if (ord.lt(x, y)) x :: merge(xs1, ys)
        else y :: merge(xs, ys1)

    }

    val (fst, snd) = xs splitAt n

    merge(msortOrderingImplicit(fst), msortOrderingImplicit(snd)
  }
}


msortOrderingImplicit(nums)
msortOrderingImplicit(fruits)
