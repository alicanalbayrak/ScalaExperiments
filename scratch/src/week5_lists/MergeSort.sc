def msort(xs: List[Int]): List[Int] = {

  val n = xs.length / 2
  if (n == 0) xs
  else {

    /* Use pair match instead
    def merge(xs: List[Int], ys: List[Int]): List[Int] = {
      xs match {
        case Nil =>
          ys
        case x :: xs1 =>
          ys match {
            case Nil => xs
            case y :: ys1 =>
              if (x < y) x :: merge(xs1, ys)
              else y :: merge(xs, ys1)
          }
      }
    }
    */
    def merge(xs: List[Int], ys: List[Int]): List[Int] = {
      (xs, ys) match {

        case (Nil, ys_1) => ys

        case (xs_1, Nil) => xs

        case (x :: xs1, y :: ys1) =>

          if (x < y) x :: merge(xs1, ys)

          else y :: merge(xs, ys1)

      }
    }

    val (fst, snd) = xs splitAt n

    merge(msort(fst), msort(snd))

  }
}


msort(List(6, 3, 5, 12, 1, 91, 2, 9, 8, 7, 6))

