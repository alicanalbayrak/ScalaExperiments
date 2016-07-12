
def square(x: Int) = x * x

// ===========================================
// Transition with pattern match
// ===========================================
def squareList(xs: List[Int]): List[Int] =
  xs match {
    case Nil => Nil
    case y :: ys => square(y) :: squareList(ys)
  }


// ===========================================
//  Mapping
// ===========================================
def squareListMap(xs: List[Int]): List[Int] =
  xs map (x => square(x))


squareList(List(1, 2, 3, 4))
squareListMap(List(1, 2, 3, 4))

