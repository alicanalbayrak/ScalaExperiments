
// Exercise on custom init function

def myInitFunction[T](xs: List[T]): List[T] = xs match {
  case List() => throw new Error("Empty list")
  case List(x) => Nil
  case y :: ys => y :: myInitFunction(ys)
}

// Quiz
def flatten(xs: List[Any]): List[Any] = {
  xs match {
    case List() => Nil
    case List(x) =>
      x match {
        case List() => throw new Error("Empty sublist")
        case m :: ms =>
          flatten(List(m)) ::: flatten(ms)
        case value => List(value)
      }
    case y :: ys => flatten(List(y)) ::: flatten(ys)
  }
}

def removeAt[T](xs: List[T], n: Int): List[T] = {

  val splitted = xs.splitAt(n)
  val head = splitted._1
  val tail = splitted._2.drop(1)

  head ::: tail

  // (xs take n) ::: (xs drop n+1)
}

val firstList = 4 :: 7 :: 5 :: Nil

val secondList = 1 :: 2 :: 3 :: Nil

val mergedList = firstList ::: secondList ::: List(21)

mergedList.length
mergedList last

mergedList init

mergedList take 4

val thirdList = List(56, 57, 58)

val concat = mergedList ++ thirdList
val reversedConcat = concat.reverse

val duplicate = firstList ::: (List(4) :: List(363))

duplicate indexOf (363)

1 :: List(2, 3)

4 :: 3 :: 2 :: List(2, 3).::(1)

val newList = 1 :: 2 :: 3 :: 4 :: 5 :: Nil

myInitFunction(newList)

flatten(List(List(1, 1), 2, List(3, List(5, 8))))
flatten(List(List(1, 1), 2, List(3, List(5, 8), List(List(List(56, 57, 58))))))



