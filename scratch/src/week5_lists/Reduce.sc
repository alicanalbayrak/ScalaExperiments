
object Reduce {


  def sum(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x + y)

  def product(xs: List[Int]) = (1 :: xs) reduceLeft ((x, y) => x * y)

  sum(List(1, 2, 3))
  product(List(1, 2, 3))

  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    (xs foldRight List[U]()) ((x, y) => f(x) :: y)

  def lengthFun[T](xs: List[T]): Int =
    (xs foldRight 0) ((x, y) => y + 1)

  // (x: Int) => x * x
  def square(x: Int) = x * x

  mapFun(List(1, 2, 3, 5), square)

  lengthFun(List(1, 2, 3, 5))

}