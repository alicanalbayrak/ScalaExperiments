object RationalWorksheet {

  val x = new Rational(1, 3)
  val y = new Rational(5, 7)
  val z = new Rational(3, 2)

  x.numer
  x.denom
  x.sub(y).sub(z)
  y.add(y)
  x.less(y)
  new Rational(2)

  class Rational(x: Int, y: Int) {

    require(y != 0, "y must be non zero int")

    def this(x: Int) = this(x, 1)

    // finds greatest comoon divisor
    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    private val g = gcd(x, y)

    // Not recalculated later
    //    val numer = x / g
    //    val denom = y / g

    def numer = x / g

    def denom = y / g

    def less(that: Rational) = this.numer * that.denom < that.numer * this.denom

    def add(that: Rational) = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

    def neg: Rational = new Rational(-numer, denom)

    def sub(that: Rational) = new Rational(numer * that.denom - that.numer * denom, denom * that.denom)

    override def toString: String = numer + "/" + denom
  }


}