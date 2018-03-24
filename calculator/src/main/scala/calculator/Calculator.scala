package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr


object Calculator {

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for {
      entry <- namedExpressions
    } yield (entry._1, Var(eval(entry._2(), namedExpressions)))

  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def innerEval(expr: Expr): Double = {
      expr match {
        case Literal(v) => v
        case Plus(a, b) => innerEval(a) + innerEval(b)
        case Minus(a, b) => innerEval(a) - innerEval(b)
        case Times(a, b) => innerEval(a) * innerEval(b)
        case Divide(a, b) => innerEval(a) / innerEval(b)
        case Ref(name) => innerEval(getReferenceExpr(name, references))
      }
    }

    innerEval(expr)

  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
