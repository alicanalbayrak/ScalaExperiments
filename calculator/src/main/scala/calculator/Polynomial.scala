package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    Signal {
      Math.pow(b(), 2) - (4 * a() * c())
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {

      delta() match {
        case v if v < 0 => Set()
        case _ =>
          List(-1d, 1d).map(elem => {
            ((-1d * b()) + (elem * Math.sqrt(delta()))) / (2d * a())
          }).toSet
      }


    }
  }


}
