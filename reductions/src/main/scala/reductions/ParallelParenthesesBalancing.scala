package reductions

import common._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {


    def balanceTailRec(idx: Int, acc: Int): Int = {

      if (idx == chars.length || acc < 0) {
        acc
      } else {
        chars(idx) match {
          case '(' => balanceTailRec(idx + 1, acc + 1)
          case ')' => balanceTailRec(idx + 1, acc - 1)
          case _ => balanceTailRec(idx + 1, acc)
        }
      }
    }

    balanceTailRec(0, 0) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx == until) {
        arg1 match{
          case e if e < 0 => (arg1, -1)
          case _ => (arg1, 0)
        }
      } else {
        chars(idx) match {
          case '(' => traverse(idx + 1, until, arg1 + 1, arg2)
          case ')' => traverse(idx + 1, until, arg1 - 1, arg2)
          case _ => traverse(idx + 1, until, arg1, arg2)
        }
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {

      if (until - from < threshold) {
        traverse(from, until, 0, 0)
      } else {

        val mid = from + (until - from) / 2
        val (left, right) = parallel(reduce(from, mid), reduce(mid, until))

        val helper = if (left._2 > right._2 ) 0 else -1
        (left._1 + right._1, helper)

      }

    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
