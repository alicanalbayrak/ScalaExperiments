package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._


abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      x <- arbitrary[Int]
      y <- oneOf(const(empty), genHeap)
    } yield insert(x, y)

  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * If you insert any two elements into an empty heap, finding the minimum of
    * the resulting heap should get the smallest of the two elements back.
    */
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    Math.min(a, b) == findMin(h)
  }

  /**
    * If you insert an element into an empty heap, then delete the minimum,
    * the resulting heap should be empty.
    */
  property("deleteMin") = forAll { a: Int =>
    val h = insert(a, empty)
    empty == deleteMin(h)
  }


  /**
    * Given any heap, you should get a sorted sequence of elements when
    * continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)
    */
  property("sortedSeq") = forAll { h: H =>
    def sortedSeq(h: H, acc: List[Int]): List[Int] = {
      if (isEmpty(h)) Nil
      else
        findMin(h) :: sortedSeq(deleteMin(h), acc)
    }

    val s = sortedSeq(h, List())
    s == s.sorted
  }

  /**
    * Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
    */
  property("meldMin") = forAll { (h1: H, h2: H) =>
    val meldMin = findMin(meld(h1, h2))
    meldMin == findMin(h1) || meldMin == findMin(h2)
  }

}
