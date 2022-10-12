package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  def exhaustHeap(h: H): List[A] = h match {
    case Nil if isEmpty(h) => Nil
    case _ => findMin(h) :: exhaustHeap(deleteMin(h))
  }

  lazy val genHeap: Gen[H] =
    for {
      int <- arbitrary[A]
      heap <- oneOf(Gen.const(empty), genHeap)
    } yield insert(int, heap)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("findMin of inserted min should return expected min") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("findMin of single element heap should return given element") = forAll { (a: A) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("findMin of 2 elements inserted into empty heap should return min of two elements") = forAll { (a: A, b: A) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == a.min(b)
  }

  property("deleteMin of single element heap should return empty element") = forAll { (a: A) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("findMin/deleteMin recursively on heap should return sorted sequence") = forAll { (h: H) =>
    val exhausted = exhaustHeap(h)
    exhausted == exhausted.sorted
  }

  property("Minimum of melding of any two heaps should return the minimum of one and the other") = forAll {
    (h1: H, h2: H, a: A) =>
      val m1 = if isEmpty(h1) then 0 else findMin(h1)
      val m2 = if isEmpty(h2) then 0 else findMin(h2)
      val h12 = insert(a, meld(h1, h2))
      findMin(h12) == m1.min(m2).min(a)
  }

  property("findMin/deleteMin recursively on melded heaps should return sorted sequence") = forAll {
    (h1: H, h2: H) =>
    val exhausted = exhaustHeap(meld(h1, h2))
    exhausted == exhausted.sorted
  }

  property("Moving elements from one heap to another and melding should be equivalent to melding original heaps") = forAll {
    (h1: H, h2: H) =>
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      val newh1 = insert(m2, deleteMin(h1))
      val newh2 = insert(m1, deleteMin(h2))
      exhaustHeap(meld(newh1, newh2)) == exhaustHeap(meld(h1, h2))
  }
