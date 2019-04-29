package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for{
    angka <- arbitrary[Int]
    cupu <- oneOf[H]( Gen.const(empty),genHeap )
  } yield insert(angka, cupu)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("minimum 1") = forAll{ (h:H, angka1:Int, angka2:Int) =>
    val min = if(angka1 < angka2) angka1 else angka2
    val que = insert(angka1, insert(angka2, empty))
    val minFromQue = findMin(que)
    minFromQue == min
  }

  property("pass the link") = forAll{ (h1:H, h2:H) =>
    val minA = findMin(h1)
    val minB = findMin(h2)
    val minAll =  if( minA < minB) minA else minB
    val c = meld(h1,h2)
    val minC = findMin(c)
    minC == minAll
  }

  property("listMinimumValue") = forAll { (h1: H, h2: H) =>
    def generateMinimum(ts: H, as: List[Int]): List[Int] = {
      if (isEmpty(ts)) as
      else findMin(ts) :: generateMinimum(deleteMin(ts), as)
    }

    val meld1 = meld(h1, h2)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    //generateMinimum(meld2, Nil) == generateMinimum(meld1, Nil)
    generateMinimum(meld1, Nil) == generateMinimum(meld2, Nil)
  }



}
