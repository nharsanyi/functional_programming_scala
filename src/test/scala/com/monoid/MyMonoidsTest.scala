package com.monoid

import org.scalatest.flatspec.AnyFlatSpec

class MyMonoidsTest extends AnyFlatSpec  {

  "foldMapV" should "fold elements in splits" in {
    val res = MyMonoids.foldMapV(IndexedSeq[Int](1, 2, 3), MyMonoids.strConcat)((x: Int) => x.toString)
    assertResult("123")(res)
  }

  "foldMapV" should "fold elements in splits if empty" in {
    val res = MyMonoids.foldMapV(IndexedSeq[Int](), MyMonoids.strConcat)((x: Int) => x.toString)
    assertResult("")(res)
  }
}
