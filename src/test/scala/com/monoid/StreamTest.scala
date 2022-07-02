package com.monoid

import com.streams
import org.scalatest.flatspec.AnyFlatSpec

class StreamTest extends AnyFlatSpec  {

  "headOption" should "return head if stream not empty" in {
    val stream = streams.Stream.apply(10, 20, 30)
    assertResult(Some(10))(stream.headOption)
  }

  "headOption" should "return None if stream empty" in {
    val stream = streams.Stream.empty[Int]
    assertResult(None)(stream.headOption)
  }

  "forAll" should "return false if stream not empty and condition applied" in {
    val stream = streams.Stream.apply(10, 20, 30)
    assertResult(false)(stream.forAll(_ < 30))
  }

  "forAll" should "return correct result if stream not empty and condition applied" in {
    val stream = streams.Stream.apply(10, 20, 30)
    assertResult(true)(stream.forAll(_ % 10 == 0))
  }

  "forAll" should "return True if stream empty" in {
    val stream = streams.Stream.empty[Int]
    assertResult(None)(stream.headOption)
  }

  "exists" should "return true if exists" in {
    val stream = streams.Stream.apply(1, 3, 7, 8)
    assertResult(true)(stream.exists(_ % 2 == 0))
  }

  "exists" should "return false if does not exists" in {
    val stream = streams.Stream.apply(1, 3, 7, 8)
    assertResult(false)(stream.exists(_ > 10))
  }
}
