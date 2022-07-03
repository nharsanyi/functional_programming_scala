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

  "headOptionWithFold" should "return head if stream not empty" in {
    val stream = streams.Stream.apply(10, 20, 30)
    assertResult(Some(10))(stream.headOptionWithFold())
  }

  "headOptionWithFold" should "return None if stream empty" in {
    val stream = streams.Stream.empty[Int]
    assertResult(None)(stream.headOptionWithFold())
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

  "toList" should "return stream as list if stream not empty" in {
    val stream = streams.Stream.apply(1, 3, 7, 8)
    assertResult(List(1, 3, 7, 8))(stream.toList)
  }
  "toList" should "return stream as empty list if stream empty" in {
    val stream = streams.Stream.apply()
    assertResult(Nil)(stream.toList)
  }

  "takeWhile" should "iterate stream while condition met" in {
    val stream = streams.Stream.apply(2, 1, 3, 7, 8, 0)
    assertResult(List(2, 1, 3))(stream.takeWhile(_ < 4).toList)
  }

  "takeWhile" should "iterate stream while condition met 2" in {
    val stream = streams.Stream.apply(2, -1, 3, 7, 8, 0)
    assertResult(Nil)(stream.takeWhile(_ < 0).toList)
  }

  "takeWhileWithFold" should "iterate stream while condition met" in {
    val stream = streams.Stream.apply(2, 1, 3, 7, 8, 0)
    assertResult(List(2, 1, 3))(stream.takeWhileWithFold(_ < 4).toList)
  }

  "takeWhileWithFold" should "iterate stream while condition met 2" in {
    val stream = streams.Stream.apply(2, -1, 3, 7, 8, 0)
    assertResult(Nil)(stream.takeWhileWithFold(_ < 0).toList)
  }

  "map" should "map stream to another" in {
    val stream = streams.Stream.apply(2, 3, 5, 1)
    assertResult(List(4, 6, 10, 2))(stream.map(_ * 2).toList)
  }

  "map" should "map empty stream to another empty stream" in {
    val stream = streams.Stream.empty[Int]
    val actual = stream.map(_.toString)
    assertResult(streams.Stream.empty[String])(actual)
    assertResult(Nil)(actual.toList)
  }

  "take" should "return first n element" in {
    val stream = streams.Stream.apply(2, 3, 5, 1)
    assertResult(List(2, 3))(stream.take(2).toList)
  }

  "take" should "return first min(n, s=stream.size) element if n is greater than s" in {
    val stream = streams.Stream.apply(2, 3, 5, 1)
    assertResult(List(2, 3, 5, 1))(stream.take(10).toList)
  }

  "fromWithUnfold" should "generate infinite stream starting from n" in {
    val stream = streams.Stream.fromWithUnfold(10)
    assertResult(List(10, 11, 12, 13, 14, 15))(stream.take(6).toList)
  }

  "fromWithUnfold" should "generate infinite stream starting from negative n" in {
    val stream = streams.Stream.fromWithUnfold(-10)
    assertResult(List(-10, -9, -8, -7, -6, -5))(stream.take(6).toList)
  }
}
