package com.streams

// covariant -> if A,B and B subtype of A, then Stream[B] is subtype of Stream[A]
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def forAll(p: A => Boolean): Boolean = this match{
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](head: () => A, tail: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val h = hd // cache to avoid recalculation
    lazy val t = tl
    Cons(() => h, () => t)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail:_*))

  def from(n: Int): Stream[Int] = {
     cons(n, from(n + 1))
  }
}
