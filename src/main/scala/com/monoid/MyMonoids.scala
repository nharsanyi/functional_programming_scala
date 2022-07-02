package com.monoid

object MyMonoids {

  val strConcat = new Monoid[String] {
    override def zero: String = ""

    override def op(a1: String, a2: String): String = a1 + a2
  }
  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = {
      a1 + a2
    }
    override def zero: Int = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int) = {
      a1 * a2
    }
    override def zero: Int = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = {
      a1 || a2
    }

    override def zero: Boolean = false //  T || F => T, F || F => T
  }

  val booleanAnd = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = {
      a1 && a2
    }

    override def zero: Boolean = {
      true // T && T => T, F && T => F
    }
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      v match {
        case IndexedSeq() =>
          println("Empty")
          m.zero
        case IndexedSeq(h) =>
          println(s"h=$h")
          f(h)
        case v =>
          val (p1, p2) = v.splitAt(v.size / 2)
          println(s"splitted: ${p1.mkString("-")} |||| ${p2.mkString("-")}")
          m.op(foldMapV(p1, m)(f), foldMapV(p2, m)(f))
      }
  }


}
