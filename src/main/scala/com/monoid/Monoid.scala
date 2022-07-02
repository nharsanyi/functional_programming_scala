package com.monoid

trait SemiGroup[A] {
  def op(a1: A, a2: A): A
}

trait Monoid[A] extends SemiGroup[A] {
  def zero: A
}


