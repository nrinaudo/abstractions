package com.nrinaudo.abstractions

import simulacrum.{op, typeclass}


object Ordering {
  case object LT extends Ordering
  case object EQ extends Ordering
  case object GT extends Ordering
}
sealed trait Ordering

@typeclass trait Ord[A] extends Eq[A] {
  def compare(a1: A, a2: A): Ordering

  override def equal(a1: A, a2: A): Boolean = compare(a1, a2) == Ordering.EQ
  def lt(a1: A, a2: A): Boolean = compare(a1, a2) == Ordering.LT
  def gt(a1: A, a2: A): Boolean = compare(a1, a2) == Ordering.GT
  def lte(a1: A, a2: A): Boolean = {
    val o = compare(a1, a2)
    o == Ordering.LT || o == Ordering.EQ
  }

  def gte(a1: A, a2: A): Boolean = {
    val o = compare(a1, a2)
    o == Ordering.GT || o == Ordering.EQ
  }

  def min(a1: A, a2: A): A = if(lte(a1, a2)) a1 else a2
  def max(a1: A, a2: A): A = if(gte(a1, a2)) a1 else a2
}

object Ord {
  def Min[A: Ord]: Semigroup[A] = new Semigroup[A] {
    override def append(a: A, b: A): A = Ord[A].min(a, b)
  }

  def Max[A: Ord]: Semigroup[A] = new Semigroup[A] {
    override def append(a: A, b: A): A = Ord[A].max(a, b)
  }
}
