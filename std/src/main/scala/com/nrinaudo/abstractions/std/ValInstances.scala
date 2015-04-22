package com.nrinaudo.abstractions.std

import com.nrinaudo.abstractions._
import simulacrum.op

trait ValInstances {
  // - Eq instances ----------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  implicit val intEq: Eq[Int]       = Eq.natural
  implicit val floatEq: Eq[Float]   = Eq.natural
  implicit val longEq: Eq[Long]     = Eq.natural
  implicit val doubleEq: Eq[Double] = Eq.natural
  implicit val boolEq: Eq[Boolean]  = Eq.natural
  implicit val strEq: Eq[String]    = Eq.natural



  // - Monoid instances ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** {{{Boolean}}} monoid instance that aggregates values with the boolean `and`.
    *
    * Folding over a collection of booleans will yield `true` if and only if all values are `true`.
    */
  val Conjunction: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty                          = true
    override def append(a: Boolean, b: Boolean) = a && b
  }

  /** {{Boolean}} monoid instance that aggregates values with the boolean `or`.
    *
    * Folding over a collection of booleans instance will yield `true` at least one value is `true`.
    */
  val Disjunction: Monoid[Boolean] = new Monoid[Boolean] {
    override def empty                          = false
    override def append(a: Boolean, b: Boolean) = a || b
  }

  /** {{Int}} monoid instance that aggregates values with `+`.
    *
    * Folding over a collection of ints will yield the sum of all values, or `0` if the collection is empty.
    */
  implicit val Addition: Monoid[Int] = new Monoid[Int] {
    override def empty                  = 0
    override def append(a: Int, b: Int) = a + b
  }

  /** {{Int}} monoid instance that aggregates values with `*`.
    *
    * Folding over a collection of ints will yield the product of all values, or `1` if the collection is empty.
    */
  val Multiplication: Monoid[Int] = new Monoid[Int] {
    override def empty                  = 1
    override def append(a: Int, b: Int) = a * b
  }
}
