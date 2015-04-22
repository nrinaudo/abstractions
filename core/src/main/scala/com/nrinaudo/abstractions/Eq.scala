package com.nrinaudo.abstractions

import simulacrum._

/** Type class used to define type safe equality. */
@typeclass trait Eq[E] {
  /** Type safe equality. */
  @op("===") def equal(e1: E, e2: E): Boolean

  /** Type safe inequality. */
  @op("=/=") def notEqual(e1: E, e2: E): Boolean = !equal(e1, e2)
}

object Eq {
  /** Creates a new {{{Eq}}} instance using the specified function for comparison. */
  def apply[A](f: (A, A) => Boolean): Eq[A] = new Eq[A] {
    override def equal(e1: A, e2: A): Boolean = f(e1, e2)
  }

  /** Creates a new {{{Eq}}} instance using the natural `==` operator for comparison. */
  def natural[A]: Eq[A] = Eq(_ == _)
}
