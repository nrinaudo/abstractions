package com.nrinaudo.abstractions

import simulacrum.{op, typeclass}

@typeclass trait Semigroup[A] {
  @op("|+|", alias=true)
  def append(a: A, b: A): A
}

object Semigroup {
  implicit def fromSemigroupHK[F[_]: SemigroupHK, A]: Semigroup[F[A]] = SemigroupHK[F].concrete[A]
}