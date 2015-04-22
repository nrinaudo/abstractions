package com.nrinaudo.abstractions

import simulacrum.{typeclass, op}

/** Higher-kinded semigroup. */
@typeclass trait SemigroupHK[F[_]] { self =>
  @op("|+|") def append[A](a: F[A], b: F[A]): F[A]

  def concrete[A]: Semigroup[F[A]] = new SemigroupHK.AsSemigroup[F, A] {
    override val F = self
  }
}

object SemigroupHK {
  trait AsSemigroup[F[_], A] extends Semigroup[F[A]] {
    implicit def F: SemigroupHK[F]

    override def append(a: F[A], b: F[A]): F[A] = F.append(a, b)
  }

}