package com.nrinaudo.abstractions

import simulacrum.typeclass

/** Higher-kinded monoid. */
@typeclass trait MonoidHK[F[_]] extends SemigroupHK[F] { self =>
  def empty[A]: F[A]

  override def concrete[A]: Monoid[F[A]] = new MonoidHK.AsMonoid[F, A] {
    override val F = self
  }
}

object MonoidHK {
  trait AsMonoid[F[_], A] extends Monoid[F[A]] with SemigroupHK.AsSemigroup[F, A] {
    implicit def F: MonoidHK[F]

    override def empty: F[A] = F.empty
  }
}