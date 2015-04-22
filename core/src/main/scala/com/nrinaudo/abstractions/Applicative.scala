package com.nrinaudo.abstractions

import simulacrum._

/** Applicative functors. */
@typeclass trait Applicative[F[_]] extends Apply[F] { self =>
  // - Abstract methods ------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def pure[A](a: A): F[A]



  // - Functor implementation ------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  /** Default implementation of `map` using {{{ap}}} and {{{pure}}}.
    *
    * Specific `Applicative` instances often have a more efficient implementation that directly manipulates the
    * underlying data structure.
    */
  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))


  // - Tools -----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  def *>[A, B](fa: F[A], fb: F[B]): F[B] = apply2(fa, fb)(pure((_, b) => b))
  def <*[A, B](fa: F[A], fb: F[B]): F[A] = apply2(fa, fb)(pure((a, _) => a))

  def compose[G[_]: Applicative]: Applicative[λ[X => F[G[X]]]] = new Applicative.Composite[F, G] {
    override def F = self
    override def G = Applicative[G]
  }


  def toMonoid: Monoid[F[Unit]] = new Monoid[F[Unit]] {
    override def empty: F[Unit] = self.pure(())
    override def append(a: F[Unit], b: F[Unit]): F[Unit] = self.*>(a, b)
  }
}

object Applicative {
  trait Composite[F[_], G[_]] extends Applicative[λ[X => F[G[X]]]] with Apply.Composite[F, G] {
    def F: Applicative[F]
    def G: Applicative[G]

    override def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
  }
}
