package com.nrinaudo.abstractions

import simulacrum._

/** Type class for type constructors that can be mapped over. */
@typeclass trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  def compose[G[_]: Functor]: Functor[λ[X => F[G[X]]]] =
    new Functor.Composite[F, G] {
      override def F = Functor.this
      override def G = Functor[G]
    }
}

object Functor {
  implicit def composite[F[_]: Functor, G[_]: Functor]: Functor[λ[X => F[G[X]]]] = Functor[F].compose(Functor[G])

  trait Composite[F[_], G[_]] extends Functor[λ[X => F[G[X]]]] {
    def F: Functor[F]
    def G: Functor[G]

    override def map[A, B](fga: F[G[A]])(f: A => B): F[G[B]] = F.map(fga)(G.lift(f))
  }

  def idFunctor[X]: Functor[Id] = new Functor[Id] {
    override def map[A, B](fa: Id[A])(f: (A) => B): Id[B] = f(fa)
  }
}
