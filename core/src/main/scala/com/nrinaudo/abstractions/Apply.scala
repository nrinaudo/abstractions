package com.nrinaudo.abstractions

import simulacrum._

@typeclass trait Apply[F[_]] extends Functor[F] {
  @op("<*>") def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  @noop def apply2[A, B, R](fa: F[A], fb: F[B])(f: F[(A, B) => R]): F[R] =
    ap(fa)(ap(fb)(map(f)(ff => (b: B) => (a: A) => ff(a, b))))

  @noop def map2[A, B, X](fa: F[A], fb: F[B])(f: (A, B) => X): F[X] =
    ap(fa)(map(fb)(b => (a: A) => f(a, b)))

  def compose[G[_]: Apply]: Apply[λ[X => F[G[X]]]] =
    new Apply.Composite[F, G] {
      override def F = Apply.this
      override def G = Apply[G]
    }
}

object Apply {
  trait Composite[F[_], G[_]] extends Functor.Composite[F, G] with Apply[λ[X => F[G[X]]]] {
    def F: Apply[F]
    def G: Apply[G]

    override def ap[A, B](fga: F[G[A]])(f: F[G[A => B]]): F[G[B]] =
      F.map2(fga, f)((ga, g) => G.ap(ga)(g))

  }
}