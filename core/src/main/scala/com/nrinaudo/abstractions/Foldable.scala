package com.nrinaudo.abstractions

import simulacrum.typeclass
import ops._

/** Type class for data structures that can be folded onto themselves. */
@typeclass trait Foldable[F[_]] { self =>
  def foldRight[A, B](fa: F[A], init: B)(f: (A, => B) => B): B

  // TODO: it's almost certainly possible to write foldLeft in terms of foldLeft by creating and wrapping thunks.
  def foldLeft[A, B](fa: F[A], init: B)(f: (B, A) => B): B


  def fold[A: Monoid](fa: F[A]): A = foldMap(fa)(identity)
  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B = foldLeft(fa, Monoid[B].empty)((b, a) => f(a) |+| b)

  /** Higher-kinded `fold`. */
  def foldHK[M[_]: MonoidHK, A](fa: F[M[A]]): M[A] = foldMapHK(fa)(identity)

  /** Higher-kinded `foldMap`. */
  def foldMapHK[M[_]: MonoidHK, A, B](fa: F[A])(f: A => M[B]): M[B] =
    foldMap(fa)(f)(MonoidHK[M].concrete[B])

  def asum[G[_]: MonoidHK, A](fga: F[G[A]]): G[A] =
    foldLeft(fga, MonoidHK[G].empty[A])(_ |+| _)


  // TODO: keep? remove?
  /*
  def sequence_[G[_]: Applicative, A, B](fga: F[G[A]]): G[Unit] =
      fold(fga)(Applicative[G].asMonoid)

  def traverse_[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[Unit] =
    foldMap(fa)(f)(Applicative[G].asMonoid)
    //foldLeft(fa, Applicative[G].pure(()))((acc, a) => Applicative[G].map2(acc, f(a))((_, _) => ()))
    */



  def compose[G[_]: Foldable]: Foldable[λ[X => F[G[X]]]] = new Foldable.Composite[F, G] {
    override def F = self
    override def G = Foldable[G]
  }
}

object Foldable {
  trait Composite[F[_], G[_]] extends Foldable[λ[X => F[G[X]]]] {
    implicit def F: Foldable[F]
    implicit def G: Foldable[G]

    override def foldLeft[A, B](fa: F[G[A]], init: B)(f: (B, A) => B): B =
      F.foldLeft(fa, init)((b, ga) => G.foldLeft(ga, b)(f))

    override def foldRight[A, B](fa: F[G[A]], init: B)(f: (A, => B) => B): B =
      F.foldRight(fa, init)((ga, b) => G.foldRight(ga, b)(f))
  }
}
