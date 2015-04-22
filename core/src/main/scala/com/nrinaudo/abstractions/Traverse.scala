package com.nrinaudo.abstractions

import simulacrum.typeclass

@typeclass trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
  def sequence[G[_]: Applicative, A](fga: F[G[A]]): G[F[A]] = traverse(fga)(identity)


  def compose[G[_]: Traverse]: Traverse[λ[X => F[G[X]]]] = new Traverse.Composite[F, G] {
    override val F = self
    override val G = Traverse[G]
  }

  // - Functor implementation -----------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(Monad.idMonad)



  // - Foldable implementation -----------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  override def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B =
    traverse[Const[B, ?], A, Nothing](fa)(f)(Monoid[B].asApplicative)

  // TODO: work this out.
  override def foldLeft[A, B](fa: F[A], init: B)(f: (B, A) => B): B =
    foldMap(fa)(a => (b: B) => f(b, a))(Monoid.endoMonoid[B].dual)(init)
}


object Traverse {
  trait Composite[F[_], G[_]] extends Traverse[λ[X => F[G[X]]]] with Foldable.Composite[F, G] with Functor.Composite[F, G] {
    implicit def F: Traverse[F]
    implicit def G: Traverse[G]

    override def traverse[H[_] : Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
      F.traverse(fga)(ga => G.traverse(ga)(f))
  }
}