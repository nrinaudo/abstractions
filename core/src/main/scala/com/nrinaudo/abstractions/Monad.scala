package com.nrinaudo.abstractions

import simulacrum.typeclass

@typeclass trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  override def map[A, B](fa: F[A])(f: A => B) = flatMap(fa)(a => pure(f(a)))
}

object Monad {
  def idMonad: Monad[Id] = new Monad[Id] {
    override def flatMap[A, B](a: Id[A])(f: A => Id[B]) = f(a)
    override def pure[A](a: A) = a
  }
}
