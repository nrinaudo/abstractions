package com.nrinaudo.abstractions

import simulacrum.typeclass

@typeclass trait Monad[F[_]] extends FlatMap[F] with Applicative[F] {
  override def map[A, B](fa: F[A])(f: A => B) = flatMap(fa)(a => pure(f(a)))
}