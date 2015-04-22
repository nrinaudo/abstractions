package com.nrinaudo.abstractions

import simulacrum.{op, typeclass}

@typeclass trait FlatMap[F[_]] extends Apply[F] {
  @op(">>=", alias = true)
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(identity)

  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))
}
