package com.nrinaudo.abstractions

import simulacrum.typeclass

@typeclass trait Monoid[A] extends Semigroup[A] { self =>
  def empty: A

  def asApplicative: Applicative[Const[A, ?]] = new Monoid.AsApplicative[A] {
    override val M = self
  }

  def dual: Monoid[A] = new Monoid[A] {
    override def empty: A = self.empty
    override def append(a: A, b: A): A = self.append(b, a)
  }
}

object Monoid {
  implicit def fromMonoidHK[F[_]: MonoidHK, A]: Monoid[F[A]] = MonoidHK[F].concrete[A]

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def empty: A => A = identity
    override def append(f: A => A, g: A => A): A => A = f compose g
  }

  trait AsApplicative[M] extends Applicative[Const[M, ?]] {
    implicit def M: Monoid[M]

    override def pure[A](a: A): Const[M, A] = M.empty
    override def ap[A, B](fa: Const[M, A])(f: Const[M, A => B]): Const[M, B] = M.append(fa, f)
  }
}
