package com.nrinaudo.abstractions.common

import com.nrinaudo.abstractions.{Monad, Functor}
import com.nrinaudo.abstractions.ops._

case class OrT[F[_], A, B](run: F[Or[A, B]]) {
  def map[C](f: B => C)(implicit F: Functor[F]): OrT[F, A, C] = OrT(run map (_ map f))
  def flatMap[C](f: B => OrT[F, A, C])(implicit F: Monad[F]): OrT[F, A, C] =
    OrT(run flatMap {
      case a@Left(_) => F.pure(a)
      case Right(b)  => f(b).run
    })
}

trait OrTInstances {
  implicit def orTFunctor[F[_]: Functor, A]: Functor[OrT[F, A, ?]] = new Functor[OrT[F, A, ?]] {
    override def map[B, C](fa: OrT[F, A, B])(f: B => C): OrT[F, A, C] = fa map f
  }

  implicit def orTMonad[F[_]: Monad, A]: Monad[OrT[F, A, ?]] = new Monad[OrT[F, A, ?]] {
    override def pure[B](b: B): OrT[F, A, B] = OrT(Monad[F].pure(Right(b)))

    override def flatMap[B, C](fa: OrT[F, A, B])(f: B => OrT[F, A, C]): OrT[F, A, C] = fa flatMap f
  }
}
