package com.nrinaudo.abstractions.common

import com.nrinaudo.abstractions.{Id, Monad, Functor}
import com.nrinaudo.abstractions.ops._

case class ReaderT[F[_], A, B](run: A => F[B]) {
  def map[C](f: B => C)(implicit F: Functor[F]): ReaderT[F, A, C] = ReaderT { a => run(a) map f }

  def flatMap[C](f: B => ReaderT[F, A, C])(implicit F: Monad[F]): ReaderT[F, A, C] = ReaderT { a =>
    run(a) flatMap (b => f(b).run(a))
  }
}

object Reader {
  def apply[A, B](f: A => B): Reader[A, B] = ReaderT[Id, A, B](f)
}

trait ReaderTInstances {
  implicit def readerTFunctor[F[_]: Functor, A]: Functor[ReaderT[F, A, ?]] = new Functor[ReaderT[F, A, ?]] {
    override def map[C, D](fa: ReaderT[F, A, C])(f: C => D): ReaderT[F, A, D] = fa.map(f)
  }

  implicit def readerTMonad[F[_]: Monad, A]: Monad[ReaderT[F, A, ?]] = new Monad[ReaderT[F, A, ?]] {
    override def pure[B](b: B): ReaderT[F, A, B] = ReaderT(_ => Monad[F].pure(b))
    override def flatMap[B, C](fb: ReaderT[F, A, B])(f: B => ReaderT[F, A, C]): ReaderT[F, A, C] = fb flatMap f
  }
}
