package com.nrinaudo.abstractions.std

import com.nrinaudo.abstractions._
import com.nrinaudo.abstractions.ops._

trait Function1Instances {
  // - Semigroup -------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  trait Function1Semigroup[F, G] extends Semigroup[F => G] {
    implicit def G: Semigroup[G]

    override def append(f: F => G, g: F => G) = a => f(a) |+| g(a)
  }

  implicit def function1Semigroup[F, G: Semigroup]: Semigroup[F => G] = new Function1Semigroup[F, G] {
    override val G = Semigroup[G]
  }



  // - Monoid ----------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  trait Function1Monoid[F, G] extends Monoid[F => G] with Function1Semigroup[F, G] {
    def G: Monoid[G]
    override def empty = _ => G.empty
  }

  implicit def function1Monoid[F, G: Monoid]: Monoid[F => G] = new Function1Monoid[F, G] {
    override val G = Monoid[G]
  }

  implicit def function1Instances[P] = new Applicative[P => ?] {
    override def map[A, B](fa: P => A)(f: A => B): P => B = f compose fa
    override def pure[A](a: A): P => A = _ => a
    override def ap[A, B](fa: P => A)(f: P => (A => B)): P => B = r => f(r)(fa(r))
  }
}
